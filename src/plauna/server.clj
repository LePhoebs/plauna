(ns plauna.server
  (:require [cheshire.core :refer [generate-string parse-string]]
            [clojure.core.async :as async]
            [clojure.data :as cd]
            [clojure.java.io :as io]
            [clojure.string :as st]
            [compojure.core :as comp]
            [compojure.route :as route]
            [nrepl.server :as nrepl]
            [plauna.analysis :as analysis]
            [plauna.application :as app]
            [plauna.auth :as auth]
            [plauna.client :as client]
            [plauna.client.oauth :as oauth]
            [plauna.diagnostics :as diagnostics]
            [plauna.core.email :as core-email]
            [plauna.core.flags :as flags]
            [plauna.database :as db]
            [plauna.db-config :as db-cfg]
            [plauna.db-migration :as db-mig]
            [plauna.files :as files]
            [plauna.markup :as markup]
            [plauna.messaging :as messaging]
            [plauna.i18n :as i18n]
            [plauna.preferences :as p]
            [plauna.settings :as settings]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.keyword-params :refer [wrap-keyword-params]]
            [ring.middleware.multipart-params :refer [wrap-multipart-params]]
            [ring.middleware.params :refer [wrap-params]]
            [ring.middleware.session :refer [wrap-session]]
            [ring.middleware.session.cookie :refer [cookie-store]]
            [ring.util.codec :refer [base64-decode]]
            [ring.util.response :refer [response redirect]]
            [selmer.parser :as selmer]
            [taoensso.telemere :as t])
  (:import [java.net ServerSocket]
           [java.time Duration Instant LocalTime ZonedDateTime ZoneId]
           [java.util UUID]
           [java.util.concurrent ExecutorService Executors ScheduledExecutorService ThreadFactory TimeUnit]
           [org.eclipse.jetty.server Server]
           [org.eclipse.jetty.util.thread QueuedThreadPool]))

(set! *warn-on-reflection* true)

;; Holds {:jetty ^Server, :executor ^ExecutorService} when running, nil otherwise.
(defonce server (atom nil))

(defonce repl-server (atom nil))

(defonce ^:private training-scheduler (atom nil))

(def ^:private training-lock (Object.))

(def html-headers {"Content-Type" "text/html; charset=UTF-8"})

(defonce global-messages (atom []))

(defonce training-progress
  ;; The state of the current (or last) model training run, polled by the progress page. Training runs
  ;; on a background thread because a full run takes minutes and a synchronous response would be cut
  ;; off by any reverse proxy's read timeout.
  (atom {:status :idle}))

(defonce batch-moves
  ;; Transient state of "move this parse batch to its category folders" jobs, keyed by batch id.
  (atom {}))

(defn message
  "A message for the toast area by its text key (see plauna.i18n). It is shown in the language of the
   page that shows it; :content keeps the English text for logs and callers that read it."
  [type k & args]
  {:type type :key k :args (vec args) :content (binding [i18n/*lang* :en] (i18n/t k args))})

(defn add-to-messages [message] (swap! global-messages (fn [messages] (conj messages message))))

(defn take-messages!
  "Atomically remove and return the pending toasts. A read followed by a separate reset would lose a
   toast that a background job (parse, move, training) adds in between."
  []
  (first (swap-vals! global-messages (constantly []))))

(defn interleave-all [& seqs]
  (reduce (fn [acc index] (into acc (map #(get % index) seqs)))
          []
          (range (apply max (map count seqs)))))

(defn vectorize [items]
  (if (vector? items) items [items]))

(def ^:private max-selection-values
  "A column filter's include/exclude list is cut off here: every value becomes a bound SQL parameter,
   and a crafted URL with tens of thousands of them would exceed the database's limit."
  1000)

(defn- vectorize-capped [items]
  (vec (take max-selection-values (vectorize items))))

(defn flatten-map [param-map]
  (let [message-ids (vectorize (get param-map :message-id []))
        languages (vectorize (get param-map :language []))
        categories (vectorize (get param-map :category []))
        language-confidence (vectorize (get param-map :language-confidence []))
        category-confidence (vectorize (get param-map :category-confidence))]
    (map (fn [vect] {:message-id (nth vect 0) :language (nth vect 1) :category (nth vect 2) :language-confidence (nth vect 3) :category-confidence (nth vect 4)}) (partition 5 (interleave-all message-ids languages categories language-confidence category-confidence)))))

(defn- parse-confidence
  "A confidence from a form field; 1.0 (a value the user set) when the field is missing, blank or not a
   number, instead of a NullPointerException and an error page."
  [value]
  (or (some-> value str st/trim parse-double) 1.0))

(defn params->update-request [params]
  (let [language (:language params)
        category-id (:category params)
        language-exists (and (some? language) (seq language))
        category-exists (and (some? category-id) (seq category-id))]
    {:language   (when language-exists (:language params))
     :category-id (when category-exists (Integer/parseInt (:category params)))
     :category-confidence  (when category-exists (parse-confidence (:category-confidence params)))
     :language-confidence (when language-exists (parse-confidence (:language-confidence params)))}))

(defn per-connection
  "Group located e-mails ({:connection-id ...}) by account, call (f connection-id emails) for each
   account and sum the counts it returns."
  [emails f]
  (apply merge-with + {} (for [[connection-id group] (group-by :connection-id emails)]
                           (f connection-id group))))

(defn sync-flags!
  "Let the flags of e-mails whose category changed without an IMAP move follow the new category
   (see plauna.core.flags/flag-change), on a background thread. changes: [{:message-id :old-category-id
   :new-category-id}]; unchanged categories and e-mails without a known location are skipped."
  [changes]
  (let [colors (into {} (map (juxt :id #(flags/valid-color (:flag_color %)))) (db/get-categories))
        jobs (into []
                   (keep (fn [{:keys [message-id old-category-id new-category-id]}]
                           (when (not= old-category-id new-category-id)
                             (when-let [flag (flags/flag-change (get colors old-category-id) (get colors new-category-id))]
                               (let [{:keys [folder connection-id]} (db/email-location message-id)]
                                 (when (and folder connection-id)
                                   {:message-id message-id :folder folder :connection-id connection-id :flag flag}))))))
                   changes)]
    (when (seq jobs)
      (async/thread
        (try (per-connection jobs client/apply-flags!)
             (catch Throwable e (t/log! {:level :warn :error e} "Could not update the flags of recategorized e-mails")))))))

(defn save-metadata-form
  "Save the language and category fields of a metadata form. With sync-flags? the flags of e-mails
   whose category changed follow the new category; leave it off when an IMAP move has already done it."
  ([params] (save-metadata-form params true))
  ([params sync-flags?]
   (let [transformed (flatten-map params)
         changes (mapv (fn [x]
                         (let [request (params->update-request x)
                               old-category-id (:category-id (db/email-category-state (:message-id x)))]
                           (db/update-metadata (:message-id x) (:category-id request) (:category-confidence request) (:language request) (:language-confidence request))
                           (when (not= old-category-id (:category-id request))
                             (db/category-changed! (:message-id x)))
                           {:message-id (:message-id x) :old-category-id old-category-id :new-category-id (:category-id request)}))
                       transformed)]
     (when sync-flags? (sync-flags! changes)))))

(defn success-html-with-body [body]
  {:status  200
   :headers {"Content-Type" "text/html; charset=UTF-8"}
   :body    body})

(defn safe-redirect-path
  "Only allow same-origin, relative redirect targets (a single leading slash, not \"//host\" or a scheme).
   Anything else (an absolute URL, protocol-relative URL, or nil) falls back to `default`, preventing an
   attacker-supplied redirect-url from turning into an open redirect."
  [target default]
  ;; A backslash is excluded as second character too: browsers normalise \"/\\\\evil.example\" to the
  ;; protocol-relative \"//evil.example\".
  ;; Control characters are refused as well: browsers strip tabs and newlines while parsing a URL, so
  ;; "/\t/evil.example" would become "//evil.example".
  (if (and (string? target)
           (not (re-find #"[\x00-\x1f\x7f]" target))
           (re-matches #"/(?:[^/\\].*)?" target))
    target
    default))

(defn same-origin-referer
  "The path (+query) of the request's Referer, as a same-origin redirect target, or nil. Only the path is
   taken - the Referer's host is ignored - and it must pass safe-redirect-path, so a Referer from another
   site can never send the browser away, and a reverse proxy that rewrites Host or the port does not
   break the redirect back to the page the user came from."
  [request]
  (when-let [referer (get (:headers request) "referer")]
    (try
      (let [uri (java.net.URI. referer)
            path (.getRawPath uri)
            q (.getRawQuery uri)]
        (safe-redirect-path (str (if (st/blank? path) "/" path) (when q (str "?" q))) nil))
      (catch Exception _ nil))))

(defn redirect-to-referer [request]
  {:status 303
   ;; Fall back to the app root when the Referer is absent or off-site, so we never emit a 303 with a
   ;; nil Location or redirect the user to an attacker-controlled origin.
   :headers {"Location" (or (same-origin-referer request) "/")}})

(defn redirect-request
  ([request]
   {:status 303 :headers {"Location" (safe-redirect-path (get-in request [:params :redirect-url]) (:uri request))}})
  ([request messages]
   (swap! global-messages (fn [m] (conj m messages)))
   {:status 303 :headers {"Location" (safe-redirect-path (get-in request [:params :redirect-url]) (:uri request))}}))

(defn not-found-response [text] {:status 404 :headers html-headers :body (str "Not found: " text)})

(defn bad-request-response [text] {:status 400 :headers html-headers :body (str "Invalid request: " text)})

(defn- decode-message-id
  "The Message-ID encoded in an /emails/:id URL, or nil when the id is not valid base64. Links use the
   URL-safe alphabet; standard Base64 from older bookmarks is accepted too."
  [id]
  (try (String. ^bytes (base64-decode (-> (str id) (st/replace "-" "+") (st/replace "_" "/"))) "UTF-8")
       (catch Exception _ nil)))

(def session-max-age-millis
  "A login is good for this long; afterwards the browser has to log in again even with a valid cookie."
  (* 30 24 60 60 1000))

(defn authenticate-session
  "The session of a browser that just proved who it is: marked authenticated for the current credential
   generation (see p/session-generation) and stamped with the login time."
  [session]
  (assoc session
         :authenticated true
         :auth-generation (p/session-generation)
         :authenticated-at (System/currentTimeMillis)))

(defn authenticated-session?
  "True for a session that logged in with the CURRENT credentials and not too long ago. A password or
   login-name change bumps the generation and thereby ends every session created before it - the cookie
   store is stateless, so nothing else could revoke a leaked cookie. Sessions from before the generation
   existed carry neither field and stay valid until the credentials change."
  [session]
  (boolean
   (and (:authenticated session)
        (= (long (or (:auth-generation session) 0)) (long (p/session-generation)))
        (let [at (:authenticated-at session)]
          (or (nil? at) (< (- (System/currentTimeMillis) (long at)) session-max-age-millis))))))

(def ^:private auth-provider-columns
  "The columns of auth_providers an administrator may set. Form field names must never double as SQL
   identifiers, so everything else a request carries is dropped."
  [:name :auth_url :token_url :redirect_url :client_id :client_secret :scope])

(defn- auth-provider-fields [params] (select-keys params auth-provider-columns))

(defonce ^:private event-bus-job
  ;; The mbox import and the language re-detection push work through the shared event bus, each held
  ;; back by a limiter that counts events by TYPE. Two of them at once would release each other's
  ;; tokens, flood the bus and can deadlock it - so only one runs at a time.
  (atom nil))

(defn- claim-event-bus-job!
  "Claim the event bus for the job described by what; false when another job holds it."
  [what]
  (compare-and-set! event-bus-job nil what))

(defn- release-event-bus-job! [] (reset! event-bus-job nil))

(def event-bus-busy-message
  (message :info :msg.another-mbox-import-or-language-re))

(defn- start-mbox-import!
  "Parse an uploaded mbox on a background thread and report the outcome as a toast. The multipart
   temp file is moved into Plauna's data folder first: the upload middleware deletes its temp files
   after a while, which could cut a long import short. The copy is removed when the import ends.
   Returns false without doing anything while another event-bus job runs."
  [^java.io.File temp-file original-name]
  (if-not (claim-event-bus-job! :mbox-import)
    false
    (let [target (io/file (files/file-dir) (str "import-" (UUID/randomUUID) ".mbox"))]
      (try
        (io/make-parents target)
        (when-not (.renameTo temp-file target)
          (io/copy temp-file target))
        (catch Throwable e
          (release-event-bus-job!)
          (throw e)))
      (async/thread
        (try
          (files/read-emails-from-mbox (io/input-stream target) @messaging/main-chan)
          (add-to-messages {:type :success :content (str "Finished reading the mbox file " original-name ". Its e-mails are being categorized and saved.")})
          (catch Throwable e
            (t/log! {:level :error :error e} ["Importing the mbox file" original-name "failed"])
            (add-to-messages {:type :alert :content (str "Importing the mbox file " original-name " failed. Please see the logs.")}))
          (finally
            (io/delete-file target true)
            (release-event-bus-job!))))
      true)))

(def ^:private redetection-chunk-size 200)

(defn start-language-redetection!
  "Queue a language detection for every stored e-mail without a language and return how many were
   queued. The ids are collected first and each e-mail is queued exactly once, on a background thread:
   paging the 'language IS NULL' set while the detections are still being written asynchronously would
   queue the same e-mails again and again (and never end when some of them yield no language at all).
   Returns :busy without doing anything while another event-bus job runs."
  []
  (if-not (claim-event-bus-job! :language-redetection)
    :busy
    (let [ids (try (mapv :message-id (db/query-db {:select [:headers.message-id]
                                                   :from [:headers]
                                                   :left-join [:metadata [:= :headers.message-id :metadata.message-id]]
                                                   :where [:= :metadata.language nil]}))
                   (catch Throwable e (release-event-bus-job!) (throw e)))]
      (if (empty? ids)
        (release-event-bus-job!)
        (async/thread
          (let [limiter (messaging/channel-limiter :enriched-email)]
            (try
              (t/log! :info ["Re-detecting the language of" (count ids) "e-mail(s) without one."])
              (doseq [chunk (partition-all redetection-chunk-size ids)
                      enriched-email (db/fetch-data {:entity :enriched-email :strict false}
                                                    {:where [:in :headers.message-id (vec chunk)]})]
                (async/>!! (:bucket limiter) :token)
                ;; :metadata-only - the e-mails are stored already; only their metadata row is rewritten.
                (async/>!! @messaging/main-chan {:type :language-detection-request :options {:metadata-only true} :payload enriched-email}))
              (add-to-messages {:type :success :content (str "Language detection was queued for " (count ids) " e-mail(s).")})
              (catch Throwable e
                (t/log! {:level :error :error e} "Re-detecting languages failed.")
                (add-to-messages (message :alert :msg.re-detecting-languages-failed-please-see)))
              (finally
                (messaging/close-limiter! limiter)
                (release-event-bus-job!))))))
      (count ids))))

(defn- normalize-prefs [prefs]
  (mapv #(update % :use_in_training (fn [v] (or (true? v) (= 1 v)))) prefs))

(defn language-preferences []
  (let [preferences (db/get-language-preferences)
        ;; Drop nil/blank languages as well as "n/a": metadata rows without a detected language would
        ;; otherwise be treated as a preference to insert, violating the NOT NULL constraint (which
        ;; MariaDB propagates as a 500, unlike SQLite's INSERT OR IGNORE).
        languages (filterv (fn [lang] (and (not (st/blank? lang)) (not= "n/a" lang)))
                           (mapv :language (db/get-languages)))]
    (if (empty? languages)
      []
      ;; Always the difference, not a comparison of counts: preferences of languages no e-mail has any
      ;; more stay stored, so equal counts can still hide a new language.
      (let [missing (vec (distinct (remove (set (map :language preferences)) languages)))]
        (normalize-prefs
         (if (seq missing)
           (do (db/add-language-preferences (mapv vector missing (repeat (count missing) false)))
               (db/get-language-preferences))
           preferences))))))

(defn languages-to-use-in-training []
  (map :language (db/get-activated-language-preferences)))

(defn- categorized-email-count
  "How many categorized e-mails exist in the given languages: the size of the collection phase."
  [languages]
  (if (seq languages)
    (or (:count (first (db/query-db {:select [[[:count :*] :count]]
                                     :from [:metadata]
                                     :where [:and [:<> :category nil] [:in :language (vec languages)]]})))
        0)
    0))

(defn- report-training-progress!
  "Phase-level events merge into the run state; per-language events (those carrying :language) are
   kept side by side under :language-progress because the languages train in parallel."
  [progress]
  (if-let [language (:language progress)]
    (swap! training-progress assoc-in [:language-progress language] (dissoc progress :language))
    (swap! training-progress merge progress)))

(defn- attach-bodies
  "Load the body parts of e-mails that were fetched without them (one query for the whole page)."
  [emails]
  (let [ids (mapv #(-> % :header :message-id) emails)
        bodies (group-by :message-id (db/fetch-bodies-for ids))]
    (mapv (fn [email]
            (assoc email :body (map core-email/construct-body-part (get bodies (-> email :header :message-id)))))
          emails)))

(defn- cached-classification-tokens
  "The classification tokens of a page of e-mails as {message-id tokens}: from the training_tokens
   cache where present, otherwise computed now (in parallel, this is the expensive text cleaning) and
   cached for every later run. on-computed receives the number of e-mails analysed for the first time."
  [emails on-computed]
  (let [message-id #(-> % :header :message-id)
        cached (db/fetch-training-tokens-for (mapv message-id emails) analysis/training-tokens-version)
        missing (remove #(contains? cached (message-id %)) emails)
        computed (when (seq missing)
                   (into {} (pmap (fn [email] [(message-id email) (analysis/training-tokens-text email)])
                                  (attach-bodies missing))))]
    (when (seq computed)
      (db/save-training-tokens! computed analysis/training-tokens-version)
      (on-computed (count computed)))
    (merge cached computed)))

(defn- training-tokens-for [emails]
  (cached-classification-tokens emails #(swap! training-progress update :tokens-computed (fnil + 0) %)))

(defn write-all-categorized-emails-to-training-files []
  (files/delete-files-with-type :train)
  (let [languages (languages-to-use-in-training)]
    (report-training-progress! {:phase :collecting :emails-written 0 :tokens-computed 0
                                :emails-total (categorized-email-count languages)
                                :training-languages (vec languages)})
    (doseq [language languages
            ;; Bodies are not loaded with the page: cached tokens make them unnecessary for most e-mails.
            :let [entity-query {:entity :enriched-email :page {:size 200 :page 1} :with-bodies false}
                  ;; A stable order: LIMIT/OFFSET pages without one may skip or repeat e-mails.
                  sql-query {:where [:and [:<> :category nil] [:= :language language]]
                             :order-by [[:headers.message-id :asc]]}
                  write-func (fn [emails]
                               (let [tokens (training-tokens-for emails)
                                     lines (analysis/format-training-lines
                                            (map (fn [email] [(-> email :metadata :category-id) (get tokens (-> email :header :message-id))]) emails))]
                                 (when (seq lines)
                                   (files/write-to-training-file language lines)))
                               (swap! training-progress update :emails-written (fnil + 0) (count emails)))]]
      (core-email/iterate-over-all-pages db/fetch-data write-func entity-query sql-query false))))

(defn- category-label-name
  "Human-readable name for a training label (a category id; legacy files may hold names)."
  [label]
  ;; Written by this run, so always a category id.
  (or (:name (analysis/label->category label :ids)) label))

(defn- single-outcome-note [{:keys [language samples labels]}]
  (str language ": all " samples " categorized e-mail(s) belong to one category (" (category-label-name (first labels))
       "); a model needs at least two categories"))

(defn training-outcome-message
  "Summarize a training run that trained some languages and skipped or failed others. Returns nil
   when every language trained, an :info when at least one model was written, otherwise an :alert."
  [trained skipped failed]
  (let [notes (concat (map single-outcome-note skipped)
                      (map (fn [{:keys [language error]}] (str language ": training failed (" (.getMessage ^Throwable error) ")")) failed))]
    (cond
      (empty? notes) nil

      (seq trained)
      {:type :info
       :content (str "Trained the " (st/join ", " (map :language trained)) " model(s). Not trained - "
                     (st/join "; " notes) ".")}

      :else
      {:type :alert
       :content (str "No model was trained. " (st/join "; " notes) ".")})))

(defn train-categorization-model!
  "Build the requested model family from the current categorized e-mails. Model files are kept per
   algorithm, so training an inactive family cannot damage the active classifier. A language whose
   data holds only one category, or whose training fails, is skipped and named in the result while
   the other languages are still trained. Returns nil when every language trained, an :info map for a
   partial result and an :alert map when nothing could be trained."
  [model]
  ;; Rebuilding the training files is destructive. Serialize manual and automatic runs so one cannot
  ;; delete or overwrite the files while the other is still reading them.
  (locking training-lock
    (if (seq (languages-to-use-in-training))
      (do (write-all-categorized-emails-to-training-files)
          (let [training-files (vec (files/training-files))]
            (if (seq training-files)
              (let [inspected (map #(merge % (analysis/training-file-outcomes (:file %))) training-files)
                    ;; OpenNLP rejects data with a single outcome (InsufficientTrainingDataException); skip
                    ;; those languages up front instead of letting one of them abort the whole run.
                    {trainable true skipped false} (group-by #(> (count (:labels %)) 1) inspected)
                    _ (report-training-progress! {:phase :training :languages (count trainable) :language-progress {}
                                                  :skipped-languages (mapv (fn [entry] {:language (:language entry) :reason (single-outcome-note entry)}) skipped)})
                    results (analysis/train-data (vec trainable) model report-training-progress!)
                    {trained :trained failed :failed} (group-by #(if (:error %) :failed :trained) results)]
                (report-training-progress! {:phase :writing :models-written 0 :models-total (count trained)})
                (doseq [training-model trained]
                  (files/write-model-file-atomically!
                   (:language training-model)
                   model
                   #(analysis/serialize-and-write-model! (:model training-model) %))
                  (swap! training-progress update :models-written (fnil inc 0)))
                (training-outcome-message trained skipped failed))
              (message :alert :msg.there-are-no-categorized-e-mails))))
      (message :alert :msg.there-are-no-selected-languages-to))))

(defn write-emails-to-training-files-and-train []
  (train-categorization-model! (p/categorization-model)))

(defn training-percent
  "Overall progress of a training run as 0-100. Collecting the training data is the first quarter;
   the remaining three quarters are split evenly across the languages (which train in parallel) and,
   within a language, advance linearly with the iterations. Trainers without iteration callbacks
   (Naive Bayes, L-BFGS) report only their completion."
  [{:keys [status phase emails-written emails-total languages language-progress]}]
  (let [fraction (fn [part whole] (if (and whole (pos? whole)) (min 1.0 (/ (double (or part 0)) whole)) 0.0))
        language-fraction (fn [{:keys [done? iteration iterations]}] (if done? 1.0 (fraction iteration iterations)))
        value (case phase
                :collecting (* 25 (fraction emails-written emails-total))
                :training (let [language-count (max 1 (or languages (count language-progress) 1))]
                            (+ 25 (* 75 (/ (reduce + 0.0 (map language-fraction (vals language-progress))) language-count))))
                :writing 99
                0)]
    (if (= :finished status) 100 (int (Math/round (double value))))))

(defn training-running? [] (= :running (:status @training-progress)))

(defn- claim-training-run!
  "Atomically mark a training run as started. Returns false when another run is still in progress."
  [label]
  (let [[before _] (swap-vals! training-progress
                               (fn [state]
                                 (if (= :running (:status state))
                                   state
                                   {:status :running :label label :phase :starting
                                    :started-at (System/currentTimeMillis) :emails-written 0 :emails-total 0})))]
    (not= :running (:status before))))

(def training-busy-message (message :info :msg.a-model-training-run-is-already))

(def training-success-message (message :success :msg.training-finished-the-updated-model-is))

(defn- execute-training-job!
  "Run a claimed training job to completion and publish its outcome. job-fn returns a message map
   (:type :alert/:info/:success) or nil for a plain success."
  [job-fn]
  (let [result (try
                 (or (job-fn) training-success-message)
                 (catch Throwable e
                   (t/log! {:level :error :error e} "Model training failed.")
                   (message :alert :msg.training-failed-the-existing-model-remains)))]
    ;; The phase is left where the run ended, so the step list can show which step failed.
    (swap! training-progress assoc :status :finished :finished-at (System/currentTimeMillis) :result result)
    ;; Also surface the outcome on the next page load: the progress page may long have been closed.
    (add-to-messages result)
    result))

(defn run-training-job!
  "Synchronously run job-fn as the current training run (used by the automatic scheduler). Returns its
   result message, or training-busy-message when another run holds the slot."
  [label job-fn]
  (if (claim-training-run! label)
    (execute-training-job! job-fn)
    training-busy-message))

(defn start-training-job!
  "Run job-fn as the current training run on a background thread. Returns true when the run was
   started and false when another run is already in progress."
  [label job-fn]
  (if (claim-training-run! label)
    (do (async/thread (execute-training-job! job-fn)) true)
    false))

(defn manual-training-job
  "Retrain the active model. A run that wrote at least one model (nil or :info result) counts as
   successful for the daily schedule, so a language that cannot be trained yet does not make Plauna
   retrain on every start."
  []
  (let [result (write-emails-to-training-files-and-train)]
    (when (or (nil? result) (= :info (:type result)))
      (p/record-successful-training! (Instant/now)))
    result))

;; ── Model scores ───────────────────────────────────────────────────────────────────
;; The e-mail list's suggestion filter can look for e-mails the model would put into a category. That
;; needs the model's probabilities for every stored e-mail, which a background run computes and stores
;; (category_scores) - scoring thousands of e-mails takes too long for a request.

(defonce scoring-progress (atom {:status :idle}))

(defn scoring-running? [] (= :running (:status @scoring-progress)))

(def ^:private min-stored-score
  "Scores below 1 % are not stored: the model suggestions start at 1 %, and storing every e-mail ×
   category pair would grow the table by millions of useless rows."
  0.01)

(defn- score-page!
  "Score one page of e-mails with the current models and replace their stored scores; returns how many
   e-mails got scores (those of a language without a model get none, and lose their old ones). A page
   that cannot be stored is logged and counted as failed, so one bad page does not end the run."
  [emails]
  (let [tokens (cached-classification-tokens emails #(swap! scoring-progress update :tokens-computed (fnil + 0) %))
        scored-rows (for [email emails
                          :let [message-id (-> email :header :message-id)
                                scores (analysis/category-scores-for-tokens-text (get tokens message-id) (-> email :metadata :language))]
                          score scores]
                      [message-id (:id score) (double (:confidence score))])
        scored (count (distinct (map first scored-rows)))
        stored? (try (db/replace-category-scores! (mapv #(-> % :header :message-id) emails)
                                                  (filterv #(>= (double (nth % 2)) min-stored-score) scored-rows))
                     true
                     (catch Exception e
                       (t/log! {:level :warn :error e} "Could not store the model scores of a page of e-mails.")
                       false))]
    (swap! scoring-progress #(cond-> (update % :done (fnil + 0) (count emails))
                               stored? (update :scored (fnil + 0) scored)
                               (not stored?) (update :failed (fnil + 0) (count emails))))
    (if stored? scored 0)))

(def ^:private scoring-page-size 200)

(defn score-all-emails!
  "Replace the stored model scores with those of the current models for every e-mail with a detected
   language, page by page: until the run is over, the other e-mails keep their previous scores. Pages go
   by message id (keyset), so e-mails arriving meanwhile do not shift the pages. Records when it
   finished and with which model, for the Model page; returns how many e-mails got scores."
  []
  (let [model (p/categorization-model)
        trained-at (some-> (p/last-successful-training-at) str)
        entity-query {:entity :enriched-email :with-bodies false}
        where [:<> :language nil]
        total (:total (db/fetch-data (assoc entity-query :page {:size 1 :page 1}) {:where where}))]
    (p/record-model-scores-started!)
    (swap! scoring-progress assoc :total (or total 0))
    (loop [after nil]
      (let [emails (db/fetch-data entity-query {:where (if after [:and where [:> :headers.message-id after]] where)
                                                :order-by [[:headers.message-id :asc]]
                                                :limit scoring-page-size})]
        (when (seq emails)
          (score-page! emails)
          (when (= scoring-page-size (count emails))
            (recur (-> emails last :header :message-id))))))
    (db/delete-unscorable-category-scores!)
    (let [{:keys [scored failed] :or {scored 0 failed 0}} @scoring-progress]
      (p/record-model-scores! (Instant/now) scored model trained-at (zero? failed))
      scored)))

(defn start-scoring-job!
  "Score every e-mail on a background thread. false when a scoring run is already in progress."
  []
  (let [[before _] (swap-vals! scoring-progress
                               (fn [state]
                                 (if (= :running (:status state))
                                   state
                                   {:status :running :started-at (System/currentTimeMillis) :done 0 :scored 0 :failed 0 :total 0 :tokens-computed 0})))]
    (if (= :running (:status before))
      false
      (do (async/thread
            (let [result (try
                           (let [scored (score-all-emails!)
                                 failed (:failed @scoring-progress 0)]
                             (if (pos? failed)
                               {:type :alert :key :scoring.partial :args [scored failed]}
                               {:type :success :key :scoring.done :args [scored]}))
                           (catch Throwable e
                             (t/log! {:level :error :error e} "Scoring the e-mails with the model failed.")
                             {:type :alert :key :scoring.failed}))]
              (swap! scoring-progress assoc :status :finished :finished-at (System/currentTimeMillis) :result result)
              ;; Also shown on the next page load: the list reloads itself when the run is over.
              (add-to-messages result)))
          true))))

(defn model-scores-summary
  "What the Model page says about the stored model scores: when they were computed (in the configured
   time zone), for how many e-mails, whether they are from an older model (a training ran or another
   model type was activated since), whether the last run did not finish, and whether a run is in
   progress."
  []
  (let [{:keys [at emails model trained-at complete?]} (p/model-scores-info)
        current-trained-at (p/last-successful-training-at)]
    {:at-text (when at (.format (.atZone ^Instant at ^java.time.ZoneId (p/zone-id)) (java.time.format.DateTimeFormatter/ofPattern "dd.MM.yyyy HH:mm")))
     :emails emails
     :stale? (boolean (and at
                           (if model
                             (or (not= model (p/categorization-model))
                                 (not= trained-at (some-> current-trained-at str)))
                             ;; Scores stored before the model was recorded with them.
                             (and current-trained-at (.isAfter ^Instant current-trained-at ^Instant at)))))
     :incomplete? (and (not complete?) (not (scoring-running?)))
     :running? (scoring-running?)}))

(defn- format-instant [^Instant instant]
  (some-> instant (.atZone (p/zone-id)) (.format (java.time.format.DateTimeFormatter/ofPattern "dd.MM.yyyy HH:mm"))))

(defn model-page-data
  "What the Model page shows: training state and schedule, languages, thresholds, the model type and
   the stored scores."
  []
  {:training-running? (training-running?)
   :last-training (format-instant (p/last-successful-training-at))
   :training-time (p/automatic-training-time)
   :model-type (p/categorization-model)
   :model-options analysis/supported-categorization-models
   :language-preferences (language-preferences)
   :language-detection-threshold (p/language-detection-threshold)
   :categorization-threshold (p/categorization-threshold)
   :model-scores (model-scores-summary)})

(def category-tabs #{"suggestions" "emails" "settings"})

(defn category-page
  "A category's page with the tabs suggestions (default), emails and settings. params: :tab, and for
   the suggestions :by (sender, domain or model; default sender) and :min (the model's minimum
   percentage)."
  ([context category params] (category-page context category params nil))
  ([context category params messages]
  (let [id (:id category)
        tab (get category-tabs (:tab params) "suggestions")
        flag (first (filter #(= (:flag_color category) (:id %)) (markup/flag-colors)))
        base {:category (assoc category :flag_hex (:hex flag) :flag_label (:label flag))
              :email-count (app/category-email-count context id)
              :tab tab}]
    (markup/category-page
     (case tab
       "emails" (assoc base :emails (app/recent-category-emails context id 25))
       "settings" (assoc base :flag-colors (markup/flag-colors))
       (let [{:keys [method min-percent]} (app/suggestion-parameters {:suggest-category (str id) :suggest-by (:by params) :suggest-min (:min params)})
             ;; Only the selected method's count: the page loads the others afterwards.
             counts (app/suggestion-counts context id min-percent [method])
             selected-count (get counts method 0)
             candidates (when (pos? (long selected-count)) (app/suggestion-candidates context id method min-percent))
             scores (when (= method "model") (db/category-scores-for id (mapv #(get-in % [:header :message-id]) candidates)))]
         (assoc base
                :method method
                :min-percent min-percent
                :counts (update-keys counts keyword)
                :selected-count selected-count
                :groups (app/group-suggestions candidates method scores)
                :capped? (> (long selected-count) (count candidates))
                :shown-count (count candidates)
                :review-url (str "/emails?size=50&suggest-category=" id "&suggest-by=" method "&suggest-min=" min-percent)
                :model-scores (model-scores-summary))))
     messages))))

(defn scoring-status
  "The e-mail list's JSON view of the current (or last) scoring run."
  []
  (let [{:keys [status done total result]} @scoring-progress]
    {:status status :done (or done 0) :total (or total 0)
     :result (some-> result :key (i18n/t (:args result))) :result-type (:type result)}))

(def ^:private training-phase-order [:starting :collecting :training :writing])

(defn- step-state
  "pending / running / done / failed for the step of phase step-phase given the run's current phase and
   status. A finished run marks every step before the last phase done; the last phase is done or failed
   depending on the result; later steps stay pending."
  [step-phase {:keys [status phase result]}]
  (let [index (fn [ph] (.indexOf ^java.util.List training-phase-order ph))
        current (index (or phase :starting))
        mine (index step-phase)]
    (cond
      (< mine current) "done"
      (> mine current) "pending"
      (= :finished status) (if (= :alert (:type result)) "failed" "done")
      :else "running")))

(defn- language-steps
  "One sub-step per language: pending, running (with the iteration), done, failed or skipped."
  [{:keys [training-languages language-progress skipped-languages status phase]}]
  (let [skipped (into {} (map (juxt :language :reason)) skipped-languages)
        languages (distinct (concat training-languages (keys language-progress) (keys skipped)))]
    (vec (for [language (sort languages)
               :let [{:keys [iteration iterations done? failed?]} (get language-progress language)]]
           {:label language
            :state (cond
                     (contains? skipped language) "skipped"
                     failed? "failed"
                     done? "done"
                     (and (= :finished status) (not= :writing phase)) (if (contains? language-progress language) "failed" "pending")
                     (contains? language-progress language) "running"
                     :else "pending")
            :detail (cond
                      (contains? skipped language) (get skipped language)
                      (and (not done?) (some-> iteration pos?)) (i18n/t :training.iteration [iteration iterations])
                      :else nil)}))))

(defn training-steps
  "The checklist shown on the progress page: every step of a training run with its state and a detail
   line for the running step."
  [{:keys [status emails-written emails-total tokens-computed models-written models-total result] :as state}]
  (let [collect-state (step-state :collecting state)
        training-state (step-state :training state)
        writing-state (step-state :writing state)]
    [{:id "prepare" :label (i18n/t :training.prepare) :state (step-state :starting state) :detail nil}
     {:id "collect" :label (i18n/t :training.collect)
      :state collect-state
      :detail (when (not= "pending" collect-state)
                (str (i18n/t :training.collected [(or emails-written 0) (or emails-total 0)])
                     (when (some-> tokens-computed pos?) (i18n/t :training.cached [tokens-computed]))))}
     {:id "train" :label (i18n/t :training.train)
      :state training-state
      :detail nil
      :children (language-steps state)}
     {:id "write" :label (i18n/t :training.write)
      :state writing-state
      :detail (when (not= "pending" writing-state) (i18n/t :training.written [(or models-written 0) (or models-total 0)]))}
     {:id "finish" :label (i18n/t :training.finish)
      :state (cond (not= :finished status) "pending"
                   (= :alert (:type result)) "failed"
                   :else "done")
      :detail (when (= :finished status) (:content result))}]))

(def ^:private training-label-keys
  "The text keys of the training runs' labels (the labels stay English in the state and the logs)."
  {"Automatic training" :training.label-automatic
   "Manual training" :training.label-manual
   "Model switch" :training.label-switch})

(defn training-status
  "The progress page's JSON view of the current run."
  []
  (let [raw @training-progress
        ;; A keyed message reads in the language of the progress page that polls for it - in the result
        ;; and in the "Finish" step that shows it.
        state (update raw :result #(if (:key %) (assoc % :content (i18n/t (:key %) (:args %))) %))]
    (-> (select-keys state [:status :label :phase :started-at :finished-at :result])
        (update :label #(if-let [k (get training-label-keys %)] (i18n/t k) %))
        (assoc :percent (training-percent state)
               :steps (training-steps state)))))

(defn training-progress-url [back] (str "/training/progress?back=" (java.net.URLEncoder/encode (str back) "UTF-8")))

(def model-switch-confirmation "Start training")

(defn- languages-without-model
  "The training languages for which no model file of the given family exists."
  [model]
  (vec (remove #(.exists ^java.io.File (files/model-file % model)) (languages-to-use-in-training))))

(defn- languages-with-model [model]
  (vec (filter #(.exists ^java.io.File (files/model-file % model)) (languages-to-use-in-training))))

(defn- uncategorized-languages-note [languages]
  (when (seq languages)
    (str " E-mails in " (st/join ", " languages) " are not categorized by this model until those languages have e-mails in at least two categories and a training run has succeeded for them.")))

(defn model-switch-validation
  "The alert/info message that rejects a model switch request, or nil when the request is valid.
   Kept separate from the switch itself so a rejected request never occupies the training slot."
  [{:keys [model confirmation]}]
  (let [valid-models (set (map :id analysis/supported-categorization-models))
        target (when (contains? valid-models model) model)
        current (p/categorization-model)]
    (cond
      (nil? target)
      (message :alert :msg.unknown-categorization-model-nothing-was-changed)

      (= target current)
      (message :info :msg.the-selected-categorization-model-is-already)

      (not= model-switch-confirmation confirmation)
      {:type :alert :content (str "Model switch not confirmed. Enter ‘" model-switch-confirmation "’ exactly.")}

      :else nil)))

(defn switch-categorization-model!
  "Validate and perform an administrator-requested model switch. The active preference changes only
   after every requested target model was trained successfully (or a complete target model set was
   already available)."
  [{:keys [model use-current-categories] :as params}]
  (let [target model
        train-current? (contains? #{true "true" "on"} use-current-categories)]
    (or
      (model-switch-validation params)
      (try
        (if train-current?
          (let [result (train-categorization-model! target)]
            (if (= :alert (:type result))
              ;; Nothing could be trained: there is no model to switch to.
              result
              ;; Every trainable language has a model now. A language whose e-mails all share one
              ;; category never had a usable model in any family, so it must not block the switch; it
              ;; is named instead.
              (do (p/update-preference :categorization-algorithm target)
                  (p/record-successful-training! (Instant/now))
                  (if (nil? result)
                    {:type :success
                     :content (str "Switched to " target " after training with the current category assignments.")}
                    {:type :info
                     :content (str "Switched to " target ". " (:content result)
                                   (uncategorized-languages-note (languages-without-model target)))}))))
          (let [missing (languages-without-model target)]
            (if (seq (languages-with-model target))
              (do (p/update-preference :categorization-algorithm target)
                  (if (seq missing)
                    {:type :info :content (str "Switched to the existing " target " model." (uncategorized-languages-note missing))}
                    {:type :success :content (str "Switched to the existing " target " model.")}))
              {:type :alert
               :content "No model of that type exists yet. Keep the training option selected for the first switch."})))
        (catch Throwable e
          (t/log! {:level :error :error e} "Categorization model switch failed; the previous model remains active.")
          {:type :alert
           :content "Training failed. The previous categorization model remains active."})))))

(defn- run-automatic-training! []
  ;; Never let an exception escape the scheduled task: ScheduledExecutorService would otherwise
  ;; silently suppress every subsequent run. run-training-job! turns failures into an :alert result.
  (try
    (t/log! :info "Starting automatic model training.")
    (let [result (run-training-job! "Automatic training" manual-training-job)]
      (cond
        (identical? result training-busy-message)
        (do (t/log! :warn "Automatic model training skipped: another training run is in progress. Plauna will try again at the next scheduled time.")
            false)

        (contains? #{:success :info} (:type result))
        (do (t/log! :info ["Automatic model training completed." (when (= :info (:type result)) (:content result))])
            true)

        :else
        (do (t/log! :warn ["Automatic model training did not complete:" (:content result) "Plauna will try again at the next scheduled time."])
            false)))
    (catch Throwable e
      (t/log! {:level :error :error e}
              "Automatic model training failed. Plauna will try again at the next scheduled time.")
      false)))

(defn- training-thread-factory []
  (reify ThreadFactory
    (newThread [_ runnable]
      (doto (Thread. ^Runnable runnable "plauna-training-scheduler")
        (.setDaemon true)))))

(defn daily-training-delay-millis
  "Milliseconds from now until the next required daily training. If today's configured time has
   passed and no successful run was recorded for it, return zero so a restart catches it up."
  [^ZonedDateTime now ^LocalTime training-time last-success]
  (let [zone (.getZone now)
        date (.toLocalDate now)
        ;; The training time is joined with each calendar day on its own: shifting today's moment by a
        ;; day would carry a DST adjustment over (02:00 does not exist on the day clocks go forward and
        ;; becomes 03:00 - the next day must be 02:00 again).
        at-day (fn [^java.time.LocalDate d] (.atZone (.atTime d training-time) zone))
        ^ZonedDateTime today (at-day date)
        before-today? (.isBefore now today)
        ^ZonedDateTime latest-due (if before-today? (at-day (.minusDays date 1)) today)
        missed? (if last-success
                  (.isBefore ^Instant last-success (.toInstant latest-due))
                  (not before-today?))
        ^ZonedDateTime next-time (if missed?
                    now
                    (if before-today? today (at-day (.plusDays date 1))))]
    (max 0 (.toMillis (Duration/between (.toInstant now) (.toInstant next-time))))))

(declare schedule-next-daily-training!)

(defn- schedule-next-daily-training!
  [^ScheduledExecutorService executor catch-up-missed?]
  (let [^ZoneId zone (p/zone-id)
        ^LocalTime training-time (LocalTime/parse (p/automatic-training-time))
        ^ZonedDateTime now (ZonedDateTime/now zone)
        ;; Only startup/reconfiguration catches up a missed persisted run. After an attempted run,
        ;; even a failure waits for the next day instead of spinning in an immediate retry loop.
        effective-last-success (if catch-up-missed?
                                 (p/last-successful-training-at)
                                 (.toInstant now))
        ^long delay (daily-training-delay-millis now training-time effective-last-success)]
    (t/log! :info ["Next automatic model training is scheduled for"
                   (.plusNanos now (* delay 1000000))])
    (.schedule executor
               ^Runnable (fn []
                           (run-automatic-training!)
                           (when (and (identical? executor @training-scheduler)
                                      (not (.isShutdown executor)))
                             ;; Re-scheduling reads the preferences again; if that throws (a malformed
                             ;; time or zone saved meanwhile) the chain must not end silently.
                             (try
                               (schedule-next-daily-training! executor false)
                               (catch Throwable e
                                 (t/log! {:level :error :error e} "Could not schedule the next automatic training from the preferences; retrying in one hour.")
                                 (try (.schedule executor
                                                 ^Runnable (fn [] (when (and (identical? executor @training-scheduler) (not (.isShutdown executor)))
                                                                    (try (schedule-next-daily-training! executor false)
                                                                         (catch Throwable inner (t/log! {:level :error :error inner} "Automatic training could not be rescheduled again; it stays off until the preferences are saved or Plauna restarts.")))))
                                                 (long 1) TimeUnit/HOURS)
                                      (catch Throwable _ nil))))))
               delay
               TimeUnit/MILLISECONDS)))

(defn start-training-scheduler!
  "Schedule model training at the configured local wall-clock time. A persisted successful-run
   timestamp makes a missed run catch up after restart. Calling this repeatedly is idempotent."
  ([]
   (locking training-scheduler
     (if-let [^ScheduledExecutorService existing @training-scheduler]
       existing
       (let [^ScheduledExecutorService executor
             (Executors/newSingleThreadScheduledExecutor (training-thread-factory))]
         (try
           (reset! training-scheduler executor)
           (schedule-next-daily-training! executor true)
           executor
           (catch Throwable e
             (reset! training-scheduler nil)
             (.shutdownNow executor)
             (throw e)))))))
  ;; Deterministic interval form retained for tests and diagnostics.
  ([initial-delay interval ^TimeUnit time-unit]
   (locking training-scheduler
     (if-let [^ScheduledExecutorService existing @training-scheduler]
       existing
       (let [^ScheduledExecutorService executor
             (Executors/newSingleThreadScheduledExecutor (training-thread-factory))]
         (try
           (.scheduleWithFixedDelay executor
                                    ^Runnable (fn [] (run-automatic-training!))
                                    (long initial-delay)
                                    (long interval)
                                    time-unit)
           (reset! training-scheduler executor)
           (t/log! :info ["Scheduled automatic model training every" interval time-unit])
           executor
           (catch Throwable e
             (.shutdownNow executor)
             (throw e))))))))

(defn stop-training-scheduler! []
  (locking training-scheduler
    (when-let [^ScheduledExecutorService executor @training-scheduler]
      ;; Clear the shared state before interrupting a running training task so a later startup can
      ;; always create a fresh scheduler.
      (reset! training-scheduler nil)
      (.shutdownNow executor)
      (t/log! :info "Stopped automatic model training."))))

(defn restart-training-scheduler! []
  (stop-training-scheduler!)
  (start-training-scheduler!))

;; ── Retention: moving the e-mails of a category to the trash after some days ──────────────────────

(def retention-interval-hours
  "How often the retention run looks for expired e-mails. Retention is counted in days, so a few runs
   a day are precise enough."
  6)

(def retention-initial-delay-minutes
  "Wait after startup so the run does not compete with connecting and the reconnect back-fill."
  10)

(defonce ^:private retention-running (atom false))

(defonce ^:private retention-scheduler (atom nil))

(defn- retention-thread-factory []
  (reify ThreadFactory
    (newThread [_ runnable]
      (doto (Thread. ^Runnable runnable "plauna-retention-scheduler")
        (.setDaemon true)))))

(defn retention-summary-text [{:keys [trashed protected missing elsewhere failed offline no-trash]}]
  (st/join ", " (remove nil? [(str (or trashed 0) " moved to the trash")
                             (when (pos? (or protected 0)) (str protected " kept because their flag was removed"))
                             (when (pos? (or elsewhere 0)) (str elsewhere " kept because they are not in their category's folder"))
                             (when (pos? (or missing 0)) (str missing " no longer in their folder"))
                             (when (pos? (or offline 0)) (str offline " waiting for a disconnected account"))
                             (when (pos? (or no-trash 0)) (str no-trash " kept because the account has no trash folder"))
                             (when (pos? (or failed 0)) (str failed " failed"))])))

(defn run-retention!
  "Move every e-mail whose category's retention has run out to the trash of its account. The age is
   counted from when the e-mail got its category (see db/expired-emails). When the category also sets a flag color, an e-mail whose flag the
   user removed in the mail client is kept: taking the flag away is the way to keep one. Plauna keeps
   the e-mail in its own database as a training example. Returns the summed counts, or nil when a run
   is already in progress."
  []
  (when (compare-and-set! retention-running false true)
    (try
      (let [now (quot (System/currentTimeMillis) 1000)
            expired (into []
                          (mapcat (fn [{:keys [id name retention_days flag_color]}]
                                    (when-let [days (app/clean-retention-days retention_days)]
                                      (let [protect? (some? (flags/valid-color flag_color))]
                                        (map #(assoc % :protect? protect? :category name)
                                             (db/expired-emails id (- now (* 86400 (long days)))))))))
                          (db/get-categories))
            ;; One dedicated connection per account for the e-mails of every category.
            totals (per-connection expired client/trash-expired-emails!)]
        ;; E-mails that are kept stay in the list and are counted on every run: only report real work.
        (if (some pos? (vals (select-keys totals [:trashed :failed :no-trash])))
          (t/log! :info ["Retention run:" (retention-summary-text totals)])
          (t/log! :debug "Retention run: no expired e-mails."))
        totals)
      (catch Throwable e
        (t/log! {:level :error :error e} "The retention run failed.")
        {:failed 1 :error true})
      (finally (reset! retention-running false)))))

(defn start-retention-scheduler!
  "Run run-retention! every retention-interval-hours on a daemon thread. Idempotent."
  []
  (locking retention-scheduler
    (or @retention-scheduler
        (let [^ScheduledExecutorService executor (Executors/newSingleThreadScheduledExecutor (retention-thread-factory))]
          (.scheduleWithFixedDelay executor ^Runnable (fn [] (run-retention!))
                                   (long (* 60 retention-initial-delay-minutes))
                                   (long (* 3600 retention-interval-hours))
                                   TimeUnit/SECONDS)
          (reset! retention-scheduler executor)))))

(defn stop-retention-scheduler!
  "Stop scheduling runs and wait up to 5 seconds for a running one: interrupting it between a move to
   the trash and recording it would leave the database pointing at the old folder. Not longer: docker
   stop kills the JVM after 10 seconds, and the rest of the shutdown (web server, IMAP, database pool)
   must still get its turn. An e-mail interrupted that way is found in the trash folder on the next run's
   search and counted as missing, nothing is lost."
  []
  (locking retention-scheduler
    (when-let [^ScheduledExecutorService executor @retention-scheduler]
      (reset! retention-scheduler nil)
      (.shutdown executor)
      (when-not (.awaitTermination executor 5 TimeUnit/SECONDS)
        (t/log! :warn "The retention run did not finish within 5 seconds; stopping it.")
        (.shutdownNow executor)))))

(defn start-manual-retention!
  "Run the retention now on a background thread and report the result as a toast. Returns false when
   a run is already in progress."
  []
  (if @retention-running
    false
    (do (async/thread
          (let [totals (run-retention!)]
            (cond (nil? totals) (add-to-messages (message :info :msg.another-clean-up-was-already-running))
                  (:error totals) (add-to-messages (message :alert :msg.the-clean-up-failed-please-see))
                  :else (add-to-messages {:type :success :content (str "Clean-up finished: " (retention-summary-text totals) ".")}))))
        true)))

(defn flag-existing-emails!
  "Give every stored e-mail of the category its flag color in the mail account, on a background
   thread, and report the result as a toast."
  [category]
  (async/thread
    (try
      (let [color (flags/valid-color (:flag_color category))
            ;; Only where the category is filed, like the clean-up - not in an archive or the sent folder.
            totals (per-connection (map #(assoc % :flag color :category (:name category)) (db/categorized-email-locations (:id category)))
                                   client/apply-flags!)]
        (add-to-messages {:type :success
                          :content (str "Flagged the e-mails of " (:name category) ": " (or (:flagged totals) 0) " flagged"
                                        (when (pos? (or (:missing totals) 0)) (str ", " (:missing totals) " no longer in their folder"))
                                        (when (pos? (or (:elsewhere totals) 0)) (str ", " (:elsewhere totals) " left alone outside the category's folder"))
                                        (when (pos? (or (:offline totals) 0)) (str ", " (:offline totals) " in a disconnected account"))
                                        (when (pos? (or (:failed totals) 0)) (str ", " (:failed totals) " failed"))
                                        ".")}))
      (catch Throwable e
        (t/log! {:level :error :error e} ["Flagging the e-mails of" (:name category) "failed"])
        (add-to-messages (message :alert :msg.flagging-failed-please-see-the-logs))))))

(defn mime-type-statistics []
  ;; Header MIME type represents one value per e-mail. Counting body parts here would count multipart
  ;; messages more than once and make this chart disagree with the total e-mail count.
  (db/query-db {:select [:headers.mime-type [[:count :headers.message-id] :count]]
                :from [:headers]
                :group-by [:headers.mime-type]
                :order-by [[[:count :headers.message-id] :desc]]}))

(defn language-statistics []
  ;; Start at headers and LEFT JOIN metadata so e-mails without detected language remain visible.
  (db/query-db {:select [:metadata.language [[:count :headers.message-id] :count]]
                :from [:headers]
                :left-join [:metadata [:= :headers.message-id :metadata.message-id]]
                :group-by [:metadata.language]
                :order-by [[[:count :headers.message-id] :desc]]}))

(defn category-statistics []
  ;; Resolve category names in SQL and retain uncategorized e-mails as the NULL group. This avoids
  ;; loading every category merely to translate ids after the aggregate query.
  (db/query-db {:select [:categories.name [[:count :headers.message-id] :count]]
                :from [:headers]
                :left-join [:metadata [:= :headers.message-id :metadata.message-id]
                            :categories [:= :metadata.category :categories.id]]
                :group-by [:categories.id :categories.name]
                :order-by [[[:count :headers.message-id] :desc]]}))

(defn enriched-email-by-message-id [id] (first (db/fetch-data {:entity :enriched-email :strict false} {:where [:= :message-id id]})))

(defn refetch-email-and-fill!
  "Re-read a message from the IMAP server and fill in data that is now parseable: the body parts (saved
   if missing; existing rows are left untouched) and the language when it was never detected. Returns a
   message map for the UI."
  [message-id]
  (if-let [refetched (client/refetch-message-by-id message-id)]
    (do
      ;; Save in two groups so refetch is idempotent AND can repair a row whose content was missing.
      ;; A body part's identity is (mime-type, filename, content-disposition) so that, e.g., the main
      ;; text/plain body and a text/plain "note.txt" attachment are treated as distinct rows.
      ;;  - Content-bearing parts (text): deduped by the DB's UNIQUE(mime_type, message_id, content) via
      ;;    INSERT OR IGNORE. A stored part of the same identity whose content differs - blank, or decoded
      ;;    wrongly before (umlauts lost) - is deleted first, so the repair replaces it rather than leaving
      ;;    a duplicate that would still be read first. Delete and insert run in one transaction.
      ;;  - Attachment parts (nil content): a UNIQUE constraint does not dedupe NULLs, so only add one when
      ;;    no row of the same identity exists yet.
      (let [blank?           (fn [p] (st/blank? (str (:content p))))
            part-key         (juxt :mime-type :filename :content-disposition)
            existing         (db/fetch-bodies-for [message-id])
            existing-keys    (set (map part-key existing))
            ;; Fitted like the stored rows (a huge part is kept cut), so an unchanged part compares equal.
            content-parts    (map db/fit-body-row (remove blank? (:body refetched)))
            content-keys     (set (map part-key content-parts))
            attachment-parts (remove (comp existing-keys part-key) (filter blank? (:body refetched)))
            fresh-contents   (group-by part-key content-parts)
            stale-ids        (->> existing
                                  (filter (fn [r] (and (contains? content-keys (part-key r))
                                                       (not (some #(= (:content r) (:content %)) (get fresh-contents (part-key r)))))))
                                  (keep :id))]
        (db/replace-bodies! stale-ids content-parts)
        ;; Anything re-fetched may change the features (body text, sender), so the cached classification
        ;; tokens must be derived again.
        (db/delete-training-tokens! message-id)
        (when (seq attachment-parts) (db/save-bodies attachment-parts)))
      (when (seq (:participants refetched))
        (db/save-contacts (:participants refetched))
        (db/save-communications (:participants refetched)))
      ;; The body text is available now, so fill in the language if it was never detected. We leave an
      ;; already-set language alone so a manual correction is never clobbered.
      (let [current-lang (:language (db/fetch-metadata message-id))]
        (when (or (nil? current-lang) (st/blank? (str current-lang)) (= "n/a" current-lang))
          (let [lang (analysis/language-result refetched)]
            (when (and (:code lang) (not= "n/a" (:code lang)))
              (db/update-metadata-language message-id (:code lang) (:confidence lang))))))
      (message :success :msg.re-fetched-the-email-from-the))
    (message :alert :msg.could-not-re-fetch-this-email)))

(defn- int-or-default
  "Parse an integer request parameter, falling back to the default on blank or non-numeric input.
   The page-size field is a free-form number input that submits size= when cleared, so a bare
   Integer/parseInt would reject the whole request over a fixable value."
  [default]
  (fn [value] (try (Integer/parseInt value) (catch NumberFormatException _ default))))

(def ^:private valid-log-levels
  ;; The values telemere's set-min-level! accepts; the preferences page offers a subset.
  #{"trace" "debug" "info" "warn" "error" "fatal" "report"})

(defn- valid-preference?
  "Allowlist + range validation for POST /admin/preferences: only known keys with usable values
   may be persisted. The select submits log levels as ':error'-style keyword strings."
  [key value]
  (case key
    (:language-detection-threshold :categorization-threshold)
    (try (<= 0.0 (Double/parseDouble value) 1.0) (catch Exception _ false))

    :client-health-check-interval
    (try (pos? (Long/parseLong value)) (catch Exception _ false))

    :category-folder-scan-interval
    (try (not (neg? (Long/parseLong value))) (catch Exception _ false))

    :automatic-training-time
    (try (LocalTime/parse value) true (catch Exception _ false))

    :time-zone
    (try (ZoneId/of value) true (catch Exception _ false))

    :log-level
    (contains? valid-log-levels (st/replace (str value) #"^:" ""))

    false))

;; TODO change name template
(def emails-template {:size {:default 20 :type-fn (int-or-default 20)}
                      :page {:default 1 :type-fn (int-or-default 1)}
                      :filter {:default "all" :type-fn identity}
                      :search-text {:default nil :type-fn identity}
                      :subject-values {:default nil :type-fn vectorize-capped}
                      :subject-values-exclude {:default nil :type-fn vectorize-capped}
                      :subject-values-none {:default nil :type-fn identity}
                      :from-keys {:default nil :type-fn vectorize-capped}
                      :from-keys-exclude {:default nil :type-fn vectorize-capped}
                      :from-keys-none {:default nil :type-fn identity}
                      :to-keys {:default nil :type-fn vectorize-capped}
                      :to-keys-exclude {:default nil :type-fn vectorize-capped}
                      :to-keys-none {:default nil :type-fn identity}
                      :category-ids {:default nil :type-fn vectorize-capped}
                      :category-ids-exclude {:default nil :type-fn vectorize-capped}
                      :category-ids-none {:default nil :type-fn identity}
                      :date-from {:default nil :type-fn identity}
                      :date-to {:default nil :type-fn identity}
                      :batch {:default nil :type-fn identity}
                      :suggest-category {:default nil :type-fn identity}
                      :suggest-by {:default nil :type-fn identity}
                      :suggest-min {:default nil :type-fn identity}
                      :filters {:default nil :type-fn identity}})

(defn template->request-parameters [template]
  (fn [rp] (reduce (fn [acc [k v]] (if (contains? rp k)
                                     (conj acc {k ((:type-fn v) (get rp k))})
                                     (conj acc {k (:default v)})))
                   {} template)))

(defn add-sanitized-text-to-enriched-email [email]
  {:header (:header email)
   :metadata (:metadata email)
   :participants (:participants email)
   :body (map (fn [body-part] (if (core-email/body-text-content? body-part)
                                (conj body-part {:sanitized-content (analysis/normalize-body-part body-part)})
                                body-part)) (:body email))})

(defn get-status-repl-server [] {:status (some? @repl-server) :port 7888})

(defn- without-provider-secrets [providers]
  (mapv #(dissoc % :client-secret :client_secret) providers))

(defn target-changed?
  "True when one of the pairs [submitted-value stored-value] differs - where a stored credential would
   be sent: a server, port, user, encryption setting, token URL ... Case, surrounding spaces and blank
   versus missing do not count as a change."
  [pairs]
  (let [normalized #(some-> % str st/trim st/lower-case not-empty)]
    (boolean (some (fn [[submitted stored]] (not= (normalized submitted) (normalized stored))) pairs))))

(defn secret-redirected?
  "True when a form keeps a STORED secret (its field is left blank and a secret is stored) while
   changing where that secret is sent (see target-changed?). A stolen session could otherwise send the
   stored IMAP, database or OAuth client secret to a server of its own; whoever changes the target must
   enter the secret again. Without a stored secret there is nothing to protect."
  [submitted-secret stored-secret pairs]
  (and (st/blank? submitted-secret)
       (not (st/blank? (str (or stored-secret ""))))
       (target-changed? pairs)))

(defn- forget-oauth-tokens!
  "Delete the stored OAuth tokens of the connections: their target changed, and the tokens must not be
   sent to the new one - the next connect asks for a new OAuth login."
  [connection-ids]
  (doseq [id connection-ids]
    (when (some? (db/get-oauth-tokens id))
      (db/delete-access-token id)
      (t/log! :info ["The server, user or OAuth provider of connection" id "changed: its OAuth tokens were deleted, a new OAuth login is needed."]))))

(def ^:private secret-redirected-response
  {:status 400 :headers {"Content-Type" "text/plain; charset=UTF-8"}
   :body "The server or user changed: please enter the password or secret again."})

(declare mariadb-port)

(defn- database-password-redirected?
  "secret-redirected? for the MariaDB form: only a stored password needs protecting."
  [{:keys [host port name user password]} existing]
  (secret-redirected? password (:password existing) [[host (:host existing)]
                                                     [(mariadb-port port) (:port existing)]
                                                     [name (:name existing)]
                                                     [user (:user existing)]]))

(defn- keep-existing-when-blank [submitted existing]
  (if (st/blank? submitted) existing submitted))

(defn- current-password-check
  "Check the administrator's current password for a credential change under the same brake as the
   login (see auth/claim-login-attempt!): a stolen session cookie must not allow unlimited guessing.
   Returns :ok, :wrong or {:wait seconds}."
  [password]
  (let [wait (long (auth/claim-login-attempt!))]
    (cond (pos? wait) {:wait (max 1 (long (Math/ceil (/ wait 1000.0))))}
          (auth/verify-web-password? password) (do (auth/record-login-success!) :ok)
          :else :wrong)))

(defn- password-check-alert
  "The message for a failed current-password-check."
  [check]
  {:type :alert
   :content (if (map? check)
              (str "Too many wrong passwords. Please wait " (:wait check) " second(s) and try again.")
              "Current password is incorrect.")})

(defn- mariadb-port
  "The port of the database form: MariaDB's default 3306 when the field is left blank."
  [port]
  (if (st/blank? port) 3306 (Integer/parseInt (st/trim port))))

(defn- connection-update-from-params [params existing]
  {:id (get params :id)
   :host (get params :host)
   :user (get params :user)
   :secret (keep-existing-when-blank (get params :secret) (:secret existing))
   :folder (get params :folder)
   :debug (= "true" (get params :debug))
   :security (get params :security)
   :port (when (seq (get params :port)) (Integer/parseInt (get params :port)))
   :check-ssl-certs (= "true" (get params :check-ssl-certs))
   :auth-type (get params :auth-type)
   :auth-provider (when (seq (get params :auth-provider))
                    (Integer/parseInt (get params :auth-provider)))})

(defn- auth-provider-update-from-params [params existing]
  ;; Form fields use SQL-style underscores while rows read from next.jdbc use kebab-case.
  (update params :client_secret keep-existing-when-blank (:client-secret existing)))

(defn- watched-folder
  "The folder the live monitor of connection id watches (INBOX when none is configured). It is read
   from the live connection - the settings it connected with - and not from the stored row, which a
   saved edit changes without reconnecting; stored-folder is only the answer for a connection that is
   not registered. The page label and the one-click re-scan both use this, so they always agree."
  [id stored-folder]
  (client/monitor-folder-name (if-let [connection-data (client/connection-data-from-id id)]
                                (get-in connection-data [:config :folder])
                                stored-folder)))

(defn connection-information [id]
  (let [conn (db/get-connection id)]
    ;; A stored password must never enter the template context, even though the password input is blank.
    ;; :monitored-folder is the folder the one-click "re-scan recent e-mails" form reads.
    (-> (merge conn (client/monitor->map (get @client/connections (:id conn))))
        (assoc :monitored-folder (watched-folder (:id conn) (:folder conn)))
        (dissoc :secret))))
(defn connection-folders [conn]
  (if (= true (:connected conn))
    (client/folders-in-store (:store (client/connection-data-from-id (:id conn))))
    []))

(defn- connect-control-response [context request id]
  (let [action (app/connect-to-client context id)]
    (cond
      (= :redirect (:result action))
      (let [csrf (.toString (UUID/randomUUID))]
        (-> (redirect (oauth/authorize-uri (:provider action) csrf))
            ;; Cookie-backed sessions belong to the browser. Store only the provider id, never its
            ;; client secret; the callback loads the provider from the database when it needs it.
            (assoc :session (-> (:session request)
                                (dissoc :provider)
                                (assoc :oauth-csrf csrf
                                       :connection-id id
                                       :provider-id (:id (:provider action)))))))

      (= :ok (:result action))
      (redirect-request request)

      (= :error (:result action))
      (redirect-request request (message :alert :msg.connection-failed-please-see-the-logs)))))

(defn- reconnect-control-response [context request id]
  (when-let [connection-data (client/connection-data-from-id id)]
    (client/disconnect connection-data))
  (connect-control-response context request id))

(def sqlite-delete-confirmation "Delete SQLite")

(def ^:private compared-tables
  "Tables whose row counts the database page shows for the old SQLite file next to MariaDB, so the
   user can see that the SQLite file is an outdated copy before deleting it."
  [["connections" "IMAP connections"] ["headers" "E-mails"] ["categories" "Categories"]])

(defn- format-bytes [bytes]
  (cond (>= bytes (* 1024 1024 1024)) (format "%.1f GB" (/ bytes 1024.0 1024.0 1024.0))
        (>= bytes (* 1024 1024)) (format "%.1f MB" (/ bytes 1024.0 1024.0))
        (>= bytes 1024) (format "%.1f KB" (/ bytes 1024.0))
        :else (str bytes " B")))

(defn- sqlite-file-exists? []
  (.exists (clojure.java.io/file (files/path-to-db-file))))

(defn- database-status []
  (if (= :mariadb (db/db-type))
    (let [cfg (db-cfg/load-config)]
      {:type "mariadb" :host (:host cfg) :port (:port cfg) :name (:name cfg) :user (:user cfg)})
    {:type "sqlite" :path (files/path-to-db-file)}))

(defn old-sqlite-info
  "What the database page shows about a SQLite file left behind after switching to MariaDB, or nil
   when SQLite is the active database or no file exists."
  []
  (when (and (= :mariadb (db/db-type)) (sqlite-file-exists?))
    (let [tables (map first compared-tables)
          sqlite-counts (db-mig/table-counts (db-mig/sqlite-ds) tables)
          mariadb-counts (db-mig/table-counts (db/ds) tables)]
      {:path (files/path-to-db-file)
       :size (format-bytes (files/database-files-size))
       :counts (mapv (fn [[table label]] {:label label
                                          :sqlite (or (get sqlite-counts table) "–")
                                          :mariadb (or (get mariadb-counts table) "–")})
                     compared-tables)})))

(defn delete-old-sqlite!
  "Delete the SQLite file left behind after the switch to MariaDB. Refused while SQLite is the active
   database and without the exact confirmation text. Returns the message for the database page."
  [confirmation]
  (cond
    (not= :mariadb (db/db-type))
    {:type "error" :text "SQLite is the active database; it cannot be deleted."}

    (not= sqlite-delete-confirmation (some-> confirmation st/trim))
    {:type "error" :text (str "Deletion not confirmed. Enter ‘" sqlite-delete-confirmation "’ exactly.")}

    (not (sqlite-file-exists?))
    {:type "error" :text "There is no SQLite file to delete."}

    :else
    (try (files/delete-database-file)
         (t/log! :info ["Deleted the old SQLite database" (files/path-to-db-file) "on request."])
         {:type "success" :text "The old SQLite database was deleted."}
         (catch Exception e
           (t/log! {:level :error :error e} "Could not delete the old SQLite database.")
           {:type "error" :text (str "Could not delete the SQLite file: " (.getMessage e))}))))

(defn- render-database-page [extra]
  (let [cfg (db-cfg/load-config)]
    (success-html-with-body
     (markup/page "admin-database.html"
                         (merge {:db-status (database-status)
                                 :saved-config (when (= :mariadb (:type cfg)) (dissoc cfg :password))
                                 :sqlite-exists (sqlite-file-exists?)
                                 :old-sqlite (old-sqlite-info)
                                 :sqlite-delete-confirmation sqlite-delete-confirmation
                                 :header "Database"
                                 :active-nav :system}
                                extra)))))

(defn empty-global-messages [] (take-messages!))

(defn parse-batch-size
  "Parse the optional batch-size field of the folder parse form. Blank, non-numeric or non-positive
   input means 'no limit' (process the whole folder), matching the behaviour before the field existed."
  [value]
  (let [parsed (try (Integer/parseInt (st/trim (str value))) (catch NumberFormatException _ nil))]
    (when (and parsed (pos? parsed)) parsed)))

(def max-since-days
  "The longest look-back the folder parse accepts. Anything beyond it reaches back past the oldest
   possible mail anyway; the cap keeps a typo from producing an absurd date."
  36500)

(defn parse-since-days
  "Parse the optional 'only e-mails of the last N days' field of the folder parse form. Blank,
   non-numeric or non-positive input means 'no time limit'; more than max-since-days is capped."
  [value]
  (some-> (parse-batch-size value) (min max-since-days)))

(defn batch-emails-url [batch-id] (str "/emails?batch=" batch-id))

(defn since-days-text
  "'the last day' / 'the last 7 days': how a look-back of n days reads in a message."
  [n]
  (if (= 1 n) "the last day" (str "the last " n " days")))

(defn folder-parse-summary-message
  "Turn the summary of a finished folder parse into a toast for the next page load. The remaining count
   tells the user whether another run of the same batch is needed; the link opens the e-mails this run
   saved so their detected categories can be checked right away."
  [{:keys [folder processed skipped skipped-elsewhere errors remaining batch-size batch-id aborted since-days recategorized]}]
  (let [remaining-text (cond
                         (some? aborted) (str " The run was " aborted ". " remaining " e-mail(s) were not examined - check the connection and run the parse again.")
                         (pos? remaining) (str " " remaining " older e-mail(s) were not examined because the batch of " batch-size " was full - run the parse again to continue.")
                         (and (some? batch-size) (some? since-days)) " All e-mails of that period have been examined; there is nothing left for another batch."
                         (some? batch-size) " The folder has been examined completely; there is nothing left for another batch."
                         :else "")]
    (cond-> {:type (if (or (pos? errors) (some? aborted)) :info :success)
             :content (str "Finished parsing " folder
                           (when since-days (str " (e-mails received in " (since-days-text since-days) ")"))
                           ": " processed " new e-mail(s) saved and categorized, "
                           (- skipped (or recategorized 0)) " already stored e-mail(s) skipped"
                           (when (pos? (- skipped (or recategorized 0)))
                             (str " (" (- skipped (or skipped-elsewhere 0) (or recategorized 0)) " saved by earlier runs of this folder and still waiting to be moved, "
                                  (or skipped-elsewhere 0) " duplicate(s) of e-mails stored from other folders)"))
                           (when (pos? (or recategorized 0))
                             (str ", " recategorized " already stored e-mail(s) given the chosen category"))
                           (when (pos? errors) (str ", " errors " could not be read (see the logs)"))
                           "." remaining-text)}
      (and (some? batch-id) (pos? processed)) (assoc :link (batch-emails-url batch-id)
                                                     :link-text "Review this batch"))))

(defn- run-as-parse-batch!
  "The lifecycle every background run over a folder shares: register a parse_batches row, start the
   run with an :on-complete that stores the summary and surfaces summary-message-fn's toast, and never
   leave the row 'running' when the start itself fails. start-fn receives [batch-id options] and
   returns whatever the caller wants back."
  [id folder batch-size options summary-message-fn start-fn]
  (let [batch-id (.toString (UUID/randomUUID))]
    (db/create-parse-batch! {:id batch-id :connection-id id :folder folder :batch-size batch-size})
    (try
      (start-fn batch-id (assoc options
                                :batch-id batch-id
                                :on-complete (fn [summary]
                                               ;; The row must never stay 'running' (the connection page would
                                               ;; reload itself every 10 s until a restart), and the toast must
                                               ;; reach the user even when storing the counts fails.
                                               (try (db/finish-parse-batch! batch-id summary)
                                                    (catch Exception e
                                                      (t/log! {:level :error :error e} ["Could not store the result of parse run" batch-id])
                                                      (try (db/abort-parse-batch! batch-id) (catch Exception _ nil))))
                                               (add-to-messages (summary-message-fn (assoc summary :batch-id batch-id))))))
      (catch Exception e
        ;; Opening the folder failed before any thread started: the run never examined anything, so it
        ;; is aborted rather than shown as finished with zero counts.
        (try (db/abort-parse-batch! batch-id) (catch Exception _ nil))
        (throw e)))))

(defn- start-folder-parse!
  "Register a parse run, start it and return the number of messages in the folder. The run's summary
   is stored when the background thread finishes and surfaced as a toast with a review link.
   Throws when the connection is not active or the folder cannot be opened - the route turns that
   into an alert."
  [context id folder {:keys [move? batch-size assigned-category since-days recategorize-known?]}]
  (let [conn-data (client/connection-data-from-id id)]
    (when (or (nil? conn-data) (not (client/connected? conn-data)))
      (throw (ex-info "The connection is not active. Connect first, then parse." {:type :inactive-connection})))
    (when (st/blank? folder)
      (throw (ex-info "No folder given." {:type :no-folder})))
    (run-as-parse-batch! id folder batch-size
                         {:move? move?
                          :batch-size batch-size
                          :since-days since-days
                          :assigned-category (:name assigned-category)
                          :assigned-category-id (:id assigned-category)
                          ;; Only meaningful with a category: without one there is nothing to assign.
                          :recategorize-known? (and (boolean recategorize-known?) (some? assigned-category))}
                         folder-parse-summary-message
                         (fn [_ options] (app/read-emails-from-folder conn-data folder options context)))))

(defn- start-parse-response
  "Start a folder parse and answer the controls request with its outcome: a toast on the connection page,
   an alert when the run could not be started (inactive connection, unknown folder, dropped store)."
  [context request id folder {:keys [move? batch-size since-days] :as parse-options}]
  (try
    (let [message-count (start-folder-parse! context id folder parse-options)]
      (redirect-request request {:type :success :content (str "Started parsing " folder " asynchronously. There are " message-count " emails in the folder. "
                                                              (when since-days (str "Only e-mails received in " (since-days-text since-days) " are read; e-mails already stored are skipped. "))
                                                              (if batch-size
                                                                (str "Batch size: " batch-size " new e-mails; already stored e-mails are skipped and do not count. ")
                                                                (if since-days "Batch size: unlimited. " "Batch size: unlimited (whole folder). "))
                                                              "Move folders after parsing: " move?)}))
    (catch clojure.lang.ExceptionInfo e
      (redirect-request request {:type :alert :content (.getMessage e)}))
    (catch Exception e
      ;; Typically the folder does not exist or the store dropped: an alert, not an error page.
      (t/log! {:level :warn :error e} ["Could not start parsing folder" folder "of connection" id])
      (redirect-request request {:type :alert :content (str "Could not start parsing " folder ": " (.getMessage e))}))))

(defn- parse-control-response [context request id]
  (let [params (:params request)]
    (start-parse-response context request id (:folder params)
                          {:move? (some? (:move params))
                           :batch-size (parse-batch-size (:batch-size params))
                           :since-days (parse-since-days (:since-days params))
                           :assigned-category (when-not (st/blank? (:assigned-category params)) (db/category-by-id (:assigned-category params)))
                           :recategorize-known? (some? (:recategorize-known params))})))

(defn- rescan-control-response
  "The one-click 're-scan recent e-mails': the watched folder of the live connection, restricted to the
   last N days. Unlike the general parse form the number of days is required - a blank field must not
   silently turn into a read of the whole folder."
  [context request id]
  (let [params (:params request)
        since-days (parse-since-days (:since-days params))]
    (if (nil? since-days)
      (redirect-request request (message :alert :msg.please-enter-how-many-days-to))
      (start-parse-response context request id
                            (watched-folder id nil)
                            {:move? (some? (:move params)) :since-days since-days}))))

(defn batch-move-url [batch-id]
  (str "/parse-batches/" (java.net.URLEncoder/encode (str batch-id) "UTF-8") "/move"))

(defn move-summary-message
  "Toast for a finished 'move to category folders' job over a set of e-mails described by what-text
   (e.g. \"the Old batch\" or \"the folder Newsletter_Alt\")."
  [what-text {:keys [moved total not-found failed uncategorized in-place]}]
  {:type (if (pos? (+ not-found failed)) :info :success)
   :content (str "Moved " moved " of " total " categorized e-mail(s) of " what-text " into their category folders."
                 (when (pos? (or in-place 0)) (str " " in-place " were in their category folder already."))
                 (when (pos? not-found) (str " " not-found " were not found on the server."))
                 (when (pos? failed) (str " " failed " could not be moved (see the logs)."))
                 (when (pos? uncategorized) (str " " uncategorized " e-mail(s) without a category were left in place.")))})

(defn batch-move-summary-message
  "Toast for a finished 'move batch to category folders' job."
  [{:keys [folder] :as summary}]
  (move-summary-message (str "the " folder " batch") summary))

(defn- start-move-job!
  "Run move-fn (context progress-fn -> summary) on a background thread under job-key in batch-moves,
   publishing progress and a toast on completion. Returns false when that job is already running."
  [context job-key what-text move-fn]
  (let [[before _] (swap-vals! batch-moves
                               (fn [moves]
                                 (if (= "running" (get-in moves [job-key :status]))
                                   moves
                                   (assoc moves job-key {:status "running" :moved 0 :in-place 0 :total 0 :not-found 0 :failed 0}))))]
    (if (= "running" (get-in before [job-key :status]))
      false
      (do (async/thread
            (let [summary (try
                            (move-fn context (fn [progress] (swap! batch-moves update job-key merge progress)))
                            (catch Throwable e
                              (t/log! {:level :error :error e} ["Moving" what-text "failed"])
                              {:total 0 :moved 0 :in-place 0 :not-found 0 :failed 0 :uncategorized 0 :error true}))]
              (swap! batch-moves update job-key merge summary {:status "finished"})
              (add-to-messages (if (:error summary)
                                 {:type :alert :content (str "Moving " what-text " failed. Please see the logs.")}
                                 (move-summary-message what-text summary)))))
          true))))

(defn start-batch-move!
  "Move the categorized e-mails of a parse batch into their category folders on a background thread.
   Returns false when a move for the batch is already running."
  [context {:keys [id folder connection-id]}]
  (start-move-job! context id (str "the " folder " batch")
                   (fn [context progress-fn] (app/move-parse-batch-emails! context id connection-id progress-fn))))

(defn folder-move-key [connection-id folder] (str "folder:" connection-id ":" folder))

(defn start-folder-move!
  "Move every stored, categorized e-mail still recorded in the folder into its category folder,
   whichever run stored it. Returns false when that move is already running."
  [context connection-id folder]
  (start-move-job! context (folder-move-key connection-id folder) (str "the folder " folder)
                   (fn [context progress-fn] (app/move-folder-emails! context connection-id folder progress-fn))))

(defn- category-destination-folders
  "The folders categorized e-mails are moved INTO; e-mails recorded there are already where they belong."
  []
  (into #{} (keep :destination_folder) (db/get-categories)))

(defn connection-folder-emails
  "Per recorded folder of a connection: how many stored e-mails are categorized (movable) and how many
   are not, with the state of a running or finished folder move. Category destination folders and the
   default 'Categories/...' tree are left out - their e-mails are already in place."
  [id]
  (let [destinations (category-destination-folders)
        moves @batch-moves]
    {:folder-emails (->> (db/folder-email-counts id)
                         (remove (fn [{:keys [folder]}]
                                   (or (contains? destinations folder)
                                       (re-matches #"(?i)categories[./\\].*" (str folder)))))
                         (mapv (fn [{:keys [folder] :as counts}]
                                 (assoc counts
                                        :move-url (str "/admin/connections/" id "/folders/move")
                                        :move (get moves (folder-move-key id folder))))))}))

(defn retry-summary-message
  "Toast for a finished retry of previously failed messages, with a review link when something was saved."
  [{:keys [folder processed skipped gone errors batch-id]}]
  (cond-> {:type (if (pos? errors) :info :success)
           :content (str "Retried the failed messages of " folder ": " processed " saved and categorized, "
                         skipped " turned out to be stored already, " gone " no longer exist on the server"
                         (if (pos? errors) (str ", " errors " failed again (see the logs and the failure list).") "."))}
    (and (some? batch-id) (pos? processed)) (assoc :link (batch-emails-url batch-id) :link-text "Review these e-mails")))

(defn start-failure-retry!
  "Retry every recorded failure of one folder as a new parse run, so the recovered e-mails can be
   reviewed as a batch. Returns the number of messages to retry, or nil when there is nothing to do."
  [context id folder move?]
  (let [failures (db/parse-failures-for-folder id folder)]
    (when (seq failures)
      (let [conn-data (client/connection-data-from-id id)]
        (run-as-parse-batch! id folder nil {:move? move?} retry-summary-message
                             (fn [_ options] (app/retry-failed-messages! conn-data folder failures options context)))))))

(def failure-list-limit
  "Failure rows rendered on the connection page; the per-folder counts stay exact."
  200)

(defn connection-parse-failures
  "The recorded read failures of a connection, grouped by folder for the connection page."
  [id]
  (let [rows (group-by :folder (db/parse-failures-for-connection id failure-list-limit))]
    {:parse-failures (mapv (fn [{:keys [folder count]}]
                             (let [shown (vec (get rows folder))]
                               {:folder folder
                                :count count
                                :shown (clojure.core/count shown)
                                :truncated? (< (clojure.core/count shown) count)
                                :retry-url (str "/admin/connections/" id "/failures/retry")
                                :failures shown}))
                           (db/parse-failure-counts id))}))

(defn connection-parse-batches
  "The recent folder parse runs of a connection for the connection page, plus whether one is running."
  [id]
  (let [moves @batch-moves
        batches (mapv (fn [batch] (assoc batch
                                         :emails-url (batch-emails-url (:id batch))
                                         :move-url (batch-move-url (:id batch))
                                         :move (get moves (:id batch))))
                      (db/parse-batches-for-connection id 10))]
    {:parse-batches batches
     :parse-running? (boolean (or (some #(or (= "running" (:status %)) (= "running" (get-in % [:move :status]))) batches)
                                  (some (fn [[job-key state]]
                                          (and (= "running" (:status state)) (st/starts-with? (str job-key) (str "folder:" id ":"))))
                                        moves)))}))

(defn connection-activity
  "Whether a folder parse, retry or batch move of the connection is running: the connection page polls
   this to reload itself only while something is going on (and never while the user is editing)."
  [id]
  {:running (:parse-running? (connection-parse-batches id))})

(defn recategorize-email-response
  "Handle an inline category change. A missing IMAP message is reported separately so the browser
   can ask whether only Plauna's stored metadata should be corrected. The follow-up force=true
   request deliberately skips the IMAP move and updates that metadata."
  [context params]
  (let [message-id (:message-id params)
        category (:category params)
        force? (contains? #{true "true"} (:force params))
        new-category-id (when (seq category) (Integer/parseInt category))]
    (cond
      ;; Deleted from Plauna while the list was open: there is nothing to update (and writing metadata
      ;; for it would violate its foreign key). 410, not 404 - 404 asks about a missing IMAP message.
      (not (db/email-exists? message-id))
      {:status 410 :headers {"Content-Type" "text/plain; charset=UTF-8"} :body "email-not-stored"}

      ;; Clearing the category never needs an IMAP move, but Plauna's flag goes with the category.
      (nil? new-category-id)
      (let [old-category-id (:category-id (db/email-category-state message-id))]
        (db/update-metadata-category message-id nil 1.0)
        (sync-flags! [{:message-id message-id :old-category-id old-category-id :new-category-id nil}])
        {:status 204})

      ;; The user explicitly confirmed that the message is gone. Only update Plauna's metadata.
      force?
      (do (db/update-metadata-category message-id new-category-id 1.0)
          {:status 204})

      :else
      (let [email-before (enriched-email-by-message-id message-id)
            new-category-name (get (first (filter #(= (:id %) new-category-id) (db/get-categories))) :name "")
            process (app/move-email-to-category email-before new-category-name context)]
        (case (:result process)
          :ok (do (db/update-metadata-category message-id new-category-id 1.0)
                  {:status 204})
          :not-found {:status 404
                      :headers {"Content-Type" "text/plain; charset=UTF-8"}
                      :body "email-not-found"}
          ;; A general move failure is intentionally not overridable through the missing-email
          ;; dialog: the category stays unchanged and the field receives its existing amber signal.
          {:status 200
           :headers {"Content-Type" "text/plain; charset=UTF-8"}
           :body "saved-not-moved"})))))

(defmacro result-with-messages [markup-call messages-var]
  `(let [messages# (first (swap-vals! ~messages-var (constantly [])))]
     (if (seq messages#)
       (~@markup-call messages#)
       ~markup-call)))

(defonce ^:private maintenance?
  ;; Set right before the SQLite → MariaDB copy: from then on nothing may be written to SQLite until the
  ;; restart that switches databases, or it would be missing on MariaDB.
  (atom false))

(defn in-maintenance?
  "True once Plauna paused for the SQLite → MariaDB copy (see enter-maintenance!)."
  []
  @maintenance?)

(defn running-writers
  "What is writing to the database right now, as short descriptions: incoming e-mails being saved,
   folder parses and retries, moves to category folders, a clean-up, a training run, a category folder
   scan. Empty when nothing is."
  []
  (cond-> []
    (pos? (long (app/messages-in-progress))) (conj "incoming e-mails being saved")
    (pos? (long (db/running-parse-batch-count))) (conj "a folder parse or retry")
    (some #(= "running" (:status %)) (vals @batch-moves)) (conj "a move to category folders")
    @retention-running (conj "a clean-up")
    (= :running (:status @training-progress)) (conj "a training run")
    (scoring-running?) (conj "a model scoring run")
    (client/category-folder-scan-running?) (conj "a category folder scan")))

(def maintenance-wait-millis
  "How long the pause before the copy waits for running work to finish."
  (* 120 1000))

(defn await-no-writers!
  "Wait until running-writers is empty, checking every poll-millis for at most wait-millis. Throws when
   something is still writing then: the copy must not start while SQLite still changes."
  ([] (await-no-writers! maintenance-wait-millis 500))
  ([wait-millis poll-millis]
   (let [deadline (+ (System/currentTimeMillis) (long wait-millis))]
     (loop []
       (let [busy (running-writers)]
         (cond
           (empty? busy) true
           (> (System/currentTimeMillis) deadline)
           (throw (ex-info (str "Plauna could not pause: still running after " (quot (long wait-millis) 1000) " s: " (st/join ", " busy) ". Nothing was copied.")
                           {:busy busy}))
           :else (do (Thread/sleep (long poll-millis)) (recur))))))))

(defn- enter-maintenance!
  "Pause everything that writes to SQLite, then confirm that nothing does any more:
   - no new work: state-changing requests and the OAuth callback are refused (wrap-maintenance), the
     connection retries skip (plauna.entry checks in-maintenance?), training and clean-up are no longer
     scheduled, the IMAP monitors and their health checks are stopped;
   - running work ends: folder parses, retries and moves stop at their next e-mail (app/stop-bulk-work!),
     and everything listed by running-writers is awaited (await-no-writers!);
   - a barrier is sent through the event bus (messaging/barrier!): it is confirmed only after every
     e-mail queued before it - by an mbox import or a language re-detection that ended just before -
     has been parsed, enriched and saved, its transaction committed. The bus job is held by the
     migration, so nothing new is queued after it.
   Throws when the work does not end in time: the copy is not started, and Plauna stays paused."
  []
  (reset! maintenance? true)
  (t/log! :warn "Entering maintenance for the SQLite → MariaDB copy: Plauna stays paused until it is restarted.")
  (app/stop-bulk-work!)
  (doseq [[label stop!] [["automatic training" stop-training-scheduler!]
                         ["retention" stop-retention-scheduler!]
                         ["IMAP connections" client/disconnect-all]
                         ["IMAP health checks" client/stop-health-checks!]]]
    (try (stop!) (catch Throwable e (t/log! {:level :warn :error e} ["Could not stop" label]))))
  (await-no-writers!)
  (let [{:keys [ok timeout failed]} (messaging/barrier! maintenance-wait-millis)]
    (cond
      ok nil
      timeout (throw (ex-info "Plauna could not pause: e-mails on the event bus were not saved in time. Nothing was copied." {}))
      :else (throw (ex-info (str (count failed) " e-mail(s) could not be saved before the copy, so they are not in SQLite and would not be on MariaDB either: "
                                 (st/join ", " (take 10 failed)) (when (> (count failed) 10) ", ...")
                                 ". The reason is in the log. Nothing was copied. Start the migration again to copy without them, and import them again on MariaDB afterwards.")
                            {:failed failed}))))
  (await-no-writers! 1000 200)
  (t/log! :info "Nothing writes to SQLite any more; copying."))

(declare public-path?)

(defn wrap-maintenance
  "While maintenance? is set, only the database page, the login and logout and the static files answer;
   every other request gets 503. An allowlist and not a list of writing routes: some pages write while
   they are shown (the language page records new languages, the OAuth callback stores tokens), and any
   such write would be missing on MariaDB."
  [handler]
  (fn [request]
    (let [uri (str (:uri request))]
      (if (and @maintenance?
               (not (st/starts-with? uri "/admin/database"))
               (not= "/logout" uri)
               (or (= "/oauth2/callback" uri) (not (public-path? uri))))
        {:status 503 :headers html-headers
         :body "<p>Plauna is paused for the switch to MariaDB. Save the database configuration and restart Plauna to continue.</p><p><a href=\"/admin/database\">Database settings</a></p>"}
        (handler request)))))

(defn make-routes [context]
  (comp/routes

   (comp/GET "/login" request
     ;; The configured login name is deliberately NOT pre-filled: it is one of the two secrets the login
     ;; asks for, and the page is the only one an anonymous visitor can see.
     (if (authenticated-session? (:session request))
       (redirect "/")
       (success-html-with-body (markup/login-page {}))))

   (comp/POST "/login" request
     (let [login-name (-> request :params :login-name)
           password   (-> request :params :password)]
       (cond
         (authenticated-session? (:session request))
         (redirect "/")

         :else
         ;; Brake after recent failures (see auth/claim-login-attempt!): an attempt that arrives before
         ;; the pause is over is refused without checking anything - no PBKDF2 work for the guesser, and
         ;; no request thread parked in a sleep. Claiming is atomic, so parallel attempts cannot all
         ;; slip through the moment a pause ends.
         (let [wait (long (auth/claim-login-attempt!))]
           (cond
             (pos? wait)
             (let [seconds (max 1 (long (Math/ceil (/ wait 1000.0))))]
               {:status 429
                :headers (assoc html-headers "Retry-After" (str seconds))
                :body (markup/login-page {:error (i18n/t :login.too-many [seconds])})})

             (auth/verify-web-credentials? login-name password)
             (do (auth/record-login-success!)
                 (-> (redirect "/")
                     (assoc :session (authenticate-session (:session request)))))

             :else
             (do (t/log! :warn "Rejected a web login attempt: wrong login name or password.")
                 (success-html-with-body
                  (markup/login-page {:error (i18n/t :login.invalid)}))))))))

   ;; The language switch of the sidebar and the phone's "More" menu.
   (comp/POST "/ui-language" request
     (let [value (get-in request [:params :ui-language])]
       (when (contains? p/ui-languages value)
         (p/update-preference :ui-language value))
       ;; The form names the page it was used on; the Referer is wrong for a page that answered a POST.
       (if-let [target (safe-redirect-path (get-in request [:params :redirect-url]) nil)]
         {:status 303 :headers {"Location" target}}
         (redirect-to-referer request))))

   (comp/GET "/logout" {}
     (-> (redirect "/login") (assoc :session nil)))

   (comp/GET "/admin/threads" {}
     ;; A full thread dump for diagnosing freezes. Also written to the log. Returned as plain text so
     ;; it can be copied directly from the browser.
     (let [dump (diagnostics/thread-dump-string)]
       (diagnostics/log-thread-dump! "requested via /admin/threads")
       {:status  200
        :headers {"Content-Type" "text/plain; charset=UTF-8"}
        :body    dump}))

   (comp/GET "/admin/database" {}
     (render-database-page {}))

   (comp/POST "/admin/database/config" request
     (let [{:keys [host port name user password]} (:params request)
           existing (db-cfg/load-config)
           ;; The form edits only the connection; settings made in db-config.edn by hand (:ssl-mode,
           ;; :startup-timeout-seconds) must survive a save.
           cfg (merge (or (db-cfg/read-file) {})
                      {:type     :mariadb
                       :host     host
                       :port     (mariadb-port port)
                       :name     name
                       :user     user
                       :password (if (st/blank? password) (:password existing "") password)})]
       (if (database-password-redirected? (:params request) existing)
         ;; Show what was entered (without the password), so only the password has to be added.
         (render-database-page {:saved-config (merge (dissoc existing :password) (select-keys (:params request) [:host :port :name :user]))
                                :message {:type "error" :text (:body secret-redirected-response)}})
         (do (db-cfg/save-config! cfg)
             (redirect "/admin/database?saved=1")))))

   (comp/POST "/admin/database/test" request
     (let [{:keys [host port name user password]} (:params request)
           existing (db-cfg/load-config)
           cfg {:host     host
                :port     (mariadb-port port)
                :name     name
                :user     user
                :password (if (st/blank? password) (:password existing "") password)
                :ssl-mode (:ssl-mode existing)}
           result (if (database-password-redirected? (:params request) existing)
                    {:ok false :error (:body secret-redirected-response)}
                    (db-mig/test-connection! cfg))]
       (render-database-page {:saved-config (dissoc existing :password)
                              :message (if (:ok result)
                                         {:type "success" :text "Connection successful!"}
                                         {:type "error"   :text (str "Connection failed: " (:error result))})})))

   (comp/POST "/admin/database/migrate" {}
     (let [busy (running-writers)
           result (cond
                    ;; Refused before anything is paused: a refused attempt changes nothing.
                    (seq busy)
                    {:ok false :error (str "Please wait until this has finished: " (st/join ", " busy) ".")}

                    ;; A pause that already began (an attempt that stopped before copying) keeps the bus
                    ;; job; the next attempt may go on with it.
                    (not (or (claim-event-bus-job! :migration)
                             (and @maintenance? (= :migration @event-bus-job))))
                    {:ok false :error (:content event-bus-busy-message)}

                    :else
                    (try (db-mig/migrate! enter-maintenance!)
                         ;; Only a pause that really began keeps the bus job (no import or re-detection
                         ;; before the restart); a refusal before it - MariaDB not empty or unreachable -
                         ;; gives it back.
                         (finally (when-not @maintenance? (release-event-bus-job!)))))]
       (render-database-page {:saved-config (dissoc (db-cfg/load-config) :password)
                              :message (cond
                                         (not (:ok result))
                                         {:type "error" :text (str "Migration failed: " (:error result)
                                                                   (when @maintenance? " Plauna is paused until it is restarted."))}
                                         (pos? (:skipped-total result 0))
                                         {:type "error" :text (str "Migration finished with " (:skipped-total result) " skipped row(s) — check the logs for details. Do not restart until the issue is resolved.")}
                                         :else
                                         {:type "success" :text "Migration complete with no losses. Plauna is paused so nothing is written to SQLite any more: save the configuration above and restart Plauna to switch to MariaDB."})
                              :migration-counts (when (:ok result)
                                                  (map (fn [[t v]] {:table t :inserted (:inserted v) :skipped (:skipped v) :total (:total v)}) (:counts result)))})))

   (comp/POST "/admin/database/delete-sqlite" request
     (render-database-page {:message (delete-old-sqlite! (get-in request [:params :confirmation]))}))

   (comp/GET "/admin/password" {}
     (success-html-with-body
      (markup/password-page {:env-managed (auth/password-from-env-var?)
                             :login-name  (auth/web-login-name)})))

   (comp/POST "/admin/login-name" request
     (let [{:keys [current-password login-name]} (:params request)
           check (current-password-check current-password)]
       (if-not (= :ok check)
         (redirect-request request (password-check-alert check))
         (try
           (auth/set-login-name! login-name)
           ;; Every other session was established with the old credentials: end them all, and renew
           ;; this one so the administrator stays logged in.
           (p/bump-session-generation!)
           (assoc (redirect-request request (message :success :msg.login-name-changed-successfully))
                  :session (authenticate-session (:session request)))
           (catch clojure.lang.ExceptionInfo e
             (redirect-request request {:type :alert :content (.getMessage e)}))))))

   (comp/POST "/admin/password" request
     (let [{:keys [current-password new-password confirm-password]} (:params request)
           check (current-password-check current-password)]
       (cond
         (not= :ok check)
         (redirect-request request (password-check-alert check))

         (not= new-password confirm-password)
         (redirect-request request (message :alert :msg.new-password-and-confirmation-do-not))

         (< (count (or new-password "")) 8)
         (redirect-request request (message :alert :msg.new-password-must-be-at-least))

         :else
         (do (auth/set-password! new-password)
             ;; A changed password must end every other session (a leaked cookie stays valid otherwise);
             ;; the administrator's own session is renewed.
             (p/bump-session-generation!)
             (assoc (redirect-request request (message :success :msg.password-changed-successfully-other-browsers-have))
                    :session (authenticate-session (:session request)))))))

   (comp/GET "/admin/mtls" request
     (let [messages (take-messages!)
           state    (assoc (auth/mtls-admin-state)
                           :current-certificate (auth/mtls-admin-certificate-state request))]
       (success-html-with-body
        (if (seq messages)
          (markup/mtls-page state messages)
          (markup/mtls-page state)))))

   (comp/POST "/admin/mtls" request
     ;; The password is checked first, under the login brake; a correct password followed by another
     ;; mistake in the form (a short secret, a malformed fingerprint) must not count as a wrong guess.
     (let [check (current-password-check (get-in request [:params :current-password]))]
       (if-not (= :ok check)
         (redirect-request request (password-check-alert check))
         (try
           (let [{:keys [revoked?]} (auth/save-mtls-settings-from-request! request)]
             (if revoked?
               ;; A removed certificate must not keep the sessions it opened; this one is renewed.
               (do (p/bump-session-generation!)
                   (assoc (redirect-request request (message :success :msg.mtls-login-configuration-saved-and-activated))
                          :session (authenticate-session (:session request))))
               (redirect-request request (message :success :msg.mtls-login-configuration-saved-and-activated-2))))
           (catch clojure.lang.ExceptionInfo e
             (redirect-request request {:type :alert :content (.getMessage e)}))))))

   (comp/GET "/" {} {:status  302
                     :headers {"Location" (if (db/any-emails?) "/emails" "/admin")}})

   (comp/GET "/admin" {}
     (success-html-with-body (result-with-messages (markup/administration {:repl (get-status-repl-server)}) global-messages)))

   (comp/POST "/emails/parse" request
     (let [upload (get-in request [:params :filename])
           ^java.io.File temp-file (when (map? upload) (:tempfile upload))]
       (if (or (nil? temp-file) (not (.exists temp-file)) (zero? (.length temp-file)))
         (redirect-request request (message :alert :msg.please-choose-a-non-empty-mbox))
         (if (start-mbox-import! temp-file (str (:filename upload)))
           (redirect-request request {:type :success :content (str "Importing the mbox file " (:filename upload) " in the background. Its e-mails appear in the list as they are processed; a message reports when the file has been read completely.")})
           (redirect-request request event-bus-busy-message)))))

   (comp/GET "/categories" {}
     (let [counts (into {} (map (juxt :id :email_count)) (db/categories-with-counts))
           categories (->> (db/get-categories)
                           (map #(assoc % :email_count (get counts (:id %) 0)))
                           (sort-by #(st/lower-case (str (:name %)))))]
       (success-html-with-body (result-with-messages (markup/categories-page categories) global-messages))))

   ;; The category overview moved out of the administration (docs/redesign.org).
   (comp/GET "/admin/categories" {} (redirect "/categories" 301))

   (comp/GET "/categories/:id" {route-params :route-params params :params}
     (if-let [category (some-> (:id route-params) parse-long db/category-by-id)]
       (success-html-with-body (result-with-messages (category-page context category params) global-messages))
       {:status 404 :headers html-headers :body "<p>This category does not exist.</p><p><a href=\"/categories\">Categories</a></p>"}))

   ;; The category page's suggestion counts of the methods that are not selected (loaded after the page).
   (comp/GET "/categories/:id/suggestion-counts" {route-params :route-params params :params}
     (if-let [category (some-> (:id route-params) parse-long db/category-by-id)]
       (let [{:keys [min-percent]} (app/suggestion-parameters {:suggest-category (str (:id category)) :suggest-min (:min params)})
             methods (filterv (set (st/split (str (:methods params)) #",")) app/suggestion-method-order)]
         {:status 200
          :headers {"Content-Type" "application/json; charset=UTF-8" "Cache-Control" "no-store"}
          :body (generate-string (app/suggestion-counts context (:id category) min-percent methods))})
       {:status 404 :headers {"Content-Type" "application/json; charset=UTF-8"} :body "{}"}))

   (comp/GET "/model" {}
     (success-html-with-body (result-with-messages (markup/model-page (model-page-data)) global-messages)))

   ;; The languages moved to the Model page (docs/redesign.org).
   (comp/GET "/admin/languages" {} (redirect "/model#languages" 301))

   (comp/GET "/admin/preferences" {}
     (let [language-datection-threshold (p/language-detection-threshold)
           categorization-threshold (p/categorization-threshold)
           client-health-check-interval (p/client-health-check-interval)
           category-folder-scan-interval (p/category-folder-scan-interval)
           automatic-training-time (p/automatic-training-time)
           time-zone (p/time-zone)
           categorization-model (p/categorization-model)
           log-level (p/log-level)]
       (success-html-with-body
        (result-with-messages
         (markup/preferences-page
          {:language-detection-threshold language-datection-threshold
           :categorization-threshold categorization-threshold
           :log-level log-level
           :client-health-check-interval client-health-check-interval
           :category-folder-scan-interval category-folder-scan-interval
           :automatic-training-time automatic-training-time
           :time-zone time-zone
           :categorization-model categorization-model
           :categorization-model-options analysis/supported-categorization-models})
         global-messages))))

   (comp/POST "/admin/preferences" request
     (let [prefs (dissoc (:params request) :redirect-url)
           invalid (remove (fn [[k v]] (valid-preference? k v)) prefs)]
       (if (seq invalid)
         ;; Save nothing when any value is invalid: a zero/negative health-check interval breaks
         ;; scheduleAtFixedRate for new connections, out-of-range thresholds break categorization,
         ;; and an unknown log level breaks logging.
         (redirect-request request {:type :alert :content (str "Invalid value(s) for: " (st/join ", " (map (comp name first) invalid)) ". Nothing was saved.")})
         (let [before-time (p/automatic-training-time)
               before-zone (p/time-zone)]
           (doseq [[k v] prefs]
             (p/update-preference k v))
           (t/set-min-level! (p/log-level))
           ;; Both fields are always submitted: restart only when one really changed, because a
           ;; restart interrupts a running training and may start a catch-up run.
           (when (or (not= (str before-time) (str (p/automatic-training-time)))
                     (not= (str before-zone) (str (p/time-zone))))
             (restart-training-scheduler!))
           (redirect-request request)))))

   (comp/POST "/admin/preferences/model" request
     ;; The switch may train a complete model set, so it runs like a manual training run - but only
     ;; after the request itself passed validation, so a typo never claims the training slot.
     (let [params (:params request)]
       (if-let [rejection (model-switch-validation params)]
         (redirect-request request rejection)
         (if (start-training-job! "Model switch" #(switch-categorization-model! params))
           (redirect (training-progress-url "/model") 303)
           (redirect-request request training-busy-message)))))

   (comp/POST "/admin/languages" {params :params :as request}
     (let [langs-to-use (if (vector? (:use params)) (:use params) [(:use params)])]
       (doseq [preference (mapv (fn [id language]
                                  {:id id :language language :use (some? (some #(= language %) langs-to-use))})
                                (vectorize (:id params))
                                (vectorize (:language params)))]
         (db/update-language-preference preference)))
     ;; Categorization caches the activated languages briefly; a change must apply at once.
     (analysis/clear-reference-cache!)
     (add-to-messages {:type :success :key :model.languages-saved})
     (redirect-to-referer request))

   (comp/POST "/admin/categories" {params :params}
     (if-let [problem (app/category-name-problem (:name params) (map :name (db/get-categories)))]
       (do (add-to-messages {:type :alert :content problem})
           (redirect "/categories" 303))
       (do (app/create-new-category! context (st/trim (:name params)) (:destination-folder params) (:color params)
                                     {:flag-color (:flag-color params) :retention-days (:retention-days params)})
           (analysis/clear-reference-cache!)
           (redirect "/categories" 303))))

   (comp/POST "/admin/categories/clean-up" {}
     (add-to-messages (if (start-manual-retention!)
                        (message :info :msg.moving-expired-e-mails-to-the)
                        (message :info :msg.a-clean-up-is-already-running)))
     (redirect "/categories" 303))

   (comp/POST "/admin/categories/:id/flag" {route-params :route-params :as request}
     (let [category (some-> (parse-long (str (:id route-params))) db/category-by-id)]
       (cond (nil? category)
             (add-to-messages (message :alert :msg.unknown-category))
             (nil? (flags/valid-color (:flag_color category)))
             (add-to-messages (message :alert :msg.this-category-has-no-flag-color))
             :else
             (do (flag-existing-emails! category)
                 (add-to-messages (message :info :msg.flagging-in-background (:name category)))))
       ;; Back to the page the form was on: a category's settings tab, or the overview.
       (redirect-to-referer request)))

   (comp/POST "/admin/categories/:id" {route-params :route-params params :params :as request}
     (let [id (parse-long (str (:id route-params)))
           old-category (some-> id db/category-by-id)]
       (if (nil? old-category)
         ;; A deleted category (a tab left open) or a made-up id: saving it would still create IMAP
         ;; folders under the posted name and report success.
         (do (add-to-messages (message :alert :msg.unknown-category))
             {:status 303 :headers {"Location" "/categories"}})
         (let [old-color (flags/valid-color (:flag_color old-category))
               ;; A form rendered before these fields existed (a tab left open across an update) must not
               ;; erase them.
               new-color (if (contains? params :flag-color) (:flag-color params) old-color)
               retention-days (if (contains? params :retention-days) (:retention-days params) (:retention_days old-category))]
           (app/update-category! context (:id route-params) (:name params) (:destination-folder params) (:color params)
                                 {:flag-color new-color :retention-days retention-days})
           ;; A category that stops flagging takes its flags away. A new color is not pushed onto existing
           ;; e-mails automatically: that would re-flag the ones the user unflagged to keep them.
           (when (and (some? old-color) (nil? (flags/valid-color new-color)))
             (let [flagged (mapv #(assoc % :flag :clear) (db/categorized-email-locations id))]
               (when (seq flagged)
                 (async/thread
                   (try (per-connection flagged client/apply-flags!)
                        (catch Throwable e (t/log! {:level :warn :error e} "Could not remove the flags of the category")))))))
           (analysis/clear-reference-cache!)
           (add-to-messages {:type :success :key :category.saved})
           (redirect-to-referer request)))))

   (comp/DELETE "/admin/categories/:id" {route-params :route-params}
     (let [id (Integer/parseInt (:id route-params))
           ;; The e-mails lose their category below: take Plauna's flag away with it.
           flagged (when (flags/valid-color (:flag_color (db/category-by-id id)))
                     (mapv #(assoc % :flag :clear) (db/categorized-email-locations id)))]
       (db/delete-category-by-id id)
       (when (seq flagged)
         (async/thread
           (try (per-connection flagged client/apply-flags!)
                (catch Throwable e (t/log! {:level :warn :error e} "Could not remove the flags of the deleted category"))))))
     (analysis/clear-reference-cache!)
     (redirect "/categories" 303))

   (comp/POST "/admin/database" {}
     (files/check-and-create-database-file)
     (db/create-db)
     {:status  301
      :headers {"Location" "/admin"}
      :body    (markup/administration {:repl (get-status-repl-server)})})

   (comp/GET "/statistics" {}
     (success-html-with-body
      (markup/statistics-overall (db/yearly-email-stats)
                                 (mime-type-statistics)
                                 (language-statistics)
                                 (category-statistics))))

   (comp/POST "/metadata/category" request
     ;; Recategorize a single email immediately (the e-mail list's category dropdown calls this on
     ;; change, so no "Batch Update" click is needed). A missing IMAP message returns 404 and lets the
     ;; UI offer a metadata-only update; other move failures remain non-overridable.
     (recategorize-email-response context (:params request)))

   (comp/POST "/metadata/language" request
     ;; Update a single email's detected language immediately (the language field calls this on change).
     ;; A blank value is treated as nil (clearing the language) to match the old batch-update behaviour
     ;; and avoid leaving an empty string that confuses enriched-only filters.
     (let [{:keys [message-id language]} (:params request)
           lang (when (seq language) language)]
       (cond
         ;; The language becomes part of training and model file names.
         (and (some? lang) (not= "n/a" lang) (not (files/valid-language-code? lang)))
         {:status 400 :headers {"Content-Type" "text/plain; charset=UTF-8"} :body "invalid-language"}

         (and (seq message-id) (db/email-exists? message-id))
         (do (db/update-metadata-language message-id lang 1.0)
             {:status 204})
         ;; metadata.message_id references headers: without the e-mail the insert would fail with a
         ;; constraint violation and a 500.
         :else
         {:status 404 :headers {"Content-Type" "text/plain; charset=UTF-8"} :body "email-not-found"})))

   (comp/POST "/metadata" request
     ;; The n/a category is submitted as a blank string; only enter the move branch for a real
     ;; category, otherwise just save (clearing the category never moves the email).
     (if (and (some? (:move (:params request))) (seq (:category (:params request))))
       (let [message-id (:message-id (:params request))
             email-before-update (enriched-email-by-message-id message-id)
             new-category-id (Integer/parseInt (:category (:params request)))
             new-category-name (get (first (filter #(= (:id %) new-category-id) (db/get-categories))) :name "")
             process (app/move-email-to-category email-before-update new-category-name context)]
         (case (:result process)
           :error (add-to-messages (:message process))
           ;; Like the e-mail list, never change only Plauna's category without the user's consent when
           ;; the mail server does not have the e-mail: the user would believe it was moved.
           :not-found (add-to-messages {:type :alert
                                        :content "The e-mail was not found on the mail server, so nothing was changed. To change only Plauna's category, save again without \"Move email after update\"."})
           ;; The move already gave the e-mail the flag of its new category.
           (save-metadata-form (:params request) false)))
       (save-metadata-form (:params request)))
     (redirect-to-referer request))

   (comp/POST "/training" request
     ;; Training runs in the background; the browser is sent to a progress page that polls
     ;; /training/status. A synchronous response used to exceed reverse proxy timeouts (504).
     (let [back (or (same-origin-referer request) "/emails")]
       (if (start-training-job! "Manual training" manual-training-job)
         (redirect (training-progress-url back) 303)
         ;; Another run holds the slot: say so instead of showing that run as if it were this request.
         (do (add-to-messages training-busy-message)
             (redirect back 303)))))

   (comp/GET "/training/progress" request
     (let [back (safe-redirect-path (get-in request [:params :back]) "/emails")]
       (success-html-with-body (markup/training-progress-page back))))

   (comp/GET "/training/status" _
     {:status 200
      :headers {"Content-Type" "application/json; charset=UTF-8" "Cache-Control" "no-store"}
      :body (generate-string (training-status))})

   (comp/GET "/emails" {params :params}
     (let [parse-fn (template->request-parameters emails-template)
           result (app/fetch-emails context (parse-fn params))
           batch-id (get-in result [:parameters :batch])
           options (cond-> (:optional result)
                     ;; Only for a batch that exists: the id comes from the URL, and a crafted one such as
                     ;; "../../admin/..." would turn the button into a POST to any route of Plauna.
                     (and (some? batch-id) (some? (db/parse-batch batch-id))) (assoc :batch-move-url (batch-move-url batch-id)))]
       (success-html-with-body (result-with-messages (markup/list-emails (:data result) (:parameters result) options) global-messages))))

   ;; "Score e-mails now" (Model page, category suggestions): computes the model scores in the background.
   (comp/POST "/model/score" request
     (when-not (start-scoring-job!)
       (add-to-messages {:type :info :key :scoring.busy}))
     (redirect-to-referer request))

   (comp/GET "/model/score/status" _
     {:status 200
      :headers {"Content-Type" "application/json; charset=UTF-8" "Cache-Control" "no-store"}
      :body (generate-string (scoring-status))})

   ;; The search box of a cut-off Subject/From/To checklist: the values that did not fit into the page.
   ;; Declared before /emails/:id, which would otherwise take "filter-options" for a message id.
   (comp/GET "/emails/filter-options" {params :params}
     (let [parse-fn (template->request-parameters emails-template)
           column (get {"subject" :subject "from" :from "to" :to} (:column params))]
       {:status 200
        :headers {"Content-Type" "application/json; charset=UTF-8" "Cache-Control" "no-store"}
        :body (generate-string (app/filter-options context (parse-fn params) column (:q params)))}))

   (comp/POST "/admin/connections/:id/folders/move" request
     (let [id (:id (:route-params request))
           folder (:folder (:params request))
           conn (client/connection-data-from-id id)]
       (cond
         (or (nil? conn) (not (client/connected? conn)))
         (redirect-request request (message :alert :msg.the-connection-is-not-active-connect))

         (st/blank? folder)
         (redirect-request request (message :alert :msg.no-folder-given))

         (not (start-folder-move! context id folder))
         (redirect-request request {:type :info :content (str "The e-mails of " folder " are already being moved.")})

         :else
         (redirect-request request {:type :success :content (str "Moving the categorized e-mails of " folder " into their category folders in the background.")}))))

   (comp/POST "/parse-batches/:id/move" request
     (let [batch-id (:id (:route-params request))
           batch (db/parse-batch batch-id)]
       (cond
         (nil? batch)
         (redirect-request request (message :alert :msg.unknown-parse-batch))

         (not (start-batch-move! context batch))
         (redirect-request request (message :info :msg.this-batch-is-already-being-moved))

         :else
         (redirect (str "/admin/connections/" (:connection-id batch)) 303))))

   (comp/GET "/emails/:id" [id]
     (if-let [decoded-id (decode-message-id id)]
       (if-let [email (enriched-email-by-message-id decoded-id)]
         (let [email-data (add-sanitized-text-to-enriched-email email)
               categories (conj (db/get-categories) {:id nil :name (i18n/t :category.none)})
               ;; What the current model computes for the e-mail, also below the threshold and also when it
               ;; already has a category.
               category-scores (try (some->> (analysis/category-scores-for-email email (-> email :metadata :language))
                                             (take 5)
                                             vec)
                                    (catch Exception e
                                      (t/log! {:level :warn :error e} ["Could not compute the category scores of" decoded-id])
                                      nil))]
           (success-html-with-body (result-with-messages (markup/list-email-contents email-data categories category-scores) global-messages)))
         (not-found-response "This e-mail is not stored (any more)."))
       (bad-request-response "Invalid e-mail id.")))

   (comp/DELETE "/emails/:id" [id]
     (if-let [decoded-id (decode-message-id id)]
       (do (db/delete-email-by-message-id decoded-id)
           {:status 200})
       (bad-request-response "Invalid e-mail id.")))

   (comp/POST "/emails/:id/refetch" [id :as request]
     (if-let [decoded-id (decode-message-id id)]
       (do (add-to-messages (refetch-email-and-fill! decoded-id))
           (redirect-to-referer request))
       (bad-request-response "Invalid e-mail id.")))

   (comp/GET "/admin/connections" _
     (let [messages (take-messages!)]
       (if (seq messages)
         (success-html-with-body (markup/connections-list (mapv (fn [conn] (merge conn (client/monitor->map (get @client/connections (:id conn))))) (db/get-connections)) messages))
         (success-html-with-body (markup/connections-list (mapv (fn [conn] (merge conn (client/monitor->map (get @client/connections (:id conn))))) (db/get-connections)))))))

   (comp/POST "/admin/connections" request
     (let [params (:params request)
           config {:host (get params :host) :user (get params :user) :secret (get params :secret) :folder (get params :folder) :debug (= "true" (get params :debug)) :security (get params :security) :port (when (seq (get params :port)) (Integer/parseInt (get params :port))) :check-ssl-certs (= "true" (get params :check-ssl-certs))
                   :auth-type (get params :auth-type) :auth-provider (when (seq (get params :auth-provider)) (Integer/parseInt (get params :auth-provider)))}
           id (client/id-from-config config)]
       (db/add-connection (merge config {:id id}))
       (redirect-request request)))

   (comp/DELETE "/admin/connections/:id" request
     (let [id (get (:params request) :id)]
       ;; Close and deregister the live connection first: deleting only the DB row would leave an
       ;; active monitor running with no way to reach it from the UI anymore.
       ;; Under the connect lock, so a login still in progress cannot register the deleted connection.
       (app/with-connection-lock id (fn []
                                      (client/remove-connection! id)
                                      (db/delete-connection id)))
       {:status 200}))

   (comp/GET "/admin/new-connection" []
     (let [providers (without-provider-secrets (db/get-auth-providers))]
       (success-html-with-body (markup/new-connection providers))))

   (comp/DELETE "/admin/auth-providers/:id" request
     (let [params (:params request)
           body (parse-string (slurp (:body request)) true)]
       (db/delete-auth-provider (get params :id))
       (if (empty? (:conn-id body))
         (redirect "/admin/new-connection" 303)
         (redirect (str "/admin/connections/" (:conn-id body)) 303))))

   (comp/POST "/admin/auth-providers" request
     (let [params (:params request)]
       ;; Only the known columns reach the INSERT: the form field names must not double as SQL identifiers.
       (db/add-auth-provider (auth-provider-fields params))
       (if (= "/admin/connections/" (:redirect-url params))
         (redirect-request (assoc-in request [:params :redirect-url] "/admin/new-connection"))
         (redirect-request request))))

   (comp/PUT "/admin/auth-providers/:id" request
     (let [params (:params request)
           existing (db/get-auth-provider (:id params))
           target-pairs [[(:token_url params) (:token-url existing)]
                         [(:auth_url params) (:auth-url existing)]
                         [(:client_id params) (:client-id existing)]]]
       (if (secret-redirected? (:client_secret params) (:client-secret existing) target-pairs)
         secret-redirected-response
         (do (db/update-auth-provider (-> (auth-provider-fields params)
                                          (auth-provider-update-from-params existing)
                                          (assoc :id (:id params))))
             ;; The refresh tokens of its connections would go to the new token URL.
             (when (target-changed? target-pairs)
               (forget-oauth-tokens! (->> (db/get-connections)
                                          (filter #(= (str (:auth-provider %)) (str (:id params))))
                                          (map :id))))
             ;; The handler used to end with the JDBC result vector, which Compojure cannot render (a 500
             ;; after a successful update).
             {:status 204}))))

   (comp/GET "/admin/connections/:id/activity" request
     {:status 200
      :headers {"Content-Type" "application/json; charset=UTF-8" "Cache-Control" "no-store"}
      :body (generate-string (connection-activity (:id (:route-params request))))})

   (comp/POST "/admin/connections/:id/failures/retry" request
     (let [id (:id (:route-params request))
           folder (:folder (:params request))
           move? (some? (:move (:params request)))
           conn (client/connection-data-from-id id)]
       (cond
         (or (nil? conn) (not (client/connected? conn)))
         (redirect-request request (message :alert :msg.the-connection-is-not-active-connect-2))

         (st/blank? folder)
         (redirect-request request (message :alert :msg.no-folder-given))

         :else
         (if-let [n (start-failure-retry! context id folder move?)]
           (redirect-request request {:type :success :content (str "Retrying " n " failed message(s) of " folder " in the background. Move after categorization: " move?)})
           (redirect-request request {:type :info :content (str "There are no recorded failures for " folder ".")})))))

   (comp/POST "/admin/connections/:id/failures/:failure-id/dismiss" request
     (let [failure-id (parse-long (str (:failure-id (:route-params request))))]
       (when failure-id (db/delete-parse-failure! failure-id))
       (redirect-request request)))

   (comp/GET "/admin/connections/:id" [id]
     (let [conn-info (merge (connection-information id) (connection-parse-batches id) (connection-parse-failures id) (connection-folder-emails id))
           providers (without-provider-secrets (db/get-auth-providers))
           categories (db/get-categories)
           messages (take-messages!)]
       (if (seq messages)
         (success-html-with-body (markup/connection (assoc conn-info :auth-providers providers) (connection-folders conn-info) messages categories))
         (success-html-with-body (markup/connection (assoc conn-info :auth-providers providers) (connection-folders conn-info) categories)))))

   (comp/PUT "/admin/connections/:id" request
     (let [params (:params request)
           existing (db/get-connection (:id params))
           ;; Everything that decides where the stored password or OAuth token goes, and whether it
           ;; travels encrypted. The OAuth provider only matters for the tokens: switching a connection
           ;; to OAuth does not send its stored password anywhere.
           server-pairs [[(:host params) (:host existing)]
                         [(:port params) (:port existing)]
                         [(:user params) (:user existing)]
                         [(:security params) (:security existing)]
                         [(= "true" (:check-ssl-certs params)) (true? (:check-ssl-certs existing))]]
           token-pairs (conj server-pairs [(:auth-provider params) (:auth-provider existing)])]
       (if (secret-redirected? (:secret params) (:secret existing) server-pairs)
         secret-redirected-response
         (do (db/update-connection (connection-update-from-params params existing))
             (when (target-changed? token-pairs)
               (forget-oauth-tokens! [(:id params)]))
             {:status 200}))))

   (comp/POST "/admin/connections/:id/controls" request
     (let [id (:id (:route-params request))
           operation (:operation (:params request))]
       (cond (= "reconnect" operation) (reconnect-control-response context request id)
             (= "disconnect" operation) (do (app/with-connection-lock
                                             id #(if-let [connection-data (client/connection-data-from-id id)]
                                                   (client/disconnect connection-data)
                                                   (t/log! :info ["Connection" id "is not registered; nothing to disconnect."])))
                                            (client/forget-disconnected-cache!)
                                            (redirect-request request))
             (= "connect" operation) (connect-control-response context request id)
             (= "parse" operation) (parse-control-response context request id)
             (= "rescan" operation) (rescan-control-response context request id)
             :else (redirect-request request {:type :alert :content (str "Unknown connection operation: " operation)}))))

   (comp/POST "/metadata/languages" request
     (let [queued (start-language-redetection!)]
       (redirect-request request (if (= :busy queued)
                                   event-bus-busy-message
                                   {:type :info :content (str "Re-detecting the language of " queued " e-mail(s) without one in the background.")}))))

   (comp/POST "/repl" request
     (let [operation (get-in request [:params :operation])]
       ;; locking, not swap!: starting and stopping a server are side effects that must run once, and
       ;; "stop" without a running server must not hand nil to stop-server.
       (locking repl-server
         (cond (= operation "start") (when (nil? @repl-server)
                                       (t/log! :info "Starting repl server")
                                       (reset! repl-server (nrepl/start-server :bind "127.0.0.1" :port 7888)))
               (= operation "stop") (when-let [server @repl-server]
                                      (t/log! :info "Stopping repl server")
                                      (nrepl/stop-server server)
                                      (reset! repl-server nil))
               :else (t/log! :error ["Unsupported operation" operation "at /repl"])))
       (redirect-request request)))

   (comp/GET "/oauth2/callback" request
     (let [params (:params request)
           session (:session request)
           state (:state params)
           expected-csrf (:oauth-csrf session)]
       (if (and (seq state) (seq expected-csrf) (= state expected-csrf))
         (try
           (let [provider (db/get-auth-provider (:provider-id session))
                 response (oauth/exchange-code-for-access-token provider (:code params))]
             (db/save-oauth-token (assoc response :connection-id (:connection-id session)))
             (app/connect-to-client context (:connection-id session))
             (assoc (redirect "/admin/connections")
                    :session (dissoc session :oauth-csrf :connection-id :provider-id :provider)))
           (catch Exception e
             (t/log! :error e)
             (assoc (redirect "/admin/connections")
                    :session (dissoc session :oauth-csrf :connection-id :provider-id :provider))))
         (do (t/log! :warn "OAuth callback rejected: missing or mismatched CSRF token.")
             (assoc (redirect "/admin/connections")
                    :session (dissoc session :oauth-csrf :connection-id :provider-id :provider))))))

   (route/resources "/")

   ;; Without this, an unknown URL makes the routes return nil, which the server answers with a 500.
   (route/not-found (not-found-response "There is no such page."))))

(defn upload-progress [_ bytes-read content-length item-count]
  (t/log! {:level :info
           :limit  [[1 5000]]
           :limit-by content-length
           :let [read-percent  (* 100 (float (/ bytes-read content-length)))]}
          ["Writing" item-count "files. Read" read-percent "% until now. Total length: " content-length]))

(defn- public-path?
  "Paths reachable without authentication: the login endpoint, the OAuth callback, and the static assets the login page needs."
  [^String uri]
  (or (= uri "/login")
      (= uri "/oauth2/callback")
      (.startsWith uri "/css/")
      (.startsWith uri "/favicon")
      (.startsWith uri "/android-chrome")
      (= uri "/plauna-banner.png")
      (= uri "/site.webmanifest")))

(def ^:private state-changing-methods #{:post :put :delete :patch})

(defn- request-host
  "The host the browser addressed: the reverse proxy's X-Forwarded-Host when present, else Host."
  [request]
  (or (get-in request [:headers "x-forwarded-host"]) (get-in request [:headers "host"])))

(defn cross-site-request?
  "True when a state-changing request was sent by another site. Browsers say so in Sec-Fetch-Site,
   which no proxy rewriting and no Referrer-Policy affects (with no-referrer a browser sends even a
   same-origin form POST with Origin: null). Without it (older browsers) the Origin header, or the
   Referer when Origin is missing, must name the host the browser addressed. Requests with none of
   these headers (non-browser clients) are not judged."
  [request]
  (let [headers (:headers request)
        site (some-> (get headers "sec-fetch-site") st/trim st/lower-case)
        origin (or (get headers "origin") (get headers "referer"))]
    (boolean
     (and (contains? state-changing-methods (:request-method request))
          (cond
            (some? site) (not (contains? #{"same-origin" "none"} site))
            (some? origin) (or (= "null" origin)
                               (not= (some-> (try (.getAuthority (java.net.URI. ^String origin)) (catch Exception _ nil))
                                             st/lower-case)
                                     (some-> (request-host request) st/lower-case)))
            :else false)))))

(def ^:private max-anonymous-body-bytes
  "The largest request body an anonymous request may send: the login form is a few hundred bytes."
  (* 64 1024))

(defn anonymous-body-refusal
  "A response refusing the body of a request that is not authenticated, or nil when it may pass. Public
   paths (the login page, static files, the OAuth callback) are reached without a login, and the
   parameter middleware behind authentication reads whatever body they carry - a multipart upload is
   even spooled to disk. So an anonymous request may not send a multipart body at all, nor a body
   larger than max-anonymous-body-bytes (or one of unknown length)."
  [request]
  (let [headers (:headers request)
        content-type (some-> (get headers "content-type") st/lower-case)
        length (some-> (get headers "content-length") st/trim parse-long)
        chunked? (some-> (get headers "transfer-encoding") st/lower-case (st/includes? "chunked"))]
    (cond
      (and content-type (st/starts-with? content-type "multipart/"))
      {:status 415 :headers html-headers :body "Uploads need a login."}

      (or (and length (> (long length) max-anonymous-body-bytes)) chunked?)
      {:status 413 :headers html-headers :body "Request body too large."})))

(defn wrap-authentication
  "Require a logged-in session or an allowlisted mTLS client certificate for every non-public
   request. A successful certificate authentication is promoted to the normal signed browser
   session; invalid or unconfigured proxy headers fall back to the password login.
   Sessions end when the password or login name changes (see authenticated-session?).
   A certificate-authenticated state-changing request from another site is refused: the browser
   presents the client certificate to the proxy for cross-site requests too, so the SameSite rule that
   protects cookie sessions against CSRF does not cover this login path."
  [handler]
  (fn [request]
    (let [already-authenticated?     (authenticated-session? (:session request))
          certificate-authenticated? (and (not already-authenticated?)
                                          (auth/mtls-request-authorized? request))
          authenticated-request      (if certificate-authenticated?
                                       (assoc request :session (authenticate-session (:session request)))
                                       request)]
      (cond
        ;; SameSite=Lax keeps the session cookie off requests from other *sites*, but every service under
        ;; the same registrable domain counts as the same site. The browser also presents the client
        ;; certificate to the proxy for cross-site requests. So a state-changing request that another
        ;; origin sent is refused, whichever way it would be authenticated.
        (cross-site-request? request)
        (do (t/log! :warn ["Refused a cross-site" (name (:request-method request)) "to" (:uri request)])
            {:status 403 :headers html-headers :body "Cross-site request refused."})

        (and (not already-authenticated?) (not certificate-authenticated?) (anonymous-body-refusal request))
        (anonymous-body-refusal request)

        (or (public-path? (:uri request))
            already-authenticated?
            certificate-authenticated?)
        (let [response (handler authenticated-request)]
          ;; Ring only persists a modified request session when the response carries :session.
          ;; Respect explicit route decisions such as /logout setting it to nil.
          (if (and certificate-authenticated?
                   response
                   (not (contains? response :session)))
            (assoc response :session (:session authenticated-request))
            response))

        ;; A page script's request (fetch) would follow the redirect and get the login page as its
        ;; answer; tell it plainly instead, and base.html sends the browser to the login page.
        (let [mode (get-in request [:headers "sec-fetch-mode"])]
          (or (some? (get-in request [:headers "x-plauna-fetch"]))
              (and (some? mode) (not= "navigate" mode))))
        {:status 401 :headers {"Content-Type" "text/plain; charset=UTF-8"} :body "login-required"}

        :else
        (redirect "/login")))))

(defn wrap-exception-handling
  "Catch exceptions escaping a handler so bad input or unexpected failures return a clean response
   instead of a 500 with a leaked stack trace. A NumberFormatException (e.g. a non-numeric port or
   category param) becomes a 400 rather than crashing the request handler."
  [handler]
  (fn [request]
    (try
      (handler request)
      (catch NumberFormatException e
        (t/log! {:level :warn :error e} ["Non-numeric value in a numeric parameter for" (:uri request)])
        {:status 400 :headers html-headers :body "Invalid request: a numeric field received a non-numeric value."})
      (catch Throwable e
        (t/log! {:level :error :error e} ["Unhandled error while processing" (:uri request)])
        {:status 500 :headers html-headers :body "An unexpected error occurred."}))))

(defn wrap-static-asset-revalidation
  "Keep static assets cacheable, but require browsers to check for a newer copy after a deployment.
   Asset URLs are intentionally stable, so long-lived freshness would otherwise leave users with old
   CSS or JavaScript until they clear their browser cache manually."
  [handler]
  (fn [request]
    (let [response (handler request)
          uri (:uri request)]
      (if (and response
               (or (.startsWith ^String uri "/css/")
                   (.startsWith ^String uri "/js/")
                   (.startsWith ^String uri "/favicon")
                   (.startsWith ^String uri "/android-chrome")
                   (= uri "/plauna-banner.png")
                   (= uri "/site.webmanifest")))
        (assoc-in response [:headers "Cache-Control"] "no-cache")
        response))))

(defn- https-request?
  "True when the browser reached Plauna over HTTPS: directly, or through a reverse proxy that says so in
   X-Forwarded-Proto."
  [request]
  (or (= :https (:scheme request))
      (= "https" (some-> (get-in request [:headers "x-forwarded-proto"]) (st/split #",") first st/trim st/lower-case))))

(defn wrap-secure-session-cookie
  "Mark the cookies of a response Secure when the request came in over HTTPS, so the browser never
   sends the session over plain HTTP. Requests over plain HTTP (e.g. directly in the LAN) keep working
   because they get the cookie without the attribute."
  [handler]
  (fn [request]
    (let [response (handler request)
          cookies (get-in response [:headers "Set-Cookie"])]
      (if (and cookies (https-request? request))
        (assoc-in response [:headers "Set-Cookie"]
                  (mapv (fn [^String cookie] (if (re-find #"(?i);\s*Secure(;|$)" cookie) cookie (str cookie ";Secure")))
                        (if (string? cookies) [cookies] cookies)))
        response))))

(defn wrap-language
  "Bind the interface language of the request (see plauna.i18n): the setting, or the browser's
   Accept-Language for \"auto\". Pages are rendered inside the handler, so they all see it."
  [handler]
  (fn [request]
    (binding [i18n/*lang* (i18n/request-language (p/ui-language) (get-in request [:headers "accept-language"]))]
      (handler request))))

(defn- shell-data
  "What the sidebar shows on every page."
  []
  {:nav-categories (db/categories-with-counts)})

(defn app [context]
  (reset! markup/shell-provider shell-data)
  (-> (fn [req] ((make-routes context) req))
                        wrap-language
                        wrap-keyword-params
                        (wrap-multipart-params {:progress-fn upload-progress})
                        wrap-params
                        ;; Authentication runs BEFORE the body is parsed: an anonymous request must not be
                        ;; able to make the server read (and spool to disk) an arbitrarily large multipart
                        ;; upload. Authentication only needs the session and the headers.
                        wrap-maintenance
                        wrap-authentication
                        wrap-exception-handling
                        wrap-static-asset-revalidation
                        (wrap-session {:store (cookie-store {:key (.getBytes ^String (settings/session-key) "UTF-8")})
                                       ;; HttpOnly keeps the cookie out of JS; SameSite=Lax blocks forged
                                       ;; cross-site POSTs (CSRF) while still allowing the OAuth provider's
                                       ;; top-level redirect back to /oauth2/callback to carry the session.
                                       :cookie-attrs {:http-only true :same-site :lax}})
                        wrap-secure-session-cookie))

(defn get-random-port []
  (with-open [socket (ServerSocket. 0)]
    (.getLocalPort socket)))

(defn start-server [context]
  (let [config (:config context)
        port (if (some? (-> (:server config) :port)) (-> (:server config) :port) (get-random-port))
        new-app (app context)
        executor (Executors/newVirtualThreadPerTaskExecutor)]
    (t/log! :info [(str "Starting server: http://0.0.0.0:" port)])
    (try
      (let [jetty (jetty/run-jetty (fn [req] (new-app req))
                                   {:port        port
                                    :join?       false
                                    :configurator (fn [^Server s]
                                                    (.setVirtualThreadsExecutor
                                                     ^QueuedThreadPool (.getThreadPool s)
                                                     executor))})]
        (reset! server {:jetty jetty :executor executor}))
      (catch Exception e
        (.close ^ExecutorService executor)
        (throw e)))))

(defn stop-server []
  (if-some [{^Server jetty :jetty ^ExecutorService executor :executor} @server]
    (do
      (let [port (.getPort (.getURI jetty))]
        (t/log! {:level :info} ["Stopping server on port" port]))
      (try
        (.stop jetty)
        (finally
          (.close executor)))
      (reset! server nil)
      nil)
    (do (t/log! :info "No server running.")
        nil)))
