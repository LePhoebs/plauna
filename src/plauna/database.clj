(ns plauna.database
  (:require [clojure.java.io]
            [clojure.walk :refer [postwalk]]
            [plauna.files :as files]
            [plauna.util.async :as async-utils]
            [plauna.core.email :as core.email]
            [clojure.string :as string]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :refer [as-unqualified-lower-maps as-unqualified-kebab-maps]]
            [honey.sql :as honey]
            [honey.sql.helpers :refer [insert-into upsert values on-conflict do-update-set]]
            [next.jdbc.sql.builder :as builder]
            [plauna.util.page :as page]
            [plauna.util.sql :as sql]
            [taoensso.telemere :as t]
            [plauna.interfaces :as int]
            [clojure.core.async :as async])
  (:import (org.flywaydb.core Flyway)
           (com.zaxxer.hikari HikariDataSource HikariConfig)))

(set! *warn-on-reflection* true)

;; ── Datasource management ──────────────────────────────────────────────────────

(defonce ^:private active-db-type (atom :sqlite))

(defonce ^:private mariadb-pool (atom nil))

(defn sqlite-url []
  ;; foreign_keys is a connection-scoped pragma, and every jdbc/execute! may open a fresh
  ;; connection — putting it in the URL is the only way it reliably applies everywhere.
  (str "jdbc:sqlite:" (files/path-to-db-file)
       "?busy_timeout=30000&journal_mode=WAL&synchronous=NORMAL&foreign_keys=true"))

(def mariadb-retry-initial-ms
  "Pause after the first failed attempt to reach MariaDB at startup. It doubles after every further
   failure up to mariadb-retry-max-ms."
  2000)

(def mariadb-retry-max-ms 30000)

(defn retry-until-connected
  "Call attempt! until it returns a value instead of throwing, pausing between failures with an
   exponential back-off. timeout-seconds nil (or not positive) waits indefinitely; otherwise the last
   failure is rethrown once the time is up. sleep! and now-ms are injectable for tests."
  ([attempt! timeout-seconds] (retry-until-connected attempt! timeout-seconds #(Thread/sleep (long %)) #(System/currentTimeMillis)))
  ([attempt! timeout-seconds sleep! now-ms]
   (let [deadline (when (and timeout-seconds (pos? timeout-seconds))
                    (+ (now-ms) (* 1000 (long timeout-seconds))))]
     (loop [attempt 1
            pause mariadb-retry-initial-ms]
       (let [result (try {:value (attempt!)}
                         (catch Exception e {:error e}))]
         (if (contains? result :value)
           (:value result)
           (let [^Exception e (:error result)]
             (if (and deadline (>= (+ (now-ms) pause) deadline))
               (throw e)
               (do (t/log! :warn ["MariaDB is not reachable yet (attempt" attempt "):" (.getMessage e)
                                  "- retrying in" (quot pause 1000) "s. Plauna does not start before the database is available."])
                   (sleep! pause)
                   (recur (inc attempt) (min mariadb-retry-max-ms (* 2 pause))))))))))))

(def default-mariadb-ssl-mode
  "The MariaDB driver's sslMode when none is configured. Driver 4 defaults to verify-full (TLS with a
   certificate a trusted CA signed), which fails against a server without TLS or with a self-signed
   certificate - the usual MariaDB container next to Plauna. 'disable' keeps the behaviour of driver 3;
   set PLAUNA_DB_SSL_MODE to trust, verify-ca or verify-full to encrypt the connection."
  "disable")

(defn mariadb-jdbc-url [{:keys [host port name ssl-mode]}]
  (str "jdbc:mariadb://" host ":" port "/" name
       "?sslMode=" (if (string/blank? ssl-mode) default-mariadb-ssl-mode (string/trim ssl-mode))))

(defn- open-mariadb-pool ^HikariDataSource [{:keys [user password] :as cfg}]
  (let [hcfg (HikariConfig.)]
    (.setJdbcUrl hcfg (mariadb-jdbc-url cfg))
    (.setUsername hcfg user)
    (.setPassword hcfg (or password ""))
    (.setMaximumPoolSize hcfg 10)
    (.setMinimumIdle hcfg 2)
    (.setConnectionTimeout hcfg 30000)
    (.setValidationTimeout hcfg 5000)
    ;; The constructor opens a first connection and throws when the server cannot be reached.
    (HikariDataSource. hcfg)))

(defn setup-db!
  "Initialise the datasource for the given db-config map.
   Must be called once at startup before any other DB function.
   A configured MariaDB that cannot be reached yet (e.g. its container is still starting after a
   reboot) is retried until it answers - or until :startup-timeout-seconds, after which this throws.
   It never falls back to SQLite: that would silently serve an old, unrelated data set."
  [{:keys [type host port name startup-timeout-seconds] :as cfg}]
  (reset! active-db-type type)
  (when (= :mariadb type)
    (reset! mariadb-pool (retry-until-connected #(open-mariadb-pool cfg) startup-timeout-seconds))
    (t/log! :info ["Connected to MariaDB at" (str host ":" port "/" name)]))
  cfg)

(defn ds
  "Return the active datasource (HikariCP pool for MariaDB, JDBC URL for SQLite)."
  []
  (or @mariadb-pool
      (jdbc/get-datasource {:jdbcUrl (sqlite-url)})))

(defn db-type [] @active-db-type)

(defn flyway []
  (let [location (if (= :mariadb @active-db-type)
                   "classpath:db/migration/mariadb"
                   "classpath:db/migration/sqlite")]
    (.load (doto (Flyway/configure)
             (.dataSource (ds))
             (.locations ^"[Ljava.lang.String;" (into-array String [location]))))))

(def my-addresses (atom #{}))

(defn create-db []
  ;; SQLite pragmas (foreign_keys, WAL, busy_timeout) are set in the JDBC URL — see sqlite-url —
  ;; because they are connection-scoped and would not carry over to later connections if set here.
  (.migrate ^Flyway (flyway)))

(def builder-function {:builder-fn as-unqualified-lower-maps})

(def builder-function-kebab {:builder-fn as-unqualified-kebab-maps})

;; ── Dialect helpers ────────────────────────────────────────────────────────────

(defn- mariadb? [] (= :mariadb @active-db-type))

(defn fulltext-min-token-length
  "Read MariaDB's effective InnoDB FULLTEXT token length. The value is server-configurable, so the
   query planner must not assume the default of three characters. Returns nil for SQLite."
  []
  (when (mariadb?)
    (try
      (some-> (jdbc/execute-one! (ds) ["SELECT @@innodb_ft_min_token_size AS min_token_length"]
                                 builder-function-kebab)
              :min-token-length
              long)
      (catch Exception e
        (t/log! {:level :warn :error e}
                "Could not read innodb_ft_min_token_size; using MariaDB's default value 3.")
        3))))

(defn- sql-now
  "Current time as a Unix epoch integer, dialect-appropriate."
  []
  (if (mariadb?) [:unix_timestamp] [:strftime "%s" "now"]))

(defn- in-clause-placeholders [n] (string/join ", " (repeat n "?")))

;; Insert Clauses

(defn insert->insert-ignore [insert-query]
  (let [sql (first insert-query)]
    (if (mariadb?)
      ;; ON DUPLICATE KEY UPDATE col = col is a true no-op: row is unchanged, no other
      ;; constraints are bypassed, and non-duplicate errors still propagate.
      (let [first-col (second (re-find #"(?i)INSERT INTO \w+ \((\w+)" sql))]
        (conj (rest insert-query)
              (str sql " ON DUPLICATE KEY UPDATE " first-col " = " first-col)))
      (conj (rest insert-query) (string/replace sql #"INSERT" "INSERT OR IGNORE")))))

(defn insert->insert-update [insert-query]
  ;; Only used for preferences on SQLite; MariaDB callers use ON DUPLICATE KEY UPDATE directly.
  (let [insert-part (first insert-query)]
    (conj (rest insert-query) (string/replace insert-part #"INSERT" "INSERT OR REPLACE"))))

(defn insert->metadata-upsert
  "SQLite upsert for metadata that updates ONLY the supplied columns on conflict, preserving the rest of
   the row (e.g. folder, category_modified, language_modified). INSERT OR REPLACE must NOT be used here:
   it deletes and re-inserts the row, wiping any columns not present in the INSERT."
  [insert-query]
  (conj (rest insert-query)
        (str (first insert-query)
             " ON CONFLICT(message_id) DO UPDATE SET"
             " language = excluded.language,"
             " language_confidence = excluded.language_confidence,"
             " category = excluded.category,"
             " category_confidence = excluded.category_confidence,"
             ;; Never blank a known account with a NULL from a source that does not know it (mbox import,
             ;; re-enrichment).
             " connection_id = COALESCE(excluded.connection_id, metadata.connection_id)")))

(defn email-exists? [message-id]
  (some? (jdbc/execute-one! (ds)
           (honey/format {:select [1] :from :headers :where [:= :message_id message-id]})
           builder-function)))

(defn save-headers
  ([headers] (save-headers (ds) headers))
  ([conn headers]
   (jdbc/execute! conn
                  (->>
                   (builder/for-insert-multi
                    :headers
                    [:mime_type :subject :message_id :date :in_reply_to]
                    (mapv (juxt :mime-type :subject :message-id :date :in-reply-to) headers) {})
                   (insert->insert-ignore))
                  {:batch true})))

(defn save-bodies
  ([bodies] (save-bodies (ds) bodies))
  ([conn bodies]
   (jdbc/execute! conn
                  (->>
                   (builder/for-insert-multi
                    :bodies
                    [:content :mime_type :charset :transfer_encoding :message_id :filename :content_disposition]
                    (mapv (juxt :content :mime-type :charset :transfer-encoding :message-id :filename :content-disposition) bodies) {})
                   (insert->insert-ignore))
                  {:batch true})))

(defn delete-bodies-by-ids
  "Delete the body rows with the given primary-key ids. Used by refetch to drop a specific stale (empty)
   body row before saving its repaired, content-bearing version. Deleting by id keeps the match exact
   (and dialect-agnostic) instead of relying on null-safe multi-column SQL."
  [ids]
  (when (seq ids)
    (jdbc/execute! (ds)
                   (into [(str "DELETE FROM bodies WHERE id IN (" (string/join ", " (repeat (count ids) "?")) ")")]
                         ids))))

(defn save-contacts
  ([contacts] (save-contacts (ds) contacts))
  ([conn contacts]
   (jdbc/execute! conn
                  (->>
                   (builder/for-insert-multi
                    :contacts
                    [:contact_key :name :address]
                    (mapv (juxt :contact-key :name :address) contacts) {})
                   (insert->insert-ignore))
                  {:batch true})))

(defn save-communications
  ([contacts] (save-communications (ds) contacts))
  ([conn contacts]
   (jdbc/execute! conn
                  (->> (builder/for-insert-multi
                        :communications
                        [:message_id :contact_key :type]
                        (mapv (juxt :message-id :contact-key (comp name :type)) contacts) {})
                       (insert->insert-ignore))
                  {:batch true})))

(defn update-metadata-batch
  ([metadata] (update-metadata-batch (ds) metadata))
  ([conn metadata]
   (when (seq metadata)
     (if (mariadb?)
       ;; One multi-row upsert per 200 e-mails instead of one round trip per e-mail.
       (doseq [chunk (partition-all 200 metadata)]
         (jdbc/execute! conn
           (into [(str "INSERT INTO metadata (message_id, language, language_confidence, category, category_confidence, connection_id) VALUES "
                       (string/join ", " (repeat (count chunk) "(?, ?, ?, ?, ?, ?)"))
                       " ON DUPLICATE KEY UPDATE language = VALUES(language), language_confidence = VALUES(language_confidence), category = VALUES(category), category_confidence = VALUES(category_confidence), connection_id = COALESCE(VALUES(connection_id), connection_id)")]
                 (mapcat (juxt :message-id :language :language-confidence :category-id :category-confidence :connection-id) chunk))))
       (jdbc/execute! conn
                      (->> (builder/for-insert-multi
                            :metadata
                            [:message_id :language :language_confidence :category :category_confidence :connection_id]
                            (mapv (juxt :message-id :language :language-confidence :category-id :category-confidence :connection-id) metadata) {})
                           (insert->metadata-upsert))
                      {:batch true})))))

(def batch-size 500)

(defn empty-buffer [] {:headers [] :bodies [] :participants [] :metadata [] :count 0})

(defn buffer-size
  "How many e-mails a buffer holds (headers plus metadata-only entries)."
  [buffer]
  (or (:count buffer) (count (:headers buffer))))

(defn add-to-buffer
  "Queue one e-mail for the next batch save. With metadata-only? (the re-enrichment of an e-mail that
   is already stored, see the :metadata-only event option) only its metadata is queued: the header is
   there already, and re-saving the bodies would add a duplicate row for every attachment because the
   UNIQUE constraint on bodies does not cover their NULL content."
  ([e-mail buffer] (add-to-buffer e-mail buffer false))
  ([e-mail buffer metadata-only?]
   (let [with-metadata (fn [b] (if (some? (:metadata e-mail)) (update b :metadata conj (:metadata e-mail)) b))]
     (-> (if metadata-only?
           buffer
           (-> buffer
               (update :headers conj (:header e-mail))
               (update :bodies into (:body e-mail))
               (update :participants into (:participants e-mail))))
         with-metadata
         (update :count (fnil inc 0))))))

(defn- bodyless-message-ids [headers bodies]
  (let [covered (set (map :message-id bodies))]
    (vec (remove covered (map :message-id headers)))))

(def body-insert-max-rows 100)

(def body-insert-max-chars
  "Body content per INSERT statement. MariaDB rejects a statement larger than max_allowed_packet
   (16 MB by default) - one oversized multi-row INSERT would then lose the complete batch of up to 500
   e-mails. 3 M characters stay below that even when every character needs four UTF-8 bytes."
  (* 3 1024 1024))

(defn chunk-bodies
  "Split body rows into groups that fit one INSERT (see body-insert-max-chars); order is preserved."
  [bodies]
  (loop [remaining (seq bodies) current [] size 0 chunks []]
    (if-let [body (first remaining)]
      (let [len (count (str (:content body)))]
        (if (and (seq current)
                 (or (>= (count current) body-insert-max-rows)
                     (> (+ size len) body-insert-max-chars)))
          (recur remaining [] 0 (conj chunks current))
          (recur (rest remaining) (conj current body) (+ size len) chunks)))
      (if (seq current) (conj chunks current) chunks))))

(def ^:private participant-insert-max-rows 500)

(defn save-emails-in-buffer
  "Persist everything in the buffer in ONE transaction: either the whole batch commits or nothing
   does. Without this, a failure after save-headers would leave partial emails whose headers make
   email-exists? true, so backfills would permanently skip re-saving their missing bodies/metadata.
   A failure propagates to the caller; the rollback leaves no header behind, so the same messages
   are picked up again by a later backfill or re-parse."
  [buffer]
  (jdbc/with-transaction [tx (ds)]
    (when (seq (:headers buffer))
      (save-headers tx (:headers buffer))
      (let [missing (bodyless-message-ids (:headers buffer) (:bodies buffer))]
        (when (seq missing)
          (t/log! :warn ["No body parts parsed for message ID(s):" missing]))))
    (doseq [chunk (chunk-bodies (:bodies buffer))]
      (save-bodies tx chunk))
    (doseq [chunk (partition-all participant-insert-max-rows (:participants buffer))]
      (save-contacts tx chunk)
      (save-communications tx chunk))
    (when (seq (:metadata buffer)) (update-metadata-batch tx (:metadata buffer)))))

(defn buffer->single-email-buffers
  "Split a batch buffer into one buffer per e-mail (by message id), so a failed batch can be retried
   e-mail by e-mail. Metadata-only entries (re-enrichment) have no header and become buffers of their own."
  [buffer]
  (let [bodies (group-by :message-id (:bodies buffer))
        participants (group-by :message-id (:participants buffer))
        metadata (group-by :message-id (:metadata buffer))
        header-ids (set (map :message-id (:headers buffer)))]
    (concat
     (map (fn [header]
            (let [id (:message-id header)]
              {:headers [header] :bodies (vec (get bodies id)) :participants (vec (get participants id))
               :metadata (vec (get metadata id)) :count 1}))
          (:headers buffer))
     (map (fn [m] {:headers [] :bodies [] :participants [] :metadata [m] :count 1})
          (remove #(contains? header-ids (:message-id %)) (:metadata buffer))))))

(defn- save-buffer-logging-errors!
  "Keep the database event loop alive when a batch save fails: the transaction has already rolled
   back, so nothing partial was written. A failed batch of several e-mails is retried one e-mail at a
   time, so a single row the database rejects (a column limit on MariaDB, a constraint) costs that one
   e-mail and not the other 499 of an import."
  [buffer]
  (try
    (save-emails-in-buffer buffer)
    (catch Exception e
      (if (<= (buffer-size buffer) 1)
        (t/log! {:level :error :error e}
                ["Saving e-mail" (or (some :message-id (:headers buffer)) (some :message-id (:metadata buffer)))
                 "failed and was rolled back. It was not written and will be picked up by a later backfill or re-parse:" (.getMessage e)])
        (do (t/log! {:level :warn :error e}
                    ["Saving a batch of" (buffer-size buffer) "e-mails failed and was rolled back; retrying them one by one so a single rejected e-mail does not discard the others:" (.getMessage e)])
            (doseq [single (buffer->single-email-buffers buffer)]
              (save-buffer-logging-errors! single)))))))

(defn- metadata-only-event?
  "True for a re-enrichment of an e-mail that is already stored (see plauna.server's language
   re-detection): only the metadata row is written."
  [event]
  (true? (get-in event [:options :metadata-only])))

(defn database-event-loop [publisher]
  (let [parsed-chan (async/chan)
        enriched-chan (async/chan)
        local-chan (async/merge [parsed-chan enriched-chan] batch-size)]
    ;; Subscribe the publisher to the merge *inputs*. Subscribing both topics to local-chan (the
    ;; merge output) creates two producers for a channel core.async itself owns and can close it
    ;; before the publisher has delivered its final queued event during shutdown.
    (async/sub publisher :parsed-email parsed-chan)
    (async/sub publisher :enriched-email enriched-chan)
    ;; JDBC is blocking work and must not occupy core.async's shared go-dispatch pool. A dedicated
    ;; thread also lets shutdown block until the final partial buffer has been flushed.
    (async/thread
      (try
        (loop [event (async/<!! local-chan)
               buffer (empty-buffer)]
          (cond
            (nil? event)
            (do
              (when (pos? (buffer-size buffer))
                (t/log! :info ["Database event loop is stopping; flushing" (buffer-size buffer) "buffered email(s)."])
                (save-buffer-logging-errors! buffer))
              :stopped)

            (= :timed-out event)
            (do (when (pos? (buffer-size buffer))
                  (t/log! :debug ["Received timeout. Saving everything in the buffer."])
                  (save-buffer-logging-errors! buffer))
                (recur (async/<!! local-chan) (empty-buffer)))

            (>= (buffer-size buffer) batch-size)
            (do (t/log! :debug ["DB buffer full. Emptying"])
                (save-buffer-logging-errors! (add-to-buffer (:payload event) buffer (metadata-only-event? event)))
                (recur (async/<!! local-chan) (empty-buffer)))

            :else
            (recur (async-utils/fetch-or-timeout!! local-chan 1000)
                   (add-to-buffer (:payload event) buffer (metadata-only-event? event)))))
        (catch Throwable t
          (t/log! {:level :error :error t} "The database event loop crashed. Its subscriptions are released so the supervisor's restart can take over.")
          :crashed)
        (finally
          ;; A dead loop must not stay subscribed: the publisher would block on its unread channels and
          ;; stall every producer of :parsed-email / :enriched-email events.
          (async/unsub publisher :parsed-email parsed-chan)
          (async/unsub publisher :enriched-email enriched-chan)
          (async/close! parsed-chan)
          (async/close! enriched-chan))))))

(defn honey-time-buckets []
  (if (mariadb?)
    {:yearly  [:year [:from_unixtime :date]]
     :monthly [:date_format [:from_unixtime :date] "%Y-%m"]}
    {:yearly  [:strftime "%Y" [:datetime :date "unixepoch"]]
     :monthly [:strftime "%Y-%m" [:datetime :date "unixepoch"]]}))

(defn update-metadata [message_id category cat-confidence language lang-confidence]
  (if (mariadb?)
    (jdbc/execute! (ds)
      ["INSERT INTO metadata (message_id, category, category_modified, category_confidence, language, language_modified, language_confidence) VALUES (?, ?, UNIX_TIMESTAMP(), ?, ?, UNIX_TIMESTAMP(), ?) ON DUPLICATE KEY UPDATE category = VALUES(category), category_modified = VALUES(category_modified), category_confidence = VALUES(category_confidence), language = VALUES(language), language_modified = VALUES(language_modified), language_confidence = VALUES(language_confidence)"
       message_id category cat-confidence language lang-confidence])
    (jdbc/execute! (ds) (-> (insert-into :metadata)
                            (values [{:message_id          message_id
                                      :category            category
                                      :category_modified   (sql-now)
                                      :category_confidence cat-confidence
                                      :language            language
                                      :language_modified   (sql-now)
                                      :language_confidence lang-confidence}])
                            (upsert (-> (on-conflict :message_id)
                                        (do-update-set :category
                                                       :category_modified
                                                       :category_confidence
                                                       :language
                                                       :language_modified
                                                       :language_confidence)))
                            (honey/format)))))

(defn update-metadata-category [message_id category confidence]
  (if (mariadb?)
    (jdbc/execute! (ds)
      ["INSERT INTO metadata (message_id, category, category_modified, category_confidence) VALUES (?, ?, UNIX_TIMESTAMP(), ?) ON DUPLICATE KEY UPDATE category = VALUES(category), category_modified = VALUES(category_modified), category_confidence = VALUES(category_confidence)"
       message_id category confidence])
    (jdbc/execute! (ds) (-> (insert-into :metadata)
                            (values [{:message_id message_id :category category :category_modified (sql-now) :category_confidence confidence}])
                            (upsert (-> (on-conflict :message_id)
                                        (do-update-set :category :category_modified :category_confidence)))
                            (honey/format)))))

(defn update-metadata-language [message_id language confidence]
  (if (mariadb?)
    (jdbc/execute! (ds)
      ["INSERT INTO metadata (message_id, language, language_modified, language_confidence) VALUES (?, ?, UNIX_TIMESTAMP(), ?) ON DUPLICATE KEY UPDATE language = VALUES(language), language_modified = VALUES(language_modified), language_confidence = VALUES(language_confidence)"
       message_id language confidence])
    (jdbc/execute! (ds) (-> (insert-into :metadata)
                            (values [{:message_id message_id :language language :language_modified (sql-now) :language_confidence confidence}])
                            (upsert (-> (on-conflict :message_id)
                                        (do-update-set :language :language_modified :language_confidence)))
                            (honey/format)))))

(defn update-email-folder
  "Record the IMAP folder a message currently lives in, so later moves can find it regardless of category-to-folder mapping changes."
  [message-id folder]
  (jdbc/execute! (ds) (honey/format {:update :metadata
                                     :set    {:folder folder}
                                     :where  [:= :message-id message-id]})))

(defn update-email-connection
  "Record the IMAP account a message is known to live in."
  [message-id connection-id]
  (when (some? connection-id)
    (jdbc/execute! (ds) (honey/format {:update :metadata
                                       :set    {:connection-id connection-id}
                                       :where  [:= :message-id message-id]}))))

(defn email-connection-id
  "Return the recorded IMAP account id for a message, or nil if none was recorded."
  [message-id]
  (:connection_id (jdbc/execute-one! (ds) (honey/format {:select [:connection-id]
                                                         :from   :metadata
                                                         :where  [:= :message-id message-id]}) builder-function)))

(defn email-location
  "Where a stored e-mail is known to live: {:folder ... :connection-id ...}, or nil when unknown."
  [message-id]
  (let [row (jdbc/execute-one! (ds) (honey/format {:select [:folder :connection-id]
                                                   :from   :metadata
                                                   :where  [:= :message-id message-id]}) builder-function-kebab)]
    (when row {:folder (:folder row) :connection-id (:connection-id row)})))

(defn categorized-message-ids-in-folder
  "The Message-IDs of stored, categorized e-mails whose recorded location is the folder of the connection."
  [connection-id folder]
  (mapv :message-id
        (jdbc/execute! (ds) (honey/format {:select [:metadata.message-id]
                                           :from [:metadata]
                                           :join [:headers [:= :headers.message-id :metadata.message-id]]
                                           :where [:and [:= :metadata.connection-id connection-id]
                                                   [:= :metadata.folder folder]
                                                   [:<> :metadata.category nil]]
                                           :order-by [[:headers.date :desc]]})
                       builder-function-kebab)))

(defn folder-email-counts
  "Per recorded folder of a connection: how many stored e-mails are categorized and how many are not.
   [{:folder ... :categorized n :uncategorized m} ...]"
  [connection-id]
  (mapv (fn [row] {:folder (:folder row) :categorized (long (or (:categorized row) 0)) :uncategorized (long (or (:uncategorized row) 0))})
        (jdbc/execute! (ds) (honey/format {:select [:folder
                                                    [[:sum [:case [:<> :category nil] [:inline 1] :else [:inline 0]]] :categorized]
                                                    [[:sum [:case [:= :category nil] [:inline 1] :else [:inline 0]]] :uncategorized]]
                                           :from [:metadata]
                                           :where [:and [:= :connection-id connection-id] [:<> :folder nil]]
                                           :group-by [:folder]
                                           :order-by [[:folder :asc]]})
                       builder-function-kebab)))

(defn email-folder
  "Return the recorded IMAP folder for a message, or nil if none was recorded."
  [message-id]
  (:folder (jdbc/execute-one! (ds) (honey/format {:select [:folder]
                                                  :from   :metadata
                                                  :where  [:= :message-id message-id]}) builder-function)))

(defn- epoch-seconds [] (quot (System/currentTimeMillis) 1000))

(defn create-parse-batch!
  "Register a manual folder parse run before it starts, so its e-mails can be collected under one id."
  [{:keys [id connection-id folder batch-size]}]
  (jdbc/execute! (ds) (honey/format {:insert-into :parse-batches
                                     :values [{:id id :connection-id connection-id :folder folder
                                               :batch-size batch-size :status "running"
                                               :started-at (epoch-seconds)}]})))

(defn record-parse-batch-email!
  "Remember that message-id was newly saved by the batch. Re-recording the same pair is a no-op."
  [batch-id message-id]
  (jdbc/execute! (ds) (insert->insert-ignore
                       (honey/format {:insert-into :parse-batch-emails
                                      :values [{:batch-id batch-id :message-id message-id}]}))))

(defn finish-parse-batch!
  "Store the final counts of a run and mark it finished."
  [id {:keys [processed skipped skipped-elsewhere errors remaining]}]
  (jdbc/execute! (ds) (honey/format {:update :parse-batches
                                     :set {:status "finished" :finished-at (epoch-seconds)
                                           :processed (or processed 0) :skipped (or skipped 0)
                                           :skipped-elsewhere (or skipped-elsewhere 0)
                                           :errors (or errors 0) :remaining (or remaining 0)}
                                     :where [:= :id id]})))

(defn abort-running-parse-batches!
  "Called at startup: a run that was still 'running' when Plauna stopped can never finish, so mark it
   aborted instead of showing it as running forever."
  []
  (jdbc/execute! (ds) (honey/format {:update :parse-batches
                                     :set {:status "aborted" :finished-at (epoch-seconds)}
                                     :where [:= :status "running"]})))

(defn abort-parse-batch!
  "Mark one run as aborted: it never got to process anything (e.g. its folder could not be opened)."
  [id]
  (jdbc/execute! (ds) (honey/format {:update :parse-batches
                                     :set {:status "aborted" :finished-at (epoch-seconds)}
                                     :where [:= :id id]})))

(defn any-emails?
  "True when at least one e-mail is stored. Cheap, unlike aggregating the mailbox by year."
  []
  (some? (jdbc/execute-one! (ds) ["SELECT 1 FROM headers LIMIT 1"])))

(defn content-match-message-ids
  "The message ids whose body content contains search-text literally: the SQLite body search, resolved
   once so the e-mail list's statements can inline the ids instead of each rescanning every body."
  [search-text]
  (mapv :message-id
        (jdbc/execute! (ds) (honey/format {:select-distinct [:message-id]
                                           :from [:bodies]
                                           :where (sql/like-contains :content search-text)})
                       builder-function-kebab)))

(defn parse-batch [id]
  (jdbc/execute-one! (ds) (honey/format {:select [:*] :from [:parse-batches] :where [:= :id id]}) builder-function-kebab))

(defn parse-batches-for-connection
  "The most recent runs of a connection, newest first."
  [connection-id limit]
  (jdbc/execute! (ds) (honey/format {:select [:*] :from [:parse-batches]
                                     :where [:= :connection-id connection-id]
                                     :order-by [[:started-at :desc] [:id :desc]]
                                     :limit limit}) builder-function-kebab))

(defn fetch-training-tokens-for
  "Cached classification tokens as {message-id tokens} for the given ids, only from rows produced by
   the given tokenizer version (older rows are ignored and get recomputed)."
  [message-ids version]
  (if (empty? message-ids)
    {}
    (into {} (map (juxt :message_id :tokens))
          (jdbc/execute! (ds)
                         (into [(str "SELECT message_id, tokens FROM training_tokens WHERE version = ? AND message_id IN (" (in-clause-placeholders (count message-ids)) ")") version]
                               message-ids)
                         builder-function))))

(defn save-training-tokens!
  "Cache classification tokens ({message-id tokens}) produced by the given tokenizer version; an
   existing row is replaced."
  [tokens-by-id version]
  (when (seq tokens-by-id)
    (let [now (epoch-seconds)
          insert (builder/for-insert-multi :training_tokens
                                           [:message_id :tokens :generated_at :version]
                                           (mapv (fn [[id tokens]] [id tokens now version]) tokens-by-id)
                                           {})]
      (jdbc/execute! (ds)
                     (if (mariadb?)
                       (conj (rest insert) (str (first insert) " ON DUPLICATE KEY UPDATE tokens = VALUES(tokens), generated_at = VALUES(generated_at), version = VALUES(version)"))
                       (conj (rest insert) (string/replace-first (first insert) #"^INSERT" "INSERT OR REPLACE")))))))

(defn delete-training-tokens!
  "Drop the cached tokens of a message whose content changed, so the next training recomputes them."
  [message-id]
  (jdbc/execute! (ds) (honey/format {:delete-from :training-tokens :where [:= :message-id message-id]})))

(defn- existing-parse-failure
  "The entry a repeated failure belongs to: the same UID, or - when the UID could not be read - the
   same sequence number among the entries without UID. Without this fallback a folder whose store
   dropped would add one new row per remaining message on every run."
  [connection-id folder uid message-number]
  (let [match (cond
                (some? uid) [:= :uid uid]
                (some? message-number) [:and [:= :uid nil] [:= :message-number message-number]]
                :else nil)]
    (when match
      (jdbc/execute-one! (ds) (honey/format {:select [:*] :from [:parse-failures]
                                             :where [:and [:= :connection-id connection-id] [:= :folder folder] match]})
                         builder-function-kebab))))

(defn record-parse-failure!
  "Insert a failure, or bump the attempt count of the entry that already exists for the same message."
  [{:keys [connection-id folder uid message-number message-id subject error]}]
  (let [now (epoch-seconds)
        error-text (some-> error str (subs 0 (min 1000 (count (str error)))))]
    (if-let [existing (existing-parse-failure connection-id folder uid message-number)]
      (jdbc/execute! (ds) (honey/format {:update :parse-failures
                                         :set {:message-number message-number
                                               :message-id (or message-id (:message-id existing))
                                               :subject (or subject (:subject existing))
                                               :error error-text
                                               :attempts (inc (or (:attempts existing) 1))
                                               :last-seen now}
                                         :where [:= :id (:id existing)]}))
      (jdbc/execute! (ds) (honey/format {:insert-into :parse-failures
                                         :values [{:connection-id connection-id :folder folder :uid uid
                                                   :message-number message-number :message-id message-id
                                                   :subject subject :error error-text
                                                   :attempts 1 :first-seen now :last-seen now}]})))))

(defn resolve-parse-failures!
  "Delete the failures of a message that was read successfully after all (matched by UID and/or Message-ID)."
  [connection-id folder uid message-id]
  (let [matches (cond-> []
                  (some? uid) (conj [:= :uid uid])
                  (not (string/blank? (str message-id))) (conj [:= :message-id message-id]))]
    (when (seq matches)
      (jdbc/execute! (ds) (honey/format {:delete-from :parse-failures
                                         :where [:and [:= :connection-id connection-id] [:= :folder folder]
                                                 (if (= 1 (count matches)) (first matches) (into [:or] matches))]})))))

(defn delete-parse-failure! [id]
  (jdbc/execute! (ds) (honey/format {:delete-from :parse-failures :where [:= :id id]})))

(defn parse-failures-for-connection
  "The most recent recorded failures of a connection for display (at most limit rows); the full
   per-folder counts come from parse-failure-counts."
  [connection-id limit]
  (jdbc/execute! (ds) (honey/format {:select [:*] :from [:parse-failures]
                                     :where [:= :connection-id connection-id]
                                     :order-by [[:folder :asc] [:last-seen :desc] [:id :desc]]
                                     :limit limit})
                 builder-function-kebab))

(defn parse-failure-counts
  "Number of recorded failures per folder of a connection: [{:folder ... :count n} ...]."
  [connection-id]
  (jdbc/execute! (ds) (honey/format {:select [:folder [[:count :*] :count]] :from [:parse-failures]
                                     :where [:= :connection-id connection-id]
                                     :group-by [:folder]
                                     :order-by [[:folder :asc]]})
                 builder-function-kebab))

(defn parse-failure-keys
  "The identities of a folder's recorded failures, so a run can tell whether a message it just stored
   had a failure entry without issuing a DELETE per message."
  [connection-id folder]
  (let [rows (jdbc/execute! (ds) (honey/format {:select [:uid :message-id] :from [:parse-failures]
                                                :where [:and [:= :connection-id connection-id] [:= :folder folder]]})
                            builder-function-kebab)]
    {:uids (into #{} (keep :uid) rows)
     :message-ids (into #{} (keep :message-id) rows)}))

(defn parse-failures-for-folder [connection-id folder]
  (jdbc/execute! (ds) (honey/format {:select [:*] :from [:parse-failures]
                                     :where [:and [:= :connection-id connection-id] [:= :folder folder]]
                                     :order-by [[:uid :desc] [:id :desc]]})
                 builder-function-kebab))

(defn parse-batch-message-ids
  "The Message-IDs saved by a folder parse run that are still in the database, newest first."
  [batch-id]
  (mapv :message-id
        (jdbc/execute! (ds) (honey/format {:select [:headers.message-id]
                                           :from [:parse-batch-emails]
                                           :join [:headers [:= :headers.message-id :parse-batch-emails.message-id]]
                                           :where [:= :parse-batch-emails.batch-id batch-id]
                                           :order-by [[:headers.date :desc]]})
                       builder-function-kebab)))

(defn get-categories []
  (jdbc/execute! (ds) (honey/format {:select [:*] :from :categories}) builder-function))

(defn create-category
  ([category] (create-category category nil nil))
  ([category destination-folder] (create-category category destination-folder nil))
  ([category destination-folder color]
   (jdbc/execute! (ds) (honey/format {:insert-into :categories :columns [:name :destination_folder :color] :values [[category destination-folder color]]}))))

(defn delete-category-by-id
  "Delete a category and, in the same transaction, clear it from every metadata row that still
   references it. metadata.category is a non-cascading foreign key, so deleting first would fail
   under MariaDB (and now under SQLite too) — and skipping the cleanup would leave dangling ids."
  [id]
  (jdbc/with-transaction [tx (ds)]
    (jdbc/execute! tx ["UPDATE metadata SET category = NULL, category_confidence = NULL WHERE category = ?" id])
    (jdbc/execute! tx ["DELETE FROM categories WHERE id = ?" id])))

(defn update-category [id destination-folder color]
  (jdbc/execute! (ds) (honey/format {:update :categories
                                     :set    {:destination_folder destination-folder :color color}
                                     :where  [:= :id id]})))

(defn delete-email-by-message-id [message-id]
  ;; Cascades to bodies/communications/metadata; SQLite enforces this via foreign_keys=true in the URL.
  (jdbc/execute! (ds) ["DELETE FROM headers WHERE message_id = ?" message-id]))

(defn category-by-name [category-name]
  (jdbc/execute-one! (ds) (honey/format {:select [:*] :from :categories :where [:= :name category-name]}) builder-function))

(defn category-by-id [id]
  (jdbc/execute-one! (ds) (honey/format {:select [:*] :from :categories :where [:= :id id]}) builder-function))

(def distinct-value-limit
  "Cap the Subject/From/To filter dropdowns to this many distinct values. They're rendered as
   checkboxes in the page (with a client-side search box to narrow them), so an unbounded list
   could mean tens of thousands of DOM nodes on a large mailbox."
  500)

(defn distinct-subjects
  "Distinct, non-blank subjects. other-filters-where (a honeysql where-clause built from the OTHER
   active column filters, or nil) scopes the list to subjects that are still reachable given those
   filters — e.g. once a category is picked, only subjects that occur in that category are offered —
   the same way Excel's own AutoFilter narrows a column's dropdown as other filters are applied.
   The join to metadata exists only so other-filters-where can reference metadata columns."
  ([] (distinct-subjects nil))
  ([other-filters-where]
   (let [base-cond [:and [:is-not :headers.subject nil] [:<> :headers.subject ""]]
         result (jdbc/execute! (ds) (honey/format {:select-distinct [:headers.subject]
                                                    :from [:headers]
                                                    :left-join [:metadata [:= :headers.message-id :metadata.message-id]]
                                                    :where (if other-filters-where [:and base-cond other-filters-where] base-cond)
                                                    :order-by [[:headers.subject :asc]]
                                                    :limit distinct-value-limit})
                                builder-function)]
     (when (= distinct-value-limit (count result))
       (t/log! :info ["Subject filter: more than" distinct-value-limit "distinct subjects exist; showing only the first" distinct-value-limit]))
     result)))

(defn- distinct-contacts-by-type
  "Distinct (contact-key, name, address) contacts that appear with any of participant-types
   (e.g. [\"sender\" \":sender\"] — legacy rows may store the type with a leading colon, see the
   defensive strip in core.email/construct-participants). other-filters-where scopes the list to
   contacts still reachable given the OTHER active column filters — see distinct-subjects."
  ([participant-types] (distinct-contacts-by-type participant-types nil))
  ([participant-types other-filters-where]
   (let [type-cond [:in :communications.type participant-types]
         result (jdbc/execute! (ds) (honey/format {:select-distinct [:contacts.contact-key :contacts.name :contacts.address]
                                                    :from [:contacts]
                                                    :join [:communications [:= :communications.contact-key :contacts.contact-key]
                                                           :headers [:= :headers.message-id :communications.message-id]]
                                                    :left-join [:metadata [:= :headers.message-id :metadata.message-id]]
                                                    :where (if other-filters-where [:and type-cond other-filters-where] type-cond)
                                                    :order-by [[:contacts.address :asc]]
                                                    :limit distinct-value-limit})
                                builder-function)]
     (when (= distinct-value-limit (count result))
       (t/log! :info ["Contact filter: more than" distinct-value-limit "distinct contacts exist for types" participant-types "; showing only the first" distinct-value-limit]))
     result)))

(defn distinct-senders
  ([] (distinct-contacts-by-type ["sender" ":sender"]))
  ([other-filters-where] (distinct-contacts-by-type ["sender" ":sender"] other-filters-where)))

(defn distinct-recipients
  ([] (distinct-contacts-by-type ["receiver" ":receiver"]))
  ([other-filters-where] (distinct-contacts-by-type ["receiver" ":receiver"] other-filters-where)))

(defn distinct-header-categories
  "Distinct metadata.category values (a real category id, or nil for uncategorized) among headers
   matching other-filters-where (or every header, if nil). Used to scope the Category filter's
   checklist to categories still reachable given the OTHER active column filters — see
   distinct-subjects — since categories themselves come from a small, separate table rather than
   being read off the headers/metadata rows directly."
  [other-filters-where]
  (jdbc/execute! (ds) (honey/format (cond-> {:select-distinct [:metadata.category]
                                             :from [:headers]
                                             :left-join [:metadata [:= :headers.message-id :metadata.message-id]]}
                                     other-filters-where (assoc :where other-filters-where)))
                 builder-function))

(defn get-languages []
  (jdbc/execute! (ds) ["select language from metadata group by language"] builder-function))

(defn get-language-preferences []
  (jdbc/execute! (ds) ["select * from category_training_preferences where language <> 'n/a'"] builder-function))

(defn get-activated-language-preferences []
  (jdbc/execute! (ds) ["select * from category_training_preferences where use_in_training = 1"] builder-function))

(defn add-language-preferences [preferences]
  (jdbc/execute! (ds) (insert->insert-ignore (honey/format {:insert-into :category-training-preferences :columns [:language :use-in-training] :values preferences}))))

(defn update-language-preference [preference]
  (jdbc/execute! (ds) ["UPDATE category_training_preferences SET use_in_training = ?  WHERE id = ?" (:use preference) (:id preference)] builder-function))

;;;;;;;;;;;;;; Refactored call stuff

(defn headers-for-strict-options [strict]
  (if strict
    "SELECT headers.message_id, in_reply_to, subject, mime_type, date FROM headers INNER JOIN metadata ON headers.message_id = metadata.message_id"
    "SELECT headers.message_id, in_reply_to, subject, mime_type, date FROM headers LEFT JOIN metadata ON headers.message_id = metadata.message_id"))

(defn body-parts-for-options [] "SELECT * FROM bodies INNER JOIN metadata ON metadata.message_id = bodies.message_id")

(defn time-bucket-for-honey [key]
  (let [buckets (honey-time-buckets)]
    (get buckets key (:yearly buckets))))

(defn convert-to-count [sql-result entity]
  (let [sql (first sql-result)
        ;; replace-first (not replace): a WHERE ... IN (SELECT ... FROM ...) subquery (e.g. the "From"
        ;; e-mail filter) has its own "SELECT ... FROM" later in the string. A global replace would
        ;; rewrite that one too, leaving a second unfilled "%s" for the single `format` argument below.
        ;; Non-greedy ".*?" keeps this first replacement scoped to the outer projection up to its FROM;
        ;; (?is) makes it case-insensitive and dot-matches-newline. Strip a trailing ORDER BY (counts
        ;; must not carry ordering).
        to-format (-> sql
                      (string/replace-first #"(?is)SELECT .*? FROM" "SELECT COUNT(%s) as count FROM")
                      (string/replace #"(?is)\s+ORDER BY .*$" ""))]
    (cond (= entity :enriched-email) (flatten [(format to-format "headers.message_id") (rest sql-result)])
          (= entity :body-part) (flatten [(format to-format "bodies.message_id") (rest sql-result)]))))

(def key-lookup {:message-id :headers.message_id
                 :date :headers.date
                 :category :metadata.category
                 :language :metadata.language
                 :category-modified :metadata.category_modified})

(defn change-important-keys [key]
  (let [lookup (get key-lookup key)]
    (if (nil? lookup)
      key
      lookup)))

(defmulti data->sql :entity)

(defmethod data->sql :body-part [_ sql-clause]
  (let [jdbc-sql (honey/format (postwalk change-important-keys sql-clause))]
    (flatten [(str (body-parts-for-options) " " (first jdbc-sql)) (rest jdbc-sql)])))

(defmethod data->sql :enriched-email
  ([entity-clause sql-clause]
   (let [strict (:strict entity-clause)
         jdbc-sql (honey/format (postwalk change-important-keys sql-clause))]
     (flatten [(str (headers-for-strict-options strict) " " (first jdbc-sql)) (rest jdbc-sql)])))
  ([entity-clause]
   (let [strict (:strict entity-clause)]
     [(headers-for-strict-options strict)])))

(defmethod data->sql :participant [_ sql-clause]
  (let [first-part  {:select [:communications.contact-key :message-id :type :name :address] :from [:communications] :join [:contacts [:= :contacts.contact-key :communications.contact-key]]}]
    (->> (conj first-part sql-clause)
         honey/format)))

(defn fetch-headers [entity-clause sql-clause] (jdbc/execute! (ds) (data->sql entity-clause sql-clause) builder-function-kebab))

(defn fetch-metadata [message-id] (jdbc/execute-one! (ds) ["SELECT message_id, language, language_modified, language_confidence, metadata.category AS category_id, category_modified, category_confidence, categories.name AS category, connection_id FROM metadata LEFT JOIN categories ON metadata.category = categories.id WHERE metadata.message_id = ?" message-id] builder-function-kebab))

(defn fetch-bodies [message-id] (jdbc/execute! (ds) ["SELECT * FROM bodies WHERE message_id = ?" message-id] builder-function-kebab))

(defn fetch-participants [message-id] (jdbc/execute! (ds) ["SELECT * FROM communications LEFT JOIN contacts ON contacts.contact_key = communications.contact_key WHERE message_id = ? " message-id] builder-function-kebab))

(defn db->metadata [db-metadata] (apply core.email/->Metadata ((juxt :message-id :language :language-modified :language-confidence :category :category-id :category-modified :category-confidence :connection-id) db-metadata)))

(defn related-data-to-header [header]
  (let [message-id (:message-id header)
        metadata (db->metadata (fetch-metadata message-id))
        bodies (map core.email/construct-body-part (fetch-bodies message-id))
        participants (map core.email/construct-participants (fetch-participants message-id))]
    (core.email/->EnrichedEmail header bodies participants metadata)))


(defn fetch-metadata-for [message-ids]
  (when (seq message-ids)
    (jdbc/execute! (ds)
                   (into [(str "SELECT message_id, language, language_modified, language_confidence, metadata.category AS category_id, category_modified, category_confidence, categories.name AS category, connection_id FROM metadata LEFT JOIN categories ON metadata.category = categories.id WHERE metadata.message_id IN (" (in-clause-placeholders (count message-ids)) ")")]
                         message-ids)
                   builder-function-kebab)))

(defn fetch-bodies-for [message-ids]
  (when (seq message-ids)
    (jdbc/execute! (ds)
                   (into [(str "SELECT * FROM bodies WHERE message_id IN (" (in-clause-placeholders (count message-ids)) ")")]
                         message-ids)
                   builder-function-kebab)))

(defn fetch-participants-for [message-ids]
  (when (seq message-ids)
    (jdbc/execute! (ds)
                   (into [(str "SELECT * FROM communications LEFT JOIN contacts ON contacts.contact_key = communications.contact_key WHERE message_id IN (" (in-clause-placeholders (count message-ids)) ")")]
                         message-ids)
                   builder-function-kebab)))

(defn related-data-to-headers
  "Batch variant of related-data-to-header. Loads metadata, bodies and participants for a whole page of
   headers in three queries total (instead of three per header), then assembles the EnrichedEmails.
   Preserves the order of the incoming headers. Pass {:with-bodies false} to skip loading body
   content (fetch-bodies-for pulls the full MIME text of every e-mail) for consumers that only
   render header-level data, like the e-mail list."
  ([headers] (related-data-to-headers headers {:with-bodies true}))
  ([headers {:keys [with-bodies]}]
   (let [message-ids        (mapv :message-id headers)
         metadata-by-id     (into {} (map (juxt :message-id identity)) (fetch-metadata-for message-ids))
         bodies-by-id       (if with-bodies (group-by :message-id (fetch-bodies-for message-ids)) {})
         participants-by-id (group-by :message-id (fetch-participants-for message-ids))]
     (mapv (fn [header]
             (let [mid (:message-id header)]
               (core.email/->EnrichedEmail
                header
                (map core.email/construct-body-part (get bodies-by-id mid))
                (map core.email/construct-participants (get participants-by-id mid))
                (db->metadata (get metadata-by-id mid)))))
           headers))))

(defmulti fetch-data (fn [options _] (:entity options)))

(defmethod fetch-data :body-part [entity-clause sql-clause]
  (if (nil? (:page entity-clause))
    (let [result (jdbc/execute! (ds) (data->sql entity-clause sql-clause) builder-function-kebab)]
      (map (fn [el] (core.email/->EnrichedBodyPart (core.email/construct-body-part el) (core.email/map->Metadata el))) result))
    (let [limit-offset (page/page-request->limit-offset (:page entity-clause))
          sql-clause-with-limit-offset (conj sql-clause limit-offset)
          result (jdbc/execute! (ds) (data->sql entity-clause sql-clause-with-limit-offset) builder-function-kebab)
          data (map (fn [el] (core.email/->EnrichedBodyPart (core.email/construct-body-part el) (core.email/map->Metadata el))) result)]
      {:data  data
       :size  (count data)
       :page  (inc (quot (:offset limit-offset) (:limit limit-offset)))
       :total (:count (jdbc/execute-one! (ds) (convert-to-count (data->sql entity-clause sql-clause) (:entity entity-clause)) builder-function-kebab))})))

(defmethod fetch-data :enriched-email [entity-clause sql-clause]
  ;; :with-bodies defaults to true; only an explicit false skips body content.
  (let [related-opts {:with-bodies (not (false? (:with-bodies entity-clause)))}]
    (if (nil? (:page entity-clause))
      (related-data-to-headers (map core.email/construct-header (fetch-headers entity-clause sql-clause)) related-opts)
      (let [limit-offset (page/page-request->limit-offset (:page entity-clause))
            sql-clause-with-limit-offset (conj sql-clause limit-offset)
            data (related-data-to-headers (map core.email/construct-header (fetch-headers entity-clause sql-clause-with-limit-offset)) related-opts)]
        {:data  data
         :size  (count data)
         :page  (inc (quot (:offset limit-offset) (:limit limit-offset)))
         :total (:count (jdbc/execute-one! (ds) (convert-to-count (data->sql entity-clause sql-clause) (:entity entity-clause)) builder-function-kebab))}))))

(defmethod fetch-data :participant [entity-clause sql-clause]
  (map core.email/map->Participant (jdbc/execute! (ds) (data->sql entity-clause sql-clause) builder-function-kebab)))

(defn email-statistics-query
  "Build a portable query that counts e-mails in database-side time buckets. The expression itself is
   repeated in GROUP BY and ORDER BY instead of referring to a SELECT alias. In particular, never use
   `interval` as an unquoted alias: INTERVAL is reserved by MariaDB."
  [period]
  (let [time-bucket (time-bucket-for-honey period)]
    {:select [[time-bucket :time-bucket]
              [[:count :headers.message-id] :count]]
     :from [:headers]
     :where [:is-not :headers.date nil]
     :group-by [time-bucket]
     :order-by [[time-bucket :asc]]}))

(defn email-statistics-by-period [period]
  (jdbc/execute! (ds)
                 (honey/format (email-statistics-query period))
                 builder-function-kebab))

(defn yearly-email-stats []
  (email-statistics-by-period :yearly))

(defn query-db [honeysql-query]
  (jdbc/execute! (ds) (honey/format honeysql-query) builder-function-kebab))

(comment (honey/format
          {:select [[[:count :headers.message-id] :count] :bodies.mime-type] :from [:bodies]
           :join [:headers [:= :bodies.message-id :headers.message_id]]
           :group-by [:bodies.mime-type]
           :order-by [[:count :desc]]}))

(defn update-preference [preference value]
  (if (mariadb?)
    (jdbc/execute! (ds)
      ["INSERT INTO preferences (preference, value) VALUES (?, ?) ON DUPLICATE KEY UPDATE value = VALUES(value)"
       (name preference) value])
    (jdbc/execute! (ds)
                   (-> {:insert-into [:preferences]
                        :columns     [:preference :value]
                        :values      [[(name preference) value]]}
                       (honey/format)
                       (insert->insert-update)) builder-function-kebab)))

(defn fetch-preference [preference]
  (let [result (jdbc/execute-one! (ds)
                                  (honey/format {:select [:value]
                                                 :from [:preferences]
                                                 :where [:= :preference (name preference)]}) builder-function-kebab)]
    (when (some? result) (:value result))))

(defn- coerce-bool
  "SQLite stores BOOLEANs as 1/0 integers; MariaDB JDBC can return Java true/false.
   Normalise both representations to a Clojure boolean."
  [v]
  (or (= 1 v) (true? v)))

(defn db-connection->model [db-conn]
  (apply (comp core.email/map->ImapConnection
               (fn [conn] (update conn :check-ssl-certs coerce-bool))
               (fn [conn] (update conn :debug coerce-bool))) [db-conn]))

(defn get-connections [] (map
                          db-connection->model
                          (jdbc/execute! (ds) (honey/format {:select [:*] :from [:connections]}) builder-function-kebab)))

(defn get-connection
  "The stored connection with the given id, or nil when there is none (instead of a record of nils)."
  [id]
  (some-> (jdbc/execute-one! (ds) (honey/format {:select [:*] :from [:connections] :where [:= :id id]}) builder-function-kebab)
          db-connection->model))

(defn get-oauth-tokens [connection-id] (jdbc/execute-one! (ds) (honey/format {:select [:*] :from [:oauth-tokens] :where [:= :connection-id connection-id]}) builder-function-kebab))

(defn save-oauth-token
  "Upsert the token row for a connection (connection_id is unique, see migration V12). COALESCE
   keeps the stored refresh token when the provider omits one on re-authorization — Google and
   others only return a refresh token on the first consent."
  [token-response]
  (let [params [(:connection-id token-response) (:access_token token-response) (:expires_in token-response)
                (:refresh_token token-response) (:scope token-response) (:token_type token-response)]]
    (if (mariadb?)
      (jdbc/execute! (ds)
        (into ["INSERT INTO oauth_tokens (connection_id, access_token, expires_in, refresh_token, scope, token_type) VALUES (?, ?, ?, ?, ?, ?) ON DUPLICATE KEY UPDATE access_token = VALUES(access_token), expires_in = VALUES(expires_in), refresh_token = COALESCE(VALUES(refresh_token), refresh_token), scope = VALUES(scope), token_type = VALUES(token_type)"] params))
      (jdbc/execute! (ds)
        (into ["INSERT INTO oauth_tokens (connection_id, access_token, expires_in, refresh_token, scope, token_type) VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT(connection_id) DO UPDATE SET access_token = excluded.access_token, expires_in = excluded.expires_in, refresh_token = COALESCE(excluded.refresh_token, oauth_tokens.refresh_token), scope = excluded.scope, token_type = excluded.token_type"] params)))))

(defn update-access-token [connection-id token-response]
  (if (nil? (:refresh_token token-response))
    (jdbc/execute! (ds) (-> {:update [:oauth_tokens]
                             :set {:access_token (:access_token token-response) :expires_in (:expires_in token-response)}
                             :where [:= :connection_id connection-id]}
                            (honey/format)))
    (jdbc/execute! (ds) (-> {:update [:oauth_tokens]
                             :set {:access_token (:access_token token-response) :expires_in (:expires_in token-response) :refresh_token (:refresh_token token-response)}
                             :where [:= :connection_id connection-id]}
                            (honey/format)))))

(defn delete-access-token [connection-id]
  (jdbc/execute! (ds) (honey/format {:delete-from [:oauth_tokens]
                                     :where [:= :connection_id connection-id]}) builder-function-kebab))

(defn add-connection [connection]
  (jdbc/execute! (ds)
                 (honey/format {:insert-into [:connections]
                                :columns [:id :host :user :secret :folder :debug :security :port :check-ssl-certs :auth_type :auth_provider]
                                :values [[(:id connection) (:host connection) (:user connection)
                                          (:secret connection) (:folder connection) (:debug connection) (:security connection) (:port connection) (:check-ssl-certs connection)
                                          (:auth-type connection) (:auth-provider connection)]]})
                 builder-function))

(defn update-connection [connection]
  (jdbc/execute! (ds)
                 (honey/format {:update [:connections]
                                :set {:host (:host connection) :user (:user connection) :secret (:secret connection) :folder (:folder connection) :debug (:debug connection) :port (:port connection) :security (:security connection) :check-ssl-certs (:check-ssl-certs connection) :auth_type (:auth-type connection) :auth-provider (:auth-provider connection)}
                                :where  [:= :id (:id connection)]})
                 builder-function))

(defn delete-connection [id] (jdbc/execute! (ds) (honey/format {:delete-from [:connections]
                                                                :where [:= :id id]}) builder-function-kebab))

(defn add-auth-provider [provider]
  (jdbc/execute! (ds)
                 (honey/format {:insert-into [:auth_providers]
                                :values [provider]})
                 builder-function))

(defn get-auth-providers []
  (jdbc/execute! (ds) (honey/format {:select [:*] :from [:auth_providers]}) builder-function-kebab))

(defn get-auth-provider [id]
  (jdbc/execute-one! (ds) (honey/format {:select [:*] :from [:auth_providers] :where [:= :id id]}) builder-function-kebab))

(defn delete-auth-provider
  "Delete a provider after detaching it from any connection that references it —
   connections.auth_provider is a non-cascading foreign key."
  [id]
  (jdbc/with-transaction [tx (ds)]
    (jdbc/execute! tx ["UPDATE connections SET auth_provider = NULL WHERE auth_provider = ?" id])
    (jdbc/execute! tx ["DELETE FROM auth_providers WHERE id = ?" id])))

(defn update-auth-provider [provider]
  (let [wo-id (dissoc provider :id)]
    (jdbc/execute! (ds)
                   (honey/format {:update [:auth_providers]
                                  :set wo-id
                                  :where  [:= :id (:id provider)]})
                   builder-function)))

(defn close-pool!
  "Shut down the MariaDB connection pool gracefully (called on app stop)."
  []
  (when-let [pool ^HikariDataSource @mariadb-pool]
    (.close pool)
    (reset! mariadb-pool nil)))

(deftype SqliteDB []
  int/DB
  (fetch-connection [_ id] (get-connection id))
  (fetch-oauth-token-data [_ connection-id] (get-oauth-tokens connection-id))
  (fetch-auth-provider [_ id] (get-auth-provider id))
  (fetch-categories [_] (get-categories))
  (fetch-distinct-subjects [_ other-filters-where] (distinct-subjects other-filters-where))
  (fetch-distinct-senders [_ other-filters-where] (distinct-senders other-filters-where))
  (fetch-distinct-recipients [_ other-filters-where] (distinct-recipients other-filters-where))
  (fetch-header-categories [_ other-filters-where] (distinct-header-categories other-filters-where))
  (fetch-emails [_ entity customization] (fetch-data entity customization))
  (save-category [_ category-name destination-folder color] (create-category category-name destination-folder color))
  (update-category [_ id destination-folder color] (update-category id destination-folder color))
  (update-email-folder [_ message-id folder] (update-email-folder message-id folder))
  (email-exists? [_ message-id] (email-exists? message-id))
  (record-parse-batch-email [_ batch-id message-id] (record-parse-batch-email! batch-id message-id))
  (record-parse-failure [_ failure] (record-parse-failure! failure))
  (resolve-parse-failures [_ connection-id folder uid message-id] (resolve-parse-failures! connection-id folder uid message-id))
  (parse-failure-keys [_ connection-id folder] (parse-failure-keys connection-id folder))
  (fetch-parse-batch-message-ids [_ batch-id] (parse-batch-message-ids batch-id))
  (fetch-email-location [_ message-id] (email-location message-id))
  (fetch-categorized-message-ids-in-folder [_ connection-id folder] (categorized-message-ids-in-folder connection-id folder))
  (fetch-parse-batch [_ id] (parse-batch id))
  (fetch-content-match-ids [_ search-text] (content-match-message-ids search-text))
  (save-email [_ email]
    ;; One transaction per email, and failures propagate: a partially-saved email whose header
    ;; already exists would otherwise be skipped by every future backfill (see save-emails-in-buffer).
    (jdbc/with-transaction [tx (ds)]
      (save-headers tx [(:header email)])
      (if (seq (:body email))
        (save-bodies tx (:body email))
        (t/log! :warn ["No body parts parsed for message ID:" (-> email :header :message-id)]))
      (when (seq (:participants email))
        (save-contacts tx (:participants email))
        (save-communications tx (:participants email)))
      (when (seq (:metadata email)) (update-metadata-batch tx [(:metadata email)])))))
