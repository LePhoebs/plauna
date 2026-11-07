(ns plauna.markup
  (:require [clojure.data.json :as json]
            [selmer.parser :refer [render-file set-resource-path!]]
            [selmer.filters :refer [add-filter!]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [plauna.client :as client]
            [plauna.core.email :as core-email]
            [plauna.core.flags :as flags]
            [plauna.i18n :as i18n]
            [plauna.preferences :as preferences]
            [taoensso.telemere :as t])
  (:import
   (java.time Instant LocalDateTime)
   (java.util Locale)))

(set! *warn-on-reflection* true)

(set-resource-path! (io/resource "templates"))

(comment
  (selmer.parser/cache-off!))

(defonce shell-provider
  ;; What the page shell (sidebar, phone bar) shows on every page besides the page itself: the
  ;; categories with their counts, for example. The server installs a provider that reads the
  ;; database; without one (tests) the shell shows no categories.
  (atom (constantly {})))

(defn- shell-context []
  (try (@shell-provider)
       (catch Exception e
         (t/log! {:level :warn :error e} "Could not load the sidebar data; the page is shown without it.")
         {})))

(defn- render
  "render-file wrapper that injects the shell data - the inactive connections, the sidebar's
   categories, the interface language - into every authenticated page."
  [template ctx]
  (let [disconnected (seq (client/disconnected-connections))
        context (merge {:disconnected disconnected
                        :disconnected-count (count disconnected)
                        :disconnected-text (str/join ", " (map #(str (:user %) "@" (:host %)) disconnected))
                        :lang (name i18n/*lang*)
                        :ui-language (preferences/ui-language)}
                       (shell-context)
                       ctx)]
    ;; The phone bar shows Accounts, Statistics and System under "More".
    (render-file template (assoc context :more-active? (contains? #{:accounts :statistics :system} (:active-nav context))))))

(defn page
  "Render a page template inside the shell (see render) - for pages assembled outside this namespace."
  [template ctx]
  (render template ctx))

(defn timestamp->date [timestamp]
  (if (nil? timestamp)
    nil
    (LocalDateTime/ofInstant (Instant/ofEpochSecond timestamp) (preferences/zone-id))))

(def ^:private toast-icons
  {:alert "M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5Zm3.707 11.793a1 1 0 1 1-1.414 1.414L10 11.414l-2.293 2.293a1 1 0 0 1-1.414-1.414L8.586 10 6.293 7.707a1 1 0 0 1 1.414-1.414L10 8.586l2.293-2.293a1 1 0 0 1 1.414 1.414L11.414 10l2.293 2.293Z"
   :success "M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5Zm3.707 8.207-4 4a1 1 0 0 1-1.414 0l-2-2a1 1 0 0 1 1.414-1.414L9 10.586l3.293-3.293a1 1 0 0 1 1.414 1.414Z"
   :info "M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5ZM10 15a1 1 0 1 1 0-2 1 1 0 0 1 0 2Zm1-4a1 1 0 0 1-2 0V6a1 1 0 0 1 2 0v5Z"})

(defn type->toast-role
  "Prepare a message for the toast area: its icon, its tone (error / success / info) and whether it
   closes by itself (everything but errors). A message with :key is translated now, in the language
   of the page that shows it - messages of background jobs are created without a request."
  [message]
  (let [message (cond-> message
                  (:key message) (assoc :content (i18n/t (:key message) (:args message))))
        tone (case (:type message) :alert "error" :success "success" :info "info" nil)]
    (if tone
      (assoc message :path (get toast-icons (:type message)) :tone tone :sticky? (= "error" tone)
             :id (str "toast-" (hash message)))
      message)))

(defn administration
  ([data] (render "admin.html" {:active-nav :system :data data}))
  ([data messages] (render "admin.html" {:messages (mapv type->toast-role messages) :active-nav :system :data data})))

(defn login-page
  ([] (login-page {}))
  ([data] (render-file "login.html" (assoc data :lang (name i18n/*lang*)))))

(defn password-page [data] (render "admin-password.html" (conj {:active-nav :system} data)))

(defn mtls-page
  ([data] (render "admin-mtls.html" (assoc data :active-nav :system)))
  ([data messages]
   (render "admin-mtls.html"
           (assoc data :active-nav :system :messages (mapv type->toast-role messages)))))

(defn concat-string [contact]
  (if (nil? (:name contact))
    (:address contact)
    (str (:name contact) " - " (:address contact))))

(defn concat-contacts
  ([key contacts]
   (->> contacts
        (filter (fn [contact] (= (:type contact) key)))
        (reduce (fn [acc el]
                  (if (empty? acc)
                    (str acc (concat-string el))
                    (str acc ", " (concat-string el))))
                "")))
  ([contacts]
   (reduce (fn [acc el] (if (empty? acc)
                          (str acc (:address el))
                          (str acc ", " (:address el)))) "" contacts)))

;; "From" shows the From address where Plauna stored it, else the sender as before (the Sender header,
;; else From): e-mails saved before both were kept only have the sender.
(defn- from-text [contacts]
  (let [from (concat-contacts :from contacts)]
    (if (seq from) from (concat-contacts :sender contacts))))

(add-filter! :concat-senders from-text)

;; The Sender header (a mailing service, a secretary) when it differs from what "From" shows.
(add-filter! :envelope-sender (fn [contacts]
                                (let [sender (concat-contacts :sender contacts)]
                                  (when (and (seq sender) (not= sender (from-text contacts))) sender))))

(add-filter! :concat-receivers (partial concat-contacts :receiver))

(add-filter! :concat-cc (partial concat-contacts :cc))

(add-filter! :concat-bcc (partial concat-contacts :bcc))

(add-filter! :iconize (fn [pred] (if pred "✓" "⤫")))

(add-filter! :readable-message-id core-email/imap-message-id)

;; URL-safe Base64 (- and _ instead of + and /): the value is a path segment of /emails/:id, and a / in
;; it - for example from a Message-ID containing ? - would make the route miss.
(add-filter! :base64-encode (fn [^String string] (.encodeToString (java.util.Base64/getUrlEncoder) (.getBytes string java.nio.charset.StandardCharsets/UTF_8))))

(defn confidence->percent
  "Render a stored 0..1 model probability as a percentage. A missing probability is deliberately
   shown as unknown instead of as 0 %, because those two states have different meanings."
  [confidence]
  (if (nil? confidence)
    "—"
    (String/format Locale/ROOT "%.2f %%"
                   (object-array [(* 100.0 (double confidence))]))))

(add-filter! :confidence-percent confidence->percent)

(def default-category-color
  "Used for a category saved before color-coding existed, or with a corrupted value.
   Must match plauna.application/default-category-color."
  "#9ca3af")

(defn- script-safe-json
  "JSON for an inline <script>: < is escaped too, so no value can close the script element or open an
   HTML comment in it."
  [value]
  (str/replace (json/write-str value) "<" "\\u003c"))

;; A value inside an inline <script>, as a JavaScript string literal (quotes included). HTML escaping
;; is the wrong escaping there: the script would see &amp; instead of &.
(add-filter! :js (fn [value] [:safe (script-safe-json (str value))]))

(add-filter! :category-color
             ;; Re-validate here too (not just on save): renders straight into a style="" attribute,
             ;; so a malformed value must never reach the page even if it somehow got into the DB.
             (fn [color] (if (re-matches #"#[0-9a-fA-F]{6}" (str color)) color default-category-color)))

(defn list-date
  "How the e-mail list shows a date: the time for today, day and month for this year, the full date
   for older e-mails."
  ([^LocalDateTime date-time] (list-date date-time (java.time.LocalDate/now ^java.time.ZoneId (preferences/zone-id))))
  ([^LocalDateTime date-time ^java.time.LocalDate today]
   (when date-time
     (let [day (.toLocalDate date-time)]
       (.format date-time (java.time.format.DateTimeFormatter/ofPattern
                           (cond (= day today) "HH:mm"
                                 (= (.getYear day) (.getYear today)) "dd.MM."
                                 :else "dd.MM.yyyy")))))))

(defn- list-sender
  "The contact the list shows as the sender: the first From address, else the envelope sender."
  [contacts]
  (or (first (filter #(= :from (:type %)) contacts))
      (first (filter #(= :sender (:type %)) contacts))))

(defn list-row
  "An e-mail prepared for a row of the list."
  [email]
  (let [date (timestamp->date (get-in email [:header :date]))
        sender (list-sender (:participants email))
        confidence (get-in email [:metadata :category-confidence])]
    (-> email
        (assoc-in [:header :date] date)
        (assoc :date-text (list-date date)
               :sender-name (or (not-empty (str/trim (str (:name sender)))) (:address sender))
               :sender-address (when (not-empty (str/trim (str (:name sender)))) (:address sender))
               ;; A category the model assigned and nobody confirmed: shown with its probability.
               :model-percent (when (and (some? (get-in email [:metadata :category-id])) (number? confidence) (< (double confidence) 1.0))
                                (str (Math/round (* 100.0 (double confidence))) " %"))))))

(defn list-emails
  "filter-options supplies the Excel-style column filter dropdowns' possible values:
   {:categories ... :subjects ... :senders ... :recipients ...}"
  ([emails page-info filter-options]
   (render "emails.html" (merge {:emails (map list-row emails) :page page-info :header "Emails" :active-nav :emails} filter-options)))
  ([emails page-info filter-options messages]
   (render "emails.html" (merge {:emails (map list-row emails) :page page-info :header "Emails" :messages (mapv type->toast-role messages) :active-nav :emails} filter-options))))

(defn list-email-contents
  "category-scores: the model's most likely categories of the e-mail ([{:id :name :color
   :confidence}]), or nil when there is none to show."
  ([email-data categories]
   (list-email-contents email-data categories nil))
  ([email-data categories category-scores]
   (render "email.html" {:email (update-in email-data [:header :date] timestamp->date) :categories categories
                         :category-scores category-scores :active-nav :emails}))
  ([email-data categories category-scores messages]
   (render "email.html" {:email (update-in email-data [:header :date] timestamp->date) :categories categories
                         :category-scores category-scores :active-nav :emails :messages (mapv type->toast-role messages)})))

(def ^:private vega-lite-schema "https://vega.github.io/schema/vega-lite/v6.json")
(def ^:private chart-color "#8c0327")

(defn- count-value [row]
  (long (or (:count row) 0)))

(defn- sum-counts [rows]
  (reduce + 0 (map count-value rows)))

(defn- present-label? [value]
  (and (some? value) (not (str/blank? (str value))) (not= "n/a" value)))

(defn- normalized-breakdown
  "One bar per label, largest first. Rows whose raw value maps to the same label (NULL and \"n/a\" both
   become missing-label) are merged, so a chart never shows the same label twice."
  [rows field missing-label]
  (->> rows
       (map (fn [row]
              {field (if (present-label? (get row field)) (str (get row field)) missing-label)
               :count (count-value row)}))
       (group-by field)
       (map (fn [[label grouped]] {field label :count (reduce + 0 (map :count grouped))}))
       (sort-by :count >)
       vec))

(defn- base-chart [description values]
  {(keyword "$schema") vega-lite-schema
   :description description
   :width "container"
   :autosize {:type "fit-x" :contains "padding" :resize true}
   :data {:values values}
   :config {:background nil
            :view {:stroke nil}
            :axis {:labelColor "#374151" :titleColor "#374151" :gridColor "#e5e7eb"}
            :font "system-ui"}})

(defn- volume-chart [values]
  (merge (base-chart (i18n/t :stats.per-year) values)
         {:height 280
          :mark {:type "bar" :tooltip true :color chart-color
                 :cornerRadiusTopLeft 3 :cornerRadiusTopRight 3}
          :encoding {:x {:field "time-bucket" :type "ordinal" :title (i18n/t :stats.year) :sort nil}
                     :y {:field "count" :type "quantitative" :title (i18n/t :stats.emails)
                         :axis {:tickMinStep 1 :format "d"}}
                     :tooltip [{:field "time-bucket" :type "ordinal" :title (i18n/t :stats.year)}
                               {:field "count" :type "quantitative" :title (i18n/t :stats.emails)}]}}))

(defn- breakdown-chart [description values field label-title]
  (merge (base-chart description values)
         {:height (max 60 (* 30 (count values)))
          :mark {:type "bar" :tooltip true :color chart-color :cornerRadiusEnd 3 :size 22}
          :encoding {:y {:field (name field) :type "nominal" :title nil :sort "-x"
                         :axis {:labelLimit 240}}
                     :x {:field "count" :type "quantitative" :title (i18n/t :stats.emails)
                         :axis {:tickMinStep 1 :format "d"}}
                     :tooltip [{:field (name field) :type "nominal" :title label-title}
                               {:field "count" :type "quantitative" :title (i18n/t :stats.emails)}]}}))

(defn- percentage-string [part total]
  (if (zero? total)
    "0.0 %"
    (String/format Locale/ROOT "%.1f %%"
                   (object-array [(* 100.0 (/ (double part) (double total)))]))))

(defn- chart-context [id title description values spec]
  {:id id
   :title title
   :description description
   :has-data (boolean (seq values))
   :json-data (when (seq values) (script-safe-json spec))})

(defn statistics-overall [yearly-emails mime-types languages categories]
  (let [email-values (->> yearly-emails
                          (keep (fn [{:keys [time-bucket] :as row}]
                                  (when (some? time-bucket)
                                    {:time-bucket (str time-bucket) :count (count-value row)})))
                          vec)
        mime-values (normalized-breakdown mime-types :mime-type (i18n/t :stats.unknown))
        language-values (normalized-breakdown languages :language (i18n/t :stats.not-detected))
        category-values (normalized-breakdown categories :name (i18n/t :stats.uncategorized))
        total-emails (sum-counts categories)
        categorized-emails (sum-counts (filter #(present-label? (:name %)) categories))
        detected-languages (count (set (keep #(when (present-label? (:language %)) (:language %)) languages)))
        known-mime-types (count (set (keep #(when (present-label? (:mime-type %)) (:mime-type %)) mime-types)))]
    (render "statistics.html"
            {:active-nav :statistics
             :summary [{:label (i18n/t :stats.total) :value total-emails}
                       {:label (i18n/t :stats.categorized) :value (percentage-string categorized-emails total-emails)}
                       {:label (i18n/t :stats.languages-detected) :value detected-languages}
                       {:label (i18n/t :stats.mime-types) :value known-mime-types}]
             :charts [(chart-context "email-volume" (i18n/t :stats.volume) (i18n/t :stats.volume-help)
                                     email-values (volume-chart email-values))
                      (chart-context "mime-types" (i18n/t :stats.mime-types) (i18n/t :stats.mime-help)
                                     mime-values (breakdown-chart (i18n/t :stats.by-mime) mime-values :mime-type (i18n/t :stats.mime-type)))
                      (chart-context "languages" (i18n/t :stats.language-chart) (i18n/t :stats.language-help)
                                     language-values (breakdown-chart (i18n/t :stats.by-language) language-values :language (i18n/t :stats.language)))
                      (chart-context "categories" (i18n/t :stats.category-chart) (i18n/t :stats.category-help)
                                     category-values (breakdown-chart (i18n/t :stats.by-category) category-values :name (i18n/t :stats.category)))]})))

(defn flag-colors
  "The flag colors with their names in the interface language."
  []
  (mapv #(assoc % :label (i18n/t (keyword (str "flag." (:id %))))) flags/colors))

(defn- with-flag-hex [category]
  (let [flag (first (filter #(= (:flag_color category) (:id %)) (flag-colors)))]
    (assoc category :flag_hex (:hex flag) :flag_label (:label flag))))

(defn categories-page
  ([categories] (categories-page categories nil))
  ([categories messages]
   (render "admin-categories.html" (cond-> {:categories (mapv with-flag-hex categories)
                                            :flag-colors (flag-colors)
                                            :active-nav :categories}
                                     (some? messages) (assoc :messages (mapv type->toast-role messages))))))

(defn- percent [confidence]
  (when (number? confidence) (str (Math/round (* 100.0 (double confidence))) " %")))

(defn- suggestion-group
  "A group of suggested e-mails prepared for the category page."
  [group]
  (-> group
      (assoc :min-percent (percent (:min-confidence group))
             :max-percent (percent (:max-confidence group))
             :message-ids (str/join "\n" (map :message-id (:emails group))))
      (update :emails (fn [emails]
                        (mapv #(assoc % :date-text (list-date (timestamp->date (:date %))) :percent (percent (:confidence %)))
                              emails)))))

(defn category-page
  "A category's own page (see plauna.server/category-page for data). messages: pending toasts."
  ([data] (category-page data nil))
  ([data messages]
   (render "category.html" (-> data
                              (cond-> (seq messages) (assoc :messages (mapv type->toast-role messages)))
                              (update :tab #(or % "suggestions"))
                              (update :emails #(mapv list-row %))
                              (update :groups #(mapv suggestion-group %))
                              (as-> page (assoc page :group-count (count (:groups page))))
                              (assoc :active-nav :categories :active-category (get-in data [:category :id]))))))

(defn model-page
  "The Model area: training, languages, thresholds, model type and scoring. messages: pending toasts."
  ([data] (model-page data nil))
  ([data messages]
   (render "model.html" (cond-> (assoc data :active-nav :model)
                          (seq messages) (assoc :messages (mapv type->toast-role messages))))))

(defn languages-admin-page
  ([language-preferences] (languages-admin-page language-preferences nil))
  ([language-preferences messages]
   (render "admin-languages.html" (cond-> {:language-preferences language-preferences :active-nav :model}
                                    (seq messages) (assoc :messages (mapv type->toast-role messages))))))

(defn connections-list
  ([connections] (render "admin-connections.html" {:configs connections :active-nav :accounts}))
  ([connections messages] (render "admin-connections.html" {:configs connections :active-nav :accounts :messages (mapv type->toast-role messages)})))


(defn- format-timestamp [epoch-seconds]
  (some-> epoch-seconds timestamp->date str (str/replace "T" " ") (subs 0 16)))

(defn- format-parse-batch
  "Human-readable start time for a folder parse run row on the connection page."
  [batch]
  (assoc batch :started (format-timestamp (:started-at batch))))

(defn- format-parse-failure-group [group]
  (update group :failures (fn [failures] (mapv #(assoc % :last-seen-text (format-timestamp (:last-seen %))) failures))))

(defn- connection-context [config folders categories]
  (merge config {:folders folders
                 :active-nav :accounts
                 :categories (cons nil categories)
                 :parse-batches (mapv format-parse-batch (:parse-batches config))
                 :parse-failures (mapv format-parse-failure-group (:parse-failures config))}))

(defn connection
  ([config folders categories] (render "admin-connection.html" (connection-context config folders categories)))
  ([config folders messages categories] (render "admin-connection.html" (assoc (connection-context config folders categories) :messages (mapv type->toast-role messages)))))

(defn training-progress-page
  "The live progress view of a model training run; back is the same-origin path to return to."
  [back]
  (render "training-progress.html" {:active-nav :model :back back}))

(defn preferences-page
  ([data] (preferences-page data nil))
  ([data messages]
   (let [log-levels {:log-level-options [{:key :error :name "Error"}
                                         {:key :info :name "Info"}
                                         {:key :debug :name "Debug"}]
                     :active-nav :system}]
     (render "admin-preferences.html"
             (cond-> (conj data log-levels)
               (seq messages) (assoc :messages (mapv type->toast-role messages)))))))

(defn new-connection [providers]
  (render "admin-new-connection.html" {:auth-providers providers :active-nav :accounts}))
