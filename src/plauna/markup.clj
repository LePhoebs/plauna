(ns plauna.markup
  (:require [clojure.data.json :as json]
            [selmer.parser :refer [render-file set-resource-path!]]
            [selmer.filters :refer [add-filter!]]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [ring.util.codec :refer [base64-encode]]
            [plauna.client :as client]
            [plauna.core.email :as core-email]
            [plauna.core.flags :as flags]
            [plauna.preferences :as preferences])
  (:import
   (java.time Instant LocalDateTime)
   (java.util Locale)))

(set! *warn-on-reflection* true)

(set-resource-path! (io/resource "templates"))

(comment
  (selmer.parser/cache-off!))

(defn- render
  "render-file wrapper that injects disconnected-connections into every authenticated page."
  [template ctx]
  (render-file template (merge {:disconnected (seq (client/disconnected-connections))} ctx)))

(defn timestamp->date [timestamp]
  (if (nil? timestamp)
    nil
    (LocalDateTime/ofInstant (Instant/ofEpochSecond timestamp) (preferences/zone-id))))

(defn type->toast-role [message]
  (cond
    (= :alert (:type message)) (conj message {:path "M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5Zm3.707 11.793a1 1 0 1 1-1.414 1.414L10 11.414l-2.293 2.293a1 1 0 0 1-1.414-1.414L8.586 10 6.293 7.707a1 1 0 0 1 1.414-1.414L10 8.586l2.293-2.293a1 1 0 0 1 1.414 1.414L11.414 10l2.293 2.293Z"
                                              :color "text-red-500"
                                              :bg-color "bg-red-100"
                                              :id (str "toast-" (hash message))})
    (= :success (:type message)) (conj message {:path "M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5Zm3.707 8.207-4 4a1 1 0 0 1-1.414 0l-2-2a1 1 0 0 1 1.414-1.414L9 10.586l3.293-3.293a1 1 0 0 1 1.414 1.414Z"
                                                :color "text-green-500"
                                                :bg-color "bg-green-100"
                                                :id (str "toast-" (hash message))})
    (= :info (:type message)) (conj message {:path "M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5ZM10 15a1 1 0 1 1 0-2 1 1 0 0 1 0 2Zm1-4a1 1 0 0 1-2 0V6a1 1 0 0 1 2 0v5Z"
                                             :color "text-orange-500"
                                             :bg-color "bg-orange-100"
                                             :id (str "toast-" (hash message))})
    :else message))

(defn administration
  ([data] (render "admin.html" {:active-nav :admin :data data}))
  ([data messages] (render "admin.html" {:messages (mapv type->toast-role messages) :active-nav :admin :data data})))

(defn login-page
  ([] (login-page {}))
  ([data] (render-file "login.html" data)))

(defn password-page [data] (render "admin-password.html" (conj {:active-nav :admin} data)))

(defn mtls-page
  ([data] (render "admin-mtls.html" (assoc data :active-nav :admin)))
  ([data messages]
   (render "admin-mtls.html"
           (assoc data :active-nav :admin :messages (mapv type->toast-role messages)))))

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

(add-filter! :concat-senders (partial concat-contacts :sender))

(add-filter! :concat-receivers (partial concat-contacts :receiver))

(add-filter! :concat-cc (partial concat-contacts :cc))

(add-filter! :concat-bcc (partial concat-contacts :bcc))

(add-filter! :iconize (fn [pred] (if pred "✓" "⤫")))

(add-filter! :readable-message-id core-email/imap-message-id)

(add-filter! :base64-encode (fn [^String string] (base64-encode (.getBytes string))))

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

(add-filter! :category-color
             ;; Re-validate here too (not just on save): renders straight into a style="" attribute,
             ;; so a malformed value must never reach the page even if it somehow got into the DB.
             (fn [color] (if (re-matches #"#[0-9a-fA-F]{6}" (str color)) color default-category-color)))

(defn list-emails
  "filter-options supplies the Excel-style column filter dropdowns' possible values:
   {:categories ... :subjects ... :senders ... :recipients ...}"
  ([emails page-info filter-options]
   (let [emails-with-java-date (map #(update-in % [:header :date] timestamp->date) emails)]
     (render "emails.html" (merge {:emails emails-with-java-date :page page-info :header "Emails" :active-nav :emails} filter-options))))
  ([emails page-info filter-options messages]
   (let [emails-with-java-date (map #(update-in % [:header :date] timestamp->date) emails)]
     (render "emails.html" (merge {:emails emails-with-java-date :page page-info :header "Emails" :messages (mapv type->toast-role messages) :active-nav :emails} filter-options)))))

(defn list-email-contents
  "category-scores: the model's most likely categories of an uncategorized e-mail ([{:id :name :color
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
  (merge (base-chart "Number of e-mails per year" values)
         {:height 280
          :mark {:type "bar" :tooltip true :color chart-color
                 :cornerRadiusTopLeft 3 :cornerRadiusTopRight 3}
          :encoding {:x {:field "time-bucket" :type "ordinal" :title "Year" :sort nil}
                     :y {:field "count" :type "quantitative" :title "E-mails"
                         :axis {:tickMinStep 1 :format "d"}}
                     :tooltip [{:field "time-bucket" :type "ordinal" :title "Year"}
                               {:field "count" :type "quantitative" :title "E-mails"}]}}))

(defn- breakdown-chart [description values field label-title]
  (merge (base-chart description values)
         {:height (max 60 (* 30 (count values)))
          :mark {:type "bar" :tooltip true :color chart-color :cornerRadiusEnd 3 :size 22}
          :encoding {:y {:field (name field) :type "nominal" :title nil :sort "-x"
                         :axis {:labelLimit 240}}
                     :x {:field "count" :type "quantitative" :title "E-mails"
                         :axis {:tickMinStep 1 :format "d"}}
                     :tooltip [{:field (name field) :type "nominal" :title label-title}
                               {:field "count" :type "quantitative" :title "E-mails"}]}}))

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
   :json-data (when (seq values) (json/write-str spec))})

(defn statistics-overall [yearly-emails mime-types languages categories]
  (let [email-values (->> yearly-emails
                          (keep (fn [{:keys [time-bucket] :as row}]
                                  (when (some? time-bucket)
                                    {:time-bucket (str time-bucket) :count (count-value row)})))
                          vec)
        mime-values (normalized-breakdown mime-types :mime-type "Unknown")
        language-values (normalized-breakdown languages :language "Not detected")
        category-values (normalized-breakdown categories :name "Uncategorized")
        total-emails (sum-counts categories)
        categorized-emails (sum-counts (filter #(present-label? (:name %)) categories))
        detected-languages (count (set (keep #(when (present-label? (:language %)) (:language %)) languages)))
        known-mime-types (count (set (keep #(when (present-label? (:mime-type %)) (:mime-type %)) mime-types)))]
    (render "statistics.html"
            {:active-nav :statistics
             :summary [{:label "Total e-mails" :value total-emails}
                       {:label "Categorized" :value (percentage-string categorized-emails total-emails)}
                       {:label "Detected languages" :value detected-languages}
                       {:label "MIME types" :value known-mime-types}]
             :charts [(chart-context "email-volume" "E-mail volume" "Number of stored e-mails per year."
                                     email-values (volume-chart email-values))
                      (chart-context "mime-types" "MIME types" "Message formats across stored e-mails."
                                     mime-values (breakdown-chart "E-mails by MIME type" mime-values :mime-type "MIME type"))
                      (chart-context "languages" "Languages" "Detected language across all stored e-mails."
                                     language-values (breakdown-chart "E-mails by detected language" language-values :language "Language"))
                      (chart-context "categories" "Categories" "Current category assignments, including uncategorized e-mails."
                                     category-values (breakdown-chart "E-mails by category" category-values :name "Category"))]})))

(defn- with-flag-hex [category]
  (assoc category :flag_hex (:hex (first (filter #(= (:flag_color category) (:id %)) flags/colors)))))

(defn categories-page
  ([categories] (categories-page categories nil))
  ([categories messages]
   (render "admin-categories.html" (cond-> {:categories (mapv with-flag-hex categories)
                                            :flag-colors flags/colors
                                            :active-nav :admin}
                                     (some? messages) (assoc :messages (mapv type->toast-role messages))))))

(defn languages-admin-page
  ([language-preferences] (languages-admin-page language-preferences nil))
  ([language-preferences messages]
   (render "admin-languages.html" (cond-> {:language-preferences language-preferences :active-nav :admin}
                                    (seq messages) (assoc :messages (mapv type->toast-role messages))))))

(defn connections-list
  ([connections] (render "admin-connections.html" {:configs connections :active-nav :admin}))
  ([connections messages] (render "admin-connections.html" {:configs connections :active-nav :admin :messages (mapv type->toast-role messages)})))


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
                 :active-nav :admin
                 :categories (cons nil categories)
                 :parse-batches (mapv format-parse-batch (:parse-batches config))
                 :parse-failures (mapv format-parse-failure-group (:parse-failures config))}))

(defn connection
  ([config folders categories] (render "admin-connection.html" (connection-context config folders categories)))
  ([config folders messages categories] (render "admin-connection.html" (assoc (connection-context config folders categories) :messages (mapv type->toast-role messages)))))

(defn training-progress-page
  "The live progress view of a model training run; back is the same-origin path to return to."
  [back]
  (render "training-progress.html" {:active-nav :emails :back back}))

(defn preferences-page
  ([data] (preferences-page data nil))
  ([data messages]
   (let [log-levels {:log-level-options [{:key :error :name "Error"}
                                         {:key :info :name "Info"}
                                         {:key :debug :name "Debug"}]
                     :active-nav :admin}]
     (render "admin-preferences.html"
             (cond-> (conj data log-levels)
               (seq messages) (assoc :messages (mapv type->toast-role messages)))))))

(defn new-connection [providers]
  (render "admin-new-connection.html" {:auth-providers providers}))
