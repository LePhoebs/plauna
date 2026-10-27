(ns plauna.preferences
  (:require
   [clojure.core.cache.wrapped :as w]
   [plauna.settings :as settings]))

(def cache (w/ttl-cache-factory {} :ttl 6000))

(def fetch-fn (atom settings/fetch-setting))

(def converters {clojure.lang.Keyword (fn [^String s] (keyword (cond-> s (.startsWith s ":") (.substring 1))))
                 java.lang.Double (fn [v] (if (instance? Number v) (double v) (Double/parseDouble (str v))))
                 java.lang.Long   (fn [v] (if (instance? Number v) (long v)   (Long/parseLong (str v))))})

(defmacro preference-with-default [property pred default]
  `(let [value# (~pred (@fetch-fn ~property) ~default)
         default-type# (class ~default)
         type# (class value#)]
     (if (= default-type# type#)
       value#
       ((get converters default-type#) value#))))

(defn update-preference [key value]
  (settings/update-setting! key value)
  (w/evict cache key))

(defn log-level [] (w/lookup-or-miss cache
                                     :log-level
                                     (fn [key] (preference-with-default key or :info))))

(defn language-detection-threshold [] (w/lookup-or-miss cache
                                                        :language-detection-threshold
                                                        (fn [key] (preference-with-default key or 0.80))))

(defn categorization-threshold [] (w/lookup-or-miss cache
                                                    :categorization-threshold
                                                    (fn [key] (preference-with-default key or 0.65))))

(defn canonical-categorization-model
  "Normalise both Plauna's stable setting names and the OpenNLP constants stored by older versions."
  [value]
  (case (some-> value str clojure.string/lower-case)
    ("maxent" "max-ent") "maxent"
    ("maxent-qn" "maxent_qn" "maxentqn" "max-ent-qn" "lbfgs" "l-bfgs") "maxent-qn"
    ("naivebayes" "naive-bayes" "naive_bayes") "naive-bayes"
    "naive-bayes"))

(defn categorization-model []
  (w/lookup-or-miss cache
                    :categorization-algorithm
                    (fn [key] (canonical-categorization-model (@fetch-fn key)))))

(defn client-health-check-interval [] (w/lookup-or-miss cache
                                                        :client-health-check-interval
                                                        (fn [key] (preference-with-default key or 60))))

(defn category-folder-scan-interval
  "Seconds between two scans of the category folders for e-mails moved there in a mail client; 0 turns
   the scan off."
  [] (w/lookup-or-miss cache
                       :category-folder-scan-interval
                       (fn [key] (preference-with-default key or 300))))

(defn automatic-training-time [] (w/lookup-or-miss cache
                                                   :automatic-training-time
                                                   (fn [key] (preference-with-default key or "02:00"))))

(defn time-zone [] (w/lookup-or-miss cache
                                    :time-zone
                                    (fn [key] (preference-with-default key or
                                                                       (.getId (java.time.ZoneId/systemDefault))))))

(defn zone-id []
  (try
    (java.time.ZoneId/of (time-zone))
    (catch Exception _ (java.time.ZoneId/systemDefault))))

(defn session-generation
  "Increases whenever the web credentials change; sessions remember the generation they logged in with,
   so a bump ends every session established with the old credentials. Anything that is not a whole
   number counts as generation 0."
  []
  (w/lookup-or-miss cache
                    :session-generation
                    (fn [key]
                      (let [value (@fetch-fn key)]
                        (cond (number? value) (long value)
                              :else (try (Long/parseLong (str value)) (catch Exception _ 0)))))))

(defn bump-session-generation! []
  (update-preference :session-generation (inc (long (session-generation)))))

(defn last-successful-training-at []
  (some-> (settings/fetch-setting :last-successful-training-at) str java.time.Instant/parse))

(defn record-successful-training! [instant]
  (settings/update-setting! :last-successful-training-at (str instant)))
