(ns plauna.i18n
  "The interface texts in German and English. A text is looked up by key in resources/i18n/<lang>.edn
   for the language of the current request (*lang*, bound by the server for every request); a key
   missing in German falls back to English, one missing everywhere shows the key itself. Templates
   use the tag {% t key arg ... %}: each argument is a context path (category.name) or a \"quoted\"
   literal and fills {0}, {1} ... of the text, HTML-escaped."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [selmer.parser :as selmer]))

(def languages [:en :de])

(def ^:dynamic *lang*
  "The language of the current request."
  :en)

(defn- load-texts [lang]
  (if-let [resource (io/resource (str "i18n/" (name lang) ".edn"))]
    (edn/read-string (slurp resource))
    {}))

(def texts (delay (into {} (map (juxt identity load-texts)) languages)))

(defn- fill [text args]
  (reduce-kv (fn [acc i arg] (str/replace acc (str "{" i "}") (str arg))) text (vec args)))

(defn- text-of [k]
  (let [k (keyword (name k))]
    (or (get-in @texts [*lang* k]) (get-in @texts [:en k]) (name k))))

(defn t
  "The text of key in the current language, with {0}, {1} ... replaced by args."
  ([k] (t k nil))
  ([k args] (fill (text-of k) args)))

(defn- quality
  "The q value of one Accept-Language entry (\"de;q=0.8\" → 0.8), 1.0 without one."
  [entry]
  (or (some->> entry (re-find #";\s*q=([0-9.]+)") second parse-double) 1.0))

(defn browser-language
  "The interface language an Accept-Language header asks for: German when German is preferred over
   English, otherwise English."
  [accept-language]
  (let [entries (->> (str/split (or accept-language "") #",")
                     (map str/trim)
                     (remove str/blank?)
                     (map (fn [entry] {:tag (str/lower-case (first (str/split entry #";"))) :q (quality entry)}))
                     (sort-by :q >))
        first-known (first (filter #(or (str/starts-with? (:tag %) "de") (str/starts-with? (:tag %) "en")) entries))]
    (if (some-> first-known :tag (str/starts-with? "de")) :de :en)))

(defn request-language
  "The interface language of a request: the setting (\"de\", \"en\") or, for \"auto\", the browser's."
  [setting accept-language]
  (case setting
    "de" :de
    "en" :en
    (browser-language accept-language)))

(defn- escape-html [s]
  (-> (str s)
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")
      (str/replace "'" "&#39;")))

(defn- tag-argument [arg context-map]
  (if (and (str/starts-with? arg "\"") (str/ends-with? arg "\"") (> (count arg) 1))
    (subs arg 1 (dec (count arg)))
    (get-in context-map (mapv keyword (str/split arg #"\.")))))

(defn t-html
  "t for HTML: the text and every argument HTML-escaped, so the result is safe as element content and
   inside a double-quoted attribute. Texts never contain markup."
  [k args]
  (fill (escape-html (text-of k)) (mapv escape-html args)))

(selmer/add-tag! :t
                 (fn [args context-map]
                   (t-html (first args) (mapv #(tag-argument % context-map) (rest args)))))
