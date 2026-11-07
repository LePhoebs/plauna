(ns plauna.i18n-test
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer :all]
            [plauna.i18n :as i18n]
            [selmer.parser :as selmer]))

(defn- texts [lang] (edn/read-string (slurp (io/resource (str "i18n/" (name lang) ".edn")))))

(deftest both-languages-have-the-same-texts
  (let [en (set (keys (texts :en)))
        de (set (keys (texts :de)))]
    (is (empty? (set/difference en de)) "English texts without a German translation")
    (is (empty? (set/difference de en)) "German texts without an English original")))

(deftest every-text-a-template-uses-exists
  (let [en (set (map name (keys (texts :en))))
        used (->> (file-seq (io/file "resources/templates"))
                  (filter #(.isFile ^java.io.File %))
                  (mapcat #(re-seq #"\{% t ([a-z0-9.-]+)" (slurp %)))
                  (map second)
                  set)]
    (is (seq used))
    (is (empty? (set/difference used en)) "Templates use texts that do not exist")))

(deftest texts-are-filled-in-and-fall-back-to-english
  (is (= "Review e-mails (3)" (i18n/t :suggest.review [3])))
  (binding [i18n/*lang* :de]
    (is (= "E-Mails ansehen (3)" (i18n/t :suggest.review [3])))
    (is (= "no.such.text" (i18n/t :no.such.text)) "An unknown key shows itself")))

(deftest the-browser-decides-the-language-unless-it-is-fixed
  (is (= :de (i18n/browser-language "de-DE,de;q=0.9,en;q=0.8")))
  (is (= :en (i18n/browser-language "en-GB,en;q=0.9,de;q=0.8")))
  (is (= :de (i18n/browser-language "fr-FR,de;q=0.7,en;q=0.5")) "The first known language counts")
  (is (= :de (i18n/browser-language "en;q=0.5,de;q=0.9")) "Ordered by quality, not by position")
  (is (= :en (i18n/browser-language nil)))
  (is (= :en (i18n/request-language "en" "de-DE")))
  (is (= :de (i18n/request-language "de" "en-US")))
  (is (= :de (i18n/request-language "auto" "de-DE"))))

(deftest the-template-tag-escapes-its-arguments
  (is (= "Suggested for &lt;b&gt;X&lt;/b&gt; (same sender)"
         (selmer/render "{% t list.suggested-sender category.name %}" {:category {:name "<b>X</b>"}})))
  (is (= "Review e-mails (5)" (selmer/render "{% t suggest.review \"5\" %}" {}))))
