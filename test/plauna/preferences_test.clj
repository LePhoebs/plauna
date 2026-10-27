(ns plauna.preferences-test
  (:require [plauna.preferences :as sut]
            [clojure.core.cache :as c]
            [clojure.test :as t]))

(defn return-fn-for-preferences [returns] (swap! sut/fetch-fn (fn [_] (fn [_] returns))))

(t/deftest fetch-returns-default
  (swap! sut/cache (fn [_] (c/ttl-cache-factory {} :ttl 1)))
  (return-fn-for-preferences nil)
  (t/is (= (sut/log-level) :info)))

(t/deftest fetch-returns-string
  (swap! sut/cache (fn [_] (c/ttl-cache-factory {} :ttl 1)))
  (return-fn-for-preferences ":debug")
  (t/is (= (sut/log-level) :debug)))

(t/deftest fetch-returns-double
  (swap! sut/cache (fn [_] (c/ttl-cache-factory {} :ttl 1)))
  (return-fn-for-preferences "0.01")
  (t/is (= (sut/categorization-threshold) 0.01)))

(t/deftest categorization-model-supports-new-and-legacy-setting-values
  (t/is (= "naive-bayes" (sut/canonical-categorization-model nil)))
  (t/is (= "naive-bayes" (sut/canonical-categorization-model "NAIVEBAYES")))
  (t/is (= "maxent" (sut/canonical-categorization-model "MAXENT")))
  (t/is (= "maxent-qn" (sut/canonical-categorization-model "MAXENT_QN")))
  (t/is (= "maxent-qn" (sut/canonical-categorization-model "maxent-qn"))))

(t/deftest scheduled-training-preferences-return-validated-string-values
  (swap! sut/cache (fn [_] (c/ttl-cache-factory {} :ttl 1)))
  (return-fn-for-preferences "03:30")
  (t/is (= "03:30" (sut/automatic-training-time)))
  (swap! sut/cache (fn [_] (c/ttl-cache-factory {} :ttl 1)))
  (return-fn-for-preferences "Europe/Berlin")
  (t/is (= "Europe/Berlin" (sut/time-zone)))
  (t/is (= (java.time.ZoneId/of "Europe/Berlin") (sut/zone-id))))

(t/deftest category-folder-scan-interval-defaults-to-five-minutes-and-can-be-turned-off
  (swap! sut/cache (fn [_] (c/ttl-cache-factory {} :ttl 1)))
  (return-fn-for-preferences nil)
  (t/is (= 300 (sut/category-folder-scan-interval)))
  (swap! sut/cache (fn [_] (c/ttl-cache-factory {} :ttl 1)))
  (return-fn-for-preferences "0")
  (t/is (= 0 (sut/category-folder-scan-interval)) "0 is a value, not a missing setting"))
