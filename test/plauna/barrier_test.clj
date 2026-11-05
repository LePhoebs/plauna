(ns plauna.barrier-test
  "The barrier the SQLite → MariaDB copy waits for, through the real parser, enrichment and database
   loops: it is confirmed only after a save that was in progress has finished."
  (:require [clojure.test :refer :all]
            [clojure.core.async :as async]
            [plauna.analysis :as analysis]
            [plauna.database :as db]
            [plauna.messaging :as messaging]
            [plauna.parser :as parser]
            [taoensso.telemere :as t]))

(t/set-ns-filter! {:disallow "plauna.*"})

(defn- with-bus
  "Run f with a fresh event bus and the three real loops on it; the global bus is restored afterwards."
  [f]
  (let [old-chan @messaging/main-chan
        old-pub @messaging/main-publisher
        bus (async/chan 1000)
        publisher (async/pub bus :type)]
    (dosync (ref-set messaging/main-chan bus) (ref-set messaging/main-publisher publisher))
    (try
      (parser/parser-event-loop publisher bus)
      (analysis/enrichment-event-loop publisher bus)
      (db/database-event-loop publisher)
      (f bus)
      (finally
        (async/close! bus)
        (dosync (ref-set messaging/main-chan old-chan) (ref-set messaging/main-publisher old-pub))))))

(deftest the-barrier-waits-for-a-save-in-progress
  (let [release (promise)
        saved (atom [])]
    (with-redefs-fn {#'db/save-buffer-logging-errors! (fn [buffer]
                                                       @release
                                                       (swap! saved conj (mapv :message-id (:headers buffer))))}
      (fn []
        (with-bus
          (fn [bus]
            (async/>!! bus {:type :parsed-email :payload {:header {:message-id "<queued@x>"} :body [] :participants []}})
            (let [confirmed (future (messaging/barrier! 10000))]
              (Thread/sleep 500)
              (is (not (realized? confirmed)) "Not confirmed while the save is blocked")
              (deliver release true)
              (is (= {:ok true} (deref confirmed 10000 :timeout)) "Confirmed once the save has finished")
              (is (= [["<queued@x>"]] @saved) "The e-mail queued before the barrier was saved first"))))))))

(deftest a-save-that-does-not-end-means-no-copy
  (let [release (promise)]
    (with-redefs-fn {#'db/save-buffer-logging-errors! (fn [_] @release)}
      (fn []
        (with-bus
          (fn [bus]
            (async/>!! bus {:type :parsed-email :payload {:header {:message-id "<stuck@x>"} :body [] :participants []}})
            (is (= {:ok false :timeout true} (messaging/barrier! 700)) "Not confirmed in time: the migration does not copy")
            (deliver release true)))))))

(deftest a-failed-save-is-reported-not-confirmed
  (with-redefs-fn {#'db/save-emails-in-buffer (fn [_] (throw (ex-info "rejected by the database" {})))}
    (fn []
      (with-bus
        (fn [bus]
          (async/>!! bus {:type :parsed-email :payload {:header {:message-id "<lost@x>"} :body [] :participants []}})
          (is (= {:ok false :failed ["<lost@x>"]} (messaging/barrier! 10000))
              "The barrier names the e-mail that was not saved instead of confirming")
          (is (= {:ok true} (messaging/barrier! 10000))
              "A failure is reported once; the next barrier reports what happened after it"))))))

(deftest a-batch-that-is-saved-partly-reports-the-rejected-e-mail
  (with-redefs-fn {#'db/save-emails-in-buffer (fn [buffer]
                                                (when (or (> (count (:headers buffer)) 1)
                                                          (= "<bad@x>" (:message-id (first (:headers buffer)))))
                                                  (throw (ex-info "rejected" {}))))}
    (fn []
      (is (= ["<bad@x>"]
             (#'db/save-buffer-logging-errors!
              (->> (db/empty-buffer)
                   (db/add-to-buffer {:header {:message-id "<good@x>"} :body [] :participants []})
                   (db/add-to-buffer {:header {:message-id "<bad@x>"} :body [] :participants []}))))
          "The batch failed, the retry saved one e-mail: the other one is reported"))))
