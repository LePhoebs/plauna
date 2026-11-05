(ns plauna.core.events-test
  (:require [plauna.core.events :as events]
            [clojure.core.async :as async]
            [clojure.test :as test]))

(defn- await-count
  "Wait for an asynchronous worker count without relying on a fixed scheduler delay."
  [counter expected]
  (let [deadline (+ (System/nanoTime) 2000000000)]
    (loop []
      (cond
        (= expected @counter) true
        (< (System/nanoTime) deadline) (do (Thread/sleep 10) (recur))
        :else false))))

(test/deftest return-key-on-complete-works
  (let [test-chan (async/chan)
        test-fn (fn [] (async/go (async/<! test-chan)))
        test-case (events/return-key-on-complete :test-key test-fn)]
    (async/close! test-chan)
    (test/is (= :test-key (async/<!! test-case)))))

(test/deftest keep-track-works
  (let [test-atom (atom 0)
        test-chan (atom (async/chan))
        test-fn (fn [] (async/go (swap! test-atom inc) (async/<! @test-chan)))
        test-register {:test-case test-fn}]
    (let [supervisor (events/keep-track
                      {:test-case (events/return-key-on-complete :test-case test-fn)}
                      test-register)]
      (try
        (test/is (await-count test-atom 1))
        (swap! test-chan (fn [old] (async/close! old) (async/chan)))
        (test/is (await-count test-atom 2))
        (swap! test-chan (fn [old] (async/close! old) (async/chan)))
        (test/is (await-count test-atom 3))
        (finally
          (events/stop-event-loops!)
          (async/close! @test-chan)
          (async/<!! supervisor))))))

(test/deftest event-register-works
  (let [test-atom (atom 0)
        test-chan (atom (async/chan))
        test-fn (fn [] (async/go (swap! test-atom inc) (async/<! @test-chan)))
        test-register {:test-case test-fn}]
    (let [supervisor (events/start-event-loops test-register)]
      (try
        (test/is (await-count test-atom 1))
        (swap! test-chan (fn [old] (async/close! old) (async/chan)))
        (test/is (await-count test-atom 2))
        (swap! test-chan (fn [old] (async/close! old) (async/chan)))
        (test/is (await-count test-atom 3))
        (swap! test-chan (fn [old] (async/close! old) (async/chan)))
        (test/is (await-count test-atom 4))
        (finally
          (events/stop-event-loops!)
          (async/close! @test-chan)
          (async/<!! supervisor))))))

(test/deftest options-survive-a-triggering-event-without-options
  (test/is (= {:a 1} (:options (events/create-event :x nil {:a 1} {:type :y}))))
  (test/is (= {:a 1 :b 2} (:options (events/create-event :x nil {:b 2} {:type :y :options {:a 1}})))))
