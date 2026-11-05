(ns plauna.messaging
  (:require [clojure.core.async :refer [chan pub sub <! go-loop close!] :as async]))

(set! *warn-on-reflection* true)

(def main-chan (ref (chan 1000)))

(def main-publisher (ref (pub @main-chan :type)))

(defn restart-main-chan [] (dosync (alter main-chan (fn [old-chan] (close! old-chan)))
                                   (alter main-chan (fn [_] (chan 1000)))
                                   (alter main-publisher (fn [_] (pub @main-chan :type)))))

(defn stop! []
  (close! @main-chan))
(def limiter-limit (ref 300))

(defn channel-limiter
  "Backpressure for a producing operation: the producer puts a token on :bucket before each event it
   sends, and a go-loop drains one token per published event of the target type(s), so the producer
   can never run more than the bucket size ahead of the consumers. target-types is one event type or
   a set of them: a producer whose input can be DROPPED downstream (an unparseable mbox message never
   becomes a :parsed-enrichable-email) must also listen to the event the consumer emits for a drop,
   otherwise the orphaned tokens fill the bucket and the producer blocks forever.
   Returns a handle {:bucket ... :target ... :types ...}. The producer MUST call close-limiter! when
   it finishes (try/finally): the subscription and its go-loop otherwise leak, and every future event
   of these types keeps being delivered to the abandoned subscriber."
  [target-types]
  (let [types (if (set? target-types) target-types #{target-types})
        bucket-channel (chan @limiter-limit)
        target-channel (chan)]
    (doseq [target-type types]
      (sub @main-publisher target-type target-channel))
    (go-loop []
      (when-some [_ (<! target-channel)]
        ;; Consume a token without blocking: pub/mult only deliver the next event once ALL
        ;; subscribers accepted, so a blocking take on an empty bucket could park the whole topic.
        ;; The producer puts its token before its event, so the token is normally already here.
        (async/poll! bucket-channel)
        (recur)))
    {:bucket bucket-channel :target target-channel :types types}))

(defn close-limiter!
  "Unsubscribe and close a limiter created by channel-limiter, ending its go-loop. Events published
   after this simply skip the limiter; any in-flight ones no longer need tokens."
  [{:keys [bucket target types]}]
  (doseq [target-type types]
    (async/unsub @main-publisher target-type target))
  (close! target)
  (close! bucket))

(defn barrier-event?
  "True for a barrier event (see barrier!)."
  [event]
  (= ::barrier (:payload event)))

(def ^:private barrier-entry-types
  "Where e-mails enter the event bus: the mbox import (:received-email, then parser and enrichment),
   the language re-detection (:language-detection-request, then enrichment) and the parsed-only path
   (:parsed-email). Every stage forwards a barrier in order (core.async pipelines keep the input
   order), and the database loop answers it once it has saved everything that came before."
  [:received-email :language-detection-request :parsed-email])

(defn barrier!
  "Send a barrier through every path of the event bus and wait until the database loop answered each
   one: then every e-mail put on the bus before has been parsed, enriched and saved - or dropped on the
   way, or failed to save. Returns {:ok true} when everything was saved, {:ok false :failed [ids]} with
   the Message-IDs the database loop could not save since its last barrier, or {:ok false :timeout true}
   when not every answer came within timeout-millis (a stalled bus, a write that does not end)."
  [timeout-millis]
  (let [deadline (+ (System/currentTimeMillis) (long timeout-millis))
        remaining #(max 1 (- deadline (System/currentTimeMillis)))
        confirmations (vec (for [entry-type barrier-entry-types]
                             (let [confirmed (promise)
                                   sent (async/alt!! [[@main-chan {:type entry-type :payload ::barrier :options {:barrier confirmed}}]] true
                                                     (async/timeout (remaining)) false)]
                               (when sent confirmed))))]
    (let [answers (when (every? some? confirmations)
                    (mapv #(deref % (remaining) nil) confirmations))]
      (cond
        (or (nil? answers) (some nil? answers)) {:ok false :timeout true}
        :else (let [failed (vec (distinct (mapcat :failed answers)))]
                (if (seq failed) {:ok false :failed failed} {:ok true}))))))

(comment
  (restart-main-chan))
