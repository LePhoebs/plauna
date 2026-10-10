(ns plauna.entry
  (:require
   [clojure.core.async :as async]
   [plauna.analysis :as analysis]
   [plauna.application :as app]
   [plauna.auth :as auth]
   [plauna.client :as client]
   [plauna.core.email :as core.email]
   [plauna.core.events :as events]
   [plauna.database :as db]
   [plauna.db-config :as db-cfg]
   [plauna.settings :as settings]
   [plauna.diagnostics :as diagnostics]
   [plauna.files :as files]
   [plauna.messaging :as messaging]
   [plauna.parser :as parser]
   [plauna.preferences :as preferences]
   [plauna.server :as server]
   [taoensso.telemere :as t])
  (:import [plauna.client ImapClient]
           [plauna.database SqliteDB]
           [plauna.analysis BasicAnalyzer])
  (:gen-class))

(defn setup-logging []
  (t/set-min-level! :info)
  ;; jetty is very noisy. Disable all jetty logs.
  (t/set-ns-filter! {:disallow "org.eclipse.jetty.*"})
  (diagnostics/install-uncaught-exception-handler!))

(set! *warn-on-reflection* true)

(defonce event-loop-workers (atom {}))

(defn- track-event-worker [key start-fn]
  (let [worker (start-fn)]
    (swap! event-loop-workers assoc key worker)
    worker))

(def event-register {:enrichment-event-loop (fn []
                                               (track-event-worker
                                                :enrichment-event-loop
                                                #(analysis/enrichment-event-loop @messaging/main-publisher @messaging/main-chan)))
                     ;:client-event-loop (fn [] (client/client-event-loop @messaging/main-publisher))
                     :database-event-loop (fn []
                                            (track-event-worker
                                             :database-event-loop
                                             #(db/database-event-loop @messaging/main-publisher)))
                     :parser-event-loop (fn []
                                          (track-event-worker
                                           :parser-event-loop
                                           #(parser/parser-event-loop @messaging/main-publisher @messaging/main-chan)))})

(defn- await-event-workers! []
  (doseq [[key worker] @event-loop-workers]
    (let [[result port] (async/alts!! [worker (async/timeout 10000)])]
      (if (= port worker)
        (t/log! :info ["Event worker" key "stopped with" result])
        (t/log! :warn ["Timed out waiting for event worker" key "to stop."]))))
  (reset! event-loop-workers {}))

(defn- connect-and-prepare!
  "Connect one stored connection and create the category folders on it. Returns true when it is
   connected. Failures are logged, never propagated: one unreachable or misbehaving mail server must
   not keep the web server and the event loops from starting."
  [context id]
  (try
    (let [connection-result (app/connect-to-client context id)]
      (if (= :ok (:result connection-result))
        (do (client/create-category-folders! (get @client/connections id) (mapv :name (db/get-categories)))
            true)
        (do (t/log! :info ["Connection" id "is not connected; not creating its category folders."])
            false)))
    (catch Exception e
      (t/log! {:level :error :error e} ["Connecting to" id "failed."])
      false)))

(defn start-imap-client
  [context]
  (let [connections-in-db (db/get-connections)]
    (if (seq connections-in-db)
      (do (t/log! :debug ["Connections table contains" (count connections-in-db) "connection configuration(s)."])
          (doseq [client-config connections-in-db]
            (connect-and-prepare! context (:id client-config))))
      (do (t/log! :debug "Connections table in the db is empty. Trying to read connections from the config file.")
          (doseq [client-config (:clients (-> context :config :email))]
            (t/log! :info ["Adding connection data from the config file to the database. Next time Plauna will use the data from the database."])
            (try
              (let [connection-with-id (core.email/construct-imap-connection-from-config-file (conj client-config {:id (client/id-from-config client-config)}))]
                (db/add-connection connection-with-id)
                (connect-and-prepare! context (:id connection-with-id)))
              (catch Exception e
                (t/log! {:level :error :error e} ["Could not add the connection from the config file:" (dissoc client-config :secret)])))))))
  (t/log! :debug "Listening to new emails from listen-channel"))

;; ── Connection retries ─────────────────────────────────────────────────────────
;; A connection that could not be established at startup (the mail server was still booting, DNS was
;; not ready) has no ConnectionData and therefore no health check that would ever reconnect it. This
;; task retries such connections periodically, so a restart of Plauna and its mail server in the wrong
;; order heals by itself.

(defonce ^:private connection-retry-scheduler (atom nil))

(def connection-retry-interval-seconds 120)

(def connection-retry-max-backoff-seconds
  "Longest pause between two retries of the same connection. The pause doubles after every failed
   attempt (2, 4, 8 ... minutes), so a wrong password or a revoked token does not hammer the provider
   every two minutes forever - providers lock accounts for that - while a server that is merely down
   is still tried again within the hour."
  3600)

(defonce ^:private connection-retry-state
  ;; {connection-id {:attempts n :next-at epoch-millis}}
  (atom {}))

(defn next-retry-delay-seconds
  "The pause before retry number attempts (1-based): the interval doubled per earlier failure, capped."
  [attempts]
  (min connection-retry-max-backoff-seconds
       (* connection-retry-interval-seconds (long (Math/pow 2 (dec (max 1 attempts)))))))

(defn- due-for-retry? [id now]
  (let [{:keys [next-at]} (get @connection-retry-state id)]
    (or (nil? next-at) (>= now (long next-at)))))

(defn- note-retry-outcome! [id connected? now]
  (if connected?
    (swap! connection-retry-state dissoc id)
    (swap! connection-retry-state update id
           (fn [{:keys [attempts]}]
             (let [attempts (inc (long (or attempts 0)))]
               {:attempts attempts :next-at (+ now (* 1000 (next-retry-delay-seconds attempts)))})))))

(defn- retry-unconnected-connections!
  "Connect every stored connection that has no live registration and whose back-off pause is over -
   except OAuth connections that still need the manual login (they would only produce a warning every
   time)."
  [context]
  (let [now (System/currentTimeMillis)]
    (doseq [connection (db/get-connections)
            :let [id (:id connection)]
            :when (nil? (client/connection-data-from-id id))
            :when (due-for-retry? id now)
            :when (or (not= "oauth2" (:auth-type connection))
                      (some? (:refresh-token (db/get-oauth-tokens id))))]
      (t/log! :info ["Retrying the connection to" (:host connection) "as" (:user connection)])
      (let [connected? (connect-and-prepare! context id)]
        (note-retry-outcome! id connected? now)
        (when connected?
          (client/forget-disconnected-cache!))))))

(defn start-connection-retries! [context]
  (when (nil? @connection-retry-scheduler)
    (let [executor (java.util.concurrent.Executors/newSingleThreadScheduledExecutor
                    (reify java.util.concurrent.ThreadFactory
                      (newThread [_ runnable]
                        (doto (Thread. ^Runnable runnable "plauna-connection-retry") (.setDaemon true)))))]
      (.scheduleWithFixedDelay executor
                               ^Runnable (fn []
                                           (try (retry-unconnected-connections! context)
                                                (catch Throwable e
                                                  (t/log! {:level :error :error e} "Retrying unconnected connections failed."))))
                               (long connection-retry-interval-seconds)
                               (long connection-retry-interval-seconds)
                               java.util.concurrent.TimeUnit/SECONDS)
      (reset! connection-retry-scheduler executor))))

(defn stop-connection-retries! []
  (when-let [^java.util.concurrent.ScheduledExecutorService executor @connection-retry-scheduler]
    (.shutdownNow executor)
    (reset! connection-retry-scheduler nil)))

(defn- register-shutdown-hook!
  "Ensure automatic training, IMAP connections, the web server, and the watchdog are torn down cleanly on SIGTERM
   (e.g. `docker stop`) instead of the JVM being killed mid-flight."
  []
  (.addShutdownHook
   (Runtime/getRuntime)
   (Thread.
    ^Runnable (fn []
                (t/log! :info "Shutdown signal received. Stopping Plauna gracefully.")
                (doseq [[label teardown]
                        [["automatic training" server/stop-training-scheduler!]
                         ["connection retries" stop-connection-retries!]
                         ["web server" server/stop-server]
                         ["IMAP connections" client/disconnect-all]
                         ["IMAP health checks" client/stop-health-checks!]
                         ["event-loop supervisor" events/stop-event-loops!]
                         ["event input" messaging/stop!]
                         ["event workers" await-event-workers!]
                         ["database pool" db/close-pool!]
                         ["watchdog" diagnostics/stop-watchdog!]]]
                  (try (teardown)
                       (catch Throwable e
                         (t/log! {:level :error :error e} ["Error while stopping" label]))))))))

(defn -main
  [& args]
  (setup-logging)
  (register-shutdown-hook!)
  (let [application-config (files/parse-config-from-cli-arguments args)
        db-config (db-cfg/load-config)]
    (try (db/setup-db! db-config)
         (catch Exception e
           ;; Only reached with PLAUNA_DB_STARTUP_TIMEOUT set; exit so a restart policy can try again.
           (t/log! {:level :error :error e} "MariaDB could not be reached within the startup timeout. Exiting.")
           (System/exit 1)))
    (when (= :sqlite (db/db-type))
      (files/check-and-create-database-file))
    (let [context {:config application-config :client (ImapClient.) :db (SqliteDB.)
                   :db-type (db/db-type)
                   :fulltext-min-token-length (db/fulltext-min-token-length)
                   :analyzer (BasicAnalyzer.)}]
      (db/create-db)
      ;; A folder parse that was still running when Plauna last stopped cannot resume; do not show it as running.
      (db/abort-running-parse-batches!)
      (let [db-vals (into {} (map #(vector % (db/fetch-preference %))
                                  [:log-level :language-detection-threshold
                                   :categorization-threshold :client-health-check-interval
                                   :categorization-algorithm]))]
        (when (settings/migrate-from-db-values! db-vals)
          (t/log! :info "Preferences migrated to settings.json.")))
      (auth/initialize!)
      (t/log! :info "Setting log level according to preferences.")
      (t/set-min-level! (preferences/log-level))
      (diagnostics/start-watchdog! 60)
      (start-imap-client context)
      (start-connection-retries! context)
      (events/start-event-loops event-register)
      (server/start-server context)
      (server/start-training-scheduler!))))

(comment
  (server/start-server {:config {:server {:port 8080}}})
  (server/start-training-scheduler!)
  (server/stop-training-scheduler!)
  (server/stop-server)
  (require '[flow-storm.api :as fs-api])
  (fs-api/local-connect)
  (client/disconnect-all))
