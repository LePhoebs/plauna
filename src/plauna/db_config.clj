(ns plauna.db-config
  "Reads and writes the database configuration from {data-dir}/db-config.edn.
   Environment variables override the file; the file overrides the built-in SQLite default.
   The config file must live outside the database (no chicken-and-egg problem)."
  (:require [clojure.java.io :as io]
            [clojure.tools.reader.edn :as edn]
            [plauna.files :as files]
            [taoensso.telemere :as t]))

(def ^:private config-filename "db-config.edn")

(defn config-file-path []
  (str (io/file (files/file-dir) config-filename)))

(defn- read-file []
  (let [f (io/file (config-file-path))]
    (when (.exists f)
      (try (edn/read-string (slurp f))
           (catch Exception e
             (t/log! :warn ["Could not parse db-config.edn, using defaults:" (.getMessage e)])
             nil)))))

(defn- env-str [k] (let [v (System/getenv k)] (when-not (clojure.string/blank? v) v)))

(defn load-config
  "Returns the effective DB config map, with env vars taking precedence over the file.
   Result is one of:
     {:type :sqlite}
     {:type :mariadb :host ... :port ... :name ... :user ... :password ... :startup-timeout-seconds ... :ssl-mode ...}
   :startup-timeout-seconds (PLAUNA_DB_STARTUP_TIMEOUT) limits how long startup waits for MariaDB;
   nil means wait indefinitely. :ssl-mode (PLAUNA_DB_SSL_MODE) is the driver's sslMode, see
   plauna.database/default-mariadb-ssl-mode."
  []
  (let [file-cfg  (or (read-file) {})
        db-type   (or (some-> (env-str "PLAUNA_DB_TYPE") keyword) (:type file-cfg) :sqlite)]
    (if (= :mariadb db-type)
      {:type     :mariadb
       :host     (or (env-str "PLAUNA_DB_HOST")     (:host file-cfg)     "localhost")
       :port     (or (some-> (env-str "PLAUNA_DB_PORT") Integer/parseInt) (:port file-cfg) 3306)
       :name     (or (env-str "PLAUNA_DB_NAME")     (:name file-cfg)     "plauna")
       :user     (or (env-str "PLAUNA_DB_USER")     (:user file-cfg)     "plauna")
       :password (or (env-str "PLAUNA_DB_PASSWORD") (:password file-cfg) "")
       :startup-timeout-seconds (or (some-> (env-str "PLAUNA_DB_STARTUP_TIMEOUT") Integer/parseInt)
                                    (:startup-timeout-seconds file-cfg))
       :ssl-mode (or (env-str "PLAUNA_DB_SSL_MODE") (:ssl-mode file-cfg))}
      {:type :sqlite})))

(defn save-config!
  "Persist a MariaDB config map to disk. Never called for SQLite (no file needed)."
  [{:keys [type] :as cfg}]
  (when (= :mariadb type)
    (let [path (config-file-path)]
      (io/make-parents path)
      (spit path (pr-str cfg))
      (t/log! :info ["MariaDB configuration saved to" path]))))

(defn mariadb-configured?
  "True if a MariaDB host is recorded (file or env), regardless of current active DB."
  []
  (= :mariadb (:type (load-config))))
