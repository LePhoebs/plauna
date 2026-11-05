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

(defn read-file
  "The settings stored in db-config.edn, without the environment overrides, or nil when there is no
   such file. A file that exists but cannot be read is an error, not a missing file: treating it as
   missing would start Plauna on SQLite - an old or empty data set - instead of the configured MariaDB."
  []
  (let [f (io/file (config-file-path))]
    (when (.exists f)
      (let [parsed (try (edn/read-string (slurp f))
                        (catch Exception e
                          (throw (ex-info (str "The database configuration " (config-file-path) " cannot be read (" (.getMessage e) "). Fix or remove it, then start Plauna again.")
                                          {:path (config-file-path)} e))))]
        (when-not (or (nil? parsed) (map? parsed))
          (throw (ex-info (str "The database configuration " (config-file-path) " must contain a map.") {:path (config-file-path)})))
        parsed))))

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
        ;; "MariaDB" or a string :type in the file must not silently mean SQLite - and neither must a
        ;; typo: only a missing type means the SQLite default.
        type-of   (fn [v source]
                    (when-let [t (some-> v name clojure.string/trim clojure.string/lower-case not-empty keyword)]
                      (if (contains? #{:sqlite :mariadb} t)
                        t
                        (throw (ex-info (str "Unknown database type \"" (name v) "\" in " source " - use mariadb or sqlite.") {:type v})))))
        db-type   (or (type-of (env-str "PLAUNA_DB_TYPE") "PLAUNA_DB_TYPE") (type-of (:type file-cfg) (config-file-path)) :sqlite)]
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
    (let [path (config-file-path)
          temp (io/file (str path ".tmp"))]
      (io/make-parents path)
      ;; Written to a temporary file and moved into place: an interrupted write must never leave a
      ;; half-written configuration behind (see read-file).
      (spit temp (pr-str cfg))
      (java.nio.file.Files/move (.toPath temp) (.toPath (io/file path))
                                (into-array java.nio.file.CopyOption [java.nio.file.StandardCopyOption/REPLACE_EXISTING
                                                                      java.nio.file.StandardCopyOption/ATOMIC_MOVE]))
      (t/log! :info ["MariaDB configuration saved to" path]))))

(defn mariadb-configured?
  "True if a MariaDB host is recorded (file or env), regardless of current active DB."
  []
  (= :mariadb (:type (load-config))))
