(ns plauna.db-migration
  "One-shot data migration from a SQLite file to a configured MariaDB database.
   Reads from SQLite directly (regardless of the current active datasource), runs
   Flyway on the target MariaDB to create the schema, then copies every table in
   FK-safe order using ON DUPLICATE KEY UPDATE col = col so partial re-runs are idempotent.
   Rows are streamed from SQLite and inserted in chunks: a whole table (the bodies of every
   e-mail!) is never held in memory, and one statement carries many rows instead of one."
  (:require [clojure.string :as string]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs :refer [as-unqualified-lower-maps]]
            [plauna.database :as db]
            [plauna.db-config :as db-cfg]
            [plauna.files :as files]
            [taoensso.telemere :as t])
  (:import (org.flywaydb.core Flyway)
           (com.zaxxer.hikari HikariDataSource HikariConfig)))

(set! *warn-on-reflection* true)

;; Tables in insertion order — parents before children. Every table both migration sets create must be
;; listed here, or its data silently stays behind in SQLite.
(def migration-order
  ["preferences" "contacts" "categories" "category_training_preferences"
   "auth_providers" "headers" "connections" "bodies" "communications"
   "metadata" "oauth_tokens" "parse_batches" "parse_batch_emails" "parse_failures"
   "category_folder_watermarks"
   "training_tokens"])

(def insert-batch-rows 200)

(def insert-batch-chars
  "Characters of row data per INSERT statement: MariaDB refuses statements above max_allowed_packet
   (16 MB by default), and bodies rows can be large."
  (* 3 1024 1024))

(defn sqlite-ds
  ([] (sqlite-ds (files/path-to-db-file)))
  ([path]
   (jdbc/get-datasource
    {:jdbcUrl (str "jdbc:sqlite:" path "?busy_timeout=10000&journal_mode=WAL")})))

(defn- mariadb-ds ^HikariDataSource [{:keys [user password] :as cfg}]
  (let [hcfg (HikariConfig.)]
    (.setJdbcUrl hcfg (db/mariadb-jdbc-url cfg))
    (.setUsername hcfg user)
    (.setPassword hcfg (or password ""))
    (.setMaximumPoolSize hcfg 5)
    (.setConnectionTimeout hcfg 15000)
    (HikariDataSource. hcfg)))

(defn- run-flyway! [^HikariDataSource pool]
  (.migrate
   (.load (doto (Flyway/configure)
            (.dataSource pool)
            (.locations ^"[Ljava.lang.String;"
                        (into-array String ["classpath:db/migration/mariadb"]))))))

(defn insert-sql
  "A multi-row INSERT for table with the given columns. `col = col` on a duplicate key is a true no-op:
   the row is not mutated, but non-duplicate errors (type mismatch, FK violation) still propagate so
   integrity is enforced."
  [table cols row-count]
  (let [placeholders (str "(" (string/join ", " (repeat (count cols) "?")) ")")]
    (str "INSERT INTO " table
         " (" (string/join ", " cols) ")"
         " VALUES " (string/join ", " (repeat row-count placeholders))
         " ON DUPLICATE KEY UPDATE " (first cols) " = " (first cols))))

(defn row-values [cols row]
  (map #(get row (keyword %)) cols))

(def ^:private source-row-options
  ;; Unqualified, lower-case column keys. next.jdbc builds plan rows with the builder given to `plan`
  ;; itself (datafiable-row ignores a builder in its own opts), so the SAME options go to both calls.
  {:builder-fn as-unqualified-lower-maps})

(defn source-rows
  "Stream the rows of table from ds as plain maps with unqualified lower-case keys, without holding
   the table in memory: `reduce` over the result to process them."
  [ds table]
  (eduction (map (fn [row] (rs/datafiable-row row ds source-row-options)))
            (jdbc/plan ds [(str "SELECT * FROM " table)] source-row-options)))

(defn- insert-rows!
  "Insert rows (all with the same columns) with one statement. When that fails, fall back to one
   statement per row so a single rejected row is logged and skipped instead of the whole chunk."
  [dst-ds table cols rows]
  (try
    (jdbc/execute! dst-ds (into [(insert-sql table cols (count rows))] (mapcat #(row-values cols %) rows)))
    (catch Exception e
      (if (= 1 (count rows))
        (t/log! :warn ["Failed row in" table ":" (.getMessage e)])
        (do (t/log! :warn ["A chunk of" (count rows) "rows of" table "was rejected (" (.getMessage e) "); inserting them one by one."])
            (doseq [row rows]
              (insert-rows! dst-ds table cols [row])))))))

(defn- table-count [ds table]
  (long (or (:count (jdbc/execute-one! ds [(str "SELECT COUNT(*) AS count FROM " table)] {:builder-fn as-unqualified-lower-maps})) 0)))

(defn table-counts
  "Row counts of the given tables in ds, as {table count}. A table that cannot be counted (missing in
   an old schema) maps to nil."
  [ds tables]
  (into {} (for [table tables]
             [table (try (table-count ds table) (catch Exception _ nil))])))

(defn- row-chars [row]
  (reduce + 0 (map #(count (str %)) (vals row))))

(defn- migrate-table!
  "Stream every row of table from SQLite into MariaDB. Returns {:total :inserted :skipped}; inserted is
   measured as the growth of the destination table, so re-runs report duplicates as skipped."
  [src-ds ^HikariDataSource dst-ds table]
  (t/log! :info ["Migrating" table])
  (let [before (table-count dst-ds table)
        total (atom 0)
        flush! (fn [rows]
                 (when (seq rows)
                   (insert-rows! dst-ds table (map name (keys (first rows))) rows)))
        pending (reduce (fn [{:keys [rows chars]} m]
                          (let [rows (conj rows m)
                                chars (+ chars (row-chars m))]
                            (swap! total inc)
                            (if (or (>= (count rows) insert-batch-rows) (>= chars insert-batch-chars))
                              (do (flush! rows) {:rows [] :chars 0})
                              {:rows rows :chars chars})))
                        {:rows [] :chars 0}
                        (source-rows src-ds table))]
    (flush! (:rows pending))
    (let [n @total
          inserted (max 0 (- (table-count dst-ds table) before))
          skipped (max 0 (- n inserted))]
      (if (pos? skipped)
        (t/log! :warn ["Table" table ":" skipped "of" n "rows were skipped (duplicates or errors). Inserted:" inserted])
        (t/log! :info ["Table" table ":" inserted "of" n "rows inserted."]))
      {:total n :inserted inserted :skipped skipped})))

(defn migrate!
  "Run the full SQLite → MariaDB migration.
   Returns {:ok true :counts {table n ...}} or {:ok false :error message}."
  []
  (let [cfg (db-cfg/load-config)]
    (if (not= :mariadb (:type cfg))
      {:ok false :error "No MariaDB configuration found. Save a MariaDB config first."}
      (let [src (sqlite-ds)]
        (try
          (t/log! :info "Starting SQLite → MariaDB migration.")
          (with-open [dst (mariadb-ds cfg)]
            (t/log! :info "Running Flyway on MariaDB target.")
            (run-flyway! dst)
            (let [counts       (into {} (for [t migration-order]
                                          [t (migrate-table! src dst t)]))
                  total-skip   (reduce + (map #(:skipped (val %)) counts))]
              (t/log! :info ["Migration complete. Total skipped rows:" total-skip "Counts:" counts])
              {:ok true :counts counts :skipped-total total-skip}))
          (catch Exception e
            (t/log! :error ["Migration failed:" (.getMessage e)])
            {:ok false :error (.getMessage e)}))))))

(defn test-connection!
  "Try to open one connection to the configured MariaDB. Returns {:ok true} or {:ok false :error message}."
  [{:keys [host port name user password]}]
  (try
    (with-open [ds (mariadb-ds {:host host :port port :name name :user user :password password})]
      (jdbc/execute-one! ds ["SELECT 1"])
      {:ok true})
    (catch Exception e
      {:ok false :error (.getMessage e)})))
