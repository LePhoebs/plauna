(ns plauna.db-migration-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [next.jdbc]
            [plauna.core.email]
            [plauna.db-migration :as migration]))

(deftest every-table-of-the-schema-is-migrated
  (let [tables (->> (file-seq (io/file "resources/db/migration/mariadb"))
                    (filter #(.isFile ^java.io.File %))
                    (mapcat #(re-seq #"(?i)CREATE TABLE (?:IF NOT EXISTS )?(\w+)" (slurp %)))
                    (map (comp str/lower-case second))
                    set)]
    (is (= tables (set migration/migration-order))
        "A table created by the migrations but missing here would silently stay behind in SQLite"))
  "The SQLite -> MariaDB copy covers every table of the schema")

(deftest multi-row-insert-statement-is-idempotent
  (is (= "INSERT INTO t (a, b) VALUES (?, ?), (?, ?) ON DUPLICATE KEY UPDATE a = a"
         (migration/insert-sql "t" ["a" "b"] 2))))

(deftest source-rows-stream-plain-unqualified-maps
  ;; next.jdbc's plan builds rows with the builder passed to `plan`; a builder passed only to
  ;; datafiable-row is ignored and would leave table-qualified keys, so every value looked up by column
  ;; name would be nil and the migration would copy NULLs.
  (let [file (java.io.File/createTempFile "plauna-migration-source-" ".db")
        ds (migration/sqlite-ds (.getAbsolutePath file))]
    (try
      (next.jdbc/execute! ds ["CREATE TABLE headers (message_id TEXT PRIMARY KEY, subject TEXT, date INTEGER)"])
      (next.jdbc/execute! ds ["INSERT INTO headers (message_id, subject, date) VALUES ('m-1', 'Hello', 42), ('m-2', NULL, 43)"])
      (let [rows (into [] (migration/source-rows ds "headers"))
            cols (map name (keys (first rows)))]
        (is (= [{:message_id "m-1" :subject "Hello" :date 42} {:message_id "m-2" :subject nil :date 43}] rows))
        (is (= ["m-1" "Hello" 42] (migration/row-values cols (first rows)))
            "Column names derived from the first row read every value of a row"))
      (finally
        (io/delete-file file true)))))

(deftest copied-message-ids-fit-mariadb
  (is (= [(plauna.core.email/storable-message-id "<ü@x>") "s"]
         (migration/row-values ["message_id" "subject"] {:message_id "<ü@x>" :subject "s"}))))
