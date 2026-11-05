(ns plauna.db-config-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [plauna.db-config :as db-cfg]))

(deftest only-a-missing-type-means-sqlite
  (with-redefs [db-cfg/read-file (constantly {:type :mariad})]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown database type" (db-cfg/load-config))
        "A typo must not start Plauna on SQLite"))
  (with-redefs [db-cfg/read-file (constantly {:type "MariaDB" :host "db"})]
    (is (= :mariadb (:type (db-cfg/load-config)))))
  (with-redefs [db-cfg/read-file (constantly nil)]
    (is (= {:type :sqlite} (db-cfg/load-config)) "No configuration at all is the SQLite default")))

(deftest an-unreadable-configuration-is-an-error-not-a-missing-file
  (let [file (java.io.File/createTempFile "db-config" ".edn")]
    (try
      (spit file "{:type :mariadb :host")
      (with-redefs [db-cfg/config-file-path (constantly (str file))]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"cannot be read" (db-cfg/read-file))))
      (finally (io/delete-file file true)))))

(deftest the-configuration-is-replaced-in-one-step
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory "db-config" (make-array java.nio.file.attribute.FileAttribute 0)))
        path (str (io/file dir "db-config.edn"))]
    (try
      (with-redefs [db-cfg/config-file-path (constantly path)]
        (db-cfg/save-config! {:type :mariadb :host "db" :port 3306})
        (is (= {:type :mariadb :host "db" :port 3306} (db-cfg/read-file)))
        (is (not (.exists (io/file (str path ".tmp")))) "No temporary file is left behind"))
      (finally (run! #(io/delete-file % true) (reverse (file-seq dir)))))))
