(ns plauna.category-folder-scan-test
  "Learning categories from e-mails the user moved into category folders, against a real IMAP server
   (GreenMail) and an in-memory stand-in for the database."
  (:require [clojure.test :refer :all]
            [plauna.application :as app]
            [plauna.client :as client]
            [plauna.interfaces :as int]
            [plauna.preferences]
            [taoensso.telemere :as t])
  (:import (com.icegreen.greenmail.util GreenMail ServerSetupTest)
           (jakarta.mail Folder Message Message$RecipientType Session Store)
           (jakarta.mail.internet InternetAddress MimeMessage)
           (java.util Properties)))

(t/set-ns-filter! {:disallow "plauna.*"})

;; ── Pure folder selection ───────────────────────────────────────────────────────

(deftest category-folders-use-destination-or-default-folder
  (is (= {"Categories/Work" {:id 1 :name "work"}
          "Archive/Projects" {:id 2 :name "projects"}}
         (client/category-folders "/" [{:id 1 :name "work" :destination_folder nil}
                                       {:id 2 :name "projects" :destination_folder " Archive/Projects "}]
                                  "INBOX"))))

(deftest category-folders-leave-out-the-monitored-and-shared-folders
  (is (= {"Categories.Travel" {:id 4 :name "travel"}}
         (client/category-folders "." [{:id 1 :name "keep" :destination_folder "inbox"}
                                       {:id 2 :name "news" :destination_folder "Shared"}
                                       {:id 3 :name "promo" :destination_folder "Shared"}
                                       {:id 4 :name "travel" :destination_folder ""}]
                                  nil))
      "INBOX matches case-insensitively, a folder of two categories is ambiguous")
  (is (= {} (client/category-folders "/" [{:id 1 :name "watched" :destination_folder "Mail/In"}] "Mail/In"))
      "A category kept in the monitored folder is not watched"))

;; ── In-memory database ──────────────────────────────────────────────────────────

(defn- fake-db
  "An int/DB over an atom: {:emails {message-id {:category-id :confidence :folder :connection-id}}
   :watermarks {[connection-id folder] {:uid-validity :last-uid}} :failures [...]}."
  [state categories]
  (reify int/DB
    (fetch-categories [_] categories)
    (fetch-email-category-state [_ message-id]
      (some-> (get-in @state [:emails message-id]) (select-keys [:category-id :folder :connection-id])))
    (email-exists? [_ message-id] (contains? (:emails @state) message-id))
    (save-email [_ email]
      (let [metadata (:metadata email)]
        (swap! state assoc-in [:emails (-> email :header :message-id)]
               {:category-id (:category-id metadata) :confidence (:category-confidence metadata)
                :connection-id (:connection-id metadata) :subject (-> email :header :subject)})))
    (update-email-category [_ message-id category-id confidence]
      (swap! state update-in [:emails message-id] assoc :category-id category-id :confidence confidence))
    (update-email-folder [_ message-id folder] (swap! state assoc-in [:emails message-id :folder] folder))
    (update-email-connection [_ message-id connection-id] (swap! state assoc-in [:emails message-id :connection-id] connection-id))
    (fetch-category-folder-watermark [_ connection-id folder] (get-in @state [:watermarks [connection-id folder]]))
    (save-category-folder-watermark [_ connection-id folder uid-validity last-uid]
      (swap! state assoc-in [:watermarks [connection-id folder]] {:uid-validity uid-validity :last-uid last-uid}))
    (record-parse-failure [_ failure] (swap! state update :failures (fnil conj []) failure))))

(def ^:private analyzer
  (reify int/Analyzer
    (detect-language [_ _] {:code "eng" :confidence 0.9})))

(def ^:private email-client
  (reify int/EmailClient
    (current-folder-name [_ folder] (.getFullName ^Folder folder))))

;; ── Decision per message ────────────────────────────────────────────────────────

(deftest learning-takes-the-folder-category-as-a-user-decision
  (let [state (atom {:emails {"a" {:category-id 1 :confidence 0.6 :folder "INBOX" :connection-id "c1"}
                              "b" {:category-id 2 :confidence 0.6 :folder "Categories/Work" :connection-id "c1"}
                              "c" {:category-id nil :confidence nil :folder nil :connection-id nil}}})
        context {:db (fake-db state []) :analyzer analyzer :client email-client}
        learn (fn [message-id] (app/learn-category-from-folder! context {:connection-id "c1" :folder-name "Categories/Work"
                                                                         :category {:id 2 :name "work"} :message-id message-id
                                                                         :read-email #(throw (ex-info "must not read a stored e-mail" {}))}))]
    (is (= :learned (learn "a")))
    (is (= {:category-id 2 :confidence 1.0 :folder "Categories/Work" :connection-id "c1"} (get-in @state [:emails "a"])))
    (is (= :confirmed (learn "b")))
    (is (= 0.6 (get-in @state [:emails "b" :confidence])) "Plauna's own filing is not turned into a user decision")
    (is (= :learned (learn "c")))
    (is (= {:category-id 2 :confidence 1.0 :folder "Categories/Work" :connection-id "c1"} (get-in @state [:emails "c"])))))

;; ── Against an IMAP server ──────────────────────────────────────────────────────

(def ^:private imap-user "learner@localhost")

(defn- connection-config [^GreenMail greenmail]
  {:id "conn-scan" :host "127.0.0.1" :port (.getPort (.getImap greenmail)) :security "plain"
   :user imap-user :secret "secret" :folder "INBOX" :auth-type "basic"})

(defn- message [message-id subject]
  (doto (MimeMessage. (Session/getInstance (Properties.)))
    (.setFrom (InternetAddress. "sender@example.com"))
    (.setRecipient Message$RecipientType/TO (InternetAddress. imap-user))
    (.setSubject subject)
    (.setText (str subject " body"))
    (.saveChanges)
    (.setHeader "Message-ID" message-id)))

(defn- append! [^Store store ^String folder-name & messages]
  (let [folder (.getFolder store folder-name)]
    (when-not (.exists folder) (.create folder Folder/HOLDS_MESSAGES))
    (.appendMessages folder (into-array Message messages))))

(deftest scan-learns-from-messages-moved-into-category-folders
  (let [greenmail (doto (GreenMail. ServerSetupTest/IMAP) (.start))]
    (try
      (.setUser greenmail imap-user "secret")
      (let [config (connection-config greenmail)
            categories [{:id 1 :name "work" :destination_folder nil}
                        {:id 2 :name "news" :destination_folder "Newsletters"}]
            state (atom {:emails {"<known@x>" {:category-id 2 :confidence 0.7 :folder "INBOX" :connection-id "conn-scan"}
                                  "<filed@x>" {:category-id 1 :confidence 0.8 :folder "INBOX" :connection-id "conn-scan"}}})
            context {:db (fake-db state categories) :analyzer analyzer :client email-client}
            connection-data {:config config}]
        (with-open [^Store store (client/login config)]
          (let [work (str "Categories" (client/folder-separator store) "Work")]
            (append! store work (message "<old@x>" "Filed before Plauna watched"))
            (append! store "Newsletters")

            (testing "the first scan only marks where each folder ends"
              (let [summaries (client/scan-category-folders! connection-data context)]
                (is (= #{work "Newsletters"} (set (map :folder summaries))))
                (is (every? :baseline summaries))
                (is (not (contains? (:emails @state) "<old@x>")))))

            (append! store work
                     (message "<known@x>" "Moved by the user")
                     (message "<filed@x>" "Filed by Plauna itself")
                     (message "<new@x>" "Never seen before"))

            (testing "a later scan handles exactly the messages that arrived since"
              (let [summary (first (filter #(= work (:folder %)) (client/scan-category-folders! connection-data context)))]
                (is (= {:learned 1 :confirmed 1 :saved 1 :errors 0} (select-keys summary [:learned :confirmed :saved :errors]))))
              (is (= {:category-id 1 :confidence 1.0 :folder work :connection-id "conn-scan"} (get-in @state [:emails "<known@x>"])))
              (is (= 0.8 (get-in @state [:emails "<filed@x>" :confidence])))
              (is (= work (get-in @state [:emails "<filed@x>" :folder])))
              (is (= {:category-id 1 :confidence 1 :connection-id "conn-scan" :folder work :subject "Never seen before"}
                     (get-in @state [:emails "<new@x>"]))))

            (testing "nothing is handled twice"
              (swap! state assoc-in [:emails "<known@x>" :category-id] 99)
              (let [summaries (client/scan-category-folders! connection-data context)]
                (is (every? #(zero? (+ (:learned %) (:saved %) (:confirmed %))) summaries)))
              (is (= 99 (get-in @state [:emails "<known@x>" :category-id])))))))
      (finally (.stop greenmail)))))

(deftest a-changed-uidvalidity-starts-over-from-the-folder-end
  ;; A server reports a new UIDVALIDITY when a folder was recreated: the saved UIDs then mean nothing.
  (let [greenmail (doto (GreenMail. ServerSetupTest/IMAP) (.start))]
    (try
      (.setUser greenmail imap-user "secret")
      (let [config (connection-config greenmail)
            state (atom {:emails {} :watermarks {["conn-scan" "Newsletters"] {:uid-validity -1 :last-uid 0}}})
            context {:db (fake-db state [{:id 2 :name "news" :destination_folder "Newsletters"}]) :analyzer analyzer :client email-client}]
        (with-open [^Store store (client/login config)]
          (append! store "Newsletters" (message "<a@x>" "Filed under the old UIDVALIDITY")))
        (let [[summary] (client/scan-category-folders! {:config config} context)]
          (is (:baseline summary))
          (is (= 1 (get-in @state [:watermarks ["conn-scan" "Newsletters"] :last-uid])))
          (is (not= -1 (get-in @state [:watermarks ["conn-scan" "Newsletters"] :uid-validity])))
          (is (empty? (:emails @state)))))
      (finally (.stop greenmail)))))

(deftest a-missing-category-folder-is-skipped
  (let [greenmail (doto (GreenMail. ServerSetupTest/IMAP) (.start))]
    (try
      (.setUser greenmail imap-user "secret")
      (let [state (atom {:emails {}})
            context {:db (fake-db state [{:id 2 :name "news" :destination_folder "Nowhere"}]) :analyzer analyzer :client email-client}]
        (is (= [{:folder "Nowhere" :learned 0 :saved 0 :confirmed 0 :errors 0 :missing true}]
               (client/scan-category-folders! {:config (connection-config greenmail)} context)))
        (is (nil? (:watermarks @state))))
      (finally (.stop greenmail)))))

(deftest scans-are-scheduled-per-connection-and-can-be-turned-off
  (let [scans (deref #'client/category-folder-scans)]
    (with-redefs [plauna.preferences/category-folder-scan-interval (constantly 0)]
      (client/schedule-category-folder-scans {:config {:id "conn-off"}} {})
      (is (not (contains? @scans "conn-off")) "0 schedules nothing"))
    (with-redefs [plauna.preferences/category-folder-scan-interval (constantly 3600)]
      (client/schedule-category-folder-scans {:config {:id "conn-on"}} {})
      (let [first-scan (get @scans "conn-on")]
        (client/schedule-category-folder-scans {:config {:id "conn-on"}} {})
        (is (.isCancelled ^java.util.concurrent.Future first-scan) "A reconnect replaces the previous schedule")
        (is (not (.isCancelled ^java.util.concurrent.Future (get @scans "conn-on"))))))
    ((deref #'client/cancel-category-folder-scan!) "conn-on")
    (is (not (contains? @scans "conn-on")))))
