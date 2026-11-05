(ns plauna.flags-and-retention-imap-test
  "Apple Mail flags and moving expired e-mails to the trash, against a real IMAP server (GreenMail)."
  (:require [clojure.test :refer :all]
            [plauna.client :as client]
            [plauna.core.email]
            [plauna.database :as db]
            [taoensso.telemere :as t])
  (:import (com.icegreen.greenmail.util GreenMail ServerSetupTest)
           (jakarta.mail Flags$Flag Folder Message Message$RecipientType Session Store)
           (jakarta.mail.internet InternetAddress MimeMessage)
           (jakarta.mail.search MessageIDTerm)
           (java.util Properties)))

(t/set-ns-filter! {:disallow "plauna.*"})

(def ^:private imap-user "flags@localhost")

(defn- connection-config [^GreenMail greenmail]
  {:id "conn-flags" :host "127.0.0.1" :port (.getPort (.getImap greenmail)) :security "plain"
   :user imap-user :secret "secret" :folder "INBOX" :auth-type "basic"})

(defn- message [message-id]
  (doto (MimeMessage. (Session/getInstance (Properties.)))
    (.setFrom (InternetAddress. "parcels@example.com"))
    (.setRecipient Message$RecipientType/TO (InternetAddress. imap-user))
    (.setSubject message-id)
    (.setText "Your parcel is on its way")
    (.saveChanges)
    (.setHeader "Message-ID" message-id)))

(defn- flags-of
  "#{:flagged keyword...} of message-id in folder-name, or nil when it is not there."
  [config folder-name message-id]
  (with-open [^Store store (client/login config)]
    (let [folder (.getFolder store ^String folder-name)]
      (.open folder Folder/READ_ONLY)
      (try
        (when-let [^Message found (first (.search folder (MessageIDTerm. message-id)))]
          (into (if (.isSet found Flags$Flag/FLAGGED) #{:flagged} #{})
                (.getUserFlags (.getFlags found))))
        (finally (.close folder false))))))

(deftest a-failing-account-is-counted-not-thrown
  (with-redefs [client/connection-data-from-id (fn [_] {:config {:id "broken" :host "127.0.0.1" :port 1 :security "plain"
                                                                 :user "x" :secret "y" :folder "INBOX" :auth-type "basic"}})
                client/connected? (constantly true)]
    (is (= {:failed 2} (client/trash-expired-emails! "broken" [{:message-id "<a@x>" :folder "INBOX"} {:message-id "<b@x>" :folder "INBOX"}])))
    (is (= {:failed 1} (client/apply-flags! "broken" [{:message-id "<a@x>" :folder "INBOX" :flag "red"}]))))
  (with-redefs [client/connection-data-from-id (constantly nil)]
    (is (= {:offline 1} (client/apply-flags! "gone" [{:message-id "<a@x>" :folder "INBOX" :flag "red"}])))
    (is (= {} (client/apply-flags! "gone" [])))))

(deftest flags-and-expired-emails-on-a-real-imap-server
  (let [greenmail (doto (GreenMail. ServerSetupTest/IMAP) (.start))]
    (try
      (.setUser greenmail imap-user "secret")
      (let [config (connection-config greenmail)
            inbox [{:message-id "<go@x>" :folder "INBOX"}
                   {:message-id "<kept@x>" :folder "INBOX"}]
            marked (atom [])]
        (with-open [^Store store (client/login config)]
          (.appendMessages (.getFolder store "INBOX") (into-array Message [(message "<go@x>") (message "<kept@x>")]))
          (.create (.getFolder store "Trash") Folder/HOLDS_MESSAGES))
        (with-redefs [client/connection-data-from-id (fn [_] {:config config})
                      client/connected? (constantly true)
                      db/mark-email-expired! (fn [message-id folder] (swap! marked conj [message-id folder]))
                      ;; "parcels" keeps its e-mails in the INBOX; "work" files into its own folder.
                      db/category-by-name (fn [name] ({"parcels" {:name "parcels" :destination_folder "INBOX" :flag_color "orange"}
                                                       "work" {:name "work" :destination_folder "Work" :flag_color nil}} name))
                      db/update-email-folder (fn [& _] nil)
                      db/update-email-connection (fn [& _] nil)]

          (testing "an orange flag is the plain flag plus color bit 0"
            (is (= {:flagged 2} (client/apply-flags! "conn-flags" (mapv #(assoc % :flag "orange") inbox))))
            (is (= #{:flagged "$MailFlagBit0"} (flags-of config "INBOX" "<go@x>"))))

          (testing "a new color replaces the old bits"
            (client/apply-flags! "conn-flags" (mapv #(assoc % :flag "gray") inbox))
            (is (= #{:flagged "$MailFlagBit1" "$MailFlagBit2"} (flags-of config "INBOX" "<go@x>"))))

          (testing "the user takes the flag away from one e-mail"
            (with-open [^Store store (client/login config)]
              (let [folder (.getFolder store "INBOX")]
                (.open folder Folder/READ_WRITE)
                (try (client/set-category-flag! folder (seq (.search folder (MessageIDTerm. "<kept@x>"))) :clear)
                     (finally (.close folder false)))))
            (is (= #{} (flags-of config "INBOX" "<kept@x>"))))

          (testing "expired e-mails go to the trash unless unflagged or gone"
            (is (= {:trashed 1 :protected 1 :missing 1}
                   (client/trash-expired-emails! "conn-flags"
                                                 (mapv #(assoc % :protect? true :category "parcels")
                                                       (conj inbox {:message-id "<moved-away@x>" :folder "INBOX"})))))
            (is (nil? (flags-of config "INBOX" "<go@x>")))
            (is (= #{:flagged "$MailFlagBit1" "$MailFlagBit2"} (flags-of config "Trash" "<go@x>"))
                "The e-mail keeps its flag in the trash")
            (is (= #{} (flags-of config "INBOX" "<kept@x>")) "The unflagged e-mail stays in the inbox")
            (is (= [["<go@x>" "Trash"]] @marked)))

          (testing "an e-mail outside its category's folder is not touched"
            (is (= {:elsewhere 1}
                   (client/trash-expired-emails! "conn-flags" [{:message-id "<kept@x>" :folder "INBOX" :category "work" :protect? false}])))
            (is (= #{} (flags-of config "INBOX" "<kept@x>"))))

          (testing "a recategorization flags the e-mail in its new folder, after the move"
            (with-open [^Store store (client/login config)]
              (.create (.getFolder store "Work") Folder/HOLDS_MESSAGES))
            (is (true? (client/move-message-on-dedicated-store! config "INBOX" "Work" "<kept@x>" "green")))
            (is (nil? (flags-of config "INBOX" "<kept@x>")))
            (is (= #{:flagged "$MailFlagBit0" "$MailFlagBit1"} (flags-of config "Work" "<kept@x>"))))))
      (finally (.stop greenmail)))))

(deftest a-non-ascii-message-id-is-stored-in-ascii-and-found-on-the-server
  (let [greenmail (doto (GreenMail. ServerSetupTest/IMAP) (.start))]
    (try
      (.setUser greenmail imap-user "secret")
      (let [config (connection-config greenmail)
            utf8-session (Session/getInstance (doto (Properties.) (.put "mail.mime.allowutf8" "true")))
            raw-id "<rückgabe-1@beispiel.de>"]
        (with-open [^Store store (client/login config)]
          (.appendMessages (.getFolder store "INBOX")
                           (into-array Message [(doto (MimeMessage. utf8-session)
                                                  (.setFrom (InternetAddress. "shop@example.com"))
                                                  (.setSubject "Rückgabe")
                                                  (.setText "Ihre Rückgabe")
                                                  (.saveChanges)
                                                  (.setHeader "Message-ID" raw-id))])))
        (let [stored-id (with-open [^Store store (client/login config)]
                          (let [folder (.getFolder store "INBOX")]
                            (.open folder Folder/READ_ONLY)
                            (try (client/message-id-of (first (.getMessages folder)))
                                 (finally (.close folder false)))))]
          (is (every? #(< (int %) 128) stored-id) "MariaDB's ASCII columns accept the stored id")
          (is (= raw-id (plauna.core.email/imap-message-id stored-id)))
          (with-redefs [client/connection-data-from-id (fn [_] {:config config})
                        client/connected? (constantly true)]
            (is (= {:flagged 1} (client/apply-flags! "conn-flags" [{:message-id stored-id :folder "INBOX" :flag "red"}]))
                "The IMAP search finds the e-mail by its real Message-ID"))))
      (finally (.stop greenmail)))))
