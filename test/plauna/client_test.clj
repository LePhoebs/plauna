(ns plauna.client-test
  (:require [clojure.test :refer :all]
            [plauna.client :as client :refer [->ConnectionData]]
            [plauna.database :as db]
            [plauna.interfaces :as int-protocol]
            [plauna.core.email :as core-email :refer [->Email ->Participant]]
            [taoensso.telemere :as t])
  (:import (java.util Properties)
           (java.io ByteArrayInputStream)
           (java.time LocalDate ZoneId)
           (jakarta.mail Message Session)
           (jakarta.mail.internet MimeMessage)
           (jakarta.mail.search ComparisonTerm ReceivedDateTerm SearchTerm)
           (org.eclipse.angus.mail.imap IMAPFolder IMAPMessage)
           (org.mockito ArgumentCaptor Mockito)))

(t/set-ns-filter! {:disallow "plauna.*"})

(defn- expected-properties
  "The properties every session gets, plus the given key/value pairs."
  [& kvs]
  (let [properties (doto (new Properties)
                     (.setProperty "mail.imap.usesocketchannels" "true")
                     (.setProperty "mail.imap.connectiontimeout" client/imap-connect-timeout-millis)
                     (.setProperty "mail.imap.timeout" client/imap-read-timeout-millis)
                     (.setProperty "mail.imap.partialfetch" "false")
                     (.setProperty "mail.imap.fetchsize" "1048576"))]
    (doseq [[k v] (partition 2 kvs)]
      (.setProperty properties k v))
    properties))

(deftest ssl-properties-set-correctly
  (let [session ^Session (client/config->session {:security "ssl" :port 993})]
    (is (= (expected-properties "mail.imap.ssl.enable" "true" "mail.imap.port" "993")
           (.getProperties session)))))

(deftest starttls-properties-set-correctly
  (let [session ^Session (client/config->session {:security "starttls" :port 143})]
    (is (= (expected-properties "mail.imap.starttls.enable" "true"
                                "mail.imap.starttls.required" "true"
                                "mail.imap.port" "143")
           (.getProperties session))
        "STARTTLS is required, never silently downgraded to plaintext")))

(deftest plain-text-properties-set-correctly
  (let [session ^Session (client/config->session {:security "plain" :port 143})]
    (is (= (expected-properties "mail.imap.port" "143") (.getProperties session)))))

(deftest empty-values-return-ssl
  (let [session ^Session (client/config->session {})]
    (is (= (expected-properties "mail.imap.ssl.enable" "true" "mail.imap.port" "993")
           (.getProperties session)))))

(deftest non-compliant-security-values-return-ssl
  (let [session ^Session (client/config->session {:security "does-not-exist"})]
    (is (= (expected-properties "mail.imap.ssl.enable" "true" "mail.imap.port" "993")
           (.getProperties session)))))

(deftest every-session-has-connect-and-read-timeouts
  (let [session ^Session (client/config->session {})]
    (is (= "15000" (.getProperty session "mail.imap.connectiontimeout"))
        "A connect to an unreachable host must not block a health check for minutes")
    (is (= "30000" (.getProperty session "mail.imap.timeout"))
        "A single slow read (a large body on a slow link) must not count as a dead connection")))

(deftest debug-false-on-default
  (let [session ^Session (client/config->session {})]
    (is (= false (.getDebug session)))))

(deftest debug-can-be-set-true
  (let [session ^Session (client/config->session {:debug true})]
    (is (= true (.getDebug session)))))

(deftest set-cert-checks-to-false
  (let [session ^Session (client/config->session {:security "ssl" :check-ssl-certs false})]
    (is (= "*" (.getProperty session "mail.imap.ssl.trust")))))

(deftest id-creating
  (let [config {:host "imap.testmail.com" :user "test@testmail.com" :secret "12345" :folder "Inbox" :debug false}
        same-config-different-secret-and-debug {:host "imap.testmail.com" :user "test@testmail.com" :secret "67890" :folder "Inbox" :debug true}
        config-but-different-folder {:host "imap.testmail.com" :user "test@testmail.com" :secret "12345" :folder "Otherbox" :debug false}
        expected-uuid "2d9f0ab8-00f2-3600-b6b9-24054e1e3337"]
    (is (= expected-uuid (client/id-from-config config)))
    (is (= expected-uuid (client/id-from-config same-config-different-secret-and-debug)))
    (is (not (= expected-uuid (client/id-from-config config-but-different-folder))))))

(deftest adding-to-connections
  (let [test-config {:id "test-id" :host "imap.testmail.com" :user "test@testmail.com" :secret "12345" :folder "Inbox" :debug false :security "starttls"}
        test-con-data (client/->ConnectionData test-config nil nil nil nil nil)]
    (client/add-to-connections test-con-data)
    (is (= (get @client/connections "test-id") test-con-data))))

(deftest connection-id-from-email-success-single-connection
  (let [email (->Email nil nil
                       [(->Participant "test@test.com" nil nil :receiver nil)
                        (->Participant "nope@test.com" nil nil :sender nil)])
        connection-data (->ConnectionData {:user "test@test.com" :id "correct-id"} nil nil nil nil nil)]
    (is (= "correct-id" (client/connection-id-for-email [connection-data] email)))))

(deftest connection-id-from-email-success-multiple-connections
  (let [email (->Email nil nil
                       [(->Participant "test@test.com" nil nil :receiver nil)
                        (->Participant "nope@test.com" nil nil :sender nil)])
        connection-data (->ConnectionData {:user "test@test.com" :id "correct-id"} nil nil nil nil nil)
        connection-data2 (->ConnectionData {:user "nope@test.com" :id "wrong-id"} nil nil nil nil nil)]
    (is (= "correct-id" (client/connection-id-for-email [connection-data connection-data2] email)))))

(deftest connection-id-from-email-fail
  (let [email (->Email nil nil
                       [(->Participant "test@test.com" nil nil :receiver nil)
                        (->Participant "nope@test.com" nil nil :sender nil)])
        connection-data (->ConnectionData {:user "test2@test.com" :id "some-id"} nil nil nil nil nil)]
    (is (nil? (client/connection-id-for-email {"correct-id" connection-data} email)))))

(deftest folder-monitor-tests
  (let [null (client/monitor-folder-name nil)
        empty (client/monitor-folder-name "")
        valid (client/monitor-folder-name "MyInbox")]
    (is (= null "INBOX"))
    (is (= empty "INBOX"))
    (is (= valid "MyInbox"))))

(deftest body-parts-are-eagerly-realized
  (let [realized (atom [])
        plain (core-email/->Body-Part "msg" "utf-8" "text/plain" nil "plain body" nil nil)
        html (core-email/->Body-Part "msg" "utf-8" "text/html" nil "<p>html body</p>" nil nil)
        body-parts (lazy-seq
                    (swap! realized conj :outer)
                    [(lazy-seq
                      (swap! realized conj :inner)
                      [plain html])])
        result (#'client/realize-body-parts body-parts)]
    (is (= [:outer :inner] @realized))
    (is (vector? result))
    (is (= [plain html] result))))

(deftest create-category-does-nothing-when-not-connected
  (let [create-folder-called (atom nil)]
    (with-redefs [client/connected? (fn [_] false)
                  client/create-folders (fn [_ category] (swap! create-folder-called (fn [_] category)))]
      (client/create-category-folders! {:store nil} "my-cat")
      (is (= nil @create-folder-called)))))

(deftest create-category-calls-function-when-connected
  (let [create-folder-called (atom nil)]
    (with-redefs [client/connected? (fn [_] true)
                  client/create-folders (fn [_ category] (swap! create-folder-called (fn [_] category)))]
      (client/create-category-folders! {:store nil} "my-cat")
      (is (= "my-cat" @create-folder-called)))))

(deftest reconnect-does-not-login-an-already-connected-store
  (let [login-called? (atom false)
        connection-data (->ConnectionData {:id "id1" :host "imap.example.com" :user "me@example.com"} nil nil nil nil nil)]
    (with-redefs [client/connected? (fn [_] true)
                  client/login (fn [& _] (reset! login-called? true))]
      (is (= connection-data (client/reconnect connection-data)))
      (is (false? @login-called?)))))

(deftest reconnect-logs-in-a-disconnected-store
  (let [login-args (atom nil)
        config {:id "id1" :host "imap.example.com" :user "me@example.com" :auth-type "basic"}
        connection-data (->ConnectionData config :store nil nil nil nil)]
    (with-redefs [client/connected? (fn [_] false)
                  client/login (fn [cfg store] (reset! login-args [cfg store]) :logged-in)]
      (is (= :logged-in (client/reconnect connection-data)))
      (is (= [config :store] @login-args)))))

(deftest connection-id-for-email-happy-path
  (let [test-email (->Email nil nil
                            [(->Participant "find@me.com" nil nil :receiver nil)
                             (->Participant "nope@test.com" nil nil :sender nil)])
        test-connection1 (->ConnectionData {:id "id1" :user "find@me.com"} nil nil nil nil nil)
        test-connection2 (->ConnectionData {:id "id2" :user "wrong@one.com"} nil nil nil nil nil)
        result (client/connection-id-for-email [test-connection1 test-connection2] test-email)]
    (is (= "id1" result)))
  "Given a vector of ConnectionData, the function finds the correct ConnectionData id.")

(deftest connection-id-for-email-sad-path
  (let [test-email (->Email nil nil
                            [(->Participant "cannotfind@me.com" nil nil :receiver nil)
                             (->Participant "nope@test.com" nil nil :sender nil)])
        test-connection1 (->ConnectionData {:id "id1" :user "find@me.com"} nil nil nil nil nil)
        test-connection2 (->ConnectionData {:id "id2" :user "wrong@one.com"} nil nil nil nil nil)
        result (client/connection-id-for-email [test-connection1 test-connection2] test-email)]
    (is (nil? result)))
  "Given a vector of ConnectionData, the function returns nil if it cannot find the correct id.")

(deftest move-emails-test-no-connection
  (with-redefs [client/connected? (fn [_] false)
                client/connection-data-from-id (fn [_] {})]
    (is (false? (client/move-messages-by-id-between-category-folders "fake" "does-no-exist" "test" "test2" {}))))
  "If the store is not connected, return false immediately.")

(deftest move-emails-same-folder-leaves-in-place
  (let [recorded (atom nil)]
    (with-redefs [client/connected? (fn [_] true)
                  client/connection-data-from-id (fn [_] {:store nil :config {:id 1 :folder "INBOX"}})
                  db/email-folder (fn [_] nil)
                  db/update-email-folder (fn [message-id folder] (reset! recorded [message-id folder]))
                  db/update-email-connection (fn [_ _] nil)
                  client/inbox-or-default-category-folder-name (fn [_ _ _] "INBOX")
                  client/inbox-or-category-folder-name (fn [_ _ _] "INBOX")]
      (is (true? (client/move-messages-by-id-between-category-folders "fake" "msg-id" "n/a" "keep" {})))
      ;; Even though nothing moved, the resolved folder is recorded so a legacy email stops being nil.
      (is (= ["msg-id" "INBOX"] @recorded))))
  "When the resolved source and target folders are identical, the message is left in place, the move reports success without touching IMAP, and the resolved folder is still recorded.")

(deftest move-emails-same-custom-folder-leaves-in-place
  (with-redefs [client/connected? (fn [_] true)
                client/connection-data-from-id (fn [_] {:store nil :config {:id 1 :folder "INBOX"}})
                db/email-folder (fn [_] nil)
                db/update-email-folder (fn [_ _] nil)
                db/update-email-connection (fn [_ _] nil)
                client/inbox-or-default-category-folder-name (fn [_ _ _] "Archive/Projects")
                client/inbox-or-category-folder-name (fn [_ _ _] "Archive/Projects")]
    (is (true? (client/move-messages-by-id-between-category-folders "fake" "msg-id" "work" "projects" {}))))
  "The same-folder guard is generic: it applies to any folder, not just the inbox. When source and target both resolve to the same custom folder the message is left in place.")

(deftest copy-message-returns-false-on-failure
  (is (false? (client/copy-message nil nil nil)))
  "copy-message returns false when the IMAP operations throw, so a failed copy+delete is never recorded as a successful move.")

(deftest move-emails-unrecorded-uses-default-category-folder-as-source
  (with-redefs [client/connected? (fn [_] true)
                client/connection-data-from-id (fn [_] {:store nil :config {:id 1 :folder "INBOX"}})
                db/email-folder (fn [_] nil)
                db/update-email-folder (fn [_ _] nil)
                db/update-email-connection (fn [_ _] nil)
                ;; default resolver (correct for legacy emails) and the custom-destination resolver
                ;; return DIFFERENT values; the target resolves to the same value as the default resolver.
                client/inbox-or-default-category-folder-name (fn [_ _ _] "Categories/Work")
                client/inbox-or-category-folder-name (fn [_ folder-name _] (if (= folder-name "projects") "Categories/Work" "Custom/WrongSource"))]
    (is (true? (client/move-messages-by-id-between-category-folders "fake" "msg-id" "work" "projects" {}))))
  "For an email with no recorded folder, the source resolves through the DEFAULT category folder, not the (mutable) custom destination. Here that makes source == target so the move is a safe no-op; had the custom-destination resolver been used for the source it would differ and the code would hit the nil store.")

(deftest move-emails-uses-recorded-folder-as-source
  (with-redefs [client/connected? (fn [_] true)
                client/connection-data-from-id (fn [_] {:store nil :config {:id 1 :folder "INBOX"}})
                db/email-folder (fn [_] "Recorded/Folder")
                db/update-email-folder (fn [_ _] nil)
                db/update-email-connection (fn [_ _] nil)
                ;; category-derived resolution would give a *different* source ("Derived/Work").
                client/inbox-or-category-folder-name (fn [_ folder-name _] (if (= folder-name "projects") "Recorded/Folder" "Derived/Work"))]
    (is (true? (client/move-messages-by-id-between-category-folders "fake" "msg-id" "work" "projects" {}))))
  "The recorded folder drives source resolution: the recorded source equals the resolved target so the move is a no-op (true). If the category-derived folder were used instead, source would be 'Derived/Work' != target and the code would try to open a (nil) store and fail.")

(deftest move-emails-uses-a-dedicated-store-and-leaves-the-monitor-store-open
  (let [monitored-store (Object.)
        connection-config {:id "conn-1" :folder "INBOX" :auth-type "basic"}
        connection-data (->ConnectionData connection-config monitored-store nil nil nil nil)
        move-call (atom nil)]
    (with-redefs [client/connection-data-from-id (fn [_] connection-data)
                  client/connected? (fn [_] true)
                  client/inbox-or-default-category-folder-name (fn [store _ _]
                                                                 (is (identical? monitored-store store))
                                                                 "INBOX")
                  client/inbox-or-category-folder-name (fn [store _ _]
                                                         (is (identical? monitored-store store))
                                                         "Categories/Work")
                  client/move-message-on-dedicated-store! (fn [& args]
                                                            (reset! move-call args)
                                                            true)
                  db/email-folder (fn [_] "INBOX")
                  db/update-email-folder (fn [& _]
                                           (throw (ex-info "The dedicated move owns the folder update" {})))]
      (is (true? (client/move-messages-by-id-between-category-folders
                  "conn-1" "msg-id" nil "work" {})))
      (is (= [connection-config "INBOX" "Categories/Work" "msg-id"] @move-call)
          "The actual move is delegated without opening a folder on the monitored Store")))
  "Recategorization resolves names from the live connection but performs the move on a dedicated Store")

(deftest unexpected-store-close-is-recognised-only-for-the-live-store
  (let [store (Object.)
        other-store (Object.)
        connection-data (->ConnectionData {:id "listener-test"} store nil nil nil nil)
        intentional #'client/intentional-closes]
    (try
      (swap! client/connections assoc "listener-test" connection-data)
      (swap! @intentional disj "listener-test")
      (is (true? (client/unexpected-store-close? "listener-test" store)) "The live store closing without a request is a dropped connection")
      (is (false? (client/unexpected-store-close? "listener-test" other-store)) "A stale store replaced by a reconnect is ignored")
      (is (false? (client/unexpected-store-close? "unknown" store)) "A deleted connection has nothing to recover")
      (swap! @intentional conj "listener-test")
      (is (false? (client/unexpected-store-close? "listener-test" store)) "A user-requested disconnect is not a dropped connection")
      (finally
        (swap! client/connections dissoc "listener-test")
        (swap! @intentional disj "listener-test")))))

(defn- mime-message [^String raw]
  (MimeMessage. (Session/getInstance (Properties.)) (ByteArrayInputStream. (.getBytes raw "UTF-8"))))

(deftest raw-header-fallback-rebuilds-header-and-participants
  (let [message (mime-message (str "Message-ID: <broken-envelope@example.com>\r\n"
                                   "In-Reply-To: <parent@example.com>\r\n"
                                   "Subject: =?UTF-8?Q?Rechnung_f=C3=BCr_M=C3=A4rz?=\r\n"
                                   "Date: Tue, 2 Sep 2026 10:00:00 +0200\r\n"
                                   "From: \"Shop\" <shop@example.com>\r\n"
                                   "To: me@example.com, Other <other@example.com>\r\n"
                                   "Cc: this is not an address\r\n"
                                   "Content-Type: text/plain; charset=utf-8\r\n"
                                   "\r\n"
                                   "Hello\r\n"))
        header (client/header-from-raw-headers message)
        participants (client/participants-from-raw-headers message (:message-id header))]
    (is (= "<broken-envelope@example.com>" (:message-id header)))
    (is (= "<parent@example.com>" (:in-reply-to header)))
    (is (= "Rechnung für März" (:subject header)) "Encoded words are decoded")
    (is (= "text/plain" (:mime-type header)))
    (is (= 1788336000 (:date header)) "The Date header (10:00 +02:00) is parsed to epoch seconds, 08:00 UTC")
    (is (= [[:sender "shop@example.com" "Shop"] [:receiver "me@example.com" nil] [:receiver "other@example.com" "Other"]]
           (mapv (juxt :type :address :name) participants))
        "From and To are parsed leniently; the unparseable Cc header is dropped instead of failing")
    (is (every? #(= "<broken-envelope@example.com>" (:message-id %)) participants))))

(deftest raw-header-fallback-synthesizes-a-missing-message-id
  (let [raw (str "Date: Tue, 2 Sep 2026 10:00:00 +0200\r\nFrom: shop@example.com\r\nSubject: Invoice\r\nContent-Type: text/plain\r\n\r\nbody\r\n")
        header (client/header-from-raw-headers (mime-message raw))]
    (is (= (core-email/synthetic-message-id 1788336000 "shop@example.com" "Invoice") (:message-id header))
        "The synthetic id is built from date, sender and subject, so mbox and IMAP agree")
    (is (= (:message-id header) (:message-id (client/header-from-raw-headers (mime-message raw)))) "Deterministic across runs")
    (is (every? #(= (:message-id header) (:message-id %)) (client/participants-from-raw-headers (mime-message raw) (:message-id header))))))

(deftest raw-header-fallback-tolerates-missing-headers
  (let [message (mime-message "Content-Type: text/plain\r\n\r\nbody\r\n")
        header (client/header-from-raw-headers message)]
    (is (core-email/synthetic-message-id? (:message-id header)) "Without any header there is still a (synthetic) id")
    (is (nil? (:subject header)))
    (is (nil? (:date header)))
    (is (= [] (client/participants-from-raw-headers message nil)))
    (is (nil? (client/header-date-seconds "not a date")))))

(deftest received-since-message-numbers-searches-the-received-date-from-the-given-day
  (let [folder (Mockito/mock IMAPFolder)
        message (fn [n] (let [m (Mockito/mock IMAPMessage)]
                          (.thenReturn (Mockito/when (.getMessageNumber m)) (int n))
                          m))
        since (LocalDate/of 2026 9 30)
        ;; Built before the stubbing starts: Mockito does not allow stubbing a mock inside another stubbing.
        answer (into-array Message [(message 9) (message 3) (message 7)])]
    (.thenReturn (Mockito/when (.search folder ^SearchTerm (Mockito/any SearchTerm))) answer)
    (is (= [3 7 9] (client/received-since-message-numbers folder since))
        "The sequence numbers come back ascending whatever order the server answers in")
    (let [captor (ArgumentCaptor/forClass SearchTerm)]
      (.search ^IMAPFolder (Mockito/verify folder) ^SearchTerm (.capture captor))
      (let [^ReceivedDateTerm term (.getValue captor)]
        (is (instance? ReceivedDateTerm term) "The RECEIVED date (INTERNALDATE) is searched, not the Date header")
        (is (= ComparisonTerm/GE (.getComparison term)) "On or after the day: IMAP SINCE")
        (is (= since (-> (.getDate term) .toInstant (.atZone (ZoneId/systemDefault)) .toLocalDate))
            "The date is the requested calendar day in the JVM's own zone, in which the mail library writes it"))))
  "The date search sends SINCE <day> to the server and returns the matching sequence numbers.")

(deftest received-since-message-numbers-is-empty-when-nothing-matches
  (let [folder (Mockito/mock IMAPFolder)]
    (.thenReturn (Mockito/when (.search folder ^SearchTerm (Mockito/any SearchTerm)))
                 (into-array Message []))
    (is (= [] (client/received-since-message-numbers folder (LocalDate/of 2026 10 1))))))

(deftest the-imap-client-answers-the-date-search-through-received-since-message-numbers
  (let [folder (Mockito/mock IMAPFolder)
        message (let [m (Mockito/mock IMAPMessage)]
                  (.thenReturn (Mockito/when (.getMessageNumber m)) (int 4))
                  m)
        answer (into-array Message [message])]
    (.thenReturn (Mockito/when (.search folder ^SearchTerm (Mockito/any SearchTerm))) answer)
    (is (= [4] (int-protocol/message-numbers-since (client/->ImapClient) folder (LocalDate/of 2026 10 1)))
        "The protocol method of the real client is wired to the folder it is given")))
