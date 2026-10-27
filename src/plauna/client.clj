(ns plauna.client
  (:require
   [plauna.preferences :as p]
   [plauna.database :as db]
   [plauna.client.oauth :as oauth]
   [clojure.string :as s]
   [taoensso.telemere :as t]
   [plauna.interfaces :as int]
   [plauna.application :as app]
   [plauna.core.email :as core-email]
   [clojure.core.async :as async])
  (:import
   (plauna.core.email Header Body-Part Participant Email)
   (jakarta.mail Store Session Folder BodyPart Multipart Message Message$RecipientType Flags$Flag AuthenticationFailedException MessagingException FetchProfile FetchProfile$Item UIDFolder$FetchProfileItem)
   (jakarta.mail.internet InternetAddress MailDateFormat MimeMessage MimePart MimeUtility)
   (org.eclipse.angus.mail.imap IMAPFolder IMAPMessage)
   (jakarta.mail.event ConnectionAdapter ConnectionEvent MessageCountAdapter MessageCountEvent MessageCountListener)
   (jakarta.mail.search ComparisonTerm MessageIDTerm ReceivedDateTerm)
   (java.lang AutoCloseable)
   (java.time LocalDate ZoneId)
   (java.util Date Properties UUID)
   (org.eclipse.angus.mail.imap IdleManager IMAPStore)
   (java.util.concurrent Executors ExecutorService TimeUnit ScheduledExecutorService ScheduledFuture ThreadFactory)))

(set! *warn-on-reflection* true)

;; A small pool rather than a single thread: each connection's health check does blocking IMAP/HTTP
;; work, so one slow or hung connection on a single-thread scheduler would stall every other
;; connection's health check.
(defonce executor-service (Executors/newScheduledThreadPool 4))

(defonce parent-folder-name "Categories")

(defonce connections (atom {}))

(defonce health-checks (atom {}))

(declare connect)

(declare reconnect)

(declare start-monitoring)

(declare stop-monitoring)

(declare schedule-health-checks)

(declare refresh-access-token)

(defn default-port-for-security [security]
  (if (= security "ssl") 993 143))

(defn oauth2? [connection-config] (= "oauth2" (:auth-type connection-config)))

(defn security [connection-config]
  (let [security (get connection-config :security "ssl")]
    (if (some #(= security %) ["ssl" "starttls" "plain"])
      security
      "ssl")))

(defn port [connection-config]
  (str (get connection-config :port (default-port-for-security (security connection-config)))))

(defn check-ssl-certs? [connection-config] (get connection-config :check-ssl-certs true))

(def imap-connect-timeout-millis
  "Bound on establishing the TCP connection. Without it an unreachable host blocks the caller for the
   operating system's default (minutes) - on the health-check pool or a request thread."
  "15000")

(def imap-read-timeout-millis
  "Bound on a single socket read. IDLE is not affected (the IdleManager waits on a selector), but every
   command and every body fetch is: five seconds was too little for large messages on slow links and
   turned a slow server into a 'lost' connection."
  "30000")

(defn default-imap-properties ^Properties [connection-config]
  (doto (new Properties)
    (.setProperty "mail.imap.port" (port connection-config))
    (.setProperty "mail.imap.usesocketchannels" "true")
    (.setProperty "mail.imap.connectiontimeout" imap-connect-timeout-millis)
    (.setProperty "mail.imap.timeout" imap-read-timeout-millis)
    (.setProperty "mail.imap.partialfetch" "false")
    (.setProperty "mail.imap.fetchsize" "1048576")))

(defn oauth-properties [connection-config]
  (fn [^Properties properties]
    (if (oauth2? connection-config)
      (doto properties (.setProperty "mail.imap.auth.mechanisms" "XOAUTH2"))
      properties)))

(defn security-properties [connection-config]
  (let [security-key (security connection-config)]
    (fn [^Properties properties]
      (cond (= security-key "ssl") (doto properties (.setProperty "mail.imap.ssl.enable" "true"))
            ;; starttls.required: without it the library silently continues in plaintext (sending the
            ;; password) when the server does not offer STARTTLS.
            (= security-key "starttls") (doto properties
                                          (.setProperty "mail.imap.starttls.enable" "true")
                                          (.setProperty "mail.imap.starttls.required" "true"))
            (= security-key "plain") properties
            :else (doto properties (.setProperty "mail.imap.ssl.enable" "true"))))))

(defn certification-check-properties [connection-config]
  (if (not (check-ssl-certs? connection-config))
    (fn [^Properties properties] (doto properties (.setProperty "mail.imap.ssl.trust" "*")))
    (fn [^Properties properties] properties)))

(defn set-debug-mode [connection-config]
  (let [debug? (get connection-config :debug false)]
    (fn [^Session session]
      (if debug? (doto session (.setDebug true)) session))))

(defn config->session [connection-config]
  (-> (default-imap-properties connection-config)
      ((security-properties connection-config))
      ((oauth-properties connection-config))
      ((certification-check-properties connection-config))
      Session/getInstance
      ((set-debug-mode connection-config))))

(defn connection-config->store [connection-config]
  ;; Always use the "imap" store. SSL is enabled via mail.imap.ssl.enable (see security-properties), not
  ;; by switching to the "imaps" store. This matters because IdleManager requires the folder to use
  ;; socket channels (mail.imap.usesocketchannels=true), and that — along with every other property here —
  ;; lives under the mail.imap.* prefix. The "imaps" store reads mail.imaps.* instead, so usesocketchannels
  ;; would be ignored and IdleManager.watch would fail with "Folder is not using SocketChannels".
  (.getStore ^Session (config->session connection-config) "imap"))

(defn login
  ([connection-config ^Store store]
   (if (oauth2? connection-config)
     (let [tokens (db/get-oauth-tokens (:id connection-config))]
       (.connect store (:host connection-config) (:user connection-config) (:access-token tokens)))
     (.connect store (:host connection-config) (:user connection-config) (:secret connection-config)))
   store)
  ([connection-config]
   (login connection-config (connection-config->store connection-config))))

(defn folder-separator [^Store store] (.getSeparator (.getDefaultFolder store)))

(defn create-folder [^Store store ^String folder-name result-map]
  (let [folder ^IMAPFolder (.getFolder store folder-name)]
    (if (not (.exists folder))
      (do (.create folder Folder/HOLDS_MESSAGES)
          (conj result-map {folder-name :created}))
      (conj result-map {folder-name :already-exists}))))

(defn default-category-folder-name
  "The default folder for a category, ignoring any custom destination: 'Categories/<Name>'."
  [store lower-case-folder-name]
  (str parent-folder-name (folder-separator store) (s/capitalize lower-case-folder-name)))

(defn structured-folder-name
  "The folder a category's mail should be moved to: the custom destination if configured, otherwise the default 'Categories/<Name>'."
  [store lower-case-folder-name]
  (let [destination-folder (:destination_folder (db/category-by-name lower-case-folder-name))]
    (if (s/blank? destination-folder)
      (default-category-folder-name store lower-case-folder-name)
      destination-folder)))

(defn create-folders
  ([store folder-names]
   (create-folders store folder-names {}))
  ([store folder-names result-map]
   (if (empty? folder-names)
     result-map
     (let [result (create-folder store (structured-folder-name store (first folder-names)) result-map)]
       (recur store (rest folder-names) result)))))

(defn swap-new-period-check [identifier future]
  ;; Cancel any health check already scheduled for this connection before replacing it; otherwise a
  ;; reconnect orphans the previous ScheduledFuture, which keeps running forever on the shared executor.
  ;; swap-vals! makes the replacement atomic, so two concurrent schedulers cannot both keep their future.
  (let [[before _] (swap-vals! health-checks assoc identifier future)]
    (when-let [^ScheduledFuture existing (get before identifier)]
      (when-not (identical? existing future)
        (.cancel existing true)))))

;; Primitives

(defn clean-config [config]
  (-> (dissoc config :secret)
      (dissoc :debug)))

(defn id-from-config [config]
  (str (UUID/nameUUIDFromBytes (.getBytes ^String (str (hash (clean-config config)))))))

(defn- close-folder-without-expunge!
  "Close an IMAP folder WITHOUT expunging. Folder.close() with no argument is close(true): it would
   permanently remove every message the user has flagged \\Deleted in a mail client but not yet
   expunged - in the monitored INBOX or in a category folder - which is not Plauna's decision to make."
  [^Folder folder]
  (when (and (some? folder) (.isOpen folder))
    (.close folder false)))

(defn- attempt-close-step!
  "Run one teardown step, logging instead of propagating: closing a connection must run every step
   even when an earlier one fails (e.g. a folder that became invalid after the store dropped)."
  [label f]
  (try (f)
       (catch Throwable e
         (t/log! {:level :warn :error e} ["Error while" label "- continuing with the remaining teardown steps."]))))

(defrecord ConnectionData [config ^Store store ^Folder folder ^IdleManager idle-manager capabilities ^MessageCountListener message-count-listener]
  AutoCloseable
  (close [this]
    (t/log! :info "Closing the idle manager, removing from health checks, closing the folder and the store.")
    (attempt-close-step! "stopping the idle manager" #(when idle-manager (.stop idle-manager)))
    (attempt-close-step! "removing the message listener" #(stop-monitoring this))
    (swap! health-checks dissoc (:id config))
    (attempt-close-step! "closing the monitored folder" #(close-folder-without-expunge! folder))
    (attempt-close-step! "closing the store" #(when store (.close store)))))

(defn get-connections [] (vals @connections))

(defn connection-data-from-id ^ConnectionData [id]
  (get @connections id))

(declare disconnect)

(declare forget-disconnected-cache!)

(defn add-to-connections
  "Register connection-data under its id. A different ConnectionData registered before (a second
   'connect' while the first is still live) is closed first: otherwise its store, idle manager and
   message listener would keep running unreachable from the UI and every new e-mail would be handled
   twice."
  [^ConnectionData connection-data]
  (let [id (:id (.config connection-data))
        [before _] (swap-vals! connections assoc id connection-data)
        previous (get before id)]
    (when (and (some? previous) (not (identical? previous connection-data)))
      (t/log! :info ["Replacing the existing connection registration of" id "- closing the previous one."])
      (try (disconnect previous)
           (catch Exception e (t/log! {:level :warn :error e} ["Could not close the replaced connection" id]))))
    (forget-disconnected-cache!)
    @connections))

;; Construct email from message

(defn text? [content-type] (s/starts-with? (s/lower-case content-type) "text"))

(defn mime-type [content-type] (s/lower-case (first (s/split content-type #";"))))

(defonce fallback-charset "us-ascii")

(defn charset [content-type]
  ;; Extract the charset parameter wherever it appears (handling optional quotes). A text/* part is not
  ;; required to declare a charset (e.g. "Content-Type: text/plain"); fall back rather than NPE when the
  ;; charset parameter is absent or malformed.
  (or (when (text? content-type)
        (some-> (re-find #"(?i)charset\s*=\s*\"?([^\";\s]+)" content-type)
                second
                s/lower-case))
      fallback-charset))

(defn disposition [disposition] (when (some? disposition) (s/lower-case disposition)))

(defn- sender-address-of
  "The first sender address of a message from its ENVELOPE, or nil."
  [^IMAPMessage message]
  (let [^InternetAddress sender (or (.getSender message) (first (.getFrom message)))]
    (when sender (.getAddress sender))))

(defn envelope-message-id
  "The Message-ID from the ENVELOPE, or a stable synthetic one (see core-email/synthetic-message-id)
   when the message has no such header - Plauna cannot store a message without an id."
  [^IMAPMessage message]
  (let [message-id (.getMessageID message)]
    (if (s/blank? message-id)
      (let [sent (.getSentDate message)]
        (core-email/synthetic-message-id (when sent (quot (.getTime sent) 1000)) (sender-address-of message) (.getSubject message)))
      message-id)))

(defn create-header [^IMAPMessage message]
  (let [sent (.getSentDate message)]
    (new Header (envelope-message-id message) (.getInReplyTo message) (.getSubject message)
                (mime-type (.getContentType message))
                (when sent (quot (.getTime sent) 1000)))))

;; Body parts receive the Message-ID explicitly instead of asking the message again: getMessageID
;; needs the IMAP ENVELOPE, which is exactly what is unavailable for the messages handled by the
;; raw-header fallback below (see envelope-or-raw-headers).
(defmulti create-body-part (fn [body-part _ _] (type body-part)))

(defn- transfer-encoding
  "The Content-Transfer-Encoding of a message or part from the BODYSTRUCTURE the server already sent.
   Reading it through getHeader instead would fetch the part's header block: one extra round trip per
   part and per message."
  [^MimePart part]
  (try (.getEncoding part) (catch Exception _ nil)))

(defmethod create-body-part String [content ^IMAPMessage message message-id]
  (new Body-Part message-id (charset (.getContentType message)) (mime-type (.getContentType message)) (transfer-encoding message) content (.getFileName message) (.getDisposition message)))

(defmethod create-body-part BodyPart [^BodyPart bodypart ^IMAPMessage message message-id]
  (let [content-type (.getContentType bodypart)]
    (if (.isMimeType bodypart "multipart/*")
      (create-body-part (.getContent bodypart) message message-id)
      ;; Only textual content is persisted (as a String). Attachments (PDFs, images, ...) are never
      ;; downloaded: getContent would transfer the complete part from the server only for its bytes to
      ;; be dropped here. Storing them would bloat the DB and, on MariaDB, fail the insert outright.
      ;; Attachments are intentionally not stored, mirroring the mbox parser.
      (let [content (when (text? content-type) (.getContent bodypart))]
        (new Body-Part message-id (charset content-type) (mime-type content-type) (transfer-encoding bodypart)
             (when (string? content) content)
             (.getFileName bodypart) (disposition (.getDisposition bodypart)))))))

(defmethod create-body-part :default [_ ^IMAPMessage message message-id]
  ;; A non-multipart message whose body is neither a String nor a recognised part (e.g. a bare
  ;; attachment): keep its metadata but do not store the (binary) content.
  (new Body-Part message-id (charset (.getContentType message)) (mime-type (.getContentType message)) (transfer-encoding message) nil (.getFileName message) (.getDisposition message)))

(defmethod create-body-part Multipart [^Multipart multipart ^IMAPMessage message message-id]
  (mapv (fn [i] (create-body-part (.getBodyPart multipart i) message message-id))
        (range 0 (.getCount multipart))))

(defn- realize-body-parts [body-parts]
  (vec (flatten [body-parts])))

;; TODO remove duplication with parser.clj
(defn uuid [^String name] (str (java.util.UUID/nameUUIDFromBytes (.getBytes name))))

(defmulti create-participant (fn [address _ _] (type address)))

(defmethod create-participant InternetAddress [^InternetAddress address contact-type message-id]
  (let [name (.getPersonal address)
        address (.getAddress address)
        contact-key (uuid (str name address))]
    (new Participant address name contact-key contact-type message-id)))

(defn create-participants
  "The participants of a message, tagged with message-id (the header's id, synthetic or not)."
  [^IMAPMessage message message-id]
  (let [sender (.getSender message)
        sender-participant (when sender (create-participant sender :sender message-id))
        recipient-participants (mapv (fn [address] (create-participant address :receiver message-id)) (.getRecipients message Message$RecipientType/TO))
        cc-participants (mapv (fn [address] (create-participant address :cc message-id)) (.getRecipients message Message$RecipientType/CC))
        bcc-participants (mapv (fn [address] (create-participant address :bcc message-id)) (.getRecipients message Message$RecipientType/BCC))]
    (filterv some? (flatten [sender-participant recipient-participants cc-participants bcc-participants]))))

;; Raw-header fallback. IMAPMessage answers getMessageID/getSubject/getSentDate/getSender/getRecipients
;; from the server's ENVELOPE. For a message with malformed address headers some servers return an
;; ENVELOPE that angus-mail cannot parse, and every one of those accessors then fails with
;; "Failed to load IMAP envelope" - forever, on every backfill. The raw header block (getHeader) is
;; fetched separately and parses fine, so these helpers rebuild the same data from it, leniently.

(defn raw-header
  "The first value of a header, or nil."
  [^MimeMessage message ^String name]
  (first (.getHeader message name)))

(defn decode-header-text
  "Decode RFC 2047 encoded words; return the text unchanged if it is not decodable."
  [text]
  (when (some? text)
    (try (MimeUtility/decodeText text) (catch Exception _ text))))

(defn header-date-seconds
  "Parse an RFC 5322 Date header into epoch seconds, or nil if it is missing or unparseable."
  [text]
  (when-not (s/blank? text)
    (try (quot (.getTime (.parse (MailDateFormat.) (s/trim text))) 1000)
         (catch Exception _ nil))))

(defn- valid-address? [^InternetAddress address]
  (try (.validate address) true (catch Exception _ false)))

(defn header-addresses
  "Every syntactically valid address of a header, parsed leniently. Invalid entries (and a header that
   cannot be parsed at all) are dropped instead of failing the whole message."
  [^MimeMessage message ^String name]
  (let [values (.getHeader message name)]
    (if (empty? values)
      []
      (try (filterv valid-address? (InternetAddress/parseHeader (s/join ", " values) false))
           (catch Exception e
             (t/log! :debug ["Ignoring unparseable" name "header:" (.getMessage e)])
             [])))))

(defn header-from-raw-headers [^MimeMessage message]
  (let [subject (decode-header-text (raw-header message "Subject"))
        date (header-date-seconds (raw-header message "Date"))
        message-id (raw-header message "Message-ID")
        ^InternetAddress from (or (first (header-addresses message "From")) (first (header-addresses message "Sender")))]
    (new Header (if (s/blank? message-id)
                  (core-email/synthetic-message-id date (when from (.getAddress from)) subject)
                  message-id)
                (raw-header message "In-Reply-To")
                subject
                (mime-type (.getContentType message))
                date)))

(defn participants-from-raw-headers [^MimeMessage message message-id]
  (let [from (or (first (header-addresses message "From")) (first (header-addresses message "Sender")))
        sender-participant (when from (create-participant from :sender message-id))
        recipients (mapv #(create-participant % :receiver message-id) (header-addresses message "To"))
        cc (mapv #(create-participant % :cc message-id) (header-addresses message "Cc"))
        bcc (mapv #(create-participant % :bcc message-id) (header-addresses message "Bcc"))]
    (filterv some? (flatten [sender-participant recipients cc bcc]))))

(defn- envelope-or-raw-headers
  "The [header participants] of a message: from the IMAP ENVELOPE when the server delivers a usable
   one, otherwise rebuilt from the raw header block."
  [^IMAPMessage message]
  (try
    (let [header (create-header message)]
      [header (create-participants message (:message-id header))])
    (catch MessagingException e
      (t/log! :info ["The IMAP envelope of message" (.getMessageNumber message) "could not be loaded (" (.getMessage e) "). Reading its raw headers instead."])
      (let [header (header-from-raw-headers message)]
        [header (participants-from-raw-headers message (:message-id header))]))))

(defn message-id-of
  "The Message-ID of an IMAP message (synthetic when the header is missing), from the ENVELOPE or, when
   that cannot be loaded, the raw header block. The same id message->email stores, so the cheap
   already-stored check recognises messages without a Message-ID header too."
  [^IMAPMessage message]
  (try (envelope-message-id message)
       (catch MessagingException _ (:message-id (header-from-raw-headers message)))))

(defn- message-content
  "What create-body-part should build the body from: the Multipart of a multipart message (built from the
   BODYSTRUCTURE, no body download), the text of a single text part, or a placeholder for a single
   binary part whose bytes are never downloaded because only text is stored."
  [^IMAPMessage message]
  (cond (.isMimeType message "multipart/*") (.getContent message)
        (text? (.getContentType message)) (.getContent message)
        :else ::binary-content))

(defn message->email [^IMAPMessage message]
  (let [[header participants] (envelope-or-raw-headers message)]
    (new Email
         header
         (realize-body-parts (create-body-part (message-content message) message (:message-id header)))
         participants)))

;; Calls

(defn capability-name [^IMAPStore store ^String cap-name]
  (when (.hasCapability store cap-name)
    (keyword (clojure.string/lower-case cap-name))))

(defn capabilities [^Store store]
  (filterv some? (mapv #(capability-name store %) ["MOVE"])))

(declare new-idle-manager)

(defn- ensure-idle-manager!
  "The registered ConnectionData of the connection, with a fresh IdleManager when the current one has
   stopped running (its selector thread died): .watch would otherwise fail with 'IdleManager is not
   running' at every health check and new mail would never be delivered again."
  ^ConnectionData [^ConnectionData connection-data]
  (when (some? connection-data)
    (let [^IdleManager current (:idle-manager connection-data)]
      (if (and (some? current) (.isRunning current))
        connection-data
        (let [id (-> connection-data :config :id)
              ^IdleManager fresh (new-idle-manager (:config connection-data))
              replacement (assoc connection-data :idle-manager fresh)
              ;; Only replace the registration that is still current, so a concurrent reconnect is not undone.
              after (swap! connections (fn [registry] (if (identical? (get registry id) connection-data) (assoc registry id replacement) registry)))]
          (if (identical? (get after id) replacement)
            (do (t/log! :warn ["The IDLE manager of connection" id "was not running any more. Started a new one."])
                replacement)
            ;; Lost the race against a concurrent reconnect: the manager just created would run
            ;; unreferenced forever, so stop it and use whatever is registered now.
            (do (.stop fresh)
                (get after id))))))))

(defn start-idling-for-id [id]
  (when-let [^ConnectionData connection-data (ensure-idle-manager! (connection-data-from-id id))]
    (t/log! :debug ["Starting to idle for id:" id])
    (.watch ^IdleManager (.idle-manager connection-data) (.folder connection-data))))

(defn- record-monitor-failure!
  "Keep a trace of a new message the monitor could not read, so it is listed with the folder's
   failures and can be retried by UID. Best effort: identification must never fail the caller."
  [context connection-id ^IMAPFolder folder folder-name ^IMAPMessage message ^Exception error]
  (try
    (let [attempt (fn [f] (try (f) (catch Exception _ nil)))]
      (int/record-parse-failure (:db context)
                                {:connection-id connection-id
                                 :folder folder-name
                                 :uid (attempt #(.getUID folder message))
                                 :message-number (attempt #(.getMessageNumber message))
                                 :message-id (attempt #(message-id-of message))
                                 :subject (attempt #(try (.getSubject message)
                                                         (catch MessagingException _ (decode-header-text (raw-header message "Subject")))))
                                 :error (str (.getMessage error))}))
    (catch Throwable inner
      (t/log! {:level :warn :error inner} ["Could not record the failure of a new message in" folder-name]))))

(defn message-count-listener [connection-id folder folder-name context]
  (proxy [MessageCountAdapter] []
    (messagesAdded [^MessageCountEvent event]
      (t/log! :debug "Received new message event.")
      (try
        (doseq [^IMAPMessage message (.getMessages event)]
          (try
            (t/log! :debug ["Processing message:" message])
            (.setPeek message true)
            (let [parsed-email (message->email message)
                  process (app/handle-incoming-imap-email parsed-email
                                                          {:connection-id connection-id :origin-folder folder :message message :move? true}
                                                          context)]
              (if (= :error (:result process))
                (t/log! :error ["An error occured while handling incoming message" (:exception process)])
                (t/log! :info ["The email with subject" (-> parsed-email :header :subject) "was handled successfully"])))
            (catch Exception e
              ;; The mail library's event queue swallows listener exceptions silently. Without this catch
              ;; one unreadable message would abort the other new messages of the same event.
              (t/log! {:level :error :error e} ["A new message in" folder-name "could not be read or processed; recording it as a failure and continuing with the next one."])
              (record-monitor-failure! context connection-id folder folder-name message e))))
        (finally
          ;; Re-arm IDLE exactly once, whatever happened above. Otherwise new mail would only be noticed
          ;; again at the next health check.
          (try
            (when-let [conn-data ^ConnectionData (connection-data-from-id connection-id)]
              (t/log! :debug ["Idling on the folder" folder-name "while waiting for new messages."])
              (.watch ^IdleManager (.idle-manager conn-data) (.folder conn-data)))
            (catch Exception e
              (t/log! {:level :error :error e} ["Could not resume IDLE on" folder-name "- the next health check will retry."]))))))))

(defn open-folder-in-store [^Store store ^String folder-name]
  (let [folder ^IMAPFolder (.getFolder store folder-name)]
    (when (not (.isOpen folder))
      (.open folder Folder/READ_WRITE))
    folder))

(defn copy-message
  "Copy a message to the target folder and delete it from the source. Returns true on confirmed success, false if any step failed."
  [^Message message ^Folder source-folder ^Folder target-folder]
  (try
    (.setPeek ^IMAPMessage message true)
    (.copyMessages source-folder (into-array Message [message]) target-folder)
    (t/log! :debug ["Copied" message])
    (.setFlag message Flags$Flag/DELETED true)
    (t/log! :debug ["Set DELETED flag for" message])
    ;; Expunge ONLY the message we copied (UID EXPUNGE, which needs the UIDPLUS extension). The no-arg
    ;; expunge would remove every DELETED-flagged message in the folder, including unrelated ones the
    ;; user flagged in a mail client. Without UIDPLUS the copy stays flagged until a client expunges.
    (if (.hasCapability ^IMAPStore (.getStore source-folder) "UIDPLUS")
      (do (.expunge ^IMAPFolder source-folder (into-array Message [message]))
          (t/log! :debug ["Expunged the copied message from the source folder"]))
      (t/log! :info ["The server supports neither MOVE nor UIDPLUS: the copied message stays in" (.getFullName source-folder) "flagged as deleted until a mail client expunges the folder."]))
    true
    (catch Exception e (t/log! {:level :error :error e} ["There was an error copying and deleting the message" message])
           false)))

(defn inbox-or-category-folder-name [^Store store ^String folder-name default]
  (let [real-default (if (s/blank? default) "INBOX" default)]
    (if (nil? folder-name) real-default (structured-folder-name store folder-name))))

(defn inbox-or-default-category-folder-name
  "Like inbox-or-category-folder-name but resolves a category to its DEFAULT folder, ignoring any custom destination.
   Used to locate emails that were filed before per-email folder tracking existed: they always live under the default scheme."
  [^Store store ^String folder-name default]
  (let [real-default (if (s/blank? default) "INBOX" default)]
    (if (nil? folder-name) real-default (default-category-folder-name store folder-name))))

(defn- ensure-folder-exists!
  "Create a target folder the server does not have yet (a custom category destination that was never
   created). Returns true when the folder exists afterwards."
  [^IMAPFolder folder]
  (or (.exists folder)
      (do (t/log! :info ["Creating the missing IMAP folder" (.getFullName folder)])
          (.create folder Folder/HOLDS_MESSAGES))))

(defn- move-supported? [^Store store]
  (.hasCapability ^IMAPStore store "MOVE"))

(defn move-message
  "Find the proper location for the email and move it there. Returns the name of the folder to which
   the email was moved, or nil when the move did not complete (the message is then still in the
   source folder). Never throws: the e-mail has already been saved when this runs, and a failed move
   must not be mistaken for an unreadable message."
  [connection-id ^Message message ^Folder source-folder ^String target-name]
  (let [connection-data (connection-data-from-id connection-id)
        ;; Resolve and open the target folder from the SAME Store as the source folder. moveMessages and
        ;; the copy fallback cannot operate across two different Stores, and during a bulk parse the source
        ;; lives in a dedicated bulk-read Store rather than the monitor's Store.
        store ^Store (.getStore source-folder)
        structured-folder (inbox-or-category-folder-name store target-name (-> connection-data :config :folder))]
    (try
      (let [target-folder ^IMAPFolder (.getFolder ^Store store ^String structured-folder)]
        (cond
          (= (.getFullName source-folder) structured-folder)
          (do (t/log! :debug ["Target folder" structured-folder "is the same as the source folder. Leaving the message in place."])
              structured-folder)

          (not (ensure-folder-exists! target-folder))
          (do (t/log! :warn ["The target folder" structured-folder "does not exist and could not be created. Leaving the message in place."])
              nil)

          (move-supported? store)
          (do (t/log! :debug ["Moving message from" source-folder "to" target-folder])
              (.setPeek ^IMAPMessage message true)
              (.moveMessages ^IMAPFolder source-folder (into-array Message [message]) target-folder)
              structured-folder)

          :else
          (do (t/log! :debug "Server does not support the IMAP MOVE command. Using copy and delete as fallback.")
              ;; Only report the new folder if the copy+delete actually succeeded, so a failed
              ;; fallback is never recorded as a completed move.
              (when (copy-message message source-folder target-folder)
                structured-folder))))
      (catch Exception e
        (t/log! {:level :warn :error e} ["Moving a message from" (.getFullName source-folder) "to" structured-folder "failed. The message stays where it is."])
        nil))))

(defn monitor->map [monitor]
  (if (nil? monitor)
    {:connected false :folder-open false}
    (let [store ^Store (-> monitor :store)
          folder ^IMAPFolder (-> monitor :folder)]
      {:connected (.isConnected ^Store store)
       :folder-open    (.isOpen ^IMAPFolder folder)})))

(defn folders-in-store [^Store store]
  (.list (.getDefaultFolder store) "*"))

(defn connected? [^ConnectionData connection-data] (.isConnected ^Store (:store connection-data)))

(def disconnected-cache-millis
  "How long the disconnected-connections result is reused. It is computed for the warning banner of
   EVERY page, and computing it costs a database query plus one IMAP NOOP per connection
   (IMAPStore.isConnected pings the server)."
  5000)

(defonce ^:private disconnected-cache (atom nil))

(defn- compute-disconnected-connections []
  (let [active @connections]
    (filterv (fn [conn]
               (let [cd (get active (:id conn))]
                 (or (nil? cd) (not (connected? cd)))))
             (db/get-connections))))

(defn disconnected-connections
  "Returns configured connections (from DB) that are not currently connected.
   A connection is disconnected when it has no active ConnectionData or its store reports false.
   The result is cached briefly, see disconnected-cache-millis."
  []
  (let [now (System/currentTimeMillis)
        cached @disconnected-cache]
    (if (and cached (< (- now (:at cached)) disconnected-cache-millis))
      (:value cached)
      (let [value (compute-disconnected-connections)]
        (reset! disconnected-cache {:at now :value value})
        value))))

(defn forget-disconnected-cache!
  "Drop the cached banner state, e.g. right after a connection was added, connected or removed."
  []
  (reset! disconnected-cache nil))

(defn- set-message-as-peek [^IMAPMessage message] (.setPeek message true))

(defn- set-messages-as-peek [messages] (doseq [message messages] (set-message-as-peek message)))

(defn- move-found-messages!
  "Move already located messages between two open folders of the same Store: with the MOVE command
   when the server has it, otherwise by copy + delete (the library itself has no fallback and fails
   with a BAD response on servers without MOVE). Returns true when every message ended up in target."
  [^IMAPFolder source-folder ^IMAPFolder target-folder found-messages]
  (set-messages-as-peek found-messages)
  (if (move-supported? (.getStore source-folder))
    (do (.moveMessages source-folder (into-array Message found-messages) target-folder)
        true)
    (do (t/log! :debug "Server does not support the IMAP MOVE command. Using copy and delete as fallback.")
        (every? true? (mapv #(copy-message % source-folder target-folder) found-messages)))))

(defn- move-message-between-open-folders!
  "Search message-id in source-folder and move it to target-folder; both folders are already open on
   the same Store. Records the new location. Returns true, :not-found, or false when the move did not
   complete."
  [connection-id ^IMAPFolder source-folder ^IMAPFolder target-folder ^String source-folder-name ^String target-folder-name message-id]
  (let [found-messages (.search source-folder (MessageIDTerm. message-id))]
    (t/log! :debug ["Found" (count found-messages) "messages when searched for the message-id:" message-id])
    (if (some? (seq found-messages))
      (do
        (t/log! :debug ["Moving e-mail from" source-folder-name "to" target-folder-name "using a dedicated IMAP connection"])
        (if (move-found-messages! source-folder target-folder found-messages)
          (do (db/update-email-folder message-id target-folder-name)
              (db/update-email-connection message-id connection-id)
              true)
          false))
      (do (t/log! :info ["No messages found in" source-folder-name "for" message-id])
          :not-found))))

(defn move-message-on-dedicated-store!
  "Move message-id between two folders using a short-lived Store that is independent of IdleManager.
   The Store and both folders are always closed after the attempt - without expunging: with-open would
   call Folder.close(), which is close(true) and permanently deletes every message the user has
   flagged for deletion in a mail client."
  [connection-config source-folder-name target-folder-name message-id]
  (when (oauth2? connection-config)
    (refresh-access-token connection-config))
  (with-open [^Store move-store (login connection-config)]
    (let [open-folders (atom [])
          open! (fn ^IMAPFolder [^String folder-name]
                  (let [folder (open-folder-in-store move-store folder-name)]
                    (swap! open-folders conj folder)
                    folder))]
      (try
        (let [target-folder (open! target-folder-name)
              source-folder (open! source-folder-name)]
          (move-message-between-open-folders! (:id connection-config) source-folder target-folder source-folder-name target-folder-name message-id))
        (finally
          (doseq [folder @open-folders]
            (attempt-close-step! "closing a move folder" #(close-folder-without-expunge! folder))))))))

(defn- resolve-move-folders
  "[source-folder-name target-folder-name] for moving message-id into category target-name on the
   connection. The recorded folder is the source; without one the e-mail predates folder tracking and
   lives under the DEFAULT folder of source-name, never a (newer, possibly-changed) custom destination."
  [^ConnectionData connection-data message-id ^String source-name ^String target-name]
  (let [^Store monitored-store (:store connection-data)
        connection-config (:config connection-data)
        recorded-folder (db/email-folder message-id)]
    [(if (s/blank? recorded-folder)
       (inbox-or-default-category-folder-name monitored-store source-name (:folder connection-config))
       recorded-folder)
     (inbox-or-category-folder-name monitored-store target-name (:folder connection-config))]))

(defn- record-in-place!
  "Nothing moves, but record the resolved location so a previously-unrecorded (legacy) e-mail gets a
   concrete folder and account and stays findable for future moves."
  [connection-id message-id folder-name]
  (t/log! :info ["Source and target folder are both" folder-name "- leaving the message in place."])
  (db/update-email-folder message-id folder-name)
  (db/update-email-connection message-id connection-id)
  true)

(defn move-messages-by-id-between-category-folders
  "Return true if the message could be moved, :not-found if its source folder was searched but no
   matching message exists, and false for other failures (for example a disconnected store)."
  [^String id message-id ^String source-name ^String target-name _context]
  (let [^ConnectionData connection-data (connection-data-from-id id)]
    (if (and (some? connection-data) (connected? connection-data))
      (let [[source-folder-name target-folder-name] (resolve-move-folders connection-data message-id source-name target-name)]
        (if (= source-folder-name target-folder-name)
          (record-in-place! id message-id target-folder-name)
          ;; Never open move folders on the Store used by IdleManager. Closing either folder after the
          ;; move could otherwise close the monitored INBOX and create a gap in real-time delivery.
          (move-message-on-dedicated-store! (:config connection-data) source-folder-name target-folder-name message-id)))
      (do
        (t/log! :info ["IMAP store in connection" id "is not connected. Cancelling the move attempt."])
        false))))

(defn move-emails-by-id!
  "Move many stored messages ([{:message-id :category}]) into their category folders over ONE dedicated
   Store, keeping every folder open for the whole run: one login (and one OAuth refresh) instead of one
   per message. Returns the results in order: true, :not-found or false. A message whose move throws
   counts as false and does not stop the others."
  [^String id moves]
  (let [^ConnectionData connection-data (connection-data-from-id id)]
    (if-not (and (some? connection-data) (connected? connection-data))
      (do (t/log! :info ["IMAP store in connection" id "is not connected. Cancelling the batch move."])
          (vec (repeat (count moves) false)))
      (let [connection-config (:config connection-data)
            open-folders (atom {})]
        (when (oauth2? connection-config) (refresh-access-token connection-config))
        (with-open [^Store move-store (login connection-config)]
          (let [folder-named (fn ^IMAPFolder [^String folder-name]
                               (or (get @open-folders folder-name)
                                   (let [folder (open-folder-in-store move-store folder-name)]
                                     (swap! open-folders assoc folder-name folder)
                                     folder)))]
            (try
              (mapv (fn [{:keys [message-id category]}]
                      (try
                        (let [[source-folder-name target-folder-name] (resolve-move-folders connection-data message-id category category)]
                          (if (= source-folder-name target-folder-name)
                            (record-in-place! id message-id target-folder-name)
                            (move-message-between-open-folders! id (folder-named source-folder-name) (folder-named target-folder-name)
                                                                source-folder-name target-folder-name message-id)))
                        (catch Exception e
                          (t/log! {:level :error :error e} ["Moving" message-id "failed"])
                          false)))
                    moves)
              (finally
                (doseq [^IMAPFolder folder (vals @open-folders)]
                  (try (when (.isOpen folder) (.close folder false))
                       (catch Exception e (t/log! {:level :warn :error e} "Error closing a batch-move folder"))))))))))))

(defn- invalid-grant-error?
  "True only when the provider explicitly rejected the refresh token (HTTP 4xx with an
   invalid_grant error), as opposed to a transient network/5xx error or a timeout."
  [e]
  (let [{:keys [status body]} (ex-data e)]
    (boolean (and status (<= 400 status 499)
                  (re-find #"invalid_grant" (str body))))))

(defn refresh-access-token [connection-config]
  (let [provider (db/get-auth-provider (:auth-provider connection-config))
        token-data (db/get-oauth-tokens (:id connection-config))]
    (if (s/blank? (:refresh-token token-data))
      ;; Nothing to refresh with (never authorized, or the token was deleted after an invalid_grant):
      ;; asking the provider with a nil token would only produce a misleading 'transient error'.
      (t/log! :warn ["Connection" (:user connection-config) "has no stored OAuth refresh token. Log in again from the Connections page."])
      (let [result (try {:token (oauth/exchange-refresh-token-for-access-token provider (:refresh-token token-data))}
                        (catch Exception e {:error e}))]
        (cond
          (some? (:token result))
          (db/update-access-token (:id connection-config) (:token result))

          (invalid-grant-error? (:error result))
          (do (t/log! :info ["Refresh token was rejected by the provider (invalid_grant). Deleting the stored token; the user must log in manually again."])
              (db/delete-access-token (:id connection-config)))

          :else
          ;; Transient failure (network, 5xx, timeout, or empty response): keep the refresh token and retry on the next cycle.
          (t/log! {:level :error :error (:error result)}
                  ["Could not refresh the access token due to a transient error. Keeping the stored refresh token to retry later."]))))))

(defn monitor-folder-name [folder-name]
  (if (or (nil? folder-name) (s/blank? folder-name)) "INBOX" folder-name))

;; Public Interface

(declare restore-connection-if-needed!)

(defonce ^:private intentional-closes
  ;; Ids whose store was closed on purpose (disconnect, delete). JavaMail delivers connection events
  ;; asynchronously, so the CLOSED event of an intentional close arrives after the close call has
  ;; returned; the id stays in this set until the account is connected again.
  (atom #{}))

(defonce ^:private recoveries-in-progress (atom #{}))

(def store-close-recovery-delay-seconds
  "Grace period between an unexpected store close and the reconnect attempt, so the store has finished
   its own cleanup and a short network blip can settle."
  3)

(defn unexpected-store-close?
  "True when a CLOSED/DISCONNECTED event from store belongs to the currently registered connection
   for id and nobody asked for that close. Events from a store that has since been replaced by a
   reconnect are ignored by identity."
  [id store]
  (let [connection-data (connection-data-from-id id)]
    (boolean (and (some? connection-data)
                  (identical? store (:store connection-data))
                  (not (contains? @intentional-closes id))))))

(defn- recover-dropped-connection!
  "Schedule the same repair the periodic health check performs, but right away (after a short grace
   period) instead of waiting for the next interval. At most one recovery runs per connection."
  [id context]
  (let [[before _] (swap-vals! recoveries-in-progress conj id)]
    (when-not (contains? before id)
      (t/log! :warn ["The IMAP store of connection" id "closed unexpectedly. Reconnecting in" store-close-recovery-delay-seconds "seconds."])
      (try
        ;; The scheduler only times the grace period; the blocking reconnect runs on its own thread so
        ;; several recoveries never starve the other connections' health checks on the small pool.
        (.schedule ^ScheduledExecutorService executor-service
                   ^Runnable (fn []
                               (async/thread
                                 (try
                                   (when-let [connection-data (connection-data-from-id id)]
                                     (when-not (contains? @intentional-closes id)
                                       (restore-connection-if-needed! connection-data context)))
                                   (catch Exception e
                                     (t/log! {:level :error :error e} ["Recovering connection" id "after an unexpected store close failed."]))
                                   (finally (swap! recoveries-in-progress disj id)))))
                   (long store-close-recovery-delay-seconds) TimeUnit/SECONDS)
        (catch java.util.concurrent.RejectedExecutionException _
          ;; Shutting down: the executor is gone and so is the need to recover.
          (swap! recoveries-in-progress disj id))))))

(defn- store-connection-listener
  "React to the server dropping the store without waiting for the next health check. An unexpected
   drop surfaces as CLOSED (IMAPStore's cleanup) or DISCONNECTED; both are handled the same way."
  [id context]
  (proxy [ConnectionAdapter] []
    (disconnected [^ConnectionEvent event]
      (when (unexpected-store-close? id (.getSource event))
        (recover-dropped-connection! id context)))
    (closed [^ConnectionEvent event]
      (when (unexpected-store-close? id (.getSource event))
        (recover-dropped-connection! id context)))))

(defonce ^:private idle-executor
  ;; Shared by every connection's IdleManager, which only uses it to run its selector loop. One pool
  ;; instead of one per connect avoids leaking a thread pool on every (re)connect; daemon threads never
  ;; keep the JVM alive after the shutdown hook.
  (Executors/newCachedThreadPool
   (reify ThreadFactory
     (newThread [_ runnable]
       (doto (Thread. ^Runnable runnable "plauna-imap-idle") (.setDaemon true))))))

(defn- new-idle-manager ^IdleManager [connection-config]
  (IdleManager. (config->session connection-config) idle-executor))

(defn construct-connection-data [connection-config context]
  (let [id (:id connection-config)
        store (login connection-config)]
    (try
      (let [folder-name-to-monitor (monitor-folder-name (:folder connection-config))
            folder (open-folder-in-store store folder-name-to-monitor)
            ;; Created only once login and folder open succeeded: an IdleManager starts a selector thread
            ;; that a failed connect attempt would otherwise leak every time.
            idle-manager (new-idle-manager connection-config)
            listener (message-count-listener id folder folder-name-to-monitor context)
            connection-data (->ConnectionData connection-config store folder idle-manager (capabilities store) listener)]
        (.addConnectionListener ^Store store (store-connection-listener id context))
        ;; Registering may close a previous registration of this id (marking that close as intentional),
        ;; so only afterwards is the fresh, deliberately opened store cleared of earlier intentional closes.
        (add-to-connections connection-data)
        (swap! intentional-closes disj id)
        connection-data)
      (catch Exception e
        ;; Opening the monitored folder failed: do not leak the connected store.
        (try (.close ^Store store) (catch Exception _ nil))
        (throw e)))))

(def backfill-message-limit
  "On every (re)connect, re-read at most this many of the most recent messages from the monitored
   folder so mail delivered while the IDLE monitor was down — a dropped connection the health check
   later restores, or the container being restarted — is still saved and categorized. IMAP IDLE only
   pushes messages that arrive while it is connected, so without this a gap is lost until a manual
   folder parse. Bounded because it runs on every connect: already-saved messages are skipped (see
   plauna.application/incoming-email-workflow), so only the genuinely-missed messages do real work.
   A gap larger than this window is still recoverable with the manual folder Parse control."
  200)

(defn backfill-monitored-folder!
  "Catch up on mail missed while the monitor was disconnected by re-reading the most recent messages
   of the monitored folder over a dedicated bulk-read connection (so the live IDLE monitor is left
   undisturbed). Safe to call on every (re)connect: messages already in the database are skipped, so
   this only saves and moves the ones that were actually missed. Processing happens asynchronously."
  [^ConnectionData connection-data context]
  (let [folder-name (monitor-folder-name (-> connection-data :config :folder))]
    (try
      (t/log! :info ["Backfilling up to" backfill-message-limit "recent messages from" folder-name "to catch up on any mail missed while disconnected."])
      (app/read-emails-from-folder connection-data folder-name
                                   {:move? true :limit backfill-message-limit}
                                   context)
      (catch Exception e
        (t/log! {:level :error :error e} ["Could not back-fill missed messages from" folder-name])))))

;; Learning from manual moves: the category folders of every account are scanned periodically for
;; messages that arrived since the last scan. A message the user moved there in a mail client takes
;; the folder's category. A scan over a short-lived Store is used instead of IDLE on every category
;; folder, which would need one permanently open IMAP connection per folder.

(defn category-folders
  "The folders of an account that stand for exactly one category: {folder-name {:id :name}}. A
   category's folder is its destination folder, or the default 'Categories/<Name>'. Left out are the
   monitored folder (a category whose mail is left in place there says nothing about a move) and
   folders shared by several categories (a message there cannot tell which one it means)."
  [separator categories monitored-folder]
  (let [folder-of (fn [{:keys [name destination_folder]}]
                    (if (s/blank? destination_folder)
                      (str parent-folder-name separator (s/capitalize name))
                      (s/trim destination_folder)))
        monitored (monitor-folder-name monitored-folder)
        ;; INBOX is case-insensitive in IMAP, every other folder name is not.
        same-folder? (fn [a b] (if (= "INBOX" (s/upper-case b)) (= "INBOX" (s/upper-case a)) (= a b)))]
    (into {}
          (for [[folder categories-of-folder] (group-by folder-of categories)
                :when (= 1 (count categories-of-folder))
                :when (not (same-folder? folder monitored))]
            [folder (select-keys (first categories-of-folder) [:id :name])]))))

(def category-folder-scan-chunk-size
  "Messages whose envelope and UID are fetched with one IMAP command during a category folder scan."
  200)

(defn- last-uid-in-folder
  "The highest UID a message of the open folder can have right now: UIDNEXT - 1, or the UID of the last
   message when the server does not report UIDNEXT. 0 for an empty folder."
  ^long [^IMAPFolder folder]
  (let [uid-next (.getUIDNext folder)]
    (cond (pos? uid-next) (dec uid-next)
          (pos? (.getMessageCount folder)) (.getUID folder (.getMessage folder (.getMessageCount folder)))
          :else 0)))

(defn- learn-from-scanned-message!
  "Run one message of a category folder through app/learn-category-from-folder!. A message that cannot
   be read is recorded as a failure of the folder and reported as :errors."
  [context connection-id ^IMAPFolder folder folder-name category ^IMAPMessage message]
  (try
    (set-message-as-peek message)
    (app/learn-category-from-folder! context {:connection-id connection-id
                                              :folder folder
                                              :folder-name folder-name
                                              :category category
                                              :message-id (message-id-of message)
                                              :read-email (fn [] {:email (message->email message) :message message})})
    (catch Exception e
      (t/log! {:level :error :error e} ["A message in the category folder" folder-name "could not be read or processed; recording it as a failure."])
      (record-monitor-failure! context connection-id folder folder-name message e)
      :errors)))

(defn scan-category-folder!
  "Handle the messages that arrived in one category folder since the previous scan. The first scan of a
   folder (or one whose UIDVALIDITY changed) only records where the folder ends now: what is already in
   it was filed before and is not treated as a move. Progress is saved after every chunk. Returns
   {:folder :learned :saved :confirmed :errors} plus :baseline or :missing."
  [^Store store connection-id ^String folder-name category {:keys [db] :as context}]
  (let [^IMAPFolder folder (.getFolder store folder-name)
        summary {:folder folder-name :learned 0 :saved 0 :confirmed 0 :errors 0}]
    (if-not (.exists folder)
      (assoc summary :missing true)
      (try
        (.open folder Folder/READ_ONLY)
        (let [uid-validity (.getUIDValidity folder)
              last-uid (last-uid-in-folder folder)
              watermark (int/fetch-category-folder-watermark db connection-id folder-name)]
          (cond
            (or (nil? watermark) (not= uid-validity (:uid-validity watermark)))
            (do (int/save-category-folder-watermark db connection-id folder-name uid-validity last-uid)
                (t/log! :info ["Watching the category folder" folder-name "for e-mails moved there from now on."])
                (assoc summary :baseline true))

            (<= last-uid (:last-uid watermark))
            summary

            :else
            (let [messages (.getMessagesByUID folder (inc (long (:last-uid watermark))) last-uid)
                  profile (doto (FetchProfile.)
                            (.add FetchProfile$Item/ENVELOPE)
                            (.add UIDFolder$FetchProfileItem/UID))]
              (reduce (fn [summary chunk]
                        (let [chunk-array (into-array Message chunk)]
                          (.fetch folder chunk-array profile)
                          (let [result (reduce (fn [summary message]
                                                 (update summary (learn-from-scanned-message! context connection-id folder folder-name category message) inc))
                                               summary
                                               chunk)]
                            ;; Failed messages are recorded as failures, so the scan moves past them as well.
                            (int/save-category-folder-watermark db connection-id folder-name uid-validity
                                                                (.getUID folder ^Message (last chunk)))
                            result)))
                      summary
                      (partition-all category-folder-scan-chunk-size (remove nil? messages))))))
        (finally (close-folder-without-expunge! folder))))))

(defn scan-category-folders!
  "Scan every category folder of a connection over a short-lived Store (the IDLE monitor is left
   undisturbed). One folder that cannot be scanned does not stop the others. Returns the summaries."
  [^ConnectionData connection-data {:keys [db] :as context}]
  (let [config (:config connection-data)
        id (:id config)]
    (when (oauth2? config) (refresh-access-token config))
    (with-open [^Store store (login config)]
      (let [folders (category-folders (folder-separator store) (int/fetch-categories db) (:folder config))
            summaries (mapv (fn [[folder-name category]]
                              (try (scan-category-folder! store id folder-name category context)
                                   (catch Exception e
                                     (t/log! {:level :warn :error e} ["Could not scan the category folder" folder-name "of" (:user config)])
                                     {:folder folder-name :failed true})))
                            folders)
            totals (apply merge-with + (map #(select-keys % [:learned :saved :errors]) summaries))]
        (if (some pos? (vals totals))
          (t/log! :info ["Category folder scan of" (:user config) "-" (:learned totals 0) "recategorized," (:saved totals 0) "newly saved," (:errors totals 0) "failed"])
          (t/log! :debug ["Category folder scan of" (:user config) "found no moved e-mails."]))
        summaries))))

(defonce ^:private category-folder-scans
  ;; connection id -> the ScheduledFuture of its periodic category folder scan.
  (atom {}))

(defonce ^:private category-folder-scans-running (atom #{}))

(defn- run-category-folder-scan!
  "Scan the category folders of the connection registered under id on its own thread, unless a scan
   of it is still running. Does nothing while the connection is down: the scan picks up the moves
   made meanwhile once it is back, because it continues from the saved UIDs."
  [id context]
  (let [[before _] (swap-vals! category-folder-scans-running conj id)]
    (when-not (contains? before id)
      (async/thread
        (try
          (when-let [connection-data (connection-data-from-id id)]
            (when (and (pos? (p/category-folder-scan-interval)) (connected? connection-data))
              (scan-category-folders! connection-data context)))
          (catch Exception e
            (t/log! {:level :error :error e} ["The category folder scan of connection" id "failed."]))
          (finally (swap! category-folder-scans-running disj id)))))))

(defn- cancel-category-folder-scan! [id]
  (let [[before _] (swap-vals! category-folder-scans dissoc id)]
    (when-let [^ScheduledFuture scheduled (get before id)]
      (.cancel scheduled false))))

(def category-folder-scan-initial-delay-seconds
  "Wait after connecting before the first category folder scan, so it does not compete with the
   reconnect back-fill."
  60)

(defn schedule-category-folder-scans
  "Scan the connection's category folders every category-folder-scan-interval seconds (read when the
   connection is established; 0 schedules nothing). Replaces an earlier schedule of the same id."
  [connection-data context]
  (let [id (get-in connection-data [:config :id])
        interval (p/category-folder-scan-interval)]
    (cancel-category-folder-scan! id)
    (when (pos? interval)
      (let [scheduled (.scheduleWithFixedDelay ^ScheduledExecutorService executor-service
                                               ^Runnable (fn [] (run-category-folder-scan! id context))
                                               (long category-folder-scan-initial-delay-seconds) (long interval) TimeUnit/SECONDS)
            [before _] (swap-vals! category-folder-scans assoc id scheduled)]
        (when-let [^ScheduledFuture existing (get before id)]
          (.cancel existing false))))
    connection-data))

(defn disconnect [^AutoCloseable connection-data]
  ;; Mark the close as intentional BEFORE closing: the store's CLOSED event is delivered
  ;; asynchronously and must not be mistaken for a dropped connection.
  (when-let [id (get-in connection-data [:config :id])]
    (swap! intentional-closes conj id))
  (try (.close connection-data)
       (finally (forget-disconnected-cache!))))

(defn remove-connection!
  "Close a connection's live resources (monitor, folder, store) and drop it from the runtime
   registry. Used when a connection's configuration is deleted: without this the ConnectionData
   keeps monitoring the mailbox while no longer appearing anywhere in the UI."
  [id]
  (when-let [connection-data (connection-data-from-id id)]
    (try
      (disconnect connection-data)
      (catch Exception e
        (t/log! {:level :warn :error e} ["Error while closing connection" id "before removing it from the registry."])))
    (swap! connections dissoc id)
    ;; Nothing is registered under this id any more, so the store listener ignores its CLOSED event by
    ;; itself; the marker would otherwise outlive the connection.
    (swap! intentional-closes disj id)
    (forget-disconnected-cache!)))

(defn disconnect-all
  "Close every registered connection. One connection that fails to close must not keep the others open."
  []
  (doseq [connection (vals @connections)]
    (try (disconnect connection)
         (catch Exception e
           (t/log! {:level :warn :error e} ["Error while disconnecting" (get-in connection [:config :id])])))))

(defn stop-health-checks!
  "Cancel the periodic health checks and shut down the client's executors (health checks and IDLE)."
  []
  (doseq [[_ ^ScheduledFuture scheduled] @health-checks]
    (.cancel scheduled true))
  (reset! health-checks {})
  (doseq [[_ ^ScheduledFuture scheduled] @category-folder-scans]
    (.cancel scheduled true))
  (reset! category-folder-scans {})
  (.shutdownNow ^ScheduledExecutorService executor-service)
  (.shutdownNow ^ExecutorService idle-executor))

(defn reconnect [^ConnectionData connection-data]
  (try
    (t/log! :info ["Trying to reconnect to" (-> connection-data .config :host) "as" (-> connection-data .config :user)])
    (if (connected? connection-data)
      (do
        (t/log! :info ["IMAP store is already connected for" (-> connection-data .config :user) "- leaving it open."])
        connection-data)
      (do
        (when (oauth2? (.config connection-data)) (refresh-access-token (.config connection-data)))
        (login (.config connection-data) (.store connection-data))))
    (catch AuthenticationFailedException e (t/log! :error e))))

(defn start-monitoring [connection-data _context]
  (try
    ;; Attach the listener stored in the ConnectionData record — the same instance stop-monitoring
    ;; removes. Attaching a freshly created listener here would make the removal in stop-monitoring a
    ;; no-op, so every stop/start cycle (e.g. a category move on the monitored folder) would stack one
    ;; more listener and each new email would get processed once per stacked listener.
    (.addMessageCountListener ^IMAPFolder (:folder connection-data) ^MessageCountListener (:message-count-listener connection-data))
    (t/log! :info ["Started monitoring for" (:folder (:config connection-data)) "in" (.getURLName ^Store (:store connection-data))])
    (.watch ^IdleManager (:idle-manager connection-data) ^Folder (:folder connection-data))
    (catch Exception e
      (t/log! {:level :error :error e} (.getMessage e))))
  connection-data)

(defn stop-monitoring [connection-data]
  (t/log! :info ["Removing message count listener from folder" (-> connection-data :config :folder)])
  (let [connection-id (:id (:config connection-data))
        sf ^ScheduledFuture (get @health-checks connection-id)]
    (when (some? sf) (.cancel sf true))
    (cancel-category-folder-scan! connection-id))
  (.removeMessageCountListener ^IMAPFolder (:folder connection-data) (:message-count-listener connection-data))
  connection-data)

(defn create-category-folders!
  "Creates folders for the selected categories. Checks if the connection is still intact. Does nothing, if the connection is not intact."
  [connection-data categories]
  (if (connected? connection-data)
    (do (t/log! :info ["Creating directories from category names" categories])
        (let [result (create-folders (:store connection-data) categories)]
          (t/log! {:level :info
                   :data  {:result result}}
                  "Created the directories.")))
    (t/log! :info "Could not create directories on the IMAP server: The store is not connected."))
  connection-data)

(defn restore-connection-if-needed!
  "Reconnect a dropped store, back-fill mail missed meanwhile, reopen the monitored folder and resume
   IDLE. Shared by the periodic health check and the store's connection listener; the two are
   serialized per connection so a drop is never repaired twice at once."
  [^ConnectionData connection-data context]
  ;; The lock is the Store, not the record: replacing the IdleManager (ensure-idle-manager!) creates a
  ;; new record for the same store, and the health check's closure still holds the old one.
  (locking (:store connection-data)
   (let [^Store store (:store connection-data)
        ^Folder folder (:folder connection-data)
        config (:config connection-data)
        registered (connection-data-from-id (:id config))]
    (if-not (and (some? registered) (identical? store (:store registered)))
      ;; A stale health check of a connection that was reconnected or removed meanwhile: touching the
      ;; old store would only re-open a zombie IMAP session nobody listens to.
      (t/log! :debug ["Skipping the health check of a replaced connection" (:id config)])
    (try
      (t/log! :debug ["Checking if the connection for" (:user config) "is open"])
      (let [reconnected? (if (.isConnected store)
                           (do (t/log! :debug "Store is still connected.") false)
                           (do (t/log! :warn "Connection lost. Reconnecting to email server...")
                               (reconnect connection-data)
                               (.isConnected store)))
            _ (t/log! :debug ["Checking if the folder " (:folder config) "is open"])
            reopened? (if (.isOpen folder)
                        (do (t/log! :debug "Folder is still open.") false)
                        (do (t/log! :info "Folder is closed. Opening it again.")
                            (.open folder Folder/READ_WRITE)
                            true))]
        ;; While the store was down OR the folder was closed, no IDLE push could arrive and re-opening a
        ;; folder does not replay what arrived meanwhile: back-fill in both cases.
        (when (or reconnected? reopened?)
          (backfill-monitored-folder! connection-data context))
        (t/log! :debug "Idling and waiting for messages after a health check.")
        (start-idling-for-id (:id config)))
      (catch Exception e
        (t/log! {:level :error :error e} "There was an error during health check. The connection is probably in a broken state.")))))))

(defn schedule-health-checks [^ConnectionData connection-data context]
  (let [config (:config connection-data)
        scheduled-future (.scheduleAtFixedRate ^ScheduledExecutorService executor-service
                                               #(restore-connection-if-needed! connection-data context)
                                               120 (p/client-health-check-interval) TimeUnit/SECONDS)]
    ;; Two connects for the same id can interleave (the UI and the startup retry): only the connection
    ;; that is registered right now may own the health check; a check for a replaced (closed) store
    ;; would keep re-opening a zombie IMAP session.
    (if (identical? (connection-data-from-id (:id config)) connection-data)
      (swap-new-period-check (:id config) scheduled-future)
      (do (t/log! :info ["Connection" (:id config) "was replaced while connecting; not scheduling a health check for the replaced one."])
          (.cancel ^ScheduledFuture scheduled-future true)))
    connection-data))

(defn folder-from-connection [connection-data folder-name]
  (let [^Store store (:store connection-data)]
    (open-folder-in-store store folder-name)))

(defn id-from-connection [connection-data] (get-in connection-data [:config :id]))

(defn open-folder-for-bulk-read
  "Open folder-name on a DEDICATED Store connection, separate from the monitoring connection.
   Bulk reading and the IDLE monitor therefore never share a connection or folder object, which
   avoids deadlocking the monitored folder's IDLE. Returns a handle to close with close-folder-for-bulk-read."
  [connection-data folder-name]
  (let [connection-config (:config connection-data)]
    (when (oauth2? connection-config) (refresh-access-token connection-config))
    (let [^Store store (login connection-config)]
      (try
        (let [^IMAPFolder folder (open-folder-in-store store folder-name)]
          {:store store
           :folder folder
           :message-count (.getMessageCount folder)
           :connection-id (id-from-connection connection-data)})
        (catch Exception e
          ;; Opening/counting failed - don't leak the just-opened Store.
          (try (.close store) (catch Exception _ nil))
          (throw e))))))

(defn close-folder-for-bulk-read [bulk-handle]
  (let [^Folder folder (:folder bulk-handle)
        ^Store store (:store bulk-handle)]
    (when (some? folder)
      (try (when (.isOpen folder) (.close folder false))
           (catch Exception e (t/log! {:level :error :error e} "Error closing bulk-read folder"))))
    (when (some? store)
      (try (.close store)
           (catch Exception e (t/log! {:level :error :error e} "Error closing bulk-read store"))))))

(defn received-since-message-numbers
  "The sequence numbers (ascending) of the messages of an open folder that were received on or after the
   calendar day since (IMAP SEARCH SINCE on the INTERNALDATE: the server compares whole days and ignores
   the time of day and time zone). The date is built in the JVM's own zone because the mail library
   writes it into the command in that zone - any other zone could shift it onto a neighbouring day."
  [^IMAPFolder folder ^LocalDate since]
  (let [date (Date/from (.toInstant (.atStartOfDay since (ZoneId/systemDefault))))
        found (.search folder (ReceivedDateTerm. ComparisonTerm/GE date))]
    (vec (sort (map (fn [^Message message] (.getMessageNumber message)) found)))))

(defn refetch-message-by-id
  "Re-read a single message from the IMAP server by its Message-ID and return a freshly parsed Email,
   or nil if it cannot be found on any connected account. Searches the email's recorded folder first,
   then each account's monitored folder and INBOX. Uses a dedicated bulk-read connection so the IDLE
   monitor is left undisturbed."
  [message-id]
  (let [recorded (db/email-folder message-id)
        recorded-connection (db/email-connection-id message-id)
        candidate-folders (fn [^ConnectionData cd]
                            (distinct (remove s/blank? [recorded (-> cd :config :folder) "INBOX"])))
        ;; Look in the account the e-mail is known to belong to before trying the others.
        ordered-connections (sort-by (fn [^ConnectionData cd] (if (= recorded-connection (-> cd :config :id)) 0 1))
                                     (vals @connections))]
    (some (fn [^ConnectionData cd]
            (when (connected? cd)
              (some (fn [folder-name]
                      (let [bulk (try (open-folder-for-bulk-read cd folder-name)
                                      (catch Exception e
                                        (t/log! {:level :warn :error e} ["Could not open folder" folder-name "while re-fetching" message-id])
                                        nil))]
                        (when bulk
                          (try
                            (when-let [^IMAPMessage msg (first (.search ^IMAPFolder (:folder bulk) (MessageIDTerm. message-id)))]
                              (set-message-as-peek msg)
                              (message->email msg))
                            (finally (close-folder-for-bulk-read bulk))))))
                    (candidate-folders cd))))
          ordered-connections)))

(defmulti connect (fn [config _] (:auth-type config)))

(defmethod connect "oauth2" [connection-config context]
  (refresh-access-token connection-config)
  (try (let [connection-data (-> (construct-connection-data connection-config context)
                                 (start-monitoring context)
                                 (schedule-health-checks context)
                                 (schedule-category-folder-scans context))]
         (backfill-monitored-folder! connection-data context)
         connection-data)
       (catch AuthenticationFailedException e (t/log! :error e))))

(defmethod connect :default [connection-config context]
  (let [connection-data (-> (construct-connection-data connection-config context)
                            (start-monitoring context)
                            (schedule-health-checks context)
                            (schedule-category-folder-scans context))]
    (backfill-monitored-folder! connection-data context)
    connection-data))

(defn connection-id-for-email
  "Tries to find out the id of the connection the email belongs to. Returns nil if no active connection is found."
  [connection-data-vec email]
  (loop  [connections connection-data-vec
          result nil]
    (if (or (some? result) (nil? (seq connections)))
      result
      (let [^ConnectionData connection (first connections)
            recipients (filterv #(= :receiver (:type %)) (:participants email))
            connection-user (get-in connection [:config :user])
            match (get (filterv (fn [sender] (= (:address sender) connection-user)) recipients) 0)]
        (if (some? match) (recur (rest connections) (get-in connection [:config :id])) (recur (rest connections) nil))))))

(defrecord ImapClient []
  int/EmailClient
  (start-monitor [_ config context] (connect config context))
  (connections [_] @connections)
  (create-category-directories! [_ connection-data category-names] (create-category-folders! connection-data category-names))
  (connection-id-for-email [_ connections email] (connection-id-for-email connections email))
  (move-email-between-categories [_ connection-id message-id old-category new-category context] (move-messages-by-id-between-category-folders connection-id message-id old-category new-category context))
  (move-email-to-category [_ connection-id message original-folder category] (move-message connection-id message original-folder category))
  (move-emails-by-id [_ connection-id moves] (move-emails-by-id! connection-id moves))
  (prefetch-message-identities [_ folder from to]
    ;; One FETCH for a whole range of envelopes, so the per-message identity lookups that follow are
    ;; answered from the cache. Best effort: a range the server cannot serve is simply read one by one.
    (try
      (let [^IMAPFolder folder folder
            messages (.getMessages folder (int (min from to)) (int (max from to)))
            profile (doto (FetchProfile.) (.add FetchProfile$Item/ENVELOPE))]
        (.fetch folder messages profile)
        (count messages))
      (catch Exception e
        (t/log! :debug ["Could not prefetch envelopes" from "-" to ":" (.getMessage e)])
        0)))
  (message-numbers-since [_ folder since] (received-since-message-numbers folder since))
  (current-folder-name [_ folder] (.getFullName ^Folder folder))
  (open-folder-for-bulk-read [_ connection-data folder-name] (open-folder-for-bulk-read connection-data folder-name))
  (close-folder-for-bulk-read [_ bulk-handle] (close-folder-for-bulk-read bulk-handle))
  (nth-email-from-folder [_ n folder]
    (let [message (.getMessage ^IMAPFolder folder n)]
      (set-message-as-peek message)
      (t/log! :debug ["Reading message number" n "from" (.getName ^IMAPFolder folder)])
      {:email (message->email message)
       :message message}))
  (nth-message-id-from-folder [_ n folder]
    ;; Only the envelope (or, as a fallback, the header block) is loaded, not the body, so this stays
    ;; cheap for known messages.
    (let [^IMAPMessage message (.getMessage ^IMAPFolder folder n)]
      (set-message-as-peek message)
      (message-id-of message)))
  (nth-message-identity-from-folder [_ n folder]
    (let [^IMAPMessage message (.getMessage ^IMAPFolder folder n)
          attempt (fn [f] (try (f) (catch Exception _ nil)))]
      (set-message-as-peek message)
      {:uid (attempt #(.getUID ^IMAPFolder folder message))
       :message-id (attempt #(message-id-of message))
       :subject (attempt #(try (.getSubject message)
                               (catch MessagingException _ (decode-header-text (raw-header message "Subject")))))}))
  (email-by-uid-from-folder [_ uid folder]
    (when-let [^IMAPMessage message (.getMessageByUID ^IMAPFolder folder (long uid))]
      (set-message-as-peek message)
      (t/log! :debug ["Reading message with UID" uid "from" (.getName ^IMAPFolder folder)])
      {:email (message->email message)
       :message message})))
