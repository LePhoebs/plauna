(ns plauna.core.email
  (:require [clojure.math :as math]
            [clojure.string :as s]))

(set! *warn-on-reflection* true)

(defrecord Header [message-id in-reply-to subject mime-type date])

(defrecord Body-Part [message-id charset mime-type transfer-encoding content filename content-disposition])

(defrecord Participant [address name contact-key type message-id])

(defrecord Email [^Header header body participants])

(defrecord Metadata [message-id language language-modified language-confidence category category-id category-modified category-confidence connection-id])

(defrecord EnrichedEmail [^Header header body participants ^Metadata metadata])

(defrecord EnrichedBodyPart [^Body-Part body-part ^Metadata metadata])

(defn synthetic-message-id
  "A stable stand-in for a missing Message-ID header, derived from the message's date (epoch seconds),
   first sender address and subject. The same message therefore gets the same id whether it is read
   from IMAP or an mbox file, in whichever folder it lies, so it is recognised as already stored on a
   later run. Two genuinely different messages with identical date, sender and subject collide, which
   is the same outcome as for two messages sharing a real Message-ID."
  [date sender-address subject]
  (str "<plauna-"
       (java.util.UUID/nameUUIDFromBytes (.getBytes (str (or date "") "|" (s/lower-case (str sender-address)) "|" (s/trim (str subject)))
                                                    java.nio.charset.StandardCharsets/UTF_8))
       "@generated.plauna>"))

(defn synthetic-message-id?
  "True for an id produced by synthetic-message-id."
  [message-id]
  (boolean (and (string? message-id) (s/starts-with? message-id "<plauna-") (s/ends-with? message-id "@generated.plauna>"))))

(def ^:private encoded-message-id-pattern #"=\?UTF-8\?B\?([A-Za-z0-9+/]*={0,2})\?=")

(defn storable-message-id
  "The form in which Plauna stores and compares a Message-ID. An id of ASCII characters - practically
   every id - stays as it is. One with other characters (internationalized mail, RFC 6532) becomes
   =?UTF-8?B?<base64 of its UTF-8 bytes>?=: MariaDB keeps Message-IDs in ASCII columns, which reject it
   and with it the whole e-mail. No real Message-ID can look like that encoded form, so the two never
   collide, and imap-message-id turns it back for the mail server."
  [message-id]
  (if (and (string? message-id) (some #(> (int %) 127) message-id))
    (str "=?UTF-8?B?"
         (.encodeToString (java.util.Base64/getEncoder) (.getBytes ^String message-id java.nio.charset.StandardCharsets/UTF_8))
         "?=")
    message-id))

(defn imap-message-id
  "The Message-ID as the mail server knows it: storable-message-id undone. Used for IMAP searches and
   for showing the id."
  [message-id]
  (if-let [[_ encoded] (and (string? message-id) (re-matches encoded-message-id-pattern message-id))]
    (try (String. (.decode (java.util.Base64/getDecoder) ^String encoded) java.nio.charset.StandardCharsets/UTF_8)
         (catch IllegalArgumentException _ message-id))
    message-id))

(defn construct-body-part [body-part] (map->Body-Part body-part))

(defn construct-participants [participant]
  (let [args ((juxt :address :name :contact-key :type :message-id) participant)]
    (if (keyword? (get args 3))
      (apply ->Participant args)
      (apply ->Participant (assoc args 3 (keyword (s/replace-first (get args 3) ":" "")))))))

(defn construct-header [header] (map->Header header))

(defn construct-email [raw-header raw-body raw-participants]
  ;(sp/conform ::email-specs/body raw-body)
  (let [body-parts (map construct-body-part raw-body)
        participants (map construct-participants raw-participants)
        header (construct-header raw-header)]
    (->Email header body-parts participants)))

(defn construct-enriched-email
  "connection-id (optional) records the IMAP account the e-mail was read from, so a later move can go
   straight to that account."
  ([email language-metadata category-metadata]
   (construct-enriched-email email language-metadata category-metadata nil))
  ([email language-metadata category-metadata connection-id]
   (->EnrichedEmail (:header email)
                    (:body email)
                    (:participants email)
                    (->Metadata (-> email :header :message-id)
                                (:language language-metadata)
                                (get language-metadata :language-modified nil)
                                (:language-confidence language-metadata)
                                (:category category-metadata)
                                (:category-id category-metadata)
                                (get category-metadata :category-modified nil)
                                (:category-confidence category-metadata)
                                connection-id))))

(defn iterate-over-all-pages [call-with-pagination fun query sql-query mutates?]
  (let [data-with-current-page (call-with-pagination query sql-query)
        size (:size (:page query))
        remaining-pages (if (and size (pos? size) (:total data-with-current-page))
                          (-> (/ (:total data-with-current-page) size)
                              math/ceil
                              (- (:page data-with-current-page)))
                          0)]
    (fun (:data data-with-current-page))
    (if (> remaining-pages 0)
      (recur call-with-pagination fun (if mutates? query (update-in query [:page :page] inc)) sql-query mutates?)
      nil)))

(defn attachment? [body-part] (or (= "attachment" (:content-disposition body-part)) (some? (:filename body-part))))

(defn- usable-charset
  "The java.nio Charset a declared MIME charset names, or nil when it names none Java knows. Sloppy
   UTF-8 declarations (\"utf-8\" in quotes, \\UTF-8, utf8) count as UTF-8."
  ^java.nio.charset.Charset [declared]
  (let [cleaned (some-> declared str s/trim (s/replace #"[\"'\\]" "") s/lower-case)]
    (cond (s/blank? cleaned) nil
          (re-matches #".*utf-?8.*" cleaned) java.nio.charset.StandardCharsets/UTF_8
          :else (try (java.nio.charset.Charset/forName cleaned) (catch Exception _ nil)))))

(defn- valid-utf8? [^bytes bytes]
  (try (-> java.nio.charset.StandardCharsets/UTF_8
           .newDecoder
           (.onMalformedInput java.nio.charset.CodingErrorAction/REPORT)
           (.onUnmappableCharacter java.nio.charset.CodingErrorAction/REPORT)
           (.decode (java.nio.ByteBuffer/wrap bytes)))
       true
       (catch java.nio.charset.CharacterCodingException _ false)))

(defn decode-text
  "Decode the bytes of a text part. A part that declares a charset Java knows is decoded with it - except
   US-ASCII, which is also what mail libraries assume when a part declares nothing: such a part often
   carries umlauts anyway, so it is read as UTF-8 when the bytes are valid UTF-8 and as Windows-1252
   otherwise (pure ASCII reads the same either way). Windows-1252 text is practically never valid UTF-8,
   so the guess is reliable."
  ^String [^bytes bytes declared-charset]
  (let [charset (usable-charset declared-charset)]
    (if (and (some? charset) (not= charset java.nio.charset.StandardCharsets/US_ASCII))
      (String. bytes charset)
      (String. bytes (if (valid-utf8? bytes)
                       java.nio.charset.StandardCharsets/UTF_8
                       (java.nio.charset.Charset/forName "windows-1252"))))))

(defn text-content? [mime-type] (and (some? mime-type) (.startsWith ^String mime-type "text")))

(defn body-text-content? [body-part] (text-content? (:mime-type body-part)))

(defn text-content-type [body-part]
  (let [mime-type (:mime-type body-part)]
    (cond (nil? mime-type) :plain
          (.endsWith ^String mime-type "html") :html
          (.endsWith ^String mime-type "rtf") :rtf
          :else :plain)))

(defn body-part-for-mime-type
  "When supplied with an e-mail, select a mime-type and extract its contents for training purposes.
  If the selected mime-type does not exist, it returns the text content of the first mime-type available.
  If the e-mail has no body, returns nil."
  [mime-type email]
  (let [body-parts (filter #(and (not (attachment? %)) (body-text-content? %)) (:body email))]
    (cond (empty? (:body email)) ;; is this possible?
          nil
          (= 1 (count body-parts))
          (first body-parts)
          :else
          (let [first-match (first (filter #(.equals ^String (:mime-type %) mime-type) body-parts))]
            (if (some? first-match) first-match (first body-parts))))))

(defrecord ImapConnection [host user secret folder security port debug check-ssl-certs])

(def type-check-imap-connection
  ;; These fields used to optional in the configuration. Now we need to make sure that they are set properly.
  (comp (fn [connection] (update connection :check-ssl-certs #(or (nil? %) (= % true))))
        (fn [connection] (update connection :debug (fn [x] (if (nil? x) false x))))
        (fn [connection] (update connection :security (fn [x] (if (nil? x) "ssl" x))))))

(defn construct-imap-connection-from-config-file [data-map]
  (cond (and (some? (:host data-map))
             (some? (:user data-map))
             (some? (:secret data-map))
             (some? (:folder data-map)))
        (map->ImapConnection (type-check-imap-connection data-map))))
