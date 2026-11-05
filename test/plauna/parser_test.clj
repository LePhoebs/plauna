(ns plauna.parser-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [plauna.files :as files]
            [taoensso.telemere :as t]
            [plauna.util.async :as async-utils]
            [plauna.core.email :as core-email]
            [plauna.parser :as parser]
            [clojure.core.async :refer [pub sub chan >!!] :as async]))

(t/set-ns-filter! {:disallow "plauna.*"})

(defn resource->is [resource-path]
  (io/input-stream (io/resource resource-path)))

;; Testing email parsing

(deftest basic-parse-test
  (let [test-chan (chan)
        test-pub (pub test-chan :type)
        email-bytes (.getBytes ^String (slurp (io/resource "test/email_corpus/simple-lorem-ipsum.eml")))]
    (parser/parser-event-loop test-pub test-chan)
    (>!! test-chan {:type :received-email :options {} :payload email-bytes})
    (let [results-chan (chan)
          _ (sub test-pub :parsed-email results-chan)
          parsed-mail (:payload (async-utils/fetch-or-timeout!! results-chan 1000))]
      (is (= "<unique_message_id@example.com>" (:message-id (:header parsed-mail))))
      (is (= "Lorem Ipsum Sample" (:subject (:header parsed-mail))))
      (is (= "text/plain" (:mime-type (first (:body parsed-mail)))))
      (is (= "Dear Test,\r
\r
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed ac justo vel odio efficitur consectetur. Integer nec urna vitae elit imperdiet ultrices. Fusce vel neque vel justo dapibus luctus a eget quam.\r
\r
Sincerely,\r
Tester\r
" (:content (first (:body parsed-mail))))))))

(deftest parse-test-2
  (let [test-chan (chan)
        test-pub (pub test-chan :type)
        email-bytes (.getBytes ^String (slurp (io/resource "test/email_corpus/greek-text.mbox")))]
    (parser/parser-event-loop test-pub test-chan)
    (>!! test-chan {:type :received-email :options {} :payload email-bytes})
    (let [results-chan (chan)
          _ (sub test-pub :parsed-email results-chan)
          parsed-mail (:payload (async-utils/fetch-or-timeout!! results-chan 1000))]
      (is (= "Παράδοση" (:subject (:header parsed-mail)))))))

(deftest parse-test-3
  (let [test-chan (chan)
        test-pub (pub test-chan :type)
        email-bytes (.getBytes ^String (slurp (io/resource "test/email_corpus/multipart-with-text-attachment.eml")))]
    (parser/parser-event-loop test-pub test-chan)
    (>!! test-chan {:type :received-email :options {} :payload email-bytes})
    (let [results-chan (chan)
          _ (sub test-pub :parsed-email results-chan)
          parsed-mail (:payload (async-utils/fetch-or-timeout!! results-chan 1000))]
      (is (= "Multipart With Text Attachment" (:subject (:header parsed-mail))))
      (is (= 2 (count (:body parsed-mail))))
      (is (false? (core-email/attachment? (first (:body parsed-mail)))))
      (is (true? (core-email/attachment? (second (:body parsed-mail))))))))

;; Wrong data tests

(deftest wrong-data-1
  ;; The mbox holds 16 messages, most of them patch mails WITHOUT a Message-ID header but with From,
  ;; Date and Subject. Those used to be dropped; they now get a stable synthetic id and get through.
  ;; Only fragments with none of the identifying headers are still dropped.
  (let [inner-chan (chan 20)
        test-chan (pub inner-chan :type)]
    (parser/parser-event-loop test-chan inner-chan)
    (files/read-emails-from-mbox (resource->is "test/email_corpus/weird-mbox.mbox") inner-chan)
    (let [results-chan (chan)]
      (sub test-chan :parsed-enrichable-email results-chan)
      (loop [event (async-utils/fetch-or-timeout!! results-chan 200) results []]
        (if (or (nil? event) (= :timed-out event))
          (let [message-ids (map #(get-in % [:payload :header :message-id]) results)]
            (is (> (count results) 3) "Messages without a Message-ID header are kept")
            (is (every? #(not (clojure.string/blank? %)) message-ids) "Every kept message has an id")
            (is (some core-email/synthetic-message-id? message-ids) "Most of them carry a synthetic id")
            (is (some #(= "<u5tacjjdpxq.fsf@lysator.liu.se>" %) message-ids) "Real Message-IDs are kept as they are"))
          (recur (async-utils/fetch-or-timeout!! results-chan 200) (conj results event)))))))


(deftest mbox-message-without-message-id-gets-a-synthetic-one
  (let [raw (str "From: shop@example.com\r\nTo: me@example.com\r\nSubject: Invoice\r\nDate: Tue, 2 Sep 2026 10:00:00 +0200\r\nContent-Type: text/plain\r\n\r\nbody\r\n")
        email (parser/parse-email (java.io.ByteArrayInputStream. (.getBytes raw "UTF-8")))]
    (is (= (core-email/synthetic-message-id 1788336000 "shop@example.com" "Invoice") (-> email :header :message-id)))
    (is (every? #(= (-> email :header :message-id) (:message-id %)) (:participants email)) "Participants carry the same id")
    (is (every? #(= (-> email :header :message-id) (:message-id %)) (:body email)) "Body parts carry the same id")))

(deftest a-fragment-without-any-identifying-header-is-still-dropped
  (let [email (parser/parse-email (java.io.ByteArrayInputStream. (.getBytes "Content-Type: text/plain\r\n\r\njust text\r\n" "UTF-8")))]
    (is (nil? (-> email :header :message-id)))
    (is (false? (parser/with-message-id? email)))))

(deftest dropped-fragments-are-answered-with-a-discarded-email-event
  (let [test-chan (chan)
        test-pub (pub test-chan :type)
        results-chan (chan)]
    (parser/parser-event-loop test-pub test-chan)
    (sub test-pub :discarded-email results-chan)
    (>!! test-chan {:type :received-email :options {:enrich true} :payload (.getBytes "Content-Type: text/plain\r\n\r\njust text\r\n" "UTF-8")})
    (let [event (async-utils/fetch-or-timeout!! results-chan 2000)]
      (is (= :discarded-email (:type event)))
      (is (= :no-message-id (get-in event [:payload :reason])))
      (is (true? (get-in event [:options :enrich])) "The original options travel with the answer")))
  "The mbox limiter can account for fragments that never become e-mails")

(deftest mbox-e-mails-store-sender-and-from-like-imap
  (let [raw (str "Message-ID: <list-1@x>\r\n"
                 "From: News <news@shop.de>\r\n"
                 "Sender: bounce@mcsv.net\r\n"
                 "To: me@example.com\r\n"
                 "Subject: Angebote\r\n"
                 "Content-Type: text/plain\r\n\r\nHallo\r\n")
        email (parser/parse-email (java.io.ByteArrayInputStream. (.getBytes raw "UTF-8")))]
    (is (= [[:sender "bounce@mcsv.net"] [:from "news@shop.de"] [:receiver "me@example.com"]]
           (mapv (juxt :type :address) (:participants email))))))
