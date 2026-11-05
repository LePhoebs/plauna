(ns plauna.core.email-test
  (:require [clojure.test :refer :all]
            [plauna.core.email :as core.email]))

(defn create-mock-header []
  (core.email/->Header "test123" nil "test subject" "multipart" 1123))

(defn create-body-part [mime-type content]
  (core.email/->Body-Part "test123" "utf-8" mime-type "test" content nil nil))

(defn create-email-with-fake-body [mimetype-content-tuples]
  (core.email/->Email (create-mock-header) (map (fn [tuple] (create-body-part (first tuple) (second tuple))) mimetype-content-tuples) []))

(deftest training-content-single-body-part
  (let [email (core.email/->Email (create-mock-header)
                                  [(create-body-part "text/html" "Simple content")]
                                  [])
        training-data (core.email/body-part-for-mime-type "text/html" email)]
    (testing
     (is (= (:message-id training-data) "test123"))
      (is (= (:content training-data) "Simple content")))))

(deftest training-content-single-body-part-wrong-mime-type
  (let [email (core.email/->Email (create-mock-header)
                                  [(create-body-part "text/html" "Simple content")]
                                  [])
        training-data (core.email/body-part-for-mime-type "text/text" email)]
    (testing
     (is (= (:message-id training-data) "test123"))
      (is (= (:content training-data) "Simple content")))))

(deftest training-content-multiple-body-parts
  (let [email (core.email/->Email (create-mock-header)
                                  [(create-body-part "text/plain" "Simple content")
                                   (create-body-part "text/html" "Better content")]
                                  [])
        training-data (core.email/body-part-for-mime-type "text/html" email)]
    (testing
     (is (= "test123" (:message-id training-data)))
      (is (= "Better content" (:content training-data))))))

(deftest training-content-empty-text-contents
  (let [mock-data (core.email/->Email (create-mock-header) [] [])
        result (core.email/body-part-for-mime-type "text/plain" mock-data)]
    (is (= nil result))))

(deftest training-content-single-text-contents
  (let [mock-data (create-email-with-fake-body [["text/plain" "test"]])
        result (core.email/body-part-for-mime-type "text/plain" mock-data)]
    (is (= "text/plain" (:mime-type result)))
    (is (= "test" (:content result)))))

(deftest trained-emails-single-text-contents-different-mimetype
  (let [mock-data (create-email-with-fake-body [["text/html" "test"]])
        result (core.email/body-part-for-mime-type "text/plain" mock-data)]
    (is (= "text/html" (:mime-type result)))
    (is (= "test" (:content result)))))

(deftest trained-emails-multiple-text-contents
  (let [mock-data (create-email-with-fake-body [["text/html" "test"] ["text/plain" "test2"]])
        result (core.email/body-part-for-mime-type "text/plain" mock-data)]
    (is (= "text/plain" (:mime-type result)))
    (is (= "test2" (:content result)))))

(deftest trained-emails-without-any-text
  (let [mock-data (create-email-with-fake-body [["image/jpg" "binary-data"] ["image/pn" "binary-data"]])
        result (core.email/body-part-for-mime-type "text/plain" mock-data)]
    (is (= nil result))))

(deftest construct-enriched-email-records-the-connection
  (let [email {:header {:message-id "m-1"} :body [] :participants []}
        without (core.email/construct-enriched-email email {:language "eng"} {:category "a"})
        with (core.email/construct-enriched-email email {:language "eng"} {:category "a"} "conn-1")]
    (is (nil? (-> without :metadata :connection-id)))
    (is (= "conn-1" (-> with :metadata :connection-id)))
    (is (= "a" (-> with :metadata :category)))))

(deftest synthetic-message-id-is-stable-and-recognisable
  (let [a (core.email/synthetic-message-id 1756800000 "Shop@Example.com" " Invoice ")
        b (core.email/synthetic-message-id 1756800000 "shop@example.com" "Invoice")
        c (core.email/synthetic-message-id 1756800001 "shop@example.com" "Invoice")]
    (is (= a b) "Case of the address and surrounding whitespace do not change the id")
    (is (not= a c) "A different date gives a different id")
    (is (core.email/synthetic-message-id? a))
    (is (not (core.email/synthetic-message-id? "<real@example.com>")))
    (is (not (core.email/synthetic-message-id? nil)))
    (is (string? (core.email/synthetic-message-id nil nil nil)) "Even a message with nothing to hash gets an id")))

(deftest text-without-a-usable-charset-keeps-its-umlauts
  (let [utf8 (.getBytes "Rückgabe" "UTF-8")
        latin (.getBytes "Rückgabe" "windows-1252")]
    (is (= "Rückgabe" (core.email/decode-text utf8 nil)) "Valid UTF-8 without a declaration")
    (is (= "Rückgabe" (core.email/decode-text latin nil)) "Windows-1252 without a declaration")
    (is (= "Rückgabe" (core.email/decode-text latin "us-ascii")) "US-ASCII is what libraries assume when nothing is declared")
    (is (= "Rückgabe" (core.email/decode-text latin "ISO-8859-1")) "A declared charset is used")
    (is (= "Rückgabe" (core.email/decode-text utf8 "\\\"UTF-8\\\"")) "A sloppy UTF-8 declaration")
    (is (= "Rückgabe" (core.email/decode-text utf8 "x-unknown")) "An unknown charset is guessed")))
