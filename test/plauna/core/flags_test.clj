(ns plauna.core.flags-test
  (:require [clojure.test :refer :all]
            [plauna.core.flags :as flags]))

(deftest apple-mail-flag-colors-map-to-their-keyword-bits
  (is (= [#{} #{"$MailFlagBit0" "$MailFlagBit1" "$MailFlagBit2"}] (flags/keywords-for "red"))
      "Red is the plain flag and clears every color bit")
  (is (= [#{"$MailFlagBit0"} #{"$MailFlagBit1" "$MailFlagBit2"}] (flags/keywords-for "orange")))
  (is (= [#{"$MailFlagBit0" "$MailFlagBit1"} #{"$MailFlagBit2"}] (flags/keywords-for "green")))
  (is (= [#{"$MailFlagBit1" "$MailFlagBit2"} #{"$MailFlagBit0"}] (flags/keywords-for "gray")))
  (testing "every color survives the round trip through its keywords"
    (doseq [{:keys [id]} flags/colors]
      (is (= id (flags/color-of (first (flags/keywords-for id))))))))

(deftest only-known-flag-colors-are-accepted
  (is (= "blue" (flags/valid-color "blue")))
  (is (nil? (flags/valid-color "")))
  (is (nil? (flags/valid-color nil)))
  (is (nil? (flags/valid-color "javascript:alert(1)"))))

(deftest the-flag-follows-the-new-category
  (is (= "orange" (flags/flag-change nil "orange")))
  (is (= "green" (flags/flag-change "orange" "green")))
  (is (= :clear (flags/flag-change "orange" nil)) "Plauna's flag goes with the category it stood for")
  (is (nil? (flags/flag-change nil nil)) "A flag the user set on an e-mail of an unflagged category stays"))

(deftest an-unknown-color-is-the-plain-flag
  (is (= [#{} #{"$MailFlagBit0" "$MailFlagBit1" "$MailFlagBit2"}] (flags/keywords-for "pink"))))
