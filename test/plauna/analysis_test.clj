(ns plauna.analysis-test
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [clojure.java.io :as io]
            [plauna.analysis :as analysis]))

;; Normalization tests

(deftest normalization-1
  (let [res (analysis/normalize (slurp (io/resource "test/normalization/original-text-1.txt")))]
    (is (= (s/trim (slurp (io/resource "test/normalization/normalized-text-1.txt"))) res))))

(deftest normalization-2
  (let [res (analysis/normalize (slurp (io/resource "test/normalization/greek-lorem-ipsum.txt")))]
    (is (= (s/trim (slurp (io/resource "test/normalization/normalized-greek-lorem-ipsum.txt"))) res))))

(deftest training-and-prediction-share-namespaced-features
  (let [email {:header {:subject "Invoice Ready"}
               :participants [{:type :sender :address "Billing@Example.COM"}
                              {:type :receiver :address "me@example.net"}]
               :body [{:mime-type "text/plain"
                       :content (s/join " " (repeat 510 "Payment"))}]
               :metadata {:category-id 7}}
        tokens (analysis/classification-tokens email)
        training-line (analysis/format-training-lines [[(get-in email [:metadata :category-id]) (analysis/training-tokens-text email)]])]
    (is (= ["sender-address:billing@example.com"
            "sender-domain:example.com"
            "subject:invoice"
            "subject:ready"]
           (subvec tokens 0 4)))
    (is (= analysis/max-body-features
           (count (filter #(s/starts-with? % "body:") tokens)))
        "Long bodies are bounded before either training or prediction")
    (is (= (str "7 " (s/join " " tokens) "\n") training-line)
        "The serialized training example contains exactly the prediction features")))

(deftest legacy-models-keep-their-original-body-only-features
  (let [email {:header {:subject "New Subject"}
               :participants [{:type :sender :address "sender@example.com"}]
               :body [{:mime-type "text/plain" :content "Legacy Body Tokens"}]}]
    (is (= ["Legacy" "Body" "Tokens"]
           (analysis/legacy-classification-tokens email)))
    (is (not-any? #(or (s/starts-with? % "sender-")
                       (s/starts-with? % "subject:")
                       (s/starts-with? % "body:"))
                  (analysis/legacy-classification-tokens email)))
    (is (= (analysis/legacy-classification-tokens email)
           (analysis/classification-tokens-for-model
            email "eng" (java.io.File. "train-eng.bin"))))
    (is (= (analysis/classification-tokens email)
           (analysis/classification-tokens-for-model
            email "eng" (java.io.File. "train-eng-maxent.bin"))))))

(deftest categorization-model-names-map-to-opennlp-trainers
  (is (= "NAIVEBAYES" (analysis/categorization-algorithm "naive-bayes")))
  (is (= "MAXENT" (analysis/categorization-algorithm "maxent"))))

(deftest naive-bayes-and-maxent-can-train-the-same-feature-data
  (let [training-file (java.io.File/createTempFile "plauna-training-" ".train")
        model-file (java.io.File/createTempFile "plauna-model-" ".bin")
        samples (str "1 sender-domain:shop.example subject:invoice body:payment\n"
                     "1 sender-domain:shop.example subject:receipt body:purchase\n"
                     "1 sender-domain:billing.example subject:invoice body:amount\n"
                     "2 sender-domain:friends.example subject:dinner body:tomorrow\n"
                     "2 sender-domain:friends.example subject:weekend body:meeting\n"
                     "2 sender-domain:club.example subject:invitation body:party\n")]
    (try
      (spit training-file samples)
      (doseq [model ["naive-bayes" "maxent"]]
        (let [trained (first (analysis/train-data [{:language "eng" :file training-file}] model))]
          (with-open [os (io/output-stream model-file)]
            (analysis/serialize-and-write-model! (:model trained) os))
          (is (= "1" (:name (analysis/categorize-tokens
                              ["sender-domain:shop.example" "subject:invoice" "body:payment"]
                              model-file)))
              (str model " produces a readable classifier"))))
      (finally
        (io/delete-file training-file true)
        (io/delete-file model-file true)))))

(deftest maxent-training-reports-iteration-progress
  (let [training-file (java.io.File/createTempFile "plauna-training-" ".train")
        events (atom [])]
    (try
      (spit training-file (str "1 sender-domain:shop.example subject:invoice body:payment\n"
                               "2 sender-domain:friends.example subject:dinner body:tomorrow\n"))
      (let [trained (analysis/train-data [{:language "eng" :file training-file}] "maxent" #(swap! events conj %))]
        (is (= 1 (count trained)))
        (is (some? (:model (first trained))))
        (is (= {:language "eng" :iteration 0 :iterations analysis/training-iterations}
               (first @events))
            "Training announces the language before the first iteration")
        (is (some #(and (pos? (:iteration %)) (= "eng" (:language %))) @events)
            "MaxEnt reports finished iterations")
        (is (:done? (last @events)) "The final event marks the end of training"))
      (finally
        (io/delete-file training-file true)))))

(deftest training-file-outcomes-counts-samples-and-labels
  (let [training-file (java.io.File/createTempFile "plauna-training-" ".train")]
    (try
      (spit training-file "1 subject:a body:b\n1 subject:c\n\n2 subject:d\n")
      (is (= {:samples 3 :labels #{"1" "2"}} (analysis/training-file-outcomes training-file)))
      (finally (io/delete-file training-file true)))))

(deftest a-failing-language-does-not-abort-the-other-models
  (let [good (java.io.File/createTempFile "plauna-training-good-" ".train")
        single (java.io.File/createTempFile "plauna-training-single-" ".train")]
    (try
      (spit good (str "1 sender-domain:shop.example subject:invoice body:payment\n"
                      "2 sender-domain:friends.example subject:dinner body:tomorrow\n"))
      (spit single "1 sender-domain:shop.example subject:invoice body:payment\n")
      (let [results (analysis/train-data [{:language "eng" :file good} {:language "deu" :file single}] "naive-bayes")]
        (is (= ["eng" "deu"] (mapv :language results)))
        (is (some? (:model (first results))) "The trainable language still yields a model")
        (is (nil? (:model (second results))))
        (is (instance? Exception (:error (second results))) "The single-outcome language reports its error instead of throwing"))
      (finally
        (io/delete-file good true)
        (io/delete-file single true)))))

(deftest lbfgs-maxent-trains-and-predicts
  (let [training-file (java.io.File/createTempFile "plauna-training-" ".train")
        model-file (java.io.File/createTempFile "plauna-model-" ".bin")
        events (atom [])]
    (try
      (spit training-file (str "1 sender-domain:shop.example subject:invoice body:payment\n"
                               "1 sender-domain:shop.example subject:receipt body:purchase\n"
                               "2 sender-domain:friends.example subject:dinner body:tomorrow\n"
                               "2 sender-domain:friends.example subject:weekend body:meeting\n"))
      (let [trained (first (analysis/train-data [{:language "eng" :file training-file}] "maxent-qn" #(swap! events conj %)))]
        (is (some? (:model trained)))
        (with-open [os (io/output-stream model-file)]
          (analysis/serialize-and-write-model! (:model trained) os))
        (is (= "1" (:name (analysis/categorize-tokens ["sender-domain:shop.example" "subject:invoice" "body:payment"] model-file))))
        (is (= "2" (:name (analysis/categorize-tokens ["sender-domain:friends.example" "subject:dinner"] model-file))))
        (is (:done? (last @events)) "A trainer without iteration callbacks still reports completion"))
      (finally
        (io/delete-file training-file true)
        (io/delete-file model-file true)))))

(deftest training-parameters-carry-algorithm-and-threads
  (let [params (analysis/training-parameters "maxent-qn" 4)]
    (is (= "MAXENT_QN" (.getStringParameter params "Algorithm" "")))
    (is (= 4 (.getIntParameter params "Threads" 1))))
  (is (= "MAXENT" (.getStringParameter (analysis/training-parameters "maxent") "Algorithm" "")))
  (is (= 1 (.getIntParameter (analysis/training-parameters "maxent") "Threads" 0)) "Single-threaded unless asked otherwise"))

(deftest format-training-lines-skips-unusable-pairs
  (is (= "1 subject:a body:b\n2 subject:c\n"
         (analysis/format-training-lines [[1 "subject:a body:b"] [nil "subject:x"] [2 "subject:c"] [3 ""] [4 nil]]))))

(deftest training-tokens-text-matches-the-classification-tokens
  (let [email {:header {:message-id "m" :subject "Invoice"}
               :participants [{:type :sender :address "shop@example.com"}]
               :body [{:mime-type "text/plain" :content "please pay"}]}]
    (is (= (clojure.string/join " " (analysis/classification-tokens email)) (analysis/training-tokens-text email)))
    (is (clojure.string/includes? (analysis/training-tokens-text email) "sender-domain:example.com"))))

(deftest categorization-models-are-loaded-once-and-reloaded-when-the-file-changes
  (let [training-file (java.io.File/createTempFile "plauna-training-" ".train")
        model-file (java.io.File/createTempFile "plauna-model-" ".bin")]
    (try
      (spit training-file (str "1 sender-domain:shop.example subject:invoice body:payment\n"
                               "2 sender-domain:friends.example subject:dinner body:tomorrow\n"))
      (let [trained (first (analysis/train-data [{:language "eng" :file training-file}] "naive-bayes"))]
        (with-open [os (io/output-stream model-file)]
          (analysis/serialize-and-write-model! (:model trained) os)))
      (analysis/forget-cached-models!)
      (let [first-load (analysis/categorizer-for model-file)]
        (is (identical? first-load (analysis/categorizer-for model-file))
            "The deserialized model is reused for the next e-mail instead of being read from disk again")
        (is (= "1" (:name (analysis/categorize-tokens ["sender-domain:shop.example" "subject:invoice" "body:payment"] model-file))))
        (.setLastModified model-file (+ (.lastModified model-file) 5000))
        (is (not (identical? first-load (analysis/categorizer-for model-file)))
            "A replaced model file (new stamp) is loaded afresh"))
      (finally
        (analysis/forget-cached-models!)
        (io/delete-file training-file true)
        (io/delete-file model-file true))))
  "Categorizing thousands of e-mails deserializes each model once, not once per e-mail")

(deftest language-detection-never-throws
  (with-redefs [cld.core/detect (fn [_] (throw (RuntimeException. "detector exploded")))]
    (is (= analysis/undetected-language (analysis/detect-language "some text that is long enough"))))
  (is (= analysis/undetected-language (analysis/detect-language "ab")) "Too short to detect")
  (is (nil? (analysis/detect-language nil)))
  "A detector failure marks the language as undetected instead of dropping the e-mail")

(deftest a-failing-categorization-still-yields-the-email
  (with-redefs [analysis/category-for-email (fn [& _] (throw (ex-info "model broken" {})))]
    (let [enriched (analysis/detect-language-and-categorize-email
                    {:header {:message-id "x" :subject "Hello"}
                     :body [{:mime-type "text/plain" :content "Hello world, this is a short english text about invoices."}]
                     :participants []})]
      (is (= "x" (-> enriched :header :message-id)))
      (is (nil? (-> enriched :metadata :category)))
      (is (some? (-> enriched :metadata :language)) "The language was still detected")))
  "An exception in the categorizer must not lose the e-mail in the enrichment pipeline")

(deftest category-scores-list-every-label-most-likely-first-regardless-of-the-threshold
  (let [training-file (java.io.File/createTempFile "plauna-training-" ".train")
        model-file (java.io.File/createTempFile "plauna-model-" ".bin")]
    (try
      (spit training-file (str "1 sender-domain:shop.example subject:invoice body:payment\n"
                               "2 sender-domain:friends.example subject:dinner body:tomorrow\n"
                               "3 sender-domain:club.example subject:training body:saturday\n"))
      (let [trained (first (analysis/train-data [{:language "eng" :file training-file}] "naive-bayes"))]
        (with-open [os (io/output-stream model-file)]
          (analysis/serialize-and-write-model! (:model trained) os)))
      (analysis/forget-cached-models!)
      (with-redefs [plauna.preferences/categorization-threshold (constantly 1.0)]
        (let [tokens ["sender-domain:shop.example" "subject:invoice" "body:payment"]
              scores (analysis/category-scores-for-tokens tokens model-file)]
          (is (nil? (:name (analysis/categorize-tokens tokens model-file))) "Below the threshold nothing is assigned")
          (is (= #{"1" "2" "3"} (set (map :name scores))) "Yet every label still has a score")
          (is (= "1" (:name (first scores))))
          (is (apply >= (map :confidence scores)) "Most likely first")
          (is (< 0.99 (reduce + (map :confidence scores)) 1.01))))
      (is (= [] (analysis/category-scores-for-tokens [] model-file)))
      (is (= [] (analysis/category-scores-for-tokens ["x"] (java.io.File. "/nonexistent/model.bin"))))
      (finally
        (analysis/forget-cached-models!)
        (io/delete-file training-file true)
        (io/delete-file model-file true)))))

(deftest an-mbox-email-that-is-already-stored-is-not-categorized-again
  (with-redefs [plauna.database/email-exists? (fn [message-id] (= "<stored@x>" message-id))
                analysis/detect-language-and-categorize-event (fn [event] (:payload event))]
    (is (nil? (analysis/handle-enrichment {:type :parsed-enrichable-email :payload {:header {:message-id "<stored@x>"}}}))
        "Re-importing must not overwrite the stored category with the model's guess")
    (is (= :enriched-email (:type (analysis/handle-enrichment {:type :parsed-enrichable-email :payload {:header {:message-id "<new@x>"}}}))))))
