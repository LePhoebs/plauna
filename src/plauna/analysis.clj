(ns plauna.analysis
  (:require [clojure.string :as st]
            [plauna.database :as db]
            [plauna.core.events :as events]
            [plauna.messaging :as messaging]
            [plauna.core.email :as core-email]
            [plauna.preferences :as p]
            [plauna.util.text-transform :as tt]
            [clojure.core.async :as async]
            [taoensso.telemere :as t]
            [cld.core :as lang]
            [plauna.files :as files]
            [plauna.interfaces :as int])
  (:import
   (opennlp.tools.util.normalizer AggregateCharSequenceNormalizer NumberCharSequenceNormalizer ShrinkCharSequenceNormalizer CharSequenceNormalizer)
   (opennlp.tools.util MarkableFileInputStreamFactory ObjectStream PlainTextByLineStream TrainingConfiguration TrainingParameters)
   (opennlp.tools.doccat DocumentSampleStream DocumentCategorizerME DocumentCategorizerEventStream DoccatFactory DoccatModel)
   (opennlp.tools.ml EventTrainer TrainerFactory)
   (opennlp.tools.ml.maxent GISTrainer)
   (opennlp.tools.ml.maxent.quasinewton QNTrainer)
   (opennlp.tools.ml.naivebayes NaiveBayesTrainer)
   (opennlp.tools.monitoring TrainingProgressMonitor)
   (java.util HashMap Locale)
   (java.util.regex Pattern)
   (java.io File OutputStream)))

(set! *warn-on-reflection* true)

(def BracketsNormalizer (reify CharSequenceNormalizer
                          (normalize [_ text] ((comp st/trim #(st/replace % #"\( \)" "")) text))))

(def MailtoNormalizer (reify CharSequenceNormalizer
                        (normalize [_ text] ((comp st/trim #(st/replace % #"(mailto:)?(?<![-+_.0-9A-Za-z])[-+_.0-9A-Za-z]+@[-0-9A-Za-z]+[-.0-9A-Za-z]+" "")) text))))

(def BetterURLNormalizer (reify CharSequenceNormalizer
                           (normalize [_ text] ((comp st/trim #(st/replace % #"https?://[-_.?&~%;+=/#0-9A-Za-z]+" "")) text))))

(def NonCharNormalizer (reify CharSequenceNormalizer
                         (normalize [_ text] (#(st/replace % (Pattern/compile "[^\\s\\w]" (bit-or Pattern/MULTILINE Pattern/UNICODE_CHARACTER_CLASS)) " ") text))))

(def ExtraWhiteSpaceNormalizer (reify CharSequenceNormalizer
                                 (normalize [_ text] (#(st/replace % (Pattern/compile "\\s{2,}" Pattern/MULTILINE) " ") text))))

(def NonPrintableCharNormalizer (reify CharSequenceNormalizer
                                  (normalize [_ text] (#(st/replace % (Pattern/compile "\\p{C}") " ") text))))

(def ^CharSequenceNormalizer normalizer (new AggregateCharSequenceNormalizer
                                             (into-array CharSequenceNormalizer
                                                         [BetterURLNormalizer
                                                          MailtoNormalizer
                                                          (NumberCharSequenceNormalizer/getInstance)
                                                          BracketsNormalizer
                                                          NonPrintableCharNormalizer
                                                          NonCharNormalizer
                                                          (ShrinkCharSequenceNormalizer/getInstance)
                                                          ExtraWhiteSpaceNormalizer])))

(defn normalize [^String text] (.normalize normalizer text))

(def supported-categorization-models
  [{:id "naive-bayes" :name "Naive Bayes"}
   {:id "maxent" :name "Maximum Entropy (MaxEnt, GIS)"}
   {:id "maxent-qn" :name "Maximum Entropy (MaxEnt, L-BFGS - faster training)"}])

(defn categorization-algorithm
  (^String [] (categorization-algorithm (p/categorization-model)))
  (^String [model]
   (case (p/canonical-categorization-model model)
     "maxent" GISTrainer/MAXENT_VALUE
     ;; The same model family, fitted with the quasi-Newton (L-BFGS) optimiser, which converges in far
     ;; fewer passes over the data than GIS. Prediction reads both with DocumentCategorizerME.
     "maxent-qn" QNTrainer/MAXENT_QN_VALUE
     NaiveBayesTrainer/NAIVE_BAYES_VALUE)))

(lang/default-init!)

(defn lang-code-set3 [language]
  ;; forLanguageTag handles hyphenated langdetect codes like "zh-cn"/"zh-tw" (new Locale would throw
  ;; MissingResourceException on .getISO3Language). Fall back to the raw code if no ISO3 form exists.
  (try
    (let [iso3 (.getISO3Language (Locale/forLanguageTag language))]
      (if (st/blank? iso3) language iso3))
    (catch Exception _ language)))

(def undetected-language {:code "n/a" :confidence 0.0})

(defn detect-language
  "The detected language of text as {:code :confidence}; undetected-language for text that is too short,
   below the confidence threshold, or that the detector cannot handle. Never throws: an e-mail whose
   language cannot be told must still be saved."
  [^String text]
  (when (some? text)
    (try
      (if (> (count text) 3)
        ;; lang/detect returns [best-language probabilities-map]. Use the explicitly detected best
        ;; language and look up its probability, rather than relying on the (unordered) map's first entry.
        (let [[best-lang probabilities] (lang/detect text)
              confidence (Double/parseDouble (or (get probabilities best-lang) "0.0"))
              lang-code (lang-code-set3 best-lang)]
          {:code (if (< confidence (p/language-detection-threshold)) "n/a" lang-code)
           :confidence confidence})
        undetected-language)
      (catch com.cybozu.labs.langdetect.NoFeatureInTextException _
        (t/log! :debug ["No language features in text:" text])
        undetected-language)
      (catch Exception e
        (t/log! {:level :warn :error e} "The language detector failed; treating the language as undetected.")
        (t/log! :debug ["The following text threw an exception:" text])
        undetected-language))))

(defn training-data-stream [file]
  (-> (MarkableFileInputStreamFactory. file)
      (PlainTextByLineStream. "UTF-8")
      (DocumentSampleStream.)))

(defn training-body-part [email] (core-email/body-part-for-mime-type "text/html" email))

(def max-body-features
  "Bound long messages so repeated boilerplate cannot overwhelm sender and subject evidence."
  500)

(defn- normalized-words [text]
  (if (st/blank? text)
    []
    ;; (?U): split on every Unicode space (no-break space, narrow no-break space ...) exactly like
    ;; OpenNLP's WhitespaceTokenizer splits the training file. Splitting only on ASCII whitespace gave
    ;; prediction the token "hallo\u00a0welt" where training saw "hallo" and "welt".
    (map st/lower-case
         (remove st/blank? (st/split (normalize text) #"(?U)\s+")))))

(defn- participant-addresses
  "The distinct, lower-cased addresses of the e-mail's participants of one type (types: the keyword and
   its stored string form)."
  [email types]
  (->> (:participants email)
       (filter #(contains? types (:type %)))
       (keep :address)
       (map st/lower-case)
       ;; Every kind of space removed, not only trimmed: a feature must stay ONE token, and OpenNLP
       ;; splits the training file at any Unicode space ("x y@a.de" or a trailing no-break space would
       ;; otherwise train different tokens than prediction sees).
       (map #(st/replace % #"(?U)\s+" ""))
       (remove st/blank?)
       distinct))

(defn- sender-domain [address]
  (let [separator (.lastIndexOf ^String address "@")]
    (when (< -1 separator (dec (count address)))
      (subs address (inc separator)))))

(defn body-text
  "The plain text of the e-mail's training body part (HTML tags and RTF markup removed), or nil without
   a text part. The cleaning is the expensive step of feature extraction; callers that also need the text
   for language detection pass it on instead of cleaning twice."
  [email]
  (when-let [body-part (training-body-part email)]
    (tt/clean-text-content (:content body-part) (core-email/text-content-type body-part))))

(defn classification-feature-groups
  "Build the exact same namespaced features for training and prediction. Keeping sender address,
   domain, subject and body in separate namespaces prevents an identical word from being treated as
   the same signal everywhere. cleaned-body, when given, is the result of body-text for this e-mail."
  ([email] (classification-feature-groups email (body-text email)))
  ([email cleaned-body]
   (let [address-features (fn [prefix addresses]
                            (concat (map #(str prefix "-address:" %) addresses)
                                    (keep #(when-let [domain (sender-domain %)] (str prefix "-domain:" domain)) addresses)))
         subject (get-in email [:header :subject])]
     ;; sender: the envelope sender - the Sender header, else From - as Plauna has always stored it.
     ;; from: the From address, stored since the sender was split in two. E-mails saved before that
     ;; simply lack the from features; what the model learned from the sender features still applies.
     {:sender (concat (address-features "sender" (participant-addresses email #{:sender "sender" ":sender"}))
                      (address-features "from" (participant-addresses email #{:from "from" ":from"})))
      :subject (map #(str "subject:" %) (normalized-words subject))
      :body (map #(str "body:" %) (take max-body-features (normalized-words cleaned-body)))})))

(defn classification-tokens
  ([email] (classification-tokens email (body-text email)))
  ([email cleaned-body]
   (let [{:keys [sender subject body]} (classification-feature-groups email cleaned-body)]
     (vec (concat sender subject body)))))

(defn legacy-classification-tokens
  "Reproduce the pre-migration prediction input for a legacy train-<lang>.bin model. Those models do
   not know the new feature namespaces and would otherwise see every token as unknown."
  [email]
  (let [body-part (training-body-part email)]
    (if body-part
      (let [content (tt/clean-text-content (:content body-part)
                                           (core-email/text-content-type body-part))]
        (if (st/blank? content)
          []
          (vec (remove st/blank? (st/split (normalize content) #" ")))))
      [])))

(defn classification-tokens-for-model
  ([email language-code ^File model-file]
   (classification-tokens-for-model email language-code model-file (body-text email)))
  ([email language-code ^File model-file cleaned-body]
   (if (= (.getName model-file) (str "train-" language-code ".bin"))
     (legacy-classification-tokens email)
     (classification-tokens email cleaned-body))))

(def training-tokens-version
  "Bump whenever classification-feature-groups or the normalizers change: cached training_tokens rows
   of an older version are ignored and recomputed on the next training run."
  2)

(defn training-tokens-text
  "The classification features of an e-mail as one space-separated line, the form cached in the
   training_tokens table. Blank when the e-mail yields no usable feature."
  [email]
  (st/join " " (classification-tokens email)))

(defn format-training-lines
  "Training file content from [category-id tokens-text] pairs; pairs without a category or without
   tokens are left out. The model label is the category ID, not its name: DocumentSampleStream treats
   the first whitespace-delimited token as the label, so a name like \"Work Projects\" would be
   trained as \"Work\" and never resolve back to a category. IDs are single tokens by construction."
  [pairs]
  ;; A StringBuilder instead of repeated str: concatenating growing strings is quadratic in the size of
  ;; the page (200 e-mails of a few KB each).
  (let [builder (StringBuilder.)]
    (doseq [[category tokens] pairs
            :when (and (some? category) (not (st/blank? tokens)))]
      (.append builder (str category))
      (.append builder " ")
      (.append builder ^String tokens)
      (.append builder "\n"))
    (.toString builder)))

(def training-iterations
  "Upper bound of optimisation iterations per model. MaxEnt may stop earlier once it converges;
   Naive Bayes needs a single pass and never reports iterations."
  1000)

(defn available-processors [] (.availableProcessors (Runtime/getRuntime)))

(defn training-parameters
  "threads (optional) lets the GIS and L-BFGS trainers compute each iteration on several cores."
  ([] (training-parameters (p/categorization-model)))
  ([model] (training-parameters model 1))
  ([model threads]
   (doto (new TrainingParameters)
     (.put TrainingParameters/ITERATIONS_PARAM (int training-iterations))
     (.put TrainingParameters/CUTOFF_PARAM 0)
     (.put TrainingParameters/THREADS_PARAM (int (max 1 threads)))
     (.put TrainingParameters/ALGORITHM_PARAM (categorization-algorithm model)))))

(comment NaiveBayesTrainer/NAIVE_BAYES_VALUE
         GISTrainer/MAXENT_VALUE
         "")

(defn serialize-and-write-model! [^DoccatModel model ^OutputStream os]
  (when (some? model) (.serialize model os)))

(defn- progress-monitor
  "Bridge OpenNLP's per-iteration callbacks to progress-fn, which receives
   {:iteration i :iterations n} after every iteration and additionally :done? true once training ends."
  ^TrainingProgressMonitor [progress-fn]
  (let [finished (atom false)]
    (reify TrainingProgressMonitor
      (finishedIteration [_ iteration _ _ _ _]
        (progress-fn {:iteration iteration :iterations training-iterations}))
      (finishedTraining [_ iteration _]
        (reset! finished true)
        (progress-fn {:iteration iteration :iterations training-iterations :done? true}))
      (isTrainingFinished [_] @finished)
      (display [_ _] nil))))

(declare label-format-property)

(defn train-model
  "The equivalent of DocumentCategorizerME/train that also reports iteration progress: OpenNLP only
   accepts a progress monitor through TrainingConfiguration, which the convenience method does not
   expose, so the trainer is assembled here from the same parts."
  (^DoccatModel [^String language ^File file model progress-fn]
   (train-model language file model progress-fn 1))
  (^DoccatModel [^String language ^File file model progress-fn threads]
  (let [parameters (training-parameters model threads)
        ;; Labels are category ids (format-training-lines): say so in the model, so they are never
        ;; mistaken for names (see label-format-property).
        manifest (doto (HashMap.) (.put label-format-property "ids"))
        factory (DoccatFactory.)
        ^EventTrainer trainer (TrainerFactory/getEventTrainer parameters manifest
                                                              (TrainingConfiguration. (progress-monitor progress-fn) nil))
        ;; Both streams are closed, also when training fails or is cancelled: each holds the training
        ;; file open, and the nightly runs for every language would otherwise pile up file handles.
        maxent-model (with-open [^ObjectStream samples (training-data-stream file)
                                 ^ObjectStream events (DocumentCategorizerEventStream. samples (.getFeatureGenerators factory))]
                       (.train trainer events))]
    (DoccatModel. language maxent-model manifest factory))))

(defn training-file-outcomes
  "Inspect a training file: how many samples it holds and which labels (category ids) occur. The label
   is the first whitespace-delimited token of each line, see format-training-lines."
  [^File file]
  (with-open [reader (clojure.java.io/reader file)]
    (reduce (fn [summary line]
              (if-let [label (first (st/split (st/trim line) #"\s+"))]
                (if (st/blank? label)
                  summary
                  (-> summary (update :samples inc) (update :labels conj label)))
                summary))
            {:samples 0 :labels #{}}
            (line-seq reader))))

(defn train-data
  "Train one model per training file, all languages in parallel, each trainer using its share of the
   available cores. progress-fn (optional) is called with {:language :iteration :iterations} as a
   language advances and once more with :done? true when it finishes. A language whose training fails
   does not abort the others: its entry carries :error instead of :model. The result preserves the
   order of training-files."
  ([training-files] (train-data training-files (p/categorization-model)))
  ([training-files model] (train-data training-files model (fn [_] nil)))
  ([training-files model progress-fn]
   (let [threads (max 1 (quot (available-processors) (max 1 (count training-files))))
         cancelled? (atom false)
         train-one (fn [tf]
                     (let [language (:language tf)
                           ;; A cancelled run's trainers must not report into the progress of the next run.
                           report #(when-not @cancelled? (progress-fn (assoc % :language language)))]
                       (report {:iteration 0 :iterations training-iterations})
                       (try
                         (let [result {:model (train-model language (:file tf) model report threads) :language language}]
                           ;; Trainers without iteration callbacks (Naive Bayes, L-BFGS) still end with a done marker.
                           (report {:iteration training-iterations :iterations training-iterations :done? true})
                           result)
                         (catch Exception e
                           (t/log! {:level :error :error e} ["Training the" language "model failed."])
                           (report {:done? true :failed? true})
                           {:language language :error e}))))]
     (let [futures (mapv #(future (train-one %)) training-files)]
       (try
         (mapv deref futures)
         (catch InterruptedException e
           ;; The run was cancelled (the scheduler restarted): stop the trainers as far as they listen
           ;; to interrupts and silence them, instead of leaving them to compete with the next run.
           (reset! cancelled? true)
           (run! future-cancel futures)
           (throw e)))))))

(defonce ^:private categorizer-cache
  ;; {absolute-path {:stamp [lastModified length] :categorizer DocumentCategorizerME}}. Deserializing a
  ;; model costs tens of milliseconds to seconds and used to happen for EVERY categorized e-mail. Models
  ;; are replaced atomically (files/write-model-file-atomically!), so a changed stamp means a new model.
  ;; DocumentCategorizerME.categorize is stateless and safe to share between threads.
  (atom {}))

(defn- model-stamp [^File model-file]
  [(.lastModified model-file) (.length model-file)])

(def label-format-property
  "Manifest property in which a model trained by Plauna records how its labels name categories. Its
   value \"ids\" means category ids (see format-training-lines). Models trained before it was written
   have none: their labels may be ids or - in older models - names, which digits alone cannot tell."
  "plauna-label-format")

(defn- named-with-algorithm?
  "True for a model file named train-<language>-<algorithm>.bin. Plauna writes models under such names
   only since 7 August 2026 (MaxEnt next to Naive Bayes), and its labels are category ids since
   10 July 2026: a model with such a name has id labels for certain. Only the older
   train-<language>.bin name can hold a model with category names as labels."
  [^File model-file]
  (let [file-name (.getName model-file)]
    (boolean (some #(st/ends-with? file-name (str "-" (:id %) ".bin")) supported-categorization-models))))

(defn- cached-model
  "{:categorizer :label-format} of a model file, cached and reloaded when the file changed."
  [^File model-file]
  (let [path (.getAbsolutePath model-file)
        stamp (model-stamp model-file)
        cached (get @categorizer-cache path)]
    (if (and cached (= stamp (:stamp cached)))
      cached
      (let [model (DoccatModel. model-file)
            entry {:stamp stamp
                   :categorizer (DocumentCategorizerME. model)
                   :label-format (when (or (= "ids" (.getManifestProperty model ^String label-format-property))
                                           (named-with-algorithm? model-file))
                                   :ids)}]
        (t/log! :info ["Loaded categorization model" path])
        (swap! categorizer-cache assoc path entry)
        entry))))

(defn categorizer-for
  "The (cached) categorizer for a model file, reloaded when the file changed."
  ^DocumentCategorizerME [^File model-file]
  (:categorizer (cached-model model-file)))

(defn forget-cached-models!
  "Drop every cached model (tests and diagnostics; production relies on the file stamp)."
  []
  (reset! categorizer-cache {}))

(defn categorize-tokens
  "The most likely label of the model for the tokens, when its probability is above the categorization
   threshold: {:name label :confidence p}, else {:name nil :confidence 0}. With usable-label?, only
   labels it accepts compete - a label of a deleted category must not win and leave the e-mail
   uncategorized while an existing category is above the threshold too."
  ([tokens model-file] (categorize-tokens tokens model-file (constantly true)))
  ([tokens ^File model-file usable-label?]
   (if (and (.exists model-file) (seq tokens))
     (let [doccat (categorizer-for model-file)
           cat-results (.categorize doccat (into-array String tokens))
           best (->> (range (.getNumberOfCategories doccat))
                     (map (fn [i] {:name (.getCategory doccat (int i)) :confidence (aget ^doubles cat-results (int i))}))
                     (filter #(usable-label? (:name %)))
                     ;; The first of equally likely labels wins, as with getBestCategory.
                     (reduce (fn [a b] (if (or (nil? a) (> (double (:confidence b)) (double (:confidence a)))) b a)) nil))]
       (if (and (some? best) (> (double (:confidence best)) (double (p/categorization-threshold))))
         best
         {:name nil :confidence 0}))
     {:name nil :confidence 0})))

(defn category-scores-for-tokens
  "Every label of the model with its probability for the tokens, most likely first, as
   [{:name label :confidence p}]. Unlike categorize-tokens this ignores the categorization threshold:
   it shows what the model thinks, not what Plauna would assign. Empty without a model or tokens."
  [tokens ^File model-file]
  (if (and (.exists model-file) (seq tokens))
    (let [doccat (categorizer-for model-file)
          cat-results (.categorize doccat (into-array String tokens))]
      (->> (range (.getNumberOfCategories doccat))
           (map (fn [i] {:name (.getCategory doccat (int i)) :confidence (aget cat-results (int i))}))
           (sort-by :confidence >)
           vec))
    []))

(defn categorize [text ^File model-file]
  (categorize-tokens (when-not (st/blank? text) (st/split text #"\s+")) model-file))

(defn normalize-body-part [body-part]
  (when (some? body-part)
    (normalize (tt/clean-text-content (:content body-part) (core-email/text-content-type body-part)))))

;; ── Reference data cache ─────────────────────────────────────────────────────────
;; The activated training languages and the category table change only through the administration
;; pages, yet categorization asked the database for them once per e-mail - thousands of tiny queries
;; during an import. They are cached briefly; the administration routes clear the cache on every change.

(def reference-cache-millis 5000)

(defonce ^:private reference-cache (atom {}))

(defn clear-reference-cache! [] (reset! reference-cache {}))

(defn- cached-reference [key load-fn]
  (let [now (System/currentTimeMillis)
        entry (get @reference-cache key)]
    (if (and entry (< (- now (long (:at entry))) reference-cache-millis))
      (:value entry)
      (let [value (load-fn)]
        (swap! reference-cache assoc key {:at now :value value})
        value))))

(defn- activated-languages []
  (cached-reference :activated-languages #(mapv :language (db/get-activated-language-preferences))))

(defn- categories-by-id []
  (cached-reference :categories-by-id #(into {} (map (juxt :id identity)) (db/get-categories))))

(defn- categories-by-name []
  (cached-reference :categories-by-name #(into {} (map (juxt :name identity)) (db/get-categories))))

(defn model-label-format
  "How the labels of a model name categories: :ids when the model says so in its manifest (every model
   Plauna trains now, see label-format-property) or its file name proves it (see
   named-with-algorithm?), nil when it is not known - an old train-<language>.bin, which the next
   training replaces. nil without a model."
  [^File model-file]
  (when (.exists model-file)
    (:label-format (cached-model model-file))))

(defn label->category
  "Resolve a model label to its category row, or nil when it names no existing category. With the
   model's label-format (see model-label-format) a label is ONLY an id or ONLY a name: guessing would
   resolve the legacy name \"7\" to the category with id 7, or a deleted category's id to a category
   that happens to be named like that number. Without a format (old callers) the id is tried first."
  ([label] (label->category label nil))
  ([label label-format]
   (when (some? label)
     (case label-format
       :ids (some->> (parse-long (str label)) (get (categories-by-id)))
       :names (get (categories-by-name) (str label))
       ;; Unknown format (an old train-<language>.bin): a label that is a number may be an id or a name -
       ;; and the category it meant may be deleted while the other reading still exists. Nothing tells
       ;; them apart, so it stays unresolved (the e-mail uncategorized) until the next training writes
       ;; a model whose format is known. Only a label that is no number is certainly a name.
       (when-not (parse-long (str label))
         (get (categories-by-name) (str label)))))))

(defn category-for-email
  "The model's category for the e-mail in the given language, or nil. cleaned-body (optional) is the
   result of body-text for this e-mail, so the caller's language detection and the feature extraction
   share one HTML cleaning pass."
  ([email language-code] (category-for-email email language-code (body-text email)))
  ([email language-code cleaned-body]
   (when (and (some? email) (some? language-code))
     (when (some #(= language-code %) (activated-languages))
       (let [model-file (files/model-file language-code (p/categorization-model))
             tokens (classification-tokens-for-model email language-code model-file cleaned-body)
             label-format (model-label-format model-file)]
         (assoc (categorize-tokens tokens model-file
                                   ;; A database hiccup must not make every label unusable: then all compete, as before.
                                   (fn [label] (try (some? (label->category label label-format)) (catch Exception _ true))))
                :label-format label-format))))))

(defn category-scores-for-email
  "The model's probability for each existing category of the e-mail in the given language, most
   likely first, as [{:id :name :color :confidence}] - regardless of the categorization threshold.
   nil when there is no prediction: no or a non-activated language, or no model yet. Labels of
   deleted categories are left out."
  ([email language-code] (category-scores-for-email email language-code (body-text email)))
  ([email language-code cleaned-body]
   (when (and (some? email) (some? language-code)
              (some #(= language-code %) (activated-languages)))
     (let [model-file (files/model-file language-code (p/categorization-model))]
       (when (.exists ^File model-file)
         (let [tokens (classification-tokens-for-model email language-code model-file cleaned-body)
               label-format (model-label-format model-file)]
           (into []
                 (keep (fn [{:keys [name confidence]}]
                         (when-let [category (label->category name label-format)]
                           (assoc (select-keys category [:id :name :color]) :confidence confidence))))
                 (category-scores-for-tokens tokens model-file))))))))

(defn category-scores-for-tokens-text
  "The model's probability for each existing category, as [{:id :confidence}], for an e-mail of the
   given language whose classification features are tokens-text (see training-tokens-text, the form the
   training_tokens table caches). nil when there is no prediction: no or a non-activated language, no
   model, or an old train-<language>.bin model, which expects other features than the cached ones."
  [tokens-text language-code]
  (when (and (some? language-code) (not (st/blank? tokens-text))
             (some #(= language-code %) (activated-languages)))
    (let [model-file ^File (files/model-file language-code (p/categorization-model))]
      (when (and (.exists model-file) (not= (.getName model-file) (str "train-" language-code ".bin")))
        (let [label-format (model-label-format model-file)]
          (into []
                (keep (fn [{:keys [name confidence]}]
                        (when-let [category (label->category name label-format)]
                          {:id (:id category) :confidence confidence})))
                (category-scores-for-tokens (st/split tokens-text #" ") model-file)))))))

(defn detect-language-and-categorize-email
  "Detect the language of an e-mail and, for an activated language with a model, its category. The
   HTML body is cleaned once for both. A failure in either step yields an e-mail without that piece
   of metadata instead of an exception: the e-mail itself must always reach the database."
  [email]
  (let [cleaned-body (try (body-text email)
                          (catch Exception e
                            (t/log! {:level :warn :error e} ["Could not extract the body text of" (-> email :header :message-id)])
                            nil))
        ;; No text at all leaves the language unknown (NULL), as before; text whose language cannot be
        ;; told is "n/a".
        language-result (if cleaned-body (detect-language (normalize cleaned-body)) {:code nil :confidence nil})
        category-result (try (category-for-email email (:code language-result) cleaned-body)
                             (catch Exception e
                               (t/log! {:level :warn :error e} ["Categorizing" (-> email :header :message-id) "failed; saving it without a category."])
                               nil))
        ;; Resolving the label reads the categories from the database: a failure there must not lose
        ;; the e-mail either.
        category (try (label->category (:name category-result) (:label-format category-result))
                      (catch Exception e
                        (t/log! {:level :warn :error e} ["Resolving the category of" (-> email :header :message-id) "failed; saving it without a category."])
                        nil))]
    (core-email/construct-enriched-email email
                                         {:language (:code language-result) :language-confidence (:confidence language-result)}
                                         {:category (:name category) :category-confidence (:confidence category-result) :category-id (:id category)})))

(defn detect-language-and-categorize-event [event]
  (detect-language-and-categorize-email (:payload event)))

(defn language-result
  "The detected language of an e-mail's training body part as {:code :confidence}: both nil without any
   text, \"n/a\" when the text's language cannot be told (also when the detector fails)."
  [email]
  (let [training-content (try (normalize-body-part (core-email/body-part-for-mime-type "text/html" email))
                              (catch Exception e
                                (t/log! {:level :warn :error e} ["Could not extract the body text of" (-> email :header :message-id)])
                                nil))]
    (if training-content
      (detect-language training-content)
      {:code nil :confidence nil})))

(defn detect-language-event [event]
  (let [email (:payload event)
        language-result (language-result email)]
    (core-email/construct-enriched-email email
                                         {:language (:code language-result) :language-confidence (:confidence language-result)}
                                         {:category (-> email :metadata :category) :category-confidence (-> email :metadata :category-confidence) :category-id (-> email :metadata :category-id)}
                                         (-> email :metadata :connection-id))))

(defmulti handle-enrichment :type)

(defn- already-stored?
  "True when the e-mail is in the database already. A failing lookup counts as not stored: saving is
   an upsert, so the worst case is the old behavior."
  [message-id]
  (try (boolean (and (some? message-id) (db/email-exists? message-id)))
       (catch Exception _ false)))

(defmethod handle-enrichment :parsed-enrichable-email [event]
  ;; An mbox can repeat e-mails Plauna already has (the same export twice, a monitored mailbox):
  ;; categorizing them again would overwrite their stored category - also one set by hand - with the
  ;; model's guess, and duplicate their attachment rows. The IMAP paths skip stored e-mails the same way.
  (if (already-stored? (-> event :payload :header :message-id))
    (do (t/log! :debug ["Skipping the already stored e-mail" (-> event :payload :header :message-id)])
        nil)
    (events/create-event :enriched-email (detect-language-and-categorize-event event) nil event)))

(defmethod handle-enrichment :language-detection-request [event]
  (events/create-event :enriched-email (detect-language-event event) nil event))

(defn enrichment-event-loop
  "Enriches the e-mails. Listens to two events:

  :parsed-enrichable-email    - Detects both the language and the category
  :language-detection-request - Only detects the language"
  [publisher events-channel]
  (let [parsed-enrichable-email-chan (async/chan)
        language-detection-request-chan (async/chan)
        local-chan (async/merge [parsed-enrichable-email-chan language-detection-request-chan])]
    (async/sub publisher :parsed-enrichable-email parsed-enrichable-email-chan)
    (async/sub publisher :language-detection-request language-detection-request-chan)
    (async/pipeline 4
                    events-channel
                    ;; keep: an already stored e-mail yields nil and is dropped (see handle-enrichment).
                    ;; A barrier is passed on in order towards the database loop (see messaging/barrier!).
                    (keep (fn [event] (if (messaging/barrier-event? event)
                                        (assoc event :type :enriched-email)
                                        (handle-enrichment event))))
                    local-chan
                    true
                    (fn [^Throwable th] (t/log! {:level :error :error th} (.getMessage th))))))

(defrecord BasicAnalyzer []
  int/Analyzer
  (enrich-email [_ email] (detect-language-and-categorize-email email))
  (detect-language [_ email] (language-result email)))
