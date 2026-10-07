(ns plauna.application-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [honey.sql :as honey]
            [plauna.interfaces :as int]
            [taoensso.telemere :as t]
            [plauna.core.email :as core-email]
            [plauna.application :as app]))

(t/set-ns-filter! {:disallow "plauna.*"})

(deftest basic-auth
  (let [database (reify int/DB (fetch-connection [_ id] {:id id :auth-type "basic"}))
        client (reify int/EmailClient (start-monitor [_ config context] {:connected true}))
        context {:db database :client client}]
    (is (= {:result :ok} (app/connect-to-client context "abc"))  "Basic authentication calls email-client's login method and returns ok")))

(deftest basic-auth-2
  (let [database (reify int/DB (fetch-connection [_ id] {:id id}))
        client (reify int/EmailClient (start-monitor [_ config context] {:connected true}))
        context {:db database :client client}]
    (is (= {:result :ok} (app/connect-to-client context "abc"))  "If no auth-type is defined, fall back on basic auth and return ok")))

(deftest failed-login-returns-error
  (let [database (reify int/DB (fetch-connection [_ id] {:id id :auth-type "basic"}))
        ;; The client returns nil when authentication fails (it catches
        ;; AuthenticationFailedException internally); that must not be reported as :ok.
        client (reify int/EmailClient (start-monitor [_ config context] nil))
        context {:db database :client client}]
    (is (= :error (:result (app/connect-to-client context "abc")))
        "A nil start-monitor result (failed login) is reported as an error, not :ok")))

(deftest oauth2-auth
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] nil)
                   (fetch-auth-provider [_ id] {:id id}))
        client (reify int/EmailClient (start-monitor [_ config context] {:connected true}))
        context {:db database :client client}]
    (is (= {:result :redirect, :provider {:id 2}}
           (app/connect-to-client context "abc"))
        "auth-type 'oauth2' with auth provider but no token data returns a :redirect with the provider")))

(deftest oauth2-auth-2
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] {:access-token "not empty" :refresh-token "not empty"})
                   (fetch-auth-provider [_ id] {:id id}))
        client (reify int/EmailClient (start-monitor [_ config context] {:connected true}))
        context {:db database :client client}]
    (is (= {:result :ok}
           (app/connect-to-client context "abc"))
        "auth-type 'oauth2' with auth provider and token data calls client login and returns ok")))

(deftest oauth2-auth-3
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] nil)
                   (fetch-auth-provider [_ id] nil))
        client (reify int/EmailClient (start-monitor [_ config context] {:connected true}))
        context {:db database :client client}]
    (is (= :error (:result (app/connect-to-client context "abc"))))
    "auth-type 'oauth2' with no auth provider returns an errorq"))

(deftest oauth2-auth-4
  (let [database (reify int/DB
                   (fetch-connection [_ id] {:id id :auth-type "oauth2" :auth-provider 2})
                   (fetch-oauth-token-data [_ id] {:access-token "not empty"})
                   (fetch-auth-provider [_ id] {:id id}))
        client (reify int/EmailClient (start-monitor [_ config context] {:connected true}))
        context {:db database :client client}]
    (is (= {:result :redirect, :provider {:id 2}}
           (app/connect-to-client context "abc"))
        "auth-type 'oauth2' with auth provider and access token but no refresh token calls client login and returns ok")))

(defn- stub-emails-db
  "A reify int/DB with every method fetch-emails needs beyond fetch-emails itself: the three
   distinct-value lookups and fetch-header-categories (all empty by default, ignoring whatever
   other-filters-where they're called with) plus fetch-categories. capture-fn receives the
   customization clause passed to fetch-emails; result is what fetch-emails should return."
  [capture-fn result]
  (reify int/DB
    (fetch-categories [_] [])
    (fetch-distinct-subjects [_ _] [])
    (fetch-distinct-senders [_ _] [])
    (fetch-distinct-recipients [_ _] [])
    (fetch-header-categories [_ _] [])
    (fetch-emails [_ _ customization] (capture-fn customization) result)))

(deftest emails-query-filter-wo-search
  (let [query (atom "")
        database (stub-emails-db #(reset! query %) {:total 10 :size 1 :page 1})]
    (app/fetch-emails {:db database} {:filter "enriched-only" :size 1})
    (is (= @query {:where [:and [:<> :metadata.category nil] [:<> :metadata.language nil]], :order-by [[:date :desc]]}))
    (app/fetch-emails {:db database} {:filter "without-category" :size 1})
    (is (= @query {:where [:= :metadata.category nil] :order-by [[:date :desc]]}))
    (app/fetch-emails {:db database} {:size 1})
    (is (= {:order-by [[:date :desc]]} @query))))

(deftest emails-query-search-wo-filter
  (let [query (atom "")
        database (stub-emails-db #(reset! query %) {:total 10 :size 1 :page 1})]
    (app/fetch-emails {:db database} {:search-text "test text" :size 1})
    (is (= {:where [:in :headers.message-id
                    {:select [:bodies.message-id] :from [:bodies] :where [:like :bodies.content [:escape "%test text%" "\\"]]}]
            :order-by [[:date :desc]]}
           @query))))

(deftest emails-query-search-filter
  (let [query (atom "")
        database (stub-emails-db #(reset! query %) {:total 10 :size 1 :page 1})]
    (app/fetch-emails {:db database} {:filter "enriched-only" :search-text "test text" :size 1})
    (is (= {:where [:and [:and [:<> :metadata.category nil] [:<> :metadata.language nil]]
                    [:in :headers.message-id
                     {:select [:bodies.message-id] :from [:bodies] :where [:like :bodies.content [:escape "%test text%" "\\"]]}]]
            :order-by [[:date :desc]]}
           @query))))

(deftest emails-query-uses-mariadb-fulltext-with-short-term-fallback
  (let [query (atom nil)
        database (stub-emails-db #(reset! query %) {:total 0 :data []})]
    (app/fetch-emails {:db database :db-type :mariadb}
                      {:search-text "annual invo \"exact phrase\" AI" :size 20})
    (let [[sql & params] (honey/format (:where @query))]
      (is (re-find #"MATCH\(bodies\.content\) AGAINST \(\? IN BOOLEAN MODE\)" sql))
      (is (some #{"+annual* +invo* +\"exact phrase\""} params)
          "Words are required prefix matches and quoted text remains a phrase")
      (is (some #{"%AI%"} params) "Terms below InnoDB's default token length retain a LIKE fallback")))
  "MariaDB body searches use FULLTEXT without silently breaking short search terms")

(deftest emails-query-falls-back-for-stopwords-and-the-configured-token-length
  (let [query (atom nil)
        database (stub-emails-db #(reset! query %) {:total 0 :data []})]
    (app/fetch-emails {:db database :db-type :mariadb :fulltext-min-token-length 4}
                      {:search-text "annual the www abc" :size 20})
    (let [[sql & params] (honey/format (:where @query))]
      (is (re-find #"MATCH\(bodies\.content\)" sql))
      (is (some #{"+annual*"} params))
      (is (every? (set params) ["%the%" "%www%" "%abc%"])
          "Default stopwords and tokens below the server's configured minimum retain LIKE fallbacks")))
  "FULLTEXT search honours both InnoDB stopwords and innodb_ft_min_token_size")

(deftest emails-query-keeps-punctuation-only-search-literal-on-mariadb
  (let [query (atom nil)
        database (stub-emails-db #(reset! query %) {:total 0 :data []})]
    (app/fetch-emails {:db database :db-type :mariadb}
                      {:search-text "---" :size 20})
    (let [[sql & params] (honey/format (:where @query))]
      (is (not (re-find #"MATCH" sql)))
      (is (re-find #"bodies\.content LIKE" sql))
      (is (some #{"%---%"} params))))
  "A non-blank query that has no indexable token must not silently become an unfiltered search")

(deftest create-a-category
  (let [db-called (atom false)
        client-called (atom false)
        database (reify int/DB (save-category [_ _ _ _] (swap! db-called (fn [_] true))))
        client (reify int/EmailClient
                 (connections [_] {"does not matter" "some-data"})
                 (create-category-directories! [_ _ _] (swap! client-called (fn [_] true))))]
    (app/create-new-category! {:db database :client client} "test" nil nil)
    (is (= true @db-called))
    (is (= true @client-called)))
  "Creating a new category makes correct database and client calls")

(deftest create-category-passes-through-a-valid-color
  (let [captured-color (atom :not-called)
        database (reify int/DB (save-category [_ _ _ color] (reset! captured-color color)))
        client (reify int/EmailClient
                 (connections [_] {})
                 (create-category-directories! [_ _ _] nil))]
    (app/create-new-category! {:db database :client client} "test" nil "#3b82f6")
    (is (= "#3b82f6" @captured-color)))
  "A valid hex color is saved as-is")

(deftest create-category-drops-an-invalid-color
  (let [captured-color (atom :not-called)
        database (reify int/DB (save-category [_ _ _ color] (reset! captured-color color)))
        client (reify int/EmailClient
                 (connections [_] {})
                 (create-category-directories! [_ _ _] nil))]
    (app/create-new-category! {:db database :client client} "test" nil "javascript:alert(1)")
    (is (nil? @captured-color))
    (app/create-new-category! {:db database :client client} "test" nil "")
    (is (nil? @captured-color)))
  "An invalid or blank color is saved as nil rather than trusted verbatim")

(deftest update-category-passes-through-destination-and-color
  (let [captured (atom nil)
        database (reify int/DB (update-category [_ id destination-folder color] (reset! captured [id destination-folder color])))
        client (reify int/EmailClient
                 (connections [_] {})
                 (create-category-directories! [_ _ _] nil))]
    (app/update-category! {:db database :client client} "7" "test" "Archive/Test" "#ff0000")
    (is (= ["7" "Archive/Test" "#ff0000"] @captured)))
  "update-category! forwards the trimmed destination folder and the validated color")

(deftest move-email-without-connections
  (let [client (reify int/EmailClient
                 (connections [_] {})
                 (connection-id-for-email [_ _ _] nil))
        test-result (app/move-email-to-category {} "test" {:client client})]
    (is (= :error (:result test-result))))
  "If there are no connections, just moving email returns an error result.")

(deftest move-email-with-guessed-connection-id-success
  (let [client (reify int/EmailClient
                 (connections [_] {"test" {}})
                 (connection-id-for-email [_ _ _] "test")
                 (move-email-between-categories [_ _ _ _ _ _] true))
        test-result (app/move-email-to-category {} "test-cat" {:client client})]
    (is (= :ok (:result test-result))))
  "Successfully moving an email with a guessed connection id returns result :ok")

(deftest move-email-with-guessed-connection-id-error
  (let [client (reify int/EmailClient
                 (connections [_] {"test" {}})
                 (connection-id-for-email [_ _ _] "test")
                 (move-email-between-categories [_ _ _ _ _ _] false))
        test-result (app/move-email-to-category {} "test-cat" {:client client})]
    (is (= :error (:result test-result))))
  "Unsuccessfully moving an email with a guessed connection id returns result :error")

(deftest move-email-with-guessed-connection-id-not-found
  (let [client (reify int/EmailClient
                 (connections [_] {"test" {}})
                 (connection-id-for-email [_ _ _] "test")
                 (move-email-between-categories [_ _ _ _ _ _] :not-found))
        test-result (app/move-email-to-category {} "test-cat" {:client client})]
    (is (= :not-found (:result test-result))))
  "A successfully searched source folder without the message remains distinguishable from a general move error")

(deftest move-email-without-guessed-connection-id-success
  (let [client (reify int/EmailClient
                 (connections [_] {"test1" {:config {:id "test1"}} "test2" {:config {:id "test2"}}})
                 (connection-id-for-email [_ _ _] nil)
                 (move-email-between-categories [_ id _ _ _ _] (= id "test2")))
        test-result (app/move-email-to-category {} "test-cat" {:client client})]
    (is (= :ok (:result test-result))))
  "Successfully moving an email without guessed connection id returns result :ok even if the process failed in some other connection.")

(deftest move-email-without-guessed-connection-id-error
  (let [client (reify int/EmailClient
                 (connections [_] {"test1" {:config {:id "test1"}} "test2" {:config {:id "test2"}}})
                 (connection-id-for-email [_ _ _] nil)
                 (move-email-between-categories [_ id _ _ _ _] (= id "test3")))
        test-result (app/move-email-to-category {} "test-cat" {:client client})]
    (is (= :error (:result test-result))))
  "Unsuccessfully moving an email without guessed connection id returns result :error")

(deftest move-email-without-guessed-connection-id-not-found
  (let [client (reify int/EmailClient
                 (connections [_] {"test1" {:config {:id "test1"}} "test2" {:config {:id "test2"}}})
                 (connection-id-for-email [_ _ _] nil)
                 (move-email-between-categories [_ _ _ _ _ _] :not-found))
        test-result (app/move-email-to-category {} "test-cat" {:client client})]
    (is (= :not-found (:result test-result))))
  "When every connected account was searched without a match, the result is :not-found")

(deftest move-email-does-not-mask-a-general-error-as-not-found
  (let [client (reify int/EmailClient
                 (connections [_] {"test1" {:config {:id "test1"}} "test2" {:config {:id "test2"}}})
                 (connection-id-for-email [_ _ _] nil)
                 (move-email-between-categories [_ id _ _ _ _] (if (= id "test1") :not-found false)))
        test-result (app/move-email-to-category {} "test-cat" {:client client})]
    (is (= :error (:result test-result))))
  "A disconnected or otherwise failed account keeps the aggregate result generic even if another account reports :not-found")

(deftest handle-incoming-email-analyzer-exception
  (let [analyzer (reify int/Analyzer (enrich-email [_ _] (throw (ex-info "test exception" {}))))
        test-result (app/handle-incoming-imap-email {} {} {:analyzer analyzer})]
    (is (= :error (:result test-result))))
  "Return an error result if something goes wrong with the analyzer")

(deftest handle-incoming-email-db-exception
  (let [analyzer (reify int/Analyzer (enrich-email [_ _] "test"))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ _] (throw (ex-info "test exception" {}))))
        test-result (app/handle-incoming-imap-email {} {} {:analyzer analyzer :db db})]
    (is (= :error (:result test-result))))
  "Return an error result of something goes wrong with the database")

(deftest handle-incoming-email-client-exception
  ;; The e-mail is saved BEFORE the move. A move that throws (missing target folder, dropped
  ;; connection) must therefore not turn the stored e-mail into an error / a parse failure: it is
  ;; processed, and its current folder is recorded exactly like for a move that returned nil.
  (let [recorded (atom nil)
        analyzer (reify int/Analyzer (enrich-email [_ _] {:metadata {:category "test"}}))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ _] true)
             (update-email-folder [_ message-id folder] (reset! recorded [message-id folder])))
        client (reify int/EmailClient
                 (move-email-to-category [_ _ _ _ _] (throw (ex-info "test exception" {})))
                 (current-folder-name [_ folder] (str folder)))
        test-result (app/handle-incoming-imap-email {:header {:message-id "throw-1"}} {:move? true :origin-folder :inbox} {:analyzer analyzer :db db :client client})]
    (is (= :ok (:result test-result)))
    (is (= ["throw-1" ":inbox"] @recorded) "The e-mail stays findable in its source folder"))
  "A client exception during the move does not fail an e-mail that is already saved")

(deftest handle-incoming-email-client-exception-move-false
  (let [analyzer (reify int/Analyzer (enrich-email [_ _] {:metadata {:category "test"}}))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ _] true))
        client (reify int/EmailClient (move-email-to-category [_ _ _ _ _] (throw (ex-info "test exception" {}))))
        test-result (app/handle-incoming-imap-email {} {:move? false} {:analyzer analyzer :db db :client client})]
    (is (= :ok (:result test-result))))
  "Client is not called if move=false")

(deftest handle-incoming-email-client-exception-move-true
  (let [analyzer (reify int/Analyzer (enrich-email [_ _] {:metadata {:category nil}}))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ _] true))
        client (reify int/EmailClient (move-email-to-category [_ _ _ _ _] (throw (ex-info "test exception" {}))))
        test-result (app/handle-incoming-imap-email {} {:move? true} {:analyzer analyzer :db db :client client})]
    (is (= :ok (:result test-result))))
  "Client is not called if move=true but not category")

(deftest handle-incoming-email-happy-path
  (let [recorded (atom nil)
        analyzer (reify int/Analyzer (enrich-email [_ _] {:metadata {:category "test"}}))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ _] true)
             (update-email-folder [_ message-id folder] (reset! recorded [message-id folder])))
        client (reify int/EmailClient (move-email-to-category [_ _ _ _ _] "Categories/Test"))
        test-result (app/handle-incoming-imap-email {:header {:message-id "happy-1"}} {:move? true} {:analyzer analyzer :db db :client client})]
    (is (= :ok (:result test-result)))
    (is (= ["happy-1" "Categories/Test"] @recorded) "A successful move records the destination folder"))
  "Happy path. All underlying functions are called normally. Return an :ok result.")

(deftest handle-incoming-email-failed-move-records-source-folder
  (let [recorded (atom nil)
        analyzer (reify int/Analyzer (enrich-email [_ _] {:metadata {:category "test"}}))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ _] true)
             (update-email-folder [_ message-id folder] (reset! recorded [message-id folder])))
        client (reify int/EmailClient
                 (move-email-to-category [_ _ _ _ _] nil)         ; move did not complete (e.g. copy fallback failed)
                 (current-folder-name [_ folder] (str folder)))
        test-result (app/handle-incoming-imap-email {:header {:message-id "fail-1"}} {:move? true :origin-folder :inbox} {:analyzer analyzer :db db :client client})]
    (is (= :ok (:result test-result)))
    (is (= ["fail-1" ":inbox"] @recorded) "A failed move records the email's actual (source) folder, not the destination"))
  "When a move does not complete, the email stays in its source folder and that folder is recorded so later recategorization can still find it.")

(deftest handle-incoming-email-skips-existing-email
  (let [analyzer (reify int/Analyzer
                   (enrich-email [_ _] (throw (ex-info "existing email should not be enriched" {})))
                   (detect-language [_ _] (throw (ex-info "existing email should not be language-detected" {}))))
        db (reify int/DB
             (email-exists? [_ message-id] (= "known-1" message-id))
             (save-email [_ _] (throw (ex-info "existing email should not be saved" {})))
             (update-email-folder [_ _ _] (throw (ex-info "existing email folder should not be updated" {}))))
        client (reify int/EmailClient
                 (move-email-to-category [_ _ _ _ _] (throw (ex-info "existing email should not be moved" {}))))
        test-result (app/handle-incoming-imap-email {:header {:message-id "known-1"}}
                                                    {:move? true :origin-folder :spam :message :message}
                                                    {:analyzer analyzer :db db :client client})]
    (is (= :ok (:result test-result)))
    (is (nil? (:move test-result))))
  "Existing emails are not re-categorized, re-saved, or moved when a folder parse sees them again.")

(deftest read-emails-from-folder-continues-after-read-exception
  (let [read-attempts (atom [])
        saved-subjects (atom [])
        client (reify int/EmailClient
                 (open-folder-for-bulk-read [_ _ _]
                   {:message-count 3 :connection-id "test-connection" :folder :test-folder})
                 (close-folder-for-bulk-read [_ _] nil)
                 (current-folder-name [_ folder] (str folder))
                 (nth-message-id-from-folder [_ _ _] nil)
                 (nth-email-from-folder [_ n _]
                   (swap! read-attempts conj n)
                   (if (= n 2)
                     (throw (ex-info "failed to read message" {:n n}))
                     {:email {:header {:subject (str "email-" n)}} :message n})))
        analyzer (reify int/Analyzer
                   (enrich-email [_ email] (assoc email :metadata {:category nil})))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ email] (swap! saved-subjects conj (-> email :header :subject)))
             (update-email-folder [_ _ _] nil))]
    (is (= 3 (app/read-emails-from-folder {} "Newsletter" {:move? false} {:client client :analyzer analyzer :db db})))
    (let [deadline (+ (System/currentTimeMillis) 1000)]
      (loop []
        (when (and (< (count @saved-subjects) 2)
                   (< (System/currentTimeMillis) deadline))
          (Thread/sleep 10)
          (recur))))
    ;; Sequence numbers are processed high -> low so that moving/expunging a message never shifts the
    ;; numbers of messages not yet processed. Message 2 throws on read and is skipped; 3 and 1 succeed.
    (is (= [3 2 1] @read-attempts))
    (is (= ["email-3" "email-1"] @saved-subjects))))

(deftest read-emails-from-folder-limit-reads-only-most-recent
  (let [read-attempts (atom [])
        saved-subjects (atom [])
        client (reify int/EmailClient
                 (open-folder-for-bulk-read [_ _ _]
                   {:message-count 10 :connection-id "test-connection" :folder :test-folder})
                 (close-folder-for-bulk-read [_ _] nil)
                 (current-folder-name [_ folder] (str folder))
                 (nth-message-id-from-folder [_ _ _] nil)
                 (nth-email-from-folder [_ n _]
                   (swap! read-attempts conj n)
                   {:email {:header {:subject (str "email-" n)}} :message n}))
        analyzer (reify int/Analyzer
                   (enrich-email [_ email] (assoc email :metadata {:category nil})))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ email] (swap! saved-subjects conj (-> email :header :subject)))
             (update-email-folder [_ _ _] nil))]
    (is (= 10 (app/read-emails-from-folder {} "INBOX" {:move? false :limit 3} {:client client :analyzer analyzer :db db})))
    (let [deadline (+ (System/currentTimeMillis) 1000)]
      (loop []
        (when (and (< (count @saved-subjects) 3)
                   (< (System/currentTimeMillis) deadline))
          (Thread/sleep 10)
          (recur))))
    ;; Only the 3 most recent (highest) sequence numbers are read, still high -> low, and everything
    ;; below the window (7..1) is left untouched.
    (is (= [10 9 8] @read-attempts))
    (is (= ["email-10" "email-9" "email-8"] @saved-subjects)))
  "With :limit N, only the N most recent messages (highest sequence numbers) are processed — the reconnect back-fill's bounded catch-up.")

(defn- batch-test-client
  "A folder of message-count messages whose Message-IDs are id-<n>. Full reads are recorded in
   full-reads; Message-ID lookups in id-reads."
  [message-count full-reads id-reads]
  (reify int/EmailClient
    (open-folder-for-bulk-read [_ _ _]
      {:message-count message-count :connection-id "test-connection" :folder :test-folder})
    (close-folder-for-bulk-read [_ _] nil)
    (current-folder-name [_ folder] (str folder))
    (nth-message-id-from-folder [_ n _]
      (swap! id-reads conj n)
      (str "id-" n))
    (nth-email-from-folder [_ n _]
      (swap! full-reads conj n)
      {:email {:header {:message-id (str "id-" n) :subject (str "email-" n)}} :message n})))

(defn- await-summary [summary]
  (deref summary 2000 ::timeout))

(deftest read-emails-from-folder-batch-size-stops-after-n-new-emails
  (let [full-reads (atom [])
        id-reads (atom [])
        saved (atom [])
        recorded-folders (atom [])
        summary (promise)
        client (batch-test-client 10 full-reads id-reads)
        analyzer (reify int/Analyzer
                   (enrich-email [_ email] (assoc email :metadata {:category "test"})))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ email] (swap! saved conj (-> email :header :message-id)))
             (update-email-folder [_ message-id folder] (swap! recorded-folders conj [message-id folder])))]
    (is (= 10 (app/read-emails-from-folder {} "Old" {:move? false :batch-size 3 :on-complete #(deliver summary %)}
                                           {:client client :analyzer analyzer :db db})))
    (let [result (await-summary summary)]
      (is (= [10 9 8] @full-reads) "Only the first three (newest) unknown messages are read in full")
      (is (= ["id-10" "id-9" "id-8"] @saved))
      (is (= [["id-10" ":test-folder"] ["id-9" ":test-folder"] ["id-8" ":test-folder"]] @recorded-folders)
          "Without move, the e-mails stay in their folder and that folder is recorded")
      (is (= {:folder "Old" :message-count 10 :batch-size 3 :processed 3 :skipped 0 :skipped-elsewhere 0 :errors 0 :examined 3 :remaining 7}
             result))))
  "With :batch-size N the parse stops after N newly saved e-mails and reports how many were left unexamined.")

(deftest read-emails-from-folder-records-newly-saved-emails-under-the-batch-id
  (let [full-reads (atom [])
        id-reads (atom [])
        recorded (atom [])
        summary (promise)
        client (batch-test-client 5 full-reads id-reads)
        analyzer (reify int/Analyzer
                   (enrich-email [_ email] (assoc email :metadata {:category nil})))
        db (reify int/DB
             (email-exists? [_ message-id] (= "id-4" message-id))
             (save-email [_ _] nil)
             (record-parse-batch-email [_ batch-id message-id] (swap! recorded conj [batch-id message-id]))
             (update-email-folder [_ _ _] nil))]
    (app/read-emails-from-folder {} "Old" {:move? false :batch-size 2 :batch-id "run-1" :on-complete #(deliver summary %)}
                                 {:client client :analyzer analyzer :db db})
    (await-summary summary)
    (is (= [["run-1" "id-5"] ["run-1" "id-3"]] @recorded)
        "Only e-mails this run saved are recorded; the known one is skipped and the batch stops after two"))
  "The batch id collects exactly the newly saved e-mails so the run can be reviewed afterwards.")

(deftest fetch-emails-applies-batch-filter
  (let [captured (atom nil)
        db (reify int/DB
             (fetch-categories [_] [])
             (fetch-distinct-subjects [_ _] [])
             (fetch-distinct-senders [_ _] [])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ _] [])
             (fetch-parse-batch [_ id] (when (= "run-1" id) {:id id :folder "Old" :processed 100}))
             (fetch-emails [_ _ customization] (reset! captured customization) {:data [] :total 0}))
        result (app/fetch-emails {:db db} {:batch "run-1" :size 20})]
    (is (= [:in :headers.message-id {:select [:parse-batch-emails.message-id]
                                     :from [:parse-batch-emails]
                                     :where [:= :parse-batch-emails.batch-id "run-1"]}]
           (:where @captured)))
    (is (true? (get-in result [:parameters :batch-active?])))
    (is (true? (get-in result [:parameters :any-filter-active?])))
    (is (= "run-1" (get-in result [:parameters :batch])))
    (is (= "Batch: Old (100 new)" (get-in result [:parameters :batch-filter-label])))))

(deftest fetch-emails-ignores-blank-batch
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})
        result (app/fetch-emails {:db db} {:batch "" :size 20})]
    (is (nil? (:where @captured)))
    (is (false? (get-in result [:parameters :batch-active?])))
    (is (nil? (get-in result [:parameters :batch])))))

(deftest read-emails-from-folder-batch-size-skips-known-emails-without-counting-them
  (let [full-reads (atom [])
        id-reads (atom [])
        saved (atom [])
        summary (promise)
        client (batch-test-client 10 full-reads id-reads)
        analyzer (reify int/Analyzer
                   (enrich-email [_ email] (assoc email :metadata {:category nil})))
        ;; The previous batch already stored the four newest messages.
        known #{"id-10" "id-9" "id-8" "id-7"}
        db (reify int/DB
             (email-exists? [_ message-id] (contains? known message-id))
             (save-email [_ email] (swap! saved conj (-> email :header :message-id)))
             (update-email-folder [_ _ _] nil))]
    (app/read-emails-from-folder {} "Old" {:move? false :batch-size 3 :on-complete #(deliver summary %)}
                                 {:client client :analyzer analyzer :db db})
    (let [result (await-summary summary)]
      (is (= [10 9 8 7 6 5 4] @id-reads) "Every visited message is identified by its Message-ID first")
      (is (= [6 5 4] @full-reads) "Known messages are never downloaded in full")
      (is (= ["id-6" "id-5" "id-4"] @saved) "The batch continues where the previous one stopped")
      (is (= {:processed 3 :skipped 4 :errors 0 :examined 7 :remaining 3}
             (select-keys result [:processed :skipped :errors :examined :remaining])))))
  "Already stored e-mails are skipped cheaply and do not use up the batch, so repeated runs walk through the folder.")

(deftest read-emails-from-folder-without-batch-size-processes-everything
  (let [full-reads (atom [])
        id-reads (atom [])
        summary (promise)
        client (batch-test-client 4 full-reads id-reads)
        analyzer (reify int/Analyzer
                   (enrich-email [_ email] (assoc email :metadata {:category nil})))
        db (reify int/DB
             (email-exists? [_ message-id] (= "id-2" message-id))
             (save-email [_ _] nil)
             (update-email-folder [_ _ _] nil))]
    (app/read-emails-from-folder {} "Old" {:move? false :on-complete #(deliver summary %)}
                                 {:client client :analyzer analyzer :db db})
    (let [result (await-summary summary)]
      (is (= [4 3 1] @full-reads))
      (is (= {:processed 3 :skipped 1 :errors 0 :examined 4 :remaining 0 :batch-size nil}
             (select-keys result [:processed :skipped :errors :examined :remaining :batch-size])))))
  "Without a batch size the whole folder is examined, as before.")

(deftest read-emails-from-folder-empty-folder-reports-completion
  (let [summary (promise)
        client (reify int/EmailClient
                 (open-folder-for-bulk-read [_ _ _] {:message-count 0 :connection-id "c" :folder :f})
                 (close-folder-for-bulk-read [_ _] nil))]
    (is (= 0 (app/read-emails-from-folder {} "Empty" {:batch-size 100 :on-complete #(deliver summary %)} {:client client})))
    (is (= {:folder "Empty" :message-count 0 :batch-size 100 :processed 0 :skipped 0 :skipped-elsewhere 0 :errors 0 :examined 0 :remaining 0}
           (await-summary summary)))))

(defn- since-test-client
  "A folder of message-count messages whose Message-IDs are id-<n>; a date search answers with
   matching-numbers (ascending, like the real client) and remembers what it was asked in searched."
  [message-count matching-numbers searched full-reads prefetches]
  (reify int/EmailClient
    (open-folder-for-bulk-read [_ _ _]
      {:message-count message-count :connection-id "test-connection" :folder :test-folder})
    (close-folder-for-bulk-read [_ _] nil)
    (current-folder-name [_ folder] (str folder))
    (message-numbers-since [_ folder since]
      (is (= :test-folder folder) "The search runs on the folder object of the dedicated bulk-read handle")
      (swap! searched conj since)
      matching-numbers)
    (prefetch-message-identities [_ _ from to]
      (swap! prefetches conj [from to]))
    (nth-message-id-from-folder [_ n _] (str "id-" n))
    (nth-email-from-folder [_ n _]
      (swap! full-reads conj n)
      {:email {:header {:message-id (str "id-" n) :subject (str "email-" n)}} :message n})))

(deftest read-emails-from-folder-since-days-reads-only-the-messages-found-by-the-date-search
  (let [searched (atom [])
        full-reads (atom [])
        prefetches (atom [])
        saved (atom [])
        summary (promise)
        ;; 3 and 4 were moved into the folder later, so they sit between messages the search does not match.
        client (since-test-client 12 [3 4 10 11 12] searched full-reads prefetches)
        analyzer (reify int/Analyzer
                   (enrich-email [_ email] (assoc email :metadata {:category nil})))
        db (reify int/DB
             (email-exists? [_ message-id] (= "id-11" message-id))
             (save-email [_ email] (swap! saved conj (-> email :header :message-id)))
             (update-email-folder [_ _ _] nil))
        today (java.time.LocalDate/now (java.time.ZoneId/of "UTC"))]
    (is (= 12 (app/read-emails-from-folder {} "INBOX" {:move? false :since-days 7 :on-complete #(deliver summary %)}
                                           {:client client :analyzer analyzer :db db :time-zone "UTC"}))
        "The return value is still the number of messages in the folder")
    (let [result (await-summary summary)]
      (is (= [(.minusDays today 7)] @searched) "The search reaches back to the day 7 days ago (today included)")
      (is (= [12 10 4 3] @full-reads) "Only matching messages are read, newest first; the known one (11) is skipped")
      (is (= ["id-12" "id-10" "id-4" "id-3"] @saved))
      (is (= #{[3 4] [10 12]} (set @prefetches)) "Envelopes are prefetched per run of consecutive numbers, never across the gap 5..9")
      (is (= {:folder "INBOX" :message-count 12 :since-days 7 :processed 4 :skipped 1 :errors 0 :examined 5 :remaining 0}
             (select-keys result [:folder :message-count :since-days :processed :skipped :errors :examined :remaining])))))
  "A :since-days run examines exactly the messages the server reports as received in the period, wherever they sit in the folder.")

(deftest read-emails-from-folder-since-days-honours-the-batch-size
  (let [full-reads (atom [])
        summary (promise)
        client (since-test-client 20 [5 6 7 8 9] (atom []) full-reads (atom []))
        analyzer (reify int/Analyzer
                   (enrich-email [_ email] (assoc email :metadata {:category nil})))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ _] nil)
             (update-email-folder [_ _ _] nil))]
    (app/read-emails-from-folder {} "INBOX" {:move? false :since-days 3 :batch-size 2 :on-complete #(deliver summary %)}
                                 {:client client :analyzer analyzer :db db :time-zone "UTC"})
    (let [result (await-summary summary)]
      (is (= [9 8] @full-reads))
      (is (= {:processed 2 :remaining 3 :since-days 3}
             (select-keys result [:processed :remaining :since-days])))))
  "The batch stops after N new e-mails inside the period and reports the rest of the period as not examined.")

(deftest read-emails-from-folder-since-days-is-capped-by-limit
  (let [full-reads (atom [])
        summary (promise)
        client (since-test-client 20 [5 6 7 8 9] (atom []) full-reads (atom []))
        analyzer (reify int/Analyzer
                   (enrich-email [_ email] (assoc email :metadata {:category nil})))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ _] nil)
             (update-email-folder [_ _ _] nil))]
    (app/read-emails-from-folder {} "INBOX" {:move? false :since-days 3 :limit 2 :on-complete #(deliver summary %)}
                                 {:client client :analyzer analyzer :db db :time-zone "UTC"})
    (await-summary summary)
    (is (= [9 8] @full-reads))))

(deftest read-emails-from-folder-since-days-without-matches-completes-and-closes-the-connection
  (let [closed (atom 0)
        summary (promise)
        client (reify int/EmailClient
                 (open-folder-for-bulk-read [_ _ _] {:message-count 500 :connection-id "c" :folder :f})
                 (close-folder-for-bulk-read [_ _] (swap! closed inc))
                 (message-numbers-since [_ _ _] []))]
    (is (= 500 (app/read-emails-from-folder {} "INBOX" {:since-days 7 :on-complete #(deliver summary %)}
                                            {:client client :db (reify int/DB) :time-zone "UTC"})))
    (is (= {:folder "INBOX" :message-count 500 :batch-size nil :since-days 7 :processed 0 :skipped 0 :skipped-elsewhere 0 :errors 0 :examined 0 :remaining 0}
           (await-summary summary)) "A period without mail still reports completion, so the run never stays 'running'")
    (is (= 1 @closed) "The dedicated connection is released"))
  "Nothing received in the period: nothing to do, nothing left open.")

(deftest read-emails-from-folder-since-days-skips-the-search-in-an-empty-folder
  (let [summary (promise)
        client (reify int/EmailClient
                 (open-folder-for-bulk-read [_ _ _] {:message-count 0 :connection-id "c" :folder :f})
                 (close-folder-for-bulk-read [_ _] nil)
                 (message-numbers-since [_ _ _] (throw (ex-info "must not be called" {}))))]
    (app/read-emails-from-folder {} "INBOX" {:since-days 7 :on-complete #(deliver summary %)}
                                 {:client client :db (reify int/DB) :time-zone "UTC"})
    (is (= 0 (:message-count (await-summary summary))))))

(deftest read-emails-from-folder-since-days-releases-the-connection-when-the-search-fails
  (let [closed (atom 0)
        client (reify int/EmailClient
                 (open-folder-for-bulk-read [_ _ _] {:message-count 5 :connection-id "c" :folder :f})
                 (close-folder-for-bulk-read [_ _] (swap! closed inc))
                 (message-numbers-since [_ _ _] (throw (ex-info "SEARCH failed" {}))))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"SEARCH failed"
                          (app/read-emails-from-folder {} "INBOX" {:since-days 7}
                                                       {:client client :db (reify int/DB) :time-zone "UTC"})))
    (is (= 1 @closed) "The failed search must not leak the dedicated IMAP connection")))

(deftest contiguous-runs-split-numbers-at-every-gap
  (is (= [] (#'app/contiguous-runs [])))
  (is (= [[7 7]] (#'app/contiguous-runs [7])))
  (is (= [[3 5] [9 9] [12 13]] (#'app/contiguous-runs [13 3 4 5 9 12]))
      "Unsorted input is handled; runs are ascending")
  "Prefetching one FETCH per run keeps a date search with gaps from loading the envelopes in between.")

(deftest fetch-emails-applies-date-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :search-text nil
                                :page 1 :size 20 :date-from "2026-06-01" :date-to "2026-06-30"})
    (let [where-flat (flatten (:where @captured))]
      (is (some #{:>=} where-flat) "Lower date bound is present")
      (is (some #{:<} where-flat) "Upper date bound is present")
      (is (some #{:headers.date} where-flat) "Filters on the date column")))
  "Date-from/date-to are translated into a date-range filter on the email date")

(deftest fetch-emails-interprets-calendar-dates-in-the-configured-time-zone
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db :time-zone "Europe/Berlin"}
                      {:filter "all" :page 1 :size 20
                       :date-from "2026-06-01" :date-to "2026-06-01"})
    (let [[_ [_ _ lower] [_ _ upper]] (:where @captured)]
      (is (= 1780264800 lower) "Midnight is converted with the Berlin summer offset")
      (is (= 1780351200 upper) "The exclusive next-day boundary uses the same local calendar")))
  "Date filters and displayed local dates share the configured time zone")

(deftest fetch-emails-uses-date-first-related-filter-strategy
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :search-text "invoice"
                                :from-keys ["sender-key"] :to-keys ["recipient-key"]
                                :page 1 :size 20 :date-from "2026-06-01" :date-to "2026-06-30"})
    (let [where (:where @captured)
          nodes (tree-seq coll? seq where)
          exists-clauses (filter #(and (vector? %) (= :exists (first %))) nodes)]
      (is (= :and (first where)))
      (is (= :and (first (second where))) "The indexed date range is the first outer condition")
      (is (= 3 (count exists-clauses)) "Body, sender and recipient use correlated EXISTS")
      (is (some #{[:= :bodies.message-id :headers.message-id]} nodes))
      (is (some #{[:= :communications.message-id :headers.message-id]} nodes))))
  "An active date range drives related-table filters from the reduced header set")

(deftest fetch-emails-applies-content-search
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :search-text "someone@example.com" :page 1 :size 20})
    (is (= {:where [:in :headers.message-id
                    {:select [:bodies.message-id] :from [:bodies]
                     :where [:like :bodies.content [:escape "%someone@example.com%" "\\"]]}]
            :order-by [[:date :desc]]}
           @captured)))
  "The top Search Text field matches the e-mail body content, not the subject")

(deftest fetch-emails-escapes-like-wildcards
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :search-text "a_b%c\\d" :page 1 :size 20})
    (let [likes (filter #(and (vector? %) (= :like (first %))) (tree-seq coll? seq (:where @captured)))]
      (is (seq likes) "The content search produces a LIKE clause")
      (doseq [[_ _ [_ pattern escape-char]] likes]
        (is (= "%a\\_b\\%c\\\\d%" pattern) "LIKE wildcards and the escape char itself are escaped")
        (is (= "\\" escape-char) "An explicit ESCAPE character is set (SQLite has no default)"))))
  "User input containing % _ or \\ matches literally instead of acting as LIKE wildcards")

(deftest fetch-emails-no-content-filter-when-blank
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :search-text nil :page 1 :size 20})
    (is (not (contains? @captured :where))))
  "A blank Search Text adds no filter")

(deftest fetch-emails-applies-subject-values-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :subject-values ["Invoice" "Newsletter"] :page 1 :size 20})
    (is (= {:where [:in :headers.subject ["Invoice" "Newsletter"]] :order-by [[:date :desc]]} @captured)))
  "Checked subjects are translated into an IN filter on the subject column")

(deftest fetch-emails-no-subject-values-filter-when-empty
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :subject-values nil :page 1 :size 20})
    (is (not (contains? @captured :where))))
  "An absent subject selection (the default, where all boxes render checked) adds no filter")

(deftest fetch-emails-applies-subject-values-exclude-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :subject-values-exclude ["Spam"] :page 1 :size 20})
    (is (= {:where [:or [:not-in :headers.subject ["Spam"]] [:is :headers.subject nil]] :order-by [[:date :desc]]} @captured)))
  "Excluded subjects match every other subject, including emails without a subject (NOT IN is UNKNOWN for NULL)")

(deftest fetch-emails-distinguishes-no-subjects-from-an-unfiltered-subject-list
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})
        result (app/fetch-emails {:db db} {:filter "all" :subject-values-none "true" :page 1 :size 20})]
    (is (= {:where [:= 1 0] :order-by [[:date :desc]]} @captured)
        "An explicitly empty selection matches no rows instead of resetting the filter")
    (is (true? (get-in result [:parameters :subject-filter-active?])))
    (is (= "0" (get-in result [:parameters :subject-filter-badge])))
    (is (= "true" (get-in result [:parameters :subject-values-none]))))
  "The UI can represent the intuitive all-unchecked state independently from Reset")

(deftest fetch-emails-applies-from-keys-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :from-keys ["key-1" "key-2"] :page 1 :size 20})
    (is (= {:where [:in :headers.message-id
                    {:select [:communications.message-id] :from [:communications]
                     :where [:and [:in :communications.type ["sender" ":sender"]]
                                  [:in :communications.contact-key ["key-1" "key-2"]]]}]
            :order-by [[:date :desc]]}
           @captured)))
  "Checked senders are translated into a message-id semi-join by contact-key")

(deftest fetch-emails-applies-to-keys-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :to-keys ["key-3"] :page 1 :size 20})
    (is (= {:where [:in :headers.message-id
                    {:select [:communications.message-id] :from [:communications]
                     :where [:and [:in :communications.type ["receiver" ":receiver"]]
                                  [:in :communications.contact-key ["key-3"]]]}]
            :order-by [[:date :desc]]}
           @captured)))
  "Checked recipients are translated into a message-id semi-join by contact-key, using the receiver type")

(deftest fetch-emails-applies-from-keys-exclude-filter
  ;; Unchecking a couple of senders out of hundreds still leaves hundreds checked; the checklist UI
  ;; submits the (few) unchecked ones under from-keys-exclude instead, so the query stays short.
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :from-keys-exclude ["key-1"] :page 1 :size 20})
    (is (= {:where [:not [:in :headers.message-id
                          {:select [:communications.message-id] :from [:communications]
                           :where [:and [:in :communications.type ["sender" ":sender"]]
                                        [:in :communications.contact-key ["key-1"]]]}]]
            :order-by [[:date :desc]]}
           @captured)))
  "Excluded senders hide exactly the e-mails involving them; an e-mail without any sender row (Cc-only, unparseable From) is kept, like the subject filter keeps missing subjects")

(deftest fetch-emails-applies-to-keys-exclude-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :to-keys-exclude ["key-3"] :page 1 :size 20})
    (is (= {:where [:not [:in :headers.message-id
                          {:select [:communications.message-id] :from [:communications]
                           :where [:and [:in :communications.type ["receiver" ":receiver"]]
                                        [:in :communications.contact-key ["key-3"]]]}]]
            :order-by [[:date :desc]]}
           @captured)))
  "Excluded recipients hide exactly the e-mails addressed to them; an e-mail where the user was only in Cc is not hidden by unchecking somebody else")

(deftest fetch-emails-from-keys-include-wins-over-exclude
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :from-keys ["key-1"] :from-keys-exclude ["key-2"] :page 1 :size 20})
    (let [where (:where @captured)]
      (is (some #{[:in :communications.contact-key ["key-1"]]} (tree-seq coll? seq where)))))
  "If both are somehow present, include takes precedence (the UI only ever sends one)")

(deftest fetch-emails-clamps-page-size
  (let [captured (atom nil)
        db (reify int/DB
             (fetch-categories [_] [])
             (fetch-distinct-subjects [_ _] [])
             (fetch-distinct-senders [_ _] [])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ _] [])
             (fetch-emails [_ entity-clause _] (reset! captured entity-clause) {:data [] :total 0}))]
    (app/fetch-emails {:db db} {:filter "all" :page 1 :size 0})
    (is (= 1 (:size (:page @captured))) "A size of 0 is clamped up to 1, never an invalid LIMIT")
    (app/fetch-emails {:db db} {:filter "all" :page 1 :size 100000})
    (is (= 500 (:size (:page @captured))) "An unbounded size is capped at 500"))
  "fetch-emails clamps a free-form page size into a safe range")

(deftest fetch-emails-applies-category-ids-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids ["2" "3"] :page 1 :size 20})
    (is (= {:where [:in :metadata.category [2 3]] :order-by [[:date :desc]]} @captured)))
  "Selected numeric category ids are translated into an IN filter")

(deftest fetch-emails-applies-uncategorized-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids [app/uncategorized-token] :page 1 :size 20})
    (is (= {:where [:= :metadata.category nil] :order-by [[:date :desc]]} @captured)))
  "Selecting only the 'n/a' checkbox filters to e-mails with no category")

(deftest fetch-emails-applies-mixed-category-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids ["1" app/uncategorized-token] :page 1 :size 20})
    (is (= {:where [:or [:in :metadata.category [1]] [:= :metadata.category nil]] :order-by [[:date :desc]]} @captured)))
  "Selecting both real categories and 'n/a' ORs the two conditions together")

(deftest fetch-emails-no-category-filter-when-empty
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids nil :page 1 :size 20})
    (is (not (contains? @captured :where))))
  "No category-ids selected (the default, before any checkbox is unchecked) adds no filter")

(deftest fetch-emails-applies-category-ids-exclude-filter
  ;; The checklist UI submits an -exclude param instead of -ids once more than half the checkboxes
  ;; are checked, so it never has to send hundreds of ids just to exclude a couple.
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids-exclude ["2" "3"] :page 1 :size 20})
    (is (= {:where [:or [:not-in :metadata.category [2 3]] [:= :metadata.category nil]] :order-by [[:date :desc]]} @captured)))
  "Excluded category ids match everything else, including uncategorized e-mails")

(deftest fetch-emails-applies-category-ids-exclude-uncategorized-only
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids-exclude [app/uncategorized-token] :page 1 :size 20})
    (is (= {:where [:<> :metadata.category nil] :order-by [[:date :desc]]} @captured)))
  "Excluding only 'n/a' requires a real (non-null) category")

(deftest fetch-emails-applies-mixed-category-exclude-filter
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids-exclude ["1" app/uncategorized-token] :page 1 :size 20})
    (is (= {:where [:and [:<> :metadata.category nil] [:not-in :metadata.category [1]]] :order-by [[:date :desc]]} @captured)))
  "Excluding both a real category and 'n/a' requires a non-null, non-excluded category")

(deftest fetch-emails-category-include-wins-over-exclude
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids ["1"] :category-ids-exclude ["2"] :page 1 :size 20})
    (is (= {:where [:in :metadata.category [1]] :order-by [[:date :desc]]} @captured)))
  "If both are somehow present, include takes precedence (the UI only ever sends one)")

(deftest fetch-emails-marks-selected-categories-checked
  (let [db (reify int/DB
             (fetch-categories [_] [{:id 1 :name "Work"} {:id 2 :name "Personal"}])
             (fetch-distinct-subjects [_ _] [])
             (fetch-distinct-senders [_ _] [])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ _] [])
             (fetch-emails [_ _ _] {:data [] :total 0}))
        result (app/fetch-emails {:db db} {:filter "all" :category-ids ["1"] :page 1 :size 20})
        by-name (into {} (map (juxt :name :checked?)) (:categories (:optional result)))]
    (is (= {"Work" true "Personal" false "n/a" false} by-name)))
  "Only the selected category (and not the others, including n/a) is marked checked")

(deftest fetch-emails-marks-every-category-checked-when-unfiltered
  (let [db (reify int/DB
             (fetch-categories [_] [{:id 1 :name "Work"} {:id 2 :name "Personal"}])
             (fetch-distinct-subjects [_ _] [])
             (fetch-distinct-senders [_ _] [])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ _] [])
             (fetch-emails [_ _ _] {:data [] :total 0}))
        result (app/fetch-emails {:db db} {:filter "all" :page 1 :size 20})
        checked-flags (map :checked? (:categories (:optional result)))]
    (is (every? true? checked-flags)))
  "Before any checkbox is unchecked, every category (including n/a) shows as checked")

(deftest fetch-emails-marks-categories-checked-in-exclude-mode
  (let [db (reify int/DB
             (fetch-categories [_] [{:id 1 :name "Work"} {:id 2 :name "Personal"}])
             (fetch-distinct-subjects [_ _] [])
             (fetch-distinct-senders [_ _] [])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ _] [])
             (fetch-emails [_ _ _] {:data [] :total 0}))
        result (app/fetch-emails {:db db} {:filter "all" :category-ids-exclude ["1"] :page 1 :size 20})
        by-name (into {} (map (juxt :name :checked?)) (:categories (:optional result)))]
    (is (= {"Work" false "Personal" true "n/a" true} by-name)))
  "In exclude mode, every category is checked except the excluded one")

(deftest fetch-emails-marks-selected-subjects-senders-and-recipients-checked
  (let [db (reify int/DB
             (fetch-categories [_] [])
             (fetch-distinct-subjects [_ _] [{:subject "Invoice"} {:subject "Newsletter"}])
             (fetch-distinct-senders [_ _] [{:contact_key "s1" :name "Alice" :address "alice@example.com"}
                                          {:contact_key "s2" :name "Bob" :address "bob@example.com"}])
             (fetch-distinct-recipients [_ _] [{:contact_key "r1" :name "Me" :address "me@example.com"}])
             (fetch-header-categories [_ _] [])
             (fetch-emails [_ _ _] {:data [] :total 0}))
        result (app/fetch-emails {:db db} {:filter "all" :subject-values ["Invoice"] :from-keys ["s2"] :page 1 :size 20})
        optional (:optional result)]
    (is (= {"Invoice" true "Newsletter" false} (into {} (map (juxt :subject :checked?)) (:subjects optional))))
    (is (= {"s1" false "s2" true} (into {} (map (juxt :contact_key :checked?)) (:senders optional))))
    (is (every? true? (map :checked? (:recipients optional))) "Recipients are untouched, so all show as checked"))
  "Each column filter's checked state is independent of the others")

(deftest fetch-emails-marks-senders-checked-in-exclude-mode
  (let [db (reify int/DB
             (fetch-categories [_] [])
             (fetch-distinct-subjects [_ _] [])
             (fetch-distinct-senders [_ _] [{:contact_key "s1" :name "Alice" :address "alice@example.com"}
                                          {:contact_key "s2" :name "Bob" :address "bob@example.com"}])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ _] [])
             (fetch-emails [_ _ _] {:data [] :total 0}))
        result (app/fetch-emails {:db db} {:filter "all" :from-keys-exclude ["s1"] :page 1 :size 20})
        by-key (into {} (map (juxt :contact_key :checked?)) (:senders (:optional result)))]
    (is (= {"s1" false "s2" true} by-key)))
  "In exclude mode, every sender is checked except the excluded one")

(deftest fetch-emails-scopes-from-checklist-to-other-active-filters
  ;; The From checklist's possible values must reflect every OTHER active filter (here, category),
  ;; but never the From filter's own selection - otherwise picking a sender would immediately narrow
  ;; the From dropdown down to just that one sender.
  (let [captured-where (atom :not-called)
        db (reify int/DB
             (fetch-categories [_] [])
             (fetch-distinct-subjects [_ _] [])
             (fetch-distinct-senders [_ where] (reset! captured-where where) [])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ _] [])
             (fetch-emails [_ _ _] {:data [] :total 0}))]
    (app/fetch-emails {:db db} {:filter "all" :category-ids ["1"] :from-keys ["some-key"] :page 1 :size 20})
    (is (= [:in :metadata.category [1]] @captured-where)
        "Scoped to the category filter only; the From filter itself is not applied to its own checklist"))
  "Each checklist's possible-values query excludes its own filter but keeps every other active one")

(deftest fetch-emails-scopes-subject-checklist-to-other-active-filters
  (let [captured-where (atom :not-called)
        db (reify int/DB
             (fetch-categories [_] [])
             (fetch-distinct-subjects [_ where] (reset! captured-where where) [])
             (fetch-distinct-senders [_ _] [])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ _] [])
             (fetch-emails [_ _ _] {:data [] :total 0}))]
    (app/fetch-emails {:db db} {:filter "all" :from-keys ["some-key"] :subject-values ["Invoice"] :page 1 :size 20})
    (let [flat (tree-seq coll? seq @captured-where)]
      (is (some #{"some-key"} flat) "The From filter is applied")
      (is (not (some #{"Invoice"} flat)) "The Subject filter's own selection is not applied to its own checklist")))
  "The Subject checklist is scoped by From but not by its own selection")

(deftest fetch-emails-scopes-category-checklist-to-other-active-filters
  (let [captured-where (atom :not-called)
        db (reify int/DB
             (fetch-categories [_] [{:id 1 :name "Work"} {:id 2 :name "Personal"}])
             (fetch-distinct-subjects [_ _] [])
             (fetch-distinct-senders [_ _] [])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ where] (reset! captured-where where) [{:category 1}])
             (fetch-emails [_ _ _] {:data [] :total 0}))
        result (app/fetch-emails {:db db} {:filter "all" :from-keys ["alice-key"] :page 1 :size 20})]
    (is (= [:in :headers.message-id
            {:select [:communications.message-id] :from [:communications]
             :where [:and [:in :communications.type ["sender" ":sender"]] [:in :communications.contact-key ["alice-key"]]]}]
           @captured-where)
        "The category checklist's reachable set is scoped by the From filter")
    (is (= ["Work"] (mapv :name (:category-filter-options (:optional result))))
        "Only categories fetch-header-categories reports as reachable appear in the checklist")
    (is (= #{"Work" "Personal" "n/a"} (into #{} (map :name) (:categories (:optional result))))
        "The per-row reassignment dropdown (:categories) is never narrowed"))
  "The Category checklist narrows to reachable categories; the per-row assignment dropdown does not")

(deftest fetch-emails-no-date-filter-when-blank
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :search-text nil
                                :page 1 :size 20 :date-from "" :date-to ""})
    (is (not (some #{:>=} (flatten (:where @captured)))) "No date bound is added when both dates are blank"))
  "Blank date inputs add no date filter")

(deftest fetch-emails-combines-all-filters
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "enriched-only" :search-text "hello" :subject-values ["Invoice"]
                                :from-keys ["s1"] :to-keys ["r1"] :category-ids ["1"]
                                :date-from "2026-06-01" :date-to "2026-06-30" :page 1 :size 20})
    (let [where (:where @captured)]
      (is (= :and (first where)) "Every active filter is ANDed together")
      ;; combine-wheres builds one flat [:and ...] of all 7 active clauses; just check each
      ;; filter's signature is present somewhere in the tree rather than pinning the exact shape.
      (let [flat (tree-seq coll? seq where)]
        (is (some #{:metadata.category} flat) "Metadata filter present")
        (is (some #{:bodies.content} flat) "Content search present")
        (is (some #{:headers.subject} flat) "Subject filter present")
        (is (some #{"sender" ":sender"} flat) "From filter present")
        (is (some #{"receiver" ":receiver"} flat) "To filter present")
        (is (some #{:headers.date} flat) "Date filter present"))))
  "All filters (metadata, content search, subject, from, to, category, date) combine with AND")

(deftest move-parse-batch-emails-moves-only-categorized-emails-in-chunks
  (let [moves (atom [])
        progress (atom [])
        fetched (atom [])
        by-id {"m-1" {:header {:message-id "m-1"} :metadata {:category "invoices"}}
               "m-2" {:header {:message-id "m-2"} :metadata {:category nil}}
               "m-3" {:header {:message-id "m-3"} :metadata {:category "friends"}}
               "m-4" {:header {:message-id "m-4"} :metadata {:category "invoices"}}
               "m-5" {:header {:message-id "m-5"} :metadata {:category "friends"}}}
        db (reify int/DB
             (fetch-parse-batch-message-ids [_ batch-id] (is (= "run-1" batch-id)) ["m-1" "m-2" "m-3" "m-4" "m-5"])
             (fetch-emails [_ entity customization]
               (is (= {:entity :enriched-email :strict true :with-bodies false} entity))
               (let [[op column ids] (:where customization)]
                 (is (= [:in :headers.message-id] [op column]))
                 (swap! fetched conj ids)
                 (mapv by-id ids))))
        client (reify int/EmailClient
                 (move-emails-by-id [_ connection-id requested]
                   (swap! moves conj [connection-id requested])
                   (mapv (fn [{:keys [message-id]}] (case message-id "m-3" :not-found "m-4" false "m-5" false true)) requested)))
        summary (with-redefs [app/batch-move-chunk-size 2]
                  (app/move-parse-batch-emails! {:db db :client client} "run-1" "conn-1" #(swap! progress conj %)))]
    (is (= [["m-1" "m-2"] ["m-3" "m-4"] ["m-5"]] @fetched) "The batch is fetched in chunks, never with one IN clause over everything")
    (is (= [["conn-1" [{:message-id "m-1" :category "invoices"}]]
            ["conn-1" [{:message-id "m-3" :category "friends"} {:message-id "m-4" :category "invoices"}]]
            ["conn-1" [{:message-id "m-5" :category "friends"}]]]
           @moves)
        "Only categorized e-mails are moved, one call per chunk over the batch's connection")
    (is (= {:batch-id "run-1" :total 4 :uncategorized 1 :moved 1 :not-found 1 :failed 2} summary))
    (is (= 3 (count @progress)) "Progress is reported after every chunk")
    (is (= summary (last @progress)))))

(deftest move-email-prefers-the-recorded-connection
  (let [used (atom nil)
        client (reify int/EmailClient
                 (connections [_] {"masked" {:config {:id "masked"}} "real" {:config {:id "real"}}})
                 (connection-id-for-email [_ _ _] "masked")
                 (move-email-between-categories [_ connection-id _ _ _ _] (reset! used connection-id) true))
        email {:header {:message-id "m-1"} :metadata {:category "old" :connection-id "real"}}
        test-result (app/move-email-to-category email "new" {:client client})]
    (is (= :ok (:result test-result)))
    (is (= "real" @used) "The account recorded with the e-mail wins over the recipient-address guess")))

(deftest move-email-falls-back-to-guessing-when-the-recorded-connection-is-gone
  (let [used (atom nil)
        client (reify int/EmailClient
                 (connections [_] {"masked" {:config {:id "masked"}}})
                 (connection-id-for-email [_ _ _] "masked")
                 (move-email-between-categories [_ connection-id _ _ _ _] (reset! used connection-id) true))
        email {:header {:message-id "m-1"} :metadata {:category "old" :connection-id "deleted-account"}}
        test-result (app/move-email-to-category email "new" {:client client})]
    (is (= :ok (:result test-result)))
    (is (= "masked" @used) "A recorded account that is no longer active is ignored")))

(deftest incoming-email-records-the-connection-it-arrived-on
  (let [saved (atom [])
        analyzer (reify int/Analyzer
                   (enrich-email [_ email] (core-email/construct-enriched-email email {:language "eng" :language-confidence 0.9} {:category "test" :category-id 1 :category-confidence 0.8}))
                   (detect-language [_ _] {:code "eng" :confidence 0.9}))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ email] (swap! saved conj email))
             (update-email-folder [_ _ _] nil))
        client (reify int/EmailClient (current-folder-name [_ folder] (str folder)))
        context {:analyzer analyzer :db db :client client}]
    (app/handle-incoming-imap-email {:header {:message-id "auto-1"}} {:connection-id "conn-1" :origin-folder :inbox :move? false} context)
    (app/handle-incoming-imap-email {:header {:message-id "assigned-1"}} {:connection-id "conn-1" :origin-folder :inbox :move? false :assigned-category "test" :assigned-category-id 1} context)
    (is (= ["conn-1" "conn-1"] (mapv #(-> % :metadata :connection-id) @saved))
        "Both the automatic and the assigned-category path store the account id")
    (is (= "test" (-> @saved first :metadata :category)) "The rest of the metadata is untouched")))

(deftest read-emails-from-folder-records-failures-with-their-identity
  (let [recorded (atom [])
        summary (promise)
        client (reify int/EmailClient
                 (open-folder-for-bulk-read [_ _ _] {:message-count 3 :connection-id "conn-1" :folder :f})
                 (close-folder-for-bulk-read [_ _] nil)
                 (current-folder-name [_ folder] (str folder))
                 (nth-message-id-from-folder [_ n _] (if (= n 2) (throw (ex-info "Failed to load IMAP envelope" {})) (str "id-" n)))
                 (nth-message-identity-from-folder [_ n _] {:uid (* 100 n) :message-id nil :subject (str "subject-" n)})
                 (nth-email-from-folder [_ n _] {:email {:header {:message-id (str "id-" n)}} :message n}))
        analyzer (reify int/Analyzer (enrich-email [_ email] (assoc email :metadata {:category nil})))
        db (reify int/DB
             (email-exists? [_ _] false)
             (save-email [_ _] nil)
             (update-email-folder [_ _ _] nil)
             (resolve-parse-failures [_ _ _ _ _] nil)
             (record-parse-failure [_ failure] (swap! recorded conj failure)))]
    (app/read-emails-from-folder {} "INBOX" {:move? false :on-complete #(deliver summary %)} {:client client :analyzer analyzer :db db})
    (let [result (await-summary summary)]
      (is (= 1 (:errors result)))
      (is (= [{:connection-id "conn-1" :folder "INBOX" :uid 200 :message-number 2 :message-id nil :subject "subject-2" :error "Failed to load IMAP envelope"}]
             @recorded)
          "The failed message is recorded with UID, subject and the error text"))))

(deftest retry-failed-messages-handles-recovered-gone-and-still-failing-messages
  (let [saved (atom [])
        batch (atom [])
        resolved (atom [])
        recorded (atom [])
        summary (promise)
        client (reify int/EmailClient
                 (open-folder-for-bulk-read [_ _ _] {:message-count 50 :connection-id "conn-1" :folder :f})
                 (close-folder-for-bulk-read [_ _] nil)
                 (current-folder-name [_ folder] (str folder))
                 (email-by-uid-from-folder [_ uid _]
                   (case (long uid)
                     11 {:email {:header {:message-id "recovered-11"}} :message 11}
                     12 nil
                     13 (throw (ex-info "still broken" {}))
                     14 {:email {:header {:message-id "known-14"}} :message 14})))
        analyzer (reify int/Analyzer (enrich-email [_ email] (assoc email :metadata {:category "test"})))
        db (reify int/DB
             (email-exists? [_ message-id] (= "known-14" message-id))
             (save-email [_ email] (swap! saved conj (-> email :header :message-id)))
             (update-email-folder [_ _ _] nil)
             (parse-failure-keys [_ _ _] {:uids #{11 12 13 14} :message-ids #{}})
             (record-parse-batch-email [_ batch-id message-id] (swap! batch conj [batch-id message-id]))
             (resolve-parse-failures [_ _ folder uid message-id] (swap! resolved conj [folder uid message-id]))
             (record-parse-failure [_ failure] (swap! recorded conj (select-keys failure [:uid :error]))))
        failures [{:id 1 :uid 11} {:id 2 :uid 12} {:id 3 :uid 13} {:id 4 :uid 14} {:id 5 :uid nil}]]
    (is (= 5 (app/retry-failed-messages! {} "INBOX" failures {:move? false :batch-id "retry-1" :on-complete #(deliver summary %)}
                                         {:client client :analyzer analyzer :db db})))
    (let [result (await-summary summary)]
      (is (= {:processed 1 :skipped 1 :gone 1 :errors 2 :examined 5} (select-keys result [:processed :skipped :gone :errors :examined])))
      (is (= ["recovered-11"] @saved))
      (is (= [["retry-1" "recovered-11"]] @batch) "Only the recovered e-mail joins the review batch")
      (is (= #{["INBOX" 11 "recovered-11"] ["INBOX" 12 nil] ["INBOX" 14 "known-14"]} (set @resolved))
          "Recovered, gone and already-stored messages all clear their failure entry")
      (is (= [{:uid 13 :error "still broken"}] @recorded) "A message that fails again updates its entry"))))

(deftest read-emails-from-folder-gives-up-after-consecutive-failures
  (let [recorded (atom [])
        summary (promise)
        client (reify int/EmailClient
                 (open-folder-for-bulk-read [_ _ _] {:message-count 200 :connection-id "conn-1" :folder :f})
                 (close-folder-for-bulk-read [_ _] nil)
                 (nth-message-id-from-folder [_ _ _] (throw (ex-info "folder closed" {})))
                 (nth-message-identity-from-folder [_ _ _] (throw (ex-info "folder closed" {}))))
        db (reify int/DB
             (record-parse-failure [_ failure] (swap! recorded conj failure)))]
    (app/read-emails-from-folder {} "INBOX" {:on-complete #(deliver summary %)} {:client client :db db})
    (let [result (await-summary summary)]
      (is (= 50 (:errors result)) "The run stops after the consecutive-error limit instead of touching every message")
      (is (= 150 (:remaining result)))
      (is (string? (:aborted result)))
      (is (= 50 (count @recorded)))
      (is (every? #(and (nil? (:uid %)) (some? (:message-number %))) @recorded)
          "Failures without UID still carry the sequence number the database deduplicates on"))))

(deftest read-emails-from-folder-reports-completion-even-when-the-loop-throws
  (let [summary (promise)
        client (reify int/EmailClient
                 (open-folder-for-bulk-read [_ _ _] {:message-count 3 :connection-id "conn-1" :folder :f})
                 (close-folder-for-bulk-read [_ _] (throw (Error. "close blew up")))
                 (nth-message-id-from-folder [_ _ _] "known")
                 (nth-message-identity-from-folder [_ _ _] {}))
        db (reify int/DB
             (email-exists? [_ _] true))]
    (app/read-emails-from-folder {} "INBOX" {:on-complete #(deliver summary %)} {:client client :db db})
    (let [result (await-summary summary)]
      (is (not= ::timeout result) "on-complete runs although closing the folder threw an Error")
      (is (str/starts-with? (:aborted result) "aborted:")))))

(deftest resolving-failures-only-touches-the-database-when-an-entry-exists
  (let [deletes (atom [])
        summary (promise)
        client (reify int/EmailClient
                 (open-folder-for-bulk-read [_ _ _] {:message-count 3 :connection-id "conn-1" :folder :f})
                 (close-folder-for-bulk-read [_ _] nil)
                 (nth-message-id-from-folder [_ n _] (str "id-" n)))
        db (reify int/DB
             (parse-failure-keys [_ _ _] {:uids #{} :message-ids #{"id-2"}})
             (email-exists? [_ _] true)
             (resolve-parse-failures [_ _ folder uid message-id] (swap! deletes conj [folder uid message-id])))]
    (app/read-emails-from-folder {} "INBOX" {:on-complete #(deliver summary %)} {:client client :db db})
    (await-summary summary)
    (is (= [["INBOX" nil "id-2"]] @deletes) "Only the message with a recorded failure triggers a DELETE")))

(deftest move-email-falls-back-to-other-connections-when-the-recorded-one-lacks-the-message
  (let [tried (atom [])
        client (reify int/EmailClient
                 (connections [_] {"a" {:config {:id "a"}} "b" {:config {:id "b"}}})
                 (connection-id-for-email [_ _ _] nil)
                 (move-email-between-categories [_ connection-id _ _ _ _]
                   (swap! tried conj connection-id)
                   (if (= "b" connection-id) true :not-found)))
        email {:header {:message-id "m-1"} :metadata {:category "old" :connection-id "a"}}
        result (app/move-email-to-category email "new" {:client client})]
    (is (= :ok (:result result)))
    (is (= ["a" "b"] @tried) "The recorded account is tried first, the other one afterwards")))

(deftest skipped-emails-are-classified-by-where-they-were-stored-from
  (let [summary (promise)
        client (reify int/EmailClient
                 (open-folder-for-bulk-read [_ _ _] {:message-count 3 :connection-id "conn-1" :folder :f})
                 (close-folder-for-bulk-read [_ _] nil)
                 (nth-message-id-from-folder [_ n _] (str "id-" n)))
        db (reify int/DB
             (email-exists? [_ _] true)
             (fetch-email-location [_ message-id] (case message-id
                                                    "id-3" {:folder "Old" :connection-id "conn-1"}
                                                    "id-2" {:folder "INBOX" :connection-id "conn-1"}
                                                    nil)))]
    (app/read-emails-from-folder {} "Old" {:on-complete #(deliver summary %)} {:client client :db db})
    (let [result (await-summary summary)]
      (is (= 3 (:skipped result)))
      (is (= 2 (:skipped-elsewhere result)) "Stored from another folder, or from an unknown place, counts as elsewhere")
      (is (= 0 (:processed result))))))

(deftest move-folder-emails-uses-the-recorded-folder-membership
  (let [asked (atom nil)
        db (reify int/DB
             (fetch-categorized-message-ids-in-folder [_ connection-id folder] (reset! asked [connection-id folder]) ["m-1"])
             (fetch-emails [_ _ _] [{:header {:message-id "m-1"} :metadata {:category "news"}}]))
        client (reify int/EmailClient
                 (move-emails-by-id [_ _ moves] (mapv (constantly true) moves)))
        summary (app/move-folder-emails! {:db db :client client} "conn-1" "Newsletter_Alt" (fn [_] nil))]
    (is (= ["conn-1" "Newsletter_Alt"] @asked))
    (is (= {:folder "Newsletter_Alt" :total 1 :uncategorized 0 :moved 1 :not-found 0 :failed 0} summary))))

(deftest category-filter-with-only-unknown-tokens-matches-nothing-without-an-empty-in
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :category-ids ["abc"] :page 1 :size 20})
    (is (= [:= 1 0] (:where @captured)) "Only unknown categories were selected: nothing can match, and no `IN ()` is rendered")
    (app/fetch-emails {:db db} {:filter "all" :category-ids-exclude ["abc"] :page 1 :size 20})
    (is (not (contains? @captured :where)) "Excluding only unknown categories excludes nothing"))
  "A hand-crafted category filter never produces an empty IN list (a syntax error on MariaDB)")

(deftest malformed-date-filters-are-ignored-instead-of-failing-the-list
  (let [captured (atom nil)
        db (stub-emails-db #(reset! captured %) {:data [] :total 0})]
    (app/fetch-emails {:db db} {:filter "all" :date-from "2024-1-1" :date-to "today" :page 1 :size 20})
    (is (not (contains? @captured :where)))
    (app/fetch-emails {:db db :time-zone "UTC"} {:filter "all" :date-from "nonsense" :date-to "2024-01-02" :page 1 :size 20})
    (is (= [:< :headers.date 1704240000] (:where @captured)) "The valid bound still applies"))
  "A date the browser or a bookmark could not format as ISO is dropped like a blank field")

(deftest sqlite-body-search-is-resolved-once-and-inlined
  (let [captured (atom nil)
        lookups (atom 0)
        db (reify int/DB
             (fetch-categories [_] [])
             (fetch-distinct-subjects [_ _] [])
             (fetch-distinct-senders [_ _] [])
             (fetch-distinct-recipients [_ _] [])
             (fetch-header-categories [_ _] [])
             (fetch-content-match-ids [_ text] (swap! lookups inc) (if (= "invoice" text) ["m-1" "m-2"] []))
             (fetch-emails [_ _ customization] (reset! captured customization) {:data [] :total 0}))]
    (app/fetch-emails {:db db :db-type :sqlite} {:filter "all" :search-text "invoice" :page 1 :size 20})
    (is (= 1 @lookups) "The bodies table is scanned once per page load, not once per statement")
    (is (= [:in :headers.message-id ["m-1" "m-2"]] (:where @captured)))
    (app/fetch-emails {:db db :db-type :sqlite} {:filter "all" :search-text "nothing" :page 1 :size 20})
    (is (= [:= 1 0] (:where @captured)) "No match means an empty list, without a subquery"))
  "On SQLite the LIKE scan over every body runs once and its ids are inlined into the list, count and checklist queries")

(deftest category-names-must-be-usable-as-imap-folder-names
  (is (nil? (app/category-name-problem "Invoices")))
  (is (nil? (app/category-name-problem "  Family & Friends ")))
  (is (some? (app/category-name-problem "")))
  (is (some? (app/category-name-problem "   ")))
  (is (some? (app/category-name-problem nil)))
  (is (some? (app/category-name-problem "Work/Projects")))
  (is (some? (app/category-name-problem "News*")))
  (is (some? (app/category-name-problem "Tab\there")))
  "Category names become IMAP folder names and must not carry separators, wildcards or control characters")
