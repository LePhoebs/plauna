(ns plauna.server-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [honey.sql :as honey]
            [plauna.analysis :as analysis]
            [plauna.application :as app]
            [plauna.auth :as auth]
            [plauna.client :as client]
            [plauna.client.oauth :as oauth]
            [plauna.database :as db]
            [plauna.files :as files]
            [plauna.preferences :as preferences]
            [plauna.server :as server])
  (:import [java.time Instant LocalTime ZoneId ZonedDateTime]
           [java.util.concurrent ScheduledExecutorService TimeUnit]))

(deftest statistics-queries-are-portable-and-fully-aggregated
  (let [captured (atom [])]
    (with-redefs [db/query-db (fn [query] (swap! captured conj query) [])]
      (server/mime-type-statistics)
      (server/language-statistics)
      (server/category-statistics))
    (let [sql (mapv #(first (honey/format % {:inline true})) @captured)]
      (is (= 3 (count sql)))
      (is (every? #(str/includes? % "COUNT(headers.message_id)") sql))
      (is (every? #(not (re-find #"(?i)\binterval\b" %)) sql))
      (is (str/includes? (second sql) "LEFT JOIN metadata"))
      (is (str/includes? (nth sql 2) "LEFT JOIN categories")))))

(deftest yearly-statistics-query-does-not-use-a-reserved-alias
  (doseq [[mariadb? expected-bucket]
          [[true "YEAR(FROM_UNIXTIME(date))"]
           [false "STRFTIME('%Y', DATETIME(date, 'unixepoch'))"]]]
    (with-redefs [db/mariadb? (constantly mariadb?)]
      (let [sql (first (honey/format (db/email-statistics-query :yearly) {:inline true}))]
        (is (str/includes? sql (str expected-bucket " AS time_bucket")))
        (is (str/includes? sql (str "GROUP BY " expected-bucket)))
        (is (not (re-find #"(?i)\bAS\s+interval\b" sql))))))
  "MariaDB and SQLite aggregate by year in SQL instead of returning one row per timestamp")

(defn- ok-handler [_] {:status 200 :body "secret"})

(deftest automatic-training-retries-after-a-failed-run-and-does-not-start-twice
  (let [attempts (atom 0)
        completed (promise)
        executor (atom nil)]
    (try
      (with-redefs [preferences/record-successful-training! (fn [_])
                    server/write-emails-to-training-files-and-train
                    (fn []
                      (if (= 1 (swap! attempts inc))
                        (throw (ex-info "simulated training failure" {}))
                        (do (deliver completed true)
                            nil)))]
        (let [^ScheduledExecutorService first-executor
              (server/start-training-scheduler! 0 10 TimeUnit/MILLISECONDS)
              second-executor (server/start-training-scheduler! 0 10 TimeUnit/MILLISECONDS)]
          (reset! executor first-executor)
          (is (identical? first-executor second-executor)
              "Starting the scheduler twice reuses the existing schedule")
          (is (true? (deref completed 1000 false))
              "A failed automatic run does not prevent the next training attempt")
          (is (<= 2 @attempts))))
      (finally
        (server/stop-training-scheduler!)
        (when-let [^ScheduledExecutorService ex @executor]
          (is (.isShutdown ex) "Stopping Plauna also shuts down automatic training")))))
  "Automatic training remains periodic after errors and has only one scheduler")

(deftest daily-training-schedule-uses-a-wall-clock-time-and-catches-up-missed-runs
  (let [zone (ZoneId/of "Europe/Berlin")
        time (LocalTime/parse "02:00")
        before (ZonedDateTime/of 2026 8 7 1 0 0 0 zone)
        after (ZonedDateTime/of 2026 8 7 3 0 0 0 zone)
        successful-today (Instant/parse "2026-08-07T00:05:00Z")
        successful-yesterday (Instant/parse "2026-08-06T00:05:00Z")]
    (is (= (* 60 60 1000)
           (server/daily-training-delay-millis before time nil))
        "Before today's time, a new installation waits until that time")
    (is (zero? (server/daily-training-delay-millis after time nil))
        "After today's time, a missing first run starts immediately")
    (is (= (* 23 60 60 1000)
           (server/daily-training-delay-millis after time successful-today))
        "A completed daily run schedules tomorrow at the configured time")
    (is (zero? (server/daily-training-delay-millis after time successful-yesterday))
        "A restart catches up when today's run was missed")))

(deftest categorization-model-switch-requires-the-exact-confirmation
  (let [calls (atom [])]
    (with-redefs [preferences/categorization-model (fn [] "naive-bayes")
                  server/train-categorization-model! (fn [model] (swap! calls conj [:train model]))
                  preferences/update-preference (fn [& args] (swap! calls conj [:update args]))]
      (let [result (server/switch-categorization-model!
                    {:model "maxent" :use-current-categories "true" :confirmation "start training"})]
        (is (= :alert (:type result)))
        (is (empty? @calls) "Neither training nor the preference changes without exact confirmation")))))

(deftest categorization-model-switch-trains-before-activating-the-target
  (let [calls (atom [])]
    (with-redefs [preferences/categorization-model (fn [] "naive-bayes")
                  server/train-categorization-model! (fn [model] (swap! calls conj [:train model]) nil)
                  preferences/update-preference (fn [key value] (swap! calls conj [:update key value]))
                  preferences/record-successful-training! (fn [_] (swap! calls conj [:record-success]))]
      (let [result (server/switch-categorization-model!
                    {:model "maxent"
                     :use-current-categories "true"
                     :confirmation server/model-switch-confirmation})]
        (is (= :success (:type result)))
        (is (= [[:train "maxent"]
                [:update :categorization-algorithm "maxent"]
                [:record-success]]
               @calls)
            "The active setting changes only after target training completed")))))

(deftest failed-model-migration-keeps-the-previous-model-active
  (let [updates (atom [])]
    (with-redefs [preferences/categorization-model (fn [] "naive-bayes")
                  server/train-categorization-model! (fn [_] (throw (ex-info "training failed" {})))
                  preferences/update-preference (fn [& args] (swap! updates conj args))]
      (let [result (server/switch-categorization-model!
                    {:model "maxent"
                     :use-current-categories "true"
                     :confirmation server/model-switch-confirmation})]
        (is (= :alert (:type result)))
        (is (empty? @updates) "A failed target model is never made active")))))

(deftest first-switch-cannot-skip-training-when-no-target-model-exists
  (with-redefs [preferences/categorization-model (fn [] "naive-bayes")
                server/languages-to-use-in-training (fn [] ["deu"])
                files/model-file (fn [_ _] (java.io.File. "/definitely/not/a/plauna/model.bin"))]
    (let [result (server/switch-categorization-model!
                  {:model "maxent"
                   :confirmation server/model-switch-confirmation})]
      (is (= :alert (:type result)))
      (is (str/includes? (:content result) "No model of that type exists yet")))))

(deftest wrap-authentication-blocks-unauthenticated
  (let [handler (server/wrap-authentication ok-handler)
        response (handler {:uri "/emails" :session {}})]
    (is (= 302 (:status response)) "Unauthenticated request is redirected")
    (is (= "/login" (get-in response [:headers "Location"])) "Redirected to the login page"))
  "Unauthenticated requests to protected paths are redirected to /login")

(deftest connections-page-is-served-as-html
  (with-redefs [db/get-connections (constantly [])]
    (let [response ((server/make-routes {})
                    {:request-method :get
                     :uri "/admin/connections"})]
      (is (= 200 (:status response)))
      (is (= "text/html; charset=UTF-8"
             (get-in response [:headers "Content-Type"])))
      (is (string? (:body response)))))
  "Browsers must render the IMAP connections page instead of downloading an untyped response")

(deftest new-connection-page-is-served-as-html
  (with-redefs [db/get-connections (constantly [])
                db/get-auth-providers (constantly [])]
    (let [response ((server/make-routes {})
                    {:request-method :get
                     :uri "/admin/new-connection"})]
      (is (= 200 (:status response)))
      (is (= "text/html; charset=UTF-8"
             (get-in response [:headers "Content-Type"])))
      (is (string? (:body response)))))
  "Safari must render the new-connection form instead of downloading an untyped response")

(deftest wrap-authentication-allows-authenticated
  (let [handler (server/wrap-authentication ok-handler)
        response (handler {:uri "/emails" :session {:authenticated true}})]
    (is (= 200 (:status response)))
    (is (= "secret" (:body response))))
  "A logged-in session can reach protected paths")

(deftest wrap-authentication-promotes-allowlisted-mtls-certificate-to-session
  (with-redefs [auth/mtls-request-authorized? (constantly true)]
    (let [handler  (server/wrap-authentication ok-handler)
          response (handler {:uri "/emails"
                             :session {:oauth-csrf "keep-me"}
                             :headers {}})]
      (is (= 200 (:status response)))
      (is (= "secret" (:body response)))
      (is (= {:oauth-csrf "keep-me" :authenticated true}
             (select-keys (:session response) [:oauth-csrf :authenticated]))
          "The signed browser session is established without discarding other session state")
      (is (contains? (:session response) :auth-generation) "The promoted session is bound to the current credential generation")
      (is (number? (get-in response [:session :authenticated-at])) "The promoted session carries its login time")))
  "An allowlisted client certificate bypasses the password and creates a normal session")

(deftest wrap-authentication-does-not-trust-unapproved-mtls-request
  (with-redefs [auth/mtls-request-authorized? (constantly false)]
    (let [response ((server/wrap-authentication ok-handler)
                    {:uri "/emails" :session {} :headers {}})]
      (is (= 302 (:status response)))
      (is (= "/login" (get-in response [:headers "Location"])))))
  "Failed or incomplete proxy assertions still require the Plauna password")

(deftest mtls-admin-post-saves-and-activates-the-submitted-settings
  (let [submitted (atom nil)]
    (try
      (with-redefs [auth/save-mtls-settings-from-request!
                    (fn [request] (reset! submitted request) {:enabled true})]
        (let [response ((server/make-routes {})
                        {:request-method :post
                         :uri "/admin/mtls"
                         :params {:trusted-cert-sha256 "fingerprint"
                                  :proxy-secret "secret"
                                  :current-password "admin-password"
                                  :redirect-url "/admin/mtls"}
                         :session {:authenticated true}})]
          (is (= "fingerprint" (get-in @submitted [:params :trusted-cert-sha256])))
          (is (= "secret" (get-in @submitted [:params :proxy-secret])))
          (is (= "admin-password" (get-in @submitted [:params :current-password])))
          (is (= 303 (:status response)))
          (is (= "/admin/mtls" (get-in response [:headers "Location"])))))
      (finally
        (server/empty-global-messages))))
  "Saving through the admin endpoint uses PRG and activates the settings immediately")

(deftest password-login-requires-the-configured-login-name
  (auth/record-login-success!)
  (with-redefs [auth/verify-web-credentials? #(and (= "alice" %1) (= "admin-password" %2))
                auth/web-login-name (constantly "alice")]
    (let [valid-response ((server/make-routes {})
                          {:request-method :post
                           :uri "/login"
                           :params {:login-name "alice" :password "admin-password"}
                           :session {}})
          invalid-response ((server/make-routes {})
                            {:request-method :post
                             :uri "/login"
                             :params {:login-name "root" :password "admin-password"}
                             :session {}})]
      (is (= 302 (:status valid-response)))
      (is (true? (get-in valid-response [:session :authenticated])))
      (is (= 200 (:status invalid-response)))
      (is (str/includes? (:body invalid-response) "Invalid login name or password."))))
  "A correct password cannot authenticate under a different login name")

(deftest login-name-change-requires-the-current-password
  (let [saved (atom nil)
        bumped (atom 0)]
    (with-redefs [auth/verify-web-password? #(= "admin-password" %)
                  auth/set-login-name! #(reset! saved %)
                  preferences/bump-session-generation! (fn [] (swap! bumped inc))
                  preferences/session-generation (fn [] @bumped)]
      (let [response ((server/make-routes {})
                      {:request-method :post
                       :uri "/admin/login-name"
                       :params {:login-name "alice"
                                :current-password "admin-password"
                                :redirect-url "/admin"}
                       :session {:authenticated true}})]
        (is (= "alice" @saved))
        (is (= 303 (:status response)))
        (is (= 1 @bumped) "Changing the login name ends every other session")
        (is (= 1 (get-in response [:session :auth-generation])) "The administrator's own session is renewed for the new generation")))
    (reset! saved nil)
    (with-redefs [auth/verify-web-password? (constantly false)
                  auth/set-login-name! #(reset! saved %)]
      ((server/make-routes {})
       {:request-method :post
        :uri "/admin/login-name"
        :params {:login-name "attacker" :current-password "wrong" :redirect-url "/admin"}
        :session {:authenticated true}})
      (is (nil? @saved)))
    (server/empty-global-messages))
  "An authenticated session alone cannot rename the web login")

(deftest emails-parameters-tolerate-blank-numbers
  ;; The page-size field is a free-form number input; clearing it submits size= (empty string).
  ;; That must fall back to the default instead of throwing NumberFormatException and 400-ing
  ;; the whole request.
  (let [parse-fn (server/template->request-parameters server/emails-template)]
    (is (= 20 (:size (parse-fn {:size ""}))) "A blank size falls back to the default")
    (is (= 20 (:size (parse-fn {:size "abc"}))) "A non-numeric size falls back to the default")
    (is (= 50 (:size (parse-fn {:size "50"}))) "A valid size is parsed")
    (is (= 1 (:page (parse-fn {:page ""}))) "A blank page falls back to the default")
    (is (= "true" (:subject-values-none (parse-fn {:subject-values-none "true"})))
        "An explicitly empty checklist survives request parsing"))
  "Blank or non-numeric size/page parameters fall back to their defaults")

(deftest wrap-authentication-allows-public-paths
  (let [handler (server/wrap-authentication ok-handler)]
    (doseq [uri ["/login" "/css/tailwind.css" "/favicon-32x32.png"
                 "/plauna-banner.png" "/site.webmanifest"]]
      (is (= 200 (:status (handler {:uri uri :session {}})))
          (str uri " is reachable without authentication"))))
  "Login and static assets are reachable without authentication")

(deftest static-assets-are-revalidated-after-a-deployment
  (let [handler (server/wrap-static-asset-revalidation
                 (fn [_] {:status 200 :headers {"Content-Type" "text/css"} :body "asset"}))]
    (is (= "no-cache" (get-in (handler {:uri "/css/tailwind.css"}) [:headers "Cache-Control"])))
    (is (= "no-cache" (get-in (handler {:uri "/js/vendor/vega.min.js"}) [:headers "Cache-Control"])))
    (is (nil? (get-in (handler {:uri "/statistics"}) [:headers "Cache-Control"]))))
  "Stable asset URLs must not leave browsers on an old UI after a container update")

(deftest recategorize-email-reports-a-missing-imap-message-without-saving
  (let [updates (atom [])]
    (with-redefs [server/enriched-email-by-message-id (fn [_] {:header {:message-id "missing-1"}})
                  db/get-categories (fn [] [{:id 7 :name "Correct"}])
                  app/move-email-to-category (fn [_ _ _] {:result :not-found})
                  db/update-metadata-category (fn [& args] (swap! updates conj args))]
      (let [response (server/recategorize-email-response {} {:message-id "missing-1" :category "7"})]
        (is (= 404 (:status response)))
        (is (= "email-not-found" (:body response)))
        (is (empty? @updates) "The category is not changed before the user confirms")))))

(deftest recategorize-email-force-updates-only-plauna-metadata
  (let [updates (atom [])]
    (with-redefs [app/move-email-to-category (fn [& _] (throw (ex-info "IMAP must not be called" {})))
                  db/update-metadata-category (fn [& args] (swap! updates conj args))]
      (let [response (server/recategorize-email-response {} {:message-id "missing-1" :category "7" :force "true"})]
        (is (= 204 (:status response)))
        (is (= [["missing-1" 7 1.0]] @updates))))))

(deftest recategorize-email-keeps-general-move-errors-non-overridable
  (let [updates (atom [])]
    (with-redefs [server/enriched-email-by-message-id (fn [_] {:header {:message-id "error-1"}})
                  db/get-categories (fn [] [{:id 7 :name "Correct"}])
                  app/move-email-to-category (fn [_ _ _] {:result :error})
                  db/update-metadata-category (fn [& args] (swap! updates conj args))]
      (let [response (server/recategorize-email-response {} {:message-id "error-1" :category "7"})]
        (is (= 200 (:status response)))
        (is (= "saved-not-moved" (:body response)))
        (is (empty? @updates))))))

(deftest reconnect-control-restarts-the-connection
  (let [calls (atom [])
        existing-connection {:id "conn-1"}]
    (with-redefs [client/connection-data-from-id (fn [id]
                                                   (swap! calls conj [:lookup id])
                                                   existing-connection)
                  client/disconnect (fn [connection-data]
                                      (swap! calls conj [:disconnect connection-data]))
                  app/connect-to-client (fn [_ id]
                                          (swap! calls conj [:connect id])
                                          {:result :ok})]
      (let [response (#'server/reconnect-control-response {} {:uri "/admin/connections"
                                                               :params {}
                                                               :session {}}
                      "conn-1")]
        (is (= [[:lookup "conn-1"]
                [:disconnect existing-connection]
                [:connect "conn-1"]]
               @calls))
        (is (= 303 (:status response)))
        (is (= "/admin/connections" (get-in response [:headers "Location"])))))))

(deftest blank-secrets-keep-the-values-already-stored
  (let [connection (#'server/connection-update-from-params
                    {:id "connection-1"
                     :host "imap.example.com"
                     :user "me@example.com"
                     :secret ""
                     :folder "INBOX"
                     :security "ssl"
                     :auth-type "basic"}
                    {:secret "stored-imap-secret"})
        provider (#'server/auth-provider-update-from-params
                  {:id "12" :name "Example" :client_secret ""}
                  {:client-secret "stored-oauth-secret"})]
    (is (= "stored-imap-secret" (:secret connection)))
    (is (= "stored-oauth-secret" (:client_secret provider)))
    (is (= "new-secret"
           (:secret (#'server/connection-update-from-params
                     {:id "connection-1" :secret "new-secret" :security "ssl" :auth-type "basic"}
                     {:secret "stored-imap-secret"}))))
    (is (= "new-client-secret"
           (:client_secret (#'server/auth-provider-update-from-params
                            {:id "12" :client_secret "new-client-secret"}
                            {:client-secret "stored-oauth-secret"}))))))

(deftest connection-template-context-excludes-the-stored-imap-secret
  (with-redefs [db/get-connection (fn [_] {:id "connection-1"
                                           :host "imap.example.com"
                                           :secret "must-not-enter-template-context"})
                client/monitor->map (fn [_] {:connected false})]
    (let [connection (server/connection-information "connection-1")]
      (is (= "imap.example.com" (:host connection)))
      (is (not (contains? connection :secret))))))

(deftest the-rescan-label-and-the-rescan-action-use-the-same-watched-folder
  (let [live (client/->ConnectionData {:id "connection-1" :folder "INBOX"} nil nil nil nil nil)]
    (with-redefs [db/get-connection (fn [_] {:id "connection-1" :host "h" :folder "Archive"})
                  client/monitor->map (fn [_] {:connected true})]
      (swap! client/connections assoc "connection-1" live)
      (try
        (is (= "INBOX" (:monitored-folder (server/connection-information "connection-1")))
            "A saved but not yet applied edit of the watched folder does not change what the live monitor watches")
        (finally (swap! client/connections dissoc "connection-1")))
      (is (= "Archive" (:monitored-folder (server/connection-information "connection-1")))
          "Without a live connection the stored folder is the answer")))
  (with-redefs [db/get-connection (fn [_] {:id "connection-2" :host "h" :folder ""})
                client/monitor->map (fn [_] {:connected false})]
    (is (= "INBOX" (:monitored-folder (server/connection-information "connection-2")))
        "A blank watched folder means INBOX")))

(deftest folder-parse-summary-message-does-not-claim-the-whole-folder-for-a-since-days-batch
  (let [period (server/folder-parse-summary-message {:folder "INBOX" :processed 2 :skipped 3 :errors 0 :remaining 0 :batch-size 50 :since-days 1})
        whole (server/folder-parse-summary-message {:folder "INBOX" :processed 2 :skipped 3 :errors 0 :remaining 0 :batch-size 50})]
    (is (str/includes? (:content period) "All e-mails of that period have been examined"))
    (is (not (str/includes? (:content period) "The folder has been examined completely")))
    (is (str/includes? (:content whole) "The folder has been examined completely") "Without a period the message is unchanged")))

(deftest oauth-start-stores-only-the-provider-id-in-the-browser-session
  (let [provider {:id 12
                  :name "Example"
                  :auth-url "https://login.example.com/authorize"
                  :client-id "client-id"
                  :client-secret "must-stay-on-server"
                  :redirect-url "https://plauna.example.com/oauth2/callback"
                  :scope "mail"}]
    (with-redefs [app/connect-to-client (fn [_ _] {:result :redirect :provider provider})]
      (let [response (#'server/connect-control-response
                      {}
                      {:session {:authenticated true
                                 :provider {:client-secret "old-secret-from-an-existing-session"}}}
                      "connection-1")
            session (:session response)]
        (is (= 302 (:status response)))
        (is (= 12 (:provider-id session)))
        (is (not (contains? session :provider)))
        (is (not (str/includes? (pr-str session) "must-stay-on-server")))))))

(deftest oauth-callback-loads-provider-server-side-and-clears-one-time-session-data
  (let [provider {:id 12 :client-secret "server-side-secret"}
        exchanged (atom nil)]
    (with-redefs [db/get-auth-provider (fn [id]
                                        (is (= 12 id))
                                        provider)
                  oauth/exchange-code-for-access-token
                  (fn [loaded-provider code]
                    (reset! exchanged [loaded-provider code])
                    {:access_token "token"})
                  db/save-oauth-token (fn [_])
                  app/connect-to-client (fn [_ _] {:result :ok})]
      (let [response ((server/make-routes {})
                      {:request-method :get
                       :uri "/oauth2/callback"
                       :params {:state "csrf" :code "authorization-code"}
                       :session {:authenticated true
                                 :oauth-csrf "csrf"
                                 :connection-id "connection-1"
                                 :provider-id 12}})]
        (is (= [provider "authorization-code"] @exchanged))
        (is (= 302 (:status response)))
        (is (= {:authenticated true} (:session response)))))))

(deftest refetch-email-saves-participants
  (let [participants [{:message-id "msg-1" :contact-key "sender-key" :name "Sender" :address "sender@example.com" :type :sender}
                      {:message-id "msg-1" :contact-key "to-key" :name "Receiver" :address "to@example.com" :type :receiver}
                      {:message-id "msg-1" :contact-key "cc-key" :name "Copy" :address "cc@example.com" :type :cc}
                      {:message-id "msg-1" :contact-key "bcc-key" :name "Blind" :address "bcc@example.com" :type :bcc}]
        calls (atom [])]
    (with-redefs [client/refetch-message-by-id (fn [_] {:body [] :participants participants})
                  db/fetch-bodies-for (fn [_] [])
                  db/delete-training-tokens! (fn [_] nil)
                  db/save-contacts (fn [contacts] (swap! calls conj [:contacts contacts]))
                  db/save-communications (fn [contacts] (swap! calls conj [:communications contacts]))
                  db/fetch-metadata (fn [_] {:language "en"})]
      (is (= {:type :success :content "Re-fetched the email from the server and filled in its contents."}
             (server/refetch-email-and-fill! "msg-1")))
      (is (= [[:contacts participants]
              [:communications participants]]
             @calls)))))

(deftest parse-batch-size-accepts-only-positive-integers
  (is (= 100 (server/parse-batch-size "100")))
  (is (= 5 (server/parse-batch-size " 5 ")))
  (is (nil? (server/parse-batch-size "")) "Blank means the whole folder")
  (is (nil? (server/parse-batch-size nil)))
  (is (nil? (server/parse-batch-size "abc")))
  (is (nil? (server/parse-batch-size "0")))
  (is (nil? (server/parse-batch-size "-3"))))

(deftest folder-parse-summary-message-explains-the-next-step
  (let [full-batch (server/folder-parse-summary-message {:folder "Old" :processed 100 :skipped 250 :errors 0 :remaining 1234 :batch-size 100})
        finished (server/folder-parse-summary-message {:folder "Old" :processed 12 :skipped 88 :errors 0 :remaining 0 :batch-size 100})
        with-errors (server/folder-parse-summary-message {:folder "Old" :processed 1 :skipped 0 :errors 2 :remaining 0 :batch-size nil})]
    (is (= :success (:type full-batch)))
    (is (str/includes? (:content full-batch) "100 new e-mail(s)"))
    (is (str/includes? (:content full-batch) "250 already stored"))
    (is (str/includes? (:content full-batch) "1234 older e-mail(s) were not examined"))
    (is (str/includes? (:content full-batch) "run the parse again"))
    (is (str/includes? (:content finished) "examined completely"))
    (is (= :info (:type with-errors)))
    (is (str/includes? (:content with-errors) "2 could not be read"))
    (is (not (str/includes? (:content with-errors) "examined completely")) "No batch size: the batch note is left out")))

(deftest parse-since-days-accepts-only-positive-integers-and-caps-absurd-ones
  (is (= 7 (server/parse-since-days "7")))
  (is (= 30 (server/parse-since-days " 30 ")))
  (is (nil? (server/parse-since-days "")) "Blank means no time limit")
  (is (nil? (server/parse-since-days nil)))
  (is (nil? (server/parse-since-days "abc")))
  (is (nil? (server/parse-since-days "0")))
  (is (nil? (server/parse-since-days "-3")))
  (is (= server/max-since-days (server/parse-since-days "999999")) "A look-back beyond any mailbox is capped, not rejected")
  (is (nil? (server/parse-since-days "99999999999")) "A number that does not even fit an integer is no limit"))

(deftest folder-parse-summary-message-names-the-period-of-a-since-days-run
  (let [week (server/folder-parse-summary-message {:folder "INBOX" :processed 3 :skipped 9 :errors 0 :remaining 0 :batch-size nil :since-days 7})
        day (server/folder-parse-summary-message {:folder "INBOX" :processed 0 :skipped 2 :errors 0 :remaining 0 :batch-size nil :since-days 1})
        plain (server/folder-parse-summary-message {:folder "INBOX" :processed 3 :skipped 9 :errors 0 :remaining 0 :batch-size nil})]
    (is (str/starts-with? (:content week) "Finished parsing INBOX (e-mails received in the last 7 days): 3 new e-mail(s) saved and categorized, 9 already stored"))
    (is (str/includes? (:content day) "(e-mails received in the last day)"))
    (is (str/starts-with? (:content plain) "Finished parsing INBOX: 3 new") "Without a period the message reads as before")))

(deftest folder-parse-summary-message-links-to-the-batch-review
  (let [with-emails (server/folder-parse-summary-message {:folder "Old" :processed 3 :skipped 0 :errors 0 :remaining 0 :batch-size 100 :batch-id "run-1"})
        nothing-new (server/folder-parse-summary-message {:folder "Old" :processed 0 :skipped 50 :errors 0 :remaining 0 :batch-size 100 :batch-id "run-2"})]
    (is (= "/emails?batch=run-1" (:link with-emails)))
    (is (= "Review this batch" (:link-text with-emails)))
    (is (nil? (:link nothing-new)) "A run that saved nothing has nothing to review")))

(deftest emails-template-accepts-the-batch-parameter
  (let [parse-fn (server/template->request-parameters server/emails-template)]
    (is (= "run-1" (:batch (parse-fn {:batch "run-1"}))))
    (is (nil? (:batch (parse-fn {}))))))

(deftest training-percent-moves-through-collection-and-per-language-training
  (is (= 0 (server/training-percent {:status :running :phase :starting})))
  (is (= 0 (server/training-percent {:status :running :phase :collecting :emails-written 0 :emails-total 200})))
  (is (= 13 (server/training-percent {:status :running :phase :collecting :emails-written 100 :emails-total 200})))
  (is (= 25 (server/training-percent {:status :running :phase :training :languages 2 :language-progress {}})))
  (is (= 44 (server/training-percent {:status :running :phase :training :languages 2
                                      :language-progress {"deu" {:iteration 500 :iterations 1000} "eng" {:iteration 0 :iterations 1000}}})))
  (is (= 63 (server/training-percent {:status :running :phase :training :languages 2
                                      :language-progress {"deu" {:done? true} "eng" {:iteration 0 :iterations 1000}}}))
      "A finished language counts fully even without iteration callbacks")
  (is (= 100 (server/training-percent {:status :finished :phase :finished})))
  (is (= 25 (server/training-percent {:status :running :phase :training :languages 0 :language-progress nil}))
      "Missing counts never divide by zero"))

(deftest only-one-training-job-runs-at-a-time
  (let [release (promise)
        finished (promise)]
    (reset! server/training-progress {:status :idle})
    (is (true? (server/start-training-job! "Manual training" (fn [] @release (deliver finished true) nil))))
    (is (true? (server/training-running?)))
    (is (false? (server/start-training-job! "Manual training" (fn [] nil))) "A second start is refused while the first runs")
    (is (= server/training-busy-message (server/run-training-job! "Automatic training" (fn [] nil)))
        "The scheduler also waits instead of running concurrently")
    (deliver release true)
    (is (true? (deref finished 2000 false)))
    (let [deadline (+ (System/currentTimeMillis) 2000)]
      (while (and (server/training-running?) (< (System/currentTimeMillis) deadline)) (Thread/sleep 10)))
    (let [status (server/training-status)]
      (is (= :finished (:status status)))
      (is (= 100 (:percent status)))
      (is (= server/training-success-message (:result status)) "A nil job result is reported as success"))
    (is (true? (server/start-training-job! "Manual training" (fn [] {:type :alert :content "nothing to train"}))) "A finished slot can be claimed again")
    (let [deadline (+ (System/currentTimeMillis) 2000)]
      (while (and (server/training-running?) (< (System/currentTimeMillis) deadline)) (Thread/sleep 10)))
    (is (= {:type :alert :content "nothing to train"} (:result (server/training-status))))))

(deftest failing-training-job-is-reported-not-thrown
  (reset! server/training-progress {:status :idle})
  (let [result (server/run-training-job! "Manual training" (fn [] (throw (ex-info "boom" {}))))]
    (is (= :alert (:type result)))
    (is (= :finished (:status @server/training-progress)))))

(deftest batch-move-summary-message-explains-the-outcome
  (let [clean (server/batch-move-summary-message {:folder "Old" :moved 90 :total 90 :not-found 0 :failed 0 :uncategorized 10})
        partial (server/batch-move-summary-message {:folder "Old" :moved 80 :total 90 :not-found 7 :failed 3 :uncategorized 0})]
    (is (= :success (:type clean)))
    (is (str/includes? (:content clean) "Moved 90 of 90"))
    (is (str/includes? (:content clean) "10 e-mail(s) without a category were left in place"))
    (is (= :info (:type partial)))
    (is (str/includes? (:content partial) "7 were not found"))
    (is (str/includes? (:content partial) "3 could not be moved"))))

(deftest retry-summary-message-links-to-recovered-emails
  (let [recovered (server/retry-summary-message {:folder "INBOX" :processed 4 :skipped 0 :gone 1 :errors 0 :batch-id "r-1"})
        nothing (server/retry-summary-message {:folder "INBOX" :processed 0 :skipped 0 :gone 0 :errors 2 :batch-id "r-2"})]
    (is (= :success (:type recovered)))
    (is (= "/emails?batch=r-1" (:link recovered)))
    (is (str/includes? (:content recovered) "4 saved and categorized"))
    (is (= :info (:type nothing)))
    (is (nil? (:link nothing)))
    (is (str/includes? (:content nothing) "2 failed again"))))

(deftest training-outcome-message-names-skipped-and-failed-languages
  (with-redefs [analysis/label->category (fn [label] (when (= "7" label) {:id 7 :name "Rechnungen"}))]
    (is (nil? (server/training-outcome-message [{:language "deu"}] [] [])) "Everything trained: the default success message applies")
    (let [partial (server/training-outcome-message [{:language "deu"}]
                                                   [{:language "eng" :samples 12 :labels #{"7"}}]
                                                   [{:language "fra" :error (ex-info "boom" {})}])]
      (is (= :info (:type partial)))
      (is (str/includes? (:content partial) "Trained the deu model(s)"))
      (is (str/includes? (:content partial) "eng: all 12 categorized e-mail(s) belong to one category (Rechnungen)"))
      (is (str/includes? (:content partial) "fra: training failed (boom)")))
    (let [nothing (server/training-outcome-message [] [{:language "eng" :samples 3 :labels #{"9"}}] [])]
      (is (= :alert (:type nothing)))
      (is (str/includes? (:content nothing) "No model was trained"))
      (is (str/includes? (:content nothing) "(9)") "An unknown label falls back to the raw label"))))

(deftest partial-training-counts-as-a-successful-scheduled-run
  (let [recorded (atom 0)]
    (with-redefs [preferences/record-successful-training! (fn [_] (swap! recorded inc))
                  server/write-emails-to-training-files-and-train (fn [] {:type :info :content "partial"})]
      (is (= {:type :info :content "partial"} (server/manual-training-job)))
      (is (= 1 @recorded)))
    (with-redefs [preferences/record-successful-training! (fn [_] (swap! recorded inc))
                  server/write-emails-to-training-files-and-train (fn [] {:type :alert :content "nothing"})]
      (server/manual-training-job)
      (is (= 1 @recorded) "A run that trained nothing is not recorded as successful"))))

(deftest model-switch-proceeds-after-a-partial-training
  (let [switched (atom nil)]
    (with-redefs [server/train-categorization-model! (fn [_] {:type :info :content "Trained the deu model(s). Not trained - fra: one category."})
                  preferences/update-preference (fn [key value] (reset! switched [key value]))
                  preferences/record-successful-training! (fn [_])
                  files/model-file (fn [language _] (java.io.File. (str "/nonexistent/" language ".bin")))
                  server/languages-to-use-in-training (fn [] ["deu" "fra"])]
      (let [result (server/switch-categorization-model! {:model "maxent" :use-current-categories "true" :confirmation "Start training"})]
        (is (= [:categorization-algorithm "maxent"] @switched) "Languages that cannot be trained do not block the switch")
        (is (= :info (:type result)))
        (is (str/includes? (:content result) "Switched to maxent."))
        (is (str/includes? (:content result) "Not trained - fra: one category."))
        (is (str/includes? (:content result) "E-mails in deu, fra are not categorized by this model"))))))

(deftest model-switch-is-refused-only-when-nothing-could-be-trained
  (with-redefs [server/train-categorization-model! (fn [_] {:type :alert :content "No model was trained."})
                preferences/update-preference (fn [_ _] (throw (ex-info "must not switch" {})))]
    (let [result (server/switch-categorization-model! {:model "maxent" :use-current-categories "true" :confirmation "Start training"})]
      (is (= :alert (:type result)))
      (is (str/includes? (:content result) "No model was trained")))))

(deftest training-steps-follow-the-run-through-its-phases
  (let [state-ids (fn [steps] (mapv (juxt :id :state) steps))]
    (is (= [["prepare" "running"] ["collect" "pending"] ["train" "pending"] ["write" "pending"] ["finish" "pending"]]
           (state-ids (server/training-steps {:status :running :phase :starting}))))
    (let [collecting (server/training-steps {:status :running :phase :collecting :emails-written 120 :emails-total 400 :tokens-computed 30
                                             :training-languages ["deu" "eng"]})]
      (is (= [["prepare" "done"] ["collect" "running"] ["train" "pending"] ["write" "pending"] ["finish" "pending"]] (state-ids collecting)))
      (is (= "120 of 400 e-mails, 30 analysed for the first time and cached" (:detail (second collecting))))
      (is (= [["deu" "pending"] ["eng" "pending"]] (mapv (juxt :label :state) (:children (nth collecting 2))))
          "The languages are listed as pending before training starts"))
    (let [training (server/training-steps {:status :running :phase :training :languages 1
                                           :training-languages ["deu" "eng" "fra"]
                                           :language-progress {"deu" {:iteration 250 :iterations 1000}}
                                           :skipped-languages [{:language "fra" :reason "fra: all 3 categorized e-mail(s) belong to one category (X); a model needs at least two categories"}]})
          children (:children (nth training 2))]
      (is (= [["prepare" "done"] ["collect" "done"] ["train" "running"] ["write" "pending"] ["finish" "pending"]] (state-ids training)))
      (is (= [["deu" "running" "iteration 250 of at most 1000"] ["eng" "pending" nil]
              ["fra" "skipped" "fra: all 3 categorized e-mail(s) belong to one category (X); a model needs at least two categories"]]
             (mapv (juxt :label :state :detail) children))))
    (let [writing (server/training-steps {:status :running :phase :writing :models-written 1 :models-total 2
                                          :training-languages ["deu" "eng"]
                                          :language-progress {"deu" {:done? true} "eng" {:done? true :failed? true}}})]
      (is (= [["prepare" "done"] ["collect" "done"] ["train" "done"] ["write" "running"] ["finish" "pending"]] (state-ids writing)))
      (is (= "1 of 2 model file(s)" (:detail (nth writing 3))))
      (is (= [["deu" "done"] ["eng" "failed"]] (mapv (juxt :label :state) (:children (nth writing 2))))))
    (let [finished (server/training-steps {:status :finished :phase :writing :models-written 2 :models-total 2
                                           :training-languages ["deu"] :language-progress {"deu" {:done? true}}
                                           :result {:type :success :content "Training finished."}})]
      (is (= [["prepare" "done"] ["collect" "done"] ["train" "done"] ["write" "done"] ["finish" "done"]] (state-ids finished)))
      (is (= "Training finished." (:detail (last finished)))))
    (let [failed-early (server/training-steps {:status :finished :phase :collecting :emails-written 0 :emails-total 0
                                               :result {:type :alert :content "There are no categorized e-mails in the selected training languages."}})]
      (is (= [["prepare" "done"] ["collect" "failed"] ["train" "pending"] ["write" "pending"] ["finish" "failed"]] (state-ids failed-early))
          "A run that stopped while collecting marks that step as failed and leaves the rest untouched"))
    (is (= 99 (server/training-percent {:status :running :phase :writing})))
    (is (contains? (server/training-status) :steps))))

(deftest a-busy-slot-does-not-count-as-a-completed-automatic-run
  (reset! server/training-progress {:status :running :label "Manual training" :phase :training})
  (try
    (is (false? (#'server/run-automatic-training!)) "A refused daily run reports failure so it is caught up later")
    (finally (reset! server/training-progress {:status :idle}))))

(deftest finished-training-jobs-surface-their-result-as-a-message
  (reset! server/training-progress {:status :idle})
  (reset! server/global-messages [])
  (server/run-training-job! "Manual training" (fn [] {:type :alert :content "nothing to train"}))
  (is (= [{:type :alert :content "nothing to train"}] @server/global-messages))
  (reset! server/global-messages []))

(deftest model-switch-validation-rejects-before-claiming-the-slot
  (with-redefs [preferences/categorization-model (fn [] "naive-bayes")]
    (is (= :alert (:type (server/model-switch-validation {:model "nonsense" :confirmation "Start training"}))))
    (is (= :info (:type (server/model-switch-validation {:model "naive-bayes" :confirmation "Start training"}))))
    (is (= :alert (:type (server/model-switch-validation {:model "maxent" :confirmation "start training"}))))
    (is (nil? (server/model-switch-validation {:model "maxent" :confirmation "Start training"})))))

(deftest folder-parse-summary-message-explains-an-aborted-run
  (let [aborted (server/folder-parse-summary-message {:folder "INBOX" :processed 3 :skipped 10 :errors 50 :remaining 800 :batch-size 100 :batch-id "b"
                                                       :aborted "50 consecutive messages could not be read; the connection to the folder is probably lost"})]
    (is (= :info (:type aborted)))
    (is (str/includes? (:content aborted) "The run was 50 consecutive messages could not be read"))
    (is (str/includes? (:content aborted) "800 e-mail(s) were not examined"))))

(deftest folder-parse-summary-message-explains-the-skips
  (let [message (server/folder-parse-summary-message {:folder "Old" :processed 100 :skipped 71 :skipped-elsewhere 12 :errors 0 :remaining 4344 :batch-size 100 :batch-id "b"})]
    (is (str/includes? (:content message) "71 already stored e-mail(s) skipped (59 saved by earlier runs of this folder and still waiting to be moved, 12 duplicate(s) of e-mails stored from other folders)"))))

(deftest move-summary-message-names-what-was-moved
  (let [message (server/move-summary-message "the folder Old" {:moved 90 :total 95 :not-found 5 :failed 0 :uncategorized 3})]
    (is (= :info (:type message)))
    (is (str/includes? (:content message) "Moved 90 of 95 categorized e-mail(s) of the folder Old"))
    (is (str/includes? (:content message) "5 were not found"))))

(deftest safe-redirect-path-rejects-backslash-protocol-relative-targets
  (is (= "/emails" (server/safe-redirect-path "/emails" "/")))
  (is (= "/" (server/safe-redirect-path "/" "/x")))
  (is (= "/x" (server/safe-redirect-path "//evil.example" "/x")))
  (is (= "/x" (server/safe-redirect-path "/\\evil.example" "/x")) "Browsers read /\\host as //host")
  (is (= "/x" (server/safe-redirect-path "https://evil.example" "/x")))
  (is (= "/x" (server/safe-redirect-path nil "/x"))))

(deftest cross-site-state-changing-requests-are-recognised
  (is (true? (server/cross-site-request? {:request-method :post :headers {"origin" "https://evil.example" "host" "plauna.example"}})))
  (is (false? (server/cross-site-request? {:request-method :post :headers {"origin" "https://plauna.example" "host" "plauna.example"}})))
  (is (false? (server/cross-site-request? {:request-method :post :headers {"origin" "https://plauna.example" "host" "plauna:8080" "x-forwarded-host" "plauna.example"}}))
      "Behind a reverse proxy the forwarded host counts")
  (is (false? (server/cross-site-request? {:request-method :get :headers {"origin" "https://evil.example" "host" "plauna.example"}})) "Reads are not judged")
  (is (false? (server/cross-site-request? {:request-method :post :headers {"host" "plauna.example"}})) "Non-browser clients send no Origin"))

(deftest certificate-authenticated-cross-site-posts-are-refused
  (with-redefs [auth/mtls-request-authorized? (constantly true)]
    (let [handler (server/wrap-authentication ok-handler)]
      (is (= 403 (:status (handler {:uri "/admin/categories" :request-method :post :session {}
                                    :headers {"origin" "https://evil.example" "host" "plauna.example"}}))))
      (is (= 200 (:status (handler {:uri "/admin/categories" :request-method :post :session {}
                                    :headers {"origin" "https://plauna.example" "host" "plauna.example"}}))))))
  "The browser presents the client certificate for cross-site requests too, so SameSite cannot protect this login path")

(deftest sessions-end-when-the-credential-generation-changes
  (with-redefs [preferences/session-generation (fn [] 3)]
    (let [handler (server/wrap-authentication ok-handler)
          now (System/currentTimeMillis)]
      (is (= 200 (:status (handler {:uri "/emails" :session {:authenticated true :auth-generation 3 :authenticated-at now}}))))
      (is (= 302 (:status (handler {:uri "/emails" :session {:authenticated true :auth-generation 2 :authenticated-at now}})))
          "A session from before a password change is no longer accepted")
      (is (= 302 (:status (handler {:uri "/emails" :session {:authenticated true :auth-generation 3 :authenticated-at (- now server/session-max-age-millis 1000)}})))
          "A login older than the maximum age is no longer accepted")
      (is (= 302 (:status (handler {:uri "/emails" :session {:authenticated true}})))
          "A session from before generations existed counts as generation 0")))
  "A leaked cookie is worthless after the password changes or a month has passed")

(deftest pending-toasts-are-taken-atomically
  (server/empty-global-messages)
  (server/add-to-messages {:type :info :content "one"})
  (server/add-to-messages {:type :info :content "two"})
  (let [taken (server/take-messages!)]
    (is (= ["one" "two"] (mapv :content taken)))
    (is (empty? (server/take-messages!)) "Taking empties the queue in the same step")))

(deftest categorizing-without-activated-languages-does-nothing
  (with-redefs [db/get-activated-language-preferences (constantly [])
                db/fetch-data (fn [& _] (throw (ex-info "must not query with an empty IN list" {})))]
    (is (= 0 (server/categorize-uncategorized-n-emails 20))))
  "No activated language means no `IN ()` clause and no work")

(deftest unknown-urls-and-malformed-email-ids-get-proper-status-codes
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [routes (server/make-routes {})]
      (is (= 404 (:status (routes {:request-method :get :uri "/no/such/page" :session {:authenticated true}}))))
      (is (= 400 (:status (routes {:request-method :get :uri "/emails/not*base64!" :session {:authenticated true}}))))
      (is (= 400 (:status (routes {:request-method :delete :uri "/emails/not*base64!" :session {:authenticated true}}))))))
  "An unknown page is a 404 and a garbled e-mail id a 400, never a 500")

(deftest auth-provider-update-answers-204-and-writes-only-known-columns
  (let [updated (atom nil)]
    (with-redefs [db/get-auth-provider (fn [_] {:id 7 :client-secret "keep-me"})
                  db/update-auth-provider (fn [provider] (reset! updated provider))]
      (let [response ((server/make-routes {})
                      {:request-method :put :uri "/admin/auth-providers/7"
                       :params {:name "Google" :client_id "cid" :client_secret "" :scope "mail" :auth_url "a" :token_url "t" :redirect_url "r" :evil "DROP TABLE" :redirect-url "/x"}
                       :session {:authenticated true}})]
        (is (= 204 (:status response)))
        (is (= {:id "7" :name "Google" :client_id "cid" :client_secret "keep-me" :scope "mail" :auth_url "a" :token_url "t" :redirect_url "r"} @updated)
            "Unknown form fields never reach the SQL, and a blank secret keeps the stored one"))))
  "The provider update no longer returns the JDBC result (which rendered as a 500)")

(deftest connection-controls-answer-unknown-operations-and-inactive-connections-with-alerts
  (server/empty-global-messages)
  (with-redefs [client/connection-data-from-id (fn [_] nil)]
    (let [routes (server/make-routes {})
          control (fn [params] (routes {:request-method :post :uri "/admin/connections/c1/controls"
                                        :params (assoc params :redirect-url "/admin/connections/c1")
                                        :session {:authenticated true}}))
          unknown (control {:operation "explode"})
          disconnect (control {:operation "disconnect"})
          parse (control {:operation "parse" :folder "INBOX"})]
      (is (= 303 (:status unknown)))
      (is (= 303 (:status disconnect)) "Disconnecting an unregistered connection is a no-op, not a 500")
      (is (= 303 (:status parse)))
      (let [messages (server/take-messages!)]
        (is (some #(str/includes? (:content %) "Unknown connection operation") messages))
        (is (some #(str/includes? (:content %) "not active") messages) "Parsing on an inactive connection is refused with an alert"))))
  "Connection controls never answer ordinary mistakes with an error page")

(deftest login-attempts-during-the-brake-are-refused-with-429
  (auth/record-login-success!)
  (with-redefs [auth/verify-web-credentials? (fn [_ _] false)]
    (let [routes (server/make-routes {})
          attempt (fn [] (routes {:request-method :post :uri "/login" :params {:login-name "x" :password "y"} :session {}}))]
      (is (= 200 (:status (attempt))) "The first wrong guess is answered normally")
      (let [second-response (attempt)]
        (is (= 429 (:status second-response)) "A guess arriving inside the pause is refused outright")
        (is (some? (get-in second-response [:headers "Retry-After"])))
        (is (str/includes? (:body second-response) "Too many failed login attempts")))))
  (auth/record-login-success!)
  "The brake refuses fast retries instead of parking a request thread")

(deftest rescan-control-requires-the-number-of-days
  (server/empty-global-messages)
  (with-redefs [client/connection-data-from-id (fn [_] (throw (ex-info "must not even look for the connection" {})))]
    (let [routes (server/make-routes {})
          rescan (fn [params] (routes {:request-method :post :uri "/admin/connections/c1/controls"
                                       :params (merge {:operation "rescan" :redirect-url "/admin/connections/c1"} params)
                                       :session {:authenticated true}}))]
      (doseq [days [nil "" "abc" "0" "-2"]]
        (is (= 303 (:status (rescan {:since-days days})))))
      (let [messages (server/take-messages!)]
        (is (= 5 (count messages)))
        (is (every? #(and (= :alert (:type %)) (str/includes? (:content %) "how many days")) messages)
            "A blank field is refused instead of silently reading the whole folder"))))
  "The one-click re-scan never degrades into a full-folder parse.")

(deftest rescan-control-starts-a-since-days-run-on-the-watched-folder
  (server/empty-global-messages)
  (let [started (atom nil)]
    (with-redefs [client/connection-data-from-id (fn [_] {:config {:id "c1" :folder "Posteingang"}})
                  server/start-folder-parse! (fn [_ id folder options] (reset! started [id folder options]) 42)]
      (let [routes (server/make-routes {})
            response (routes {:request-method :post :uri "/admin/connections/c1/controls"
                              :params {:operation "rescan" :since-days "7" :move "on" :folder "Spam" :batch-size "5" :redirect-url "/admin/connections/c1"}
                              :session {:authenticated true}})]
        (is (= 303 (:status response)))
        (is (= ["c1" "Posteingang" {:move? true :since-days 7}] @started)
            "The folder is the one the live connection watches (a submitted folder or batch size is ignored); the period is passed on")
        (let [[message] (server/take-messages!)]
          (is (= :success (:type message)))
          (is (str/includes? (:content message) "Only e-mails received in the last 7 days"))))))
  (server/empty-global-messages)
  (let [started (atom nil)]
    (with-redefs [client/connection-data-from-id (fn [_] {:config {:id "c1"}})
                  server/start-folder-parse! (fn [_ id folder options] (reset! started [id folder options]) 3)]
      ((server/make-routes {}) {:request-method :post :uri "/admin/connections/c1/controls"
                                :params {:operation "rescan" :since-days "2" :redirect-url "/admin/connections/c1"}
                                :session {:authenticated true}})
      (is (= ["c1" "INBOX" {:move? false :since-days 2}] @started)
          "No configured folder means INBOX, and an unticked box means no move"))))

(deftest parse-control-passes-the-since-days-field-on
  (server/empty-global-messages)
  (let [started (atom nil)]
    (with-redefs [server/start-folder-parse! (fn [_ id folder options] (reset! started [id folder options]) 10)]
      ((server/make-routes {}) {:request-method :post :uri "/admin/connections/c1/controls"
                                :params {:operation "parse" :folder "Archive" :since-days "14" :batch-size "50" :redirect-url "/admin/connections/c1"}
                                :session {:authenticated true}})
      (is (= ["c1" "Archive" {:move? false :batch-size 50 :since-days 14 :assigned-category nil}] @started))
      (server/empty-global-messages)
      ((server/make-routes {}) {:request-method :post :uri "/admin/connections/c1/controls"
                                :params {:operation "parse" :folder "Archive" :since-days "" :redirect-url "/admin/connections/c1"}
                                :session {:authenticated true}})
      (is (= ["c1" "Archive" {:move? false :batch-size nil :since-days nil :assigned-category nil}] @started)
          "A blank period field keeps the whole-folder behaviour of before")))
  (server/empty-global-messages))
