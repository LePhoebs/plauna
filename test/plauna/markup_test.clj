(ns plauna.markup-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [plauna.client :as client]
            [plauna.markup :as markup]
            [plauna.preferences :as preferences]))

(deftest timestamps-are-rendered-in-the-configured-time-zone
  (with-redefs [preferences/zone-id (fn [] (java.time.ZoneId/of "Europe/Berlin"))]
    (is (= (java.time.LocalDateTime/of 1970 1 1 1 0)
           (markup/timestamp->date 0)))))

(deftest confidences-are-rendered-as-percentages
  (is (= "100.00 %" (markup/confidence->percent 1.0)))
  (is (= "87.65 %" (markup/confidence->percent 0.87654)))
  (is (= "0.00 %" (markup/confidence->percent 0)))
  (is (= "—" (markup/confidence->percent nil))))

(deftest preferences-page-renders-the-daily-training-time-and-time-zone
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/preferences-page {:automatic-training-time "03:30"
                                         :time-zone "Europe/Berlin"
                                         :categorization-model "naive-bayes"
                                         :categorization-model-options [{:id "naive-bayes" :name "Naive Bayes"}
                                                                        {:id "maxent" :name "Maximum Entropy (MaxEnt)"}]
                                         :log-level-options []})]
      (is (str/includes? html "name=\"automatic-training-time\" type=\"time\" value=\"03:30\""))
      (is (str/includes? html "name=\"time-zone\" value=\"Europe/Berlin\""))
      (is (str/includes? html "action=\"/admin/preferences/model\""))
      (is (str/includes? html "value=\"naive-bayes\" selected"))
      (is (str/includes? html "value=\"maxent\""))
      (is (str/includes? html "Switch categorization model?"))
      (is (str/includes? html "Use current category assignments for training"))
      (is (str/includes? html "Start training"))
      (is (str/includes? html "class=\"label-text block\" for=\"model-switch-confirmation\""))
      (is (str/includes? html "class=\"input input-bordered mt-1 w-full\""))
      (is (str/includes? html "class=\"modal-action flex-wrap\""))
      (is (not (str/includes? html "class=\"form-control mt-4\"")))
      (is (str/includes? html "confirmationInput.value !== 'Start training'")))))

(deftest disconnected-banner-is-hidden-when-no-connections-are-disconnected
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/administration {:repl {:status false}})]
      (is (false? (str/includes? html "IMAP connection not active:"))))))

(deftest disconnected-banner-shows-disconnected-accounts
  (with-redefs [client/disconnected-connections (fn [] [{:user "me" :host "imap.example.com"}])]
    (let [html (markup/administration {:repl {:status false}})]
      (is (str/includes? html "IMAP connection not active:"))
      (is (str/includes? html "me@imap.example.com")))))

(deftest connection-pages-never-render-stored-secrets-or-statistics-scripts
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [stored-imap-secret "imap-secret-must-not-reach-browser"
          stored-client-secret "oauth-secret-must-not-reach-browser"
          html (markup/connection
                {:id "connection-1"
                 :host "imap.example.com"
                 :user "me@example.com"
                 :secret stored-imap-secret
                 :auth-providers [{:id 1
                                   :name "Example"
                                   :client-secret stored-client-secret}]}
                []
                [])]
      (is (not (str/includes? html stored-imap-secret)))
      (is (not (str/includes? html stored-client-secret)))
      (is (str/includes? html "Leave blank to keep the current secret"))
      (is (not (str/includes? html "cdn.jsdelivr.net")))
      (is (not (str/includes? html "/js/vendor/vega.min.js")))))
  "Admin pages contain blank password fields and do not load chart code")

(deftest connection-page-offers-the-one-click-rescan-of-recent-emails
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [connection {:id "connection-1" :host "imap.example.com" :user "me@example.com" :folder ""
                      :monitored-folder "INBOX" :connected true}
          html (markup/connection connection ["INBOX" "Spam"] [{:id 1 :name "Work"}])
          form (fn [id page] (re-find (re-pattern (str "(?s)<form id=\"" id "\".*?</form>")) page))
          rescan (form "rescan" html)
          parse (form "parse" html)]
      (is (some? rescan) "A connected account offers the re-scan")
      (is (str/includes? rescan "action=\"/admin/connections/connection-1/controls\""))
      (is (str/includes? rescan "name=\"operation\" value=\"rescan\""))
      (is (re-find #"(?s)<input[^>]*name=\"since-days\"[^>]*value=\"7\"[^>]*required" rescan)
          "The days field is required and defaults to a week")
      (is (re-find #"(?s)<input[^>]*name=\"move\"[^>]*checked" rescan)
          "Rescued e-mails are moved after categorization by default, like live mail")
      (is (not (str/includes? rescan "name=\"folder\"")) "The server decides the folder: it is always the watched one")
      (is (str/includes? html "<strong>INBOX</strong>") "The page names the folder that is re-scanned")
      (is (re-find #"(?s)<input[^>]*name=\"since-days\"(?![^>]*required)" parse)
          "The general parse form has an optional period field")
      (is (let [ids (map second (re-seq #"id=\"([^\"]+)\"" html))] (= (count ids) (count (set ids))))
          "No duplicate element ids on the page")
      (is (nil? (form "rescan" (markup/connection (assoc connection :connected false) [] [])))
          "Without a live connection there is nothing to re-scan")))
  "The connection page has a one-click re-scan of the last N days next to the general folder parse")

(deftest mtls-page-shows-fingerprints-but-never-renders-the-proxy-secret
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [secret "proxy-secret-must-not-reach-browser"
          current-fingerprint (apply str (repeat 32 "cd"))
          html (markup/mtls-page {:enabled true
                                  :environment-managed false
                                  :secret-configured true
                                  :trusted-cert-sha256 "aabbcc"
                                  :current-certificate {:fingerprint current-fingerprint
                                                        :trusted false
                                                        :can-add true}
                                  :proxy-secret secret
                                  :current-password secret})]
      (is (str/includes? html "name=\"trusted-cert-sha256\""))
      (is (str/includes? html "aabbcc"))
      (is (str/includes? html "Leave blank to keep the current secret"))
      (is (str/includes? html "Delete the stored proxy secret"))
      (is (str/includes? html "name=\"current-password\""))
      (is (str/includes? html "required"))
      (is (str/includes? html current-fingerprint))
      (is (str/includes? html "name=\"add-current-certificate\""))
      (is (not (str/includes? html "name=\"fingerprint\"")))
      (is (not (str/includes? html secret)))))
  "The administration form treats the secret as write-only and derives current certificate identity server-side")

(deftest login-page-requires-the-configured-login-name-and-has-no-certificate-enrollment
  (let [fingerprint (apply str (repeat 32 "ab"))
        html (markup/login-page {:login-name "root" :mtls-candidate {:fingerprint fingerprint}})]
    (is (str/includes? html "name=\"login-name\""))
    (is (not (str/includes? html "value=\"root\""))
        "The configured login name is one of the two secrets asked for and is never pre-filled for anonymous visitors")
    (is (str/includes? html "autocomplete=\"username\""))
    (is (str/includes? html "name=\"password\""))
    (is (not (str/includes? html "add-mtls-certificate")))
    (is (not (str/includes? html fingerprint))))
  "Certificate enrollment is absent from login and the configured login name is required")

(deftest login-settings-page-allows-changing-the-login-name
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/password-page {:login-name "root" :env-managed false})]
      (is (str/includes? html "action=\"/admin/login-name\""))
      (is (str/includes? html "name=\"login-name\""))
      (is (str/includes? html "value=\"root\""))
      (is (str/includes? html "Save login name"))
      (is (str/includes? html "action=\"/admin/password\""))))
  "Login name and password are managed together on the login settings page")

(deftest statistics-page-loads-only-local-chart-scripts
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/statistics-overall [] [] [] [])]
      (is (str/includes? html "/js/vendor/vega.min.js"))
      (is (str/includes? html "/js/vendor/vega-lite.min.js"))
      (is (str/includes? html "/js/vendor/vega-embed.min.js"))
      (is (not (str/includes? html "cdn.jsdelivr.net")))
      (is (str/includes? html "Total e-mails"))
      (is (str/includes? html "No data available for this chart.")))))

(deftest statistics-page-renders-responsive-direct-vega-lite-specifications
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/statistics-overall
                [{:time-bucket 2024 :count 5}]
                [{:mime-type "text/plain" :count 4} {:mime-type nil :count 1}]
                [{:language "en" :count 3} {:language nil :count 2}]
                [{:name "Work" :count 4} {:name nil :count 1}])]
      (is (str/includes? html ">5</strong>"))
      (is (str/includes? html ">80.0 %</strong>"))
      (is (str/includes? html "\"$schema\""))
      (is (str/includes? html "vega-lite\\/v6.json"))
      (is (str/includes? html "\"width\":\"container\""))
      (is (str/includes? html "\"time-bucket\":\"2024\""))
      (is (str/includes? html "Uncategorized"))
      (is (str/includes? html "Not detected"))
      (is (str/includes? html "Unknown"))
      (is (str/includes? html "{actions: false, renderer: 'svg'}"))
      (is (not (str/includes? html "\"format\":{\"type\":\"csv\"")))))
  "Charts use small JSON value arrays directly and size themselves to their cards")

(deftest email-list-renders-explicit-column-filter-workflow-and-active-chips
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/list-emails
                []
                {:filter "all" :size 20 :page 1 :total-pages 0
                 :any-filter-active? true
                 :search-active? true :search-text "annual invoice"
                 :subject-filter-active? true :subject-filter-label "Subject: 1 selected"
                 :subject-filter-badge "1"}
                {:categories [] :category-filter-options []
                 :subjects [{:subject "Invoice" :checked? true}
                            {:subject "Newsletter" :checked? false}]
                 :senders [] :recipients []})]
      (is (str/includes? html "Active filters:"))
      (is (str/includes? html "Body: annual invoice"))
      (is (str/includes? html "Subject: 1 selected"))
      (is (str/includes? html "Select all visible"))
      (is (str/includes? html "<details class=\"dropdown\">"))
      (is (str/includes? html "<summary class=\"inline-flex"))
      (is (str/includes? html "w-96 max-w-[90vw] min-w-0 overflow-hidden"))
      (is (str/includes? html "id=\"subject-checklist\" class=\"max-h-56 overflow-y-auto overflow-x-hidden flex flex-col gap-1 w-full max-w-full min-w-0\""))
      (is (str/includes? html "grid grid-cols-3 gap-1 mt-2 w-full max-w-full min-w-0"))
      (is (str/includes? html "id=\"missing-email-category-dialog\" role=\"dialog\""))
      (is (str/includes? html "E-Mail konnte nicht gefunden werden. Trotzdem Kategorie ändern?"))
      (is (str/includes? html "answerMissingEmailCategory(false)\">Nein"))
      (is (str/includes? html "answerMissingEmailCategory(true)\">Ja"))
      (is (str/includes? html "fields.force = 'true'"))
      (is (str/includes? html ">Apply</button>"))
      (is (str/includes? html ">Reset</button>"))
      (is (str/includes? html ">Cancel</button>"))
      (is (str/includes? html "data-applied-checked=\"true\""))
      (is (str/includes? html "data-applied-checked=\"false\""))
      (is (not (str/includes? html "onchange=\"document.getElementById('page-form').submit()\"")))))
  "Column-filter edits stay local until Apply and active filters remain visible above the table")

(deftest connection-page-lists-parse-runs-with-review-links
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/connection
                {:id "connection-1" :host "imap.example.com" :user "me@example.com"
                 :parse-batches [{:id "run-1" :folder "Old" :status "finished" :batch-size 100
                                  :processed 100 :skipped 20 :errors 0 :remaining 300
                                  :started-at 1756800000 :finished-at 1756800600
                                  :emails-url "/emails?batch=run-1"}
                                 {:id "run-2" :folder "Old" :status "running" :batch-size 100
                                  :processed 0 :skipped 0 :errors 0 :remaining 0
                                  :started-at 1756801000 :finished-at nil
                                  :emails-url "/emails?batch=run-2"}]
                 :parse-running? true}
                []
                [])]
      (is (str/includes? html "Recent Folder Parse Runs"))
      (is (str/includes? html "href=\"/emails?batch=run-1\""))
      (is (not (str/includes? html "href=\"/emails?batch=run-2\"")) "A run without new e-mails offers no review link")
      (is (str/includes? html "var running = true;") "The page polls and reloads while a run is in progress")
      (is (not (str/includes? html "http-equiv=\"refresh\"")) "No blind meta refresh that would discard form input")))
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/connection {:id "connection-1" :host "h" :user "u" :parse-batches [] :parse-running? false} [] [])]
      (is (not (str/includes? html "Recent Folder Parse Runs")))
      (is (str/includes? html "var running = false;")))))

(deftest training-progress-page-polls-the-status-endpoint
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/training-progress-page "/admin/preferences")]
      (is (str/includes? html "/training/status"))
      (is (str/includes? html "href=\"/admin/preferences\""))
      (is (str/includes? html "<progress"))
      (is (str/includes? html "id=\"training-steps\"") "The step checklist has its container"))))

(deftest connection-page-offers-to-move-a-finished-batch
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/connection
                {:id "connection-1" :host "h" :user "u"
                 :parse-batches [{:id "run-1" :folder "Old" :status "finished" :batch-size 100
                                  :processed 100 :skipped 0 :errors 0 :remaining 0
                                  :started-at 1756800000 :finished-at 1756800600
                                  :emails-url "/emails?batch=run-1" :move-url "/parse-batches/run-1/move"
                                  :move {:status "running" :moved 12 :total 90}}]
                 :parse-running? true}
                [] [])]
      (is (str/includes? html "moving 12 / 90"))
      (is (not (str/includes? html "action=\"/parse-batches/run-1/move\"")) "No second move can be started while one runs"))
    (let [html (markup/connection
                {:id "connection-1" :host "h" :user "u"
                 :parse-batches [{:id "run-1" :folder "Old" :status "finished" :batch-size 100
                                  :processed 100 :skipped 0 :errors 0 :remaining 0
                                  :started-at 1756800000 :finished-at 1756800600
                                  :emails-url "/emails?batch=run-1" :move-url "/parse-batches/run-1/move"}]
                 :parse-running? false}
                [] [])]
      (is (str/includes? html "action=\"/parse-batches/run-1/move\"")))))

(deftest connection-page-lists-read-failures-with-a-retry-form
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/connection
                {:id "connection-1" :host "h" :user "u"
                 :parse-failures [{:folder "INBOX" :count 2 :retry-url "/admin/connections/connection-1/failures/retry"
                                   :failures [{:id 7 :uid 2327 :message-number 2327 :message-id nil :subject "Broken" :error "Failed to load IMAP envelope" :attempts 3 :last-seen 1756800000}
                                              {:id 8 :uid nil :message-number 12 :message-id "<x@y>" :subject nil :error "boom" :attempts 1 :last-seen nil}]}]}
                [] [])]
      (is (str/includes? html "Messages That Could Not Be Read"))
      (is (str/includes? html "Retry 2 message(s)"))
      (is (str/includes? html "action=\"/admin/connections/connection-1/failures/retry\""))
      (is (str/includes? html "action=\"/admin/connections/connection-1/failures/7/dismiss\""))
      (is (str/includes? html "Failed to load IMAP envelope"))))
  (with-redefs [client/disconnected-connections (fn [] [])]
    (is (not (str/includes? (markup/connection {:id "c" :host "h" :user "u" :parse-failures []} [] []) "Messages That Could Not Be Read")))))

(deftest connection-page-lists-folders-with-movable-emails
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/connection
                {:id "c1" :host "h" :user "u"
                 :folder-emails [{:folder "Newsletter_Alt" :categorized 640 :uncategorized 60 :move-url "/admin/connections/c1/folders/move" :move nil}
                                 {:folder "INBOX" :categorized 0 :uncategorized 3 :move-url "/admin/connections/c1/folders/move" :move nil}]}
                [] [])]
      (is (str/includes? html "E-mails Still in Their Original Folders"))
      (is (str/includes? html "Move 640 to category folders"))
      (is (= 1 (count (re-seq #"to category folders</button>" html))) "A folder without categorized e-mails has no move button"))))

(deftest statistics-merge-values-that-share-a-label
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/statistics-overall [] [] [{:language nil :count 2} {:language "n/a" :count 3} {:language "eng" :count 1}] [])]
      (is (= 1 (count (re-seq #"Not detected" html))) "NULL and n/a languages become one bar")
      (is (str/includes? html "\"count\":5"))))
  "A chart never shows the same label twice")
