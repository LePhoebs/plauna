(ns plauna.markup-test
  (:require [clojure.string :as str]
            [clojure.test :refer :all]
            [plauna.client :as client]
            [plauna.i18n :as i18n]
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

(deftest general-settings-keep-the-time-zone-and-intervals
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/preferences-page {:time-zone "Europe/Berlin" :category-folder-scan-interval 300
                                         :client-health-check-interval 60 :log-level :info
                                         :log-level-options [{:key :info :name "Info"}]})]
      (is (str/includes? html "name=\"time-zone\" value=\"Europe/Berlin\""))
      (is (str/includes? html "name=\"category-folder-scan-interval\" type=\"number\" min=\"0\" value=\"300\""))
      (is (not (str/includes? html "categorization-threshold")) "Thresholds moved to the Model page")
      (is (str/includes? html "href=\"/model\"")))))

(deftest model-page-holds-languages-thresholds-schedule-and-the-model-switch
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/model-page {:training-time "03:30" :model-type "naive-bayes"
                                   :model-options [{:id "naive-bayes" :name "Naive Bayes"} {:id "maxent" :name "Maximum Entropy (MaxEnt)"}]
                                   :language-preferences [{:id 1 :language "deu" :use_in_training true} {:id 2 :language "eng" :use_in_training false}]
                                   :categorization-threshold 0.65 :language-detection-threshold 0.8
                                   :model-scores {}})]
      (is (str/includes? html "name=\"automatic-training-time\" type=\"time\" value=\"03:30\""))
      (is (str/includes? html "id=\"language-deu\" checked"))
      (is (not (str/includes? html "id=\"language-eng\" checked")))
      (is (str/includes? html "name=\"categorization-threshold\" value=\"0.65\""))
      (is (str/includes? html "action=\"/admin/preferences/model\""))
      (is (str/includes? html "value=\"naive-bayes\" selected"))
      (is (str/includes? html "Switch the categorization model?"))
      (is (str/includes? html "Enter “Start training” to confirm:"))
      (is (str/includes? html "confirmationInput.value !== 'Start training'"))
      (is (str/includes? html "action=\"/metadata/languages\"")))))

(deftest disconnected-banner-is-hidden-when-no-connections-are-disconnected
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/administration {:repl {:status false}})]
      (is (false? (str/includes? html "Not connected:")))
      (is (str/includes? html "No connection problems")))))

(deftest disconnected-banner-shows-disconnected-accounts
  (with-redefs [client/disconnected-connections (fn [] [{:user "me" :host "imap.example.com"}])]
    (let [html (markup/administration {:repl {:status false}})]
      (is (str/includes? html "Not connected: me@imap.example.com."))
      (is (str/includes? html "Accounts not connected: 1")))))

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

(deftest email-list-keeps-an-active-suggestion-filter
  ;; The suggestion filter is set on a category's page; the list only shows it as a chip, keeps it
  ;; when other filters change and preselects the category for assigning.
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [email {:header {:message-id "s-m" :subject "Hello" :date 0} :metadata {:category-id nil} :participants []}
          html (markup/list-emails [email]
                                   {:filter "all" :size 20 :page 2 :total-pages 3
                                    :suggest-category 3 :suggest-by "model" :suggest-min 40
                                    :any-filter-active? true :suggestion-active? true
                                    :suggestion-filter-label "Suggested for Temporary (model ≥ 40 %)"}
                                   {:categories [{:id 3 :name "Temporary"} {:id nil :name "n/a"}] :category-filter-options []
                                    :subjects [] :senders [] :recipients []})]
      (is (not (str/includes? html "Suggest for Category")) "No suggestion controls on the list any more")
      (is (not (str/includes? html "Data Training")) "Training moved to the Model page")
      (is (str/includes? html "name=\"suggest-category\" id=\"suggest-category-input\" value=\"3\""))
      (is (str/includes? html "<input type=\"hidden\" name=\"suggest-by\" value=\"model\" />") "Pagination keeps the filter")
      (is (str/includes? html "Suggested for Temporary (model ≥ 40 %)"))
      (is (str/includes? html "href=\"/categories/3?by=model&amp;min=40\""))
      (is (re-find #"<option value=\"3\"\s*selected\s*>Temporary</option>" html) "The bulk bar preselects the category"))))

(deftest category-page-shows-suggestion-counts-and-the-review-link
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [render (fn [method]
                   (markup/category-page {:category {:id 3 :name "Temporary" :color "#d97706" :destination_folder "INBOX"
                                                     :retention_days 30 :flag_hex "#ff9500" :flag_label "Orange"}
                                          :email-count 214 :method method :min-percent 60
                                          :counts {:sender 37 :domain 85 :model 0} :selected-count (if (= method "model") 0 37)
                                          :review-url "/emails?size=50&suggest-category=3&suggest-by=sender&suggest-min=60"
                                          :model-scores {:at-text "07.11.2026 14:03" :emails 120 :stale? true :running? false}}))
          sender (render "sender")
          model (render "model")]
      (is (str/includes? sender "E-mails: 214"))
      (is (str/includes? sender "Folder: INBOX"))
      (is (str/includes? sender "To the trash after 30 days"))
      (is (str/includes? sender "aria-checked=\"true\""))
      (is (str/includes? sender "This method finds no e-mails outside the category.") "No groups were passed")
      (is (not (str/includes? sender "Score e-mails now")) "Scoring only shows for the model method")
      (is (str/includes? model "name=\"min\" min=\"1\" max=\"99\" value=\"60\""))
      (is (str/includes? model "Scores from 07.11.2026 14:03 (120 e-mails). The model was trained since"))
      (is (str/includes? model "The model suggests no e-mails at this probability."))
      (is (str/includes? model "menu-active") "The category is highlighted in the sidebar"))))

(deftest model-page-offers-training-and-scoring
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [idle (markup/model-page {:training-running? false :last-training "07.11.2026 02:00" :training-time "02:00"
                                   :model-type "naive-bayes" :model-scores {:running? false}})
          busy (markup/model-page {:training-running? true :model-scores {:running? true}})]
      (is (str/includes? idle "07.11.2026 02:00"))
      (is (str/includes? idle "action=\"/training\""))
      (is (str/includes? idle "No e-mails have been scored yet."))
      (is (str/includes? idle "action=\"/model/score\""))
      (is (str/includes? busy "href=\"/training/progress?back=/model\""))
      (is (str/includes? busy "data-running=\"true\""))
      (is (str/includes? busy "Scoring the e-mails…")))))

(deftest shell-is-bilingual
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [english (markup/model-page {:model-scores {}})
          german (binding [i18n/*lang* :de] (markup/model-page {:model-scores {}}))]
      (is (str/includes? english "<html lang=\"en\">"))
      (is (str/includes? english ">Categories</a>"))
      (is (str/includes? german "<html lang=\"de\">"))
      (is (str/includes? german ">Kategorien</a>"))
      (is (str/includes? german "Jetzt trainieren")))))

(deftest email-list-offers-a-bulk-category-change-for-selected-e-mails
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [email {:header {:message-id "bulk-m" :subject "Hello" :date 0}
                 :metadata {:category-id 1} :participants []}
          options {:categories [{:id 1 :name "Bills"} {:id nil :name "n/a"}] :category-filter-options []
                   :subjects [] :senders [] :recipients []}
          html (markup/list-emails [email] {:filter "all" :size 20 :page 1 :total-pages 1} options)
          empty-html (markup/list-emails [] {:filter "all" :size 20 :page 1 :total-pages 0} options)]
      (is (str/includes? html "id=\"bulk-category-bar\""))
      (is (str/includes? html "id=\"email-select-all\""))
      (is (= 1 (count (re-seq #"email-select-checkbox\" autocomplete" html)))
          "One checkbox per e-mail row")
      (is (re-find #"<option value=\"1\"\s*>Bills</option>" html))
      (is (re-find #"<option value=\"\"\s*>n/a</option>" html) "n/a clears the category, like the row dropdown")
      (is (not (str/includes? empty-html "id=\"bulk-category-bar\"")) "No bar without e-mails"))))

(deftest email-list-marks-a-cut-off-sender-checklist-for-the-server-search
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [render (fn [truncated?]
                   (markup/list-emails [] {:filter "all" :size 20 :page 1 :total-pages 0}
                                       {:categories [] :category-filter-options [] :subjects [] :recipients []
                                        :senders [{:contact_key "k" :name "Zed" :address "zed@example.com" :checked? true}]
                                        :senders-truncated? truncated?}))
          cut-off (render true)
          complete (render false)]
      (is (str/includes? cut-off "data-column=\"from\""))
      (is (str/includes? cut-off "data-input-name=\"from-keys\" data-master-id=\"from-filter-select-all\" data-truncated=\"true\""))
      (is (str/includes? cut-off "Only the senders of the newest e-mails are listed."))
      (is (str/includes? complete "data-input-name=\"from-keys\" data-master-id=\"from-filter-select-all\" data-truncated=\"false\""))
      (is (not (str/includes? complete "Only the senders of the newest e-mails are listed."))))))

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
      (is (str/includes? html "Text: annual invoice"))
      (is (str/includes? html "Subject: 1 selected"))
      (is (str/includes? html "Select all visible"))
      (is (str/includes? html "<details class=\"dropdown hidden group-data-[filters=on]:inline-block\">")
          "The column filters show only in filter mode")
      (is (str/includes? html "id=\"email-table\" class=\"group rounded-box border border-base-300 bg-base-100\" data-filters=\"off\"")
          "Filter mode starts off while no column filter is active")
      (is (str/includes? html "id=\"filter-toggle\""))
      (is (str/includes? html "w-96 max-w-[90vw] min-w-0 overflow-hidden"))
      (is (str/includes? html "id=\"subject-checklist\" class=\"max-h-56 overflow-y-auto overflow-x-hidden flex flex-col gap-1 w-full max-w-full min-w-0\""))
      (is (str/includes? html "grid grid-cols-3 gap-1 mt-2 w-full max-w-full min-w-0"))
      (is (str/includes? html "id=\"missing-email-category-dialog\" role=\"dialog\""))
      (is (str/includes? html "The e-mail was not found on the mail server. Change its category anyway?"))
      (is (str/includes? html "answerMissingEmailCategory(false)\">No"))
      (is (str/includes? html "answerMissingEmailCategory(true)\">Yes"))
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
      (is (str/includes? html "Recent folder parse runs"))
      (is (str/includes? html "href=\"/emails?batch=run-1\""))
      (is (not (str/includes? html "href=\"/emails?batch=run-2\"")) "A run without new e-mails offers no review link")
      (is (str/includes? html "var running = true;") "The page polls and reloads while a run is in progress")
      (is (not (str/includes? html "http-equiv=\"refresh\"")) "No blind meta refresh that would discard form input")))
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/connection {:id "connection-1" :host "h" :user "u" :parse-batches [] :parse-running? false} [] [])]
      (is (not (str/includes? html "Recent folder parse runs")))
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
      (is (str/includes? html "Messages that could not be read"))
      (is (str/includes? html "Retry 2 message(s)"))
      (is (str/includes? html "action=\"/admin/connections/connection-1/failures/retry\""))
      (is (str/includes? html "action=\"/admin/connections/connection-1/failures/7/dismiss\""))
      (is (str/includes? html "Failed to load IMAP envelope"))))
  (with-redefs [client/disconnected-connections (fn [] [])]
    (is (not (str/includes? (markup/connection {:id "c" :host "h" :user "u" :parse-failures []} [] []) "Messages that could not be read")))))

(deftest connection-page-lists-folders-with-movable-emails
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/connection
                {:id "c1" :host "h" :user "u"
                 :folder-emails [{:folder "Newsletter_Alt" :categorized 640 :uncategorized 60 :move-url "/admin/connections/c1/folders/move" :move nil}
                                 {:folder "INBOX" :categorized 0 :uncategorized 3 :move-url "/admin/connections/c1/folders/move" :move nil}]}
                [] [])]
      (is (str/includes? html "E-mails still in their original folders"))
      (is (str/includes? html "Move 640 to category folders"))
      (is (= 1 (count (re-seq #"to category folders</button>" html))) "A folder without categorized e-mails has no move button"))))

(deftest statistics-merge-values-that-share-a-label
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/statistics-overall [] [] [{:language nil :count 2} {:language "n/a" :count 3} {:language "eng" :count 1}] [])]
      (is (= 1 (count (re-seq #"Not detected" html))) "NULL and n/a languages become one bar")
      (is (str/includes? html "\"count\":5"))))
  "A chart never shows the same label twice")

(deftest email-page-shows-the-model-scores
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [email {:header {:message-id "m" :subject "Hello" :date 0}
                 :metadata {:category-id nil}
                 :participants [] :body []}
          categories [{:id 1 :name "Bills"} {:id nil :name "n/a"}]
          html (markup/list-email-contents email categories [{:id 1 :name "Bills" :color "#112233" :confidence 0.42}
                                                             {:id 2 :name "Friends" :color nil :confidence 0.3}])]
      (is (str/includes? html "Model scores"))
      (is (str/includes? html "Bills"))
      (is (str/includes? html "42.00 %"))
      (is (str/includes? html "30.00 %")))
    (testing "without a prediction the page says why"
      (let [html (markup/list-email-contents {:header {:message-id "m" :date 0} :metadata {} :participants [] :body []}
                                             [{:id nil :name "n/a"}] nil)]
        (is (str/includes? html "No prediction"))))
    (testing "a categorized e-mail shows the model scores too, its own category in bold"
      (let [html (markup/list-email-contents {:header {:message-id "m" :date 0} :metadata {:category-id 2} :participants [] :body []}
                                             [{:id 1 :name "Bills"} {:id 2 :name "Friends"}]
                                             [{:id 1 :name "Bills" :color nil :confidence 0.6}
                                              {:id 2 :name "Friends" :color nil :confidence 0.4}])]
        (is (str/includes? html "Model scores"))
        (is (str/includes? html "60.00 %"))
        (is (re-find #"font-semibold\"><span[^>]*></span><span>Friends</span>" html))
        (is (not (re-find #"font-semibold\"><span[^>]*></span><span>Bills</span>" html)))
        (is (str/includes? html "The assigned category is in bold"))))
    (testing "an uncategorized e-mail gets no hint about a bold category"
      (let [html (markup/list-email-contents {:header {:message-id "m" :date 0} :metadata {:category-id nil} :participants [] :body []}
                                             [{:id nil :name "n/a"}] [{:id 1 :name "Bills" :color nil :confidence 0.4}])]
        (is (not (str/includes? html "font-semibold\"><span")))
        (is (not (str/includes? html "The assigned category is in bold")))))))

(deftest categories-overview-lists-the-categories-and-a-new-category-form
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/categories-page [{:id 1 :name "parcels" :color "#112233" :destination_folder "INBOX" :flag_color "orange" :retention_days 30 :email_count 12}
                                        {:id 2 :name "newsletter" :color nil :destination_folder nil :flag_color nil :retention_days nil :email_count 0}])]
      (is (str/includes? html "href=\"/categories/1?tab=settings\""))
      (is (str/includes? html "E-mails: 12"))
      (is (str/includes? html "Folder: INBOX"))
      (is (str/includes? html "action=\"/admin/categories\""))
      (is (str/includes? html "action=\"/admin/categories/clean-up\"")))))

(deftest category-settings-offer-flag-colors-retention-and-delete
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [render (fn [category] (markup/category-page {:tab "settings" :email-count 3 :category category
                                                        :flag-colors [{:id "orange" :label "Orange" :hex "#ff9500"}]}))
          flagged (render {:id 1 :name "parcels" :color "#112233" :destination_folder "INBOX" :flag_color "orange" :retention_days 30})
          plain (render {:id 2 :name "newsletter" :color nil :destination_folder nil :flag_color nil :retention_days nil})]
      (is (str/includes? flagged "name=\"flag-color\""))
      (is (str/includes? flagged "value=\"orange\" selected"))
      (is (str/includes? flagged "name=\"retention-days\" value=\"30\""))
      (is (str/includes? flagged "action=\"/admin/categories/1/flag\"") "A flagged category offers to flag its existing e-mails")
      (is (not (str/includes? plain "action=\"/admin/categories/2/flag\"")))
      (is (str/includes? plain "fetch('/admin/categories/2', {method: 'DELETE'})")))))

(deftest category-suggestions-are-grouped-by-sender
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [html (markup/category-page {:category {:id 3 :name "Temporary"} :email-count 1 :method "model" :min-percent 60
                                      :counts {:sender 2 :domain 2 :model 2} :selected-count 2
                                      :groups [{:key "news@shop.example" :name "Shop" :count 2 :min-confidence 0.7 :max-confidence 0.95
                                                :emails [{:message-id "a" :subject "Your code" :date 0 :confidence 0.95}
                                                         {:message-id "b" :subject "Shipped" :date 0 :confidence 0.7}]}]
                                      :model-scores {}})]
      (is (str/includes? html "Suggested e-mails: 2 from 1 senders."))
      (is (str/includes? html "<b>Shop</b>"))
      (is (str/includes? html "70 %–95 %"))
      (is (str/includes? html "Assign all 2"))
      (is (str/includes? html "data-message-id=\"a\""))
      (is (str/includes? html "95 % ·"))
      (is (str/includes? html "/js/assign.js")))))

(deftest list-dates-are-short-for-recent-e-mails
  (let [today (java.time.LocalDate/of 2026 11 7)]
    (is (= "09:14" (markup/list-date (java.time.LocalDateTime/of 2026 11 7 9 14) today)))
    (is (= "03.11." (markup/list-date (java.time.LocalDateTime/of 2026 11 3 18 0) today)))
    (is (= "24.12.2025" (markup/list-date (java.time.LocalDateTime/of 2025 12 24 8 0) today)))
    (is (nil? (markup/list-date nil today)))))

(deftest email-list-rows-show-sender-category-and-model-probability
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [email (fn [id category confidence]
                  {:header {:message-id id :subject (str "Subject " id) :date 0}
                   :metadata {:category-id category :category-confidence confidence}
                   :participants [{:type :sender :name nil :address "bounce@mailer.example"}
                                  {:type :from :name "Paketdienst" :address "versand@paket.example"}]})
          html (markup/list-emails [(email "by-model" 3 0.91) (email "confirmed" 3 1.0) (email "none" nil nil)]
                                   {:filter "model-assigned" :size 20 :page 1 :total-pages 1 :total 3 :first-index 1 :last-index 3}
                                   {:categories [{:id 3 :name "Temporary"} {:id nil :name "n/a"}] :category-filter-options []
                                    :subjects [] :senders [] :recipients []})]
      (is (str/includes? html ">Paketdienst</a>") "The From address is shown, not the envelope sender")
      (is (str/includes? html "versand@paket.example"))
      (is (= 1 (count (re-seq #">91 %</span>" html))) "Only the unconfirmed model assignment shows its probability")
      (is (= 1 (count (re-seq #"category-select border-dashed" html))))
      (is (str/includes? html "1–3 of 3"))
      (is (re-find #"data-view=\"model-assigned\" aria-pressed=\"true\"" html) "The active view is marked")
      (is (not (str/includes? html "updateLanguage(")) "The language column moved to the detail page"))))

(deftest new-pages-show-pending-messages
  ;; result-with-messages passes pending toasts as an extra argument; a page without that arity
  ;; failed with an ArityException whenever a message was waiting.
  (with-redefs [client/disconnected-connections (fn [] [])]
    (let [message {:type :success :key :category.saved}]
      (is (str/includes? (markup/model-page {:model-scores {}} [message]) "The category was saved."))
      (is (str/includes? (markup/category-page {:category {:id 1 :name "X"} :tab "emails" :emails []} [message]) "The category was saved.")))))

(deftest account-update-sends-the-account-form-not-the-first-form-of-the-page
  ;; The sidebar's language switch is the first form of every page since the new shell.
  (let [template (slurp "resources/templates/admin-connection.html")]
    (is (str/includes? template "const form = document.getElementById('update');"))
    (is (not (str/includes? template "document.querySelector('form')")))))
