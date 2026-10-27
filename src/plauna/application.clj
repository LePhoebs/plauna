(ns plauna.application
  (:require [plauna.interfaces :as int]
            [taoensso.telemere :as t]
            [clojure.core.async :as async]
            [clojure.string :as str]
            [plauna.core.email :as core-email]
            [plauna.preferences :as preferences]
            [plauna.util.page :as page]
            [plauna.util.sql :as sql]))

(def ^:private like-contains
  "See plauna.util.sql/like-contains: the same escaping the database layer uses for its own LIKE scans."
  sql/like-contains)

(def ^:private default-fulltext-min-token-length 3)

(def ^:private innodb-default-stopwords
  "The built-in InnoDB stopword list. Falling back to LIKE for these words preserves the literal
   search semantics even though MATCH cannot return them. A server with stopwords disabled may do
   a little extra work for these uncommon searches, but never returns an incorrect empty result."
  #{"a" "about" "an" "are" "as" "at" "be" "by" "com" "de" "en" "for" "from"
    "how" "i" "in" "is" "it" "la" "of" "on" "or" "that" "the" "this" "to" "was"
    "what" "when" "where" "who" "will" "with" "und" "www"})

(defn- search-units
  "Parse a user query into quoted phrases and unquoted word tokens. Boolean-mode operators are not
   passed through: search input is always treated as text, never as MariaDB query syntax."
  [search-text]
  (mapcat (fn [[_ phrase unquoted]]
            (let [text (or phrase unquoted)
                  words (re-seq #"[\p{L}\p{N}_]+" text)]
              (if phrase
                (when (seq words) [{:type :phrase :text (str/join " " words) :words words}])
                (map (fn [word] {:type :word :text word :words [word]}) words))))
          (re-seq #"\"([^\"]+)\"|(\S+)" search-text)))

(defn- fulltext-search-plan
  "Translate plain user input into a safe MariaDB boolean query plus LIKE fallbacks. Unquoted words
   are required prefix matches; quoted text is a required phrase. Phrases containing a short token
   and individual short words use LIKE because InnoDB does not index them by default."
  [search-text min-token-length]
  (reduce (fn [{:keys [boolean-parts] :as plan} {:keys [type text words]}]
            (if (every? #(and (>= (count %) min-token-length)
                              (not (contains? innodb-default-stopwords (str/lower-case %))))
                        words)
              (update plan :boolean-parts conj
                      (if (= :phrase type) (str "+\"" text "\"") (str "+" text "*")))
              (update plan :like-terms conj text)))
          {:boolean-parts [] :like-terms []}
          (search-units search-text)))

(defn- maria-fulltext-condition
  [search-text min-token-length]
  (let [{:keys [boolean-parts like-terms]} (fulltext-search-plan search-text min-token-length)
        conditions (concat
                    (when (seq boolean-parts)
                      [[:raw ["MATCH(bodies.content) AGAINST ("
                               [:lift (str/join " " boolean-parts)]
                               " IN BOOLEAN MODE)"]]])
                    (map #(like-contains :bodies.content %) like-terms))]
    (cond
      ;; Input containing only punctuation has no FULLTEXT token. Keep literal substring semantics
      ;; for that edge case instead of accidentally turning a non-blank search into no condition.
      (empty? conditions) (like-contains :bodies.content search-text)
      (= 1 (count conditions)) (first conditions)
      :else (into [:and] conditions))))

(defn- filter->where [filter]
  (cond
    (= filter "enriched-only") [:and [:<> :metadata.category nil] [:<> :metadata.language nil]]
    (= filter "without-category") [:= :metadata.category nil]
    :else nil))

(defn- related-message->where
  "Build a semi-join from headers to a related table. With an active date filter, use a correlated
   EXISTS so the optimizer can start with the indexed headers.date range and only probe related rows
   for those headers. Without a date filter, retain the non-correlated IN form, which resolves the
   related message ids once instead of probing the related table for every header in the mailbox."
  [table message-id-column related-condition date-filter-active?]
  (if date-filter-active?
    [:exists {:select [message-id-column]
              :from [table]
              :where [:and
                      [:= message-id-column :headers.message-id]
                      related-condition]}]
    [:in :headers.message-id
     {:select [message-id-column]
      :from [table]
      :where related-condition}]))

(def max-inlined-search-ids
  "Up to this many body-search matches are inlined as an IN list instead of repeating the LIKE subquery.
   Well below SQLite's default bind-variable limit (32766), and a bigger result set means the search is
   not selective anyway."
  5000)

(defn- resolve-search-ids
  "SQLite only: the message ids whose body contains search-text, resolved ONCE. The e-mail list runs six
   statements per page (four checklist scopes, the page, the count) and every one of them would
   otherwise rescan the whole bodies table with LIKE. nil when unavailable (the caller keeps the subquery)."
  [db db-type search-text]
  (when (= :sqlite db-type)
    (try (int/fetch-content-match-ids db search-text)
         (catch Exception e
           (t/log! {:level :warn :error e} "Could not pre-resolve the body search; using the subquery instead.")
           nil))))

(defn- content->where
  "Build a where-clause matching e-mails whose body content contains search-text. A blank/nil
   search-text adds no filter. When a date filter is active, correlated? selects a date-first EXISTS
   plan; otherwise the matching message ids are resolved once with a non-correlated IN subquery.
   MariaDB uses its FULLTEXT index; SQLite retains the literal substring search and, when the ids were
   resolved up front (resolved-ids, see resolve-search-ids), inlines them."
  [search-text correlated? fulltext-min-token-length resolved-ids]
  (when-not (str/blank? search-text)
    (cond
      fulltext-min-token-length
      (related-message->where :bodies :bodies.message-id
                              (maria-fulltext-condition search-text fulltext-min-token-length)
                              correlated?)

      (and (some? resolved-ids) (empty? resolved-ids))
      [:= 1 0]

      (and (some? resolved-ids) (<= (count resolved-ids) max-inlined-search-ids))
      [:in :headers.message-id (vec resolved-ids)]

      :else
      (related-message->where :bodies :bodies.message-id
                              (like-contains :bodies.content search-text)
                              correlated?))))

(defn- contact-keys->where
  "Build a where-clause for e-mails with a participant of one of participant-types (e.g.
   [\"sender\" \":sender\"] — legacy rows may store the type with a leading colon, see the defensive
   strip in core.email/construct-participants). selection is {:include [...]} to match only those
   contact-keys, or {:exclude [...]} to hide the e-mails that involve one of those contact-keys (the
   checklist UI submits whichever list is shorter — see emails.html's submitFilterForm — so a mailbox
   with hundreds of senders never has to send hundreds of query parameters just to exclude a few).
   Exclusion is a NOT over the excluded participants, not a semi-join over the remaining ones: an
   e-mail with no participant of the type at all (the user was only in Cc, or the sender could not be
   parsed) is shown while the filter is untouched and must not vanish because ONE named sender was
   unchecked - the same rule subject-values->where applies to missing subjects.
   Both empty adds no filter, same as every other filter field's 'blank means unfiltered' convention.
   :none? is the explicit empty selection and therefore matches no rows."
  [participant-types {:keys [include exclude none?]} correlated?]
  (cond
    none? [:= 1 0]

    (seq include)
    (related-message->where :communications :communications.message-id
                            [:and
                             [:in :communications.type participant-types]
                             [:in :communications.contact-key include]]
                            correlated?)

    (seq exclude)
    [:not (related-message->where :communications :communications.message-id
                                  [:and
                                   [:in :communications.type participant-types]
                                   [:in :communications.contact-key exclude]]
                                  correlated?)]

    :else nil))

(defn- sender-keys->where [selection correlated?]
  (contact-keys->where ["sender" ":sender"] selection correlated?))

(defn- recipient-keys->where [selection correlated?]
  (contact-keys->where ["receiver" ":receiver"] selection correlated?))

(defn- subject-values->where
  "selection is {:include [...]} to match only those subjects, or {:exclude [...]} to match every
   subject EXCEPT those (see contact-keys->where for why). Both empty adds no filter.
   Exclusion must keep NULL-subject rows explicitly: NOT IN is UNKNOWN for NULL, so unchecking one
   named subject would otherwise also hide every email with a missing subject, even though missing
   subjects never appear in the checklist."
  [{:keys [include exclude none?]}]
  (cond
    none? [:= 1 0]
    (seq include) [:in :headers.subject include]
    (seq exclude) [:or [:not-in :headers.subject exclude] [:is :headers.subject nil]]
    :else nil))

(defn- date-string->epoch-seconds
  "Convert a 'YYYY-MM-DD' string to a unix timestamp at the start of that day in the configured zone,
   or the start of the following day when next-day? is true (used for an inclusive upper bound)."
  [date-str next-day? zone-id]
  (let [^java.time.LocalDate base (java.time.LocalDate/parse date-str)
        ^java.time.LocalDate day (if next-day? (.plusDays base 1) base)]
    (.toEpochSecond (.atStartOfDay day zone-id))))

(defn- date-bound
  "The epoch-second bound for one date field, or nil when the field is blank OR not a valid ISO date
   (a hand-edited URL or a browser without a native date input): a malformed bound is ignored like an
   empty one instead of failing the whole e-mail list."
  [date-str next-day? zone-id]
  (when-not (str/blank? date-str)
    (try (date-string->epoch-seconds (str/trim date-str) next-day? zone-id)
         (catch java.time.format.DateTimeParseException _
           (t/log! :debug ["Ignoring an unparseable date filter value:" date-str])
           nil))))

(defn- date->where
  "Build a where-clause filtering headers.date (stored as unix seconds) between the given dates.
   Either bound may be blank/nil. The upper bound is inclusive of the whole 'to' day. Uses the fully
   qualified column (rather than relying on database.clj's key-lookup postwalk, which only runs for
   the main e-mail-list query) so this fragment can also be reused directly in the checklist filters'
   scoped distinct-value queries — see other-filters-where."
  [date-from date-to zone-id]
  (let [from (date-bound date-from false zone-id)
        to (date-bound date-to true zone-id)]
    (cond
      (and from to) [:and [:>= :headers.date from] [:< :headers.date to]]
      from [:>= :headers.date from]
      to [:< :headers.date to]
      :else nil)))

(defn- combine-wheres
  "AND together the active where-clauses; nil (no filter at all) when none are active."
  [wheres]
  (let [active (remove nil? wheres)]
    (cond (empty? active) nil
          (= 1 (count active)) (first active)
          :else (into [:and] active))))

(def uncategorized-token
  "Sentinel value for the 'n/a' checkbox in the category filter, distinguishing 'no category was
   selected at all' (an absent/blank category-ids param) from 'the uncategorized bucket was selected'."
  "n-a")

(defn- category-tokens->numeric-ids
  "Parse every token except uncategorized-token as a category id; non-numeric tokens are dropped."
  [tokens]
  (keep (fn [token] (when (not= uncategorized-token token) (try (Integer/parseInt token) (catch NumberFormatException _ nil))))
        tokens))

(defn- category-ids->where
  "Build a where-clause for the category checklist. uncategorized-token stands for 'no category'
   (metadata.category IS NULL).
   - selection's :include matches only those categories.
   - selection's :exclude matches every category EXCEPT those (the checklist UI submits whichever
     list is shorter — see emails.html's submitFilterForm — so this only kicks in once more than
     half the checkboxes are checked, keeping the query string short either way).
   Both empty adds no filter, same as every other filter field's 'blank means unfiltered' convention
   (also how an Excel column filter behaves before you touch it: nothing unchecked yet). :none? is
   the explicit empty selection and therefore matches no rows."
  [{:keys [include exclude none?]}]
  (let [include-uncategorized? (contains? (set include) uncategorized-token)
        include-ids (vec (category-tokens->numeric-ids include))
        exclude-uncategorized? (contains? (set exclude) uncategorized-token)
        exclude-ids (vec (category-tokens->numeric-ids exclude))]
    ;; The branches test the PARSED selection: a list made only of unknown tokens (a hand-crafted URL)
    ;; must never render `IN ()`, which is a syntax error on MariaDB.
    (cond
      none? [:= 1 0]

      ;; Only unknown categories were selected: nothing can match them.
      (and (seq include) (not include-uncategorized?) (empty? include-ids)) [:= 1 0]

      (or include-uncategorized? (seq include-ids))
      (cond
        (and include-uncategorized? (seq include-ids)) [:or [:in :metadata.category include-ids] [:= :metadata.category nil]]
        include-uncategorized? [:= :metadata.category nil]
        :else [:in :metadata.category include-ids])

      (or exclude-uncategorized? (seq exclude-ids))
      (cond
        ;; Excluding "n/a" too: a real, non-excluded category is required.
        (and exclude-uncategorized? (seq exclude-ids)) [:and [:<> :metadata.category nil] [:not-in :metadata.category exclude-ids]]
        exclude-uncategorized? [:<> :metadata.category nil]
        ;; "n/a" is not excluded, so an uncategorized e-mail still passes alongside any non-excluded category.
        :else [:or [:not-in :metadata.category exclude-ids] [:= :metadata.category nil]])

      :else nil)))

(defn- batch->where
  "Restrict the list to the e-mails newly saved by one folder parse run (see read-emails-from-folder's
   :batch-id). Fully qualified column names: the :message-id shorthand is rewritten to headers.message_id
   by the database layer, which would break inside this subquery."
  [batch-id]
  (when-not (str/blank? batch-id)
    [:in :headers.message-id {:select [:parse-batch-emails.message-id]
                              :from [:parse-batch-emails]
                              :where [:= :parse-batch-emails.batch-id batch-id]}]))

(defn- batch-filter-label
  "Chip text for an active batch filter; falls back to the bare id when the run is unknown."
  [db batch-id]
  (let [batch (when-not (str/blank? batch-id) (int/fetch-parse-batch db batch-id))]
    (cond (nil? batch) (str "Batch: " batch-id)
          :else (str "Batch: " (:folder batch) " (" (:processed batch) " new)"))))

(defn- success-result [result-type data] (conj {:result result-type} data))

(defn- error-result [exception alert-content] {:result :error :exception exception :message {:type :alert :content alert-content}})

(def default-category-color
  "Used for the 'n/a' pseudo-category and any category saved before color-coding existed."
  "#9ca3af")

(defn- valid-hex-color?
  "True for a '#' followed by exactly 6 hex digits, the format <input type=\"color\"> always submits.
   Rejects anything else so a hand-crafted request can't smuggle arbitrary text into the color
   attribute rendered on every category select."
  [color]
  (boolean (re-matches #"#[0-9a-fA-F]{6}" (or color ""))))

(defn- clean-color
  "A valid hex color, trimmed; nil for a blank or invalid one so it falls back to default-category-color."
  [color]
  (let [trimmed (when (string? color) (str/trim color))]
    (when (valid-hex-color? trimmed) trimmed)))

(defn categories
  "There is no entry for 'no entry' in the database. This function adds a 'n/a' entry to the actual list."
  [db] (conj (int/fetch-categories db) {:id nil :name "n/a" :color default-category-color}))

(defn connect-to-client
  "Returns {:result :ok}, {:result :redirect :provider provider} in case of oauth2, or
   {:result :error ...} when the connection could not be established."
  [{:keys [db client] :as context} id]
  (try
    ;; start-monitor returns nil when the login fails (the client catches
    ;; AuthenticationFailedException); treat that as an error instead of reporting :ok for a
    ;; connection that never came up.
    (let [connection (int/fetch-connection db id)
          monitor-result (fn [] (if (some? (int/start-monitor client connection context))
                                  (success-result :ok nil)
                                  (error-result nil "Could not connect. Please see the logs for the details.")))]
      (if (= "oauth2" (:auth-type connection))
        (let [auth-provider (int/fetch-auth-provider db (:auth-provider connection))
              oauth-data (int/fetch-oauth-token-data db id)]
          (cond
            (nil? auth-provider) (throw (ex-info "Auth type is 'oauth2' but there is no auth provider." {:connection connection}))
            (or (nil? oauth-data) (nil? (:access-token oauth-data)) (nil? (:refresh-token oauth-data)))
            (do
              (t/log! :warn ["Connection" (:user connection) (:host connection) "is set to use oauth2 but has no tokens in the db. You need to login manually from the 'Connections' page first."])
              (success-result :redirect {:provider (int/fetch-auth-provider db (:auth-provider connection))}))
            :else (monitor-result)))
        (monitor-result)))
    (catch Exception e (do (t/log! :error ["There was an error when trying to log in:" e])
                           (error-result e "There was an error when trying to log in.")))))

(defn- checklist-selection
  "Normalize a checklist filter's raw request parameters into
   {:include [...] :exclude [...] :none? boolean}.
   Exactly one side is ever populated by the UI (see emails.html's submitFilterForm), but both are
   defensively blank-filtered here."
  [parameters include-key exclude-key none-key]
  {:include (remove str/blank? (get parameters include-key))
   :exclude (remove str/blank? (get parameters exclude-key))
   :none? (= "true" (get parameters none-key))})

(defn- selection-filter-active? [{:keys [include exclude none?]}]
  (boolean (or none? (seq include) (seq exclude))))

(defn- selection-filter-label [label {:keys [include exclude none?]}]
  (cond
    none? (str label ": none")
    (seq include) (str label ": " (count include) " selected")
    (seq exclude) (str label ": all except " (count exclude))
    :else nil))

(defn- selection-filter-badge [{:keys [include exclude none?]}]
  (cond none? "0"
        (seq include) (str (count include))
        (seq exclude) (str "−" (count exclude))
        :else nil))

(defn- annotate-checked-by
  "Mark each item's :checked? state to match selection {:include [...] :exclude [...]}:
   - Both empty (the default, unfiltered state — nothing has been unchecked yet): every item checked.
   - :include non-empty: only items whose (key-fn item) appears in it are checked.
   - :exclude non-empty: every item is checked EXCEPT those whose (key-fn item) appears in it.
   - :none? true: every item is unchecked."
  [items {:keys [include exclude none?]} key-fn]
  (cond
    none?
    (mapv (fn [item] (assoc item :checked? false)) items)

    (seq include)
    (let [selected (set include)]
      (mapv (fn [item] (assoc item :checked? (contains? selected (str (key-fn item))))) items))

    (seq exclude)
    (let [excluded (set exclude)]
      (mapv (fn [item] (assoc item :checked? (not (contains? excluded (str (key-fn item)))))) items))

    :else
    (mapv (fn [item] (assoc item :checked? true)) items)))

(defn- other-filters-where
  "Combine every active filter's where-clause EXCEPT the one named by excluding (one of :category
   :subject :from :to). This scopes a column filter's checklist to values that are still reachable
   given every OTHER active filter — e.g. once a category is picked, the From checklist only offers
   senders who actually have mail in that category — the same way Excel's own AutoFilter narrows a
   column's dropdown as other filters are applied."
  [parameters date-filter content-where excluding category-selection subject-selection from-selection to-selection]
  (let [date-filter-active? (some? date-filter)]
    (combine-wheres
     [date-filter
      (batch->where (:batch parameters))
      (filter->where (:filter parameters))
      content-where
      (when-not (= excluding :subject) (subject-values->where subject-selection))
      (when-not (= excluding :from) (sender-keys->where from-selection date-filter-active?))
      (when-not (= excluding :to) (recipient-keys->where to-selection date-filter-active?))
      (when-not (= excluding :category) (category-ids->where category-selection))])))

(defn- reachable-category-tokens
  "The set of category tokens (a numeric id, stringified, or uncategorized-token) that occur among
   headers matching other-filters-where."
  [db other-filters-where]
  (into #{} (map (fn [row] (str (or (:category row) uncategorized-token))))
        (int/fetch-header-categories db other-filters-where)))

(defn- context-zone-id
  "The time zone that decides which calendar day it is: the context's :time-zone when given (tests),
   otherwise the configured preference."
  [context]
  (if-let [configured-zone (:time-zone context)]
    (java.time.ZoneId/of configured-zone)
    (preferences/zone-id)))

(defn fetch-emails
  "Returns a list of emails. Customizable by parameters which can contain the following keys:
   :size, :page, :filter (all, enrieched-only, or without-category), :search-text (matches the
   e-mail body content), :date-from, :date-to, and for each Excel-style column filter (subject,
   from, to, category) an :xxx-values/:xxx-values-exclude (or :xxx-keys/:xxx-keys-exclude, or
   :category-ids/:category-ids-exclude) pair of collections — the checklist UI submits whichever
   one is shorter, so only one side is ever populated for a given filter.

   Each column filter's checklist of possible values is scoped to what's still reachable given every
   OTHER active filter (see other-filters-where), except the per-row category reassignment dropdown
   (:categories in the result), which always offers every category regardless of the list's filter."
  [context parameters]
  (let [db (:db context)
        category-selection (checklist-selection parameters :category-ids :category-ids-exclude :category-ids-none)
        subject-selection (checklist-selection parameters :subject-values :subject-values-exclude :subject-values-none)
        from-selection (checklist-selection parameters :from-keys :from-keys-exclude :from-keys-none)
        to-selection (checklist-selection parameters :to-keys :to-keys-exclude :to-keys-none)
        zone-id (context-zone-id context)
        date-filter (date->where (:date-from parameters) (:date-to parameters) zone-id)
        date-filter-active? (some? date-filter)
        fulltext-min-token-length (when (= :mariadb (:db-type context))
                                    (long (or (:fulltext-min-token-length context)
                                              default-fulltext-min-token-length)))
        search-text (:search-text parameters)
        ;; Built once and shared by the checklist scopes, the page query and the count: a body search
        ;; is the most expensive predicate of the list.
        content-where (content->where search-text date-filter-active? fulltext-min-token-length
                                      (when-not (or (str/blank? search-text) fulltext-min-token-length)
                                        (resolve-search-ids db (:db-type context) search-text)))
        other-where (partial other-filters-where parameters date-filter content-where)
        cat-list (annotate-checked-by (categories db) category-selection #(or (:id %) uncategorized-token))
        reachable-categories (reachable-category-tokens db (other-where :category category-selection subject-selection from-selection to-selection))
        category-filter-options (filterv #(contains? reachable-categories (str (or (:id %) uncategorized-token))) cat-list)
        subject-list (annotate-checked-by (int/fetch-distinct-subjects db (other-where :subject category-selection subject-selection from-selection to-selection)) subject-selection :subject)
        sender-list (annotate-checked-by (int/fetch-distinct-senders db (other-where :from category-selection subject-selection from-selection to-selection)) from-selection :contact_key)
        recipient-list (annotate-checked-by (int/fetch-distinct-recipients db (other-where :to category-selection subject-selection from-selection to-selection)) to-selection :contact_key)
        batch-where (batch->where (:batch parameters))
        where (combine-wheres [date-filter
                               batch-where
                               (filter->where (:filter parameters))
                               content-where
                               (subject-values->where subject-selection)
                               (sender-keys->where from-selection date-filter-active?)
                               (recipient-keys->where to-selection date-filter-active?)
                               (category-ids->where category-selection)])
        customization-clause (cond-> {:order-by [[:date :desc]]}
                               where (assoc :where where))
        page-req (page/page-request (:page parameters) (:size parameters))
        ;; :with-bodies false — the list renders no body content, so don't materialize
        ;; up to 500 e-mails' full MIME bodies per page.
        result (int/fetch-emails db {:entity :enriched-email :strict false :page page-req :with-bodies false} customization-clause)]
    (let [subject-active? (selection-filter-active? subject-selection)
          from-active? (selection-filter-active? from-selection)
          to-active? (selection-filter-active? to-selection)
          category-active? (selection-filter-active? category-selection)
          search-active? (not (str/blank? (:search-text parameters)))
          date-active? (some? date-filter)
          batch-active? (some? batch-where)
          metadata-active? (contains? #{"enriched-only" "without-category"} (:filter parameters))]
      {:data (:data result)
       :parameters {:filter (:filter parameters)
                  :batch (when batch-active? (:batch parameters))
                  :batch-active? batch-active?
                  :batch-filter-label (when batch-active? (batch-filter-label db (:batch parameters)))
                  :total-pages (page/calculate-pages-total (:total result) (:size page-req))
                  :size (:size page-req)
                  :page (:page result)
                  :total (:total result)
                  :search-text (:search-text parameters)
                  :subject-values (:include subject-selection)
                  :subject-values-exclude (:exclude subject-selection)
                  :subject-values-none (when (:none? subject-selection) "true")
                  :subject-filter-active? subject-active?
                  :subject-filter-label (selection-filter-label "Subject" subject-selection)
                  :subject-filter-badge (selection-filter-badge subject-selection)
                  :from-keys (:include from-selection)
                  :from-keys-exclude (:exclude from-selection)
                  :from-keys-none (when (:none? from-selection) "true")
                  :from-filter-active? from-active?
                  :from-filter-label (selection-filter-label "From" from-selection)
                  :from-filter-badge (selection-filter-badge from-selection)
                  :to-keys (:include to-selection)
                  :to-keys-exclude (:exclude to-selection)
                  :to-keys-none (when (:none? to-selection) "true")
                  :to-filter-active? to-active?
                  :to-filter-label (selection-filter-label "To" to-selection)
                  :to-filter-badge (selection-filter-badge to-selection)
                  :category-ids (:include category-selection)
                  :category-ids-exclude (:exclude category-selection)
                  :category-ids-none (when (:none? category-selection) "true")
                  :category-filter-active? category-active?
                  :category-filter-label (selection-filter-label "Category" category-selection)
                  :category-filter-badge (selection-filter-badge category-selection)
                  :date-from (:date-from parameters)
                  :date-to (:date-to parameters)
                  :search-active? search-active?
                  :date-active? date-active?
                  :metadata-active? metadata-active?
                  :any-filter-active? (boolean (or search-active? date-active? metadata-active? batch-active?
                                                   subject-active? from-active? to-active? category-active?))}
       :optional {:categories cat-list :category-filter-options category-filter-options
                  :subjects subject-list :senders sender-list :recipients recipient-list}})))

(defn category-name-problem
  "Why a category name cannot be used, or nil when it is fine. Category names become IMAP folder names
   ('Categories/<Name>'), so they must not be blank and must not contain the characters IMAP servers
   use as hierarchy separators or LIST wildcards, nor control characters."
  [category-name]
  (let [trimmed (some-> category-name str str/trim)]
    (cond
      (str/blank? trimmed) "The category name must not be empty."
      (> (count trimmed) 100) "The category name must not exceed 100 characters."
      (re-find #"[\p{Cntrl}]" trimmed) "The category name must not contain control characters."
      (re-find #"[/\\*%\"]" trimmed) "The category name must not contain / \\ * % or double quotes: they have a special meaning in IMAP folder names."
      :else nil)))

(defn create-new-category! [context category destination-folder color]
  (let [db (:db context)
        client (:client context)
        cleaned (when-not (str/blank? destination-folder) (str/trim destination-folder))]
    (int/save-category db category cleaned (clean-color color))
    (doseq [connection-data (vals (int/connections client))]
      (int/create-category-directories! client connection-data [category]))))

(defn update-category!
  "Persist the destination folder and display color for a category, and make sure the folder exists on
   every active connection. A blank destination-folder restores the default 'Categories/<Name>'
   behaviour; an invalid or blank color falls back to default-category-color. Existing emails are not moved."
  [context id category-name destination-folder color]
  (let [db (:db context)
        client (:client context)
        cleaned (when-not (str/blank? destination-folder) (str/trim destination-folder))]
    (int/update-category db id cleaned (clean-color color))
    (doseq [connection-data (vals (int/connections client))]
      (int/create-category-directories! client connection-data [category-name]))))

(defn move-email-to-category
  "Prefer the account recorded with the e-mail's metadata (see incoming-email-workflow). Without it,
   fall back to guessing: the recipient address is usually the connection's user name, unless a mail
   masking service is used, in which case every active connection is tried in turn. This all
   presupposes that the message-id is really unique."
  [email category {:keys [client] :as context}]
  (let [connections (vals (int/connections client))
        active-ids (set (map #(get-in % [:config :id]) connections))
        recorded-id (-> email :metadata :connection-id)
        connection-id-guess (if (contains? active-ids recorded-id)
                              recorded-id
                              (int/connection-id-for-email client connections email))
        message-id (-> email :header :message-id)
        old-category (-> email :metadata :category)]
    (try
      (cond (nil? (seq connections))
            (error-result nil "There are no active connections.")

            :else
            ;; Try the most likely account first; when it does not hold the message (:not-found), fall
            ;; back to every other active connection - a recorded account is a strong hint, not proof.
            (let [all-ids (map #(get-in % [:config :id]) connections)
                  ordered (if connection-id-guess
                            (cons connection-id-guess (remove #{connection-id-guess} all-ids))
                            all-ids)
                  results (loop [ids ordered results []]
                            (if (empty? ids)
                              results
                              (let [id (first ids)
                                    _ (t/log! :debug ["Trying to move message-id" message-id "via connection" id])
                                    result (int/move-email-between-categories client id message-id old-category category context)]
                                (if (true? result)
                                  (conj results result)
                                  (recur (rest ids) (conj results result))))))]
              (cond
                (some true? results) (success-result :ok nil)
                (every? #(= :not-found %) results) (success-result :not-found nil)
                :else (error-result nil "Moving email failed. Please check the logs."))))

      (catch Exception e (t/log! :error e) (error-result e "Moving email failed. Please check the logs.")))))

(defn- move-message [move? folder connection-id email-message category {:keys [db client]}]
  (let [message-id (-> email-message :email :header :message-id)
        ;; The email stays in the folder it arrived in. Record that folder so a later recategorization
        ;; can find it even after the category's destination is changed.
        record-current-folder! (fn []
                                 (when (some? folder)
                                   (let [current-folder (int/current-folder-name client folder)]
                                     (when-not (str/blank? current-folder)
                                       (int/update-email-folder db message-id current-folder)))))]
    (if (and (true? move?) (some? category))
      ;; The e-mail is already saved when this runs. A move that throws (target folder missing, the
      ;; connection dropped, a server NO) must therefore never surface as 'the message could not be
      ;; read': it is logged and the e-mail is recorded in its current folder like any other failed move.
      (let [moved-to-folder (try (int/move-email-to-category client connection-id (:message email-message) folder category)
                                 (catch Exception e
                                   (t/log! {:level :warn :error e} ["Moving the e-mail with subject" (-> email-message :email :header :subject) "to its category folder failed."])
                                   nil))]
        (if (string? moved-to-folder)
          (do (int/update-email-folder db message-id moved-to-folder)
              (t/log! :debug ["Email with subject:" (-> email-message :email :header :subject) "was successfully moved to the corresponding folder"]))
          ;; The move did not complete (e.g. the copy+delete fallback failed). The email is still in
          ;; its source folder, so record that rather than leaving the location unknown.
          (do (t/log! :warn ["Email with subject:" (-> email-message :email :header :subject) "could not be moved; recording its current folder so it stays findable."])
              (record-current-folder!))))
      (do (t/log! :debug ["move option:" move? "category:" category "the email" (-> email-message :email :header :subject) "will not move moved"])
          (record-current-folder!)
          :na))))

(defn- incoming-email-workflow
  "Save, categorize and (optionally) move one freshly read message. Returns {:status :skipped} when the
   message is already stored, otherwise {:status :processed :move <move-message result>}."
  [email-message connection-id folder {:keys [move? assigned-category assigned-category-id]} {:keys [analyzer db] :as context}]
  (let [message-id (-> email-message :email :header :message-id)]
    (if (int/email-exists? db message-id)
      (do (t/log! :info ["Email" message-id "already in database — skipping re-categorization"])
          {:status :skipped})
      (if (some? assigned-category)
        (let [language-result (int/detect-language analyzer (:email email-message))
              enriched-email (core-email/construct-enriched-email (:email email-message) {:language (:code language-result) :language-confidence (:confidence language-result)} {:category assigned-category :category-id assigned-category-id :category-confidence 1} connection-id)]
          (int/save-email db enriched-email)
          (t/log! :debug ["Email with subject:" (-> email-message :email :header :subject) "was successfully saved to the database"])
          {:status :processed :move (move-message move? folder connection-id email-message assigned-category context)})
        (let [enriched-email (cond-> (int/enrich-email analyzer (:email email-message))
                               ;; Remember the account the e-mail arrived on, so a later move needs no guessing.
                               (some? connection-id) (assoc-in [:metadata :connection-id] connection-id))
              category (:category (:metadata enriched-email))]
          (t/log! :debug ["Email with subject:" (-> email-message :email :header :subject) "was categorized as" category])
          (int/save-email db enriched-email)
          (t/log! :debug ["Email with subject:" (-> email-message :email :header :subject) "was successfully saved to the database"])
          {:status :processed :move (move-message move? folder connection-id email-message category context)})))))

(defn learn-category-from-folder!
  "Handle one message that arrived in a category folder of an account - moved there by the user in a
   mail client, or filed there by Plauna itself. The folder stands for exactly one category, so:
   - a stored e-mail with another (or no) category takes the folder's category as an explicit user
     decision (confidence 1.0, like a change on the e-mail page) and its new location: :learned
   - a stored e-mail that already has that category only gets its location recorded: :confirmed
   - an unknown e-mail is read with read-email (returning {:email :message}), saved with the folder's
     category and left where it is: :saved
   category is {:id :name}; folder is the open IMAP folder, folder-name its full name."
  [{:keys [db] :as context} {:keys [connection-id folder folder-name category message-id read-email]}]
  (let [state (int/fetch-email-category-state db message-id)
        record-location! (fn []
                           (when (not= folder-name (:folder state))
                             (int/update-email-folder db message-id folder-name))
                           (when (and (some? connection-id) (not= connection-id (:connection-id state)))
                             (int/update-email-connection db message-id connection-id)))]
    (cond
      (nil? state)
      (let [{:keys [status]} (incoming-email-workflow (read-email) connection-id folder
                                                      {:move? false :assigned-category (:name category) :assigned-category-id (:id category)}
                                                      context)]
        (when (= :processed status)
          (t/log! :info ["Saved the new e-mail" message-id "found in" folder-name "with its category" (:name category)]))
        (if (= :processed status) :saved :confirmed))

      (and (some? (:category-id state)) (== (:category-id state) (:id category)))
      (do (record-location!) :confirmed)

      :else
      (do (int/update-email-category db message-id (:id category) 1.0)
          (record-location!)
          (t/log! :info ["Learned category" (:name category) "for" message-id "from its move into" folder-name])
          :learned))))

(defn handle-incoming-imap-email
  "Handle incoming emails synchronously on a single thread. Returns a result."
  [parsed-email {:keys [connection-id origin-folder message] :as options} context]
  (try (let [process-result (incoming-email-workflow {:email parsed-email :message message} connection-id origin-folder options context)]
         (success-result :ok {:move (:move process-result)}))
       (catch Exception e (error-result e "Error encountered when processing incoming email"))))

(defn- record-parse-failure-safely!
  "Keep a trace of a message that could not be read or processed, so it can be listed and retried
   later. Identification is best effort and must never turn one failure into two."
  [client db n folder-name folder connection-id ^Exception error]
  (try
    (let [identity (try (int/nth-message-identity-from-folder client n folder)
                        (catch Exception _ {}))]
      (int/record-parse-failure db (merge identity
                                          {:connection-id connection-id
                                           :folder folder-name
                                           :message-number n
                                           :error (str (.getMessage error))})))
    (catch Throwable inner
      (t/log! {:level :warn :error inner} ["Could not record the failure of message" n "in" folder-name]))))

(defn- known-failures
  "The identities of the folder's recorded failures, loaded once per run so the loop only issues a
   DELETE for messages that actually have an entry. Empty when the lookup is unavailable."
  [db connection-id folder-name]
  (try (or (int/parse-failure-keys db connection-id folder-name) {:uids #{} :message-ids #{}})
       (catch Throwable _ {:uids #{} :message-ids #{}})))

(defn- resolve-parse-failures-safely!
  "Clear the failure entry of a message that is stored now - only when one exists for its UID or Message-ID."
  [db connection-id folder-name uid message-id {:keys [uids message-ids]}]
  (when (or (and (some? uid) (contains? uids uid))
            (and (some? message-id) (contains? message-ids message-id)))
    (try (int/resolve-parse-failures db connection-id folder-name uid message-id)
         (catch Throwable e (t/log! {:level :warn :error e} ["Could not clear recorded failures for" message-id])))))

(defn- finish-processed-email!
  "Bookkeeping shared by the sequence-number and UID paths after a message went through the workflow."
  [db email-message status connection-id folder-name uid options]
  (let [message-id (-> email-message :email :header :message-id)]
    ;; Membership is recorded only for e-mails this run actually saved, so the batch view shows
    ;; exactly what needs reviewing.
    (when (and (= :processed status) (some? (:batch-id options)))
      (int/record-parse-batch-email db (:batch-id options) message-id))
    ;; A message that is stored now is no longer a failure, whichever way it got in.
    (when (contains? #{:processed :skipped} status)
      (resolve-parse-failures-safely! db connection-id folder-name uid message-id (:known-failures options)))
    status))

(defn- process-nth-email-from-folder
  "Process message number n of a folder opened for bulk reading. Returns :processed, :skipped or :error.
   The Message-ID is read on its own first: an already stored message is recognised from that one
   small header fetch, so re-scanning a folder that is mostly known does not download every body again."
  [client n folder-name folder options {:keys [db] :as context} messages-result]
  (let [connection-id (:connection-id messages-result)]
    (try
      (let [known-message-id (int/nth-message-id-from-folder client n folder)]
        (if (and (some? known-message-id) (int/email-exists? db known-message-id))
          (let [location (try (int/fetch-email-location db known-message-id) (catch Throwable _ nil))
                ;; Stored from THIS folder by an earlier run (still here until moved), or a duplicate of an
                ;; e-mail that was stored from another folder or account.
                outcome (if (= folder-name (:folder location)) :skipped-here :skipped-elsewhere)]
            (t/log! :debug ["Email number" n "(" known-message-id ") already in database — skipping (" (name outcome) ")"])
            (resolve-parse-failures-safely! db connection-id folder-name nil known-message-id (:known-failures options))
            outcome)
          (let [email-message (int/nth-email-from-folder client n folder)
                {:keys [status]} (incoming-email-workflow email-message connection-id folder options context)]
            (finish-processed-email! db email-message status connection-id folder-name nil options))))
      (catch Exception e
        (t/log! :error ["Skipping email number" n "from folder" folder-name "because it could not be read or processed:" e])
        (record-parse-failure-safely! client db n folder-name folder connection-id e)
        :error))))

(defn- retry-one-failure!
  "Re-read one previously failed message by UID. Returns :processed, :skipped, :gone or :errors."
  [client db folder folder-name connection-id options {:keys [uid] :as failure} context]
  (if (nil? uid)
    ;; Without a UID the message cannot be addressed; the entry stays for information.
    :errors
    (try
      (if-let [email-message (int/email-by-uid-from-folder client uid folder)]
        (let [message-id (-> email-message :email :header :message-id)]
          (if (and (some? message-id) (int/email-exists? db message-id))
            (do (resolve-parse-failures-safely! db connection-id folder-name uid message-id (:known-failures options))
                :skipped)
            (let [{:keys [status]} (incoming-email-workflow email-message connection-id folder options context)]
              (finish-processed-email! db email-message status connection-id folder-name uid options)
              (if (= :processed status) :processed :skipped))))
        (do (t/log! :info ["Message with UID" uid "no longer exists in" folder-name "- dropping its failure entry."])
            (resolve-parse-failures-safely! db connection-id folder-name uid nil (:known-failures options))
            :gone))
      (catch Exception e
        (t/log! :error ["Retrying message with UID" uid "from folder" folder-name "failed again:" e])
        (try (int/record-parse-failure db (assoc failure :error (str (.getMessage e))))
             (catch Throwable _ nil))
        :errors))))

(defn- run-completing!
  "Run body-fn on a background thread and ALWAYS hand a summary to on-complete: a Throwable escaping
   the loop is turned into an aborted summary instead of leaving the run marked as running forever."
  [label on-complete base-summary body-fn]
  (async/thread
    (let [result (try
                   (body-fn)
                   (catch Throwable t
                     (t/log! {:level :error :error t} [label "aborted unexpectedly"])
                     (assoc base-summary :aborted (str "aborted: " (.getMessage t)))))]
      (try (on-complete result)
           (catch Throwable e (t/log! {:level :error :error e} [label "completion callback failed"]))))))

(defn retry-failed-messages!
  "Re-read previously failed messages of one folder by their IMAP UID and run them through the normal
   workflow (categorize, save, optionally move). Runs on a background thread. options are those of
   read-emails-from-folder (:move? :batch-id :assigned-category ... :on-complete); the summary passed
   to :on-complete is {:folder :processed :skipped :gone :errors :examined :remaining 0 :batch-size nil}
   where :gone counts messages that no longer exist on the server (their failure entries are dropped)."
  [connection-data folder-name failures options {:keys [client db] :as context}]
  (let [bulk (int/open-folder-for-bulk-read client connection-data folder-name)
        folder (:folder bulk)
        connection-id (:connection-id bulk)
        options (assoc options :known-failures (known-failures db connection-id folder-name))
        on-complete (or (:on-complete options) (fn [_] nil))
        base {:folder folder-name :message-count (:message-count bulk) :batch-size nil
              :processed 0 :skipped 0 :gone 0 :errors 0 :examined 0 :remaining 0}]
    (run-completing! (str "Retrying failed messages of " folder-name) on-complete base
                     (fn []
                       (let [result (try
                                      (reduce (fn [summary failure]
                                                (let [outcome (retry-one-failure! client db folder folder-name connection-id options failure context)]
                                                  (-> summary (update outcome inc) (update :examined inc))))
                                              base
                                              failures)
                                      (finally (int/close-folder-for-bulk-read client bulk)))]
                         (t/log! :info ["Finished retrying" (:examined result) "failed message(s) in" folder-name "-" (:processed result) "new," (:skipped result) "already stored," (:gone result) "gone," (:errors result) "failed again"])
                         result)))
    (count failures)))

(def ^:private batch-move-chunk-size
  "E-mails per database fetch and per IMAP move call: small enough for SQLite's bind-variable limit
   (999 on older builds), large enough to keep the round trips few."
  200)

(defn move-emails-to-category-folders!
  "Move every listed stored e-mail that has a category into that category's IMAP folder on the given
   connection. E-mails without a category stay where they are. Works in chunks over ONE dedicated IMAP
   connection (see int/move-emails-by-id). progress-fn is called with the running summary after every
   chunk; the final summary {:total :uncategorized :moved :not-found :failed} (merged into
   base-summary) is returned."
  [{:keys [db client]} connection-id message-ids base-summary progress-fn]
  (let [chunks (partition-all batch-move-chunk-size message-ids)
        summary (atom (merge {:total 0 :uncategorized 0 :moved 0 :not-found 0 :failed 0} base-summary))]
    (doseq [chunk chunks
            :let [emails (int/fetch-emails db {:entity :enriched-email :strict true :with-bodies false}
                                           {:where [:in :headers.message-id (vec chunk)]})
                  categorized (filter #(some? (-> % :metadata :category)) emails)
                  moves (mapv (fn [email] {:message-id (-> email :header :message-id) :category (-> email :metadata :category)}) categorized)
                  results (if (seq moves)
                            (try (int/move-emails-by-id client connection-id moves)
                                 (catch Exception e
                                   (t/log! {:level :error :error e} ["Moving a chunk of e-mails on connection" connection-id "failed"])
                                   (repeat (count moves) false)))
                            [])]]
      (swap! summary (fn [current]
                       (reduce (fn [acc result]
                                 (update acc (cond (true? result) :moved
                                                   (= :not-found result) :not-found
                                                   :else :failed) inc))
                               (-> current
                                   (update :total + (count moves))
                                   (update :uncategorized + (- (count emails) (count categorized))))
                               results)))
      (progress-fn @summary))
    @summary))

(defn move-parse-batch-emails!
  "Move the categorized e-mails of one parse batch into their category folders: the fix for a batch
   that was parsed with 'move' unchecked."
  [{:keys [db] :as context} batch-id connection-id progress-fn]
  (move-emails-to-category-folders! context connection-id (int/fetch-parse-batch-message-ids db batch-id)
                                    {:batch-id batch-id} progress-fn))

(defn move-folder-emails!
  "Move every stored, categorized e-mail whose recorded location is the given folder into its category
   folder - regardless of which parse run (or the live monitor) stored it."
  [{:keys [db] :as context} connection-id folder progress-fn]
  (move-emails-to-category-folders! context connection-id (int/fetch-categorized-message-ids-in-folder db connection-id folder)
                                    {:folder folder} progress-fn))

(defn- empty-folder-summary [folder-name message-count batch-size]
  {:folder folder-name :message-count message-count :batch-size batch-size
   :processed 0 :skipped 0 :skipped-elsewhere 0 :errors 0 :examined 0 :remaining 0})

(def ^:private prefetch-chunk-size
  "Sequence numbers whose envelopes are fetched with one IMAP command before they are examined."
  100)

(def ^:private consecutive-error-limit
  "Consecutive unreadable messages after which a run gives up: that many failures in a row mean the
   folder or the connection is gone, and going on would record one failure per remaining message."
  50)

(defn- contiguous-runs
  "Split sequence numbers into [from to] pairs of consecutive numbers. The numbers of a date search can
   have gaps, and one FETCH over min..max would then also load the envelopes of every message between."
  [numbers]
  (reduce (fn [runs n]
            (let [[_ to] (peek runs)]
              (if (and (some? to) (= n (inc to)))
                (conj (pop runs) [(first (peek runs)) n])
                (conj runs [n n]))))
          []
          (sort numbers)))

(defn- prefetch-identities-safely! [client folder numbers]
  (doseq [[from to] (contiguous-runs numbers)]
    (try (int/prefetch-message-identities client folder from to)
         (catch Throwable _ nil))))

(defn- process-folder-messages!
  "Walk the sequence numbers of a folder from newest to oldest and process each message, stopping
   early once batch-size messages were newly saved, or when consecutive-error-limit messages in a row
   could not be read (the summary then carries :aborted). Returns the summary map."
  [client folder-name folder options context bulk sequence-numbers summary]
  (let [batch-size (:batch-size options)]
    (loop [remaining-numbers sequence-numbers
           prefetched #{}
           consecutive-errors 0
           summary summary]
      (cond
        (empty? remaining-numbers)
        (assoc summary :remaining 0)

        (and (some? batch-size) (>= (:processed summary) batch-size))
        (assoc summary :remaining (count remaining-numbers))

        (>= consecutive-errors consecutive-error-limit)
        (do (t/log! :error [consecutive-errors "consecutive messages of" folder-name "could not be read - the folder or the connection is probably gone. Giving up on this run."])
            (assoc summary :remaining (count remaining-numbers)
                   :aborted (str consecutive-errors " consecutive messages could not be read; the connection to the folder is probably lost")))

        :else
        (let [n (first remaining-numbers)
              prefetched (if (contains? prefetched n)
                           prefetched
                           (let [chunk (take prefetch-chunk-size remaining-numbers)]
                             (prefetch-identities-safely! client folder chunk)
                             (set chunk)))
              outcome (process-nth-email-from-folder client n folder-name folder options context bulk)
              counter (case outcome :processed :processed (:skipped :skipped-here :skipped-elsewhere) :skipped :errors)]
          (recur (rest remaining-numbers)
                 prefetched
                 (if (= :errors counter) (inc consecutive-errors) 0)
                 (-> summary
                     (update counter inc)
                     (update :examined inc)
                     (cond-> (= :skipped-elsewhere outcome) (update :skipped-elsewhere inc)))))))))

(defn- since-date
  "The calendar day a :since-days run reaches back to: that many days before today in the configured zone."
  [days context]
  (.minusDays (java.time.LocalDate/now ^java.time.ZoneId (context-zone-id context)) (long days)))

(defn- sequence-numbers-to-read
  "The sequence numbers a run examines, newest (highest) first: the whole folder, its most recent :limit
   messages or - with :since-days - the messages received within that many days (again capped by :limit).
   Throws when the server cannot answer the date search."
  [client bulk {:keys [limit since-days]} context]
  (let [message-count (:message-count bulk)]
    (if (some? since-days)
      (if (pos? message-count)
        (cond->> (sort > (int/message-numbers-since client (:folder bulk) (since-date since-days context)))
          (some? limit) (take limit))
        [])
      ;; Sequence numbers are 1-based; higher = more recent. With :limit take only the top N, without it
      ;; everything down to 1.
      (let [lowest (if limit (max 1 (- (inc message-count) limit)) 1)]
        (range message-count (dec lowest) -1)))))

(defn read-emails-from-folder
  "Read emails from a folder and process them. Returns the number of messages in the folder.
   Emails are read over a DEDICATED IMAP connection (separate from the IDLE monitor, so the two never
   contend for the same folder/connection) and processed on another thread.

   options may include:
   - :limit N to process only the N most recent messages (the highest sequence numbers) instead of the
     whole folder. The reconnect back-fill uses this so a large mailbox isn't fully re-scanned every
     time; already-saved messages are skipped either way (see incoming-email-workflow), so a bounded
     scan still catches up every genuinely-missed message within the window.
   - :since-days N to process only the messages RECEIVED within the last N days (whole calendar days,
     today included: with 7 on the 14th, everything received since the 7th). This finds e-mails that
     were moved into the folder later - e.g. out of the spam folder - wherever they ended up in the
     folder, while everything already stored is skipped as usual. Combines with :limit and :batch-size.
   - :batch-size N to stop after N messages were NEWLY saved and categorized. Already stored messages
     do not count towards the batch, so repeating the same parse works through a large folder in
     batches of N: each run skips what earlier runs stored and continues with the next N unknown
     messages.
   - :batch-id, an identifier under which every NEWLY saved e-mail is recorded (int/record-parse-batch-email)
     so the run can be reviewed afterwards as a filtered e-mail list.
   - :on-complete, a function called with a summary map once the background thread has finished:
     {:folder :message-count :batch-size :processed :skipped :skipped-elsewhere :errors :examined :remaining}
     where :remaining is the number of (older) messages that were not examined because the batch was
     full and :skipped-elsewhere the part of :skipped that was stored from another folder (duplicates).
     A :since-days run adds :since-days to the summary."
  [connection-data folder-name options {:keys [client db] :as context}]
  (let [bulk (int/open-folder-for-bulk-read client connection-data folder-name)
        folder (:folder bulk)
        message-count (:message-count bulk)
        batch-size (:batch-size options)
        since-days (:since-days options)
        options (assoc options :known-failures (known-failures db (:connection-id bulk) folder-name))
        on-complete (or (:on-complete options) (fn [_] nil))
        ;; Searching can fail (e.g. the connection dropped): the dedicated Store opened above must not leak.
        sequence-numbers (try (sequence-numbers-to-read client bulk options context)
                              (catch Throwable e
                                (int/close-folder-for-bulk-read client bulk)
                                (throw e)))
        to-process (count sequence-numbers)
        summary (cond-> (empty-folder-summary folder-name message-count batch-size)
                  (some? since-days) (assoc :since-days since-days))]
    (if (pos? to-process)
      (do
        (t/log! :info ["There are" message-count "emails in" folder-name "-" to-process "will get processed asynchronously"
                       (if since-days (str "(received in the last " since-days " day(s))") "")
                       (if batch-size (str "(stopping after " batch-size " newly saved emails)") "")])
        ;; A real thread (run-completing! uses async/thread), not async/go: each email does blocking JDBC
        ;; and IMAP work, and a long blocking loop inside a go block would tie up a shared dispatch thread.
        (run-completing! (str "Parsing " folder-name) on-complete summary
                         (fn []
                           (let [result (try
                                          ;; Iterate sequence numbers high -> low. When move? is enabled a processed message is moved
                                          ;; out and expunged, which shifts the sequence numbers of all HIGHER messages down by one.
                                          ;; Going downward means the numbers we have not processed yet are never affected, so no
                                          ;; message is skipped or referenced after it moved. (Sequence numbers are 1-based.)
                                          (process-folder-messages! client folder-name folder options context bulk
                                                                    sequence-numbers summary)
                                          (finally
                                            (int/close-folder-for-bulk-read client bulk)))]
                             (t/log! :info ["Finished parsing" folder-name "-" (:processed result) "new," (:skipped result) "already stored,"
                                            (:errors result) "failed," (:remaining result) "not examined" (or (:aborted result) "")])
                             result))))
      (do
        (int/close-folder-for-bulk-read client bulk)
        (t/log! :info [(if since-days
                         (str "No emails were received in " folder-name " within the last " since-days " day(s). Doing nothing.")
                         "There are no emails in the folder. Doing nothing.")])
        (on-complete summary)))
    message-count))
