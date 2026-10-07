(ns plauna.interfaces)

(defprotocol DB
  "Database protocol"
  (fetch-connection [this id] "Get connection for id.")
  (fetch-oauth-token-data [this id] "Get oauth token data for a connection")
  (fetch-auth-provider [this id])
  (fetch-categories [this] "Get a list of all categories")
  (fetch-distinct-subjects [this other-filters-where] "Get every distinct, non-blank e-mail subject reachable given other-filters-where (a honeysql where-clause from the OTHER active column filters, or nil for every subject). Capped, see distinct-value-limit.")
  (fetch-distinct-senders [this other-filters-where] "Get every distinct sender contact reachable given other-filters-where. Capped, see distinct-value-limit.")
  (fetch-distinct-recipients [this other-filters-where] "Get every distinct 'To' recipient contact reachable given other-filters-where. Capped, see distinct-value-limit.")
  (fetch-header-categories [this other-filters-where] "Get every distinct category id (or nil for uncategorized) reachable given other-filters-where.")
  (fetch-emails [this entity customization] "Get a list of emails")
  (save-category [this category-name destination-folder color])
  (update-category [this id destination-folder color] "Set the IMAP folder that emails of this category should be moved to (a blank value restores the default 'Categories/<Name>' behaviour) and its display color.")
  (update-email-folder [this message-id folder] "Record the IMAP folder a message currently lives in.")
  (email-exists? [this message-id] "Return true if a header with this message-id is already in the database.")
  (record-parse-batch-email [this batch-id message-id] "Remember that message-id was newly saved by the folder parse run batch-id.")
  (record-parse-failure [this failure] "Remember a message that could not be read or processed: {:connection-id :folder :uid :message-number :message-id :subject :error}. A repeated failure of the same UID updates the existing entry.")
  (resolve-parse-failures [this connection-id folder uid message-id] "Forget recorded failures of a message that was read successfully after all, matched by UID or Message-ID.")
  (parse-failure-keys [this connection-id folder] "The identities of the recorded failures of a folder: {:uids #{...} :message-ids #{...}}, so a run only resolves failures that exist.")
  (fetch-parse-batch-message-ids [this batch-id] "The Message-IDs saved by a folder parse run, newest first.")
  (fetch-email-location [this message-id] "Where a stored e-mail is known to live: {:folder ... :connection-id ...}, or nil.")
  (fetch-categorized-message-ids-in-folder [this connection-id folder] "The Message-IDs of stored, categorized e-mails whose recorded location is the given folder of the connection.")
  (fetch-parse-batch [this id] "Get one folder parse run by id, or nil.")
  (fetch-content-match-ids [this search-text] "The message ids whose body content contains search-text literally (used to resolve a body search once per page instead of once per statement). nil when the lookup is not available.")
  (save-email [this email]))

(defprotocol EmailClient
  "Email client"
  (start-monitor [this config context] "Connect to the client")
  (connections [this] "Get a list of connections")
  (create-category-directories! [this connection-data category-names])
  (connection-id-for-email [this connections email])
  (move-email-between-categories [this connection-id message-id old-category new-category context]
    "Move a stored message between category folders. Return true on success, :not-found when the
     source folder was searched successfully but contains no matching message, and false for other
     failures such as a disconnected store.")
  (move-email-to-category [this connection-id original-message original-folder category])
  (move-emails-by-id [this connection-id moves] "Move several stored messages ([{:message-id :category}]) into their category folders over ONE dedicated connection. Returns the per-move results in order: true, :not-found or false.")
  (prefetch-message-identities [this folder from to] "Best-effort bulk fetch of the envelopes of messages from..to (sequence numbers) so the following per-message identity lookups need no round trip.")
  (message-numbers-since [this folder since] "The sequence numbers (ascending) of the messages of a folder opened for bulk reading that were RECEIVED on or after the calendar day since (a java.time.LocalDate). The server compares whole days and ignores the time of day. The received date (IMAP INTERNALDATE) is used rather than the Date header because moving a message between folders keeps it, while the Date header can be arbitrarily old or wrong.")
  (nth-email-from-folder [this n folder])
  (nth-message-id-from-folder [this n folder] "Return only the Message-ID of message number n in a folder (a cheap header fetch, without the body), or nil when the message has none.")
  (nth-message-identity-from-folder [this n folder] "Best-effort identification of message number n for the failure list: {:uid :message-id :subject}, each nil when it cannot be read.")
  (email-by-uid-from-folder [this uid folder] "Read the message with the given IMAP UID from a folder opened for bulk reading, like nth-email-from-folder; nil when no such message exists any more.")
  (current-folder-name [this folder] "Return the full IMAP name of a folder object.")
  (open-folder-for-bulk-read [this connection-data folder-name] "Open folder-name on a dedicated Store connection, isolated from the IDLE monitor. Returns a handle {:folder :message-count :connection-id ...}.")
  (close-folder-for-bulk-read [this bulk-handle] "Close the dedicated connection opened by open-folder-for-bulk-read."))

(defprotocol Analyzer
  "Language detection and categorization"
  (enrich-email [this email])
  (detect-language [this email]))
