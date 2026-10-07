# Changelog

All notable changes to this project will be documented in this file.

## Unreleased

### 🐛 Bug Fixes

- Plauna no longer expunges IMAP folders. Closing the monitored folder on disconnect and closing the
  source and target folders of every recategorization used `Folder.close()`, which is `close(true)`:
  every message a mail client had flagged for deletion but not yet expunged was permanently removed
  by Plauna. Folders are now closed without expunging.
- One unreadable new message no longer stops the other new messages of the same IMAP event from being
  processed or leaves the monitored folder without IDLE until the next health check. The message is
  recorded under "Messages That Could Not Be Read" and IDLE is re-armed in any case.
- An IMAP move that fails after the e-mail was saved (a missing target folder, a dropped connection)
  is no longer counted as a read failure: the e-mail keeps its category, is recorded in its current
  folder and belongs to its parse batch. A missing target folder is created on the fly.
- Recategorizing an e-mail from the list on a server without the IMAP MOVE extension failed; it now
  falls back to copy and delete like the incoming-mail path. Servers without UIDPLUS leave the copied
  original flagged as deleted instead of failing the move.
- Unchecking a single sender or recipient in the "From"/"To" checklist hid every e-mail that had no
  participant of that type at all (for example e-mails where you were only in Cc). Excluding now
  hides exactly the e-mails involving the unchecked entries, as the subject filter already did.
- The e-mail list answered with an error page for a malformed date filter or a category filter made
  only of unknown values (which rendered an empty `IN ()`, a syntax error on MariaDB). Unparseable
  dates are ignored and unknown categories match nothing.
- Saving an OAuth provider from the connection page always ended in a "500" (the update itself had
  succeeded); the request now answers 204, and only the known provider columns reach the database.
- Unknown URLs, garbled e-mail ids, an unknown connection operation, "disconnect" on an unregistered
  connection and starting a folder parse on an inactive or missing connection produced error pages;
  they now answer with 404/400 or an alert message. A parse whose folder could not be opened is shown
  as aborted instead of finished with zero counts, and a run whose final counts cannot be stored no
  longer stays "running" forever. The "Start REPL" form redirected to a non-existent page.
- "Categorize Fresh Data" failed with a SQL error when no training language was activated, and always
  processed 20 e-mails regardless of the requested count.
- The SQLite → MariaDB migration skipped the parse runs, parse batch membership, read failures and
  cached training features (every table added since the migration was written). It also loaded each
  table completely into memory and inserted one row per statement; it now streams rows and inserts in
  chunks, falling back to single rows only for a rejected chunk.
- Re-running the language detection for e-mails without a language queued the same e-mails over and
  over (the not-yet-written results were re-read from page one) and duplicated every attachment row of
  those e-mails; it now queues each e-mail once, in the background, and rewrites only the metadata.
- Importing an mbox blocked for good once 300 fragments had been dropped by the parser, because the
  back-pressure tokens of dropped messages were never released. mbox files were also decoded as UTF-8
  before parsing, which mangled 8-bit Latin-1/Windows-1252 bodies; the bytes are now passed through
  unchanged. An upload without a file gave an error page; the import runs in the background and
  reports when the file has been read.
- A language detector failure or a categorizer failure dropped the e-mail from the import entirely;
  such e-mails are now saved without the failed piece of metadata.
- A batch of up to 500 e-mails was lost when MariaDB rejected a single row (a column limit, or the whole
  multi-row INSERT exceeding `max_allowed_packet`); body inserts are now chunked and a rejected batch
  is retried e-mail by e-mail. A crashed database event loop no longer leaves stale subscriptions that
  would stall every producer.
- Training and model file names were parsed as a fixed three-letter language code; language tags of
  other lengths (such as `zh-cn`) now round-trip. A statistics chart no longer shows "Not detected"
  twice when both NULL and "n/a" languages exist.
- A refresh-token lookup for an OAuth connection without a stored token no longer posts a nil token
  and reports a "transient error" at every health check. `get-connection` returns nil for an unknown
  id instead of a record of nils.
- Messages without a Message-ID header could not be stored ("Column 'message_id' cannot be null").
  They now get a stable substitute id derived from date, sender and subject, identical for IMAP and
  mbox, so they are saved once and recognised as stored afterwards.

- A language whose categorized e-mails all belong to one category no longer aborts the whole training
  run with "Training data must contain more than one outcome". The language is skipped and named in
  the result, the other languages are trained, and a failing language is reported the same way.
- Read e-mails whose IMAP ENVELOPE the server delivers in a form the mail library cannot parse
  (typically malformed address headers, reported as "Failed to load IMAP envelope"). Such messages
  failed on every backfill and folder parse; Plauna now rebuilds Message-ID, subject, date and
  participants from the raw header block instead, parsing addresses leniently.

### ⚡ Performance

- The categorization model is deserialized once per model file and cached (reloaded when the file
  changes) instead of being read from disk for every single e-mail.
- Attachments are no longer downloaded from the IMAP server only to be discarded: only text parts
  are fetched, and the transfer encoding is read from the BODYSTRUCTURE instead of one extra header
  fetch per part.
- The body search on SQLite scans the bodies table once per page load and inlines the matching ids
  into the list, count and checklist statements instead of repeating the LIKE scan six times.
- The HTML body is cleaned once for language detection and feature extraction; activated languages
  and categories are cached briefly instead of being queried per e-mail; MariaDB metadata upserts
  are written in multi-row statements; training lines are built with a StringBuilder.
- The "connection not active" banner no longer sends an IMAP NOOP per connection on every page
  render (the state is cached for five seconds), and the start page no longer aggregates the whole
  mailbox by year just to decide where to redirect.
- SQLite gets an index on `headers(subject)` for the Subject checklist.

### 🛡️ Hardening

- IMAP connections get a connect timeout (15 s) and a longer read timeout (30 s instead of 5 s, so a
  slow body fetch no longer counts as a dead connection). STARTTLS is required when selected instead
  of silently falling back to plaintext.
- Wrong login guesses impose a growing pause (attempts inside it are refused with 429 without a
  password check), the login page no longer pre-fills the login name, a wrong login name costs the
  same time as a wrong password, and sessions end when the
  password or login name changes or 30 days after the login. A certificate-authenticated request
  from another site (cross-site request forgery via mTLS) is refused. Request bodies and uploads are
  only parsed after authentication. Redirect targets of the form `/\host` are rejected.
- Category names are validated (no separators, wildcards or control characters).
- Connections that could not be established at startup are retried with a doubling pause (2, 4,
  8 ... minutes, at most an hour); a failing connection no longer prevents the web server from
  starting. A dead IDLE manager is replaced, the
  health check back-fills mail after re-opening a closed folder, tearing down a connection continues
  past a failing step, and a "connect" while a connection is already live closes the old one first.
- Automatic training survives a failure to compute its next run time; a damaged `settings.json` is
  reported with its path instead of a bare parser error.

### ✨ Features

- Faster training: the classification features of each e-mail are computed once and cached in the
  new `training_tokens` table (invalidated when an e-mail is re-fetched), the languages train in
  parallel, and the MaxEnt trainers use several processor cores. A new "MaxEnt, L-BFGS" model option
  trains the same model family with the quasi-Newton optimiser in far fewer passes than GIS.

- Allow explicitly selected mTLS client certificates, verified by an NGINX reverse proxy, to bypass
  the web UI password and establish a normal signed Plauna session. The opt-in integration uses a
  SHA-256 certificate-fingerprint allowlist and requires a shared proxy secret so forwarded headers
  cannot be spoofed by a client with direct access to Plauna. The allowlist and write-only proxy
  secret can be managed and activated immediately from the administration UI; environment variables
  remain available as a read-only higher-priority configuration source. Configuration changes
  require the current admin password again. A CA-verified but not-yet-allowed certificate can be
  enrolled from the mTLS settings page without accepting its fingerprint from the browser as input.
- Require a configurable login name in addition to the web UI password. Existing and new
  installations default to `root`; administrators can change the case-sensitive name under Login
  Settings after re-entering the current password.
- Re-scan the e-mails of the last N days: the connection page has a "Re-scan Recent E-mails" box
  (days required, default 7; "Move e-mails after categorization" ticked by default) that re-reads
  the watched folder and processes only the e-mails Plauna does not know yet - for mail that landed
  in the spam folder first and was moved back to the inbox. The "Parse E-mails from Folders" form
  gets the same limit as an optional "Only e-mails of the last N days" field, combinable with the
  batch size. The period is matched against the day the e-mail reached the server (IMAP received
  date, whole calendar days, today included), which a move between folders keeps; the finished
  run's message names the period.
- Parse IMAP folders in batches: the "Parse E-mails from Folders" form has a "Batch size" field
  (default 100) that stops a run after that many newly saved e-mails. Already stored e-mails are
  recognised by their Message-ID alone, skipped without downloading the body again and do not count
  towards the batch, so repeating the same parse continues with the next batch. When a run finishes,
  a message reports how many e-mails were saved, skipped, failed and how many older e-mails remain
  unexamined. Leaving the field blank processes the whole folder as before.
- Review a folder parse run right after it finishes: each run is stored with its counts and the
  e-mails it saved, listed under "Recent Folder Parse Runs" on the connection page (which reloads
  while a run is in progress), and both the finished message and the table link to the E-mails page
  filtered to exactly those e-mails. The batch filter is a removable chip that combines with the
  other filters. Runs interrupted by a restart are marked as aborted.
- Record messages that could not be read during a folder parse or the reconnect catch-up (folder,
  IMAP UID, Message-ID, subject, error, attempts) and list them on the connection page (the most
  recent 200 rows, with exact per-folder counts). A "Retry" re-reads them by UID and presents the
  recovered e-mails as a reviewable batch; entries clear themselves once the message is stored or has
  disappeared from the server, and "Dismiss" removes one by hand. A run gives up after 50 unreadable
  messages in a row - the sign of a lost connection - instead of recording every remaining message,
  and repeated failures of a message whose UID cannot be read are still deduplicated (by sequence
  number). Folder parses prefetch message envelopes in blocks of 100, cutting the per-message round
  trips when re-scanning a mostly known folder.
- Move all categorized e-mails still recorded in a folder into their category folders at once, across
  every parse run: the connection page lists each source folder with its stored e-mails with and
  without a category and offers "Move N to category folders".
- Parse runs distinguish why an e-mail was skipped: saved by an earlier run of the same folder (still
  waiting to be moved) or a duplicate of an e-mail stored from another folder ("elsewhere"). Both the
  run's message and the run table show the breakdown.
- Move a parse batch to its category folders afterwards: a run parsed without "Move e-mails after
  categorization" offers "Move to category folders" on the connection page and on the batch's e-mail
  list. The categorized e-mails of the batch are moved in the background over a single IMAP
  connection, in chunks of 200, and a summary reports how many were moved, not found or left in place
  because they have no category.
- Remember the IMAP account each e-mail was read from (`metadata.connection_id`, back-filled from
  parse runs and, with a single configured account, from that account). Moves and re-fetches try that
  account first instead of guessing from the recipient address; the other active connections remain
  the fallback when it does not hold the message. Adopted from upstream Plauna.
- React to the IMAP server closing the connection right away: a connection listener on the store
  schedules the same reconnect-and-catch-up the periodic health check performs, a few seconds after
  the drop, instead of waiting for the next health-check interval; listener and health check are
  serialized per connection so a drop is never repaired twice at once. Adopted from upstream Plauna.
- The connection page reloads itself while a parse, retry or batch move runs by polling a small
  status endpoint, and never while a form field is being edited.
- Model training runs in the background and shows a live progress page with a progress bar, a
  checklist of every step (prepare, collect, train per language, write, finish) that is ticked off as
  the run advances, the collection count and the per-language MaxEnt iteration, instead of blocking
  the request until the reverse proxy reports a 504 gateway time-out. Manual training, the model switch and the daily
  automatic run share one slot, so only one training runs at a time. A request that finds the slot occupied is
  told so instead of being dropped; the run's result is also shown as a message on the next page load;
  a model switch is validated before it occupies the slot; languages that cannot be trained (a
  single category) do not block the switch but are named in the result.

## [2026-07-10.0] - 2026-07-10

### 🐛 Bug Fixes

- Save each e-mail (and each batch) in a single database transaction. A failure
  mid-save could previously leave a header without its bodies, participants, or
  metadata — and because "already saved" checks only look at the header, such a
  partial e-mail was permanently skipped by every later backfill.
- Actually enforce SQLite foreign keys. The PRAGMA was only applied to one
  temporary connection, so cascades and reference validation never ran for
  normal queries; it is now part of the JDBC URL and applies to every
  connection. Deleting a connection now also removes its OAuth tokens, as the
  schema always intended.
- Deleting a category now clears it from all affected e-mails in the same
  transaction (previously it failed outright under MariaDB and left dangling
  category references under SQLite). Deleting an OAuth provider likewise
  detaches it from connections that referenced it.
- Store one OAuth token row per connection (upsert) instead of appending a new
  row on every authorization and then reading back an arbitrary — often the
  oldest — token. A re-authorization that omits a refresh token keeps the
  stored one.
- Creating a connection with OAuth 2 selected now saves the authentication type
  and provider; previously both were silently dropped and the connection was
  created as basic/unconfigured.
- Deleting a connection now closes its active monitor first. Previously the
  monitor kept running (and accessing the mailbox) while the connection
  vanished from the UI, leaving no way to disconnect it.
- Categories with spaces in their names now train and predict correctly: models
  are trained on category ids instead of names (a name like "Work Projects" was
  silently trained as "Work" and never matched again). Old models keep working
  until the next re-training.
- A failed IMAP login is now reported as an error on the Connections page
  instead of pretending the connection succeeded.
- Fixed the e-mail detail page throwing when updating an uncategorized e-mail
  or selecting "n/a".
- Unchecking a subject in the Subject filter no longer also hides every e-mail
  that has no subject at all.
- The saved OAuth provider is selected again when reopening a connection page;
  previously it rendered unselected and an unrelated edit could wipe it.
- Fixed the provider "Update" button doing nothing in browsers without the
  non-standard global `event` object (e.g. Firefox).
- Success and error toasts redirected to the admin page are shown again instead
  of being silently discarded.
- Preferences are now validated before saving: thresholds must be between 0 and
  1, the health-check interval must be a positive number (a saved 0 used to
  break all new connections), and only known settings and log levels are
  accepted. Invalid input saves nothing.
- E-mails without a detected language no longer break the language
  administration page under MariaDB.
- Fixed a slow leak where every mbox upload or bulk language-detection run left
  behind a permanent internal event subscriber.

## [2026-07-09.0] - 2026-07-09

### 🚀 Features

- Automatically catch up on mail missed while a connection was offline. Live
  monitoring uses IMAP IDLE, which only delivers messages that arrive while
  connected, so a dropped connection or a container restart previously left a
  permanent gap (mail that arrived during the outage never appeared, even after
  reconnecting) until a manual folder parse. Now, whenever a connection is
  (re)established - at startup, on a manual reconnect, or when the periodic
  health check restores a dropped connection - Plauna re-reads the most recent
  messages in the monitored folder and saves/categorizes anything that was
  missed. Already-stored e-mails are skipped, so nothing is duplicated. The
  scan is bounded to the most recent messages; a larger gap is still
  recoverable with the manual folder Parse control.

## [2026-07-08.3] - 2026-07-08

### 🚀 Features

- Each Subject/From/To/Category filter checklist now only lists values still
  reachable given every other active filter (e.g. once a category is picked, the
  From checklist only shows senders who actually have mail in that category),
  matching Excel's own AutoFilter behavior. The per-row category reassignment
  dropdown is unaffected and always offers every category.

### 🐛 Bug Fixes

- Fix long subjects and sender names/addresses overflowing the filter dropdowns:
  widened the popovers and switched to proper CSS ellipsis truncation, with the
  full value still available as a hover tooltip.

## [2026-07-08.2] - 2026-07-08

### 🐛 Bug Fixes

- Fix the "(Select All)" checkbox in a Subject/From/To/Category filter dropdown
  immediately resetting the filter instead of letting you pick specific values:
  it now only toggles the checkboxes and no longer submits by itself.
- Fix HTTP 414 (URI Too Long) still occurring when actually narrowing a
  Subject/From/To filter (e.g. unchecking a couple of senders out of hundreds
  still left most of them checked). Each checklist now always submits
  whichever side - checked or unchecked - is smaller, capping the query
  string at roughly half the list regardless of which few values you're
  including or excluding.

## [2026-07-08.1] - 2026-07-08

### 🐛 Bug Fixes

- Fix HTTP 414 (URI Too Long) when using the Subject/From/To filter checklists on
  a mailbox with many distinct senders or recipients: a fully-checked checklist
  (the default, unfiltered state) no longer submits every individual value.
- Checking or unchecking a box in the Subject/From/To/Category filter dropdowns
  now applies the filter immediately, instead of requiring a scroll down to the
  Apply button.

## [2026-07-08.0] - 2026-07-08

### 🚀 Features

- Add Excel-style checklist filters to the e-mail list's "Subject", "From", and
  "To" column headers, matching the existing "Category" filter: check one or
  more distinct values and click Apply. Each dropdown includes a search box to
  narrow a long list of values, since unlike categories these can number in
  the hundreds.
- The "Search Text" field now matches e-mail body content instead of the
  subject, since Subject filtering moved to its own column header filter.

## [2026-07-07.4] - 2026-07-07

### 🚀 Features

- Add an Excel-style filter to the e-mail list's "Category" column header: click it
  to check one or more categories (plus "n/a" for uncategorized e-mails) and click
  Apply. Every category is checked by default; a "(Select All)" toggle flips them
  all at once, and the selection is preserved across pagination.
- Extend category color-coding to the whole row: each e-mail list row is now tinted
  with its category's color at 20% opacity, in addition to the dropdown's left
  border added previously.

## [2026-07-07.3] - 2026-07-07

### 🚀 Features

- Add color-coding for e-mail categories. Set a category's color with a color picker
  in Admin > Categories; the category dropdown on the e-mail list and on an e-mail's
  details page shows a left-border accent matching the selected category's color.

## [2026-07-07.2] - 2026-07-07

### 🐛 Bug Fixes

- Fix the Subject filter added in 2026-07-07.1: it rendered full-width inside the
  table header, which distorted the column widths and hid other columns. Removed it
  and restored the normal-sized "Search Text" field in the filter card instead.

## [2026-07-07.1] - 2026-07-07

### 🚀 Features

- Add a calendar date-range picker for the e-mail list's date filter, replacing the
  two separate "Date From"/"Date To" boxes. Vendored locally rather than loaded from
  a CDN, so it keeps working without internet access.
- Add a "Subject" filter directly in the e-mail list's header row, and a new "From"
  filter that matches a sender's name or address.
- Make the e-mail list's page size a free-form field (previously a fixed 10/20/30
  dropdown), moved to the bottom of the list.

### 🐛 Bug Fixes

- Fix a crash (500) when using the new "From" filter: the pagination count query
  rewrote SQL text with a regex that also matched the filter's own subquery,
  producing an unfillable placeholder.
- Escape `%` and `_` in Subject/From search text so they match literally instead of
  acting as SQL LIKE wildcards.
- Match e-mails whose sender participant type was stored as the legacy `:sender`
  spelling (with the leading colon), not just `sender`.
- Skip loading full e-mail body content for the list view, which never renders it -
  relevant now that the page size can go as high as 500.

## [2026-07-07.0] - 2026-07-07

### 🐛 Bug Fixes

- Fix the IMAP message listener being recreated (instead of reused) on every
  monitoring restart. Since `stop-monitoring` only ever removed the listener
  stored on the connection, each restart (e.g. after moving an email into the
  monitored folder) left the old listener attached and added a new one, so
  incoming emails were processed once per stacked listener.
- Restart the periodic health check after a category move that pauses and
  resumes monitoring on the connection's own folder. It was being cancelled
  but never rescheduled, so the connection silently lost automatic reconnect
  after the first such move.
- Fix connections read from the config file on first boot never actually
  connecting: the code looked up the freshly generated connection id in the
  wrong map, always got `nil`, and only succeeded on the next restart once the
  connection existed in the database.
- Fix "Categorize Fresh Data" always clearing the category (and never setting
  one) because it read a `:sanitized-content` field that only exists on
  emails prepared for display, not on raw body parts from the database.
- Fix a broken redirect URL after deleting an auth provider (the status code
  ended up appended inside the URL string instead of being passed to the
  redirect).
- Close the output stream used to write a freshly trained language model
  instead of leaking the file descriptor.
- Fix a rate limiter that could freeze the whole event pipeline (e.g. on a
  second mbox upload) if an earlier upload or "detect languages" run left its
  limiter's token bucket abandoned and blocked on.

## [2026-06-28.3] - 2026-06-28

### 🐛 Bug Fixes

- Fix IMAP IDLE monitoring for SSL connections, which broke with "Folder is not using
  SocketChannels" during health checks. SSL is enabled via mail.imap.ssl.enable on the
  "imap" store; switching SSL configs to the "imaps" store (in 2026-06-28.0) moved
  property resolution to the mail.imaps.* prefix, so mail.imap.usesocketchannels no
  longer applied and IdleManager could not watch the folder.

## [2026-06-28.2] - 2026-06-28

### 🚀 Features

- Add a "Re-fetch from server" button on the e-mail details page. It re-reads the
  message from the IMAP server by Message-ID and fills in data that could not be
  extracted earlier (body parts, and the language when it was never detected),
  without clobbering content or a language the user set manually.

## [2026-06-28.1] - 2026-06-28

### 🐛 Bug Fixes

- IMAP emails with a non-text attachment (e.g. a PDF) were stored with no body parts
  ("Email has no parseable text content"). JavaMail returns an attachment's content as
  an InputStream; storing it broke the body insert on MariaDB (the header was saved
  first, so the email survived without any body). Attachment content is now dropped at
  parse time, matching the mbox parser and the documented "attachments are not stored"
  behaviour, so the text parts are saved.
- Live IMAP parsing no longer fails for a text part with no charset parameter (e.g.
  "Content-Type: text/plain"); the charset helper falls back instead of throwing an NPE.

## [2026-06-28.0] - 2026-06-28

### 🔒 Security

- Bind the in-app nREPL to localhost instead of all network interfaces
- Persist the session cookie key and set HttpOnly + SameSite=Lax (mitigates CSRF)
- Reject off-site redirect targets from redirect-url/Referer (open redirect)
- Return 400/500 with a generic message instead of leaking stack traces on bad input

### 🐛 Bug Fixes

- IMAP connections always used plaintext instead of SSL
- BCC recipients were stored with the CC type
- Message rate limiter could busy-spin after the main channel closed
- OAuth refresh token was deleted on transient network errors; add HTTP timeouts
- Add graceful shutdown of IMAP connections, server and watchdog on SIGTERM
- Language detection picked an arbitrary candidate instead of the best match, and crashed on hyphenated codes (e.g. zh-cn)
- Categorizing an email with empty content no longer kills the process
- Guard against nil MIME types and nil content while parsing emails
- Plain-text bodies are no longer mangled by HTML stripping
- Decode message bodies as UTF-8 deterministically
- Empty mbox files no longer emit a spurious empty email
- IMAP moves restore monitoring even on failure; health-check tasks no longer leak; only the moved message is expunged

### ⚡ Performance

- Batch-load email listing data, eliminating per-row N+1 queries
- Run IMAP health checks on a thread pool so one slow connection cannot stall the others
- Index metadata(language) and metadata(category)
- Fix a MariaDB pagination crash on the first page (negative OFFSET)
- Use a column-preserving upsert for the SQLite metadata batch (was INSERT OR REPLACE)

### 🚜 Refactor

- Remove the unused specs namespace and clear migration reflection warnings

## [2026-03-18.0] - 2026-03-18

### 🚀 Features

- Add search functionality to the email list

### 🐛 Bug Fixes

- Server error due to changes in pagination structure
- Creating categories and moving emails fail
- *(client)* Empty folder name not recognized as default folder name
- Empty strings are treated as nil in default folder names
- Non latin texts are not normalized correctly

### 🚜 Refactor

- Logic for moving emails
- Simplify logic for incoming emails and add tests

### ⚙️ Miscellaneous Tasks

- Update dependencies and docker images

## [2026-01-18.0] - 2026-01-18

### 🚀 Features

- IMAP connections are managed in the ui instead of the config file
- Use environment variables for configuration
- Add oauth2 authentication for imap

### 🐛 Bug Fixes

- Imap parsing, add new connection, statistics page
- Client deletes access token after a non-200 response from the oauth server
- Parsing emails from IMAP folders now handles all emails
- Moving an email does not go through all connections if the connection id can be guessed
- Client falls back to INBOX if the folder to monitor is empty in the config
- *(client)* Monitored folder falls back to "Inbox" when no name is provided

### ⚙️ Miscellaneous Tasks

- *(ui)* Clean up statistics page

## [2025-09-06.0] - 2025-09-06

### 🚀 Features

- Toggle the repl on/off over the ui

### 🐛 Bug Fixes

- Imap client now properly idles and handles dead connections
- Closed folder when moving emails causes emails to get stuck in the inbox
- Text normalizer removes extra whitespaces and special characters from texts properly

## [2025-07-29.0] - 2025-07-29

### 🚀 Features

- Visiting root path on server redirects to /emails if there are e-mails to show
- Add delete functionality for e-mails in the db

### 🐛 Bug Fixes

- Creating categories without any IMAP connections returns 500
- Broken reconnection logic in the IMAP client
- [**breaking**] Email url uses base64 encoding instead of urlescaping

### 📚 Documentation

- Add screenshots from new design to docs and improve

### ⚙️ Miscellaneous Tasks

- Update dependencies for security reasons
- Update ring dependencies

## [2025-05-26.0] - 2025-05-26

### 🚀 Features

- Use IMAP copy and delete when move is not available
- Move e-mail when its category is changed by user
- *(ui)* Make ui better and mobile friendly
- *(ui)* Add the new plauna logo

### 🐛 Bug Fixes

- Change text sanitization for cleaner training texts
- Adjust text sanitization for cleaner training texts
- Moving emails no longer triggers a search through the whole folder
- *(imap client)* Wrong method call during reconnection
- Faulty partial update of connection data on reconnect
- Setting category to n/a now moves messages back to Inbox
- *(ui)* Toast messages cannot be closed anymore

### ⚙️ Miscellaneous Tasks

- Add flow-storm for better debugging experience
- Update ring dependencies

## [2025-03-22.0] - 2025-03-22

### 🚀 Features

- Health check interval for IMAP client watcher is configurable
- Show sanitized text next to the original on the email details page
- *(ui)* Add pie charts to statistics pages for better data overview
- Add optional config parameters for the email client

### 🐛 Bug Fixes

- Choose correct text content to train on when text attachments present
- Evict preferences cache after updating a value
- Throw an exception if no config file can be found during startup

### 📚 Documentation

- Fix the link to the Docker image in README
- Add 'features' and 'screenshots' subsections

### 🎨 Styling

- Remove the delete buttons from admin ui

### ⚙️ Miscellaneous Tasks

- Update JRE 23 Docker image

## [2025-02-23.0] - 2025-02-23

### 🚀 Features

- *(ui)* Remove links to half-baked features

## [2025-02-21.0] - 2025-02-21

### 🚀 Features

- *(ui)* Reorganize e-mail lists and data training
- *(ui)* Clean up and visually improve /emails
- *(ui)* Unify input styles on different pages
- *(ui)* Email details page is styled in the new fashion
- [**breaking**] Remove check for training binaries before starting imap client
- Enrich e-mails parsed from an mbox
- Rename the page "watchers" to "connections"
- Create directories on imap servers upon category creation
- Set log level from ui
- *(ui)* Async operations and errors are shown to the user as toast messages

### 🐛 Bug Fixes

- Message ids are now url encoded in the email list
- Exception when moving emails if they could not be categorized
- Save language metadata in detail view and preferences
- Display category as n/a in email views if note set
- N/a no longer listed as a language in admin
- Event loops restart when they fail
- Change wrong order of functions on main
- Compare categorization threshold with the probability correctly
- Restart all event loops after a failure or restart in messaging
- Add new languages to language preferences with the value false
- Confidence is set properly after categorization
