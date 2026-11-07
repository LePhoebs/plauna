-- The Subject checklist of the e-mail list keeps the subjects with the newest e-mails: it groups by
-- subject and orders by MAX(date). With (subject, date) in one index SQLite reads only the index for
-- that, not every header row. It replaces the subject-only index of V20, whose uses it covers too.
-- (MariaDB stores subjects as TEXT, where only a prefix index is possible, so it gets none.)
CREATE INDEX IF NOT EXISTS idx_headers_subject_date ON headers(subject, date);
DROP INDEX IF EXISTS idx_headers_subject;
