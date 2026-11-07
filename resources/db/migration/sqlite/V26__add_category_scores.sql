-- What the categorization model thinks of every stored e-mail: one row per e-mail and category with the
-- model's probability, written by the "Score e-mails" run of the e-mail list. The list's suggestion
-- filter reads it to find older e-mails that may belong into a category. Rows vanish with their e-mail
-- or category; a new run replaces them.
CREATE TABLE category_scores(
       message_id TEXT NOT NULL REFERENCES headers(message_id) ON DELETE CASCADE,
       category_id INTEGER NOT NULL REFERENCES categories(id) ON DELETE CASCADE,
       confidence REAL NOT NULL,
       PRIMARY KEY (message_id, category_id));
CREATE INDEX IF NOT EXISTS idx_category_scores_category ON category_scores(category_id, confidence);
