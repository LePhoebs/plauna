-- What the categorization model thinks of every stored e-mail: one row per e-mail and category with the
-- model's probability, written by the "Score e-mails" run of the e-mail list. The list's suggestion
-- filter reads it to find older e-mails that may belong into a category. Rows vanish with their e-mail
-- or category; a new run replaces them.
CREATE TABLE IF NOT EXISTS category_scores (
    message_id VARCHAR(998) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,
    category_id INT NOT NULL,
    confidence DOUBLE NOT NULL,
    PRIMARY KEY (message_id, category_id),
    FOREIGN KEY (message_id) REFERENCES headers(message_id) ON DELETE CASCADE,
    FOREIGN KEY (category_id) REFERENCES categories(id) ON DELETE CASCADE
);
CREATE INDEX IF NOT EXISTS idx_category_scores_category ON category_scores(category_id, confidence);
