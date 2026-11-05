-- The connection page counts the e-mails per folder and the category folder scan lists the e-mails of
-- one folder; both filter metadata by account and folder and read the whole table without this index.
-- folder is TEXT, so it is indexed by a prefix (255 characters cover every real folder path).
CREATE INDEX IF NOT EXISTS idx_metadata_connection_folder ON metadata(connection_id, folder(255));
