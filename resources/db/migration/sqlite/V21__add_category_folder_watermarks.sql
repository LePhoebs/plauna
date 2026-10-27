-- How far Plauna has looked into each category folder of an account, so the periodic scan only reads
-- the messages that arrived there since (moved in by the user in a mail client, or by Plauna itself).
-- An IMAP UID is only meaningful together with the folder's UIDVALIDITY: when the server reports a
-- different one, the folder was recreated and the scan starts over from its current end.
CREATE TABLE category_folder_watermarks(
       connection_id TEXT NOT NULL REFERENCES connections(id) ON DELETE CASCADE,
       folder TEXT NOT NULL,
       uid_validity INTEGER NOT NULL,
       last_uid INTEGER NOT NULL,
       updated_at INTEGER,
       PRIMARY KEY (connection_id, folder));
