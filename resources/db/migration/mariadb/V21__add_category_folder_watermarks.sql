-- How far Plauna has looked into each category folder of an account, so the periodic scan only reads
-- the messages that arrived there since (moved in by the user in a mail client, or by Plauna itself).
-- An IMAP UID is only meaningful together with the folder's UIDVALIDITY: when the server reports a
-- different one, the folder was recreated and the scan starts over from its current end.
-- VARCHAR(998) ascii_bin matches connections.id; folder is limited so the primary key fits InnoDB's
-- 3072-byte index limit.
CREATE TABLE IF NOT EXISTS category_folder_watermarks (
    connection_id VARCHAR(998) CHARACTER SET ascii COLLATE ascii_bin NOT NULL,
    folder VARCHAR(500) NOT NULL,
    uid_validity BIGINT NOT NULL,
    last_uid BIGINT NOT NULL,
    updated_at BIGINT,
    PRIMARY KEY (connection_id, folder),
    FOREIGN KEY (connection_id) REFERENCES connections(id) ON DELETE CASCADE
);
