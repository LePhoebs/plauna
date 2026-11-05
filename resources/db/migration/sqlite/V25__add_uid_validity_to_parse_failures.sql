-- A UID only names a message together with the folder's UIDVALIDITY: after a folder was deleted and
-- created again (or restored on the server), the same UID can be another message. A retry compares it.
ALTER TABLE parse_failures ADD COLUMN uid_validity INTEGER;
