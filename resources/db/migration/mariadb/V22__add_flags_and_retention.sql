-- A category can mark its e-mails with an Apple Mail flag color and move them to the trash a number of
-- days after their date. expired_at records when Plauna moved an e-mail to the trash for that reason.
ALTER TABLE categories ADD COLUMN flag_color VARCHAR(16);
ALTER TABLE categories ADD COLUMN retention_days INT;
ALTER TABLE metadata ADD COLUMN expired_at BIGINT;
