-- When the e-mail got its current category (epoch seconds). A category's retention counts from here, so
-- an old e-mail that is filed into the category later still gets the full number of days. NULL - every
-- e-mail categorized on arrival and every row from before this column - falls back to the e-mail's date.
ALTER TABLE metadata ADD COLUMN categorized_at BIGINT;
