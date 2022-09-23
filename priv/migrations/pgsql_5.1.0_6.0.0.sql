-- DOMAINS
ALTER TABLE domains_settings ALTER COLUMN enabled DROP DEFAULT;

ALTER TABLE domains_settings
  ALTER COLUMN enabled TYPE SMALLINT USING CASE WHEN enabled THEN 1 ELSE 0 END;

ALTER TABLE domains_settings
  RENAME enabled TO status;

ALTER TABLE domains_settings ALTER COLUMN status SET DEFAULT 1;
