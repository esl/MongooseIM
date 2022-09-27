-- DOMAINS
ALTER TABLE domain_settings ALTER COLUMN enabled DROP DEFAULT;
ALTER TABLE domain_settings CHANGE enabled status TINYINT NOT NULL;
ALTER TABLE domain_settings ALTER COLUMN status SET DEFAULT 1;
