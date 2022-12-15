-- DOMAINS
ALTER TABLE domain_settings ALTER COLUMN enabled DROP DEFAULT;

ALTER TABLE domain_settings
  ALTER COLUMN enabled TYPE SMALLINT USING CASE WHEN enabled THEN 1 ELSE 0 END;

ALTER TABLE domain_settings
  RENAME enabled TO status;

ALTER TABLE domain_settings ALTER COLUMN status SET DEFAULT 1;

-- Table containing domain_admins
-- They are being used by graphql_admin API
CREATE TABLE domain_admins(
     domain VARCHAR(250) NOT NULL,
     pass_details TEXT NOT NULL,
     PRIMARY KEY(domain)
);
