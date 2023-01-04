-- DOMAINS
ALTER TABLE domain_settings ALTER COLUMN enabled DROP DEFAULT;
ALTER TABLE domain_settings CHANGE enabled status TINYINT NOT NULL;
ALTER TABLE domain_settings ALTER COLUMN status SET DEFAULT 1;

-- Table containing domain_admins
-- They are being used by graphql_admin API
CREATE TABLE domain_admins(
     domain VARCHAR(250) NOT NULL,
     pass_details text NOT NULL,
     PRIMARY KEY(domain)
);
