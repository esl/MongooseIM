-- DOMAINS
sp_rename 'domain_settings.enabled', 'status', 'COLUMN';

-- Table containing domain_admins
-- They are being used by graphql_admin API
CREATE TABLE domain_admins(
     domain VARCHAR(250) NOT NULL PRIMARY KEY,
     pass_details NVARCHAR(max) NOT NULL
);
