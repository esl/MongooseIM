-- MOD_LAST

ALTER TABLE last ADD server [nvarchar](250);
-- FIXME Replace localhost with your domain name
UPDATE last SET server = 'localhost';
ALTER TABLE last ALTER COLUMN server [nvarchar](250) NOT NULL;

BEGIN TRANSACTION;
ALTER TABLE last DROP CONSTRAINT PK_last_username;
ALTER TABLE last ADD CONSTRAINT PK_last_username PRIMARY KEY CLUSTERED (server ASC, username ASC);
COMMIT;

CREATE INDEX i_last_server_seconds ON last (server, seconds);

-- MOD_PRIVACY

--Table privacy_default_list
ALTER TABLE privacy_default_list ADD server [nvarchar](250);
-- FIXME Replace localhost with your domain name
UPDATE privacy_default_list SET server = 'localhost';
ALTER TABLE privacy_default_list ALTER COLUMN server [nvarchar](250) NOT NULL;

BEGIN TRANSACTION;
ALTER TABLE privacy_default_list DROP CONSTRAINT PK_privacy_default_list_username;
ALTER TABLE privacy_default_list ADD CONSTRAINT PK_privacy_default_list_username PRIMARY KEY CLUSTERED (server ASC, username ASC);
COMMIT;

--Table privacy_list
ALTER TABLE privacy_list ADD server [nvarchar](250);
-- FIXME Replace localhost with your domain name
UPDATE privacy_list SET server = 'localhost';
ALTER TABLE privacy_list ALTER COLUMN server [nvarchar](250) NOT NULL;

BEGIN TRANSACTION;
ALTER TABLE privacy_list DROP CONSTRAINT privacy_list$id;
ALTER TABLE privacy_list ADD CONSTRAINT PK_privacy_list PRIMARY KEY CLUSTERED (server ASC, username ASC, name ASC);
COMMIT;

-- MOD_PRIVATE

ALTER TABLE private_storage ADD server [nvarchar](250);
-- FIXME Replace localhost with your domain name
UPDATE private_storage SET server = 'localhost';
ALTER TABLE private_storage ALTER COLUMN server [nvarchar](250) NOT NULL;

BEGIN TRANSACTION;
ALTER TABLE private_storage DROP CONSTRAINT private_storage$i_private_storage_username_namespace;
ALTER TABLE private_storage ADD CONSTRAINT PK_private_storage PRIMARY KEY CLUSTERED (server ASC, username ASC, namespace ASC);
COMMIT;

-- MOD_ROSTER

--Table rosterusers
ALTER TABLE rosterusers DROP COLUMN type;
ALTER TABLE rosterusers DROP COLUMN subscribe;
ALTER TABLE rosterusers DROP COLUMN server;

ALTER TABLE rosterusers ADD server [nvarchar](250);
-- FIXME Replace localhost with your domain name
UPDATE rosterusers SET server = 'localhost';
ALTER TABLE rosterusers ALTER COLUMN server [nvarchar](250) NOT NULL;

BEGIN TRANSACTION;
ALTER TABLE rosterusers DROP CONSTRAINT rosterusers$i_rosteru_user_jid;
ALTER TABLE rosterusers ADD CONSTRAINT rosterusers$i_rosteru_server_user_jid UNIQUE CLUSTERED (server ASC, username ASC, jid ASC);
COMMIT;

CREATE INDEX i_rosteru_server_user ON rosterusers (server, username)
CREATE INDEX i_rosteru_jid ON rosterusers (jid)

--Table rostergroups
ALTER TABLE rostergroups ADD server [nvarchar](250);
-- FIXME Replace localhost with your domain name
UPDATE rostergroups SET server = 'localhost';
ALTER TABLE rostergroups ALTER COLUMN server [nvarchar](250) NOT NULL;

CREATE INDEX i_rosterg_server_user_jid ON rostergroups (server, username, jid);

--Table roster_version
ALTER TABLE roster_version ADD server [nvarchar](250);
-- FIXME Replace localhost with your domain name
UPDATE roster_version SET server = 'localhost';
ALTER TABLE roster_version ALTER COLUMN server [nvarchar](250) NOT NULL;

BEGIN TRANSACTION;
ALTER TABLE roster_version DROP CONSTRAINT PK_roster_version_username;
ALTER TABLE roster_version ADD CONSTRAINT PK_roster_version_server_user PRIMARY KEY CLUSTERED (server ASC, username ASC);
COMMIT;

-- MOD_MUC_LIGHT 

CREATE INDEX i_muc_light_blocking_su ON muc_light_blocking (lserver, luser);
DROP INDEX i_muc_light_blocking ON muc_light_blocking;

-- MOD_INBOX

CREATE INDEX i_inbox_su_ts ON inbox(lserver, luser, timestamp);
DROP INDEX i_inbox_ts ON inbox;

BEGIN TRANSACTION;
ALTER TABLE inbox DROP CONSTRAINT PK_inbox;
ALTER TABLE inbox ADD CONSTRAINT PK_inbox PRIMARY KEY CLUSTERED (lserver ASC, luser ASC, remote_bare_jid ASC);
COMMIT;

-- OTHER CHANGES  

--Table users
ALTER TABLE users ADD server [nvarchar](250);
-- FIXME Replace localhost with your domain name
UPDATE users SET server = 'localhost';
ALTER TABLE users ALTER COLUMN server [nvarchar](250) NOT NULL;

BEGIN TRANSACTION;
ALTER TABLE users DROP CONSTRAINT PK_users_username;
ALTER TABLE users ADD CONSTRAINT PK_users_username PRIMARY KEY CLUSTERED (server ASC, username ASC);
COMMIT;

-- Table domain_settings
-- Mapping from domain hostname to host_type.
-- Column id is used for ordering only.
CREATE TABLE domain_settings (
    id BIGINT IDENTITY(1,1) PRIMARY KEY,
    domain VARCHAR(250) NOT NULL,
    host_type VARCHAR(250) NOT NULL,
    enabled SMALLINT NOT NULL DEFAULT 1
);

-- Table domain_events
-- A new record is inserted into domain_events, each time
-- domain_settings table is updated.
-- Column id is used for ordering and not related to domain_settings.id.
CREATE TABLE domain_events (
    id BIGINT IDENTITY(1,1) PRIMARY KEY,
    domain VARCHAR(250) NOT NULL
);
CREATE INDEX i_domain_events_domain ON domain_events(domain);
