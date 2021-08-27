-- MOD_LAST

ALTER TABLE last ADD COLUMN server varchar(250);
UPDATE last SET server = 'localhost';
ALTER TABLE last ALTER COLUMN server SET NOT NULL;

CREATE UNIQUE INDEX i_last_su ON last (server, username);
CREATE INDEX i_last_server_seconds ON last (server, seconds);
DROP INDEX i_last_seconds;

BEGIN;
ALTER TABLE last DROP CONSTRAINT last_pkey;
ALTER TABLE last ADD  CONSTRAINT last_pkey PRIMARY KEY USING INDEX i_last_su;
COMMIT;

-- MOD_PRIVACY

-- Table privacy_default_list
ALTER TABLE privacy_default_list ADD COLUMN server varchar(250);
UPDATE privacy_default_list SET server = 'localhost';
ALTER TABLE privacy_default_list ALTER COLUMN server SET NOT NULL;

CREATE UNIQUE INDEX i_privacy_default_list_su ON privacy_default_list (server, username);

BEGIN;
ALTER TABLE privacy_default_list DROP CONSTRAINT privacy_default_list_pkey;
ALTER TABLE privacy_default_list ADD CONSTRAINT privacy_default_list_pkey PRIMARY KEY USING INDEX i_privacy_default_list_su;
COMMIT;

-- Table privacy_list
ALTER TABLE privacy_list ADD COLUMN server varchar(250);
UPDATE privacy_list SET server = 'localhost';
ALTER TABLE privacy_list ALTER COLUMN server SET NOT NULL;

CREATE UNIQUE INDEX i_privacy_list_sun ON privacy_list (server, username, name);

BEGIN;
ALTER TABLE privacy_list DROP CONSTRAINT privacy_list_pkey;
ALTER TABLE privacy_list ADD CONSTRAINT privacy_list_pkey PRIMARY KEY USING INDEX i_privacy_list_sun;
COMMIT;

-- MOD_PRIVATE

ALTER TABLE private_storage ADD server varchar(250);
UPDATE private_storage SET server = 'localhost';
ALTER TABLE private_storage ALTER COLUMN server SET NOT NULL;

CREATE UNIQUE INDEX i_private_storage_sun ON private_storage (server, username, namespace);

DROP INDEX i_private_storage_username;
DROP INDEX i_private_storage_username_namespace;

ALTER TABLE private_storage ADD CONSTRAINT private_storage_pkey PRIMARY KEY USING INDEX i_private_storage_sun;

-- MOD_ROASTER

--Table rosterusers
ALTER TABLE rosterusers DROP COLUMN "type";
ALTER TABLE rosterusers DROP COLUMN subscribe;
ALTER TABLE rosterusers DROP COLUMN server;

ALTER TABLE rosterusers ADD COLUMN server varchar(250);
UPDATE rosterusers SET server = 'localhost';
ALTER TABLE rosterusers ALTER COLUMN server SET NOT NULL;

CREATE UNIQUE INDEX i_rosteru_server_user_jid ON rosterusers (server, username, jid);
CREATE INDEX i_rosteru_server_user ON rosterusers (server, username);
DROP INDEX i_rosteru_user_jid;
DROP INDEX i_rosteru_username;

--Table rostergroups 
ALTER TABLE rostergroups ADD COLUMN server varchar(250);
UPDATE rostergroups SET server = 'localhost';
ALTER TABLE rostergroups ALTER COLUMN server SET NOT NULL;

CREATE INDEX i_rosterg_server_user_jid ON rostergroups (server, username, jid);
DROP INDEX pk_rosterg_user_jid;

-- Table roster_version
ALTER TABLE roster_version ADD server varchar(250);
UPDATE roster_version SET server = 'localhost';
ALTER TABLE roster_version ALTER COLUMN server SET NOT NULL;

CREATE UNIQUE INDEX i_roster_version_su ON roster_version (server, username);

BEGIN;
ALTER TABLE roster_version DROP CONSTRAINT roster_version_pkey;
ALTER TABLE roster_version ADD CONSTRAINT roster_version_pkey PRIMARY KEY USING INDEX i_roster_version_su;
COMMIT;

-- MOD_MUC

CREATE INDEX i_muc_light_blocking_su ON muc_light_blocking(lserver, luser);
DROP INDEX i_muc_light_blocking;

-- MOD_INBOX

CREATE UNIQUE INDEX i_inbox_sur ON inbox (lserver, luser, remote_bare_jid);

BEGIN;
ALTER TABLE inbox DROP CONSTRAINT inbox_pkey;
ALTER TABLE inbox ADD CONSTRAINT inbox_pkey PRIMARY KEY USING INDEX i_inbox_sur;
COMMIT;

CREATE INDEX i_inbox_timestamp ON inbox USING BTREE(lserver, luser, timestamp);
DROP INDEX i_inbox;

-- OTHER CHANGES

-- Table users
ALTER TABLE users ADD COLUMN server varchar(250);
UPDATE users SET server = 'localhost';
ALTER TABLE users ALTER COLUMN server SET NOT NULL;

CREATE UNIQUE INDEX i_users_su ON users (server, username);

BEGIN;
ALTER TABLE users DROP CONSTRAINT users_pkey;
ALTER TABLE users ADD CONSTRAINT users_pkey PRIMARY KEY USING INDEX i_users_su;
COMMIT;

-- Table domain_settings
-- Mapping from domain hostname to host_type.
-- Column id is used for ordering only.
CREATE TABLE domain_settings (
    id BIGSERIAL NOT NULL UNIQUE,
    domain VARCHAR(250) NOT NULL,
    host_type VARCHAR(250) NOT NULL,
    enabled BOOLEAN NOT NULL DEFAULT true,
    PRIMARY KEY(domain)
);

-- Table domain_events 
-- A new record is inserted into domain_events, each time
-- domain_settings table is updated: i.e. when a domain is removed,
-- inserted, enabled or disabled.
-- Column id is used for ordering and not related to domain_settings.id.
CREATE TABLE domain_events (
    id BIGSERIAL NOT NULL,
    domain VARCHAR(250) NOT NULL,
    PRIMARY KEY(id)
);
CREATE INDEX i_domain_events_domain ON domain_events(domain);
