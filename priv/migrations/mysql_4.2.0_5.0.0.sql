-- MOD_LAST

ALTER TABLE last ADD server varchar(250) NOT NULL DEFAULT '';
ALTER TABLE last DROP PRIMARY KEY, ADD PRIMARY KEY(server, username);

CREATE INDEX i_last_server_seconds ON last (server, seconds);
DROP INDEX i_last_seconds ON last;

-- MOD_PRIVACY

-- Table privacy_default_list
ALTER TABLE privacy_default_list ADD COLUMN server varchar(250) NOT NULL DEFAULT '';
ALTER TABLE privacy_default_list DROP PRIMARY KEY, ADD PRIMARY KEY (server, username);

-- Table privacy_list
ALTER TABLE privacy_list ADD COLUMN server varchar(250) NOT NULL DEFAULT '';
ALTER TABLE privacy_list DROP PRIMARY KEY, ADD PRIMARY KEY (server, username, name);

-- MOD_PRIVATE

ALTER TABLE private_storage ADD server varchar(250) NOT NULL DEFAULT '';
ALTER TABLE private_storage ADD PRIMARY KEY (server, username, namespace);

DROP INDEX i_private_storage_username ON private_storage;
DROP INDEX i_private_storage_username_namespace ON private_storage;

-- MOD_ROSTER

-- Table rosterusers 
ALTER TABLE rosterusers DROP COLUMN `type`, DROP COLUMN subscribe, DROP COLUMN server;

ALTER TABLE rosterusers ADD server varchar(250) NOT NULL DEFAULT '';

CREATE UNIQUE INDEX i_rosteru_server_user_jid ON rosterusers (server, username, jid);
CREATE INDEX i_rosteru_server_user ON rosterusers (server, username);
DROP INDEX i_rosteru_user_jid ON rosterusers;
DROP INDEX i_rosteru_username ON rosterusers;

-- Table rostergroups
ALTER TABLE rostergroups ADD server varchar(250) NOT NULL DEFAULT '';

CREATE INDEX i_rosterg_server_user_jid ON rostergroups (server, username, jid);
DROP INDEX pk_rosterg_user_jid ON rostergroups;

-- Table roster_version
ALTER TABLE roster_version ADD server varchar(250) NOT NULL DEFAULT '';
ALTER TABLE roster_version DROP PRIMARY KEY, ADD PRIMARY KEY (server, username);

-- MOD_MUC_LIGHT
CREATE INDEX i_muc_light_blocking_su USING BTREE ON muc_light_blocking (lserver, luser);
DROP INDEX i_muc_light_blocking ON muc_light_blocking;

-- MOD_INBOX
ALTER TABLE inbox DROP PRIMARY KEY, ADD PRIMARY KEY (lserver, luser, remote_bare_jid);

DROP INDEX i_inbox ON inbox;
CREATE INDEX i_inbox ON inbox(lserver, luser, timestamp);

-- OTHER CHANGES

-- Table users
ALTER TABLE users ADD server varchar(250) NOT NULL DEFAULT '';
ALTER TABLE users DROP PRIMARY KEY, ADD PRIMARY KEY (server, username);

-- Table domain_settings
-- Mapping from domain hostname to host_type.
-- Column id is used for ordering only.
CREATE TABLE domain_settings (
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    domain VARCHAR(250) NOT NULL,
    host_type VARCHAR(250) NOT NULL,
    enabled BOOLEAN NOT NULL DEFAULT true
);

-- Table domain_events
-- A new record is inserted into domain_events, each time
-- domain_settings table is updated.
-- Column id is used for ordering and not related to domain_settings.id.
CREATE TABLE domain_events (
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    domain VARCHAR(250) NOT NULL
);
CREATE INDEX i_domain_events_domain ON domain_events(domain);
