-- MOD_LAST

ALTER TABLE last ADD server varchar(250);
UPDATE last SET server = 'localhost';
ALTER TABLE last MODIFY COLUMN server varchar(250) NOT NULL;

CREATE INDEX i_last_server_seconds ON last (server, seconds);
DROP INDEX i_last_seconds ON last;

START TRANSACTION;
ALTER TABLE last DROP PRIMARY KEY;
ALTER TABLE last ADD PRIMARY KEY(server, username);
COMMIT;

-- MOD_PRIVACY

-- Table privacy_default_list
ALTER TABLE privacy_default_list ADD COLUMN server varchar(250);
UPDATE privacy_default_list SET server = 'localhost';
ALTER TABLE privacy_default_list MODIFY COLUMN server varchar(250) NOT NULL;

START TRANSACTION;
ALTER TABLE privacy_default_list DROP PRIMARY KEY;
ALTER TABLE privacy_default_list ADD PRIMARY KEY (server, username);
COMMIT;

-- Table privacy_list
ALTER TABLE privacy_list ADD COLUMN server varchar(250) NOT NULL;
UPDATE privacy_list SET server = 'localhost';
ALTER TABLE privacy_list MODIFY COLUMN server varchar(250) NOT NULL;

START TRANSACTION;
ALTER TABLE privacy_list DROP PRIMARY KEY;
ALTER TABLE privacy_list ADD PRIMARY KEY (server, username, name);
COMMIT;

-- MOD_PRIVATE

ALTER TABLE private_storage ADD server varchar(250);
UPDATE private_storage SET server = 'localhost';
ALTER TABLE private_storage MODIFY COLUMN server varchar(250) NOT NULL;

DROP INDEX i_private_storage_username ON private_storage;
DROP INDEX i_private_storage_username_namespace ON private_storage;

ALTER TABLE private_storage ADD PRIMARY KEY (server, username, namespace);

-- MOD_ROSTER

-- Table rosterusers 
ALTER TABLE rosterusers DROP COLUMN `type`;
ALTER TABLE rosterusers DROP COLUMN subscribe;
ALTER TABLE rosterusers DROP COLUMN server;

ALTER TABLE rosterusers ADD server varchar(250);
UPDATE rosterusers SET server = 'localhost';
ALTER TABLE rosterusers MODIFY COLUMN server varchar(250) NOT NULL;

CREATE UNIQUE INDEX i_rosteru_server_user_jid ON rosterusers (server, username, jid);
CREATE INDEX i_rosteru_server_user ON rosterusers (server, username);
DROP INDEX i_rosteru_user_jid ON rosterusers;
DROP INDEX i_rosteru_username ON rosterusers;

-- Table rostergroups
ALTER TABLE rostergroups ADD server varchar(250);
UPDATE rostergroups SET server = 'localhost';
ALTER TABLE rostergroups MODIFY COLUMN server varchar(250) NOT NULL;

CREATE INDEX i_rosterg_server_user_jid ON rostergroups (server, username, jid);
DROP INDEX pk_rosterg_user_jid ON rostergroups;

-- Table roster_version
ALTER TABLE roster_version ADD server varchar(250);
UPDATE roster_version SET server = 'localhost';
ALTER TABLE roster_version MODIFY COLUMN server varchar(250) NOT NULL;

START TRANSACTION;
ALTER TABLE roster_version DROP PRIMARY KEY;
ALTER TABLE roster_version ADD PRIMARY KEY (server, username);
COMMIT;

-- MOD_MUC
CREATE INDEX i_muc_light_blocking_su USING BTREE ON muc_light_blocking (lserver, luser);
DROP INDEX i_muc_light_blocking ON muc_light_blocking;


-- MOD_INBOX
START TRANSACTION;
ALTER TABLE inbox DROP PRIMARY KEY;
ALTER TABLE inbox ADD PRIMARY KEY (lserver, luser, remote_bare_jid);
COMMIT;

CREATE INDEX i_inbox_timestamp ON inbox(lserver, luser, timestamp);
DROP INDEX i_inbox ON inbox;

-- OTHER CHANGES

-- Table users
ALTER TABLE users ADD server varchar(250);
UPDATE users SET server = 'localhost';
ALTER TABLE users MODIFY COLUMN server varchar(250) NOT NULL;

START TRANSACTION;  
ALTER TABLE users DROP PRIMARY KEY;
ALTER TABLE users ADD PRIMARY KEY (server, username);
COMMIT;

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
