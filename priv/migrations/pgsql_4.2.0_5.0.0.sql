-- MOD_LAST

ALTER TABLE last ADD COLUMN server varchar(250) NOT NULL DEFAULT'';
ALTER TABLE last DROP CONSTRAINT last_pkey, ADD PRIMARY KEY (server, username);

CREATE INDEX i_last_server_seconds ON last (server, seconds);
DROP INDEX i_last_seconds;

-- MOD_PRIVACY

-- Table privacy_default_list
ALTER TABLE privacy_default_list ADD COLUMN server varchar(250) NOT NULL DEFAULT '';
ALTER TABLE privacy_default_list DROP CONSTRAINT privacy_default_list_pkey, ADD PRIMARY KEY (server, username);

-- Table privacy_list
ALTER TABLE privacy_list ADD COLUMN server varchar(250) NOT NULL DEFAULT '';
ALTER TABLE privacy_list DROP CONSTRAINT privacy_list_pkey, ADD PRIMARY KEY (server, username, name);

-- MOD_PRIVATE

-- Table private_storage
ALTER TABLE private_storage ADD server varchar(250) NOT NULL DEFAULT '';
ALTER TABLE private_storage ADD PRIMARY KEY (server, username, namespace);

DROP INDEX i_private_storage_username;
DROP INDEX i_private_storage_username_namespace;

-- MOD_ROSTER

--Table rosterusers
ALTER TABLE rosterusers DROP COLUMN "type", DROP COLUMN subscribe, DROP COLUMN server;
ALTER TABLE rosterusers ADD COLUMN server varchar(250) NOT NULL DEFAULT '';

CREATE UNIQUE INDEX i_rosteru_server_user_jid ON rosterusers (server, username, jid);
CREATE INDEX i_rosteru_server_user ON rosterusers (server, username);

DROP INDEX i_rosteru_user_jid;
DROP INDEX i_rosteru_username;

--Table rostergroups 
ALTER TABLE rostergroups ADD COLUMN server varchar(250) NOT NULL DEFAULT '';
CREATE INDEX i_rosterg_server_user_jid ON rostergroups (server, username, jid);
DROP INDEX pk_rosterg_user_jid;

-- Table roster_version
ALTER TABLE roster_version ADD server varchar(250) NOT NULL DEFAULT '';
ALTER TABLE roster_version DROP CONSTRAINT roster_version_pkey, ADD PRIMARY KEY (server, username);

-- MOD_MUC_LIGHT

CREATE INDEX i_muc_light_blocking_su ON muc_light_blocking (lserver, luser);
DROP INDEX i_muc_light_blocking;

-- MOD_INBOX

ALTER TABLE inbox DROP CONSTRAINT inbox_pkey, ADD PRIMARY KEY (lserver, luser, remote_bare_jid);
CREATE INDEX i_inbox_timestamp ON inbox USING BTREE (lserver, luser, timestamp);
DROP INDEX i_inbox;

-- OTHER CHANGES

-- Table users
ALTER TABLE users ADD COLUMN server varchar(250) NOT NULL DEFAULT '';
ALTER TABLE users DROP CONSTRAINT users_pkey, ADD PRIMARY KEY (server, username);

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
