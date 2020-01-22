--
-- ejabberd, Copyright (C) 2002-2011   ProcessOne
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
-- 02111-1307 USA
--

-- Needs MySQL (at least 5.5.14) with innodb back-end
-- See the MongooseIM Database Backends documentation for how to configure
-- MySQL versions 5.5.14 to 5.7.8 to use this schema.
-- Later versions are compatible by default.

CREATE TABLE test_types(
    unicode text CHARACTER SET utf8mb4,
    `binary_data_8k` blob, -- blob has 65k bytes limit
    `binary_data_65k` blob,
    `binary_data_16m` mediumblob, -- mediumblob has 16MB size limit
    `ascii_char` character(1),
    `ascii_string` varchar(250),
    `int32` int,
    `int64` bigint,
    `int8` tinyint,
    `enum_char` ENUM('A','B', 'C'),
    `bool_flag` boolean
);

CREATE TABLE users (
    username varchar(250) PRIMARY KEY,
    password text NOT NULL,
    pass_details text,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;


CREATE TABLE last (
    username varchar(250) PRIMARY KEY,
    seconds int NOT NULL,

    state text NOT NULl
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE INDEX i_last_seconds ON last(seconds);


CREATE TABLE rosterusers (
    username varchar(250) NOT NULL,
    jid varchar(250) NOT NULL,
    nick text NOT NULL,
    subscription character(1) NOT NULL,
    ask character(1) NOT NULL,
    askmessage text NOT NULL,
    server character(1) NOT NULL,
    subscribe text NOT NULL,
    type text,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE UNIQUE INDEX i_rosteru_user_jid ON rosterusers(username(75), jid(75));
CREATE INDEX i_rosteru_username ON rosterusers(username);
CREATE INDEX i_rosteru_jid ON rosterusers(jid);

CREATE TABLE rostergroups (
    username varchar(250) NOT NULL,
    jid varchar(250) NOT NULL,
    grp text NOT NULL
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE INDEX pk_rosterg_user_jid ON rostergroups(username(75), jid(75));


CREATE TABLE vcard (
    username varchar(150),
    server varchar(150),
    vcard mediumtext NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (username, server)
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;


CREATE TABLE vcard_search (
    username varchar(150) NOT NULL,
    lusername varchar(100),
    server varchar(150),
    fn text NOT NULL,
    lfn varchar(250) NOT NULL,
    family text NOT NULL,
    lfamily varchar(250) NOT NULL,
    given text NOT NULL,
    lgiven varchar(250) NOT NULL,
    middle text NOT NULL,
    lmiddle varchar(250) NOT NULL,
    nickname text NOT NULL,
    lnickname varchar(250) NOT NULL,
    bday text NOT NULL,
    lbday varchar(250) NOT NULL,
    ctry text NOT NULL,
    lctry varchar(250) NOT NULL,
    locality text NOT NULL,
    llocality varchar(250) NOT NULL,
    email text NOT NULL,
    lemail varchar(250) NOT NULL,
    orgname text NOT NULL,
    lorgname varchar(250) NOT NULL,
    orgunit text NOT NULL,
    lorgunit varchar(250) NOT NULL,
    PRIMARY KEY (lusername, server)
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE INDEX i_vcard_search_server    ON vcard_search(server);
CREATE INDEX i_vcard_search_lfn       ON vcard_search(lfn);
CREATE INDEX i_vcard_search_lfamily   ON vcard_search(lfamily);
CREATE INDEX i_vcard_search_lgiven    ON vcard_search(lgiven);
CREATE INDEX i_vcard_search_lmiddle   ON vcard_search(lmiddle);
CREATE INDEX i_vcard_search_lnickname ON vcard_search(lnickname);
CREATE INDEX i_vcard_search_lbday     ON vcard_search(lbday);
CREATE INDEX i_vcard_search_lctry     ON vcard_search(lctry);
CREATE INDEX i_vcard_search_llocality ON vcard_search(llocality);
CREATE INDEX i_vcard_search_lemail    ON vcard_search(lemail);
CREATE INDEX i_vcard_search_lorgname  ON vcard_search(lorgname);
CREATE INDEX i_vcard_search_lorgunit  ON vcard_search(lorgunit);

CREATE TABLE privacy_default_list (
    username varchar(250) PRIMARY KEY,
    name varchar(250) NOT NULL
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE TABLE privacy_list (
    username varchar(250) NOT NULL,
    name varchar(250) NOT NULL,
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (username(75), name(75))
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE TABLE privacy_list_data (
    id bigint,
    t character(1) NOT NULL,
    value text NOT NULL,
    action character(1) NOT NULL,
    ord bigint NOT NULL,
    match_all boolean NOT NULL,
    match_iq boolean NOT NULL,
    match_message boolean NOT NULL,
    match_presence_in boolean NOT NULL,
    match_presence_out boolean NOT NULL,
    PRIMARY KEY (id, ord)
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE TABLE private_storage (
    username varchar(250) NOT NULL,
    namespace varchar(250) NOT NULL,
    data text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE INDEX i_private_storage_username USING BTREE ON private_storage(username);
CREATE UNIQUE INDEX i_private_storage_username_namespace USING BTREE ON private_storage(username(75), namespace(75));

-- Not tested in mysql
CREATE TABLE roster_version (
    username varchar(250) PRIMARY KEY,
    version text NOT NULL
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

-- To update from 1.x:
-- ALTER TABLE rosterusers ADD COLUMN askmessage text AFTER ask;
-- UPDATE rosterusers SET askmessage = '';
-- ALTER TABLE rosterusers ALTER COLUMN askmessage SET NOT NULL;
--
-- To update from 2.0.0:
-- ALTER TABLE mam_message ADD COLUMN search_body text;
-- ALTER TABLE mam_muc_message ADD COLUMN search_body text;
--
-- NOTE: A "minified" JID is an encoded form of JID storing only
--       the difference between a user's JID and the real JID.
--       Consult with mod_mam_utils:jid_to_opt_binary/2
--
--       How it works. We produce encoded_jid using information from
--       archive_jid and real_jid:
--       | archive_jid      | real_jid        | encoded_jid
--       | ---------------- | --------------- | --------------------
--       | alice@host       | alice@host      |
--       | alice@host       | alice@host/r1   | /r1
--       | alice@host       | bob@host        | bob
--       | alice@host       | bob@host/r1     | bob/r1
--       | alice@host       | kate@example    | example:kate
--       | alice@host       | kate@example/r1 | example@kate/r1
--
--
-- SIMPLE FORMAT
-- =============
--
-- Differences between old and simple format:
-- - message is stored as XML
-- - remote_bare_jid is not "minified"
-- To enable simple format pass {simple, true} as an option for mod_mam_rdbms_arch
CREATE TABLE mam_message(
  -- Message UID (64 bits)
  -- A server-assigned UID that MUST be unique within the archive.
  id BIGINT UNSIGNED NOT NULL,
  user_id INT UNSIGNED NOT NULL,
  -- FromJID used to form a message without looking into stanza.
  -- This value will be send to the client "as is".
  -- This JID is "minified".
  from_jid varchar(250) CHARACTER SET binary NOT NULL,
  -- The remote JID that the stanza is to (for an outgoing message) or from (for an incoming message).
  -- This field is for sorting and filtering.
  -- This JID is "minified".
  remote_bare_jid varchar(250) CHARACTER SET binary NOT NULL,
  remote_resource varchar(250) CHARACTER SET binary NOT NULL,
  -- I - incoming, remote_jid is a value from From.
  -- O - outgoing, remote_jid is a value from To.
  -- Has no meaning for MUC-rooms.
  direction ENUM('I','O') NOT NULL,
  -- Term-encoded message packet
  -- Don't try to decode it using MySQL tools
  -- Type test_types.binary_data_16m
  message mediumblob NOT NULL,
  search_body mediumtext,
  PRIMARY KEY (user_id, id),
  INDEX i_mam_message_rem USING BTREE (user_id, remote_bare_jid, id)
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC
  ENGINE=InnoDB
  PARTITION BY HASH(user_id)
  PARTITIONS 32;
-- Partition is selected based on MOD(user_id, 32)
-- See for more information
-- http://dev.mysql.com/doc/refman/5.1/en/partitioning-hash.html


CREATE TABLE mam_config(
  user_id INT UNSIGNED NOT NULL,
  -- If empty, than it is a default behaviour.
  remote_jid varchar(250) CHARACTER SET binary NOT NULL,
  -- A - always archive;
  -- N - never archive;
  -- R - roster (only for remote_jid == "")
  behaviour ENUM('A', 'N', 'R') NOT NULL,
  PRIMARY KEY (user_id, remote_jid)
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE TABLE mam_server_user(
  id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  server    varchar(250) CHARACTER SET binary NOT NULL,
  user_name varchar(250) CHARACTER SET binary NOT NULL,
  PRIMARY KEY(id) USING HASH,
  CONSTRAINT uc_mam_server_user_name UNIQUE USING HASH (server, user_name)
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE TABLE mam_muc_message(
  -- Message UID
  -- A server-assigned UID that MUST be unique within the archive.
  id BIGINT UNSIGNED NOT NULL,
  room_id INT UNSIGNED NOT NULL,
  sender_id INT UNSIGNED NOT NULL,
  -- A nick of the message's originator
  nick_name varchar(250) NOT NULL,
  -- Term-encoded message packet
  message mediumblob NOT NULL,
  search_body mediumtext,
  PRIMARY KEY (room_id, id)
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE INDEX i_mam_muc_message_sender_id USING BTREE ON mam_muc_message(sender_id);

CREATE TABLE offline_message(
  id BIGINT UNSIGNED        NOT NULL AUTO_INCREMENT PRIMARY KEY,
  timestamp BIGINT UNSIGNED NOT NULL,
  expire    BIGINT UNSIGNED,
  server    varchar(250)    NOT NULL,
  username  varchar(250)    NOT NULL,
  from_jid  varchar(250)    NOT NULL,
  packet    mediumblob      NOT NULL,
  permanent_fields    mediumblob
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;
CREATE INDEX i_offline_message USING BTREE ON offline_message(server, username, id);

CREATE TABLE muc_light_rooms(
    id BIGINT UNSIGNED      NOT NULL AUTO_INCREMENT,
    luser VARCHAR(250)      NOT NULL,
    lserver VARCHAR(250)    NOT NULL,
    version VARCHAR(20)     NOT NULL,
    PRIMARY KEY (lserver, luser),
    UNIQUE KEY k_id USING HASH (id)
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE INDEX i_muc_light_rooms USING HASH ON muc_light_rooms(id);

CREATE TABLE muc_light_occupants(
    room_id BIGINT UNSIGNED NOT NULL REFERENCES muc_light_rooms(id),
    luser VARCHAR(250)      NOT NULL,
    lserver VARCHAR(250)    NOT NULL,
    aff TINYINT UNSIGNED    NOT NULL
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE INDEX i_muc_light_occupants_id USING HASH ON muc_light_occupants(room_id);
CREATE INDEX i_muc_light_occupants_us USING HASH ON muc_light_occupants(lserver, luser);

CREATE TABLE muc_light_config(
    room_id BIGINT UNSIGNED NOT NULL REFERENCES muc_light_rooms(id),
    opt VARCHAR(100)        NOT NULL,
    val VARCHAR(250)        NOT NULL
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE INDEX i_muc_light_config USING HASH ON muc_light_config(room_id);

CREATE TABLE muc_light_blocking(
    luser VARCHAR(250)      NOT NULL,
    lserver VARCHAR(250)    NOT NULL,
    what TINYINT UNSIGNED   NOT NULL,
    who VARCHAR(500)        NOT NULL
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE INDEX i_muc_light_blocking USING HASH ON muc_light_blocking(luser, lserver);

CREATE TABLE inbox (
    luser VARCHAR(250)               NOT NULL,
    lserver VARCHAR(250)             NOT NULL,
    remote_bare_jid VARCHAR(250)     NOT NULL,
    content blob                     NOT NULL,
    unread_count int                 NOT NULL,
    msg_id varchar(250),
    timestamp BIGINT UNSIGNED        NOT NULL,
    PRIMARY KEY(luser, lserver, remote_bare_jid));

CREATE INDEX i_inbox USING BTREE ON inbox(luser, lserver, timestamp);

CREATE TABLE pubsub_nodes (
    nidx BIGINT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY,
    p_key VARCHAR(250)     NOT NULL,
    name VARCHAR(250)    NOT NULL,
    type VARCHAR(250)    NOT NULL,
    owners JSON          NOT NULL,
    options JSON         NOT NULL
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE UNIQUE INDEX i_pubsub_nodes USING BTREE ON pubsub_nodes(p_key, name);

CREATE TABLE pubsub_node_collections (
    name VARCHAR(250)        NOT NULL,
    parent_name VARCHAR(250) NOT NULL,
    PRIMARY KEY(name, parent_name)
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE TABLE pubsub_affiliations (
    nidx BIGINT UNSIGNED NOT NULL,
    luser VARCHAR(250) NOT NULL,
    lserver VARCHAR(250) NOT NULL,
    aff TINYINT UNSIGNED NOT NULL,
    PRIMARY KEY(luser, lserver(50), nidx)
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE INDEX i_pubsub_affiliations_nidx USING BTREE ON pubsub_affiliations(nidx);

CREATE TABLE pubsub_items (
    nidx BIGINT UNSIGNED NOT NULL,
    itemid VARCHAR(250) NOT NULL,
    created_luser VARCHAR(250) NOT NULL,
    created_lserver VARCHAR(250) NOT NULL,
    created_at BIGINT                   NOT NULL,
    modified_luser VARCHAR(250)         NOT NULL,
    modified_lserver VARCHAR(250)       NOT NULL,
    modified_lresource VARCHAR(250)     NOT NULL,
    modified_at BIGINT                  NOT NULL,
    publisher TEXT,
    payload TEXT                        NOT NULL,
    PRIMARY KEY(nidx, itemid)
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE TABLE pubsub_last_item (
    nidx BIGINT                      NOT NULL,
    itemid VARCHAR(250)              NOT NULL,
    created_luser VARCHAR(250)       NOT NULL,
    created_lserver VARCHAR(250)     NOT NULL,
    created_at BIGINT                NOT NULL,
    payload TEXT                     NOT NULL,
	PRIMARY KEY (nidx)
);

-- we skip luser and lserver in this one as this is little chance (even impossible?)
-- to have itemid duplication for distinct users
CREATE INDEX i_pubsub_items_nidx_itemid USING BTREE ON pubsub_items(nidx);
CREATE INDEX i_pubsub_items_lus_nidx USING BTREE ON pubsub_items(created_luser, created_lserver(50), nidx);


CREATE TABLE pubsub_subscriptions (
    nidx BIGINT UNSIGNED NOT NULL,
    luser VARCHAR(250) NOT NULL,
    lserver VARCHAR(250) NOT NULL,
    lresource VARCHAR(250) NOT NULL,
    type TINYINT UNSIGNED NOT NULL,
    sub_id VARCHAR(125) NOT NULL,
    options JSON NOT NULL
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE INDEX i_pubsub_subscriptions_lus_nidx USING BTREE ON pubsub_subscriptions(luser, lserver(50), nidx);
CREATE INDEX i_pubsub_subscriptions_nidx USING BTREE ON pubsub_subscriptions(nidx);

CREATE TABLE event_pusher_push_subscription (
     owner_jid VARCHAR(250),
     node VARCHAR(250),
     pubsub_jid VARCHAR(250),
     form JSON NOT NULL,
     created_at BIGINT NOT NULL,
     PRIMARY KEY(owner_jid, node, pubsub_jid)
 ) CHARACTER SET utf8mb4
   ROW_FORMAT=DYNAMIC;

CREATE INDEX i_event_pusher_push_subscription ON event_pusher_push_subscription(owner_jid);

CREATE TABLE mongoose_cluster_id (
    k varchar(50) PRIMARY KEY,
    v text
);
