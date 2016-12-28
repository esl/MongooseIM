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

-- Needs MySQL (at least 4.0.x) with innodb back-end

CREATE TABLE users (
    username varchar(250) PRIMARY KEY,
    password text NOT NULL,
    pass_details text,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8;


CREATE TABLE last (
    username varchar(250) PRIMARY KEY,
    seconds int NOT NULL,
    state text NOT NULl
) CHARACTER SET utf8;

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
) CHARACTER SET utf8;

CREATE UNIQUE INDEX i_rosteru_user_jid ON rosterusers(username(75), jid(75));
CREATE INDEX i_rosteru_username ON rosterusers(username);
CREATE INDEX i_rosteru_jid ON rosterusers(jid);

CREATE TABLE rostergroups (
    username varchar(250) NOT NULL,
    jid varchar(250) NOT NULL,
    grp text NOT NULL
) CHARACTER SET utf8;

CREATE INDEX pk_rosterg_user_jid ON rostergroups(username(75), jid(75));


CREATE TABLE vcard (
    username varchar(150),
    server varchar(150),
    vcard mediumtext NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (username, server)
) CHARACTER SET utf8;


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
) CHARACTER SET utf8;

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
) CHARACTER SET utf8;

CREATE TABLE privacy_list (
    username varchar(250) NOT NULL,
    name varchar(250) NOT NULL,
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT UNIQUE,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (username(75), name(75))
) CHARACTER SET utf8;

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
) CHARACTER SET utf8;

CREATE TABLE private_storage (
    username varchar(250) NOT NULL,
    namespace varchar(250) NOT NULL,
    data text NOT NULL,
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP
) CHARACTER SET utf8;

CREATE INDEX i_private_storage_username USING BTREE ON private_storage(username);
CREATE UNIQUE INDEX i_private_storage_username_namespace USING BTREE ON private_storage(username(75), namespace(75));

-- Not tested in mysql
CREATE TABLE roster_version (
    username varchar(250) PRIMARY KEY,
    version text NOT NULL
) CHARACTER SET utf8;

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
-- To enable simple format pass {simple, true} as an option for mod_mam_odbc_arch
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
  message blob NOT NULL,
  search_body text,
  PRIMARY KEY (user_id, id),
  INDEX i_mam_message_rem USING BTREE (user_id, remote_bare_jid, id)
)  ENGINE=InnoDB
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
);

CREATE TABLE mam_server_user(
  id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  server    varchar(250) CHARACTER SET binary NOT NULL,
  user_name varchar(250) CHARACTER SET binary NOT NULL,
  PRIMARY KEY(id) USING HASH,
  CONSTRAINT uc_mam_server_user_name UNIQUE USING HASH (server, user_name)
);

CREATE TABLE mam_muc_message(
  -- Message UID
  -- A server-assigned UID that MUST be unique within the archive.
  id BIGINT UNSIGNED NOT NULL,
  room_id INT UNSIGNED NOT NULL,
  -- A nick of the message's originator
  nick_name varchar(250) NOT NULL,
  -- Term-encoded message packet
  message blob NOT NULL,
  search_body text,
  PRIMARY KEY (room_id, id)
);

CREATE TABLE offline_message(
  id BIGINT UNSIGNED        NOT NULL AUTO_INCREMENT PRIMARY KEY,
  timestamp BIGINT UNSIGNED NOT NULL,
  expire    BIGINT UNSIGNED,
  server    varchar(250)    NOT NULL,
  username  varchar(250)    NOT NULL,
  from_jid  varchar(250)    NOT NULL,
  packet    blob            NOT NULL
);
CREATE INDEX i_offline_message USING BTREE ON offline_message(server, username, id);

CREATE TABLE muc_light_rooms(
    id BIGINT UNSIGNED      NOT NULL AUTO_INCREMENT,
    luser VARCHAR(250)      NOT NULL,
    lserver VARCHAR(250)    NOT NULL,
    version VARCHAR(20)     NOT NULL,
    PRIMARY KEY (lserver, luser),
    UNIQUE KEY k_id USING HASH (id)
);

CREATE INDEX i_muc_light_rooms USING HASH ON muc_light_rooms(id);

CREATE TABLE muc_light_occupants(
    room_id BIGINT UNSIGNED NOT NULL REFERENCES muc_light_rooms(id),
    luser VARCHAR(250)      NOT NULL,
    lserver VARCHAR(250)    NOT NULL,
    aff TINYINT UNSIGNED    NOT NULL
);

CREATE INDEX i_muc_light_occupants_id USING HASH ON muc_light_occupants(room_id);
CREATE INDEX i_muc_light_occupants_us USING HASH ON muc_light_occupants(lserver, luser);

CREATE TABLE muc_light_config(
    room_id BIGINT UNSIGNED NOT NULL REFERENCES muc_light_rooms(id),
    opt VARCHAR(100)        NOT NULL,
    val VARCHAR(250)        NOT NULL
);

CREATE INDEX i_muc_light_config USING HASH ON muc_light_config(room_id);

CREATE TABLE muc_light_blocking(
    luser VARCHAR(250)      NOT NULL,
    lserver VARCHAR(250)    NOT NULL,
    what TINYINT UNSIGNED   NOT NULL,
    who VARCHAR(500)        NOT NULL
);

CREATE INDEX i_muc_light_blocking USING HASH ON muc_light_blocking(luser, lserver);

