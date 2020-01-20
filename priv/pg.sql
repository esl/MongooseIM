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

CREATE TYPE test_enum_char AS ENUM('A','B', 'C');
CREATE TABLE test_types(
    unicode text,
    binary_data_8k bytea, -- byte a has 1 GB limit
    binary_data_65k bytea,
    binary_data_16m bytea,
    ascii_char character(1),
    ascii_string varchar(250),
    int32 integer,
    int64 bigint,
    int8 smallint, -- has no tinyint, so the next one is 2-bytes smallint
    enum_char test_enum_char,
    bool_flag boolean
);

CREATE TABLE users (
    username varchar(250) PRIMARY KEY,
    "password" text NOT NULL,
    pass_details text,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);


CREATE TABLE last (
    username varchar(250) PRIMARY KEY,
    seconds integer NOT NULL,
    state text NOT NULL
);

CREATE INDEX i_last_seconds ON last USING btree (seconds);


CREATE TABLE rosterusers (
    username varchar(250) NOT NULL,
    jid text NOT NULL,
    nick text NOT NULL,
    subscription character(1) NOT NULL,
    ask character(1) NOT NULL,
    askmessage text NOT NULL,
    server character(1) NOT NULL,
    subscribe text,
    "type" text,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE UNIQUE INDEX i_rosteru_user_jid ON rosterusers USING btree (username, jid);
CREATE INDEX i_rosteru_username ON rosterusers USING btree (username);
CREATE INDEX i_rosteru_jid ON rosterusers USING btree (jid);


CREATE TABLE rostergroups (
    username varchar(250) NOT NULL,
    jid text NOT NULL,
    grp text NOT NULL
);

CREATE INDEX pk_rosterg_user_jid ON rostergroups USING btree (username, jid);


CREATE TABLE vcard (
    username varchar(150),
    server varchar(100),
    vcard text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now(),
    PRIMARY KEY (server, username)
);

CREATE TABLE vcard_search (
    username varchar(150) NOT NULL,
    lusername varchar(100),
    server varchar(250),
    fn text NOT NULL,
    lfn text NOT NULL,
    family text NOT NULL,
    lfamily text NOT NULL,
    given text NOT NULL,
    lgiven text NOT NULL,
    middle text NOT NULL,
    lmiddle text NOT NULL,
    nickname text NOT NULL,
    lnickname text NOT NULL,
    bday text NOT NULL,
    lbday text NOT NULL,
    ctry text NOT NULL,
    lctry text NOT NULL,
    locality text NOT NULL,
    llocality text NOT NULL,
    email text NOT NULL,
    lemail text NOT NULL,
    orgname text NOT NULL,
    lorgname text NOT NULL,
    orgunit text NOT NULL,
    lorgunit text NOT NULL,
    PRIMARY KEY (server, lusername)
);

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
    name text NOT NULL
);

CREATE TABLE privacy_list (
    username varchar(250) NOT NULL,
    name text NOT NULL,
    id SERIAL UNIQUE,
    created_at TIMESTAMP NOT NULL DEFAULT now(),
    PRIMARY KEY (username,name)
);

CREATE TABLE privacy_list_data (
    id bigint REFERENCES privacy_list(id) ON DELETE CASCADE,
    t character(1) NOT NULL,
    value text NOT NULL,
    action character(1) NOT NULL,
    ord NUMERIC NOT NULL,
    match_all boolean NOT NULL,
    match_iq boolean NOT NULL,
    match_message boolean NOT NULL,
    match_presence_in boolean NOT NULL,
    match_presence_out boolean NOT NULL,
    PRIMARY KEY (id, ord)
);

CREATE TABLE private_storage (
    username varchar(250) NOT NULL,
    namespace text NOT NULL,
    data text NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT now()
);

CREATE INDEX i_private_storage_username ON private_storage USING btree (username);
CREATE UNIQUE INDEX i_private_storage_username_namespace ON private_storage USING btree (username, namespace);


CREATE TABLE roster_version (
    username varchar(250) PRIMARY KEY,
    version text NOT NULL
);

-- To update from 0.9.8:
-- CREATE SEQUENCE spool_seq_seq;
-- ALTER TABLE spool ADD COLUMN seq integer;
-- ALTER TABLE spool ALTER COLUMN seq SET DEFAULT nextval('spool_seq_seq');
-- UPDATE spool SET seq = DEFAULT;
-- ALTER TABLE spool ALTER COLUMN seq SET NOT NULL;

-- To update from 1.x:
-- ALTER TABLE rosterusers ADD COLUMN askmessage text;
-- UPDATE rosterusers SET askmessage = '';
-- ALTER TABLE rosterusers ALTER COLUMN askmessage SET NOT NULL;

-- To update from 2.0.0:
-- ALTER TABLE mam_message ADD COLUMN search_body text;
-- ALTER TABLE mam_muc_message ADD COLUMN search_body text;

CREATE TYPE mam_behaviour AS ENUM('A', 'N', 'R');
CREATE TYPE mam_direction AS ENUM('I','O');

CREATE TABLE mam_message(
  -- Message UID (64 bits)
  -- A server-assigned UID that MUST be unique within the archive.
  id BIGINT NOT NULL,
  user_id INT NOT NULL,
  -- FromJID used to form a message without looking into stanza.
  -- This value will be send to the client "as is".
  from_jid varchar(250) NOT NULL,
  -- The remote JID that the stanza is to (for an outgoing message) or from (for an incoming message).
  -- This field is for sorting and filtering.
  remote_bare_jid varchar(250) NOT NULL,
  remote_resource varchar(250) NOT NULL,
  -- I - incoming, remote_jid is a value from From.
  -- O - outgoing, remote_jid is a value from To.
  -- Has no meaning for MUC-rooms.
  direction mam_direction NOT NULL,
  -- Term-encoded message packet
  message bytea NOT NULL,
  search_body text,
  PRIMARY KEY(user_id, id)
);
CREATE INDEX i_mam_message_username_jid_id
    ON mam_message
    USING BTREE
    (user_id, remote_bare_jid, id);

CREATE TABLE mam_config(
  user_id INT NOT NULL,
  -- If empty, than it is a default behaviour.
  remote_jid varchar(250) NOT NULL,
  -- A - always archive;
  -- N - never archive;
  -- R - roster (only for remote_jid == "")
  behaviour mam_behaviour NOT NULL,
  PRIMARY KEY(user_id, remote_jid)
);

CREATE TABLE mam_server_user(
  id SERIAL UNIQUE PRIMARY KEY,
  server    varchar(250) NOT NULL,
  user_name varchar(250) NOT NULL
);
CREATE UNIQUE INDEX i_mam_server_user_name
    ON mam_server_user
    USING BTREE
    (server, user_name);


CREATE TABLE mam_muc_message(
  -- Message UID
  -- A server-assigned UID that MUST be unique within the archive.
  id BIGINT NOT NULL,
  room_id INT NOT NULL,
  sender_id INT NOT NULL,
  -- A nick of the message's originator
  nick_name varchar(250) NOT NULL,
  -- Term-encoded message packet
  message bytea NOT NULL,
  search_body text,
  PRIMARY KEY (room_id, id)
);

CREATE INDEX i_mam_muc_message_sender_id ON mam_muc_message USING BTREE (sender_id);

CREATE TABLE offline_message(
    id SERIAL UNIQUE PRIMARY Key,
    timestamp BIGINT NOT NULL,
    expire    BIGINT,
    server    varchar(250)    NOT NULL,
    username  varchar(250)    NOT NULL,
    from_jid  varchar(250)    NOT NULL,
    packet    text            NOT NULL,
    permanent_fields bytea
);
CREATE INDEX i_offline_message
    ON offline_message
    USING BTREE
    (server, username, id);

CREATE TABLE auth_token(
    owner   TEXT    NOT NULL PRIMARY KEY,
    seq_no  BIGINT  NOT NULL
);

CREATE TABLE muc_light_rooms(
    id BIGSERIAL            NOT NULL UNIQUE,
    luser VARCHAR(250)      NOT NULL,
    lserver VARCHAR(250)    NOT NULL,
    version VARCHAR(20)     NOT NULL,
    PRIMARY KEY (lserver, luser)
);

CREATE TABLE muc_light_occupants(
    room_id BIGINT          NOT NULL REFERENCES muc_light_rooms(id),
    luser VARCHAR(250)      NOT NULL,
    lserver VARCHAR(250)    NOT NULL,
    aff SMALLINT            NOT NULL
);

CREATE INDEX i_muc_light_occupants_id ON muc_light_occupants (room_id);
CREATE INDEX i_muc_light_occupants_us ON muc_light_occupants (lserver, luser);

CREATE TABLE muc_light_config(
    room_id BIGINT          NOT NULL REFERENCES muc_light_rooms(id),
    opt VARCHAR(100)        NOT NULL,
    val VARCHAR(250)        NOT NULL
);

CREATE INDEX i_muc_light_config ON muc_light_config (room_id);

CREATE TABLE muc_light_blocking(
    luser VARCHAR(250)      NOT NULL,
    lserver VARCHAR(250)    NOT NULL,
    what SMALLINT           NOT NULL,
    who VARCHAR(500)        NOT NULL
);

CREATE INDEX i_muc_light_blocking ON muc_light_blocking (luser, lserver);

CREATE TABLE inbox (
    luser VARCHAR(250)               NOT NULL,
    lserver VARCHAR(250)             NOT NULL,
    remote_bare_jid VARCHAR(250)     NOT NULL,
    content bytea                    NOT NULL,
    unread_count int                 NOT NULL,
    msg_id varchar(250),
    timestamp BIGINT                 NOT NULL,
    PRIMARY KEY(luser, lserver, remote_bare_jid));

CREATE INDEX i_inbox
    ON inbox
    USING BTREE(luser, lserver, timestamp);

CREATE TABLE pubsub_nodes (
    nidx               BIGSERIAL PRIMARY KEY,
    p_key VARCHAR(250) NOT NULL,
    name VARCHAR(250)  NOT NULL,
    type VARCHAR(250)  NOT NULL,
    owners JSON        NOT NULL,
    options JSON       NOT NULL
);

CREATE UNIQUE INDEX i_pubsub_nodes_key_name ON pubsub_nodes USING btree (p_key, name);

CREATE TABLE pubsub_node_collections (
    name VARCHAR(250)        NOT NULL,
    parent_name VARCHAR(250) NOT NULL,
    PRIMARY KEY(name, parent_name)
);

CREATE TABLE pubsub_affiliations (
    nidx BIGINT             NOT NULL,
    luser VARCHAR(250)      NOT NULL,
    lserver VARCHAR(250)    NOT NULL,
    aff SMALLINT            NOT NULL,
    PRIMARY KEY(luser, lserver, nidx)
);

CREATE INDEX i_pubsub_affiliations_nidx ON pubsub_affiliations(nidx);

CREATE TABLE pubsub_items (
    nidx BIGINT                         NOT NULL,
    itemid VARCHAR(250)                 NOT NULL,
    created_luser VARCHAR(250)          NOT NULL,
    created_lserver VARCHAR(250)        NOT NULL,
    created_at BIGINT                   NOT NULL,
    modified_luser VARCHAR(250)         NOT NULL,
    modified_lserver VARCHAR(250)       NOT NULL,
    modified_lresource VARCHAR(250)     NOT NULL,
    modified_at BIGINT                  NOT NULL,
    publisher TEXT,
    payload TEXT                        NOT NULL,
    PRIMARY KEY(nidx, itemid)
);

CREATE TABLE pubsub_last_item (
    nidx                BIGINT       NOT NULL,
    itemid              VARCHAR(250) NOT NULL,
    created_luser       VARCHAR(250) NOT NULL,
    created_lserver     VARCHAR(250) NOT NULL,
    created_at          BIGINT       NOT NULL,
    payload             TEXT         NOT NULL,
	PRIMARY KEY (nidx)
);

-- we skip luser and lserver in this one as this is little chance (even impossible?)
-- to have itemid duplication for distinct users
CREATE INDEX i_pubsub_items_lus_nidx ON pubsub_items(created_luser, created_lserver, nidx);
CREATE INDEX i_pubsub_items_nidx ON pubsub_items(nidx);


CREATE TABLE pubsub_subscriptions (
    nidx BIGINT             NOT NULL,
    luser VARCHAR(250)      NOT NULL,
    lserver VARCHAR(250)    NOT NULL,
    lresource VARCHAR(250)  NOT NULL,
    type SMALLINT           NOT NULL,
    sub_id VARCHAR(125)     NOT NULL,
    options JSON            NOT NULL
);

CREATE INDEX i_pubsub_subscriptions_lus_nidx ON pubsub_subscriptions(luser, lserver, nidx);
CREATE INDEX i_pubsub_subscriptions_nidx ON pubsub_subscriptions(nidx);

CREATE TABLE event_pusher_push_subscription (
     owner_jid VARCHAR(250),
     node VARCHAR(250),
     pubsub_jid VARCHAR(250),
     form JSON NOT NULL,
     created_at BIGINT NOT NULL,
     PRIMARY KEY(owner_jid, node, pubsub_jid)
 );

CREATE INDEX i_event_pusher_push_subscription ON event_pusher_push_subscription(owner_jid);

CREATE TABLE mongoose_cluster_id (
    k varchar(50) PRIMARY KEY,
    v text
);
