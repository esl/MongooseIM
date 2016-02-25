CREATE KEYSPACE mam
         WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 3};
USE mam;

CREATE TABLE mam_message(
  id bigint,
  user_jid varchar,
  from_jid varchar,
  message blob,
  PRIMARY KEY ((user_jid), id));

CREATE TABLE mam_message_next_index(
  user_jid varchar,
  remote_jid varchar,
  next_index bigint,
  PRIMARY KEY ((user_jid), remote_jid));

CREATE TABLE mam_message_index_to_id(
  id bigint,
  index bigint,
  user_jid varchar,
  remote_jid varchar,
  PRIMARY KEY ((user_jid), remote_jid, index));

CREATE TABLE mam_message_id_to_index(
  id bigint,
  index bigint,
  user_jid varchar,
  remote_jid varchar,
  PRIMARY KEY ((user_jid), remote_jid, id));

CREATE TABLE mam_message_purged(
  from_id bigint,
  to_id bigint,
  user_jid varchar,
  PRIMARY KEY ((user_jid), from_id));

CREATE TABLE mam_muc_message (
  id bigint,
  room_id int,
  nick_name varchar,
  message blob,
  PRIMARY KEY (room_id, id));
