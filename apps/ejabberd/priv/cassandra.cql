CREATE KEYSPACE mam
         WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 3};
USE mam;

CREATE TABLE mam_muc_message (
  id bigint,
  room_id int,
  nick_name varchar,
  message blob,
  PRIMARY KEY (room_id, id));

CREATE TABLE mam_con_message(
  id bigint,
  lower_jid varchar,
  upper_jid varchar,
  is_from_lower boolean,
  message blob,
  PRIMARY KEY ((lower_jid, upper_jid), id));

CREATE TABLE mam_con_user(
  local_jid varchar,
  remote_jid varchar,
  last_message_id bigint,
  PRIMARY KEY (local_jid, remote_jid));

