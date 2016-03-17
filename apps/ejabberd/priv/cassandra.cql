CREATE KEYSPACE mongooseim
         WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 3};
USE mongooseim;

CREATE TABLE mam_message(
  id bigint,
  user_jid varchar,
  from_jid varchar,
  // full remote bare jid
  remote_jid varchar,
  // One of:
  // - remote full jid
  // - remote bare jid
  // - empty
  with_jid varchar,
  message blob,
  PRIMARY KEY ((user_jid), with_jid, id));

CREATE TABLE mam_muc_message(
  id bigint,
  room_jid varchar,
  nick_name varchar,
  // One of:
  // - nick_name
  // - empty
  with_nick varchar,
  message blob,
  PRIMARY KEY ((room_jid), with_nick, id));

CREATE TABLE private_storage (
  username varchar,
  namespace varchar,
  data text,
  PRIMARY KEY ((username), namespace));
