CREATE KEYSPACE mam
         WITH REPLICATION = {'class' : 'SimpleStrategy', 'replication_factor': 3};
USE mam;

CREATE TABLE mam_message(
  id bigint,
  user_jid varchar,
  from_jid varchar,
  remote_bare_jid varchar,
  remote_full_jid varchar,
  is_incoming boolean,
  message blob,
  PRIMARY KEY ((user_jid), id));

CREATE INDEX i_mam_message_rem_bare ON mam_message (remote_bare_jid);
CREATE INDEX i_mam_message_rem_full ON mam_message (remote_full_jid);

CREATE TABLE mam_muc_message (
  id bigint,
  room_id int,
  nick_name varchar,
  message blob,
  PRIMARY KEY (room_id, id));
