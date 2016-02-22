CREATE TABLE mam_message_00(
  -- Message UID (64 bits)
  -- A server-assigned UID that MUST be unique within the archive.
  id BIGINT UNSIGNED NOT NULL,
  user_id INT UNSIGNED NOT NULL,
  -- FromJID used to form a message without looking into stanza.
  -- This value will be send to the client "as is".
  from_jid varchar(250) CHARACTER SET binary NOT NULL,
  -- The remote JID that the stanza is to (for an outgoing message) or from (for an incoming message).
  -- This field is for sorting and filtering.
  remote_bare_jid varchar(250) CHARACTER SET binary NOT NULL,
  remote_resource varchar(250) CHARACTER SET binary NOT NULL,
  -- I - incoming, remote_jid is a value from From.
  -- O - outgoing, remote_jid is a value from To.
  -- Has no meaning for MUC-rooms.
  direction ENUM('I','O') NOT NULL,
  -- Term-encoded message packet
  message blob NOT NULL,
  PRIMARY KEY (user_id, id),
  INDEX i_mam_message_rem USING BTREE (user_id, remote_bare_jid, id)
)  ENGINE=InnoDB;

CREATE TABLE mam_message_01 LIKE mam_message_00;
CREATE TABLE mam_message_02 LIKE mam_message_00;
CREATE TABLE mam_message_03 LIKE mam_message_00;
CREATE TABLE mam_message_04 LIKE mam_message_00;
CREATE TABLE mam_message_05 LIKE mam_message_00;
CREATE TABLE mam_message_06 LIKE mam_message_00;
CREATE TABLE mam_message_07 LIKE mam_message_00;
CREATE TABLE mam_message_08 LIKE mam_message_00;
CREATE TABLE mam_message_09 LIKE mam_message_00;
CREATE TABLE mam_message_10 LIKE mam_message_00;
CREATE TABLE mam_message_11 LIKE mam_message_00;
CREATE TABLE mam_message_12 LIKE mam_message_00;
CREATE TABLE mam_message_13 LIKE mam_message_00;
CREATE TABLE mam_message_14 LIKE mam_message_00;
CREATE TABLE mam_message_15 LIKE mam_message_00;
