CREATE TABLE mam_muc_message_00(
  -- Message UID
  -- A server-assigned UID that MUST be unique within the archive.
  id BIGINT UNSIGNED NOT NULL PRIMARY KEY,
  room_id INT UNSIGNED NOT NULL,
  -- A nick of the message's originator
  nick_name varchar(250) NOT NULL,
  -- Term-encoded message packet
  message blob NOT NULL,
  search_body text
)  ENGINE=InnoDB;
CREATE INDEX i_mam_muc_message_room_name_added_at USING BTREE ON mam_muc_message_00(room_id, id);

CREATE TABLE mam_muc_message_01 LIKE mam_muc_message_00;
CREATE TABLE mam_muc_message_02 LIKE mam_muc_message_00;
CREATE TABLE mam_muc_message_03 LIKE mam_muc_message_00;
CREATE TABLE mam_muc_message_04 LIKE mam_muc_message_00;
CREATE TABLE mam_muc_message_05 LIKE mam_muc_message_00;
CREATE TABLE mam_muc_message_06 LIKE mam_muc_message_00;
CREATE TABLE mam_muc_message_07 LIKE mam_muc_message_00;
CREATE TABLE mam_muc_message_08 LIKE mam_muc_message_00;
CREATE TABLE mam_muc_message_09 LIKE mam_muc_message_00;
CREATE TABLE mam_muc_message_10 LIKE mam_muc_message_00;
CREATE TABLE mam_muc_message_11 LIKE mam_muc_message_00;
CREATE TABLE mam_muc_message_12 LIKE mam_muc_message_00;
CREATE TABLE mam_muc_message_13 LIKE mam_muc_message_00;
CREATE TABLE mam_muc_message_14 LIKE mam_muc_message_00;
CREATE TABLE mam_muc_message_15 LIKE mam_muc_message_00;
