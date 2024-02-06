-- Store information whether the message is of type "groupchat" in the user's archive
ALTER TABLE mam_message
ADD is_groupchat smallint NOT NULL DEFAULT 0;
