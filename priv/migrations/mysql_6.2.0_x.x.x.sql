-- Store information whether the message is of type "groupchat" in the user's archive
ALTER TABLE mam_message
ADD COLUMN is_groupchat boolean NOT NULL DEFAULT false;
