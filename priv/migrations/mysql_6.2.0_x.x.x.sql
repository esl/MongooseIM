-- Update roster schema
DROP INDEX i_rosteru_server_user_jid ON rosterusers;
DROP INDEX i_rosteru_server_user ON rosterusers;
DROP INDEX i_rosteru_jid ON rosterusers;
ALTER TABLE rosterusers ADD PRIMARY KEY(server, username, jid);

DROP INDEX i_rosterg_server_user_jid ON rostergroups;
ALTER TABLE rostergroups MODIFY COLUMN grp VARCHAR(255), ADD PRIMARY KEY(server, username, jid, grp);

-- Store information whether the message is of type "groupchat" in the user's archive
ALTER TABLE mam_message
ADD COLUMN is_groupchat boolean NOT NULL DEFAULT false;
