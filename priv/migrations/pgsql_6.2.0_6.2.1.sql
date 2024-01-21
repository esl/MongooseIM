DROP INDEX i_rosteru_server_user_jid;
DROP INDEX i_rosteru_server_user;
DROP INDEX i_rosteru_jid;
ALTER TABLE rosterusers ADD PRIMARY KEY (server, username, jid);

DROP INDEX i_rosterg_server_user_jid;
ALTER TABLE rostergroups ADD PRIMARY KEY (server, username, jid, grp);
