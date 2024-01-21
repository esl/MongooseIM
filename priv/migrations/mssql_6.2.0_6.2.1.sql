DROP INDEX i_rosteru_server_user_jid ON rosterusers;
DROP INDEX i_rosteru_server_user ON rosterusers;
DROP INDEX i_rosteru_jid ON rosterusers;
ALTER TABLE rosterusers
ADD CONSTRAINT PK_rosterusers PRIMARY KEY CLUSTERED (server ASC, username ASC, jid ASC);

DROP INDEX i_rosteru_jid ON rostergroups;
ALTER TABLE rostergroups
ALTER COLUMN grp VARCHAR(250),
ADD CONSTRAINT PK_rostergroups PRIMARY KEY CLUSTERED (server ASC, username ASC, jid ASC, grp ASC);
