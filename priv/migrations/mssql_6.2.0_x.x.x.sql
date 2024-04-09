-- Update roster schema
DROP INDEX i_rosteru_server_user ON rosterusers;
DROP INDEX i_rosteru_jid ON rosterusers;
ALTER TABLE rosterusers
DROP CONSTRAINT rosterusers$i_rosteru_server_user_jid;
ALTER TABLE rosterusers
ADD CONSTRAINT PK_rosterusers PRIMARY KEY CLUSTERED (server ASC, username ASC, jid ASC);

DROP INDEX i_rosterg_server_user_jid ON rostergroups;
ALTER TABLE rostergroups
ALTER COLUMN grp VARCHAR(250) NOT NULL;
ALTER TABLE rostergroups
ADD CONSTRAINT PK_rostergroups PRIMARY KEY CLUSTERED (server ASC, username ASC, jid ASC, grp ASC);

-- Store information whether the message is of type "groupchat" in the user's archive
ALTER TABLE mam_message
ADD is_groupchat smallint NOT NULL DEFAULT 0;

-- Create table for mod_caps
CREATE TABLE caps (
    node varchar(250) NOT NULL,
    sub_node varchar(250) NOT NULL,
    features text NOT NULL,
    PRIMARY KEY (node, sub_node)
);

-- Delete PK constraint before replacing it with a new one
DECLARE @pk VARCHAR(max) = (SELECT CONSTRAINT_NAME FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS
                            WHERE TABLE_NAME='discovery_nodes' AND CONSTRAINT_TYPE='PRIMARY KEY');
EXEC('ALTER TABLE discovery_nodes DROP CONSTRAINT ' + @pk);

-- In case of duplicates, you need to remove stale rows manually or wait for cleanup
ALTER TABLE discovery_nodes ADD PRIMARY KEY (node_name);
