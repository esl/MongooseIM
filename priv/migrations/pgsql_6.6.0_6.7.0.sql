CREATE TABLE invites (
    token text NOT NULL,
    username text NOT NULL,
    host text NOT NULL,
    invitee text NOT NULL DEFAULT '',
    created_at timestamp NOT NULL DEFAULT now(),
    expires timestamp NOT NULL DEFAULT now(),
    "type" character(1) NOT NULL,
    account_name text NOT NULL,
    PRIMARY KEY (token)
);
CREATE INDEX i_invite_token_username_server_host ON invites USING btree (username, server_host);
CREATE INDEX i_invite_token_invitee ON invites USING btree (invitee);
