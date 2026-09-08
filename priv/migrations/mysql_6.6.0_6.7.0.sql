CREATE TABLE invites (
    token text NOT NULL,
    username text NOT NULL,
    host varchar(250) NOT NULL,
    invitee varchar(191) NOT NULL DEFAULT '',
    created_at timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    expires timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
    type character(1) NOT NULL,
    account_name text NOT NULL,
    PRIMARY KEY (token(191))
) ENGINE=InnoDB CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci;

CREATE INDEX i_invite_token_username USING BTREE ON invites(username(191), server_host(191));
CREATE INDEX i_invite_token_invitee USING BTREE ON invites(invitee(191));
