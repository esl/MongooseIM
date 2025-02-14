-- XEP-0484: Fast Authentication Streamlining Tokens
-- Module: mod_fast_auth_token
CREATE TABLE fast_auth_token(
     server VARCHAR(250) NOT NULL,
     username VARCHAR(250) NOT NULL,
     -- Device installation ID (User-Agent ID)
     -- Unique for each device
     -- https://xmpp.org/extensions/xep-0388.html#initiation
     user_agent_id VARCHAR(250) NOT NULL,
     current_token VARCHAR(250),
     current_expire BIGINT, -- seconds unix timestamp
     current_count INT, -- replay counter
     current_mech_id TINYINT,
     new_token VARCHAR(250),
     new_expire BIGINT, -- seconds unix timestamp
     new_count INT,
     new_mech_id TINYINT,
     PRIMARY KEY(server, username, user_agent_id)
);
