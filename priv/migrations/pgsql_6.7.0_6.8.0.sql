-- mod_pubsub

CREATE TYPE pubsub_node_access_model AS ENUM('open', 'presence');

CREATE TABLE pubsub_node (
    service_domain VARCHAR(125) NOT NULL,
    service_user VARCHAR(125) NOT NULL,
    node_id VARCHAR(125) NOT NULL,
    access_model pubsub_node_access_model NOT NULL,
    max_items BIGINT,
    PRIMARY KEY (service_domain, service_user, node_id)
);

CREATE TABLE pubsub_item (
    service_domain VARCHAR(125) NOT NULL,
    service_user VARCHAR(125) NOT NULL,
    node_id VARCHAR(125) NOT NULL,
    item_id VARCHAR(125) NOT NULL,
    publisher_domain VARCHAR(125) NOT NULL,
    publisher_user VARCHAR(125) NOT NULL,
    publisher_resource VARCHAR(125) NOT NULL,
    payload TEXT NOT NULL,
    published_at BIGINT NOT NULL,
    PRIMARY KEY (service_domain, service_user, node_id, item_id),
    FOREIGN KEY (service_domain, service_user, node_id)
        REFERENCES pubsub_node(service_domain, service_user, node_id) ON DELETE CASCADE
);

CREATE INDEX i_pubsub_item_published_at ON pubsub_item USING btree
    (service_domain, service_user, node_id, published_at);

CREATE TABLE pubsub_subscription (
    service_domain VARCHAR(125) NOT NULL,
    service_user VARCHAR(125) NOT NULL,
    node_id VARCHAR(125) NOT NULL,
    subscriber_domain VARCHAR(125) NOT NULL,
    subscriber_user VARCHAR(125) NOT NULL,
    subscriber_resource VARCHAR(125) NOT NULL,
    PRIMARY KEY (service_domain, service_user, node_id,
                 subscriber_domain, subscriber_user, subscriber_resource),
    FOREIGN KEY (service_domain, service_user, node_id)
        REFERENCES pubsub_node(service_domain, service_user, node_id) ON DELETE CASCADE
);

CREATE INDEX i_pubsub_subscription_subscriber ON pubsub_subscription USING btree
    (subscriber_domain, subscriber_user);

-- mod_broadcast

CREATE TYPE broadcast_state AS ENUM ('running', 'finished', 'abort_error', 'abort_admin');
CREATE TYPE broadcast_recipient_group AS ENUM ('all_users_in_domain');

CREATE TABLE broadcast_jobs (
    id SERIAL PRIMARY KEY,
    name VARCHAR(250) NOT NULL,
    server VARCHAR(250) NOT NULL,
    host_type VARCHAR(250) NOT NULL,
    from_jid VARCHAR(250) NOT NULL,
    subject VARCHAR(1024) NOT NULL,
    body TEXT NOT NULL,
    rate INTEGER NOT NULL,
    recipient_group broadcast_recipient_group NOT NULL,
    recipient_count INTEGER NOT NULL,
    execution_state broadcast_state NOT NULL DEFAULT 'running',
    abortion_reason TEXT,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    started_at TIMESTAMPTZ,
    stopped_at TIMESTAMPTZ
);

CREATE INDEX i_broadcast_jobs_server ON broadcast_jobs USING btree (server, id);
CREATE INDEX i_broadcast_jobs_host_state
    ON broadcast_jobs USING btree (host_type, execution_state);
CREATE INDEX i_broadcast_jobs_server_state
    ON broadcast_jobs USING btree (server, execution_state);

CREATE TABLE broadcast_jobs_ownership (
    broadcast_id INTEGER NOT NULL REFERENCES broadcast_jobs(id) ON DELETE CASCADE,
    owner_node VARCHAR(250) NOT NULL,
    updated_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    expires_at TIMESTAMPTZ NOT NULL,
    PRIMARY KEY (broadcast_id)
);

CREATE INDEX i_broadcast_jobs_ownership_owner_node
    ON broadcast_jobs_ownership USING btree (owner_node);
CREATE INDEX i_broadcast_jobs_ownership_expires_at
    ON broadcast_jobs_ownership USING btree (expires_at);

CREATE TABLE broadcast_worker_state (
    broadcast_id INTEGER NOT NULL REFERENCES broadcast_jobs(id) ON DELETE CASCADE,
    cursor_user VARCHAR(250),
    recipients_processed INTEGER NOT NULL DEFAULT 0,
    finished BOOLEAN NOT NULL DEFAULT FALSE,
    PRIMARY KEY (broadcast_id)
);
