-- mod_pubsub

CREATE TABLE pubsub_node (
    service_domain VARCHAR(125) NOT NULL,
    service_user VARCHAR(125) NOT NULL,
    node_id VARCHAR(125) NOT NULL,
    access_model ENUM('open', 'presence') NOT NULL,
    max_items BIGINT,
    PRIMARY KEY (service_domain, service_user, node_id)
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

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
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE INDEX i_pubsub_item_published_at USING BTREE
    ON pubsub_item(service_domain, service_user, node_id, published_at);

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
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE INDEX i_pubsub_subscription_subscriber USING BTREE
    ON pubsub_subscription(subscriber_domain, subscriber_user);

-- mod_broadcast

CREATE TABLE broadcast_jobs (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(250) NOT NULL,
    server VARCHAR(250) NOT NULL,
    host_type VARCHAR(250) NOT NULL,
    from_jid VARCHAR(250) NOT NULL,
    subject VARCHAR(1024) NOT NULL,
    body TEXT NOT NULL,
    rate INT NOT NULL,
    recipient_group ENUM('all_users_in_domain') NOT NULL,
    recipient_count INT NOT NULL,
    execution_state ENUM('running', 'finished', 'abort_error', 'abort_admin') NOT NULL DEFAULT 'running',
    abortion_reason TEXT,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    started_at TIMESTAMP NULL,
    stopped_at TIMESTAMP NULL,
    INDEX i_broadcast_jobs_server (server, id),
    INDEX i_broadcast_jobs_host_state (host_type, execution_state),
    INDEX i_broadcast_jobs_server_state (server, execution_state)
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE TABLE broadcast_jobs_ownership (
    broadcast_id INT NOT NULL,
    owner_node VARCHAR(250) NOT NULL,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    expires_at TIMESTAMP NOT NULL,
    PRIMARY KEY (broadcast_id),
    INDEX i_broadcast_jobs_ownership_owner_node (owner_node),
    INDEX i_broadcast_jobs_ownership_expires_at (expires_at),
    FOREIGN KEY (broadcast_id) REFERENCES broadcast_jobs(id) ON DELETE CASCADE
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;

CREATE TABLE broadcast_worker_state (
    broadcast_id INT NOT NULL,
    cursor_user VARCHAR(250),
    recipients_processed INT NOT NULL DEFAULT 0,
    finished BOOLEAN NOT NULL DEFAULT FALSE,
    PRIMARY KEY (broadcast_id),
    FOREIGN KEY (broadcast_id) REFERENCES broadcast_jobs(id) ON DELETE CASCADE
) CHARACTER SET utf8mb4
  ROW_FORMAT=DYNAMIC;
