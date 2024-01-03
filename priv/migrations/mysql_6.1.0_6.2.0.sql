-- Cluster node discovery used by CETS
CREATE TABLE discovery_nodes (
    cluster_name varchar(250) NOT NULL,
    node_name varchar(250) NOT NULL,
    node_num INT UNSIGNED NOT NULL,
    address varchar(250) NOT NULL DEFAULT '', -- empty means we should ask DNS
    updated_timestamp BIGINT NOT NULL, -- in seconds
    PRIMARY KEY (cluster_name, node_name)
);
CREATE UNIQUE INDEX i_discovery_nodes_node_num USING BTREE ON discovery_nodes(cluster_name, node_num);
