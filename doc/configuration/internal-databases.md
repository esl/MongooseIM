Internal databases are used to cluster MongooseIM nodes, and to replicate session list data between them.

Mnesia is a legacy way to cluster MongooseIM nodes. It is also could be used to store persistent data, but we recommend
to use RDBMS databases instead because of scalability and stability reasons.

CETS is a new way to cluster MongooseIM nodes.
CETS needs to know a list of nodes for the node discovery. There are two ways to get a list of nodes:

- A text file with a list of nodes on each line. It is useful when there is an external script to make this file based on
  some custom logic (for example, a bash script that uses AWS CLI to discover instances in the autoscaling group). This file
  would be automatilly reread on change.
- RDBMS database. MongooseIM would write into RDBMS its nodename and read a list of other nodes. It is pretty simple, but
  RDBMS database could be a single point of failure.

Section example:

```toml
[internal_databases]
    [internal_databases.mnesia]

    [internal_databases.cets]
        backend = "rdbms"
        cluster_name = "mongooseim"
```

or

```toml
[internal_databases]
    [internal_databases.cets]
        backend = "file"
        node_list_file = "cets_disco.txt"
```

To enable just CETS, define only `internal_databases.cets` section:

```toml
[internal_databases]
    [internal_databases.cets]
```

# CETS Options

### `internal_databases.cets.backend`

Backend for CETS discovery.

* **Syntax:** string, one of `"rdbms"`, `"file"`.
* **Default:** `"rdbms"`
* **Example:** `backend = "rdbms"`

### `internal_databases.cets.cluster_name`

Namespace for the cluster. Only nodes with the same cluster name would be discoverd. This option is for RDBMS backend.

* **Syntax:** string.
* **Default:** `"mongooseim"`
* **Example:** `cluster_name = "mongooseim"`

### `internal_databases.cets.node_list_file`

File to read a list of nodes from. Relative to the MongooseIM's release directory. This option is for the file backend.
Required, if `backend = "file"`.

* **Syntax:** path.
* **Default:** not specified.
* **Example:** `node_list_file = "/etc/mim_nodes.txt"`
