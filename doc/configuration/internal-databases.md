Internal databases are used to cluster MongooseIM nodes, and to replicate in-memory data (e.g. client sessions) between them.

Mnesia is a legacy way to cluster MongooseIM nodes. It is also could be used to store persistent data, but we recommend
to use RDBMS databases instead because of scalability and stability reasons.

CETS is a new way to cluster MongooseIM nodes.
CETS needs to know a list of nodes for the node discovery. There are two ways to get a list of nodes:

- RDBMS database. MongooseIM would write into RDBMS its nodename and read a list of other nodes.
This is the best option if you are already using a relational database.
- A text file with a list of nodes on each line. It is useful when there is an external script to make this file based on
  some custom logic (for example, a bash script that uses AWS CLI to discover instances in the autoscaling group). This file
  would be automatically reread on change.

Omitting this section entirely is equivalent to having only Mnesia enabled:

```toml
[internal_databases.mnesia]
```

The following example enables only CETS with the default RDBMS discovery backend:

```toml
[internal_databases.cets]
```

!!! Warning
    When switching to CETS, you need to configure particular backends to actually use it:

    * general backends: [`sm_backend`](general.md#generalsm_backend), [`s2s_backend`](general.md#generals2s_backend), [`component_backend`](general.md#generalcomponent_backend)
    * module backends: [`mod_bosh`](../modules/mod_bosh.md#modulesmod_boshbackend), [`mod_stream_management`](../modules/mod_stream_management.md#modulesmod_stream_managementbackend), [`mod_jingle_sip`](../modules/mod_jingle_sip.md#modulesmod_jingle_sipbackend), [`mod_muc`](../modules/mod_muc.md#modulesmod_muconline_backend)

Sometimes you might want to have both databases enabled and choose which backends use a particular DB:

```toml
[internal_databases.mnesia]

[internal_databases.cets]
```

## CETS Options

### `internal_databases.cets.backend`

Backend for CETS discovery.

* **Syntax:** string, one of `"rdbms"`, `"file"`.
* **Default:** `"rdbms"`
* **Example:** `backend = "rdbms"`

### `internal_databases.cets.cluster_name`

Namespace for the cluster. Only nodes with the same cluster name would be discovered. This option is for RDBMS backend.

* **Syntax:** string.
* **Default:** `"mongooseim"`
* **Example:** `cluster_name = "mongooseim"`

### `internal_databases.cets.node_list_file`

File to read a list of nodes from. Relative to the MongooseIM's release directory. This option is for the file backend.
Required, if `backend = "file"`.

* **Syntax:** path.
* **Default:** not specified.
* **Example:** `node_list_file = "/etc/mim_nodes.txt"`

### Example

The following example enables CETS with the file discovery backend:

```toml

[internal_databases.cets]
    backend = "file"
    node_list_file = "cets_disco.txt"
```
