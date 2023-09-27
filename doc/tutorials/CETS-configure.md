## CETS Config Example

[CETS](https://github.com/esl/cets/) is a library, which allows to replicate in-memory data
across the MongooseIM cluster. It could be used to store:

- information about online XMPP sessions;
- information about outgoung S2S connections;
- stream management session IDs;
- information about online MUC rooms.

If you want to use CETS instead of Mnesia, ensure that these options are set:

```toml
[general]
  sm_backend = "cets"
  component_backend = "cets"
  s2s_backend = "cets"

[internal_databases.cets]

# The list of modules that use CETS
# You should enable only modules that you use
[modules.mod_stream_management]
  backend = "cets"

[modules.mod_bosh]
  backend = "cets"

[modules.mod_muc]
  online_backend = "cets"

[modules.mod_jingle_sip]
  backend = "cets"
```

Ensure that `outgoing_pools` are configured with RDBMS, so CETS could get a list of MongooseIM nodes, which use the same
relational database and cluster them together.

A preferred way to install MongooseIM is [Helm Charts](https://github.com/esl/MongooseHelm/) on Kubernetes, so it allows
to set `volatileDatabase` to `cets` and the values would be applied using Helm's templates


## CETS with the file discovery backend

It is possible to read a list of nodes to cluster from a file. But MongooseIM does not modify this file, so it is the task
for the operator to update the file. But MongooseIM would reread the file without the restart:

```toml
[internal_databases.cets]
    backend = "file"
    node_list_file = "/etc/mongooseim/mongooseim_nodes.txt"
```

And the format of the `node_list_file` file is a new line separated list of nodes:

```
mongooseim@host1.example.com
mongooseim@host2.example.com
mongooseim@host3.example.com
```

File backend for CETS is only useful if you do not use an RDBMS database.
You could use some external script to get the list of nodes from the AWS CLI command or some other way.
