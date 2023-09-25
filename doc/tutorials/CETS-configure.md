## CETS Config Example

[CETS](https://github.com/esl/cets/) is a library, which allows to replicate in-memory data
across the MongooseIM cluster. It could be used to store a list of online XMPP sessions, a list
of outgoung S2S connections, steam management session IDs, a list of online MUC rooms.

If you want to use CETS instead of Mnesia, ensure that these options are set:

```ini
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
