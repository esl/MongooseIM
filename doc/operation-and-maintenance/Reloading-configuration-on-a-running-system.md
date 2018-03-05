### Reloading configuration on a running system 

`mongooseimctl` subcommands `reload_cluster` and `reload_local` are now available. The syntax is:

`mongooseimctl reload_local`

`mongooseimctl reload_cluster`

`reload_local` is unsafe as it reloads the configuration only on the local node.
This might introduce inconsistencies between different nodes of the cluster.
It's available as a safety mechanism for the rare case of a cluster-global reload failing.

`reload_cluster [safety_check]` is generally safe. It will try to apply the configuration
on all nodes of the cluster.
The prerequisite is that the modified config file must be available on
all nodes at the same location (the location where MongooseIM expects its config file).
Option `safety_check`can be one of the following:
* hard - ctl will check whether all configs are the same on all the nodes vefore reloading changes
* soft - ctl will only check if config files on nodes are modified after config was loaded to memory and reload changes on nodes that satisfy this contraint
* none - will not check consistency between nodes' configs

### Non-reloadable options
Some options require restarting the server in order to be reloaded.
The following options' changes will be ignored when using `mongooseimctl` tool:
* domain_certfile
* s2s_*
* all_metrics_are_global
* odbc_*
