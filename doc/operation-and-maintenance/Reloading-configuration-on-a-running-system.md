### Reloading configuration on a running system 

`mongooseimctl` subcommands for configuration reloading are:

`mongooseimctl reload_local`

`mongooseimctl reload_cluster`

`mongooseimctl reload_cluster_dryrun`

`reload_local` is unsafe as it reloads the configuration only on the local node.
This might introduce inconsistencies between different nodes in the cluster.
It's available as a safety mechanism for the rare case of a cluster-global reload failing.

`reload_cluster` applies the configuration on all nodes in the cluster.
The prerequisite is that the same version of a config file must be available on
all nodes. All nodes in a cluster must have the same config loaded into memory
as well. There is a small exception from this rule, see "Node-specific options"
below on this page.

`reload_cluster_dryrun` calculates and prints config changes,
but does not apply them.
Useful for debugging.


### Non-reloadable options
Some options require restarting the server in order to be reloaded.
The following options' changes will be ignored when using `mongooseimctl` tool:

* s2s.\*
* general.all_metrics_are_global
* \*.rdbms.\*


### Node-specific options

This option is deprecated and not available when using a config file in the TOML
format.

For the documentation of this option for the `cfg` config format please refer to
the [MIM 3.7.1 documentation](https://mongooseim.readthedocs.io/en/3.7.1/operation-and-maintenance/Reloading-configuration-on-a-running-system/)
or older.
