### Reloading configuration on a running system 

`mongooseimctl` subcommands `reload_cluster` and `reload_local` are now available. The syntax is:

>  mongooseimctl reload_local

>  mongooseimctl reload_cluster

`reload_local` is unsafe as it reloads the configuration only on the local node.
This might introduce inconsistencies between different nodes of the cluster.
It's available as a safety mechanism for the rare case of a cluster-global reload failing.

`reload_cluster` is generally safe. It will try to apply the configuration
on all nodes of the cluster.
The prerequisite is that the modified config file must be available on
all nodes at the same location (the location where MongooseIM expects its config file).
