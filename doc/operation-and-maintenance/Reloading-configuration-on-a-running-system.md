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
* domain_certfile
* s2s_*
* all_metrics_are_global
* rdbms_*


### Node-specific options

Usually all nodes in cluster share the same configuration.

But sometimes we want different configs for each node in a cluster.

By default in such cases `reload_cluster` would detect a configuration
inconsistency and would not allow configuration updates.

To tell `reload_cluster` to ignore such options, extra information should be
provided in `mongooseim.cfg`.

It's called `node_specific_options`.

They are defined on top level of the configuration file using
`node_specific_options` tuple. This tuple should be the same for all configs
in a cluster.

`node_specific_options` contains a list of match patterns. If you are familiar
with ETS tables or Mnesia tuple matching - it's the same thing.
Match patterns are documented in
[Erlang/OTP docs](http://erlang.org/doc/apps/erts/match_spec.html).

The pattern mechanism is also documented in
[Learn you some Erlang book](http://learnyousomeerlang.com/ets).

Basically, it allows you to put `'_'` to match every possible host.

`[h,'_',module_opt,mod_muc_log, outdir]` would match
`[h,<<"localhost">>,module_opt,mod_muc_log, outdir]` and
`[h,<<"any.other.host">>,module_opt,mod_muc_log, outdir]`.

Example showing where to put `node_specific_options`.

```erlang
{hosts, ["localhost"]}.
{node_specific_options, [
    [h,'_',module_opt,mod_muc_log, outdir]
 ]}.
{modules, [....]}.
```

The `node_specific_options` patterns are matched against flat configuration
options. To print your config in a flat form, use the command with a running
node `mongooseimctl print_flat_config`.

Example:

```erlang
_build/mim1/rel/mongooseim/bin/mongooseimctl print_flat_config
Flat options:
{[l,listen],'FLAT'}.
{[l,listener,{5280,{0,0,0,0},tcp},ejabberd_cowboy],'FLAT'}.
{[l,listener_opt,{5280,{0,0,0,0},tcp},ejabberd_cowboy,num_acceptors],10}.
{[l,listener_opt,{5280,{0,0,0,0},tcp},ejabberd_cowboy,transport_options],
 [{max_connections,1024}]}.
...
{[h,<<"anonymous.localhost">>,auth_method],anonymous}.
{[h,<<"localhost.bis">>,modules],'FLAT'}.
{[h,<<"localhost.bis">>,module,mod_carboncopy],'FLAT'}.
{[h,<<"localhost.bis">>,module,mod_stream_management],'FLAT'}.
{[h,<<"localhost.bis">>,module,mod_muc_commands],'FLAT'}.
{[h,<<"localhost.bis">>,module,mod_amp],'FLAT'}.
{[h,<<"localhost.bis">>,module,mod_offline],'FLAT'}.
{[h,<<"localhost.bis">>,module_opt,mod_offline,access_max_user_messages],
 max_user_offline_messages}.
...
```

[More information about flat options format](../developers-guide/flat_options.md)

#### Node-specific options for Global Distribution

Node-specific options mechanism was designed to allow `mod_global_distrib`
to be configured with different parameters for each node.

Real life configuration example:

```erlang
{node_specific_options, [
    [h,'_',module_opt,mod_global_distrib,endpoints],
    [h,'_',module_opt,mod_global_distrib,advertised_endpoints],
    [h,'_',module_subopt,mod_global_distrib,connections,endpoints],
    [h,'_',module_subopt,mod_global_distrib,connections,advertised_endpoints],
    [h,'_',module_subopt,mod_global_distrib,redis,server],
    [h,'_',module_subopt,mod_global_distrib_bounce,connections,advertised_endpoints],
    [h,'_',module_subopt,mod_global_distrib_bounce,connections,advertised_endpoints,'_'],
    [h,'_',module_subopt,mod_global_distrib_bounce,connections,endpoints],
    [h,'_',module_subopt,mod_global_distrib_bounce,connections,endpoints,'_'],
    [h,'_',module_subopt,mod_global_distrib_disco,connections,advertised_endpoints],
    [h,'_',module_subopt,mod_global_distrib_disco,connections,advertised_endpoints,'_'],
    [h,'_',module_subopt,mod_global_distrib_hosts_refresher,connections,advertised_endpoints],
    [h,'_',module_subopt,mod_global_distrib_hosts_refresher,connections,advertised_endpoints,'_'],
    [h,'_',module_subopt,mod_global_distrib_hosts_refresher,connections,endpoints],
    [h,'_',module_subopt,mod_global_distrib_hosts_refresher,connections,endpoints,'_'],
    [h,'_',module_subopt,mod_global_distrib_hosts_refresher,redis,server],
    [h,'_',module_subopt,mod_global_distrib_mapping,connections,advertised_endpoints],
    [h,'_',module_subopt,mod_global_distrib_mapping,connections,advertised_endpoints,'_'],
    [h,'_',module_subopt,mod_global_distrib_mapping,connections,endpoints],
    [h,'_',module_subopt,mod_global_distrib_mapping,connections,endpoints,'_'],
    [h,'_',module_subopt,mod_global_distrib_mapping,redis,server],
    [h,'_',module_subopt,mod_global_distrib_receiver,advertised_endpoints],
    [h,'_',module_subopt,mod_global_distrib_receiver,advertised_endpoints,'_'],
    [h,'_',module_subopt,mod_global_distrib_receiver,connections,advertised_endpoints],
    [h,'_',module_subopt,mod_global_distrib_receiver,connections,advertised_endpoints,'_'],
    [h,'_',module_subopt,mod_global_distrib_receiver,connections,endpoints],
    [h,'_',module_subopt,mod_global_distrib_receiver,connections,endpoints,'_'],
    [h,'_',module_subopt,mod_global_distrib_receiver,endpoints],
    [h,'_',module_subopt,mod_global_distrib_receiver,endpoints,'_'],
    [h,'_',module_subopt,mod_global_distrib_receiver,redis,server],
    [h,'_',module_subopt,mod_global_distrib_sender,advertised_endpoints],
    [h,'_',module_subopt,mod_global_distrib_sender,advertised_endpoints,'_'],
    [h,'_',module_subopt,mod_global_distrib_sender,connections,advertised_endpoints],
    [h,'_',module_subopt,mod_global_distrib_sender,connections,advertised_endpoints,'_'],
    [h,'_',module_subopt,mod_global_distrib_sender,connections,endpoints],
    [h,'_',module_subopt,mod_global_distrib_sender,connections,endpoints,'_'],
    [h,'_',module_subopt,mod_global_distrib_sender,endpoints],
    [h,'_',module_subopt,mod_global_distrib_sender,endpoints,'_'],
    [h,'_',module_subopt,mod_global_distrib_sender,redis,server]
]}.
```

`'_'` means that any value can be there.

Usually, all modules are configured using just one level of option nesting.
`module_subopt` means that we are interested in a nested option.

`node_specific_options` can be used with any module, not just
`mod_global_distrib` (but usually you want all options to be the same on all
nodes!).

Any flat option can be used in `node_specific_options`.


#### Node-specific modules

We don't compare options of node-specific modules for configuration consistency
check. We also don't check, if all nodes run these modules (it's fine to run
them only on some nodes in a cluster).

```erlang
{node_specific_options, [
    [h,'_',module,mod_global_distrib]
]}.
```
