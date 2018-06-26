### Reloading configuration on a running system 

`mongooseimctl` subcommands for configuration reloading are:

`mongooseimctl reload_local`

`mongooseimctl reload_cluster`

`mongooseimctl reload_cluster_dryrun`

`reload_local` is unsafe as it reloads the configuration only on the local node.
This might introduce inconsistencies between different nodes of the cluster.
It's available as a safety mechanism for the rare case of a cluster-global reload failing.

`reload_cluster` applies the configuration on all nodes of the cluster.
The prerequisite is that the latest version of config file must be available on
all nodes.

`reload_cluster_dryrun` calculates and prints config changes,
but does not applies them. Useful for debugging.

### Non-reloadable options
Some options require restarting the server in order to be reloaded.
The following options' changes will be ignored when using `mongooseimctl` tool:
* domain_certfile
* s2s_*
* all_metrics_are_global
* odbc_*


### Node-specific options

Very rarely we want different configs for each node in cluster.
Than `reload_cluster` would detect configuration inconsistency and would not
allow configuration updates.

To tell `reload_cluster` to ignore such options, extra information should be
provided in `ejabberd.cfg`.

It's called `node_specific_options`.

They are defined on top level of the configuration file using
`node_specific_options` tuple. This tuple should be the same for all configs
in cluster.

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

The `node_specific_options` patterns are matched against flatten configuration
options. To print your config in flatten form, use the command with running
node `mongooseimctl print_flatten_config`.

Example:

```erlang
_build/mim1/rel/mongooseim/bin/mongooseimctl print_flatten_config
Flatten options:
{[l,listen],'FLATTEN'}.
{[l,listener,{5280,{0,0,0,0},tcp},ejabberd_cowboy],'FLATTEN'}.
{[l,listener_opt,{5280,{0,0,0,0},tcp},ejabberd_cowboy,num_acceptors],10}.
{[l,listener_opt,{5280,{0,0,0,0},tcp},ejabberd_cowboy,transport_options],
 [{max_connections,1024}]}.
...
{[h,<<"anonymous.localhost">>,auth_method],anonymous}.
{[h,<<"localhost.bis">>,modules],'FLATTEN'}.
{[h,<<"localhost.bis">>,module,mod_carboncopy],'FLATTEN'}.
{[h,<<"localhost.bis">>,module,mod_stream_management],'FLATTEN'}.
{[h,<<"localhost.bis">>,module,mod_muc_commands],'FLATTEN'}.
{[h,<<"localhost.bis">>,module,mod_amp],'FLATTEN'}.
{[h,<<"localhost.bis">>,module,mod_offline],'FLATTEN'}.
{[h,<<"localhost.bis">>,module_opt,mod_offline,access_max_user_messages],
 max_user_offline_messages}.
...
```

[More information about flatten options format](../developers-guide/flatten_options.md)

### Node-specific options for Global Distribution

Node-specific options mechanism was designed to allow `mod_global_distrib`
to be configured with different parameters for each node.

Real life configuration example:

```erlang
{node_specific_options, [
    [h,'_',module_opt,mod_global_distrib,endpoints],
    [h,'_',module_opt,mod_global_distrib,advertised_endpoints],
    [h,'_',module_subopt,mod_global_distrib,connections,endpoints],
    [h,'_',module_subopt,mod_global_distrib,connections,advertised_endpoints],
    [h,'_',module_subopt,mod_global_distrib,redis,server]
]}.
```

### Node-specific modules

We don't compare options of node-specific modules for configuration consistency
check. We also don't check, if all nodes run these modules (it's fine to run
them only on some nodes in a cluster).

```erlang
{node_specific_options, [
    [h,'_',module,mod_global_distrib]
]}.
```
