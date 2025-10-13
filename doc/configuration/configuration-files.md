The following files are used to configure MongooseIM:

* `mongooseim.toml` for MongooseIM settings,

* `vm.args` to affect the Erlang VM behaviour (performance tuning, node name),

* `app.config` to change low-level logging parameters and settings of other Erlang applications.

## mongooseim.toml

This [TOML](https://github.com/toml-lang/toml) file contains the configuration options for the MongooseIM server. It is located at `[MongooseIM repo root]/rel/files/` if you are building from source or `[MongooseIM install root]/etc/` if you are using a pre-built version.

The file is divided into the following sections:

* [**general**](general.md) - Served XMPP domains, log level, server language and some other miscellaneous settings.
* [**listen**](listen.md) - Configured listeners, receiving incoming XMPP and HTTP connections.
* [**auth**](auth.md) - Supported client authentication methods and their options.
* [**internal_databases**](internal-databases.md) - Options for Mnesia and CETS. They are primarily used for clustering.
* [**outgoing_pools**](outgoing-connections.md) - Outgoing connections to external services, including databases, message queues and HTTP services.
* [**services**](Services.md) - Internal services like an administration API and system metrics.
* [**modules**](Modules.md) - [XMPP extension](https://xmpp.org/extensions/) modules, which extend the basic functionality provided by XMPP.
* [**shaper**](shaper.md) - Traffic shapers that limit the incoming XMPP traffic, providing a safety valve to protect the server.
* [**acl**](acl.md) - Access classes to which connecting users are assigned.
* [**access**](access.md) - Access rules, specifying the privileges of the defined access classes.
* [**s2s**](s2s.md) - Server-to-server connection options, used for XMPP federation.
* [**host_config**](host_config.md) - Configuration options for different XMPP domains or host types (groups of domains).

The section names above are links to the detailed documentation of each section.

!!! Warning
    It is recommended to use the same configuration file for all nodes in the cluster, but there is no protection against using different option values for each node, because it can happen in two cases:

    * During a [rolling upgrade](../operation-and-maintenance/Rolling-upgrade.md) procedure, when nodes are restarted one by one with new configuration.
    * When you need different network-specific parameters (e.g. listening IP addresses) for each node.

## vm.args

This file contains parameters passed directly to the Erlang VM. To configure it, go to `[MongooseIM root]/rel/files/`.

Let's explore the default options.

### Options

* `-sname` - Erlang node name. Can be changed to `name`, if necessary
* `-setcookie` - Erlang cookie. All nodes in a cluster must use the same cookie value.
* `+K` - Enables kernel polling. It improves the stability when a large number of sockets is opened, but some systems might benefit from disabling it. Might be a subject of individual load testing.
* `+A 5` - Sets the asynchronous threads number. Async threads improve I/O operations efficiency by relieving scheduler threads of IO waits.
* `+P 10000000` - Process count limit. This is a maximum allowed number of processes running per node. In general, it should exceed the tripled estimated online user count.
* `-env ERL_MAX_PORTS 250000` - Open port count. This is a maximum allowed number of ports opened per node. In general, it should exceed the tripled estimated online user count. Keep in mind that increasing this number also increases the memory usage by a constant amount, so finding the right balance for it is important for every project.
* `-env ERL_FULLSWEEP_AFTER 2` - affects garbage collection. Reduces memory consumption (forces often full g.c.) at the expense of CPU usage.
* `-sasl sasl_error_logger false` - MongooseIM's solution for logging is Lager, so SASL error logger is disabled.

## app.config

A file with Erlang application configuration. To configure it, go to `[MongooseIM root]/rel/files/`.
By default only the following applications can be found there:

* `logger` - check [Logger's documentation](https://erlang.org/doc/man/logger.html) for more information.
* `ssl`
    * `session_lifetime` (default specified in the file: `600` seconds) - This parameter says for how long should the ssl session remain in the cache for further re-use, should `ssl session resumption` happen.

## Configuring TLS: Certificates & Keys

TLS private key and certificate (chain) have to be be specified in __separate__ files, `keyfile` and `certfile` respectively.

More information about configuring TLS for these endpoints is available in the [listen section configuration](../listeners/listen-c2s.md#tls-options-for-c2s) page.
