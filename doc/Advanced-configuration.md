For advanced configuration use the following files:

* `mongooseim.cfg` for pure MongooseIM settings,

* `vm.args` to affect the Erlang VM behaviour (performance tuning, node name),

* `app.config` to change low-level logging parameters and settings of other Erlang applications.

Since you've gotten this far, we assume you're already familiar with Erlang syntax.

# mongooseim.toml

This [TOML](https://github.com/toml-lang/toml) file contains the configuration options for the MongooseIM server. It is located at `[MongooseIM repo root]/rel/files/` if you are building from source or `[MongooseIM install root]/etc/` if you are using a pre-built version.

The file is divided into the following sections:

* [**general**](advanced-configuration/general.md) - Served XMPP domains, log level, server language and some other miscellaneous settings.
* [**listen**](advanced-configuration/listen.md) - Configured listeners, receiving incoming XMPP and HTTP connections.
* [**auth**](advanced-configuration/auth.md) - Supported client authentication methods and their options.
* **outgoing_pools** - Outgoing connections to external services, including databases, message queues and HTTP services.
* **services** - Internal services like an administration API and system metrics.
* **modules** - [XMPP extension](https://xmpp.org/extensions/) modules, which extend the basic functionality provided by XMPP.
* [**shaper**](advanced-configuration/shaper.md) - Traffic shapers that limit the incoming XMPP traffic, providing a safety valve to protect the server.
* [**acl**](advanced-configuration/acl.md) - Access classes to which connecting users are assigned.
* [**access**](advanced-configuration/access.md) - Access rules, specifying the privileges of the defined access classes.
* [**s2s**](advanced-configuration/s2s.md) - Server-to-server connection options, used for XMPP federation.
* **host_config** - Configuration options that need to be different for a specific XMPP domain. May contain the following subsections: **general**, **auth**, **modules**, **shaper**, **acl**, **access**, **s2s**.

## Options

* All options except `hosts`, `host`, `host_config`, `listen` and `outgoing_connections` may be used in the `host_config` tuple.

* There are two kinds of local options - those that are kept separately for each domain in the config file (defined inside `host_config`) and the options local for a node in the cluster.

* "global" options are shared by all cluster nodes and all domains.

* Options labeled as "multi" (in this page) can be declared multiple times in a row, e.g. one per domain.

* Section names below correspond with the ones in the file.

### Outgoing connections setup

* **outgoing_pools** (local)
    * **Description** Declares pools for outgoing connections.
      See more in [outgoing connections configuration](./advanced-configuration/outgoing-connections.md)
    * **Syntax** `[{Type, Host, Tag, PoolOptions, ConnectionOptions}]`
    * **Example**:
```erlang
        [{riak, global, default, [], [{address, "127.0.0.1"}]},
         {http, host, auth, [], [{server, "127.0.0.1"}]}
```

### Modules

For a specific configuration, please refer to [Modules](advanced-configuration/Modules.md) page.

* **modules** (local)
    * **Description:** List of enabled modules with their options.

### Services

For a specific configuration, please refer to [Services](advanced-configuration/Services.md) page.

* **services** (local)
    * **Description:** List of enabled services with their options.

### Per-domain configuration

The `host_config` allows configuring most options separately for specific domains served by the cluster. It is best to put `host_config` tuple right after the global section it overrides/complements or even at the end of `mongooseim.cfg`.

* **host_config** (multi, local)
    * **Syntax:** `{host_config, Domain, [ {{add, modules}, [{mod_some, Opts}]}, {access, c2s, [{deny, local}]}, ... ]}.`

# vm.args

This file contains parameters passed directly to the Erlang VM. To configure it, go to `[MongooseIM root]/rel/files/`.

Let's explore the default options.

## Options

* `-sname` - Erlang node name. Can be changed to `name`, if necessary
* `-setcookie` - Erlang cookie. All nodes in a cluster must use the same cookie value.
* `+K` - Enables kernel polling. It improves the stability when a large number of sockets is opened, but some systems might benefit from disabling it. Might be a subject of individual load testing.
* `+A 5` - Sets the asynchronous threads number. Async threads improve I/O operations efficiency by relieving scheduler threads of IO waits.
* `+P 10000000` - Process count limit. This is a maximum allowed number of processes running per node. In general, it should exceed the tripled estimated online user count.
* `-env ERL_MAX_PORTS 250000` - Open port count. This is a maximum allowed number of ports opened per node. In general, it should exceed the tripled estimated online user count. Keep in mind that increasing this number also increases the memory usage by a constant amount, so finding the right balance for it is important for every project.
* `-env ERL_FULLSWEEP_AFTER 2` - affects garbage collection. Reduces memory consumption (forces often full g.c.) at the expense of CPU usage.
* `-sasl sasl_error_logger false` - MongooseIM's solution for logging is Lager, so SASL error logger is disabled.

# app.config

A file with Erlang application configuration. To configure it, go to `[MongooseIM root]/rel/files/`.
By default only the following applications can be found there:

* `logger` - check [Logger's documentation](https://erlang.org/doc/man/logger.html) for more information.
* `ejabberd`
    * `config` (default: `"etc/mongooseim.cfg"`) - path to MongooseIM config file.
* `ssl`
    * `session_lifetime` (default specified in the file: `600` seconds) - This parameter says for how long should the ssl session remain in the cache for further re-use, should `ssl session resumption` happen.

# Configuring TLS: Certificates & Keys

TLS is configured in one of two ways: some modules need a private key and certificate (chain) in __separate__ files, while others need both in a __single__ file. This is because recent additions use OTP's `ssl` library, while older modules use `p1_tls`, respectively.

* Client-to-server connections need both in the __same__ `.pem` file
* Server-to-server connections need both in the __same__ `.pem` file
* BOSH, WebSockets and REST APIs need them in __separate__ files

In order to create private key & certificate bundle, you may simply concatenate them.

More information about configuring TLS for these endpoints is available in [Listener modules](advanced-configuration/Listener-modules.md) page.
