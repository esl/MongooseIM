The following files are used to configure MongooseIM:

* `mongooseim.toml` for MongooseIM settings,

* `vm.args` to affect the Erlang VM behaviour (performance tuning, node name),

* `app.config` to change low-level logging parameters and settings of other Erlang applications.

## mongooseim.toml

This [TOML](https://github.com/toml-lang/toml) file contains the configuration options for the MongooseIM server. It is located at `[MongooseIM repo root]/rel/files/` if you are building from source or `[MongooseIM install root]/etc/` if you are using a pre-built version.

The file is divided into the following sections:

* [**general**](advanced-configuration/general.md) - Served XMPP domains, log level, server language and some other miscellaneous settings.
* [**listen**](advanced-configuration/listen.md) - Configured listeners, receiving incoming XMPP and HTTP connections.
* [**auth**](advanced-configuration/auth.md) - Supported client authentication methods and their options.
* [**outgoing_pools**](advanced-configuration/outgoing-connections.md) - Outgoing connections to external services, including databases, message queues and HTTP services.
* [**services**](advanced-configuration/Services.md) - Internal services like an administration API and system metrics.
* [**modules**](advanced-configuration/Modules.md) - [XMPP extension](https://xmpp.org/extensions/) modules, which extend the basic functionality provided by XMPP.
* [**shaper**](advanced-configuration/shaper.md) - Traffic shapers that limit the incoming XMPP traffic, providing a safety valve to protect the server.
* [**acl**](advanced-configuration/acl.md) - Access classes to which connecting users are assigned.
* [**access**](advanced-configuration/access.md) - Access rules, specifying the privileges of the defined access classes.
* [**s2s**](advanced-configuration/s2s.md) - Server-to-server connection options, used for XMPP federation.
* [**host_config**](advanced-configuration/host_config.md) - Configuration options that need to be different for a specific XMPP domain.

The section names above are links to the detailed documentation of each section.

### Option scope

Each configuration option has its **scope**, which is one of the following:

* **local** - configured separately for each node in the cluster - each node can have a different value,
* **global** - configured for the entire cluster - all nodes share the same value.

The scope of each option is defined in the documentation above - either at the top of the section page or for each option individually.

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

TLS is configured in one of two ways: some modules need a private key and certificate (chain) in __separate__ files, while others need both in a __single__ file. This is because recent additions use OTP's `ssl` library, while older modules use `p1_tls`, respectively.

* Client-to-server connections need both in the __same__ `.pem` file
* Server-to-server connections need both in the __same__ `.pem` file
* BOSH, WebSockets and REST APIs need them in __separate__ files

In order to create private key & certificate bundle, you may simply concatenate them.

More information about configuring TLS for these endpoints is available in the [listen section configuration](advanced-configuration/listen.md) page.
