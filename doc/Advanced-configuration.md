For advanced configuration use the following files:

* `mongooseim.cfg` for pure MongooseIM settings,

* `vm.args` to affect the Erlang VM behaviour (performance tuning, node name),

* `app.config` to change low-level logging parameters and settings of other Erlang applications.

Since you've gotten this far, we assume you're already familiar with Erlang syntax.

# mongooseim.cfg

This file consists of multiple erlang tuples terminated with a period.
In order to configure it, go to `[MongooseIM repo root]/rel/files/` (if you're building from source) or `[MongooseIM install root]/etc/` if you're using a pre-built version.

The tuple order is important, unless no `host_config` option is set.
Retaining the default layout is recommended so that the experienced MongooseIM users can smoothly traverse the file.

`mongooseim.cfg` is full of useful comments and in most cases they should be sufficient help in changing the configuration.

## Options

* All options except `hosts`, `host`, `host_config`, `listen` and `outgoing_connections` may be used in the `host_config` tuple.

* There are two kinds of local options - those that are kept separately for each domain in the config file (defined inside `host_config`) and the options local for a node in the cluster.

* "global" options are shared by all cluster nodes and all domains.

* Options labeled as "multi" (in this page) can be declared multiple times in a row, e.g. one per domain.

* Section names below correspond with the ones in the file.

### Override stored options

* **override_global, override_local, override_acls** - optional
    * **Description:** Will cause MongooseIM to erase all global/local/acl options in database respectively. This ensures that ALL settings of a specific type will be reloaded on startup.

### Debugging

* **loglevel** (local)
    * **Description:** Log level configured with an integer: 0 (disabled), 1 (critical), 2 (error), 3 (warning), 4 (info), 5 (debug). Recommended values for production systems are 2 or 3 (5 is for development).


### Served hostnames

* **hosts** (global)
    * **Description:** List of domains supported by this cluster.
    * **Warning:** Extension modules and database backends will be started separately for every domain. When increasing the number of domains please make sure you have enough resources available (e.g. connection limit set in DBMS).
    * **Example:** `["localhost", "domain2"]`

* **route_subdomain** (local)
    * **Description:** If a stanza is addressed to a subdomain of the served domain and this option is set to `s2s`, such a stanza will be transmitted over s2s. Without it, MongooseIM will try to route the stanza to one of the internal services.
    * **Note:** `s2s` is the only valid value. Any other will simply disable the feature.

### Listening ports

* **listen** (local)
    * **Description:** List of modules handling the incoming connections. By default, 3 are enabled: `ejabberd_cowboy`, `ejabberd_c2s` and `ejabberd_s2s_in`. They accept XMPP, BOSH, Websocket and S2S connections (plus queries to metrics API).
    * **Syntax:** List of tuples: `{Port, Module, ModuleSpecificOptions}`
    * **See also:** [Listener modules](advanced-configuration/Listener-modules.md)

* **s2s_use_starttls** (global)
    * **Description:** Controls StartTLS feature for S2S connections.
    * **Values:**
        * `false`
        * `optional`
        * `required`
        * `required_trusted` - uses OpenSSL's function [SSL_get_verify_result](http://www.openssl.org/docs/ssl/SSL_get_verify_result.html)

* **s2s_certfile** (global)
    * **Description:** Path to X509 PEM file with a certificate and a private key inside (not protected by any password). Required if `s2s_use_starttls` is enabled.

* **s2s_ciphers** (global) <a name="s2s-ciphers"></a>
    * **Description:** Defines a list of accepted SSL ciphers in **outgoing** S2S connection.
      Please refer to the [OpenSSL documentation](http://www.openssl.org/docs/apps/ciphers.html) for the cipher string format.
    * **Default:** `"TLSv1.2:TLSv1.3"`

* **domain_certfile** (multi, global)
    * **Description:** Overrides common certificates with new ones specific for chosen XMPP domains.
                       Applies to S2S and C2S connections.
    * **Syntax:** `{domain_certfile, "example.com", "/path/to/example.com.pem"}.`

* **s2s_default_policy** (local)
    * **Description:** Default policy for a new S2S (server-to-server) **both incoming and outgoing** connection to/from an unknown remote server.

* **s2s_host** (multi, local)
    * **Description:** Allows black/whitelisting S2S destinations.
    * **Syntax:** `{ {s2s_host, "somehost.com"}, allow|deny }.`

* **outgoing_s2s_port** (local)
    * **Description:** Defines a port to be used for outgoing S2S connections. Cannot be random.
    * **Default:** 5269

* **s2s_addr** (multi, global)
    * **Description:** Override DNS lookup for a specific non-local XMPP domain and use a predefined server IP and port for S2S connection.
    * **Syntax:** `"{ {s2s_addr, \"some-domain\"}, { {10,20,30,40}, 7890 } }."`

* **outgoing_s2s_options** (global)
    * **Description:** Specifies the order of address families to try when establishing S2S connection and the connection timeout (in milliseconds or atom `infinity`).
    * **Default:** `{outgoing_s2s_options, [ipv4, ipv6], 10000}.`
    * **Family values:** `inet4`/`ipv4`, `inet6`/`ipv6`

* **s2s_shared** (global)
    * **Description:** S2S shared secret used in [Server Dialback](https://xmpp.org/extensions/xep-0220.html) extension.
    * **Syntax:** `{s2s_shared, <<"shared secret">>}`.
    * **Default:** 10 strong random bytes, hex-encoded.

* **s2s_dns_options** (local)
    * **Description:** Parameters used in DNS lookups for outgoing S2S connections.
    * **Syntax:** `{s2s_dns_options, [{Opt, Val}, ...]}.`
    * **Supported options**
        * `timeout` (integer, seconds, default: 10) - A timeout for DNS lookup.
        * `retries` (integer, default: 2) - How many DNS lookups will be attempted.
    * **Example:** `{s2s_dns_options, [{timeout, 30}, {retries, 1}]}.`

* **s2s_max_retry_delay** (local)
    * **Description:** How many seconds MIM node should wait until next attempt to connect to remote XMPP cluster.
    * **Syntax:** `{s2s_max_retry_delay, Delay}.`
    * **Default:** 300
    * **Example:** `{s2s_max_retry_delay, 30}.`

### Session backend

* **sm_backend** (global)
    * **Description:** Backend for storing user session data.
      Currently all nodes in a cluster must have access to a complete session database.
      Valid backends are `mnesia` and `redis`.
      Mnesia is sufficient in most cases, use Redis only in large deployments when you notice issues with the mnesia backend.
    * **Mnesia:** `{sm_backend, {mnesia, []}}`
    * **Redis:** `{sm_backend, {redis, []}}`
      Requires redis pool defined in `outgoing_pools`: <br/> `{redis, global, default, ..., ...}`.
      See [redis section in outgoing connections doc](./advanced-configuration/outgoing-connections.md#redis-connection-setup)

### Authentication

* **auth_method** (local)
    * **Description:** Chooses an authentication module or a list of modules. Modules from the list are queried one after another until one of them replies positively.
    * **Valid values:** `internal` (Mnesia), `rdbms`, `external`, `anonymous`, `ldap`, `jwt`, `riak`, `http`, `pki`
    * **Warning:** Authentication backends support only specific SASL mechanisms, see [auth backends capabilities](#authentication-backend-capabilities).
    * **Examples:** `rdbms`, `[internal, anonymous]`

* **auth_opts** (local)
    * **Description:** Provides different parameters that will be applied to a choosen authentication method.
                       `auth_password_format` and `auth_scram_iterations` are common to `http`, `rdbms`, `internal` and `riak`.

        * **auth_password_format**
             * **Description:** Decide whether user passwords will be kept plain or hashed in the database. Currently the popular XMPP clients support the SCRAM method, so it is strongly recommended to use the hashed version. The older ones can still use `PLAIN` mechiansm. `DIGEST-MD5` is not available with `scram`.
             * **Values:** `plain`, `scram`
             * **Default:** `plain` (for compatibility reasons, might change soon)

        * **auth_scram_iterations**
             * **Description:** Hash function round count. The higher the value, the more difficult breaking the hashes is. We advise against setting it too low.
             * **Default:** 4096

        * [`external` backend options](authentication-backends/External-authentication-module.md#configuration-options)

        * [`http` backend options](authentication-backends/HTTP-authentication-module.md#configuration-options)

        * [`jwt` backend options](authentication-backends/JWT-authentication-module.md#configuration-options)

        * [`ldap` backend options](authentication-backends/LDAP-authentication-module.md#configuration-options)

        * [`riak` backend options](authentication-backends/Riak-authentication-module.md#configuration-options)

* **sasl_mechanisms** (local)
    * **Description:** Specifies a list of allowed SASL mechanisms. It affects the methods announced during stream negotiation and is enforced eventually (user can't pick mechanism not listed here but available in the source code).
    * **Warning:** This list is still filtered by [auth backends capabilities](#authentication-backend-capabilities)
    * **Valid values:** `cyrsasl_plain, cyrsasl_digest, cyrsasl_scram, cyrsasl_anonymous, cyrsasl_oauth, cyrsasl_external`
    * **Default:** `[cyrsasl_plain, cyrsasl_digest, cyrsasl_scram, cyrsasl_anonymous, cyrsasl_oauth, cyrsasl_external]`
    * **Examples:** `[cyrsasl_plain]`, `[cyrsasl_anonymous, cyrsasl_scram]`

* **extauth_instances** (local)
    * **Description:** Specifies a number of workers serving external authentication requests.
    * **Syntax:** `{extauth_instances, Count}.`
    * **Default:** 1

#### Authentication backend capabilities

The table below shows the supported SASL mechanisms for each authentication backend module.

|           | cyrsasl<br>plain | cyrsasl<br>digest | cyrsasl<br>scram | cyrsasl<br>anonymous | cyrsasl<br>external |
|-----------|:----------------:|:-----------------:|:----------------:|:--------------------:|:-------------------:|
| internal  |         x        |         x         |         x        |                      |                     |
| rdbms     |         x        |         x         |         x        |                      |                     |
| external  |         x        |                   |                  |                      |                     |
| anonymous |         x        |         x         |         x        |           x          |                     |
| ldap      |         x        |                   |                  |                      |          x          |
| jwt       |         x        |                   |                  |                      |                     |
| riak      |         x        |         x         |         x        |                      |                     |
| http      |         x        |         x         |         x        |                      |                     |
| pki       |                  |                   |                  |                      |          x          |

`cyrsasl_oauth` does not use the auth backends at all and requires the `mod_auth_token` module enabled instead.

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

### RDBMS connection setup

RDBMS connection pools are set using [outgoing connections configuration](./advanced-configuration/outgoing-connections.md).
There are some additional options that influence all database connections in the server:

* **pgsql_users_number_estimate** (local)
    * **Description:** PostgreSQL's internal structure can make the row counting slow.
    Enabling this option uses alternative query to `SELECT COUNT`, that might be not as accurate but is always fast.
    * **Syntax:** `{pgsql_users_number_estimate, false | true}`
    * **Default:** `false`

* **rdbms_server_type** (local)
    * **Description:** Specifies RDBMS type. Some modules may optimise queries for certain DBs (e.g. `mod_mam_rdbms_user` uses different query for `mssql`).
    * **Syntax:** `{rdbms_server_type, Type}`
    * **Supported values:** `mssql`, `pgsql` or `undefined`
    * **Default:** `undefined`

### Traffic shapers

* **shaper** (mutli, global)
    * **Description:** Define a class of a shaper which is a mechanism for limiting traffic to prevent DoS attack or calming down too noisy clients.
    * **Syntax:** `{shaper, AtomName, {maxrate, BytesPerSecond}}`

* **max_fsm_queue** (local)
    * **Description:** When enabled, will terminate certain processes (e.g. client handlers) that exceed message limit, to prevent resource exhaustion.
                       This option is set for C2S, outgoing S2S and component connections and can be overridden for particular `ejabberd_s2s` or `ejabberd_service` listeners in their configurations.
                       **Use with caution!**
    * **Syntax:** `{max_fsm_queue, MaxFsmQueueLength}`

### Access control lists

* **acl** (multi)
    * **Description:** Define access control list class.
    * **Syntax:** `{acl, AtomName, Definition}`
    * **Regexp format:** Syntax for `_regexp` can be found in [Erlang documentation](http://www.erlang.org/doc/man/re.html) - it's based on AWK syntax. For `_glob` use `sh` regexp syntax.
    * **Valid definitions:**
        * `all`
        * `{user, U}` - check if the username equals `U` and the domain either equals the one specified by the module executing the check or (if the module does a `global` check) is on the served domains list (`hosts` option)
        * `{user, U, S}` - check if the username equals `U` and the domain equals `S`
        * `{server, S}` - check if the domain equals `S`
        * `{resource, R}` - check if the resource equals `R`
        * `{user_regexp, UR}` - perform a regular expression `UR` check on the username and check the server name like in `user`
        * `{user_regexp, UR, S}` - perform a regular expression `UR` check on the username and check if the domain equals `S`
        * `{server_regexp, SR}` - perform a regular expression `SR` check on a domain
        * `{resource_regexp, RR}` - perform a regular expression `SR` check on a resource
        * `{node_regexp, UR, SR}` - username must match `UR` and domain must match `SR`
        * `{user_glob, UR}` - like `_regexp` variant but with `sh` syntax
        * `{server_glob, UR}` - like `_regexp` variant but with `sh` syntax
        * `{resource_glob, UR}` - like `_regexp` variant but with `sh` syntax
        * `{node_glob, UR}` - like `_regexp` variant but with `sh` syntax

### Access rules

* **access** (multi, global)
    * **Description:** Define an access rule for internal checks. The configuration file contains all built-in ones with proper comments.
    * **Syntax:** `{access, AtomName, [{Value, AclName}]}`

* **registration_timeout** (local)
    * **Description:** Limits the registration frequency from a single IP. Valid values are `infinity` or a number of seconds.

* **mongooseimctl_access_commands** (local)
    * **Description:** Defines access rules to chosen `mongooseimctl` commands.
    * **Syntax:** `{mongooseimctl_access_commands, [Rule1, Rule2, ...]}.`
    * **Rule syntax:** `{AccessRule, Commands, ArgumentRestrictions}`
        * `AccessRule` - A name of a rule defined with `acl` config key.
        * `Commands` - A list of command names (e.g. `["restart", "stop"]`) or `all`.
        * `ArgumentRestrictions` - A list of permitted argument values (e.g. `[{domain, "localhost"}]`).
    * **Example:** `{mongooseimctl_access_commands, [{local, ["join_cluster"], [{node, "mongooseim@prime"}]}]}.`

### Default language

* **language** (global)
    * **Description:** Default language for messages sent by the server to users. You can get a full list of supported codes by executing `cd [MongooseIM root] ; ls priv/*.msg | awk '{split($0,a,"/"); split(a[4],b,"."); print b[1]}'` (`en` is not listed there)
    * **Default:** `en`

### Miscellaneous

* **all_metrics_are_global** (local)
    * **Description:** When enabled, all per-host metrics are merged into global equivalents. It means it is no longer possible to view individual host1, host2, host3, ... metrics, only sums are available. This option significantly reduces CPU and (especially) memory footprint in setups with exceptionally many domains (thousands, tens of thousands).
    * **Default:** `false`

* **routing_modules** (local)
    * **Description:** Provides an ordered list of modules used for routing messages. If one of the modules accepts packet for processing, the remaining ones are not called.
    * **Syntax:** `{routing_modules, ModulesList}.`
    * **Valid modules:**
        * `mongoose_router_global` - Calls `filter_packet` hook.
        * `mongoose_router_localdomain` - Routes packets addressed to a domain supported by the local cluster.
        * `mongoose_router_external_localnode` - Delivers packet to an XMPP component connected to the node, which processes the request.
        * `mongoose_router_external` - Delivers packet to an XMPP component connected to the local cluster.
        * `ejabberd_s2s` - Forwards a packet to another XMPP cluster over XMPP Federation.
    * **Default:** `[mongoose_router_global, mongoose_router_localdomain, mongoose_router_external_localnode, mongoose_router_external, ejabberd_s2s]`
    * **Example:** `{routing_modules, [mongoose_router_global, mongoose_router_localdomain]}.`

* **replaced_wait_timeout** (local)
    * **Description:** When a user session is replaced (due to a full JID conflict) by a new one, this parameter specifies the time MongooseIM waits for the old sessions to close. The default value is sufficient in most cases. If you observe `replaced_wait_timeout` warning in logs, then most probably the old sessions are frozen for some reason and it should be investigated.
    * **Syntax:** `{replaced_wait_timeout, TimeInMilliseconds}`
    * **Default:** `2000`

* **cowboy_server_name** (local)
    * **Description:** If configured, replaces Cowboy's default name returned in the `server` HTTP response header. It may be used for extra security, as it makes it harder for the malicious user to learn what HTTP software is running under a specific port. This option applies to **all** listeners started by the `ejabberd_cowboy` module.
    * **Syntax:** `{cowboy_server_name, NewName}`
    * **Default:** no value, i.e. `Cowboy` is used as a header value
    * **Example:** `{cowboy_server_name, "Apache"}`

* **hide_service_name** (local)
    * **Description:** According to RFC 6210, even when a client sends invalid data after opening a connection, the server must open an XML stream and return a stream error anyway. For extra security, this option may be enabled. It changes MIM behaviour to simply close the connection without any errors returned (effectively hiding the server's identity).
    * **Syntax:** `{hide_service_name, Boolean}`
    * **Default:** `false`
    * **Example:** `{hide_service_name, true}`

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

* `lager` - check [Lager's documentation](https://github.com/basho/lager) for more information. Here you can change the logs location and the file names (`file`), as well as the rotation strategy (`size` and `count`) and date formatting (`date`). Ignore the log level parameters - by default they are overridden with the value set in `mongooseim.cfg`.
* `ejabberd`
    * `keep_lager_intact` (default: `false`) - set it to `true` when you want to keep `lager` log level parameters from `app.config`. `false` means overriding the log levels with the value set in `mongooseim.cfg`.
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
