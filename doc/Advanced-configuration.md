For advanced configuration use the following files:

* `ejabberd.cfg` for pure MongooseIM settings, 

* `vm.args` to affect the Erlang VM behaviour (performance tuning, node name), 

* `app.config` to change low-level logging parameters and settings of other Erlang applications. 

Since you've gotten this far, we assume you're already familiar with Erlang syntax.

# ejabberd.cfg

This file consists of multiple erlang tuples terminated with a period. To configure it, go to `[MongooseIM root]/rel/files/`.

The tuple order is important, unless the no `host_config` option is set. Retaining the default layout is recommended so that the experienced MongooseIM users can smoothly traverse the file.

`ejabberd.cfg` is full of useful comments and in most cases they should be sufficient help in changing the configuration.

## Options

* All options except `hosts`, `host`, `host_config`, `pool` and the ODBC options can be used in the `host_config` tuple.

* There are two kinds of local options - those that are kept separately for each domain in the config file (defined inside `host_config`) and the options local for a node in the cluster.

* "global" options are shared by all cluster nodes and all domains.

* "multi" options can be declared multiple times in a row, e.g. one per domain.

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

* **s2s_ciphers** (global)
    * **Description:** Defines a list of accepted SSL ciphers in **outgoing** S2S connection. Please refer to the [OpenSSL documentation](http://www.openssl.org/docs/apps/ciphers.html) for the cipher string format.
    * **Default:** As of OpenSSL 1.0.0 it's `ALL:!aNULL:!eNULL` ([source](https://www.openssl.org/docs/apps/ciphers.html#CIPHER_STRINGS))

* **domain_certfile** (multi, global)
    * **Description:** Overrides common certificates with new ones specific for chosen XMPP domains.
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

### Session backend

* **sm_backend** (global)
    * **Description:** Backend for storing user session data. Currently all nodes in a cluster must have access to a complete session database. Valid backends are `mnesia` and `redis`. Mnesia is sufficient in most cases, use Redis only in large deployments.
    * **Mnesia:** `{sm_backend, {mnesia, []}}`
    * **Redis:** `{redis, [{pool_size, Size}, {worker_config, [{host, "Host"}, {port, Port}]}]}}`

### LDAP Connection
- **ldap_servers**
    * **Description:** List of IP addresses or DNS names of your LDAP servers.
    * **Values:** `[Servers, ...]`
    * **Default:**  no default value. This option is required when setting up an LDAP connection.

- **ldap_encrypt**
    * **Description:** Enable connection encryption with your LDAP server.
        The value tls enables encryption by using LDAP over SSL. Note that STARTTLS encryption is not supported.
    * **Values:** `none`, `tls`
    * **Default:** `none`

- **ldap_tls_verify** This option specifies whether to verify LDAP server certificate or not when TLS is enabled. 
    When `hard` is enabled ejabberd doesn’t proceed if a certificate is invalid.
    When `soft` is enabled ejabberd proceeds even if the check fails. 
    `False` means no checks are performed.
    * **Values:** `soft`, `hard`, `false`
    * **Default:** `false`

- **ldap_tls_cacertfile**
    * **Description:** Path to a file containing PEM encoded CA certificates.
    * **Values:** Path
    * **Default:** This option is needed (and required) when TLS verification is enabled.

- **ldap_tls_depth**
    * **Description:**  Specifies the maximum verification depth when TLS verification is enabled.
         i.e. how far in a chain of certificates the verification process can proceed before the verification is considered to fail.
         Peer certificate = 0, CA certificate = 1, higher level CA certificate = 2, etc. The value 2 means that a chain can at most contain peer cert, CA cert, next CA cert, and an additional CA cert.
    * **Values:** Integer
    * **Default:** 1

- **ldap_port**
    * **Description:** Port to connect to your LDAP server.
    * **Values:** Integer
    * **Default:** 389 if encryption is disabled. 636 if encryption is enabled.

- **ldap_rootdn**
    * **Description:** Bind DN
    * **Values:** String
    * **Default:** empty string which is `anonymous connection`

- **ldap_password**
    * **Description:** Bind password
    * **Values:** String
    * **Default:** empty string

- **ldap_deref_aliases**
    * **Description:** Whether or not to dereference aliases
    * **Values:** `never`, `always`, `finding`, `searching`
    * **Default:** `never`

### Authentication

- **auth_method** (local)
    * **Description:** Chooses an authentication module or a list of modules. Modules from a list are queried one after another until one of them replies positively.
    * **Valid values:** `internal` (Mnesia), `odbc`, `external`, `anonymous`, `ldap`, `jwt`, `riak`, `http`
    * **Warning:** `external`, `jwt` and `ldap` work only with `PLAIN` SASL mechanism.
    * **Examples:** `odbc`, `[internal, anonymous]`

- **auth_opts** (local)
    * **Description:** Provides different parameters that will be applied to a choosen authentication method.
                       `auth_password_format` and `auth_scram_iterations` are common to `http`, `odbc`, `internal` and `riak`.

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
        
- `ldap` backend options are not yet a part of `auth_opt` tuple, so [these parameters](authentication-backends/LDAP-authentication-module.md#configuration-options) are top-level keys in `ejabberd.cfg` file.

### Database setup

#### Connection pools

* **pool** (multi, local)
    * **Description:** Declares a named pool of connections to the database. At least one pool is required to connect to an SQL database.
    * **Syntax:** `{pool, odbc, PoolName}.` or `{pool, odbc, PoolName, Options}.`
    * **Examples:** `{pool, odbc, default}.`

* **odbc_pool** (local)
    * **Description:** Name of the default connection pool used to connect to the database.
    * **Syntax:** `{odbc_pool, PoolName}`
    * **Default:** `default`

#### Connection setup

The following options can be used to configure a connection pool. To set the options for all connection pools, put them on the top level of the configuration file. To set them for an individual pool, put them inside the `Options` list in a pool specification. Setting `odbc_server` is mandatory.

*Note*: `odbc` prefixes may be misleading. The options apply to all kinds of DB connections, not only pure ODBC.

* **odbc_server** (local)
    * **Description:** SQL DB connection configuration. Currently supported DB types are `mysql` and `pgsql`.
    * **Syntax:** `{odbc_server, {Type, Host, Port, DBName, Username, Password}}.`

* **pgsql_users_number_estimate** (local)
    * **Description:** PostgreSQL's internal structure can make the row counting slow. Enabling this option uses alternative query to `SELECT COUNT`, that might be not as accurate but is always fast.

* **odbc_pool_size** (local)
    * **Description:** How many DB client workers should be started per each domain.
    * **Default:** 10

* **odbc_keepalive_interval** (local)
    * **Description:** When enabled, will send `SELECT 1` query through every DB connection at given interval to keep them open.
    This option should be used to ensure that database connections are
    restarted after they became broken (e.g. due to a database restart or a load
    balancer dropping connections). Currently, not every network related error
    returned from a database driver to a regular query will imply a connection
    restart.

You should remember that SQL databases require creating a schema.
See [Database backends configuration](./advanced-configuration/database-backends-configuration.md) for more information.

### Traffic shapers

* **shaper** (mutli, global)
    * **Description:** Define a class of a shaper which is a mechanism for limiting traffic to prevent DoS attack or calming down too noisy clients.
    * **Syntax:** `{shaper, AtomName, {maxrate, BytesPerSecond}}`

* **max_fsm_queue** (local)
    * **Description:** When enabled, will terminate certain processes (e.g. client handlers) that exceed message limit, to prevent resource exhaustion. This option is set for all the listeners but can be overridden for particular `ejabberd_s2s` or `ejabberd_service` listeners in their configurations. **Use with caution!**
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
        * `{shared_group, G}` - check if the user is in a shared group `G` in the domain specified by the module executing the check
        * `{shared_group, G, H}`- check if the user is in shared group `G` in domain `H`

### Access rules

* **access** (multi, global)
    * **Description:** Define an access rule for internal checks. The configuration file contains all built-in ones with proper comments.
    * **Syntax:** `{access, AtomName, [{Value, AclName}]}`

* **registration_timeout** (local)
    * **Description:** Limits the registration frequency from a single IP. Valid values are `infinity` or a number of seconds.

### Default language

* **language** (global)
    * **Description:** Default language for messages sent by the server to users. You can get a full list of supported codes by executing `cd [MongooseIM root] ; ls apps/ejabberd/priv/*.msg | awk '{split($0,a,"/"); split(a[4],b,"."); print b[1]}'` (`en` is not listed there)
    * **Default:** `en`

### Miscellaneous

* **all_metrics_are_global** (local)
    * **Description:** When enabled, all per-host metrics are merged into global equivalents. It means it is no longer possible to view individual host1, host2, host3, ... metrics, only sums are available. This option significantly reduces CPU and (especially) memory footprint in setups with exceptionally many domains (thousands, tens of thousands).
    * **Default:** `false`

### Modules

For a specific configuration, please refer to [Modules](advanced-configuration/Modules.md) page.

* **modules** (local)
    * **Description:** List of enabled modules with their options.

### Per-domain configuration

The `host_config` allows configuring most options separately for specific domains served by the cluster. It is best to put `host_config` tuple right after the global section it overrides/complements or even at the end of `ejabberd.cfg`.

* **host_config** (multi, local)
    * **Syntax:** `{host_config, Domain, [ {{add, modules}, [{mod_some, Opts}]}, {access, c2s, [{deny, local}]}, ... ]}.`

### Outgoing HTTP connections

The `http_connections` option configures a list of named pools of outgoing HTTP connections that may be used by various modules. Each of the pools has a name (atom) and a list of options:

* **Syntax:** `{http_connections, [{PoolName1, PoolOptions1}, {PoolName2, PoolOptions2}, ...]}.`

Following pool options are recognized - all of them are optional.

* `{server, HostName}` - string, default: `"http://localhost"` - the URL of the destination HTTP server (including a port number if needed).
* `{pool_size, Number}` - positive integer, default: `20` - number of workers in the connection pool.
* `{max_overflow, Number}` - non-negative integer, default: `5` - maximum number of extra workers that can be allocated when the whole pool is busy.
* `{path_prefix, Prefix}` - string, default: `"/"` - the part of the destination URL that is appended to the host name (`host` option).
* `{pool_timeout, TimeoutValue}` - non-negative integer, default: `200` - maximum number of milliseconds to wait for an available worker from the pool.
* `{request_timeout, TimeoutValue}` - non-negative integer, default: `2000` - maximum number of milliseconds to wait for the HTTP response.

**Example:**
```
{http_connections, [{conn1, [{server, "http://my.server:8080"},
                             {pool_size, 50},
                             {path_prefix, "/my/path/"}]}
                   ]}.
```

# vm.args

This file contains parameters passed directly to the Erlang VM. To configure it, go to `[MongooseIM root]/rel/files/`.

Let's explore the default options/

## Options

* `-sname` - Erlang node name. Can be changed to `name`, if necessary
* `-setcookie` - Erlang cookie. All nodes in a cluster must use the same cookie value.
* `+K` - Enables kernel polling. It improves the stability when a large number of sockets is opened, but some systems might benefit from disabling it. Might be a subject of individual load testing.
* `+A 5` - Sets the asynchronous threads number. Async threads improve I/O operations efficiency by relieving scheduler threads of IO waits.
* `+P 10000000` - Process count limit. This is a maximum allowed number of processes running per node. In general, it should exceed the tripled estimated online user count.
* `-env ERL_MAX_PORTS 250000` - Open port count. This is a maximum allowed number of ports opened per node. In general, it should exceed the tripled estimated online user count. Keep in mind that increasing this number also increases the memory usage by a constant amount, so finding the right balance for it is crucial for every project.
* `-env ERL_FULLSWEEP_AFTER 2` - affects garbage collection. Reduces memory consumption (forces often full g.c.) at the expense of CPU usage.
* `-sasl sasl_error_logger false` - MongooseIM's solution for logging is Lager, so SASL error logger is disabled.

# app.config

A file with Erlang application configuration. To configure it, go to `[MongooseIM root]/rel/files/`.
By default only the following applications can be found there:

* `lager` - check [Lager's documentation](https://github.com/basho/lager) for more information.
   
    Here you can change the logs location and the file names (`file`), as well as the rotation strategy (`size` and `count`) 
   and date formatting (`date`). Ignore the log level parameters - they are overridden with the value in `ejabberd.cfg`.

* `ejabberd` - set `keep_lager_intact` parameter to `true` when you want to use `lager` log level parameters from `app.config`. 
    Missing value or`false` for this parameter means overriding the log levels with the value in `ejabberd.cfg`.

* `ssl` only `session_lifetime` parameter is specified in this file. 
    Its default value is **600s**. 
    This parameter says for how long should the ssl session remain in the cache for further re-use, should `ssl session resumption` happen.


# Configuring TLS: Certificates & Keys

TLS is configured in one of two ways: some modules need a private key and certificate (chain) in __separate__ files, while others need both in a __single__ file. This is because recent additions use OTP's `ssl` library, while older modules use `p1_tls`, respectively.

* Client-to-server connections need both in the __same__ `.pem` file (find more information under *Options* in *Basic Configuration Overview* and *Listener Modules*)
* Server-to-server connections need both in the __same__ `.pem` file (find more information under Listening Ports in *Advanced Configuration Overview*)
* BOSH & Web Sockets use Cowboy, which uses OTP's `ssl` module like all our HTTPS endpoints, so they need them in __separate__ files (find more information in *Listener Modules*)

When the private key and certificate (chain) need to be in the same file, it should suffice to concatenate them.
