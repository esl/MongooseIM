For advanced configuration use the following files: `ejabberd.cfg`, `vm.args` and `app.config`. The first one contains all pure XMPP settings, the second affects Erlang VM behaviour (performance tuning, node name) and third usually has to be modified to change low-level logging parameters. Since you've gotten this far, we assume you're already familiar with Erlang syntax.

# ejabberd.cfg

This file consists of multiple erlang tuples, terminated with a period. It can be found in `[MongooseIM root]/rel/files/`.

The tuple order is important, unless the no host_config option is set. Retaining the default layout is recommended so that experienced MongooseIM users can smoothly traverse the file. 

`ejabberd.cfg` is full of useful comments and in most cases they should be sufficient help in changing the configuration.

## Options

* All options except `hosts`, `host` and `host_config` can be used in `host_config` tuple.

* There are two kinds of local options - those that are kept separately for each domain in config file (defined inside `host_config`) and the options local for a node in the cluster.

* "global" options are shared by all cluster nodes and all domains.

* "multi" options can be declared multiple times in a row, e.g. one per domain.

* Sections names below correspond with the ones in the file.

### Override stored options

* **override_global, override_local, override_acls** - optional
    * **Description:** Will cause MongooseIM to erase all global/local/acl options in database respectively. This ensures that ALL settings of specific type will be reloaded on startup.

### Debugging

* **loglevel** (local)
    * **Description:** Log level configured with integer: 0 (disabled), 1 (critical), 2 (error), 3 (warning), 4 (info), 5 (debug). Recommended values for production systems are 2 or 3 (5 is for development).

* **alarms** (global)
    * **Description:** Definition of the alarms to be set inside the node.
    * **Alarm types:**
        * `long_gc` - when garbage collection time exceeds given time in milliseconds
        * `large_heap` - when process heap exceeds given size in bytes
    * **Alarm handlers:**
        * `alarms_basic_handler` - logs alarms and stores a brief alarm summary
        * `alarms_folsom_handler` - stores alarm details in folsom metrics
    * **Example:** `{alarms, [{long_gc, 10000}, {large_heap, 1000000}, {handlers, [alarms_basic_handler]}]}.`

* **watchdog_admins** (global)
    * **Descritption:** List of JIDs, that should receive information about alarms.

### Served hostnames

* **hosts** (global)
    * **Description:** List of domains supported by this cluster.
    * **Warning:** extension modules and database backends will be started separately for every domain, so when increasing the number of domains please make sure you have enough resources available (e.g. connection limit set in DBMS).
    * **Example:** `["localhost", "domain2"]`

* **route_subdomain** (local)
    * **Description:** If stanza is addressed to a subdomain of the served domain and this option is set to `s2s`, such stanza will be transmitted over s2s. Without it, MongooseIM will try to route the stanza to one of internal services.
    * **Note:** `s2s` is only valid value. Any other will simply disable the feature.

### Listening ports

* **listen** (local)
    * **Description:** List of modules handling incoming connections. By default, 3 are enabled: `ejabberd_cowboy`, `ejabberd_c2s` and `ejabberd_s2s_in`. They accept XMPP, BOSH, Websocket and S2S connections (plus queries to metrics API).
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
    * **Description:** Defines list of accepted SSL ciphers in **outgoing** S2S connection. Please refer to [OpenSSL documentation](http://www.openssl.org/docs/apps/ciphers.html) for cipher string format.
    * **Default:** As of OpenSSL 1.0.0 it's `ALL:!aNULL:!eNULL` ([source](https://www.openssl.org/docs/apps/ciphers.html#CIPHER_STRINGS))

* **domain_certfile** (multi, global)
    * **Description:** Overrides common certificates with new ones specific for chosen XMPP domains.
    * **Syntax:** `{domain_certfile, "example.com", "/path/to/example.com.pem"}.`

* **s2s_default_policy** (local)
    * **Description:** Default policy for new S2S (server-to-server) **both incoming and outgoing** connection to/from unknown remote server.

* **s2s_host** (multi, local)
    * **Description:** Allows black/whitelisting S2S destinations.
    * **Syntax:** `{ {s2s_host, "somehost.com"}, allow|deny }.`

* **outgoing_s2s_port** (local)
    * **Description:** Defines a port to be used for outgoing S2S connections. Cannot be random.
    * **Default:** 5269

* **s2s_addr** (multi, global)
    * **Description:** Override DNS lookup for specific non-local XMPP domain and use predefined server IP and port for S2S connection.
    * **Syntax:** `"{ {s2s_addr, \"some-domain\"}, { {10,20,30,40}, 7890 } }."`

* **outgoing_s2s_options** (global)
    * **Description:** Specifies order of address families to try when establishing S2S connection and connection timeout (in milliseconds or atom `infinity`).
    * **Default:** `{outgoing_s2s_options, [ipv4, ipv6], 10000}.`
    * **Family values:** `inet4`/`ipv4`, `inet6`/`ipv6`

### Session backend

* **sm_backend** (global)
    * **Description:** Backend for storing user session data. Currently all nodes in a cluster must have access to a complete session database. Valid backends are `mnesia` and `redis`. Mnesia is sufficient in most cases, use Redis only in large deployments.
    * **Mnesia:** `{sm_backend, {mnesia, []}}`
    * **Redis:** `{redis, [{pool_size, Size}, {worker_config, [{host, "Host"}, {port, Port}]}]}}`

### Authentication

* **auth_method** (local)
    * **Description:** Chooses authentication module or list of modules. Modules from a list are queried one after another until one of them replies positively.
    * **Valid values:** `internal` (Mnesia), `odbc`, `external`, `anonymous`, `ldap`
    * **Warning:** `external` and `ldap` limit SASL mechanisms list to `PLAIN` and `ANONYMOUS`.
    * **Examples:** `odbc`, `[internal, anonymous]`

* **auth_password_format** (local)
    * **Description:** Decide whether user passwords will be kept plain or hashed in the database. As of today popular XMPP clients support the SCRAM method, so it is strongly recommended to use hashed version, older ones can still use `PLAIN` mechiansm. `DIGEST-MD5` is not available with `scram`.
    * **Values:** `plain`, `scram`
    * **Default:** `plain` (for compatibility reasons, might change soon)

* **auth_scram_iterations** (local)
    * **Description:** Hash function round count. It is best to set custom value that is not too low, making breaking hashes much more difficult.
    * **Default:** 4096

* **ext_auth_script** (local)
    * **Description:** Path to the authentication script used by `external` auth module. Script API specification can be found in [[External authentication script]].

* **LDAP-related options**
  * [[Everything about LDAP]]

### Database setup

`odbc` prefixes may be misleading. Such options apply to all kinds of DB connections, not only pure ODBC.

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

### Traffic shapers

* **shaper** (mutli, global)
    * **Description:** Define a class of shaper, mechanism for limiting traffic to prevent DoS attack or calming down too noisy clients.
    * **Syntax:** `{shaper, AtomName, {maxrate, BytesPerSecond}}`

* **max_fsm_queue** (local)
    * **Description:** When enabled, will terminate certain processes (e.g. client handlers) that exceed message limit, to prevent resource exhaustion. **Use with caution!**

### Access control lists

* **acl** (multi)
    * **Description:** Define access control list class.
    * **Syntax:** `{acl, AtomName, Definition}`
    * **Regexp format:** Syntax for `_regexp` can be found in [Erlang documentation](http://www.erlang.org/doc/man/re.html) - it's based on AWK syntax. For `_glob` use `sh` regexp syntax.
    * **Valid definitions:**
        * `all`
        * `{user, U}` - check if username equals `U` and the domain equals the one specified by module executing the check or domain is on served domains list (`hosts` option), if module does `global` check
        * `{user, U, S}` - check if the username equals `U` and the domain equals `S`
        * `{server, S}` - check if the domain equals `S`
        * `{resource, R}` - check if the resource equals `R`
        * `{user_regexp, UR}` - perform regular expression `UR` check on username and check server name like in `user`
        * `{user_regexp, UR, S}` - perform regular expression `UR` check on username and check if domain equals `S`
        * `{server_regexp, SR}` - perform regular expression `SR` check on domain
        * `{resource_regexp, RR}` - perform regular expression `SR` check on resource
        * `{node_regexp, UR, SR}` - username must match `UR` and domain must match `SR`
        * `{user_glob, UR}` - like `_regexp` variant but with `sh` syntax
        * `{server_glob, UR}` - like `_regexp` variant but with `sh` syntax
        * `{resource_glob, UR}` - like `_regexp` variant but with `sh` syntax
        * `{node_glob, UR}` - like `_regexp` variant but with `sh` syntax
        * `{shared_group, G}` - check if user is in shared group `G` in domain specified by module executing the check
        * `{shared_group, G, H}`- check if users is in shared group `G` in domain `H`

### Access rules

* **access** (multi, global)
    * **Description:** Define access rule for internal checks. Configuration file contains all built-in ones with proper comments.
    * **Syntax:** `{access, AtomName, [{Value, AclName}]}`

* **registration_timeout** (local)
    * **Description:** Limits registration frequency from single IP. Valid values are `infinity` or number of seconds.

### Default language

* **language** (global)
    * **Description:** Default language for messages sent by server to users. You can get a full list of supported codes by executing `cd [MongooseIM root] ; ls apps/ejabberd/priv/*.msg | awk '{split($0,a,"/"); split(a[4],b,"."); print b[1]}'` (`en` is not listed there)
    * **Default:** `en`

### Modules

For specific configuration, please refer to [Modules](advanced-configuration/Modules.md) page.

* **modules** (local)
    * **Description:** List of enabled modules with their options.

### Per-domain configuration

The `host_config` allows configuring most options separately for specific domains served by the cluster. It is best to put `host_config` tuple right after the global section it overrides/complements or even at the end of `ejabberd.cfg`.

* **host_config** (multi, local)
    * **Syntax:** `{host_config, Domain, [ {{add, modules}, [{mod_some, Opts}]}, {access, c2s, [{deny, local}]}, ... ]}.`

# vm.args

This file contains parameters passed directly to the Erlang VM. It can be found in `[MongooseIM root]/rel/files/`.

Section below describes the default options.

## Options

*    `-sname` - Erlang node name. Can be changed to `name`, if necessary
* `-setcookie` - Erlang cookie. All nodes in a cluster must use the same cookie value.
* `+K` - Enables kernel polling. It improves the stability when a large number of sockets is opened, but some systems might benefit from disabling it. Might be a subject of individual load testing.
* `+A 5` - Sets the asynchronous threads number. Async threads improve I/O operations efficiency by relieving scheduler threads of IO waits.
* `+P 10000000` - Process count limit. This is a maximum allowed number of processes running per node. In general, it should exceed the tripled estimated online user count.
* `-env ERL_MAX_PORTS 250000` - Open port count. This is a maximum allowed number of ports opened per node. In general, it should exceed the tripled estimated online user count. Keep in mind that increasing this number also increases memory usage by a constant amount, so finding the right balance for it is crucial for every project.
* `-env ERL_FULLSWEEP_AFTER 2` - affects garbage collection. Reduces memory consumption (forces often full g.c.) at the expense of CPU usage.
* `-sasl sasl_error_logger false` - MongooseIM's solution for logging is Lager, so SASL error logger is disabled.

# app.config

A file with Erlang application configuration. It can be found in `[MongooseIM root]/rel/files/`. By default only Lager config can be found there. Check [Lager's documentation](https://github.com/basho/lager) for more information.

Here you can change logs location and file names (`file`), rotation strategy (`size` and `count`) and date formatting (`date`). Ignore log level parameters - they are overridden with the value in `ejabberd.cfg`.