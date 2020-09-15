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
* **auth** - Supported client authentication methods and their options.
* **outgoing_pools** - Outgoing connections to external services, including databases, message queues and HTTP services.
* **services** - Internal services like an administration API and system metrics.
* **modules** - [XMPP extension](https://xmpp.org/extensions/) modules, which extend the basic functionality provided by XMPP.
* **shaper** - Traffic shapers that limit the incoming XMPP traffic, providing a safety valve to protect the server.
* **acl** - Access control lists, defining access groups to which connecting users are classified.
* **access** - Access rules, specifying the privileges of the defined access groups.
* **s2s** - Server-to-server connection options, used for connecting federated XMPP servers.
* **host_config** - Configuration options that need to be different for a specific XMPP domain. May contain the following subsections: **general**, **auth**, **modules**, **shaper**, **acl**, **access**, **s2s**.

## Options

* All options except `hosts`, `host`, `host_config`, `listen` and `outgoing_connections` may be used in the `host_config` tuple.

* There are two kinds of local options - those that are kept separately for each domain in the config file (defined inside `host_config`) and the options local for a node in the cluster.

* "global" options are shared by all cluster nodes and all domains.

* Options labeled as "multi" (in this page) can be declared multiple times in a row, e.g. one per domain.

* Section names below correspond with the ones in the file.

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

### Authentication

* **auth_method** (local)
    * **Description:** Chooses an authentication module or a list of modules. Modules from the list are queried one after another until one of them replies positively.
    * **Valid values:** `internal` (Mnesia), `rdbms`, `external`, `anonymous`, `ldap`, `jwt`, `riak`, `http`, `pki`
    * **Warning:** Authentication backends support only specific SASL mechanisms, see [auth backends capabilities](#authentication-backend-capabilities).
    * **Examples:** `rdbms`, `[internal, anonymous]`

* **auth_opts** (local)
    * **Description:** Provides different parameters that will be applied to a chosen authentication method.
                       `auth_password_format` and `auth_scram_iterations` are common to `http`, `rdbms`, `internal` and `riak`.

        * **auth_password_format**
             * **Description:** Decide whether user passwords will be kept plain or hashed in the database.
             Currently, popular XMPP clients support the SCRAM method and it is strongly recommended to use the hashed version.
             MongooseIM supports SHA-1, SHA-224, SHA-256, SHA-384 and SHA-512 for SCRAM hashing which can be provided as an argument and this will result in storing and supporting only hashes specified in the configuration.
             The older XMPP clients can still use the `PLAIN` mechanism. `DIGEST-MD5` is not available with `scram`.
             * **Values:** `plain`, `scram`, `{scram, [sha256]}` (`scram` and `{scram, [sha, sha224, sha256, sha384, sha512]}` are equivalent configurations)
             * **Default:** `scram`

        * **auth_scram_iterations**
             * **Description:** Hash function round count.
               This is a tradeoff between latency and security.
               The higher the value, the more difficult breaking the hashes is: it is a work factor: increasing the count increases the work it requires to compute a full hash, which effectively slows down brute-force attacks.
               But it adds load on both client and server, so this parameter should be tuned as high as the business-rules allow.
               Note that increasing the security of a password has a higher impact over the security of the algorithm, without impacting its load.
               See more information in this [NIST guide, Appendix A.2.2](https://csrc.nist.gov/publications/detail/sp/800-132/final),
             * **Default:** 10000, as recommended in this [XEP](https://xmpp.org/extensions/xep-0438.html#pbkdf2) and this [NIST Guidelines](https://pages.nist.gov/800-63-3/sp800-63b.html#sec5)

        * [`external` backend options](authentication-backends/External-authentication-module.md#configuration-options)

        * [`http` backend options](authentication-backends/HTTP-authentication-module.md#configuration-options)

        * [`jwt` backend options](authentication-backends/JWT-authentication-module.md#configuration-options)

        * [`ldap` backend options](authentication-backends/LDAP-authentication-module.md#configuration-options)

        * [`riak` backend options](authentication-backends/Riak-authentication-module.md#configuration-options)

* **sasl_mechanisms** (local)
    * **Description:** Specifies a list of allowed SASL mechanisms.
    It affects the methods announced during stream negotiation and is enforced eventually (user can't pick mechanism not listed here but available in the source code).
    All SCRAM-SHA mechanisms support channel binding and are advertised as a separate authentication mechanisms that is suffixed with `-PLUS`.
    Please note that the list of advertised authentication mechanisms is filtered out by the supported password formats to assure that it is possible to authenticate using authentication mechanisms that are offered.
    * **Warning:** This list is still filtered by [auth backends capabilities](#authentication-backend-capabilities)
    * **Valid values:** `cyrsasl_scram_sha512_plus, cyrsasl_scram_sha512, cyrsasl_scram_sh384_plus, cyrsasl_scram_sh384, cyrsasl_scram_sha256_plus, cyrsasl_scram_sha256, cyrsasl_scram_sha224_plus, cyrsasl_scram_sha224, cyrsasl_scram_sha1_plus, cyrsasl_scram_sha1, cyrsasl_plain, cyrsasl_anonymous, cyrsasl_oauth, cyrsasl_external, cyrsasl_digest`
    * **Default:** `[cyrsasl_scram_sha512_plus, cyrsasl_scram_sha512, cyrsasl_scram_sh384_plus, cyrsasl_scram_sh384, cyrsasl_scram_sha256_plus, cyrsasl_scram_sha256, cyrsasl_scram_sha224_plus, cyrsasl_scram_sha224, cyrsasl_scram_sha1_plus, cyrsasl_scram_sha1, cyrsasl_plain, cyrsasl_anonymous, cyrsasl_oauth]`

        Please note that configuring the `sasl_mechanisms` parameter will take precedence over the default list.
        Should more than one parameter be configured in the list of `sasl_mechanisms`, the order of how they are listed in the config will be taken as the order in which they are advertised.

    * **Examples:** `[cyrsasl_plain]`, `[cyrsasl_scram_sha256_plus, cyrsasl_anonymous]`
    * **Deprecations:** Please note that the DIGEST-MD5 authentication method `cyrsasl_digest` is deprecated and will be removed in the next release.

* **extauth_instances** (local)
    * **Description:** Specifies a number of workers serving external authentication requests.
    * **Syntax:** `{extauth_instances, Count}.`
    * **Default:** 1

#### Authentication backend capabilities

The table below shows the supported SASL mechanisms for each authentication backend module.

|           | cyrsasl<br>plain | cyrsasl<br>digest | cyrsasl<br>scram_sha* | cyrsasl<br>anonymous | cyrsasl<br>external |
|-----------|:----------------:|:-----------------:|:---------------------:|:--------------------:|:-------------------:|
| internal  |         x        |         x         |           x           |                      |                     |
| rdbms     |         x        |         x         |           x           |                      |                     |
| external  |         x        |                   |                       |                      |                     |
| anonymous |         x        |         x         |           x           |           x          |                     |
| ldap      |         x        |                   |                       |                      |          x          |
| jwt       |         x        |                   |                       |                      |                     |
| riak      |         x        |         x         |           x           |                      |                     |
| http      |         x        |         x         |           x           |                      |                     |
| pki       |                  |                   |                       |                      |          x          |

`cyrsasl_oauth` does not use the auth backends at all and requires the `mod_auth_token` module enabled instead.

`cyrsasl_digest` is deprecated and will be removed in the next release.

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

### Traffic shapers

* **shaper** (multi, global)
    * **Description:** Define a class of a shaper which is a mechanism for limiting traffic to prevent DoS attack or calming down too noisy clients.
    * **Syntax:** `{shaper, AtomName, {maxrate, BytesPerSecond}}`

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

### Miscellaneous

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
