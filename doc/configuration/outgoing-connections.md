MongooseIM can be configured to talk to external services like databases or HTTP servers.
The interface for outgoing connections management is available via the `outgoing_pools` config option for the following types of connections:

* `cassandra` - pool of connections to Cassandra cluster
* `riak` - pool of connections to Riak cluster
* `redis` - pool of connections to Redis server
* `http` - pool of connections to an HTTP(S) server MongooseIM can talk to, for example HTTP authentication backend or HTTP notifications
* `elastic` - pool of connections to ElasticSearch server
* `rdbms` - pool of connections to an RDBMS database
* `rabbit` - pool of connections to a RabbitMQ server
* `ldap` - pool of connections to an LDAP server

* **Syntax:** Each pool is specified in a subsection starting with `[outgoing_pools.type.tag]`, where `type` is one of available connection types and `tag` is an arbitrary value uniquely identifying the pool within its type.
This allows you to create multiple dedicated pools of the same type.

## General pool options

### `outgoing_pools.*.*.scope`
* **Syntax:** string, one of:`"global"`, `"host"`, `"single_host"`
* **Default:** `"global"`
* **Example:** `scope = "host"`

### `outgoing_pools.*.*.host`
* **Syntax:** string
* **Default:** no default; required if `"single_host"` scope is specified
* **Example:** `host = "anotherhost.com"`

`scope` can be set to:

* `global` - meaning that the pool will be started once no matter how many XMPP hosts are served by MongooseIM
* `host` - the pool will be started for each XMPP host or host type served by MongooseIM
* `single_host` - the pool will be started for the selected host or host type only (you must provide the name).

## Worker pool options

All pools are managed by the [inaka/worker_pool](https://github.com/inaka/worker_pool) library.

Available options are:
### `outgoing_pools.*.*.strategy`
* **Syntax:** string, one of:`"best_worker"`, `"random_worker"`, `"next_worker"`, `"available_worker"`, `"next_available_worker"`
* **Default:** `"best_worker"`
* **Example:** `strategy = "available_worker"`

Defines worker selection strategy. Consult worker_pool documentation for details.

### `outgoing_pools.*.*.workers`
* **Syntax:** positive integer
* **Default:** 10 (20 for Cassandra pool)
* **Example:** `workers = 100`

Number of workers to be started by the pool.

### `outgoing_pools.*.*.call_timeout`
* **Syntax:** positive integer
* **Default:** 5000 (60000 for RDBMS pool)
* **Example:** `call_timeout = 3000`

Number of milliseconds after which a call to the pool will time out.

## Connection options

Options specific to a pool connection are defined in a subsection starting with `[outgoing_pools.*.*.connection]`.
For example:

```
[outgoing_pools.rdbms.default]
  scope = "global"
  workers = 5

  [outgoing_pools.rdbms.default.connection]
  ...
```

### RDBMS options

#### `outgoing_pools.rdbms.*.connection.driver`
* **Syntax:** string, one of `"pgsql"`, `"mysql"` or `"odbc"` (a supported driver)
* **Example:** `driver = "psgql"`

Selects the driver for RDBMS connection. The choice of a driver impacts the set of available options.

### ODBC options

#### `outgoing_pools.rdbms.*.connection.settings`
* **Syntax:** string
* **Default:** no default; required if the `"odbc"` driver is specified
* **Example:** `settings = "DSN=mydb"`

ODBC - specific string defining connection parameters.

##### ODBC SSL connection setup

If you've configured MongooseIM to use an ODBC driver, then the SSL options, along other connection options, should be present in the `~/.odbc.ini` file.

To enable SSL connection the `sslmode` option needs to be set to `verify-full`.
Additionally, you can provide the path to the CA certificate using the `sslrootcert` option.

###### Example ~/.odbc.ini configuration

```ini
[mydb]
Driver      = ...
ServerName  = ...
Port        = ...
...
sslmode     = verify-full
sslrootcert = /path/to/ca/cert
```

### Other RDBMS backends

#### `outgoing_pools.rdbms.*.connection.host`
* **Syntax:** string
* **Example:** `host = "localhost"`

#### `outgoing_pools.rdbms.*.connection.database`
* **Syntax:** string
* **Example:** `database = "mim-db"`

#### `outgoing_pools.rdbms.*.connection.username`
* **Syntax:** string
* **Example:** `username = "mim-user"`

#### `outgoing_pools.rdbms.*.connection.password`
* **Syntax:** string
* **Example:** `password = "mim-password"`

#### `outgoing_pools.rdbms.*.connection.keepalive_interval`
* **Syntax:** positive integer
* **Default:** undefined (keep-alive not activated)
* **Example:** `keepalive_interval = 30`

When enabled, MongooseIM will send SELECT 1 query through every DB connection at given interval to keep them open. This option should be used to ensure that database connections are restarted after they became broken (e.g. due to a database restart or a load balancer dropping connections). Currently, not every network-related error returned from a database driver to a regular query will imply a connection restart.

## HTTP options

### `outgoing_pools.http.*.connection.host`
* **Syntax:** `"http[s]://string[:integer]"`
* **Example:** `host = "https://server.com:879"`

### `outgoing_pools.http.*.connection.path_prefix`
* **Syntax:** string
* **Default:** `"/"`
* **Example:** `path_prefix = "/api/auth/"`

Initial part of path which will be common to all calls. Prefix will be automatically prepended to path specified by a call to the pool.

### `outgoing_pools.http.*.connection.request_timeout`
* **Syntax:** positive integer
* **Default:** `2000` (milliseconds)
* **Example:** `request_timeout = 5000`

Number of milliseconds after which http call to the server will time out. It should be lower than `call_timeout` set at the pool level.

HTTP also supports all TLS-specific options described in the TLS section.

## Redis-specific options

Redis can be used as a session manager backend. 
Global distribution (implemented in `mod_global_distrib`) requires Redis pool.

There are two important limitations:

* for a session backend, the `Tag` parameter has to be equal to `default`
* `redis` backend is not compatible with `available_worker` strategy.

### `outgoing_pools.redis.*.connection.host`
* **Syntax:** string
* **Default:** `"127.0.0.1"`
* **Example:** `host = "redis.local"`

### `outgoing_pools.redis.*.connection.port`
* **Syntax:** integer, between 0 and 65535, non-inclusive
* **Default:** `6379`
* **Example:** `port = 9876`

### `outgoing_pools.redis.*.connection.database`
* **Syntax:** non-negative integer
* **Default:** `0`
* **Example:** `database = 2`

Logical database index (zero-based).

### `outgoing_pools.redis.*.connection.password`
* **Syntax:** string
* **Default:** `""`
* **Example:** `password = "topsecret"`

## Riak options

Currently, only one Riak connection pool can exist for each supported XMPP host (the default pool).

!!! WARNING
    `riak` backend is not compatible with `available_worker` strategy.

### `outgoing_pools.riak.*.connection.address`
* **Syntax:** string
* **Example:** `address = "127.0.0.1"`

### `outgoing_pools.riak.*.connection.port`
* **Syntax:** integer
* **Example:** `port = 8087`

### `outgoing_pools.riak.*.connection.credentials`
* **Syntax:** `{user = "username", password = "pass"}`
* **Default:** none
* **Example:** `credentials = {user = "myuser", password = "tisismepasswd"}`

This is optional - setting this option forces connection over TLS

Riak also supports all TLS-specific options described in the TLS section.

## Cassandra options

### `outgoing_pools.cassandra.*.connection.servers`
* **Syntax:** a TOML array of tables containing keys `"ip_adddress"` and `"port"`
* **Default:** `[{ip_address = "localhost", port = 9042}]`
* **Example:** `servers = [{ip_address = "host_one", port = 9042}, {ip_address = "host_two", port = 9042}]`

### `outgoing_pools.cassandra.*.connection.keyspace`
* **Syntax:** string
* **Default:** `"mongooseim"`
* **Example:** `keyspace = "big_mongooseim_database"`

To use plain text authentication (using cqerl_auth_plain_handler module):

### `outgoing_pools.cassandra.*.connection.auth.plain.username`
* **Syntax:** string
* **Example:** `username = "auser"`

### `outgoing_pools.cassandra.*.connection.auth.plain.password`
* **Syntax:** string
* **Example:** `password = "somesecretpassword"`

Support for other authentication modules may be added in the future.

Cassandra also supports all TLS-specific options described in the TLS section.

## Elasticsearch options

Currently, only one pool tagged `default` can be used.

### `outgoing_pools.elastic.default.connection.host`
* **Syntax:** string
* **Default:** `"localhost"`
* **Example:** `host = "otherhost"`

### `outgoing_pools.elastic.default.connection.port`
* **Syntax:** positive integer
* **Default:** `9200`
* **Example:** `port = 9211`

MongooseIM uses [inaka/tirerl](https://github.com/inaka/tirerl) library to communicate with ElasticSearch.
This library uses `worker_pool` in a bit different way than MongooseIM does, so the following options are not configurable:

* `call_timeout` (infinity)
* worker selection strategy (`available_worker` or what's set as `default_strategy` of `worker_pool` application)

The only pool-related variable you can tweak is thus the number of workers.

Run the following function in the MongooseIM shell to verify that the connection has been established:

```erlang
1> mongoose_elasticsearch:health().
{ok,#{<<"active_primary_shards">> => 15,<<"active_shards">> => 15,
       <<"active_shards_percent_as_number">> => 50.0,
       <<"cluster_name">> => <<"docker-cluster">>,
       <<"delayed_unassigned_shards">> => 0,
       <<"initializing_shards">> => 0,
       <<"number_of_data_nodes">> => 1,
       <<"number_of_in_flight_fetch">> => 0,
       <<"number_of_nodes">> => 1,
       <<"number_of_pending_tasks">> => 0,
       <<"relocating_shards">> => 0,
       <<"status">> => <<"yellow">>,
       <<"task_max_waiting_in_queue_millis">> => 0,
       <<"timed_out">> => false,
       <<"unassigned_shards">> => 15}}
```

Note that the output might differ based on your ElasticSearch cluster configuration.

## RabbitMQ options

The `Tag` parameter must be set to `event_pusher` in order to be able to use
the pool for [`mod_event_pusher_rabbit`](../modules/mod_event_pusher_rabbit.md).
Any other `Tag` can be used for other purposes.

### `outgoing_pools.rabbit.*.connection.amqp_host`
* **Syntax:** string
* **Default:** `"localhost"`
* **Example:** `amqp_host = "anotherhost"`

### `outgoing_pools.rabbit.*.connection.amqp_port`
* **Syntax:** integer
* **Default:** `5672`
* **Example:** `amqp_port = 4561`

### `outgoing_pools.rabbit.*.connection.amqp_username`
* **Syntax:** string
* **Default:** `"guest"`
* **Example:** `amqp_username = "corpop"`

### `outgoing_pools.rabbit.*.connection.amqp_password`
* **Syntax:** string
* **Default:** `"guest"`
* **Example:** `amqp_password = "guest"`

### `outgoing_pools.rabbit.*.connection.confirms_enabled`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `confirms_enabled = false`

Enables/disables one-to-one publishers confirms.

### `outgoing_pools.rabbit.*.connection.max_worker_queue_len`
* **Syntax:** non-negative integer or `"infinity"`
* **Default:** `1000`
* **Example:** `max_worker_queue_len = "infinity"`

Sets a limit of messages in a worker's mailbox above which the worker starts dropping the messages. If a worker message queue length reaches the limit, messages from the head of the queue are dropped until the queue length is again below the limit. Use `infinity` to disable.

## LDAP options

### `outgoing_pools.ldap.*.connection.servers`
* **Syntax:** an array of strings
* **Default:** `["localhost"]`
* **Example:** `servers = ["ldap_one", "ldap_two"]`

### `outgoing_pools.ldap.*.connection.port`
* **Syntax:** integer
* **Default:** `389` (or `636` if encryption is enabled)
* **Example:** `port = 800`

### `outgoing_pools.ldap.*.connection.rootdn`
* **Syntax:** string
* **Default:** empty string 
* **Example:** `rootdn = "cn=admin,dc=example,dc=com"`

Leaving out this option makes it an anonymous connection, which most likely is what you want.

### `outgoing_pools.ldap.*.connection.password`
* **Syntax:** string
* **Default:** empty string
* **Example:** `password = "topsecret"`

### `outgoing_pools.ldap.*.connection.connect_interval`
* **Syntax:** integer
* **Default:** `10000`
* **Example:** `connect_interval = 20000`

Reconnect interval after a failed connection.

### `outgoing_pools.ldap.*.connection.encrypt`
* **Syntax:** string, one of: `"none"` or `"tls"`
* **Default:** `"none"`
* **Example:** `encrypt = "tls"`

LDAP also supports all TLS-specific options described in the TLS section (provided `encrypt` is set to `tls`).

## TLS options

TLS options for a given pool type/tag pair are defined in a subsection starting with `[outgoing_pools.[pool_type].[pool_tag].connection.tls]`.

### `outgoing_pools.*.*.connection.tls.required`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `tls.required = true`

This option is Postgresql-specific, doesn't apply in other cases.

### `outgoing_pools.*.*.connection.tls.verify_peer`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `tls.verify_peer = true`

Enforces verification of a client certificate. Requires a valid `cacertfile`.

### `outgoing_pools.*.*.connection.tls.certfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.certfile = "server.pem"`

Path to the X509 PEM file with a certificate and a private key (not protected by a password). 
If the certificate is signed by an intermediate CA, you should specify here the whole CA chain by concatenating all public keys together and appending the private key after that.

### `outgoing_pools.*.*.connection.tls.cacertfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.cacertfile = "ca.pem"`

Path to the X509 PEM file with a CA chain that will be used to verify clients. It won't have any effect if `verify_peer` is not enabled.

### `outgoing_pools.*.*.connection.tls.dhfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.dhfile = "dh.pem"`

Path to the Diffie-Hellman parameter file.

### `outgoing_pools.*.*.connection.tls.keyfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `tls.keyfile = "key.pem"`

Path to the X509 PEM file with the private key.

### `outgoing_pools.*.*.connection.tls.password`
* **Syntax:** string
* **Default:** not set
* **Example:** `tls.password = "secret"`

Password to the X509 PEM file with the private key.

### `outgoing_pools.*.*.connection.tls.ciphers`
* **Syntax:** string with the OpenSSL cipher suite specification
* **Default:** not set, all supported cipher suites are accepted
* **Example:** `tls.ciphers = "ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384"`

Cipher suites to use. Please refer to the [OpenSSL documentation](http://www.openssl.org/docs/man1.0.2/apps/ciphers.html) for the cipher string format. For allowed values, see the [Erlang/OTP SSL documentation](https://erlang.org/doc/man/ssl.html#type-ciphers).

### `outgoing_pools.*.*.connection.tls.versions`
* **Syntax:** list of strings
* **Default:** not set, all supported versions are accepted
* **Example:** `tls.versions = ["tlsv1.2", "tlsv1.3"]`

Cipher suites to use. For allowed values, see the [Erlang/OTP SSL documentation](https://erlang.org/doc/man/ssl.html#type-ciphers)

### `outgoing_pools.*.*.connection.tls.server_name_indication`
* **Syntax:** boolean
* **Default:** `false`, but enabled if the `verify_peer` option is set to `true`
* **Example:** `tls.server_name_indication = false`

Enables SNI extension to TLS protocol. If set to `true`, the `server_name_indication_host` option should be provided.

### `outgoing_pools.*.*.connection.tls.server_name_indication_host`
* **Syntax:** string
* **Default:** not set
* **Example:** `tls.server_name_indication_host = "domain.com"`

Domain against which the certificates will be checked, using SNI. It can be specified only when `server_name_indication` is set to `true`.

### `outgoing_pools.*.*.connection.tls.server_name_indication_protocol`
* **Syntax:** string, one of "default" or "https"
* **Default:** "default"
* **Example:** `tls.server_name_indication_protocol = "https"`

See [https://www.erlang.org/doc/man/public_key.html#pkix_verify_hostname_match_fun-1] for an explanation. You'd usually want to set it to https for reasons described in [https://erlef.github.io/security-wg/secure_coding_and_deployment_hardening/ssl.html].
