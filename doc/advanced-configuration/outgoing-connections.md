# Outgoing connections

MongooseIM can be configured to talk to external service like databases or HTTP servers in order to get or set the required data.
The interface for outgoing connections management was unified and is now available via the `outgoing_pools` config option for the following type of connections:

* `cassandra` - pool of connections to Cassandra cluster
* `riak` - pool of connections to Riak cluster
* `redis` - pool of connections to Redis server
* `http` - pool of connections to various HTTP(S) servers MongooseIM can talk to, in example HTTP authentication backend or HTTP notifications
* `elastic` - pool of connections to ElasticSearch server
* `rdbms` - pool of connections to an RDBMS database
* `rabbit` - pool of connections to a RabbitMQ server
* `ldap` - pool of connections to an LDAP server
* `generic` - pool of generic workers not associated directly with a particular connection (SNS, PushNotifications)

All the above pools are managed by [inaka/worker_pool](https://github.com/inaka/worker_pool) library.

Every entry in the `outgoing_pools` is a 5-element tuple:

```erlang
{Type, Host, Tag, PoolOptions, ConnectionOptions}
```

Where:

* `Type` is one of the types listed above
* `Host` can be set to:
    * `global` - meaning the pool will started once no matter how many XMPP hosts served by MongooseIM
    * `host` - the pool will be started for all the XMPP hosts served by MongooseIM
    * a binary representing a specific XMPP host like `<<"domain1.chat.im">>`
* `Tag` is a name to distinguish pools with the same `Type` and `Host` parameter.
* `PoolOptions` is a list of `{key, value}` pairs as defined in [worker_pool doc](https://github.com/inaka/worker_pool#starting-a-pool)
   with the following exception:
    * `strategy` - specifies the worker selection strategy for the given pool, default is `best_worker`,
      more details on this can be found in [Choosing strategy in worker_pool doc](https://github.com/inaka/worker_pool#choosing-a-strategy)
      *WARNING:* `redis` and `riak` backends are not compatible with `available_worker` strategy.
    * `call_timeout` - specifies the timeout, in milliseconds, for a call operation to the pool
* `ConnectionOptions` - options list passed to the `start` function of the pool type


### Examples

Provided MongooseIM serves domains `<<"a.com">>`, `<<"b.com">>`, `<<"c.eu">>` and `<<"d.eu">>`
the following `outgoing_pools` configuration:

```erlang
{redis, <<"a.com">>, default, PoolOpts, ConnOptsForDomain1},
{redis, host, default, PoolOpts, ConnOpts},
{redis, <<"c.eu", default, PoolOpts, ConnOptsForDomain2}
```

will be expanded to the following configuration:

```erlang
{redis, <<"a.com">>, default, PoolOpts, ConnOptsForDomain1},
{redis, <<"b.com">>, default, PoolOpts, ConnOpts},
{redis, <<"c.eu", default, PoolOpts, ConnOptsForDomain2},
{redis, <<"d.eu">>, default, PoolOpts, ConnOpts}
```

## RDBMS connection setup

An example RDBMS configuration inside `outgoing_pools` may look like this:

```erlang
{outgoing_pools, [
 {rdbms, global, default, [{workers, 5}],
  [{server, {mysql, "localhost", 3306, "mydb", "user", "passwd"}}]}
]}.
```

This configuration will create a default, global pool of 5 connections to a mysql database.
We might also want to add a dedicated pool for a specific host:

```erlang
{outgoing_pools, [
 {rdbms, global, default, [{workers, 5}],
  [{server, {mysql, "localhost", 3306, "mydb", "user", "passwd"}}]},
 {rdbms, "myhost.com", default, [{workers, 3}],
  [{server, {mysql, "localhost", 3306, "mydb", "user", "passwd"}}]}
]}.
```

Please remember that SQL databases require creating a schema.
See [Database backends configuration](./database-backends-configuration.md) for more information.
Also see [Advanced configuration](../Advanced-configuration.md) for additional options that influence RDBMS connections.
Currently all pools must use the same RDBMS type (e.g. `mysql`, `pgsql`).

### Connection options

* **server**
    * **Description:** SQL DB connection configuration. Currently supported DB types are `mysql` and `pgsql`.
    * **Syntax:** `{server, {Type, Host, Port, DBName, Username, Password}}.` **or** `{server, "<ODBC connection string>"}`
    * **Default:** `undefined`

* **keepalive_interval**
    * **Description:** When enabled, will send `SELECT 1` query through every DB connection at given interval to keep them open.
    This option should be used to ensure that database connections are restarted after they became broken (e.g. due to a database restart or a load balancer dropping connections).
    Currently, not every network related error returned from a database driver to a regular query will imply a connection restart.
    * **Syntax:** `{keepalive_interval, IntervalSeconds}.`
    * **Example:** `{keepalive_interval, 30}.`
    * **Default:** `undefined`

#### MySQL and PostgreSQL SSL connection setup

In order to establish a secure connection with a database, additional options must be passed in the `server` tuple.
Here is the proper syntax:

`{server, {Type, Host, Port, DBName, Username, Password, SSL}}.`

##### MySQL

SSL configuration options for MySQL:

* **SSL**
    * **Description:** Specifies SSL connection options.
    * **Syntax:** `[Opt]`
    * **Supported values:** The options are just a **list** of Erlang `ssl:ssl_option()`. More details can be found in [official Erlang ssl documentation](http://erlang.org/doc/man/ssl.html).

###### Example configuration

An example configuration can look as follows:

```erlang
{outgoing_pools, [
 {rdbms, global, default, [{workers, 5}],
  [{server, {mysql, "localhost", 3306, "mydb", "mim", "mimpass",
             [{verify, verify_peer}, {cacertfile, "path/to/cacert.pem"},
              {server_name_indication, disable}]}}]}
]}.
```

##### PostgreSQL

SSL configuration options for PGSQL:

* **SSL**
    * **Description:** Specifies general options for SSL connection.
    * **Syntax:** `[SSLMode, SSLOpts]`

* **SSLMode**
    * **Description:** Specifies a mode of SSL connection. Mode expresses how much the PostgreSQL driver carries about security of the connections.
    For more information click [here](https://github.com/epgsql/epgsql).
    * **Syntax:** `{ssl, Mode}`
    * **Supported values:** `false`, `true`, `required`

* **SSLOpts**
    * **Description:** Specifies SSL connection options.
    * **Syntax:** `{ssl_opts, [Opt]}`
    * **Supported values:** The options are just a **list** of Erlang `ssl:ssl_option()`. More details can be found in [official Erlang ssl documentation](http://erlang.org/doc/man/ssl.html).

###### Example configuration

An example configuration can look as follows:

```erlang
{outgoing_pools, [
 {rdbms, global, default, [{workers, 5}],
  [{server, {pgsql, "localhost", 5432, "mydb", "mim", "mimpass",
             [{ssl, required}, {ssl_opts, [{verify, verify_peer}, {cacertfile, "path/to/cacert.pem"}]}]}}]}
]}.
```

##### ODBC SSL connection setup

If you've configured MongooseIM to use an ODBC driver, i.e. you've provided an ODBC connection string in the `server` option, e.g.

```erlang
{server, "DSN=mydb"}.
```

then the SSL options, along other connection options, should be present in the `~/.odbc.ini` file.

To enable SSL connection the `sslmode` option needs to be set to `verify-full`.
Additionally, you can provide the path to the CA certificate using the `sslrootcert` option.

###### Example ~/.odbc.ini configuration

```
[mydb]
Driver      = ...
ServerName  = ...
Port        = ...
...
sslmode     = verify-full
sslrootcert = /path/to/ca/cert
```

## HTTP connections setup

Some MongooseIM modules need an HTTP connection to external service.
These pools need to be configured and started before the module needs them.
Below is a sample configuration:

```erlang
{outgoing_pools, [
 {http, global, default, PoolOptions, ConnectionOptions}
]}.
```

`PoolOptions` are described above, below there are the recommended `PoolOptions` for `HTTP` pools:

* `strategy` - the recommended value is `available_worker`
* `call_timeout` - it should be equal or longer than the value set in `request_timeout` below.

`ConnectionOptions` can take the following `{key, value}` pairs:

* `{server, HostName}` - string, default: `"http://localhost"` - the URL of the destination HTTP server (including a port number if needed).
* `{path_prefix, Prefix}` - string, default: `"/"` - the part of the destination URL that is appended to the host name (`host` option).
* `{request_timeout, TimeoutValue}` - non-negative integer, default: `2000` - maximum number of milliseconds to wait for the HTTP response.

##### Example configuration

```Erlang
{outgoing_pools, [
  {http, global, http_auth,
   [{strategy, available_worker}], [{server, "https://my_server:8080"}]}
]}.
```

## Redis connection setup

Session manager backend or `mod_global_distrib` requires a redis pool defined in the `outgoing_pools` option.
They can be defined as follows:

```erlang
{ougtoing_pools, [
 {redis, global, Tag, WorkersOptions, ConnectionOptions}
]}.
```

*WARNING:* `redis` backend is not compatible with `available_worker` strategy.

The `Tag` parameter can only be set to `default` for a session backend.
For `mod_global_distrib` module it can take any value (default is **global_distrib**) but the name needs to be passed as:

```erlang
{redis, [{pool, Tag}]}
```
in the `mod_global_distrib` options. See [mod_global_distrib doc](../modules/mod_global_distrib.md) for details and examples.

The `ConnectionOptions` list can take following parametrs as `{key, value`} pairs:

* **host** (default: **"localhost"**) the hostname or IP address of the Redis server
* **port** (default: **6379**) the port of the Redis server
* **database** (default: **0**) number of the database to use by the pool
* **password** (default: **""**) the password to the database (if set).

### Example

```erlang
{ougtoing_pools, [
 {redis, global, default, [{strategy, random_worker}],
  [{host, "198.172.15.12"},
   {port, 9923}]}
]}.
```

## Riak connection setup

Currently only one Riak connection pool can exist for each supported XMPP host.
It is configured with the following tuple inside the `outgoing_pools` config option.

```erlang
{outgoing_pools, [
 {riak, global, default, [{workers, 20}], [{address, "127.0.0.1"}, {port, 8087}]}
]}.
```

*WARNING:* `riak` backend is not compatible with `available_worker` strategy.

#### Riak SSL connection setup

Using SSL for Riak connection requires passing extra options in `ConnectionOptions` to the
aforementioned `riak` tuple.

Here is the proper syntax:

* **Credentials**
    * **Description:** Specifies credentials to use to connect to the database.
    * **Syntax:** `{credentials, User, Password}`
    * **Supported values** `User` and `Password` are strings with a database username and password respectively.

* **CACert**
    * **Description:** Specifies a path to the CA certificate that was used to sign the database certificates.
    * **Syntax:** `{cacertfile, Path}`
    * **Supported values** `Path` is a string with a path to the CA certificate file.

* **SSL_Opts**
    * **Description**: list of SSL options as defined in [Erlang SSL module doc](http://erlang.org/doc/man/ssl.html)
      They will be passed verbatim to the `ssl:connect` function.
    * **Syntax** `{ssl_opts, ListOfSSLOpts}`
    * **Example**:

```erlang
{ssl_opts, [{ciphers, ["AES256-SHA", "DHE-RSA-AES128-SHA256"]},
            {server_name_indication, disable}]}
```

##### Example configuration

An example configuration can look as follows:

```erlang
{outgoing_pools, [
 {riak, global, default, [{workers, 20}, {strategy, next_worker}],
   [{address, "127.0.0.1"}, {port, 8087},
    {credentials, "username", "pass"},
    {cacertfile, "path/to/cacert.pem"}]}
]}.
```

## Cassandra connection setup

The minimum pool definition for cassandra workers looks as follows:

```erlang
{outgoing_pools, [
 {cassandra, global, default, [], []}
]}.
```

In this case MongooseIM will by default try to connect to Cassandra server on "localhost" and port 9042.
The keyspace used in queries will be `mongooseim`.


#### ConnectionOptions

The `ConnectionOptions` list can take following parameters as `{key, Value}` pairs:

* **servers** - A list of servers in Cassandra cluster in `{HostnameOrIP, Port}` format.
* **keyspace** - A name of keyspace to use in queries executed in this pool.
* You can find a full list in `cqerl` [documentation](https://github.com/matehat/cqerl#all-modes).

#### Example

```
{cassandra, global, default, [],
  [
   {servers, [{"cassandra_server1.example.com", 9042}, {"cassandra_server2.example.com", 9042}] },
   {keyspace, "big_mongooseim"}
  ]
 ]}
```

#### SSL connection setup

In order to establish a secure connection to Cassandra you must make some changes in the MongooseIM and Cassandra configuration files.

##### Create server keystore
Follow [this](https://docs.datastax.com/en/cassandra/3.0/cassandra/configuration/secureSSLCertWithCA.html) guide if you need to create certificate files.

##### Change the Cassandra configuration file
Find `client_encryption_options` in `cassandra.yaml` and make these changes:

```yaml
client_encryption_options:
    enabled: true
    keystore: /your_certificate_directory/server.keystore
    keystore_password: your_password
```

Save the changes and restart Cassandra.

##### Enable MongooseIM to connect with SSL
An SSL connection can be established with both self-signed and CA-signed certificates.

###### Self-signed certificate

Add the following to `ConnectionOptions` list:

```erlang
{ssl, [{verify, verify_none}]}
```

Save the changes and restart MongooseIM.

###### CA-signed certificate

Add the following to `ConnectionOptions` list:
```
{ssl, [{cacertfile,
        "/path/to/rootCA.pem"},
       {verify, verify_peer}]}
```
Save the changes and restart MongooseIM.

##### Testing the connection

Make sure Cassandra is running and then run MongooseIM in live mode:

```bash
 $ ./mongooseim live
 $ (mongooseim@localhost)1> cqerl:get_client(default).
 {ok,{<0.474.0>,#Ref<0.160699839.1270874114.234457>}}
 $ (mongooseim@localhost)2> sys:get_state(pid(0,474,0)).
 {live,{client_state,cqerl_auth_plain_handler,undefined,
                    undefined,
                    {"localhost",9042},
                    ssl,
                    {sslsocket,{gen_tcp,#Port<0.8458>,tls_connection,undefined},
                               <0.475.0>},
                    undefined,mongooseim,infinity,<<>>,undefined,
                    [...],
                    {[],[]},
                    [0,1,2,3,4,5,6,7,8,9,10,11|...],
                    [],hash,
                    {{"localhost",9042},
                     [...]}}}
```

If no errors occurred and your output is similar to the one above then your MongooseIM and Cassandra nodes can communicate over SSL.

## ElasticSearch connection setup

A connection pool to ElasticSearch can be configured as follows:

```erlang
{outgoing_pools, [
 {elastic, global, default, [], [{host, "localhost"}]}
]}.
```

Currently only one pool with tag `default` can be used.

MongooseIM uses [inaka/tirerl](https://github.com/inaka/tirerl) library to communicate with ElasticSearch.
This library uses `worker_pool` in a bit different way than MongooseIM does, so the following options are not configurable via `WPoolOpts`:

* `call_timeout` (inifinity)
* worker selection strategy (`available_worker` or what's set as `default_strategy` of `worker_pool` application)
* `overrun_warning` (infinity)
* `overrun_handler`, ({error_logger, warning_report})

Other `worker_pool` options are possible to set.

In `ConnectionOpts`  you can add (as `{key, value}` pairs):

* `host` (default: `"localhost"`) - hostname or IP address of ElasticSearch node
* `port` (default: `9200`) - port the ElasticSearch node's HTTP API is listening on

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

## RabbitMQ connection setup

RabbitMQ backend for [`mod_event_pusher`](../modules/mod_event_pusher.md)
requires a `rabbit` pool defined in the `outgoing_pools` option.
They can be defined as follows:

```erlang
{ougtoing_pools, [
 {rabbit, host, Tag, WorkersOptions, ConnectionOptions}
]}.
```

Notice that `Host` parameter is set to atom `host`. This basically means that
MongooseIM will start as many `rabbit` pools as XMPP hosts are served by
the server.

The `Tag` parameter must be set to `event_pusher` in order to be able to use
the pool for [`mod_event_pusher_rabbit`](../modules/mod_event_pusher_rabbit.md).
Any other `Tag` can be used for any other RabbitMQ connection pool.

The `ConnectionOptions` list can take following parameters as `{key, value`} pairs:

* **amqp_host** (default: `"localhost"`) - Defines RabbitMQ server host (domain or IP address; both as a string);
* **amqp_port** (default: `5672`) - Defines RabbitMQ server AMQP port;
* **amqp_username** (default: `"guest"`) - Defines RabbitMQ server username;
* **amqp_password** (default: `"guest"`) - Defines RabbitMQ server password;
* **confirms_enabled** (default: `false`) - Enables/disables one-to-one publishers confirms;
* **max_worker_queue_len** (default: `1000`; use `infinity` to disable it) -
Sets a limit of messages in a worker's mailbox above which the worker starts
dropping the messages. If a worker message queue length reaches the limit,
messages from the head of the queue are dropped until the queue length is again
below the limit.

### Example

```erlang
{ougtoing_pools, [
 {rabbit, host, event_pusher, [{workers, 20}],
  [{amqp_host, "localhost"},
   {amqp_port, 5672},
   {amqp_username, "guest"},
   {amqp_password, "guest"},
   {confirms_enabled, true},
   {max_worker_queue_len, 100}]}
]}.
```

## LDAP connection setup

To configure a pool of connections to an LDAP server, use the following syntax:

```erlang
{ldap, Host, Tag, PoolOptions, ConnectionOptions}
```

### Connection options

The following options can be specified in the `ConnectionOptions` list:

* **servers**
    * **Description:** List of IP addresses or DNS names of your LDAP servers. They are tried sequentially until the connection succeeds.
    * **Value:** A list of strings
    * **Default:** `["localhost"]`
    * **Example:** `["primary-ldap-server.example.com", "secondary-ldap-server.example.com"]`

* **encrypt**
    * **Description:** Enable connection encryption with your LDAP server.
        The value `tls` enables encryption by using LDAP over SSL. Note that STARTTLS encryption is not supported.
    * **Values:** `none`, `tls`
    * **Default:** `none`

* **tls_options**
    * **Description:** Specifies TLS connection options. Requires `{encrypt, tls}` (see above).
    * **Value:** List of `ssl:tls_client_option()`. More details can be found in the [official Erlang ssl documentation](http://erlang.org/doc/man/ssl.html).
    * **Default:** no options
    * **Example:** `[{verify, verify_peer}, {cacertfile, "path/to/cacert.pem"}]`

* **port**
    * **Description:** Port to connect to your LDAP server.
    * **Values:** Integer
    * **Default:** 389 if encryption is disabled. 636 if encryption is enabled.

* **rootdn**
    * **Description:** Bind DN
    * **Values:** String
    * **Default:** empty string which is `anonymous connection`

* **password**
    * **Description:** Bind password
    * **Values:** String
    * **Default:** empty string

* **connect_interval**
    * **Description:** Interval between consecutive connection attempts in case of connection failure
    * **Value:** Integer (milliseconds)
    * **Default:** 10000

### Example

A pool started for each host with the `default` tag and 5 workers. The LDAP server is at `ldap-server.example.com:389`. MongooseIM will authenticate as `cn=admin,dc=example,dc=com` with the provided password.

```erlang
{outgoing_pools, [
 {ldap, host, default, [{workers, 5}],
  [{servers, ["ldap-server.example.com"]},
   {rootdn, "cn=admin,dc=example,dc=com"},
   {password, "ldap-admin-password"}]
 }
]}.
```
