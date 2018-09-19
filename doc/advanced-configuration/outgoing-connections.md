# Outgoing connections

MongooseIM can be configured to talk to external service like databases or HTTP servers in order to get or set the required data.
The interface for outgoing connections management was unified and is now available via the `outgoing_pools` config option for the following type of connections:

* `cassandra` - pool of connections to Cassandra cluster
* `riak` - pool of connections to Riak cluster
* `redis` - pool of connections to Redis server
* `http` - pool of connections to various HTTP(S) servers MongooseIM can talk to, in example HTTP authentication backend or HTTP notifications
* `elastic` - pool of connections to ElasticSearch server
* `generic` - pool of generic workers not assosiated directly with a paritcualr connection (SNS, PushNotifications)

All the above pools are managed by [inaka/worker_pool](https://github.com/inaka/worker_pool) library.

Every entry in the `outgoing_pools` is a 5 element tuple:

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
* `PoolOptions` is a list of `{key, value}` pairs as defined in [worker_pool doc](https://github.com/inaka/worker_pool#choosing-a-strategy)
   with the following exception:
    * `strategy` - specifies the worker selection strategy for the given pool, default is `best_worker`,
      more details on this can be found in [Choosing strategy in worker_pool doc](https://github.com/inaka/worker_pool#choosing-a-strategy)
    * `call_timeout` - specifies the timeout for a call operation to the pool
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

## Riak connection setup

Currently only one Riak connection pool can exist for each supported XMPP host.
It is configured with the following tuple inside the `outgoing_pools` config option.

```erlang
{outgoing_pools, [
 {riak, global, default, [{workers, 20}], [{address, "127.0.0.1"}, {port, 8087}]}
]}.
```

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

A connection pool to ElasticSearch can be configured as below:

```erlang
{outgoing_pools, [
 {elastic, global, elasticsearch, [], []}
]}.
```

MongooseIM uses [inaka/tirerl](https://github.com/inaka/tirerl) library to communicate with ElasticSearch.
This library starts pool of worker by its own so:

* number of workers (which is 50)
* `call_timeout` (inifinity)
* worker selection strategy (`available_worker` or what's set as `default_strategy` of `worker_pool` application)

are not possible to change via `WPoolOpts`.

Only `ConnectionOpts` can be set and in them the following options can be added (as `{key, value}` pairs):

* `host` (default: `"localhost"`) - hostname or IP address of ElasticSearch node
* `port` (default: `9200`) - port the ElasticSearch node's HTTP API is listening on

You can verify that MongooseIM has established the connection by running the following function in the MongooseIM shell:

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

