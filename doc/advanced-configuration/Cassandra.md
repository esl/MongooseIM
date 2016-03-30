## Advanced Cassandra configuration

### Default cassandra configuration

```erlang
{cassandra_servers, [{default, []}]}.
```

MongooseIM will create one pool with one worker to connect to localhost:9042.


### Options

* servers - a list of servers to connect. Each server is `{Address, Port, NumberOfWorkers}` where
    * `Address` is DNS-name or IP address;
    * `Port` is an integer TCP port;
    * `NumberOfWorkers` is number of connections to this server.
* keyspace - keyspace to use;
* connect_timeout - connection timeout, 5 seconds is good enough;
* credentials - name and password. Format `[{"username", "mongooseim"}, {"password", "secret"}]`.

### Alternative configuration example 1

Configuration example below includes:

* 5 connections to each server with addresses from 10.0.0.1 to 10.0.0.4;
* Keyspace "big_mongoose";
* Custom connect timeout in milliseconds;
* Custom credentials.

```erlang
{cassandra_servers,
 [
  {default,
   [
    {servers,
     [
      {"10.0.0.1", 9042, 5},
      {"10.0.0.2", 9042, 5},
      {"10.0.0.3", 9042, 5},
      {"10.0.0.4", 9042, 5}
     ]
    },
    {keyspace, "big_mongoose"},
    {connect_timeout, 5000}, % five seconds
    {credentials, [{"username", "mongooseim"}, {"password", "secret"}]}
   ]
  }
 ]
}.
```

### Alternative configuration example 2

Two pools: for mam and for mam_muc

* Each pool uses different keyspace
* Module mod_mam_cassandra_arch uses pool "mam" and keyspace "mam_space"
* Module mod_mam_muc_cassandra_arch uses pool "mam_muc" and keyspace "mam_muc_space"

```erlang
{cassandra_servers, [{mam, [{keyspace, "mam_space"}]}, {mam_muc, [{keyspace, "mam_muc_space"}]}]}.
```

Set pool_name for modules:

```erlang
{modules,
   [{mod_mam_cassandra_arch, [{pool_name, mam}]},
    {mod_mam_muc_cassandra_arch, [{pool_name, mam_muc}]}]}.
```

