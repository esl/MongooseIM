## Advanced Cassandra configuration

### Default cassandra configuration

```erlang
{cassandra_server, []}.
```

MongooseIM will create one pool with one worker to connect to localhost:9042.


### Options

* address - host name. It's a string of IP addresses or DNS names separated
  with pipe sign. Examples:
  * "localhost"
  * "127.0.0.1"
  * "127.0.0.1|127.0.0.2"
  * "127.0.0.1:9042|127.0.0.1:9043"
  * "cassandra.example.com"
* port - default port;
* pool_size - number of workers;
* keyspace - keyspace to use;
* connect_timeout - connection timeout, 5 seconds is good enough;
* username - cassandra username;
* password - cassandra password.

### Alternative configuration example 1

Configuration example below includes:

* 5 connections to each server with addresses from 10.0.0.1 to 10.0.0.4;
* Keyspace "big_mongoose";
* Custom connect timeout in milliseconds;
* Custom credentials.

```erlang
{cassandra_server, [
    {address, "10.0.0.1|10.0.0.2|10.0.0.3|10.0.0.4"},
    {keyspace, "big_mongoose"},
    {connect_timeout, 5000}, % five seconds
    {username, "mongooseim"},
    {password, "secret"}
   ]}.
```

### Alternative configuration example 2

Two pools: for mam and for mam_muc

* Each pool uses different keyspace
* Module mod_mam_cassandra_arch uses pool "mam" and keyspace "mam_space"
* Module mod_mam_muc_cassandra_arch uses pool "mam_muc" and keyspace "mam_muc_space"

```erlang
{cassandra_server, mam, [{keyspace, "mam_space"}]}.
{cassandra_server, mam_muc, [{keyspace, "mam_muc_space"}]}.
```

Set pool_name for modules:

```erlang
{modules,
   [{mod_mam_cassandra_arch, [{pool_name, mam}]},
    {mod_mam_muc_cassandra_arch, [{pool_name, mam_muc}]}]}.
```

