### Module Description
This module enables global distribution of a single XMPP domain, i.e. multiple federated servers can share a single domain name and route messages using federation capabilities.

The module depends on Cassandra's cross-datacenter replication feature. Please refer to [Cassandra documentation](https://docs.datastax.com/en/cassandra/3.0/cassandra/initialize/initMultipleDS.html) on details how to configure the Cassandra database. The database instances should be configured to work as a part of a single cluster, and each data center should be recognized by Cassandra as a separate datacenter. The database needs to be initialized with a correct schema; please consult `cassandra.cql` included in MongooseIM distribution for example schema configuration (you will probably need to tweak the `NetworkTopologyStrategy` configuration).

### Manually specifying other clusters' addresses

By default the messages will be routed between XMPP clusters sharing the global domain using DNS lookups and addressing the messages to the port given via **listen_port** option (default: 5555).

Consider two servers sharing a domain `mim-global.com`, with one of the servers (**A**) having a local domain `mim-a.com` and the other (**B**) local domain `mim-b.com`. There are two users: Alice connected to server **A** and Bob connected to server **B**. When Alice sends a message to Bob, server **A** establishes a TCP connection to `mim-b.com:5555` and sends the message via this connection. Likewise when Bob replies to Alice, **B** will send the reply via its own connection to `mim-a.com:5555`.

The addresses can be overridden per target host via a top-level configuration option `{ {global_distrib_addr, TargetHost}, { TargetAddress, TargetPort} }.`. The option should be put on the top level of ejabberd.cfg. For example: `{ {global_distrib_addr, "mim-a.com"}, { {127,0,0,1}, 5556} }.`

### Notes

* You should only start `mod_global_distrib` by configuring it under `modules` option in `ejabberd.cfg`. Do not add it as host-specific module via `host_config`.
* Do not use `mod_offline` on domains given via `global_host` or `local_host` options, as it will decrease messaging robustness.
* XMPP domains given in `global_host` and `local_host` options have to also be set in top-level `host` option of `ejabberd.cfg`.

### Options

* **global_host** (string, required): The XMPP domain that will be shared between datacenters. *Note:* this needs to be one of domains given in `host` option in `ejabberd.cfg`.
* **local_host** (string, required): XMPP domain that maps uniquely to the local data center; it will be used for inter-center routing. *Note:* this needs to be one of domains given in `host` option in `ejabberd.cfg`.
* **database_pool** (atom, default: `global`): Name of the database pool to use for storing session mappings. Refer to example `ejabberd.config` included in MongooseIM distribution for example Cassandra configuration.
* **num_of_workers** (integer, default: `1000`): Number of workers that will perform local routing operations on messages rerouted from other XMPP servers sharing the global domain.
* **connections** (list, default: `[]`): Options for connections maintained by the module. See *Connections' options* section.
* **cache** (list, default: `[]`): Options for caching database lookups. See *Database cache options* section.
* **bounce** (list | `false`, default: `[]`): Options for message bouncing. If `false`, message bouncing is disabled. See *Message bouncing options* section.

#### Connections' options

* **listen_port** (integer, default: `5555`): Port on which the server will listen to TCP connections from other XMPP clusters sharing the global domain.
* **num_of_connections** (integer, default: `1`): Number of outgoing connections that will be established to other XMPP clusters sharing the global domain.
* **certfile** (string, required): Path to the client certificate that will be used by the TLS connection. The file has to include a private key.
* **cafile** (string, required): Path to a CA certificate that will be used to authenticate connections with other XMPP clusters sharing the global domain.

#### Database cache options

* **cache_missed** (boolean, default: `true`): Determines whether an internal session cache should cache lookup failures. When `false`, only successful database lookups will result in the value being cached. This option has grat impact on performance.
* **domain_lifetime_seconds** (integer, default: `600`): How long should subdomain mappings be cached (e.g. `muc.example.com -> datacenter1.test`).
* **jid_lifetime_seconds** (integer, default: `5`): How long should full and bare JIDmappings be cached (e.g. `user1@example.com/res1 -> datacenter1.test`).
* **max_jids** (integer, default: `10000`): The maximum number of JID entries that can be stored in cache at any point in time.

#### Message bouncing options

* **resend_after_ms** (integer, default: `200`): Time after which message will be resent in case of delivery error.
* **max_retries** (integer, default: `4`): Numbe of times message delivery will be retried in case of errors.

### Example configuration

```Erlang
{cassandra_servers, [
        {global, [
              {servers, [{"localhost", 9042}]},
              {keyspace, "mongooseim_global"}
             ]}
       ]}.
```

```Erlang
{mod_global_distrib, [
        {global_host, "example.com"},
        {local_host, "datacenter1.example.com"},
        {cookie, "secret"},
        {domain_lifetime_seconds, 60},
        {bounce, [
              {resend_after_ms, 300},
              {max_retries, 3}
             ]}
       ]}.
```
