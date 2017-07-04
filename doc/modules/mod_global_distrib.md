### Module Description
This module enables global distribution of a single XMPP domain.
With `mod_global_distrib`, multiple distinct MongooseIM clusters can share a single domain name and route messages to the specific datacenter where the recipient is available.

### How it works

There are multiple subsystems that cooperate to enable global distribution:

#### Metadata sharing

Sharing of metadata is done by leveraging a database with cross-datacenter replication.
Currently, only Redis is supported, with [Dynomite](https://github.com/Netflix/dynomite) layer for replication.
The most important metadata stored in the database is a **session/routing table**.
The table stores mappings between currently logged users' JIDs and datacenters on which they are logged in.
Because access to the session table is very frequent, its entries are additionally cached on each node.

To preserve consistency between database instances, all data is stored with a set expiration time and is periodically refreshed.
Each node of each cluster is responsible for refreshing its own data.
Thus, in an event of a netsplit datacenters will have their information about other datacenter's users expire, as those users are now unreachable; but once the connection is reestablished, the data will be replicated again as datacenters refresh their entries.
Additionally, to prevent edge cases where an incoming message is received and replied to before the datacenter learns about the sender's host, an incoming message also carries information about its origin which may be used to temporarily update the local routing table.

#### Message routing

`mod_global_distrib` establishes its own listeners and dedicated TCP/TLS connections for message routing.
Each node listens on preconfigured endpoints, where each node in a datacenter can have any number of endpoints, including none.
The endpoints are shared between all datacenters.
If a node becomes unavailable, its endpoint entries in the database will expire and will be readded once the node comes back online.

Connections between nodes in distinct datacenters are opened on the first request and then maintained indefinitely.
Each connection to a remote datacenter C connects to a randomly selected endpoint of C's nodes; the endpoint will be sampled again in the event of reconnection.
Whenever a node receives a message that is determined (by consulting the session table) to be destined for another datacenter, the routing procedure in the current datacenter is interrupted, the message is transported to the other datacenter via the dedicated connections, and the routing procedure is restarted there by a dedicated (but potentially short lived) worker process bound to the sender's JID (or subdomain if the sender's JIDs does not belong to the globally distributed domain).
Client's process binds itself to a connection to a remote datacenter on first use, and henceforth always uses this connection to route messages directed to this datacenter.
This - along with the dedicated worker process on the receiver's side - ensures that simple cross-datacenter messages between two entities are delivered in their sending order.

It may happen that a message is rerouted through multiple datacenters (e.g. if the user has reconnected to a different datacenter while the message was already in flight).
Messages are given a TTL parameter by the source datacenter so that they cannot be rerouted indefinitely.
The TTL is decreased on each reroute.
Note that in the edge case of multi-datacenter routing, the messages may be received out-of-order at the destination datacenter.

#### Bounce

Consider the following edge case: user **U1** logged into datacenter **DC2** quickly reconnects to datacenter **DC3**.
Because session table has not yet been replicated, **DC2** does not see **U1** in the session table, while a different datacenter **DC1** still sees **U1** logged into **DC2**.
When **U2**, logged into **DC1**, sends a message to **U1**, it will now be rerouted to **DC2** even though the user is now available at **DC3**.

![State after U1's reconnection](mod_global_distrib_bounce_example.svg)

Bounce mechanism solves this and similar edge cases by storing messages for which there is no known routing in the current datacenter.
The stored messages are then assigned a bounce-TTL value and periodically - with backoff - are attempted to be routed again.
In the example above, the message from **U2** would be temporarily stored at **DC2** and rerouted successfully once **DC2** learns (via replication) that **U1** is available at **DC3**.

> Note: bounce mechanism, similarly to multi-datacenter routing, may result in out-of-order messages being received at the destination datacenter.

#### Metrics

Global distribution modules expose several per-datacenter metrics that can be used to monitor health of the system. All metrics begin with **global.mod_global_distrib** prefix:

* **outgoing.messages**: number of cross-datacenter messages sent by this cluster.
* **incoming.messages**: number of cross-datacenter messages received by this cluster.
* **outgoing.queue_time** *[us]*: time elapsed while message waits in sending connection's queue.
* **incoming.queue_time** *[us]*: time elapsed while message waits in routing worker's queue.
* **incoming.transfer_time** *[us]*: time elapsed between sending and receiving the message over the network.
  The duration is calculated using wall clock times on sender and receiver node.
* **mapping_fetch_time** *[us]*: time spent on fetching an entry from the session table, cached or otherwise.
* **mapping_fetches**: number of fetches of session table entries, cached or otherwise.
* **mapping_cache_misses**: number of fetches of session table entries that hit the database.

### Notes

* You should only start `mod_global_distrib` by configuring it under `modules` option in `ejabberd.cfg`. Do not add it as host-specific module via `host_config`.
* Do not use `mod_offline` on domains given via `global_host` or `local_host` options, as it will decrease messaging robustness; the users logged in other datacenters will not be registered as available by `mod_offline`, and so the messages will not be flushed.

### Options

* **global_host** (string, required): The XMPP domain that will be shared between datacenters.
  *Note:* this needs to be one of the domains given in `host` option in `ejabberd.cfg`.
* **local_host** (string, required): XMPP domain that maps uniquely to the local datacenter; it will be used for inter-center routing.
  *Note:* this needs to be one of the domains given in `host` option in `ejabberd.cfg`.
* **message_ttl** (integer, default: `4`): Number of times a message can be rerouted between datacenters.
* **connections** (list, default: `[]`): Options for connections maintained by the module; see *Connections' options* section.
* **cache** (list, default: `[]`): Options for caching database lookups; see *Database cache options* section.
* **bounce** (list | `false`, default: `[]`): Options for message bouncing; if `false`, message bouncing is disabled.
  See *Message bouncing options* section.
* **redis** (list, default: `[]`): Options for Redis session storage backend.

#### Connections' options

* **endpoints** (list, default: `[{LocalHost, 5555}]`): A list of `{Host, Port}` tuples on which the server will listen for connections.
  `Host` can be given as a hostname, in which case it will be resolved to an IP address before first on module start.
  The endpoint list will be shared with other datacenters via the replicated backend.
* **num_of_connections** (integer, default: `1`): Number of outgoing connections that will be established from the current node to each other XMPP cluster sharing the global domain.
* **tls_opts** (list, required): Options for TLS connections passed to the `fast_tls` driver.
  May be set to `false`, in which case all data will be sent via standard TCP connections.
  Otherwise, they should at least include `certfile` and `cafile` options. 

#### Redis session storage options

* **server** (string, default: `"127.0.0.1"`): Address of the Redis listener.
* **port** (integer, default: `8102`): Port of the Redis listener.
* **password** (string, default: `""`): Password to the Redis instance.
* **pool_size** (integer, default: `1`): Number of persistent connections to the Redis instance.
* **expire_after** (integer, default: `120`): Number of seconds after which a session entry written by this cluster will expire.
* **refresh_after** (integer, default: `60`): Number of seconds after which session's expiration timer will be refreshed.

#### Database cache options

* **cache_missed** (boolean, default: `true`): Determines whether an internal session cache should cache lookup failures.
  When `false`, only successful database lookups will result in the value being cached.
  Changing this option has great negative impact on performance.
* **domain_lifetime_seconds** (integer, default: `600`): How long should subdomain mappings be cached (e.g. `muc.example.com -> datacenter1.test`).
* **jid_lifetime_seconds** (integer, default: `5`): How long should full and bare JID mappings be cached (e.g. `user1@example.com/res1 -> datacenter1.test`).
* **max_jids** (integer, default: `10000`): The maximum number of JID entries that can be stored in cache at any point in time.

#### Message bouncing options

* **resend_after_ms** (integer, default: `200`): Time after which message will be resent in case of delivery error.
* **max_retries** (integer, default: `4`): Number of times message delivery will be retried in case of errors.

### Overriding remote datacenter endpoints

There may be cases when the endpoint list given via **endpoints** option does not accurately specify endpoints on which the node may be reached from other datacenters; e.g. in case the node is behind NAT, or in testing environment.
The endpoints used for connection to a remote datacenter may be overridden by global option `{ {global_distrib_addr, Host}, [{IP, Port}] }`.

### Example configuration

#### Configuring mod_global_distrib

```Erlang
{mod_global_distrib, [
        {global_host, "example.com"},
        {local_host, "datacenter1.example.com"},
        {connections [
              {endpoints, [{"172.16.0.2", 5555}]},
              {num_of_connections, 22},
              {tls_opts, [
                    {certfile, "/home/user/dc1.pem"},
                    {cafile, "/home/user/ca.pem"}
                   ]}
             ]},
        {cache, [
              {domain_lifetime_seconds, 60}
             ]},
        {bounce, [
              {resend_after_ms, 300},
              {max_retries, 3}
             ]},
        {redis, [
              {pool_size, 24},
              {server, "172.16.0.3"}
             ]}
       ]}
```

#### Overriding endpoints to a remote datacenter

``` Erlang
{ {global_distrib_addr, "datacenter2.example.com"}, [{"124.12.4.3", 5556}, {"182.172.23.55", 5555}] }.
```

#### Configuring Dynomite

For more information about Dynomite configuration, consult [Dynomite wiki](https://github.com/Netflix/dynomite/wiki).

``` yaml
dyn_o_mite:
  datacenter: dc1
  rack: rack1
  dyn_listen: 172.16.0.3:8101
  dyn_seeds:
  - 124.12.4.4:8101:rack1:dc2:1383429731
  listen: 172.16.0.3:8102
  servers:
  - 172.16.0.4:6379:1
  tokens: '138342973'
  secure_server_option: datacenter
  pem_key_file: dynomite.pem
  data_store: 0
  stats_listen: 0.0.0.0:22221
```

``` yaml
dyn_o_mite:
  datacenter: dc2
  rack: rack1
  dyn_listen: 124.12.4.4:8101
  dyn_seeds:
  - 172.16.0.3:8101:rack1:dc1:1383429731
  listen: 124.12.4.4:8102
  servers:
  - 124.12.4.5:6379:1
  tokens: '138342973'
  secure_server_option: datacenter
  pem_key_file: dynomite.pem
  data_store: 0
  stats_listen: 0.0.0.0:22221
```
