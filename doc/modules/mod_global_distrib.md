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
Thus, in an event of a netsplit, datacenters' information about unreachable datacenters' users will expire, as those users are now unreachable; but once the connection is reestablished, the data will be replicated again as datacenters refresh their entries.
Additionally, to prevent edge cases where an incoming message is received and replied to before the datacenter learns about the sender's host, an incoming message also carries information about its origin which may be used to temporarily update the local routing table.

##### Redis entries

Following structures are stored in Redis:

* JID mappings are stored as normal key-value entries, where user's JID (full and bare) is the key, and the value is the local hostname where the user is logged in.
Example: `"user1@example.com/res" -> "dc2.example.com"`.
* Domains of components and services registered on the globally distributed host are stored in per-node set structures where the key is `<local_host>#<node_name>#{domains}`, and the values are the domain names.
Example: `"dc1.example.com#mongoose1@dc1.example.com#{domains}" -> {"muc1.example.com", "muc2.example.com"}`.
* Domains of non-hidden components and services (see [`ejabberd_service`](../advanced-configuration/Listener-modules.md#xmpp-components-ejabberd_service) documentation) are stored in per-node set structures where the key is `<local_host>#<node_name>#{public_domains}`, and the values are the domain names.
* Declared endpoints available on a node are similarly stored in a per-node set structure where the key is `<local_host>#<node_name>#{endpoints}` and the values represent the TCP endpoints of the node.
Example: `"dc1.example.com#mongoose1@dc1.example.com#{endpoints}" -> {"172.16.2.14#8231", "2001:0db8:85a3:0000:0000:8a2e:0370:7334#8882"}`.
* Nodes that comprise a host are stored in a set structure with key `<local_host>#{nodes}` and values being the names of the nodes.
Example: `"dc2.example.com#{nodes}" -> {"node1@dc2.example.com", "node3@dc2.example.com"}`.
* Hosts are stored in a set with key `hosts` and values being the individual local XMPP domains.
Example: `"hosts" -> {"dc1.example.com", "dc2.example.com"}`.

#### Message routing

`mod_global_distrib` establishes its own listeners and dedicated TCP/TLS connections for message routing.
Each node listens on preconfigured endpoints, where each node in a datacenter can have any number of endpoints, including none.
The endpoints are shared between all datacenters.
If a node becomes unavailable, its endpoint entries in the database will expire and will be readded once the node comes back online.

Connections between nodes in distinct datacenters are opened on the first request and then maintained as long as the destination endpoint is present in Redis.
When a node needs to connect to a remote cluster, specified number of connections are opened to every endpoint reported by that datacenter.
Global distribution features automatic rebalancing feature that will "disable" connections when their respective endpoints disappear from Redis.
A new pool of connections is created each time a new endpoint is recognised.
Whenever a node receives a message that is determined (by consulting the session table) to be destined for another datacenter, the routing procedure in the current datacenter is interrupted, the message is transported to the other datacenter via the dedicated connections, and the routing procedure is restarted there by a dedicated (but potentially short lived) worker process bound to the sender's JID (or subdomain if the sender's JIDs does not belong to the globally distributed domain).
Client's process binds itself to a connection to a remote datacenter on first use, and henceforth always uses this connection to route messages directed to this datacenter.
This - along with the dedicated worker process on the receiver's side - ensures that simple cross-datacenter messages between two entities are delivered in their sending order.

It may happen that a message is rerouted through multiple datacenters (e.g. if the user has reconnected to a different datacenter while the message was already in flight).
Messages are given a TTL parameter by the source datacenter so that they cannot be rerouted indefinitely.
The TTL is decreased on each reroute.
Note that in the edge case of multi-datacenter routing, the messages may be received out-of-order at the destination datacenter.

#### Bounce

Consider the following edge case: user **U1** logged into datacenter **DC2** and then quickly reconnected to datacenter **DC3**.
Because session table has not yet been replicated, **DC2** does not see **U1** in the session table, while a different datacenter **DC1** still sees **U1** logged into **DC2**.
When **U2** logged into **DC1** and sent a message to **U1**, it will now be rerouted to **DC2** even though the user is now available at **DC3**.

![State after U1's reconnection](mod_global_distrib_bounce_example.svg)

Bounce mechanism solves this and similar edge cases by storing messages for which there is no known routing in the current datacenter.
The stored messages are then assigned a bounce-TTL value and periodically - with backoff - are attempted to be routed again.
In the example above, the message from **U2** would be temporarily stored at **DC2** and rerouted successfully once **DC2** learns (via replication) that **U1** is available at **DC3**.

> Note: bounce mechanism, similarly to multi-datacenter routing, may result in out-of-order messages being received at the destination datacenter.

#### Metrics

Global distribution modules expose several per-datacenter metrics that can be used to monitor health of the system. All metrics begin with **global.mod_global_distrib** prefix:

* `outgoing.messages.<host>`: number of cross-datacenter messages sent by this cluster to a given host.
* `incoming.messages.<host>`: number of cross-datacenter messages received by this cluster from a given host.
* `incoming.transfer_time.<host>` *[us]*: time elapsed between sending and receiving the message over the network from a given host.
  The duration is calculated using wall clock times on sender and receiver node.
* `outgoing.queue_time.<host>` *[us]*: time elapsed while message waits in a queue of a sender's connection to a given host.
  High value of this metric may be remedied by increasing the number of connections to other hosts.
* `incoming.queue_time` *[us]*: time elapsed while message waits in routing worker's queue.
  This value is not reported per-host as routing workers are bound to the sender's JID.
* `incoming.established`: incremented when a new connection is established from another cluster.
  At this point the origin domain of the cluster is not known, so this metric is common for all of them.
* `incoming.first_packet.<host>`: incremented when a receiver process gets the first packet from a remote cluster and learns its local domain.
* `incoming.closed.<host>`: incremented when an incoming connection gets closed.
* `incoming.errored.<host>`: incremented when an incoming connection gets closed with an error.
* `outgoing.established.<host>`: incremented when an outgoing connection is established.
* `outgoing.closed.<host>`: incremented when an outgoing connection gets closed.
* `outgoing.errored.<host>`: incremented when an outgoing connection gets closed with an error.
* `mapping_fetch_time` *[us]*: time spent on fetching an entry from the session table, cached or otherwise.
* `mapping_fetches`: number of fetches of session table entries, cached or otherwise.
* `mapping_cache_misses`: number of fetches of session table entries that hit the database.
* `delivered_with_ttl`: A histogram of packets' TTL values recorded when the global routing layer decides to route them locally (but not due to TTL = 0).
* `stop_ttl_zero`: A number of packets that weren't processed by global routing due to TTL=0.
* `bounce_queue_size`: a number of messages enqueued for rerouting (the value of this metric is individual per MongooseIM node!).

### Notes

* You should only start `mod_global_distrib` by configuring it under `modules` option in `mongooseim.cfg`. Do not add it as host-specific module via `host_config`.
* Do not use `mod_offline` on domains given via `global_host` or `local_host` options, as it will decrease messaging robustness; the users logged in other datacenters will not be registered as available by `mod_offline`, and so the messages will not be flushed.

### Options

#### `modules.mod_global_distrib.global_host`
* **Syntax:** string
* **Default:** none, this option is mandatory
* **Example:** `global_host = "example.com"`

The XMPP domain that will be shared between datacenters.
*Note:* this needs to be one of the domains given in `general.hosts` option in `mongooseim.toml`.

#### `modules.mod_global_distrib.local_host`
* **Syntax:** string
* **Default:** none, this option is mandatory
* **Example:** `local_host = "datacenter1.example.com"`

XMPP domain that maps uniquely to the local datacenter; it will be used for inter-center routing.
*Note:* this needs to be one of the domains given in `general.hosts` option in `mongooseim.toml`.

#### `modules.mod_global_distrib.message_ttl`
* **Syntax:** non-negative integer
* **Default:** `4`
* **Example:** `message_ttl = 5`

Number of times a message can be rerouted between datacenters.

#### `modules.mod_global_distrib.bounce`
* **Syntax:** boolean with only `false` being a valid option
* **Default:** not set and `bounce` is enabled.
* **Example:** `bounce = false`

If this option is present and set to false, message bouncing will be disabled. Refer [here](#message-bouncing-options) for more details.

#### `modules.mod_global_distrib.hosts_refresh_interval`
* **Syntax:** non-negative integer, value given in milliseconds
* **Default:** `3000`
* **Example:** `hosts_refresh_interval = 3000`

The interval telling how often Redis should be asked if new hosts appeared.

#### Connections' options

#### `modules.mod_global_distrib.connections.endpoints`
 * **Syntax:** Array of TOML tables with the following keys: `host` and `port`, and the following values: {host = `string`, port = `non_negative_integer`}
 * **Default:** `[{host = "LocalHost", port = 5555}]`
 * **Example:** `endpoints = [{host = "172.16.0.2", port = 5555}]`

A list of endpoints on which the server will listen for connections.
`host` can be given as a hostname, in which case it will be resolved to an IP address on module start.
The endpoint list will be shared with other datacenters via the replicated backend.

#### `modules.mod_global_distrib.connections.advertised_endpoints`
* **Syntax:** Array of TOML tables with the following keys: `host` and `port`, and the following values: {host = `string`, port = `non_negative_integer`} **or** `false`
* **Default:** `false`
* **Example:** `advertised_endpoints = [{host = "172.16.0.2", port = 5555}]`

A list of endpoints which will be advertised in Redis and therefore used to establish connection with this node by other nodes. If not specified, `endpoints` value (after resolution) is considered `advertised_endpoints`. The host may be either IP or domain, just like in case of endpoints. The difference is, the domain name won't be resolved but inserted directly to the mappings backend instead.

#### `modules.mod_global_distrib.connections.connections_per_endpoint`
* **Syntax:** non-negative integer
* **Default:** `1`
* **Example:** `connections_per_endpoint = 30`

Number of outgoing connections that will be established from the current node to each endpoint assigned to a remote domain.

#### `modules.mod_global_distrib.connections.endpoint_refresh_interval`
* **Syntax:** positive integer, value given in seconds
* **Default:** `60`
* **Example:** `endpoint_refresh_interval = 30`

An interval between remote endpoint list refresh (and connection rebalancing).
A separate timer is maintained for every remote domain.

#### `modules.mod_global_distrib.connections.endpoint_refresh_interval_when_empty`
* **Syntax:** positive integer, value given in seconds
* **Default:** `3`
* **Example:** `endpoint_refresh_interval_when_empty = 3`

Endpoint refresh interval, when array of endpoints is empty.

#### `modules.mod_global_distrib.connections.disabled_gc_interval`
* **Syntax:** non-negative integer, value given in seconds
* **Default:** `60`
* **Example:** `disabled_gc_interval = 60`

An interval between disabled endpoints "garbage collection".
It means that disabled endpoints are periodically verified and if Global Distribution detects that connections is no longer alive, the connection pool is closed completely.

#### `modules.mod_global_distrib.connections.tls`
* **Syntax:** boolean with only `false` being a valid option
* **Default:** none, this option is mandatory. Details in the description
* **Example:** `tls = false`

If this option is present, all data will be sent via standard TCP connections.
To enable TLS support, refer to [TLS](#tls-options) options.

#### TLS options
To enable TLS support at least the `cacertfile` and `certfile` options have to be present. These options will be passed to the `fast_tls` driver.

#### `modules.mod_global_distrib.connections.tls.certfile`
* **Syntax:** string, path in the file system
* **Default:** none, this options is mandatory to enable TLS support
* **Example:** `certfile = "priv/dc1.pem"`

#### `modules.mod_global_distrib.connections.tls.cacertfile`
* **Syntax:** string, path in the file system
* **Default:** none, this options is mandatory to enable TLS support
* **Example:** `cacertfile = "priv/ca.pem"`

#### `modules.mod_global_distrib.connections.tls.ciphers`
* **Syntax:** string
* **Default:** not set
* **Example:** `ciphers = "ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384"`

Cipher suites to use with StartTLS or TLS. Please refer to the [OpenSSL documentation](http://www.openssl.org/docs/man1.0.2/apps/ciphers.html) for the cipher string format.

#### `modules.mod_global_distrib.connections.tls.dhfile`
* **Syntax:** string, path in the file system
* **Default:** not set
* **Example:** `dhfile = "dh.pem"`

#### Redis session storage options

#### `modules.mod_global_distrib.redis.pool`
* **Syntax:** string
* **Default:** `"global_distrib"`
* **Example:** `pool = "global_distrib"`

Name of the redis pool defined in [outgoing pools](../advanced-configuration/outgoing-connections.md).

#### `modules.mod_global_distrib.redis.expire_after`
* **Syntax:** non-negative integer
* **Default:** `120`
* **Example:** `expire_after = 120`

Number of seconds after which a session entry written by this cluster will expire.

#### `modules.mod_global_distrib.redis.refresh_after`
* **Syntax:** non-negative integer
* **Default:** `60`
* **Example:** `refresh_after = 60`

Number of seconds after which session's expiration timer will be refreshed.

#### Database cache options
Options for caching database lookups, by default no options are passed.

#### `modules.mod_global_distrib.cache.cache_missed`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `cache_missed = true`

Determines whether an internal session cache should cache lookup failures.
When `false`, only successful database lookups will result in the value being cached.
Changing this option has great negative impact on performance.

#### `modules.mod_global_distrib.cache.domain_lifetime_seconds`
* **Syntax:** non-negative integer, value given in seconds
* **Default:** `600`
* **Example:** `domain_lifetime_seconds = 600`

How long should subdomain mappings be cached (e.g. `muc.example.com -> datacenter1.test`).

#### `modules.mod_global_distrib.cache.jid_lifetime_seconds`
* **Syntax:** non-negative integer, value given in seconds
* **Default:** `5`
* **Example:** `jid_lifetime_seconds = 5`

How long should full and bare JID mappings be cached (e.g. `user1@example.com/res1 -> datacenter1.test`).

#### `modules.mod_global_distrib.cache.max_jids`
* **Syntax:** non-negative integer
* **Default:** `10000`
* **Example:** `max_jids = 10000`

The maximum number of JID entries that can be stored in cache at any point in time.

#### Message bouncing options
Options for message bouncing.

#### `modules.mod_global_distrib.bounce.resend_after_ms`
* **Syntax:** non-negative integer
* **Default:** `200`
* **Example:** `resend_after_ms = 200`

Time after which message will be resent in case of delivery error.

#### `modules.mod_global_distrib.bounce.max_retries`
* **Syntax:** non-negative integer
* **Default:** `4`
* **Example:** `max_retries = 4`

Number of times message delivery will be retried in case of errors.

#### Global Distribution and Service Discovery

`mod_global_distrib` extension relies on [`mod_disco`](mod_disco.md)'s option `users_can_see_hidden_services`, when provided. If it is not configured, the default value is `true`. `mod_disco` does not have to be enabled for `mod_global_distrib` to work, as this parameter is used only for processing Disco requests by Global Distribution.

### Overriding remote datacenter endpoints

There may be cases when the endpoint list given via **endpoints** option does not accurately specify endpoints on which the node may be reached from other datacenters; e.g. in case the node is behind NAT, or in testing environment.
The endpoints used for connection to a remote datacenter may be overridden by global option `{ {global_distrib_addr, Host}, [{IP, Port}] }`.

### Example configuration

#### Configuring mod_global_distrib

```
[modules.mod_global_distrib]
  global_host = "example.com"
  local_host = "datacenter1.example.com"
  connections.endpoints = [{host = "172.16.0.2", port = 5555}]
  connections.advertised_endpoints = [{host = "172.16.0.2", port = 5555}]
  connections.tls.certfile = "priv/dc1.pem"
  connections.tls.cacertfile = "priv/ca.pem"
  connections.connections_per_endpoint = 30
  cache.domain_lifetime_seconds = 60
  bounce.resend_after_ms = 300
  bounce.max_retries = 3
  redis.pool = "global_distrib"
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
