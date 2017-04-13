### Module Description
This module enables global distribution of a single XMPP domain, i.e. multiple federated servers can share a single domain name and route messages using federation capabilities.

The module depends on Cassandra's cross-datacenter replication feature. Please refer to [Cassandra documentation](https://docs.datastax.com/en/cassandra/3.0/cassandra/initialize/initMultipleDS.html) on details how to configure the Cassandra database. The database instances should be configured to work as a part of a single cluster, and each data center should be recognized by Cassandra as a separate datacenter. The database needs to be initialized with a correct schema; please consult `cassandra.cql` included in MongooseIM distribution for example schema configuration (you will probably need to tweak the `NetworkTopologyStrategy` configuration).

### Notes

* You should only start `mod_global_distrib` by configuring it under `modules` option in `ejabberd.cfg`. Do not add it as host-specific module via `host_config`.
* Do not use `mod_offline` on domains given via `global_host` or `local_host` options, as it will decrease messaging robustness.
* XMPP domains given in `global_host` and `local_host` options have to also be set in top-level `host` option of `ejabberd.cfg`.
* If your `local_host` domains are subhosts of the `global_host` (eg. `global_host`: `"example.com"`, peer's `local_host`: `"us.example.com"`), you have to enable `{route_subdomains, s2s}` option in `ejabberd.cfg` so that the messages to the peer can be routed out of the local server.

### Options

* **global_host** (string, default: unset): The XMPP domain that will be shared between datacenters. *Note:* this needs to be one of domains given in `host` option in `ejabberd.cfg`.
* **local_host** (string, default: unset): XMPP domain that maps uniquely to the local data center; it will be used for inter-center routing. *Note:* this needs to be one of domains given in `host` option in `ejabberd.cfg`.
* **cookie** (string, default: unset): A secret shared between datacenters. The cookie has to be set to the same value on each datacenter sharing the `global_host`.
* **database_pool** (atom, default: `global`): Name of the database pool to use for storing session mappings. Refer to example `ejabberd.config` included in MongooseIM distribution for example Cassandra configuration.
* **cache_missed** (boolean, default: `false`): Determines whether an internal session cache should cache lookup failures. When `false`, only successful database lookups will result in the value being cached.
* **domain_lifetime_seconds** (integer, default: `600`): How long should subdomain mappings be cached (e.g. `muc.example.com -> datacenter1.test`).
* **jid_lifetime_seconds** (integer, default: `5`): How long should full and bare JIDmappings be cached (e.g. `user1@example.com/res1 -> datacenter1.test`).
* **max_jids** (integer, default: `10000`): The maximum number of JID entries that can be stored in cache at any point in time.
* **bounce** (list | `false`, default: `[]`): Options for message bouncing. If `false`, message bouncing is disabled.

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
