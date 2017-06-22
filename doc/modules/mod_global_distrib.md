### Module Description
This module enables global distribution of a single XMPP domain, i.e. multiple federated servers can share a single domain name and route messages using federation capabilities.

### Notes

* You should only start `mod_global_distrib` by configuring it under `modules` option in `ejabberd.cfg`. Do not add it as host-specific module via `host_config`.
* Do not use `mod_offline` on domains given via `global_host` or `local_host` options, as it will decrease messaging robustness.
* XMPP domains given in `global_host` and `local_host` options have to also be set in top-level `host` option of `ejabberd.cfg`.

### Options

* **global_host** (string, required): The XMPP domain that will be shared between datacenters. *Note:* this needs to be one of domains given in `host` option in `ejabberd.cfg`.
* **local_host** (string, required): XMPP domain that maps uniquely to the local data center; it will be used for inter-center routing. *Note:* this needs to be one of domains given in `host` option in `ejabberd.cfg`.
* **num_of_workers** (integer, default: `1000`): Number of workers that will perform local routing operations on messages rerouted from other XMPP servers sharing the global domain.
* **message_ttl** (integer, default: `4`): Number of times a message can be rerouted between datacenters.
* **connections** (list, default: `[]`): Options for connections maintained by the module. See *Connections' options* section.
* **cache** (list, default: `[]`): Options for caching database lookups. See *Database cache options* section.
* **bounce** (list | `false`, default: `[]`): Options for message bouncing. If `false`, message bouncing is disabled. See *Message bouncing options* section.
* **redis** (list, default: `[]`): Options for Redis session storage backend.

#### Connections' options

* **endpoints** (list, required): A list of `{IP, Port}` tuples on which the server will listen for connections. The endpoint list will be shared with other data centers via the replicated backend.
* **num_of_connections** (integer, default: `1`): Number of outgoing connections that will be established to each other XMPP cluster sharing the global domain.
* **tls_opts** (list, required): Options for TLS connections passed to the `fast_tls` driver. As a minimum they should include `certfile` and `cafile` settings. 

#### Redis session storage options

* **server** (string, default: `"127.0.0.1"`): Address of the Redis listener.
* **port** (integer, default: `8102`): Port of the Redis listener.
* **password** (string, default: `""`): Password to the Redis instance.
* **pool_size** (integer, default: `1`): Number of persistent connections to the Redis instance.
* **expire_after** (integer, default: `120`): Number of seconds after which a session entry written by this cluster will expire.
* **refresh_after** (integer, default: `60`): Number of seconds after which session's expiration timer will be refreshed.

#### Database cache options

* **cache_missed** (boolean, default: `true`): Determines whether an internal session cache should cache lookup failures. When `false`, only successful database lookups will result in the value being cached. This option has grat impact on performance.
* **domain_lifetime_seconds** (integer, default: `600`): How long should subdomain mappings be cached (e.g. `muc.example.com -> datacenter1.test`).
* **jid_lifetime_seconds** (integer, default: `5`): How long should full and bare JIDmappings be cached (e.g. `user1@example.com/res1 -> datacenter1.test`).
* **max_jids** (integer, default: `10000`): The maximum number of JID entries that can be stored in cache at any point in time.

#### Message bouncing options

* **resend_after_ms** (integer, default: `200`): Time after which message will be resent in case of delivery error.
* **max_retries** (integer, default: `4`): Number of times message delivery will be retried in case of errors.

### Example configuration

```Erlang
{mod_global_distrib, [
        {global_host, "example.com"},
        {local_host, "datacenter1.example.com"},
        {connections [
              {endpoints, [{172.16.0.2, 5555}]},
              {num_of_connections, 22}
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
              {password, "secret"}
             ]}
       ]}.
```
