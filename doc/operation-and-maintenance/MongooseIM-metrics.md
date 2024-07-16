# MongooseIM metrics

MongooseIM by default collects many metrics showing the user behaviour and general system statistics.
They are managed by [exometer](https://github.com/Feuerlabs/exometer).
MongooseIM uses [ESL's fork of this project](https://github.com/esl/exometer/tree/1.2.1-patched).

All metrics are divided into the following groups:

* Per host type metrics: Gathered separately for every host type supported by the cluster.

    !!! Warning
        If a cluster supports many (thousands or more) host types, performance issues might occur.
        To avoid this, use global equivalents of the metrics with `all_metrics_are_global` config option.

    * Hook metrics.
    They are created for every [hook](../developers-guide/Hooks-and-handlers.md) and incremented on every call to it.

* Global metrics: Metrics common for all host types.
    * Data metrics.
    These are misc. metrics related to data transfers (e.g. sent and received stanza size statistics).
    * VM metrics. Basic Erlang VM statistics.
* Backend metrics: Histograms with timings of calls to various backends.

## Metrics types

### `spiral`

This kind of metric provides 2 values: `total` event count (e.g. stanzas processed) and a value in 60s window (`one` value).
Dividing `one` value by 60 provides an average per-second value over last minute.

**Example:** `[{total, 1000}, {one, 20}]`

### `value`

A simple value.
It is actually a one-element proplist: `[{value, N}]`.

**Example:** `[{value, 256}]`

### `gauge`

It is similar to a `value` type but consists of two properties:

* `value`
* `ms_since_reset` - Time in milliseconds elapsed from the last metric update.

**Example:** `[{value, 12}, {ms_since_reset, 91761}]`

### `proplist`

A metric which is a nonstandard proplist.
You can find the lists of keys in metrics descriptions.

**Example:** `[{total,295941736}, {processes_used,263766824}, {atom_used,640435}, {binary,1513152}, {ets,3942592}, {system,32182072}]`

### `histogram`

A histogram collects values over a sliding window of 60s and exposes the following stats:

* `n` - A number of samples.
* `mean` - An arithmetic mean.
* `min`
* `max`
* `median`
* `50`, `75`, `90`, `95`, `99`, `999` - 50th, 75th, 90th, 95th, 99th and 99.9th percentile

## Per host type metrics

### Hook metrics

There are more hook metrics than what is listed in this table, because they are automatically created for every new hook.
As a result it makes more sense to maintain a list of the most relevant or useful items, rather than keeping this table fully in sync with the code.

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[HostType, anonymous_purge]` | spiral | An anonymous user disconnects. |
| `[HostType, disco_info]` | spiral | An information about the server has been requested via Disco protocol. |
| `[HostType, disco_local_features]` | spiral | A list of server features is gathered. |
| `[HostType, disco_local_identity]` | spiral | A list of server identities is gathered. |
| `[HostType, disco_local_items]` | spiral | A list of server's items (e.g. services) is gathered. |
| `[HostType, disco_sm_features]` | spiral | A list of user's features is gathered. |
| `[HostType, disco_sm_identity]` | spiral | A list of user's identities is gathered. |
| `[HostType, disco_sm_items]` | spiral | A list of user's items is gathered. |
| `[HostType, mam_lookup_messages]` | spiral | An archive lookup is performed. |
| `[HostType, offline_message]` | spiral | A message was sent to an offline user. (Except for "error", "headline" and "groupchat" message types.) |
| `[HostType, offline_groupchat_message]` | spiral | A groupchat message was sent to an offline user. |
| `[HostType, privacy_updated_list]` | spiral | User's privacy list is updated. |
| `[HostType, resend_offline_messages]` | spiral | A list of offline messages is gathered for delivery to a user's new connection. |
| `[HostType, roster_get_subscription_lists]` |  spiral | Presence subscription lists (based on which presence updates are broadcasted) are gathered. |
| `[HostType, roster_in_subscription]` | spiral | A presence with subscription update is processed. |
| `[HostType, roster_out_subscription]` | spiral | A presence with subscription update is received from a client. |
| `[HostType, sm_broadcast]` | spiral | A stanza is broadcasted to all of user's resources. |
| `[HostType, unset_presence]` | spiral | A user disconnects or sends an `unavailable` presence. |

### Presences & rosters

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[HostType, sm_presence_subscription, subscription_count]` | spiral | Presence subscription is processed. |
| `[HostType, sm_presence_subscription, unsubscription_count]` | spiral | Presence unsubscription is processed. |
| `[HostType, mod_roster_get, count]` | spiral | User's roster is fetched. |
| `[HostType, mod_roster_push, count]` | spiral | A roster update is pushed to a single session. |
| `[HostType, mod_roster_set, count]` | spiral | User's roster is updated. |

### Privacy lists

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[HostType, mod_privacy_get, count]` | spiral | IQ privacy `get` is processed. |
| `[HostType, mod_privacy_push_item, count]` | spiral | Privacy list update is sent to a single session. |
| `[HostType, mod_privacy_set, count]` | spiral | IQ privacy `set` is processed. |
| `[HostType, mod_privacy_set, active_count]` | spiral | Active privacy list is changed. |
| `[HostType, mod_privacy_set, default_count]` | spiral | Default privacy list is changed. |
| `[HostType, mod_privacy_check_packet, count]` | spiral | A packet is checked against the privacy list. |
| `[HostType, mod_privacy_check_packet, denied_count]` | spiral | Privacy list check resulted in `deny`. |
| `[HostType, mod_privacy_check_packet, blocked_count]` | spiral | Privacy list check resulted in `block`. |

### Other

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[HostType, c2s_auth_failed, count]` | spiral | A client failed to authenticate. |
| `[HostType, sm_session, count]` | counter | Number of active sessions. |
| `[HostType, sm_session, logouts]` | spiral | A client session is closed. |
| `[HostType, sm_session, logins]` | spiral | A client session is opened. |
| `[HostType, c2s_element_in, count]` | spiral | An XML element is received from a client. |
| `[HostType, c2s_element_in, stanza_count]` | spiral | An XMPP stanza is received from a client. |
| `[HostType, c2s_element_in, message_count]` | spiral | A message stanza is received from a client. |
| `[HostType, c2s_element_in, iq_count]` | spiral | An IQ stanza is received from a client. |
| `[HostType, c2s_element_in, presence_count]` | spiral | A presence stanza is received from a client. |
| `[HostType, c2s_element_in, error_count]` | spiral | An error is received from a client. |
| `[HostType, c2s_element_in, message_error_count]` | spiral | A message error is received from a client. |
| `[HostType, c2s_element_in, iq_error_count]` | spiral | An IQ error is received from a client. |
| `[HostType, c2s_element_in, presence_error_count]` | spiral | A presence error is received from a client. |
| `[HostType, c2s_element_out, count]` | spiral | An XML element is sent to a client. |
| `[HostType, c2s_element_out, stanza_count]` | spiral | An XMPP stanza is sent to a client. |
| `[HostType, c2s_element_out, iq_count]` | spiral | An IQ stanza is sent to a client. |
| `[HostType, c2s_element_out, message_count]` | spiral | A message stanza is sent to a client. |
| `[HostType, c2s_element_out, presence_count]` | spiral | A presence stanza is sent to a client. |
| `[HostType, c2s_element_out, error_count]` | spiral | An error is sent to a client. |
| `[HostType, c2s_element_out, iq_error_count]` | spiral | An IQ error is sent to a client. |
| `[HostType, c2s_element_out, message_error_count]` | spiral | A message error is sent to a client. |
| `[HostType, c2s_element_out, presence_error_count]` | spiral | A presence error is sent to a client. |
| `[HostType, c2s_message_processing_time`] | histogram | Processing time for incomming c2s stanzas. |
| `[HostType, sm_message_bounced, count]` | spiral | A `service-unavailable` error is sent, because the message recipient is offline. |
| `[HostType, router_stanza_dropped, count]` | spiral | A stanza is dropped due to an AMP rule or a `filter_local_packet` processing flow. |

### Pool metrics

For every RDBMS pool defined, an instance of these metrics are available.

| Name                                                         | Type    | Description (when it gets incremented) |
|--------------------------------------------------------------|---------|----------------------------------------|
| `[HostType, wpool_rdbms_stats, PoolTag, workers]`   | counter | Number of workers in the pool          |
| `[HostType, wpool_rdbms_stats, PoolTag, recv_oct]`  | spiral  | Number of bytes received               |
| `[HostType, wpool_rdbms_stats, PoolTag, recv_cnt]`  | spiral  | Number of packets received             |
| `[HostType, wpool_rdbms_stats, PoolTag, recv_max]`  | gauge   | Size of the largest packet, in bytes   |
| `[HostType, wpool_rdbms_stats, PoolTag, send_oct]`  | spiral  | Number of bytes sent                   |
| `[HostType, wpool_rdbms_stats, PoolTag, send_max]`  | gauge   | Size of the largest packet             |
| `[HostType, wpool_rdbms_stats, PoolTag, send_cnt]`  | spiral  | Number of packets sent                 |
| `[HostType, wpool_rdbms_stats, PoolTag, send_pend]` | spiral  | Number of bytes waiting to be sent     |

When using a Rabbit worker pool, metrics defined in [mod_event_pusher_rabbit](../modules/mod_event_pusher_rabbit.md) are
available.

### Extension-specific metrics

Metrics specific to an extension, e.g. Message Archive Management, are described in respective module documentation pages.

## Global metrics

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[global, routingErrors]` | spiral | It is not possible to route a stanza (all routing handlers failed). |
| `[global, sm_node_sessions, count]` | gauge | A number of sessions connected to a given MongooseIM node. |
| `[global, sm_total_sessions, count]` | gauge | A number of sessions connected to a MongooseIM cluster. |
| `[global, sm_unique_sessions, count]` | gauge | A number of unique users connected to a MongooseIM cluster (e.g. 3 sessions of the same user will be counted as 1 in this metric). |
| `[global, system_up_time, seconds]` | value | Node uptime. |
| `[global, mnesia_info, running_db_nodes]` | value | A number of nodes in a MongooseIM cluster seen by a given MongooseIM node (based on Mnesia). For CETS, use `[global, cets_info, joined_nodes]` instead. |
| `[global, system_tcp_ports, count]` | value | A number of open tcp connections. This should relate to the number of connected sessions and databases, as well as federations and http requests. A constantly growing value might indicate a connection leak. |
| `[global, system_process_queue_lengths, total]` | probe | The total number of incoming messages queued in the Erlang processes. It is a good indicator of an overloaded system: if too many messages are queued at the same time, the system is most likely overloaded with incoming data. |

### Data metrics

| Metric name | Type | Description |
| ----------- | ---- | ----------- |
| `[global, xmpp_stanza_size_received, byte_size]` | histogram | A size (in bytes) of a received stanza after decryption. |
| `[global, xmpp_stanza_size_sent, byte_size]` | histogram | A size (in bytes) of a sent stanza before encryption. |
| `[global, c2s_tcp_data_received, byte_size]` | spiral | A size (in bytes) of unencrypted data received from a client via TCP channel. |
| `[global, c2s_tcp_data_sent, byte_size]` | spiral | A size (in bytes) of unencrypted data sent to a client via TCP channel. |
| `[global, c2s_tls_data_received, byte_size]` | spiral | A size (in bytes) of a data received from a client via TLS channel after decryption. |
| `[global, c2s_tls_data_sent, byte_size]` | spiral | A size (in bytes) of a data sent to a client via TLS channel before encryption. |
| `[global, mod_bosh_data_received, byte_size]` | spiral | A size (in bytes) of a data received from a client via BOSH connection. |
| `[global, mod_bosh_data_sent, byte_size]` | spiral | A size (in bytes) of a data sent to a client via BOSH connection. |
| `[global, mod_websocket_data_received, byte_size]` | spiral | A size (in bytes) of a data received from a client via WebSocket connection. |
| `[global, mod_websocket_data_sent, byte_size]` | spiral | A size (in bytes) of a data sent to a client via WebSocket connection. |
| `[global, data, xmpp, received, s2s]` | spiral | A size (in bytes) of a data received via TCP and TLS (after decryption) Server-to-Server connections. |
| `[global, data, xmpp, sent, s2s]` | spiral | A size (in bytes) of a data sent via TCP and TLS (before encryption) Server-to-Server connections. |
| `[global, data, xmpp, received, component]` | spiral | A size (in bytes) of a data received from XMPP component. |
| `[global, data, xmpp, sent, component]` | spiral | A size (in bytes) of a data sent to XMPP component. |
| `[global, system_dist_data, Metric]` | gauge | Network stats for Erlang distributed communication. `Metric` can be `recv_oct`, `recv_cnt`, `recv_max`, `send_oct`, `send_max`, `send_cnt`, `send_pend` or `connections`. |

### CETS system metrics

| Metric name | Type | Description |
| ----------- | ---- | ----------- |
| `[global, cets, system]` | proplist | A proplist with a list of stats. Description is below. |

| Stat Name | Description |
| ----------- | ----------- |
| `available_nodes` | Available nodes (nodes that are connected to us and have the CETS disco process started). |
| `unavailable_nodes` | Unavailable nodes (nodes that do not respond to our pings). |
| `joined_nodes` | Joined nodes (nodes that have our local tables running). |
| `discovered_nodes` | Discovered nodes (nodes that are extracted from the discovery backend). |
| `remote_nodes_without_disco` | Nodes that have more tables registered than the local node. |
| `remote_nodes_with_unknown_tables` | Nodes with unknown tables. |
| `remote_unknown_tables` | Unknown remote tables. |
| `remote_nodes_with_missing_tables` | Nodes that are available, but do not host some of our local tables. |
| `remote_missing_tables` | Nodes that replicate at least one of our local tables to a different list of nodes. |
| `conflict_nodes` | Nodes that replicate at least one of our local tables to a different list of nodes. |
| `conflict_tables` | Tables that have conflicting replication destinations. |
| `discovery_works` | Returns 1 if the last discovery attempt is successful (otherwise returns 0). |

### Pool metrics

For RDBMS global pool defined, an instance of these metrics are available.

| Name                                                              | Type    | Description (when it gets incremented) |
|-------------------------------------------------------------------|---------|----------------------------------------|
| `[global, wpool_global_rdbms_stats, PoolTag, workers]`   | counter | Number of workers in the pool          |
| `[global, wpool_global_rdbms_stats, PoolTag, recv_oct]`  | spiral  | Number of bytes received               |
| `[global, wpool_global_rdbms_stats, PoolTag, recv_cnt]`  | spiral  | Number of packets received             |
| `[global, wpool_global_rdbms_stats, PoolTag, recv_max]`  | gauge   | Size of the largest packet, in bytes   |
| `[global, wpool_global_rdbms_stats, PoolTag, send_oct]`  | spiral  | Number of bytes sent                   |
| `[global, wpool_global_rdbms_stats, PoolTag, send_max]`  | gauge   | Size of the largest packet             |
| `[global, wpool_global_rdbms_stats, PoolTag, send_cnt]`  | spiral  | Number of packets sent                 |
| `[global, wpool_global_rdbms_stats, PoolTag, send_pend]` | spiral  | Number of bytes waiting to be sent     |


### VM metrics

| Metric name | Type | Description |
| ----------- | ---- | ----------- |
| `[global, system_memory, Metric]` | gauge | Erlang memory statistics from [`erlang:memory/0`](https://www.erlang.org/doc/apps/erts/erlang.html#memory/0). `Metric` specifies the memory type, e.g. `total`, `processes_used`, `atom_used`, `binary`, `ets` or `system`. |
| `[global, system_info, Metric]` | gauge | Erlang system statistics from [`erlang:system_info/1`](https://www.erlang.org/doc/apps/erts/erlang.html#system_info/1). `Metric` can be `port_count`, `port_limit`, `process_count`, `process_limit`, `ets_count` or `ets_limit`. |

## Backend metrics

Some extension modules expose histograms with timings of calls made to their backends.
Please check the documentation of modules that are enabled in your config file, in order to learn if they provide them.

All module backend metrics names use the following convention: `[global, backends, Module, BackendAction]` and `[global, backends, Module, BackendAction, count]`.
The former is a histogram of operation times. However, the time is not recorded if a backend operation exits with an exception.
The latter is a number of calls (spiral metric), incremented for *every* call (even a failed one).

Besides these, following authentication metrics are always available:

* `[HostType, backends, auth, authorize]`
* `[HostType, backends, auth, check_password]`
* `[HostType, backends, auth, try_register]`
* `[HostType, backends, auth, does_user_exist]`

These are **total** times of respective operations.
One operation usually requires only a single call to an auth backend but sometimes with e.g. 3 backends configured, the operation may fail for first 2 backends.
In such case, these metrics will be updated with combined time of 2 failed and 1 successful request.

Additionally, the RDBMS layer in MongooseIM exposes two more metrics, if RDBMS is configured:

* `[global, backends, mongoose_rdbms, query]` - Execution time of a "simple" (not prepared) query by a DB driver.
* `[global, backends, mongoose_rdbms, execute]` - Execution time of a prepared query by a DB driver.
