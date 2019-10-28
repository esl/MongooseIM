# MongooseIM metrics

MongooseIM by default collects many metrics showing the user behaviour and general system statistics.
They are managed by [exometer](https://github.com/Feuerlabs/exometer).
MongooseIM uses [ESL's fork of this project](https://github.com/esl/exometer/tree/1.2.1-patched).

All metrics are divided into the following groups:

* Per host metrics: Gathered separately for every XMPP host supported by the cluster.
 **Warning:** If a cluster supports many (thousands or more) domains, performance issues might occur.
 To avoid this, use global equivalents of the metrics with `all_metrics_are_global` config option.
    * Hook metrics.
    They are created for every [hook](../developers-guide/Hooks-and-handlers.md) and incremented on every call to it.
* Global metrics: Metrics common for all XMPP hosts.
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

It is similiar to a `value` type but consists of two properties:

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

## Per host metrics

### Hook metrics

There are more hook metrics than what is listed in this table, because they are automatically created for every new hook.
As a result it makes more sense to maintain a list of the most relevant or useful items, rather than keeping this table fully in sync with the code.

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[Host, anonymous_purge_hook]` | spiral | An anonymous user disconnects. |
| `[Host, c2s_unauthenticated_iq]` | spiral | An IQ sent from a user to a server without authentication. |
| `[Host, disco_info]` | spiral | An information about the server has been requested via Disco protocol. |
| `[Host, disco_local_features]` | spiral | A list of server features is gathered. |
| `[Host, disco_local_identity]` | spiral | A list of server identities is gathered. |
| `[Host, disco_local_items]` | spiral | A list of server's items (e.g. services) is gathered. |
| `[Host, disco_sm_features]` | spiral | A list of user's features is gathered. |
| `[Host, disco_sm_identity]` | spiral | A list of user's identities is gathered. |
| `[Host, disco_sm_items]` | spiral | A list of user's items is gathered. |
| `[Host, mam_lookup_messages]` | spiral | An archive lookup is performed. |
| `[Host, offline_message_hook]` | spiral | A message was sent to an offline user. (Except for "error", "headline" and "groupchat" message types.) |
| `[Host, offline_groupchat_message_hook]` | spiral | A groupchat message was sent to an offline user. |
| `[Host, privacy_updated_list]` | spiral | User's privacy list is updated. |
| `[Host, resend_offline_messages_hook]` | spiral | A list of offline messages is gathered for delivery to a user's new connection. |
| `[Host, roster_get_subscription_lists]` |  spiral | Presence subscription lists (based on which presence updates are broadcasted) are gathered. |
| `[Host, roster_in_subscription]` | spiral | A presence with subscription update is processed. |
| `[Host, roster_out_subscription]` | spiral | A presence with subscription update is received from a client. |
| `[Host, sm_broadcast]` | spiral | A stanza is broadcasted to all of user's resources. |
| `[Host, unset_presence_hook]` | spiral | A user disconnects or sends an `unavailable` presence. |

### Presences & rosters

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[Host, modPresenceSubscriptions]` | spiral | Presence subscription is processed. |
| `[Host, modPresenceUnsubscriptions]` | spiral | Presence unsubscription is processed. |
| `[Host, modRosterGets]` | spiral | User's roster is fetched. |
| `[Host, modRosterPush]` | spiral | A roster update is pushed to a single session. |
| `[Host, modRosterSets]` | spiral | User's roster is updated. |

### Privacy lists

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[Host, modPrivacyGets]` | spiral | IQ privacy `get` is processed. |
| `[Host, modPrivacyPush]` | spiral | Privacy list update is sent to a single session. |
| `[Host, modPrivacySets]` | spiral | IQ privacy `set` is processed. |
| `[Host, modPrivacySetsActive]` | spiral | Active privacy list is changed. |
| `[Host, modPrivacySetsDefault]` | spiral | Default privacy list is changed. |
| `[Host, modPrivacyStanzaAll]` | spiral | A packet is checked against the privacy list. |
| `[Host, modPrivacyStanzaDenied]` | spiral | Privacy list check resulted in `deny`. |
| `[Host, modPrivacyStanzaBlocked]` | spiral | Privacy list check resulted in `block`. |

### Other

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[Host, sessionAuthAnonymous]` | spiral | A client authenticates anonymously. |
| `[Host, sessionAuthFails]` | spiral | A client failed to authenticate. |
| `[Host, sessionCount]` | counter | Number of active sessions. |
| `[Host, sessionLogouts]` | spiral | A client session is closed. |
| `[Host, sessionSuccessfulLogins]` | spiral | A client session is opened. |
| `[Host, xmppErrorIq]` | spiral | An `error` IQ is sent to a client. |
| `[Host, xmppErrorMessage]` | spiral | An `error` message is sent to a client. |
| `[Host, xmppErrorPresence]` | spiral | An `error` presence is sent to a client. |
| `[Host, xmppErrorTotal]` | spiral | A stanza with `error` type is routed. |
| `[Host, xmppMessageBounced]` | spiral | A `service-unavailable` error is sent, because the message recipient if offline. |
| `[Host, xmppIqSent]` | spiral | An IQ is sent by a client. |
| `[Host, xmppMessageSent]` | spiral | A message is sent by a client |
| `[Host, xmppPresenceSent]` | spiral | A presence is sent by a client. |
| `[Host, xmppStanzaSent]` | spiral | A stanza is sent by a client. |
| `[Host, xmppIqReceived]` | spiral | An IQ is sent to a client. |
| `[Host, xmppMessageReceived]` | spiral | A message is sent to a client. |
| `[Host, xmppPresenceReceived]` | spiral | A presence is sent to a client. |
| `[Host, xmppStanzaReceived]` | spiral | A stanza is sent to a client. |
| `[Host, xmppStanzaCount]` | spiral | A stanza is sent to a client. |
| `[Host, xmppStanzaDropped]` | spiral | A stanza is dropped due to an AMP rule or a `filter_packet` processing flow. |

### Extension-specific metrics

Metrics specific to an extension, e.g. Message Archive Management, are described in respective module documentation pages.

## Global metrics

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[global, routingErrors]` | spiral | It is not possible to route a stanza (all routing handlers failed). |
| `[global, nodeSessionCount]` | value | A number of sessions connected to a given MongooseIM node. |
| `[global, totalSessionCount]` | value | A number of sessions connected to a MongooseIM cluster. |
| `[global, uniqueSessionCount]` | value | A number of unique users connected to a MongooseIM cluster (e.g. 3 sessions of the same user will be counted as 1 in this metric). |
| `[global, cache, unique_sessions_number]` | gauge | A cached value of `uniqueSessionCount`. It is automatically updated when a unique session count is calculated. |
| `[global, nodeUpTime]` | value | Node uptime. |
| `[global, clusterSize]` | value | A number of nodes in a MongooseIM cluster seen by a given MongooseIM node. |
| `[global, tcpPortsUsed]` | value | A number of open tcp connections. This should relate to the number of connected sessions and databases, as well as federations and http requests, in order to detect connection leaks. |
| `[global, processQueueLengths]` | probe | The number of queued messages in the internal message queue of every erlang process, and the internal queue of every fsm (ejabberd\_c2s). This is sampled every 30 seconds asynchronously. It is a good indicator of an overloaded system: if too many messages are queued at the same time, the system is not able to process the data at the rate it was designed for. |

### Data metrics

| Metric name | Type | Description |
| ----------- | ---- | ----------- |
| `[global, data, xmpp, received, xml_stanza_size]` | histogram | A size (in bytes) of a received stanza after decompression and decryption. |
| `[global, data, xmpp, sent, xml_stanza_size]` | histogram | A size (in bytes) of a stanza sent to a client socket. |
| `[global, data, xmpp, received, compressed_size]` | histogram | A size (in bytes) of a received stanza before decompression. |
| `[global, data, xmpp, sent, compressed_size]` | histogram | A size (in bytes) of a stanza after compression. |
| `[global, data, xmpp, received, encrypted_size]` | histogram | A size (in bytes) of a received stanza before decryption. |
| `[global, data, xmpp, sent, encrypted_size]` | histogram | A size (in bytes) of a stanza after encryption. |
| `[global, data, dist]` | proplist | Network stats for an Erlang distributed communication. A proplist with values: `recv_oct`, `recv_cnt`, `recv_max`, `send_oct`, `send_max`, `send_cnt`, `send_pend`, `connections` |
| `[global, data, rdbms, PoolName]` | proplist | For every RDBMS pool defined, an instance of this metric is available. It is a proplist with values `workers`, `recv_oct`, `recv_cnt`, `recv_max`, `send_oct`, `send_max`, `send_cnt`, `send_pend`. |

### VM metrics

| Metric name | Type | Description |
| ----------- | ---- | ----------- |
| `[global, erlang, memory]` | proplist | A proplist with `total`, `processes_used`, `atom_used`, `binary`, `ets` and `system` memory stats. |
| `[global, erlang, system_info]` | proplist | A proplist with `port_count`, `port_limit`, `process_count`, `process_limit`, `ets_limit` stats. |

## Backend metrics

Some extension modules expose histograms with timings of calls made to their backends.
Please check the documentation of modules that are enabled in your config file, in order to learn if they provide them.

All module backend metrics names use the following convention: `[global, backends, Module, BackendAction]` and `[global, backends, Module, BackendAction, count]`.
The former is a histogram of operation times. However, the time is not recorded if a backend operation exits with an exception.
The latter is a number of calls (spiral metric), incremented for *every* call (even a failed one).

Besides these, following authentication metrics are always available:

* `[Host, backends, auth, authorize]`
* `[Host, backends, auth, check_password]`
* `[Host, backends, auth, try_register]`
* `[Host, backends, auth, does_user_exist]`

These are **total** times of respective operations.
One operation usually requires only a single call to an auth backend but sometimes with e.g. 3 backends configured, the operation may fail for first 2 backends.
In such case, these metrics will be updated with combined time of 2 failed and 1 successful request.

Additionaly, RDBMS layer in MongooseIM exposes two more metrics, if RDBMS is configured:

* `[global, backends, mongoose_rdbms, query]` - Execution time of a "simple"" (not prepared) query by a DB driver.
* `[global, backends, mongoose_rdbms, execute]` - Execution time of a prepared query by a DB driver.
