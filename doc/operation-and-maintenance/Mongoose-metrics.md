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
    * Backend metrics.
    Histograms with timings of calls to various backends.
    * VM metrics. Basic Erlang VM statistics.

### What does `spiral` mean?

This kind of metric provides 2 values: `total` event count (e.g. stanzas processed) and a value in 60s window (`one` value).
Dividing `one` value by 60 provides an average per-second value over last minute.

## Per host metrics

### Hook metrics

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | ----------------------------------- |
| `[Host, adhoc_local_commands]` | spiral | An adhoc command is executed in `ejabberd_local` context. |
| `[Host, adhoc_sm_commands]` | spiral | And adhoc command is executed in `ejabberd_sm` context. |
| `[Host, adhoc_local_items]` | spiral | A list of available adhoc commands is requested via Disco protocol in `ejabberd_local` context. |
| `[Host, adhoc_sm_items]` | spiral | A list of available adhoc commands is requested via Disco protocol in `ejabberd_sm` context. |
| `[Host, anonymous_purge_hook]` | spiral | An anonymous user disconnects. |
| `[Host, c2s_stream_features]` | spiral | Stream features are collected and sent. |
| `[Host, c2s_unauthenticated_iq]` | spiral | An IQ sent from a user to a server without authentication. |
| `[Host, disco_info]` | spiral | An information about the server has been requested via Disco protocol. |
| `[Host, disco_local_features]` | spiral | A list of server's features is gathered. |
| `[Host, disco_local_identity]` | spiral | A server's identities list is gathered. |
| `[Host, disco_local_items]` | spiral | A list of server's items (e.g. services) is gathered. |
| `[Host, disco_sm_features]` | spiral | A list of user's features is gathered. |
| `[Host, disco_sm_identity]` | spiral | A list of user's identities is gathered. |
| `[Host, disco_sm_items]` | spiral | A list of user's items is gathered. |
| `[Host, host_config_update]` | spiral | When a config update is being performed and there is a change in the ldap configuratio.n |
| `[Host, local_send_to_resource_hook]` | spiral | A stanza is sent to a server's resource (e.g. `localhost/watchdogs`) |
| `[Host, mam_lookup_messages]` | spiral | An archive lookup is performed. |
| `[Host, offline_message_hook]` | spiral | A message was sent to an offline user. (Except for "error", "headline" and "groupchat" message types.) |
| `[Host, offline_groupchat_message_hook]` | spiral | A groupchat message was sent to an offline user. |
| `[Host, privacy_get_user_list]` | spiral | User's default privacy list is fetched. |
| `[Host, privacy_updated_list]` | spiral | User's privacy list is updated. |
| `[Host, resend_offline_messages_hook]` | spiral | A list of offline messages is gathered for delivery to a user's new connection. |
| `[Host, roster_get_jid_info]` | spiral | An information about specific roster entry status is fetched. |
| `[Host, roster_get_subscription_lists]` |  spiral | Presence subscription lists (based on which presence updates are broadcasted) are gathered. |
| `[Host, roster_get_versioning_feature]` |  spiral | Roster versioning support is checked. |
| `[Host, roster_in_subscription]` | spiral | A presence with subscription update is processed. |
| `[Host, roster_out_subscription]` | spiral | A presence with subscription update is received from a client. |
| `[Host, sm_broadcast]` | spiral | A stanza is broadcasted to all of user's resources. |
| `[Host, unset_presence_hook]` | spiral | A user disconnects or sends an `unavailable` presence. |

### Message Archive Management metrics

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | ----------------------------------- |
| `[Host, modMamArchiveRemoved]` | spiral | User's entire archive is removed. |
| `[Host, modMamArchived]` | spiral | A message is stored in user's archive. |
| `[Host, modMamDropped]` | spiral | A message couldn't be enqueued due to overloaded async worker. |
| `[Host, modMamDropped2]` | spiral | A message couldn't be stored in the DB (and got dropped). |
| `[Host, modMamDroppedIQ]` | spiral | MAM IQ has been dropped due to: high query frequency/invalid syntax or type. |
| `[Host, modMamFlushed]` | spiral | Message was stored to DB asynchronously. |
| `[Host, modMamForwarded]` | spiral | A message is sent to a client as a part of MAM query result. |
| `[Host, modMamLookups]` | spiral | A MAM lookup is performed. |
| `[Host, modMamSinglePurges]` | spiral | A single purge request is processed by MAM. |
| `[Host, modMamMultiplePurges]` | spiral | A bulk purge request is processed by MAM. |
| `[Host, modMamPrefsGets]` | spiral | Archiving preferences have been requested by a client. |
| `[Host, modMamPrefsSets]` | spiral | Archiving preferences have been updated by a client. |
| `[Host, modMucMamArchiveRemoved]` | spiral | Room's entire archive is removed. |
| `[Host, modMucMamArchived]` | spiral | A message is stored in room's archive. |
| `[Host, modMucMamForwarded]` | spiral | A message is sent to a client as a part of MAM query result from MUC room. |
| `[Host, modMucMamLookups]` | spiral | A MAM lookup in MUC room is performed. |
| `[Host, modMucMamSinglePurges]` | spiral | A single purge request for MUC room is processed by MAM. |
| `[Host, modMucMamMultiplePurges]` | spiral | A bulk purge request for MUC room is processed by MAM. |
| `[Host, modMucMamPrefsGets]` | spiral | MUC archiving preferences have been requested by a client. |
| `[Host, modMucMamPrefsSets]` | spiral | MUC archiving preferences have been updated by a client. |

### Other

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | ----------------------------------- |
| `[Host, modCSIInactive]` | spiral | A client becomes inactive. ([Client State Indication](https://xmpp.org/extensions/xep-0352.html)) |
| `[Host, modCSIActive]` | spiral | A client becomes active. ([Client State Indication](https://xmpp.org/extensions/xep-0352.html)) |
| `[Host, modPresenceSubscriptions]` | spiral | Presence subscriptions is processed. |
| `[Host, modPresenceUnsubscriptions]` | spiral | Presence unsubscription is processed. |
| `[Host, modPrivacyGets]` | spiral | IQ privacy `get` is processed. |
| `[Host, modPrivacyPush]` | spiral | Privacy list update is sent to a single session. |
| `[Host, modPrivacySets]` | spiral | IQ privacy `set` is processed. |
| `[Host, modPrivacySetsActive]` | spiral | Active privacy list is changed. |
| `[Host, modPrivacySetsDefault]` | spiral | Default privacy list is changed. |
| `[Host, modPrivacyStanzaAll]` | spiral | A packet is checked against privacy list. |
| `[Host, modPrivacyStanzaDenied]` | spiral | Privacy list check resulted in `deny`. |
| `[Host, modPrivacyStanzaBlocked]` | spiral | Privacy list check resulted in `block`. |
| `[Host, modRegisterCount]` | spiral | A user registers via `mod_register` module. |
| `[Host, modUnregisterCount]` | spiral | A user unregisters via `mod_register` module. |
| `[Host, modRosterGets]` | spiral | User's roster is fetched. |
| `[Host, modRosterPush]` | spiral | A roster update is pushed to a single session. |
| `[Host, modRosterSets]` | spiral | User's roster is updated. |
| `[Host, sessionAuthAnonymous]` | spiral | A client authenticates anonymously. |
| `[Host, sessionAuthFails]` | spiral | A client failed to authenticate. |
| `[Host, sessionCount]` | counter | Number of active sessions. |
| `[Host, sessionLogouts]` | spiral | A client session is closed. |
| `[Host, sessionSuccessfulLogins]` | spiral | A client session is opened. |
| `[Host, xmppErrorIq]` | spiral | An `error` IQ is sent to a client. |
| `[Host, xmppErrorMessage]` | spiral | An `error` message is sent to a client. |
| `[Host, xmppErrorPresence]` | spiral | An `error` presence is sent to a client. |
| `[Host, xmppErrorTotal]` | spiral | A stanza with `error` type is routed. |
| `[Host, xmppMessageBounced]` | spiral | A `service-unavailable` error is sent, because message recipient if offline. |
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

## Global metrics

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | ----------------------------------- |
| `[global, nodeSessionCount]` | value | A number of sessions connected to a given MongooseIM node. |
| `[global, totalSessionCount]` | value | A number of sessions connected to a MongooseIM cluster. |
| `[global, uniqueSessionCount]` | value | A number of unique users connected to a MongooseIM cluster (e.g. 3 sessions of the same user will be counted as 1 in this metric). |
| `[global nodeUpTime]` | value | A node uptime. |

### Data metrics

| Metric name | Type | Description |
| ----------- | ---- | ----------- |
| `[global, data, xmpp, received, xml_stanza_size]` | histogram | A size (in bytes) of a received stanza after decompression and decryption. |
| `[global, data, xmpp, sent, xml_stanza_size]` | histogram | A size (in bytes) of a stanza sent to a client socket. |
| `[global, data, xmpp, received, compressed_size]` | histogram | A size (in bytes) of a received stanza before decompression. |
| `[global, data, xmpp, sent, compressed_size]` | histogram | A size (in bytes) of a stanza after compression. |
| `[global, data, xmpp, received, encrypted_size]` | histogram | A size (in bytes) of a received stanza before decryption. |
| `[global, data, xmpp, sent, encrypted_size]` | histogram | A size (in bytes) of a stanza after encryption. |
| `[global, data, dist]` | function | Network stats for an Erlang distributed communication. A proplist with values: `recv_oct`, `recv_cnt`, `recv_max`, `send_oct`, `send_max`, `send_cnt`, `send_pend`, `connections` |
| `[global, data, odbc, PoolName]` | function | For every ODBC pool defined, an instance of this metric is available. It is a proplist with values `workers`, `recv_oct`, `recv_cnt`, `recv_max`, `send_oct`, `send_max`, `send_cnt`, `send_pend`. |

### Backend metrics

Some extension modules expose histograms with timings of calls made to their backends.
Please check the documentation of modules that are enabled in your config file, in order to learn if they provide them.

Besides these, following authentication metrics are always available:

* `[global, backends, auth, authorize]`
* `[global, backends, auth, check_password]`
* `[global, backends, auth, try_register]`
* `[global, backends, auth, does_user_exist]`

These are **total** times of respective operations.
One operation usually requires only a single call to an auth backend but sometimes with e.g. 3 backends configured, the operation may fail for first 2 backends.
In such case, these metrics will be updated with a combined time of 2 failed and 1 successful request.

Additionaly, RDMBS layer in MongooseIM exposes one more metric, if RDBMS is configured:

* `[global, backends, mongoose_rdbms, query]` - An execution time of a "not prepared" query by a DB driver.

### VM metrics

| Metric name | Type | Description |
| ----------- | ---- | ----------- |
| `[global, erlang, memory]` | function | A proplist with `total`, `processes_used`, `atom_used`, `binary`, `ets` and `system` memory stats. |
| `[global, erlang, system_info]` | function | A proplist with `port_count`, `port_limit`, `process_count`, `process_limit`, `ets_limit` stats. |

