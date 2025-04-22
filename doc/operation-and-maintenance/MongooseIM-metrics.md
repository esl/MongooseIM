# MongooseIM metrics

MongooseIM by default collects many metrics showing the user behaviour and general system statistics.

Metrics are exposed in the [Prometheus](https://prometheus.io/) format, or they are managed by [ESL's fork of the Exometer project](https://github.com/esl/exometer/tree/1.2.1-patched).
Both can be configured in the [instrumentation](../configuration/instrumentation.md) section.
There are some differences between the two systems, when it comes to conventions, naming and metric types.

In the default configuration file, Prometheus metrics are enabled, and available at the <http://127.0.0.1:9091/metrics> endpoint.

All metrics are divided into the following groups:

* [Per host type metrics](#per-host-type-metrics): Gathered separately for every host type supported by the cluster.

    !!! Warning
        If a cluster supports many (thousands or more) host types, performance issues might occur.
        To avoid this, when using Exometer, use global equivalents of the metrics with [`all_metrics_are_global` config option](../configuration/instrumentation.md#instrumentationexometerall_metrics_are_global).

* [Global metrics](#global-metrics): Metrics common for all host types.
    * Data metrics.
    These are misc. metrics related to data transfers (e.g. sent and received stanza size statistics).
    * VM metrics. Basic Erlang VM statistics.
* [Backend metrics](#backend-metrics): Histograms with timings of calls to various backends.

## Metrics types

=== "Prometheus"

    <h3>`counter`</h3>

    A monotonically increasing metric type. It is used for events like the number of stanzas processed by the system.

    **Example:**
        ```
        # TYPE xmpp_element_in_message_count counter
        # HELP xmpp_element_in_message_count Event: xmpp_element_in, Metric: message_count
        xmpp_element_in_message_count{connection_type="c2s",host_type="localhost"} 0
        ```

    <h3>`gauge`</h3>

    A metric that represents a current value in the system.

    **Example:**
        ```
        # TYPE mnesia_info_running_db_nodes gauge
        # HELP mnesia_info_running_db_nodes Event: mnesia_info, Metric: running_db_nodes
        mnesia_info_running_db_nodes 0
        ```

    <h3>`histogram`</h3>

    A histogram collects values and groups them in buckets.

    **Example:**
        ```
        # TYPE xmpp_element_in_byte_size histogram
        # HELP xmpp_element_in_byte_size Event: xmpp_element_in, Metric: byte_size
        xmpp_element_in_byte_size_bucket{connection_type="c2s",host_type="localhost",le="1"} 0
        ...
        xmpp_element_in_byte_size_bucket{connection_type="c2s",host_type="localhost",le="1073741824"} 0
        xmpp_element_in_byte_size_bucket{connection_type="c2s",host_type="localhost",le="+Inf"} 0
        ```

=== "Exometer"

    <h3>`spiral`</h3>

    This kind of metric provides 2 values: `total` event count (e.g. stanzas processed) and a value in 60s window (`one` value).
    Dividing `one` value by 60 provides an average per-second value over last minute.

    **Example:** `[{total, 1000}, {one, 20}]`

    <h3>`counter`</h3>

    A simple monotonically increasing value. It consists of two properties:

    **Example:** `[{value, 12}, {ms_since_reset, 91761}]`

    <h3>`gauge`</h3>

    It is similar to a `counter` type but can be set to any value.

    * `value`
    * `ms_since_reset` - Time in milliseconds elapsed from the last metric update.

    **Example:** `[{value, 12}, {ms_since_reset, 91761}]`

    <h3>`histogram`</h3>

    A histogram collects values over a sliding window of 60s and exposes the following stats:

    * `n` - A number of samples.
    * `mean` - An arithmetic mean.
    * `min`
    * `max`
    * `median`
    * `50`, `75`, `90`, `95`, `99`, `999` - 50th, 75th, 90th, 95th, 99th and 99.9th percentile

## List of metrics

The metrics listed below are grouped by the covered functionality.

### Presences & rosters

These metrics have the `host_type` label for Prometheus.
Since Exometer doesn't support labels, the host type is a part of the metric name.

=== "Prometheus"

    | Name | Type | Description (when it gets incremented) |
    | ---- | ---- | -------------------------------------- |
    | `sm_presence_subscription_subscription_count` | counter | Presence subscription is processed. |
    | `sm_presence_subscription_unsubscription_count` | counter | Presence unsubscription is processed. |
    | `mod_roster_get_count` | counter | User's roster is fetched. |
    | `mod_roster_push_count` | counter | A roster update is pushed to a single session. |
    | `mod_roster_set_count` | counter | User's roster is updated. |

=== "Exometer"

    | Name | Type | Description (when it gets incremented) |
    | ---- | ---- | -------------------------------------- |
    | `[HostType, sm_presence_subscription, subscription_count]` | spiral | Presence subscription is processed. |
    | `[HostType, sm_presence_subscription, unsubscription_count]` | spiral | Presence unsubscription is processed. |
    | `[HostType, mod_roster_get, count]` | spiral | User's roster is fetched. |
    | `[HostType, mod_roster_push, count]` | spiral | A roster update is pushed to a single session. |
    | `[HostType, mod_roster_set, count]` | spiral | User's roster is updated. |

### Privacy lists

These metrics have the `host_type` label for Prometheus.
Since Exometer doesn't support labels, the host type is a part of the metric name.

=== "Prometheus"

    | Name | Type | Description (when it gets incremented) |
    | ---- | ---- | -------------------------------------- |
    | `mod_privacy_get_count` | counter | IQ privacy `get` is processed. |
    | `mod_privacy_push_item_count` | counter | Privacy list update is sent to a single session. |
    | `mod_privacy_set_count` | counter | IQ privacy `set` is processed. |
    | `mod_privacy_set_active_count` | counter | Active privacy list is changed. |
    | `mod_privacy_set_default_count` | counter | Default privacy list is changed. |
    | `mod_privacy_check_packet_count` | counter | A packet is checked against the privacy list. |
    | `mod_privacy_check_packet_denied_count` | counter | Privacy list check resulted in `deny`. |
    | `mod_privacy_check_packet_blocked_count` | counter | Privacy list check resulted in `block`. |

=== "Exometer"

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

### Sessions and routing

Some of these metrics have the `host_type` label for Prometheus.
Since Exometer doesn't support labels, in such cases the host type is a part of the metric name.

=== "Prometheus"

    | Name | Labels | Type | Description (when it gets incremented) |
    | ---- | ------ | ---- | -------------------------------------- |
    | `c2s_auth_failed_count` | `host_type` | counter | A connecting client failed to authenticate. |
    | `s2s_auth_failed_count` | - | counter | A connecting server failed to authenticate, or MongooseIM failed to authenticate when connecting to another server. |
    | `component_auth_failed_count` | - | counter | A connecting external component failed to authenticate. |
    | `sm_session_count` | `host_type` | gauge | Number of active sessions. |
    | `sm_session_logouts` | `host_type` | counter | A client session is closed. |
    | `sm_session_logins` | `host_type` | counter | A client session is opened. |
    | `c2s_message_processed_time` | `host_type` | histogram | Processing time for incoming c2s stanzas. |
    | `sm_message_bounced_count` | `host_type` | counter | A `service-unavailable` error is sent, because the message recipient is offline. |
    | `router_stanza_dropped_count` | `host_type` | counter | A stanza is dropped due to an AMP rule or a `filter_local_packet` processing flow. |
    | `router_no_route_found_count` | `host_type` | counter | It is not possible to route a stanza (all routing handlers failed). |

=== "Exometer"

    | Name | Type | Description (when it gets incremented) |
    | ---- | ---- | -------------------------------------- |
    | `[HostType, c2s_auth_failed, count]` | spiral | A connecting client failed to authenticate. |
    | `[global, s2s_auth_failed, count]` | spiral | A connecting server failed to authenticate, or MongooseIM failed to authenticate when connecting to another server. |
    | `[global, component_auth_failed, count]` | spiral | A connecting external component failed to authenticate. |
    | `[HostType, sm_session, count]` | counter | Number of active sessions. |
    | `[HostType, sm_session, logouts]` | spiral | A client session is closed. |
    | `[HostType, sm_session, logins]` | spiral | A client session is opened. |
    | `[HostType, c2s_message_processing_time`] | histogram | Processing time for incoming c2s stanzas. |
    | `[HostType, sm_message_bounced, count]` | spiral | A `service-unavailable` error is sent, because the message recipient is offline. |
    | `[HostType, router_stanza_dropped, count]` | spiral | A stanza is dropped due to an AMP rule or a `filter_local_packet` processing flow. |
    | `[HostType, router_no_route_found, count]` | spiral | It is not possible to route a stanza (all routing handlers failed). |

### Connection pools

For every RDBMS pool defined, an instance of these metrics are available.
Prometheus metrics have a `host_type` and `pool_tag` labels associated with these metrics.
Since Exometer doesn't support labels, the host types and tags are part of the metric names.

=== "Prometheus"

    | Name | Type | Description (when it gets incremented) |
    | ---- | ---- | -------------------------------------- |
    | `wpool_rdbms_stats_workers`   | gauge | Number of workers in the pool |
    | `wpool_rdbms_stats_recv_oct`  | gauge | Number of bytes received |
    | `wpool_rdbms_stats_recv_cnt`  | gauge | Number of packets received |
    | `wpool_rdbms_stats_recv_max`  | gauge | Size of the largest packet, in bytes |
    | `wpool_rdbms_stats_send_oct`  | gauge | Number of bytes sent |
    | `wpool_rdbms_stats_send_max`  | gauge | Size of the largest packet |
    | `wpool_rdbms_stats_send_cnt`  | gauge | Number of packets sent |
    | `wpool_rdbms_stats_send_pend` | gauge | Number of bytes waiting to be sent |

=== "Exometer"

    | Name                                                         | Type    | Description (when it gets incremented) |
    |--------------------------------------------------------------|---------|----------------------------------------|
    | `[HostType, wpool_rdbms_stats, PoolTag, workers]`   | gauge | Number of workers in the pool          |
    | `[HostType, wpool_rdbms_stats, PoolTag, recv_oct]`  | gauge | Number of bytes received               |
    | `[HostType, wpool_rdbms_stats, PoolTag, recv_cnt]`  | gauge | Number of packets received             |
    | `[HostType, wpool_rdbms_stats, PoolTag, recv_max]`  | gauge | Size of the largest packet, in bytes   |
    | `[HostType, wpool_rdbms_stats, PoolTag, send_oct]`  | gauge | Number of bytes sent                   |
    | `[HostType, wpool_rdbms_stats, PoolTag, send_max]`  | gauge | Size of the largest packet             |
    | `[HostType, wpool_rdbms_stats, PoolTag, send_cnt]`  | gauge | Number of packets sent                 |
    | `[HostType, wpool_rdbms_stats, PoolTag, send_pend]` | gauge | Number of bytes waiting to be sent     |

When using a Rabbit worker pool, metrics defined in [mod_event_pusher_rabbit](../modules/mod_event_pusher_rabbit.md) are
available.

### Extension-specific metrics

Metrics specific to an extension, e.g. Message Archive Management, are described in respective module documentation pages.

### Global gauges

All of these metrics are updated periodically. The interval at which they are probed can be configured with the [`instrumentation.probe_interval`](../configuration/instrumentation.md#instrumentationprobe_interval) option.

=== "Prometheus"

    | Name | Type | Description |
    | ---- | ---- | -------------------------------------- |
    | `sm_node_sessions_count` | gauge | A number of sessions connected to a given MongooseIM node. |
    | `sm_total_sessions_count` | gauge | A number of sessions connected to a MongooseIM cluster. |
    | `sm_unique_sessions_count` | gauge | A number of unique users connected to a MongooseIM cluster (e.g. 3 sessions of the same user will be counted as 1 in this metric). |
    | `system_up_time_seconds` | gauge | Node uptime. |
    | `mnesia_info_running_db_nodes` | gauge | A number of nodes in a MongooseIM cluster seen by a given MongooseIM node (based on Mnesia). For CETS, use `cets_info, joined_nodes` instead. |
    | `system_tcp_ports_count` | gauge | A number of open tcp connections. This should relate to the number of connected sessions and databases, as well as federations and http requests. A constantly growing value might indicate a connection leak. |
    | `system_process_queue_lengths_total` | gauge | The total number of incoming messages queued in the Erlang processes. It is a good indicator of an overloaded system: if too many messages are queued at the same time, the system is most likely overloaded with incoming data. |
    | `system_dist_data_Metric` | gauge | Network stats for Erlang distributed communication. `Metric` can be `recv_oct`, `recv_cnt`, `recv_max`, `send_oct`, `send_max`, `send_cnt`, `send_pend` or `connections`. |

=== "Exometer"

    | Name | Type | Description |
    | ---- | ---- | -------------------------------------- |
    | `[global, sm_node_sessions, count]` | gauge | A number of sessions connected to a given MongooseIM node. |
    | `[global, sm_total_sessions, count]` | gauge | A number of sessions connected to a MongooseIM cluster. |
    | `[global, sm_unique_sessions, count]` | gauge | A number of unique users connected to a MongooseIM cluster (e.g. 3 sessions of the same user will be counted as 1 in this metric). |
    | `[global, system_up_time, seconds]` | gauge | Node uptime. |
    | `[global, mnesia_info, running_db_nodes]` | gauge | A number of nodes in a MongooseIM cluster seen by a given MongooseIM node (based on Mnesia). For CETS, use `[global, cets_info, joined_nodes]` instead. |
    | `[global, system_tcp_ports, count]` | gauge | A number of open tcp connections. This should relate to the number of connected sessions and databases, as well as federations and http requests. A constantly growing value might indicate a connection leak. |
    | `[global, system_process_queue_lengths, total]` | probe | The total number of incoming messages queued in the Erlang processes. It is a good indicator of an overloaded system: if too many messages are queued at the same time, the system is most likely overloaded with incoming data. |
    | `[global, system_dist_data, Metric]` | gauge | Network stats for Erlang distributed communication. `Metric` can be `recv_oct`, `recv_cnt`, `recv_max`, `send_oct`, `send_max`, `send_cnt`, `send_pend` or `connections`. |

### XMPP traffic metrics

Prometheus metrics have the following labels:

* `host_type` indicates the host type associated with the connection. If the host type is unknown (e.g. for component connections or early stream errors), the label is empty.
* `connection_type` can be `c2s` (for clients), `s2s` (for federated servers) or `component` (for external components).

Since Exometer doesn't support labels, `HostType` and `ConnType` parts are included in the metric names.
If host type is unknown, `HostType` is set to `global`.

=== "Prometheus"

    | Name | Type | Description (when it gets incremented) |
    | ---- | ---- | -------------------------------------- |
    | `xmpp_element_in_count` | counter | An XML element is received from a client, server or component. |
    | `xmpp_element_in_stanza_count` | counter | An XMPP stanza is received from a client, server or component. |
    | `xmpp_element_in_message_count` | counter | A message stanza is received from a client, server or component. |
    | `xmpp_element_in_iq_count` | counter | An IQ stanza is received from a client, server or component. |
    | `xmpp_element_in_presence_count` | counter | A presence stanza is received from a client, server or component. |
    | `xmpp_element_in_error_count` | counter | An error is received from a client, server or component. |
    | `xmpp_element_in_message_error_count` | counter | A message error is received from a client, server or component. |
    | `xmpp_element_in_iq_error_count` | counter | An IQ error is received from a client, server or component. |
    | `xmpp_element_in_presence_error_count` | counter | A presence error is received from a client, server or component. |
    | `xmpp_element_in_byte_size` | histogram | Size of an XML element received from a client, server or component. |
    | `xmpp_element_out_count` | counter | An XML element is sent to a client, server or component. |
    | `xmpp_element_out_stanza_count` | counter | An XMPP stanza is sent to a client, server or component. |
    | `xmpp_element_out_iq_count` | counter | An IQ stanza is sent to a client, server or component. |
    | `xmpp_element_out_message_count` | counter | A message stanza is sent to a client, server or component. |
    | `xmpp_element_out_presence_count` | counter | A presence stanza is sent to a client, server or component. |
    | `xmpp_element_out_error_count` | counter | An error is sent to a client, server or component. |
    | `xmpp_element_out_iq_error_count` | counter | An IQ error is sent to a client, server or component. |
    | `xmpp_element_out_message_error_count` | counter | A message error is sent to a client, server or component. |
    | `xmpp_element_out_presence_error_count` | counter | A presence error is sent to a client, server or component. |
    | `xmpp_element_out_byte_size` | histogram | Size of an XML element sent to a client, server or component. |

=== "Exometer"

    | Name | Type | Description (when it gets incremented) |
    | ---- | ---- | -------------------------------------- |
    | `[HostType, xmpp_element_in, ConnType, count]` | spiral | An XML element is received from a client, server or component. |
    | `[HostType, xmpp_element_in, ConnType, stanza_count]` | spiral | An XMPP stanza is received from a client, server or component. |
    | `[HostType, xmpp_element_in, ConnType, message_count]` | spiral | A message stanza is received from a client, server or component. |
    | `[HostType, xmpp_element_in, ConnType, iq_count]` | spiral | An IQ stanza is received from a client, server or component. |
    | `[HostType, xmpp_element_in, ConnType, presence_count]` | spiral | A presence stanza is received from a client, server or component. |
    | `[HostType, xmpp_element_in, ConnType, error_count]` | spiral | An error is received from a client, server or component. |
    | `[HostType, xmpp_element_in, ConnType, message_error_count]` | spiral | A message error is received from a client, server or component. |
    | `[HostType, xmpp_element_in, ConnType, iq_error_count]` | spiral | An IQ error is received from a client, server or component. |
    | `[HostType, xmpp_element_in, ConnType, presence_error_count]` | spiral | A presence error is received from a client, server or component. |
    | `[HostType, xmpp_element_in, ConnType, byte_size]` | histogram | Size of an XML element received from a client. |
    | `[HostType, xmpp_element_out, ConnType, count]` | spiral | An XML element is sent to a client, server or component. |
    | `[HostType, xmpp_element_out, ConnType, stanza_count]` | spiral | An XMPP stanza is sent to a client, server or component. |
    | `[HostType, xmpp_element_out, ConnType, iq_count]` | spiral | An IQ stanza is sent to a client, server or component. |
    | `[HostType, xmpp_element_out, ConnType, message_count]` | spiral | A message stanza is sent to a client, server or component. |
    | `[HostType, xmpp_element_out, ConnType, presence_count]` | spiral | A presence stanza is sent to a client, server or component. |
    | `[HostType, xmpp_element_out, ConnType, error_count]` | spiral | An error is sent to a client, server or component. |
    | `[HostType, xmpp_element_out, ConnType, iq_error_count]` | spiral | An IQ error is sent to a client, server or component. |
    | `[HostType, xmpp_element_out, ConnType, message_error_count]` | spiral | A message error is sent to a client, server or component. |
    | `[HostType, xmpp_element_out, ConnType, presence_error_count]` | spiral | A presence error is sent to a client, server or component. |
    | `[HostType, xmpp_element_out, ConnType, byte_size]` | histogram | Size of an XML element sent to a client. |

### Network data metrics

All metrics are in bytes, and refer to unencrypted data (before encryption or after decryption in case of TLS).
Some Prometheus metrics have the `connection_type` label, which can be `c2s` (for clients), `s2s` (for federated servers) or `component` (for external components).
Since Exometer doesn't support labels, `ConnType` is included in the metrics names.

=== "Prometheus"

    | Metric name | Labels | Type | Description |
    | ----------- | ------ | ---- | ----------- |
    | `tcp_data_in_byte_size` | `connection_type` | counter | Amount of data received from a client, another XMPP server or component via TCP channel. |
    | `tcp_data_out_byte_size` | `connection_type` | counter | Amount of data sent to a client, another XMPP server or component via TCP channel. |
    | `tls_data_in_byte_size` | `connection_type` | counter | Amount of data received from a client, another XMPP server or component via TLS channel. |
    | `tls_data_out_byte_size` | `connection_type` | counter | Amount of data sent to a client, another XMPP server or component via TLS channel. |
    | `mod_bosh_data_received_byte_size` | - | counter | Amount of data received from a client via BOSH connection. |
    | `mod_bosh_data_sent_byte_size` | - | counter | Amount of data sent to a client via BOSH connection. |
    | `mod_websocket_data_received_byte_size` | - | counter | Amount of data received from a client via WebSocket connection. |
    | `mod_websocket_data_sent_byte_size` | - | counter | Amount of data sent to a client via WebSocket connection. |

=== "Exometer"

    | Metric name | Type | Description |
    | ----------- | ---- | ----------- |
    | `[global, tcp_data_in, ConnType, byte_size]` | spiral | Amount of data received from a client, another XMPP server or component via TCP channel. |
    | `[global, tcp_data_out, ConnType, byte_size]` | spiral | Amount of data sent to a client, another XMPP server or component via TCP channel. |
    | `[global, tls_data_in, ConnType, byte_size]` | spiral | Amount of data received from a client, another XMPP server or component via TLS channel. |
    | `[global, tls_data_out, ConnType, byte_size]` | spiral | Amount of data sent to a client, another XMPP server or component via TLS channel. |
    | `[global, mod_bosh_data_received, byte_size]` | spiral | Amount of data received from a client via BOSH connection. |
    | `[global, mod_bosh_data_sent, byte_size]` | spiral | Amount of data sent to a client via BOSH connection. |
    | `[global, mod_websocket_data_received, byte_size]` | spiral | Amount of data received from a client via WebSocket connection. |
    | `[global, mod_websocket_data_sent, byte_size]` | spiral | Amount of data sent to a client via WebSocket connection. |

### CETS system metrics

All of these metrics are updated periodically. The interval at which they are probed can be configured with the [`instrumentation.probe_interval` option].

=== "Prometheus"

    | Metric name | Type | Description |
    | ----------- | ---- | ----------- |
    | `cets_info_available_nodes` | gauge | Available nodes (nodes that are connected to us and have the CETS disco process started). |
    | `cets_info_unavailable_nodes` | gauge | Unavailable nodes (nodes that do not respond to our pings). |
    | `cets_info_joined_nodes` | gauge | Joined nodes (nodes that have our local tables running). |
    | `cets_info_discovered_nodes` | gauge | Discovered nodes (nodes that are extracted from the discovery backend). |
    | `cets_info_remote_nodes_without_disco` | gauge | Nodes that have more tables registered than the local node. |
    | `cets_info_remote_nodes_with_unknown_tables` | gauge | Nodes with unknown tables. |
    | `cets_info_remote_unknown_tables` | gauge | Unknown remote tables. |
    | `cets_info_remote_nodes_with_missing_tables` | gauge | Nodes that are available, but do not host some of our local tables. |
    | `cets_info_remote_missing_tables` | gauge | Nodes that replicate at least one of our local tables to a different list of nodes. |
    | `cets_info_conflict_nodes` | gauge | Nodes that replicate at least one of our local tables to a different list of nodes. |
    | `cets_info_conflict_tables` | gauge | Tables that have conflicting replication destinations. |
    | `cets_info_discovery_works` | gauge | Returns 1 if the last discovery attempt is successful (otherwise returns 0). |

=== "Exometer"

    | Metric name | Type | Description |
    | ----------- | ---- | ----------- |
    | `[global, cets_info, available_nodes]` | gauge | Available nodes (nodes that are connected to us and have the CETS disco process started). |
    | `[global, cets_info, unavailable_nodes]` | gauge | Unavailable nodes (nodes that do not respond to our pings). |
    | `[global, cets_info, joined_nodes]` | gauge | Joined nodes (nodes that have our local tables running). |
    | `[global, cets_info, discovered_nodes]` | gauge | Discovered nodes (nodes that are extracted from the discovery backend). |
    | `[global, cets_info, remote_nodes_without_disco]` | gauge | Nodes that have more tables registered than the local node. |
    | `[global, cets_info, remote_nodes_with_unknown_tables]` | gauge | Nodes with unknown tables. |
    | `[global, cets_info, remote_unknown_tables]` | gauge | Unknown remote tables. |
    | `[global, cets_info, remote_nodes_with_missing_tables]` | gauge | Nodes that are available, but do not host some of our local tables. |
    | `[global, cets_info, remote_missing_tables]` | gauge | Nodes that replicate at least one of our local tables to a different list of nodes. |
    | `[global, cets_info, conflict_nodes]` | gauge | Nodes that replicate at least one of our local tables to a different list of nodes. |
    | `[global, cets_info, conflict_tables]` | gauge | Tables that have conflicting replication destinations. |
    | `[global, cets_info, discovery_works]` | gauge | Returns 1 if the last discovery attempt is successful (otherwise returns 0). |

### Pool metrics

For RDBMS global pool defined, an instance of these metrics are available.
Prometheus metrics have a `pool_tag` label associated with these metrics.
Since Exometer doesn't support labels, the tags are part of the metric names.

All of these metrics are updated periodically. The interval at which they are probed can be configured with the [`instrumentation.probe_interval` option].

=== "Prometheus"

    | Name | Type | Description |
    | ---- | ---- | ----------- |
    | `wpool_global_rdbms_stats_workers`   | gauge | Number of workers in the pool          |
    | `wpool_global_rdbms_stats_recv_oct`  | counter  | Number of bytes received               |
    | `wpool_global_rdbms_stats_recv_cnt`  | counter  | Number of packets received             |
    | `wpool_global_rdbms_stats_recv_max`  | gauge   | Size of the largest packet, in bytes   |
    | `wpool_global_rdbms_stats_send_oct`  | counter  | Number of bytes sent                   |
    | `wpool_global_rdbms_stats_send_max`  | gauge   | Size of the largest packet             |
    | `wpool_global_rdbms_stats_send_cnt`  | counter  | Number of packets sent                 |
    | `wpool_global_rdbms_stats_send_pend` | counter  | Number of bytes waiting to be sent     |

=== "Exometer"

    | Name | Type | Description |
    | ---- | ---- | ----------- |
    | `[global, wpool_global_rdbms_stats, PoolTag, workers]`   | counter | Number of workers in the pool          |
    | `[global, wpool_global_rdbms_stats, PoolTag, recv_oct]`  | spiral  | Number of bytes received               |
    | `[global, wpool_global_rdbms_stats, PoolTag, recv_cnt]`  | spiral  | Number of packets received             |
    | `[global, wpool_global_rdbms_stats, PoolTag, recv_max]`  | gauge   | Size of the largest packet, in bytes   |
    | `[global, wpool_global_rdbms_stats, PoolTag, send_oct]`  | spiral  | Number of bytes sent                   |
    | `[global, wpool_global_rdbms_stats, PoolTag, send_max]`  | gauge   | Size of the largest packet             |
    | `[global, wpool_global_rdbms_stats, PoolTag, send_cnt]`  | spiral  | Number of packets sent                 |
    | `[global, wpool_global_rdbms_stats, PoolTag, send_pend]` | spiral  | Number of bytes waiting to be sent     |

### VM metrics

=== "Prometheus"

    | Metric name | Type | Description |
    | ----------- | ---- | ----------- |
    | `system_memory_Metric` | gauge | Erlang memory statistics from [`erlang:memory/0`](https://www.erlang.org/doc/apps/erts/erlang.html#memory/0). `Metric` specifies the memory type, e.g. `total`, `processes_used`, `atom_used`, `binary`, `ets` or `system`. |
    | `system_info_Metric` | gauge | Erlang system statistics from [`erlang:system_info/1`](https://www.erlang.org/doc/apps/erts/erlang.html#system_info/1). `Metric` can be `port_count`, `port_limit`, `process_count`, `process_limit`, `ets_count` or `ets_limit`. |

=== "Exometer"

    | Metric name | Type | Description |
    | ----------- | ---- | ----------- |
    | `[global, system_memory, Metric]` | gauge | Erlang memory statistics from [`erlang:memory/0`](https://www.erlang.org/doc/apps/erts/erlang.html#memory/0). `Metric` specifies the memory type, e.g. `total`, `processes_used`, `atom_used`, `binary`, `ets` or `system`. |
    | `[global, system_info, Metric]` | gauge | Erlang system statistics from [`erlang:system_info/1`](https://www.erlang.org/doc/apps/erts/erlang.html#system_info/1). `Metric` can be `port_count`, `port_limit`, `process_count`, `process_limit`, `ets_count` or `ets_limit`. |

### Backend metrics

Some extension modules expose histograms with timings of calls made to their backends.
Please check the documentation of modules that are enabled in your config file, in order to learn if they provide them.

Prometheus metrics have a `host_type` label associated with these metrics, as well as a `function` label describing the backend action.
Since Exometer doesn't support labels, the host types and backend actions are part of the metric names.

=== "Prometheus"

    | Metric name | Type | Description |
    | ----------- | ---- | ----------- |
    | `BackendModule_count` | counter | Number of calls (spiral metric), incremented for *every* call (even a failed one). |
    | `BackendModule_time` | histogram | Times of successful operations. |

=== "Exometer"

    | Metric name | Type | Description |
    | ----------- | ---- | ----------- |
    | `[HostType, BackendModule, BackendAction, count]` | spiral | Number of calls (spiral metric), incremented for *every* call (even a failed one). |
    | `[HostType, BackendModule, BackendAction, time]` | histogram | Times of successful operations. |


Besides these, following authentication metrics are always available.
Prometheus metrics have a `host_type` label associated with these metrics.
Since Exometer doesn't support labels, the host types are part of the metric names.

=== "Prometheus"

    | Metric name | Type | Description |
    | ----------- | ---- | ----------- |
    | `auth_register_user_count` | counter | A user registered successfully. |
    | `auth_unregister_user_count` | counter | A user unregistered successfully. |
    | `auth_authorize_count` | counter | A user tried to authorize. |
    | `auth_authorize_time` | histogram | Time it took to authorize a user. |
    | `auth_check_password_count` | counter | A password was checked. |
    | `auth_check_password_time` | histogram | Time it took to check a password. |
    | `auth_try_register_count` | counter | A user tried to register. |
    | `auth_try_register_time` | histogram | Time it took to register a user. |
    | `auth_does_user_exist_count` | counter | Whether a user exists was checked. |
    | `auth_does_user_exist_time` | histogram | Time it took to check whether a user exists. |

=== "Exometer"

    | Metric name | Type | Description |
    | ----------- | ---- | ----------- |
    | `[HostType, auth_register_user, count]` | spiral | A user registered successfully. |
    | `[HostType, auth_unregister_user, count]` | spiral | A user unregistered successfully. |
    | `[HostType, auth_authorize, count]` | spiral | A user tried to authorize. |
    | `[HostType, auth_authorize, time]` | histogram | Time it took to authorize a user. |
    | `[HostType, auth_check_password, count]` | spiral | A password was checked. |
    | `[HostType, auth_check_password, time]` | histogram | Time it took to check a password. |
    | `[HostType, auth_try_register, count]` | spiral | A user tried to register. |
    | `[HostType, auth_try_register, time]` | histogram | Time it took to register a user. |
    | `[HostType, auth_does_user_exist, count]` | spiral | Whether a user exists was checked. |
    | `[HostType, auth_does_user_exist, time]` | histogram | Time it took to check whether a user exists. |

These are **total** times of respective operations.
One operation usually requires only a single call to an auth backend but sometimes with e.g. 3 backends configured, the operation may fail for first 2 backends.
In such case, these metrics will be updated with combined time of 2 failed and 1 successful request.
