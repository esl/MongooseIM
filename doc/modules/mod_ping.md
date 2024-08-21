## Module Description

This module implements XMPP Ping functionality as described in [XEP-0199: XMPP Ping](http://www.xmpp.org/extensions/xep-0199.html).

## Options

### `modules.mod_ping.send_pings`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `send_pings = true`

If set to true, the server will send ping iqs to the client if they are not active for a `ping_interval`.

### `modules.mod_ping.ping_interval`
* **Syntax:** positive integer (seconds)
* **Default:** `60`
* **Example:** `ping_interval = 30`

Defines the client inactivity timeout after which the server will send a ping request if the above option is set to `true`.

### `modules.mod_ping.timeout_action`
* **Syntax:** string, one of `"none"`, `"kill"`
* **Default:** `"none"`
* **Example:** `timeout_action = "kill"`

Defines if the client connection should be closed if it doesn't reply to a ping request in less than `ping_req_timeout`.

### `modules.mod_ping.ping_req_timeout`
* **Syntax:** positive integer (seconds)
* **Default:** `32`
* **Example:** `ping_req_timeout = 60`

Defines how long the server waits for the client to reply to the ping request.

### `modules.mod_ping.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"no_queue"`

Strategy to handle incoming stanzas. For details, please refer to
[IQ processing policies](../configuration/Modules.md#iq-processing-policies).

## Example Configuration

```toml
[modules.mod_ping]
  send_pings = true
  ping_interval = 60
  timeout_action = "none"
  ping_req_timeout = 32
```

## Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

Prometheus metrics have a `host_type` label associated with these metrics.
Since Exometer doesn't support labels, the host types, or word `global`, are part of the metric names, depending on the [`instrumentation.exometer.all_metrics_are_global`](../configuration/instrumentation.md#instrumentationexometerall_metrics_are_global) option.

=== "Prometheus"

    | Name | Type | Description (when it gets incremented) |
    |------|------|----------------------------------------|
    | `mod_ping_response_count` | counter | Client responds to a ping. |
    | `mod_ping_response_time` | histogram | Response times (doesn't include timeouts). |
    | `mod_ping_response_timeout_count` | counter | Ping request timeouts without a response from client. |

=== "Exometer"

    | Name | Type | Description (when it gets incremented) |
    |------|------|----------------------------------------|
    | `[HostType, mod_ping_response, count]` | spiral | Client responds to a ping. |
    | `[HostType, mod_ping_response, time]` | histogram | Response times (doesn't include timeouts). |
    | `[HostType, mod_ping_response_timeout, count]` | spiral | Ping request timeouts without a response from client. |
