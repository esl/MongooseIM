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

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| ``[HostType, mod_ping, ping_response]`` | spiral | Client responds to a ping. |
| ``[HostType, mod_ping, ping_response_timeout]`` | spiral | Ping request timeouts without a response from client. |
| ``[HostType, mod_ping, ping_response_time]`` | histogram | Response times (doesn't include timeouts). |
