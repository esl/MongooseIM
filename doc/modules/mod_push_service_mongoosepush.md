# mod_push_service_mongoosepush

## Module Description

This module handles the `push_notifications` hook generated either by `mod_pubsub_old` with an active `push` node, or by `mod_event_pusher_push` module's virtual pubsub host.
Each notification is sent to [MongoosePush](https://github.com/esl/MongoosePush) as a `POST /<api_version>/notification/<device_id>` request.

### Request parameters

Below is a summary of all parameters used when sending requests to MongoosePush.
See the [MongoosePush request documentation](https://esl.github.io/MongoosePush/v2.3.0/http_api.html#request) for their detailed meaning and default values.
The listed publish options are set when [enabling push notifications](../tutorials/push-notifications/Push-notifications-client-side.md#enabling-push-notifications).

| MongoosePush parameter | Source | Description |
|---|---|---|
| `<api_version>` | `api_version` module option | Selects the MongoosePush HTTP API version; see the [module option](#modulesmod_push_service_mongoosepushapi_version) |
| `<device_id>` | Mandatory `device_id` publish option | Identifies the target device using its FCM or APNs [device token](../tutorials/push-notifications/Push-notifications-client-side.md#registering-with-a-push-service-provider) |
| `service` | Mandatory `service` publish option | Selects FCM or APNs as the delivery provider |
| `mode` | `mode` publish option | Selects a named MongoosePush connection pool |
| `priority` | `priority` publish option | Sets the delivery priority for the provider |
| `topic` | `topic` publish option | Sets the APNs `apns-topic` request header |
| `mutable_content` | `mutable_content` publish option | Requests APNs notification service extension processing |
| `time_to_live` | `time_to_live` publish option | Limits how long FCM may retain the notification, in seconds |

For message notifications, the remaining parameters depend on the notification type, determined by the `silent` publish option.
By default, it is set to `false`, resulting in `alert` notifications with the following parameters:

| MongoosePush parameter | Source | Description |
|---|---|---|
| `alert.body` | Message `<body>` | Sets the visible notification text |
| `alert.title` | Message sender JID | Sets the visible notification title |
| `alert.tag` | Message sender JID | Groups or replaces related FCM notifications |
| `alert.badge` | Unread message count | Sets the APNs app icon badge count |
| `alert.click_action` | `click_action` publish option | Selects the FCM activity or APNs category used on interaction |
| `alert.sound` | `sound` publish option | Selects the sound played when the notification arrives |

When `silent` is set to `true`, MongooseIM sends silent (data-only) notifications
with the parameters defined in [XEP-0357: Push Notifications][XEP-0357]:

| MongoosePush parameter | Source | Description |
|---|---|---|
| `data.last-message-body` | Message `<body>` | Carries the message text as custom data |
| `data.last-message-sender` | Message sender JID | Carries the sender JID as custom data |
| `data.message-count` | Unread message count | Carries the unread message count as custom data |

For [Jingle Message Initiation][XEP-0353] (JMI) notifications triggered by the [`content = "jingle"`](./mod_event_pusher_push.md#modulesmod_event_pusherpushrulescontent) rule, regardless of the `silent` option,
MongooseIM sends requests containing `data` and no `alert`, with the following parameters:

| MongoosePush parameter | Source | Description |
|---|---|---|
| `data.type` | Constant `jmi` | Marks the notification as JMI |
| `data.jmi-sid` | JMI element `id` | Identifies the Jingle session |
| `data.jmi-from` | Full message sender JID | Identifies the caller by full JID |

!!! Note
    JMI notifications require MongoosePush 2.3.0 or later.
    See [JMI call notifications](https://esl.github.io/MongoosePush/v2.3.0/http_api.html#jmi-call-notifications)
    for provider-specific delivery details.

## Prerequisites

This module uses a connection pool via `mongoose_http_client`.
It must be defined in [outgoing_pools setting](../configuration/outgoing-connections.md#http-options).

## Options

### `modules.mod_push_service_mongoosepush.pool_name`
* **Syntax:** non-empty string
* **Default:** `"undefined"`
* **Example:** `pool_name = "mongoose_push_http"`

The name of the pool to use (as defined in [`outgoing_pools`](../configuration/outgoing-connections.md)).

### `modules.mod_push_service_mongoosepush.api_version`
* **Syntax:** string, `"v2"` or `"v3"`
* **Default:** `"v3"`
* **Example:** `api_version = "v3"`

REST API version to be used.

### `modules.mod_push_service_mongoosepush.async_sender.enabled`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `modules.mod_push_service_mongoosepush.async_sender.enabled = false`

Enables asynchronous, batched push notification delivery. When disabled, push requests are sent synchronously, which may increase message processing time.

### `modules.mod_push_service_mongoosepush.async_sender.flush_interval`
* **Syntax:** non-negative integer
* **Default:** `2000`
* **Example:** `modules.mod_push_service_mongoosepush.async_sender.flush_interval = 1000`

How often (in milliseconds) the buffered push notifications are sent.

### `modules.mod_push_service_mongoosepush.async_sender.batch_size`
* **Syntax:** non-negative integer
* **Default:** `30`
* **Example:** `modules.mod_push_service_mongoosepush.async_sender.batch_size = 20`

Maximum number of messages in a batch before the queue is considered full and push notifications are sent. When the buffer is full, push notifications are sent immediately and the flush timer is reset.

### `modules.mod_push_service_mongoosepush.async_sender.pool_size`
* **Syntax:** non-negative integer
* **Default:** `4 * erlang:system_info(schedulers_online)`
* **Example:** `modules.mod_push_service_mongoosepush.async_sender.pool_size = 32`

Number of workers in the pool. Increasing the number of workers can improve throughput by allowing more messages to be processed concurrently. The optimal pool size depends on the deployment and can be fine-tuned to achieve the best performance.

## Example configuration

```toml
[outgoing_pools.http.mongoose_push_http]
  scope = "global"
  workers = 50

  [outgoing_pools.http.mongoose_push_http.connection]
    host = "https://localhost:8443"
    path_prefix = "/"
    request_timeout = 2000

[modules.mod_push_service_mongoosepush]
  pool_name = "mongoose_push_http"
  api_version = "v3"

  [modules.mod_push_service_mongoosepush.async_sender]
    flush_interval = 2000
    batch_size = 30
    pool_size = 100
```

## Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

Prometheus metrics have a `host_type` label associated with these metrics.
Since Exometer doesn't support labels, the host types, or word `global`, are part of the metric names, depending on the [`instrumentation.exometer.all_metrics_are_global`](../configuration/instrumentation.md#instrumentationexometerall_metrics_are_global) option.

=== "Prometheus"

    | Name | Type | Description |
    | ---- | ---- | ----------- |
    | `mod_push_service_mongoosepush_sent_success_count` | counter | MongoosePush HTTP notification requests completed successfully. |
    | `mod_push_service_mongoosepush_sent_failed_count` | counter | MongoosePush HTTP notification requests that failed. |
    | `mod_push_service_mongoosepush_sent_time` | histogram | Time spent on each MongoosePush HTTP notification request. |

=== "Exometer"

    | Name | Type | Description |
    | ---- | ---- | ----------- |
    | `[Host, mod_push_service_mongoosepush_sent, success_count]` | spiral | MongoosePush HTTP notification requests completed successfully. |
    | `[Host, mod_push_service_mongoosepush_sent, failed_count]` | spiral | MongoosePush HTTP notification requests that failed. |
    | `[Host, mod_push_service_mongoosepush_sent, time]` | histogram | Time spent on each MongoosePush HTTP notification request. |

[XEP-0353]: https://xmpp.org/extensions/xep-0353.html
[XEP-0357]: https://xmpp.org/extensions/xep-0357.html
