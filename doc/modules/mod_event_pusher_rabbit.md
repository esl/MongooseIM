## Module Description

This module is a backend of [mod_event_pusher] that enables support for the
RabbitMQ integration. Currently, there are 5 available notifications:

* **user presence changed** - Carries the user id (full jid by default) and
a boolean field corresponding to the current user online status.
* **private message sent/received** - Carries the user ids (both sender and
receiver) along with the message body.
* **group message sent/received** - Carries the user id and the room id
(full jids by default) along with the message body.

All these notifications are sent as JSON strings to RabbitMQ exchanges. Type
of exchanges can be chosen as desired. Each type of the notifications is sent
to its dedicated exchange. There are three exchanges created on startup of the
module, for presences, private messages and group chat messages related events.

Messages are published to a RabbitMQ server with routing key being set to a user
bare jid (`user@domain`) and configurable topic e.g `alice@localhost.private_message_sent`.

The module requires `rabbit` pool of AMQP connections to be configured in order
to make the module work. It's well advised to read through
[*Advanced configuration/Outgoing connections*](../configuration/outgoing-connections.md)
section before enabling the module.

## Presence options

To enable user presence notifications, you need to include the `presence_exchange` section in your configuration.
The specified exchange will be created automatically.

### `modules.mod_event_pusher.rabbit.presence_exchange.name`
* **Syntax:** non-empty string
* **Default:** `"presence"`
* **Example:** `name = "custom_presence_name"`

Defines RabbitMQ presence exchange name.

### `modules.mod_event_pusher.rabbit.presence_exchange.type`
* **Syntax:** non-empty string
* **Default:** `"topic"`
* **Example:** `type = "custom_presence_topic"`

Defines RabbitMQ presence exchange type.

### `modules.mod_event_pusher.rabbit.presence_exchange.durable`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `durable = true`

When set to `true`, the RabbitMQ exchange is created as durable; otherwise it is transient.

## Chat message options

To enable private chat message notifications, you need to include the `chat_msg_exchange` section in your configuration.
The specified exchange will be created automatically.

### `modules.mod_event_pusher.rabbit.chat_msg_exchange.name`
* **Syntax:** non-empty string
* **Default:** `"chat_msg"`
* **Example:** `name = "custom_msg_name"`

Defines RabbitMQ chat message exchange name.

### `modules.mod_event_pusher.rabbit.chat_msg_exchange.type`
* **Syntax:** non-empty string
* **Default:** `"topic"`
* **Example:** `type = "custom_msg_topic"`

Defines RabbitMQ chat message exchange type.

### `modules.mod_event_pusher.rabbit.presence_exchange.durable`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `durable = true`

When set to `true`, the RabbitMQ exchange is created as durable; otherwise it is transient.

### `modules.mod_event_pusher.rabbit.chat_msg_exchange.sent_topic`
* **Syntax:** non-empty string
* **Default:** `"chat_msg_sent"`
* **Example:** `sent_topic = "custom_sent_topic"`

Defines RabbitMQ chat message sent topic name.

### `modules.mod_event_pusher.rabbit.chat_msg_exchange.recv_topic`
* **Syntax:** non-empty string
* **Default:** `"chat_msg_recv"`
* **Example:** `recv_topic = "custom_recv_topic"`

Defines RabbitMQ chat message received topic name.

## Group chat message options

To enable group chat message notifications, you need to include the `groupchat_msg_exchange` section in your configuration.
The specified exchange will be created automatically.

### `modules.mod_event_pusher.rabbit.groupchat_msg_exchange.name`
* **Syntax:** non-empty string
* **Default:** `"groupchat_msg"`
* **Example:** `name = "custom_group_msg_name"`

Defines RabbitMQ group chat message exchange name.

### `modules.mod_event_pusher.rabbit.groupchat_msg_exchange.type`
* **Syntax:** non-empty string
* **Default:** `"topic"`
* **Example:** `type = "custom_group_msg_topic"`

Defines RabbitMQ group chat message exchange type.

### `modules.mod_event_pusher.rabbit.presence_exchange.durable`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `durable = true`

When set to `true`, the RabbitMQ exchange is created as durable; otherwise it is transient.

### `modules.mod_event_pusher.rabbit.groupchat_msg_exchange.sent_topic`
* **Syntax:** non-empty string
* **Default:** `"groupchat_msg_sent"`
* **Example:** `sent_topic = "custom_group_sent_topic"`

Defines RabbitMQ group chat message sent topic name.

### `modules.mod_event_pusher.rabbit.groupchat_msg_exchange.recv_topic`
* **Syntax:** non-empty string
* **Default:** `"groupchat_msg_recv"`
* **Example:** `recv_topic = "custom_group_recv_topic"`

Defines RabbitMQ group chat message received topic name.

## Example configuration

The following example enables all types of notifications with custom exchange and topic names:

```toml
[modules.mod_event_pusher.rabbit]
  presence_exchange.name = "presence"
  chat_msg_exchange.name = "chat_msg"
  chat_msg_exchange.sent_topic = "chat_msg_sent"
  chat_msg_exchange.recv_topic = "chat_msg_recv"
  groupchat_msg_exchange.name = "groupchat_msg"
  groupchat_msg_exchange.sent_topic = "groupchat_msg_sent"
  groupchat_msg_exchange.recv_topic = "groupchat_msg_recv"
```

This alternative version enables only the presence notifications, and uses default settings:

```toml
[modules.mod_event_pusher_rabbit.presence_exchange]
```

## JSON Schema examples

The different kinds of notifications deliver slightly different messages.
The messages are delivered in a JSON format.

### Presence updates

The JSON format for an online presence update notification is:
```JSON
{
    "user_id": "alice@localhost/res1",
    "present": true
}
```

For offline presence updates, the `present` boolean value is set to false:

```JSON
{
    "user_id": "alice@localhost/res1",
    "present": false
}
```

### Sent/received messages

The JSON format for a private message notification is:

```JSON
{
    "to_user_id": "bob@localhost/res1",
    "message": "Hello, Bob",
    "from_user_id": "alice@localhost/res1"
}
```

The notification is similar for group messages. For example for "sent" events:

```JSON
{
    "to_user_id": "muc_publish@muc.localhost",
    "message": "Hi, Everyone!",
    "from_user_id": "bob@localhost/res1"
}
```

and for "received" events:

```JSON
{
    "to_user_id": "bob@localhost/res1",
    "message": "Hi, Everyone!",
    "from_user_id": "muc_publish@muc.localhost/alice"
}
```

## Additional metadata

If you decide to [customize the events](mod_event_pusher.md#event-customization) with additional metadata, the additional key-value pairs will be added directly to the JSON object. You can override existing properties, but it is counter-intuitive and thus not recommended.

## Metrics

The module provides some metrics related to RabbitMQ connections and messages
as well.

Prometheus metrics have `host_type` and `pool_tag` labels associated with these metrics.
Since Exometer doesn't support labels, the pool tag, as well as the host types, or word `global`, are part of the metric names, depending on the [`instrumentation.exometer.all_metrics_are_global`](../configuration/instrumentation.md#instrumentationexometerall_metrics_are_global) option.

=== "Prometheus"

    | Name                                                             | Type      | Description (when it gets incremented/decremented)                                                                                               |
    |------------------------------------------------------------------|-----------|--------------------------------------------------------------------------------------------------------------------------------------------------|
    | `wpool_rabbit_connections_active`         | gauge   | A connection to a RabbitMQ server is opened(+1)/closed(-1).                                                                                      |
    | `wpool_rabbit_connections_opened`         | counter    | A connection to a RabbitMQ server is opened.                                                                                                     |
    | `wpool_rabbit_connections_closed`         | counter    | A connection to a RabbitMQ server is closed.                                                                                                     |
    | `wpool_rabbit_connections_failed`         | counter    | A try to open a connection to a RabbitMQ server failed.                                                                                          |
    | `wpool_rabbit_messages_published_count`   | counter    | A message to a RabbitMQ server is published.                                                                                                     |
    | `wpool_rabbit_messages_published_failed`  | counter    | A message to a RabbitMQ server is rejected.                                                                                                      |
    | `wpool_rabbit_messages_published_timeout` | counter    | A message to a RabbitMQ server timed out (weren't confirmed by the server).                                                                      |
    | `wpool_rabbit_messages_published_time`    | histogram | Amount of time it takes to publish a message to a RabbitMQ server and receive a confirmation. It's measured only for successful messages.        |
    | `wpool_rabbit_messages_published_size`    | histogram | Size of a message (in bytes) that was published to a RabbitMQ server (including message properties). It's measured only for successful messages. |

=== "Exometer"

    | Name                                                             | Type      | Description (when it gets incremented/decremented)                                                                                               |
    |------------------------------------------------------------------|-----------|--------------------------------------------------------------------------------------------------------------------------------------------------|
    | `[HostType, PoolTag, wpool_rabbit_connections_active]`         | counter   | A connection to a RabbitMQ server is opened(+1)/closed(-1).                                                                                      |
    | `[HostType, PoolTag, wpool_rabbit_connections_opened]`         | spiral    | A connection to a RabbitMQ server is opened.                                                                                                     |
    | `[HostType, PoolTag, wpool_rabbit_connections_closed]`         | spiral    | A connection to a RabbitMQ server is closed.                                                                                                     |
    | `[HostType, PoolTag, wpool_rabbit_connections_failed]`        | spiral    | A try to open a connection to a RabbitMQ server failed.                                                                                          |
    | `[HostType, PoolTag, wpool_rabbit_messages_published_count]`   | spiral    | A message to a RabbitMQ server is published.                                                                                                     |
    | `[HostType, PoolTag, wpool_rabbit_messages_published_failed]`  | spiral    | A message to a RabbitMQ server is rejected.                                                                                                      |
    | `[HostType, PoolTag, wpool_rabbit_messages_published_timeout]` | spiral    | A message to a RabbitMQ server timed out (weren't confirmed by the server).                                                                      |
    | `[HostType, PoolTag, wpool_rabbit_messages_published_time]`    | histogram | Amount of time it takes to publish a message to a RabbitMQ server and receive a confirmation. It's measured only for successful messages.        |
    | `[HostType, PoolTag, wpool_rabbit_messages_published_size]`    | histogram | Size of a message (in bytes) that was published to a RabbitMQ server (including message properties). It's measured only for successful messages. |


## Guarantees

There are no guarantees. The current implementation uses "best effort" approach
which means that we don't care if a message is delivered to a RabbitMQ server.
If [`publisher confirms`](#publisher-confirms) are enabled and a message
couldn't be delivered to the server for some reason (the server sent negative
acknowledgment/didn't sent it at all or there was a channel exception)
the module just updates appropriate metrics and prints some log messages. Notice
that there might be situations when a message silently gets lost.

## Type of exchanges

By default all the exchanges used are of type `topic`. Using topic exchanges
gives a lot of flexibility when binding queues to such an exchange by using
`#` and `*` in binding keys. But flexibility comes at the cost of performance -
imagine a scenario where there are thousands of users and AMQP consumers use
binding keys for particular users which look like `user_N@host.#`. In such
case RabbitMQ has to go through all the users in order to find out where
a message should be sent to. This operations is proved to be costly. In a load
test with 100k users a delay caused by this operation was substantial (about an
order of magnitude higher than compared to a load test with 60k users).

If performance is a top priority go for `direct` exchanges. Using this type of
exchanges is proved to work efficiently with 100k users. Keep in mind it gives
up flexibility over performance.

## Publisher confirms

By default publisher confirmations are disabled. However, one-to-one
confirmations can be enabled (see
[*RabbitMQ connection setup*](../configuration/outgoing-connections.md#rabbitmq-options)
section). When a worker sends a message to a RabbitMQ server it waits for a
confirmation from the server before it starts to process next message. This
approach allows to introduce backpressure on a RabbitMQ server connection cause
the server can reject/not confirm messages when it's overloaded. On the other
hand it can cause performance degradation.

## Worker selection strategy

The module uses `mongoose_wpool` for managing worker processes  and `best_worker`
strategy, for choosing a worker, is in use by default. Different strategies
imply different behaviors of the system.

### Event messages queuing

When `available_worker` strategy is in use all the event messages are queued in
single worker pool manager process state. When different strategy is set e.g.
`best_worker` those messages are placed in worker processes inboxes. Worker
selection strategy can be set in `rabbit` pool configuration.

### Event messages ordering

None of worker selection strategies ensures that user events will be delivered to
a RabbitMQ server properly ordered in time.

[mod_event_pusher]: ./mod_event_pusher.md
