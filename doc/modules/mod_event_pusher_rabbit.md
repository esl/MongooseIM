### Current status

**This module is still in an experimental phase.**

### Module Description

This module is a backend of [mod_event_pusher] that enables support for the
RabbitMQ integration. Currently there are 5 available notifications:

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
[*Advanced configuration/Outgoing connections*](../advanced-configuration/outgoing-connections.md)
section before enabling the module.

### Options

* **presence_exchange** - Defines presence exchange options, such as:
  * `name` - (string, default: `<<"presence">>`) - Defines RabbitMQ presence exchange name;
  * `type` (string, default: `<<"topic">>`) - Defines RabbitMQ presence exchange type;
* **chat_msg_exchange** - Defines chat message exchange options, such as:
  * `name` - (string, default: `<<"chat_msg">>`) - Defines RabbitMQ chat message exchange name;
  * `type` (string, default: `<<"topic">>`) - Defines RabbitMQ chat message exchange type;
  * `sent_topic` - (string, default: `<<"chat_msg_sent">>`) - Defines RabbitMQ chat message sent topic name;
  * `recv_topic` - (string, default: `<<"chat_msg_recv">>`) - Defines RabbitMQ chat message received topic name;
* **groupchat_msg_exchange** - Defines group chat message exchange options, such as:
  * `name` - (string, default: `<<"groupchat_msg">>`) - Defines RabbitMQ group chat message exchange name;
  * `type` (string, default: `<<"topic">>`) - Defines RabbitMQ group chat message exchange type;
  * `sent_topic` (string, default: `<<"groupchat_msg_sent">>`) - Defines RabbitMQ group chat message sent topic name;
  * `recv_topic` (string, default: `<<"groupchat_msg_recv">>`) - Defines RabbitMQ group chat message received topic name;

### Example configuration

```Erlang
{mod_event_pusher, [
    {backends, [
        {rabbit, [
            {presence_exchange, [{name, <<"presence">>},
                                 {type, <<"topic">>}]},
            {chat_msg_exchange, [{name, <<"chat_msg">>},
                                 {sent_topic, <<"chat_msg_sent">>},
                                 {recv_topic, <<"chat_msg_recv">>}]},
            {groupchat_msg_exchange, [{name, <<"groupchat_msg">>},
                                      {sent_topic, <<"groupchat_msg_sent">>},
                                      {recv_topic, <<"groupchat_msg_recv">>}]}
        ]}
    ]}
]}
```

### JSON Schema examples
The different kinds of notifications deliver slightly different messages.
The messages are delivered in a JSON format.
#### Presence updates

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
#### Sent/received messages
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

### Metrics

The module provides some metrics related to RabbitMQ connections and messages
as well. Provided metrics:

| name                             | type      | description (when it gets incremented/decremented)                                                                                               |
| ----                             | ----      | --------------------------------------                                                                                                           |
| [`Host`, `connections_active`]   | spiral    | A connection to a RabbitMQ server is opened(+1)/closed(-1).                                                                                      |
| [`Host`, `connections_opened`]   | spiral    | A connection to a RabbitMQ server is opened.                                                                                                     |
| [`Host`, `connections_closed`]   | spiral    | A connection to a RabbitMQ server is closed.                                                                                                     |
| [`Host`, `connection_failed` ]   | spiral    | A try to open a connection to a RabbitMQ server failed.                                                                                          |
| [`Host`, `messages_published`]   | spiral    | A message to a RabbitMQ server is published.                                                                                                     |
| [`Host`, `messages_failed`]      | spiral    | A message to a RabbitMQ server is rejected.                                                                                                      |
| [`Host`, `messages_timeout`]     | spiral    | A message to a RabbitMQ server timed out (weren't confirmed by the server).                                                                      |
| [`Host`, `message_publish_time`] | histogram | Amount of time it takes to publish a message to a RabbitMQ server and receive a confirmation. It's measured only for successful messages.        |
| [`Host`, `message_payload_size`] | histogram | Size of a message (in bytes) that was published to a RabbitMQ server (including message properties). It's measured only for successful messages. |

> All the above metrics have a prefix which looks as follows:  
> `<xmpp_host>.backends.mod_event_pusher_rabbit.<metric_name>`.
> For example a proper metric name would look like:
> `localhost.backends.mod_event_pusher_rabbit.connections_active`

### Guarantees

There are no guarantees. The current implementation uses "best effort" approach
which means that we don't care if a message is delivered to a RabbitMQ server.
If [`publisher confirms`](#publisher-confirms) are enabled and a message
couldn't be delivered to the server for some reason (the server sent negative
acknowledgment/didn't sent it at all or there was a channel exception)
the module just updates appropriate metrics and prints some log messages. Notice
that there might be situations when a message silently gets lost.

### Type of exchanges

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

### Publisher confirms

By default publisher confirmations are disabled. However, one-to-one
confirmations can be enabled (see
[*RabbitMQ connection setup*](../advanced-configuration/outgoing-connections.md#rabbitmq-connection-setup)
section). When a worker sends a message to a RabbitMQ server it waits for a
confirmation from the server before it starts to process next message. This
approach allows to introduce backpressure on a RabbitMQ server connection cause
the server can reject/not confirm messages when it's overloaded. On the other
hand it can cause performance degradation.

### Worker selection strategy

The module uses `mongoose_wpool` for managing worker processes  and `best_worker`
strategy, for choosing a worker, is in use by default. Different strategies
imply different behaviors of the system.

#### Event messages queuing

When `available_worker` strategy is in use all the event messages are queued in
single worker pool manager process state. When different strategy is set e.g
`best_worker` those messages are placed in worker processes inboxes. Worker
selection strategy can be set in `rabbit` pool configuration.

#### Event messages ordering

None of worker selection strategies ensures that user events will be delivered to
a RabbitMQ server properly ordered in time.

[mod_event_pusher]: ./mod_event_pusher.md
