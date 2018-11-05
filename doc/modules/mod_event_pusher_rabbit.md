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
to it's dedicated exchange. There are three exchanges created on startup of the
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

  * `connections_active` - number of active connections to a RabbitMQ
  server
  * `connections_opened` - number of opened connections to a RabbitMQ
  server since module startup
  * `connections_closed` - number of closed connections to a RabbitMQ
  server since module startup
  * `connections_failed` - number of failed connections to a RabbitMQ
  server since module startup
  * `messages_published` - number of published messages to a RabbitMQ server
  since module startup
  * `messages_timeout` - number of messages to a RabbitMQ server that weren't
  confirmed by the server since module startup
  * `messages_failed` - number of messages to a RabbitMQ server that were
  rejected by the server since module startup
  * `message_publish_time` - amount of time it takes to publish a message to
  a RabbitMQ server and receives a confirmation
  * `message_payload_size` - size of a messages (in bytes) that was published to
  a RabbitMQ server

> All the above metrics have a prefix which looks as follows:  
> `mongooseim.<xmpp_host>.backends.mod_event_pusher_rabbit.<metric_name>`.
> For example a proper metric name would look like:
> `mongooseim.localhost.backends.mod_event_pusher_rabbit.connections_active`

### Current status

This module is still in an experimental phase.

### Guarantees

There are no guarantees. The current implementation uses "best effort" approach
which means that we don't care if a message is delivered to a RabbitMQ server.
If a message couldn't be delivered to the server for any reason the module
just updates appropriate metrics and print some log messages.

### Type of exchanges

By default all the exchanges used are of type `topic`. Using topic exchanges
gives a lot of flexibility when binding queues to such an exchange by using
`#` and `*` in binding keys. But flexibility comes at the cost of performance -
imagine a scenario where there are thousands of users and AMQP consumers use
bindig keys for particular users which looks like `user_N@host.#`. In such
case RabbitMQ has to go through all the users in order to find out where
a message should be sent to. This operations is proved to be costly. In a load
test with 100k users a delay caused by this operation was substantial (about an
order of magnitue higher than compared to a load test with 60k users).

If perfromance is a top priority go for `direct` exchanges. Using this type of
exchanges is proved to work efficiently with 100k users. Keep in mind it gives
up flexibility over perfromance.

### Publisher confirms

By default publisher confirmations are disabled. However, one-to-one
confirmations can be enabled (see
[*RabbitMQ connection setup*](../advanced-configuration/outgoing-connections.md#rabbitmq-connection-setup)
section). When a worker sends a message to a RabbitMQ server it waits for a
confirmation from the server before it starts to process next message. This
approach allows to introduce backpressure on a RabbitMQ server connection cause
the server can reject messages when it's overloaded. On the other hand it can
cause performance degratation.

### Worker selection strategy

The module uses `mongoose_wpool` for managing worker processes  and `avaiable_worker`
strategy is in use. Different strategies imply different behaviours of the system.

#### Event messages queueing

When `available_worker` strategy is set all the event messages are queued in
single worker pool manager process state. When diffrenet strategy is set e.g
`next_worker` those messages are placed in worker processes inboxes. There is no
possiblity to change the worker strategy for now.

#### Event messages ordering

`available_worker` strategy does not ensure that user events will be delivered to
a RabbitMQ server properly ordered in time.

[mod_event_pusher]: ./mod_event_pusher.md
