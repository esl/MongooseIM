### Module Description

This module is a backend of [mod_event_pusher] that enables support for the
RabbitMQ integration. Currently there are 5 available notifications:

* **user presence changed** - Carries the user id (full jid by default) and
a boolean field corresponding to the current user online status.
* **private message sent/received** - Carries the user ids (both sender and
receiver) along with the message body.
* **group message sent/received** - Carries the user id and the room id
(full jids by default) along with the message body.

All these notifications are sent as JSON strings to RabbitMQ topic exchanges.
Each type of the notifications is sent to it's dedicated exchange. There are
three exchanges created on startup of the module, for presences, private
messages and group chat messages related events.

Messages are published to a RabbitMQ server with routing key being set to a user
bare jid (`user@domain`) and configurable topic e.g `alice@localhost.private_message_sent`.

### Options

* **amqp_host** (charlist, default: `"localhost"`) - Defines RabbitMQ server host (domain or IP address);
* **amqp_port** (integer, default: `5672`) - Defines RabbitMQ server AMQP port;
* **amqp_username** (string, default: `<<"guest">>`) - Defines RabbitMQ server username;
* **amqp_password** (string, default: `<<"guest">>`) - Defines RabbitMQ server password;
* **presence_exchange** (string, default: `<<"presence">>`) - Defines RabbitMQ presence exchange name;
* **chat_msg_exchange** (string, default: `<<"chat_msg">>`) - Defines RabbitMQ chat message exchange name;
* **groupchat_msg_exchange** (string, default: `<<"groupchat_msg">>`) - Defines RabbitMQ group chat message exchange name;
* **chat_msg_sent_topic** (string, default: `<<"chat_msg_sent">>`) - Defines RabbitMQ chat message sent topic name;
* **chat_msg_recv_topic** (string, default: `<<"chat_msg_recv">>`) - Defines RabbitMQ chat message received topic name;
* **groupchat_msg_sent_topic** (string, default: `<<"groupchat_msg_sent">>`) - Defines RabbitMQ group chat message sent topic name;
* **groupchat_msg_recv_topic** (string, default: `<<"groupchat_msg_recv">>`) - Defines RabbitMQ group chat message received topic name;
* **pool_size** (integer, default: `100`) - Worker pool size for publishing notifications.

### Example configuration

```Erlang
{mod_event_pusher, [
    {backends, [
        {rabbbit, [
            {amqp_host, "localhost"},
            {amqp_port, 5672},
            {amqp_username, <<"guest">>},
            {amqp_password, <<"guest">>},
            {presence_exchange, <<"presence">>},
            {chat_msg_exchange, <<"chat_msg">>},
            {chat_msg_sent_topic, <<"chat_msg_sent">>},
            {chat_msg_recv_topic, <<"chat_msg_recv">>},
            {groupchat_msg_exchange, <<"groupchat_msg">>},
            {groupchat_msg_sent_topic, <<"groupchat_msg_sent">>},
            {groupchat_msg_recv_topic, <<"groupchat_msg_recv"},
            {pool_size, 50}
        ]}
    ]}
]}
```

## JSON Schema examples
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

[mod_event_pusher]: ./mod_event_pusher.md
