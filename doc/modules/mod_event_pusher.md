## Module Description

This module is a generic interface for pushing **events** to the configured **backends**.
The events include presence updates and incoming/outgoing messages.
Currently supported backends include [http], [push], [rabbit] and [sns].
Refer to their specific documentation to learn more about their functions and configuration options.

!!! Warning
    This module does not support [dynamic domains](../configuration/general.md#generalhost_types).

## Configuration

Each backend is configured in a corresponding subsection.
The example below enables all backends.
The `[modules.mod_event_pusher]` section itself is omitted - this is allowed in TOML, because the presence of a subsection implies that the corresponding parent section is also present.

!!! Note
    Some backends require configuring connection pools in the `outgoing_pools` section.
    See the detailed documentation for each backend.

```toml
[modules.mod_event_pusher.sns]
  presence_updates_topic = "user_presence_updated"
  pm_messages_topic = "user_message_sent"
  muc_messages_topic = "user_messagegroup_sent"
  sns_host = "eu-west-1.amazonaws.com"
  region = "eu-west-1"
  access_key_id = "AKIAIOSFODNN7EXAMPLE"
  secret_access_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
  account_id = "123456789012"

[modules.mod_event_pusher.push]
  wpool.workers = 200

[modules.mod_event_pusher.http]
  handlers = [{path = "/notifications"}]

[modules.mod_event_pusher.rabbit]
```

## How it works

The events are standardized as records that can be found in the `mod_event_pusher_events.hrl` file.
Common events like user presence changes (offline and online), chat and groupchat messages (incoming and outgoing) are handled in the `mod_event_pusher_hook_translator` module.
Each event has a corresponding [hook](../developers-guide/Hooks-and-handlers.md), e.g. `user_send_message` is run when a user sends a message.
`mod_event_pusher_hook_translator` has a handler function for each supported hook.

Handling an event includes the following steps:

1. The event hook is executed, and the corresponding handler function in `mod_event_pusher_hook_translator` is called.
1. The handler function calls `mod_event_pusher:push_event(Acc, Event)`.
1. `mod_event_pusher:push_event/2` runs another hook called `push_event`.
1. All configured backend modules have handlers for the `push_event` hook, and all these handlers are called.

### Custom event processing

By implementing your own module handling the `push_event` hook, you can:

- Push the events to a new service, such as a message queue or a database.
- Filter the events by returning `{ok, ...}` to keep an event, or `{stop, ...}` to drop it.
- Add a map with **metadata** to the events. The keys need to be atoms, and the values need to be atoms, binaries or numbers.

There is an example [mod_event_pusher_filter.erl](https://github.com/esl/MongooseIM/blob/master/big_tests/tests/mod_event_pusher_filter.erl) module, demonstrating how to filter the events and append additional metadata.

!!! Note
    Execution order of handlers depends on their priorities. In particular, filtering events or adding metadata needs to happend before pushing notifications to external services. The example handler has the priority value of 10, while backends have the priority of 50.

!!! Warning
    Currently only the [rabbit](mod_event_pusher_rabbit.md#additional-metadata) backend supports adding metadata to the published notifications.

[http]: ./mod_event_pusher_http.md
[push]: ./mod_event_pusher_push.md
[rabbit]: ./mod_event_pusher_rabbit.md
[sns]: ./mod_event_pusher_sns.md
