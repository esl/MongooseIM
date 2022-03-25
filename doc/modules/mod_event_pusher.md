## Module Description

This module is a generic interface for event-pushing backends.
It defines a single callback, `push_event/2` that forwards the event to all registered backends.
Each backend decides how and if to handle the event in its `push_event/2` implementation.

Currently supported backends include [http], [push], [rabbit] and [sns].
Refer to their specific documentation to learn more about their functions and configuration options.

### How it works

The events are standardized as records that can be found in the `mod_event_pusher_events.hrl` file.
Common events like user presence changes (offline and online), chat and groupchat messages (incoming
and outgoing) are already handled in the `mod_event_pusher_hook_translator` module, which is a proxy between various hooks and the `push_event/2` handler.

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

[http]: ./mod_event_pusher_http.md
[push]: ./mod_event_pusher_push.md
[rabbit]: ./mod_event_pusher_rabbit.md
[sns]: ./mod_event_pusher_sns.md
