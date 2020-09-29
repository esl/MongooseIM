### Module Description

This module is a generic interface for event-pushing backends.
It defines a single callback, `push_event/3` that forwards the event to all registered backends.
Each backend decides how and if to handle the event in its `push_event/2` implementation.

The events are standardized as records that can be found in the `mod_event_pusher_events.hrl` file.
Common events like user presence changes (offline and online), chat and groupchat messages (incoming
and outgoing) are already hooked up to the frontend via `mod_event_pusher_hook_translator`, a
`mod_event_pusher` dependency, which is a proxy between various hooks and the `push_event/3` hook
handler.

### Options

#### `modules.mod_event_pusher.backend`
* **Syntax:** Array of TOML tables. See description.
* **Default:** see description
* **Example:** see description

Specifies backends to register with the frontend, along with arguments that will be passed to the backend.
Currently supported backends include [sns], [push], [http_notification] and [rabbit].
Refer to their specific documentation to learn more about their functions and configuration options.

### Example configuration

```
[modules.mod_event_pusher]
  backend.sns.access_key_id = "AKIAIOSFODNN7EXAMPLE"
  backend.sns.secret_access_key = "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY"
# ...

  backend.push.backend = "mnesia"
  backend.push.wpool.workers = 200
# ...
  
  backend.http.pool_name = "http_pool"
  backend.http.path = "/notifications"
# ...

  backend.rabbit.presence_exchange.name ="presence"
  backend.rabbit.presence_exchange.type = "topic"
# ...
```

[sns]: ./mod_event_pusher_sns.md
[push]: ./mod_event_pusher_push.md
[http_notification]: ./mod_event_pusher_http.md
[rabbit]: ./mod_event_pusher_rabbit.md