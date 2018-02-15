### Module Description

This module is a backend of [mod_event_pusher] that implements [XEP-0357: Push Notifications](https://xmpp.org/extensions/xep-0357.html).
It enables a service that notifies `PubSub` of a user's choice about every message that they could miss while being offline.
There are two control stanzas that the client can send to this module: `enable` and `disable`.
The `enable` stanza enables push notifications and forwards them to a specified `PubSub` node.
This stanza may also contain an optional `Data Form` that will be added to each and every notification to `PubSub` node as `publish-options`.
Please be sure to provide all form fields required by the specified `PubSub` node.
Any publish error may result in disabling push notifications to this node.

### Options

* **backend** (atom, default: `mnesia`) - Backend to use for storing the registrations.
 Currently only `mnesia` may be used.
* **wpool** (list, default: `[]`) - List of options that will be passed to the `worker_pool` library that handles all the requests.
 Please refer to the [Project Site](https://github.com/inaka/worker_pool) for more details.
* **plugin_module** (atom, default: `mod_event_pusher_push_plugin_defaults`) - module implementing `mod_event_pusher_push_plugin` behaviour,
  used for dynamic configuration of push notifications. Read more about it [here](#plugin-module)

### Plugin module

A plugin module handles dynamic configuration of push notifications. It implements `mod_event_pusher_push_plugin` behaviour which
requires two callbacks:

* `should_publish/3` - callback used for filtering push notifications. A push notification is triggered for given a message only if this
callback returns `true`.

```
-spec should_publish(From :: ejabberd:jid(), To :: ejabberd:jid(), Packet :: jlib:xmlel()) -> boolean().
```

* `sender_id/2` - allows modifying `last-message-sender` field, which ultimately becomes a `title` of the delivered push notification.

```
-spec sender_id(From :: ejabberd:jid(), Packet :: jlib:xmlel()) -> SenderId :: binary().
```

### Example configuration

```Erlang
{mod_event_pusher, [
    {backends, [
        {push, [
            {backend, mnesia},
            {wpool, [{workers, 200}]},
            {plugin_module, mod_event_pusher_push_plugin_defaults}
        ]}
    ]}
]}
```

[mod_event_pusher]: ./mod_event_pusher.md
