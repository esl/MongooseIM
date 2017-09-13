### Module Description
This module implements [XEP-0357: Push Notifications](https://xmpp.org/extensions/xep-0357.html).
 It enables a service that notifies `PubSub` of a user's choice about every message that he could miss while being offline.
 There are two control stanzas that the client may send to this module: `enable` and `disable`.
 The `enable` stanza enables push notifications and forwards them to a specified `PubSub` node.
 This stanza may also contain an optional `Data Form` that will be added to each and every notification to `PubSub` node as `publish-options`.
 Please be sure to provide all form fields required by the specified `PubSub` node.
 Any publish error may result in disabling push notifications to this node.

### Options

* **backend** (atom, default: `mnesia`) - Backend to use for storing the registrations.
 Currently only `mnesia` may be used.
* **wpool** (list, default: `[]`) - List of options that will be passed to the `worker_pool` library that handles all the requests.
 Please refer to the [Project Site](https://github.com/inaka/worker_pool) for more details.
* **plugin_module** (atom, default: `mod_push_plugin_default`) - module implementing `mod_push_plugin` behaviour,
  used for dynamic configuration of push notifications. Read more about it [here](#plugin-module)

### Plugin module

A plugin module handles dynamic configuration of push notifications. It implements `mod_push_plugin` behaviour which
requires two callbacks:

* `should_publish/3` - callback used for filtering push notifications. Push notification is triggered for given message only if this
callback returns `true`.

```
-spec should_publish(From :: ejabberd:jid(), To :: ejabberd:jid(), Packet :: jlib:xmlel()) -> boolean().
```

* `sender_id/3` - allows to modify `last-message-sender` field, which ultimately becomes a `title` in the delivered push notification.

```
-spec sender_id(Host :: ejabberd:server(), From :: ejabberd:jid(), Packet :: jlib:xmlel()).
```

### Example configuration

```Erlang
{mod_push, [
    {backend, mnesia},
    {wpool, [{workers, 200}]},
    {plugin_module, mod_push_plugin_default}
]}.
```
