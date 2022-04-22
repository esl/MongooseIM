## Module Description

This module is a backend for [mod_event_pusher][] that implements
[XEP-0357: Push Notifications][XEP-0357].
It provides push notification data to the service that delivers actual notifications
to a client device.

We've prepared [a detailed tutorial][tutorial] for a proper push
notifications setup on both client and server side.

Please make sure that clients provide all form fields required by the specified `PubSub` node.
Some publish errors may result in disabling push notifications for the specific device until it
attempts to enable them again.

This module is very easy to enable, just paste the following to your MongooseIM configuration file:

```toml
[modules.mod_event_pusher]
  push.wpool.workers = 100
```

And that's basically it. You have just enabled the push notification support
with 100 asynchronous workers that will handle all push notification related work.

## Options

### `modules.mod_event_pusher.push.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"one_queue"`

Strategy to handle incoming stanzas. For details, please refer to
[IQ processing policies](../configuration/Modules.md#iq-processing-policies).

### `modules.mod_event_pusher.push.backend`
* **Syntax:** string, one of `"mnesia"`, `"rdbms"`
* **Default:** `"mnesia"`
* **Example:** `backend = "rdbms"`

Backend to use for storing the registrations.

### `modules.mod_event_pusher.push.wpool`
* **Syntax:** TOML table with worker pool options
* **Default:** see description
* **Example:** `wpool.workers = 200`

Pool options that will be passed to the `worker_pool` library that handles all the requests. The options allowed here are the same as for the [outgoing connection pools](../configuration/outgoing-connections.md#worker-pool-options). The only difference is that the default `strategy` is `"available_worker"`.

### `modules.mod_event_pusher.push.plugin_module`
* **Syntax:** non-empty string
* **Default:** `"mod_event_pusher_push_plugin_defaults"`
* **Example:** `plugin_module = "mod_event_pusher_push_plugin_defaults"`

The module implementing `mod_event_pusher_push_plugin` behaviour, used for dynamic configuration of push notifications.
See the [relevant section](#plugin-module) for more details.

### `modules.mod_event_pusher.push.virtual_pubsub_hosts`
* **Syntax:** array of strings
* **Default:** `[]`
* **Example:** `virtual_pubsub_hosts = ["host1", "host2"]`

The list of "simulated" Publish-Subscribe domains. You may use the `@HOST@` pattern in the domain name.
It will automatically be replaced by a respective XMPP domain (e.g. `localhost`).
See the [relevant section](#virtual-pubsub-hosts) for more details.

## Virtual PubSub hosts

If a notification is published to one of the configured domains, the internal push notification hook
is executed in MongooseIM instead of the XEP-0357 typical behaviour. If an existing PubSub domain
is added to this list, it will be shadowed in the push notifications context. To ensure complete
shadowing of all the PubSub subdomains you must use the `@HOST@` pattern, otherwise only the
subdomain of the user is shadowed. It enables easy migration from PubSub-full deployments to
PubSub-less variants.

### Migration from XEP-0357 to virtual hosts

This is an example of how you can migrate the existing setup to the new model. PubSub service still
exists, just for the case of a user attempting to create a node. However, its domain is overridden
for the purpose of sending push notifications. Please note the value of `virtual_pubsub_hosts`
option. `"pubsub.@HOST@"` is the default domain for `mod_pubsub`.

```toml
[modules.mod_pubsub]
  plugins = ["push"] # mandatory minimal config

[modules.mod_event_pusher.push]
  backend = "mnesia" # optional
  wpool.workers = 200 # optional
  plugin_module = "mod_event_pusher_push_plugin_defaults" # optional
  virtual_pubsub_hosts = ["pubsub.@HOST@"]
```

#### Advantages
* Versatility: PubSub-less and PubSub-full mechanisms can be configured with different domains and
  therefore give fine-grained control over the push notification handling
* Takes advantage of the PubSub-less efficiency when told to do so
* Fully compliant with [XEP-0357: Push Notifications][XEP-0357] and therefore with most 3rd party client libraries
* Ideal for migrations to PubSub-less deployments.

#### Drawbacks
* More complex configuration on the server side
* Pays the PubSub performance penalty when the PubSub path is taken


## Plugin module

You can also control the format of the "sender" of the push notification (which ultimately becomes
the title of push notification) and filter which messages will trigger the notification.
In order to achieve that, you need to create a plugin module that implements the
`mod_event_pusher_push_plugin` behaviour and enable this plugin in the `plugin_module` section as
above.

A plugin module handles the dynamic configuration of push notifications. 
It contains the filtering and custom logic for notifying about messages.

Two plugin implementations are provided.
They offer different behaviour considering unacknowledged messages when using [XEP-0198: Stream Management][XEP-0198]:


* `mod_event_pusher_push_plugin_defaults`, which implements an older behaviour. It does not notify
  the user of unacknowledged messages immediately after detecting a lost connection to the user.
* `mod_event_pusher_push_plugin_enhanced`, which pushes notifications as soon as the server detects
  that the client has disconnected and waits for stream resumption (by an `unack_msg_event` event
  generated by the `unacknowledged_message` hook). This immediate notification prevents the unneeded
  suspension of the client's application, if there are no unacknowledged messages yet. This allows
  to create more power efficient mobile applications.

In order for the enhanced plugin to work, each device (an entity that may receive push
notifications) should be uniquely identified. The only correct way to identify a device from the
XMPP standpoint is to use the data provided with the [enable stanza][enabling]. Because of that,
each device should (re)enable the push notifications at the beginning of each and every connection.

### Custom plugins

A custom module implementing the optional callbacks of `mod_event_pusher_push_plugin`
may be used as a plugin to change the default behaviour. In the case of not implemented callbacks
the defaults are used instead.

[mod_event_pusher]: ./mod_event_pusher.md
[XEP-0198]: https://xmpp.org/extensions/xep-0198.html
[enabling]: https://xmpp.org/extensions/xep-0357.html#enabling
[tutorial]: ../tutorials/push-notifications/Push-notifications.md
[XEP-0357]: https://xmpp.org/extensions/xep-0357.html
