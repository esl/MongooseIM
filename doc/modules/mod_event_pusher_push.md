# Push backend

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

!!! Tip
    For most cases, it is recommended to use [rules](#modulesmod_event_pusherpushrules) instead of plugins as they provide a higher level of configurability and flexibility.

### `modules.mod_event_pusher.push.virtual_pubsub_hosts`
* **Syntax:** array of strings
* **Default:** `[]`
* **Example:** `virtual_pubsub_hosts = ["host1", "host2"]`

The list of "simulated" Publish-Subscribe domains. You may use the `@HOST@` pattern in the domain name.
It will automatically be replaced by a respective XMPP domain (e.g. `localhost`).
See the [relevant section](#virtual-pubsub-hosts) for more details.

### `modules.mod_event_pusher.push.rules`
* **Syntax:** array of TOML tables
* **Default:** not set

An ordered list of rules controlling which push notifications are sent and what content they have.
Each rule consists of a list of [conditions](#modulesmod_event_pusherpushrulesconditions), an [action](#modulesmod_event_pusherpushrulesaction), and, when the action is `"push"`, [content](#modulesmod_event_pusherpushrulescontent) specification.

Rules are checked in order, and the first matching rule determines the action. No subsequent rules
are checked. If no rule matches, the notification is skipped.

!!! Warning
    Rules and plugins are mutually exclusive.
    Make sure you don't specify `rules` and [`plugin_module`](#modulesmod_event_pusherpushplugin_module) together.

#### `modules.mod_event_pusher.push.rules.conditions`
* **Syntax:** non-empty array of non-empty TOML tables
* **Default:** not set

Specifies when the rule matches with a list of alternative condition tables.
All fields within one table must match, while the tables are alternatives, so any one matching table is sufficient.
Omitting `conditions` makes the rule match unconditionally.

The following condition fields are available:

| Condition | Values | Meaning |
| --- | --- | --- |
| `event` | `"msg"`, `"unack_msg"` | `"msg"` is generated while a message is routed to the recipient. `"unack_msg"` is generated for each unacknowledged message when its [XEP-0198: Stream Management][XEP-0198] connection closes and enters the resumption state. |
| `type` | `"chat"`, `"groupchat"` | The XMPP message type. |
| `body` | `"absent"`, `"empty"`, `"non_empty"` | The state of the message `<body>` element. |
| `hint` | `"no_store"`, `"store"` | Presence of the corresponding [XEP-0334: Message Processing Hints][XEP-0334] hint. |
| `jingle` | `true`, `false` | Whether the stanza contains an [XEP-0353: Jingle Message Initiation][XEP-0353] element. |
| <nobr>`user_status`</nobr> | `"online"`, `"offline"` | Recipient status at routing time; available for `msg` events. |
| <nobr>`client_state`</nobr> | `"active"`, `"inactive"` | Recipient [XEP-0352: Client State Indication][XEP-0352] state; available for online `msg` events when [`mod_csi`](./mod_csi.md) is enabled. |

#### `modules.mod_event_pusher.push.rules.action`
* **Syntax:** string, one of `"push"`, `"skip"`
* **Default:** no default, this option is mandatory

Determines whether a matching rule sends or skips the notification.

#### `modules.mod_event_pusher.push.rules.content`
* **Syntax:** string, one of `"message"`, `"jingle"`
* **Default:** no default

Determines the content of the last message body in the push notification:

* `"message"` includes the message body,
* `"jingle"` builds it from a valid Jingle Message Initiation element in the format `Jingle message: <action>, session ID: <id>`.

This option is mandatory when `action` is `"push"` and cannot be specified when `action` is `"skip"`.
If the selected content cannot be built from the stanza, the notification is **not** sent and a warning is logged, so make sure to match `conditions` with `content`.

!!! Warning
    Jingle notifications are experimental and very likely to change in future versions.

#### Example

This example configuration enables the module with the RDBMS backend, and using a virtual pubsub host.
Push notifications are not sent for any messages with a `no-store` processing hint.
For other messages with non-empty body, they are sent when the recipient is either offline, or online with all sessions selected for delivery in the inactive CSI state.

```toml
[modules.mod_event_pusher.push]
  backend = "rdbms"
  virtual_pubsub_hosts = ["push.@HOST@"]

  [[modules.mod_event_pusher.push.rules]]
    conditions = [{ hint = "no_store" }]
    action = "skip"

  [[modules.mod_event_pusher.push.rules]]
    conditions = [
      { event = "msg", body = "non_empty", user_status = "offline" },
      { event = "msg", body = "non_empty", user_status = "online", client_state = "inactive" },
      { event = "unack_msg", body = "non_empty" }
    ]
    action = "push"
    content = "message"
```

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
option. `"pubsub.@HOST@"` is the default domain for `mod_pubsub_old`.

```toml
[modules.mod_pubsub_old]
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

### Plugin message notification logic

For regular chat and groupchat messages, both provided plugins send push notifications if one of the following conditions occurs at the moment of routing:

* Recipient has no online sessions selected for delivery.
* Recipient has one or more online sessions selected for delivery, but [`mod_csi`](./mod_csi.md) is enabled, and all these sessions are in the `inactive` CSI state.

### Custom plugins

A custom module implementing the optional callbacks of `mod_event_pusher_push_plugin`
may be used as a plugin to change the default behaviour. In the case of not implemented callbacks
the defaults are used instead.

[mod_event_pusher]: ./mod_event_pusher.md
[XEP-0198]: https://xmpp.org/extensions/xep-0198.html
[XEP-0334]: https://xmpp.org/extensions/xep-0334.html
[XEP-0352]: https://xmpp.org/extensions/xep-0352.html
[XEP-0353]: https://xmpp.org/extensions/xep-0353.html
[enabling]: https://xmpp.org/extensions/xep-0357.html#enabling
[tutorial]: ../tutorials/push-notifications/Push-notifications.md
[XEP-0357]: https://xmpp.org/extensions/xep-0357.html
