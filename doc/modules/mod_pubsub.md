## Module description

This module provides a lightweight, performance-focused implementation of [XEP-0163: Personal Eventing Protocol](https://xmpp.org/extensions/xep-0163.html), using selected parts of [XEP-0060: Publish-Subscribe](https://xmpp.org/extensions/xep-0060.html).
It handles same-server PEP requests addressed to users' bare JIDs and does not expose a generic PubSub service on a `pubsub.@HOST@` domain.

`mod_pubsub` stores nodes, subscriptions, and items in RDBMS tables.
It has a hard dependency on [mod_caps](mod_caps.md), which stores entity capabilities used for PEP notification filtering.

!!! Note
    The goal is to gradually extend this lightweight implementation while phasing out [mod_pubsub_old](mod_pubsub_old.md).
    Until the missing functionality is implemented here, deployments that need a generic PubSub component, collection nodes, plugin-based node implementations, or push-notification PubSub nodes still need the old module.

## Supported functionality

* Automatic creation of PEP nodes on publish.
* Explicit node creation and deletion.
* Node configuration and publish options with the `pubsub#access_model` option.
* The `open` and `presence` access models.
* Publishing one item at a time.
* Retrieving all items or selected items by ID.
* Explicit subscribe and unsubscribe.
* Implicit PEP subscriptions based on presence subscription and entity capabilities.
* Filtered item notifications and node deletion notifications.
* Last published item delivery, with `urn:xmpp:delay` metadata.
* Service discovery for PEP support on users' bare JIDs, node-qualified disco info, and disco items listing discoverable PEP nodes.

## Known omissions and deviations

`mod_pubsub` is intended to grow over time, but it does not implement the full XEP-0060 feature set yet.
Current intentional omissions and deviations are:

* Only the `open` and `presence` access models are supported.
* The only supported node and publish option is `pubsub#access_model`.
* Subscription options, multiple subscriptions for the same JID and node, and collection nodes are not implemented.
* Affiliations, default node configuration requests, purge, retract, and subscription management by node owners are not implemented.
* Result Set Management is not implemented for item retrieval.

## Options

### `modules.mod_pubsub.backend`
* **Syntax:** string, currently only `"rdbms"` is supported.
* **Default:** `"rdbms"`
* **Example:** `backend = "rdbms"`

Database backend used to store PEP nodes, subscriptions, and items.
The `rdbms` backend requires a `default` RDBMS connection pool in [`outgoing_pools`](../configuration/outgoing-connections.md#rdbms-options).

### `modules.mod_pubsub.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"no_queue"`

Strategy to handle incoming IQ stanzas.
By default, IQ requests are handled by the C2S process of a sender.
For details, please refer to [IQ processing policies](../configuration/Modules.md#iq-processing-policies).

## Example Configuration

It is recommended to start with the default options:

```toml
[modules.mod_pubsub]
```

Note that `mod_caps` is not configured, because it will be started automatically.
The example below shows a different configuration where IQ requests are handled by a pool of 50 workers:

```toml
[modules.mod_pubsub]
  iqdisc.type = "queues"
  iqdisc.workers = 50
```
