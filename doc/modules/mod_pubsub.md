## Module Description

This module provides a lightweight, performance-focused implementation of [XEP-0163: Personal Eventing Protocol](https://xmpp.org/extensions/xep-0163.html), using selected parts of [XEP-0060: Publish-Subscribe](https://xmpp.org/extensions/xep-0060.html).
It handles same-server PEP requests addressed to users' bare JIDs and does not expose a generic PubSub service on a `pubsub.@HOST@` domain.

`mod_pubsub` stores nodes, subscriptions and items in RDBMS tables.
It has a hard dependency on [mod_caps](mod_caps.md), which stores entity capabilities used for PEP notification filtering.

!!! Note
    The goal is to gradually extend this lightweight implementation while phasing out [mod_pubsub_old](mod_pubsub_old.md).
    Until the missing functionality is implemented here, deployments that need a generic PubSub component, collection nodes, plugin-based node implementations or push-notification PubSub nodes still need the old module.

## Supported functionality

* Automatic creation of PEP nodes on publish.
* Explicit node creation and deletion.
* Node configuration with the `pubsub#access_model` option.
* The `open` and `presence` access models.
* Publishing one item at a time, including publishing options for `pubsub#access_model`.
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
* Only the `pubsub#access_model` node configuration option is supported.
* Subscription options, multiple subscriptions for the same JID and node, and collection nodes are not implemented.
* Affiliations, default configuration requests, purge, retract, and subscription management by node owners are not implemented.
* RSM is not implemented for item retrieval.

## Options

### `modules.mod_pubsub.backend`
* **Syntax:** string, currently only `"rdbms"` is supported.
* **Default:** `"rdbms"`
* **Example:** `backend = "rdbms"`

Database backend used to store PEP nodes, subscriptions and items.
The `rdbms` backend requires a `default` RDBMS connection pool in [`outgoing_pools`](../configuration/outgoing-connections.md#rdbms-options).

### `modules.mod_pubsub.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"no_queue"`

Strategy to handle incoming stanzas.
For details, please refer to [IQ processing policies](../configuration/Modules.md#iq-processing-policies).

## Example Configuration

It is recommended to start with the default options:

```toml
[modules.mod_pubsub]
```

Note that there is no `mod_caps` configured, because it will be started by default.
