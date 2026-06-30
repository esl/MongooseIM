## Module description

This module provides a lightweight, performance-focused implementation of [XEP-0163: Personal Eventing Protocol](https://xmpp.org/extensions/xep-0163.html), using selected parts of [XEP-0060: Publish-Subscribe](https://xmpp.org/extensions/xep-0060.html).
It handles same-server PEP requests addressed to users' bare JIDs and does not expose a generic PubSub service on a `pubsub.@HOST@` domain.

`mod_pubsub` stores nodes, subscriptions, and items in RDBMS tables.
[mod_caps](mod_caps.md) should also be enabled for filtered PEP notifications.

!!! Note
    The goal is to gradually extend this lightweight implementation while phasing out [mod_pubsub_old](mod_pubsub_old.md).
    Until the missing functionality is implemented here, deployments that need a generic PubSub component, collection nodes, plugin-based node implementations, or push-notification PubSub nodes still need the old module.

## Supported functionality

* [Automatic creation](https://xmpp.org/extensions/xep-0060.html#publisher-publish-autocreate) of PEP nodes on publish.
* Explicit node [creation](https://xmpp.org/extensions/xep-0060.html#owner-create) and [deletion](https://xmpp.org/extensions/xep-0060.html#owner-delete).
* [Node configuration](https://xmpp.org/extensions/xep-0060.html#owner-configure) and [publish options](https://xmpp.org/extensions/xep-0060.html#publisher-publish-options), with supported options described below.
* [Publishing](https://xmpp.org/extensions/xep-0060.html#publisher-publish) and [retracting](https://xmpp.org/extensions/xep-0060.html#publisher-delete) an item.
* Retrieving [all items](https://xmpp.org/extensions/xep-0060.html#subscriber-retrieve-requestall), [selected items by ID](https://xmpp.org/extensions/xep-0060.html#subscriber-retrieve-requestone), or the [most recent N items](https://xmpp.org/extensions/xep-0060.html#subscriber-retrieve-requestrecent).
* Explicit [subscribe](https://xmpp.org/extensions/xep-0060.html#subscriber-subscribe) and [unsubscribe](https://xmpp.org/extensions/xep-0060.html#subscriber-unsubscribe).
* [Implicit PEP subscriptions](https://xmpp.org/extensions/xep-0163.html#notify-autosubscribe) based on presence subscription and entity capabilities.
* [Filtered item notifications](https://xmpp.org/extensions/xep-0163.html#notify-filterednotifications) with [mod_caps](mod_caps.md), and [node deletion notifications](https://xmpp.org/extensions/xep-0060.html#owner-delete-success).
* [Last published item delivery](https://xmpp.org/extensions/xep-0163.html#notify-last), with [`urn:xmpp:delay`](https://xmpp.org/extensions/xep-0203.html) metadata.
* Service discovery for [PEP support](https://xmpp.org/extensions/xep-0163.html#support-owner) on users' bare JIDs, [node-qualified disco info](https://xmpp.org/extensions/xep-0060.html#entity-info), and [disco items](https://xmpp.org/extensions/xep-0060.html#entity-nodes) listing discoverable PEP nodes.

### Node configuration options

`mod_pubsub` supports the following node configuration and publish options:

* [`pubsub#access_model`](https://xmpp.org/extensions/xep-0060.html#accessmodels): controls who can access a node. Supported values are `open` and `presence`.
* `pubsub#max_items`: controls how many published items are stored for later retrieval. It defaults to `max`, which stores all published items up to the server-wide [`max_items_per_node`](#modulesmod_pubsubmax_items_per_node) limit. A non-negative integer limits stored items for the node; when the limit is exceeded, the oldest stored item is removed. `0` allows publish notifications but does not persist items for later retrieval.

### Known omissions and limitations

`mod_pubsub` is intended to grow over time, but it does not implement the full XEP-0060 feature set yet.
Current intentional omissions and limitations are:

* Node and publish options other than the [supported node configuration options](#node-configuration-options) are not supported.
* [Subscription options](https://xmpp.org/extensions/xep-0060.html#subscriber-configure), [multiple subscriptions](https://xmpp.org/extensions/xep-0060.html#subscriber-subscribe-multi) for the same JID and node, and [collection nodes](https://xmpp.org/extensions/xep-0248.html) are not implemented.
* [Affiliations](https://xmpp.org/extensions/xep-0060.html#owner-affiliations), [default node configuration requests](https://xmpp.org/extensions/xep-0060.html#owner-default), [purge](https://xmpp.org/extensions/xep-0060.html#owner-purge), and [subscription management by node owners](https://xmpp.org/extensions/xep-0060.html#owner-subscriptions) are not implemented.
* [Result Set Management](https://xmpp.org/extensions/xep-0060.html#subscriber-retrieve-returnsome) is not implemented for item retrieval.

## Options

### `modules.mod_pubsub.backend`
* **Syntax:** string, currently only `"rdbms"` is supported.
* **Default:** `"rdbms"`
* **Example:** `backend = "rdbms"`

Database backend used to store PEP nodes, subscriptions, and items.
The `rdbms` backend requires a `default` RDBMS connection pool in [`outgoing_pools`](../configuration/outgoing-connections.md#rdbms-options).

### `modules.mod_pubsub.max_items_per_node`
* **Syntax:** non-negative integer or `"infinity"`
* **Default:** `"infinity"`
* **Example:** `max_items_per_node = 1000`

Maximum number of items stored in a node.
The effective limit is the lower of this value and `pubsub#max_items`.
If you reduce this limit, excess items for each already existing node are removed when it is next configured or receives a published item.

### `modules.mod_pubsub.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"no_queue"`

Strategy to handle incoming IQ stanzas.
By default, IQ requests are handled by the C2S process of a sender.
For details, please refer to [IQ processing policies](../configuration/Modules.md#iq-processing-policies).

## Example Configuration

It is recommended to start with the default options and enable [mod_caps](mod_caps.md) for filtered notifications:

```toml
[modules.mod_caps]

[modules.mod_pubsub]
```

The example below shows a different configuration where IQ requests are handled by a pool of 50 workers:

```toml
[modules.mod_pubsub]
  iqdisc.type = "queues"
  iqdisc.workers = 50
```
