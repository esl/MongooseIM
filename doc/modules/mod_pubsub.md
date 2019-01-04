### What is PubSub?

PubSub is a design pattern which mostly promotes a loose coupling between two kinds of entities - publishers and subscribers.
Like their names suggest, in the pubsub world we have publishers who fire events, and subscribers who wish to be notified about those events when publishers push data.
There might be several subscribers, several publishers, and even several channels (or nodes) where the events are sent.

### Module Description

This module implements [XEP-0060 (Publish-Subscribe)](http://www.xmpp.org/extensions/xep-0060.html).
Due to the complexity of the protocol, the PubSub engine makes successive calls to the `nodetree` and `node plugins` in order to check the validity of requests, perform the corresponding action and return a result or appropriate error.
Such an architecture makes it much easier to write custom pubsub plugins and add new storage backends.
It's all about tailoring PubSub to your needs!

### Options

* `iqdisc` (default: `one_queue`)
* `host` (string, default: `"pubsub.@HOST@"`): Subdomain for Pubsub service to reside under.
`@HOST@` is replaced with each served domain.
* `backend` (atom, default: `mnesia`) - Database backend to use. `mnesia` and `rdbms` (*warning!* experimental) are supported currently.
* `access_create` (atom, default: `all`): Who is allowed to create pubsub nodes.
* `max_items_node` (integer, default: `10`): Define the maximum number of items that can be stored in a node.
* `max_subscriptions_node` (integer, default: `undefined` - no limitation): The maximum number of subscriptions managed by a node.
* `nodetree` (binary, default: `<<"tree">>`): Specifies the storage and organisation of the pubsub nodes. See the section below.
* `ignore_pep_from_offline` (boolean, default: `true`): specify whether or not we should get last published PEP items from users in our roster which are offline when we connect.
The default option is `true` hence we will get only the last items from the online contacts.
* `last_item_cache` (boolean, default `false`): specifies whether or not pubsub should cache the last items. Such an option provides the ability to send the last published item to a new subscriber.
Such caching might speed up pubsub's performance and can increase the number of user connections but in price of memory usage.
* `plugins` ([Plugin, ...], default: `[<<"flat">>]`): List of enabled pubsub plugins.
* `pep_mapping` ([{Key, Value}, ...]): This permits creating a Key-Value list to define a custom node plugin on a given PEP namespace.
E.g. pair `{"urn:xmpp:microblog:0", "mb"}` will use module `node_mb` instead of `node_pep` when the specified namespace is used.
* `default_node_config` ([{Key, Value}, ...]): Overrides the default node configuration, regradless of the node plugin.
Node configuration still uses the default configuration defined by the node plugin, and overrides any items by the value defined in this configurable list.
* `item_publisher` (boolean, default: `false`): When enabled, a JID of the publisher will be saved in the item metadata.
 This effectively makes them an owner of this item.

#### Note about RDBMS backend

RDBMS backend is in an experimental stage.
It is a full implementation with one exception: DB operations are not protected with transactions yet.
The schema used by this backend may change until it reaches stable status.

#### Cache Backend

Although it is not a default setting, the cache backend can be configured
to use Mnesia or RDBMS. It allows storing the `pubsub_last_item` table
separately. For example you can configure `backend` to use `rdbms` and
`last_item_cache` to use `mnesia`.

### Example Configuration

```
   {mod_pubsub, [{access_createnode, pubsub_createnode},
                 {ignore_pep_from_offline, false},
                 {backend, rdbms},
                 {last_item_cache, mnesia},
                 {max_items_node, 1000},
                 {plugins, [<<"flat">>, <<"pep">>]}
  ]},
```

### Nodetrees

Called on `get`, `create` and `delete` node.
Only one nodetree can be used per host and is shared by all node plugins.

#### `<<"tree">>`

Stores nodes in a tree structure.
Every node name must be formatted like a UNIX path (e.g. `/top/middle/leaf`).
When a node is created, its direct ancestor must already exist, so in order to create `/top/middle/leaf`, `/top/middle` is needed.
A user may create any top-level node.
A user may create a subnode of a node, only if they own it or it was created by the service.


#### `<<"dag">>`

Provides experimental support for [XEP-0248 (PubSub Collection Nodes)](http://xmpp.org/extensions/xep-0248.html).
In this case you should also add the `<<"dag">>` node plugin as default, for example: `{plugins, [<<"dag">>,<<"flat">>,<<"hometree">>,<<"pep">>]}`

### Plugins

They handle affiliations, subscriptions and items and also provide default node conﬁguration and features.
PubSub clients can define which plugin to use when creating a node by adding `type='plugin-name'` attribute to the create stanza element.
If such an attribute is not specified, the default plugin will be the first on the plugin list.

#### `<<"flat">>`

No node hierarchy.
It handles the standard PubSub case.

#### `<<"hometree">>`

Uses the exact same features as the flat plugin but additionally organises nodes in a tree.
Basically it follows a scheme similar to the filesystem's structure.
Every user can create nodes in their own home root: e.g `/home/user`.
Each node can contain items and/or sub-nodes.

#### `<<"pep">>`

Implementation of [XEP-0060 (Personal Eventing Protocol)](http://xmpp.org/extensions/xep-0163.html).
In this case, items are not persisted but kept in an in-memory cache.
When the `pep` plugin is enabled, a user can have their own node (exposed as their bare jid) with a common namespace.
Requires module `mod_caps` to be enabled.

#### `<<"dag">>`

Implementation of [XEP-0248 (PubSub Collection Nodes)](https://xmpp.org/extensions/xep-0248.html).
Every node takes a place in a collection and becomes either a collection node (and have only sub-nodes) or a leaf node (contains only items).

#### `<<"push">>`

Special node type that may be used as a target node for [XEP-0357 (Push Notifications)](https://xmpp.org/extensions/xep-0357.html) capable services (e.g. `mod_event_pusher_push`).
For each published notification, a hook `push_notification` is run.
You may enable as many modules that support this hook (all module with `mod_push_service_*` name prefix) as you like (see for example `mod_push_service_mongoosepush`).
This node type **requires** `publish-options` with at least `device_id` and `service` fields supplied.

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit the [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

#### Overall PubSub action metrics

For every PubSub action, like node creation, subscription, publication there are following kind of metrics:

* count - a spiral metric measuring number of given action invocations
* errors - a spiral metric errors for given action
* time - a histogram metric showing the time it took to finish the action in case of success

Below there is a table describing all metrics related to PubSub actions

| Name | Description (when it gets incremented) |
| ---- | -------------------------------------- |
|`[HOST,pubsub,get,affiliations,TYPE]` | When node's affiliations are read |
|`[HOST,pubsub,get,configure,TYPE]` | When node's configuration is read |
|`[HOST,pubsub,get,default,TYPE]` | When node's defaults are read |
|`[HOST,pubsub,get,items,TYPE]` | When node's items are read |
|`[HOST,pubsub,get,options,TYPE]` | When node's options are read |
|`[HOST,pubsub,get,subscriptions,TYPE]` | When node's subscriptions are read |
|`[HOST,pubsub,set,affiliations,TYPE]` | When node's subscriptions are set |
|`[HOST,pubsub,set,configure,TYPE]` | When node's configuration is set |
|`[HOST,pubsub,set,create,TYPE]` | When node is created |
|`[HOST,pubsub,set,delete,TYPE]` | When node is deleted |
|`[HOST,pubsub,set,options,TYPE]` | When node's options are set |
|`[HOST,pubsub,set,publish,TYPE]` | When an item is published |
|`[HOST,pubsub,set,purge,TYPE]` | When node's items are purged |
|`[HOST,pubsub,set,retract,TYPE]` | When node's items are retracted |
|`[HOST,pubsub,set,subscribe,TYPE]` | When a subscriber subscribes to a node |
|`[HOST,pubsub,set,subscriptions,TYPE]` | When a subscription is set (for instance accepted) |
|`[HOST,pubsub,set,unsubscribe,TYPE]` | When a subscriber unsubscribes |

Where:

* `HOST` is the XMPP host for which mod_pubsub is running. Can be set to `global` if all metrics are set to be global.
* `TYPE` is one of the following `count`, `errors`, `time` (described above the table)

#### Backend operations

The are also more detailed metrics measuring execution time of backend operations.

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[global, backends, mod_pubsub_db, set_state]` | histogram | Time to update user's state for specific node. |
| `[global, backends, mod_pubsub_db, del_state]` | histogram | Time to remove user's state for specific node. |
| `[global, backends, mod_pubsub_db, get_state]` | histogram | Time to fetch user's state for specific node. |
| `[global, backends, mod_pubsub_db, get_states]` | histogram | Time to fetch node's states. |
| `[global, backends, mod_pubsub_db, get_states_by_lus]` | histogram | Time to fetch nodes' states for user + domain. |
| `[global, backends, mod_pubsub_db, get_states_by_bare]` | histogram | Time to fetch nodes' states for bare JID. |
| `[global, backends, mod_pubsub_db, get_states_by_full]` | histogram | Time to fetch nodes' states for full JID. |
| `[global, backends, mod_pubsub_db, get_own_nodes_states]` | histogram | Time to fetch state data for user's nodes. |
| `[global, backends, mod_pubsub_db, create_node, ]` | histogram | Time to set the owner after a node is created. |
| `[global, backends, mod_pubsub_db, del_node]` | histogram | Time to delete all data related to a node being removed. |
| `[global, backends, mod_pubsub_db, get_items]` | histogram | Time to fetch node's items. |
| `[global, backends, mod_pubsub_db, get_item]` | histogram | Time to fetch a specific item for a node. |
| `[global, backends, mod_pubsub_db, set_item]` | histogram | Time to insert/update item in a node. |
| `[global, backends, mod_pubsub_db, del_item]` | histogram | Time to delete an item from a node. |
| `[global, backends, mod_pubsub_db, del_items]` | histogram | Time to delete specified items from a node. |
| `[global, backends, mod_pubsub_db, set_node]` | histogram | Time to create/update a node. |
| `[global, backends, mod_pubsub_db, find_node_by_id,]` | histogram | Time to fetch a node by its id. |
| `[global, backends, mod_pubsub_db, find_nodes_by_key]` | histogram | Time to fetch nodes by key. |
| `[global, backends, mod_pubsub_db, find_node_by_name]` | histogram | Time to fetch a node by its name. |
| `[global, backends, mod_pubsub_db, delete_node]` | histogram | Time to delete a node. |
| `[global, backends, mod_pubsub_db, get_subnodes]` | histogram | Time to fetch subnodes of a node. |
| `[global, backends, mod_pubsub_db, get_subnodes_tree]` | histogram | Time to fetch all subnodes of a node. |
| `[global, backends, mod_pubsub_db, get_parentnodes_tre]` | histogram | Time to fetch all parents of a node. |

