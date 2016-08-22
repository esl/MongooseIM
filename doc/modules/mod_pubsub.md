### What is PubSub?

PubSub is a fascinating design pattern which mostly promotes a loose coupling between two kinds of entity (publishers and subscribers).
Like their names suggest, in the pubsub world we have publishers who fire events, and subscribers who wish to be notified about those events when publishers push data.
There might be several subscribers, several publishers, and even several channels (or nodes) where the events are sent.

### Module Description

This module implements [XEP-0060 (Publish-Subscribe)](http://www.xmpp.org/extensions/xep-0060.html).
Due to the complexity of the protocol, the PubSub engine makes successive calls to `nodetree` and `node plugins` in order to check the validity of requests, perform the corresponding action and return a result or appropriate error.
Such an architecture makes it much easier to write custom pubsub plugins and add new storage backends.
It's all about fitting PubSub to your needs!

### Options

* `host` (string, default: `"pubsub.@HOST@"`): Subdomain for Pubsub service to reside under.
`@HOST@` is replaced with each served domain.
* `access_create` (atom, default: `all`): Who is allowed to create pubsub nodes.
* `max_items_node` (integer, default: `10`): Define the maximum number of items that can be stored in a node.
* `max_subscriptions_node` (integer, default: `undefined` - no limitation): The maximum number of subscriptions managed by a node.
* `nodetree` (binary, default: `<<"tree">>`): Specify the storage and organisation of the pubsub nodes.
Called on `get`, `create` and `delete` node.
Only one nodetree can be used per host, and is shared by all node plugins.
    * `<<"tree">>"` - store nodes in database (only mnesia supported).
    * `<<"virtual">>` - does not store nodes in database.
    This saves resources on systems when the number of nodes is large.
    If using the `<<“virtual”>>` nodetree, you can only enable these node plugins:
    `[<<“flat”>>,<<“pep”>>]` or `[<<“flat”>>]`; any other plugin configuration will not work.
    Also, all nodes will have the defaut configuration, and this can not be changed.
    Using the `<<“virtual”>>` nodetree requires the pubsub module to be started with a clean database, it will not work if you used the default `<<“tree”>>` nodetree before.
    * `<<"dag">>"` - provides experimental support for [XEP-0248 (PubSub Collection Nodes)](http://xmpp.org/extensions/xep-0248.html).
    In this case you should also add the `<<“dag”>>` node plugin as default, for example: plugins: `[<<"dag">>,<<"flat">>,<<"hometree">>,<<"pep">>]`
* `ignore_pep_from_offline` (boolean, default: `true`): specify whether or not we should get last published PEP items from users in our roster which are offline when we connect.
The default option is `true` hence we will get only the last items from the online contacts.
* `last_item_cache` (boolean, default `false`): specify whether or not pubsub should cache the last items. Such an option provides the ability to send the last published item to a new subscriber.
Such caching might speed up pubsub's performance and can increase the number of user connections but in price of memory usage.
* `plugins` ([Plugin, ...], default: `[<<"flat">>]`): List of enabled pubsub plugins.
They handle affiliations, subscriptions and items and also provide default node conﬁguration and features.
PubSub clients can define which plugin to use when creating a node by adding `type=’plugin-name’` attribute to the create stanza element.
If such an attribute is not specified, the default plugin will be the first on the plugin list.
    * `<<"flat">>` -  No node hierarchy.
    It handles the standard PubSub case.
    * `<<"hometree">>` - Uses the exact same features as the flat plugin but additionally organises nodes in a tree.
    Basically it follows a scheme similar to the filesystem's structure.
    Every user can create nodes in its own home root: e.g `/home/user`.
    Each node can contain items and/or sub-nodes.
    * `<<"pep">>` - implementation of [XEP-0060 (Personal Eventing Protocol)](http://xmpp.org/extensions/xep-0163.html).
    In this case, items are not persisted but kept in an in-memory cache.
    When the `pep` plugin is enabled, a user can have their own node (exposed as their bare jid) with a common namespace.
    Requires module `mod_caps` to be enabled.
    * `<<"dag">>` - implementation of [XEP-0248 (PubSub Collection Nodes)](https://xmpp.org/extensions/xep-0248.html).
    Every node takes a place in a tree and is either a collection node (and have only sub-nodes) or a leaf node (contains only items).
* `pep_mapping` ([{Key, Value}, ...]): This permits the definistion of a Key-Value list to define a custom node plugin on a given PEP namespace.
E.g. pair `{"urn:xmpp:microblog:0", "mb"}` will use module `node_mb` instead of `node_pep` when the specified namespace is used.
* `default_node_config` ([{Key, Value}, ...]): Overrides default node configuration, regradless of node plugin.
Node configuration still uses the default configuration defined by the node plugin, and overrides any items by the value defined in this configurable list.

### Example Configuration

```
   {mod_pubsub, [{access_createnode, pubsub_createnode},
                 {ignore_pep_from_offline, false},
                 {last_item_cache, true},
                 {max_items_node, 1000},
                 {plugins, [<<"flat">>, <<"pep">>]}}
  ]},
```
