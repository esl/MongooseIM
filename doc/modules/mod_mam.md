### Module Description
This module implements [XEP-0313 (Message Archive Management)](https://xmpp.org/extensions/xep-0313.html).
It enables a service to store all user messages for one-to-one chats as well as group chats (MUC, MultiUser Chat).
It uses [XEP-0059: Result Set Management](http://xmpp.org/extensions/xep-0059.html) for paging.
It is a highly customizable module, that requires some skill and knowledge to operate properly and efficiently.

Configure MAM with different storage backends:

* RDBMS (databases like MySQL, PostgreSQL, MS SQL Server)
* Riak KV (NOSQL)
* Cassandra (NOSQL)
* ElasticSearch (NOSQL)

`mod_mam_meta` is a meta-module that ensures all relevant `mod_mam_*` modules are loaded and properly configured.

#### Full Text Search
This module allows message filtering by their text body (if enabled, see *Common backend options*).
This means that an XMPP client, while requesting messages from the archive may not only specify standard form fields (`with`, `start`, `end`), but also `full-text-search` (of type `text-single`).
If this happens, the client will receive only messages that contain words specified in the request.

The exact behaviour, like whether word ordering matters, may depend on the storage backend in use.
For now `rdbms` backend has very limited support for this feature, while `cassandra` does not support it at all.
`riak` and `elasticsearch` backends, on the other hand, should provide you with the best results when it comes to text filtering.

`mod_mam_rdbms_arch` returns all messages that contain all search words, order
of words does not matter. Messages are sorted by timestamp (not by relevance).

##### Note on full text search with ElasticSearch backend

When using ElasticSearch MAM backend, the value provided in `full-text-search` form field will be passed to ElasticSearch as [Simple Search Query](https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-simple-query-string-query.html).
If you're using our official ElasticSearch mappings from `priv/elasticsearch` then the query analyzer is set to `english`.
Also note that the default separator for the search query is `AND` (which roughly means that ElasticSearch will search for messages containing all the terms provided in the query string).

### Options

* **backend** (atom, default: `rdbms`) - Database backend to use. `rdbms`, `riak`, `cassandra` and `elasticsearch` are supported.
* **no_stanzaid_element** (boolean, default: `false`) - Do not add a `<stanza-id/>` element from MAM v0.6.
* **is_archivable_message** (module, default: `mod_mam_utils`) - Name of a module implementing [`is_archivable_message/3` callback](#is_archivable_message) that determines if the message should be archived.
 **Warning**: if you are using MUC Light, make sure this option is set to the MUC Light domain.
* **archive_chat_markers** (boolean, default: `false`) - If set to true, XEP-0333 chat markers will be archived. See more details [here](#archiving-chat-markers)
* **pm** (list | `false`, default: `[]`) - Override options for archivization of one-to-one messages. If the value of this option is `false`, one-to-one message archive is disabled.
* **muc** (list | `false`, default: `false`) - Override options for archivization of group chat messages. If the value of this option is `false`, group chat message archive is disabled.
* **extra_lookup_params** (atom, default: `undefined`) - a module implementing `mam_iq` behaviour.
 If this option has value other then undefined, function `extra_lookup_params/2` from this module will be called when building MAM lookup parameters.
 This can be used to extend currently supported MAM query fields by a custom field or fields.
 This field(s) can be added to lookup params later passed to MAM backend.

**backend**, **no_stanzaid_element** and **is_archivable_message** will be applied to both `pm` and `muc` (if they are enabled), unless overriden explicitly (see example below).

#### PM-specific options

* **archive_groupchats** (boolean, default: `true`) - When enabled, MAM will store groupchat messages in recipients' individual archives. **USE WITH CAUTION!** May increase archive size significantly. Disabling this option for existing installation will neither remove such messages from MAM storage, nor will filter out them from search results.
MongooseIM will print a warning on startup if `pm` MAM is enabled without `archive_groupchats` being explicitly set to a specific value. In one of the future MongooseIM releases this option will default to `false` (as it's more common use case and less DB-consuming) and the warning message will be removed.

#### MUC-specific options

* **host** (string, default: `"conference.@HOST@"`) - MUC host that will be archived if MUC archiving is enabled.

#### Example

The example below presents how to override common option for `muc` module specifically.

```erlang
{mod_mam_meta, [
  {backend, rdbms},
  {async_writer, true}, %% this option enables async writer for RDBMS backend
  {muc, [
    {async_writer, false} %% disable async writer for MUC archive only
  ]}
]}
```

#### RDBMS backend options

These options will only have effect when the `rdbms` backend is used:

* **cache_users** (boolean, default: `true`) - Enables Archive ID to integer mappings cache.
* **rdbms_message_format** (atom, default: `internal`) - When set to `simple`, stores messages in XML and full JIDs.
 When set to `internal`, stores messages and JIDs in internal format.
 **Warning**: Archive MUST be empty to change this option.
* **async_writer** (boolean, default: `true`) - Enables an asynchronous writer that is faster than the synchronous one but harder to debug.
  The async writers store batches of messages with a certain delay (see **flush_interval**), so the results of the lookup operations executed right after message routing may be incomplete until the configured time passes.
* **flush_interval** (integer, default: `2000`) How often (in milliseconds) the buffered messages are flushed to a DB.
* **max_batch_size** (integer, default, `30`) Max size of the batch insert query for an async writer.
  If the buffer is full, messages are flushed to a database immediately and the flush timer is reset.

#### Common backend options

* **user_prefs_store** (atom, default: `false`) - Leaving this option as `false` will prevent users from setting their archiving preferences. It will also increase performance. Other possible values are:
  * `rdbms` (RDBMS backend only) - User archiving preferences saved in RDBMS. Slow and not recommended, but might be used for simplicity (keeping everything in RDBMS).
  * `cassandra` (Cassandra backend only) - User archiving preferences are saved in Cassandra.
  * `mnesia` (recommended) - User archiving preferences saved in Mnesia and accessed without transactions. Recommended in most deployments, could be overloaded with lots of users updating their preferences at once. There's a small risk of an inconsistent (in a rather harmless way) state of the preferences table.
* **full_text_search** (boolean, default: `true`) - Enables full text search in message archive (see *Full Text Search* paragraph). Please note that the full text search is currently only implemented for `rdbms` and `riak` backends. Also, full text search works only for messages archived while this option is enabled.

#### <a id="is_archivable_message"></a>`is_archivable_message/3` callback

`is_archivable_message` option has to name a module exporting `is_archivable_message/3` function conforming to the spec:

```erlang
-spec is_archivable_message(Mod :: module(), Dir :: incoming | outgoing,
                          Packet :: exml:element()) -> boolean().
```

Servers SHOULD NOT archive messages that do not have a `<body/>` child tag. Servers SHOULD NOT archive delayed messages.

From MAM v0.3 onwards it is expected that all messages that hold meaningful content, rather than state changes such as Chat State Notifications, are archived.

#### Archiving chat markers

Archiving chat markers can be enabled by setting `archive_chat_markers` option to `true`. However it only works if
`is_archivable_message` callback module is set to `mod_mam_utils` or isn't set at all.

When performing full text search chat markers are treated as if they had empty message body.

### Riak backend

The Riak KV backend for MAM stores messages in weekly buckets so it's easier to remove old buckets.
Archive querying is done using Riak KV 2.0 [search mechanism](http://docs.basho.com/riak/2.1.1/dev/using/search/) called Yokozuna.
Your instance of Riak KV must be configured with Yokozuna enabled.

This backend works with Riak KV 2.0 and above, but we recommend version 2.1.1.

### Cassandra backend

Please consult [Outgoing connections](../advanced-configuration/outgoing-connections.md#cassandra-connection-setup) page to learn how to properly configure Cassandra connection pool.
By default, `mod_mam` Cassandra backend requires `global` pool with `default` tag:

```erlang
{outgoing_pools, [
    {cassandra, global, default, [], []}.
]}.
```

### ElasticSearch backend

First, make sure that your ElasticSearch cluster has expected indexes and mappings in place.
Please refer to [database backends configuration](../advanced-configuration/database-backends-configuration.md#elasticsearch) page for information on how to configure ElasticSearch properly.

Edit main config section adding:

```erlang
{elasticsearch_server, []}.
```

MongooseIM will create one pool with one worker which will try to connect to localhost:9200.
Make sure that your ElasticSearch node's HTTP API is listening on this address!
For more information on how to configure the connection pool, please refer to [advanced configuration](../Advanced-configuration.md#elasticsearch-connection-setup) page.

### Example configuration

```erlang
{mod_mam_meta, [
        {backend, rdbms},

        {no_stanzaid_element, true},

        {pm, [{user_prefs_store, rdbms}]},
        {muc, [
               {host, "muc.example.com"},
               {rdbms_message_format, simple},
               {async_writer, false},
               {user_prefs_store, mnesia}
              ]}
       ]}.
```

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[Host, modMamArchiveRemoved]` | spiral | User's entire archive is removed. |
| `[Host, modMamArchived]` | spiral | A message is stored in user's archive. |
| `[Host, modMamDropped]` | spiral | A message couldn't be enqueued due to an overloaded async worker. |
| `[Host, modMamDropped2]` | spiral | A message couldn't be stored in the DB (and got dropped). |
| `[Host, modMamDroppedIQ]` | spiral | MAM IQ has been dropped due to: high query frequency/invalid syntax or type. |
| `[Host, modMamFlushed]` | spiral | Message was stored in a DB asynchronously. |
| `[Host, modMamForwarded]` | spiral | A message is sent to a client as a part of a MAM query result. |
| `[Host, modMamLookups]` | spiral | A MAM lookup is performed. |
| `[Host, modMamSinglePurges]` | spiral | A single purge request is processed by MAM. |
| `[Host, modMamMultiplePurges]` | spiral | A bulk purge request is processed by MAM. |
| `[Host, modMamPrefsGets]` | spiral | Archiving preferences have been requested by a client. |
| `[Host, modMamPrefsSets]` | spiral | Archiving preferences have been updated by a client. |
| `[Host, modMucMamArchiveRemoved]` | spiral | Room's entire archive is removed. |
| `[Host, modMucMamArchived]` | spiral | A message is stored in room's archive. |
| `[Host, modMucMamForwarded]` | spiral | A message is sent to a client as a part of a MAM query result from MUC room. |
| `[Host, modMucMamLookups]` | spiral | A MAM lookup in MUC room is performed. |
| `[Host, modMucMamSinglePurges]` | spiral | A single purge request for MUC room is processed by MAM. |
| `[Host, modMucMamMultiplePurges]` | spiral | A bulk purge request for MUC room is processed by MAM. |
| `[Host, modMucMamPrefsGets]` | spiral | MUC archiving preferences have been requested by a client. |
| `[Host, modMucMamPrefsSets]` | spiral | MUC archiving preferences have been updated by a client. |
| `[Host, mod_mam_rdbms_async_pool_writer, per_message_flush_time]` | histogram | Average time per message insert measured in an async MAM worker. |
| `[Host, mod_mam_rdbms_async_pool_writer, flush_time]` | histogram | Average time per flush of all buffered messages measured in an async MAM worker. |
| `[Host, mod_mam_muc_rdbms_async_pool_writer, per_message_flush_time]` | histogram | Average time per message insert measured in an async MUC MAM worker. |
| `[Host, mod_mam_muc_rdbms_async_pool_writer, flush_time]` | histogram | Average time per flush of all buffered messages measured in an async MUC MAM worker. |
| `[Host, backends, mod_mam, lookup]` | histogram | Time it took to perform a lookup in an archive. |
| `[Host, backends, mod_mam, archive]` | histogram | Time it took to save one message in an archive. |
