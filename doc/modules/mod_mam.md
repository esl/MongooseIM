### Module Description
This module implements [XEP-0313 (Message Archive Management)](https://xmpp.org/extensions/xep-0313.html).
It enables a service to store all user messages for one-to-one chats as well as group chats (MUC, MultiUser Chat).
It uses [XEP-0059: Result Set Management](http://xmpp.org/extensions/xep-0059.html) for paging.
It is a highly customizable module, that requires some skill and knowledge to operate properly and efficiently.

MongooseIM is compatible with MAM 0.4-0.6.

Configure MAM with different storage backends:

* RDBMS (databases like MySQL, PostgreSQL, MS SQL Server)
* Riak KV (NOSQL)
* Cassandra (NOSQL)
* ElasticSearch (NOSQL)

`mod_mam_meta` is a meta-module that ensures all relevant `mod_mam_*` modules are loaded and properly configured.

#### Message retraction
This module supports [XEP-0424: Message Retraction](http://xmpp.org/extensions/xep-0424.html) with RDBMS storage backends. When a [retraction message](https://xmpp.org/extensions/xep-0424.html#example-4) is received, the MAM module finds the message to retract and replaces it with a tombstone. The following criteria are used to find the original message:

* The `id` attribute specified in the `apply-to` element of the retraction message has to be the same as the `id` attribute of the `origin-id` element of the original message.
* Both messages need to originate from the same user.
* Both messages need to be addressed to the same user.

If more than one message matches the criteria, only the most recent one is retracted. To avoid this case, it is recommended to use a unique identifier (UUID) as the origin ID.

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

#### `modules.mod_mam_meta.backend`
* **Syntax:** string, one of `"rdbms"`, `"riak"`, `"cassandra"` and `"elasticsearch"`
* **Default:** `"rdbms"`
* **Example:** `backend = "riak"`

Database backend to use.

#### `modules.mod_mam_meta.no_stanzaid_element`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `no_stanzaid_element = true`

Do not add a `<stanza-id/>` element from MAM v0.6.

#### `modules.mod_mam_meta.is_archivable_message`
* **Syntax:** non-empty string
* **Default:** `"mod_mam_utils"`
* **Example:** `is_archivable_message = "mod_mam_utils"`
* **Warning**: if you are using MUC Light, make sure this option is set to the MUC Light domain

Name of a module implementing [`is_archivable_message/3` callback](#is_archivable_message) that determines if the message should be archived.

#### `modules.mod_mam_meta.archive_chat_markers`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `archive_chat_markers = true`

If set to true, XEP-0333 chat markers will be archived.
See more details [here](#archiving-chat-markers).

#### `modules.mod_mam_meta.message_retraction`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `message_retraction = false`

Enables [XEP-0424: Message Retraction](http://xmpp.org/extensions/xep-0424.html).
This functionality is currently implemented only for the `rdbms` backend.
[Retraction messages](https://xmpp.org/extensions/xep-0424.html#example-4) are always archived regardless of this option.

**backend**, **no_stanzaid_element**, **is_archivable_message** and **message_retraction** will be applied to both `pm` and `muc` (if they are enabled), unless overridden explicitly (see example below).

#### Enable one-to-one message archive

Archive for one-to-one messages can be enabled in one of two ways:

* Specify `[mod_mam_meta.pm]` section
```toml
[modules.mod_mam_meta]
[modules.mod_mam_meta.pm] # defining this section enables PM support
```
* Define any PM related option
```toml
[modules.mod_mam_meta]
  pm.backend = "rdbms" # enables PM support and overrides its backend
```

#### Disable one-to-one message archive

To disable archive for one-to-one messages please remove PM section or any PM related option from the config file.

### PM-specific options

#### `modules.mod_mam_meta.pm.archive_groupchats`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `modules.mod_mam_meta.muc = true`

When enabled, MAM will store groupchat messages in recipients' individual archives. **USE WITH CAUTION!** May increase archive size significantly. Disabling this option for existing installation will neither remove such messages from MAM storage, nor will filter out them from search results.
MongooseIM will print a warning on startup if `pm` MAM is enabled without `archive_groupchats` being explicitly set to a specific value. In one of the future MongooseIM releases this option will default to `false` (as it's more common use case and less DB-consuming) and the warning message will be removed.

#### Enable MUC message archive

Archive for MUC messages can be enabled in one of two ways:

* Specify `[mod_mam_meta.muc]` section
```toml
[modules.mod_mam_meta]
[modules.mod_mam_meta.muc] # defining this section enables MUC support
```
* Define any MUC related option
```toml
[modules.mod_mam_meta]
  muc.backend = "rdbms" # enables MUC support and overrides its backend
```

#### Disable MUC message archive

To disable archive for MUC messages please remove MUC section or any MUC related option from the config file.

### MUC-specific options

#### `modules.mod_mam_meta.muc.host`
* **Syntax:** string
* **Default:** `"conference.@HOST@"`
* **Example:** `modules.mod_mam_meta.muc.host = "conference.@HOST@"`

The MUC host that will be archived if MUC archiving is enabled.

#### Example

The example below presents how to override common option for `muc` module specifically.
Please note that you can override all common options in similar way.

```toml
[modules.mod_mam_meta]
  backend = "rdbms"
  async_writer = true # this option enables async writer for RDBMS backend

  muc.async_writer = false # disable async writer for MUC archive only
```

#### RDBMS backend options

These options will only have effect when the `rdbms` backend is used:

#### `modules.mod_mam_meta.cache_users`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `modules.mod_mam_meta.cache_users = false`

Enables Archive ID to integer mappings cache.

#### `modules.mod_mam_meta.rdbms_message_format`
* **Syntax:** string, one of `"internal"` and `"simple"`
* **Default:** `"internal"`
* **Example:** `modules.mod_mam_meta.rdbms_message_format = "simple"`
* **Warning**: archive MUST be empty to change this option

When set to `simple`, stores messages in XML and full JIDs.
When set to `internal`, stores messages and JIDs in internal format.

#### `modules.mod_mam_meta.async_writer`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `modules.mod_mam_meta.async_writer = false`

Enables an asynchronous writer that is faster than the synchronous one but harder to debug.
The async writers store batches of messages with a certain delay (see **flush_interval**), so the results of the lookup operations executed right after message routing may be incomplete until the configured time passes.

#### `modules.mod_mam_meta.flush_interval`
* **Syntax:** non-negative integer
* **Default:** `2000`
* **Example:** `modules.mod_mam_meta.flush_interval = 2000`

How often (in milliseconds) the buffered messages are flushed to a DB.

#### `modules.mod_mam_meta.max_batch_size`
* **Syntax:** non-negative integer
* **Default:** `30`
* **Example:** `modules.mod_mam_meta.max_batch_size = 30`

Max size of the batch insert query for an async writer.
If the buffer is full, messages are flushed to a database immediately and the flush timer is reset.

#### Common backend options

#### `modules.mod_mam_meta.user_prefs_store`
* **Syntax:** one of `false`, `"rdbms"`, `"cassandra"`, `"mnesia"`
* **Default:** `false`
* **Example:** `modules.mod_mam_meta.user_prefs_store = 30`

Leaving this option as `false` will prevent users from setting their archiving preferences.
It will also increase performance.
The possible values are:

* `"rdbms"` (RDBMS backend only) - User archiving preferences saved in RDBMS. Slow and not recommended, but might be used for simplicity (keeping everything in RDBMS).
* `"cassandra"` (Cassandra backend only) - User archiving preferences are saved in Cassandra.
* `"mnesia"` (recommended) - User archiving preferences saved in Mnesia and accessed without transactions. Recommended in most deployments, could be overloaded with lots of users updating their preferences at once. There's a small risk of an inconsistent (in a rather harmless way) state of the preferences table.

#### `modules.mod_mam_meta.full_text_search`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `modules.mod_mam_meta.full_text_search = false`

Enables full text search in message archive (see *Full Text Search* paragraph).
Please note that the full text search is currently only implemented for `"rdbms"` and `"riak"` backends.
Also, full text search works only for messages archived while this option is enabled.

#### <a id="is_archivable_message"></a>`is_archivable_message/3` callback

`is_archivable_message` option has to name a module exporting `is_archivable_message/3` function conforming to the spec:

```erlang
-spec is_archivable_message(Mod :: module(), Dir :: incoming | outgoing,
                          Packet :: exml:element()) -> boolean().
```

Servers SHOULD NOT archive messages that do not have a `<body/>` child tag. Servers SHOULD NOT archive delayed messages.

By default, all messages that hold meaningful content, rather than state changes such as Chat State Notifications, are archived.

#### Archiving chat markers

Archiving chat markers can be enabled by setting `archive_chat_markers` option to `true`. However it only works if
`is_archivable_message` callback module is set to `mod_mam_utils` or isn't set at all.

When performing full text search chat markers are treated as if they had empty message body.

### Riak backend

The Riak KV backend for MAM stores messages in weekly buckets so it's easier to remove old buckets.
Archive querying is done using Riak KV 2.0 [search mechanism](http://docs.basho.com/riak/2.1.1/dev/using/search/) called Yokozuna.
Your instance of Riak KV must be configured with Yokozuna enabled.

This backend works with Riak KV 2.0 and above, but we recommend version 2.1.1.

##### Riak-specific options

#### `modules.mod_mam_meta.riak.bucket_type`
* **Syntax:** non-empty string
* **Default:** `"mam_yz"`
* **Example:** `modules.mod_mam_meta.riak.bucket_type = "mam_yz"`

Riak bucket type.

#### `modules.mod_mam_meta.riak.search_index`
* **Syntax:** non-empty string
* **Default:** `"mam"`
* **Example:** `modules.mod_mam_meta.riak.search_index = "mam"`

Riak index name.

### Cassandra backend

Please consult [Outgoing connections](../advanced-configuration/outgoing-connections.md#cassandra-connection-setup) page to learn how to properly configure Cassandra connection pool.
By default, `mod_mam` Cassandra backend requires `global` pool with `default` tag:


### ElasticSearch backend

First, make sure that your ElasticSearch cluster has expected indexes and mappings in place.
Please consult [Outgoing connections](../advanced-configuration/outgoing-connections.md#elasticsearch-connection-setup) page to learn how to properly configure ElasticSearch connection pool.

### Example configuration

```toml
[modules.mod_mam_meta]
  backend = "rdbms"
  no_stanzaid_element = true

  pm.user_prefs_store = "rdbms"

  muc.host = "muc.example.com"
  muc.host.rdbms_message_format = "simple"
  muc.host.async_writer = false
  muc.host.user_prefs_store = "mnesia"

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

| Backend action | Description (when it gets incremented) |
| -------------- | ---------------------------------------|
| `lookup` | A lookup in an archive. |
| `archive` | One message is saved in an archive. |
