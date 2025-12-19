## Module Description

This module implements [XEP-0313: Message Archive Management](https://xmpp.org/extensions/xep-0313.html).
It enables a service to store all user messages for one-to-one chats as well as group chats (MUC, MultiUser Chat).
It uses [XEP-0059: Result Set Management](http://xmpp.org/extensions/xep-0059.html) for paging.
It is a highly customizable module, that requires some skill and knowledge to operate properly and efficiently.

MongooseIM is compatible with MAM 0.4-1.1.0.

Configure MAM with different storage backends:

* RDBMS (databases like MySQL, PostgreSQL, CockroachDB)
* Cassandra (NoSQL)
* ElasticSearch (NoSQL)

`mod_mam` is a meta-module that ensures all relevant `mod_mam_*` modules are loaded and properly configured.

### Message retraction
This module supports [XEP-0424: Message Retraction](http://xmpp.org/extensions/xep-0424.html) with RDBMS storage backends. When a [retraction message](https://xmpp.org/extensions/xep-0424.html#example-4) is received, the MAM module finds the message to retract and replaces it with a tombstone.

The following criteria are used to find the original message:

* The `id` attribute specified in the `apply-to` element of the retraction message has to be the same as the `id` attribute of the `origin-id` (or `stanza-id` when configured, see [below](#retraction-on-the-stanza-id)) element of the original message.
* Both messages need to originate from the same user.
* Both messages need to be addressed to the same user.

If more than one message matches the criteria, only the most recent one is retracted. To avoid this case, it is recommended to use a unique identifier (UUID) as the origin ID.

#### Retraction on the stanza-id
This module also implements an extension to the XEP, where it allows to specify the [`stanza-id`](https://xmpp.org/extensions/xep-0359.html#stanza-id) as [created by](https://xmpp.org/extensions/xep-0313.html#archives_id) the server's MAM, instead of the `origin-id` that the original [XEP-0424](https://xmpp.org/extensions/xep-0424.html) specifies. It announces this capability under the namespace `urn:esl:message-retract-by-stanza-id:0`. This is specially useful in groupchats where the `stanza-id` of a message is shared and known for all participants.

In this case, to use such functionality,
```xml
<apply-to id="origin-id-1" xmlns="urn:xmpp:fasten:0">
  <retract xmlns='urn:xmpp:message-retract:0'/>
</apply-to>
```
turns into
```xml
<apply-to id="stanza-id-1" xmlns="urn:xmpp:fasten:0">
  <retract xmlns='urn:esl:message-retract-by-stanza-id:0'/>
</apply-to>
```
and likewise, the answer would be tagged by the mentioned `esl` namespace.

### Full Text Search
This module allows message filtering by their text body (if enabled, see *Common backend options*).
This means that an XMPP client, while requesting messages from the archive may not only specify standard form fields (`with`, `start`, `end`), but also `full-text-search` (of type `text-single`).
If this happens, the client will receive only messages that contain words specified in the request.

The exact behaviour, like whether word ordering matters, may depend on the storage backend in use.
For now `rdbms` backend has very limited support for this feature, while `cassandra` does not support it at all.
`elasticsearch` backend, on the other hand, should provide you with the best results when it comes to text filtering.

`mod_mam_rdbms_arch` returns all messages that contain all search words, order
of words does not matter. Messages are sorted by timestamp (not by relevance).

#### Note on full text search with ElasticSearch backend

When using ElasticSearch MAM backend, the value provided in `full-text-search` form field will be passed to ElasticSearch as [Simple Search Query](https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-simple-query-string-query.html).
If you're using our official ElasticSearch mappings from `priv/elasticsearch` then the query analyzer is set to `english`.
Also note that the default separator for the search query is `AND` (which roughly means that ElasticSearch will search for messages containing all the terms provided in the query string).

## Options

### `modules.mod_mam.backend`
* **Syntax:** string, one of `"rdbms"`, `"cassandra"` and `"elasticsearch"`
* **Default:** `"rdbms"`
* **Example:** `backend = "elasticsearch"`

Database backend to use.

### `modules.mod_mam.no_stanzaid_element`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `no_stanzaid_element = true`

Do not add a `<stanza-id/>` element from MAM v1.1.0.

### `modules.mod_mam.is_archivable_message`
* **Syntax:** non-empty string
* **Default:** `"mod_mam_utils"`
* **Example:** `is_archivable_message = "mod_mam_utils"`

Name of a module implementing [`is_archivable_message/3` callback](#is_archivable_message) that determines if the message should be archived.

### `modules.mod_mam.send_message`
* **Syntax:** non-empty string
* **Default:** `"mod_mam_utils"`
* **Example:** `send_message = "mod_mam_utils"`

Name of a module implementing `send_message/4` callback that routes a message during lookup operation.
Consult with `mod_mam_utils:send_message/4` code for more information.

Check `big_tests/tests/mam_send_message_SUITE_data/mam_send_message_example.erl` file
in the MongooseIM repository for the usage example.

### `modules.mod_mam.archive_chat_markers`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `archive_chat_markers = true`

If set to true, XEP-0333 chat markers will be archived.
See more details [here](#archiving-chat-markers).

### `modules.mod_mam.message_retraction`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `message_retraction = false`

Enables [XEP-0424: Message Retraction](http://xmpp.org/extensions/xep-0424.html).
This functionality is currently implemented only for the `rdbms` backend.
[Retraction messages](https://xmpp.org/extensions/xep-0424.html#example-4) are always archived regardless of this option.

**backend**, **no_stanzaid_element**, **is_archivable_message** and **message_retraction** will be applied to both `pm` and `muc` (if they are enabled), unless overridden explicitly (see example below).

### Enable one-to-one message archive

Archive for one-to-one messages can be enabled in one of two ways:

* Specify `[mod_mam.pm]` section

```toml
[modules.mod_mam]
[modules.mod_mam.pm] # defining this section enables PM support
```

* Define any PM related option

```toml
[modules.mod_mam]
  pm.backend = "rdbms" # enables PM support and overrides its backend
```

### Disable one-to-one message archive

To disable archive for one-to-one messages please remove PM section or any PM related option from the config file.

### PM-specific options

#### `modules.mod_mam.pm.archive_groupchats`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `modules.mod_mam.pm.archive_groupchats = true`

When enabled, MAM will store groupchat messages in recipients' individual archives. **USE WITH CAUTION!** May increase archive size significantly. Disabling this option for existing installation will neither remove such messages from MAM storage, nor will filter out them from search results. Clients can use `include-groupchat` filter to filter out groupchat messages while querying the archive.

!!! Warning
    The `include-groupchat` filter doesn't work for Cassandra backend.

#### `modules.mod_mam.pm.same_mam_id_for_peers`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `modules.mod_mam.pm.same_mam_id_for_peers = true`

When enabled, MAM will set the same MAM ID for both sender and recipient. This can be useful in combination with [retraction on the stanza-id](#retraction-on-the-stanza-id). Note that this might not work with clients across federation, as the recipient might not implement the same retraction, nor the same IDs.

### Enable MUC message archive

Archive for MUC messages can be enabled in one of two ways:

* Specify `[mod_mam.muc]` section

```toml
[modules.mod_mam]
[modules.mod_mam.muc] # defining this section enables MUC support
```

* Define any MUC related option

```toml
[modules.mod_mam]
  muc.backend = "rdbms" # enables MUC support and overrides its backend
```

### Disable MUC message archive

To disable archive for MUC messages please remove MUC section or any MUC related option from the config file.

### MUC-specific options

#### `modules.mod_mam.muc.host`
* **Syntax:** string
* **Default:** `"conference.@HOST@"`
* **Example:** `modules.mod_mam.muc.host = "conference.@HOST@"`

The MUC host that will be archived if MUC archiving is enabled.

!!! Warning
    If you are using MUC Light, make sure this option is set to the MUC Light domain

#### Example

The example below presents how to override common option for `muc` module specifically.
Please note that you can override all common options (except `cache`) in a similar way.

```toml
[modules.mod_mam]
  backend = "rdbms"
  async_writer.enabled = true # this option enables async writer for RDBMS backend
  muc.async_writer.enabled = false # disable async writer for MUC archive only
```

### RDBMS backend options

These options will only have effect when the `rdbms` backend is used:

#### `modules.mod_mam.cache_users`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `modules.mod_mam.cache_users = false`

Enables Archive ID to integer mappings cache.

If caching is enabled, by default it will spawn its own [segmented cache](https://github.com/esl/segmented_cache) cache, with defaults as in [`mod_cache_users`](./mod_cache_users.md). To change these defaults, the same config can be accessed within the `cache` key. To see details about the meaning of each flag, see [`mod_cache_users`](./mod_cache_users.md). To reuse the cache already created by `mod_cache_users`, see the option below.

```toml
modules.mod_mam.cache.strategy
modules.mod_mam.cache.time_to_live
modules.mod_mam.cache.number_of_segments
```

#### `modules.mod_mam.cache.module`
* **Syntax:** string, one of `"mod_cache_users"` or `"internal"`
* **Default:** `internal`
* **Example:** `modules.mod_mam.cache.module = "mod_cache_users"`

Configures which cache to use, either start an internal instance, or reuse the cache created by `mod_cache_users`, if such module was enabled. Note that if reuse is desired – that is, `cache.module = "mod_cache_users"`, other cache configuration parameters will be ignored.

#### `modules.mod_mam.async_writer.enabled`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `modules.mod_mam.async_writer.enabled = false`

Enables an asynchronous writer that is faster than the synchronous one but harder to debug.
The async writers store batches of messages that will be flush on a timeout (see **flush_interval**) or when the batch reaches a size (see **batch_size**), so the results of the lookup operations executed right after message routing may be incomplete until the configured time passes or the queue is full.

#### `modules.mod_mam.async_writer.flush_interval`
* **Syntax:** non-negative integer
* **Default:** `2000`
* **Example:** `modules.mod_mam.async_writer.flush_interval = 2000`

How often (in milliseconds) the buffered messages are flushed to DB.

#### `modules.mod_mam.async_writer.batch_size`
* **Syntax:** non-negative integer
* **Default:** `30`
* **Example:** `modules.mod_mam.async_writer.batch_size = 30`

Max size of the batch for an async writer before the queue is considered full and flushed.
If the buffer is full, messages are flushed to a database immediately and the flush timer is reset.

#### `modules.mod_mam.async_writer.pool_size`
* **Syntax:** non-negative integer
* **Default:** `4 * erlang:system_info(schedulers_online)`
* **Example:** `modules.mod_mam.async_writer.pool_size = 32`

Number of workers in the pool. More than the number of available schedulers is recommended, to minimise lock contention on the message queues, and more than the number of DB workers, to fully utilise the DB capacity. How much more than these two parameters is then a good fine-tuning for specific deployments.

### Common backend options

#### `modules.mod_mam.user_prefs_store`
* **Syntax:** one of `"rdbms"`, `"cassandra"`, `"mnesia"`
* **Default:** not set
* **Example:** `modules.mod_mam.user_prefs_store = "rdbms"`

Leaving this option unset will prevent users from setting their archiving preferences.
It will also increase performance.
The possible values are:

* `"rdbms"` (RDBMS backend only) - User archiving preferences saved in RDBMS. Slow and not recommended, but might be used for simplicity (keeping everything in RDBMS).
* `"cassandra"` (Cassandra backend only) - User archiving preferences are saved in Cassandra.
* `"mnesia"` (recommended) - User archiving preferences saved in Mnesia and accessed without transactions. Recommended in most deployments, could be overloaded with lots of users updating their preferences at once. There's a small risk of an inconsistent (in a rather harmless way) state of the preferences table.

#### `modules.mod_mam.full_text_search`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `modules.mod_mam.full_text_search = false`

Enables full text search in message archive (see *Full Text Search* paragraph).
Please note that the full text search is currently only implemented for `"rdbms"` backend.
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

### Cassandra backend

Please consult [Outgoing connections](../configuration/outgoing-connections.md#cassandra-options) page to learn how to properly configure Cassandra connection pool.
By default, `mod_mam` Cassandra backend requires `global` pool with `default` tag.


### ElasticSearch backend

First, make sure that your ElasticSearch cluster has expected indexes and mappings in place.
Please consult [Outgoing connections](../configuration/outgoing-connections.md#elasticsearch-options) page to learn how to properly configure ElasticSearch connection pool.

### Low-level options

These options allow for fine-grained control over MAM behaviour.

#### `modules.mod_mam.default_result_limit`
* **Syntax:** non-negative integer
* **Default:** `50`
* **Example:** `modules.mod_mam.default_result_limit = 100`

This sets the default page size of returned results.

#### `modules.mod_mam.max_result_limit`
* **Syntax:** non-negative integer
* **Default:** `50`
* **Example:** `modules.mod_mam.max_result_limit = 100`

This sets the maximum page size of returned results.

#### `modules.mod_mam.enforce_simple_queries`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `modules.mod_mam.enforce_simple_queries = true`

This enforces all mam lookups to be "simple", i.e., they skip the RSM count.
See [Message Archive Management extensions](../open-extensions/mam.md).

#### `modules.mod_mam.delete_domain_limit`

* **Syntax:** non-negative integer or the string `"infinity"`
* **Default:** `"infinity"`
* **Example:** `modules.mod_mam.delete_domain_limit = 10000`

Domain deletion can be an expensive operation, as it requires to delete potentially many thousands of records from the DB. By default, the delete operation deletes everything in a transaction, but it might be desired, to handle timeouts and table locks more gracefully, to delete the records in batches. This limit establishes the size of the batch.

#### `modules.mod_mam.db_jid_format`

* **Syntax:** string, one of `"mam_jid_rfc"`, `"mam_jid_rfc_trust"`, `"mam_jid_mini"` or a module implementing `mam_jid` behaviour
* **Default:** `"mam_jid_rfc"` for MUC archive, `"mam_jid_mini"` for PM archive
* **Example:** `modules.mod_mam.db_jid_format = "mam_jid_mini"`

Sets the internal MAM jid encoder/decoder module for RDBMS.

!!! Warning
    Archive MUST be empty to change this option

#### `modules.mod_mam.db_message_format`

* **Syntax:** string, one of `"mam_message_xml"`, `"mam_message_eterm"`, `"mam_message_compressed_eterm"` or a module implementing `mam_message` behaviour
* **Default:** `"mam_message_compressed_eterm"` for RDBMS, `"mam_message_xml"` for Cassandra
* **Example:** `modules.mod_mam.db_message_format = "mam_message_compressed_eterm"`

Sets the internal MAM message encoder/decoder module.

!!! Warning
    Archive MUST be empty to change this option

#### `modules.mod_mam.extra_fin_element`

* **Syntax:** string, a module implementing the `extra_fin_element/3` callback
* **Default:** none
* **Example:** `modules.mod_mam.extra_fin_element = "example_mod"`

This module can be used to add subelements to the `<fin>` element of the MAM lookup query response.
It can be useful to be able to add information to a mam query, that doesn't belong to any specific message but to all of them.

#### `modules.mod_mam.extra_lookup_params`

* **Syntax:** string, a module implementing the `extra_lookup_params/2` callback
* **Default:** none
* **Example:** `modules.mod_mam.extra_lookup_params = "example_mod"`

This module can be used to add extra lookup parameters to MAM lookup queries.

## Example configuration

```toml
[modules.mod_mam]
  backend = "rdbms"
  no_stanzaid_element = true

  pm.user_prefs_store = "rdbms"

  muc.host = "muc.example.com"
  muc.db_message_format = "mam_message_xml"
  muc.async_writer.enabled = false
  muc.user_prefs_store = "mnesia"

```

## Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

Prometheus metrics have a `host_type` label associated with these metrics.
Since Exometer doesn't support labels, the host types, or word `global`, are part of the metric names, depending on the [`instrumentation.exometer.all_metrics_are_global`](../configuration/instrumentation.md#instrumentationexometerall_metrics_are_global) option.

=== "Prometheus"

    | Name | Type | Description (when it gets incremented) |
    | ---- | ---- | -------------------------------------- |
    | `mod_mam_pm_archive_message_count` | counter | A PM message is stored in user's archive. |
    | `mod_mam_pm_archive_message_time` | histogram | Time it took to archive a user's message. |
    | `mod_mam_pm_lookup_count` | counter | Any MAM PM lookup is performed. |
    | `mod_mam_pm_lookup_simple` | counter | A simple MAM PM lookup is performed. |
    | `mod_mam_pm_lookup_size` | histogram | MAM PM lookup size (row count). |
    | `mod_mam_pm_lookup_time` | histogram | MAM PM lookup time. |
    | `mod_mam_pm_dropped_iq_count` | counter | MAM IQ has been dropped due to: high query frequency/invalid syntax or type. |
    | `mod_mam_pm_dropped_count` | counter | A message couldn't be stored in the DB (and got dropped). |
    | `mod_mam_pm_remove_archive_count` | counter | User's PM archive is removed. |
    | `mod_mam_pm_get_prefs_count` | counter | Archiving preferences have been requested by a client. |
    | `mod_mam_pm_set_prefs_count` | counter | Archiving preferences have been updated by a client. |
    | `mod_mam_muc_archive_message_count` | counter | A message is stored in room's archive. |
    | `mod_mam_muc_archive_message_time` | histogram | Time it took to archive a MUC message. |
    | `mod_mam_muc_lookup_count` | counter | Any MAM lookup in MUC room is performed. |
    | `mod_mam_muc_lookup_size` | histogram | MAM MUC lookup size (row count). |
    | `mod_mam_muc_lookup_time` | histogram | MAM MUC lookup time. |
    | `mod_mam_muc_dropped_iq_count` | counter | MAM MUC IQ has been dropped due to: high query frequency/invalid syntax or type. |
    | `mod_mam_muc_dropped_count` | counter | A MUC message couldn't be stored in the DB (and got dropped). |
    | `mod_mam_muc_remove_archive_count` | counter | Room's entire archive is removed. |
    | `mod_mam_muc_get_prefs_count` | counter | MUC archiving preferences have been requested by a client. |
    | `mod_mam_muc_set_prefs_count` | counter | MUC archiving preferences have been updated by a client. |
    | `mod_mam_pm_flushed_count` | counter | Async MAM worker flushed buffered messages. |
    | `mod_mam_pm_flushed_time` | histogram | Average time per flush of all buffered messages measured in an async MAM worker. |
    | `mod_mam_pm_flushed_time_per_message` | histogram | Average time per message insert measured in an async MAM worker. |
    | `mod_mam_muc_flushed_count` | counter | Async MUC MAM worker flushed buffered messages. |
    | `mod_mam_muc_flushed_time` | histogram | Average time per flush of all buffered messages measured in an async MUC MAM worker. |
    | `mod_mam_muc_flushed_time_per_message` | histogram | Average time per message insert measured in an async MUC MAM worker. |

=== "Exometer"

    | Name | Type | Description (when it gets incremented) |
    | ---- | ---- | -------------------------------------- |
    | `[HostType, mod_mam_pm_archive_message, count]` | spiral | A PM message is stored in user's archive. |
    | `[HostType, mod_mam_pm_archive_message, time]` | histogram | Time it took to archive a user's message. |
    | `[HostType, mod_mam_pm_lookup, count]` | spiral | Any MAM PM lookup is performed. |
    | `[HostType, mod_mam_pm_lookup_simple` | spiral | A simple MAM PM lookup is performed. |
    | `[HostType, mod_mam_pm_lookup, size]` | histogram | MAM PM lookup size (row count). |
    | `[HostType, mod_mam_pm_lookup, time]` | histogram | MAM PM lookup time. |
    | `[HostType, mod_mam_pm_dropped_iq, count]` | spiral | MAM IQ has been dropped due to: high query frequency/invalid syntax or type. |
    | `[HostType, mod_mam_pm_dropped, count]` | spiral | A message couldn't be stored in the DB (and got dropped). |
    | `[HostType, mod_mam_pm_remove_archive, count]` | spiral | User's PM archive is removed. |
    | `[HostType, mod_mam_pm_get_prefs, count]` | spiral | Archiving preferences have been requested by a client. |
    | `[HostType, mod_mam_pm_set_prefs, count]` | spiral | Archiving preferences have been updated by a client. |
    | `[HostType, mod_mam_muc_archive_message, count]` | spiral | A message is stored in room's archive. |
    | `[HostType, mod_mam_muc_archive_message, time]` | histogram | Time it took to archive a MUC message. |
    | `[HostType, mod_mam_muc_lookup, count]` | spiral | Any MAM lookup in MUC room is performed. |
    | `[HostType, mod_mam_muc_lookup, size]` | histogram | MAM MUC lookup size (row count). |
    | `[HostType, mod_mam_muc_lookup, time]` | histogram | MAM MUC lookup time. |
    | `[HostType, mod_mam_muc_dropped_iq, count]` | spiral | MAM MUC IQ has been dropped due to: high query frequency/invalid syntax or type. |
    | `[HostType, mod_mam_muc_dropped, count]` | spiral | A MUC message couldn't be stored in the DB (and got dropped). |
    | `[HostType, mod_mam_muc_remove_archive, count]` | spiral | Room's entire archive is removed. |
    | `[HostType, mod_mam_muc_get_prefs, count]` | spiral | MUC archiving preferences have been requested by a client. |
    | `[HostType, mod_mam_muc_set_prefs, count]` | spiral | MUC archiving preferences have been updated by a client. |
    | `[HostType, mod_mam_pm_flushed, count]` | spiral | Async MAM worker flushed buffered messages. |
    | `[HostType, mod_mam_pm_flushed, time]` | histogram | Average time per flush of all buffered messages measured in an async MAM worker. |
    | `[HostType, mod_mam_pm_flushed, time_per_message]` | histogram | Average time per message insert measured in an async MAM worker. |
    | `[HostType, mod_mam_muc_flushed, count]` | spiral | Async MUC MAM worker flushed buffered messages. |
    | `[HostType, mod_mam_muc_flushed, time]` | histogram | Average time per flush of all buffered messages measured in an async MUC MAM worker. |
    | `[HostType, mod_mam_muc_flushed, time_per_message]` | histogram | Average time per message insert measured in an async MUC MAM worker. |
