## Module Description

`Inbox` is an experimental feature implemented as a few separate modules.
It is described in detail as our [Open XMPP Extension](../open-extensions/inbox.md).
To use it, enable mod\_inbox in the config file.

## Options

### `modules.mod_inbox.backend`
* **Syntax:** string, one of `"rdbms"`, `"rdbms_async"`
* **Default:** `"rdbms"`
* **Example:** `backend = "rdbms_async"`

Only RDBMS storage is supported, but `rdbms` means flushes to DB are synchronous with each message, while `rdbms_async` is instead asynchronous.

Regular `rdbms` has worse performance characteristics, but it has better consistency properties, as events aren't lost nor reordered. `rdbms_async` processes events asynchronously and potentially unloading a lot of aggregation from the DB. Like the case of the asynchronous workers for MAM, it is the preferred method, with the risk messages being lost on an ungraceful shutdown.

### `modules.mod_inbox.async_writer.pool_size`
* **Syntax:** non-negative integer
* **Default:** `2 * erlang:system_info(schedulers_online)`
* **Example:** `modules.mod_inbox.async_writer.pool_size = 32`

Number of workers in the pool. More than the number of available schedulers is recommended, to minimise lock contention on the message queues, and more than the number of DB workers, to fully utilise the DB capacity. How much more than these two parameters is then a good fine-tuning for specific deployments.

### `modules.mod_inbox.boxes`
* **Syntax:** array of strings.
* **Default:** `[]`
* **Example:** `["classified", "spam"]`

A list of supported inbox boxes by the server. This can be used by clients to classify their inbox entries in any way that fits the end-user. The strings provided here will be used verbatim in the IQ query as described in [Inbox – Filtering and Ordering](../open-extensions/inbox.md#filtering-and-ordering).

!!! note
    `inbox`, `archive`, and `bin` are reserved box names and are always enabled, therefore they don't need to –and must not– be specified in this section. `all` has a special meaning in the box query and therefore is also not allowed as a box name.

    If the asynchronous backend is configured, automatic removals become moves to the `bin` box, also called "Trash bin". This is to ensure eventual consistency. Then the bin can be emptied, either on a [user request](../open-extensions/inbox.md#examples-emptying-the-trash-bin), with the `mongooseimctl inbox` command, through the [GraphQL API](../graphql-api/Admin-GraphQL.md), or through the [REST API](../rest-api/Administration-backend.md).

### `modules.mod_inbox.bin_ttl`
* **Syntax:** non-negative integer, expressed in days.
* **Default:** `30`
* **Example:** `modules.mod_inbox.bin_ttl = 7`

How old entries in the bin can be before the automatic bin cleaner collects them. A value of `7` would mean that entries that have been in the bin for more than 7 days will be cleaned on the next bin collection.

### `modules.mod_inbox.bin_clean_after`
* **Syntax:** non-negative integer, expressed in hours
* **Default:** `1`
* **Example:** `modules.mod_inbox.bin_clean_after = 24`

How often the automatic garbage collection runs over the bin.

### `modules.mod_inbox.delete_domain_limit`

* **Syntax:** non-negative integer or the string `"infinity"`
* **Default:** `"infinity"`
* **Example:** `modules.mod_inbox.delete_domain_limit = 10000`

Domain deletion can be an expensive operation, as it requires to delete potentially many thousands of records from the DB. By default, the delete operation deletes everything in a transaction, but it might be desired, to handle timeouts and table locks more gracefully, to delete the records in batches. This limit establishes the size of the batch.

### `modules.mod_inbox.reset_markers`
* **Syntax:** array of strings, out of `"displayed"`, `"received"`, `"acknowledged"`
* **Default:** `["displayed"]`
* **Example:** `reset_markers = ["received"]`

List of chat markers that when sent, will reset the unread message counter for a conversation.
This works when [Chat Markers](https://xmpp.org/extensions/xep-0333.html) are enabled on the client side.
Setting as empty list (not recommended) means that no chat marker can decrease the counter value.

### `modules.mod_inbox.groupchat`
* **Syntax:** array of strings
* **Default:** `["muclight"]`
* **Example:** `groupchat = ["muclight"]`

The list indicating which groupchats will be included in inbox.
Possible values are `muclight` [Multi-User Chat Light](https://xmpp.org/extensions/inbox/muc-light.html) or `muc` [Multi-User Chat](https://xmpp.org/extensions/xep-0045.html).

### `modules.mod_inbox.aff_changes`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `aff_changes = true`

Use this option when `muclight` is enabled.
Indicates if MUC Light affiliation change messages should be included in the conversation inbox.
Only changes that affect the user directly will be stored in their inbox.

### `modules.mod_inbox.remove_on_kicked`
* **Syntax:** boolean
* **Default:** `true`
* **Example:** `remove_on_kicked = true`

Use this option when `muclight` is enabled.
If true, the inbox conversation is removed for a user when they are removed from the groupchat.
Enabling this option will also clear all inbox entries associated with a destroyed room.

### `modules.mod_inbox.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"no_queue"`

Strategy to handle incoming stanzas. For details, please refer to
[IQ processing policies](../configuration/Modules.md#iq-processing-policies).

### `modules.mod_inbox.max_result_limit`
* **Syntax:** the string `"infinity"` or a positive integer
* **Default:** `"infinity"`
* **Example:** `modules.mod_inbox.max_result_limit = 100`

This option sets the maximum size of returned results when quering inbox.
It works in the same manner as [setting a limit in iq stanza](../open-extensions/inbox.md#limiting-the-query).
The special value `infinity` means no limit.

## Note about supported RDBMS

`mod_inbox` executes upsert queries, which have different syntax in every supported RDBMS.
Inbox currently supports the following DBs:

* MySQL via native driver
* PostgreSQL via native driver

## Legacy MUC support
Inbox comes with support for the legacy MUC as well. It stores all groupchat messages sent to
room in each sender's and recipient's inboxes and private messages. Currently it is not possible to
configure it to store system messages like [subject](https://xmpp.org/extensions/xep-0045.html#enter-subject)
or [affiliation](https://xmpp.org/extensions/xep-0045.html#affil) change.


## Example configuration

```toml
[modules.mod_inbox]
  backend = "rdbms_async"
  reset_markers = ["displayed"]
  aff_changes = true
  remove_on_kicked = true
  groupchat = ["muclight"]
```
