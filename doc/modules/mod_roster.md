## Module Description

The module implements roster support, specified in [RFC 6121](http://xmpp.org/rfcs/rfc6121.html).
Includes support for [XEP-0237: Roster Versioning](http://xmpp.org/extensions/xep-0237.html).
It can sometimes become quite a heavyweight feature, so there is an option to disable it.

## Options

### `modules.mod_roster.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** "one_queue"

Strategy to handle incoming stanzas. For details, please refer to
[IQ processing policies](../configuration/Modules.md#iq-processing-policies).

### `modules.mod_roster.versioning`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `versioning = true`

Turn on/off support for Roster Versioning.

### `modules.mod_roster.store_current_id`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `store_current_id = true`

Stores the last roster hash in DB (used in Roster Versioning).
Improves performance but should be disabled, when shared rosters are used.

### `modules.mod_roster.backend`
* **Syntax:** string, one of `"mnesia"`, `"rdbms"`
* **Default:** `"mnesia"`
* **Example:** `backend = "mnesia"`

## Example configuration

```toml
[modules.mod_roster]
  versioning = true
  store_current_id = true
```

## Metrics

If you'd like to learn more about metrics in MongooseIM,
please visit [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

| Backend action | Description (when it gets incremented) |
| ---- | -------------------------------------- |
| `read_roster_version` | Version of a user's roster is retrieved. |
| `write_roster_version` | Vversion of a user's roster is stored. |
| `get_roster` | A user's roster is fetched. |
| `get_roster_entry` | A specific roster entry is fetched. |
| `get_roster_entry_t` | A specific roster entry is fetched inside a transaction. |
| `get_subscription_lists` | A subscription list of a user is retrieved. |
| `roster_subscribe_t` | A subscription status between users is updated inside a transaction. |
| `update_roster_t` | A roster entry is updated in a transaction. |
| `del_roster_t` | A roster entry is removed inside a transaction. |
