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

This module provides [backend metrics](../operation-and-maintenance/MongooseIM-metrics.md#backend-metrics).
If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

Prometheus metrics have a `host_type` and `function` labels associated with these metrics.
Since Exometer doesn't support labels, the function as well as the host types, or word `global`, are part of the metric names, depending on the [`instrumentation.exometer.all_metrics_are_global`](../configuration/instrumentation.md#instrumentationexometerall_metrics_are_global) option.

Backend in the action name can be either `rdbms` or `mnesia`.

=== "Prometheus"

    | Backend action | Type | Function | Description (when it gets incremented) |
    | -------------- | ---- | -------- | -------------------------------------- |
    | `mod_private_Backend_count` | counter | `read_roster_version` | Version of a user's roster is retrieved. |
    | `mod_private_Backend_time`  | histogram | `read_roster_version` | Time to retrieve the version of a user's roster. |
    | `mod_private_Backend_count` | counter | `write_roster_version` | Version of a user's roster is stored. |
    | `mod_private_Backend_time`  | histogram | `write_roster_version` | Time to store the version of a user's roster. |
    | `mod_private_Backend_count` | counter | `get_roster` | A user's roster is fetched. |
    | `mod_private_Backend_time`  | histogram | `get_roster` | Time to fetch user's roster. |
    | `mod_private_Backend_count` | counter | `get_roster_entry` | A specific roster entry is fetched. |
    | `mod_private_Backend_time`  | histogram | `get_roster_entry` | Time to fetch a specific roster entry. |
    | `mod_private_Backend_count` | counter | `get_subscription_lists` | A subscription list of a user is retrieved. |
    | `mod_private_Backend_time`  | histogram | `get_subscription_lists` | Time to retrieve a subscription list of a user. |
    | `mod_private_Backend_count` | counter | `roster_subscribe_t` | A subscription status between users is updated inside a transaction. |
    | `mod_private_Backend_time`  | histogram | `roster_subscribe_t` | Time to update a subscription status between users inside a transaction. |
    | `mod_private_Backend_count` | counter | `update_roster_t` | A roster entry is updated in a transaction. |
    | `mod_private_Backend_time`  | histogram | `update_roster_t` | Time to update a roster entry in a transaction. |
    | `mod_private_Backend_count` | counter | `del_roster_t` | A roster entry is removed inside a transaction. |
    | `mod_private_Backend_time`  | histogram | `del_roster_t` | Time to remove a roster entry inside a transaction. |

=== "Exometer"

    | Backend action | Type | Description (when it gets incremented) |
    | -------------- | ---- | -------------------------------------- |
    | `[HostType, mod_private_Backend, read_roster_version, count]` | spiral | Version of a user's roster is retrieved. |
    | `[HostType, mod_private_Backend, read_roster_version, time]`  | histogram | Time to retrieve the version of a user's roster. |
    | `[HostType, mod_private_Backend, write_roster_version, count]` | spiral | Version of a user's roster is stored. |
    | `[HostType, mod_private_Backend, write_roster_version, time]`  | histogram | Time to store the version of a user's roster. |
    | `[HostType, mod_private_Backend, get_roster, count]` | spiral | A user's roster is fetched. |
    | `[HostType, mod_private_Backend, get_roster, time]`  | histogram | Time to fetch user's roster. |
    | `[HostType, mod_private_Backend, get_roster_entry, count]` | spiral | A specific roster entry is fetched. |
    | `[HostType, mod_private_Backend, get_roster_entry, time]`  | histogram | Time to fetch a specific roster entry. |
    | `[HostType, mod_private_Backend, get_subscription_lists, count]` | spiral | A subscription list of a user is retrieved. |
    | `[HostType, mod_private_Backend, get_subscription_lists, time]`  | histogram | Time to retrieve a subscription list of a user. |
    | `[HostType, mod_private_Backend, roster_subscribe_t, count]` | spiral | A subscription status between users is updated inside a transaction. |
    | `[HostType, mod_private_Backend, roster_subscribe_t, time]`  | histogram | Time to update a subscription status between users inside a transaction. |
    | `[HostType, mod_private_Backend, update_roster_t, count]` | spiral | A roster entry is updated in a transaction. |
    | `[HostType, mod_private_Backend, update_roster_t, time]`  | histogram | Time to update a roster entry in a transaction. |
    | `[HostType, mod_private_Backend, del_roster_t, count]` | spiral | A roster entry is removed inside a transaction. |
    | `[HostType, mod_private_Backend, del_roster_t, time]`  | histogram | Time to remove a roster entry inside a transaction. |
