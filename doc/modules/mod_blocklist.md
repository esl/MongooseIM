## Module Description

This module allows administrators to manage the blocklist by adding or removing users. Users included on the blocklist are not allowed to create sessions.
If a user has active sessions and is added to the list, their sessions will be terminated.

The blocklist can be managed with the `mongooseimctl blocklist` command, or through the [GraphQL API](../graphql-api/Admin-GraphQL.md).

## Options

### `modules.mod_blocklist.backend`

* **Syntax:** non-empty string
* **Default:** `"rbdms"`
* **Example:** `backend = "rdbms"`

The blocklist storage backend. Currently only `rdbms` is supported.

## Metrics

This module provides [backend metrics](../operation-and-maintenance/MongooseIM-metrics.md#backend-metrics).
If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

Prometheus metrics have a `host_type` and `function` labels associated with these metrics.
Since Exometer doesn't support labels, the function as well as the host types, or word `global`, are part of the metric names, depending on the [`instrumentation.exometer.all_metrics_are_global`](../configuration/instrumentation.md#instrumentationexometerall_metrics_are_global) option.

Backend in the action name can be only `rdbms`.

=== "Prometheus"

    | Backend action                | Type      | Function         | Description (when it gets incremented)                                    |
    | ----------------------------- | --------- | ---------------- | ------------------------------------------------------------------------- |
    | `mod_blocklist_Backend_count` | counter   | `upsert_block`   | Block entry is added/updated in a backend.                                |
    | `mod_blocklist_Backend_time`  | histogram | `upsert_block`   | Time to store a block entry in a backend.                                 |
    | `mod_blocklist_Backend_count` | counter   | `remove_block`   |  Block entry is removed from a backend.                                   |
    | `mod_blocklist_Backend_time`  | histogram | `remove_block`   | Time to remove a block entry from a backend.                              |

    | Name                          | Type      | Description (when it gets incremented)                                    |
    | ------------------------------| ----------| ------------------------------------------------------------------------- |
    | `mod_blocklist_denied_count`  | counter   | A session of a blocked user gets denied.                                  |

=== "Exometer"

    | Backend action                                             | Type      | Description (when it gets incremented)                                    |
    | ---------------------------------------------------------- |---------- | ------------------------------------------------------------------------- |
    | `[HostType, mod_blocklist_Backend, upsert_block, count]`   | spiral    | Block entry is added/updated in a backend.                                |
    | `[HostType, mod_blocklist_Backend, upsert_block, time]`    | histogram | Time to store a block entry in a backend.                                 |
    | `[HostType, mod_blocklist_Backend, remove_block, count]`   | spiral    | Block entry is removed from a backend.                                    |
    | `[HostType, mod_blocklist_Backend, remove_block, time]`    | histogram | Time to remove a block entry from a backend.                              |

    | Name                                       | Type      | Description (when it gets incremented)                                    |
    | ------------------------------------------ | --------- | ------------------------------------------------------------------------- |
    | `[HostType, mod_blocklist_denied, count]`  | spiral    | A session of a blocked user gets denied.                                  |
