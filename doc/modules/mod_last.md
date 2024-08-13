## Module Description

Implements [XEP-0012: Last Activity](https://xmpp.org/extensions/xep-0012.html).

Use with caution, as it was observed that a user disconnect spike might result in overloading the database with "last activity" writes.

## Options

### `modules.mod_last.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"one_queue"`

Strategy to handle incoming stanzas. For details, please refer to
[IQ processing policies](../configuration/Modules.md#iq-processing-policies).

### `modules.mod_last.backend`
* **Syntax:** string, one of `"mnesia"`, `"rdbms"`
* **Default:** `"mnesia"`
* **Example:** `backend = "rdbms"`

Storage backend.

## Example Configuration

```toml
[modules.mod_last]
  backend = "rdbms"
```

## Metrics

This module provides [backend metrics](../operation-and-maintenance/MongooseIM-metrics.md#backend-metrics).
If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

Prometheus metrics have a `host_type` and `function` label associated with these metrics.
Since Exometer doesn't support labels, the function as well as the host types, or word `global`, are part of the metric names, depending on the [`instrumentation.exometer.all_metrics_are_global`](../configuration/instrumentation.md#instrumentationexometerall_metrics_are_global) option.

Backend in the action name can be either `rdbms` or `mnesia`.

=== "Prometheus"

    | Backend action | Type | Function | Description (when it gets incremented) |
    | -------------- | ---- | -------- | -------------------------------------- |
    | `mod_last_Backend_count` | counter | `get_last` | A timestamp is fetched from the database. |
    | `mod_last_Backend_time` | histogram | `get_last` | Time spent fetching a timestamp from the database. |
    | `mod_last_Backend_count` | counter | `set_last_info` |  A timestamp is stored in the database. |
    | `mod_last_Backend_time` | histogram | `set_last_info` | Time spent storing a timestamp in the database. |
    | `mod_last_Backend_count` | counter | `session_cleanup` | A session is cleaned up from the database. |
    | `mod_last_Backend_time` | histogram | `session_cleanup` | Time spent cleaning up a from the database. |

=== "Exometer"

    | Backend action | Type | Description (when it gets incremented) |
    | -------------- | ---- | -------------------------------------- |
    | `[HostType, mod_last_Backend, get_last, count]` | counter | A timestamp is fetched from the database. |
    | `[HostType, mod_last_Backend, get_last, time]` | histogram | Time spent fetching a timestamp from the database. |
    | `[HostType, mod_last_Backend, set_last_info, count]` | counter |  A timestamp is stored in the database. |
    | `[HostType, mod_last_Backend, set_last_info, time]` | histogram | Time spent storing a timestamp in the database. |
    | `[HostType, mod_last_Backend, session_cleanup, count]` | counter | A session is cleaned up from the database. |
    | `[HostType, mod_last_Backend, session_cleanup, time]` | histogram | Time spent cleaning up a from the database. |
