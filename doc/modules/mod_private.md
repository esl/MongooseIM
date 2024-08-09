## Module Description

This module implements [XEP-0049: Private XML Storage](http://xmpp.org/extensions/xep-0049.html).
It allows users to store custom XML data in the server's database. Used e.g. for storing roster groups separator.

## Options

### `modules.mod_private.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"one_queue"`

Strategy to handle incoming stanzas. For details, please refer to
[IQ processing policies](../configuration/Modules.md#iq-processing-policies).

### `modules.mod_private.backend`
* **Syntax:** string, one of `"mnesia"`, `"rdbms"`.
* **Default:** "mnesia"
* **Example:** `backend = "mnesia"`

Database backend to use.

## Example Configuration
```toml
[modules.mod_private]
  backend = "mnesia"
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
    | `mod_private_Backend_count` | counter | `multi_get_data` | XML data is fetched from a database. |
    | `mod_private_Backend_time`  | histogram | `multi_get_data` | Time to fetch XML data from a database. |
    | `mod_private_Backend_count` | counter | `multi_set_data` | XML data is stored in a database. |
    | `mod_private_Backend_time`  | histogram | `multi_set_data` | Time to store XML data in a database. |

=== "Exometer"

    | Backend action | Type | Description (when it gets incremented) |
    | -------------- | ---- | -------------------------------------- |
    | `[HostType, mod_private_Backend, multi_get_data,  count]` | spiral | XML data is fetched from a database. |
    | `[HostType, mod_private_Backend, multi_get_data,  time]`  | histogram | Time to fetch XML data from a database. |
    | `[HostType, mod_private_Backend, multi_set_data,  count]` | spiral | XML data is stored in a database. |
    | `[HostType, mod_private_Backend, multi_set_data,  time]`  | histogram | Time to store XML data in a database. |
