## Module Description

Enables [XEP-0352: Client State Indication](http://xmpp.org/extensions/xep-0352.html) functionality.

The XEP doesn't **require** any specific server behaviour in response to CSI stanzas, there are only some suggestions.
The implementation in MongooseIM will simply buffer all packets (up to a configured limit) when the session is "inactive" and will flush the buffer when it becomes "active" again.

## Options

### `modules.mod_csi.buffer_max`
* **Syntax:** non-negative integer or the string `"infinity"`
* **Default:** `20`
* **Example:** `buffer_max = 40`

Buffer size for messages queued when session was `inactive`.

## Example Configuration

```toml
[modules.mod_csi]
  buffer_max = 40
```

## Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

Prometheus metrics have a `host_type` label associated with these metrics.
Since Exometer doesn't support labels, the host types, or word `global`, are part of the metric names, depending on the [`instrumentation.exometer.all_metrics_are_global`](../configuration/instrumentation.md#instrumentationexometerall_metrics_are_global) option.

=== "Prometheus"

    | Name | Type | Description (when it gets incremented) |
    |------|------|----------------------------------------|
    | `mod_csi_active_count` | counter | A client becomes active. |
    | `mod_csi_inactive_count` | counter | A client becomes inactive. |

=== "Exometer"

    | Name | Type | Description (when it gets incremented) |
    |------|------|----------------------------------------|
    | `[HostType, mod_csi_active, count]` | spiral | A client becomes active. |
    | `[HostType, mod_csi_inactive, count]` | spiral | A client becomes inactive. |
