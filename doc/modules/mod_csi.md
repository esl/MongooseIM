# mod_csi

## Module Description

Enables [XEP-0352: Client State Indication](http://xmpp.org/extensions/xep-0352.html) functionality.

The XEP doesn't **require** any specific server behaviour in response to CSI stanzas, there are only some suggestions.
MongooseIM stores the reported client state in the user's session. Other modules can use this information, for example to send push notifications for `inactive` sessions.

## Options

### `modules.mod_csi.buffer`

By default, `mod_csi` does not buffer stanzas. To enable buffering, configure this section.
When buffering is enabled, MongooseIM buffers packets up to the configured limit while the session is `inactive`, and flushes the buffer when it becomes `active` again.

#### `modules.mod_csi.buffer.max_size`
* **Syntax:** non-negative integer or the string `"infinity"`
* **Default:** `20`
* **Example:** `buffer.max_size = 40`

Maximum number of messages buffered while the session is `inactive`.

## Example Configuration

CSI without buffering:

```toml
[modules.mod_csi]
```

CSI with default buffering:

```toml
[modules.mod_csi]
  buffer = {}
```

CSI with buffering and a custom maximum buffer size:

```toml
[modules.mod_csi]
  buffer.max_size = 40
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
