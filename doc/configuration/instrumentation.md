This section is used to configure MongooseIM instrumentation.
It is a system of executing events when something of interest happens in the server.
They are mainly used for the purpose of metrics.

Instrumentation events are acted upon by handlers. Available instrumentation handlers are:

* `prometheus` - collects metrics for the purpose of [Prometheus](https://prometheus.io/). Endpoint to access them has to be configured in the [listener section](../listeners/listen-http.md#handler-types-prometheus-mongoose_prometheus_handler).
* `exometer` - starts [Exometer](https://github.com/esl/exometer_core), a metrics server capable of exporting metrics using reporters. Currently available is a [Graphite](https://graphiteapp.org/) reporter.
* `log` - logs instrumentation events to disk.

Enable them by adding a corresponding sections and possible configuration values.
We recommend choosing either Prometheus or Exometer as a solution for exposing metrics.

## General options

### `instrumentation.probe_interval`
* **Syntax:** positive integer
* **Default:** `15` (seconds)
* **Example:** `probe_interval = 60`

Sets the interval for periodic measurements (probes).

## Exometer options

General options for the Exometer reporter:

### `instrumentation.exometer.all_metrics_are_global`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `all_metrics_are_global = true`

When enabled, all per host type metrics are merged into global equivalents.
The option should be used if you have exceptionally many [host types](../configuration/general.md#generalhost_types) or [static hosts (static domains)](../configuration/general.md#generalhosts).
It is recommended when the number of host types or static domains is in the hundreds, as it significantly reduces CPU usage and (especially) memory footprint in those setups.

## Exometer reporter options

Multiple reporters can be configured.
Because of that, each reporter is configured in a section inside a TOML array, for example: `[[instrumentation.exometer.report.graphite]]`.

### `instrumentation.exometer.report.graphite.interval`
* **Syntax:** positive integer
* **Default:** `60000` (milliseconds)
* **Example:** `interval = 30_000`

Interval at which metrics will be sent to Graphite.

### `instrumentation.exometer.report.graphite.host`
* **Syntax:** string
* **Default:** no default, required
* **Example:** `host = "graphite.local"`

The name or IP address of the Graphite server.
This option is mandatory.

### `instrumentation.exometer.report.graphite.port`
* **Syntax:** integer, between 0 and 65535
* **Default:** `2003`
* **Example:** `port = 2033`

The port on which the Graphite server listens for connections.

### `instrumentation.exometer.report.graphite.connect_timeout`
* **Syntax:** positive integer
* **Default:** `5000` (milliseconds)
* **Example:** `connect_timeout = 10_000`

The amount of time Graphite reporter will wait before timing out.

### `instrumentation.exometer.report.graphite.api_key`
* **Syntax:** string
* **Default:** `""`
* **Example:** `api_key = "hosted_graphite_api_key"`

API key to use when reporting to a hosted graphite server.

### `instrumentation.exometer.report.graphite.prefix`
* **Syntax:** string
* **Default:** no default
* **Example:** `prefix = "mim_stats"`

A prefix to prepend all metric names with before they are sent to the graphite server.

### `instrumentation.exometer.report.graphite.env_prefix`
* **Syntax:** string
* **Default:** no default
* **Example:** `env_prefix = "GRAPHITE_METRICS_PREFIX"`

Specifies an environmental variable name from which an additional prefix will be taken.
In case both `prefix` and `env_prefix` are defined, it will be placed before the `prefix` and separated with a dot.

## Log handler options

### `instrumentation.log.level`
* **Syntax:** string, one of `"none"`, `"emergency"`, `"alert"`, `"critical"`, `"error"`, `"warning"`, `"notice"`, `"info"`, `"debug"`, `"all"`.
* **Default:** `"debug"`
* **Example:** `loglevel = "error"`

Severity level at which all the events will be logged.

!!! note
    
    In order for instrumentation events to appear in logs, the [`general.loglevel` option](../configuration/general.md#generalloglevel) has to be set to the same or lower level.

## Example Prometheus configuration

This configuration enables `prometheus`, and `log` handlers:
```toml
[instrumentation]
  probe_interval = 10_000

[instrumentation.prometheus]

[instrumentation.log]
```

## Example Exometer configuration

This configuration enables `exometer` handler with two different Graphite reporters.
```toml
[[instrumentation.exometer.report.graphite]]
  host = "127.0.0.1"
  interval = 15_000
  prefix = "mongooseim"
  connect_timeout = 5000

[[instrumentation.exometer.report.graphite]]
  host = "hosted_graphite.com"
  prefix = "mim"
```
