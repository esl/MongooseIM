## Introduction

!!! Warning
    This API is considered obsolete.
    Please use [WombatOAM](https://www.erlang-solutions.com/capabilities/wombatoam/) for monitoring or one of the [exometer reporters](../operation-and-maintenance/Logging-&-monitoring.md#monitoring) and your favourite statistics service.

To expose MongooseIM metrics, an adequate endpoint must be included in the [listen](../configuration/listen.md)
section of `mongooseim.toml`. The specific configuration options are described in
the [metrics API handlers](../configuration/listen.md#handler-types-metrics-api-obsolete-mongoose_api)
section.

An example configuration:

```toml
[[listen.http]]
  port = 5288
  transport.num_acceptors = 5
  transport.max_connections = 10  

  [[listen.http.handlers.mongoose_api]]
    host = "localhost"
    path = "/api"
    handlers = ["mongoose_api_metrics"]
```

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

### Security notice

An auth mechanism is available only for the new administration API.
That's why we recommend to expose this API only using a private interface or a port hidden behind a firewall to limit the access to the API.
The above configuration starts the API only on a loopback interface.

## Response format

The responses are composed in a JSON format with a root element containing one or more attributes as response elements.

Example response:
```json
{
    "hosts": [
        "localhost"
    ],
    "metrics": [
        "xmppErrorIq",
        "xmppPresenceReceived",
        "xmppMessageBounced",
        (...)
    ],
    "global": [
        "nodeSessionCount",
        "totalSessionCount",
        "uniqueSessionCount",
        (...)
    ]
}
```

## Services

### GET /api/metrics

Returns ```200 OK``` and two elements:

* `host_types` - A list of host type names available on the server.
* `metrics` - A list of per-host metrics.
* `global` - A list of global metrics.

### GET /api/metrics/all

Returns ```200 OK``` and an element:

* `metrics` - A list of aggregated (sum of all domains) per host type metrics with their values.

### GET /api/metrics/all/:metric

On success returns ```200 OK``` and an element:

* `metric` - An aggregated (sum of all domains) per host type metric.

Returns ```404 Not Found``` when metric `:metric` doesn't exist.

### GET /api/metrics/host_type/:host_type

On success returns ```200 OK``` and an element:

* `metrics` - A list of per host type metrics and their values for host_type `:host_type`.

Returns ```404 Not Found``` when host_type `:host_type` doesn't exist.

### GET /api/metrics/host_type/:host_type/:metric

On success returns ```200 OK``` and an element:

* `metric` - A per host type metric `:metric` and its value for host_type `:host_type`.

Returns ```404 Not Found``` when the pair (host_type `:host_type`, metric `:metric`) doesn't exist.

### GET /api/metrics/global

On success returns ```200 OK``` and an element:

* `metrics` - A list of all global metrics and their values.

### GET /api/metrics/global/:metric

On success returns ```200 OK``` and an element:

* `metric` - A global metric `:metric` and its value.

Returns ```404 Not Found``` when metric `:metric` doesn't exist.

## collectd integration

The interface is compatible with the collectd curl_json plugin.
Data fetched by collectd may be later visualized by tools like Graphite.

Here's an example of a collectd configuration entry that will fetch all available metrics for a given host:
```json
LoadPlugin curl_json
...
<Plugin curl_json>
    <URL "http://<MONGOOSEIM HOST>:<MONGOOSEIM HTTP LISTENER PORT>/api/metrics/host/<XMPP HOST>">
        Instance "mongooseim"
        <Key "metrics/sessionCount/value">
            Type "absolute"
        </Key>
        <Key "metrics/*/count">
            Type "absolute"
        </Key>
        <Key "metrics/*/one">
            Type "absolute"
        </Key>
    </URL>
</Plugin>
```
