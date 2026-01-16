Some functionalities in MongooseIM are provided by "services".
A service is similar to a module, but while a module is started for every
host type and may have global or specific configuration, a service is started
only once with global configuration.
Currently, three modules are categorised as "service providers".
Eventually the modules which are not specific for a host type will be refactored to be services.

* **Syntax:** Each service is specified in its own `services.*` section.
* **Default:** None - each service needs to be enabled explicitly.
Typical services are already specified in the example configuration file.
* **Example:** A configuration of the `service_domain_db` service.

```toml
[services.service_domain_db]
  event_cleaning_interval = 1000
  event_max_age = 5000
```

## `service_mongoose_system_metrics`

MongooseIM system metrics are being gathered to analyze the trends and needs of our users, improve MongooseIM, and get to know where to focus our efforts.
See [System Metrics Privacy Policy](../operation-and-maintenance/System-Metrics-Privacy-Policy.md) for more details.

### `services.service_mongoose_system_metrics.report`
* **Syntax:** boolean
* **Default:** not specified
* **Example:** `report = true`

An explicit acknowledgement that the metrics are gathered and reported.
When this option is not specified, the reports are gathered, and a notification
appears in logs on startup.
Enabling this option silences the notification reminder that metrics are gathered.
When this option is set to `false`, System Metrics Service is not started and metrics are not collected.

### `services.service_mongoose_system_metrics.intial_report`
* **Syntax:** non-negative integer
* **Default:** `300_000` (milliseconds - 5 minutes).
* **Example:** `intial_report = 300_000`

Time delay counted when the service is started after which the first metrics report is created and sent.

### `services.service_mongoose_system_metrics.periodic_report`
* **Syntax:** non-negative integer
* **Default:** `108_000_000` (milliseconds - 3 hours)
* **Example:** `periodic_report = 108_000_000`

Time delay for a periodic update report to be created and sent.

### `services.service_mongoose_system_metrics.tracking_id.id`:
* **Syntax:** string
* **Default:** no default.
* **Example:** `tracking_id.id = "G-123456789"`

Tracking ID to forward the reported metrics so that they can be viewed in the Google Analytics dashboard.

### `services.service_mongoose_system_metrics.tracking_id.secret`:
* **Syntax:** string
* **Default:** no default.
* **Example:** `tracking_id.secret = "Secret"`

Removing the `services.service_mongoose_system_metrics` entry will result in the service not being started.
Metrics will not be collected and shared.
It will generate a notification that the feature is not being used.
The notification can be silenced by setting the `no_report` option explicitly.

## `service_domain_db`

This service is needed to use the dynamic domains API.
It is used to synchronise dynamic domains between nodes after starting.

### `services.service_domain_db.db_pool`

* **Syntax:** string
* **Default:** `global`
* **Example:** `db_pool = "my_host_type"`

By default, this service uses the RDBMS connection pool configured with the scope `"global"`.
You can put a specific host type there to use the `default` pool with the `host_type` scope for that particular host type. See the [outgoing connections docs](outgoing-connections.md) for more information about pool scopes.

### `services.service_domain_db.event_cleaning_interval`

* **Syntax:** positive integer
* **Default:** `1800` (seconds - 30 minutes)
* **Example:** `event_cleaning_interval = 1800`

The number of seconds between cleaning attempts of the `domain_events` table.

### `services.service_domain_db.event_max_age`

* **Syntax:** positive integer
* **Default:** `7200` (seconds - 2 hours)
* **Example:** `event_max_age = 7200`

The number of seconds after an event must be deleted from the `domain_events` table.

## `service_translations`

Enables translations for system messages.
Support is minimal, you can check `priv/translations/` for translated messages.

## `service_bosh`

This service implements [XEP-0206: XMPP Over BOSH](http://xmpp.org/extensions/xep-0206.html) (using [XEP-0124: Bidirectional-streams Over Synchronous HTTP (BOSH)](http://xmpp.org/extensions/xep-0124.html)),
allowing clients to connect to MongooseIM over regular HTTP long-lived connections.

In order to accept BOSH connections, you will need to configure an HTTP listener with [`mongoose_bosh_handler`](../listeners/listen-http.md#handler-types-bosh-mongoose_bosh_handler) enabled.

## Options

### `services.service_bosh.backend`
* **Syntax:** string: `"mnesia"` or `"cets"`
* **Default:** `"mnesia"`
* **Example:** `backend = "mnesia"`

Backend to use for storing BOSH connections.

!!! Warning
    The corresponding [internal database](../configuration/internal-databases.md) has to be enabled.

### `services.service_bosh.inactivity`
 * **Syntax:** positive integer or the string `"infinity"`
 * **Default:** `30`
 * **Example:** `inactivity = 30`

Maximum allowed inactivity time (in seconds) for a BOSH connection.
Please note that a long-polling request is not considered to be an inactivity.

### `services.service_bosh.max_wait`
 * **Syntax:** positive integer or the string `"infinity"`
 * **Default:** `"infinity"`
 * **Example:** `max_wait = 30`

This is the longest time (in seconds) that the connection manager will wait before responding to any request during the session.

### `services.service_bosh.server_acks`
 * **Syntax:** boolean
 * **Default:** `false`
 * **Example:** `server_acks = true`

Enables/disables [acks](http://xmpp.org/extensions/xep-0124.html#ack-request) sent by server.

### `services.service_bosh.max_pause`
 * **Syntax:** positive integer
 * **Default:** `120`
 * **Example:** `max_pause = 30`

Maximum allowed pause in seconds (e.g. to switch between pages and then resume connection) to request by client-side.

### `services.service_bosh.host_types`
 * **Syntax:** list of strings representing host types or static hosts
 * **Default:** not set
 * **Example:** `host_types = ["localhost", "my host type"]`

When set, allows to limit the BOSH functionality to selected [host types](../configuration/general.md#generalhost_types) or static [hosts](../configuration/general.md#generalhosts).
Connections to non-matching hosts will be refused.
By default, there is no such limit, and any host type is allowed.

### Example Configuration

In the `listen` section:

```toml
[[listen.http]]
  port = 5280
  transport.num_acceptors = 10
  transport.max_connections = 1024

  [[listen.http.handlers.mongoose_bosh_handler]]
    host = "_"
    path = "/http-bind"
```

In the `services` section:

```toml
[services.service_bosh]
  inactivity = 20
  max_wait = "infinity"
  server_acks = true
  max_pause = 120
```

## Example configuration

The example below shows two services configured:

```toml
[services.service_mongoose_system_metrics]
  report = true
  initial_report = 300_000
  periodic_report = 108_000_000
  tracking_id.id = "G-123456789"
  tracking_id.secret = "Secret"

[services.service_domain_db]
  db_pool = "global"
  event_cleaning_interval = 1800
  event_max_age = 7200
```
