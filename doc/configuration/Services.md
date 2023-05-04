Some functionalities in MongooseIM are provided by "services".
A service is similar to a module, but while a module is started for every 
host type and may have global or specific configuration, a service is started 
only once with global configuration.
Currently, three modules are categorised as "service providers".
Eventually the modules which are not specific for a host type will be refactored to be services.

* **Syntax:** Each service is specified in its own `services.*` section. 
* **Default:** None - each service needs to be enabled explicitly.
Typical services are already specified in the example configuration file.
* **Example:** A configuration of the `service_admin_extra` service.

```toml
[services.service_admin_extra]
  submods = ["node", "account", "sessions", "vcard", "gdpr", "upload", "roster",
             "last", "private", "stanza", "stats"]
```

## service_admin_extra

This service provides additional commands to the mongooseimctl script.

!!! Warning
    This service is deprecated.
    The commands are still supported, but they **will be removed** soon.
    You should use the new GraphQL-based command line interface instead.

### `services.service_admin_extra.submods`
* **Syntax:** Array of strings representing function groups added by `service_admin_extra`.
* **Default:** All submodules: `["node", "account", "sessions", "vcard", "gdpr",
 "upload", "roster", "last", "private", "stanza", "stats", "domain"]`
* **Example:** `submods = ["stats", "gdpr"]`

The commands are bundled in the following groups:

* `accounts`: Adds `change_password`, `check_password_hash`, `delete_old_users`,
 `delete_old_users_vhost`, `ban_account`, `num_active_users`, `check_account`,
  `check_password`
* `last`: Adds `set_last`
* `node`: Adds `load_config`, `get_cookie`, `remove_node`
* `private`: Adds `private_get`, `private_set`
* `roster`: Adds `add_rosteritem`, `delete_rosteritem`, `process_rosteritems`,
 `get_roster`, `push_roster`, `push_roster_all`, `push_roster_alltoall`
* `sessions`: Adds `num_resources`, `resource_num`, `kick_session`, `status_num_host`,
 `status_num`, `status_list_host`, `status_list`, `connected_users_info`,
  `connected_users_vhost`, `user_sessions_info`, `set_presence`
* `stanza`: Adds `send_message_chat`, `send_message_headline`, `send_stanza_c2s`
* `stats`: Adds `stats`, `stats_host`
* `vcard`: Adds `get_vcard`, `get_vcard2`, `get_vcard2_multi`, `set_vcard`,
 `set_vcard2`, `set_vcard2_multi`
* `gdpr`: Adds `retrieve_personal_data`
* `upload` : Adds `http_upload`
* `domain` : Adds `insert_domain`, `delete_domain`, `enable_domain`, `disable_domain`

## service_mongoose_system_metrics

MongooseIM system metrics are being gathered to analyse the trends and needs of our users, improve MongooseIM, and get to know where to focus our efforts.
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

## service_domain_db

This service is needed to use the dynamic domains API.
It is used to synchronise dynamic domains between nodes after starting.

### `services.service_domain_db.db_pool`

* **Syntax:** string
* **Default:** `global`
* **Example:** `db_pool = "my_host_type"`

By default, this service uses the RDBMS connection pool configured with the scope `"global"`.
You can put a specific host type there to use the pool with the `"host"` or `"single_host"` scope for that particular host type. See the [outgoing connections docs](outgoing-connections.md) for more information about pool scopes.

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

## Example configuration

```toml
[services.service_admin_extra]
  submods = ["node", "account", "sessions", "vcard", "gdpr", "upload", "roster",
             "last", "private", "stanza", "stats"]

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
