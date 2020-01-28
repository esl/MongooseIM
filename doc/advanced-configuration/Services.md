Some functionalities in MongooseIM are provided by "services".
A service is similar to a module, but while a module is started for every virtual host and may have global or host-specific configuration, a service is started only once with global configuration.
Service configuration is similar to a module configuration, e.g.:
```
{services, [
            {service_admin_extra, [{submods, [node, accounts, sessions]}]},
            {service_mongoose_system_metrics, [report,
                                               {intial_report, 300000},
                                               {periodic_report, 108000000}]}
           ]
}.
```


## Service list

Currently, only two modules are categorised as a "service provider".
Eventually the modules which are not host-specific will be refactored to be services.

### service_admin_extra

Provides additional commands to mongooseimctl script.

#### Options
* `submods` (default: all submodules): List of function groups added by `service_admin_extra`. Allowed elements:
    * `accounts`: Adds `change_password`, `check_password_hash`, `delete_old_users`, `delete_old_users_vhost`, `ban_account`, `num_active_users`, `check_account`, `check_password`
    * `last`: Adds `set_last`
    * `node`: Adds `load_config`, `get_cookie`, `remove_node`
    * `private`: Adds `private_get`, `private_set`
    * `roster`: Adds `add_rosteritem`, `delete_rosteritem`, `process_rosteritems`, `get_roster`, `push_roster`, `push_roster_all`, `push_roster_alltoall`
    * `sessions`: Adds `num_resources`, `resource_num`, `kick_session`, `status_num_host`, `status_num`, `status_list_host`, `status_list`, `connected_users_info`, `connected_users_vhost`, `user_sessions_info`, `set_presence`
    * `stanza`: Adds `send_message_chat`, `send_message_headline`, `send_stanza_c2s`
    * `stats`: Adds `stats`, `stats_host`
    * `vcard`: Adds `get_vcard`, `get_vcard2`, `get_vcard2_multi`, `set_vcard`, `set_vcard2`, `set_vcard2_multi`
    * `gdpr`: Adds `retrieve_personal_data`

#### Example configuration
` {service_admin_extra, [{submods, [node, accounts, sessions]}]} `

### service_mongoose_system_metrics

MongooseIM system metrics are being gathered to analyse the trends and needs of our users, improve MongooseIM, and get to know where to focus our efforts.
See [System Metrics Privacy Policy](../operation-and-maintenance/System-Metrics-Privacy-Policy.md) for more details.

#### Options
* `report` (default: disabled) - Explicit acknowledgement that the metrics are gathered and reported.
Enabling this option is silencing the notification reminder that metrics are gathered.
* `no_report` (default: disabled) - When this option is set, System Metrics Service is not started and metrics are not collected.
Having this option enabled, stops the notification warning that the functionality is not being used.
* `intial_report`:
    * **Description:** Time delay counted when the service is started after which the first metrics report is created and sent.
    * **Syntax:** `{initial_report, Delay}`
    * **Default:** 300000ms (5min).
    * **Example:** `{intial_report, 300000}`
* `periodic_report`:
    * **Description:** Time delay for a periodic update report to be created and sent.
    * **Syntax:**`{periodic_report, Delay}`
    * **Default:** 108000000ms (3h)
    * **Example:** `{periodic_report, 108000000}`
* `tracking_id`:
    * **Description:** Tracking ID to forward the reported metrics so that they can be viewed in the Google Analytics dashboard.
    * **Syntax:**`{tracking_id, TrackingID}`
    * **Default:** disabled
    * **Example:** `{tracking_id, UA-123456789}`

Removing the `service_mongoose_system_metrics` entry from list of services will result in the service not being started.
Metrics will not be collected and shared.
It will generate a notification that the feature is not being used.
The notification can be silenced by setting the `no_report` option explicitly.

#### Example configuration
```
{service_mongoose_system_metrics, [
                                   report,
                                   {intial_report, 300000},
                                   {periodic_report, 108000000}
                                  ]
}
```