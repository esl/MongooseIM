Some functionalities in MongooseIM are provided by "services".
A service is similar to a module, but while a module is started for every virtual host and may have global or host-specific configuration, a service is started only once with global configuration.
Service configuration is similar to a module configuration, e.g.:
```
{services, [
            {service_admin_extra, [{submods, [node, accounts, sessions]}]}
           ]
}.
```


## Service list

As of version 2.2, only one module is a "service provider".
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

### Example configuration
` {service_admin_extra, [{submods, [node, accounts, sessions]}]} `
