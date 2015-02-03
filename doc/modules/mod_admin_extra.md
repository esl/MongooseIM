### Module Description
This module significantly extends `mongooseimctl` script capabilities. New functionalities appear as additional commands. For detailed description, just run `mongooseimctl` script with no parameters.

### Options
* **submods** - List of function groups added by `mod_admin_extra`. Allowed elements:
    * `node` - Adds: load_config, get_cookie, remove_node
    * `accounts` - Adds: change_password, check_password_hash, delete_old_users, delete_old_users_vhost, ban_account, num_active_users, check_account, check_password
    * `sessions` - Adds: num_resources, resource_num, kick_session, status_num_host, status_num, status_list_host, status_list, connected_users_info, connected_users_vhost, user_sessions_info, set_presence
    * `vcard` - Adds: get_vcard, get_vcard2, get_vcard2_multi, set_vcard, set_vcard2, set_vcard2_multi
    * `roster` - Adds: add_rosteritem, delete_rosteritem, process_rosteritems, get_roster, push_roster, push_roster_all, push_roster_alltoall
    * `last` - Adds: set_last
    * `private` - Adds: private_get, private_set
    * `stanza` - Adds: send_message_chat, send_message_headline, send_stanza_c2s
    * `stats` - Adds: stats, stats_host

### Example configuration
` {mod_admin_extra, [{submods, [node, accounts, sessions]}]} `