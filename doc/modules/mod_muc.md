### Module Description
This module implements [XEP-0045: Multi-User Chat](http://xmpp.org/extensions/xep-0045.html) (MUC).
It's a common XMPP group chat solution.
This extension consists of two Erlang modules: `mod_muc` and `mod_muc_room`, the latter being the room code itself.
Note that only `mod_muc` needs to be enabled in the configuration file.
Also `mod_muc_log` is a logging submodule.

### Options
* `host` (string, default: `"conference.@HOST@"`): Subdomain for MUC service to reside under.
 `@HOST@` is replaced with each served domain.
* `backend` (atom, default: `mnesia`): Storage backend. Currently only `mnesia` is supported.
* `access` (atom, default: `all`): Access Rule to determine who is allowed to use the MUC service.
* `access_create` (atom, default: `all`): Who is allowed to create rooms.
* `access_admin` (atom, default: `none`): Who is the administrator in all rooms.
* `access_persistent` (atom, default: `all`): Who is allowed to make the rooms persistent.
 In order to change this parameter, the user must not only match the Access Rule but also be the owner of the room.
* `history_size` (non-negative integer, default: 20): Room message history to be kept in RAM.
 After node restart, the history is lost.
* `room_shaper` (atom, default: `none`): Limits per-room data throughput with traffic shaper.
* `max_room_id` (atom or positive integer, default: `infinite`): Maximum room username length (in JID).
* `max_room_name` (atom or positive integer, default: `infinite`): Maximum room name length.
* `max_room_desc` (atom or positive integer, default: `infinite`): Maximum room description length.
* `min_message_interval` (non-negative integer, default: 0): Minimal interval (in seconds) between messages processed by the room.
* `min_presence_interval` (non-negative integer, default: 0): Minimal interval (in seconds) between presences processed by the room.
* `max_users` (positive integer, default: 200): Absolute maximum user count per room on the node.
* `max_users_admin_threshold` (positive integer, default: 5): When the server checks if a new user can join a room and they are an admin, `max_users_admin_threshold` is added to `max_users` during occupant limit check.
* `user_message_shaper` (atom, default: `none`): Shaper for user messages processed by a room (global for the room).
* `user_presence_shaper` (atom, default: `none`): Shaper for user presences processed by a room (global for the room).
* `max_user_conferences` (non-negative, default: 10): Specifies the number of rooms that a user can occupy simultaneously.
* `http_auth_pool` (atom, default: `none`): If an external HTTP service is chosen to check passwords for password-protected rooms, this option specifies the HTTP pool name to use (see [External HTTP Authentication](#external-http-authentication) below).
* `load_permanent_rooms_at_startup` (boolean, default: false) - Load all rooms at startup (can be unsafe when there are many rooms, that's why disabled).
* `hibernate_timeout` (timeout, default: `90000`): Timeout (in milliseconds) defining the inactivity period after which the room's process should be hibernated.
* `hibernated_room_check_interval` (timeout, default: `infinity`): Interval defining how often the hibernated rooms will be checked (a timer is global for a node).
* `hibernated_room_timeout` (timeout, default: `inifitniy`): A time after which a hibernated room is stopped (deeply hibernated).
 See [MUC performance optimisation](#performance-optimisations).
* `default_room_options` (list of key-value tuples, default: `[]`): List of room configuration options to be overridden in the initial state.
    * `title` (binary, default: `<<>>`): Room title, short free text.
    * `description` (binary, default: `<<>>`): Room description, long free text.
    * `allow_change_subj` (boolean, default: `true`): Allow all occupants to change the room subject.
    * `allow_query_users` (boolean, default: `true`): Allow occupants to send IQ queries to other occupants.
    * `allow_private_messages` (boolean, default: `true`): Allow private messaging between occupants.
    * `allow_visitor_status` (boolean, default: `true`): Allow occupants to use text statuses in presences.
     When disabled, text is removed by the room before broadcasting.
    * `allow_visitor_nickchange` (boolean, default: `true`): Allow occupants to change nicknames.
    * `public` (boolean, default: `true`): Room is included in the list available via Service Discovery.
    * `public_list` (boolean, default: `true`): Member list can be fetched by non-members.
    * `persistent` (boolean, default: `false`): Room will be stored in DB and survive even when the last occupant leaves or the node is restarted.
    * `moderated` (boolean, default: `true`): Only occupants with a "voice" can send group chat messages.
    * `members_by_default` (boolean, default: `true`): All new occupants are members by default, unless they have a different affiliation assigned.
    * `members_only` (boolean, default: `false`): Only users with a member affiliation can join the room.
    * `allow_user_invites` (boolean, default: `false`): Allow ordinary members to send mediated invitations.
    * `allow_multiple_sessions` (boolean, default: `false`): Allow multiple user session to use the same nick.
    * `password_protected` (boolean, default: `false`): Room is protected with a password.
    * `password` (binary, default: `<<>>`): Room password is required upon joining.
     This option has no effect when `password_protected` is `false`.
    * `anonymous` (boolean, default: `true`): Room is anonymous, meaning occupants can't see each others real JIDs, except for the room moderators.
    * `max_users` (positive integer, default: 200): Maximum user count per room.
     Admins and the room owner are not affected.
    * `logging` (boolean, default: `false`): Enables logging of room events (messages, presences) to a file on the disk. Uses `mod_muc_log`.
    * `maygetmemberlist` (list of atoms, default: `[]`): A list of roles and/or privileges that enable retrieving the room's member list.
    * `affiliations` (list of `{{<<"user">>, <<"server">>, <<"resource">>}, affiliation}` tuples, default: `[]`): A default list of affiliations set for every new room.
    * `subject` (binary, default: `<<>>`): A default subject for new room.
    * `subject_author` (binary, default: `<<>>`): A nick name of the default subject's author.


### Example Configuration
```
{mod_muc, [
             {host, "muc.example.com"},
             {access, muc},
             {access_create, muc_create}
            ]},
```

### Performance optimisations

Each room is represented by an Erlang process with its own state and can consume memory even if it's not used.
In large installations with many rooms, this might cause performance issues.
To address that problem MongooseIM has 2 levels of MUC rooms memory optimisations.

#### Room's process hibernation

By default the room's process is hibernated by the Erlang VM 90 seconds after the last activity.
This timeout can be modified by `hibernate_timeout` option.

#### Room deep hibernation

MongooseIM introduces an addtional option of deep hibernation for unused rooms.
This optimisation works only for persistent rooms as only these can be restored on demand.
The improvement works as follows:
1. All room processes are traversed at a chosen `hibernated_room_check_interval`.
1. If a `hibernated_room_timeout` is exceeded, a "stop" signal is sent to a unused room.
1. The room's process is stopped only if there are no online users or if the only one is its owner.
If the owner is online, a presence of a type unavailable is sent to it indicating that the room's process is being terminated.

The room's process can be recreated on demand, for example when a presence sent to it, or the owner wants to add more users to the room.

### External HTTP Authentication

MUC rooms can be protected by a password that is set by the room owner.
Note that MongooseIM supports another custom solution, where each attempt to enter or create a room requires the password to be checked by an external HTTP service.
To enable this option, you need to:

* Configure an [HTTP connection pool](../advanced-configuration/outgoing-connections.md#http-connections-setup).
* Set the name of the connection pool as the value of the `http_auth_pool` option of `mod_muc`.
* Enable the `password_protected` default room option (without setting the password itself).

Whenever a user tries to enter or create a room, the server will receive a GET request to the `check_password` path.
It should return a 200 response with a JSON object `{"code": Code, "msg": Message}` in the response body.
If the server returns something else, an error presence will be sent back to the client.

* `Code` is the status code: 0 indicates a successful authentication, any other value means the authentication failed.
* `Message` is a string containing the message to be sent back to the XMPP client indicating the reason for a failed authentication.
 When authentication succeeds it is ignored and can contain anything ( eg. the string `"OK"`).

**Example:**

```Erlang

{outgoing_pools,
 [{http, global, my_auth_pool,
   [{strategy, available_worker}],
   [{server, "http://my_server:8000"}]}
 ]
}.

{modules, [

  (...)

  {mod_muc, [
             {host, "muc.example.com"},
             {access, muc},
             {access_create, muc_create},
             {http_auth_pool, my_auth_pool},
             {default_room_options, [{password_protected, true}]}
            ]},

  (...)

]}.

```

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[global, mod_muc, deep_hibernations]` | spiral | A room process is stopped (applies only to persistent rooms). |
| `[global, mod_muc, process_recreations]` | spiral | A room process is recreated from a persisted state. |
| `[global, mod_muc, hibernations]` | spiral | A room process becomes hibernated (garbage collected and put in wait state). |
| `[global, mod_muc, hibernated_rooms]` | value | How many rooms are in hibernated state. Does not include rooms in "deep hibernation". |
| `[global, mod_muc, online_rooms]` | value | How many rooms have running processes (includes rooms in a hibernated state). |
