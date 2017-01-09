### Module Description
This module implements [XEP-0045: Multi-User Chat)](http://xmpp.org/extensions/xep-0045.html) (MUC). It's a common XMPP group chat solution. This extension consists of two Erlang modules: `mod_muc` and `mod_muc_room`, the latter being the room code itself. Only `mod_muc` needs to be enabled in configuration file though. Also  `mod_muc_log` is a logging submodule.

### Options
* `host` (string, default: `"conference.@HOST@"`): Subdomain for MUC service to reside under. `@HOST@` is replaced with each served domain.
* `access` (atom, default: `all`): Access Rule to determine who is allowed to use the MUC service.
* `access_create` (atom, default: `all`): Who is allowed to create rooms.
* `access_admin` (atom, default: `none`): Who is the administrator in all rooms.
* `access_persistent` (atom, default: `all`): Who is allowed to make the rooms persistent. In order to change this parameter, the user must not only match the Access Rule but also be the owner of the room.
* `history_size` (non-negative integer, default: 20): Room message history to be kept in RAM. After node restart, the history is lost.
* `room_shaper` (atom, default: `none`): Limits per-room data throughput with traffic shaper.
* `max_room_id` (atom or positive integer, default: `infinite`): Maximum room username length (in JID).
* `max_room_name` (atom or positive integer, default: `infinite`): Maximum room name length.
* `max_room_desc` (atom or positive integer, default: `infinite`): Maximum room description length.
* `min_message_interval` (non-negative integer, default: 0): Minimal interval (in seconds) between messages processed by the room.
* `min_presence_interval` (non-negative integer, default: 0): Minimal interval (in seconds) between presences processed by the room.
* `max_user` (positive integer, default: 200): Absolute maximum user count per room on the node.
* `max_users_admin_threshold` (positive integer, default: 5): Absolute maximum administrator count per room on the node.
* `user_message_shaper` (atom, default: `none`): Shaper for user messages processed by a room (global for the room).
* `user_presence_shaper` (atom, default: `none`): Shaper for user presences processed by a room (global for the room).
* `max_user_conferences` (non-negative, default: 10): Specifies the number of rooms that a user can occupy simultaneously.
* `http_auth_pool` (atom, default: `none`): If an external HTTP service is chosen to check passwords for password-protected rooms, this option specifies the HTTP pool name to use (see [External HTTP Authentication](#external-http-authentication) below).
* `hibernate_timeout` (timeout, default: `90000`): Timeout (in milliseconds) defining the inactivity period after which the room's process should be hibernated.
* `hibernated_room_check_interval` (timeout, default: `infinity`): Interval defining how often hibernated rooms will be checked.
* `hibernated_room_timeout` (timeout, default: `inifitniy`): A time after which a hibernated room is stopped (deeply hibernated). See [MUC performance optimisation]().
* `default_room_options` (list of key-value tuples, default: `[]`): List of room configuration options to be overridden in initial state.
    * `title` (binary, default: `<<>>`): Room title, short free text.
    * `description` (binary, default: `<<>>`): Room description, long free text.
    * `allow_change_subj` (boolean, default: `true`): Allow all occupants to change room subject.
    * `allow_query_users` (boolean, default: `true`): Allow occupants to send IQ queries to other occupants.
    * `allow_private_messages` (boolean, default: `true`): Allow private messaging between occupants.
    * `allow_visitor_status` (boolean, default: `true`): Allow occupants to use text statuses in presences. When disabled, text is removed by the room before broadcasting.
    * `allow_visitor_nickchange` (boolean, default: `true`): Allow occupants to change nicknames.
    * `public` (boolean, default: `true`): Room is included in the list available via Service Discovery.
    * `public_list` (boolean, default: `true`): Member list can be fetched by non-members.
    * `persistent` (boolean, default: `false`): Room will be stored in DB and survive even when last occupant leaves or node is restarted.
    * `moderated` (boolean, default: `true`): Only occupants with "voice" can send group chat messages.
    * `members_by_default` (boolean, default: `true`): All new occupants are members by default, unless they have different affiliation assigned.
    * `members_only` (boolean, default: `false`): Only users with member affiliation can join the room.
    * `allow_user_invites` (boolean, default: `false`): Allow ordinary members to send mediated invitations.
    * `password_protected` (boolean, default: `false`): Room is protected with password.
    * `password` (binary, default: `<<>>`): Room password is required upon joining. This option has no effect when `password_protected` is `false`.
    * `anonymous` (boolean, default: `true`): Room is anonymous, meaning occupants can't see each others real JIDs, except for room moderators.
    * `max_users` (positive integer, default: 200): Maximum user count per room. Admins and room owner are not affected.
    * `logging` (boolean, default: `false`): Enables logging of room events (messages, presences) to file on disk. Uses `mod_muc_log`.


### Example Configuration
```
{mod_muc, [
             {host, "muc.example.com"},
             {access, muc},
             {access_create, muc_create}
            ]},
```

### Performance optimisations

In large installation with many rooms there might be issues with memory consumption.
Each room is represented by erlang process with it's own state which can be big.
In MongooseIM there 2 levels for MUC rooms memory optimisations.

#### Room's process hibernation

By default the room's process is hibernated by the Erlang VM after 90s since last activity.
This timeout can be modified by `hibernate_timeout` option.

#### Room deep hibernation

In addition to the process hibernation there is also room's deep hibernation.
This optimisation works only for persistent rooms as only such rooms can be restored on demand.
The improvement works as follows:
1. Every `hibernated_room_check_interval` all room processes are traversed.
1. If a process is hibernated for longer time then `hibernated_room_timeout` a "stop" signal is sent to it.
1. The room's process can be stopped only if there is no online users or the only one is its owner.
In case the owner is online, a presence with type unavailable is sent to it indicating that the room's process is being terminated.

In future the room's process can be recreated on demand, for example when there is presence sent to it, or owner wants to add more users to the room.

### External HTTP Authentication

MUC rooms can be protected by password that is set by the room owner. However, MongooseIM supports another custom solution, where each attept to enter or create a room requires the password be checked by an external HTTP service. To enable this option, you need to:

* Configure an [HTTP connection pool](../Advanced-configuration.md#outgoing-http-connections).
* Set the name of the connection pool as the value of the `http_auth_pool` option of `mod_muc`.
* Enable the `password_protected` default room option (without setting the password itself).

Whenever a user tries to enter or create a room, the server will receive a GET request to the `check_password` path. It should return code 200 with a JSON object `{"code": Code, "msg": Message}` in the response body. If the server returns a different code, an error presence will be sent back to the client.

* `Code` is the status code: 0 indicates successful authentication, any other value means authentication failure.
* `Message` is a string containing the message to be sent back to the XMPP client indicating the reason of failed authentication. For successful authentication it is ignored and can eg. contain the string `"OK"`.

**Example:**

```

{http_connections, [
                    {my_auth_pool, [{server, "http://my_server:8000"}]}
                   ]}.


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
