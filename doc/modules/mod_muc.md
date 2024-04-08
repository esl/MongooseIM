### Module Description

This module implements [XEP-0045: Multi-User Chat](http://xmpp.org/extensions/xep-0045.html) (MUC).
It's a common XMPP group chat solution.
This extension consists of two Erlang modules: `mod_muc` and `mod_muc_room`, the latter being the room code itself.
Note that only `mod_muc` needs to be enabled in the configuration file.
Also `mod_muc_log` is a logging submodule.

## Options

### `modules.mod_muc.host`
 * **Syntax:** string, a valid subdomain
 * **Default:** `"conference.@HOST@"`
 * **Example:** `host = "group.@HOST@"`

Subdomain for MUC service to reside under. `@HOST@` is replaced with each served domain.

### `modules.mod_muc.backend`
 * **Syntax:** string, one of `"mnesia"` or `"rdbms"`
 * **Default:** `"mnesia"`
 * **Example:** `backend = "rdbms"`

Storage backend to store rooms and settings persistently.

### `modules.mod_muc.online_backend`
 * **Syntax:** string, one of `"mnesia"` or `"cets"`
 * **Default:** `"mnesia"`
 * **Example:** `online_backend = "cets"`

Backend to use to register and find online rooms. Queried when routing stanzas to the rooms.

!!! Warning
    The corresponding [internal database](../configuration/internal-databases.md) has to be enabled.

### `modules.mod_muc.access`
 * **Syntax:** non-empty string
 * **Default:** `"all"`
 * **Example:** `access = "muc"`

Access Rule to determine who is allowed to use the MUC service.

### `modules.mod_muc.access_create` 
 * **Syntax:** non-empty string
 * **Default:** `"all"`
 * **Example:** `access_create = "muc_create"`
 
Access Rule to determine who is allowed to create rooms.

### `modules.mod_muc.access_admin` 
 * **Syntax:** non-empty string
 * **Default:** `"none"`
 * **Example:** `access_admin = "muc_create"`

Access Rule to determine who is the administrator in all rooms.

### `modules.mod_muc.access_persistent`
 * **Syntax:** non-empty string
 * **Default:** `"all"`
 * **Example:** `access_persistent = "none"`
 
Access Rule to determine who is allowed to make the rooms persistent.
In order to change this parameter, the user must not only match the Access Rule but also be the owner of the room.

### `modules.mod_muc.history_size` 
 * **Syntax:** non-negative integer
 * **Default:** `20`
 * **Example:** `history_size = 30`

Room message history to be kept in RAM. After node restart, the history is lost.

### `modules.mod_muc.room_shaper` 
 * **Syntax:** non-empty string
 * **Default:** `"none"`
 * **Example:** `room_shaper = "muc_room_shaper"`

Limits per-room data throughput with traffic shaper.

### `modules.mod_muc.max_room_id` 
 * **Syntax:** non-negative integer or the string `"infinity"`
 * **Default:** `"infinity"`
 * **Example:** `max_room_id = 30`
 
Maximum room username length (in JID).

### `modules.mod_muc.max_room_name` 
 * **Syntax:** non-negative integer or the string `"infinity"`
 * **Default:** `"infinity"`
 * **Example:** `max_room_name = 30`

Maximum room name length.

### `modules.mod_muc.max_room_desc` 
 * **Syntax:** non-negative integer or the string `"infinity"`
 * **Default:** `"infinity"`
 * **Example:** `max_room_desc = 140`
 
Maximum room description length.

### `modules.mod_muc.min_message_interval` 
 * **Syntax:** non-negative integer
 * **Default:** `0`
 * **Example:** `min_message_interval = 1`

Minimal interval (in seconds) between messages processed by the room.

### `modules.mod_muc.min_presence_interval` 
 * **Syntax:** non-negative integer
 * **Default:** `0`
 * **Example:** `min_presence_interval = 1`
 
Minimal interval (in seconds) between presences processed by the room.

### `modules.mod_muc.max_users` 
 * **Syntax:** positive integer
 * **Default:** `200`
 * **Example:** `max_users = 100`

Absolute maximum user count per room on the node.

### `modules.mod_muc.max_users_admin_threshold` 
 * **Syntax:** positive integer
 * **Default:** `5`
 * **Example:** `max_users_admin_threshold = 10`

When the server checks if a new user can join a room and they are an admin,
`max_users_admin_threshold` is added to `max_users` during occupant limit check.

### `modules.mod_muc.user_message_shaper`
 * **Syntax:** non-empty string
 * **Default:** `"none"`
 * **Example:** `user_message_shaper = "muc_user_msg_shaper"`

Shaper for user messages processed by a room (global for the room).

### `modules.mod_muc.user_presence_shaper`
 * **Syntax:** non-empty string
 * **Default:** `"none"`
 * **Example:** `user_presence_shaper = "muc_user_presence_shaper"`

Shaper for user presences processed by a room (global for the room).

### `modules.mod_muc.max_user_conferences` 
 * **Syntax:** non-negative integer
 * **Default:** `10`
 * **Example:** `max_user_conferences = 5`

Specifies the number of rooms that a user can occupy simultaneously.

### `modules.mod_muc.http_auth_pool`
 * **Syntax:** non-empty string
 * **Default:** `"none"`
 * **Example:** `http_auth_pool = "external_auth"`

If an external HTTP service is chosen to check passwords for password-protected rooms,
this option specifies the HTTP pool name to use (see [External HTTP Authentication](#external-http-authentication) below).

### `modules.mod_muc.load_permanent_rooms_at_startup`
  * **Syntax:** boolean
  * **Default:** `false`
  * **Example:** `load_permanent_rooms_at_startup = true`
 
Load all rooms at startup. Because it can be unsafe when there are many rooms,
it is disabled by default.

### `modules.mod_muc.hibernate_timeout` 
 * **Syntax:** non-negative integer or the string `"infinity"`
 * **Default:** `90000` (milliseconds, 90 seconds)
 * **Example:** `hibernate_timeout = 60000`

Timeout (in milliseconds) defining the inactivity period after which the room's process should be hibernated.

### `modules.mod_muc.hibernated_room_check_interval` 
 * **Syntax:** non-negative integer or the string `"infinity"`
 * **Default:** `"infinity"`
 * **Example:** `hibernated_room_check_interval = 120000`

Interval defining how often the hibernated rooms will be checked (a timer is global for a node).

### `modules.mod_muc.hibernated_room_timeout` 
 * **Syntax:** non-negative integer or the string `"infinity"`
 * **Default:** `"infinity"`
 * **Example:** `hibernated_room_timeout = 120000`

A time after which a hibernated room is stopped (deeply hibernated).
 See [MUC performance optimisation](#performance-optimisations).
 
### `modules.mod_muc.default_room`
 * **Syntax:** A TOML table of options described below
 * **Default:** Default room options
 * **Example:**

```toml
  [modules.mod_muc.default_room]
    password_protected = true
    description = "An example description."
    
    [[modules.mod_muc.default_room.affiliations]]
        user = "alice"
        server = "localhost"
        resource = "resource1"
        affiliation = "member"
```

or:

```toml
  default_room.password_protected = true
  default_room.description = "An example description."

  [[modules.mod_muc.default_room.affiliations]]
    user = "alice"
    server = "localhost"
    resource = "resource1"
    affiliation = "member"
```

Available room configuration options to be overridden in the initial state:

* `modules.mod_muc.default_room.title`
    * **Syntax:** string
    * **Default:** `""`
    * **Example:** `title = "example_title"` 
   
    Room title, short free text.
   
* `modules.mod_muc.default_room.description`
    * **Syntax:** string
    * **Default:** `""`
    * **Example:** `description = "An example description."` 
 
    Room description, long free text.

* `modules.mod_muc.default_room.allow_change_subj` 
    * **Syntax:** boolean
    * **Default:** `true`
    * **Example:** `allow_change_subj = false` 

    Allow all occupants to change the room subject.
    
* `modules.mod_muc.default_room.allow_query_users` 
    * **Syntax:** boolean
    * **Default:** `true`
    * **Example:** `allow_query_users = false`
    
    Allow occupants to send IQ queries to other occupants.
    
* `modules.mod_muc.default_room.allow_private_messages` 
    * **Syntax:** boolean
    * **Default:** `true`
    * **Example:** `allow_private_messages = false`

    Allow private messaging between occupants.
    
* `modules.mod_muc.default_room.allow_visitor_status` 
    * **Syntax:** boolean
    * **Default:** `true`
    * **Example:** `allow_visitor_status = false`

    Allow occupants to use text statuses in presences.
    When disabled, text is removed by the room before broadcasting.
    
* `modules.mod_muc.default_room.allow_visitor_nickchange` 
    * **Syntax:** boolean
    * **Default:** `true`
    * **Example:** `allow_visitor_nickchange = false`
    
    Allow occupants to change nicknames.
    
* `modules.mod_muc.default_room.public` 
    * **Syntax:** boolean
    * **Default:** `true`
    * **Example:** `public = false`
    
    Room is included in the list available via Service Discovery.

* `modules.mod_muc.default_room.public_list` 
    * **Syntax:** boolean
    * **Default:** `true`
    * **Example:** `public_list = false`

    Member list can be fetched by non-members.

* `modules.mod_muc.default_room.persistent` 
    * **Syntax:** boolean
    * **Default:** `false`
    * **Example:** `persistent = true`

    Room will be stored in DB and survive even when the last occupant leaves or the node is restarted.

* `modules.mod_muc.default_room.moderated` 
    * **Syntax:** boolean
    * **Default:** `true`
    * **Example:** `moderated = false`

    Only occupants with a "voice" can send group chat messages.

* `modules.mod_muc.default_room.members_by_default`
    * **Syntax:** boolean
    * **Default:** `true`
    * **Example:** `members_by_default = false`
 
    All new occupants are members by default, unless they have a different affiliation assigned.

* `modules.mod_muc.default_room.members_only` 
    * **Syntax:** boolean
    * **Default:** `false`
    * **Example:** `members_only = true`

    Only users with a member affiliation can join the room.

* `modules.mod_muc.default_room.allow_user_invites` 
    * **Syntax:** boolean
    * **Default:** `false`
    * **Example:** `allow_user_invites = true`

    Allow ordinary members to send mediated invitations.

* `modules.mod_muc.default_room.allow_multiple_sessions` 
    * **Syntax:** boolean
    * **Default:** `false`
    * **Example:** `allow_multiple_sessions = true`

    Allow multiple user session to use the same nick.

* `modules.mod_muc.default_room.password_protected` 
    * **Syntax:** boolean
    * **Default:** `false`
    * **Example:** `password_protected = true`
    
    Room is protected with a password.

* `modules.mod_muc.default_room.password` 
    * **Syntax:** string
    * **Default:** `""`
    * **Example:** `password = "secret"`
    
    Room password is required upon joining.
    This option has no effect when `password_protected` is `false`.

* `modules.mod_muc.default_room.anonymous` 
    * **Syntax:** boolean
    * **Default:** `true`
    * **Example:** `anonymous = false`

    Room is anonymous, meaning occupants can't see each others real JIDs, except for the room moderators.

* `modules.mod_muc.default_room.max_users` 
    * **Syntax:** positive integer
    * **Default:** `200`
    * **Example:** `max_users = 100`

    Maximum user count per room. Admins and the room owner are not affected.

* `modules.mod_muc.default_room.logging`
    * **Syntax:** boolean
    * **Default:** `false`
    * **Example:** `logging = true`

    Enables logging of room events (messages, presences) to a file on the disk.
    Uses `mod_muc_log`.

* `modules.mod_muc.default_room.maygetmemberlist` 
    * **Syntax:** array of non-empty strings
    * **Default:** `[]`
    * **Example:** `maygetmemberlist = ["moderator"]`
    
    An array of roles and/or privileges that enable retrieving the room's member list.

* `modules.mod_muc.default_room.affiliations` 
    * **Syntax:** array of tables with keys:
        * `user` - non-empty string,
        * `server` - string, a valid domain,
        * `resource` - string,
        * `affiliation` - non-empty string
    * **Default:** `[]`
    * **Example:**
    
```toml
[[modules.mod_muc.default_room.affiliations]]
  user = "alice"
  server = "localhost"
  resource = "resource1"
  affiliation = "member"
                
[[modules.mod_muc.default_room.affiliations]]
  user = "bob"
  server = "localhost"
  resource = "resource2"
  affiliation = "owner"
```

This is the default list of affiliations set for every new room.

* `modules.mod_muc.default_room.subject` 
    * **Syntax:** string
    * **Default:** `""`
    * **Example:** `subject = "Lambda days"`

    A default subject for new room.

* `modules.mod_muc.default_room.subject_author`
    * **Syntax:** string
    * **Default:** `""`
    * **Example:** `subject_author = "Alice"`

    A nick name of the default subject's author.

## Example Configuration

```toml
[modules.mod_muc]
  host = "muc.example.com"
  access = "muc"
  access_create = "muc_create"
  http_auth_pool = "my_auth_pool"
  default_room.password_protected = true
  
  [[modules.mod_muc.default_room.affiliations]]
    user = "alice"
    server = "localhost"
    resource = "resource1"
    affiliation = "member"

  [[modules.mod_muc.default_room.affiliations]]
    user = "bob"
    server = "localhost"
    resource = "resource2"
    affiliation = "owner"
```

## Performance optimisations

Each room is represented by an Erlang process with its own state and can consume memory even if it's not used.
In large installations with many rooms, this might cause performance issues.
To address that problem MongooseIM has 2 levels of MUC rooms memory optimisations.

### Room's process hibernation

By default the room's process is hibernated by the Erlang VM 90 seconds after the last activity.
This timeout can be modified by `hibernate_timeout` option.

### Room deep hibernation

MongooseIM introduces an additional option of deep hibernation for unused rooms.
This optimisation works only for persistent rooms as only these can be restored on demand.
The improvement works as follows:
1. All room processes are traversed at a chosen `hibernated_room_check_interval`.
1. If a `hibernated_room_timeout` is exceeded, a "stop" signal is sent to a unused room.
1. The room's process is stopped only if there are no online users or if the only one is its owner.
If the owner is online, a presence of a type unavailable is sent to it indicating that the room's process is being terminated.

The room's process can be recreated on demand, for example when a presence sent to it, or the owner wants to add more users to the room.

## External HTTP Authentication

MUC rooms can be protected by a password that is set by the room owner.
Note that MongooseIM supports another custom solution, where each attempt to enter or create a room requires the password to be checked by an external HTTP service.
To enable this option, you need to:

* Configure an [HTTP connection pool](../configuration/outgoing-connections.md#http-options).
* Set the name of the connection pool as the value of the `http_auth_pool` option of `mod_muc`.
* Enable the `password_protected` default room option (without setting the password itself).

Whenever a user tries to enter or create a room, the server will receive a GET request to the `check_password` path.
It should return a 200 response with a JSON object `{"code": Code, "msg": Message}` in the response body.
If the server returns something else, an error presence will be sent back to the client.

* `Code` is the status code: 0 indicates a successful authentication, any other value means the authentication failed.
* `Message` is a string containing the message to be sent back to the XMPP client indicating the reason for a failed authentication.
 When authentication succeeds it is ignored and can contain anything ( eg. the string `"OK"`).

**Example:**

```toml
[outgoing_pools.http.my_auth_pool]
  strategy = "available_worker"
  connection.host = "http://my_server:8000"

[modules.mod_muc]
  host = "muc.example.com"
  access = "muc"
  access_create = "muc_create"
  http_auth_pool = "my_auth_pool"
  default_room.password_protected = true
```

## Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[global, mod_muc, deep_hibernations]` | spiral | A room process is stopped (applies only to persistent rooms). |
| `[global, mod_muc, process_recreations]` | spiral | A room process is recreated from a persisted state. |
| `[global, mod_muc, hibernations]` | spiral | A room process becomes hibernated (garbage collected and put in wait state). |
| `[global, mod_muc, hibernated_rooms]` | value | How many rooms are in hibernated state. Does not include rooms in "deep hibernation". |
| `[global, mod_muc, online_rooms]` | value | How many rooms have running processes (includes rooms in a hibernated state). |
