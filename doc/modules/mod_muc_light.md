## Module Description

This module implements [Multi-User Chat Light](../open-extensions/muc_light.md).
It's an experimental XMPP group chat solution.
This extension consists of several modules but only `mod_muc_light` needs to be enabled in the config file.

## Options

### `modules.mod_muc_light.host`
 * **Syntax:** string, a valid subdomain
 * **Default:** `"muclight.@HOST@"`
 * **Example:** `host = "group.@HOST@"`

Domain for the MUC Light service to reside under. `@HOST@` is replaced with each served domain.
 
### `modules.mod_muc_light.backend`
  * **Syntax:** string, one of `"mnesia"`, `"rdbms"`
  * **Default:** `"mnesia"`
  * **Example:** `backend = "rdbms"`

Database backend to use. 

### `modules.mod_muc_light.cache_affs.*`
  * **Syntax:** TOML section
  * **Default:** not declared
  * **Example:** `[modules.mod_muc_light.cache_affs]`

Enables caching affiliations for rooms, this has the advantage that the list of affiliations of a given room is stored locally, instead of being fetched from the DB on each message delivered to a room. On the other hand, in an edge case of a network split when the affiliations of a room are changed, there's a risk of inconsistencies for the cache having values in one node not yet synchronised with the other.

If caching is enabled, it will spawn its own [segmented cache](https://github.com/esl/segmented_cache) cache. To configure the cache parameters, the same config can be stored under the `cache_affs` section. To see details about the meaning of each flag, see [`mod_cache_users`](./mod_cache_users.md).

```toml
modules.mod_muc_light.cache_affs.strategy
modules.mod_muc_light.cache_affs.time_to_live
modules.mod_muc_light.cache_affs.number_of_segments
```

### `modules.mod_muc_light.equal_occupants`
  * **Syntax:** boolean
  * **Default:** `false`
  * **Example:** `equal_occupants = true`

When enabled, MUC Light rooms won't have owners. 
It means that every occupant will be a `member`, even the room creator.

!!! Warning
    This option does not implicitly set `all_can_invite` to `true`. 
    If that option is set to `false`, nobody will be able to join the room after the initial creation request.

### `modules.mod_muc_light.legacy_mode`
  * **Syntax:** boolean
  * **Default:** `false`
  * **Example:** `legacy_mode = true`

Enables XEP-0045 compatibility mode. 
It allows using a subset of classic MUC stanzas with some MUC Light functions limited.

### `modules.mod_muc_light.rooms_per_user`
  * **Syntax:** positive integer or the string `"infinity"`
  * **Default:** `"infinity"`
  * **Example:** `rooms_per_user = 100`

Specifies a cap on a number of rooms a user can occupy.

!!! Warning
    Setting such a limit may trigger expensive DB queries for every occupant addition.

### `modules.mod_muc_light.blocking`
  * **Syntax:** boolean
  * **Default:** `true`
  * **Example:** `blocking = false`

Blocking feature enabled/disabled.

### `modules.mod_muc_light.all_can_configure`
  * **Syntax:** boolean
  * **Default:** `false`
  * **Example:** `all_can_configure = true`

When enabled, all room occupants can change all configuration options. 
If disabled, everyone can still change the room subject.

### `modules.mod_muc_light.all_can_invite`
  * **Syntax:** boolean
  * **Default:** `false`
  * **Example:** `all_can_invite = true`
 
When enabled, all room occupants can add new occupants to the room.
Occupants added by `members` become `members` as well.

### `modules.mod_muc_light.max_occupants` 
  * **Syntax:** positive integer or the string `"infinity"`
  * **Default:** `"infinity"`
  * **Example:** `max_occupants = 100`

Specifies a cap on the occupant count per room.

### `modules.mod_muc_light.rooms_per_page`
  * **Syntax:** positive integer or the string `"infinity"`
  * **Default:** `10`
  * **Example:** `rooms_per_page = 100`

Specifies maximal number of rooms returned for a single Disco request.

### `modules.mod_muc_light.rooms_in_rosters` 
  * **Syntax:** boolean
  * **Default:** `false`
  * **Example:** `rooms_in_rosters = true`

When enabled, rooms the user occupies are included in their roster.

### `modules.mod_muc_light.allow_multiple_owners`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `allow_multiple_owners = true`

When enabled, owner can add one or more people as owners.
If disabled, there can only be one owner.
This option may be useful for creating a subset of users with admin rights, instead of giving rights for all members,
which can be done with the [all_can_configure](#modulesmod_muc_lightall_can_configure) and
[all_can_invite](#modulesmod_muc_lightall_can_invite) options.

!!! Warning
    This is a custom option, not compatible with our [MUC Light XEP](../open-extensions/muc_light.md).
    If a client is adhering to the XEP, its behaviour may be unexpected, and this option should not be enabled.

### `modules.mod_muc_light.config_schema`
  * **Syntax:** an array of `config_schema` items, as described below
  * **Default:**

```toml
        [[modules.mod_muc_light.config_schema]]
          field = "roomname"
          string_value = "Untitled"

        [[modules.mod_muc_light.config_schema]]
          field = "subject"
          string_value = ""
```

  * **Example:**

```toml
        [[modules.mod_muc_light.config_schema]]
          field = "display-lines"
          integer_value = 30
          internal_key = "display_lines"
```

Defines fields allowed in the room configuration.

Each `config_schema` item is a TOML table with the following keys:

* `field` - mandatory, non-empty string - field name.
* `string_value`, `integer_value`, `float_value` - exactly one of them has to be present, depending on the type of the field:
    * `string_value` - string,
    * `integer_value` - integer,
    * `float_value` - floating-point number.
* `internal_key` - optional, non-empty string - field name used in the internal representation, useful only for debugging or custom applications. By default it is the same as `field`.

!!! WARNING
    Lack of the `roomname` field will cause room names in Disco results and Roster items be set to the room username.

## Example Configuration

```toml
[modules.mod_muc_light]
  host = "muclight.example.com"
  equal_occupants = true
  legacy_mode = true
  rooms_per_user = 10
  blocking = false
  all_can_configure = true
  all_can_invite = true
  max_occupants = 50
  rooms_per_page = 5
  rooms_in_rosters = true

  [modules.mod_muc_light.cache_affs]
    time_to_live = 60

  [[modules.mod_muc_light.config_schema]] 
    field = "roomname"
    string_value = "The Room"
  
  [[modules.mod_muc_light.config_schema]] 
    field = "display-lines"
    integer_value = 30
    internal_key = "display_lines"
```

## Metrics

This module provides [backend metrics](../operation-and-maintenance/MongooseIM-metrics.md#backend-metrics).
If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/MongooseIM-metrics.md) page.

Prometheus metrics have a `host_type` and `function` labels associated with these metrics.
Since Exometer doesn't support labels, the function as well as the host types, or word `global`, are part of the metric names, depending on the [`instrumentation.exometer.all_metrics_are_global`](../configuration/instrumentation.md#instrumentationexometerall_metrics_are_global) option.

Backend in the action name can be either `rdbms` or `mnesia`.

=== "Prometheus"

    | Backend action | Type | Function | Description (when it gets incremented) |
    | -------------- | ---- | -------- | -------------------------------------- |
    | `mod_muc_light_db_Backend_count` | counter | `create_room` | A new room is stored in a DB. |
    | `mod_muc_light_db_Backend_time`  | histogram | `create_room` | Time to store a new room in a DB. |
    | `mod_muc_light_db_Backend_count` | counter | `destroy_room` | Room data is removed from a DB. |
    | `mod_muc_light_db_Backend_time`  | histogram | `destroy_room` | Time to remove room data from a DB. |
    | `mod_muc_light_db_Backend_count` | counter | `room_exists` | A room existence is checked. |
    | `mod_muc_light_db_Backend_time`  | histogram | `room_exists` | Time to check the existance of a room. |
    | `mod_muc_light_db_Backend_count` | counter | `get_user_rooms` | A list of rooms the user is a participant of is retrieved from a DB. |
    | `mod_muc_light_db_Backend_time`  | histogram | `get_user_rooms` | Time to retrieve a list of rooms the user is a participant of from a DB. |
    | `mod_muc_light_db_Backend_count` | counter | `get_user_rooms_count` | The count` of rooms the user is a participant of is retrieved from a DB. |
    | `mod_muc_light_db_Backend_time`  | histogram | `get_user_rooms_count` | Time to retrieve the count` of rooms the user is a participant of from a DB. |
    | `mod_muc_light_db_Backend_count` | counter | `remove_user` | All MUC Light related user data is removed from a DB. |
    | `mod_muc_light_db_Backend_time`  | histogram | `remove_user` | Time to remove all MUC Light related user data from a DB. |
    | `mod_muc_light_db_Backend_count` | counter | `remove_domain` | All MUC Light related domain data is removed from a DB. |
    | `mod_muc_light_db_Backend_time`  | histogram | `remove_domain` |Time to remove all MUC Light related domain data from a DB. |
    | `mod_muc_light_db_Backend_count` | counter | `get_config` | A room config is retrieved from a DB. |
    | `mod_muc_light_db_Backend_time`  | histogram | `get_config` | Time to retrieve a room config from a DB. |
    | `mod_muc_light_db_Backend_count` | counter | `set_config` | A room config is updated in a DB. |
    | `mod_muc_light_db_Backend_time`  | histogram | `set_config` | Time to update a room config in a DB. |
    | `mod_muc_light_db_Backend_count` | counter | `get_blocking` | Blocking data is fetched from a DB. |
    | `mod_muc_light_db_Backend_time`  | histogram | `get_blocking` | Time to fetch blocking data from a DB. |
    | `mod_muc_light_db_Backend_count` | counter | `set_blocking` | Blocking data is updated in a DB. |
    | `mod_muc_light_db_Backend_time`  | histogram | `set_blocking` | Time to update blocking data in a DB. |
    | `mod_muc_light_db_Backend_count` | counter | `get_aff_users` | Affiliated users list is fetched from a DB. |
    | `mod_muc_light_db_Backend_time`  | histogram | `get_aff_users` | Time to fetch affiliated users list from a DB. |
    | `mod_muc_light_db_Backend_count` | counter | `modify_aff_users` | Affiliations in a room are updated in a DB. |
    | `mod_muc_light_db_Backend_time`  | histogram | `modify_aff_users` | Time to update affiliations in a room in a DB. |

=== "Exometer"

    | Backend action | Type | Description (when it gets incremented) |
    | -------------- | ---- | -------------------------------------- |
    | `[HostType, mod_muc_light_db_Backend, create_room,  count]` | spiral   | A new room is stored in a DB. |
    | `[HostType, mod_muc_light_db_Backend, create_room,  time]`  | histogram | Time to store a new room in a DB. |
    | `[HostType, mod_muc_light_db_Backend, destroy_room, count]` | spiral   | Room data is removed from a DB. |
    | `[HostType, mod_muc_light_db_Backend, destroy_room, time]`  | histogram | Time to remove room data from a DB. |
    | `[HostType, mod_muc_light_db_Backend, room_exists,  count]` | spiral   | A room existence is checked. |
    | `[HostType, mod_muc_light_db_Backend, room_exists,  time]`  | histogram | Time to check the existance of a room. |
    | `[HostType, mod_muc_light_db_Backend, get_user_rooms, count]` | spiral   | A list of rooms the user is a participant of is retrieved from a DB. |
    | `[HostType, mod_muc_light_db_Backend, get_user_rooms, time]`  | histogram | Time to retrieve a list of rooms the user is a participant of from a DB. |
    | `[HostType, mod_muc_light_db_Backend, get_user_rooms_count, count]` | spiral   | The count of rooms the user is a participant of is retrieved from a DB. |
    | `[HostType, mod_muc_light_db_Backend, get_user_rooms_count, time]`  | histogram | Time to retrieve the count of rooms the user is a participant of from a DB. |
    | `[HostType, mod_muc_light_db_Backend, remove_user`, count]` | spiral   | All MUC Light related user data is removed from a DB. |
    | `[HostType, mod_muc_light_db_Backend, remove_user`, time]`  | histogram | Time to remove all MUC Light related user data from a DB. |
    | `[HostType, mod_muc_light_db_Backend, remove_domain, count]` | spiral   | All MUC Light related domain data is removed from a DB. |
    | `[HostType, mod_muc_light_db_Backend, remove_domain, time]`  | histogram |Time to remove all MUC Light related domain data from a DB. |
    | `[HostType, mod_muc_light_db_Backend, get_config, count]` | spiral   | A room config is retrieved from a DB. |
    | `[HostType, mod_muc_light_db_Backend, get_config, time]`  | histogram | Time to retrieve a room config from a DB. |
    | `[HostType, mod_muc_light_db_Backend, set_config, count]` | spiral   | A room config is updated in a DB. |
    | `[HostType, mod_muc_light_db_Backend, set_config, time]`  | histogram | Time to update a room config in a DB. |
    | `[HostType, mod_muc_light_db_Backend, get_blocking, count]` | spiral   | Blocking data is fetched from a DB. |
    | `[HostType, mod_muc_light_db_Backend, get_blocking, time]`  | histogram | Time to fetch blocking data from a DB. |
    | `[HostType, mod_muc_light_db_Backend, set_blocking, count]` | spiral   | Blocking data is updated in a DB. |
    | `[HostType, mod_muc_light_db_Backend, set_blocking, time]`  | histogram | Time to update blocking data in a DB. |
    | `[HostType, mod_muc_light_db_Backend, get_aff_users, count]` | spiral   | Affiliated users list is fetched from a DB. |
    | `[HostType, mod_muc_light_db_Backend, get_aff_users, time]`  | histogram | Time to fetch affiliated users list from a DB. |
    | `[HostType, mod_muc_light_db_Backend, modify_aff_users, count]` | spiral   | Affiliations in a room are updated in a DB. |
    | `[HostType, mod_muc_light_db_Backend, modify_aff_users, time]`  | histogram | Time to update affiliations in a room in a DB. |
