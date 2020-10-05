### Module Description

This module implements [Multi-User Chat Light](../open-extensions/muc_light.md).
It's an experimental XMPP group chat solution.
This extension consists of several modules but only `mod_muc_light` needs to be enabled in the config file.

### Options

#### `modules.mod_muc_light.host`
 * **Syntax:** string, a valid subdomain
 * **Default:** `"muclight.@HOST@"`
 * **Example:** `host = "group.@HOST@"`
 
Domain for the MUC Light service to reside under.
 `@HOST@` is replaced with each served domain.
 
#### `modules.mod_muc_light.backend`
  * **Syntax:** string, one of `"mnesia"`, `"rdbms"`
  * **Default:** `"mnesia"`
  * **Example:** `backend = "rdbms"`
 
Database backend to use. 

#### `modules.mod_muc_light.equal_occupants`
  * **Syntax:** boolean
  * **Default:** `false`
  * **Example:** `equal_occupants = true`
 
  When enabled, MUC Light rooms won't have owners. 
  It means that every occupant will be a `member`, even the room creator. 
 **Warning:** This option does not implicitly set `all_can_invite` to `true`. 
 If that option is set to `false`, nobody will be able to join the room after the initial creation request.

#### `modules.mod_muc_light.legacy_mode`
  * **Syntax:** boolean
  * **Default:** `false`
  * **Example:** `legacy_mode = true`
 
Enables XEP-0045 compatibility mode. 
It allows using a subset of classic MUC stanzas with some MUC Light functions limited.

#### `modules.mod_muc_light.rooms_per_user`
  * **Syntax:** positive integer or the string `"infinity"`
  * **Default:** `"infinity"`
  * **Example:** `rooms_per_user = 100`
  
  Specifies a cap on a number of rooms a user can occupy. 
 **Warning:** Setting such a limit may trigger expensive DB queries for every occupant addition.
 
#### `modules.mod_muc_light.blocking`
  * **Syntax:** boolean
  * **Default:** `true`
  * **Example:** `blocking = false`
 
Blocking feature enabled/disabled.

#### `modules.mod_muc_light.all_can_configure`
  * **Syntax:** boolean
  * **Default:** `false`
  * **Example:** `all_can_configure = true`
  
 When enabled, all room occupants can change all configuration options. 
 If disabled, everyone can still change the room subject.
 
#### `modules.mod_muc_light.all_can_invite`
  * **Syntax:** boolean
  * **Default:** `false`
  * **Example:** `all_can_invite = true`
 
When enabled, all room occupants can add new occupants to the room.
 Occupants added by `members` become `members` as well.
#### `modules.mod_muc_light.max_occupants` 
  * **Syntax:** positive integer or the string `"infinity"`
  * **Default:** `"infinity"`
  * **Example:** `max_occupants = 100`

Specifies a cap on the occupant count per room.

#### `modules.mod_muc_light.rooms_per_page`
  * **Syntax:** positive integer or the string `"infinity"`
  * **Default:** `10`
  * **Example:** `rooms_per_page = 100`
 
Specifies maximal number of rooms returned for a single Disco request.

#### `modules.mod_muc_light.rooms_in_rosters` 
  * **Syntax:** boolean
  * **Default:** `false`
  * **Example:** `rooms_in_rosters = true`

When enabled, rooms the user occupies are included in their roster.

#### `modules.mod_muc_light.config_schema`
  * **Syntax:** an array of `config_schema` items, as described below
  * **Default:** 

        [[modules.mod_muc_light.config_schema]] 
          field = "roomname"
          value = "Untitled"
        
        [[modules.mod_muc_light.config_schema]] 
          field = "subject"
          value = ""

  * **Example:** 

        [[modules.mod_muc_light.config_schema]] 
          field = "display-lines"
          value = 30
          internal_key = "display_lines"
          type = "integer"

 Defines fields allowed in the room configuration.
  
 Allowed `config_schema` items are (may be mixed):

* Field name and a default value. The value has to be a string. An example:
    
        field = "field_name"
        value = "default_value"
    
* Field name, a default value, an internal key representation string and a type.
Valid config field types are:

    * `binary` (i.e. any valid XML CDATA)
    * `integer`
    * `float`

    Useful only for debugging or custom applications. An example:
    
        field = "display-lines"
        value = 30
        internal_key = "display_lines"
        type = "integer"
    
**WARNING!** Lack of the `roomname` field will cause room names in Disco results and Roster items be set to the room username.


### Example Configuration

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
  
  [[modules.mod_muc_light.config_schema]] 
    field = "roomname"
    value = "The Room"
  
  [[modules.mod_muc_light.config_schema]] 
    field = "display-lines"
    value = 30
    internal_key = "display_lines"
    type = "integer"
```

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Backend action | Description (when it gets incremented) |
| | -------------------------------------- |
| `create_room` | A new room is stored in a DB. |
| `destroy_room` | Room data is removed from a DB. |
| `room_exists` | A room existence is checked. |
| `get_user_rooms` | A list of rooms the user is a participant of is retrieved from a DB. |
| `remove_user` | All MUC Light related user data is removed from a DB. |
| `get_config` | A room config is retrieved from a DB. |
| `set_config` | A room config is updated in a DB. |
| `get_blocking` | Blocking data is fetched from a DB. |
| `set_blocking` | Blocking data is updated in a DB. |
| `get_aff_users` | An affiliated users list is fetched from a DB. |
| `modify_aff_users` | Affiliations in a room are updated in a DB. |
