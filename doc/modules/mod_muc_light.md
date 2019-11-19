### Module Description

This module implements [Multi-User Chat Light](../open-extensions/muc_light.md).
It's an experimental XMPP group chat solution.
This extension consists of several modules but only `mod_muc_light` needs to be enabled in the config file.

### Options

* **host** (string, default: `"muclight.@HOST@"`) - Domain for the MUC Light service to reside under. 
 `@HOST@` is replaced with each served domain.
* **backend** (atom, default: `mnesia`) - Database backend to use. 
 `mnesia` and `rdbms` are supported.
* **equal_occupants** (boolean, default: `false`) - When enabled, MUC Light rooms won't have owners. 
 It means that every occupant will be a `member`, even the room creator. 
 **Warning:** This option does not implicitly set `all_can_invite` to `true`. 
 If that option is set to `false`, nobody will be able to join the room after the initial creation request.
* **legacy_mode** (boolean, default: `false`) - Enables XEP-0045 compatibility mode. 
 It allows using a subset of classic MUC stanzas with some MUC Light functions limited.
* **rooms_per_user** (positive integer or `infinity`, default: `infinity`) - Specifies a cap on a number of rooms a user can occupy. 
 **Warning:** Setting such a limit may trigger expensive DB queries for every occupant addition.
* **blocking** (boolean, default: `true`) - Blocking feature enabled/disabled.
* **all_can_configure** (boolean, default: `false`) - When enabled, all room occupants can change all configuration options. 
 If disabled, everyone can still the change room subject.
* **all_can_invite** (boolean, default: `false`) - When enabled, all room occupants can add new occupants to the room.
 Occupants added by `members` become `members` as well.
* **max_occupants** (positive integer or `infinity`, default: `infinity`) - Specifies a cap on the occupant count per room.
* **rooms_per_page** (positive integer or `infinity`, default: 10) - Specifies maximal number of rooms returned for a single Disco request.
* **rooms_in_rosters** (boolean, default: `false`) - When enabled, rooms the user occupies are included in their roster.
* **config_schema** (list; see below, default: `[{"roomname", "Untitled"}, {"subject", ""}]`) - A list of fields allowed in the room configuration.
 The field type may be specified but the default is "binary", i.e. effectively a string. 
 **WARNING!** Lack of the `roomname` field will cause room names in Disco results and Roster items be set to the room username.

### Config schema

Allowed `config_schema` list items are (may be mixed):

* Field name and a default value: `{"field", "value"}` - will be expanded to "field" of a type `binary` (string) with a default "value"
* Field name, a default value, an atom (internal key representation) and a type: `{"field", "value", field, float}` - useful only for debugging or custom applications

Example of such list: `[{"roomname", "My Room"}, {"subject", "Hi"}, {"priority", 0, priority, integer}]`

Valid config field types are:

* `binary` (i.e. any valid XML CDATA)
* `integer`
* `float`

### Example Configuration

```
{mod_muc_light, [
             {host, "muclight.example.com"},
             {equal_occupants, true},
             {legacy_mode, true},
             {rooms_per_user, 10},
             {blocking, false},
             {all_can_configure, true},
             {all_can_invite, true},
             {max_occupants, 50},
             {rooms_per_page, 5},
             {rooms_in_rosters, true},
             {config_schema, [{"roomname", "The Room"}, {"display-lines", 30, display_lines, integer}]}
            ]},
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

