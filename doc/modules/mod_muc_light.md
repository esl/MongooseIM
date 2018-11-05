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
* **config_schema** (list; see below, default: `["roomname", "subject"]`) - A list of fields allowed in the room configuration.
 The field type may be specified but the default is "binary", i.e. effectively a string. 
 **WARNING!** Lack of the `roomname` field will cause room names in Disco results and Roster items be set to the room username.
* **default_config** (list, default: `[{"roomname, "Untitled"}, {"subject", ""}]`) - Custom default room configuration; must be a subset of config schema. 
 It's a list of KV tuples with string keys and values of appriopriate type. 
 String values will be converted to binary automatically.

### Config schema

Allowed `config_schema` list items are (may be mixed):

* Just the field name: `"field"` - will be expanded to "field" of a type `binary`
* Field name and a type: `{"field", integer}`
* Field name, an atom and a type: `{"field", field, float}` - useful only for debugging or unusual applications

Example of such list: `["roomname", {"subject", binary}, {"priority", priority, integer}]`

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
             {config_schema, ["roomname", {"display-lines", integer}]},
             {default_config, [{"roomname", "The Room"}, {"display-lines", 30}]}
            ]},
```

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[global, backends, mod_muc_light, create_room]` | histogram | Time it takes to store the new room data in a DB. |
| `[global, backends, mod_muc_light, destroy_room]` | histogram | Time it takes to remove the room data from a DB. |
| `[global, backends, mod_muc_light, room_exists]` | histogram | Time it takes to check if a room exists in a DB. |
| `[global, backends, mod_muc_light, get_user_rooms]` | histogram | Time it takes to get a list of rooms the user is a participant of from a DB. |
| `[global, backends, mod_muc_light, remove_user]` | histogram | Time it takes to remove all MUC Light related user data from a DB. |
| `[global, backends, mod_muc_light, get_config]` | histogram | Time it takes to get the room config from a DB. |
| `[global, backends, mod_muc_light, set_config]` | histogram | Time it takes to set the room config in a DB. |
| `[global, backends, mod_muc_light, get_blocking]` | histogram | Time it takes to retrieve blocking data from a DB. |
| `[global, backends, mod_muc_light, set_blocking]` | histogram | Time it takes to set blocking data in a DB. |
| `[global, backends, mod_muc_light, get_aff_users]` | histogram | Time it takes to get an affiliated users list from a DB. |
| `[global, backends, mod_muc_light, modify_aff_users]` | histogram | Time it takes to update affiliations in a room in a DB. |

