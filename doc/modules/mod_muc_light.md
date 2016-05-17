### Module Description
This module implements [XEP Multi-User Chat Light](https://github.com/xsf/xeps/pull/118) (still being reviewed by XMPP community). It's an experimental XMPP group chat solution. This extension consists of several modules but only `mod_muc_light` needs to be enabled in config file.

### Options

* **host** (string, default: `"muclight.@HOST@"`) - Domain for MUC Light service to reside under. `@HOST@` is replaced with each served domain.
* **backend** (atom, default: `mnesia`) - Database backend to use. Currently only `mnesia` is supported.
* **equal_occupants** (boolean, default: `false`) - When enabled, MUC Light rooms won't have owners. It means that every occupant will be a `member`, even the room creator. **Warning:** This option does not implicitly set `all_can_invite` to `true`. If that option is set to `false`, nobody will be able to join the room after initial creation request.
* **legacy_mode** (boolean, default: `false`) - Enables XEP-0045 compatibility mode. It allows to use subset of classic MUC stanzas with some MUC Light functions limited.
* **rooms_per_user** (positive integer or `infinity`, default: `infinity`) - Specifies a cap on a number of rooms a user can occupy. **Warning:** Setting such limit may trigger expensive DB queries for every occupant addition.
* **blocking** (boolean, default: `true`) - Blocking feature enabled/disabled.
* **all_can_configure** (boolean, default: `false`) - When enabled, all room occupants can change all configuration options. If disabled, everyone can still change room subject anyway.
* **all_can_invite** (boolean, default: `false`) - When enabled, all room occupants can add new occupants to the room. Occupants added by `members` become `members` as well.
* **max_occupants** (positive integer or `infinity`, default: `infinity`) - Specifies a cap on occupant count per room.
* **rooms_per_page** (positive integer or `infinity`, default: 10) - Specifies maximal number of rooms returned for single Disco request.
* **rooms_in_rosters** (boolean, default: `false`) - When enabled, rooms the user occupies are included in its roster.

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
             {rooms_in_rosters, true}
            ]},
```
