### Module Description
The module implements roster support, specified in [RFC 6121](http://xmpp.org/rfcs/rfc6121.html). 
Includes support for [XEP-0237: Roster Versioning](http://xmpp.org/extensions/xep-0237.html). 
It can sometimes become quite a heavyweight feature, so there is an option to disable it.

### Options

* `iqdisc` (default: `one_queue`)
* `versioning` (boolean, default: `false`): Turn on/off support for Roster Versioning.
* `store_current_id` (boolean, default: `false`): Stores the last roster hash in DB (used in Roster Versioning). 
 Improves performance but should be disabled, when shared rosters are used.
* `backend` (atom, default: `mnesia`): Storage backend. 
 Currently `mnesia`, `rdbms` and `riak` are supported.

### Example configuration
```
{mod_roster, [
               {versioning, true},
               {store_current_id, true}
             ]}
```

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[global, backends, mod_roster, read_roster_version]` | histogram | Time it takes to fetch version of a user's roster. |
| `[global, backends, mod_roster, write_roster_version]` | histogram | Time it takes to store version of a user's roster. |
| `[global, backends, mod_roster, get_roster]` | histogram | Time it takes to fetch a user's roster. |
| `[global, backends, mod_roster, get_roster_entry]` | histogram | Time it takes to fetch a specific roster entry. |
| `[global, backends, mod_roster, get_roster_entry_t]` | histogram | Time it takes to fetch a specific roster entry inside a transaction. |
| `[global, backends, mod_roster, get_subscription_lists]` | histogram | Time it takes to fetch a subscription list of a user. |
| `[global, backends, mod_roster, roster_subscribe_t]` | histogram | Time it takes to update the subscription status between users inside a transaction. |
| `[global, backends, mod_roster, update_roster_t]` | histogram | Time it takes to update a roster entry in a transaction. |
| `[global, backends, mod_roster, del_roster_t]` | histogram | Time it takes to remove a roster entry inside a transaction. |

