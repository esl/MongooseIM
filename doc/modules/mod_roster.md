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

##### Riak-specific options

* `bucket_type` (default `<<"rosters">>`) - Riak bucket type.

* `version_bucket_type` (default `<<"roster_versions">>`) - Riak bucket type for versions information

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Backend action | Description (when it gets incremented) |
| ---- | -------------------------------------- |
| `read_roster_version` | Version of a user's roster is retrieved. |
| `write_roster_version` | Vversion of a user's roster is stored. |
| `get_roster` | A user's roster is fetched. |
| `get_roster_entry` | A specific roster entry is fetched. |
| `get_roster_entry_t` | A specific roster entry is fetched inside a transaction. |
| `get_subscription_lists` | A subscription list of a user is retrieved. |
| `roster_subscribe_t` | A subscription status between users is updated inside a transaction. |
| `update_roster_t` | A roster entry is updated in a transaction. |
| `del_roster_t` | A roster entry is removed inside a transaction. |

