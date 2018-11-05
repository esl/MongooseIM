### Module Description
This module implements [XEP-0016: Privacy Lists](http://xmpp.org/extensions/xep-0016.html). This extension allows user to block IQs, messages, presences, or all, based on JIDs, subscription, and roster groups.

### Options
* `backend` (atom, default: `mnesia`): Storage backend. Currently supported are `mnesia`, `rdbms` and `riak`.

### Example Configuration
```
{mod_privacy, []},
```

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[global, backends, mod_privacy, get_privacy_list]` | histogram | Time it takes to retrieve a specific privacy list from a DB. |
| `[global, backends, mod_privacy, get_list_names]` | histogram | Time it takes to fetch names of user's privacy lists from a DB. |
| `[global, backends, mod_privacy, get_default_list]` | histogram | Time it takes to get a default privacy list for a user from a DB. |
| `[global, backends, mod_privacy, set_default_list]` | histogram | Time it takes to set a default list's name for a user in a DB. |
| `[global, backends, mod_privacy, forget_default_list*]` | histogram | Time it takes to remove default list's name for a user in a DB. |
| `[global, backends, mod_privacy, remove_privacy_list*]` | histogram | Time it takes to delete a privacy list from a DB. |
| `[global, backends, mod_privacy, replace_privacy_lis*t]` | histogram | Time it takes to update (replace) a privacy list in a DB. |
