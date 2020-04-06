### Module Description
This module implements [XEP-0016: Privacy Lists](http://xmpp.org/extensions/xep-0016.html). This extension allows user to block IQs, messages, presences, or all, based on JIDs, subscription, and roster groups.

### Options
* `backend` (atom, default: `mnesia`): Storage backend. Currently supported are `mnesia`, `rdbms` and `riak`.

### Example Configuration
```
{mod_privacy, []},
```


##### Riak-specific options

* `defaults_bucket_type` (default `<<"privacy_defaults">>`) - Riak bucket type for information about default list name.
* `names_bucket_type` (default `<<"privacy_lists_names">>`) - Riak bucket type for information about privacy list names.
* `bucket_type` (default `<<"privacy_lists">>`) - Riak bucket type for privacy lists.

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Backend action | Description (when it gets incremented) |
| ---- | -------------------------------------- |
| `get_privacy_list` | A privacy list is retrieved from a DB. |
| `get_list_names` | Names of user's privacy lists are fetched from a DB. |
| `get_default_list` | A default privacy list for a user is fetched from a DB. |
| `set_default_list` | A default list's name for a user is set in a DB. |
| `forget_default_list` | A default list's name for a user is removed from a DB. |
| `remove_privacy_list` | A privacy list is deleted from a DB. |
| `replace_privacy_list` | A privacy list is updated (replaced) in a DB. |
