### Module Description
This module implements an offline messages storage compliant with [XEP-0160: Best Practices for Handling Offline Messages](http://xmpp.org/extensions/xep-0160.html). 
It stores one-to-one messages only when the recipient has no online resources. 
It is not well suited for applications supporting multiple user devices, because anything saved in the DB can be retrieved only once, so the message history is not synchronised between devices. 
Although `mod_offline` may be sufficient in some cases, it is preferable to use [mod_mam](mod_mam.md).

### Options
* `access_max_user_messages` (atom, default: `max_user_offline_messages`): Access Rule to use for limiting the storage size per user.
* `backend` (atom, default: `mnesia`): Storage backend. Currently `mnesia`, `rdbms` and `riak` are supported. 

##### Riak-specific options

* `bucket_type` (default `<<"offline">>`) - Riak bucket type.

### Example Configuration
```
{mod_offline, [{access_max_user_messages, max_user_offline_messages}]},
```

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Backend action | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `pop_messages` | histogram | Offline messages for a user are retrieved and deleted from a DB. |
| `write_messages` | histogram | New offline messages to a user are written in a DB. |

