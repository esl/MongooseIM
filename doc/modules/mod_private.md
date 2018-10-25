### Module Description
This module implements [XEP-0049: Private XML Storage](http://xmpp.org/extensions/xep-0049.html), allowing users to store custom XML data in the server's database. Used e.g. for storing roster groups separator.

### Options
* `iqdisc` (default: `one_queue`)
* `backend` (atom, default: `mnesia`): Storage backend. Currently `mnesia`, `rdbms`, `riak` and `mysql` are supported . `mysql` uses MySQL-specific queries so in some cases it is more efficient than generic `rdbms`.

**CAUTION:**  Riak KV backend doesn't support transactions (rollbacks), so please avoid inserting more
than one value in one set request, otherwise you may end up with partially save data, backend returns
first error.

### Example Configuration
```
{mod_private, []}
```

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[global, backends, mod_private, multi_get_data]` | histogram | Time it takes to fetch XML data from a DB. |
| `[global, backends, mod_private, multi_set_data]` | histogram | Time it takes to store XML data in a DB. |

