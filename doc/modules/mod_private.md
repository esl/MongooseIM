### Module Description
This module implements [XEP-0049: Private XML Storage](http://xmpp.org/extensions/xep-0049.html), allowing users to store custom XML data in the server's database. Used e.g. for storing roster groups separator.

### Options
* `iqdisc` (default: `one_queue`)
* `backend` (atom, default: `mnesia`): Storage backend. Currently `mnesia`, `rdbms`, `riak` and `mysql` are supported . `mysql` uses MySQL-specific queries so in some cases it is more efficient than generic `rdbms`.

**CAUTION:**  Riak KV backend doesn't support transactions (rollbacks), so please avoid inserting more
than one value in a single set request, otherwise you may end up with partially saved data. Backend returns the
first error.

##### Riak-specific options

* `bucket_type` (default `<<"private">>`) - Riak bucket type.

### Example Configuration
```
{mod_private, []}
```

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Backend operation | Description (when it gets incremented) |
| ---- | -------------------------------------- |
| `multi_get_data` | XML data is fetched from a DB. |
| `multi_set_data` | XML data is stored in a DB. |

