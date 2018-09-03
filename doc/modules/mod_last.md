### Module Description

Implements [XEP-0012: Last Activity](https://xmpp.org/extensions/xep-0012.html).

Use with caution, as it was observed that a user disconnect spike might result in overloading the database with "last activity" writes.

### Options

* **iqdisc** (default: `one_queue`)
* **backend** (atom, default: `mnesia`): Storage backend. Currently `mnesia`, `rdbms` and `riak` are supported.

### Example Configuration

` {mod_last, []} `

### Metrics

If you'd like to learn more about metrics in MongooseIM, please visit [MongooseIM metrics](../operation-and-maintenance/Mongoose-metrics.md) page.

| Name | Type | Description (when it gets incremented) |
| ---- | ---- | -------------------------------------- |
| `[global, backends, mod_last, get_last]` | histogram | Time it takes to fetch the timestamp from DB. |
| `[global, backends, mod_last, set_last_info]` | histogram | Time it takes to store the timestamp in DB. |

