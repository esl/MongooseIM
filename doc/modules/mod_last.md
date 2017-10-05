### Module Description

Implements [XEP-0012: Last Activity](https://xmpp.org/extensions/xep-0012.html).

Use with caution, as it was observed that a user disconnect spike might result in overloading the database with "last activity" writes.

### Options

* **iqdisc** (default: `one_queue`)
* **backend** (atom, default: `mnesia`): Storage backend. Currently `mnesia`, `odbc` and `riak` are supported.

### Example Configuration

` {mod_last, []} `

### Backend metrics

* `[global, backends, mod_last, get_last]` - A time to fetch the timestamp from DB.
* `[global, backends, mod_last, set_last_info]` - A time to store the timestamp in DB.

