### Module Description
Use with caution, as it was observed that a user disconnect spike might result in overloading the database with "last activity" writes.

### Options
* **iqdisc**
* **backend** (atom, default: `mnesia`) - Storage backend. Currently only `mnesia`, `odbc` are supported.

### Example Configuration
` {mod_last, []} `
