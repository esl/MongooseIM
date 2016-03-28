### Module Description
This module implements [XEP-0049 (Private XML Storage)](http://xmpp.org/extensions/xep-0049.html), allowing users to store custom XML data in server database. Used e.g. for storing roster groups separator.

### Options
* **iqdisc**
* **backend** (atom, default: `mnesia`) - Storage backend. Currently `mnesia`, `odbc`, `riak`, `cassandra` and `mysql` are supported . `mysql` uses MySQL-specific queries so in some cases it is more efficient than generic `odbc`.

**CAUTION:**  Riak backend doesn't support transactions(rollbacks), so please avoid inserting more
than one value in one set request, otherwise you may end up with partially save data, backend returns
first error.

### Example Configuration
```
{mod_private, []}
```
