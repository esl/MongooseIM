### Module Description
This module implements [XEP-0049 (Private XML Storage)](http://xmpp.org/extensions/xep-0049.html), allowing users to store custom XML data in server database. Used e.g. for storing roster groups separator.

### Options
* **iqdisc**
* **backend** (atom, default: `mnesia`) - Storage backend. Currently `mnesia`, `odbc` and `mysql` are supported . `mysql` uses MySQL-specific queries so in some cases it is more efficient than generic `odbc`.

### Example Configuration
```
{mod_private, []}
```