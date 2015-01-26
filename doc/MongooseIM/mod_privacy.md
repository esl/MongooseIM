### Module Description
This module implements [XEP-0016 (Privacy Lists)](http://xmpp.org/extensions/xep-0016.html). This extension allows user to e.g. block other users or hide their presence.

### Options
* **iqdisc**
* **backend** (atom, default: `mnesia`) - Storage backend. Currently supported are `mnesia` and `odbc`.

### Example Configuration
```
{mod_privacy, []},
```