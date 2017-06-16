### Module Description
This module implements [XEP-0016: Privacy Lists](http://xmpp.org/extensions/xep-0016.html). This extension allows user to block IQs, messages, presences, or all, based on JIDs, subscription, and roster groups.

### Options
* `backend` (atom, default: `mnesia`): Storage backend. Currently supported are `mnesia`, `odbc` and `riak`.

### Example Configuration
```
{mod_privacy, []},
```
