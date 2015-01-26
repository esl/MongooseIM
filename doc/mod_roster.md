### Module Description
These modules implement roster support, specified in [RFC 6121](http://xmpp.org/rfcs/rfc6121.html). Includes support for [XEP-0237 (Roster Versioning)](http://xmpp.org/extensions/xep-0237.html). It can sometimes become quite a heavyweight feature, so there is an option to disable it.
### Options

* **iqdisc**
* **versioning** (boolean, default: `false`) - Turn on/off support for Roster Versioning.
* **store_current_id** (boolean, default: `false`) - Stores last roster hash in DB (used in Roster Versioning). Improves performance but should be disabled, when shared rosters are used.
* **backend** (atom, default: `mnesia`) - Storage backend. Currently only `mnesia` and `odbc` are supported.

### Example Configuration
```
{mod_roster, []}
```