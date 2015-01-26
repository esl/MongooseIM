### Module Description
These modules implement roster support, specified in [RFC 6121](http://xmpp.org/rfcs/rfc6121.html). Includes support for [XEP-0237 (Roster Versioning)](http://xmpp.org/extensions/xep-0237.html). It can sometimes become quite a heavyweight feature, so there is an option to disable it. This module comes in two flavours: Mnesia and ODBC. It is not yet possible to switch backends by using the `backend` option, like in case of some other modules. Backend is changed by enabling either `mod_roster` (i.e. Mnesia) or `mod_roster_odbc`.

### Options

* **iqdisc**
* **versioning** (boolean, default: `false`) - Turn on/off support for Roster Versioning.
* **store_current_id** (boolean, default: `false`) - Stores last roster hash in DB (used in Roster Versioning). Improves performance but should be disabled, when shared rosters are used.

### Example Configuration
```
{mod_roster, []}
```