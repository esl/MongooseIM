### Module Description
This module implements [XEP-0191: Blocking command](http://xmpp.org/extensions/xep-0191.html).
The extension allows blocking the whole communication with a user (or a group of users) with a single command. 
The protocol is much simpler than privacy lists.

### Options

### Example Configuration
```
{mod_blocking, []},
```

The module is not configurable because internally it is an interface to privacy lists, so settings like storage backend apply to it automatically.

Issuing a blocking command creates a privacy list named "blocking" (if it didn't exist), adds to it items being blocked and sets this list as the default.
Unblocking contacts removes them from "blocking" privacy list.

If the user has other online resources which use privacy lists it may result in a different behaviour per resource; this is normal, and provided for in XEP.

Similar to privacy lists, a blocked contact sees the user as offline no matter what their real status is.

If the contact being blocked is subscribed to the user's presence, they receive an "unavailable" presence; when unblocked, they receive the current status of the user.
