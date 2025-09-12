## Module Description

This module implements [XEP-0191: Blocking command](http://xmpp.org/extensions/xep-0191.html).
The extension allows blocking the whole communication with a user (or a group of users) with a single command. 
The protocol is much simpler than privacy lists.

!!! Info "Relation to `mod_privacy`"
    This module uses [`mod_privacy`](./mod_privacy.md) as a dependency, because internally it is an interface to privacy lists.
    If you configure `mod_blocking`, `mod_privacy` will be started automatically with the same options.

Issuing a blocking command creates a privacy list named "blocking" (if it didn't exist), adds to it items being blocked and sets this list as the default.
Unblocking contacts removes them from "blocking" privacy list.

If the user has other online resources which use privacy lists it may result in a different behaviour per resource; this is normal, and provided for in XEP.

Similar to privacy lists, a blocked contact sees the user as offline no matter what their real status is.

If the contact being blocked is subscribed to the user's presence, they receive an "unavailable" presence; when unblocked, they receive the current status of the user.

The user can be blocked in the context of groupchat by using `domain/resource` format of the jid, see [JID Matching](https://xmpp.org/extensions/xep-0191.html#matching).
The `domain` part should be then a configured `muc` or `muclight` domain, e.g. `conference.localhost` or `muclight.localhost`.
The resource is then respectively user's nickname in case of `muc` or user's bare jid in case of `muclight`.

Note: When a blocked user sends a message to a room, the server does not respond with an error but silently ignores it, i.e. doesn't deliver it to the user that issued the blocking.

## Options

Same as in [`mod_privacy`](./mod_privacy.md#options).

## Example Configuration

```toml
[modules.mod_blocking]
  backend = "rdbms"
```
