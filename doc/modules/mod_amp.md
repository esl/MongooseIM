### Module Description

This module enables support for a subset of the functionality described under
[XEP-0079: Advanced Message
Processing](http://xmpp.org/extensions/xep-0079.html). It currently does not
provide features related to timed delivery, i.e the `expire-at` condition.

The `error` and `notify` actions are supported, while `alert` and `drop` are
not. See more below, under XEP Support.


### Options

none

### Example Configuration

```
  {mod_amp, []},
```

### XEP Support

What follows is a short description of which parts of the XEP-0079 specification
mod_amp supports.

* 2.1.1 Service Discovery

  * Both the service discovery information response (Ex.1, 2) and the
    request/response for individual actions and conditions (Ex.3, 4) are
    **supported**.

* 2.1.2 Specifying Semantics

  * "Per-hop" rule semantics are **not supported**, i.e. ignored.

* 2.2 Server Processing

  * 2.2.1 Validating Semantics: Performed as in the XEP. The first message to
  fail validation determines the error message.
  * 2.2.2 **supported** to spec.
  * 2.2.3 **supported** to spec.
  * 2.2.4 **supported** for actions: `error` and `notify`.
  * 2.2.5 **supported** for events: `error` and `notify`.

* 3.3 Defined Conditions

  * 3.3.1 deliver: **supported** for values: `direct`, `stored`, and `none`. The `stored` condition works with `mod_mam` and `mod_offline`.
  * 3.3.2 expire-at: **not supported**
  * 3.3.3 match-resource: **supported**

* 3.4 Defined Actions

  * 3.4.1 alert: **not supported**
  * 3.4.2 drop: **not supported**
  * 3.4.3 error: **supported**
  * 3.4.4 notify: **supported**. Notifications for the `stored` and `direct` conditions are sent as soon as the message has been stored or sent to the recipient.

* 6. Error Handling

  * 6.2.1 Unsupported Action: **supported**
  * 6.2.2 Unsupported Condition: **supported**
  * 6.2.3 Not Acceptable: **supported**
  * 6.2.4 Service Unavailable is **not supported**, as it pertains to "per-hop" rule processing
  * 6.2.5 Undefined Condition: **supported**

* 8. Stream Feature

  * **supported**

* 9. Security Considerations

  * Currently, the security measures described in this section have not been
implemented. It follows that `mod_amp`, in its current state, should only be
enabled for servers/domains where user presence leaks are not a threat, i.e
services where all users can see each other's presence by default.
