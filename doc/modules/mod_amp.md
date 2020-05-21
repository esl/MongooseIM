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

  * 3.3.1 deliver: **supported** for values: `direct`, `stored`, and `none`. The `stored` condition works with `mod_mam` and `mod_offline`. **Note**: if both `mod_mam` and `mod_offline` are enabled, some delivery conditions may not work correctly.
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

### Modifications

The following behaviour differs from or extends the guidelines provided in the XEP.

* The action for the `deliver` condition with value `stored` is deferred until the message is stored by `mod_mam` or `mod_offline`.
* The action for the `deliver` condition with value `direct` is deferred until the message is sent to the recipient's socket.

### Server Processing Details

When a message with AMP rules is being processed by the server, several system events may occur. For a given event, the rules are processed and each of them can get the *matched* or *undecided* status or, if the conditions are not met, it gets no status. If any rules get the *matched* status, the action for the first of them is performed. After that, the rule list is filtered so that only the *undecided* ones are left in the message, as they may be matched later.

The following system events are defined:

* *initial check* - always occurs first, when the message enters the system.
* *mod_mam failed* - `mod_mam` is enabled but fails to store the message.
* *mod_offline failed* - the recipient is offline and `mod_offline` is enabled but fails to store the message.
* *archived* - either `mod_mam` or `mod_offline` has successfully stored the message.
* *delivery failed* - the message was about to be delivered, but it could not be sent.
* *delivered* - the message has been sent to the recipient. Mutually exclusive with *delivery failed*.

Rule status is determined for each system event in the following way:

* **initial check**

    * If the recipient is online, rules for the `direct` and `none` values of the `deliver` condition become *undecided*, except rules for the `direct` value with action `error` or `drop`, which become *matched*. If `mod_mam` is enabled, rules for the `stored` value of the `deliver` condition become *undecided*.
        * If the recipient has a session for the target resource, rules for the `exact` and `any` values of the `match-resource` condition become *matched*.
        * Otherwise, rules for the `other` and `any` values of the `match-resource` condition become *matched*.
    * If the recipient is offline:
        * If `mod_mam` or `mod_offline` is enabled, rules for the `stored` and `none` values of the `deliver` conditions become *undecided*, except rules for the `stored` value with action `error` or `drop`, which become *matched*.
        * If both `mod_mam` and `mod_offline` are disabled, rules for the `none` delivery condition become *matched*.

* **mod_mam failed**

    * If the recipient is online, rules for `direct` and `none` values of the `deliver` condition become *undecided*.
    * If the recipient is offline, rules for the `none` value of the `deliver` condition become *matched*.

* **mod_offline failed**

    * Rules for the `none` value of the `deliver` condition become *matched*.

* **archived**

    * If the recipient is online, rules for `direct` and `stored` values of the `deliver` condition become *undecided*.
    * If the recipient is offline, rules for the `stored` value of the `deliver` condition become *matched*.

* **delivery failed**

    * Rules for the `none` and `stored` value of the `deliver` condition become *matched*.

* **delivered**

    * Rules for the `direct` value of the `deliver` condition become *matched*.
