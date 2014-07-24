mod_amp: XEP-0079 support for MongooseIM
========================================


This module enables support for a subset of the functionality described under
[XEP-0079: Advanced Message
Processing](http://xmpp.org/extensions/xep-0079.html).

Advanced Message Processing (amp) is enabled by adding the following Erlang
term to the `ejabberd.cfg` file, in the `modules` section:

```
  {mod_amp, []},
```

The module is currently not configurable.



XEP support status
------------------

What follows is a short description of which parts of the XEP-0079 specification
mod_amp supports.


### 2.1.1 Service Discovery

  * Both the service discovery information response (Ex.1, 2) and the
    request/response for individual actions and conditions (Ex.3, 4) are
    **supported**.

### 2.1.2 Specifying Semantics
    
  * "Per-hop" rule semantics are **not supported**, i.e. ignored.

### 2.2 Server Processing

  * 2.2.1 Validating Semantics: Performed as in the XEP. The first message to
    fail validation determines the error message.
  * 2.2.2 **supported** to spec.
  * 2.2.3 **supported** to spec.
  * 2.2.4 **supported** for actions: `error` and `notify`.
  * 2.2.5 **supported** for events: `error` and `notify`.

### 3.3 Defined Conditions

  * 3.3.1 deliver: **supported** for values: `direct`, `forward`, and `none`.
  * 3.3.2 expire-at: **not supported**
  * 3.3.3 match-resource: **supported**

### 3.4 Defined Actions
  
  * 3.4.1 alert: **not supported**
  * 3.4.2 drop: **not supported**
  * 3.4.3 error: **supported**
  * 3.4.4 notify: **supported**

### 6. Error Handling

The following error types are **supported** to spec:

  * 6.2.1 Unsupported Action
  * 6.2.2 Unsupported Condition
  * 6.2.3 Not Acceptable
  * 6.2.5 Undefined Condition

Error type 6.2.4 Service Unavailable is **not supported**, as it pertains to
"per-hop" rule processing..


### 8. Stream Feature

  * **supported**

### 9. Security Considerations

Currently, the security measures described in this section have not been
implemented. It follows that `mod_amp`, in its current state, should only be
enabled for servers/domains where user privacy is not a requirement.












