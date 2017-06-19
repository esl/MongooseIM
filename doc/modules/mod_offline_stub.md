### Module Description

RFC 6121 requires a `<service-unavailable/>` stanza error to be sent to a user messaging an unavailable recipient if the message is not stored for delayed delivery (i.e. as an "offline message").
If the recipient exists (i.e. auth module returns `true` from `is_user_exists`), `mod_mam` stores the message, but <service-unavailable/> is still returned. This is not compliant with the RFC.
This module prevents returning <service-unavailable/>.
Please note that `mod_offline_stub` is not tightly coupled with `mod_mam`. It can be used as a standalone extension, if the specific application requires it.

### Options

None.

### Example Configuration

```
{mod_offline_stub, []},
```

