## Module Description

This module implements server-side presence handling as specified in [RFC 6121](https://www.rfc-editor.org/rfc/rfc6121).

According to [RFC 6121, section 1.3](https://www.rfc-editor.org/rfc/rfc6121.html#section-1.3):

> it must be possible to use the protocol to provide a presence service,
> a messaging service, or both. (...) it is not mandatory for an XMPP
> service to offer both a presence service and a messaging service,
> and the protocol makes it possible to offer separate and distinct
> services for presence and for messaging.

This is why server-side presence management and broadcasting is provided separately by this module. It is enabled in the default configuration file, but you can disable it if your use case does not require server-side presence handling - this could significantly improve performance.

## Options

This module has no configurable options.

## Example Configuration

```toml
[modules.mod_presence]
```

## Metrics

There are no metrics specific to this module.
