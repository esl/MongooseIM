## Module Description

This module implements [XEP-0279: Server IP Check](http://xmpp.org/extensions/xep-0279.html). It allows clients to ask the server, what is the client IP and port from the server's perspective.

## Options

### `modules.mod_sic.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"one_queue"`

Strategy to handle incoming stanzas. For details, please refer to
[IQ processing policies](../configuration/Modules.md#iq-processing-policies).

## Example Configuration

```toml
[modules.mod_sic]
```
