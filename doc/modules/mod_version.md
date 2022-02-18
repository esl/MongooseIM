## Module description

This module provides the functionality specified in [XEP-0092: Software Version](https://xmpp.org/extensions/xep-0092.html).

## Options

### `modules.mod_version.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"no_queue"`

Strategy to handle incoming stanzas. For details, please refer to
[IQ processing policies](../configuration/Modules.md#iq-processing-policies).

### `modules.mod_version.os_info`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `os_info = true`

Determines whether information about the operating system will be included.

## Example configuration

```toml
[modules.mod_version]
  os_info = true
```
