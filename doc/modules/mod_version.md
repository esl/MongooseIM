### Module description

This module provides the functionality specified in [XEP-0092: Software Version](https://xmpp.org/extensions/xep-0092.html).

### Options

#### `modules.mod_version.iqdisc`
* **Syntax:** string
* **Default:** `"no_queue"`
* **Example:** `iqdisc = "one_queue"`

#### `modules.mod_version.os_info`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `os_info = true`

Determines whether information about the operating system will be included.

### Example configuration 

```
[modules.mod_version]
  os_info = true
```
