### Module description

This module provides the functionality specified in [XEP-0092: Software Version](https://xmpp.org/extensions/xep-0092.html).

### Options

* `iqdisc` (default: `one_queue`)
* `os_info` (boolean, default: `false`): Determines wheter information about the operating system will be included.

### Example configuration 

```
{mod_version, [{os_info, true}]}
```
