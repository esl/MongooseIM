### Module Description

This module enables support for communicating the local time of an entity. 
It reports time in UTC according to the entity as well as the offset from UTC. 
Protocol is described under [XEP-0202: Entity Time](http://www.xmpp.org/extensions/xep-0202.html).

### Options

#### `modules.mod_time.iqdisc`
* **Syntax:** string
* **Default:** `"one_queue"`
* **Example:** `iqdisc = "one_queue"`

### Example Configuration

```
[modules.mod_time]
```
