## Module Description

This module enables support for communicating the local time of an entity. 
It reports time in UTC according to the entity as well as the offset from UTC. 
Protocol is described under [XEP-0202: Entity Time](http://www.xmpp.org/extensions/xep-0202.html).

## Options

### `modules.mod_time.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"one_queue"`

Strategy to handle incoming stanzas. For details, please refer to
[IQ processing policies](../configuration/Modules.md#iq-processing-policies).

## Example Configuration

```toml
[modules.mod_time]
```
