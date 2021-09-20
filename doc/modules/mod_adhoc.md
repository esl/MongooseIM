## Module Description
This module implements [XEP-0050: Ad-Hoc Commands](http://xmpp.org/extensions/xep-0050.html). It allows XMPP entities to remotely execute various commands using forms.

## Options

### `modules.mod_adhoc.iqdisc.type`
* **Syntax:** string, one of `"one_queue"`, `"no_queue"`, `"queues"`, `"parallel"`
* **Default:** `"one_queue"`

Strategy to handle incoming stanzas. For details, please refer to
[IQ processing policies](../configuration/Modules.md#iq-processing-policies).

### `modules.mod_adhoc.report_commands_node`
* **Syntax:** boolean
* **Default:** `false`
* **Example:** `report_commands_node = true`

Determines whether the Ad-Hoc Commands should be announced upon Service Discovery.

## Example configuration
```toml
[modules.mod_adhoc]
  report_commands_node = true
```
