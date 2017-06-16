### Module Description
This module implements [XEP-0050: Ad-Hoc Commands](http://xmpp.org/extensions/xep-0050.html). It allows XMPP entities to remotely execute various commands using forms.

### Options

* **iqdisc** (default: `one_queue`)
* **report_commands_node** (boolean, default: `false`): determines whether the Ad-Hoc Commands should be announced upon Service Discovery

### Example configuration
` {mod_adhoc, [{report_commands_node, true}]} `
