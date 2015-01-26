### Module Description
This module implements [XEP-0050 (Ad-Hoc Commands)](http://xmpp.org/extensions/xep-0050.html). It is sometimes desirable to allow XMPP users execute various commands on the server side. 

### Options

* **iqdisc**
* **report_commands_node** (boolean, default: `false`) - determines whether the Ad-Hoc Commands should be announced upon Service Discovery

### Example configuration
` {mod_adhoc, []} `