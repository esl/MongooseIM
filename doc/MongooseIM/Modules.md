MongooseIM provides a wide range of pluggable modules, that implement various XEPs (like `mod_muc`) and custom functionalities (e.g. `mod_metrics`).

## Module list
Some of the modules feature an `iqdisc` parameter. It defines the method of handling incoming IQ stanzas. Please refer to [[IQ handlers]] for more information. Valid values: `no_queue`, `one_queue`, `{queues, N}`, `parallel`. Default: `one_queue`.

### [[mod_admin_extra]]
Significantly extends `mongooseimctl` script capabilities. 

### [[mod_adhoc]]
Implements [XEP-0050 (Ad-Hoc Commands)](http://xmpp.org/extensions/xep-0050.html). 

### [[mod_amp]]
Implements a subset of [XEP-0079: Advanced Message Processing](http://xmpp.org/extensions/xep-0079.html) functionality.

### [[mod_bosh]]
Allows users to connect to MongooseIM using BOSH (Bidirectional-streams Over Synchronous HTTP)

### [[mod_carboncopy]]
Implements [XEP-0280 (Message Carbons)](http://xmpp.org/extensions/xep-0280.html).

### [[mod_disco]]
Implements [XEP-0030 (Service Discovery)](http://xmpp.org/extensions/xep-0030.html).

### [[mod_last and mod_last_odbc]]
Implements [XEP-0012 (Last Activity)](http://xmpp.org/extensions/xep-0012.html). 

### [[mod_mam]]
Implements revision 0.2 of [XEP-0313 (Message Archive Management)](http://xmpp.org/extensions/attic/xep-0313-0.2.html). 

### [[mod_metrics]]
Enables the gathering various XMPP-related statistics.

### [[mod_muc]]
Implements [XEP-0045 (Multi-User Chat)](http://xmpp.org/extensions/xep-0045.html). 

### [[mod_muc_log]]
Implements a logging subsystem for [[mod_muc]].

### [[mod_offline]]
Provides offline storage compliant with [XEP-0160 (Best Practices for Handling Offline Messages)](http://xmpp.org/extensions/xep-0160.html).

### [[mod_privacy]]
This module implements [XEP-0016 (Privacy Lists)](http://xmpp.org/extensions/xep-0016.html).

### [[mod_private]]
Implements [XEP-0049 (Private XML Storage)](http://xmpp.org/extensions/xep-0049.html)

### [[mod_register]]
Implements [XEP-0077 (In-Band Registration)](http://xmpp.org/extensions/xep-0077.html).

### [[mod_roster and mod_roster_odbc]]
Roster support, specified in [RFC 6121](http://xmpp.org/rfcs/rfc6121.html). Includes support for [XEP-0237 (Roster Versioning)](http://xmpp.org/extensions/xep-0237.html).

### [[mod_shared_roster_ldap]]
This module, when enabled, will inject roster entries fetched from LDAP. 

### [[mod_sic]]
Implements [XEP-0279 (Server IP Check)](http://xmpp.org/extensions/xep-0279.html).

### [[mod_stream_management]]
Enables [XEP-0198 (Stream Management)](http://xmpp.org/extensions/xep-0198.html) functionality. 

### [[mod_vcard]]
Provides support for VCards, as specified in [XEP-0054 (vcard-temp)](http://xmpp.org/extensions/xep-0054.html) and [XEP-0055 (Jabber Search)](http://xmpp.org/extensions/xep-0055.html).

### [[mod_websockets]]
Allows users to connect to MongooseIM using Websockets.

