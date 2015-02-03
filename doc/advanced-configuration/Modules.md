MongooseIM provides a wide range of pluggable modules, that implement various XEPs (like `mod_muc`) and custom functionalities (e.g. `mod_metrics`).

## Module list
Some of the modules feature an `iqdisc` parameter. It defines the method of handling incoming IQ stanzas. Please refer to [[IQ handlers]] for more information. Valid values: `no_queue`, `one_queue`, `{queues, N}`, `parallel`. Default: `one_queue`.

### [mod_admin_extra](../modules/mod_admin_extra.md)
Significantly extends `mongooseimctl` script capabilities. 

### [mod_adhoc](../modules/mod_adhoc.md)
Implements [XEP-0050 (Ad-Hoc Commands)](http://xmpp.org/extensions/xep-0050.html). 

### [mod_amp](../modules/mod_amp.md)
Implements a subset of [XEP-0079: Advanced Message Processing](http://xmpp.org/extensions/xep-0079.html) functionality.

### [mod_bosh](../modules/mod_bosh.md)
Allows users to connect to MongooseIM using BOSH (Bidirectional-streams Over Synchronous HTTP)

### [mod_carboncopy](../modules/mod_carboncopy.md)
Implements [XEP-0280 (Message Carbons)](http://xmpp.org/extensions/xep-0280.html).

### [mod_disco](../modules/mod_disco.md)
Implements [XEP-0030 (Service Discovery)](http://xmpp.org/extensions/xep-0030.html).

### [mod_last](../modules/mod_last.md)
Implements [XEP-0012 (Last Activity)](http://xmpp.org/extensions/xep-0012.html). 

### [mod_mam](../modules/mod_mam.md)
Implements revision 0.2 of [XEP-0313 (Message Archive Management)](http://xmpp.org/extensions/attic/xep-0313-0.2.html). 

### [mod_metrics](../modules/mod_metrics.md)
Enables the gathering various XMPP-related statistics.

### [mod_muc](../modules/mod_muc.md)
Implements [XEP-0045 (Multi-User Chat)](http://xmpp.org/extensions/xep-0045.html). 

### [mod_muc_log](../modules/mod_muc_log.md)
Implements a logging subsystem for [mod_muc].

### [mod_offline](../modules/mod_offline.md)
Provides offline storage compliant with [XEP-0160 (Best Practices for Handling Offline Messages)](http://xmpp.org/extensions/xep-0160.html).

### [mod_privacy](../modules/mod_privacy.md)
This module implements [XEP-0016 (Privacy Lists)](http://xmpp.org/extensions/xep-0016.html).

### [mod_private](../modules/mod_private.md)
Implements [XEP-0049 (Private XML Storage)](http://xmpp.org/extensions/xep-0049.html)

### [mod_register](../modules/mod_register.md)
Implements [XEP-0077 (In-Band Registration)](http://xmpp.org/extensions/xep-0077.html).

### [mod_roster](../modules/mod_roster.md)
Roster support, specified in [RFC 6121](http://xmpp.org/rfcs/rfc6121.html). Includes support for [XEP-0237 (Roster Versioning)](http://xmpp.org/extensions/xep-0237.html).

### [mod_shared_roster_ldap](../modules/mod_shared_roster_ldap.md)
This module, when enabled, will inject roster entries fetched from LDAP. 

### [mod_sic](../modules/mod_sic.md)
Implements [XEP-0279 (Server IP Check)](http://xmpp.org/extensions/xep-0279.html).

### [mod_stream_management](../modules/mod_stream_management.md)
Enables [XEP-0198 (Stream Management)](http://xmpp.org/extensions/xep-0198.html) functionality. 

### [mod_vcard](../modules/mod_vcard.md)
Provides support for VCards, as specified in [XEP-0054 (vcard-temp)](http://xmpp.org/extensions/xep-0054.html) and [XEP-0055 (Jabber Search)](http://xmpp.org/extensions/xep-0055.html).

### [mod_websockets](../modules/mod_websockets.md)
Allows users to connect to MongooseIM using Websockets.

