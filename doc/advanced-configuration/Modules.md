MongooseIM provides a wide range of pluggable and configurable modules, that implement various features including XEPs.
For instance `mod_muc` enables Multi-User Chat (group chat), `mod_mam` gives us Message Archive Management, and `mod_stream_management` is for stanza acknowledgement and stream resumption.
This modular architecture provides great flexibility for everyday operations and feature development.

A module configuration generally looks like this:
```
  {mod_muc, [
             {host, "muc.@HOST@"},
             {access, muc},
             {access_create, muc_create}
            ]},
```

## Module list
Some of the modules feature an `iqdisc` parameter.
It defines the method for handling incoming IQ stanzas.
Please refer to [[IQ handlers]] for more information.
Valid values: `no_queue`, `one_queue`, `{queues, N}`, `parallel`. Default: `one_queue`.

### [mod_adhoc](../modules/mod_adhoc.md)
Implements [XEP-0050: Ad-Hoc Commands](http://xmpp.org/extensions/xep-0050.html) for advertising and executing application-specific commands, such as those related to a configuration workflow, using [XEP-0004: Data Forms](http://xmpp.org/extensions/xep-0004.html) in order to structure the information exchange.
This is extremely useful for use cases such as remote administration, user engagement via polls, and ChatBots.

### [mod_amp](../modules/mod_amp.md)
Implements a subset of [XEP-0079: Advanced Message Processing](http://xmpp.org/extensions/xep-0079.html) functionality, that enables entities to request, and servers to perform advanced processing of XMPP message stanzas, including reliable data transport, time-sensitive delivery, and expiration of transient messages.

### [mod_auth_token](../modules/mod_auth_token.md)
A module used by SASL X-OAUTH mechanism.
It provides an API to manage [custom OAuth tokens](../open-extensions/token-reconnection.md).
It requires [mod_keystore](../modules/mod_keystore.md) as an actual key database.

### [mod_blocking](../modules/mod_blocking.md)
Implements [XEP-0191: Blocking Command](http://xmpp.org/extensions/xep-0191.html), a simplified interface to privacy lists.

### [mod_bosh](../modules/mod_bosh.md)
Allows users to connect to MongooseIM using BOSH (Bidirectional-streams Over Synchronous HTTP), the HTTP long-polling technique described in [XEP-0124: Bidirectional-streams Over Synchronous HTTP (BOSH)](http://xmpp.org/extensions/xep-0124.html) and [XEP-0206: XMPP Over BOSH](http://xmpp.org/extensions/xep-0206.html).

### [mod_caps](../modules/mod_caps.md)
Implements [XEP-0115 Entity Capabilities](https://xmpp.org/extensions/xep-0115.html).
It queries clients for their supported functionalities and caches them in Mnesia.
This module tightly cooperates with [mod_pubsub](../modules/mod_pubsub.md) in order to deliver [PEP](https://xmpp.org/extensions/xep-0163.html) events to user's subscribers.

### [mod_carboncopy](../modules/mod_carboncopy.md)
Implements [XEP-0280: Message Carbons](http://xmpp.org/extensions/xep-0280.html) in order to keep all IM clients for a user engaged in a real-time conversation by carbon-copying all inbound and outbound messages to all interested resources (Full JIDs).

### [mod_commands](../modules/mod_commands.md)
A central gateway providing access to a subset of MongooseIM functions by channels other than XMPP.
Commands defined there are currently accessible via REST API.

### [mod_csi](../modules/mod_csi.md)
Enables the [XEP-0352: Client State Indication](http://xmpp.org/extensions/xep-0352.html) functionality.

### [mod_disco](../modules/mod_disco.md)
Implements [XEP-0030: Service Discovery](http://xmpp.org/extensions/xep-0030.html) for discovering information (capabilities, protocols, features) about other XMPP entities.

### [mod_event_pusher](../modules/mod_event_pusher.md)
A framework module to build other notification-based modules on.

#### [mod_event_pusher_sns](../modules/mod_event_pusher_sns.md)
Allows sending online/offline notifications, chat and groupchat messages as events to [Amazon Simple Notification Service](https://aws.amazon.com/sns/).

#### [mod_event_pusher_rabbit](../modules/mod_event_pusher_rabbit.md)
Allows sending presence changes (to available/unavailable), chat and groupchat messages as events to a RabbitMQ server.

#### [mod_event_pusher_push](../modules/mod_event_pusher_push.md)
Implements [XEP-0357 Push Notifiactions](https://xmpp.org/extensions/xep-0357.html) to provide push notifications to clients that are temporary unavailable.

#### [mod_event_pusher_http](../modules/mod_event_pusher_http.md)
Forward events to an external HTTP service.
This applies to situations such as sending messages or presences to mobile/SMS/email push service, big data, or an analytics service.

### [mod_http_upload](../modules/mod_http_upload.md)
Implements [XEP-0363: HTTP File Upload](https://xmpp.org/extensions/xep-0363.html) for coordinating with an XMPP server to upload files via HTTP and receive URLs that can be shared in messages.

### [mod_inbox](../modules/mod_inbox.md)
Implements custom inbox XEP

### [mod_global_distrib](../modules/mod_global_distrib.md)
Enables sharing a single XMPP domain between distinct datacenters (**experimental**).

### [mod_jingle_sip](../modules/mod_jingle_sip.md)
Enables Jingle to SIP and SIP to Jingle translator.

### [mod_keystore](../modules/mod_keystore.md)
Serves as a storage for crypto keys for `mod_auth_token`.

### [mod_last](../modules/mod_last.md)
Implements [XEP-0012: Last Activity)](http://xmpp.org/extensions/xep-0012.html) for communicating information about the last activity associated with an XMPP entity (most recent presence information from an offline contact).

### [mod_mam](../modules/mod_mam.md)
Implements [XEP-0313: Message Archive Management](http://xmpp.org/extensions/xep-0313.html), that defines a protocol to query and control an archive of messages stored on a server.

### [mod_muc](../modules/mod_muc.md)
Implements [XEP-0045: Multi-User Chat)](http://xmpp.org/extensions/xep-0045.html), for a featureful multi-user text chat (group chat), whereby multiple XMPP users can exchange messages in the context of a chat room.
It is tightly coupled with user presence in chat rooms.

### [mod_muc_commands](../modules/mod_muc_commands.md)
Provides `mod_muc` related `mongoose_commands`, accessible via the client REST API.

### [mod_muc_log](../modules/mod_muc_log.md)
Implements a logging subsystem for [mod_muc].

### [mod_muc_light](../modules/mod_muc_light.md)
Implements [XEP Multi-User Chat Light](https://github.com/xsf/xeps/pull/118).

### [mod_muc_light_commands](../modules/mod_muc_light_commands.md)
Provides `mod_muc_light` related `mongoose_commands`, accessible via client REST API.

### [mod_offline](../modules/mod_offline.md)
Provides an offline messages storage that is compliant with [XEP-0160: Best Practices for Handling Offline Messages)](http://xmpp.org/extensions/xep-0160.html).

### [mod_offline_stub](../modules/mod_offline_stub.md)
Prevents `<service-unavailable/>` error when the message recipient is offline.

### [mod_ping](../modules/mod_ping.md)
Implements [XEP-0199 XMPP Ping](http://xmpp.org/extensions/xep-0199.html), enabling periodic XMPP pings sent to clients and responds to those sent from clients.

### [mod_privacy](../modules/mod_privacy.md)
This module implements [XEP-0016: Privacy Lists)](http://xmpp.org/extensions/xep-0016.html), for enabling or disabling communication with other entities on a network.

### [mod_private](../modules/mod_private.md)
Implements [XEP-0049 (Private XML Storage)](http://xmpp.org/extensions/xep-0049.html) to store and query private user data in XML format.

### [mod_pubsub](../modules/mod_pubsub.md)
This extension implements [XEP-0060 (Publish-Subscribe)](http://www.xmpp.org/extensions/xep-0060.html). It is a pluggable implementation using behaviours provided by `node_*.erl` and `nodetree_*.erl` modules.

### [mod_push_service_mongoosepush](../modules/mod_push_service_mongoosepush.md)
Handles push notifications generated by [mod_pubsub](../modules/mod_pubsub.md)'s `node_push` and passes them to [MongoosePush](https://github.com/esl/MongoosePush) service.

### [mod_register](../modules/mod_register.md)
Implements [XEP-0077: In-Band Registration)](http://xmpp.org/extensions/xep-0077.html), that enables creating an account and changing the password once connected.
This does not provide a solution to the forgotten password use case via SMS or email.

### [mod_revproxy](../modules/mod_revproxy.md)
With this extension, MongooseIM may serve as a reverse proxy.

### [mod_roster](../modules/mod_roster.md)
Roster support, specified in [RFC 6121](http://xmpp.org/rfcs/rfc6121.html).
Includes support for [XEP-0237: Roster Versioning](http://xmpp.org/extensions/xep-0237.html).

### [mod_shared_roster_ldap](../modules/mod_shared_roster_ldap.md)
This module, when enabled, will inject roster entries fetched from LDAP.

### [mod_sic](../modules/mod_sic.md)
Implements [XEP-0279: Server IP Check)](http://xmpp.org/extensions/xep-0279.html) that enables a client to discover its external IP address.

### [mod_stream_management](../modules/mod_stream_management.md)
Enables [XEP-0198: Stream Management](http://xmpp.org/extensions/xep-0198.html) functionality that defines the active management of an XML stream between two XMPP entities, including features for stanza acknowledgements and stream resumption.

### [mod_time](../modules/mod_time.md)
[XEP-0202: Entity Time](http://www.xmpp.org/extensions/xep-0202.html) implementation. With this extensions, clients can get the current server time.

### [mod_vcard](../modules/mod_vcard.md)
Provides support for vCards, as specified in [XEP-0054: vcard-temp](http://xmpp.org/extensions/xep-0054.html) and [XEP-0055: Jabber Search](http://xmpp.org/extensions/xep-0055.html).

### [mod_version](../modules/mod_version.md)
This module provides the functionality specified in [XEP-0092: Software Version](https://xmpp.org/extensions/xep-0092.html).
