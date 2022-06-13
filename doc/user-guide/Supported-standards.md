# Supported standards

* XMPP Core: [RFC 3920](https://tools.ietf.org/html/rfc3920),
[RFC 6120](https://tools.ietf.org/html/rfc6120)
  
    !!! Note
        In RFC 6120 there are 3 different strategies defined in case of a session conflict (same full JID).
        They are described in [7.7.2.2. Conflict](https://tools.ietf.org/html/rfc6120#section-7.7.2.2).
        MongooseIM always uses the 3rd option.
        It terminates the older session with a `<conflict/>` stream error.

* XMPP Instant Messaging and Presence: [RFC 3921](https://tools.ietf.org/html/rfc3921),
[RFC 6121](https://tools.ietf.org/html/rfc6121)
* Client connections:
    * over TCP (with TLS/STARTTLS available) as defined in
    [RFC 6120](https://tools.ietf.org/html/rfc6120)
    * over WebSockets as defined in  [RFC 7395](https://tools.ietf.org/html/rfc7395)
    * over HTTP(S) long-polling (BOSH) as defined in
    [XEP-0124: Bidirectional-streams Over Synchronous HTTP (BOSH)](http://xmpp.org/extensions/xep-0124.html) and
    [XEP-0206: XMPP Over BOSH](http://xmpp.org/extensions/xep-0206.html)
    * [REST API](../rest-api/Client-frontend.md)
* Server/backend connections:
    * [REST API](../rest-api/Administration-backend.md)
* Configurable database backends:
    * Transient:
        * Mnesia
        * Redis
    * Persistent:
        * RDBMS: MySQL, PostgreSQL, generic ODBC
        * NoSQL: Riak KV, Cassandra
* Integration with third-party services
    * [Amazon Simple Notification Service](../modules/mod_event_pusher_sns.md)

## Supported XEPs

|XEP Number|Name|Module|
| ------------- | ------------- | ------------- |
|0004|[Data Forms](http://xmpp.org/extensions/xep-0004.html)||
|0012|[Last Activity](http://xmpp.org/extensions/xep-0012.html)|[`mod_last`](../modules/mod_last.md)|
|0016|[Privacy Lists](http://xmpp.org/extensions/xep-0016.html)|[`mod_privacy`](../modules/mod_privacy.md)|
|0018|[Invisible Presence](http://xmpp.org/extensions/xep-0018.html)||
|0022|[Message Events](http://xmpp.org/extensions/xep-0022.html)|[`mod_offline`](../modules/mod_offline.md)|
|0023|[Message Expiration](http://xmpp.org/extensions/xep-0023.html)|[`mod_offline`](../modules/mod_offline.md)|
|0030|[Service Discovery](http://xmpp.org/extensions/xep-0030.html)|[`mod_disco`](../modules/mod_disco.md)|
|0045|[Multi-User Chat](http://xmpp.org/extensions/xep-0045.html)|[`mod_muc`](../modules/mod_muc.md)|
|0049|[Private XML Storage](http://xmpp.org/extensions/xep-0049.html)|[`mod_private`](../modules/mod_private.md)|
|0050|[Ad-Hoc Commands](http://xmpp.org/extensions/xep-0050.html)|[`mod_adhoc`](../modules/mod_adhoc.md)|
|0054|[vcard-temp](http://xmpp.org/extensions/xep-0054.html)|[`mod_vcard`](../modules/mod_vcard.md)|
|0055|[Jabber Search](http://xmpp.org/extensions/xep-0055.html)|[`mod_vcard`](../modules/mod_vcard.md)|
|0059|[Result Set Management](http://xmpp.org/extensions/xep-0059.html)||
|0060|[Publish-Subscribe](http://xmpp.org/extensions/xep-0060.html)|[`mod_pubsub`](../modules/mod_pubsub.md)|
|0068|[Field Standardization for Data Forms](http://xmpp.org/extensions/xep-0068.html)||
|0073|[Basic IM Protocol Suite](http://xmpp.org/extensions/xep-0073.html)||
|0077|[In-Band Registration](http://xmpp.org/extensions/xep-0077.html)|[`mod_register`](../modules/mod_register.md)|
|0079|[Advanced Message Processing](http://xmpp.org/extensions/xep-0079.html)|[`mod_amp`](../modules/mod_amp.md) (partial support)|
|0082|[XMPP Date and Time Profiles](http://xmpp.org/extensions/xep-0082.html)||
|0085|[Chat State Notifications](http://xmpp.org/extensions/xep-0085.html)||
|0086|[Error Condition Mappings](http://xmpp.org/extensions/xep-0086.html)||
|0106|[JID Escaping](http://xmpp.org/extensions/xep-0106.html)||
|0114|[Jabber Component Protocol](http://xmpp.org/extensions/xep-0114.html)|`ejabberd_service`|
|0115|[Entity Capabilities](http://xmpp.org/extensions/xep-0115.html)|[`mod_caps`](../modules/mod_caps.md)|
|0124|[Bidirectional-streams Over Synchronous HTTP (BOSH)](http://xmpp.org/extensions/xep-0124.html)|[`mod_bosh`](../modules/mod_bosh.md)|
|0126|[Invisibility](http://xmpp.org/extensions/xep-0126.html)|[`mod_privacy`](../modules/mod_privacy.md)|
|0138|[Stream Compression](http://xmpp.org/extensions/xep-0138.html)||
|0153|[vCard-Based Avatars](http://xmpp.org/extensions/xep-0153.html)|[`mod_vcard`](../modules/mod_vcard.md)|
|0157|[Contact Addresses for XMPP Services](http://xmpp.org/extensions/xep-0157.html)|[`mod_disco`](../modules/mod_disco.md)|
|0160|[Best Practices for Handling Offline Messages](http://xmpp.org/extensions/xep-0160.html)|[`mod_offline`](../modules/mod_offline.md)|
|0163|[Personal Eventing Protocol](http://xmpp.org/extensions/xep-0163.html)|[`mod_pubsub`](../modules/mod_pubsub.md)|
|0170|[Recommended Order of Stream Feature Negotiation](http://xmpp.org/extensions/xep-0170.html)||
|0175|[Best Practices for Use of SASL ANONYMOUS](http://xmpp.org/extensions/xep-0175.html)||
|0185|[Dialback Key Generation and Validation](http://www.xmpp.org/extensions/xep-0185.html)||
|0191|[Blocking Command](http://xmpp.org/extensions/xep-0191.html)|[`mod_blocking`](../modules/mod_blocking.md)|
|0198|[Stream Management](http://xmpp.org/extensions/xep-0198.html)|[`mod_stream_management`](../modules/mod_stream_management.md)|
|0199|[XMPP Ping](http://xmpp.org/extensions/xep-0199.html)|[`mod_ping`](../modules/mod_ping.md)|
|0202|[Entity Time](http://www.xmpp.org/extensions/xep-0202.html)||
|0203|[Delayed Delivery](http://xmpp.org/extensions/xep-0203.html)||
|0206|[XMPP Over BOSH](http://xmpp.org/extensions/xep-0206.html)|[`mod_bosh`](../modules/mod_bosh.md)|
|0215|[External Service Discovery](http://xmpp.org/extensions/xep-0215.html)|[`mod_extdisco`](../modules/mod_extdisco.md)
|0237|[Roster Versioning](http://xmpp.org/extensions/xep-0237.html)|[`mod_roster`](../modules/mod_roster.md)
|0270|[XMPP Advanced Server 2010](http://xmpp.org/extensions/xep-0270.html)||
|0279|[Server IP Check](http://xmpp.org/extensions/xep-0279.html)|[`mod_sic`](../modules/mod_sic.md)|
|0280|[Message Carbons](http://xmpp.org/extensions/xep-0280.html)|[`mod_carboncopy`](../modules/mod_carboncopy.md)|
|0313|[Message Archive Management](http://xmpp.org/extensions/xep-0313.html)|[`mod_mam`](../modules/mod_mam.md)|
|0352|[Client State Indication](http://www.xmpp.org/extensions/xep-0352.html)|[`mod_csi`](../modules/mod_csi.md)|
|0357|[Push Notifications](http://www.xmpp.org/extensions/xep-0357.html)|[`mod_event_pusher_push`](../modules/mod_event_pusher_push.md)|
|0363|[HTTP File Upload](https://xmpp.org/extensions/xep-0363.html)|[`mod_http_upload`](../modules/mod_http_upload.md)|
|0384|[OMEMO Encryption](https://xmpp.org/extensions/xep-0384.html) (MongooseIM supports PEP, which is required by this extension)||
|0387|[XMPP Compliance Suites 2018 - all suites, Advanced Server level](https://xmpp.org/extensions/xep-0387.html)|
|0424|[Message Retraction](https://xmpp.org/extensions/xep-0424.html)|[`mod_mam`](../modules/mod_mam.md)|

## Supported Open Extensions

|Name|Module|
| ------------- | ------------- |
|[MUC Light](../open-extensions/muc_light.md)|[`mod_muc_light`](../modules/mod_muc_light.md)|
|[Inbox](../open-extensions/inbox.md)|[`mod_inbox`](../modules/mod_inbox.md)|
|[Token-based reconnection](../open-extensions/token-reconnection.md)|[`mod_auth_token`](../modules/mod_auth_token.md), [`mod_keystore`](../modules/mod_keystore.md)|
|[MAM extensions](../open-extensions/mam.md)|[`mam`](../modules/mod_mam.md)|
