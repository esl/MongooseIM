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
    * [GraphQL API](../graphql-api/User-GraphQL.md)
    * [REST API](../rest-api/Client-frontend.md) (deprecated)
* Server/backend connections:
    * [GraphQL API](../graphql-api/Admin-GraphQL.md)
    * [REST API](../rest-api/Administration-backend.md) (deprecated)
* Configurable database backends:
    * Transient:
        * Mnesia
        * Redis
    * Persistent:
        * RDBMS: MySQL, PostgreSQL
        * NoSQL: Cassandra
* Integration with third-party services
    * [Amazon Simple Notification Service](../modules/mod_event_pusher_sns.md)

## Supported XEPs

{%
   include-markdown "./Supported-XEPs.md"
%}

## Supported Open Extensions

|Name|Module|
| ------------- | ------------- |
|[MUC Light](../open-extensions/muc_light.md)|[`mod_muc_light`](../modules/mod_muc_light.md)|
|[Inbox](../open-extensions/inbox.md)|[`mod_inbox`](../modules/mod_inbox.md)|
|[Token-based reconnection](../open-extensions/token-reconnection.md)|[`mod_auth_token`](../modules/mod_auth_token.md), [`mod_keystore`](../modules/mod_keystore.md)|
|[MAM extensions](../open-extensions/mam.md)|[`mam`](../modules/mod_mam.md)|
