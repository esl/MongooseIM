Our contributions to the ecosystem.

## Third-party opensource projects

### XMPPFramework for iOS

Available on: [robbiehanson/XMPPFramework](https://github.com/robbiehanson/XMPPFramework)

* [XEP-0363: HTTP File Upload](https://github.com/robbiehanson/XMPPFramework/pull/730)
* [XEP-0313: Message Archive Management](https://github.com/robbiehanson/XMPPFramework/pull/733)
* [XEP-0030: Service Discovery](https://github.com/robbiehanson/XMPPFramework/pull/736)
* [MUC light](https://github.com/robbiehanson/XMPPFramework/pull/750)
* [Token-based reconnection](https://github.com/robbiehanson/XMPPFramework/pull/758)
* Revamped README: making people feel like this is a well maintained and up to date framework
* Created a way to Mock a piece of the framework to improve the way we write tests

### Smack for Android

Available on: [igniterealtime/Smack](https://github.com/igniterealtime/Smack)

* [XEP-0357: Push Notifications](https://github.com/igniterealtime/Smack/pull/83)
* [XEP-0191: Blocking Command](https://github.com/igniterealtime/Smack/pull/84)
* [XEP-0313: Message Archive Management](https://github.com/igniterealtime/Smack/pull/76)
* [XEP-0308: Last Message Correction](https://github.com/igniterealtime/Smack/pull/73)
* [MUC light](https://github.com/igniterealtime/Smack/pull/81)
* [Token-based reconnection](https://github.com/igniterealtime/Smack/pull/85)
* [Instant Stream Resumption](https://github.com/igniterealtime/Smack/pull/92)
* [XEP-0231: Bits of Binary](https://github.com/igniterealtime/Smack/pull/91)
* [XEP-0333: Chat Markers](https://github.com/igniterealtime/Smack/pull/90)
* [MAM documentation](https://github.com/igniterealtime/Smack/pull/86/files)

### Movim

See [movim/movim](https://github.com/movim/movim) on GitHub for more details.

* [Docker image](https://github.com/esl/movim-docker) for Movim

## Software by Erlang Solutions

### escalus

See [esl/escalus](https://github.com/esl/escalus) on GitHub for more details.

An XMPP client library in Erlang for conveniently testing XMPP servers

[Apache license 2.0](https://github.com/esl/escalus/blob/master/LICENSE)

### amoc

See [esl/amoc](https://github.com/esl/amoc) on GitHub for more details.

amoc is a simple tool for running massively parallel XMPP tests

[Apache license 2.0](https://github.com/esl/amoc/blob/master/LICENSE)

!!! Info
    amoc stands for "A Murder of Crows"

### amoc-arsenal-xmpp

See [esl/amoc-arsenal-xmpp](https://github.com/esl/amoc-arsenal-xmpp) on GitHub for more details.

A collection of scenarios for [amoc](#amoc), which we use to test MongooseIM.
They can however be used to load test any XMPP server.

[Apache license 2.0](https://github.com/esl/amoc-arsenal-xmpp/blob/master/LICENSE)

### exml

See [esl/exml](https://github.com/esl/exml) on GitHub for more details.

XML parsing library in Erlang

[Apache license 2.0](https://github.com/esl/exml/blob/master/LICENSE)

### MongooseICE: ICE (STUN/TURN) server

See [MongooseICE](https://github.com/esl/MongooseICE) on GitHub for more details.

### MongoosePush: Push notifications server (APNS/FCM)

See [MongoosePush](https://github.com/esl/MongoosePush) on GitHub for more details.

## Open standards

### MUC light

MUC stands for Multi-User Chat. [MUC light](open-extensions/muc_light.md) is a presenceless and subscription-based group chat, relying on a simplified version of MUC.

### Token-based reconnection

[Token-based reconnection](open-extensions/token-reconnection.md) (TBR) Reconnection mechanism, for temporary disconnections, using tokens instead of passwords
