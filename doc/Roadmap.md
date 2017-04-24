This is a previsional roadmap, almost every item is subject to change:
* The more it is close to the present, the more it is certain
* The more it is in the future, the more it is uncertain
* You have the power to influence it, so please feel free to comment!

## 2.1.x: mobile focus

* Flexible push notifications architecture:
    * [XEP-0357: Push Notifications](https://xmpp.org/extensions/xep-0357.html) **NEW**
    * Amazon AWS SNS (Simple Notification Service) **NEW**
    * mod_http_notification
* Full text search in archive (MAM)
* Experimental pipelining
* Instant Stream Resumption
* Bind 2.0 (experimental)
* Contributions to ecosystem

## 3.0.x: chatbots and IoT

* Conversational model: in-chat forms, for bots and humans
* IoT: things provisioning?
* SASL 2.0?
* Stories?
* [XEP-0376: Pubsub Account Management](https://xmpp.org/extensions/xep-0376.html)?
* Basic one-to-one VoIP (NAT traversal and media relaying with ICE server)
* Contributions? [XEP-xxxx: Explicit Message Encryption](https://xmpp.org/extensions/inbox/eme.html)?

## 3.1.x: chatbots and IoT

* [XEP-0156: Discovering Alternative XMPP Connection Methods](http://xmpp.org/extensions/xep-0156.html)?
* [XEP-0379: Pre-Authenticated Roster Subscription](https://xmpp.org/extensions/xep-0379.html)?
* Rich content
* Contributions?

## Not MongooseIM server, but part of the MongooseIM platform

New components:
* Mangosta Android
* Mangosta iOS
* ICE/STUN/TURN server in Elixir
* Push notifications server in Elixir: MongoosePush

Existing components:
* [exml](https://github.com/esl/exml)
* [escalus](https://github.com/esl/escalus)
* [amoc](https://github.com/esl/amoc)
