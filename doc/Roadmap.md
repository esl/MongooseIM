## Vision

The MongooseIM Platform provides solutions for high-scalability and high-performance mobile messaging, social networking, and VoIP. 
The mix of products and services allow businesses and administrations to build apps with high acquisition, retention, and referral.

## Roadmap

This is a previsional roadmap, so please note that the closer we get to an item, the more likely it is that something is going to change. 
The process we have decided on gives us the ability to react to change and demand in an agile way, keeping the MongooseIM project truly opened to external inspiration and contribution.
This means that while we are pretty confident with the plan for the near future, nothing is set in stone. 

We are excited about the vision we've came up with, but want to improve it based on your ideas.
Please contact us and tell us about what features and improvements do you see as valuable in your project.



## MongooseIM 2.1.x: mobile focus (2017Q2)

* Flexible push notifications architecture:
    * [XEP-0357: Push Notifications](https://xmpp.org/extensions/xep-0357.html) **NEW**
    * Amazon AWS SNS (Simple Notification Service) **NEW**
    * mod_http_notification
* Full text search in archive (MAM)
* Experimental pipelining
* Instant Stream Resumption
* Bind 2.0 (experimental)
* Contributions to ecosystem

## MongooseIM 3.0.x: chatbots and IoT (2017S2)

* Conversational model: in-chat forms, for bots and humans
* IoT: things provisioning?
* SASL 2.0?
* Stories?
* [XEP-0376: Pubsub Account Management](https://xmpp.org/extensions/xep-0376.html)?
* Basic one-to-one VoIP (NAT traversal and media relaying with ICE server)
* Contributions? [XEP-xxxx: Explicit Message Encryption](https://xmpp.org/extensions/inbox/eme.html)?

## MongooseIM 3.1.x: chatbots and IoT (2018)

* [XEP-0156: Discovering Alternative XMPP Connection Methods](http://xmpp.org/extensions/xep-0156.html)?
* [XEP-0379: Pre-Authenticated Roster Subscription](https://xmpp.org/extensions/xep-0379.html)?
* Rich content
* Contributions?

## MongooseIM platform (2017)

This is not the MongooseIM XMPP/REST messaging server, all these components are part of the MongooseIM platform.

New components:
* Mangosta Android
* Mangosta iOS
* ICE/STUN/TURN server in Elixir
* Push notifications server in Elixir: MongoosePush
* Mangosta Web? Strophe.js? Stanza.io?

Existing components:
* [exml](https://github.com/esl/exml)
* [escalus](https://github.com/esl/escalus)
* [amoc](https://github.com/esl/amoc)
