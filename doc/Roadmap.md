## Vision

The MongooseIM Platform provides solutions for high-scalability and high-performance messaging, social networking, and VoIP, for mobile/tablet and laptop/desktop computers. 
The products and services offered by the MongooseIM Platform allow businesses and administrations to build apps, with high and sustainable acquisition, retention, and referral, achieving massive network effect at planetary scale.

## Current state of the MongooseIM platform

The following components build up the MongooseIM platform, and not only the XMPP/REST server at its core.

### Backend/server components:

* [MongooseIM](https://github.com/esl/MongooseIM): the core server, real-time XMPP/REST messaging cluster
* [MongoosePush](https://github.com/esl/MongoosePush): open source Push notifications server in Elixir
* [MongooseICE](https://github.com/esl/Fennec): open source ICE/STUN/TURN server in Elixir
* [exml](https://github.com/esl/exml): XML library
* [escalus](https://github.com/esl/escalus): Erlang XMPP client
* [amoc](https://github.com/esl/amoc): A Murder Of Crows, load injector
* [Tide/CLT](http://tide.erlang-solutions.com/): Continuous Load Testing platform

### Frontend/client components:

* [Smack](https://github.com/igniterealtime/Smack): open source, third party library for Android
* [XMPPFramework](https://github.com/robbiehanson/XMPPFramework): open source, third party library for iOS
* [Mangosta Android](https://github.com/esl/mangosta-android): chat and social client, sample app
* [Mangosta iOS](https://github.com/esl/mangosta-ios): chat and social client, sample app

## Roadmap

This is a previsional roadmap, so please note that the closer we get to an item, the more likely it is that something is going to change. 
The process we have decided on gives us the ability to react to change and demand in a lean and agile way, keeping the MongooseIM project truly opened to external inspiration and contribution.
This means that while we are pretty confident with the plan for the near future, nothing is set in stone. 

We are excited about the vision we have come up with, but want to improve it based on your ideas.
Please contact us and tell us about what features and improvements do you see as valuable in your project.

### MongooseIM 2.x: Platform

#### MongooseIM 2.1.x: platform expansion, mobile focus (2017Q2-Q3)

* Flexible push notifications architecture:
    * [XEP-0357: Push Notifications](https://xmpp.org/extensions/xep-0357.html) **NEW**
    * Amazon AWS SNS (Simple Notification Service) **NEW**
    * mod_http_notification
* Full text search in archive (MAM)
* Experimental pipelining
* Instant Stream Resumption
* Bind 2.0 (experimental)
* Basic one-to-one VoIP (NAT traversal and media relaying with ICE/STUN/TURN server)
* Contributions to ecosystem

### MongooseIM 3.x: planetary scale

#### MongooseIM 3.0.x: real-time geoclustering (2018Q1)

* Cluster intercontinental geo-distribution for real-time content
* SASL 2.0
* [XEP-0376: Pubsub Account Management](https://xmpp.org/extensions/xep-0376.html)
* Contributions to ecosystem

#### MongooseIM 3.1.x: archive geoclustering (2018Q2)

* Intercontinental geo-distribution for message archive
* Inbox
* [XEP-0156: Discovering Alternative XMPP Connection Methods](http://xmpp.org/extensions/xep-0156.html)
* [XEP-0379: Pre-Authenticated Roster Subscription](https://xmpp.org/extensions/xep-0379.html)
* Contributions to the ecosystem

### MongooseIM 4.x: reconnecting bots and humans

#### MongooseIM 4.0.x: chatbots and IoT (2018S2)

* Conversational model:
    * In-chat forms (quick replies), for bots and humans: new ProtoXEP, sample implementations on server and clients
    * Ephemeral messages
    * Foldable content
* IoT:
    * Entity provisioning
    * OCF certification
    * Embedded client
    * Cloud/fog focus
* Contributions? [XEP-xxxx: Explicit Message Encryption](https://xmpp.org/extensions/inbox/eme.html)?

#### MongooseIM 4.1.x: social (2018S2)

* Social networking improvements
* Stories
* Rich content
* Contributions to the ecosystem

## Probabilities

* Mangosta Web
* Mangosta desktop/laptop (Windows, macOS, Linux)
* Contributions to Strophe.js and Stanza.io

## Contribute to our roadmap

Our roadmap is built based on community feedback, customer demand, market observation, and our own ideas.
We would love to invite everyone to contribute their vision, and suggest where we should go next.
No matter if you love, use or just know MongooseIM, please share your passion and inspire the next steps of our journey.
