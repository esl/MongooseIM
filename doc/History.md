## MongooseIM history

### 2011: Fork of ejabberd

MongooseIM's birthplace is a private Erlang Solutions' branch of ProcessOne's ejabberd - an XMPP/Jabber server written in Erlang.
What would later become a leading, highly customisable and scalable XMPP platform, originated in a strong idea - storing all internal strings in binaries instead of lists, among other significant improvements.

The change was introduced in 0.1.0 proto-MongooseIM release and 3.0.0-alpha-X series of ejabberd.
This opened the door for achieving higher performance, lower latency and introducing other subsequent improvements building up to a plaform we are truly proud of.


### 2012-2015: Fully independent project growing fast

The next steps were achieving full OTP and `rebar` compliance, removal of obsolete and/or rarely used modules, reduction of the runtime memory consumption and functional test coverage. 
[MongooseIM 1.0.0](https://github.com/esl/MongooseIM/releases/tag/1.0.0) was released on July 10th of 2012.

MongooseIM XMPP server fully independently went through multiple versions, following its own path with its own resources: [1.1.x](https://github.com/esl/MongooseIM/releases/tag/1.1.0) in 2012, [1.2.x](https://github.com/esl/MongooseIM/releases/tag/1.2.0) in 2013, [1.3.x](https://github.com/esl/MongooseIM/releases/tag/1.3.0), [1.4.x](https://github.com/esl/MongooseIM/releases/tag/1.4.0),  [1.5.x](https://github.com/esl/MongooseIM/releases/tag/1.5.0) in 2014, and [1.6.x](https://github.com/esl/MongooseIM/releases/tag/1.6.0) in 2015.


### 2016: Pivot to fullstack messaging platform

MongooseIM Platform appeared in 2016, with the release of [MongooseIM XMPP server 2.0.0](https://github.com/esl/MongooseIM/releases/tag/2.0.0).

The MongooseIM platform components were:

* MongooseIM XMPP server, featuring a unique REST API for client developers and MUC light
* WombatOAM, for monitoring and operations
* escalus, an Erlang XMPP client for test automation
* amoc, for load generation
* Smack for Android in Java (third party)
* XMPPFramework for iOS in Objective-C (third party)
* Retrofit by Square for Android in Java (third party)
* Jayme by Inaka for iOS in Swift


### 2017: Platform expansion and strengthening

We also introduced some MongooseIM platform components that are independent of the XMPP server.
So far the list includes:

* Mangosta iOS
* Mangosta Android
* MongoosePush
* MongooseICE


### 2018-2019: Planetary architecture, to welcome IoT-scale and chabots

The next step on our journey with the MongooseIM platform is to enable building planetary scale architectures.
This is necessary to welcome the massive influx of users that come with a full stack IoT and chatbot solution.
The ability to connect robots and humans is the requirement of the next technological breakthrough.

Erlang Solution's goal is to utilise XMPP features suited for chatbots, and build open standards for completeness of solution.
