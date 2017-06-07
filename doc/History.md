## MongooseIM history

### Fork of ejabberd

In 2010/2011, Erlang Solutions and ProcessOne were both working in parallel, uncoordinated, on separate private branches of ejabberd, the XMPP/Jabber server written in Erlang: 3.0.0-alpha-X series of ejabberd, and 0.1.0 of what would eventually be named MongooseIM.

The main change to both codebases was roughly the same goal: all internal strings stored in binaries rather than lists of characters, for higher performance, and lower latency.


### Fully independent project

Erlang Solutions then worked on full OTP compliance, `rebar`, removal of obsolete and/or rarely used modules, reduction of runtime memory consumption, functional test coverage. [MongooseIM 1.0.0](https://github.com/esl/MongooseIM/releases/tag/1.0.0) was released on July 10th of 2012.

MongooseIM XMPP server fully independently went through multiple versions, following its own path with its own resources: [1.1.x](https://github.com/esl/MongooseIM/releases/tag/1.1.0) in 2012, [1.2.x](https://github.com/esl/MongooseIM/releases/tag/1.2.0) in 2013, [1.3.x](https://github.com/esl/MongooseIM/releases/tag/1.3.0), [1.4.x](https://github.com/esl/MongooseIM/releases/tag/1.4.0),  [1.5.x](https://github.com/esl/MongooseIM/releases/tag/1.5.0) in 2014, and [1.6.x](https://github.com/esl/MongooseIM/releases/tag/1.6.0) in 2015.


### Pivot to fullstack messaging platform

MongooseIM Platform appeared in 2016, with the release of [MongooseIM XMPP server 2.0.0](https://github.com/esl/MongooseIM/releases/tag/2.0.0).

The whole MongooseIM platform was then made of:
* MongooseIM XMPP server, featuring a unique REST API for client developers and MUC light
* WombatOAM, for monitoring and operations
* escalus, Erang XMPP client for test automation
* amoc, for load generation
* Smack for Android in Java (third party)
* XMPPFramework for iOS in Objective-C (third party)
* Retrofit by Square for Android in Java (third party)
* Jayme by Inaka for iOS in Swift


### Platform expansion

MongooseIM platform then expanded independently of MongooseIM XMPP server versions:
* Mangosta iOS
* Mangosta Android
* MongoosePush
* MongooseICE


### Soon: IoT and chabots

Leveraging the MongooseIM platform, Erlang Solutions will soon be in position to deliver a full stack IoT solution.

XMPP has the most expected features for chatbots, but Erlang Solutions will build open standards for completeness of solution.
