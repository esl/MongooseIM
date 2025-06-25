# MongooseIM history

## 2025: Fast Authentication, TLS Improvements and Listeners Rework

Added support for XEP-0484 (Fast Authentication Streamlining Tokens).
Deprecated `fast_tls` and improved `just_tls`.

Major rework of S2S and component listeners, unified TLS handling, added TLS 1.3 with 0-RTT and channel bindings.

Releases:

* [MongooseIM 6.3.2](https://github.com/esl/MongooseIM/releases/tag/6.3.2) in February 2025.
* [MongooseIM 6.3.3](https://github.com/esl/MongooseIM/releases/tag/6.3.3) in April 2025.
* [MongooseIM 6.4.0](https://github.com/esl/MongooseIM/releases/tag/6.4.0) in June 2025.

## 2023-2024: C2S, CETS and Instrumentation

Created an alternative to Mnesia RAM-only tables - CETS.
It allows to run MongooseIM without Mnesia completely in RDBMS+CETS setup.

Moved the C2S implementation to state machine.
Added Docker image for arm64.

Enhanced CETS, configurable pools, and traffic shaping updates.

Improved instrumentation with enhanced configurability, Prometheus support and a more user-friendly experience.

Refined certificate validation and CockroachDB support.

Releases:

* [MongooseIM 6.3.1](https://github.com/esl/MongooseIM/releases/tag/6.3.1) in December 2024.
* [MongooseIM 6.3.0](https://github.com/esl/MongooseIM/releases/tag/6.3.0) in October 2024.
* [MongooseIM 6.2.1](https://github.com/esl/MongooseIM/releases/tag/6.2.1) in April 2024.
* [MongooseIM 6.2.0](https://github.com/esl/MongooseIM/releases/tag/6.2.0) in December 2023.
* [MongooseIM 6.1.0](https://github.com/esl/MongooseIM/releases/tag/6.1.0) in May 2023.

## 2022: GraphQL

New GraphQL API allows to access MongooseIM using HTTP protocol to extract data and make changes in a flexible way.
The command-line interface (CLI) has been reworked to match the GraphQL functionality.
The configuration for the admin and the client API has been simplified.

Release:

* [MongooseIM 6.0.0](https://github.com/esl/MongooseIM/releases/tag/6.0.0) in December 2022.

## 2020-2021: Friendly, cloud-native and dynamic

With the new configuration format, improved logging, and many more changes, MongooseIM has become more friendly for DevOps than ever before.
This goes hand in hand with the prioritisation of solutions that enable MongooseIM to be easily deployed to the cloud.

Whether in the cloud or on-premise, it is now possible to have a multi-tenant setup, powered by the new dynamic XMPP domains feature.
It means thousands of domains can be simply set up, managed, and removed dynamically, without a noticeable performance overhead.

Releases:

* [MongooseIM 5.1.0](https://github.com/esl/MongooseIM/releases/tag/5.1.0) in June 2022.
* [MongooseIM 5.0.0](https://github.com/esl/MongooseIM/releases/tag/5.0.0) in October 2021.
* [MongooseIM 4.2.0](https://github.com/esl/MongooseIM/releases/tag/4.2.0) in April 2021.
* [MongooseIM 4.1.0](https://github.com/esl/MongooseIM/releases/tag/4.1.0) in February 2021.
* [MongooseIM 4.0.0](https://github.com/esl/MongooseIM/releases/tag/4.0.0) in September 2020.
* [MongooseIM 3.7.0](https://github.com/esl/MongooseIM/releases/tag/3.7.0) in May 2020.
* [MongooseIM 3.6.0](https://github.com/esl/MongooseIM/releases/tag/3.6.0) in January 2020.

## 2018-2019: Global distribution ready

* Focus on global scale architecture.
* Chat bot integrations.
* Optimizations for IoT clients.
* GDPR compliance.
* New XML parser [exml](https://github.com/esl/exml).

Releases:

* [MongooseIM 3.5.0](https://github.com/esl/MongooseIM/releases/tag/3.5.0) in October 2019.
* [MongooseIM 3.4.0](https://github.com/esl/MongooseIM/releases/tag/3.4.0) in June 2019.
* [MongooseIM 3.3.0](https://github.com/esl/MongooseIM/releases/tag/3.3.0) in March 2019.
* [MongooseIM 3.2.0](https://github.com/esl/MongooseIM/releases/tag/3.2.0) in November 2018.
* [MongooseIM 3.1.1](https://github.com/esl/MongooseIM/releases/tag/3.1.1) in July 2018.
* [MongooseIM 3.0.1](https://github.com/esl/MongooseIM/releases/tag/3.0.1) in May 2018.
* [MongooseIM 2.2.2](https://github.com/esl/MongooseIM/releases/tag/2.2.2) in April 2018.
* [MongooseIM 2.1.1](https://github.com/esl/MongooseIM/releases/tag/2.1.1) in January 2018.

## 2017: Platform expansion and strengthening

[MongooseIM 2.1.0](https://github.com/esl/MongooseIM/releases/tag/2.1.0) in October 2017.

New components were added to the MongooseIM platform:

* [MongoosePush](https://github.com/esl/mongoosepush), push notifications server
* [MongooseICE](https://github.com/esl/MongooseICE), ICE server to help with voice calls functionality
* [Mangosta iOS](https://github.com/esl/mangosta-ios), demo XMPP client application for iOS
* [Mangosta Android](https://github.com/esl/mangosta-android), demo XMPP client application for Android

## 2016: Pivot to fullstack messaging platform

MongooseIM Platform was created, that included a list of components:

* [MongooseIM XMPP server 2.0.0](https://github.com/esl/MongooseIM/releases/tag/2.0.0), featuring a unique REST API for client developers and MUC light
* [WombatOAM](https://www.erlang-solutions.com/capabilities/wombatoam/), for monitoring and operations
* [escalus](https://github.com/esl/escalus), an Erlang XMPP client for test automation
* [amoc](https://github.com/esl/amoc), for load generation
* [Smack](https://github.com/igniterealtime/Smack) for Android in Java (third party)
* [XMPPFramework](https://github.com/robbiehanson/XMPPFramework) for iOS in Objective-C (third party)
* [Retrofit](https://square.github.io/retrofit/) by Square for Android in Java (third party)
* [Jayme](https://github.com/inaka/Jayme) by Inaka for iOS in Swift

## 2012-2015: Fully independent project growing fast

* Full OTP and `rebar` compliance.
* Removal of obsolete and/or rarely used modules.
* Reduction of the runtime memory consumption and functional test coverage.
* Added Message Archive Management support (XEP-0313).

Releases:

* [MongooseIM 1.6.x](https://github.com/esl/MongooseIM/releases/tag/1.6.0) in October 2015.
* [MongooseIM 1.5.x](https://github.com/esl/MongooseIM/releases/tag/1.5.0) in December 2014.
* [MongooseIM 1.4.x](https://github.com/esl/MongooseIM/releases/tag/1.4.0) in May 2014.
* [MongooseIM 1.3.x](https://github.com/esl/MongooseIM/releases/tag/1.3.0) in January 2014.
* [MongooseIM 1.2.x](https://github.com/esl/MongooseIM/releases/tag/1.2.0) in May 2013.
* [MongooseIM 1.1.x](https://github.com/esl/MongooseIM/releases/tag/1.1.0) in December 2012.
* [MongooseIM 1.0.0](https://github.com/esl/MongooseIM/releases/tag/1.0.0) in July 2012.

## 2011: Fork of ejabberd

This project began its life as a fork of ejabberd v.2.1.8.

Version 0.1.0 included:

* Replaced strings with binaries to significantly reduce memory consumption.
* Refactored directory structure of the project to be OTP complient.
* Replaced `autotools` with the `rebar` build tool.
* Removed obsolete and/or rarely used modules to reduce maintenance burden.
* Added functional tests based on RFCs and XEPs.
