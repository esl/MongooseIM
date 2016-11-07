# MongooseIM platform

[![Build Status](https://travis-ci.org/esl/MongooseIM.svg?branch=master)](https://travis-ci.org/esl/MongooseIM) [![Documentation Status](https://readthedocs.org/projects/mongooseim/badge/?version=latest)](http://mongooseim.readthedocs.org/en/latest/?badge=latest) [![Coverage Status](https://img.shields.io/coveralls/esl/MongooseIM.svg)](https://coveralls.io/r/esl/MongooseIM?branch=master) [![Buildtime trend](https://buildtimetrend.herokuapp.com/badge/esl/MongooseIM/latest)](https://buildtimetrend.herokuapp.com/dashboard/esl/MongooseIM/)
[![GitHub release](https://img.shields.io/github/release/esl/MongooseIM.svg)](https://github.com/esl/MongooseIM/releases) [![CII Best Practices](https://bestpractices.coreinfrastructure.org/projects/476/badge)](https://bestpractices.coreinfrastructure.org/projects/476) [![xmpp.net score](https://xmpp.net/badge.php?domain=erlang-solutions.com)](https://xmpp.net/result.php?domain=erlang-solutions.com&amp;type=client)

============
<img align="left" src="doc/MongooseIM_logo.png" alt="MongooseIM platform's logo" />

MongooseIM is Erlang Solutions' robust and efficient XMPP platform aimed at large installations. Specifically designed for enterprise purposes, it is fault-tolerant, can utilize resources of multiple clustered machines and easily scale in need of more capacity (by just adding a box/VM). MongooseIM can accept client sessions over vanilla XMPP, Websockets, HTTP long-polling (a.k.a. BOSH), and a REST API.

The MongooseIM platform comes with server-side components and client libraries. We provide a test suite and a monitoring server. We recommand third-party, open source client libraries for XMPP and REST API.

Its home on GitHub is at http://github.com/esl/MongooseIM.

The product page is available at https://www.erlang-solutions.com/products/mongooseim.html

<img src="doc/mongoose_top_banner_800.jpeg" alt="MongooseIM platform's mongooses faces" />

Documentation
-------------

Up-to-date documentation for the MongooseIM master branch can be found on ReadTheDocs:
* http://mongooseim.readthedocs.org/en/latest/
* Older versions:
  * [release 1.6.2](http://mongooseim.readthedocs.org/en/1.6.2/)
  * [release 1.6.1](http://mongooseim.readthedocs.org/en/1.6.1/)
  * [release 1.6.0](http://mongooseim.readthedocs.org/en/1.6.0/)
  * [release 1.5.1](http://mongooseim.readthedocs.org/en/1.5.1/)

When developing new features/modules, please take care to add basic documentation
to the `doc/` directory, and add a link to your document in `doc/README.md`.

Download MongooseIM server packages
-----------------------------------

For a quick start just download:
* The [pre-built packages](https://www.erlang-solutions.com/resources/download.html)
that suits your platform (Ubuntu, Debian, CentOS, and macOS)
* The [Docker image](https://hub.docker.com/r/mongooseim/mongooseim/): https://hub.docker.com/r/mongooseim/mongooseim/ (source code repository: https://github.com/esl/mongooseim-docker)

Public testing
--------------

Check out our test results:
* Continuous integration: https://travis-ci.org/esl/MongooseIM
* Code coverage: https://coveralls.io/github/esl/MongooseIM
* Stay tuned... more soon!

MongooseIM platform components
------------------------------

### Server-side components

We offer a set of server-side components:
* [WombatOAM]() is a powerful monitoring platform that comes with specific MongooseIM plugins
* Test suite: in order to test and validate your XMPP servers, here are useful tools:
    * [escalus](https://github.com/esl/escalus): Erlang XMPP client
    * [amoc](https://github.com/esl/amoc): a load testing tools
* More components? It is probable that we will offer:
    * Icicle: ICE, STUN/TRUN server
    * Platypus: a push notification server

### Client-side components

* XMPP client libraries: we recommend following client libraries:
    * iOS, Objective-C: [XMPPframework](https://github.com/robbiehanson/XMPPFramework)
    * Android, Java: [Smack](https://github.com/igniterealtime/Smack)
    * Web, JavaScript: [Stanza.io](https://github.com/otalk/stanza.io), [Strophe.js](https://github.com/strophe/strophejs)
* REST API client libraries: we recommend following client libraries:
    * iOS, Swift: [Jayme](https://github.com/inaka/Jayme)
    * Android, Java: [Retrofit](https://github.com/square/retrofit)

### Platform table

<img src="doc/MongooseIM_platform.jpeg" alt="Schema explaining MongooseIM platform" />

Participate!
------------

In case of any suggestions, questions or any thoughts, please feel free to contact us:
* Defacto standard [GitHub issues](https://github.com/esl/MongooseIM/issues): https://github.com/esl/MongooseIM/issues
* Email us at <a href='mailto:mongoose-im@erlang-solutions.com'>mongoose-im@erlang-solutions.com</a>
* Create a post on erlangcentral forums at <a href='https://erlangcentral.org/forum/mongooseim/'>https://erlangcentral.org/forum/mongooseim/</a>
* Follow our [Twitter account](https://twitter.com/MongooseIM): https://twitter.com/MongooseIM
* Like our [Facebook page](https://www.facebook.com/MongooseIM/): https://www.facebook.com/MongooseIM/
* Subscribe to our [mailing list](https://groups.google.com/d/forum/mongooseim-announce) at https://groups.google.com/d/forum/mongooseim-announce as it is only one or two emails per month, the archives are free and open (click on the blue button "Join group", then click in "Email delivery preference" on "Notify me for every new message")
