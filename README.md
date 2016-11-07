MongooseIM  [![Build Status](https://travis-ci.org/esl/MongooseIM.svg?branch=master)](https://travis-ci.org/esl/MongooseIM) [![Documentation Status](https://readthedocs.org/projects/mongooseim/badge/?version=latest)](http://mongooseim.readthedocs.org/en/latest/?badge=latest) [![Coverage Status](https://img.shields.io/coveralls/esl/MongooseIM.svg)](https://coveralls.io/r/esl/MongooseIM?branch=master) [![Buildtime trend](https://buildtimetrend.herokuapp.com/badge/esl/MongooseIM/latest)](https://buildtimetrend.herokuapp.com/dashboard/esl/MongooseIM/)
[![GitHub release](https://img.shields.io/github/release/esl/MongooseIM.svg)](https://github.com/esl/MongooseIM/releases)

============
<img align="left" src="doc/MongooseIM_logo.png"</img>

MongooseIM is Erlang Solutions' robust and efficient XMPP server aimed at large installations. Specifically designed for enterprise purposes, it is fault-tolerant, can utilize resources of multiple clustered machines and easily scale in need of more capacity (by just adding a box/VM).

MongooseIM can accept client sessions over vanilla XMPP, Websockets, and HTTP long-polling (a.k.a. BOSH).

Its home on GitHub is at http://github.com/esl/MongooseIM.

The product page is available at https://www.erlang-solutions.com/products/mongooseim.html

Download packages
-----------------

For a quick start just
[download the pre-built package](https://www.erlang-solutions.com/resources/download.html)
that suits your platform: Ubuntu, Debian, CentOS, and Mac OS X.

An _experimental_ Docker image exists on: https://registry.hub.docker.com/u/mongooseim/mongooseim-docker/
You can contribute on: https://github.com/ppikula/mongooseim-docker

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

Original documentation for Ejabberd-2.1.8, from which MongooseIM was forked, is preserved
in `doc/ejabberd-2.1.8-OLD`.

Test suite
----------

In order to test and validate your XMPP servers, here are useful tools:
* [escalus](https://github.com/esl/escalus): Erlang XMPP client
* [amoc](https://github.com/esl/amoc): a load testing tools

Public testing
--------------

Continuous integration:
https://travis-ci.org/esl/MongooseIM

Code coverage:
https://coveralls.io/github/esl/MongooseIM


Want to get in touch with us?
-----------------------------

In case of any suggestions, questions or any thoughts on this project,
please feel free to contact us by the standard GitHub ways or at
<a href='mailto:mongoose-im@erlang-solutions.com'>mongoose-im@erlang-solutions.com</a>.

Want to discuss MongooseIM, problems with your deployement or anything else?
Try: <a href='https://erlangcentral.org/forum/mongooseim/'>https://erlangcentral.org/forum/mongooseim/</a>.

Announcements mailing-list
--------------------------

We have set up a new public mailing-list for all announcements of major events happening on the MongooseIM front. Expect one or two emails per month, the archives are free and open. We highly encourage you to subscribe here: https://groups.google.com/d/forum/mongooseim-announce
Click on the blue button "Join group", then click in "Email delivery preference" on "Notify me for every new message".

Client libraries
----------------

We recommend following client libraries:
* iOS, Objective-C: [XMPPframework](https://github.com/robbiehanson/XMPPFramework)
* Android, Java: [Smack](https://github.com/igniterealtime/Smack)
* Web, JavaScript: [Stanza.io](https://github.com/otalk/stanza.io)

Social media
------------

Follow us on Twitter and Facebook, please ask questions, and propose features!

Twitter: https://twitter.com/MongooseIM

Facebook: https://www.facebook.com/MongooseIM/
