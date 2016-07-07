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


Main differences from the parent project
----------------------------------------
This project began its life as a fork of
[ejabberd v.2.1.8](https://github.com/processone/ejabberd) back in 2011, and later underwent some major cleanup, refactorization and optimization.

Major steps performed at that time:
*   bringing the project source tree to compliance with OTP project structure
    recommendations,
*   swapping `autotools` for the Erlang community-standard build tool `rebar`,
*   removal of obsolete and/or rarely used modules to reduce maintenance
    burden,
*   reduction of runtime memory consumption by refactoring the code
    to use Erlang's binary data type for string manipulation and storage
    instead of operating on linked lists of characters,
*   functional test coverage of the system according to corresponding
    RFCs and XEPs.

Key differences today:
*   massive scalability
*   code quality, through extensive refactoring, subtantial optimisations, and continuous integration
*   unique version, fully open source, fully open standards, innovations contributed to the XSF
*   professional support, and flexible customer service

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


Features and supported standards
--------------------------------

*   XMPP Core: [RFC 3920](https://tools.ietf.org/html/rfc3920),
    [RFC 6120](https://tools.ietf.org/html/rfc6120)
*   Client connections over TCP (with TLS/STARTTLS available), Websockets,
    and HTTP(S) (BOSH).
*   Configurable database backends: MySQL, Postgres, generic ODBC. Mnesia
    and Redis for transient data.
*   Supports XEPs:

|||||
|-------------|-------------|-------------|-------------|
|[XEP-0012: Last Activity](http://www.xmpp.org/extensions/xep-0012.html) | [XEP-0016: Privacy Lists](http://www.xmpp.org/extensions/xep-0016.html) | [XEP-0018: Invisible Presence](http://www.xmpp.org/extensions/xep-0018.html) | [XEP-0022: Message Events](http://www.xmpp.org/extensions/xep-0022.html) |
[XEP-0023: Message Expiration](http://www.xmpp.org/extensions/xep-0023.html) | [XEP-0030: Service Discovery](http://www.xmpp.org/extensions/xep-0030.html) | [XEP-0045: Multi-User Chat](http://xmpp.org/extensions/attic/xep-0045-1.25.html) | [XEP-0049: Private XML Storage](http://www.xmpp.org/extensions/xep-0049.html) |
[XEP-0050: Ad-Hoc Commands](http://xmpp.org/extensions/attic/xep-0050-1.2.html) | [XEP-0054: vcard-temp](http://www.xmpp.org/extensions/xep-0054.html) | [XEP-0055: Jabber Search](http://www.xmpp.org/extensions/xep-0055.html) | [XEP-0059: Result Set Management](http://www.xmpp.org/extensions/xep-0059.html) |
[XEP-0060: Publish-Subscribe](http://xmpp.org/extensions/attic/xep-0060-1.13-1.html) | [XEP-0068: Field Standardization for Data Forms](http://www.xmpp.org/extensions/xep-0068.html) | [XEP-0077: In-Band Registration](http://www.xmpp.org/extensions/xep-0077.html) | [XEP-0078: Non-SASL Authentication](http://www.xmpp.org/extensions/xep-0078.html) |
[XEP-0079: Advanced Message Processing](http://www.xmpp.org/extensions/xep-0079.html) | [XEP-0082: XMPP Date and Time Profiles](http://www.xmpp.org/extensions/xep-0082.html) | [XEP-0083: Nested Roster Groups](http://www.xmpp.org/extensions/xep-0083.html) | [XEP-0085: Chat State Notifications](http://www.xmpp.org/extensions/xep-0085.html) |
[XEP-0086: Error Condition Mappings](http://www.xmpp.org/extensions/xep-0086.html) | [XEP-0092: Software Version](http://www.xmpp.org/extensions/xep-0092.html) | [XEP-0093: Roster Item Exchange](http://www.xmpp.org/extensions/xep-0093.html) | [XEP-0114: Jabber Component Protocol](http://www.xmpp.org/extensions/xep-0114.html) |
[XEP-0115: Entity Capabilities](http://www.xmpp.org/extensions/xep-0115.html) | [XEP-0124: Bidirectional-streams Over Synchronous HTTP (BOSH)](http://www.xmpp.org/extensions/xep-0124.html) | [XEP-0126: Invisibility](http://www.xmpp.org/extensions/xep-0126.html) | [XEP-0138: Stream Compression](http://www.xmpp.org/extensions/xep-0138.html) |
[XEP-0157: Contact Addresses for XMPP Services](http://www.xmpp.org/extensions/xep-0157.html) | [XEP-0160: Best Practices for Handling Offline Messages](http://www.xmpp.org/extensions/xep-0160.html) | [XEP-0163: Personal Eventing Protocol](http://www.xmpp.org/extensions/xep-0163.html) | [XEP-0175: Best Practices for Use of SASL ANONYMOUS](http://www.xmpp.org/extensions/xep-0175.html) |
[XEP-0185: Dialback Key Generation and Validation](http://www.xmpp.org/extensions/xep-0185.html) | [XEP-0191: Blocking Command](http://www.xmpp.org/extensions/xep-0191.html) | [XEP-0198: Stream Management](http://xmpp.org/extensions/attic/xep-0198-1.3.html) | [XEP-0199: XMPP Ping](http://www.xmpp.org/extensions/xep-0199.html) |
[XEP-0202: Entity Time](http://www.xmpp.org/extensions/xep-0202.html) | [XEP-0206: XMPP Over BOSH](http://www.xmpp.org/extensions/xep-0206.html) | [XEP-0212: XMPP Basic Server 2008](http://www.xmpp.org/extensions/xep-0212.html) | [XEP-0237: Roster Versioning](http://www.xmpp.org/extensions/xep-0237.html) |
[XEP-0248: PubSub Collection Nodes](http://www.xmpp.org/extensions/xep-0248.html) | [XEP-0279: Server IP Check](http://www.xmpp.org/extensions/xep-0279.html) | [XEP-0280: Message Carbons](http://xmpp.org/extensions/attic/xep-0280-0.10.html) | [XEP-0313: Message Archive Management](http://xmpp.org/extensions/attic/xep-0313-0.2.html) |
[XEP-0313: Message Archive Management](http://xmpp.org/extensions/attic/xep-0313-0.3.html) | [XEP-0313: Message Archive Management](http://xmpp.org/extensions/attic/xep-0313-0.4.1.html) | [XEP-0313: Message Archive Management](http://xmpp.org/extensions/attic/xep-0313-0.5.html) | [XEP-0352: Client State Indication](http://www.xmpp.org/extensions/xep-0352.html) |


How to build
------------
1.  Requirements.

    To compile MongooseIM you need:
    *   GNU Make,
    *   GCC,
    *   Libexpat 1.95 or higher,
    *   Erlang/OTP 17.5 or higher,
    *   Reltool 0.6.6 or higher,
    *   OpenSSL 0.9.8 or higher, for STARTTLS, SASL and SSL encryption,
    *   Zlib 1.2.3 or higher for Stream Compression support (XEP-0138). Optional.

2.  Compiling on UNIX-like systems.

    To compile MongooseIM, go to the main repo directory `$REPO` and execute
    the command (`$` stands for the shell prompt):

        $ make

    or

        $ ./rebar get-deps
        $ ./rebar compile

    To generate full MongooseIM release (with mysql, pgsql or other deps):

        $ make rel

    If more advanced release is required (with some specific db support only,
    f.e. mysql or pgsql) or you want to set `prefix` or `user` for the
    installation script please refer to the
    [release configuration](doc/user-guide/release_config.md)
    page in our documentation

    `make rel` commands will generate a self-contained OTP system image in the
    project's `rel/mongooseim` subdirectory. The contents of that directory are as
    follows:
    *   `rel/mongooseim/bin` - startup/administration scripts,
    *   `rel/mongooseim/etc` - configuration files,
    *   `rel/mongooseim/lib` - MongooseIM binary, header and runtime files,
    *   `rel/mongooseim/var` - spool directory,
    *   `rel/mongooseim/log` - log file directory,
    *   `rel/mongooseim/releases` - release files directory.

3.  Running MongooseIM.

    To run MongooseIM from the project tree after compiling it, change
    to `$REPO/rel/mongooseim`.

    There you can use the `mongooseim` command line administration script to
    start and stop MongooseIM. For example:

        $ bin/mongooseim start

    will start the server.

    You can also run the server in interactive mode:

        $ bin/mongooseim live

    There's also a tool called `mongooseimctl` allowing you to perform some
    operations on a running instance, e.g.:

        $ bin/mongooseimctl status
        MongooseIM node mongooseim@localhost:
          operating system pid: 86026
          Erlang VM status: started (of: starting | started | stopping)
          boot script status: started
          version: 1.6.2-61-g48b8332
          uptime: 1:12:46
          logs:
            log/ejabberd.log

4.  Building the testing target and running tests.

    For testing purposes there's a different make target available:

        $ make devrel

    which will generate releases in `$REPO/dev/` and prepare
    them for testing and generating coverage reports.

    To run the tests (from project's root directory, i.e. `$REPO`):

        $ dev/mongooseim_node1/bin/mongooseim start
        $ dev/mongooseim_node2/bin/mongooseim start
        $ make quicktest

    The test results will show up in the console`.


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
