# MongooseIM 2.0.0

2016-11-08

This release includes:
- improved REST API for [backend services](http://mongooseim.readthedocs.io/en/2.0.0/swagger/index.html)
  [#985](https://github.com/esl/MongooseIM/pull/985), [#1044](https://github.com/esl/MongooseIM/pull/1044)
- extended REST API for [clients](http://mongooseim.readthedocs.io/en/2.0.0/swagger/index.html?client=true)
  [#881](https://github.com/esl/MongooseIM/pull/881), [#973](https://github.com/esl/MongooseIM/pull/973)
  [#982](https://github.com/esl/MongooseIM/pull/982), [#1003](https://github.com/esl/MongooseIM/pull/1003)
  [#1021](https://github.com/esl/MongooseIM/pull/1021)
- MUC-light improvements:
    - customisable configuration [#907](https://github.com/esl/MongooseIM/pull/907)
    - fix room destruction [#960](https://github.com/esl/MongooseIM/pull/960)
- removed support for legacy WebSockets [#1019](https://github.com/esl/MongooseIM/pull/1019).
  This requires ejabberd.cfg file as it is no longer possible to start `mod_websockets` as module (in modules sesction).
- parallelized tests: [#987](https://github.com/esl/MongooseIM/pull/987), [#1038](https://github.com/esl/MongooseIM/pull/1038)
- improved integration with dialyzer [#1025](https://github.com/esl/MongooseIM/pull/1025)
- other improvements:
    - conigurable global metrics [#940](https://github.com/esl/MongooseIM/pull/940)
    - several BOSH fixes: [#869](https://github.com/esl/MongooseIM/pull/869)
    - [complete list of merged PRs](https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222016-08-30..2016-11-08%22%20sort%3Acreated-asc%20)
    - [complete list of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aclosed%20closed%3A%222016-08-30..2016-11-08%22%20)

Special thanks to our contributors: @kenstir, @marktran, @svarlamov, @igors, @bernardd

This release repo [history](https://github.com/esl/MongooseIM/graphs/contributors?from=2016-08-29&to=2016-11-08&type=c)

# MongooseIM 2.0.0beta2

2016-08-29

This release includes:

- REST API for [backend services](http://mongooseim.readthedocs.io/en/2.0.0beta2/swagger/index.html)
- REST API for [clients](http://mongooseim.readthedocs.io/en/2.0.0beta2/swagger/index.html?client=true)
- refactored and extended authentication API [#828](https://github.com/esl/MongooseIM/pull/828)
- improved support for [XEP-0079: Advanced Message Processing](http://xmpp.org/extensions/xep-0079.html) [#833](https://github.com/esl/MongooseIM/pull/833)
- other improvements:
   - ditched support for Erlang R16 [#871](https://github.com/esl/MongooseIM/pull/871)
   - improved support for SSL options in cowboy listeners [#889](https://github.com/esl/MongooseIM/pull/889), [#893](https://github.com/esl/MongooseIM/pull/893)
   - HTTP authentication in MUC [#894](https://github.com/esl/MongooseIM/pull/894)
   - improved support for blocking commands [#900](https://github.com/esl/MongooseIM/pull/900), [#920](https://github.com/esl/MongooseIM/pull/920)
   - [complete list of merged PRs](https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222016-06-29..2016-08-28%22%20sort%3Acreated-asc%20)
   - [complete list of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aclosed%20closed%3A%222016-06-29..2016-08-28%22%20)

Special thanks to our contributors: @bernardd, @igors, @arkdro

This release repo [history](https://github.com/esl/MongooseIM/graphs/contributors?from=2016-06-29&to=2016-08-29&type=c)

# MongooseIM 2.0.0beta1

2016-06-28

This release includes:

- new XMPP extensions:
   - [XEP-0060: Publish Subscribe](http://www.xmpp.org/extensions/xep-0060.html) [#732](https://github.com/esl/MongooseIM/pull/732)
   - [XEP-0092: Software Version](http://www.xmpp.org/extensions/xep-0092.html) [#731](https://github.com/esl/MongooseIM/pull/731)
   - [XEP-0191: Blocking Command](https://xmpp.org/extensions/xep-0191.html), [#829](https://github.com/esl/MongooseIM/pull/829)
   - [XEP-0352: Client State Indication](https://xmpp.org/extensions/xep-0352.html) [#703](https://github.com/esl/MongooseIM/pull/703)
   - MUC light: [#577](https://github.com/esl/MongooseIM/pull/577)
   - token based authentication [#633](https://github.com/esl/MongooseIM/pull/633)
- HTTP notifications [#684](https://github.com/esl/MongooseIM/pull/684)
- support for FIPS mode [#628](https://github.com/esl/MongooseIM/pull/628)
- reverse Proxy [#628](https://github.com/esl/MongooseIM/pull/638)
- improved routing layer [#639](https://github.com/esl/MongooseIM/pull/639), [#729](https://github.com/esl/MongooseIM/pull/729), [#746](https://github.com/esl/MongooseIM/pull/746)
- external components improvements:
   - [#640](https://github.com/esl/MongooseIM/pull/640)
- parallelized tests:
   - [#706](https://github.com/esl/MongooseIM/pull/706)
   - [#740](https://github.com/esl/MongooseIM/pull/740)
   - [#741](https://github.com/esl/MongooseIM/pull/741)
   - [#751](https://github.com/esl/MongooseIM/pull/751)
- other improvements:
   - stream compression possible only after auth [#616](https://github.com/esl/MongooseIM/pull/616)
   - fix for TCP backlog setting [#708](https://github.com/esl/MongooseIM/pull/708)
   - carbon copies data optimisation [#700](https://github.com/esl/MongooseIM/pull/700)
   - update deps [#705](https://github.com/esl/MongooseIM/pull/705) and [#707](https://github.com/esl/MongooseIM/pull/707)
   - dialyzer checks on travis [#677](https://github.com/esl/MongooseIM/pull/677)
   - docker image building on travis [#710](https://github.com/esl/MongooseIM/pull/710)
   - improved build and installation process [#704](https://github.com/esl/MongooseIM/pull/704)
   - improved clustering [#683](https://github.com/esl/MongooseIM/683), [#717](https://github.com/esl/MongooseIM/pull/717), [#825](https://github.com/esl/MongooseIM/pull/825)
   - improved documentation
   - [complete list of merged PRs](https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222016-02-13..2016-06-28%22%20sort%3Acreated-asc%20)
   - [complete list of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aclosed%20closed%3A%222016-02-13..2016-06-28%22%20)

Special thanks to our contributors: @zsuidakra, @arkdro, @bernardd, @kshamko

This release repo [history](https://github.com/esl/MongooseIM/graphs/contributors?from=2016-02-13&to=2016-06-28&type=c)

# MongooseIM 1.6.2

2016-02-12

This release includes:

- finished Riak backends for all modules storing persistent data:
   - mod_offline [#594](https://github.com/esl/MongooseIM/pull/594)
   - mod_privacy [#618](https://github.com/esl/MongooseIM/pull/618)
   - MAM ([XEP-0313](https://xmpp.org/extensions/xep-0313.html)) for MUCs ([XEP-0045](https://xmpp.org/extensions/xep-0045.html)) [#622](https://github.com/esl/MongooseIM/pull/622)
- XMPP improvements:
   - support for MAM ([XEP-0313](https://xmpp.org/extensions/xep-0313.html)) versions 0.3, 0.4.1 and 0.5 [#668](https://github.com/esl/MongooseIM/pull/668)
   - support for Dialback Key Generation and Validation ([XEP-0185](https://xmpp.org/extensions/xep-0185.html)) [#665](https://github.com/esl/MongooseIM/pull/665)
- test improvements: [#602](https://github.com/esl/MongooseIM/pull/602), [#621](https://github.com/esl/MongooseIM/pull/621),[#637](https://github.com/esl/MongooseIM/pull/637), [#654](https://github.com/esl/MongooseIM/pull/654)
- dropped support for Erlang/OTP older than R16B03-1: [#613](https://github.com/esl/MongooseIM/pull/613), [#636](https://github.com/esl/MongooseIM/pull/636)
- updated cowboy to 1.0.4
- other improvements and code cleanup
   - [complete list of merged PRs](https://github.com/esl/MongooseIM/pulls?utf8=✓&q=is%3Apr+base%3Amaster+merged%3A%222015-12-05..2016-02-12%22+sort%3Acreated-asc+)
   - [complete list of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aclosed+closed%3A%222015-12-05..2016-02-12%22+)

Special thanks to our contributors: @gbour, @bartekgorny, @jfjalburquerque

This release repo [history](https://github.com/esl/MongooseIM/graphs/contributors?from=2015-12-06&to=2016-02-12&type=c)


# MongooseIM 1.6.1

2015-12-07

This release includes:

- more Riak backends:
   - mod_roster [#572](https://github.com/esl/MongooseIM/pull/572)
   - mod_last [#567](https://github.com/esl/MongooseIM/pull/567)
- tests improvements: [#557](https://github.com/esl/MongooseIM/pull/557), [#558](https://github.com/esl/MongooseIM/pull/558), [#575](https://github.com/esl/MongooseIM/pull/575), [#583](https://github.com/esl/MongooseIM/pull/583), [#584](https://github.com/esl/MongooseIM/pull/584), [#559](https://github.com/esl/MongooseIM/pull/559), [#590](https://github.com/esl/MongooseIM/pull/590)
- various fixes: [#570](https://github.com/esl/MongooseIM/pull/570), [#573](https://github.com/esl/MongooseIM/pull/573), [#561](https://github.com/esl/MongooseIM/pull/561)
- new API for jid manipulation [#579](https://github.com/esl/MongooseIM/pull/579)
   - this deprecates many jid-related function from `jlib` module
- further improvements in cleaning after dead node [#581](https://github.com/esl/MongooseIM/pull/581)
- other improvements and code cleanup
   - [complete list of merged PRs](https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr+base%3Amaster+merged%3A%222015-10-16..2015-12-06%22+sort%3Acreated-asc+)
   - [complete list of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aclosed+closed%3A%222015-10-16..2015-12-06%22+)

Special thanks to our contributors: @ppikula, @dharamgollapudi

This release repo [history](https://github.com/esl/MongooseIM/graphs/contributors?from=2015-10-16&to=2015-12-06&type=c)

# MongooseIM 1.6.0

2015-10-15

This release includes:

- Riak backends for the following modules:
  - authentication [#378](https://github.com/esl/MongooseIM/pull/378)
  - VCard [#460](https://github.com/esl/MongooseIM/pull/460)
  - Private XML Storage [#445](https://github.com/esl/MongooseIM/pull/445)
  - MAM (one-to-one only) [#452](https://github.com/esl/MongooseIM/pull/452)
- Improved and extended metrics
  - metrics based on hooks [#354](https://github.com/esl/MongooseIM/pull/354)
  - exometer instead of folsom [#365](https://github.com/esl/MongooseIM/pull/365)
  - automatic metrics for specified function in backend modules [#412](https://github.com/esl/MongooseIM/pull/412)
  - optionally start exometer graphite reporter via app.config file [#481](https://github.com/esl/MongooseIM/pull/481)
  - authentication related metrics [#488](https://github.com/esl/MongooseIM/pull/488)
  - MongooseIM node uptime metric [#525](https://github.com/esl/MongooseIM/pull/525)
- Merging of ejabberd_tests repo into MongooseIM [#482](https://github.com/esl/MongooseIM/pull/482),[#509](https://github.com/esl/MongooseIM/pull/509)
- Improved Redis backend for sessions [#422](https://github.com/esl/MongooseIM/pull/422)
- Tons of refactoring:
  - authentication mechanism [#426](https://github.com/esl/MongooseIM/pull/426)
  - ejabberd_c2s [#364](https://github.com/esl/MongooseIM/pull/364),[#463](https://github.com/esl/MongooseIM/pull/463),[#465](https://github.com/esl/MongooseIM/pull/465),[#495](https://github.com/esl/MongooseIM/pull/495)
  - simplified MAM [#438](https://github.com/esl/MongooseIM/pull/438)
    - removed dead code
    - removed async_writer as it was special case of async_pool_writer
    - mod_mam_odbc_server_user and mod_mam_odbc_user have been merged into one module
- XMPP improvements:
  - removed obsolete XEP-0091 timestamp [#383](https://github.com/esl/MongooseIM/pull/383)
  - omit some features when STARTTLS is required [#498](https://github.com/esl/MongooseIM/pull/498)
  - add XEP-0202: Entity Time [#510](https://github.com/esl/MongooseIM/pull/510)
  - BOSH - accept a higher hold attribute than one [#511](https://github.com/esl/MongooseIM/pull/511)
  - remove obsolete namespace from mediated invitation stanza [#513](https://github.com/esl/MongooseIM/pull/513)
- other improvements:
  - mod_offline_stub module to prevent service-unavailable errors [#429](https://github.com/esl/MongooseIM/pull/429)
  - improved log handling [#448](https://github.com/esl/MongooseIM/pull/448)
  - compatibility with Erlang/OTP 18 [#497](https://github.com/esl/MongooseIM/pull/497)
  - improved ODBC connectivity layer [#542](https://github.com/esl/MongooseIM/pull/542)
  - session cleanup after node death [#490](https://github.com/esl/MongooseIM/pull/490)
  - unified xml parsing and memory footprint optimisations [#183](https://github.com/esl/MongooseIM/pull/183)
  - supported XEPs are now documented
  - Dialyzer fixes & additions [#508](https://github.com/esl/MongooseIM/pull/508)
  - experimental [Docker image](https://hub.docker.com/r/mongooseim/mongooseim-docker/)
  - [complete list of merged PRs](https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr+base%3Amaster+merged%3A%222015-02-04..2015-10-15%22+sort%3Acreated-asc+)
  - [complete list of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=closed%3A%222015-02-04..2015-10-15%22+is%3Aissue+sort%3Acreated-desc+)

Special thanks to our contributors: @rgrinberg, @vooolll, @syhpoon, @mweibel, @Stelminator, @larshesel, @ruanpienaar, @aszlig, @jonathanve, @gmodarelli

This release repo [history](https://github.com/esl/MongooseIM/graphs/contributors?from=2015-02-04&to=2015-10-15&type=c)

# MongooseIM 1.5.1

2015-04-02

This release includes:

- fix for BOSH inactivity timeout ([#341](https://github.com/esl/MongooseIM/pull/341))
- improved compatibility with Stanza.io - including also fix for stream management ([#347](https://github.com/esl/MongooseIM/pull/347))
- removed dead code ([#328](https://github.com/esl/MongooseIM/pull/328), [#335](https://github.com/esl/MongooseIM/pull/335), [#336](https://github.com/esl/MongooseIM/pull/336), [#339](https://github.com/esl/MongooseIM/pull/339))
- unified roster logic ([#359](https://github.com/esl/MongooseIM/pull/359))
- rearranged documentation ([#363](https://github.com/esl/MongooseIM/pull/363), [#368](https://github.com/esl/MongooseIM/pull/368))
- other small improvements ([#337](https://github.com/esl/MongooseIM/pull/337), [#338](https://github.com/esl/MongooseIM/pull/338), [#345](https://github.com/esl/MongooseIM/pull/345), [#349](https://github.com/esl/MongooseIM/pull/349), [#350](https://github.com/esl/MongooseIM/pull/350), [#353](https://github.com/esl/MongooseIM/pull/353), [#364](https://github.com/esl/MongooseIM/pull/364), [#366](https://github.com/esl/MongooseIM/pull/366))
- [complete list of merged PRs](https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr+is%3Amerged+merged%3A%222014-12-01+..+2015-02-14%22)
- [complete list of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aclosed+is%3Aissue+created%3A%222014-12-01+..+2015-02-14%22+)


# MongooseIM 1.5.0

2014-12-02

This release:

- adds support for [XEP-0079: Advanced Message Processing](http://xmpp.org/extensions/xep-0079.html) - [#252](https://github.com/esl/MongooseIM/pull/252), [#314](https://github.com/esl/MongooseIM/pull/314)
- adds support for [XEP-0280: Message Carbons](http://xmpp.org/extensions/xep-0280.html) - [#264](https://github.com/esl/MongooseIM/pull/264)
- adds pluggable authentication module via HTTP - [#274](https://github.com/esl/MongooseIM/pull/274) ([documentation on wiki](https://github.com/esl/MongooseIM/wiki/HTTP-authentication-module))
- enables run time configuration reload - [#296](https://github.com/esl/MongooseIM/pull/296)
- includes security improvement - [#300](https://github.com/esl/MongooseIM/pull/300), [#304](https://github.com/esl/MongooseIM/pull/304)
- fixes ODBC layer support - [#275](https://github.com/esl/MongooseIM/pull/275)
- adds commands for clustering - [#228](https://github.com/esl/MongooseIM/pull/228)
- adds Cassandra backend for [XEP-0313: Message Archive Management](http://xmpp.org/extensions/xep-0313.html) - [#151](https://github.com/esl/MongooseIM/pull/151)
- other improvements and fixes
  - complete list of [merged pull requests](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+merged%3A%3E2014-05-20+created%3A%3C2014-12-01)
  - complete list of [closed issues](https://github.com/esl/MongooseIM/issues?q=is%3Aclosed+is%3Aissue+closed%3A%3E2014-05-20++created%3A%3C2014-12-01)


# MongooseIM 1.4.0

2014-05-20

- adds support for Stream Management [XEP-0198](http://xmpp.org/extensions/xep-0198.html) - [#195](https://github.com/esl/MongooseIM/pull/195)
- adds support for new [XMPP over WebSocket](http://tools.ietf.org/html/draft-ietf-xmpp-websocket-06) - [#173](https://github.com/esl/MongooseIM/pull/173)
- adds support for SCRAM-SHA-1 authentication both for ODBC and mnesia user backends - [#171](https://github.com/esl/MongooseIM/pull/171)

> !! requires `users` table schema update for MySQL and PostgreSQL: [68b790c](https://github.com/esl/MongooseIM/commit/68b790c6b64bed7894602d003b94ef1037059e55) !!

- improves security [#196](https://github.com/esl/MongooseIM/pull/196):
  - support for DH and ECDH key exchange protocol
  - configurable list of allowed cipher suites
- uses [travis-ci.org](https://travis-ci.org/esl/MongooseIM) with our [test suite](https://github.com/esl/ejabberd_tests)
- fixes privacy lists removal after user deletion - [#186](https://github.com/esl/MongooseIM/pull/186)
- fixes roster versioning issue [#199](https://github.com/esl/MongooseIM/pull/199)
- fixes multi-invite bug in MUC - [#180](https://github.com/esl/MongooseIM/pull/180)
- other code improvements and small fixes


# MongooseIM 1.3.2

2014-04-15

- fix for [Uncontrolled Resource Consumption with XMPP-Layer Compression](http://xmpp.org/resources/security-notices/uncontrolled-resource-consumption-with-highly-compressed-xmpp-stanzas/)

zlib driver configuration (in c2s or s2s section) takes new parameter which is max size of decompressed data

- unified logic for modules (use option `backend` in module config section with possible values `mnesia` (default) or `odbc`):

  - mod_privacy
  - mod_private
  - mod_offline
- `max_connections` parameter for `ejabberd_cowboy` listener
- improved BOSH implementation
- improved vCard search mechanism
- other small fixes/improvements

# MongooseIM 1.3.1

2014-02-21

- port XEP-0114 (ejabberd_service)
- port LDAP vCard support
- port LDAP Shared Roster support
- finalize project rename to MongooseIM by changing scripts names

# MongooseIM 1.3.0

2014-01-28

- added [XEP-0313: Message Archive Management](http://xmpp.org/extensions/xep-0313.html) support (`mod_mam`)
- port LDAP authentication from ejabberd
- added PostgreSQL support
- disable SSL 2.0 support
- disable old unsafe ciphers
- make the repo includable as a rebar dependency
- use `#xmlel{}` instead of `{xmlel, ...}` in the whole codebase

# MongooseIM 1.2.2

2013-05-23

* use upstream lager
* added CHANGELOG file
* replace exml tuples with record
* don't use binaries in ejabberd.cfg
* add support for alarms #82


# MongooseIM 1.2.1

2013-05-15

* Folsom metrics improvements
* REST interface for accessing metrics
* BOSH fixes
* Don't allow session to hang forever after pause


# MongooseIM 1.2.0

2013-05-12

- BOSH support (XMPP over HTTP)
- WSS (WebSocekt Secure)
- various XMPP related metrics exposed via REST API
  (https://github.com/esl/ejabberd/wiki/REST-interface-to-folsom-metrics)
- alarm handler for better monitoring and bottleneck finding

We've also assured compatibility with the latest ejabberd Community Edition by
ProcessOne, so that backporting ejabberd modules to MongooseIM requires
less effort.


# MongooseIM 1.1.0

2012-12-04

- list to binary conversion
- Multi-User Chat (MUC) fixes and cleanups
- style and indentation fixes
- change logger to lager


# MongooseIM 1.0.0

2012-07-10

- remove tsung from source tree
- add ejabberdctl with admin and admin_extra commands
- add devrel target
- binarise remaining modules
- fix p1_fsm get_status handler


# MongooseIM 0.9.0

2012-05-21

* add makefile target that downloads rebar
* re-added mod_offline & mod_last_odbc & mod_sic
* continue transition to binaries instead of strings and remove no ported modules:
    * ejabberd_http
    * ejabberd_http_bind
    * ejabberd_http_poll
    * mod_http_bind
    * mod_adhoc
    * mod_announce
    * mod_blocking
    * mod_configure
    * mod_configure2
    * mod_disco
    * mod_echo
    * mod_http_fileserver
    * mod_last
    * mod_muc,
    * mod_muc_log
    * mod_offline_odbc
    * mod_ping
    * mod_pres_counter
    * mod_privacy_odbc
    * mod_private
    * mod_proxy65
    * mod_pubsub
    * mod_register_web
    * mod_roster_odbc
    * mod_service
    * mod_shared_roster
    * mod_shared_roster_ldap
    * mod_stats
    * mod_timea
    * mod_vcard
    * mod_vcard_odbc
    * mod_vcard_xupdate
    * mod_version


# MongooseIM 0.1.0-pre

2012-04-17

* rebarify
* add escalus tests
* binarise core modules
* dialyzer support
* change table copy type of acl, config, local_config to ram_copies
