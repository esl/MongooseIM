# [MongooseIM 2.1.1](https://github.com/esl/MongooseIM/releases/tag/2.1.1) - 2018-01-16

## Added

- Event Pusher - a module that unifies all outgoing event channels: HTTP, Push Notifications etc. (#1414)
- TLS-secured connections to databases (#1545, #1556, #1564, #1578, #1585, #1587)
- Dedicated API in `ejabberd_auth` for accessing parameters in `auth_opts` tuple (#1593)
- Experimental Global Distribution extension (#1604)
- Max allowed stanza size may be now configured for `mod_websockets` (#1641)

## Changed

- Project structure has been transformed to single application layout (#1580, #1590)
- Message Archive Management v0.2 support is now deprecated and will be removed in 3.0.0beta1 (#1514, #1591)
- MySQL schema now uses `utf8mb4` encoding and `ROW_FORMAT=DYNAMIC`. MySQL versions older than 5.5.14 are no longer supported. (#1611, #1633)
- MongooseIM now uses updated and decoupled fork of `exometer` (#1600)

## Fixed

- User process crash when IQ result/error with Privacy Lists/Blocking Command namespace was received. (#1597)
- MongooseIM build failed on macOS High Sierra due to old version of `fast_tls` (#1606)
- Error type returned when VCard is not found (#1547)
- Race condition in `mod_muc` on room PID registration (#1608)
- Unnecessary transformations in `mod_vcard_ldap` (#1607)
- MongooseIM build on 32-bit systems (#1574)
- One of the hooks in `mod_mam_odbc_arch` wasn't properly disabled on module stop (#1576)
- Event Pusher HTTP backend used invalid hostname to fetch options (#1630)
- `mod_websockets:close/1` didn't work. (#1603)

## Other

- Removed unused API from `jlib.erl` (#1390)
- Git now treats minified JavaScript files as binaries (#1635)
- Message Archive Management refactoring (#1425)
- Documentation fixes and improvements (#1500, #1503, #1513, #1538, #1550, #1563, #1567, #1568, #1577, #1579, #1581, #1584, #1586, #1592, #1594, #1618)
- Tests improvements and refactoring (#1523, #1625, #1642, #1643, #1656)
- Code & style improvements (#1515, #1540, #1548, #1572)

## Commits, merged PRs and closed issues

- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222017-10-24..2018-01-16%22%20sort%3Acreated-asc%20)
- [List of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aclosed%20closed%3A%222017-10-24..2018-01-16%22%20)
- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2017-10-24&to=2018-01-16&type=c)

Special thanks to our contributors: @andrewvmail @igors !

# [MongooseIM 2.1.0](https://github.com/esl/MongooseIM/releases/tag/2.1.0) - 2017-10-24

## Added

- OTP 20 compatibility (#1430)
- Message Archive Management v0.6 support (#1442, #1471)
- Final stage of Mongoose Accumulators implementation (#1398, #1512)
- REST API: MUC Light rooms can be created with a specified username part (#1387)
- REST API: MUC Light rooms can be addressed with a bare JID (not only with their usernames) (#1417)
- REST API: MUC Light rooms can be destroyed (#1461)
- MAM can be configured to archive [XEP-0333 Chat Markers](https://xmpp.org/extensions/xep-0333.html) (#1377)
- `mod_http_upload_s3` can be configured to skip the ACL header (so MIM can integrate with Minio) (#1415)
- Administration REST API can be protected with the Basic HTTP Authentication (#1453)
- More configuration options for JWT authentication backend (#1321)
- (Un)Subscribing to many hooks with a single function call (#1376, #1426)
- New `ejabberd_router:route_error_reply/4` function (#1427)
- `mongoose_commands`  can accept lists of elements as an argument (#1465)

## Changed

- Switched back to strictly monotonous MAM message IDs (#1372)
- MongooseIM will not start if the ODBC connection is configured but no ODBC pools are defined (#1455)
- SASL X-OAUTH mechanism is not advertised if `mod_auth_token` is not enabled (#1450)
- `ejabberd_auth:authorize/1` is now used for authorisation in client REST API (#1409)
- DNS lookup is not performed for the S2S connection if the host is already defined in the configuration (#1314)

## Fixed

- Fix RDMBS backoff calculation (#1394)
- URL escaping and reporting in `mod_http_upload` (#1391)
- Fixed Unicode support in the MAM full text search with a Riak backend (#1407)
- Authentication crash with SASL PLAIN and an invalid password (#1433)
- Random crashes in tests (#1374, #1428)
- `mongooseim version` command was broken (#1457)

## Other

- Documentation fixes and improvements (#1373, #1380, #1382, #1385, #1396, #1399, #1402, #1408, #1416, #1418, #1434, #1441, #1445, #1451, #1456, #1468, #1469, #1472, #1475, #1477, #1480, #1482, #1483, #1484, #1485, #1486, #1487, #1488, #1489, #1490, #1492, #1493, #1494, #1495, #1496, #1498, #1499, #1501, #1502, #1503, #1504, #1506, #1507, #1508, #1532, #1534)
- First stage of `mod_mam` and its submodules' refactoring (#1381)
- Tests improvements and refactoring (#1383, #1388)
- Improved coverage check (#1397)
- Build system & scripts improvements (#1412, #1422, #1448)
- Code & style improvements (#1454)
- Updated dependency: `cqerl` (#1447)

## Commits, merged PRs and closed issues

- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222017-07-04..2017-10-23%22%20sort%3Acreated-asc%20)
- [List of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aclosed%20closed%3A%222017-07-04..2017-10-23%22%20)
- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2017-07-04&to=2017-10-23&type=c)

Special thanks to our contributors: @Beisenbek, @benkard, @deadjdona, @fblackburn1 !

# [MongooseIM 2.1.0beta2](https://github.com/esl/MongooseIM/releases/tag/2.1.0beta2) - 2017-07-06

## Added
- Roster management in client REST API (#1286)
- Silent push notifications (#1287)
- RSM support in `mod_vcard` (#978)
- MAM can be configured to archive (or not) groupchat messages in private archives (#1294)
- New command in `mongooseimctl` (#1256)
- Extended `mod_roster` backends' API (#1302)
- A warning is logged, when a module links to caller process on startup (#1247)

## Changed

- `now()` calls have been replaced with alternatives in `p1_time_compat` (#1246)
- Stanza size limit is now checked while parsing the stanza (#1285)
- `mongoose_acc` extended lifespan; supported by more hooks. (#1211, #1306)
- `crypto:rand_bytes/1` calls have been replaced with `crypto:strong_rand_bytes/1` (#1348)

## Fixed

- Improved error handling in C2S (#1264)
- MAM + MUC Light integration (#1270)
- Push nodes discovery and handling in `mod_pubsub` (#1272)
- BOSH interleaving logic (#1289)
- Error presence handling in MUC (#1307)
- Race condition between `mod_caps` and PEP (#1301)
- Minor bugs in Service Discovery (#1303)

## Other

- Expanded, better tests and improved coverage (#645, #1241, #1278, #1291)
- Ensured OTP 19.3 support. (#1251)
- Configuration improvement (#1296, #1299)
- Refactored many modules to satisfy our coding standards. (#1254, #1259)
- Many improvements and fixes in MongooseIM documentation. (#1242, #1243, #1253, #1260, #1261, #1262, #1271, #1279, #1282, #1284, #1288, #1292, #1293, #1295, #1297, #1298, #1303, #1304, #1310, #1318, #1319, #1320, #1323, #1324, #1326, #1332, #1333, #1334, #1336, #1345)
- Build improvements and fixes (#1258, #1266, #1300, #1309, #1315, #1335, #1355)

## Commits, merged PRs and closed issues

- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222017-03-29..2017-07-03%22%20sort%3Acreated-asc%20)
- [List of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aclosed%20closed%3A%222017-03-29..2017-07-03%22%20)
- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2017-03-29&to=2017-07-03&type=c)

Special thanks to our contributors: @astro @strugee @msantos @daniel-e @deadjdona !

# [MongooseIM 2.1.0beta1](https://github.com/esl/MongooseIM/releases/tag/2.1.0beta1) -  2017-03-28

## Added
- full text search in [XEP-0313: MAM](http://xmpp.org/extensions/xep-0313.html): #1136
- push notifications
    - as described in [XEP-0357: Push Notifications](https://xmpp.org/extensions/xep-0357.html) #1178, #1209
    - to Amazon SNS: #1149
- integration with [Tide](http://tide.erlang-solutions.com/) Continous Load Testing
- file size limits to [XEP-0363: HTTP File Upload](https://xmpp.org/extensions/xep-0363.html): #1171
- support for version 0.3.0 of [XEP-0363: HTTP File Upload](https://xmpp.org/extensions/xep-0363.html): #1185
- SQL prepared queries #1172, #1177
- XMPP pipelining #1181
- Erlang distribution over TLS #1228
- JSON Web Token authentication #977

## Changed

- build system to [rebar3](http://www.rebar3.org/) #1033
- hooks implementation
    - all hooks have now fold semantic (returned value is passed from hendler to handler) #1149
    - result from a hook run is passed between different hooks #1174, #1194, #1202, #1220
- RDBMS pools configuration: #1217
    - this is backward incompatible - requires change in config file
- shapers implementation: #1213
    - this lead to some race condition bug fixes

## Fixed

- handling of TLS options for S2S connections #1182
- deadlock between `mod_muc` and `mod_muc_log` #1219
- fix for uncleaned resumed sessions #1186

## Other

- tons of refactoring improving code quality and test coverage
- [complete list of merged PRs](https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222017-01-25..2017-03-28%22%20sort%3Acreated-asc%20)
- [complete list of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aclosed%20closed%3A%222017-01-25..2017-03-28%22%20)


Special thanks to our contributors: @astro, @aszlig

This release repo [history](https://github.com/esl/MongooseIM/graphs/contributors?from=2017-01-25&to=2017-03-28&type=c)

# MongooseIM 2.0.1

2017-01-24

This release includes:
- extended REST API for [clients](http://mongooseim.readthedocs.io/en/2.0.1/swagger/index.html?client=true)
    - support for SSE: [#1043](https://github.com/esl/MongooseIM/pull/1043), [#1131](https://github.com/esl/MongooseIM/pull/1131)
- improvements to [XEP-0016: Privacy Lists](http://xmpp.org/extensions/xep-0016.html): [#1084](https://github.com/esl/MongooseIM/pull/1084)
- improvements to [XEP-0191: Blocking Command](http://xmpp.org/extensions/xep-0191.html): [#1122](https://github.com/esl/MongooseIM/pull/1122)
- [XEP-0045: Multi-User Chat](http://xmpp.org/extensions/xep-0045.html) optimisations:
    - Erlang process hibernation: [#1086](https://github.com/esl/MongooseIM/pull/1086)
    - Stopping room's process if not actively used [#1095](https://github.com/esl/MongooseIM/pull/1095)
- support for [XEP-363: HTTP File Upload](http://xmpp.org/extensions/xep-0363.html): [#1139](https://github.com/esl/MongooseIM/pull/1139)
- ODBC backend for MUC Light [#1093](https://github.com/esl/MongooseIM/pull/1093)
- Cassandra backend for MAM:
    - restored Cassandra backend: [#1117](https://github.com/esl/MongooseIM/pull/1117)
    - migration to cqerl: [#1147](https://github.com/esl/MongooseIM/pull/1147)
    - performance improvements: [#1133](https://github.com/esl/MongooseIM/pull/1133)
    - migration tool for Cassandra backends in MongooseIM 1.6.2 [#1159](https://github.com/esl/MongooseIM/pull/1159)
- other improvements:
    - simplified MAM configuration [#1112](https://github.com/esl/MongooseIM/pull/1112)
    - fix for race condition in session manager [#1141](https://github.com/esl/MongooseIM/pull/1141)
    - migration to p1_mysql [#1067](https://github.com/esl/MongooseIM/pull/1067)
    - migration to p1_pgsql [#1074](https://github.com/esl/MongooseIM/pull/1074)
    - migration to fast_tls [#1078](https://github.com/esl/MongooseIM/pull/1078)
    - improved subdomain handling [#1116](https://github.com/esl/MongooseIM/pull/1116)
    - [complete list of merged PRs](https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222016-11-09..2017-01-24%22%20sort%3Acreated-asc%20)
    - [complete list of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aclosed%20closed%3A%222016-11-09..2017-01-24%22%20)

Special thanks to our contributors: @kenstir, @sstrigler, @igors, @bernardd, @msantos

This release repo [history](https://github.com/esl/MongooseIM/graphs/contributors?from=2016-11-09&to=2017-01-24&type=c)

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
