# [MongooseIM 3.6.1](https://github.com/esl/MongooseIM/releases/tag/3.6.1) - 2020-02-11

## Highlights

- Tooling and packages improvements

## All changes

### Changed

- Use `runuser` instead of `sudo` in scripts managing MongooseIM (#2617)
- Install pid and status directory explicitly (#2618)
- Massive rework of scripts building .deb and .rpm packages (#2626, #2629)
- Remove redundant time conversions in shapers (#2545)
- Small performance improvements (#2621)

## Commits, merged PRs and closed issues

- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A3.6.1)
- [List of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aclosed+closed%3A%222020-01-30..2020-02-11%22+)
- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2020-01-30&to=2020-02-11&type=c)

[List of merged PRs based on merge date]: # (https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222020-01-30..2020-02-11%22%20sort%3Acreated-asc%20)

# [MongooseIM 3.6.0](https://github.com/esl/MongooseIM/releases/tag/3.6.0) - 2020-01-29

## Highlights

- Push notifications improvements
    - Possibility to configure push notifications without real PubSub
    - Immediate push notifications to disconnected device in resume state
- Gathering of system metrics
- Support for the last two major OTP versions only: 21 and 22
- Performance improvements

There were some changes to the database schema so please take a look at [Migration guide](doc/migrations/3.5.0_3.6.0.md) when upgrading from a previous version.

## All changes

### Added

- System metrics gathering (#2523, #2532, #2550, #2571, #2578, #2580, #2586, #2591, #2594, #2598, #2601, #2603, #2607, #2610, #2612)
- MAM disco#info to MUC and MUCLight rooms (#2272)
- Chat markers support for the room's REST API (#2274)
- Possibility to close malicious connections without revealing service details (#2304)
- Ping response time metrics (#2527)
- Emoji support in VCard's nickname field (#2539)
- Swagger documentation hosted by MongooseIM (#2543, #2556)
- Persistent fields from accumulator to offline storage (#2587)
    - This requires a new column in RDBMS, see the migration guide

### Changed

- Push notifications
    - Immediate push notification to a connection in resume state (#2018, #2593)
    - RDBMS backend for `mod_event_pusher_push` (#2526)
    - PubSub-less push notifications (#2554)
    - Integration with MongoosePush API v3 (#2549)
    - Expired device_id removal (#2555)
    - Details from the push enable stanza are stored in session info now (#2568)
    - The push notification's priority can be set with enable stanza (#2569)
    - An `unacknowledged_message` hook is fired when a session is in resume state and a new message arrives (#2589)
    - Documentation update and rework (#2611)
- Riak bucket types are now configurable (#2490)
- Dependencies update:
    - `lager`: 3.8.0
    - `cowboy`: 2.7.0
    - `epgsql`: 4.3.0
    - `mysql`: 1.5.1
    - `cache_tab`: 1.0.20
    - `stringprep`: 1.0.17
    - `erlcloud`: 3.2.13
    - `jwerl`: 1.1.0
    - `observer_cli`: 1.5.3
    - `amqp_client`: 3.8.0
    - `wpool`: 4.0.1
- SASL mechanism management simplification (#2519)
- MUCLight room config simplification and unification (#2536)
- Performance improvements:
    - jid parsing in NIF (#2544)
    - use built-in base64 encoding (#2547)
    - C/C++ optimisations (#2604)
- ejabberd_sm improvements (#2566, #2582)

### Removed
- `get_stactrace` calls (#2494)
- Support for MAM v0.3 (#2496)
- Usage of `p1_time_compat` (#2498)
- `mod_push` (#2553)

### Other

- Pass the original accumulator when sending out a stanza (#2158, #2528)
- Remove sender related data from the accumulator before passing it further (#2510)
- Do not allow `riak` and `redis` pools to use the incompatible `available worker` strategy (#2243)
- Fix for connection crash when the client enables stream management but it's not available (#2482)
- Fix for REST API crash when user's JID is empty (#2543)
- Return `Bad Request` status when malformed JSON is passed to REST API (#2557)
- Possibility to call external HTTP auth backend to get valid user's certificates (#2044)
- Fix for last activity error iq responses (#2570)
- Fix c2s message bouncing issue (#2579)
- Fix for wrong namespace in ringing stanza (#2584)
- Fix warning for .deb packages (#2609)

## Commits, merged PRs and closed issues

- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A3.6.0)
- [List of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aclosed+closed%3A%222019-10-04..2020-01-29%22+)
- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2019-10-04&to=2020-01-29&type=c)

[List of merged PRs based on merge date]: # (https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222019-10-04..2020-01-29%22%20sort%3Acreated-asc%20)

## Special thanks to our contributors:
- [@vkatsuba](https://github.com/vkatsuba) (#2542, #2543, #2556, #2557)
- [@jzskca](https://github.com/jzskca) (#2272)
- [@kaniini](https://github.com/kaniini) (#2488)
- [@tomaszwojcikowski](https://github.com/tomaszwojcikowski) (#2584)
- [@ivanov-aleksander](https://github.com/ivanov-aleksander) (esl/mongooseim-docker#27, esl/mongooseim-docker#28, esl/mongooseim-docker#29, esl/mongooseim-docker#30)

# [MongooseIM 3.5.0](https://github.com/esl/MongooseIM/releases/tag/3.5.0) - 2019-10-03

## Highlights

- Reworked LDAP layer
- New method of resetting Inbox
- OTP < 21.2 support is deprecated

## All changes

### Added

- A new dedicated stanza for resetting the Inbox (#2452)
- New metrics:
  - `clusterSize`, as seen by each MIM node individually (#2322)
  - `tcpPortsUsed` (#2359, #2374)

### Changed

- Multiple improvements in the LDAP layer (#2388)
  - They are now configured like the other outgoing connections.
  - LDAP+TLS support.
  - LDAP authentication backend may be used with SASL EXTERNAL
- GDPR removal and retrieval do not query disabled backends and modules anymore (#2435)
- Push notifications are no longer sent for messages with empty body (#2394)
- Stream Management implementation has been updated to match XEP-0198 v1.6 (#2468, #2472)
- Deprecations:
  - OTP older than 21.2 (#2465)
  - XEP-0313 Message Archive Management v0.3 (#2466)
  - TLS older than 1.2 (#2377)
- Dependencies update (#2351)
  - `lager`: 3.7.0
  - `cowboy`: 2.6.3
  - `jiffy`: 1.0.1
  - `uuid`: 1.7.5
  - `fast_tls`: 1.1.1
  - `mysql`: 1.5.0
  - `cache_tab`: 1.0.19
  - `stringprep`: 1.0.16
  - `meck`: 0.8.13
  - `recon`: 2.5.0
  - `erlcloud`: 3.2.7
  - `observer_cli`: 1.5.0
  - `amqp_client`: 3.7.15
  - `eredis`: 1.2.0
  - `riakc`: 2.5.3 - no change here but downloaded from a repo now (#2397)
- Clustering operations are now protected by a global transaction (#2470)
- Client XML namespace is no longer stripped in the messages received from a client (#2423)
- `mongooseimctl` script is more robust (#2409)
- `scram` module has been renamed to `mongoose_scram` (#2401)

### Fixed

- `binary_to_atom` vulnerability (#2444)
- `push` PubSub node implementation had a bug in the affiliation check (#2438)
- The unread messages count for push notifications was retrieved improperly and triggered an error (#2481)
- Chat markers were improperly handled by Inbox (#2449)
- It is again possible to configure an idle connection timeout for Websockets (#2480)
- MUC Light role is now properly archived (#2268)
- Chat markers are now properly stored by the MUC archive (#2271)
- Inbox recognises MUC Light system messages more reliably (#2290)
- `disco#info` request no longer causes a crash when `rooms_in_rosters` is enabled (#2354)
- Mnesia backend for PubSub used to break the transaction restart logic in some operations (#2390)
- Some REST commands were unusable due to a bug in REST implementation (#2426)

### Other

- Added CircleCI integration (#2372, #2382, #2383, #2414, #2419, #2422, #2439, #2441, #2446, #2447, #2462, #2463)
- CI stabilisation (#2378, #2389, #2392, #2393, #2395, #2396, #2404, #2428, #2429, #2431, #2434, #2437, #2453, #2454, #2455, #2469, #2473, #2474)
- Test improvements and refactoring (#2351, #2381, #2398, #2399, #2403, #2408, #2411, #2417, #2418, #2421, #2424, #2432, #2457, #2458, #2459, #2475)
- Documentation updates (#2247, #2356, #2357, #2376, #2416, #2420, #2436, #2450, #2478, #2479)
- Reduced resource consumption for dev releases (#2400)
- New dev nodes are templated from existing one(s) if possible (#2407)
- Updated `.gitignore` file with new rules for logs (#2385)

## Commits, merged PRs and closed issues

- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A3.5.0)
- [List of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aclosed+closed%3A%222019-06-27..2019-10-03%22+)
- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2019-06-27&to=2019-10-03&type=c)

## Special thanks to our contributors:
- @cgrtrifork
- @Nyco
- @navneetgupta
- @jzskca

# [MongooseIM 3.4.1](https://github.com/esl/MongooseIM/releases/tag/3.4.1) - 2019-09-12

## Highlights

Patch release mitigating [ERL-944](https://bugs.erlang.org/browse/ERL-944)

### Fixed

- Vulnerability related to `erlang:binary_to_existing_atom` with `latin1` encoding (#2445)

# [MongooseIM 3.4.0](https://github.com/esl/MongooseIM/releases/tag/3.4.0) - 2019-06-26

## Highlights

- GDPR data retrieval and removal

## All changes

### Added

- GDPR data retrieval and removal (#2260, #2285, #2286, #2288, #2289, #2293, #2294, #2295, #2296, #2297, #2298, #2299, #2300, #2301, #2302, #2303, #2309, #2310, #2312, #2313, #2314, #2316, #2320, #2323, #2326, #2327, #2328, #2336, #2339, #2341, #2345, #2347, #2349)
- CLI `register` function now generates a safe, random username for a new account (#2262)
- It is possible now to change the server name returned in HTTP responses (#2308)
- A new behaviour in case of conflicting component connections: `kick_old` (#2315)

### Changed

- When a session gets replaced, the new one waits longer for the old one to close before reporting an error (#2054)
- Authentication backends no longer use a `store_type/1` function (#2254)
- Default certificates are now generated faster (#2305)

### Fixed

- Some race conditions in Stream Management (#2049)
- faulty script execution of `xep_tools` (#2252)
- MUC messages are now archived without an empty `to` attribute (#2220)
- "Offline" classic MUC rooms are now properly reported in disco results (#2238)
- Inbox responses are now properly routed to the original requester (#2276)
- Resolved issues with TLS on OTP 20.x (#2332)

### Other

- Updated `escalus` to `4.1.0` (#2337)
- Test improvements and refactoring (#2319)
- Update documentation (#2246, #2259, #2261, #2307, #2346)

## Commits, merged PRs and closed issues

- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A3.4.0)
- [List of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aclosed+closed%3A%222019-03-13..2019-06-26%22+)
- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2019-03-13&to=2019-06-26&type=c)

## Special thanks to our contributor:
- @varnerac

# [MongooseIM 3.3.0](https://github.com/esl/MongooseIM/releases/tag/3.3.0) - 2019-03-12

## Highlights

- Finalised RDBMS implementation for PubSub
- MongooseIM integration with RabbitMQ
- PKI authentication improvements

## All changes

### Added

- PubSub improvements
  * RDBMS backend for `pubsub_node` table (#2145)
  * `mod_pubsub_cache_rdbms` implementation (#2144)
  * RDBMS support for subscription options in PubSub (#2165)
  * Generic metrics measuring number of errors and execution time for different PubSub actions based on sent IQs (#2178)
  * Setting subscription options for `flat_node` (#2165)
- RabbitMQ layer (#2127, #2216)
  * RabbitMQ backend for `event_pusher`
  * `rabbit` worker pool (that can be used for any interaction with RabbitMQ)
  * `mongoose_amqp` module that deals with AMQP protocol layer
- Address best practices for using `SASL EXTERNAL` as defined by XEP-0178 (#2204, #2223, #2231)
- Upsert API for RDBMS (#2153)
- `gen_mod:opts_for_module/2` which allows you to selectively retrieve opts passed through config to a given module (#1027)
- Backend functions get a new metric: number of function calls (#2177)
- `mod_vcard` calls optional `tear_down` callback on the backend module (#2152)

### Changed

- Update dependencies:
  * `lager` `3.6.7` (#2138)
  * `cowboy` `2.6.0` (#2138)
  * `idna` `6.0.0` (#2138)
  * `uuid` `1.7.4` (#2138)
  * `fast_tls` `1.0.26`(#2138, #2203)
  * `epgsql` `4.2.1` (#2138)
  * `cache_tab` `1.0.16` (#2138)
  * `stringprep` `1.0.14` (#2138)
  * `proper` `1.3.0` (#2138)
  * `meck` `0.8.12` (#2138)
  * `bbmustache` `1.6.1` (#2138)
  * `erlcloud` `3.2.2` (#2138)
  * `observer_cli` `1.4.1`(#2138)
  * `bbmustache` `1.6.1` (#2182)
  * `jiffy` `0.15.2` (#2182)
  * `proper` `1.3.0` (#2182)
  * `escalus` `4349a80` (#2182)
  * `shotgun` `636d14e` (#2182)
  * `recon` `2.4.0` (#2162)
  * `nkpacket` `f7c5349` (#2147)
- PubSub changes
  * Extract mnesia operations around `pubsub_node` table to the backend module (#2141)
  * Optimize the way a pubsub node is removed (#2136)
  * Remove `pubsub_subscription` module, refactor opts forms processing and integrate option storage logic into DB backends (#2148)
  * `mnesia` cache backend for `pubsub_last_item` extracted to a separate _cache_ backend module (#2144)
  * Simplified `pubsub_index` API, removed `free` function (#2156)
  * Backend modules call pubsub_index when the `id` is not passed (#2156)
  * Use RDBMS autoincrementing index in place of `pubsub_index` (#2160)
  * Replace Mnesia's `transaction` and `sync_dirty` calls in the RDBMS backend with proper RDBMS equivalents (#2191)
  * Return the index when the node is created (#2160)
  * Parallelised PubSub message broadcast (#2206)
  * Spawn a new process in `mod_pubsub:broadcast_stanza/9`

- Change `stop_module_keep_config/2` and `stop_module/2` to return module opts (#1027)
- Update snippet to register users (#2181)
- Use map syntax to pass `ranch` transport options (#2188)
- Change the name of the metric responsible for the number of backend function calls (#2193)
- Replace `jsx` with `jiffy` (#2199)
- Make HTTP headers lowercase to avoid HTTP/2 connection errors (#2211)

### Fixed

- Add `mod_pubsub_db_backend:add_item` to the tracked functions (#2193)
- `erlcloud_sns:publish` content format (#2176)
- `infinity` mapped to `0` in the `matches` option in `mod_vcard_ldap` (#2179)
- `mod_vcard_ldap` ignored `ldap_uids` formed as `{"attribute"}` and only parsed `{"attribute", "format"}` correctly (#2180)
- Return MAM item-not-found IQ result when a nonexistent message ID is asked for (#2166)
- Fix `mongooseimctl debug` command to use correct hostnames (#2201)
- Use `mongooseim-docker` with a name flag and nodename fixes (#2205)
- Fix compilation errors when the compilation directory has whitespaces in it (#2203)
- Default inbox backend is set to `rdbms` (#2236)
- `mod_vcard_ldap:eldap_pool_search/6` empty list handling (#2226)
- lowercase HTTP headers in mod_bosh for HTTP/2 compliance (#2211)

### Other

- Run Travis builds on newer (not newest) Ubuntu LTS version Xenial (16.04) (#2151)
- Update mongooseim-docker to `cc7326bfd0129943206a67e57dd861ff19c403c7` (#2190)
- Test improvements and refactoring (#2165, #2162, #2164, #2170, #2127, #2142, #2146, #2147)
- Fix broken or remove outdated links in docs (#2183)
- Install the most up to date package builder epel-release for centos 7 (#2154)
- Stabilize the `ldap` job on travis (#2140)
- Update `find-hooks.awk` (#2225, #2232)
- Update `escalus` to `8911491` (#2224)
- Update documentation (#2155, #2163, #2167, #2233, #2227)

## Commits, merged PRs and closed issues

- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A3.3.0)
- [List of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aclosed+closed%3A%222018-11-21..2019-03-12%22+)
- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2018-11-20&to=2019-03-12&type=c)

## Special thanks to our contributors:
- @sstrigler
- @getong
- @cogentParadigm

# [MongooseIM 3.2.0](https://github.com/esl/MongooseIM/releases/tag/3.2.0) - 2018-11-20

## Highlights

- Client Certificate authentication for Websockets and BOSH
- Inbox improvements (push notifications integration)
- Unified outgoing connections pools

## All changes

### Added

- Improvements in `mod_inbox`:
  - User can show only active/all conversations (#2017)
  - Inbox IQ result stanza provides a total unread messages count and an active conversation count (#2047, #2056)
  - If an inbox query is malformed, a descriptive error is returned (#2052)
  - `mod_inbox` is integrated with `mod_event_pusher`, so push notifications now include an unread messages count (#2078)
- SASL EXTERNAL may be used with WebSockets and BOSH (#2093)
- `mongoose_wpool` abstraction layer (#2060, #2099, #2117)
- `rdbms`, `redis`, `riak`, `cassandra`, `http` ,`generic` and `elasticsearch` pools are started via `outgoing_pools` config option (#2077, #2079, #2084, #2087, #2101)
- Self-signed certificates may be used with `fast_tls` driver (#2102)
- `mongoose_bin` module unifies random strings generation API (#2000)
- Modules may specify "optional" dependencies to enforce the startup order (but not the startup itself) (#2029)
- Switchable RDBMS backend for `mod_pubsub` - experimental (#2122, #2113, #2129, #2131, #2134)
- Changing MUC Light room configurations is possible via REST API (#2030)
- New Message Archive Management metrics for async writers (#2023)
- New Makefile target to check code style with Elvis (#2111)

### Changed

- `ejabberd.cfg` is renamed to `mongooseim.cfg`
- OTP versions older than 19.0 are no longer supported (#2002)
- "ODBC" usage has been clarified and it has been renamed to RDBMS where relevant (#2053)
- Mongoose accumulator v2.0 is more difficult to abuse and has less implicit logic (#2076)
- Pre 1.0 XML streams are no longer supported (so is non-SASL authentication) (#1998)
- Improvements in http backend of `mod_event_pusher` (#2100)
- Cassandra layer uses a standard MIM worker pool instead of a custom one (#2043)
- Deprecated `gen_fsm` is replaced by `gen_fsm_compat` (#1996)
- Updated dependencies:
  - `fast_tls` 1.0.23 (#2002)
  - `worker_pool` 3.1.1 (#2002)
  -  `esl/cqerl` `master` (#2002)
  - `epgsql` 4.1.0 (#2002)
  - `arcusfelis/eodbc` `master` (#2002)
  - `tirerl` 1.1.0 (#2002)
  - `cache_tabz 1.0.12 (#2002)
  - `stringprep` 1.0.12 (#2002)
  - `proper` 1.2.0 from hex.pm (#2002)
  - `meck` 0.8.11  (#2002)
  - `erszcz/pa` `master` (#2002)
  - `bbmustache` 1.5.0 (#2002)
  - `recon` 2.3.6 (#2002)
  - `erlcloud` 3.1.12 (#2002)
  - `jwerl` 1.0.0 (#2002)
  - `observer_cli` 1.3.3 (#2002)
  - `eredis` 1.1.0
  - `lasse` 1.2.0 (#2016)
  - `cowboy` 2.4.0 (#2016, #2088)
  - `exml` 3.0.2 (#2050)
  - `shotgun` `9b6c1df` (#2092)
  - `escalus` `592deba` (#2016, #2092)
  - `igors/eredis` `e9688a1` (#2042)
- Implementation of MUC Light user affiliation logic is more developer-friendly (#1934)
- Generation of codecov-compatible coverage report is extracted to `rebar3_codecov` plugin (#2073)
- `mod_muc` uses maps instead of dictionaries ( #1986)
- Shaper workers are organised under dedicated supervisor (#2130)

### Fixed
- Config reload in cluster had broken verification logic (#2051)
- `fusco` clients are now properly closed in `mod_revproxy` (#2118)
- Backend proxy modules are loaded only once (#1438)
- Node cleaners are no longer crashing (#2135)
- Test runner
  - Tests are counted properly on macOS (#2004)
  - Appropriate error is returned, when there are no test nodes. (#2004)
  - Improved test specs generation and autocompletion (#2036)
- Minor fixes (#2010, #2046, #2069, #2086, #2123)
- `rebar3` release generation with OTP 21 (update to 3.6.1) (#2037)
- `NkSERVICE` cache dir configuration works as expected now (#2058)
- Dialyzer job on Travis runs with Erlang/OTP 21 and returns no errors (#2075)

### Other

- SASL authentication refactored (#2045)
- Build improvements (#1015)
- Documentation fixes and improvements ( #1527, #2024, #2038,  #2039, #2012)
- Tests improvements and refactoring (#1448, #1990, #2007, #2048, #2061,  #2085, #2120, #2132, #2133)
- Codecov thresholds for a failed build are now set to 0.5% (#1957)
- Minor cleanup (#1553, #2081)

## Commits, merged PRs and closed issues

- [List of merged PRs](https://github.com/esl/MongooseIM/issues?q=is%3Aclosed+milestone%3A3.2.0+sort%3Acreated-asc)
- [List of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aclosed%20closed%3A%222018-07-25..2018-11-20%22%20)
- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2018-07-25&to=2018-11-20&type=c)

Special thanks to our contributors: @getong @igors @justinba1010 !

# [MongooseIM 3.1.0](https://github.com/esl/MongooseIM/releases/tag/3.1.0) - 2018-07-24

## Highlights - 3.1.x

- Inbox extension enters beta stage, improved with MUC, timestamps and MSSQL support
- Test Runner - comprehensive tool for executing tests locally
- OTP 21 support
- ElasticSearch backend for message archive

## Patch: 3.1.1

- Updated `exml` to `3.0.2`, fixing an important security issue.

## All changes - 3.1.0

### Added

- `mod_inbox` enters beta stage
  - Timestamps support (#1970)
  - Classic MUC support (#1961)
  - MSSQL support (#1965)
- Test Runner (#1973, #1989, #1991)
- OTP 21 support, OTP 18.x is no longer officially supported (#1947)
- Jingle/SIP tutorial (#1980)
- ElasticSearch backend for message archive (#1900)
- Smack-specific properties support in REST API (#1976)
- `reload_cluster` command support for ignorable options (#1948)
- Jingle/SIP Re-INVITE support (#1903)
- More meaningful HTTP API errors (#1776)
- MUC hooks for user join and leave (#1898)
- Support for result limiting options in `mod_mam_meta` (#1977)

### Changed

- Message archive async writers no longer synchronise on reading operations (#1919)
- Replaced bundled LDAP driver with an OTP one (#1216)
- All worker pools are now based on one library: `worker_pool` (#1955)
- Mnesia directory is no longer removed in clustering operations (#1951)
- Inconsistent Mnesia directory names are no longer an error in clustering operations (#1904)
- `MEDIUMBLOB` is used for message archive data by default (#1873)
- Adding children to main supervisor is now more strictly verified (#1905)
- Updated dependencies:
  - `worker_pool` 3.1.1 (#1983)
  - `lager` 3.6.4 (#1992)
  - `jiffy` 0.15.2 (#1992)
  - `idna` 1.5.2 (#1992)
  - `uuid` 1.7.3 (#1992)
  - `lasse` 1.1.1 (#1992)
  - `escalus` `e7eece237a56560add06127bc9ed47d423e88dcc` (#1947)
- Removed `pooler` dependency (#1875)
- Moved some multi-module MongooseIM components to dedicated subdirectories (#1952)
- `conflict_check_failed` log severity changed to "warning" (#1981)
- Decreased severity of some log messages (#1984)

### Fixed

- `mongoose_acc` server property scope (#1925)
- `mod_inbox_odbc` was reporting false errors on MySQL upserts (#1994)
- Rosters are properly updated on subscription requests (#1931)
- DB deadlock is now handled properly in message archive preferences (#1897)
- Handling of terminating receiver process (#1949)
- `mod_jingle_sip` startup for multiple hosts (#1960)
- `Record-Route` header generation in Jingle/SIP (#1958)
- MSSQL transactions are now stable in MUC Light (#1917)

### Other

- Documentation fixes and improvements (#1906, #1936, #1966)
- Tests improvements and refactoring (#1874, #1879, #1883, #1884, #1885, #1886, #1892, #1893, #1895, #1899, #1908, #1911, #1914, #1916, #1920, #1921, #1922, #1924, #1926, #1932, #1937, #1940, #1943, #1944, #1950, #1953, #1954, #1956, #1962, #1967, #1982, #1990)
- Build improvements (#1915)
- Deps are downloaded with HTTPS (#1929)
- Codecov thresholds for failed build are now set to 0.5% (#1957)

## Commits, merged PRs and closed issues

- [List of merged PRs](https://github.com/esl/MongooseIM/issues?q=is%3Aclosed+milestone%3A3.1.0+sort%3Acreated-asc)
- [List of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aclosed%20closed%3A%222018-05-23..2018-07-24%22%20)
- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2018-05-23&to=2018-07-24&type=c)

Special thanks to our contributors: @SamuelNichols @Beisenbek @GalaxyGorilla @igors !

# [MongooseIM 3.0.0](https://github.com/esl/MongooseIM/releases/tag/3.0.0) - 2018-05-22

## Highlights

- `exml 3.0.1`, much faster and efficient than previous versions, thanks to a new XML parser: RapidXML
- Inbox extension - a way to display conversations list in chat application

## All changes

### Added

- Inbox extension - experimental (#1783)
- Acceptor pool for incoming XMPP TCP/UDP connections (#1849)
- OTP 20 support in `mod_jingle_sip` (#1825)

### Changed

- MongooseIM uses `exml 3.0.1`, based on new XML parser: RapidXML (#1729, #1870)
- Updated `fast_tls` to a version that avoids extensive usage of `stat` function (#1806)
- User sessions are hibernated (e.g. garbage collected) as frequently as possible (#1821)
- Cassandra connection pool has been refactored (#1847)
- Removed support for Message Archive Management v0.2 (#1860)
- `policy-violation` check is performed in `mod_mam(_muc)`, not in its backends (#1817)
- Removed unnecessary `-part` schemas for MySQL (#1845)
- `mod_jingle_sip` uses `origin_` acc keys instead of custom ones (#1841)

### Fixed

- ODBC support - replaced ODBC library and refactored RDBMS code (#1816, #1838)
- `mod_muc` terminated the room when a `cancel` form was received for a room in normal state (#1798)
- C2S process now ignores IQ replies addressed to previous process for the same user session (#1803)
- Metrics skipping (#1819)

### Other

- Documentation fixes and improvements (#1835, #1851, #1852)
- Tests improvements and refactoring (#1413, #1782, #1808, #1813, #1820, #1823, #1836, #1846)
- `escalus 4.0.0`
- Flexible preset application during test execution (#1802)
- Added packaging tools (#1662)

## Commits, merged PRs and closed issues

- [List of merged PRs](https://github.com/esl/MongooseIM/issues?q=is%3Aclosed+milestone%3A3.0.0+sort%3Acreated-asc)
- [List of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aclosed%20closed%3A%222018-04-18..2018-05-22%22%20)
- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2018-04-18&to=2018-05-22&type=c)

# [MongooseIM 2.2.2](https://github.com/esl/MongooseIM/releases/tag/2.2.2) - 2018-04-18

## Fixed

- SIP libraries are now excluded by `rebar.config.script` if `configure.out` file is missing.

# [MongooseIM 2.2.1](https://github.com/esl/MongooseIM/releases/tag/2.2.1) - 2018-04-17

## Fixed

- Default build failed on OTP 20.x due to SIP libraries being incompatible with this Erlang version (#1814)

# [MongooseIM 2.2.0](https://github.com/esl/MongooseIM/releases/tag/2.2.0) - 2018-04-17

## Added

- SASL `EXTERNAL` authentication method, i.e. auth with certificates (#1735)
- Jingle/SIP proxy (#1797)
- "Hidden" components capability (#1769)
- Mongoose Services (#1792)
- Hosts Refresher process for Global Distribution (#1660)
- `advertised_endpoints` option for Global Distribution (#1724)
- Pluggable backends support in `mod_muc` (#1758)
- Foreground mode (#1775)
- Now it is possible to upload test results to Google Drive (#1702)
- Conditional logging macros (#1707)
- Extended logging in `mod_push_service_mongoosepush` (#1777)
- Extra debug logs in `ejabberd_service` and `mod_websockets` (#1697)
- Extended logging in `mod_vcard` (#1715)

## Changed

- Refactored MongooseIM header files (#1570)
- `mod_event_pusher` and `mod_event_pusher_push` API has been improved; not backwards compatible (#1796)
- `gen_mod:start,stop` no longer allow to start already running and stop already stopped module (#1771)
- "Big tests" are moved from `test.disabled/ejabberd_tests/` to `big_tests` (#1778)
- Binary values are now hex-escaped in queries to MySQL (#1678)
- Updated dependencies: `fast_tls` @ `a166f0e9fe78304e5ca628fd5eff57c850241813` and `cache_tab` @ 1.0.12 (#1753, #1806)
- Updated MySQL library to 1.3.2 (#1787)
- Updated `rebar3` to 3.5.0 (#1786)
- `mod_ping` no longer pings bare JIDs (#1710)
- `mod_mam` no longer uses dynamically compiled module for accessing parameters (#1627)
- 1-1 messages REST API now uses `mongoose_acc` structure (#1744)
- Improved logging in Global Distribution (#1761)

## Fixed

- Under some conditions MongooseIM could enter infinite error routing loop (#1800)
- `mod_mam` was handling Unicode data improperly (#1748)
- `mod_event_pusher_push` could attempt pushing body-less message (#1726)
- `mod_event_pusher`'s HTTP backend was escaping data improperly (#1632)
- A corner case in Global Distribution was present that could lead to broken message order (#1689)
- Global Distribution was not caching origin info for packets from components (#1695)
- Race condition was present in outgoing connection pools' initialisation in Global Distribution (#1750)
- Global Distribution mappings manager sometimes crashed when `mongoose_router_external*` routers were first in routing chain. (#1763)
- Admin REST API wasn't working properly with MUC Light + ODBC backend (#1742)
- `mod_mam` was calculating `complete` attribute improperly when paginating backward (#1740)
- `X-OAUTH2` tokens were not deleted properly on user removal (#1746)
- MAM 0.2 is now properly deprecated (#1807)
- Received stanza size metric could be highly inaccurate (#1615)
- Tide address used `http` instead of `https` (#1701)
- MongooseIM could not be deployed in paths with spaces (#1621)

## Other

- Documentation fixes and improvements (#1676, #1696, #1709, #1727)
- Tests improvements and refactoring (#1628, #1637, #1644, #1653, #1663, #1665, #1680, #1681, #1687, #1692, #1706, #1708, #1720, #1736, #1737, #1743, #1745, #1747, #1749, #1756, #1757, #1760, #1768, #1770)
- Removed unused Riak script (#1671)
- Commit messages with Unicode characters are now properly handled (#1675)

## Commits, merged PRs and closed issues

- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222018-01-17..2018-04-17%22%20sort%3Acreated-asc%20)
- [List of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue%20is%3Aclosed%20closed%3A%222018-01-17..2018-04-17%22%20)
- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2018-01-17&to=2018-04-17&type=c)

Special thanks to our contributors: @igors @jacksgt @sstrigler @GalaxyGorilla @varnerac!

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

- Fix RDBMS backoff calculation (#1394)
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
