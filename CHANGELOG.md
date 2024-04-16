# [MongooseIM 6.2.1](https://github.com/esl/MongooseIM/releases/tag/6.2.1) - 2024-04-12

## Highlights

- Enhanced CETS
- Pools configuration
- Traffic shapers update
- Support for MAM v1.1.0
- More reliable testing
- Various improvements and fixes

## All changes

### Added
- RDBMS backend for `mod_caps` (#4211)
- Commit hash to GraphQL server status (#4221, #4262)
- Erlang Doctor debugging tool (#4248)
- Pools:
  - By host type (#4229)
  - By host config (#4235)
  - Names to `mongoose_rdbms` (#4231)

### Changed
- Improved shapers (#4187, #4203, #4213)
- Enhanced discovery requests handling (#4194)
- User-friendly errors for internal databases in the GraphQL API (#4192)
- Increase `idle_timeout` for SSE (#4196)
- Presence management refactor (#4207)
- MAM implementation update to version 1.1.0 (#4218, #4225)
- Roster management refactor (#4209)

### Fixed
- CETS improvements:
  - Pause on all nodes (#4204)
  - Node cleanup (#4234, #4250, #4251)
  - Unnecessary logs removal (#4205)
  - Node discovery (#4255, #4256)
- MAM lookup error handling (#4191)
- MUC Light `id` definition for MariaDB (#4195)
- `max_stanza_size` issue (#4197)
- Duplicate migration files (#4230)
- Invalid MAM IDs parsing and overflow (#4242)
- Certificate options verification on HTTPS (#4236)
- GitHub Actions status badge (#4261)

### Other
- Testing improvements/fixes (#4176, #4202, #4212, #4237, #4239, #4243, #4241, #4246, #4257, #4259, #4260)
- Update migration guide (#4258)

## Commits, merged PRs and closed issues
- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A6.2.1)

- [List of closed issues](https://github.com/esl/MongooseIM/issues?q=is%3Aissue+is%3Aclosed+closed%3A2023-12-13..2024-04-12)

- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2023-12-13&to=2024-04-12&type=c)

- [List of merged PRs based on merge date](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+base%3Amaster+merged%3A%222023-12-13..2024-04-12%22+sort%3Acreated-asc+)

# [MongooseIM 6.2.0](https://github.com/esl/MongooseIM/releases/tag/6.2.0) - 2023-12-13

## Highlights

- Introduced CETS as an alternative internal database to Mnesia
- Updated many XEP implementations to the newest version
- Implemented partial support for XEP-0386: Bind 2 and XEP-0388: Extensible SASL Profile
- Support for Erlang 26
- Removed support for Riak
- Various improvements and fixes

## Added

- CETS
  - CETS backend for session management and stream management (#3629, #4075, #4143)
  - Add GraphQL handler for CETS info (#4015, #4116)
  - RDBMS discovery backend for CETS (#4022, #4042, #4108, #4049, #4165, #4182)
  - CETS backend for `mod_bosh` (#4050)
  - CETS support for components (#4047)
  - CETS support for S2S (#4046)
  - CETS backend for `mod_muc` (#4066)
  - CETS backend for `mod_jingle_sip` (#4076)
  - CETS backend for `mongoose_cluster_id` (#4136)
  - Add CETS backend for `mod_keystore` (#4140)
  - Remove mnesia from `mod_register` (#4146)
  - Anonymous auth supports mnesia and CETS backends (#4148)
  - Custom EPMD module (#4179)
- Unified XEP list for xmpp.org (#4021, #4024, #4025, #4123)
- Reporting to Google Analytics 4 (#4040, #4061)
- Add config option to limit the number of users per domain (#4059)
- XEP-0386: Bind 2 and XEP-0388: Extensible SASL Profile
  - Extensible SASL Profile (#4101, #4102)
  - Bind 2 (#4113, #4114)
- Log internal-server-errors in `mod_privacy` (#4139)

## Changed

- XEP updates
  - Implement XEP-0004: Data Forms in a separate module (#4028, #4031)
  - Update XEP-0016: Privacy Lists (#4038)
  - Update XEP-0030: Service Discovery (#4039)
  - Update XEP-0050: Ad-Hoc Commands (#4043, #4048)
  - Update XEP-0363: HTTP File Upload(#4053)
  - Update attributes for XEP-0178 and add for XEP-0220 (#4057)
  - Update XEP-0045: Multi-User Chat (#4054)
  - Update XEPs: XEP-0082, XEP-0115, XEP-0124, XEP-0157, XEP-0160, XEP-0163, XEP-0199, XEP-0248, XEP-0277 (#4060)
  - Update XEP-0280 (#4083)
  - Update XEP-0060: Publish-Subscribe (#4092)
  - Update XEP-0215: External Service Discovery (#4120)
  - Advertise support for XEP-0249: Direct MUC Invitations (#4168)
- Upgrade segmented_cache library and its telemetry events (#4041)
- Improved Metrics initialization (#4070)
- Initialise domain workers in the supervision tree instead of manually (#4069)
- Config in one persistent term (#4093)
- C2S features optimisations (#4094)
- Patch `ejabberd_sm` (#4096)
- Use `jid:make_bare/2` instead of `jid:make/3` where appropiate (#4109)
- Unify `auth_module` and `info` in `c2s_data` record (#4110)
- Simplify specs for `ejabberd_sup` and let workers terminate (#4117)
- Raise an error if `mnesia:create_table/2` fails (#4138)

## Fixed

- Fix `mod_event_pusher:push_event/3` (#3939)
- Removing incorrect CORS headers (#4006)
- Fix handling of the undefined host type for stream errors (#4052)
- Put reporter init after app startup (#4085)
- Fix slow getaddrs call in global distribution (#4086)
- C2S fixes (#4095, #4129)
- Fix invalid username in scram authentication (#4118)
- Return a proper type from `mod_muc:node_cleanup_for_host_type/3` (#4122)
- Correctly handle the case when TLS is disabled (#4150)
- Fix error on ping timeout with stream management (#4153)
- Update epgsql to fix an issue with Erlang/OTP 26 (#4169)
- `mod_muc_light` config fix (#4178)
- Change domain validation logic (#4184)
- Require 'cacertfile' for just_tls when verify_mode = 'peer' (#4189)

## Removed

- Remove riak (#4035)
- Remove legacy CLI commands (#4160)

## Performance improvements

- Cache router, filter, and process handlers into funs (#4068)
- C2S features small optimisation (#4077)
- Replace `erlang:apply` with explicit function calls for hooks (#4073)
- Avoid calling `ejabberd_sm_backend:get_sessions/3` second time when routing presences (#4089)
- Put hooks into persistent_term using batching (#3878)

## Other

- Tests improvements/fixes (#4064, #4072, #4079, #4098, #4099, #4100, #4104, #4103, #4107, #4115, #4137, #4142, #4147, #4155, #4164)
- Documentation updates (#4030, #4034, #4055, #4087, #4130, #4133, #4181, #4190)
- CI improvements/fixes (#4023, #4026, #4027, #4029, #4097, #4112, #4149, #4145, #4152, #4166, #4167, #4171)
- Upgrade exometer_report_graphite (#4134)
- Support for Erlang 26 (#4121)
- Rename db and node to mongooseim (#4172)

## Commits, merged PRs and closed issues
- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A6.2.0)

- [List of closed issues](https://github.com/esl/MongooseIM/issues?q=is%3Aissue+is%3Aclosed+closed%3A2023-05-11..2023-12-13)

- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2023-05-11&to=2023-12-132023-12-13&type=c)

- [List of merged PRs based on merge date](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+base%3Amaster+merged%3A%222023-05-11..2023-12-13%22+sort%3Acreated-asc+)

# [MongooseIM 6.1.0](https://github.com/esl/MongooseIM/releases/tag/6.1.0) - 2023-05-10

## Highlights

- Reworked C2S architecture
- Docker image for arm64
- Update of Universal Analytics to Google Analytics 4
- Dialyzer types fix
- Deprecation of Riak
- More reliable test suites
- Various improvements and fixes

## All changes

### Added
- New C2S architecture (#3765)
  - `presence` (#3746)
  - `mod_last` (#3750)
  - `mod_register` (#3751)
  - `privacy` (#3747)
  - `pubsub` (#3785)
  - `ping` (#3748)
  - `metrics` (#3800)
  - `steam_management` (#3796)
  - `mod_csi` (#3880)
  - `sasl_external` (#3911)
  - More granular hooks (#3852, #3857, #3955)
  - Fixed `stream_management` timeouts (#3934)
  - Stopped routing broadcast tuples (#3946)
  - Unified metrics (#3967)
  - C2S Migration guide (#3965)
  - Fixed duplicated logout in `stream_management` (#3983)
  - Adapted existing test suites (#3772, #3778, #3783, #3787, #3786,  #3845, #3882, #3909, #3925, #3927, #3932, #3931, #3940)
  - Miscellaneous changes (#3729, #3797, #3790, #3816, #3858, #3888, #3904, #3908, #3917, #3919, #3935, #3950, #3957, #3959, #3972)
- Added information about GraphQL (#3905)
- Enable CircleCI tests insights (#3899)
- Hooks improvement (#3913, #3912)
- `mod_inbox` improvements (#3910, #3974, #4016)
- Checking push form fields (#3916)
- Checking for MUC domain when archiving messages (#3936)
- Updated base Docker image (#3943)
- Implemented `mod_pubsub_db` without dynamic modules (#3953)
- Checking the `to` JID (#3971)
- Auto registration/unregistration of hooks in `gen_mod` (#3954)
- Added port and IP in the listener options for WebSockets and BOSH (#3977)
- Improved MAM logging (#3984)
- Added error reason to batch worker termination (#3985)
- Docker image for arm64 (#3979, #3986, #3988, #4009)
- Full support for TLS version 1.3 (#3989)
- Restarting executes in transactions (#3973)
- GraphQL server status now returns MongooseIM version (#3995)
- Capability of measuring asynchronous SQL execute requests (#4002)
- Enabled codecov with the new uploader (#4013)
- `remove_domain` for `internal` auth (#4010)
- Passing SID into `store_info` (#4007)

### Changed
- Unified status icons (#4000)
- Reworked mongoose transport (#3982)
- Updated dependencies (#3918, #3976, #4017)
- Updated MySQL schema (#3944)
- Set cookie to mongooseim (#3930)
- Moved to Google Analytics 4 (#4011, #4019)

### Fixed
- Issues with GitHub actions (#3992, #4003)
- Status badges (#3975)
- Announcing session establishment (#3970)
- Input for `mam` errors (#4008)
- Issue with certificate verification for Google Analytics (#3978)
- Incremental removal query (#3924)
- Unknown types in Dialyzer (#3929)
- Scram invalid xml response (#4020)

### Removed
- Legacy C2S implementation (#3805, #3860)
- Some of the hooks with no handlers (#3990)
- Incorrect CORS headers (#4005)
- Riak is now deprecated (#3981)

### Other
- Test suites improvements (#3906, #3923, #3926, #3937, #3961, #3968, #3964, #3960, #3969, #3960, #3998, #3999)
- Updated documentation (#3914, #3962, #3966, #4014)

## Commits, merged PRs and closed issues
- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A6.1.0)

- [List of closed issues](https://github.com/esl/MongooseIM/issues?q=is%3Aissue+is%3Aclosed+closed%3A2022-12-20..2023-05-10)

- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2022-12-20&to=2023-05-10&type=c)

- [List of merged PRs based on merge date](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+base%3Amaster+merged%3A%222022-12-19..2023-05-10%22+sort%3Acreated-asc+)

## Special thanks to our contributors:
- [@askarsyzdykov](https://github.com/askarsyzdykov) Fix typo in documentation (#3966)

# [MongooseIM 6.0.0](https://github.com/esl/MongooseIM/releases/tag/6.0.0) - 2022-12-19

## Highlights

- GraphQL API and Command Line Interface (CLI) with many new commands
- Unified and reworked REST API
- Incremental and asynchronous dynamic domain deletion
- Full pagination support with RSM for Inbox
- Significant code refactoring
- Various improvements and fixes

## All changes

### Added
- GraphQL request handling and execution (#3354, #3442, #3454, #3515, #3633, #3642, #3646 #3648, #3689, #3719, #3803, #3822, #3830, #3894)
- GraphQL API schema and types (#3448, #3506, #3731, #3856, #3862, #3767)
- GraphQL API tests (#3745, #3474, #3709, #3718, #3720, #3730, #3736, #3740)
- GraphQL command implementation for the following categories:
  - account (#3503, #3824, #3895)
  - domain (#3499, #3715, #3851)
  - gdpr (#3711, #3855)
  - httpUpload (#3674, #3868)
  - inbox (#3694)
  - last (#3651, #3850)
  - metrics (#3665, #3861)
  - mnesia (#3725, #3896)
  - muc (#3615, #3627, #3799, #3875)
  - muc_light (#3538, #3563, #3563, #3576, #3742, #3881)
  - offline (#3688, #3864)
  - private (#3652, #3891)
  - roster (#3586, #3756, #3873)
  - server (#3744, #3793, #3877)
  - session (#3521, #3883)
  - stanza (#3483, #3565, #3814)
  - stat (#3700, #3715, #3870)
  - token (#3713, #3863)
  - vcard (#3639, #3890)
- GraphQL CLI (Command Line Interface) (#3701, #3702, #3708, #3710, #3714, #3724, #3739)
- GraphQL API documentation (#3704, #3773)
- Incremental and asynchronous domain deletion (#3774, #3775, #3777, #3813, #3889)
- Full pagination support for Inbox (#3827, #3843, #3844)

### Removed
- Legacy hooks module (#3892)
- Legacy REST API command registry (#3697, #3759)

### Changed
- Replaced legacy hooks and handlers with `gen_hook` (#3758, #3760, #3762, #3763, #3769, #3782, #3784, #3789, #3792, #3798, #3802, #3807, #3808, #3811, #3815, #3817, #3818, #3821, #3825, #3826, #3828, #3829, #3831, #3832, #3833, #3834, #3835, #3836, #3837, #3838, #3839, #3840, #3841, #3842, #3846, #3847, #3848, #3849, #3853, #3854, #3865, #3866, #3867, #3871, #3874, #3876, #3879, #3884, #3886, #3887)
- Reworked and unified REST API (#3741, #3753, #3768, #3771, #3776, #3780, #3801)
- Performance improvements (#3682, #3687, #3726, #3738, #3761)
- Added missing metrics (#3678)
- Improved configurability (#3728, #3733)
- MAM module names made more intuitive (#3684)
- Improved DB request aggregation (#3755)
- Improved logging (#3885)
- Minor refactoring (#3732, #3764, #3809)

### Fixed
- Cassandra authentication issue (#3872)
- Message sent to oneself was stored twice (#3859)
- DB aggregator could stop working after an error (#3757)
- Unexpected errors in logs (#3695, #3743)
- Minor issues with MAM result paging (#3734)
- Incorrect optimization flags passed to asynchronous workers (#3727)
- Invalid format of the JWT secret (#3716)
- Persistent rooms were not stored in the DB (#3707)
- Issues with the anonymous login (#3706)

### Other
- Updated documentation (#3424, #3675, #3677, #3679, #3766, #3794, #3869, #3893, #3900, #3902, #3903)
- Updated tools and scripts (#3698, #3723, #3737, #3897)
- Updated dependencies and other third-party software (#3683, #3699, #3779, #3820, #3823, #3898)
- Improved CI process (#3680, #3686, #3676)

## Commits, merged PRs and closed issues
- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A6.0.0)

- [List of closed issues](https://github.com/esl/MongooseIM/issues?q=is%3Aissue+is%3Aclosed+closed%3A2022-06-09..2022-12-19)

- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2022-06-08&to=2022-12-19&type=c)

- [List of merged PRs based on merge date](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+base%3Amaster+merged%3A%222022-06-08..2022-12-19%22+sort%3Acreated-asc+)

## Special thanks to our contributors:
- [@theowenyoung](https://github.com/theowenyoung) Add XEP-0333 Docs (#3424)

# [MongooseIM 5.1.0](https://github.com/esl/MongooseIM/releases/tag/5.1.0) - 2022-06-08

## Highlights

- Internal configuration rework
- Inbox functionality and performance improvements
- Remove the dynamically compiled modules
- The new OTP 25 support
- Miscellaneous enhancements, optimizations & improvements

## All changes

### Added
- Cache consistency. Use `segmented_cache` as a cache backend and have all caches use it (#3330)
- Persistent term config. Replace `ejabberd_config` with new persistent terms `mongoose_config`(#3367, #3338, #3343, #3356)
- Create async pools basing on the MAM async workers concept (#3433)
- Add new `ensure_subscribed_metric/3` to mongoose metrics module (#3353)
- Add some property based tests for MAM's `is_archivable_message/4` (#3450)
- Create SECURITY.md (#3562)
- Allow to use podman instead of docker (#3522, #3543)
- Support the new OTP 25 (#3667)

### Removed
- Hook handler records don't need the `key` field (#3342)
- Remove dead code (#3451)
- Remove deprecated `mod_aws_sns` (#3607)
- Remove deprecated `mod_revproxy` (#3617)
- Remove custom `eldap.hrl` (#3647)
- Removed in internal configuration rework
   - Remove all calls to `gen_mod:set_module_opt` from modules, making the module config constant(#3389)
   - Remove calls to `gen_mod:set_module_opt(s)` in big tests (#3398)
   - Remove `gen_mod:set_module_opt(s)` (#3468)
   - Remove the ETS table from `gen_mod` (#3484)
   - Eliminate `gen_mod:get_opt_subhost` (#3626)
- Remove dynamically compiled modules (#3394)
   - POC module `mod_private` (#3323)
   - Remove `ejabberd_rdbms` and `mongoose_rdbms_type` (#3392)
   - Allow `global` to be passed into `mongoose_backend` as an arg (#3386)
   - MAM cassandra backend without dynamically compiled modules (#3373)
   - Use persistent terms to store `backend_module` in `ejabberd_sm` (#3379)
   - Add `mongoose_backend:get_backend_name` (#3345)
   - `mod_auth_token` (#3336)
   - `mod_last` (#3339)
   - `mod_event_pusher_push` (#3340, #3348)
   - `mod_privacy` (#3347)
   - `mod_muc` (#3349)
   - `mod_inbox` (#3350, #3362, #3440)
   - `mod_muc_light` (#3351, #3418)
   - `mod_offline` (#3352, #3371)
   - `mod_smart_markers` (#3357)
   - `mod_vcard` (#3358)
   - `mod_roster` (#3364)
   - `mod_pubsub` (#3368)
   - `mod_http_upload` (#3372)
   - `mod_keystore` (#3375)
   - `mongoose_rdbms` (#3390)
   - `ejabberd_router`(#3381)
   - `mod_global_distrib_mapping` (#3582)

### Changed
- Hook functions (#3331)
- Extend `safely` module API (#3337, #3341)
- Make `mongoose_commands` errors human-readable (#3346)
- Add the accumulator to the `is_muc_room_owner` and `can_access_room` room hooks (#3417)
- Worker pool cleanups (#3419)
- Upgrade mongoose JID (#3525)
- Add backend logic to `mod_stream_management` (#3556)
- Don't use Mnesia for IQ response tracking (#3560)
- Use dynamic aware check for global domains in ACL module (#3327, #3332)
- Store ACL specs in the global config (#3335)
- Allow skipping cover compilation for small tests (#3382)
- REST `/commands` endpoint now lists commands with more detailed info (#3391)
- Revert scope to `global` for all global distributed Redis presets (#3393)
- Refactor some usage of stanza types in `ejabberd_sm` (#3396)
- Simplify stream management suite (#3453)
- Performance and metrics touch on async workers (#3502, #3514, #3529)
- Use ETS backend in `mongoose_router` instead of Mnesia (#3505)
- Refactor `can_change_ra` function in `mod_muc` (#3581)
- Reconfigure supervision flags in pools (#3593)
- Install a hook to filter messages being buffered (#3654)
- Make RDBMS asynchronous
   - Add RDBMS `execute_cast` to use when the return value is not needed (#3485)
   - Add async RDBMS order (#3611)
   - Add RDBMS transaction async requests (#3643)
- Perf optimizations
   - Pre-build metric prefixes and fetch-or-rebuild when needed (#3634, #3649)
   - Pattern matches the map key for more efficiency (#3635)
   - Skip pretty-printing of unused accumulator field (#3637)
- Message Archive Management improvements
   - Use `mongoose_acc` timestamp in MAM (#3320)
   - Same stanza id for peers (#3376)
   - Retract on stanza ids (#3377, #3384, #3385)
   - Retraction events (#3497, #3498, #3513)
   - Report wrong stanza id more friendly (#3591)
   - Make MAM return timestamps in microseconds (#3595)
   - Reorder MAM's `filter_local_packet` hook order (#3623)
   - MAM shaper improvements (#3641)
- Smart markers improvements (#3590)
   - Implement all the automatic removals (e.g. `remove_domain`) for the smart markers (#3544)
   - Prepare a documentation page for smart markers (#3535)
   - Implement all configurations for the smart markers (#3546, #3592)
   - Do not display a warning when processing a message with repeated markers (#3673)
- Inbox improvements (#3359, #3596, #3604)
   - Improve types for dialyzer and log errors when they happen (#3366)
   - Simplify time-unit conversions (#3405)
   - Fix inbox filter bug that puts unnecessary load on the auth backend (#3449)
   - Refactor Inbox tests (#3452)
   - Use async pools in inbox (#3462, #3500)
   - Fix `queryid` (#3494)
   - Extend queries (#3597)
   - Implement boxes that allow the classifying of entries (#3608)
   - Add async removes - make asynchronous inbox consistent (#3616, #3670)
- Internal config rework
   - Simplify MUC Light config (#3387)
   - Unfold global options without the temporary functions (#3406)
   - Prepare tests for config defaults (#3408)
   - Introduce config defaults for the 'general' section (#3409)
   - Replace `local_config` records with key-value pairs (#3410)
   - Add an option to format a section/list as a map (#3420)
   - Move auth config spec to auth backend modules (#3437)
   - Introduce defaults in the auth section (#3439)
   - Section-based auth config (#3446)
   - Introduce default values for auth methods (#3458)
   - Auth password section config rework (#3463)
   - Store module config in a map and keep it updated (#3469, #3488)
   - Use `dynamic_modules` in big tests (#3481)
   - Store listener config in a map (#3495)
   - Keep ACL conditions as maps (#3501, #3504)
   - Test config parsing with post-processing (#3507)
   - Move `domain_certfile` to the 'general' section (#3512)
   - Keep s2s options: 'outgoing', 'dns', 'address' in maps (#3516)
   - Put all s2s options in a map and allow specifying them globally or per host-type (#3523)
   - Module config as a map (#3534)
   - Allow customizing auth module list per listener (#3539)
   - Migrate segmented cache to maps (#3572, #3594)
   - Test config metrics (#3613)
  -  Put `mod_adhoc` opts in a map with defaults (#3537)
   - Put `mod_bosh` opts in a map with defaults (#3540)
   - Put `outgoing_pools` opts in a map with defaults (#3541)
   - Put `mod_inbox` opts in a map with defaults (#3547)
   - Put `mod_private` opts in a map with defaults (#3549)
   - Put `mod_disco` opts in a map with defaults (#3550)
   - Put `mod_version` opts in a map with defaults (#3552)
   - Put `mod_vcard` opts in a map with defaults (#3553)
   - Put `mod_time` opts in a map with defaults (#3554)
   - Put `gen_mod_deps` opts in a map with defaults (#3555)
   - Put `mod_last` opts in a map with defaults (#3557)
   - Put `mod_mam` opts in a map with defaults (#3559)
   - Put `mod_shared_roster_ldap` opts in a map with defaults (#3558)
   - Put `mod_privacy` opts in a map with defaults (#3567)
   - Put `mod_sic` opts in a map with defaults (#3568)
   - Put `mod_roster` opts in a map with defaults (#3569)
   - Put `mod_stream_management` opts in a map with defaults (#3571)
   - Put `mod_muc` opts in a map with defaults (#3575)
   - Put `mod_push_service_mongoosepush` opts in a map with defaults (#3577)
   - Put `mod_pubsub` opts in a map with defaults (#3578)
   - Put `mod_ping` opts in a map with defaults (#3579)
   - Put `mod_muc_light` opts in a map with defaults (#3580)
   - Put `mod_global_distrib` opts in a map with defaults (#3587)
   - Put `mod_offline` opts in a map with defaults (#3589)
   - Put `mod_caps` opts in a map with defaults (#3598)
   - Put `mod_keystore` opts in a map with defaults (#3599)
   - Put `mod_jingle_sip` opts in a map with defaults (#3600)
   - Put `mod_carboncopy` opts in a map with defaults (#3602)
   - Put `mod_event_pusher` opts in a map with defaults (#3603)
   - Put `mod_csi` opts in a map with defaults (#3605)
   - Put `mod_register` opts in a map with defaults (#3606)
   - Put `mod_offline_chatmarkers` opts in a map with defaults (#3609)
   - Put `mod_http_upload` opts in a map with defaults (#3614)
   - Rework service configuration and management (#3620)
   - Store service options in maps with defaults (#3624)
   - Service and module options only in maps (#3625)
   - Listener configuration rework (#3628)
   - Rework HTTP handler configuration (#3636)
   - Use maps for `mongoose_wpool` options (#3645)
   - TLS config rework (#3653)
   - Use `format_items = map` by default for config sections (#3655)
   - Access commands in maps (#3656)
   - Final internal config cleanup (#3659)

### Fixed
- Edoc generation (#3333)
- Rest admin API - return 404 when command not found instead of 500 (#3383)
- Identify only stored users on offline routing (#3395)
- Fix typos (#3475, #3621)
- Fix certs (#3447, #3435, #3447, #3632, #3640)
- Fix print-dots for Mac OS Monterey (#3480)
- Add a newline after module options to improve config readability (#3402)
- Correctly pass connection options to Fusco (#3426)
- Fix Github Actions CI - run MSSQL as root again (#3456)
- Fix `ejabberd_c2s` - don't drop the host type in `update_stanza` (#3464)
- Adding `queryId` to archived inbox messages (#3482, #3492)
- MAM - Avoid asking for user exist with a room (#3470)
- Fix starting new caches (#3508, #3518)
- Add `server_name_indication_host` config option (#3510)
- Less MIM compiler warnings (#3524)
- Add config flag to be able to set sni hostname matching (#3528)
- Fix `is_configured` in async pools (#3566)
- Do not call the obsolete backend API in CT hook (#3570)
- Fix metrics for auth backend not being reported (#3585)
- Fix `cowboy_static` options (#3618)
- Fix `mod_ping` with `host_types` (#3638)
- Fix a registration timeout cleanup (#3601, #3671)
- Fix restart RabbitMQ connection (#3631, #3672)
- Use metric probes instead of eval for slow calculated metrics (#3644)
- Fix flaky tests and speed up CI:
   - Gather list of failed test cases in a separate issue (#3361)
   - Fix `rest_client_SUITE:muc:messages_can_be_paginated_in_room` (#3363)
   - Fill the gaps in the event table for `service_domain_db` (#3365)
   - Run muclight inbox tests one by one instead of parallel (#3374)
   - Print preset in `tools/circle-publish-github-summary-comment.sh` (#3378)
   - Enable `mod_last` only on its test suite (#3413)
   - Enable DB modules in tests only when used (#3416)
   - Retry insert MAM prefs on deadlock (#3421)
   - Fix `domain_removal_SUITE:last_removal` (#3422)
   - Remove wait from `unavailable_resources_dont_get_carbons` (#3431)
   - Run tests in pubsub:basic in parallel (#3432)
   - Use in-memory riak backend to speed-up IO (#3434)
   - Fix `sm_SUITE` (#3436)
   - Use `wait_for_archive_size` inside `send_rsm_messages` helper (#3443)
   - Avoid a race condition in `sm_SUITE:resume_session_state_stop_c2s` (#3444)
   - Fix `offline_SUITE:max_offline_messages_reached` (#3445)
   - Reroute presences correctly on reconnect (SM fix) (#3459)
   - Wait for async writers to flush messages in tests (#3466)
   - Inbox flaky tests fixes (#3630)

### Other
- Faster, more stable CircleCI on docker with DB backends (#3401, #3519, #3530)
- Use OTP 24.3.4 for CircleCI (#3661)
- Updating dependencies and cleaning the rebar.lock file (#3425, #3430, #3465, #3490, #3520, #3531, #3533, #3573, #3657, #3669)
- Document low-level MAM options (#3329)
- Document MUC Light cache affiliations (#3509)
- Minor documentation improvements (#3369, #3400, #3404, #3412, #3438, #3532, #3545, #3668)
- Update CONTRIBUTING.md (#3622)

## Commits, merged PRs and closed issues
- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A5.1.0)

- [List of closed issues](https://github.com/esl/MongooseIM/issues?q=is%3Aissue+is%3Aclosed+closed%3A2021-10-08..2022-06-08)

- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2021-10-08&to=2022-06-08&type=c)

- [List of merged PRs based on merge date](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+base%3Amaster+merged%3A%222021-10-08..2022-06-08%22+sort%3Acreated-asc+)

## Special thanks to our contributors:
- [@kianmeng](https://github.com/kianmeng) Fix typos in doc (#3545)
- [@amccormick93](https://github.com/amccormick93) Fix a registration timeout cleanup (#3601)
- [@ColaCheng](https://github.com/ColaCheng) Fix restart RabbitMQ connection (#3631)

# [MongooseIM 5.0.0](https://github.com/esl/MongooseIM/releases/tag/5.0.0) - 2021-10-07

## Highlights

- Support for dynamic domains
- Documentation improvements and reorganization
- Miscellaneous enhancements & improvements

## All changes

### Added
- Support dynamic domains in (#3095, #3158, #3220, #3226, #3239, #3225, #3212)
  - Config (#3053)
  - Service for database domain management (#3052)
  - REST and CLI API for domains (#3058)
  - Init (#3061)
  - Auth (#3063)
  - PM (#3075)
  - Hooks (#3089)
  - IQ handlers (#3118)
  - Tests (#3108, #3109, #3235)
- Call remove domain hook from `mongoose_domain_api` (#3237)
- Use `host_types` instead of host in modules and hooks (#3120, #3170, #3097)

- Support for dynamic domains in modules:
   - `mod_ping` (#3136)
   - `ejabberd_users`(module renamed to `mongoose_users`) (#3135, #3161)
   - `mod_inbox` (#3132, #3141, #3103, #3165, #3228)
   - MAM/MUC (#3123, #3143, #3155, #3157, #3107, #3231, #3305, #3092, #3147)
   - auth backends (#3106, #3295)
   - `mod_http_upload` (#3267)
   - `mod_sic` (#3258)
   - `mod_auth_token` and `mod_keystore` (#3262)
   - `mod_csi` (#3260)
   - `mod_amp` (#3261)
   - `mod_time` (#3255)
   - `mod_version` (#3256)
   - `mod_bosh` (#3253)
   - `mod_adhoc` (#3252)
   - `mod_register` (#3247)
   - `mod_muc_commands` (#3248)
   - `mod_smart_markers` (#3243)
   - `mod_blocking` (#3196)
   - `mod_vcard` (#3221, #3304)
   - `mod_privacy` (#3189)
   - `mod_last` (#3188, #3309)
   - `mod_private` (#3175)
   - `mod_roster` (#3159, #3291)
   - `mod_caps` (#3156)
   - `mod_offline` (#3164, #3263, #3299)
   - `mod_stream_management` (#3149)
   - `mod_carboncopy` (#3130, #3144)
   - `mod_disco` (#3128, #3146, #3151)

- Tests for dynamic domains:
  - Enable mim2 node to allow running more tests for dynamic domains (#3264)
  - Enable test suites for dynamic domains (#3268, #3269, #3271, #3272, #3275, #3285, #3276, #3277, #3278, #3279, #3280, #3281, #3283, #3284, #3287, #3142, #3302, #3241, #3246)
  - Test mongooseimctl with dynamic domains (#3273, #3274)
  - Test roster metrics with dynamic domains (#3286)
  - Fix anonymous login and test it for dynamic domains (#3254)
  - Test clustering and domain management with dynamic domains (#3266)

- Other:
   - Support OTP 24.X (#3186)
   - GDPR get data takes host type as a parameter (#3140)
   - Initial implementation of subdomains management subsystem (#3116)
   - Support XEP-0201 in client api for message (#3236)
   - Support PostgreSQL 14 (#3316, #3319)

### Removed
- Remove all occurences of ?MYNAME except stream errors and the initial value in `ejabberd_c2s` (#3039)
- Remove `local_send_to_resource` hook (#3139)
- Unused files/modules (#3121, #3122, #3207, #3214, #3310)
- Unused dependencies (#3199, #3200, #3201)
- Unused code fragments (#3311, #3313, #3322)
- Support OTP 22 (#3289)

### Changed
- Prepare queries in `mongoose_cluster_id` (#3098)
- Make vcard processing parallel (#3315)
- Make `pool_name` configurable for `service_domain_db` (#3205)
- Allow to add subelements to the mam iq-fin element (#3191, #3195)
- Use map for `mod_mam:message_row()` (#3093)
- Rework of the `gen_mod` module (#3104)
- Change Room EventData type to map (#3111)
- Format stacktrace args properly (#3124)
- Make dummy auth delays configurable (#3131)
- REST API better error reporting (#3137)
- Use auth for `mongoose_domain_handler` from REST (#3160)
- Reimplement `mod_cache_users` using persistent_terms (#3169)
- Hooks framework rework (#3174)
- Refactor async writer for `mod_mam` (#3216)
- More consistent accumulator use (#3314, #3240, #3249, #3314)
- Use more full jids and avoid to_lus conversions in `mod_muc_light` (#3250)
- Tests improvements (#3133, #3134, #3181, #3208, #3213, #3217, #3218, #3219, #3227, #3230, #3232, #3127, #3238, #3257, #3297, #3312)
- Minor changes (#3100, #3324, #3317)

### Fixed
- Start only used metrics (#3096)
- Prevent infinite loop when domain isolation is on on both domains (#3110)
- Prepared query in `mongoose_cluster_id` causes errors on startup (#3112)
- Tag should be an atom in ldap types (#3178)
- Missing or not working xref (#3179)
- Occuring `mongoose_metric_hooks` error (#3184)
- FTBFS on implicit declaration of function `erts_exit` (#3222)

### Other
- Docker images update (#3166)
- Dependencies update (#3117, #3125, #3193, #3194, #3197, #3198, #3203, #3206, #3211, #3215, #3292)
- CI improvements (#3173, #3176, #3183, #3190, #3307)
- Document dynamic domains (#3242, #3245)
- Restructure the documentation (#3259, #3288)
- Update migration guide with dynamic domains changes (#3234)
- Small documentation improvements (#3105, #3162, #3102, #3180, #3114, #3290, #3300, #3303, #3306, #3308, #3312)

## Commits, merged PRs and closed issues
- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A5.0.0)

- [List of closed issues](https://github.com/esl/MongooseIM/issues?q=is%3Aissue+is%3Aclosed+closed%3A2021-04-21..2021-10-07)

- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2021-04-21&to=2021-10-07&type=c)

[List of merged PRs based on merge date]: # (https://github.com/esl/MongooseIM/pulls?q=is%3Apr+base%3Amaster+merged%3A%222021-04-21..2021-10-07%22+sort%3Acreated-asc+)

## Special thanks to our contributors:
- [@imcyee](https://github.com/imcyee) Add thread and thread parent to client api messages (#3236)

# [MongooseIM 4.2.0](https://github.com/esl/MongooseIM/releases/tag/4.2.0) - 2021-04-20

## Highlights

- Prepared queries introduced
- Inbox extensions
- Miscellaneous enhancements & improvements

## All changes

### Added
- Documentation for `mod_offline.store_groupchat_messages` option (#2992)
- Support of MS SQL and MySQL in `mod_auth_token` (#3059)
- Inbox extensions to set entries as archived, muted or read (#3067)
- `mod_domain_isolation` module to limit message passing between domains (#3070)

### Removed
- Usage of deprecated `http_uri` in `ejabberd_auth_http` and `mod_muc_room` modules (#3026)
- Config reload functionality with the flat config format (#3030)
- Outdated and unsupported `azuresql.sql` file (#3086)

### Changed
- Usage of maps instead of lists in session management (#3018)
- Improved timestamp logic (#3031)
- Prepared queries for MS SQL, MySQL and PostgreSQL (#3039, #3050, #3055, #3059, #3060, #3066, #3074, #3078)
- Improved pipeline for `mod_smart_markers` (#3068)
- Unified checks for chat markers (#3080)

### Fixed
- Creation of `modMucMamFlushed` metric (#3023)
- Starting backend containers for Mac OS X (#3033)
- Minor issues with GitHub Actions (#3045)
- Crash during session handover (#3056)
- Type declaration in `mod_muc_light.hrl` (#3057)
- Test summary is no longer printed when CT directory is not created (#3069)
- Explicit `preset_not_found` error when running tests with nonexistent preset (#3072)
- Set `mod_mam_meta.pm.archive_groupchats` to `false` by default (#3082)

### Other
- Dependencies update (#3036, #3077)
- Minor documentation improvements (#3043, #3046, #3079, #3087)

## Commits, merged PRs and closed issues
- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A4.2.0)

- [List of closed issues](https://github.com/esl/MongooseIM/issues?q=is%3Aissue+is%3Aclosed+closed%3A2021-02-04..2021-04-20+)

- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2021-02-04&to=2021-04-20&type=c)

[List of merged PRs based on merge date]: # (https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222021-02-04..2021-04-20%22%20sort%3Acreated-asc%20)

## Special thanks to our contributors:
- Inbox extensions are sponsored by [Beekeeper](https://www.beekeeper.io/)
- [@antivista](https://github.com/antivista) For the documentation supplement (#2992)
- [@imcyee](https://github.com/imcyee) For fixing handover crash (#3056)

# [MongooseIM 4.1.0](https://github.com/esl/MongooseIM/releases/tag/4.1.0) - 2021-02-02

## Highlights

- Major configuration rework: the TOML format is now the only one supported
- Documentation enhancements
- Performance improvements

## All changes

### Added
- Full support of TOML configuration format (#2929)
- Prepared queries for `mod_vcard` (#2939)
- [Humio](https://www.humio.com/) support (#2952)
- GitHub actions for MongooseIM CI workflow (#2990, #2993)
- Usage of `fast_scram` library (#3003)
- Documentation for MongooseIM cluster's rolling upgrade (#3012)
- PEP publish options (#3017)

### Removed
- Support of old `*.cfg` configuration format (#2929)
- Support of HTTP File Upload 0.2.5 (#2989)
- Unused `katt_helper` (#2999)

### Changed
- Moved documentation from readthedocs to gh-pages altogether with its face lifting (#2946, #2960, #2963, #2966, #2969)
- Helper script in small tests uses python3 instead of python2 (#2957)
- Use `tools/wait_for_service.sh` instead of `netcat` to ensure the main `minio` container is started (#2979)
- Reduced stringprepping in roster hooks, privacy modules and `mod_offline` (#2997, #3005, #3009)

### Fixed
- The response of locked MUC room to `disco#info` (#2956)
- `ct_mongoose_log_hook` initialization error (#2964)
- Catching log formatter errors to avoid death spiral of logging (#2968, #2978)
- The way big tests detect minio in CI (#2998)
- Connection issues with Cassandra (#3006)

### Other
- Added `rebar3 clean` call to `Makefile` (#2932)
- Minor docs improvements (#2945, #2981, #3000, #3013, #3020)
- Parallel loading of `nksip` in tests (#2947)
- CI improvements (#2949, #2972)
- Usage of `integer_to_binary/1` instead of `list_to_integer(binary_to_list/1)` (#3008)
- Removed `archive_groupchats` warning (#3016)

## Commits, merged PRs and closed issues
- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A4.1.0)

- [List of closed issues](https://github.com/esl/MongooseIM/issues?q=is%3Aissue+is%3Aclosed+closed%3A2020-11-17..2021-02-03+)

- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2020-11-17&to=2021-02-03&type=c)

[List of merged PRs based on merge date]: # (https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222020-11-17..2021-02-03%22%20sort%3Acreated-asc%20)


# [MongooseIM 4.0.1](https://github.com/esl/MongooseIM/releases/tag/4.0.1) - 2020-11-17

## Highlights
- A new metric now reports the type of configuration file that is being used. The aim is to determine the adoption of the new config file format.
- Better error messages are reported from wrong TOML configuration files.

## All changes

### Added
- User-friendly errors for the TOML configuration file (#2903)
- Metric to report the type of config file used (#2918)

### Removed
- Removed deprecated `mod_http_notifications` #2912

### Changed
- TOML documentation improvements (#2896, #2898, #2899, #2901, #2905, #2942)
- Some TOML configuration options have been reformatted (#2909)
- More prepared queries to improve RDBMS performance (#2924, #2928)
- Fixed Ubuntu 18.04 and OTP 23.1 for building docker images (#2926)
- Moving from untyped binaries to proper `jid` structures (#2895, #2920, #2922)

### Other
- Dependencies update (#2914)
- REST error handling cleanup (#2908)
- `nksip` is started only when used (#2937)
- RPM package improvements (#2906)
- CI improvements (#2910, #2934)

## Commits, merged PRs and closed issues
- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A4.0.1)

[List of merged PRs based on merge date]: # (https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222020-09-30..2020-11-13%22%20sort%3Acreated-asc%20)


# [MongooseIM 4.0.0](https://github.com/esl/MongooseIM/releases/tag/4.0.0) - 2020-09-30
## Overview
MongooseIM 4.0 is all about putting a friendly face to MongooseIM’s amazing features.
This includes the addition of an Erlang-agnostic configuration allowing a broader pool of developers to benefit from MongooseIM.
Logging and Kubernetes improvements to be more DevOps-friendly and Manager-friendly with load testing enabling managers to see the benefits of MongooseIM easily.

## All changes

### Added
* A new configuration file format: MongooseIM can now be configured with an entirely revamped [TOML](https://github.com/toml-lang/toml) configuration file (#2801)
  * Pesky bugs have been fixed
  * Implementation details have been abstracted away
  * Documentation has been improved
* Structured logging: the main idea of structured logging is that if we have properties of an event, let’s log them as they are, without losing their structure, and format it in a way that is most appropriate for the system we’re sending them to.
  * Moved to [OTP logger](https://erlang.org/doc/man/logger.html) from [lager](https://github.com/erlang-lager/lager) (#2810)
  * No more logging of man-made strings, we now log structured reports (#2816)
  * `logfmt` formatter, a log format that's easy to read and write (#2848)
  * `JSON` formatter, a fully structured format (#2851)
* [XEP-0215](https://xmpp.org/extensions/xep-0215.html) External Service Discovery (#2870)
* A REST endpoint for admin users which accepts any stanza, provided it has `from` and `to` attributes (#2858)
* `erl_crash.dump` dumping directory can be explicitly set (#2793)

### Changed
* Archiving messages takes input as a map instead of as many parameters (#2749)
* Base16 encoding uses a faster algorithm (#2839)

### Other
* OTP-23 compatible (#2840)
* SCRAM password dumping into RDBMS escapes username correctly (#2842)

## Commits, merged PRs and closed issues
- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A4.0.0)
- [List of closed issues](https://github.com/esl/MongooseIM/issues?q=is%3Aissue+is%3Aclosed+closed%3A2020-05-23..2020-09-30+)
- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2020-05-23&to=2020-09-30&type=c)

[List of merged PRs based on merge date]: # (https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222020-05-23..2020-09-30%22%20sort%3Acreated-asc%20)

## Special thanks to our contributors:
- [@balgillo](https://github.com/balgillo) For the small but many important fixes.

# [MongooseIM 3.7.1](https://github.com/esl/MongooseIM/releases/tag/3.7.1) - 2020-07-13

## Highlights
- Integration of the highly performant NIFs to compute the SCRAM challenge from [esl/fast_scram](https://github.com/esl/fast_scram/)

## All changes

### Changed
- Implementation of the SCRAM challenge algorithm to be NIF-based (#2787)
- Fix a bug during session resumption, where stanzas were not routed to the user (#2780)
- Fix AMP logic regarding storage of messages when delivery failed but archiving was successful (#2757)

### Other
- Update Rebar and its plugins (#2760)
- Update dependencies (#2761) (#2764) (#2765)

## Commits, merged PRs and closed issues
- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A3.7.1)
- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2020-05-23&to=2020-07-12&type=c)

[List of merged PRs based on merge date]: # (https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222020-05-23..2020-07-12%22%20sort%3Acreated-asc%20)

# [MongooseIM 3.7.0](https://github.com/esl/MongooseIM/releases/tag/3.7.0) - 2020-05-20

## Highlights

- Authentication methods improvements
    - Support for `SCRAM-SHA-256`, `SCRAM-SHA-512`, `SCRAM-SHA-224`, `SCRAM-SHA-384` authentication methods
    - Support for channel binding for all the SCRAM based authentication methods
- Support for [XEP-0424: Message Retraction][XEP-0424]
- Support for [Proxy Protocol]

There were some changes to the database schema so please take a look at the [Migration guide](doc/migrations/3.6.0_3.7.0.md) when upgrading from a previous version.

## All changes

### Added

- Support for new `SCRAM` based authentication methods:
    - `SCRAM-SHA-256` (#2685)
    - `SCRAM-SHA-[224, 384, 512]` (#2701, #2713)
    - `SCRAM-SHA-*-PLUS` (#2725)
- Support for [XEP-0424: Message Retraction][XEP-0424] (#2739)
- Support for [Proxy Protocol] (#2674)
- RDBMS backend for MUC (#2635)
- Possibility to store MUC messages in offline storage (#2640)
- Support for MySQL's new authentication methods (#2644)
- System metrics
   - Type of outgoing pools (#2657)
   - Stanza count (#2672)
- Chat markers cache (#2676)
- `mongooseimctl bootstrap` command which can be run to execute user defined init scripts (#2692)
   - Support for templating the config files before MongooseIM starts (#2712)
- `c2s_remote_hook_call` to run a hook in the context of the c2s process (#2700)
- `mongooseimctl http_upload` command to help debug HTTP file upload configuration (#2708)

### Changed

- All hooks have been wrapped in the `mongooseim_hooks` module with proper specs (#2642)
- Stream error reporting when the server receives unexpected stanza on a given stream state (#2667)
- Server sent ping scalability improvements (#2531)
- Docker base image to be based on Ubuntu 18.04 with OpenSSL 1.1 (#2693)
- HTTP file upload tests with min.io running in the container (#2696)
- Fixed issue related to passing `x-amz-acl` header to S3 (#2708)
- Default format for password storage (#2737)

### Removed

- Support for MySQL versions older then `5.7.9`

### Other

- Fix to push notifications integration making sure all parameters are sent in a request to MongoosePush (#2645)
- Extract `jid` related functionality to an external library (#2654)


## Commits, merged PRs and closed issues

- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A3.7.0)
- [List of closed issues](https://github.com/esl/MongooseIM/issues?q=is%3Aissue+is%3Aclosed+closed%3A2020-01-29..2020-05-20+)
- [Repository history for this release](https://github.com/esl/MongooseIM/graphs/contributors?from=2020-01-30&to=2020-05-20&type=c)

[List of merged PRs based on merge date]: # (https://github.com/esl/MongooseIM/pulls?utf8=%E2%9C%93&q=is%3Apr%20base%3Amaster%20merged%3A%222020-01-30..2020-05-20%22%20sort%3Acreated-asc%20)

## Special thanks to our contributors:
- [@Neustradamus](https://github.com/Neustradamus) For pushing us with the extension of SCRAM authentication methods and the small PR #2719
- [@alishir](https://github.com/alishir) #2705
- [@jasl](https://github.com/jasl) #2697
- [@tomaszwojcikowski](https://github.com/tomaszwojcikowski) #2690

[XEP-0424]: https://xmpp.org/extensions/xep-0424.html
[Proxy Protocol]: https://www.haproxy.org/download/1.8/doc/proxy-protocol.txt

# [MongooseIM 3.6.2](https://github.com/esl/MongooseIM/releases/tag/3.6.2) - 2020-02-20

## Highlights

- Tooling and packages improvements

## All changes

### Changed

- Fix a bug in mongooseimctl run as a different user as the current one (#2631)
- .deb and .rpm build and test scripts improvements (#2629, #2633)

## Commits, merged PRs and closed issues

- [List of merged PRs](https://github.com/esl/MongooseIM/pulls?q=is%3Apr+is%3Amerged+milestone%3A3.6.2)
- [List of closed issues](https://github.com/esl/MongooseIM/issues?utf8=%E2%9C%93&q=is%3Aissue+is%3Aclosed+closed%3A%222020-02-12..2020-02-20%22+)


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
