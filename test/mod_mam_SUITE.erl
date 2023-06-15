-module(mod_mam_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-import(config_parser_helper, [mod_config/2, default_mod_config/1, default_config/1, config/2]).

all() -> [
          overrides_general_options,
          sets_rdbms_as_default_backend,
          handles_only_pm,
          handles_only_muc,
          disables_sync_writer_on_async_writer,
          disables_sync_muc_writer_on_async_writer,
          produces_valid_configurations,
          handles_cassandra_config,
          example_muc_only_no_pref_good_performance,
          example_pm_only_good_performance
         ].

%% Tests

init_per_testcase(_, Config) -> Config.

end_per_testcase(_CaseName, Config) -> Config.

overrides_general_options(_Config) ->
    Deps = deps(#{backend => rdbms,
                  pm => config([modules, mod_mam, pm], #{backend => cassandra}),
                  muc => default_config([modules, mod_mam, muc])
                 }),
    ?assert(lists:keymember(mod_mam_cassandra_arch, 1, Deps)),
    ?assert(lists:keymember(mod_mam_muc_rdbms_arch, 1, Deps)),
    ?assertNot(lists:keymember(mod_mam_rdbms_arch, 1, Deps)).

sets_rdbms_as_default_backend(_Config) ->
    Deps = deps(#{pm => default_config([modules, mod_mam, pm])}),
    ?assert(lists:keymember(mod_mam_rdbms_arch, 1, Deps)).

handles_only_pm(_Config) ->
    Deps = deps(#{pm => default_config([modules, mod_mam, pm])}),
    ?assert(lists:keymember(mod_mam_pm, 1, Deps)),
    ?assertNot(lists:keymember(mod_mam_muc, 1, Deps)).

handles_only_muc(_Config) ->
    Deps = deps(#{muc => default_config([modules, mod_mam, muc])}),
    ?assertNot(lists:keymember(mod_mam_pm, 1, Deps)),
    ?assert(lists:keymember(mod_mam_muc, 1, Deps)).

disables_sync_writer_on_async_writer(_Config) ->
    PM = default_config([modules, mod_mam, pm]),
    Deps = deps(#{pm => PM}),
    check_equal_opts(mod_mam_rdbms_arch, mod_config(mod_mam_rdbms_arch, #{no_writer => true}), Deps).

disables_sync_muc_writer_on_async_writer(_Config) ->
    MUC = default_config([modules, mod_mam, muc]),
    Deps = deps(#{muc => MUC}),
    check_equal_opts(mod_mam_muc_rdbms_arch, mod_config(mod_mam_muc_rdbms_arch, #{no_writer => true}), Deps).

produces_valid_configurations(_Config) ->
    AsyncOpts = default_config([modules, mod_mam, async_writer]),
    PMCoreOpts = #{archive_groupchats => true,
                   async_writer => AsyncOpts#{enabled => false}},
    PM = config([modules, mod_mam, pm], PMCoreOpts#{user_prefs_store => rdbms}),
    MUCCoreOpts = #{host => <<"host">>},
    MUCArchOpts = #{db_message_format => mam_message_xml},
    MUC = config([modules, mod_mam, muc],
                 maps:merge(MUCCoreOpts, MUCArchOpts#{user_prefs_store => mnesia})),
    Deps = deps(#{pm => PM, muc => MUC}),
    Cache = default_config([modules, mod_mam, cache]),

    check_equal_opts(mod_mam_pm, mod_config(mod_mam_pm, PMCoreOpts), Deps),
    check_equal_opts(mod_mam_muc, mod_config(mod_mam_muc, MUCCoreOpts), Deps),
    check_equal_opts(mod_mam_rdbms_arch, default_mod_config(mod_mam_rdbms_arch), Deps),
    check_equal_opts(mod_mam_muc_rdbms_arch, mod_config(mod_mam_muc_rdbms_arch,
                                                        MUCArchOpts#{no_writer => true}), Deps),
    check_equal_opts(mod_mam_rdbms_user, #{pm => true, muc => true}, Deps),
    check_equal_opts(mod_mam_cache_user, Cache#{pm => true, muc => true}, Deps),
    check_equal_opts(mod_mam_mnesia_prefs, #{muc => true}, Deps),
    check_equal_opts(mod_mam_rdbms_prefs, #{pm => true}, Deps),
    check_equal_opts(mod_mam_muc_rdbms_arch_async, AsyncOpts, Deps).

handles_cassandra_config(_Config) ->
    PM = config([modules, mod_mam, pm], #{user_prefs_store => cassandra,
                                              db_message_format => some_format}),
    MUC = config([modules, mod_mam, muc], #{user_prefs_store => mnesia}),
    Deps = deps(#{backend => cassandra,
                  pm => config([modules, mod_mam, pm], PM),
                  muc => config([modules, mod_mam, muc], MUC)}),

    check_equal_opts(mod_mam_mnesia_prefs, #{muc => true}, Deps),
    check_equal_opts(mod_mam_cassandra_prefs, #{pm => true}, Deps),
    check_equal_opts(mod_mam_cassandra_arch, #{db_message_format => some_format}, Deps),
    check_equal_opts(mod_mam_muc_cassandra_arch, #{db_message_format => mam_message_xml}, Deps).

example_muc_only_no_pref_good_performance(_Config) ->
    MUCOpts = #{host => {prefix, "muc."}},
    MUC = config([modules, mod_mam, muc], MUCOpts),
    Deps = deps(#{muc => MUC}),
    AsyncOpts = default_config([modules, mod_mam, async_writer]),
    Cache = default_config([modules, mod_mam, cache]),

    check_equal_deps(
      [{mod_mam_rdbms_user, #{muc => true, pm => true}},
       {mod_mam_cache_user, Cache#{muc => true}},
       {mod_mam_muc_rdbms_arch, mod_config(mod_mam_muc_rdbms_arch, #{no_writer => true})},
       {mod_mam_muc_rdbms_arch_async, AsyncOpts},
       {mod_mam_muc, mod_config(mod_mam_muc, mod_config(mod_mam_muc, MUCOpts))}
      ], Deps).

example_pm_only_good_performance(_Config) ->
    PM = default_config([modules, mod_mam, pm]),
    Deps = deps(#{pm => PM, user_prefs_store => mnesia}),
    AsyncOpts = default_config([modules, mod_mam, async_writer]),
    Cache = default_config([modules, mod_mam, cache]),

    check_equal_deps(
      [{mod_mam_rdbms_user, #{pm => true}},
       {mod_mam_cache_user, Cache#{pm => true}},
       {mod_mam_mnesia_prefs, #{pm => true}},
       {mod_mam_rdbms_arch, mod_config(mod_mam_rdbms_arch, #{no_writer => true})},
       {mod_mam_rdbms_arch_async, AsyncOpts},
       {mod_mam_pm, default_mod_config(mod_mam_pm)}
      ], Deps).

%% Helpers

check_equal_deps(Expected, Actual) ->
    ?assertEqual(lists:sort([{Mod, Opts, hard} || {Mod, Opts} <- Expected]), lists:sort(Actual)).

check_equal_opts(Mod, Opts, Deps) ->
    {_, ActualOpts, _} = lists:keyfind(Mod, 1, Deps),
    ?assertEqual(Opts, ActualOpts).

deps(Opts) ->
    mod_mam:deps(<<"host">>, mod_config(mod_mam, Opts)).
