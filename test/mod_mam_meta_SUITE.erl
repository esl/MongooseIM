-module(mod_mam_meta_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [
          overrides_general_options,
          sets_rdbms_as_default_backend,
          assumes_pm_by_default,
          handles_disabled_pm,
          disables_sync_writer_on_async_writer,
          disables_sync_muc_writer_on_async_writer,
          produces_valid_configurations,
          handles_riak_config,
          handles_cassandra_config,
          example_muc_only_no_pref_good_performance,
          example_pm_only_good_performance,
          get_mam_module_configuration
         ].

%% Tests

init_per_testcase(get_mam_module_configuration, Config) ->
    meck_config(),
    Config;
init_per_testcase(_, Config) -> Config.

end_per_testcase(get_mam_module_configuration, Config) ->
    meck_cleanup(),
    Config;
end_per_testcase(_CaseName, Config) -> Config.

overrides_general_options(_Config) ->
    Deps = deps([{backend, rdbms}, {pm, [{backend, cassandra}]}, {muc, []}]),

    ?assert(lists:keymember(mod_mam_cassandra_arch, 1, Deps)),
    ?assert(lists:keymember(mod_mam_muc_rdbms_arch, 1, Deps)),
    ?assertNot(lists:keymember(mod_mam_rdbms_arch, 1, Deps)).


sets_rdbms_as_default_backend(_Config) ->
    Deps = deps([{pm, []}]),
    ?assert(lists:keymember(mod_mam_rdbms_arch, 1, Deps)).


assumes_pm_by_default(_Config) ->
    Deps = deps([]),
    ?assert(lists:keymember(mod_mam, 1, Deps)).


handles_disabled_pm(_Config) ->
    Deps = deps([{pm, false}, {muc, []}]),
    ?assertNot(lists:keymember(mod_mam, 1, Deps)).


disables_sync_writer_on_async_writer(_Config) ->
    Deps = deps([{pm, [async_writer]}]),
    {_, Args, _} = lists:keyfind(mod_mam_rdbms_arch, 1, Deps),
    ?assert(lists:member(no_writer, Args)).


disables_sync_muc_writer_on_async_writer(_Config) ->
    Deps = deps([{pm, false}, {muc, [async_writer]}]),
    {_, Args, _} = lists:keyfind(mod_mam_muc_rdbms_arch, 1, Deps),
    ?assert(lists:member(no_writer, Args)).


produces_valid_configurations(_Config) ->
    Deps = deps([
                 {backend, rdbms},
                 cache_users,

                 {pm, [{user_prefs_store, rdbms}, archive_groupchats, {async_writer, false}]},
                 {muc, [
                        {host, <<"host">>},
                        {rdbms_message_format, simple},
                        {user_prefs_store, mnesia}
                       ]}
                ]),

    ExpandedSimpleOpts = [{db_jid_format, mam_jid_rfc}, {db_message_format, mam_message_xml}],

    check_has_args(mod_mam, [{archive_groupchats, true}], Deps),
    check_has_args(mod_mam_muc, [{host, <<"host">>}], Deps),
    check_has_args(mod_mam_rdbms_arch, [pm], Deps),
    check_has_args(mod_mam_muc_rdbms_arch, [no_writer | ExpandedSimpleOpts], Deps),
    check_has_args(mod_mam_rdbms_user, [pm, muc], Deps),
    check_has_args(mod_mam_cache_user, [pm, muc], Deps),
    check_has_args(mod_mam_mnesia_prefs, [muc], Deps),
    check_has_args(mod_mam_rdbms_prefs, [pm], Deps),
    check_has_args(mod_mam_muc_rdbms_async_pool_writer, [], Deps),

    check_has_no_args(mod_mam_rdbms_arch, [muc, no_writer | ExpandedSimpleOpts], Deps),
    check_has_no_args(mod_mam_mnesia_prefs, [pm], Deps),
    check_has_no_args(mod_mam_rdbms_prefs, [muc], Deps),
    ?assertNot(lists:keymember(mod_mam_rdbms_async_pool_writer, 1, Deps)).


handles_riak_config(_Config) ->
    Deps = deps([
                 {backend, riak},
                 {db_message_format, some_format},
                 {pm, [{user_prefs_store, mnesia}]},
                 {muc, []}
                ]),

    ?assert(lists:keymember(mod_mam, 1, Deps)),
    ?assert(lists:keymember(mod_mam_muc, 1, Deps)),
    check_has_args(mod_mam_riak_timed_arch_yz, [pm, muc], Deps),
    check_has_args(mod_mam_riak_timed_arch_yz, [{db_message_format, some_format}], Deps),
    check_has_args(mod_mam_mnesia_prefs, [pm], Deps),
    check_has_no_args(mod_mam_mnesia_prefs, [muc], Deps).


handles_cassandra_config(_Config) ->
    Deps = deps([
                 {backend, cassandra},
                 simple,
                 {pm, [{user_prefs_store, cassandra}, {db_message_format, some_format}]},
                 {muc, [{user_prefs_store, mnesia}, {pool_name, some_poolname}]}
                ]),

    check_has_args(mod_mam_mnesia_prefs, [muc], Deps),
    check_has_args(mod_mam_cassandra_prefs, [pm], Deps),
    check_has_args(mod_mam_cassandra_arch, [{db_message_format, some_format}, {simple, true}], Deps),
    check_has_args(mod_mam_muc_cassandra_arch, [{pool_name, some_poolname}, {simple, true}], Deps),
    check_has_no_args(mod_mam_cassandra_arch, [{pool_name, some_poolname}], Deps),
    check_has_no_args(mod_mam_muc_cassandra_arch, [{db_message_format, some_format}], Deps).


example_muc_only_no_pref_good_performance(_Config) ->
    Deps = deps([
                 cache_users,
                 async_writer,
                 {pm, false},
                 {muc, [{host, "muc.@HOST@"}]}
                ]),

    check_equal_deps([
                      {mod_mam_rdbms_user, [muc, pm]},
                      {mod_mam_cache_user, [muc]},
                      %% 'muc' argument is ignored by the module
                      {mod_mam_muc_rdbms_arch, [muc, no_writer]},
                      %% 'muc' argument is ignored by the module
                      {mod_mam_muc_rdbms_async_pool_writer, [muc]},
                      {mod_mam_muc, [{host, "muc.@HOST@"}]}
                     ], Deps).


example_pm_only_good_performance(_Config) ->
    Deps = deps([
                 cache_users,
                 async_writer,
                 {user_prefs_store, mnesia}
                ]),

    check_equal_deps([
                      {mod_mam_rdbms_user, [pm]},
                      {mod_mam_cache_user, [pm]},
                      {mod_mam_mnesia_prefs, [pm]},
                      {mod_mam_rdbms_arch, [pm, no_writer]},
                      {mod_mam_rdbms_async_pool_writer, [pm]},
                      {mod_mam, []}
                     ], Deps).

get_mam_module_configuration(_Config) ->
    %% see mocked values at meck_config/0
    ?assertEqual(unique_value_1,
                 get_config(<<"no_config">>, mod_mam, unique_value_1)),
    ?assertEqual([here, is, some, config],
                 get_config(<<"mod_mam_config">>, mod_mam, [])),
    ?assertEqual(unique_value_2,
                 get_config(<<"meta_no_mod_mam_config">>, mod_mam, unique_value_2)),
    ?assertEqual([{archive_groupchats, true}],
                 get_config(<<"meta_valid_mod_mam_config">>, mod_mam, [])).

%% Helpers

meck_config() ->
    meck:new(ejabberd_config),
    meck:expect(ejabberd_config, get_local_option,
                fun(modules, <<"no_config">>) ->
                       [];
                   (modules, <<"mod_mam_config">>) ->
                       [{mod_mam, [here, is, some, config]}];
                   (modules, <<"meta_no_mod_mam_config">>) ->
                       [{mod_mam_meta, [{backend, rdbms},
                                        {muc, []},
                                        {pm, false}]}];
                   (modules, <<"meta_valid_mod_mam_config">>) ->
                       [{mod_mam_meta, [{backend, rdbms},
                                        cache_users,
                                        {pm, [archive_groupchats]}]}]
                end).

meck_cleanup() ->
    meck:validate(ejabberd_config),
    meck:unload(ejabberd_config).

check_equal_deps(A, B) ->
    ?assertEqual(sort_deps(A), sort_deps(B)).


sort_deps(Deps) ->
    lists:map(
      fun
          ({Mod, ArgsOrHardness}) -> {Mod, lists:sort(ArgsOrHardness)};
          ({Mod, Args, _Hardness}) -> {Mod, lists:sort(Args)}
      end,
      lists:keysort(1, Deps)).


check_has_no_args(Mod, Args, Deps) ->
    {_, ActualArgs, _} = lists:keyfind(Mod, 1, Deps),
    ?assertEqual([], ordsets:intersection(
                       ordsets:from_list(Args), ordsets:from_list(ActualArgs))).

check_has_args(Mod, Args, Deps) ->
    {_, ActualArgs, _} = lists:keyfind(Mod, 1, Deps),
    ?assert(ordsets:is_subset(
              ordsets:from_list(Args), ordsets:from_list(ActualArgs))).

get_config(Host, Mod, Def) ->
    mod_mam_meta:get_mam_module_configuration(Host, Mod, Def).

deps(Args) ->
    mod_mam_meta:deps(<<"host">>, Args).
