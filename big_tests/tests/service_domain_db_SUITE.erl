-module(service_domain_db_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).
-import(distributed_helper, [mim/0, mim2/0, mim3/0, require_rpc_nodes/1, rpc/4]).
-import(mongooseimctl_helper, [mongooseimctl/3]).

-import(domain_rest_helper,
        [set_invalid_creds/1,
         set_no_creds/1,
         set_valid_creds/1]).

-import(domain_rest_helper,
        [rest_patch_enabled/3,
         rest_put_domain/3,
         putt_domain_with_custom_body/2,
         rest_select_domain/2,
         rest_delete_domain/3,
         delete_custom/4,
         patch_custom/4]).

-import(domain_rest_helper,
        [start_listener/1,
         stop_listener/1]).

-import(domain_helper, [domain/0]).

-define(INV_PWD, <<"basic auth provided, invalid password">>).
-define(NO_PWD, <<"basic auth is required">>).
-define(UNWANTED_PWD, <<"basic auth provided, but not configured">>).

suite() ->
    require_rpc_nodes([mim, mim2, mim3]).

all() ->
    [
     api_lookup_works,
     api_lookup_not_found,
     api_cannot_insert_static,
     api_cannot_disable_static,
     api_cannot_enable_static,
     api_get_all_static,
     api_get_domains_by_host_type,
     {group, db}
    ].

groups() ->
    [{db, [], db_cases()},
     {rest_with_auth, [], [{group, rest_with_auth_parallel}|rest_cases()]},
     {rest_without_auth, [], [{group, rest_without_auth_parallel}|rest_cases()]},
     {rest_with_auth_parallel, [parallel], rest_auth_cases(true)},
     {rest_without_auth_parallel, [parallel], rest_auth_cases(false)}].

db_cases() -> [
     db_inserted_domain_is_in_db,
     db_inserted_domain_is_in_core,
     db_deleted_domain_from_db,
     db_deleted_domain_fails_with_wrong_host_type,
     db_deleted_domain_from_core,
     db_disabled_domain_is_in_db,
     db_disabled_domain_not_in_core,
     db_reenabled_domain_is_in_db,
     db_reenabled_domain_is_in_core,
     db_can_insert_domain_twice_with_the_same_host_type,
     db_cannot_insert_domain_twice_with_another_host_type,
     db_cannot_insert_domain_with_unknown_host_type,
     db_cannot_delete_domain_with_unknown_host_type,
     db_cannot_enable_domain_with_unknown_host_type,
     db_cannot_disable_domain_with_unknown_host_type,
     db_domains_with_unknown_host_type_are_ignored_by_core,
     sql_select_from,
     sql_find_gaps_between,
     db_records_are_restored_on_mim_restart,
     db_record_is_ignored_if_domain_static,
     db_events_table_gets_truncated,
     db_get_all_static,
     db_get_all_dynamic,
     db_could_sync_between_nodes,
     db_deleted_from_one_node_while_service_disabled_on_another,
     db_inserted_from_one_node_while_service_disabled_on_another,
     db_reinserted_from_one_node_while_service_disabled_on_another,
     db_out_of_sync_restarts_service,
     db_crash_on_initial_load_restarts_service,
     db_restarts_properly,
     db_keeps_syncing_after_cluster_join,
     db_gaps_are_getting_filled_automatically,
     db_event_could_appear_with_lower_id,
     cli_can_insert_domain,
     cli_can_disable_domain,
     cli_can_enable_domain,
     cli_can_delete_domain,
     cli_cannot_delete_domain_without_correct_type,
     cli_cannot_insert_domain_twice_with_another_host_type,
     cli_cannot_insert_domain_with_unknown_host_type,
     cli_cannot_delete_domain_with_unknown_host_type,
     cli_cannot_enable_missing_domain,
     cli_cannot_disable_missing_domain,
     cli_cannot_insert_domain_when_it_is_static,
     cli_cannot_delete_domain_when_it_is_static,
     cli_cannot_enable_domain_when_it_is_static,
     cli_cannot_disable_domain_when_it_is_static,
     cli_insert_domain_fails_if_db_fails,
     cli_insert_domain_fails_if_service_disabled,
     cli_delete_domain_fails_if_db_fails,
     cli_delete_domain_fails_if_service_disabled,
     cli_enable_domain_fails_if_db_fails,
     cli_enable_domain_fails_if_service_disabled,
     cli_disable_domain_fails_if_db_fails,
     cli_disable_domain_fails_if_service_disabled,
     {group, rest_with_auth},
     {group, rest_without_auth}].

rest_auth_cases(false) ->
    %% auth provided but not configured:
    [rest_cannot_insert_domain_if_auth_provided_but_not_configured,
     rest_cannot_delete_domain_if_auth_provided_but_not_configured,
     rest_cannot_enable_domain_if_auth_provided_but_not_configured,
     rest_cannot_disable_domain_if_auth_provided_but_not_configured,
     rest_cannot_select_domain_if_auth_provided_but_not_configured];
rest_auth_cases(true) ->
     %% basic auth, but wrong pass:
    [rest_cannot_insert_domain_with_wrong_pass,
     rest_cannot_delete_domain_with_wrong_pass,
     rest_cannot_enable_domain_with_wrong_pass,
     rest_cannot_disable_domain_with_wrong_pass,
     rest_cannot_select_domain_with_wrong_pass,
     %% no basic auth:
     rest_cannot_insert_domain_without_auth,
     rest_cannot_delete_domain_without_auth,
     rest_cannot_enable_domain_without_auth,
     rest_cannot_disable_domain_without_auth,
     rest_cannot_select_domain_without_auth].

rest_cases() ->
    [rest_can_insert_domain,
     rest_can_disable_domain,
     rest_can_delete_domain,
     rest_cannot_delete_domain_without_correct_type,
     rest_delete_missing_domain,
     rest_cannot_insert_domain_twice_with_another_host_type,
     rest_cannot_insert_domain_with_unknown_host_type,
     rest_cannot_delete_domain_with_unknown_host_type,
     rest_cannot_enable_missing_domain,
     rest_cannot_disable_missing_domain,
     rest_can_enable_domain,
     rest_can_select_domain,
     rest_cannot_select_domain_if_domain_not_found,
     rest_cannot_put_domain_without_host_type,
     rest_cannot_put_domain_without_body,
     rest_cannot_put_domain_with_invalid_json,
     rest_cannot_put_domain_when_it_is_static,
     rest_cannot_delete_domain_without_host_type,
     rest_cannot_delete_domain_without_body,
     rest_cannot_delete_domain_with_invalid_json,
     rest_cannot_delete_domain_when_it_is_static,
     rest_cannot_patch_domain_without_enabled_field,
     rest_cannot_patch_domain_without_body,
     rest_cannot_patch_domain_with_invalid_json,
     rest_cannot_enable_domain_when_it_is_static,
     rest_insert_domain_fails_if_db_fails,
     rest_insert_domain_fails_if_service_disabled,
     rest_delete_domain_fails_if_db_fails,
     rest_delete_domain_fails_if_service_disabled,
     rest_enable_domain_fails_if_db_fails,
     rest_enable_domain_fails_if_service_disabled,
     rest_delete_domain_cleans_data_from_mam].

-define(APPS, [inets, crypto, ssl, ranch, cowlib, cowboy]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Conf1 = store_conf(mim()),
    Conf2 = store_conf(mim2()),
    Conf3 = store_conf(mim3()),
    ensure_nodes_know_each_other(),
    service_disabled(mim()),
    service_disabled(mim2()),
    service_disabled(mim3()),
    prepare_test_queries(mim()),
    prepare_test_queries(mim2()),
    erase_database(mim()),
    Config1 = ejabberd_node_utils:init(mim(), Config),
    Config2 = dynamic_modules:save_modules(dummy_auth_host_type(), Config1),
    escalus:init_per_suite([{mim_conf1, Conf1},
                            {mim_conf2, Conf2},
                            {mim_conf3, Conf3},
                            {service_setup, per_testcase} | Config2]).

store_conf(Node) ->
    Loaded = rpc(Node, mongoose_service, is_loaded, [service_domain_db]),
    ServiceOpts = rpc(Node, mongoose_service, get_service_opts, [service_domain_db]),
    CoreOpts = rpc(Node, mongoose_domain_core, get_start_args, []),
    #{loaded => Loaded, service_opts => ServiceOpts, core_opts => CoreOpts}.

end_per_suite(Config) ->
    Conf1 = proplists:get_value(mim_conf1, Config),
    Conf2 = proplists:get_value(mim_conf2, Config),
    Conf3 = proplists:get_value(mim_conf3, Config),
    restore_conf(mim(), Conf1),
    restore_conf(mim2(), Conf2),
    restore_conf(mim3(), Conf3),
    domain_helper:insert_configured_domains(),
    dynamic_modules:restore_modules(Config),
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_group(db, Config) ->
    case mongoose_helper:is_rdbms_enabled(domain()) of
        true -> [{service, true}|Config];
        false -> {skip, require_rdbms}
    end;
init_per_group(rest_with_auth, Config) ->
    start_listener(#{}),
    [{auth_creds, valid}|Config];
init_per_group(rest_without_auth, Config) ->
    start_listener(#{skip_auth => true}),
    Config;
init_per_group(GroupName, Config) ->
    Config1 = save_service_setup_option(GroupName, Config),
    case ?config(service_setup, Config) of
        per_group -> setup_service([], Config1);
        per_testcase -> ok
    end,
    Config1.

end_per_group(rest_with_auth, _Config) ->
    stop_listener(#{});
end_per_group(rest_without_auth, _Config) ->
    stop_listener(#{skip_auth => true});
end_per_group(_GroupName, Config) ->
    case ?config(service_setup, Config) of
        per_group -> teardown_service();
        per_testcase -> ok
    end.

init_per_testcase(db_crash_on_initial_load_restarts_service, Config) ->
    maybe_setup_meck(db_crash_on_initial_load_restarts_service),
    init_with(mim(), [], []),
    Config;
init_per_testcase(TestcaseName, Config) ->
    maybe_setup_meck(TestcaseName),
    case ?config(service_setup, Config) of
        per_group -> ok;
        per_testcase -> setup_service(service_opts(TestcaseName), Config)
    end,
    init_per_testcase2(TestcaseName, Config).

service_opts(db_events_table_gets_truncated) ->
    [{event_cleaning_interval, 1}, {event_max_age, 3}];
service_opts(_) ->
    [].

end_per_testcase(TestcaseName, Config) ->
    end_per_testcase2(TestcaseName, Config),
    case TestcaseName of
        db_out_of_sync_restarts_service ->
            rpc(mim(), sys, resume, [service_domain_db]);
        _ -> ok
    end,
    maybe_teardown_meck(TestcaseName),
    case ?config(service_setup, Config) of
        per_group -> ok;
        per_testcase -> teardown_service()
    end.

init_per_testcase2(TestcaseName, Config)
    when TestcaseName =:= rest_delete_domain_cleans_data_from_mam ->
    HostType = dummy_auth_host_type(),
    Mods = [{mod_mam_meta, [{backend, rdbms}, {pm, []}]}],
    dynamic_modules:ensure_modules(HostType, Mods),
    escalus:init_per_testcase(TestcaseName, Config);
init_per_testcase2(_, Config) ->
    Config.

end_per_testcase2(TestcaseName, Config)
    when TestcaseName =:= rest_delete_domain_cleans_data_from_mam ->
    escalus:end_per_testcase(TestcaseName, Config);
end_per_testcase2(_, Config) ->
    Config.

setup_service(Opts, Config) ->
    ServiceEnabled = proplists:get_value(service, Config, false),
    Pairs1 = [{<<"example.cfg">>, <<"type1">>},
             {<<"erlang-solutions.com">>, <<"type2">>},
             {<<"erlang-solutions.local">>, <<"type2">>}],
    init_with(mim(), Pairs1, host_types_for_mim()),
    init_with(mim2(), [], host_types_for_mim2()),
    case ServiceEnabled of
        true ->
            service_enabled(mim(), Opts),
            service_enabled(mim2(), []);
        false ->
            ok
    end.

host_types_for_mim() ->
    [<<"type1">>, <<"type2">>, dummy_auth_host_type(),
     <<"dbgroup">>, <<"dbgroup2">>, <<"cfggroup">>].

host_types_for_mim2() ->
    [<<"mim2only">> | host_types_for_mim()].

teardown_service() ->
    service_disabled(mim()),
    service_disabled(mim2()),
    erase_database(mim()).

save_service_setup_option(GroupName, Config) ->
    Value = case is_parallel_group(GroupName) of
                true -> per_group;
                false -> per_testcase
            end,
    lists:keystore(service_setup, 1, Config, {service_setup, Value}).

is_parallel_group(GroupName) ->
    case lists:keyfind(GroupName, 1, groups()) of
        {_, Opts, _Cases} -> lists:member(parallel, Opts);
        _ -> false
    end.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

api_lookup_works(_) ->
    {ok, <<"type1">>} = get_host_type(mim(), <<"example.cfg">>).

api_lookup_not_found(_) ->
    {error, not_found} = get_host_type(mim(), <<"example.missing">>).

api_cannot_insert_static(_) ->
    {error, static} = insert_domain(mim(), <<"example.cfg">>, <<"type1">>).

api_cannot_disable_static(_) ->
    {error, static} = disable_domain(mim(), <<"example.cfg">>).

api_cannot_enable_static(_) ->
    {error, static} = enable_domain(mim(), <<"example.cfg">>).

%% See also db_get_all_static
api_get_all_static(_) ->
    %% Could be in any order
    [{<<"erlang-solutions.com">>, <<"type2">>},
     {<<"erlang-solutions.local">>, <<"type2">>},
     {<<"example.cfg">>, <<"type1">>}] =
        lists:sort(get_all_static(mim())).

api_get_domains_by_host_type(_) ->
    [<<"erlang-solutions.com">>, <<"erlang-solutions.local">>] =
        lists:sort(get_domains_by_host_type(mim(), <<"type2">>)),
    [<<"example.cfg">>] = get_domains_by_host_type(mim(), <<"type1">>),
    [] = get_domains_by_host_type(mim(), <<"type6">>).

%% Similar to as api_get_all_static, just with DB service enabled
db_get_all_static(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    sync(),
    %% Could be in any order
    [{<<"erlang-solutions.com">>, <<"type2">>},
     {<<"erlang-solutions.local">>, <<"type2">>},
     {<<"example.cfg">>, <<"type1">>}] =
        lists:sort(get_all_static(mim())).

db_get_all_dynamic(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    ok = insert_domain(mim(), <<"example2.db">>, <<"type1">>),
    sync(),
    [{<<"example.db">>, <<"type1">>},
     {<<"example2.db">>, <<"type1">>}] =
        lists:sort(get_all_dynamic(mim())).

db_inserted_domain_is_in_db(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    {ok, #{host_type := <<"type1">>, enabled := true}} =
       select_domain(mim(), <<"example.db">>).

db_inserted_domain_is_in_core(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    sync(),
    {ok, <<"type1">>} = get_host_type(mim(), <<"example.db">>).

db_deleted_domain_from_db(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    ok = delete_domain(mim(), <<"example.db">>, <<"type1">>),
    {error, not_found} = select_domain(mim(), <<"example.db">>).

db_deleted_domain_fails_with_wrong_host_type(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    {error, wrong_host_type} =
        delete_domain(mim(), <<"example.db">>, <<"type2">>),
    {ok, #{host_type := <<"type1">>, enabled := true}} =
        select_domain(mim(), <<"example.db">>).

db_deleted_domain_from_core(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    sync(),
    ok = delete_domain(mim(), <<"example.db">>, <<"type1">>),
    sync(),
    {error, not_found} = get_host_type(mim(), <<"example.db">>).

db_disabled_domain_is_in_db(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    ok = disable_domain(mim(), <<"example.db">>),
    {ok, #{host_type := <<"type1">>, enabled := false}} =
       select_domain(mim(), <<"example.db">>).

db_disabled_domain_not_in_core(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    ok = disable_domain(mim(), <<"example.db">>),
    sync(),
    {error, not_found} = get_host_type(mim(), <<"example.db">>).

db_reenabled_domain_is_in_db(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    ok = disable_domain(mim(), <<"example.db">>),
    ok = enable_domain(mim(), <<"example.db">>),
    {ok, #{host_type := <<"type1">>, enabled := true}} =
       select_domain(mim(), <<"example.db">>).

db_reenabled_domain_is_in_core(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    ok = disable_domain(mim(), <<"example.db">>),
    ok = enable_domain(mim(), <<"example.db">>),
    sync(),
    {ok, <<"type1">>} = get_host_type(mim(), <<"example.db">>).

db_can_insert_domain_twice_with_the_same_host_type(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>).

db_cannot_insert_domain_twice_with_another_host_type(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    {error, duplicate} = insert_domain(mim(), <<"example.db">>, <<"type2">>).

db_cannot_insert_domain_with_unknown_host_type(_) ->
    {error, unknown_host_type} = insert_domain(mim(), <<"example.db">>, <<"type6">>).

db_cannot_delete_domain_with_unknown_host_type(_) ->
    ok = insert_domain(mim2(), <<"example.db">>, <<"mim2only">>),
    sync(),
    {error, unknown_host_type} = delete_domain(mim(), <<"example.db">>, <<"mim2only">>).

db_cannot_enable_domain_with_unknown_host_type(_) ->
    ok = insert_domain(mim2(), <<"example.db">>, <<"mim2only">>),
    ok = disable_domain(mim2(), <<"example.db">>),
    sync(),
    {error, unknown_host_type} = enable_domain(mim(), <<"example.db">>).

db_cannot_disable_domain_with_unknown_host_type(_) ->
    ok = insert_domain(mim2(), <<"example.db">>, <<"mim2only">>),
    sync(),
    {error, unknown_host_type} = disable_domain(mim(), <<"example.db">>).

db_domains_with_unknown_host_type_are_ignored_by_core(_) ->
    ok = insert_domain(mim2(), <<"example.com">>, <<"mim2only">>),
    ok = insert_domain(mim2(), <<"example.org">>, <<"type1">>),
    sync(),
    {ok, <<"type1">>} = get_host_type(mim(), <<"example.org">>), %% Counter-case
    {error, not_found} = get_host_type(mim(), <<"example.com">>).

sql_select_from(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    [{_, <<"example.db">>, <<"type1">>}] =
       rpc(mim(), mongoose_domain_sql, select_from, [0, 100]).

sql_find_gaps_between(_) ->
    with_service_suspended(fun() ->
            %% Check several ranges
            check_gap_finder(20, 22),
            check_gap_finder(10, 15),
            check_gap_finder(123, 321),
            check_gap_finder(1000, 1300)
        end).

check_gap_finder(From, To) ->
    {updated, 1} = insert_full_event(mim(), From, <<"gap_start">>),
    {updated, 1} = insert_full_event(mim(), To, <<"gap_end">>),
    Expected = lists:seq(From + 1, To - 1),
    Expected = find_gaps_between(From, To).

find_gaps_between(From, To) ->
    rpc(mim(), mongoose_domain_loader, find_gaps_between, [From, To]).

db_records_are_restored_on_mim_restart(_) ->
    ok = insert_domain(mim(), <<"example.com">>, <<"type1">>),
    %% Simulate MIM restart
    service_disabled(mim()),
    init_with(mim(), [], [<<"type1">>]),
    {error, not_found} = get_host_type(mim(), <<"example.com">>),
    service_enabled(mim()),
    %% DB still contains data
    {ok, #{host_type := <<"type1">>, enabled := true}} =
       select_domain(mim(), <<"example.com">>),
    %% Restored
    {ok, <<"type1">>} = get_host_type(mim(), <<"example.com">>).

db_record_is_ignored_if_domain_static(_) ->
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    ok = insert_domain(mim(), <<"example.net">>, <<"dbgroup">>),
    %% Simulate MIM restart
    service_disabled(mim()),
    %% Only one domain is static
    init_with(mim(), [{<<"example.com">>, <<"cfggroup">>}], [<<"dbgroup">>, <<"cfggroup">>]),
    service_enabled(mim()),
    %% DB still contains data
    {ok, #{host_type := <<"dbgroup">>, enabled := true}} =
       select_domain(mim(), <<"example.com">>),
    {ok, #{host_type := <<"dbgroup">>, enabled := true}} =
       select_domain(mim(), <<"example.net">>),
     %% Static DB records are ignored
    {ok, <<"cfggroup">>} = get_host_type(mim(), <<"example.com">>),
    {ok, <<"dbgroup">>} = get_host_type(mim(), <<"example.net">>).

db_events_table_gets_truncated(_) ->
    %% Configure service with a very short interval
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    ok = insert_domain(mim(), <<"example.net">>, <<"dbgroup">>),
    ok = insert_domain(mim(), <<"example.org">>, <<"dbgroup">>),
    ok = insert_domain(mim(), <<"example.beta">>, <<"dbgroup">>),
    Max = get_max_event_id(mim()),
    true = is_integer(Max),
    true = Max > 0,
    %% The events table is not empty and the size of 1, eventually.
    F = fun() -> get_min_event_id(mim()) end,
    mongoose_helper:wait_until(F, Max, #{time_left => timer:seconds(15)}).

db_could_sync_between_nodes(_) ->
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync(),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>).

db_deleted_from_one_node_while_service_disabled_on_another(_) ->
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync(),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>),
    %% Service is disable on the second node
    service_disabled(mim2()),
    %% Removed from the first node
    ok = delete_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync_local(mim()),
    {error, not_found} = get_host_type(mim(), <<"example.com">>),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>),
    %% Sync is working again
    service_enabled(mim2()),
    {error, not_found} = get_host_type(mim2(), <<"example.com">>).

db_inserted_from_one_node_while_service_disabled_on_another(_) ->
    %% Service is disable on the second node
    service_disabled(mim2()),
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    %% Sync is working again
    service_enabled(mim2()),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>).

db_reinserted_from_one_node_while_service_disabled_on_another(_) ->
    %% This test shows the behaviour when someone
    %% reinserts a domain with a different host type.
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync(),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>),
    %% Service is disable on the second node
    service_disabled(mim2()),
    %% Removed from the first node
    ok = delete_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync_local(mim()),
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup2">>),
    sync_local(mim()),
    %% Sync is working again
    service_enabled(mim2()),
    sync(),
    %% A corner case: domain name is reinserted with different host type
    %% while service was down on mim2. check that mim2 is updated
    {ok, <<"dbgroup2">>} = get_host_type(mim(), <<"example.com">>),
    {ok, <<"dbgroup2">>} = get_host_type(mim2(), <<"example.com">>),
    %% check deleting
    ok = delete_domain(mim2(), <<"example.com">>, <<"dbgroup2">>),
    sync(),
    {error, not_found} = get_host_type(mim(), <<"example.com">>),
    {error, not_found} = get_host_type(mim2(), <<"example.com">>).

db_crash_on_initial_load_restarts_service(_) ->
    service_enabled(mim()),
    %% service is restarted
    true = rpc(mim(), meck, wait, [service_domain_db, restart, 0, timer:seconds(1)]) > 0,
    ok.

db_out_of_sync_restarts_service(_) ->
    ok = insert_domain(mim2(), <<"example1.com">>, <<"type1">>),
    ok = insert_domain(mim2(), <<"example2.com">>, <<"type1">>),
    sync(),
    %% Pause processing events on one node
    suspend_service(mim()),
    ok = insert_domain(mim2(), <<"example3.com">>, <<"type1">>),
    ok = insert_domain(mim2(), <<"example4.com">>, <<"type1">>),
    sync_local(mim2()),
    %% Truncate events table, keep only one event
    MaxId = get_max_event_id(mim2()),
    {updated, _} = delete_events_older_than(mim2(), MaxId),
    {error, not_found} = get_host_type(mim(), <<"example3.com">>),
    %% The size of the table is 1
    MaxId = get_min_event_id(mim2()),
    %% Resume processing events on one node
    resume_service(mim()),
    sync(),
    %% Out of sync detected and service is restarted
    true = rpc(mim(), meck, num_calls, [service_domain_db, restart, 0]) > 0,
    ok.

db_restarts_properly(_) ->
    PID = rpc(mim(), erlang, whereis, [service_domain_db]),
    ok = rpc(mim(), service_domain_db, restart, []),
    F = fun() ->
            PID2 = rpc(mim(), erlang, whereis, [service_domain_db]),
            PID2 =/= PID
        end,
    mongoose_helper:wait_until(F, true, #{time_left => timer:seconds(15)}).

db_keeps_syncing_after_cluster_join(Config) ->
    HostType = dummy_auth_host_type(),
    %% GIVING mim1 and mim2 are not clustered.
    %% Ask mim1 to join mim2's cluster
    %% (and mongooseim application gets restarted on mim1)
    leave_cluster(Config),
    service_enabled(mim()),
    ok = insert_domain(mim(), <<"example1.com">>, HostType),
    ok = insert_domain(mim2(), <<"example2.com">>, HostType),
    sync(),
    %% Nodes don't have to be clustered to sync the domains.
    assert_domains_are_equal(HostType),
    %% WHEN Adding mim1 joins into a cluster
    %% (and mongooseim application gets restarted on mim1)
    join_cluster(Config),
    service_enabled(mim()),
    %% THEN Sync is successful
    ok = insert_domain(mim(), <<"example3.com">>, HostType),
    ok = insert_domain(mim2(), <<"example4.com">>, HostType),
    sync(),
    assert_domains_are_equal(HostType).

db_gaps_are_getting_filled_automatically(_Config) ->
    ok = insert_domain(mim(), <<"example.com">>, <<"type1">>),
    sync(),
    Max = get_max_event_id(mim()),
    %% Create a gap in events by manually adding an event
    GapSize = 10,
    {updated, 1} = insert_full_event(mim(), Max + GapSize, <<"something.else">>),
    force_check_for_updates(mim()),
    sync_local(mim()),
    F = fun() -> get_event_ids_between(mim(), Max, Max + GapSize) end,
    mongoose_helper:wait_until(F, lists:seq(Max, Max + GapSize),
                               #{time_left => timer:seconds(15)}).

db_event_could_appear_with_lower_id(_Config) ->
    %% We use 40 and 50 as event ids
    %% We check two things:
    %% - that "lazydom" actually gets loaded
    %% - that gaps get filled
    %% Starting with an empty DB
    null = get_max_event_id(mim()),
    {updated, 1} = insert_domain_settings_without_event(mim(), <<"fastdom">>, <<"type1">>),
    {updated, 1} = insert_full_event(mim(), 50, <<"fastdom">>),
    force_check_for_updates(mim()),
    sync_local(mim()),
    {ok, <<"type1">>} = get_host_type(mim(), <<"fastdom">>),
    %% At this point we've completed the initial load and the DB sync
    %% Now create an event before the first known event
    {updated, 1} = insert_domain_settings_without_event(mim(), <<"lazydom">>, <<"type1">>),
    {updated, 1} = insert_full_event(mim(), 40, <<"lazydom">>),
    force_check_for_updates(mim()),
    sync_local(mim()),
    40 = get_min_event_id(mim()),
    50 = get_max_event_id(mim()),
    %% lazydom gets loaded
    {ok, <<"type1">>} = get_host_type(mim(), <<"lazydom">>),
    %% Check gaps
    F = fun() -> get_event_ids_between(mim(), 40, 50) end,
    mongoose_helper:wait_until(F, lists:seq(40, 50),
                               #{time_left => timer:seconds(15)}).

cli_can_insert_domain(Config) ->
    {"Added\n", 0} =
        mongooseimctl("insert_domain", [<<"example.db">>, <<"type1">>], Config),
    {ok, #{host_type := <<"type1">>, enabled := true}} =
        select_domain(mim(), <<"example.db">>).

cli_can_disable_domain(Config) ->
    mongooseimctl("insert_domain", [<<"example.db">>, <<"type1">>], Config),
    mongooseimctl("disable_domain", [<<"example.db">>], Config),
    {ok, #{host_type := <<"type1">>, enabled := false}} =
        select_domain(mim(), <<"example.db">>).

cli_can_enable_domain(Config) ->
    mongooseimctl("insert_domain", [<<"example.db">>, <<"type1">>], Config),
    mongooseimctl("disable_domain", [<<"example.db">>], Config),
    mongooseimctl("enable_domain", [<<"example.db">>], Config),
    {ok, #{host_type := <<"type1">>, enabled := true}} =
        select_domain(mim(), <<"example.db">>).

cli_can_delete_domain(Config) ->
    mongooseimctl("insert_domain", [<<"example.db">>, <<"type1">>], Config),
    mongooseimctl("delete_domain", [<<"example.db">>, <<"type1">>], Config),
    {error, not_found} = select_domain(mim(), <<"example.db">>).

cli_cannot_delete_domain_without_correct_type(Config) ->
    {"Added\n", 0} =
        mongooseimctl("insert_domain", [<<"example.db">>, <<"type1">>], Config),
    {"Error: \"Wrong host type\"\n", 1} =
        mongooseimctl("delete_domain", [<<"example.db">>, <<"type2">>], Config),
    {ok, _} = select_domain(mim(), <<"example.db">>).

cli_cannot_insert_domain_twice_with_another_host_type(Config) ->
    {"Added\n", 0} =
        mongooseimctl("insert_domain", [<<"example.db">>, <<"type1">>], Config),
    {"Error: \"Domain already exists\"\n", 1} =
        mongooseimctl("insert_domain", [<<"example.db">>, <<"type2">>], Config).

cli_cannot_insert_domain_with_unknown_host_type(Config) ->
    {"Error: \"Unknown host type\"\n", 1} =
        mongooseimctl("insert_domain", [<<"example.db">>, <<"type6">>], Config).

cli_cannot_delete_domain_with_unknown_host_type(Config) ->
    {"Error: \"Unknown host type\"\n", 1} =
        mongooseimctl("delete_domain", [<<"example.db">>, <<"type6">>], Config).

cli_cannot_enable_missing_domain(Config) ->
    {"Error: \"Domain not found\"\n", 1} =
        mongooseimctl("enable_domain", [<<"example.db">>], Config).

cli_cannot_disable_missing_domain(Config) ->
    {"Error: \"Domain not found\"\n", 1} =
        mongooseimctl("disable_domain", [<<"example.db">>], Config).

cli_cannot_insert_domain_when_it_is_static(Config) ->
    {"Error: \"Domain is static\"\n", 1} =
        mongooseimctl("insert_domain", [<<"example.cfg">>, <<"type1">>], Config).

cli_cannot_delete_domain_when_it_is_static(Config) ->
    {"Error: \"Domain is static\"\n", 1} =
        mongooseimctl("delete_domain", [<<"example.cfg">>, <<"type1">>], Config).

cli_cannot_enable_domain_when_it_is_static(Config) ->
    {"Error: \"Domain is static\"\n", 1} =
        mongooseimctl("enable_domain", [<<"example.cfg">>], Config).

cli_cannot_disable_domain_when_it_is_static(Config) ->
    {"Error: \"Domain is static\"\n", 1} =
        mongooseimctl("disable_domain", [<<"example.cfg">>], Config).

cli_insert_domain_fails_if_db_fails(Config) ->
    {"Error: \"Database error\"\n", 1} =
        mongooseimctl("insert_domain", [<<"example.db">>, <<"type1">>], Config).

cli_insert_domain_fails_if_service_disabled(Config) ->
    service_disabled(mim()),
    {"Error: \"Service disabled\"\n", 1} =
        mongooseimctl("insert_domain", [<<"example.db">>, <<"type1">>], Config).

cli_delete_domain_fails_if_db_fails(Config) ->
    {"Error: \"Database error\"\n", 1} =
        mongooseimctl("delete_domain", [<<"example.db">>, <<"type1">>], Config).

cli_delete_domain_fails_if_service_disabled(Config) ->
    service_disabled(mim()),
    {"Error: \"Service disabled\"\n", 1} =
        mongooseimctl("delete_domain", [<<"example.db">>, <<"type1">>], Config).

cli_enable_domain_fails_if_db_fails(Config) ->
    {"Error: \"Database error\"\n", 1} =
        mongooseimctl("enable_domain", [<<"example.db">>], Config).

cli_enable_domain_fails_if_service_disabled(Config) ->
    service_disabled(mim()),
    {"Error: \"Service disabled\"\n", 1} =
        mongooseimctl("enable_domain", [<<"example.db">>], Config).

cli_disable_domain_fails_if_db_fails(Config) ->
    {"Error: \"Database error\"\n", 1} =
        mongooseimctl("disable_domain", [<<"example.db">>], Config).

cli_disable_domain_fails_if_service_disabled(Config) ->
    service_disabled(mim()),
    {"Error: \"Service disabled\"\n", 1} =
        mongooseimctl("disable_domain", [<<"example.db">>], Config).

rest_can_insert_domain(Config) ->
    {{<<"204">>, _}, _} =
        rest_put_domain(Config, <<"example.db">>, <<"type1">>),
    {ok, #{host_type := <<"type1">>, enabled := true}} =
        select_domain(mim(), <<"example.db">>).

rest_can_disable_domain(Config) ->
    rest_put_domain(Config, <<"example.db">>, <<"type1">>),
    rest_patch_enabled(Config, <<"example.db">>, false),
    {ok, #{host_type := <<"type1">>, enabled := false}} =
        select_domain(mim(), <<"example.db">>).

rest_can_delete_domain(Config) ->
    rest_put_domain(Config, <<"example.db">>, <<"type1">>),
    {{<<"204">>, _}, _} =
        rest_delete_domain(Config, <<"example.db">>, <<"type1">>),
    {error, not_found} = select_domain(mim(), <<"example.db">>).

rest_cannot_delete_domain_without_correct_type(Config) ->
    rest_put_domain(Config, <<"example.db">>, <<"type1">>),
    {{<<"403">>, <<"Forbidden">>},
     {[{<<"what">>, <<"wrong host type">>}]}} =
        rest_delete_domain(Config, <<"example.db">>, <<"type2">>),
    {ok, _} = select_domain(mim(), <<"example.db">>).

rest_delete_missing_domain(Config) ->
    {{<<"204">>, _}, _} =
        rest_delete_domain(Config, <<"example.db">>, <<"type1">>).

rest_cannot_enable_missing_domain(Config) ->
    {{<<"404">>, <<"Not Found">>},
     {[{<<"what">>, <<"domain not found">>}]}} =
        rest_patch_enabled(Config, <<"example.db">>, true).

rest_cannot_insert_domain_twice_with_another_host_type(Config) ->
    rest_put_domain(Config, <<"example.db">>, <<"type1">>),
    {{<<"409">>, <<"Conflict">>}, {[{<<"what">>, <<"duplicate">>}]}} =
        rest_put_domain(Config, <<"example.db">>, <<"type2">>).

rest_cannot_insert_domain_with_unknown_host_type(Config) ->
    {{<<"403">>,<<"Forbidden">>}, {[{<<"what">>, <<"unknown host type">>}]}} =
        rest_put_domain(Config, <<"example.db">>, <<"type6">>).

rest_cannot_delete_domain_with_unknown_host_type(Config) ->
    {{<<"403">>,<<"Forbidden">>}, {[{<<"what">>, <<"unknown host type">>}]}} =
        rest_delete_domain(Config, <<"example.db">>, <<"type6">>).

%% auth provided, but not configured:
rest_cannot_insert_domain_if_auth_provided_but_not_configured(Config) ->
    {{<<"403">>,<<"Forbidden">>}, {[{<<"what">>, ?UNWANTED_PWD}]}} =
        rest_put_domain(set_valid_creds(Config), <<"example.db">>, <<"type1">>).

rest_cannot_delete_domain_if_auth_provided_but_not_configured(Config) ->
    {{<<"403">>,<<"Forbidden">>}, {[{<<"what">>, ?UNWANTED_PWD}]}} =
        rest_delete_domain(set_valid_creds(Config), <<"example.db">>, <<"type1">>).

rest_cannot_enable_domain_if_auth_provided_but_not_configured(Config) ->
    {{<<"403">>,<<"Forbidden">>}, {[{<<"what">>, ?UNWANTED_PWD}]}} =
        rest_patch_enabled(set_valid_creds(Config), <<"example.db">>, false).

rest_cannot_disable_domain_if_auth_provided_but_not_configured(Config) ->
    {{<<"403">>,<<"Forbidden">>}, {[{<<"what">>, ?UNWANTED_PWD}]}} =
        rest_patch_enabled(set_valid_creds(Config), <<"example.db">>, false).

rest_cannot_select_domain_if_auth_provided_but_not_configured(Config) ->
    {{<<"403">>, <<"Forbidden">>}, {[{<<"what">>, ?UNWANTED_PWD}]}} =
        rest_select_domain(set_valid_creds(Config), <<"example.db">>).

%% with wrong pass:
rest_cannot_insert_domain_with_wrong_pass(Config) ->
    {{<<"403">>,<<"Forbidden">>}, {[{<<"what">>, ?INV_PWD}]}} =
        rest_put_domain(set_invalid_creds(Config), <<"example.db">>, <<"type1">>).

rest_cannot_delete_domain_with_wrong_pass(Config) ->
    {{<<"403">>,<<"Forbidden">>}, {[{<<"what">>, ?INV_PWD}]}} =
        rest_delete_domain(set_invalid_creds(Config), <<"example.db">>, <<"type1">>).

rest_cannot_enable_domain_with_wrong_pass(Config) ->
    {{<<"403">>,<<"Forbidden">>}, {[{<<"what">>, ?INV_PWD}]}} =
        rest_patch_enabled(set_invalid_creds(Config), <<"example.db">>, true).

rest_cannot_disable_domain_with_wrong_pass(Config) ->
    {{<<"403">>,<<"Forbidden">>}, {[{<<"what">>, ?INV_PWD}]}} =
        rest_patch_enabled(set_invalid_creds(Config), <<"example.db">>, false).

rest_cannot_select_domain_with_wrong_pass(Config) ->
    {{<<"403">>, <<"Forbidden">>}, {[{<<"what">>, ?INV_PWD}]}} =
        rest_select_domain(set_invalid_creds(Config), <<"example.db">>).

%% without auth:
rest_cannot_insert_domain_without_auth(Config) ->
    {{<<"403">>,<<"Forbidden">>}, {[{<<"what">>, ?NO_PWD}]}} =
        rest_put_domain(set_no_creds(Config), <<"example.db">>, <<"type1">>).

rest_cannot_delete_domain_without_auth(Config) ->
    {{<<"403">>,<<"Forbidden">>}, {[{<<"what">>, ?NO_PWD}]}} =
        rest_delete_domain(set_no_creds(Config), <<"example.db">>, <<"type1">>).

rest_cannot_enable_domain_without_auth(Config) ->
    {{<<"403">>,<<"Forbidden">>}, {[{<<"what">>, ?NO_PWD}]}} =
        rest_patch_enabled(set_no_creds(Config), <<"example.db">>, true).

rest_cannot_disable_domain_without_auth(Config) ->
    {{<<"403">>,<<"Forbidden">>}, {[{<<"what">>, ?NO_PWD}]}} =
        rest_patch_enabled(set_no_creds(Config), <<"example.db">>, false).

rest_cannot_select_domain_without_auth(Config) ->
    {{<<"403">>, <<"Forbidden">>}, {[{<<"what">>, ?NO_PWD}]}} =
        rest_select_domain(set_no_creds(Config), <<"example.db">>).


rest_cannot_disable_missing_domain(Config) ->
    {{<<"404">>, <<"Not Found">>},
     {[{<<"what">>, <<"domain not found">>}]}} =
        rest_patch_enabled(Config, <<"example.db">>, false).

rest_can_enable_domain(Config) ->
    rest_put_domain(Config, <<"example.db">>, <<"type1">>),
    rest_patch_enabled(Config, <<"example.db">>, false),
    rest_patch_enabled(Config, <<"example.db">>, true),
    {ok, #{host_type := <<"type1">>, enabled := true}} =
        select_domain(mim(), <<"example.db">>).

rest_can_select_domain(Config) ->
    rest_put_domain(Config, <<"example.db">>, <<"type1">>),
    {{<<"200">>, <<"OK">>},
     {[{<<"host_type">>, <<"type1">>}, {<<"enabled">>, true}]}} =
        rest_select_domain(Config, <<"example.db">>).

rest_cannot_select_domain_if_domain_not_found(Config) ->
    {{<<"404">>, <<"Not Found">>},
     {[{<<"what">>, <<"domain not found">>}]}} =
        rest_select_domain(Config, <<"example.db">>).

rest_cannot_put_domain_without_host_type(Config) ->
    {{<<"400">>, <<"Bad Request">>},
     {[{<<"what">>, <<"'host_type' field is missing">>}]}} =
        putt_domain_with_custom_body(Config, #{}).

rest_cannot_put_domain_without_body(Config) ->
    {{<<"400">>,<<"Bad Request">>},
     {[{<<"what">>,<<"body is empty">>}]}} =
        putt_domain_with_custom_body(Config, <<>>).

rest_cannot_put_domain_with_invalid_json(Config) ->
    {{<<"400">>,<<"Bad Request">>},
     {[{<<"what">>,<<"failed to parse JSON">>}]}} =
        putt_domain_with_custom_body(Config, <<"{kek">>).

rest_cannot_put_domain_when_it_is_static(Config) ->
    {{<<"403">>, <<"Forbidden">>},
     {[{<<"what">>, <<"domain is static">>}]}} =
        rest_put_domain(Config, <<"example.cfg">>, <<"type1">>).

rest_cannot_delete_domain_without_host_type(Config) ->
    {{<<"400">>, <<"Bad Request">>},
     {[{<<"what">>, <<"'host_type' field is missing">>}]}} =
        delete_custom(Config, admin, <<"/domains/example.db">>, #{}).

rest_cannot_delete_domain_without_body(Config) ->
    {{<<"400">>,<<"Bad Request">>},
     {[{<<"what">>,<<"body is empty">>}]}} =
        delete_custom(Config, admin, <<"/domains/example.db">>, <<>>).

rest_cannot_delete_domain_with_invalid_json(Config) ->
    {{<<"400">>,<<"Bad Request">>},
     {[{<<"what">>,<<"failed to parse JSON">>}]}} =
        delete_custom(Config, admin, <<"/domains/example.db">>, <<"{kek">>).

rest_cannot_delete_domain_when_it_is_static(Config) ->
    {{<<"403">>, <<"Forbidden">>},
     {[{<<"what">>, <<"domain is static">>}]}} =
        rest_delete_domain(Config, <<"example.cfg">>, <<"type1">>).

rest_cannot_patch_domain_without_enabled_field(Config) ->
    {{<<"400">>, <<"Bad Request">>},
     {[{<<"what">>, <<"'enabled' field is missing">>}]}} =
        patch_custom(Config, admin, <<"/domains/example.db">>, #{}).

rest_cannot_patch_domain_without_body(Config) ->
    {{<<"400">>,<<"Bad Request">>},
     {[{<<"what">>,<<"body is empty">>}]}} =
        patch_custom(Config, admin, <<"/domains/example.db">>, <<>>).

rest_cannot_patch_domain_with_invalid_json(Config) ->
    {{<<"400">>,<<"Bad Request">>},
     {[{<<"what">>,<<"failed to parse JSON">>}]}} =
        patch_custom(Config, admin, <<"/domains/example.db">>, <<"{kek">>).

%% SQL query is mocked to fail
rest_insert_domain_fails_if_db_fails(Config) ->
    {{<<"500">>, <<"Internal Server Error">>},
     {[{<<"what">>, <<"database error">>}]}} =
        rest_put_domain(Config, <<"example.db">>, <<"type1">>).

rest_insert_domain_fails_if_service_disabled(Config) ->
    service_disabled(mim()),
    {{<<"403">>, <<"Forbidden">>},
     {[{<<"what">>, <<"service disabled">>}]}} =
        rest_put_domain(Config, <<"example.db">>, <<"type1">>).

%% SQL query is mocked to fail
rest_delete_domain_fails_if_db_fails(Config) ->
    {{<<"500">>, <<"Internal Server Error">>},
     {[{<<"what">>, <<"database error">>}]}} =
        rest_delete_domain(Config, <<"example.db">>, <<"type1">>).

rest_delete_domain_fails_if_service_disabled(Config) ->
    service_disabled(mim()),
    {{<<"403">>, <<"Forbidden">>},
     {[{<<"what">>, <<"service disabled">>}]}} =
        rest_delete_domain(Config, <<"example.db">>, <<"type1">>).

%% SQL query is mocked to fail
rest_enable_domain_fails_if_db_fails(Config) ->
    {{<<"500">>, <<"Internal Server Error">>},
     {[{<<"what">>, <<"database error">>}]}} =
        rest_patch_enabled(Config, <<"example.db">>, true).

rest_enable_domain_fails_if_service_disabled(Config) ->
    service_disabled(mim()),
    {{<<"403">>, <<"Forbidden">>},
     {[{<<"what">>, <<"service disabled">>}]}} =
        rest_patch_enabled(Config, <<"example.db">>, true).

rest_cannot_enable_domain_when_it_is_static(Config) ->
    {{<<"403">>, <<"Forbidden">>},
     {[{<<"what">>, <<"domain is static">>}]}} =
        rest_patch_enabled(Config, <<"example.cfg">>, true).

rest_delete_domain_cleans_data_from_mam(Config) ->
    HostType = dummy_auth_host_type(),
    rest_put_domain(Config, <<"example.com">>, HostType), %% alice3
    rest_put_domain(Config, <<"example.org">>, HostType), %% bob3
    sync(),
    %% Alice and Bob use example.com
    F = fun(FreshConfig, Alice, Bob) ->
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"OH, HAI!">>)),
        escalus:wait_for_stanza(Bob),
        mam_helper:wait_for_archive_size_with_host_type(HostType, Alice, 1),
        mam_helper:wait_for_archive_size_with_host_type(HostType, Bob, 1),
        {{<<"204">>, _}, _} =
            rest_delete_domain(Config, <<"example.com">>, HostType),
        {{<<"204">>, _}, _} =
            rest_delete_domain(Config, <<"example.org">>, HostType),
        sync(),
        %% At this point MIM cannot resolve a domain to a host type,
        %% so we have to pass it
        mam_helper:wait_for_archive_size_with_host_type(HostType, Alice, 0),
        mam_helper:wait_for_archive_size_with_host_type(HostType, Bob, 0),
        %% Already cleaned at this point
        escalus_cleaner:remove_client(FreshConfig, Alice),
        escalus_cleaner:remove_client(FreshConfig, Bob)
        end,
    escalus:fresh_story_with_config(Config, [{alice3, 1}, {bob3, 1}], F).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

service_enabled(Node) ->
    service_enabled(Node, []),
    sync_local(Node).

service_enabled(Node, Opts) ->
    rpc(Node, mongoose_service, start_service, [service_domain_db, Opts]),
    true = rpc(Node, service_domain_db, enabled, []).

service_disabled(Node) ->
    rpc(Node, mongoose_service, stop_service, [service_domain_db]),
    false = rpc(Node, service_domain_db, enabled, []).

init_with(Node, Pairs, AllowedHostTypes) ->
    rpc(Node, mongoose_domain_core, stop, []),
    rpc(Node, mongoose_domain_core, start, [Pairs, AllowedHostTypes]).

insert_domain(Node, Domain, HostType) ->
    rpc(Node, mongoose_domain_api, insert_domain, [Domain, HostType]).

delete_domain(Node, Domain, HostType) ->
    rpc(Node, mongoose_domain_api, delete_domain, [Domain, HostType]).

select_domain(Node, Domain) ->
    rpc(Node, mongoose_domain_sql, select_domain, [Domain]).

insert_full_event(Node, EventId, Domain) ->
    rpc(Node, mongoose_domain_sql, insert_full_event, [EventId, Domain]).

insert_domain_settings_without_event(Node, Domain, HostType) ->
    rpc(Node, mongoose_domain_sql, insert_domain_settings_without_event,
        [Domain, HostType]).

get_event_ids_between(Node, Min, Max) ->
    rpc(Node, mongoose_domain_sql, get_event_ids_between, [Min, Max]).

erase_database(Node) ->
    case mongoose_helper:is_rdbms_enabled(domain()) of
        true ->
            prepare_test_queries(Node),
            rpc(Node, mongoose_domain_sql, erase_database, []);
        false -> ok
    end.

prepare_test_queries(Node) ->
    case mongoose_helper:is_rdbms_enabled(domain()) of
        true -> rpc(Node, mongoose_domain_sql, prepare_test_queries, []);
        false -> ok
    end.

get_min_event_id(Node) ->
    {Min, _} = rpc(Node, mongoose_domain_sql, get_minmax_event_id, []),
    Min.

get_max_event_id(Node) ->
    {_, Max} = rpc(Node, mongoose_domain_sql, get_minmax_event_id, []),
    Max.

delete_events_older_than(Node, Id) ->
    rpc(Node, mongoose_domain_sql, delete_events_older_than, [Id]).

get_host_type(Node, Domain) ->
    rpc(Node, mongoose_domain_api, get_host_type, [Domain]).

get_domains_by_host_type(Node, HostType) ->
    rpc(Node, mongoose_domain_api, get_domains_by_host_type, [HostType]).

get_all_static(Node) ->
    rpc(Node, mongoose_domain_api, get_all_static, []).

get_all_dynamic(Node) ->
    rpc(Node, mongoose_domain_api, get_all_dynamic, []).

disable_domain(Node, Domain) ->
    rpc(Node, mongoose_domain_api, disable_domain, [Domain]).

enable_domain(Node, Domain) ->
    rpc(Node, mongoose_domain_api, enable_domain, [Domain]).

%% force_check_for_updates is already sent by insert or delete commands.
%% But it is async.
%% So, the only thing is left to sync is to call ping to the gen_server
%% to ensure we've finished the check.
sync() ->
    sync_local(mim()),
    sync_local(mim2()),
    ok.

with_service_suspended(F) ->
    suspend_service(mim()),
    suspend_service(mim2()),
    try
        F()
    after
        resume_service(mim()),
        resume_service(mim2())
    end.

suspend_service(Node) ->
    ok = rpc(Node, sys, suspend, [service_domain_db]).

resume_service(Node) ->
    ok = rpc(Node, sys, resume, [service_domain_db]).

sync_local(Node) ->
    pong = rpc(Node, service_domain_db, sync_local, []).

force_check_for_updates(Node) ->
    ok = rpc(Node, service_domain_db, force_check_for_updates, []).

restore_conf(Node, #{loaded := Loaded, service_opts := ServiceOpts, core_opts := CoreOpts}) ->
    rpc(Node, mongoose_service, stop_service, [service_domain_db]),
    [Pairs, AllowedHostTypes] = CoreOpts,
    init_with(Node, Pairs, AllowedHostTypes),
    case Loaded of
        true ->
            rpc(Node, mongoose_service, start_service, [service_domain_db, ServiceOpts]);
        _ ->
            ok
    end.

%% Needed for pg2 group to work
%% So, multiple node tests work
ensure_nodes_know_each_other() ->
    pong = rpc(mim2(), net_adm, ping, [maps:get(node, mim())]).

maybe_setup_meck(TC) when TC =:= rest_insert_domain_fails_if_db_fails;
                          TC =:= cli_insert_domain_fails_if_db_fails ->
    ok = rpc(mim(), meck, new, [mongoose_domain_sql, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_domain_sql, insert_domain, 2,
                                   {error, {db_error, simulated_db_error}}]);
maybe_setup_meck(TC) when TC =:= rest_delete_domain_fails_if_db_fails;
                          TC =:= cli_delete_domain_fails_if_db_fails ->
    ok = rpc(mim(), meck, new, [mongoose_domain_sql, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_domain_sql, delete_domain, 2,
                                   {error, {db_error, simulated_db_error}}]);
maybe_setup_meck(TC) when TC =:= rest_enable_domain_fails_if_db_fails;
                          TC =:= cli_enable_domain_fails_if_db_fails ->
    ok = rpc(mim(), meck, new, [mongoose_domain_sql, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_domain_sql, enable_domain, 1,
                                   {error, {db_error, simulated_db_error}}]);
maybe_setup_meck(cli_disable_domain_fails_if_db_fails) ->
    ok = rpc(mim(), meck, new, [mongoose_domain_sql, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_domain_sql, disable_domain, 1,
                                   {error, {db_error, simulated_db_error}}]);
maybe_setup_meck(db_crash_on_initial_load_restarts_service) ->
    ok = rpc(mim(), meck, new, [mongoose_domain_sql, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_domain_sql, select_from, 2, something_strange]),
    ok = rpc(mim(), meck, new, [service_domain_db, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [service_domain_db, restart, 0, ok]);
maybe_setup_meck(db_out_of_sync_restarts_service) ->
    ok = rpc(mim(), meck, new, [service_domain_db, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [service_domain_db, restart, 0, ok]);
maybe_setup_meck(_TestCase) ->
    ok.

maybe_teardown_meck(_) ->
    %% running unload meck makes no harm even if nothing is mocked
    rpc(mim(), meck, unload, []).

leave_cluster(Config) ->
    Cmd = "leave_cluster",
    #{node := Node} = distributed_helper:mim(),
    Args = ["--force"],
    mongooseimctl_helper:mongooseimctl(Node, Cmd, Args, Config).

join_cluster(Config) ->
    Cmd = "join_cluster",
    #{node := Node} = distributed_helper:mim(),
    #{node := Node2} = distributed_helper:mim2(),
    Args = ["--force", atom_to_list(Node2)],
    mongooseimctl_helper:mongooseimctl(Node, Cmd, Args, Config).

assert_domains_are_equal(HostType) ->
    Domains1 = lists:sort(get_domains_by_host_type(mim(), HostType)),
    Domains2 = lists:sort(get_domains_by_host_type(mim2(), HostType)),
    case Domains1 == Domains2 of
        true -> ok;
        false -> ct:fail({Domains1, Domains2})
    end.

dummy_auth_host_type() ->
    <<"dummy auth">>. %% specified in the TOML config file
