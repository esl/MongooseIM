-module(service_domain_db_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).
-import(distributed_helper, [mim/0, mim2/0, mim3/0, require_rpc_nodes/1, rpc/4,
                             remove_node_from_cluster/2]).
-import(graphql_helper, [execute_command/4]).

-import(domain_helper, [domain/0]).
-import(config_parser_helper, [config/2]).

suite() ->
    require_rpc_nodes([mim, mim2, mim3]).

all() ->
    [
     {group, no_db},
     {group, db}
    ].

groups() ->
    ParallelConfig = [parallel],
    [
        {no_db, ParallelConfig, no_db_cases()},
        {db, [], [
            {group, plain_db},
            {group, plain_db_sequential}
        ]},
        {plain_db, ParallelConfig, db_cases()},
        {plain_db_sequential, [], db_sequential_cases()}
    ].

no_db_cases() -> [
    api_lookup_works,
    api_lookup_not_found,
    api_cannot_insert_static,
    api_cannot_disable_static,
    api_cannot_enable_static,
    api_get_all_static,
    api_get_domains_by_host_type
].

db_cases() -> [
    db_get_all_static,
    db_get_all_dynamic,
    db_inserted_domain_is_in_db,
    db_inserted_domain_is_in_core,
    db_deleted_domain_from_db,
    db_deleted_domain_fails_with_wrong_host_type,
    db_deleted_domain_from_core,
    db_disabled_domain_is_in_db,
    db_disabled_domain_not_in_core,
    db_reenabled_domain_is_in_db,
    db_reenabled_domain_is_in_core,
    db_cannot_insert_domain_twice_with_the_same_host_type,
    db_cannot_insert_domain_twice_with_another_host_type,
    db_cannot_insert_domain_with_unknown_host_type,
    db_cannot_delete_domain_with_unknown_host_type,
    db_cannot_enable_domain_with_unknown_host_type,
    db_cannot_disable_domain_with_unknown_host_type,
    db_domains_with_unknown_host_type_are_ignored_by_core,
    db_can_insert_update_delete_dynamic_domain_password,
    db_cannot_set_password_for_unknown_domain,
    db_can_check_domain_password,
    db_cannot_check_password_for_unknown_domain,
    db_deleting_domain_deletes_domain_admin,
    db_get_all_domains,
    sql_select_from,
    db_could_sync_between_nodes,
    db_gaps_are_getting_filled_automatically
].

db_sequential_cases() -> [
    sql_find_gaps_between,
    db_records_are_restored_on_mim_restart,
    db_record_is_ignored_if_domain_static,
    db_events_table_gets_truncated,
    db_deleted_from_one_node_while_service_disabled_on_another,
    db_inserted_from_one_node_while_service_disabled_on_another,
    db_reinserted_from_one_node_while_service_disabled_on_another,
    db_crash_on_initial_load_restarts_service,
    db_out_of_sync_restarts_service,
    db_restarts_properly,
    db_keeps_syncing_after_cluster_join,
    db_event_could_appear_with_lower_id,
    db_can_insert_update_delete_static_domain_password
].

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config0 = dynamic_services:save_services(all_nodes(), Config),
    Config1 = dynamic_modules:save_modules(dummy_auth_host_type(), Config0),
    ensure_nodes_know_each_other(),
    service_disabled(mim()),
    service_disabled(mim2()),
    service_disabled(mim3()),
    prepare_test_queries(mim()),
    prepare_test_queries(mim2()),
    erase_database(mim()),
    Config2 = ejabberd_node_utils:init(mim(), Config1),
    mongoose_helper:inject_module(?MODULE),
    %% If there are CI issues, use this to debug SQL queries:
%   mim_loglevel:enable_logging([mim, mim2, mim3], [{mongoose_domain_sql, debug}]),
    escalus:init_per_suite([{service_setup, per_testcase} | Config2]).

end_per_suite(Config) ->
%   mim_loglevel:disable_logging([mim, mim2, mim3], [{mongoose_domain_sql, debug}]),
    [restart_domain_core(Node) || Node <- all_nodes()],
    dynamic_services:restore_services(Config),
    domain_helper:insert_configured_domains(),
    dynamic_modules:restore_modules(Config),
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

all_nodes() ->
    [mim(), mim2(), mim3()].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_group(db, Config) ->
    case mongoose_helper:is_rdbms_enabled(dummy_auth_host_type()) of
        true -> [{service, true}|Config];
        false -> {skip, require_rdbms}
    end;
init_per_group(GroupName, Config) ->
    Config1 = save_service_setup_option(GroupName, Config),
    case ?config(service_setup, Config1) of
        per_group -> setup_service(#{}, Config1);
        per_testcase -> ok
    end,
    Config1.

end_per_group(_GroupName, Config) ->
    case ?config(service_setup, Config) of
        per_group -> teardown_service();
        per_testcase -> ok
    end.

init_per_testcase(db_crash_on_initial_load_restarts_service, Config) ->
    maybe_setup_meck(db_crash_on_initial_load_restarts_service),
    restart_domain_core(mim(), [], []),
    Config;
init_per_testcase(TestcaseName, Config) ->
    maybe_setup_meck(TestcaseName),
    case ?config(service_setup, Config) of
        per_group -> ok;
        per_testcase -> setup_service(service_opts(TestcaseName), Config)
    end,
    init_per_testcase2(TestcaseName, Config).

service_opts(db_events_table_gets_truncated) ->
    #{event_cleaning_interval => 1, event_max_age => 3};
service_opts(_) ->
    #{}.

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

init_per_testcase2(db_keeps_syncing_after_cluster_join, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_testcase2(_, Config) ->
    Config.

end_per_testcase2(db_keeps_syncing_after_cluster_join, _Config) ->
    graphql_helper:clean();
end_per_testcase2(_, Config) ->
    Config.

setup_service(Opts, Config) ->
    ServiceEnabled = proplists:get_value(service, Config, false),
    Pairs1 = [{<<"example.cfg">>, <<"type1">>},
              {<<"erlang-solutions.com">>, <<"type2">>},
              {<<"erlang-solutions.local">>, <<"type2">>}],
    restart_domain_core(mim(), Pairs1, host_types_for_mim()),
    restart_domain_core(mim2(), [], host_types_for_mim2()),
    case ServiceEnabled of
        true ->
            service_enabled(mim(), Opts),
            service_enabled(mim2(), #{});
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

% no_db cases

api_lookup_works(_) ->
    {ok, <<"type1">>} = get_host_type(mim(), <<"example.cfg">>).

api_lookup_not_found(_) ->
    {error, not_found} = get_host_type(mim(), <<"example.missing">>).

api_cannot_insert_static(_) ->
    {static, <<"Domain is static">>} = insert_domain(mim(), <<"example.cfg">>, <<"type1">>).

api_cannot_disable_static(_) ->
    {static, <<"Domain is static">>} = disable_domain(mim(), <<"example.cfg">>).

api_cannot_enable_static(_) ->
    {static, <<"Domain is static">>} = enable_domain(mim(), <<"example.cfg">>).

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

% plain_db cases

%% Similar to as api_get_all_static, just with DB service enabled
db_get_all_static(_) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim(), Domain, <<"type1">>),
    sync(),
    %% Could be in any order
    [{<<"erlang-solutions.com">>, <<"type2">>},
     {<<"erlang-solutions.local">>, <<"type2">>},
     {<<"example.cfg">>, <<"type1">>}] =
        lists:sort(get_all_static(mim())).

db_get_all_domains(_) ->
    Domain1 = random_domain_name(),
    Domain2 = random_domain_name(),
    {ok, _} = insert_domain(mim(), Domain1, <<"type1">>),
    {ok, _} = insert_domain(mim(), Domain2, <<"type1">>),
    disable_domain(mim(), Domain1),
    sync(),

    AllDomains = get_all_domains(mim()),

    %% Verify Domain1 is disabled and Domain2 is enabled
    [Domain1Map] = [D || D <- AllDomains, maps:get(domain, D) == Domain1],
    ?assertMatch(#{domain := Domain1, host_type := <<"type1">>, status := disabled}, Domain1Map),
    [Domain2Map] = [D || D <- AllDomains, maps:get(domain, D) == Domain2],
    ?assertMatch(#{domain := Domain2, host_type := <<"type1">>, status := enabled}, Domain2Map).

db_get_all_dynamic(_) ->
    Domain1 = random_domain_name(),
    Domain2 = random_domain_name(),
    {ok, _} = insert_domain(mim(), Domain1, <<"type1">>),
    {ok, _} = insert_domain(mim(), Domain2, <<"type1">>),
    sync(),
    ExpectedResult = sets:from_list([{Domain1, <<"type1">>}, {Domain2, <<"type1">>}]),
    Result = sets:from_list(get_all_dynamic(mim())),
    % Other test cases can insert their own domains thus we test is_subset
    true = sets:is_subset(ExpectedResult, Result).

db_inserted_domain_is_in_db(_) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim(), Domain, <<"type1">>),
    {ok, #{host_type := <<"type1">>, status := enabled}} =
       select_domain(mim(), Domain).

db_inserted_domain_is_in_core(_) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim(), Domain, <<"type1">>),
    sync(),
    {ok, <<"type1">>} = get_host_type(mim(), Domain).

db_deleted_domain_from_db(_) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim(), Domain, <<"type1">>),
    {ok, _} = delete_domain(mim(), Domain, <<"type1">>),
    {error, not_found} = select_domain(mim(), Domain).

db_deleted_domain_fails_with_wrong_host_type(_) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim(), Domain, <<"type1">>),
    {wrong_host_type, _} =
        delete_domain(mim(), Domain, <<"type2">>),
    {ok, #{host_type := <<"type1">>, status := enabled}} =
        select_domain(mim(), Domain).

db_deleted_domain_from_core(_) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim(), Domain, <<"type1">>),
    sync(),
    {ok, _} = delete_domain(mim(), Domain, <<"type1">>),
    sync(),
    {error, not_found} = get_host_type(mim(), Domain).

db_disabled_domain_is_in_db(_) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim(), Domain, <<"type1">>),
    {ok, _} = disable_domain(mim(), Domain),
    {ok, #{host_type := <<"type1">>, status := disabled}} =
       select_domain(mim(), Domain).

db_disabled_domain_not_in_core(_) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim(), Domain, <<"type1">>),
    {ok, _} = disable_domain(mim(), Domain),
    sync(),
    {error, not_found} = get_host_type(mim(), Domain).

db_reenabled_domain_is_in_db(_) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim(), Domain, <<"type1">>),
    {ok, _} = disable_domain(mim(), Domain),
    {ok, _} = enable_domain(mim(), Domain),
    {ok, #{host_type := <<"type1">>, status := enabled}} =
       select_domain(mim(), Domain).

db_reenabled_domain_is_in_core(_) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim(), Domain, <<"type1">>),
    {ok, _} = disable_domain(mim(), Domain),
    {ok, _} = enable_domain(mim(), Domain),
    sync(),
    {ok, <<"type1">>} = get_host_type(mim(), Domain).

db_cannot_insert_domain_twice_with_the_same_host_type(_) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim(), Domain, <<"type1">>),
    {duplicate, _} = insert_domain(mim(), Domain, <<"type1">>).

db_cannot_insert_domain_twice_with_another_host_type(_) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim(), Domain, <<"type1">>),
    {duplicate, _} = insert_domain(mim(), Domain, <<"type2">>).

db_cannot_insert_domain_with_unknown_host_type(_) ->
    Domain = random_domain_name(),
    {unknown_host_type, _} = insert_domain(mim(), Domain, <<"type6">>).

db_cannot_delete_domain_with_unknown_host_type(_) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim2(), Domain, <<"mim2only">>),
    sync(),
    {unknown_host_type, _} = delete_domain(mim(), Domain, <<"mim2only">>).

db_cannot_enable_domain_with_unknown_host_type(_) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim2(), Domain, <<"mim2only">>),
    {ok, _} = disable_domain(mim2(), Domain),
    sync(),
    {unknown_host_type, _} = enable_domain(mim(), Domain).

db_cannot_disable_domain_with_unknown_host_type(_) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim2(), Domain, <<"mim2only">>),
    sync(),
    {unknown_host_type, _} = disable_domain(mim(), Domain).

db_domains_with_unknown_host_type_are_ignored_by_core(_) ->
    Domain1 = random_domain_name(),
    Domain2 = random_domain_name(),
    {ok, _} = insert_domain(mim2(), Domain1, <<"mim2only">>),
    {ok, _} = insert_domain(mim2(), Domain2, <<"type1">>),
    sync(),
    {ok, <<"type1">>} = get_host_type(mim(), Domain2), %% Counter-case
    {error, not_found} = get_host_type(mim(), Domain1).

db_can_insert_update_delete_dynamic_domain_password(_) ->
    Domain = <<"password-example.com">>,
    {ok, _} = insert_domain(mim(), Domain, <<"type1">>),
    sync(),
    {ok, _} = set_domain_password(mim(), Domain, <<"rocky1">>),
    ok = check_domain_password(mim(), Domain, <<"rocky1">>),
    {ok, _} = set_domain_password(mim(), Domain, <<"rocky2">>),
    ok = check_domain_password(mim(), Domain, <<"rocky2">>),
    {ok, _} = delete_domain_password(mim(), Domain),
    {error, not_found} = select_domain_admin(mim(), Domain).

db_cannot_set_password_for_unknown_domain(_) ->
    {not_found, _} = set_domain_password(mim(), <<"unknown_domain">>, <<>>).

db_can_check_domain_password(_) ->
    StaticDomain = <<"example.cfg">>,
    {ok, _} = set_domain_password(mim(), StaticDomain, <<"myrock">>),
    ok = check_domain_password(mim(), StaticDomain, <<"myrock">>),
    {error, wrong_password} = check_domain_password(mim(), StaticDomain, <<"wrongrock">>).

db_cannot_check_password_for_unknown_domain(_) ->
    {error, not_found} = check_domain_password(mim(), <<"unknown_domain">>, <<>>).

db_deleting_domain_deletes_domain_admin(_) ->
    Domain = <<"password-del-example.db">>,
    {ok, _} = insert_domain(mim(), Domain, <<"type1">>),
    sync(),
    {ok, _} = set_domain_password(mim(), Domain, <<"deleteme">>),
    {ok, _} = delete_domain(mim(), Domain, <<"type1">>),
    {error, not_found} = select_domain_admin(mim(), Domain).

sql_select_from(_) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim(), Domain, <<"type1">>),
    Result = rpc(mim(), mongoose_domain_sql, select_from, [0, 100]),
    % Other test cases insert their own domains thus we check for existence in list
    [_|_] = lists:filter(fun({_, D, _}) -> D == Domain end, Result).

db_could_sync_between_nodes(_) ->
    {ok, _} = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync(),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>).

db_gaps_are_getting_filled_automatically(_Config) ->
    Domain = random_domain_name(),
    {ok, _} = insert_domain(mim(), Domain, <<"type1">>),
    sync(),
    Max = get_max_event_id(mim()),
    %% Create a gap in events by manually adding an event
    GapSize = 10,
    {updated, 1} = insert_full_event(mim(), Max + GapSize, <<"something.else">>),
    force_check_for_updates(mim()),
    sync_local(mim()),
    F = fun() -> get_event_ids_between(mim(), Max, Max + GapSize) end,
    wait_helper:wait_until(F, lists:seq(Max, Max + GapSize),
                           #{time_left => timer:seconds(15)}).

% plain_db_sequential cases
% these tests are hard to parallelise due to runtime changes to service

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
    {ok, _} = insert_domain(mim(), <<"example.com">>, <<"type1">>),
    %% Simulate MIM restart
    service_disabled(mim()),
    restart_domain_core(mim(), [], [<<"type1">>]),
    {error, not_found} = get_host_type(mim(), <<"example.com">>),
    service_enabled(mim()),
    %% DB still contains data
    {ok, #{host_type := <<"type1">>, status := enabled}} =
       select_domain(mim(), <<"example.com">>),
    %% Restored
    {ok, <<"type1">>} = get_host_type(mim(), <<"example.com">>).

db_record_is_ignored_if_domain_static(_) ->
    {ok, _} = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    {ok, _} = insert_domain(mim(), <<"example.net">>, <<"dbgroup">>),
    %% Simulate MIM restart
    service_disabled(mim()),
    %% Only one domain is static
    restart_domain_core(mim(), [{<<"example.com">>, <<"cfggroup">>}], [<<"dbgroup">>, <<"cfggroup">>]),
    service_enabled(mim()),
    %% DB still contains data
    {ok, #{host_type := <<"dbgroup">>, status := enabled}} =
       select_domain(mim(), <<"example.com">>),
    {ok, #{host_type := <<"dbgroup">>, status := enabled}} =
       select_domain(mim(), <<"example.net">>),
     %% Static DB records are ignored
    {ok, <<"cfggroup">>} = get_host_type(mim(), <<"example.com">>),
    {ok, <<"dbgroup">>} = get_host_type(mim(), <<"example.net">>).

db_events_table_gets_truncated(_) ->
    %% Configure service with a very short interval
    {ok, _} = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    {ok, _} = insert_domain(mim(), <<"example.net">>, <<"dbgroup">>),
    {ok, _} = insert_domain(mim(), <<"example.org">>, <<"dbgroup">>),
    {ok, _} = insert_domain(mim(), <<"example.beta">>, <<"dbgroup">>),
    Max = get_max_event_id(mim()),
    true = is_integer(Max),
    true = Max > 0,
    %% The events table is not empty and the size of 1, eventually.
    F = fun() -> get_min_event_id(mim()) end,
    wait_helper:wait_until(F, Max, #{time_left => timer:seconds(15)}).

db_deleted_from_one_node_while_service_disabled_on_another(_) ->
    {ok, _} = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync(),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>),
    %% Service is disable on the second node
    service_disabled(mim2()),
    %% Removed from the first node
    {ok, _} = delete_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync_local(mim()),
    {error, not_found} = get_host_type(mim(), <<"example.com">>),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>),
    %% Sync is working again
    service_enabled(mim2()),
    {error, not_found} = get_host_type(mim2(), <<"example.com">>).

db_inserted_from_one_node_while_service_disabled_on_another(_) ->
    %% Service is disable on the second node
    service_disabled(mim2()),
    {ok, _} = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    %% Sync is working again
    service_enabled(mim2()),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>).

db_reinserted_from_one_node_while_service_disabled_on_another(_) ->
    %% This test shows the behaviour when someone
    %% reinserts a domain with a different host type.
    {ok, _} = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync(),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>),
    %% Service is disable on the second node
    service_disabled(mim2()),
    %% Removed from the first node
    {ok, _} = delete_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync_local(mim()),
    {ok, _} = insert_domain(mim(), <<"example.com">>, <<"dbgroup2">>),
    sync_local(mim()),
    %% Sync is working again
    service_enabled(mim2()),
    sync(),
    %% A corner case: domain name is reinserted with different host type
    %% while service was down on mim2. check that mim2 is updated
    {ok, <<"dbgroup2">>} = get_host_type(mim(), <<"example.com">>),
    {ok, <<"dbgroup2">>} = get_host_type(mim2(), <<"example.com">>),
    %% check deleting
    {ok, _} = delete_domain(mim2(), <<"example.com">>, <<"dbgroup2">>),
    sync(),
    {error, not_found} = get_host_type(mim(), <<"example.com">>),
    {error, not_found} = get_host_type(mim2(), <<"example.com">>).

db_crash_on_initial_load_restarts_service(_) ->
    service_enabled(mim()),
    %% service is restarted
    true = rpc(mim(), meck, wait, [service_domain_db, restart, 0, timer:seconds(1)]) > 0,
    ok.

db_out_of_sync_restarts_service(_) ->
    {ok, _} = insert_domain(mim2(), <<"example1.com">>, <<"type1">>),
    {ok, _} = insert_domain(mim2(), <<"example2.com">>, <<"type1">>),
    sync(),
    %% Pause processing events on one node
    suspend_service(mim()),
    {ok, _} = insert_domain(mim2(), <<"example3.com">>, <<"type1">>),
    {ok, _} = insert_domain(mim2(), <<"example4.com">>, <<"type1">>),
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
    wait_helper:wait_until(F, true, #{time_left => timer:seconds(15)}).

db_keeps_syncing_after_cluster_join(Config) ->
    HostType = dummy_auth_host_type(),
    %% GIVING mim1 and mim2 are not clustered.
    %% Ask mim1 to join mim2's cluster
    %% (and mongooseim application gets restarted on mim1)
    leave_cluster(Config),
    service_enabled(mim()),
    {ok, _} = insert_domain(mim(), <<"example1.com">>, HostType),
    {ok, _} = insert_domain(mim2(), <<"example2.com">>, HostType),
    sync(),
    %% Nodes don't have to be clustered to sync the domains.
    assert_domains_are_equal(HostType),
    %% WHEN Adding mim1 joins into a cluster
    %% (and mongooseim application gets restarted on mim1)
    join_cluster(Config),
    service_enabled(mim()),
    %% THEN Sync is successful
    {ok, _} = insert_domain(mim(), <<"example3.com">>, HostType),
    {ok, _} = insert_domain(mim2(), <<"example4.com">>, HostType),
    sync(),
    assert_domains_are_equal(HostType).

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
    wait_helper:wait_until(F, lists:seq(40, 50), #{time_left => timer:seconds(15)}).

db_can_insert_update_delete_static_domain_password(_) ->
    StaticDomain = <<"example.cfg">>,
    {ok, _} = set_domain_password(mim(), StaticDomain, <<"rocky1">>),
    ok = check_domain_password(mim(), StaticDomain, <<"rocky1">>),
    {ok, _} = set_domain_password(mim(), StaticDomain, <<"rocky2">>),
    ok = check_domain_password(mim(), StaticDomain, <<"rocky2">>),
    {ok, _} = delete_domain_password(mim(), StaticDomain),
    {error, not_found} = select_domain_admin(mim(), StaticDomain).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

service_enabled(Node) ->
    service_enabled(Node, #{}),
    sync_local(Node).

service_enabled(Node, ExtraOpts) ->
    Opts = config([services, service_domain_db], ExtraOpts),
    dynamic_services:ensure_started(Node, service_domain_db, Opts),
    true = rpc(Node, service_domain_db, enabled, []).

service_disabled(Node) ->
    dynamic_services:ensure_stopped(Node, service_domain_db),
    false = rpc(Node, service_domain_db, enabled, []).

restart_domain_core(Node, Pairs, AllowedHostTypes) ->
    domain_helper:restart_domain_core(Node, Pairs, AllowedHostTypes).

restart_domain_core(Node) ->
    domain_helper:restart_domain_core(Node).

insert_domain(Node, Domain, HostType) ->
    rpc(Node, mongoose_domain_api, insert_domain, [Domain, HostType]).

delete_domain(Node, Domain, HostType) ->
    rpc(Node, mongoose_domain_api, delete_domain, [Domain, HostType]).

request_delete_domain(Node, Domain, HostType) ->
    rpc(Node, mongoose_domain_api, request_delete_domain, [Domain, HostType]).

select_domain(Node, Domain) ->
    rpc(Node, mongoose_domain_sql, select_domain, [Domain]).

check_domain_password(Node, Domain, Password) ->
    rpc(Node, mongoose_domain_api, check_domain_password, [Domain, Password]).

set_domain_password(Node, Domain, Password) ->
    rpc(Node, mongoose_domain_api, set_domain_password, [Domain, Password]).

delete_domain_password(Node, Domain) ->
    rpc(Node, mongoose_domain_api, delete_domain_password, [Domain]).

select_domain_admin(Node, Domain) ->
    rpc(Node, mongoose_domain_sql, select_domain_admin, [Domain]).

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
            rpc(Node, mongoose_domain_sql, erase_database, [global]);
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

get_all_domains(Node) ->
    rpc(Node, mongoose_domain_api, get_all_domains, []).

disable_domain(Node, Domain) ->
    rpc(Node, mongoose_domain_api, disable_domain, [Domain]).

enable_domain(Node, Domain) ->
    rpc(Node, mongoose_domain_api, enable_domain, [Domain]).

start_domain_removal_hook(HostType) ->
    Server = spawn(fun stopper/0),
    rpc(mim(), gen_hook, add_handler,
        [ remove_domain, HostType, fun ?MODULE:domain_removal_hook_fn/3,
          #{server => Server}, 30]), %% Priority is so that it comes before muclight and mam
    Server.

stop_domain_removal_hook(HostType, Server) ->
    rpc(mim(), gen_hook, delete_handler,
        [ remove_domain, HostType, fun ?MODULE:domain_removal_hook_fn/3,
          #{server => Server}, 30]).

domain_removal_hook_fn(Acc, _Params, #{server := Server}) ->
    Server ! {wait, self()},
    receive continue -> ok end,
    {ok, Acc}.

stopper() ->
    receive
        {wait, From} ->
            receive continue -> ok end,
            From ! continue
    end.

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
    pong = rpc(Node#{timeout => timer:seconds(30)}, service_domain_db, sync_local, []).

force_check_for_updates(Node) ->
    ok = rpc(Node, service_domain_db, force_check_for_updates, []).

%% Needed for pg2 group to work
%% So, multiple node tests work
ensure_nodes_know_each_other() ->
    pong = rpc(mim2(), net_adm, ping, [maps:get(node, mim())]).

maybe_setup_meck(rest_insert_domain_fails_if_db_fails) ->
    ok = rpc(mim(), meck, new, [mongoose_domain_sql, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_domain_sql, insert_domain, 2,
                                   {error, {db_error, simulated_db_error}}]);
maybe_setup_meck(rest_delete_domain_fails_if_db_fails) ->
    ok = rpc(mim(), meck, new, [mongoose_domain_sql, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_domain_sql, delete_domain, 2,
                                   {error, {db_error, simulated_db_error}}]);
maybe_setup_meck(rest_enable_domain_fails_if_db_fails) ->
    ok = rpc(mim(), meck, new, [mongoose_domain_sql, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_domain_sql, set_status, 2,
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
    execute_command(<<"server">>, <<"leaveCluster">>, #{}, Config).

join_cluster(Config) ->
    #{node := Node2} = distributed_helper:mim2(),
    execute_command(<<"server">>, <<"joinCluster">>, #{<<"node">> => Node2}, Config).

assert_domains_are_equal(HostType) ->
    Domains1 = lists:sort(get_domains_by_host_type(mim(), HostType)),
    Domains2 = lists:sort(get_domains_by_host_type(mim2(), HostType)),
    case Domains1 == Domains2 of
        true -> ok;
        false -> ct:fail({Domains1, Domains2})
    end.

assert_rest_db_error({Result, Msg}) ->
    ?assertEqual({<<"500">>, <<"Internal Server Error">>}, Result),
    ?assertEqual(<<>>, Msg). % shouldn't leak out the DB error, it's in the logs anyway

dummy_auth_host_type() ->
    <<"dummy auth">>. %% specified in the TOML config file

random_domain_name() ->
    Prefix = integer_to_binary(erlang:unique_integer([positive])),
    <<Prefix/binary, ".example.db">>.
