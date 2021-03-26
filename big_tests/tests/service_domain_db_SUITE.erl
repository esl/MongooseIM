-module(service_domain_db_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-import(distributed_helper, [mim/0, mim2/0, require_rpc_nodes/1, rpc/4]).
-import(ejabberdctl_helper, [ejabberdctl/3]).

suite() ->
    require_rpc_nodes([mim, mim2]).

all() ->
    [
     core_lookup_works,
     core_lookup_not_found,
     core_static_domain,
     core_cannot_insert_static,
     core_cannot_disable_static,
     core_cannot_enable_static,
     core_get_all_static,
     core_get_domains_by_host_type,
     {group, db}
    ].

groups() ->
    [{db, [], db_cases()}].

db_cases() -> [
     db_inserted_domain_is_in_db,
     db_inserted_domain_is_in_core,
     db_deleted_domain_from_db,
     db_deleted_domain_fails_with_wrong_host_type,
     db_deleted_domain_from_core,
     db_disabled_domain_is_in_db,
     db_disabled_domain_not_in_core,
     db_reanabled_domain_is_in_db,
     db_reanabled_domain_is_in_core,
     db_can_insert_domain_twice_with_the_same_host_type,
     db_cannot_insert_domain_twice_with_another_host_type,
     db_cannot_insert_domain_with_unknown_host_type,
     db_cannot_delete_domain_with_unknown_host_type,
     db_cannot_enable_domain_with_unknown_host_type,
     db_cannot_disable_domain_with_unknown_host_type,
     db_domains_with_unknown_host_type_are_ignored_by_core,
     sql_select_from_works,
     db_records_are_restored_when_restarted,
     db_record_is_ignored_if_domain_static,
     db_events_table_gets_truncated,
     db_get_all_static,
     db_could_sync_between_nodes,
     db_deleted_from_one_node_while_service_disabled_on_another,
     db_inserted_from_one_node_while_service_disabled_on_another,
     db_reinserted_from_one_node_while_service_disabled_on_another,
     db_out_of_sync_crashes_node,
     db_initial_load_crashes_node,
     cli_can_insert_domain,
     cli_can_disable_domain,
     cli_can_enable_domain,
     cli_can_delete_domain,
     cli_cannot_delete_domain_without_correct_type,
     cli_cannot_insert_domain_twice_with_another_host_type,
     cli_cannot_insert_domain_with_unknown_host_type,
     cli_cannot_enable_missing_domain,
     cli_cannot_disable_missing_domain,
     rest_can_insert_domain,
     rest_can_disable_domain,
     rest_can_delete_domain,
     rest_cannot_delete_domain_without_correct_type,
     rest_delete_missing_domain,
     rest_cannot_insert_domain_twice_with_another_host_type,
     rest_cannot_insert_domain_with_unknown_host_type,
     rest_cannot_enable_missing_domain,
     rest_cannot_disable_missing_domain,
     rest_can_enable_domain,
     rest_can_select_domain,
     rest_cannot_put_domain_without_host_type,
     rest_cannot_put_domain_without_body,
     rest_cannot_put_domain_with_invalid_json,
     rest_cannot_delete_domain_without_host_type,
     rest_cannot_delete_domain_without_body,
     rest_cannot_delete_domain_with_invalid_json,
     rest_cannot_patch_domain_without_enabled_field,
     rest_cannot_patch_domain_without_body,
     rest_cannot_patch_domain_with_invalid_json,
     rest_insert_domain_fails_if_db_fails,
     rest_insert_domain_fails_if_service_disabled,
     rest_delete_domain_fails_if_db_fails,
     rest_delete_domain_fails_if_service_disabled,
     rest_enable_domain_fails_if_db_fails,
     rest_enable_domain_fails_if_service_disabled
    ].

-define(APPS, [inets, crypto, ssl, ranch, cowlib, cowboy]).

domain() -> ct:get_config({hosts, mim, domain}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Conf1 = store_conf(mim()),
    Conf2 = store_conf(mim2()),
    ensure_nodes_know_each_other(),
    service_disabled(mim()),
    service_disabled(mim2()),
    prepare_erase(mim()),
    prepare_erase(mim2()),
    erase_database(mim()),
    Config1 = ejabberd_node_utils:init(mim(), Config),
    escalus:init_per_suite([{mim_conf1, Conf1}, {mim_conf2, Conf2}|Config1]).

store_conf(Node) ->
    Loaded = rpc(Node, mongoose_service, is_loaded, [service_domain_db]),
    ServiceOpts = rpc(Node, mongoose_service, get_service_opts, [service_domain_db]),
    CoreOpts = rpc(Node, mongoose_domain_core, get_start_args, []),
    #{loaded => Loaded, service_opts => ServiceOpts, core_opts => CoreOpts}.

end_per_suite(Config) ->
    Conf1 = proplists:get_value(mim_conf1, Config),
    Conf2 = proplists:get_value(mim_conf2, Config),
    restore_conf(mim(), Conf1),
    restore_conf(mim2(), Conf2),
    escalus:end_per_suite(Config).

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_group(db, Config) ->
    case mongoose_helper:is_rdbms_enabled(domain()) of
        true -> [{service, true}|Config];
        false -> {skip, require_rdbms}
    end;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(db_initial_load_crashes_node, Config) ->
    setup_meck(db_initial_load_crashes_node),
    init_with(mim(), [], []),
    Config;
init_per_testcase(rest_insert_domain_fails_if_db_fails, Config) ->
    setup_meck(sql_insert_domain_fails),
    init_per_testcase(generic, Config);
init_per_testcase(rest_delete_domain_fails_if_db_fails, Config) ->
    setup_meck(sql_delete_domain_fails),
    init_per_testcase(generic, Config);
init_per_testcase(rest_enable_domain_fails_if_db_fails, Config) ->
    setup_meck(sql_enable_domain_fails),
    init_per_testcase(generic, Config);
init_per_testcase(TestcaseName, Config) ->
    case TestcaseName of
        db_out_of_sync_crashes_node ->
            setup_meck(db_out_of_sync_crashes_node);
        _ ->
            ok
    end,
    ServiceEnabled = proplists:get_value(service, Config, false),
    Pairs1 = [{<<"example.cfg">>, <<"type1">>},
             {<<"erlang-solutions.com">>, <<"type2">>},
             {<<"erlang-solutions.local">>, <<"type2">>}],
    CommonTypes = [<<"type1">>, <<"type2">>, <<"dbgroup">>, <<"dbgroup2">>, <<"cfggroup">>],
    Types2 = [<<"mim2only">>|CommonTypes],
    init_with(mim(), Pairs1, CommonTypes),
    init_with(mim2(), [], Types2),
    case ServiceEnabled of
        true ->
            service_enabled(mim(), service_opts(TestcaseName)),
            service_enabled(mim2(), []);
        false ->
            ok
    end,
    Config.

service_opts(db_events_table_gets_truncated) ->
    [{event_cleaning_interval, 1}, {event_max_age, 3}];
service_opts(_) ->
    [].

end_per_testcase(db_initial_load_crashes_node, Config) ->
    teardown_meck(),
    end_per_testcase(generic, Config);
end_per_testcase(db_out_of_sync_crashes_node, Config) ->
    rpc(mim(), sys, resume, [service_domain_db]),
    teardown_meck(),
    end_per_testcase(generic, Config);
end_per_testcase(C, Config) when
     C == rest_insert_domain_fails_if_db_fails;
     C == rest_delete_domain_fails_if_db_fails;
     C == rest_enable_domain_fails_if_db_fails ->
    teardown_meck(),
    end_per_testcase(generic, Config);
end_per_testcase(_TestcaseName, Config) ->
    service_disabled(mim()),
    service_disabled(mim2()),
    erase_database(mim()),
    Config.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

core_lookup_works(_) ->
    {ok, <<"type1">>} = get_host_type(mim(), <<"example.cfg">>).

core_lookup_not_found(_) ->
    {error, not_found} = get_host_type(mim(), <<"example.missing">>).

core_static_domain(_) ->
    true = is_static(<<"example.cfg">>).

core_cannot_insert_static(_) ->
    {error, static} = insert_domain(mim(), <<"example.cfg">>, <<"type1">>).

core_cannot_disable_static(_) ->
    {error, static} = disable_domain(mim(), <<"example.cfg">>).

core_cannot_enable_static(_) ->
    {error, static} = enable_domain(mim(), <<"example.cfg">>).

%% See also db_get_all_static
core_get_all_static(_) ->
    %% Could be in any order
    [{<<"erlang-solutions.com">>, <<"type2">>},
     {<<"erlang-solutions.local">>, <<"type2">>},
     {<<"example.cfg">>, <<"type1">>}] =
        lists:sort(get_all_static(mim())).

core_get_domains_by_host_type(_) ->
    [<<"erlang-solutions.com">>, <<"erlang-solutions.local">>] =
        lists:sort(get_domains_by_host_type(mim(), <<"type2">>)),
    [<<"example.cfg">>] = get_domains_by_host_type(mim(), <<"type1">>),
    [] = get_domains_by_host_type(mim(), <<"type6">>).

%% Similar to as core_get_all_static, just with DB service enabled
db_get_all_static(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    sync(),
    %% Could be in any order
    [{<<"erlang-solutions.com">>, <<"type2">>},
     {<<"erlang-solutions.local">>, <<"type2">>},
     {<<"example.cfg">>, <<"type1">>}] =
        lists:sort(get_all_static(mim())).

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

db_reanabled_domain_is_in_db(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    ok = disable_domain(mim(), <<"example.db">>),
    ok = enable_domain(mim(), <<"example.db">>),
    {ok, #{host_type := <<"type1">>, enabled := true}} =
       select_domain(mim(), <<"example.db">>).

db_reanabled_domain_is_in_core(_) ->
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

sql_select_from_works(_) ->
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    [{_, <<"example.db">>, <<"type1">>}] =
       rpc(mim(), mongoose_domain_sql, select_from, [0, 100]).

db_records_are_restored_when_restarted(_) ->
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
    sync(),
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
    %% TLDR: just keep the host_type constant or don't reuse domains.
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync(),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>),
    %% Service is disable on the second node
    service_disabled(mim2()),
    %% Removed from the first node
    ok = delete_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync(),
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup2">>),
    sync(),
    %% Sync is working again
    service_enabled(mim2()),
    sync(),
    %% A corner case: mim2 sees the change, but core ignores it
    {ok, <<"dbgroup2">>} = get_host_type(mim(), <<"example.com">>),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>),
    %% But if we delete it, it would be deleted everywhere
    ok = delete_domain(mim(), <<"example.com">>, <<"dbgroup2">>),
    sync(),
    {error, not_found} = get_host_type(mim(), <<"example.com">>),
    {error, not_found} = get_host_type(mim2(), <<"example.com">>).

db_initial_load_crashes_node(_) ->
    service_enabled(mim()),
    %% Called halt node function, but it's mocked
    true = rpc(mim(), meck, num_calls, [mongoose_domain_utils, halt_node, 1]) > 0,
    ok.

db_out_of_sync_crashes_node(_) ->
    ok = insert_domain(mim2(), <<"example1.com">>, <<"type1">>),
    ok = insert_domain(mim2(), <<"example2.com">>, <<"type1">>),
    sync(),
    %% Pause processing events on one node
    ok = rpc(mim(), sys, suspend, [service_domain_db]),
    ok = insert_domain(mim2(), <<"example3.com">>, <<"type1">>),
    ok = insert_domain(mim2(), <<"example4.com">>, <<"type1">>),
    sync_local(mim2()),
    %% Truncate events table, keep only one event
    MaxId = get_max_event_id(mim2()),
    {updated, _} = delete_events_older_than(mim2(), MaxId),
    {error, not_found} = get_host_type(mim(), <<"example3.com">>),
    %% Resume processing events on one node
    ok = rpc(mim(), sys, resume, [service_domain_db]),
    %% The size of the table is 1
    MaxId = get_min_event_id(mim2()),
    sync(),
    %% Out of sync detected.
    %% Called halt node function, but it's mocked
    true = rpc(mim(), meck, num_calls, [mongoose_domain_utils, halt_node, 1]) > 0,
    ok.

cli_can_insert_domain(Config) ->
    {"Added\n", 0} =
        ejabberdctl("insert_domain", [<<"example.db">>, <<"type1">>], Config),
    {ok, #{host_type := <<"type1">>, enabled := true}} =
        select_domain(mim(), <<"example.db">>).

cli_can_disable_domain(Config) ->
    ejabberdctl("insert_domain", [<<"example.db">>, <<"type1">>], Config),
    ejabberdctl("disable_domain", [<<"example.db">>], Config),
    {ok, #{host_type := <<"type1">>, enabled := false}} =
        select_domain(mim(), <<"example.db">>).

cli_can_enable_domain(Config) ->
    ejabberdctl("insert_domain", [<<"example.db">>, <<"type1">>], Config),
    ejabberdctl("disable_domain", [<<"example.db">>], Config),
    ejabberdctl("enable_domain", [<<"example.db">>], Config),
    {ok, #{host_type := <<"type1">>, enabled := true}} =
        select_domain(mim(), <<"example.db">>).

cli_can_delete_domain(Config) ->
    ejabberdctl("insert_domain", [<<"example.db">>, <<"type1">>], Config),
    ejabberdctl("delete_domain", [<<"example.db">>, <<"type1">>], Config),
    {error, not_found} = select_domain(mim(), <<"example.db">>).

cli_cannot_delete_domain_without_correct_type(Config) ->
    {"Added\n", 0} =
        ejabberdctl("insert_domain", [<<"example.db">>, <<"type1">>], Config),
    {"Error: \"Wrong host type\"\n", 1} =
        ejabberdctl("delete_domain", [<<"example.db">>, <<"type2">>], Config),
    {ok, _} = select_domain(mim(), <<"example.db">>).

cli_cannot_insert_domain_twice_with_another_host_type(Config) ->
    {"Added\n", 0} =
        ejabberdctl("insert_domain", [<<"example.db">>, <<"type1">>], Config),
    {"Error: \"Domain already exists\"\n", 1} =
        ejabberdctl("insert_domain", [<<"example.db">>, <<"type2">>], Config).

cli_cannot_insert_domain_with_unknown_host_type(Config) ->
    {"Error: \"Unknown host type\"\n", 1} =
        ejabberdctl("insert_domain", [<<"example.db">>, <<"type6">>], Config).

cli_cannot_enable_missing_domain(Config) ->
    {"Error: \"Domain not found\"\n", 1} =
        ejabberdctl("enable_domain", [<<"example.db">>], Config).

cli_cannot_disable_missing_domain(Config) ->
    {"Error: \"Domain not found\"\n", 1} =
        ejabberdctl("disable_domain", [<<"example.db">>], Config).

rest_can_insert_domain(Config) ->
    {{<<"204">>, _}, _} =
        rest_put_domain(<<"example.db">>, <<"type1">>),
    {ok, #{host_type := <<"type1">>, enabled := true}} =
        select_domain(mim(), <<"example.db">>).

rest_can_disable_domain(Config) ->
    rest_put_domain(<<"example.db">>, <<"type1">>),
    rest_patch_enabled(<<"example.db">>, false),
    {ok, #{host_type := <<"type1">>, enabled := false}} =
        select_domain(mim(), <<"example.db">>).

rest_can_delete_domain(Config) ->
    rest_put_domain(<<"example.db">>, <<"type1">>),
    {{<<"204">>, _}, _} =
        rest_delete_domain(<<"example.db">>, <<"type1">>),
    {error, not_found} = select_domain(mim(), <<"example.db">>).

rest_cannot_delete_domain_without_correct_type(Config) ->
    rest_put_domain(<<"example.db">>, <<"type1">>),
    {{<<"403">>, <<"Forbidden">>},
     {[{<<"what">>, <<"wrong host type">>}]}} =
        rest_delete_domain(<<"example.db">>, <<"type2">>),
    {ok, _} = select_domain(mim(), <<"example.db">>).

rest_delete_missing_domain(Config) ->
    {{<<"204">>, _}, _} =
        rest_delete_domain(<<"example.db">>, <<"type1">>).

rest_cannot_enable_missing_domain(Config) ->
    {{<<"404">>, <<"Not Found">>},
     {[{<<"what">>, <<"domain not found">>}]}} =
        rest_patch_enabled(<<"example.db">>, true).

rest_cannot_insert_domain_twice_with_another_host_type(Config) ->
    rest_put_domain(<<"example.db">>, <<"type1">>),
    {{<<"409">>, <<"Conflict">>}, {[{<<"what">>, <<"duplicate">>}]}} =
        rest_put_domain(<<"example.db">>, <<"type2">>).

rest_cannot_insert_domain_with_unknown_host_type(Config) ->
    {{<<"403">>,<<"Forbidden">>}, {[{<<"what">>, <<"unknown host type">>}]}} =
        rest_put_domain(<<"example.db">>, <<"type6">>).

rest_cannot_disable_missing_domain(Config) ->
    {{<<"404">>, <<"Not Found">>},
     {[{<<"what">>, <<"domain not found">>}]}} =
        rest_patch_enabled(<<"example.db">>, false).

rest_can_enable_domain(Config) ->
    rest_put_domain(<<"example.db">>, <<"type1">>),
    rest_patch_enabled(<<"example.db">>, false),
    rest_patch_enabled(<<"example.db">>, true),
    {ok, #{host_type := <<"type1">>, enabled := true}} =
        select_domain(mim(), <<"example.db">>).

rest_can_select_domain(Config) ->
    rest_put_domain(<<"example.db">>, <<"type1">>),
    {{<<"200">>, <<"OK">>},
     {[{<<"host_type">>, <<"type1">>}, {<<"enabled">>, true}]}} =
        rest_select_domain(<<"example.db">>).

rest_cannot_put_domain_without_host_type(Config) ->
    {{<<"400">>, <<"Bad Request">>},
     {[{<<"what">>, <<"'host_type' field is missing">>}]}} =
        rest_helper:putt(admin, <<"/domains/example.db">>, #{}).

rest_cannot_put_domain_without_body(Config) ->
    {{<<"400">>,<<"Bad Request">>},
     {[{<<"what">>,<<"body is empty">>}]}} =
        rest_helper:putt(admin, <<"/domains/example.db">>, <<>>).

rest_cannot_put_domain_with_invalid_json(Config) ->
    {{<<"400">>,<<"Bad Request">>},
     {[{<<"what">>,<<"failed to parse JSON">>}]}} =
        rest_helper:putt(admin, <<"/domains/example.db">>, <<"{kek">>).

rest_cannot_delete_domain_without_host_type(Config) ->
    {{<<"400">>, <<"Bad Request">>},
     {[{<<"what">>, <<"'host_type' field is missing">>}]}} =
        delete_custom(admin, <<"/domains/example.db">>, #{}).

rest_cannot_delete_domain_without_body(Config) ->
    {{<<"400">>,<<"Bad Request">>},
     {[{<<"what">>,<<"body is empty">>}]}} =
        delete_custom(admin, <<"/domains/example.db">>, <<>>).

rest_cannot_delete_domain_with_invalid_json(Config) ->
    {{<<"400">>,<<"Bad Request">>},
     {[{<<"what">>,<<"failed to parse JSON">>}]}} =
        delete_custom(admin, <<"/domains/example.db">>, <<"{kek">>).

rest_cannot_patch_domain_without_enabled_field(Config) ->
    {{<<"400">>, <<"Bad Request">>},
     {[{<<"what">>, <<"'enabled' field is missing">>}]}} =
        patch_custom(admin, <<"/domains/example.db">>, #{}).

rest_cannot_patch_domain_without_body(Config) ->
    {{<<"400">>,<<"Bad Request">>},
     {[{<<"what">>,<<"body is empty">>}]}} =
        patch_custom(admin, <<"/domains/example.db">>, <<>>).

rest_cannot_patch_domain_with_invalid_json(Config) ->
    {{<<"400">>,<<"Bad Request">>},
     {[{<<"what">>,<<"failed to parse JSON">>}]}} =
        patch_custom(admin, <<"/domains/example.db">>, <<"{kek">>).

%% SQL query is mocked to fail
rest_insert_domain_fails_if_db_fails(Config) ->
    {{<<"500">>, <<"Internal Server Error">>},
     {[{<<"what">>, <<"database error">>}]}} =
        rest_put_domain(<<"example.db">>, <<"type1">>).

rest_insert_domain_fails_if_service_disabled(Config) ->
    service_disabled(mim()),
    {{<<"403">>, <<"Forbidden">>},
     {[{<<"what">>, <<"service disabled">>}]}} =
        rest_put_domain(<<"example.db">>, <<"type1">>).

%% SQL query is mocked to fail
rest_delete_domain_fails_if_db_fails(Config) ->
    {{<<"500">>, <<"Internal Server Error">>},
     {[{<<"what">>, <<"database error">>}]}} =
        rest_delete_domain(<<"example.db">>, <<"type1">>).

rest_delete_domain_fails_if_service_disabled(Config) ->
    service_disabled(mim()),
    {{<<"403">>, <<"Forbidden">>},
     {[{<<"what">>, <<"service disabled">>}]}} =
        rest_delete_domain(<<"example.db">>, <<"type1">>).

%% SQL query is mocked to fail
rest_enable_domain_fails_if_db_fails(Config) ->
    {{<<"500">>, <<"Internal Server Error">>},
     {[{<<"what">>, <<"database error">>}]}} =
        rest_patch_enabled(<<"example.db">>, true).

rest_enable_domain_fails_if_service_disabled(Config) ->
    service_disabled(mim()),
    {{<<"403">>, <<"Forbidden">>},
     {[{<<"what">>, <<"service disabled">>}]}} =
        rest_patch_enabled(<<"example.db">>, true).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

service_enabled(Node) ->
    service_enabled(Node, []).

service_enabled(Node, Opts) ->
    rpc(Node, mongoose_service, start_service, [service_domain_db, Opts]),
    true = rpc(Node, service_domain_db, enabled, []).

service_disabled(Node) ->
    rpc(Node, mongoose_service, stop_service, [service_domain_db]),
    false = rpc(Node, service_domain_db, enabled, []).

init_with(Node, Pairs, AllowedHostTypes) ->
    rpc(Node, mongoose_domain_core, stop, []),
    rpc(Node, mongoose_domain_api, init, [Pairs, AllowedHostTypes]).

insert_domain(Node, Domain, HostType) ->
    rpc(Node, mongoose_domain_api, insert_domain, [Domain, HostType]).

delete_domain(Node, Domain, HostType) ->
    rpc(Node, mongoose_domain_api, delete_domain, [Domain, HostType]).

select_domain(Node, Domain) ->
    rpc(Node, mongoose_domain_sql, select_domain, [Domain]).

erase_database(Node) ->
    case mongoose_helper:is_rdbms_enabled(domain()) of
        true -> rpc(Node, mongoose_domain_sql, erase_database, []);
        false -> ok
    end.

prepare_erase(Node) ->
    case mongoose_helper:is_rdbms_enabled(domain()) of
        true -> rpc(Node, mongoose_domain_sql, prepare_erase, []);
        false -> ok
    end.

get_min_event_id(Node) ->
    rpc(Node, mongoose_domain_sql, get_min_event_id, []).

get_max_event_id(Node) ->
    rpc(Node, mongoose_domain_sql, get_max_event_id, []).

delete_events_older_than(Node, Id) ->
    rpc(Node, mongoose_domain_sql, delete_events_older_than, [Id]).

get_host_type(Node, Domain) ->
    rpc(Node, mongoose_domain_api, get_host_type, [Domain]).

get_domains_by_host_type(Node, HostType) ->
    rpc(Node, mongoose_domain_api, get_domains_by_host_type, [HostType]).

get_all_static(Node) ->
    rpc(Node, mongoose_domain_api, get_all_static, []).

disable_domain(Node, Domain) ->
    rpc(Node, mongoose_domain_api, disable_domain, [Domain]).

enable_domain(Node, Domain) ->
    rpc(Node, mongoose_domain_api, enable_domain, [Domain]).

is_static(Domain) ->
    rpc(mim(), mongoose_domain_core, is_static, [Domain]).

%% Call sync before get_host_type, if there are some async changes expected
sync() ->
    rpc(mim(), service_domain_db, sync, []).

sync_local(Node) ->
    rpc(Node, service_domain_db, sync_local, []).

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

setup_meck(sql_insert_domain_fails) ->
    ok = rpc(mim(), meck, new, [mongoose_domain_sql, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_domain_sql, insert_domain, 2, {error, {db_error, simulated_db_error}}]);
setup_meck(sql_delete_domain_fails) ->
    ok = rpc(mim(), meck, new, [mongoose_domain_sql, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_domain_sql, delete_domain, 2, {error, {db_error, simulated_db_error}}]);
setup_meck(sql_enable_domain_fails) ->
    ok = rpc(mim(), meck, new, [mongoose_domain_sql, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_domain_sql, enable_domain, 1, {error, {db_error, simulated_db_error}}]);
setup_meck(db_initial_load_crashes_node) ->
    ok = rpc(mim(), meck, new, [mongoose_domain_sql, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_domain_sql, select_from, 2, something_strange]),
    ok = rpc(mim(), meck, new, [mongoose_domain_utils, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_domain_utils, halt_node, 1, ok]);
setup_meck(db_out_of_sync_crashes_node) ->
    ok = rpc(mim(), meck, new, [mongoose_domain_utils, [passthrough, no_link]]),
    ok = rpc(mim(), meck, expect, [mongoose_domain_utils, halt_node, 1, ok]).

teardown_meck() ->
    rpc(mim(), meck, unload, []).

rest_patch_enabled(Domain, Enabled) ->
    Params = #{enabled => Enabled},
    rest_helper:make_request(#{ role => admin, method => <<"PATCH">>,
                                path => <<"/domains/", Domain/binary>>,
                                body => Params }).

rest_put_domain(Domain, Type) ->
    Params = #{host_type => Type},
    rest_helper:putt(admin, <<"/domains/", Domain/binary>>, Params).

rest_select_domain(Domain) ->
    rest_helper:gett(admin, <<"/domains/", Domain/binary>>, #{}).

rest_delete_domain(Domain, HostType) ->
    Params = #{<<"host_type">> => HostType},
    rest_helper:make_request(#{ role => admin, method => <<"DELETE">>,
                                path => <<"/domains/", Domain/binary>>,
                                body => Params }).

delete_custom(Role, Path, Body) ->
    rest_helper:make_request(#{ role => Role, method => <<"DELETE">>,
                                path => Path,
                                body => Body }).

patch_custom(Role, Path, Body) ->
    rest_helper:make_request(#{ role => Role, method => <<"PATCH">>,
                                path => Path,
                                body => Body }).
