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
     db_cannot_insert_domain_twice_with_the_another_host_type,
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
     rest_can_insert_domain
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

db_cannot_insert_domain_twice_with_the_another_host_type(_) ->
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

rest_can_insert_domain(Config) ->
    Params = #{host_type => <<"type1">>},
    {{<<"204">>, _}, _} =
        rest_helper:putt(admin, <<"/domains/example.db">>, Params),
    {ok, #{host_type := <<"type1">>, enabled := true}} =
        select_domain(mim(), <<"example.db">>).

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
