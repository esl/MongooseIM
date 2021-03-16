-module(service_domain_db_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-import(distributed_helper, [mim/0, mim2/0, require_rpc_nodes/1, rpc/4]).

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
     core_get_domains_by_host_type
    ] ++ maybe_db_cases().

db_cases() -> [
     db_inserted_domain_is_in_db,
     db_inserted_domain_is_in_core,
     db_removed_domain_from_db,
     db_removed_domain_fails_with_wrong_host_type,
     db_removed_domain_from_core,
     db_disabled_domain_is_in_db,
     db_disabled_domain_not_in_core,
     db_reanabled_domain_is_in_db,
     db_reanabled_domain_is_in_core,
     db_can_insert_domain_twice_with_the_same_host_type,
     db_cannot_insert_domain_twice_with_the_another_host_type,
     db_cannot_insert_domain_with_unknown_host_type,
     db_cannot_remove_domain_with_unknown_host_type,
     db_cannot_enable_domain_with_unknown_host_type,
     db_cannot_disable_domain_with_unknown_host_type,
     db_domains_with_unknown_host_type_are_ignored_by_core,
     sql_select_from_works,
     db_records_are_restored_when_restarted,
     db_record_is_ignored_if_domain_static,
     db_events_table_gets_truncated,
     db_get_all_static,
     db_could_sync_between_nodes,
     db_removed_from_one_node_while_service_disabled_on_another,
     db_inserted_from_one_node_while_service_disabled_on_another,
     db_reinserted_from_one_node_while_service_disabled_on_another
    ].

-define(APPS, [inets, crypto, ssl, ranch, cowlib, cowboy]).

maybe_db_cases() ->
    case mongoose_helper:is_rdbms_enabled(domain()) of
        true -> db_cases();
        false -> []
    end.

domain() -> ct:get_config({hosts, mim, domain}).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    ensure_nodes_know_each_other(),
    prepare_erase(mim()),
    prepare_erase(mim2()),
    Conf1 = store_conf(mim()),
    Conf2 = store_conf(mim2()),
    escalus:init_per_suite([{mim_conf1, Conf1}, {mim_conf2, Conf2}|Config]).

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
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_TestcaseName, Config) ->
    Config.

end_per_testcase(_TestcaseName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

core_lookup_works(_) ->
    precond(mim(), off, [<<"example.com">>, <<"type1">>], [<<"type1">>]),
    {ok, <<"type1">>} = get_host_type(mim(), <<"example.com">>).

core_lookup_not_found(_) ->
    precond(mim(), off, [], []),
    {error, not_found} = get_host_type(mim(), <<"example.life">>).

core_static_domain(_) ->
    precond(mim(), off, [<<"example.com">>, <<"type1">>], [<<"type1">>]),
    true = is_static(<<"example.com">>).

core_cannot_insert_static(_) ->
    precond(mim(), off, [<<"example.com">>, <<"type1">>], [<<"type1">>]),
    {error, static} = insert_domain(mim(), <<"example.com">>, <<"type1">>).

core_cannot_disable_static(_) ->
    precond(mim(), off, [<<"example.com">>, <<"type1">>], [<<"type1">>]),
    {error, static} = disable_domain(mim(), <<"example.com">>).

core_cannot_enable_static(_) ->
    precond(mim(), off, [<<"example.com">>, <<"type1">>], [<<"type1">>]),
    {error, static} = enable_domain(mim(), <<"example.com">>).

%% See also db_get_all_static
core_get_all_static(_) ->
    precond(mim(), off, [<<"example.com">>, <<"type1">>,
                  <<"example.org">>, <<"type2">>,
                  <<"erlang-solutions.com">>, <<"type2">>],
           [<<"type1">>, <<"type2">>]),
    %% Could be in any order
    [{<<"erlang-solutions.com">>, <<"type2">>},
     {<<"example.com">>, <<"type1">>},
     {<<"example.org">>, <<"type2">>}] =
        lists:sort(get_all_static(mim())).

core_get_domains_by_host_type(_) ->
    precond(mim(), off, [<<"example.com">>, <<"type1">>,
                  <<"example.org">>, <<"type2">>,
                  <<"erlang-solutions.com">>, <<"type2">>],
           [<<"type1">>, <<"type2">>]),
    [<<"erlang-solutions.com">>, <<"example.org">>] =
        lists:sort(get_domains_by_host_type(mim(), <<"type2">>)),
    [<<"example.com">>] = get_domains_by_host_type(mim(), <<"type1">>),
    [] = get_domains_by_host_type(mim(), <<"type6">>).

%% Similar to as core_get_all_static, just with DB service enabled
db_get_all_static(_) ->
    precond(mim(), on, [<<"example.com">>, <<"type1">>,
                 <<"example.org">>, <<"type2">>,
                 <<"erlang-solutions.com">>, <<"type2">>],
           [<<"type1">>, <<"type2">>]),
    ok = insert_domain(mim(), <<"example.db">>, <<"type1">>),
    sync(),
    %% Could be in any order
    [{<<"erlang-solutions.com">>, <<"type2">>},
     {<<"example.com">>, <<"type1">>},
     {<<"example.org">>, <<"type2">>}] =
        lists:sort(get_all_static(mim())).

db_inserted_domain_is_in_db(_) ->
    precond(mim(), on, [], [<<"testing">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"testing">>),
    {ok, #{host_type := <<"testing">>, enabled := true}} =
       select_domain(mim(), <<"example.com">>).

db_inserted_domain_is_in_core(_) ->
    precond(mim(), on, [], [<<"testing">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"testing">>),
    sync(),
    {ok, <<"testing">>} = get_host_type(mim(), <<"example.com">>).

db_removed_domain_from_db(_) ->
    precond(mim(), on, [], [<<"testing">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"testing">>),
    ok = remove_domain(mim(), <<"example.com">>, <<"testing">>),
    {error, not_found} = select_domain(mim(), <<"example.com">>).

db_removed_domain_fails_with_wrong_host_type(_) ->
    precond(mim(), on, [], [<<"testing">>, <<"testing2">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"testing">>),
    {error, wrong_host_type} =
        remove_domain(mim(), <<"example.com">>, <<"testing2">>),
    {ok, #{host_type := <<"testing">>, enabled := true}} =
        select_domain(mim(), <<"example.com">>).

db_removed_domain_from_core(_) ->
    precond(mim(), on, [], [<<"testing">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"testing">>),
    sync(),
    ok = remove_domain(mim(), <<"example.com">>, <<"testing">>),
    sync(),
    {error, not_found} = get_host_type(mim(), <<"example.com">>).

db_disabled_domain_is_in_db(_) ->
    precond(mim(), on, [], [<<"type1">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"type1">>),
    ok = disable_domain(mim(), <<"example.com">>),
    {ok, #{host_type := <<"type1">>, enabled := false}} =
       select_domain(mim(), <<"example.com">>).

db_disabled_domain_not_in_core(_) ->
    precond(mim(), on, [], [<<"type1">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"type1">>),
    ok = disable_domain(mim(), <<"example.com">>),
    sync(),
    {error, not_found} = get_host_type(mim(), <<"example.com">>).

db_reanabled_domain_is_in_db(_) ->
    precond(mim(), on, [], [<<"type1">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"type1">>),
    ok = disable_domain(mim(), <<"example.com">>),
    ok = enable_domain(mim(), <<"example.com">>),
    {ok, #{host_type := <<"type1">>, enabled := true}} =
       select_domain(mim(), <<"example.com">>).

db_reanabled_domain_is_in_core(_) ->
    precond(mim(), on, [], [<<"type1">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"type1">>),
    ok = disable_domain(mim(), <<"example.com">>),
    ok = enable_domain(mim(), <<"example.com">>),
    sync(),
    {ok, <<"type1">>} = get_host_type(mim(), <<"example.com">>).

db_can_insert_domain_twice_with_the_same_host_type(_) ->
    precond(mim(), on, [], [<<"testing">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"testing">>),
    ok = insert_domain(mim(), <<"example.com">>, <<"testing">>).

db_cannot_insert_domain_twice_with_the_another_host_type(_) ->
    precond(mim(), on, [], [<<"testing">>, <<"testing2">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"testing">>),
    {error, duplicate} = insert_domain(mim(), <<"example.com">>, <<"testing2">>).

db_cannot_insert_domain_with_unknown_host_type(_) ->
    precond(mim(), on, [], [<<"testing">>]),
    {error, unknown_host_type} = insert_domain(mim(), <<"example.com">>, <<"nesting">>).

db_cannot_remove_domain_with_unknown_host_type(_) ->
    precond(mim(), on, [], [<<"testing">>, <<"oldie">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"oldie">>),
    %% The host type has been removed from the configuration.
    precond(mim(), on, [], [<<"testing">>]),
    %% Nope. You can't touch oldies.
    {error, unknown_host_type} = remove_domain(mim(), <<"example.com">>, <<"oldie">>).

db_cannot_enable_domain_with_unknown_host_type(_) ->
    precond(mim(), on, [], [<<"testing">>, <<"oldie">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"oldie">>),
    ok = disable_domain(mim(), <<"example.com">>),
    %% The host type has been removed from the configuration.
    precond(mim(), keep_on, [], [<<"testing">>]),
    %% Nope. You can't touch oldies.
    {error, unknown_host_type} = enable_domain(mim(), <<"example.com">>).

db_cannot_disable_domain_with_unknown_host_type(_) ->
    precond(mim(), on, [], [<<"testing">>, <<"oldie">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"oldie">>),
    %% The host type has been removed from the configuration.
    precond(mim(), keep_on, [], [<<"testing">>]),
    %% Nope. You can't touch oldies.
    {error, unknown_host_type} = disable_domain(mim(), <<"example.com">>).

db_domains_with_unknown_host_type_are_ignored_by_core(_) ->
    precond(mim(), on, [], [<<"testing">>, <<"oldie">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"oldie">>),
    ok = insert_domain(mim(), <<"example.org">>, <<"testing">>),
    %% The host type has been removed from the configuration.
    precond(mim(), keep_on, [], [<<"testing">>]),
    sync(),
    {ok, <<"testing">>} = get_host_type(mim(), <<"example.org">>), %% Counter-case
    {error, not_found} = get_host_type(mim(), <<"example.com">>).

sql_select_from_works(_) ->
    precond(mim(), on, [], [<<"good">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"good">>),
    [{_, <<"example.com">>, <<"good">>}] =
       rpc(mim(), mongoose_domain_sql, select_from, [0, 100]).

db_records_are_restored_when_restarted(_) ->
    precond(mim(), on, [], [<<"cool">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"cool">>),
    %% Simulate MIM restart
    service_disabled(mim()),
    init_with(mim(), [], [<<"cool">>]),
    {error, not_found} = get_host_type(mim(), <<"example.com">>),
    service_enabled(mim()),
    %% DB still contains data
    {ok, #{host_type := <<"cool">>, enabled := true}} =
       select_domain(mim(), <<"example.com">>),
    %% Restored
    {ok, <<"cool">>} = get_host_type(mim(), <<"example.com">>).

db_record_is_ignored_if_domain_static(_) ->
    precond(mim(), on, [], [<<"dbgroup">>, <<"cfggroup">>]),
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
    precond(mim(), off, [], [<<"dbgroup">>]),
    %% Configure service with a very short interval
    service_enabled(mim(), [{event_cleaning_interval, 1}, {event_max_age, 3}]),
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    ok = insert_domain(mim(), <<"example.net">>, <<"dbgroup">>),
    ok = insert_domain(mim(), <<"example.org">>, <<"dbgroup">>),
    ok = insert_domain(mim(), <<"example.beta">>, <<"dbgroup">>),
    Max = get_max_event_id(mim()),
    true = is_integer(Max),
    true = Max > 0,
    %% The events table is not empty and the size of 1, eventually.
    F = fun() -> get_min_event_id(mim()) end,
    mongoose_helper:wait_until(F, Max, #{time_left => timer:seconds(15)}),
    ok.

db_could_sync_between_nodes(_) ->
    precond(mim(), on, [], [<<"dbgroup">>]),
    precond(mim2(), on, [], [<<"dbgroup">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync(),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>),
    ok.

db_removed_from_one_node_while_service_disabled_on_another(_) ->
    precond(mim(), on, [], [<<"dbgroup">>]),
    precond(mim2(), on, [], [<<"dbgroup">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync(),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>),
    %% Service is disable on the second node
    service_disabled(mim2()),
    %% Removed from the first node
    ok = remove_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync(),
    {error, not_found} = get_host_type(mim(), <<"example.com">>),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>),
    %% Sync is working again
    service_enabled(mim2()),
    {error, not_found} = get_host_type(mim2(), <<"example.com">>),
    ok.

db_inserted_from_one_node_while_service_disabled_on_another(_) ->
    precond(mim(), on, [], [<<"dbgroup">>]),
    precond(mim2(), on, [], [<<"dbgroup">>]),
    %% Service is disable on the second node
    service_disabled(mim2()),
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    %% Sync is working again
    service_enabled(mim2()),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>),
    ok.

db_reinserted_from_one_node_while_service_disabled_on_another(_) ->
    %% This test shows the behaviour when someone
    %% reinserts a domain with a different host type.
    %% TLDR: just keep the host_type constant or don't reuse domains.
    precond(mim(), on, [], [<<"dbgroup">>, <<"dbgroup2">>]),
    precond(mim2(), on, [], [<<"dbgroup">>, <<"dbgroup2">>]),
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync(),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>),
    %% Service is disable on the second node
    service_disabled(mim2()),
    %% Removed from the first node
    ok = remove_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    sync(),
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup2">>),
    sync(),
    %% Sync is working again
    service_enabled(mim2()),
    sync(),
    %% A corner case: mim2 sees the change, but core ignores it
    {ok, <<"dbgroup2">>} = get_host_type(mim(), <<"example.com">>),
    {ok, <<"dbgroup">>} = get_host_type(mim2(), <<"example.com">>),
    %% But if we remove it, it would be removed everywhere
    ok = remove_domain(mim(), <<"example.com">>, <<"dbgroup2">>),
    sync(),
    {error, not_found} = get_host_type(mim(), <<"example.com">>),
    {error, not_found} = get_host_type(mim2(), <<"example.com">>),
    ok.

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

remove_domain(Node, Domain, HostType) ->
    rpc(Node, mongoose_domain_api, remove_domain, [Domain, HostType]).

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

precond(Node, off, FlatPairs, AllowedHostTypes) ->
    service_disabled(Node),
    erase_database(Node),
    init_with(Node, unflat(FlatPairs), AllowedHostTypes);
precond(Node, on, FlatPairs, AllowedHostTypes) ->
    %% Restarts with clean DB
    service_disabled(Node),
    erase_database(Node),
    init_with(Node, unflat(FlatPairs), AllowedHostTypes),
    service_enabled(Node);
precond(Node, keep_on, FlatPairs, AllowedHostTypes) ->
    init_with(Node, unflat(FlatPairs), AllowedHostTypes),
    service_disabled(Node),
    %% Skip erase
    service_enabled(Node).

unflat([K,V|T]) ->
    [{K,V}|unflat(T)];
unflat([]) ->
    [].

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
