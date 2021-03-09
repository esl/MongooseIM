-module(service_domain_db_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).
-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).

suite() ->
    require_rpc_nodes([mim]).

all() ->
    [
     core_lookup_works,
     core_lookup_not_found,
     core_locked_domain,
     core_cannot_insert_locked,
     core_cannot_disable_locked,
     core_cannot_enable_locked,
     db_inserted_domain_is_in_db,
     db_inserted_domain_is_in_core,
     db_removed_domain_from_db,
     db_removed_domain_from_core,
     db_disabled_domain_is_in_db,
     db_disabled_domain_not_in_core,
     db_reanabled_domain_is_in_db,
     db_reanabled_domain_is_in_core,
     db_can_insert_domain_twice_with_the_same_host_type,
     db_cannot_insert_domain_twice_with_the_another_host_type,
     sql_select_from_works,
     db_records_are_restored_when_restarted,
     db_record_is_ignored_if_domain_locked,
     db_events_table_gets_truncated
    ].

-define(APPS, [inets, crypto, ssl, ranch, cowlib, cowboy]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    prepare_erase(mim()),
    Dump = dump(mim()),
    Loaded = rpc(mim(), mongoose_service, is_loaded, [service_domain_db]),
    ServiceOpts = rpc(mim(), mongoose_service, get_service_opts, [service_domain_db]),
    escalus:init_per_suite([{orig_dump, Dump},
                            {orig_service_opts, ServiceOpts},
                            {orig_loaded, Loaded}|Config]).

end_per_suite(Config) ->
    Dump = proplists:get_value(orig_dump, Config),
    ServiceOpts = proplists:get_value(orig_service_opts, Config),
    Loaded = proplists:get_value(orig_loaded, Config),
    restore(mim(), Dump),
    rpc(mim(), mongoose_service, stop_service, [service_domain_db]),
    case Loaded of
        true ->
            rpc(mim(), mongoose_service, start_service, [service_domain_db, ServiceOpts]);
        _ ->
            ok
    end,
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
    precond(off, [<<"example.com">>, <<"type1">>]),
    {ok, <<"type1">>} = get_host_type(mim(), <<"example.com">>).

core_lookup_not_found(_) ->
    precond(off),
    {error, not_found} = get_host_type(mim(), <<"example.life">>).

core_locked_domain(_) ->
    precond(off, [<<"example.com">>, <<"type1">>]),
    true = is_locked(<<"example.com">>).

core_cannot_insert_locked(_) ->
    precond(off, [<<"example.com">>, <<"type1">>]),
    {error, duplicate} = insert_domain(mim(), <<"example.com">>, <<"type1">>).

core_cannot_disable_locked(_) ->
    precond(off, [<<"example.com">>, <<"type1">>]),
    {error, locked} = disable_domain(mim(), <<"example.com">>).

core_cannot_enable_locked(_) ->
    precond(off, [<<"example.com">>, <<"type1">>]),
    {error, locked} = enable_domain(mim(), <<"example.com">>).

db_inserted_domain_is_in_db(_) ->
    precond(on),
    ok = insert_domain(mim(), <<"example.com">>, <<"testing">>),
    {ok, #{host_type := <<"testing">>, enabled := true}} =
       select_domain(mim(), <<"example.com">>).

db_inserted_domain_is_in_core(_) ->
    precond(on),
    ok = insert_domain(mim(), <<"example.com">>, <<"testing">>),
    sync(),
    {ok, <<"testing">>} = get_host_type(mim(), <<"example.com">>).

db_removed_domain_from_db(_) ->
    precond(on),
    ok = insert_domain(mim(), <<"example.com">>, <<"testing">>),
    ok = remove_domain(mim(), <<"example.com">>),
    {error, not_found} = select_domain(mim(), <<"example.com">>).

db_removed_domain_from_core(_) ->
    precond(on),
    ok = insert_domain(mim(), <<"example.com">>, <<"testing">>),
    sync(),
    ok = remove_domain(mim(), <<"example.com">>),
    sync(),
    {error, not_found} = get_host_type(mim(), <<"example.com">>).

db_disabled_domain_is_in_db(_) ->
    precond(on),
    ok = insert_domain(mim(), <<"example.com">>, <<"type1">>),
    ok = disable_domain(mim(), <<"example.com">>),
    {ok, #{host_type := <<"type1">>, enabled := false}} =
       select_domain(mim(), <<"example.com">>).

db_disabled_domain_not_in_core(_) ->
    precond(on),
    ok = insert_domain(mim(), <<"example.com">>, <<"type1">>),
    ok = disable_domain(mim(), <<"example.com">>),
    sync(),
    {error, not_found} = get_host_type(mim(), <<"example.com">>).

db_reanabled_domain_is_in_db(_) ->
    precond(on),
    ok = insert_domain(mim(), <<"example.com">>, <<"type1">>),
    ok = disable_domain(mim(), <<"example.com">>),
    ok = enable_domain(mim(), <<"example.com">>),
    {ok, #{host_type := <<"type1">>, enabled := true}} =
       select_domain(mim(), <<"example.com">>).

db_reanabled_domain_is_in_core(_) ->
    precond(on),
    ok = insert_domain(mim(), <<"example.com">>, <<"type1">>),
    ok = disable_domain(mim(), <<"example.com">>),
    ok = enable_domain(mim(), <<"example.com">>),
    sync(),
    {ok, <<"type1">>} = get_host_type(mim(), <<"example.com">>).

db_can_insert_domain_twice_with_the_same_host_type(_) ->
    precond(on),
    ok = insert_domain(mim(), <<"example.com">>, <<"testing">>),
    ok = insert_domain(mim(), <<"example.com">>, <<"testing">>).

db_cannot_insert_domain_twice_with_the_another_host_type(_) ->
    precond(on),
    ok = insert_domain(mim(), <<"example.com">>, <<"testing">>),
    {error, duplicate} = insert_domain(mim(), <<"example.com">>, <<"testing2">>).

sql_select_from_works(_) ->
    precond(on),
    ok = insert_domain(mim(), <<"example.com">>, <<"good">>),
    [{_, <<"example.com">>, <<"good">>}] =
       rpc(mim(), mongoose_domain_sql, select_from, [0, 100]).

db_records_are_restored_when_restarted(_) ->
    precond(on),
    ok = insert_domain(mim(), <<"example.com">>, <<"cool">>),
    %% Simulate MIM restart
    service_disabled(),
    init_with([]),
    {error, not_found} = get_host_type(mim(), <<"example.com">>),
    service_enabled(),
    %% DB still contains data
    {ok, #{host_type := <<"cool">>, enabled := true}} =
       select_domain(mim(), <<"example.com">>),
    %% Restored
    {ok, <<"cool">>} = get_host_type(mim(), <<"example.com">>).

db_record_is_ignored_if_domain_locked(_) ->
    precond(on),
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    ok = insert_domain(mim(), <<"example.net">>, <<"dbgroup">>),
    %% Simulate MIM restart
    service_disabled(),
    %% Only one domain is locked
    init_with([{<<"example.com">>, <<"cfggroup">>}]),
    service_enabled(),
    %% DB still contains data
    {ok, #{host_type := <<"dbgroup">>, enabled := true}} =
       select_domain(mim(), <<"example.com">>),
    {ok, #{host_type := <<"dbgroup">>, enabled := true}} =
       select_domain(mim(), <<"example.net">>),
     %% Locked DB records are ignored
    {ok, <<"cfggroup">>} = get_host_type(mim(), <<"example.com">>),
    {ok, <<"dbgroup">>} = get_host_type(mim(), <<"example.net">>).

db_events_table_gets_truncated(_) ->
    precond(off),
    %% Configure service with a very short interval
    service_enabled([{event_cleaning_interval, 1}, {event_max_age, 3}]),
    ok = insert_domain(mim(), <<"example.com">>, <<"dbgroup">>),
    ok = insert_domain(mim(), <<"example.net">>, <<"dbgroup">>),
    ok = insert_domain(mim(), <<"example.org">>, <<"dbgroup">>),
    ok = insert_domain(mim(), <<"example.beta">>, <<"dbgroup">>),
    Max = get_max_event_id(mim()),
    true = is_integer(Max),
    %% The events table is not empty and the size of 1, eventually.
    F = fun() -> get_min_event_id(mim()) end,
    mongoose_helper:wait_until(F, Max, #{time_left => timer:seconds(15)}),
    ok.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

service_enabled() ->
    service_enabled([]).

service_enabled(Opts) ->
    rpc(mim(), mongoose_service, start_service, [service_domain_db, Opts]),
    true = rpc(mim(), service_domain_db, enabled, []).

service_disabled() ->
    rpc(mim(), mongoose_service, stop_service, [service_domain_db]),
    false = rpc(mim(), service_domain_db, enabled, []).

init_with(Pairs) ->
    rpc(mim(), mongoose_domain_core, stop, []),
    rpc(mim(), mongoose_domain_api, init, [Pairs]).

insert_domain(Node, Domain, HostType) ->
    rpc(Node, mongoose_domain_api, insert_domain, [Domain, HostType]).

remove_domain(Node, Domain) ->
    rpc(Node, mongoose_domain_api, remove_domain, [Domain]).

select_domain(Node, Domain) ->
    rpc(Node, mongoose_domain_sql, select_domain, [Domain]).

erase_database(Node) ->
    rpc(Node, mongoose_domain_sql, erase_database, []).

prepare_erase(Node) ->
    rpc(Node, mongoose_domain_sql, prepare_erase, []).

get_min_event_id(Node) ->
    rpc(Node, mongoose_domain_sql, get_min_event_id, []).

get_max_event_id(Node) ->
    rpc(Node, mongoose_domain_sql, get_max_event_id, []).

dump(Node) ->
    rpc(Node, mongoose_domain_core, dump, []).

restore(Node, Dump) ->
    rpc(Node, mongoose_domain_core, restore, [Dump]).

get_host_type(Node, Domain) ->
    rpc(Node, mongoose_domain_api, get_host_type, [Domain]).

disable_domain(Node, Domain) ->
    rpc(Node, mongoose_domain_api, disable_domain, [Domain]).

enable_domain(Node, Domain) ->
    rpc(Node, mongoose_domain_api, enable_domain, [Domain]).

is_locked(Domain) ->
    rpc(mim(), mongoose_domain_core, is_locked, [Domain]).

%% Call sync before get_host_type, if there are some async changes expected
sync() ->
    rpc(mim(), service_domain_db, sync, []).

precond(Enabled) ->
    precond(Enabled, []).

precond(off, FlatPairs) ->
    service_disabled(),
    erase_database(mim()),
    init_with(unflat(FlatPairs));
precond(on, FlatPairs) ->
    service_disabled(),
    erase_database(mim()),
    service_enabled(),
    init_with(unflat(FlatPairs)).

unflat([K,V|T]) ->
    [{K,V}|unflat(T)];
unflat([]) ->
    [].
