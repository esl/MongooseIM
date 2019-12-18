-module(service_mongoose_system_metrics_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(SERVER_URL, "http://localhost:8765").
-define(ETS_TABLE, qs).

-record(event, {
    cid = "",
    ec = "",
    ea = "",
    ev = "",
    el = "" }).

%% API
-export([
         all/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2
        ]).

-export([
         system_metrics_are_not_reported_when_not_allowed/1,
         periodic_report_available/1,
         all_clustered_mongooses_report_the_same_client_id/1,
         system_metrics_are_reported_to_google_analytics_when_mim_starts/1
        ]).

-import(distributed_helper, [mim/0, mim2/0,
                             require_rpc_nodes/1
                            ]).

suite() ->
    require_rpc_nodes([mim]).

all() ->
    [
     system_metrics_are_not_reported_when_not_allowed,
     periodic_report_available,
     all_clustered_mongooses_report_the_same_client_id,
     system_metrics_are_reported_to_google_analytics_when_mim_starts
    ].

-define(APPS, [inets, crypto, ssl, fusco, ranch, cowlib, cowboy]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    case system_metrics_service_is_enabled(mim()) of
        false ->
            ct:fail("service_mongoose_system_metrics is not running");
        true ->
            [ {ok, _} = application:ensure_all_started(App) || App <- ?APPS ],
            http_helper:start(8765, "/[...]", fun handler_init/1),
            Config
    end.

end_per_suite(Config) ->
    http_helper:stop(),
    Config.

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(periodic_report_available, Config) ->
    create_events_collection(),
    enable_system_metrics(mim()),
    Config;
init_per_testcase(system_metrics_are_reported_to_google_analytics_when_mim_starts, Config) ->
    create_events_collection(),
    enable_system_metrics(mim()),
    Config;
init_per_testcase(system_metrics_are_not_reported_when_not_allowed, Config) ->
    create_events_collection(),
    disable_system_metrics(mim()),
    delete_prev_client_id(mim()),
    Config;
init_per_testcase(all_clustered_mongooses_report_the_same_client_id, Config) ->
    create_events_collection(),
    distributed_helper:add_node_to_cluster(mim2(), Config),
    enable_system_metrics(mim()),
    enable_system_metrics(mim2()),
    Config.

end_per_testcase(periodic_report_available, Config) ->
    clear_events_collection(),
    disable_system_metrics(mim()),
    delete_prev_client_id(mim()),
    Config;
end_per_testcase(system_metrics_are_reported_to_google_analytics_when_mim_starts, Config) ->
    clear_events_collection(),
    delete_prev_client_id(mim()),
    disable_system_metrics(mim()),
    Config;
end_per_testcase(system_metrics_are_not_reported_when_not_allowed, Config) ->
    clear_events_collection(),
    delete_prev_client_id(mim()),
    Config;
end_per_testcase(all_clustered_mongooses_report_the_same_client_id , Config) ->
    clear_events_collection(),
    delete_prev_client_id(mim()),
    Nodes = [mim(), mim2()],
    [ begin delete_prev_client_id(Node), disable_system_metrics(Node) end || Node <- Nodes ],
    distributed_helper:remove_node_from_cluster(mim2(), Config),
    Config.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------
system_metrics_are_not_reported_when_not_allowed(_Config) ->
    true = system_metrics_service_is_disabled(mim()).

periodic_report_available(_Config) ->
    ReportsNumber = get_events_collection_size(),
    mongoose_helper:wait_until(
        fun() ->
                NewReportsNumber = get_events_collection_size(),
                NewReportsNumber > ReportsNumber + 1
        end,
        true).

all_clustered_mongooses_report_the_same_client_id(_Config) ->
    mongoose_helper:wait_until(fun hosts_count_is_reported/0, true),
    all_event_have_the_same_client_id().


system_metrics_are_reported_to_google_analytics_when_mim_starts(_Config) ->
    mongoose_helper:wait_until(fun hosts_count_is_reported/0, true),
    mongoose_helper:wait_until(fun modules_are_reported/0, true),
    all_event_have_the_same_client_id().

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

all_event_have_the_same_client_id() ->
    Tab = ets:tab2list(?ETS_TABLE),
    UniqueSortedTab = lists:usort([Cid ||#event{cid = Cid} <- Tab]),
    1 = length(UniqueSortedTab).

hosts_count_is_reported() ->
    is_in_table(<<"hosts">>).

hosts_count_is_not_reported() ->
    is_not_in_table(<<"hosts">>).

modules_are_reported() ->
    is_in_table(<<"module">>).

modules_are_not_reported() ->
    is_not_in_table(<<"module">>).

is_in_table(EventCategory) ->
    Tab = ets:tab2list(?ETS_TABLE),
    lists:any(
        fun(#event{ec = EC}) ->
            verify_category(EC, EventCategory)
        end, Tab).

is_not_in_table(EventCategory) ->
    not is_in_table(EventCategory).

verify_category(EC, <<"module">>) ->
    Result = re:run(EC, "^mod_.*"),
    case Result of
        {match, _Captured} -> true;
        nomatch -> false
    end;
verify_category(EC, EC) ->
    true;
verify_category(_EC, _EventCategory) ->
    false.

get_events_collection_size() ->
    length(ets:tab2list(?ETS_TABLE)).

enable_system_metrics(Node) ->
    enable_system_metrics(Node, 100, 100).

enable_system_metrics(Node, InitialReport, PeriodicReport) ->
    UrlArgs = [google_analytics_url, ?SERVER_URL],
    {atomic, ok} = mongoose_helper:successful_rpc(Node, ejabberd_config, add_local_option, UrlArgs),
    distributed_helper:rpc(
      Node, mongoose_service, start_service,
    [service_mongoose_system_metrics, [{initial_report, InitialReport}, {periodic_report, PeriodicReport}]]).

disable_system_metrics(Node) ->
    distributed_helper:rpc(Node, mongoose_service, stop_service, [service_mongoose_system_metrics]),
    mongoose_helper:successful_rpc(Node, ejabberd_config, del_local_option, [ google_analytics_url ]).

delete_prev_client_id(Node) ->
    mongoose_helper:successful_rpc(Node, mnesia, delete_table, [service_mongoose_system_metrics]).

create_events_collection() ->
    ets:new(?ETS_TABLE, [duplicate_bag, named_table, public]).

clear_events_collection() ->
    ets:delete_all_objects(?ETS_TABLE).

system_metrics_service_is_enabled(Node) ->
    Pid = distributed_helper:rpc(Node, erlang, whereis, [service_mongoose_system_metrics]),
    erlang:is_pid(Pid).

system_metrics_service_is_disabled(Node) ->
    not system_metrics_service_is_enabled(Node).
%%--------------------------------------------------------------------
%% Cowboy handlers
%%--------------------------------------------------------------------
handler_init(Req0) ->
    {ok, Body, Req} = cowboy_req:read_body(Req0),
    StrEvents = string:split(Body, "\n", all),
    lists:map(
        fun(StrEvent) ->
            Event = str_to_event(StrEvent),
            ets:insert(?ETS_TABLE, Event)
        end, StrEvents),
    Req1 = cowboy_req:reply(200, #{}, <<"">>, Req),
    {ok, Req1, no_state}.

str_to_event(Qs) ->
    StrParams = string:split(Qs, "&", all),
    Params = lists:map(
        fun(StrParam) ->
            [StrKey, StrVal] = string:split(StrParam, "="),
            {binary_to_atom(StrKey, utf8), StrVal}
        end, StrParams),
    #event{
        cid = get_el(cid, Params),
        ec = get_el(ec, Params),
        ea = get_el(ea, Params),
        el = get_el(el, Params),
        ev = get_el(ev, Params)
    }.

get_el(Key, Proplist) ->
    proplists:get_value(Key, Proplist, undef).
