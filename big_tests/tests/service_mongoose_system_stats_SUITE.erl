-module(service_mongoose_system_stats_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(SERVER_URL, "http://localhost:8765").
-define(ETS_TABLE, qs).

-record(event, {
    el = "",  % event Label
    cid = "", % client ID
    ec = "",  % event category
    ea = ""   % event action
    }).

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
         system_stats_are_not_reported_when_not_allowed/1,
         periodic_report_available/1,
         all_clustered_mongooses_report_the_same_client_id/1,
         system_stats_are_reported_to_google_analytics_when_mim_starts/1
        ]).

-import(distributed_helper, [mim/0, mim2/0,
                             require_rpc_nodes/1
                            ]).

suite() ->
    require_rpc_nodes([mim]).

all() ->
    [
     system_stats_are_not_reported_when_not_allowed,
     periodic_report_available,
     all_clustered_mongooses_report_the_same_client_id,
     system_stats_are_reported_to_google_analytics_when_mim_starts
    ].

-define(APPS, [inets, crypto, ssl, fusco, ranch, cowlib, cowboy]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    [ {ok, _} = application:ensure_all_started(App) || App <- ?APPS ],
    http_helper:start(8765, "/[...]", fun handler_init/1),
    Config.

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
    enable_system_stats(mim()),
    Config;
init_per_testcase(system_stats_are_reported_to_google_analytics_when_mim_starts, Config) ->
    create_events_collection(),
    enable_system_stats(mim()),
    Config;
init_per_testcase(system_stats_are_not_reported_when_not_allowed, Config) ->
    create_events_collection(),
    disable_system_stats(mim()),
    delete_prev_client_id(mim()),
    Config;
init_per_testcase(all_clustered_mongooses_report_the_same_client_id, Config) ->
    create_events_collection(),
    enable_system_stats(mim()),
    enable_system_stats(mim2()),
    distributed_helper:add_node_to_cluster(mim2(), Config),
    Config.

end_per_testcase(periodic_report_available, Config) ->
    clear_events_collection(),
    disable_system_stats(mim()),
    delete_prev_client_id(mim()),
    Config;
end_per_testcase(system_stats_are_reported_to_google_analytics_when_mim_starts, Config) ->
    clear_events_collection(),
    delete_prev_client_id(mim()),
    disable_system_stats(mim()),
    Config;
end_per_testcase(system_stats_are_not_reported_when_not_allowed, Config) ->
    clear_events_collection(),
    delete_prev_client_id(mim()),
    Config;
end_per_testcase(all_clustered_mongooses_report_the_same_client_id , Config) ->
    clear_events_collection(),
    delete_prev_client_id(mim()),
    Nodes = [mim(), mim2()],
    [ begin delete_prev_client_id(Node), disable_system_stats(Node) end || Node <- Nodes ],
    Config.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------
system_stats_are_not_reported_when_not_allowed(_Config) ->
    true = system_stats_service_is_disabled(mim()).

periodic_report_available(_Config) ->
    ReportsNumber = get_events_collection_size(),
    mongoose_helper:wait_until(
        fun() ->
                NewReportsNumber = get_events_collection_size(),
                NewReportsNumber > ReportsNumber + 1
        end,
        true).

all_clustered_mongooses_report_the_same_client_id(_Config) ->
    trigger_reporting(mim()),
    trigger_reporting(mim2()),
    mongoose_helper:wait_until(fun all_event_have_the_same_client_id/0, true).

system_stats_are_reported_to_google_analytics_when_mim_starts(_Config) ->
    trigger_reporting(mim()),
    % mongoose_helper:wait_until(fun no_more_events_is_reported/0, true),
    mongoose_helper:wait_until(fun hosts_count_is_reported/0, true),
    mongoose_helper:wait_until(fun modules_are_reported/0, true),
    all_event_have_the_same_client_id().

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

all_event_have_the_same_client_id() ->
    Tab = ets:tab2list(?ETS_TABLE),
    UniqueSortedTab = lists:usort([Cid ||#event{cid = Cid} <- Tab]),
    1 == length(UniqueSortedTab).


hosts_count_is_reported() ->
    is_in_table(<<"hosts_count">>).

hosts_count_is_not_reported() ->
    is_not_in_table(<<"hosts_count">>).

modules_are_reported() ->
    is_in_table(<<"modules">>).

modules_are_not_reported() ->
    is_not_in_table(<<"modules">>).

is_in_table(EventCategory) ->
    Tab = ets:tab2list(?ETS_TABLE),
    lists:any(
        fun(#event{ec = EC}) ->
            EC == EventCategory
        end, Tab).

is_not_in_table(EventCategory) ->
    not is_in_table(EventCategory).

get_events_collection_size() ->
    length(ets:tab2list(?ETS_TABLE)).

enable_system_stats(Node) ->
    enable_system_stats(Node, 100, 100).

enable_system_stats(Node, InitialReport, PeriodicReport) ->
    UrlArgs = [google_analytics_url, ?SERVER_URL],
    {atomic, ok} = mongoose_helper:successful_rpc(Node, ejabberd_config, add_local_option, UrlArgs),
    distributed_helper:rpc(
      Node, mongoose_service, start_service,
    [service_mongoose_system_stats, [{initial_report, InitialReport}, {periodic_report, PeriodicReport}]]).

disable_system_stats(Node) ->
    distributed_helper:rpc(Node, mongoose_service, stop_service, [service_mongoose_system_stats]),
    mongoose_helper:successful_rpc(Node, ejabberd_config, del_local_option, [ google_analytics_url ]).

delete_prev_client_id(Node) ->
    mongoose_helper:successful_rpc(Node, mnesia, delete_table, [service_mongoose_system_stats]).

create_events_collection() ->
    ets:new(?ETS_TABLE, [duplicate_bag, named_table, public]).

clear_events_collection() ->
    ets:delete_all_objects(?ETS_TABLE).

system_stats_service_is_disabled(Node) ->
    Pid = distributed_helper:rpc(Node, erlang, whereis, [service_mongoose_system_stats]),
    not erlang:is_pid(Pid).

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
        el = get_el(el, Params),
        cid = get_el(cid, Params),
        ec = get_el(ec, Params),
        ea = get_el(ea, Params)
    }.

get_el(Key, Proplist) ->
    proplists:get_value(Key, Proplist, undef).

trigger_reporting(Node) ->
    [mongoose_helper:successful_rpc(Node, telemetry, execute,
        [[mongoose_system_stats], #{hosts_count => 20}, #{}]) || _ <- lists:seq(1,20)],
    [mongoose_helper:successful_rpc(Node, telemetry, execute,
        [[mongoose_system_stats], #{module => test_mod}, #{host => "0.0.0.0", opts => [{backend, test_backend}]}]) || _ <- lists:seq(1,20)].
