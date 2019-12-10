-module(service_mongoose_system_stats_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(COWBOY_DUMMY_HANDLER_MODULE, my_cowboy_mecked_module).
-define(SERVER_URL, "http://localhost:8765").
-define(ETS_TABLE, qs).

-record(event, {
    el = "",  % event Label
    cid = "", % client ID
    ec = "",  % event category
    ea = ""   % event action
    }).

-compile(export_all).

-import(distributed_helper, [mim/0, mim2/0,
                             require_rpc_nodes/1,
                             rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

suite() ->
    require_rpc_nodes([mim]).

all() ->
    [
        system_stats_are_reported_to_google_analytics_when_mim_starts,
        all_clustered_mongooses_report_the_same_client_id,
        system_stats_are_not_reported_when_not_allowed
    ].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

-define(APPS, [inets, crypto, ssl, fusco, ranch, cowlib, cowboy]).

init_per_suite(Config) ->
    [ {ok, _} = application:ensure_all_started(App) || App <- ?APPS ],
    create_dummy_cowboy_handler(),
    start_cowboy(),
    Config.

end_per_suite(Config) ->
    stop_cowboy(),
    remove_dummy_cowboy_handler(),
    Config.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

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

system_stats_are_reported_to_google_analytics_when_mim_starts(_Config) ->
    trigger_reporting(mim()),
    mongoose_helper:wait_until(fun no_more_events_is_reported/0, true),
    mongoose_helper:wait_until(fun hosts_count_is_reported/0, true),
    mongoose_helper:wait_until(fun modules_are_reported/0, true),
    all_event_have_the_same_client_id().

system_stats_are_not_reported_when_not_allowed(_Config) ->
    true = system_stats_service_is_disabled(mim()).

all_clustered_mongooses_report_the_same_client_id(_Config) ->
    trigger_reporting(mim()),
    trigger_reporting(mim2()),
    mongoose_helper:wait_until(fun no_more_events_is_reported/0, true),
    all_event_have_the_same_client_id().

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

all_event_have_the_same_client_id() ->
    Tab = ets:tab2list(?ETS_TABLE),
    UniqueSortedTab = lists:usort([Cid ||#event{cid = Cid} <- Tab]),
    1 = length(UniqueSortedTab).

no_more_events_is_reported() ->
    Prev = get_events_collection_size(),
    timer:sleep(100),
    Post = get_events_collection_size(),
    % Prev > 0 is needed because we need to wait for some reports to come.
    Prev == Post andalso Prev > 0.

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

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/[...]", ?COWBOY_DUMMY_HANDLER_MODULE, []}]}
    ]),
    {ok, _Pid} = cowboy:start_clear(http_listener,
                                    #{num_acceptors => 20,
                                      socket_opts => [{port, 8765}]},
                                    #{env => #{dispatch => Dispatch}}).

stop_cowboy() ->
    ok = cowboy:stop_listener(http_listener).

create_dummy_cowboy_handler() ->
    ok = meck:new(?COWBOY_DUMMY_HANDLER_MODULE, [non_strict, no_link]),
    ok = meck:expect(?COWBOY_DUMMY_HANDLER_MODULE, init, fun handler_init/2),
    ok = meck:expect(?COWBOY_DUMMY_HANDLER_MODULE, terminate, fun handler_terminate/3).

remove_dummy_cowboy_handler() ->
    true = meck:validate(?COWBOY_DUMMY_HANDLER_MODULE),
    ok = meck:unload(?COWBOY_DUMMY_HANDLER_MODULE).

enable_system_stats(Node) ->
    UrlArgs = [google_analytics_url, ?SERVER_URL],
    {atomic, ok} = mongoose_helper:successful_rpc(Node, ejabberd_config, add_local_option, UrlArgs),
    distributed_helper:rpc(Node, mongoose_service, start_service, [service_mongoose_system_stats, []]).

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

handler_init(Req0, _State) ->
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
        el = get(el, Params),
        cid = get(cid, Params),
        ec = get(ec, Params),
        ea = get(ea, Params)
    }.

get(Key, Proplist) ->
    proplists:get_value(Key, Proplist, undef).

handler_terminate(_Reason, _Req, _State) ->
    ok.

trigger_reporting(Node) ->
    [mongoose_helper:successful_rpc(Node, telemetry, execute,
        [[mongoose_system_stats], #{hosts_count => 20}, #{}]) || _ <- lists:seq(1,20)],
    [mongoose_helper:successful_rpc(Node, telemetry, execute,
        [[mongoose_system_stats], #{module => test_mod}, #{host => "0.0.0.0", opts => [{backend, test_backend}]}]) || _ <- lists:seq(1,20)].
