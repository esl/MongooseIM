-module(mongoose_user_stats_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(COWBOY_DUMMY_HANDLER_MODULE, my_cowboy_mecked_module).
-define(SERVER_URL, "http://localhost:8765").
-define(ETS_TABLE, qs).

-record(event, {
    el = "",
    cid = "",
    ec = "",
    ea = ""
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
        user_stats_are_reported_to_google_analytics_when_mim_starts
        , all_clustered_mongooses_report_the_same_client_id
    ].

groups() ->
    [].

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

init_per_testcase(user_stats_are_reported_to_google_analytics_when_mim_starts, Config) ->
    create_events_collection(),
    enable_user_stats(mim()),
    delete_prev_client_id(mim()),
    Config;
init_per_testcase(_TestName, Config) ->
    create_events_collection(),
    Nodes = [mim(), mim2()],
    [ begin enable_user_stats(Node), delete_prev_client_id(Node) end || Node <- Nodes ],
    distributed_helper:add_node_to_cluster(mim2(), Config),
    Config.

end_per_testcase(user_stats_are_reported_to_google_analytics_when_mim_starts, Config) ->
    delete_prev_client_id(mim()),
    disable_user_stats(mim()),
    Config;
end_per_testcase(_TestName, Config) ->
    delete_prev_client_id(mim()),
    Nodes = [mim(), mim2()],
    [ begin delete_prev_client_id(Node), disable_user_stats(Node) end || Node <- Nodes ],
    Config.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------


user_stats_are_reported_to_google_analytics_when_mim_starts(_Config) ->
    %GIVEN
    % Restart MIM will not work, because on restart config file is read and config is reloaded,
    %  which overwrites the config and the passed option is not set
    % WHEN
    mongoose_helper:successful_rpc(mongoose_user_stats, report, []),
    % THEN

    mongoose_helper:wait_until(fun no_more_events_is_reported/0, true),
    mongoose_helper:wait_until(fun hosts_count_is_reported/0, true),
    mongoose_helper:wait_until(fun modules_are_reported/0, true),

    all_event_have_the_same_client_id(),
    ok.

all_clustered_mongooses_report_the_same_client_id(_Config) ->

    mongoose_helper:successful_rpc(mim(), mongoose_user_stats, report, []),
    mongoose_helper:successful_rpc(mim2(), mongoose_user_stats, report, []),

    mongoose_helper:wait_until(fun no_more_events_is_reported/0, true),

    all_event_have_the_same_client_id(),
    ok.


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

all_event_have_the_same_client_id() ->
    Tab = ets:tab2list(?ETS_TABLE),
    1 = length(lists:usort([Cid ||#event{cid = Cid} <- Tab])).

no_more_events_is_reported() ->
    Prev = get_events_collection_size(),
    timer:sleep(100),
    Post = get_events_collection_size(),
    % Prev > 0 is needed because we need to wait for some reports to come.
    Prev == Post andalso Prev > 0.

hosts_count_is_reported() ->
    is_in_table(<<"hosts_count">>).

modules_are_reported() ->
    is_in_table(<<"modules">>).

is_in_table(EventCategory) ->
    Tab = ets:tab2list(?ETS_TABLE),
    lists:any(
        fun(#event{ec = EC}) ->
            EC == EventCategory
        end, Tab).

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

enable_user_stats(Node) ->
    UrlArgs = [google_analytics_url, ?SERVER_URL],
    {atomic, ok} = mongoose_helper:successful_rpc(Node, ejabberd_config, add_local_option, UrlArgs),
    IsAllowedArgs = [mongoose_user_stats_is_allowed, true],
    {atomic, ok} = mongoose_helper:successful_rpc(Node, ejabberd_config, add_local_option, IsAllowedArgs).

disable_user_stats(Node) ->
    mongoose_helper:successful_rpc(Node, ejabberd_config, del_local_option, [ google_analytics_url ]),
    mongoose_helper:successful_rpc(Node, ejabberd_config, del_local_option, [ mongoose_user_stats_is_allowed ]).

delete_prev_client_id(Node) ->
    mongoose_helper:successful_rpc(Node, mnesia, delete_table, [persistent_user_info]).

create_events_collection() ->
    ets:new(?ETS_TABLE, [duplicate_bag, named_table, public]).

clear_events_collection() ->
    ets:delete_all_objects(?ETS_TABLE).

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


