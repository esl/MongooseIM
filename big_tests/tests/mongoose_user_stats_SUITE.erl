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

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

suite() ->
    require_rpc_nodes([mim]).

all() ->
    [
        number_of_hosts_is_reported_to_google_analytics_when_mim_starts
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

init_per_testcase(_CaseName, Config) ->
    ets:new(?ETS_TABLE, [duplicate_bag, named_table, public]),
    Args = [google_analytics_url, "http://localhost:8765?dummy=arg"],
    {atomic, ok} = mongoose_helper:successful_rpc(ejabberd_config, add_local_option, Args),
    Config.

end_per_testcase(_CaseName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------


number_of_hosts_is_reported_to_google_analytics_when_mim_starts(_Config) ->
    % Restart MIM will not work, because on restart config file is read and config is reloaded,
    %  which overwrites the config and the passed option is not set
    % WHEN
    mongoose_helper:successful_rpc(mongoose_user_stats, report_user_stats, []),
    % THEN
    mongoose_helper:wait_until(fun hosts_count_is_reported/0, true),
    mongoose_helper:wait_until(fun hosts_count_is_reported/0, true),
    ok.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

hosts_count_is_reported() ->
    is_in_table(<<"hosts_count">>).

modules_are_reported() ->
    is_in_table(<<"modules">>).

is_in_table(EventCategory) ->
    Tab = ets:tab2list(?ETS_TABLE),
    lists:any(
        fun(#event{ec = EC}) when EC == EventCategory -> true;
        (_) -> false
        end, Tab).

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

%%--------------------------------------------------------------------
%% Cowboy handlers
%%--------------------------------------------------------------------

handler_init(Req0, _State) ->
    Qs = maps:get(qs, Req0),
    Event = qs_to_event(Qs),
    ets:insert(?ETS_TABLE, Event),
    Req1 = cowboy_req:reply(200, #{}, <<"">>, Req0),
    {ok, Req1, no_state}.

qs_to_event(Qs) ->
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


