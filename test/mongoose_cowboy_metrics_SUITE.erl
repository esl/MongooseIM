-module(mongoose_cowboy_metrics_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-import(mongoose_cowboy_metrics, [request_count_metric/2,
                                  response_count_metric/2,
                                  response_count_metric/3,
                                  response_latency_metric/3]).

-define(LISTENER, mongoose_cowboy_metrics_SUITE_listener).
-define(HANDLER, ?MODULE).
-define(METHODS, [<<"GET">>,
                  <<"HEAD">>,
                  <<"POST">>,
                  <<"PUT">>,
                  <<"DELETE">>,
                  <<"OPTIONS">>,
                  <<"PATCH">>]).
-define(DEFAULT_PREFIX, [?HANDLER]).
-define(CLASSES, [<<"1XX">>, <<"2XX">>, <<"3XX">>, <<"4XX">>, <<"5XX">>]).



%%-------------------------------------------------------------------
%% Suite definition
%%-------------------------------------------------------------------

all() ->
    [metrics_are_not_recorded_by_default,
     metrics_are_not_recorded_when_record_metrics_is_false,
     metrics_are_recorded,
     metrics_are_recorded_with_configured_prefix,
     unsent_responses_generate_metrics
    ].

%%-------------------------------------------------------------------
%% Init & teardown
%%-------------------------------------------------------------------

init_per_suite(Config) ->
    application:ensure_all_started(gun),
    ejabberd_helper:start_ejabberd_with_config(Config, "ejabberd.cfg"),
    Config.

end_per_suite(_Config) ->
    application:stop(gun),
    ejabberd_helper:stop_ejabberd(),
    ok.

%%-------------------------------------------------------------------
%% Test cases
%%-------------------------------------------------------------------

metrics_are_not_recorded_by_default(_Config) ->
    create_metrics(),
    start_listener([{'_', ?HANDLER, []}], []),

    metrics_are_not_recorded(),

    stop_listener(),
    destroy_metrics().

metrics_are_not_recorded_when_record_metrics_is_false(_Config) ->
    create_metrics(),
    start_listener([{'_', ?HANDLER, []}], [{record_metrics, false}]),

    metrics_are_not_recorded(),

    stop_listener(),
    destroy_metrics().

metrics_are_not_recorded() ->
    run_requests([get, head, post, put, delete, options, patch],
                 [<<"100">>, <<"200">>, <<"300">>, <<"400">>, <<"500">>]),
    [ensure_metric_value(M, 0)
     || M <- count_metrics(?DEFAULT_PREFIX) ++ latency_metrics(?DEFAULT_PREFIX)].

metrics_are_recorded(_Config) ->
    create_metrics(),
    start_listener([{'_', ?HANDLER, []}], [{record_metrics, true}]),

    timer:sleep(1000),
    run_requests([get, head, post, put, delete, options, patch],
                 [<<"100">>, <<"200">>, <<"300">>, <<"400">>, <<"500">>]),

    [ensure_metric_value(request_count_metric(?DEFAULT_PREFIX, M), 5) || M <- ?METHODS],
    [ensure_metric_value(response_count_metric(?DEFAULT_PREFIX, M, C), 1) || M <- ?METHODS, C <- ?CLASSES],
    [ensure_metric_bumped(response_latency_metric(?DEFAULT_PREFIX, M, C)) || M <- ?METHODS, C <- ?CLASSES],

    stop_listener(),
    destroy_metrics().


metrics_are_recorded_with_configured_prefix(_Config) ->
    Prefix = [http, api, my_endpoint],
    create_metrics(Prefix),
    start_listener([{'_', ?HANDLER, []}], [{record_metrics, true},
                                           {handler_to_metric_prefix, #{?HANDLER => Prefix}}]),

    run_requests([get, head, post, put, delete, options, patch],
                 [<<"100">>, <<"200">>, <<"300">>, <<"400">>, <<"500">>]),

    [ensure_metric_value(request_count_metric(Prefix, M), 5) || M <- ?METHODS],
    [ensure_metric_value(response_count_metric(Prefix, M, C), 1) || M <- ?METHODS, C <- ?CLASSES],
    [ensure_metric_bumped(response_latency_metric(Prefix, M, C)) || M <- ?METHODS, C <- ?CLASSES],

    stop_listener(),
    destroy_metrics(Prefix).

unsent_responses_generate_metrics(_Config) ->
    create_metrics(),
    start_listener([{'_', ?HANDLER, [{action, none}]}], [{record_metrics, true}]),

    run_requests([get, head, post, put, delete, options, patch],
                 [<<"100">>, <<"200">>, <<"300">>, <<"400">>, <<"500">>]),

    %% when response is not sent Cowboy returns 204
    [ensure_metric_value(request_count_metric(?DEFAULT_PREFIX, M), 5) || M <- ?METHODS],
    [ensure_metric_value(response_count_metric(?DEFAULT_PREFIX, M, <<"2XX">>), 5) || M <- ?METHODS],
    [ensure_metric_value(response_count_metric(?DEFAULT_PREFIX, M, C), 0) || M <- ?METHODS, C <- ?CLASSES -- [<<"2XX">>]],
    [ensure_metric_bumped(response_latency_metric(?DEFAULT_PREFIX, M, <<"2XX">>)) || M <- ?METHODS],
    [ensure_metric_value(response_latency_metric(?DEFAULT_PREFIX, M, C), 0) || M <- ?METHODS, C <- ?CLASSES -- [<<"2XX">>]],

    stop_listener(),
    destroy_metrics().

%%-------------------------------------------------------------------
%% cowboy handler callbacks
%%-------------------------------------------------------------------

init(_Type, Req, Opts) ->
    {ok, Req, proplists:get_value(action, Opts, echo_status)}.

handle(Req, echo_status = State) ->
    {BinStatus, Req1} = cowboy_req:qs_val(<<"status">>, Req),
    Status = binary_to_integer(BinStatus),
    {ok, RespReq} = cowboy_req:reply(Status, [], <<>>, Req1),
    {ok, RespReq, State};
handle(Req, none = State) ->
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%-------------------------------------------------------------------
%% Helpers
%%-------------------------------------------------------------------

start_listener(Paths, ExtraEnv) ->
    Dispatch = cowboy_router:compile([{'_', Paths}]),
    Middleware = [mongoose_cowboy_metrics_mw_before,
                  cowboy_router,
                  mongoose_cowboy_metrics_mw_after,
                  cowboy_handler],
    {ok, _} = cowboy:start_http(?LISTENER, 1, [{port, 0}],
                                [{env, [{dispatch, Dispatch} | ExtraEnv]},
                                 {middlewares, Middleware}]).

stop_listener() ->
    ok = cowboy:stop_listener(?LISTENER).

create_metrics() ->
    create_metrics(?DEFAULT_PREFIX).

create_metrics(Prefix) ->
    %% in case of request count we're interested in the exact values - let's use counter
    [ok = mongoose_metrics:ensure_metric(global, M, counter) || M <- count_metrics(Prefix)],
    %% in case of latencies it doesn't make sense to check if values are correct - we
    %% only need to know if the value has been reported, so gauge is enough
    [ok = mongoose_metrics:ensure_metric(global, M, gauge) || M <- latency_metrics(Prefix)].

destroy_metrics() ->
    destroy_metrics(?DEFAULT_PREFIX).

destroy_metrics(Prefix) ->
    [exometer:delete([global | M]) || M <- count_metrics(Prefix) ++ latency_metrics(Prefix)].

count_metrics(Prefix) ->
    [request_count_metric(Prefix, M) || M <- ?METHODS] ++
    [response_count_metric(Prefix, M, C) || M <- ?METHODS, C <- ?CLASSES].

latency_metrics(Prefix) ->
    [response_latency_metric(Prefix, M, C) || M <- ?METHODS, C <- ?CLASSES].

ensure_metric_value(Metric, Value) ->
    {ok, DataPoints} = mongoose_metrics:get_metric_value([global | Metric]),
    ?assertEqual(Value, proplists:get_value(value, DataPoints)).

ensure_metric_bumped(Metric) ->
    {ok, DataPoints} = mongoose_metrics:get_metric_value([global | Metric]),
    ?assertNotEqual(0, proplists:get_value(value, DataPoints)).

run_requests(Methods, Statuses) ->
    {ok, Conn} = gun:open("localhost", ranch:get_port(?LISTENER)),
    {ok, _} = gun:await_up(Conn),
    [begin
        StreamRef = gun:M(Conn, <<"/?status=", S/binary>>, []),
        {response, fin, _, _} = gun:await(Conn, StreamRef)
     end || M <- Methods, S <- Statuses].

