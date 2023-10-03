-module(mongooseim_metrics_SUITE).

-include_lib("exml/include/exml.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("jlib.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all, nowarn_export_all]).

all() ->
    [
     {group, ordinary_mode},
     {group, all_metrics_are_global}
    ].

groups() ->
    [
     {ordinary_mode, [], all_metrics_list()},
     {all_metrics_are_global, [], all_metrics_list()}
    ].

all_metrics_list() ->
    [
     no_skip_metric,
     subscriptions_initialised,
     tcp_connections_detected,
     tcp_metric_varies_with_tcp_variations,
     up_time_positive,
     queued_messages_increase,
     function_ensure_subscribed_metric_subscribes
    ].

init_per_suite(C) ->
    application:load(exometer_core),
    application:set_env(exometer_core, mongooseim_report_interval, 1000),
    {Port, Socket} = carbon_cache_server:start(),
    Sup = spawn(fun() ->
        mim_ct_sup:start_link(ejabberd_sup),
        Hooks = {gen_hook,
                 {mongooseim_helper, start_link_loaded_hooks, []},
                 permanent,
                 brutal_kill,
                 worker,
                 [gen_hook]},
        supervisor:start_child(ejabberd_sup, Hooks),
        receive
            stop ->
                ok
        end
    end),
    Reporters = get_reporters_cfg(Port),
    application:set_env(exometer_core, report, Reporters),
    PortServer = carbon_cache_server:wait_for_accepting(),
    gen_tcp:controlling_process(Socket, PortServer),
    {ok, _Apps} = application:ensure_all_started(exometer_core),
    exometer:new([carbon, packets], spiral),
    [{carbon_port, Port}, {test_sup, Sup}, {carbon_server, PortServer}, {carbon_socket, Socket} | C].

end_per_suite(C) ->
    Sup = ?config(test_sup, C),
    Sup ! stop,
    CarbonServer = ?config(carbon_server, C),
    erlang:exit(CarbonServer, kill),
    CarbonSocket = ?config(carbon_socket, C),
    gen_tcp:close(CarbonSocket),
    application:stop(exometer_core),
    C.

init_per_group(Group, C) ->
    mongoose_config:set_opts(opts(Group)),
    mongoose_metrics:init(),
    mongoose_metrics:init_mongooseim_metrics(),
    C.

end_per_group(_Group, _C) ->
    mongoose_metrics:remove_host_type_metrics(<<"localhost">>),
    mongoose_metrics:remove_host_type_metrics(global),
    mongoose_config:erase_opts().

init_per_testcase(CN, C) when tcp_connections_detected =:= CN;
                              tcp_metric_varies_with_tcp_variations =:= CN ->
    exometer:setopts([global, tcpPortsUsed], [{sample_interval, 50}]),
    exometer:repair([global, tcpPortsUsed]),
    C;
init_per_testcase(queued_messages_increase, C) ->
    exometer:setopts([global, processQueueLengths], [{sample_interval, 50}]),
    exometer:repair([global, processQueueLengths]),
    PidsFun = fun() -> put('$internal_queue_len', 1),
                       receive die -> ok
                       end
              end,
    Pids = [spawn(PidsFun) || _ <- lists:seq(1,5)],
    lists:foreach(fun(Pid) -> Pid ! undefined end, Pids),
    [{pids, Pids} | C];
init_per_testcase(_N, C) ->
    C.

end_per_testcase(queued_messages_increase, C) ->
    [Pid ! die || Pid <- ?config(pids, C)],
    C;
end_per_testcase(_N, C) ->
    C.

up_time_positive(_C) ->
    {ok, [{value, X}]} = mongoose_metrics:get_metric_value(global, nodeUpTime),
    ?assert(X > 0).

function_ensure_subscribed_metric_subscribes(_C) ->
    SubMetric = [happy_metric],
    UnsubMetric = [sad_metric],
    mongoose_metrics:ensure_subscribed_metric(global, SubMetric, spiral),
    mongoose_metrics:ensure_metric(global, UnsubMetric, spiral),
    Subs = exometer_report:list_subscriptions(exometer_report_graphite),
    try
        true = lists:keymember([global|SubMetric], 1, Subs),
        false = lists:keymember([global|UnsubMetric], 1, Subs)
    catch C:E:S ->
              ct:pal("Subs ~p", [Subs]),
              erlang:raise(C, E, S)
    end.

get_new_tcp_metric_value(OldValue) ->
    Validator = fun(NewValue) -> OldValue =/= NewValue end,
    {ok, {ok, [{value, X}]}} = async_helper:wait_until(
      fun() -> mongoose_metrics:get_metric_value(global, tcpPortsUsed) end,
      Validator, #{sleep_time => 30, time_left => 500}
     ),
    X.

tcp_connections_detected(_C) ->
    get_new_tcp_metric_value({ok, []}).

tcp_metric_varies_with_tcp_variations(_C) ->
    X = get_new_tcp_metric_value({ok, []}),
    {ok, Socket} = gen_tcp:listen(0, []),
    Y = get_new_tcp_metric_value({ok, [{value, X}]}),
    ?assert(Y == X + 1),
    gen_tcp:close(Socket),
    X = get_new_tcp_metric_value({ok, [{value, Y}]}).

queued_messages_increase(_C) ->
    Fun = fun(Value) ->
        case Value of
            [{fsm, 5}, {regular, 5}, {total, 10}] -> true;

            %% Sometimes there is an additional unprocessed message
            %% in the standard I/O ('user') process
            [{fsm, 5}, {regular, 6}, {total, 11}] -> true;
            _ -> false
        end
    end,
    async_helper:wait_until(
      fun() ->
              {ok, L} = mongoose_metrics:get_metric_value(global, processQueueLengths),
              lists:sort(L)
      end, Fun).

no_skip_metric(_C) ->
    ok = mongoose_metrics:create_generic_hook_metric(<<"localhost">>, sm_register_connection_hook),
    undefined = exometer:info([<<"localhost">>, sm_register_connection_hook]).

subscriptions_initialised(_C) ->
    true = wait_for_update(exometer:get_value([carbon, packets], count), 60).

wait_for_update({ok, [{count,X}]}, 0) ->
    X > 0;
wait_for_update({ok, [{count,X}]}, _N) when X > 0 ->
    true;
wait_for_update({ok, [{count,0}]}, N) ->
    timer:sleep(1000),
    wait_for_update(exometer:get_value([carbon, packets], count), N-1).

opts(Group) ->
    #{hosts => [<<"localhost">>],
      host_types => [],
      all_metrics_are_global => Group =:= all_metrics_are_global}.

get_reporters_cfg(Port) ->
    [{reporters, [
                 {exometer_report_graphite, [
                                             {prefix, "mongooseim"},
                                             {connect_timeout, 10000},
                                             {host, "127.0.0.1"},
                                             {port, Port},
                                             {api_key, ""}
                                            ]}
                ]}].
