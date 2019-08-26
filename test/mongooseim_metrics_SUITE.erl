-module(mongooseim_metrics_SUITE).

-include_lib("exml/include/exml.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("jlib.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all]).

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
     queued_messages_increase
    ].

init_per_suite(C) ->
    application:load(exometer_core),
    application:set_env(exometer_core, mongooseim_report_interval, 1000),
    {Port, Socket} = carbon_cache_server:start(1, 0),
    Sup = spawn(fun() ->
                        mim_ct_sup:start_link(ejabberd_sup),
                        Hooks =
                        {ejabberd_hooks,
                         {ejabberd_hooks, start_link, []},
                         permanent,
                         brutal_kill,
                         worker,
                         [ejabberd_hooks]},
                        C2SSupervisor =
                        {ejabberd_c2s_sup,
                         {ejabberd_tmp_sup, start_link, [ejabberd_c2s_sup, ejabberd_c2s]},
                         permanent,
                         infinity,
                         supervisor,
                         [ejabberd_tmp_sup]},
                        supervisor:start_child(ejabberd_sup, Hooks),
                        supervisor:start_child(ejabberd_sup, C2SSupervisor),
                        receive
                            stop ->
                                ok
                        end
                end),
    Reporters = get_reporters_cfg(Port),
    application:set_env(exometer_core, report, Reporters),
    PortServer = carbon_cache_server:wait_for_accepting(),
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
    setup_meck(Group),
    mongoose_metrics:init(),
    C.

end_per_group(_Name, C) ->
    mongoose_metrics:remove_host_metrics(<<"localhost">>),
    mongoose_metrics:remove_host_metrics(global),
    meck:unload(),
    C.

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

get_new_tcp_metric_value(OldValue) ->
    Validator = fun(NewValue) -> OldValue =/= NewValue end,
    {ok, {ok, [{value, X}]}} = async_helper:wait_until(
      fun() -> mongoose_metrics:get_metric_value(global, tcpPortsUsed) end,
      true, #{validator => Validator, sleep_time => 30, time_left => 500}
     ),
    X.

tcp_connections_detected(_C) ->
    get_new_tcp_metric_value({ok, []}).

tcp_metric_varies_with_tcp_variations(_C) ->
    X = get_new_tcp_metric_value({ok, []}),
    {ok, Socket} = gen_tcp:listen(1805, []),
    Y = get_new_tcp_metric_value({ok, [{value, X}]}),
    ?assert(Y == X + 1),
    gen_tcp:close(Socket),
    X = get_new_tcp_metric_value({ok, [{value, Y}]}).

queued_messages_increase(_C) ->
    async_helper:wait_until(
      fun() ->
              {ok, L} = mongoose_metrics:get_metric_value(global, processQueueLengths),
              lists:sort(L)
      end,
      [{fsm, 5}, {regular, 5}, {total, 10}]
     ).

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

setup_meck(Group) ->
    meck:new(ejabberd_config, [no_link]),
    meck:expect(ejabberd_config, get_global_option, fun(hosts) -> [<<"localhost">>] end),
    meck:expect(ejabberd_config, get_local_option,
                fun (all_metrics_are_global) -> Group =:= all_metrics_are_global;
                    (_) -> undefined
                end),
    ok.

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
