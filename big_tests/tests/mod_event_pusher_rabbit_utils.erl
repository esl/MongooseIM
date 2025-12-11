-module(mod_event_pusher_rabbit_utils).

-moduledoc """
Utilities for mod_event_pusher_rabbit_SUITE.
This module is injected into the mongooseim node.
""".

-export([start/0, stop/0, simulate_rabbit_connection_error/3]).

start() ->
    meck:new(gen_tcp, [unstick, no_link, passthrough]).

stop() ->
    meck:unload(gen_tcp).

simulate_rabbit_connection_error(HostType, Port, ReconnectFailures) ->
    {ok, Worker} = mongoose_wpool:get_worker(rabbit, HostType, event_pusher),
    State = sys:get_state(Worker),
    c:c(wpool_process, [{d, 'TEST'}]), % export get_state/1
    #{connection := Connection} = wpool_process:get_state(State),
    simulate_tcp_connect_errors(Port, ReconnectFailures),
    MonitorRef = monitor(process, Connection),
    Connection ! {socket_error, simulated},
    receive {'DOWN', MonitorRef, process, Connection, _} -> ok end.

simulate_tcp_connect_errors(_Port, 0) ->
    ok;
simulate_tcp_connect_errors(Port, Count) ->
    persistent_term:put({tcp_connect_errors, Port}, Count),
    meck:expect(gen_tcp, connect, fun tcp_connect/4).

tcp_connect(Address, Port, Opts, Timeout) ->
    case persistent_term:get({tcp_connect_errors, Port}, 0) of
        0 ->
            persistent_term:erase({tcp_connect_errors, Port}),
            meck:passthrough([Address, Port, Opts, Timeout]);
        N when N > 0 ->
            persistent_term:put({tcp_connect_errors, Port}, N - 1),
            {error, simulated_reconnect_error}
    end.
