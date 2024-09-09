-module(carbon_cache_server).

%% API for use in tests
-export([start/1, stop/1, subscribe/1, get_metric/1]).

%% Internal API
-export([server/2]).

-include_lib("common_test/include/ct.hrl").

%% API

start(Config) ->
    {Port, Socket} = start(),
    PortServer = wait_for_accepting(),
    gen_tcp:controlling_process(Socket, PortServer),
    [{carbon_port, Port}, {carbon_server, PortServer}, {carbon_socket, Socket} | Config].

stop(Config) ->
    CarbonServer = ?config(carbon_server, Config),
    erlang:exit(CarbonServer, kill),
    CarbonSocket = ?config(carbon_socket, Config),
    gen_tcp:close(CarbonSocket).

subscribe(Config) ->
    CarbonServer = ?config(carbon_server, Config),
    CarbonServer ! {subscribe, self()}.

get_metric(Metric) ->
    receive
        {packet, Metric, Value, TS} ->
            ct:log("Received metric ~p with value ~p and timestamp ~p", [Metric, Value, TS]),
            {Value, TS}
    after 0 ->
            no_metric
    end.

%% Internal functions

start() ->
    case gen_tcp:listen(0, [{active, false}, {packet, line}]) of
        {ok, ListenSock} ->
            spawn(?MODULE, server, [ListenSock, self()]),
            {ok, Port} = inet:port(ListenSock),
            {Port, ListenSock};
        {error, Reason} ->
            {error, Reason}
    end.

server(LS, Parent) ->
    Parent ! {accepting, self()},
    server(LS).

server(LS) ->
    case gen_tcp:accept(LS) of
        {ok, S} ->
            loop(S, []),
            server(LS);
        Other ->
            ct:log("accept returned ~w - goodbye!~n", [Other])
    end.

loop(S, Pids) ->
    inet:setopts(S, [{active, once}]),
    receive
        {subscribe, Pid} ->
            monitor(process, Pid),
            loop(S, [Pid | Pids]);
        {'DOWN', _Ref, process, Pid, _Reason} ->
            loop(S, Pids -- [Pid]);
        {tcp, S, Data} ->
            ct:log("Carbon cache server received packet: ~p", [Data]),
            [Metric, ValueStr, TimeStamp] = string:tokens(Data, " \n"),
            Msg = {packet, Metric, list_to_integer(ValueStr), list_to_integer(TimeStamp)},
            [Pid ! Msg || Pid <- Pids],
            loop(S, Pids);
        {tcp_closed, S} ->
            ct:log("Socket ~w closed [~w]~n", [S, self()])
    end.

wait_for_accepting() ->
    receive
        {accepting, Pid} ->
            Pid
    end.
