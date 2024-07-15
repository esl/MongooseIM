-module(carbon_cache_server).

-export([start/0]).
-export([server/2]).
-export([wait_for_accepting/0]).

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
        {ok,S} ->
            loop(S),
            server(LS);
        Other ->
            ct:log("accept returned ~w - goodbye!~n",[Other])
    end.

loop(S) ->
    inet:setopts(S, [{active, once}]),
    receive
        {tcp, S, Data} ->
            ct:log("Carbon cache server received packet: ~p", [Data]),
            exometer:update([carbon, packets], 1),
            loop(S);
        {tcp_closed, S} ->
            ct:log("Socket ~w closed [~w]~n", [S, self()])
    end.

wait_for_accepting() ->
    receive
        {accepting, Pid} ->
            Pid
    end.
