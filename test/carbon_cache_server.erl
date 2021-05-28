-module(carbon_cache_server).

-export([start/0]).
-export([server/2]).
-export([wait_for_accepting/0]).

start() ->
    case gen_tcp:listen(0,[{active, false},{packet,2}]) of
        {ok, ListenSock} ->
            spawn(?MODULE, server, [ListenSock, self()]),
            {ok, Port} = inet:port(ListenSock),
            {Port, ListenSock};
        {error,Reason} ->
            {error,Reason}
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
            io:format("accept returned ~w - goodbye!~n",[Other]),
            ok
    end.

loop(S) ->
    inet:setopts(S,[{active,once}]),
    receive
        {tcp,S,_Data} ->
            %Answer = process(Data), % Not implemented in this example
            %gen_tcp:send(S,Answer),
            %ct:print("~s",[Data]),
            exometer:update([carbon, packets], 1),
            loop(S);
        {tcp_closed,S} ->
            io:format("Socket ~w closed [~w]~n",[S,self()]),
            ok
    end.

wait_for_accepting() ->
    receive
        {accepting, Pid} ->
            Pid
    end.
