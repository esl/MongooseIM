-module(kill_nodes).
-export([main/0]).
-export([main/1]).

main() ->
    main([]).

main(Opts) ->
    Nodes = opts_to_nodes(Opts),
    Result = kill_nodes(Nodes),
    io:format("kill_nodes: done ~p~n", [Result]),
    erlang:halt().

kill_nodes(Nodes) ->
    [{Node, rpc:call(Node, init, stop, [])} || Node <- Nodes].

%% Nodes can be passed as a list of arguments "node1" "node2"
%% or as a list inside an argument "node1 node2"
opts_to_nodes(Opts) when is_list(Opts) ->
    OptsStrings = lists:map(fun atom_to_list/1, Opts),
    Joined = string:join(OptsStrings, " "),
    Nodes = string:tokens(Joined, " "),
    lists:map(fun list_to_atom/1, Nodes).
