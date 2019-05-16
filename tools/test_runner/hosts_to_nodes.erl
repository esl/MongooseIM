-module(hosts_to_nodes).
-export([main/0]).
-export([main/1]).

main() ->
    main([]).

main(Opts) ->
    Hosts = opts_to_hosts(Opts),
    Names = hosts_to_nodes(Hosts),
    print_names(Names),
    erlang:halt().

-spec hosts_to_nodes(atom()) -> [atom()].
hosts_to_nodes(Hosts) ->
    {ok, Config} = file:consult("big_tests/test.config"),
    Nodes = proplists:get_value(hosts, Config, []),
    lists:map(fun(Host) ->
                      HostConfig = proplists:get_value(Host, Nodes),
                      case HostConfig of
                          undefined ->
                              error_logger:error_msg("Host not found ~p", [Host]);
                          _ ->
                              ok
                      end,
                      proplists:get_value(node, HostConfig)
              end, Hosts).

print_names(DbNames) ->
    [io:format("~p~n", [D]) || D <- DbNames].

%% Nodes can be passed as a list of arguments "node1" "node2"
%% or as a list inside an argument "node1 node2"
opts_to_hosts(Opts) when is_list(Opts) ->
    OptsStrings = lists:map(fun atom_to_list/1, Opts),
    Joined = string:join(OptsStrings, " "),
    Nodes = string:tokens(Joined, " "),
    lists:map(fun list_to_atom/1, Nodes).
