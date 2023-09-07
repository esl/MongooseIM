-module(mongoose_cets_api).
-export([take/0]).
-include("mongoose_logger.hrl").

-type info() :: #{unavailable_nodes => [node()],
                  available_nodes => [node()],
                  joined_nodes => [node()],
                  partially_joined_nodes => [node()],
                  discovered_nodes => [node()],
                  discovery_works => boolean()}.

-spec take() -> info().
take() ->
    %% The node lists could not match for different nodes
    %% because they are updated periodically
    #{unavailable_nodes := UnNodes, nodes := Nodes, tables := Tables} =
        Info = cets_discovery:system_info(mongoose_cets_discovery),
    NodesSorted = lists:sort(Nodes),
    AvailNodes = available_nodes(),
    JoinedNodes = filter_joined_nodes(AvailNodes, Tables),
    PartNodes = AvailNodes -- JoinedNodes,
    #{unavailable_nodes => UnNodes,
      available_nodes => AvailNodes,
      joined_nodes => JoinedNodes,
      partially_joined_nodes => PartNodes,
      discovered_nodes => NodesSorted,
      discovery_works => discovery_works(Info)}.

%% Nodes, that host mongoose_cets_discovery process
available_nodes() ->
    OnlineNodes = [node() | nodes()],
    [Node || Node <- OnlineNodes, is_disco_running_on(Node)].

is_disco_running_on(Node) ->
    is_pid(rpc:call(Node, erlang, whereis, [mongoose_cets_discovery])).

%% Returns only nodes that replicate all our local CETS tables to the same list of remote nodes
filter_joined_nodes(AvailNodes, Tables) ->
    OtherNodes = lists:delete(node(), AvailNodes),
    Expected = node_list_for_tables(node(), Tables),
    OtherJoined = [Node || Node <- OtherNodes, node_list_for_tables(Node, Tables) =:= Expected],
    lists:sort([node() | OtherJoined]).

node_list_for_tables(Node, Tables) ->
    [{Table, node_list_for_table(Node, Table)} || Table <- Tables].

node_list_for_table(Node, Table) ->
    lists:sort([Node | other_nodes(Node, Table)]).

other_nodes(Node, Table) ->
    case catch rpc:call(Node, cets, other_nodes, [Table]) of
        List when is_list(List) ->
            List;
        Other ->
            ?LOG_ERROR(#{what => cets_get_other_nodes_failed, node => Node, table => Table, reason => Other}),
            []
    end.

discovery_works(#{last_get_nodes_result := {ok, _}}) ->
    true;
discovery_works(_) ->
    false.
