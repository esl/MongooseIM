-module(mongoose_cets_api).
-export([take/0]).
-include("mongoose_logger.hrl").

-type tab_nodes() :: {Table :: atom(), Nodes :: [node()]}.
-type table() :: atom().

-type info() :: #{unavailable_nodes => [node()],
                  available_nodes => [node()],
                  joined_nodes => [node()],
                  partially_joined_nodes => [node()],
                  partially_joined_tables => [table()],
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
    {JoinedNodes, PartTables} = filter_joined_nodes(AvailNodes, Tables),
    PartNodes = AvailNodes -- JoinedNodes,
    #{unavailable_nodes => UnNodes,
      available_nodes => AvailNodes,
      joined_nodes => JoinedNodes,
      partially_joined_nodes => PartNodes,
      partially_joined_tables => PartTables,
      discovered_nodes => NodesSorted,
      discovery_works => discovery_works(Info)}.

%% Nodes, that host mongoose_cets_discovery process
available_nodes() ->
    OnlineNodes = [node() | nodes()],
    [Node || Node <- OnlineNodes, is_disco_running_on(Node)].

is_disco_running_on(Node) ->
    is_pid(rpc:call(Node, erlang, whereis, [mongoose_cets_discovery])).

%% Returns only nodes that replicate all our local CETS tables to the same list of remote nodes
-spec filter_joined_nodes(AvailNodes :: [node()], Tables :: [table()]) ->
    {JoinedNodes :: [node()], PartTables :: [table()]}.
filter_joined_nodes(AvailNodes, Tables) ->
    OtherNodes = lists:delete(node(), AvailNodes),
    Expected = node_list_for_tables(node(), Tables),
    OtherTables = [{Node, node_list_for_tables(Node, Tables)} || Node <- OtherNodes],
    OtherJoined = [Node || {Node, NodeTabs} <- OtherTables, NodeTabs =:= Expected],
    JoinedNodes = lists:sort([node() | OtherJoined]),
    PartTables = filter_partially_joined_tables(Expected, OtherTables),
    {JoinedNodes, PartTables}.

-spec filter_partially_joined_tables([tab_nodes()], [{node(), [tab_nodes()]}]) -> [table()].
filter_partially_joined_tables(Expected, OtherTables) ->
    TableNodesVariants = [Expected | [NodeTabs || {_Node, NodeTabs} <- OtherTables]],
    TableVariants = lists:map(fun tab_nodes_to_tables/1, TableNodesVariants),
    SharedTables = ordsets:intersection(TableVariants),
    AllTables = ordsets:union(TableVariants),
    ordsets:subtract(AllTables, SharedTables).

-spec tab_nodes_to_tables([tab_nodes()]) -> [table()].
tab_nodes_to_tables(TabNodes) ->
    [Table || {Table, [_|_] = _Nodes} <- TabNodes].

-spec node_list_for_tables(node(), [table()]) -> [tab_nodes()].
node_list_for_tables(Node, Tables) ->
    [{Table, node_list_for_table(Node, Table)} || Table <- Tables].

node_list_for_table(Node, Table) ->
    case catch rpc:call(Node, cets, other_nodes, [Table]) of
        List when is_list(List) ->
            ordsets:add_element(Node, List);
        Other ->
            ?LOG_ERROR(#{what => cets_get_other_nodes_failed, node => Node, table => Table, reason => Other}),
            []
    end.

discovery_works(#{last_get_nodes_result := {ok, _}}) ->
    true;
discovery_works(_) ->
    false.
