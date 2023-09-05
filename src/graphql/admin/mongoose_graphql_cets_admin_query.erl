-module(mongoose_graphql_cets_admin_query).
-behaviour(mongoose_graphql).
-include("mongoose_logger.hrl").

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(Ctx, cets, <<"systemInfo">>, _) ->
    try cets_discovery:system_info(mongoose_cets_discovery) of
        %% The node lists could not match for different nodes
        %% because they are updated periodically
        #{unavailable_nodes := UnNodes, nodes := Nodes, tables := Tables} ->
            NodesSorted = lists:sort(Nodes),
            AvailNodes = available_nodes(),
            JoinedNodes = filter_joined_nodes(AvailNodes, Tables),
            PartNodes = AvailNodes -- JoinedNodes,
            {ok, #{<<"unavailableNodes">> => format_nodes(UnNodes),
                   <<"unavailableNodesCount">> => length(UnNodes),
                   <<"availableNodes">> => format_nodes(AvailNodes),
                   <<"availableNodesCount">> => length(AvailNodes),
                   <<"joinedNodes">> => format_nodes(JoinedNodes),
                   <<"joinedNodesCount">> => length(JoinedNodes),
                   <<"partiallyJoinedNodes">> => format_nodes(PartNodes),
                   <<"partiallyJoinedNodesCount">> => length(PartNodes),
                   <<"discoveredNodes">> => format_nodes(NodesSorted),
                   <<"discoveredNodesCount">> => length(NodesSorted)}}
    catch _Class:Reason ->
            make_error({Reason, <<"Failed to get CETS system info">>}, Ctx)
    end;
execute(Ctx, cets, <<"tableInfo">>, _) ->
    try cets_discovery:info(mongoose_cets_discovery) of
        Tables ->
            {ok, lists:map(fun process_result/1, Tables)}
    catch _Class:Reason ->
            make_error({Reason, <<"Failed to get CETS tables info">>}, Ctx)
    end.

process_result(#{memory := Memory, size := Size, nodes := Nodes, table := Tab}) ->
    {ok, #{<<"memory">> => Memory, <<"size">> => Size,
           <<"nodes">> => format_nodes(Nodes), <<"tableName">> => Tab}}.

format_nodes(Nodes) ->
    [{ok, Node} || Node <- Nodes].

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
