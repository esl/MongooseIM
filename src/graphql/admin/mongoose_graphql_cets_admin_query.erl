-module(mongoose_graphql_cets_admin_query).
-behaviour(mongoose_graphql).
-include("mongoose_logger.hrl").

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(Ctx, cets, <<"systemInfo">>, _) ->
    try mongoose_cets_api:take() of
        #{unavailable_nodes := UnNodes,
          available_nodes := AvailNodes,
          joined_nodes := JoinedNodes,
          partially_joined_nodes := PartNodes,
          partially_joined_tables := PartTables,
          discovered_nodes := NodesSorted,
          discovery_works := DiscoveryWorks} ->
            {ok, #{<<"unavailableNodes">> => format_nodes(UnNodes),
                   <<"unavailableNodesCount">> => length(UnNodes),
                   <<"availableNodes">> => format_nodes(AvailNodes),
                   <<"availableNodesCount">> => length(AvailNodes),
                   <<"joinedNodes">> => format_nodes(JoinedNodes),
                   <<"joinedNodesCount">> => length(JoinedNodes),
                   <<"partiallyJoinedNodes">> => format_nodes(PartNodes),
                   <<"partiallyJoinedTables">> => format_nodes(PartTables),
                   <<"partiallyJoinedNodesCount">> => length(PartNodes),
                   <<"discoveredNodes">> => format_nodes(NodesSorted),
                   <<"discoveredNodesCount">> => length(NodesSorted),
                   <<"discoveryWorks">> => DiscoveryWorks}}
    catch Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => cets_system_info_failed, class => Class,
                         reason => Reason, stacktrace => Stacktrace}),
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
