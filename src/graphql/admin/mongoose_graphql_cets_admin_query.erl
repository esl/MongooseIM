-module(mongoose_graphql_cets_admin_query).
-behaviour(mongoose_graphql).
-include("mongoose_logger.hrl").

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(Ctx, cets, <<"systemInfo">>, _) ->
    try cets_status:status(mongoose_cets_discovery) of
        #{available_nodes := AvNodes,
          unavailable_nodes := UnNodes,
          joined_nodes := Joined,
          discovered_nodes := DiscoNodes,
          discovery_works := DiscoveryWorks,
          remote_nodes_without_disco := NoDisco,
          remote_nodes_with_unknown_tables := UnkNodes,
          remote_unknown_tables := UnkTabs,
          remote_nodes_with_missing_tables := MissNodes,
          remote_missing_tables := MissTabs,
          conflict_nodes := ConNodes,
          conflict_tables := ConTabs} ->
            {ok, #{<<"availableNodes">> => format_nodes(AvNodes),
                   <<"unavailableNodes">> => format_nodes(UnNodes),
                   <<"joinedNodes">> => format_nodes(Joined),
                   <<"discoveredNodes">> => format_nodes(DiscoNodes),
                   <<"remoteNodesWithoutDisco">> => format_nodes(NoDisco),
                   <<"remoteNodesWithUnknownTables">> => format_nodes(UnkNodes),
                   <<"remoteUnknownTables">> => format_tables(UnkTabs),
                   <<"remoteNodesWithMissingTables">> => format_nodes(MissNodes),
                   <<"remoteMissingTables">> => format_tables(MissTabs),
                   <<"conflictNodes">> => format_nodes(ConNodes),
                   <<"conflictTables">> => format_tables(ConTabs),
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

format_tables(Nodes) ->
    [{ok, Node} || Node <- Nodes].
