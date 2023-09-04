-module(mongoose_graphql_cets_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(Ctx, cets, <<"systemInfo">>, _) ->
    try cets_discovery:system_info(mongoose_cets_discovery) of
        #{unavailable_nodes := UnNodes} ->
            {ok, #{<<"unavailableNodes">> => format_nodes(UnNodes),
                   <<"unavailableNodesCount">> => length(UnNodes)}}
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
