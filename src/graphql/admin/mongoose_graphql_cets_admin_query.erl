-module(mongoose_graphql_cets_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(Ctx, cets, <<"systemInfo">>, _) ->
    try cets_discovery:info(mongoose_cets_discovery) of
        Tables ->
            {ok, lists:map(fun process_result/1, Tables)}
    catch _Class:Reason ->
            make_error({Reason, <<"Failed to get CETS tables info">>}, Ctx)
    end.

process_result(#{memory := Memory, size := Size, nodes := Nodes, table := Tab}) ->
    Nodes2 = [{ok, Node} || Node <- Nodes],
    {ok, #{<<"memory">> => Memory, <<"size">> => Size,
           <<"nodes">> => Nodes2, <<"tableName">> => Tab}}.
