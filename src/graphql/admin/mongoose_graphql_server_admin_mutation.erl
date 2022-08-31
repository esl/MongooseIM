-module(mongoose_graphql_server_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).
-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, server, <<"joinCluster">>, #{<<"cluster">> := Cluster}) ->
    case server_api:join_cluster(binary_to_list(Cluster)) of
        {mnesia_error, _} = Error ->
            make_error(Error, #{cluster => Cluster});
        {error, _} = Error ->
            make_error(Error, #{cluster => Cluster});
        {pang, String} ->
            make_error({timeout_error, String}, #{cluster => Cluster});
        {_, String} ->
            {ok, String}
    end.
