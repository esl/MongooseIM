-module(mongoose_graphql_server_admin_mutation).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).
-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, server, <<"joinCluster">>, #{<<"node">> := Node}) ->
    case server_api:join_cluster(binary_to_list(Node)) of
        {mnesia_error, _} = Error ->
            make_error(Error, #{cluster => Node});
        {error, _} = Error ->
            make_error(Error, #{cluster => Node});
        {pang, String} ->
            make_error({timeout_error, String}, #{cluster => Node});
        {_, String} ->
            {ok, String}
    end;
execute(_Ctx, server, <<"leaveCluster">>, #{}) ->
    case server_api:leave_cluster() of
        {error, _} = Error ->
            make_error(Error, #{});
        {not_in_cluster, String} ->
            make_error({not_in_cluster_error, String}, #{});
        {_, String} ->
            {ok, String}
    end.
