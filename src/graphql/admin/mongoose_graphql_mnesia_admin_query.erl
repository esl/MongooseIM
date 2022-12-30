-module(mongoose_graphql_mnesia_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

execute(_Ctx, mnesia, <<"systemInfo">>, #{<<"keys">> := Keys}) ->
    {ok, ResultList} = mnesia_api:mnesia_info(Keys),
    {ok, lists:map(fun process_result/1, ResultList)}.

process_result({ok, _} = Result) -> Result;
process_result({Error, Map}) -> make_error(Error, Map).
