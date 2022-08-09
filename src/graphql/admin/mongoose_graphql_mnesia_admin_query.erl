-module(mongoose_graphql_mnesia_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-import(mongoose_graphql_helper, [make_error/2]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

execute(_Ctx, mnesia, <<"info">>, #{<<"keys">> := Keys}) ->
    ResultList = mnesia_api:mnesia_info(Keys),
    {ok, lists:foldl(fun
        ({ok, _} = Result, Acc) ->
            Acc ++ [Result];
        ({Error, Map}, Acc) ->
            Acc ++ [make_error(Error, Map)]
    end, [], ResultList)}.
