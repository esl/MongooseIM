-module(mongoose_graphql_default_resolver).

-export([execute/4]).

-ignore_xref([execute/4]).

execute(_Ctx, _Obj, <<"field">>, _Attrs) ->
    {ok, <<"Test field">>};
execute(_Ctx, _Obj, <<"id">>, #{<<"value">> := Value}) ->
    {ok, Value};
execute(_Ctx, _Obj, Field, _Attrs) ->
    {error, {not_implemented, Field}}.


