-module(mongoose_graphql_default_resolver).

-export([execute/4]).

-ignore_xref([execute/4]).

execute(_Ctx, _Obj, <<"field">>, _Attrs) ->
    {ok, <<"Test field">>};
execute(_Ctx, _Obj, <<"fieldDP">>, _Attrs) ->
    {ok, <<"Test field">>};
execute(_Ctx, _Obj, <<"id">>, #{<<"value">> := Value}) ->
    {ok, Value};
execute(_Ctx, _Obj, Cmd, _Attrs) when Cmd =:= <<"catA">>;
                                      Cmd =:= <<"catB">>;
                                      Cmd =:= <<"catC">>;
                                      Cmd =:= <<"catD">>;
                                      Cmd =:= <<"command">>;
                                      Cmd =:= <<"command2">>;
                                      Cmd =:= <<"command3">>;
                                      Cmd =:= <<"command4">>;
                                      Cmd =:= <<"command5">>;
                                      Cmd =:= <<"command6">> ->
    {ok, Cmd};
execute(_Ctx, _Obj, Field, _Attrs) ->
    {error, {not_implemented, Field}}.
