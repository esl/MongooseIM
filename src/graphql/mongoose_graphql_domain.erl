-module(mongoose_graphql_domain).

-export([execute/4]).

-ignore_xref([execute/4]).

execute(_Ctx, #{host_type := HostType}, <<"hostType">>, _Args) ->
    {ok, HostType};
execute(_Ctx, #{status := Status}, <<"status">>, _Args) ->
    {ok, Status};
execute(_Ctx, #{domain := Name}, <<"domain">>, _Args) ->
    {ok, Name};
execute(_Ctx, #{}, _, _Args) ->
    {ok, null}.
