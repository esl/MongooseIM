-module(mongoose_graphql_domain).

-export([execute/4]).

-ignore_xref([execute/4]).

execute(_Ctx, #{'__schema__' := domain, host_type := HostType}, <<"hostType">>, _Args) ->
    {ok, HostType};
execute(_Ctx, #{'__schema__' := domain, name := Name}, <<"name">>, _Args) ->
    {ok, Name}.
