-module(mongoose_graphql_domain).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("mongoose_graphql_types.hrl").

execute(_Ctx, #domain{host_type = HostType}, <<"hostType">>, _Args) ->
    {ok, HostType};
execute(_Ctx, #domain{status = Status}, <<"status">>, _Args) ->
    {ok, Status};
execute(_Ctx, #domain{domain = Name}, <<"domain">>, _Args) ->
    {ok, Name}.
