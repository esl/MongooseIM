-module(mongoose_graphql_admin_auth_info).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include_lib("jid/include/jid.hrl").

execute(#{authorized := Authorized}, admin, <<"authStatus">>, _Args) ->
    case Authorized of
        true ->
            {ok, 'AUTHORIZED'};
        false ->
            {ok, 'UNAUTHORIZED'}
    end;
execute(Ctx, admin, <<"domain">>, _Args) ->
    case maps:get(admin, Ctx, null) of
        null -> {ok, null};
        #jid{lserver = Domain} -> {ok, Domain}
    end;
execute(Ctx, admin, <<"authType">>, _Args) ->
    case maps:get(authorized_as, Ctx, null) of
        null -> {ok, null};
        domain_admin -> {ok, domain_admin};
        admin -> {ok, admin}
    end.
