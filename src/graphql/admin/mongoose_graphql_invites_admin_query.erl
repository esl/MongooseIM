-module(mongoose_graphql_invites_admin_query).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").
-include("mod_invites.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2]).

execute(_Ctx, _Obj, <<"listInvites">>, Args) ->
    list_invites(Args).

-spec list_invites(map()) -> {ok, [map()]} | {error, resolver_error()}.
list_invites(#{<<"host">> := Host}) ->
    case mod_invites:pretty_format_command_result(mod_invites:list_invites(Host)) of
        {error, _} = Error ->
            make_error(Error, #{host => Host});
        Invites ->
            {ok, [{ok, mod_invites:format_invite(Host, Invite)} || Invite <- sort(Invites)]}
    end.

sort(Invites) ->
    lists:sort(fun(#invite_token{created_at = A}, #invite_token{created_at = B}) -> A < B end, Invites).
