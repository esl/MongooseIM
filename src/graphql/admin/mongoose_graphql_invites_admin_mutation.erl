-module(mongoose_graphql_invites_admin_mutation).

-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2]).

execute(_Ctx, _Obj, <<"generateInvite">>, Args) ->
    generate_invite(Args).

-spec generate_invite(map()) -> {ok, map()} | {error, resolver_error()}.
generate_invite(#{<<"host">> := Host}) ->
    case mod_invites:generate_invite(Host) of
        {ok, Invite} ->
            {ok, mod_invites:format_invite(Host, Invite)};
        Err ->
            make_error(Err, #{host => Host})
    end.
