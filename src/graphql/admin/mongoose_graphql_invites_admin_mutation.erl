-module(mongoose_graphql_invites_admin_mutation).

-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").

-import(mongoose_graphql_helper, [make_error/2, format_result/2]).

execute(_Ctx, _Obj, <<"generateInvite">>, Args) ->
    generate_invite(Args);
execute(_Ctx, _Obj, <<"cleanupExpired">>, Args) ->
    cleanup_expired(Args);
execute(_Ctx, _Obj, <<"deleteInviteByToken">>, Args) ->
    delete_invite_by_token(Args);
execute(_Ctx, _Obj, <<"expireInvites">>, Args) ->
    expire_invites(Args);
execute(_Ctx, _Obj, <<"expireInviteByToken">>, Args) ->
    expire_invite_by_token(Args);
execute(_Ctx, _Obj, <<"generateResetToken">>, Args) ->
    generate_reset_token(Args).

cleanup_expired(_) ->
    {ok, mod_invites:cleanup_expired()}.

delete_invite_by_token(#{<<"host">> := Host, <<"token">> := Token}) ->
    handle_cmd_result(mod_invites:delete_invite_by_token(Host, Token), Host).

expire_invites(#{<<"host">> := Host, <<"username">> := Username}) ->
    handle_cmd_result(mod_invites:expire_invites(Host, Username), Host).

expire_invite_by_token(#{<<"host">> := Host, <<"token">> := Token}) ->
    handle_cmd_result(mod_invites:expire_invite_by_token(Host, Token), Host).

-spec generate_invite(map()) -> {ok, map()} | {error, resolver_error()}.
generate_invite(#{<<"host">> := Host, <<"username">> := Username0}) ->
    Username = null_to_bin(Username0),
    case mod_invites:generate_invite(Host, Username) of
        {error, _} = Error ->
            make_error(Error, #{host => Host});
        Invite ->
            {ok, mod_invites:format_invite(Host, Invite)}
    end.

generate_reset_token(#{<<"host">> := Host, <<"username">> := Username}) ->
    handle_cmd_result(mod_invites:generate_reset_token(Host, Username), Host).

null_to_bin(null) -> <<>>;
null_to_bin(Bin) when is_binary(Bin) -> Bin.

handle_cmd_result({error, _} = Error, Host) ->
    make_error(Error, #{host => Host});
handle_cmd_result(Result, _) ->
    {ok, Result}.
