-module(mod_auth_token_api).

-include("jlib.hrl").
-include("mod_auth_token.hrl").

-export([revoke_token_command/1, create_token/1]).

-import(mod_auth_token, [token/3, serialize/1]).

-spec revoke_token_command(User) -> ResTuple when
    User :: jid:jid(),
    ResCode :: ok | not_found | internal_server_error,
    ResTuple :: {ResCode, string()}.
revoke_token_command(User) ->
    #jid{lserver = LServer} = Jid = convert_user(User),
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            try mod_auth_token:revoke(HostType, Jid) of
                not_found ->
                    {not_found, "User or token not found."};
                ok ->
                    {ok, "Revoked."};
                error ->
                    {internal_server_error, "Internal server error."}
            catch _:_ ->
                    {internal_server_error, "Internal server error."}
            end;
        _ ->
            {not_found, "User or token not found."}
    end.

-spec create_token(User) -> ResTuple when
    User :: jid:jid(),
    ResCode :: ok | internal_server_error | not_found,
    ResTuple :: {ResCode, string() | #{binary() => string()}}.
create_token(User) ->
    #jid{lserver = LServer} = Jid = convert_user(User),
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            case {token(HostType, Jid, access), token(HostType, Jid, refresh)} of
                {#token{} = AccessToken, #token{} = RefreshToken} ->
                    {ok, #{<<"access">> => serialize(AccessToken),
                           <<"refresh">> => serialize(RefreshToken)}};
                _ -> {internal_server_error, "Internal server errror."}
            end;
        _ ->
            {not_found, "User or token not found."}
    end.

convert_user(User) when is_binary(User) ->
    jid:from_binary(User);
convert_user(User) ->
    User.
