-module(mod_auth_token_api).

-include("jlib.hrl").
-include("mod_auth_token.hrl").

-export([revoke_token_command/1, create_token/2]).

-spec revoke_token_command(User) -> ResTuple when
    User :: binary(),
    ResCode :: ok | not_found | error,
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
                    {error, "Internal server error"}
            catch _:_ ->
                    {error, "Internal server error"}
            end;
        _ ->
            {not_found, "User or token not found."}
    end.

-spec create_token(User, Type) -> ResTuple when
    User :: jid:jid(),
    Type :: access | refresh,
    ResCode :: ok | internal_server_error | not_found,
    ResTuple :: {ResCode, string()}.
create_token(User, Type) ->
    #jid{lserver = LServer} = Jid = convert_user(User),
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            case mod_auth_token:token(HostType, Jid, Type) of
                #token{} = Token -> {ok, mod_auth_token:serialize(Token)};
                _ -> {internal_server_error, "Internal server errror"}
            end;
        _ ->
            {not_found, "User or token not found."}
    end.

convert_user(User) when is_binary(User) ->
    jid:from_binary(User);
convert_user(User) ->
    User.
