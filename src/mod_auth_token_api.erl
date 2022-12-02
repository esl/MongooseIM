-module(mod_auth_token_api).

-include("jlib.hrl").
-include("mod_auth_token.hrl").
-include("mongoose.hrl").

-export([revoke_token_command/1, create_token/1]).

-import(mod_auth_token, [token/3, serialize/1]).

-spec revoke_token_command(User) -> Result when
    User :: jid:jid(),
    Result :: {ok, string()} | {not_found | internal_server_error, string()}.
revoke_token_command(User) ->
    #jid{lserver = LServer} = User,
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            try mod_auth_token:revoke(HostType, User) of
                not_found ->
                    {not_found, "User or token not found"};
                ok ->
                    {ok, "Revoked"};
                error ->
                    {internal_server_error, "Internal server error"}
            catch Class:Reason:Stacktrace ->
                ?LOG_ERROR(#{what => auth_token_revoke_failed,
                             class => Class, reason => Reason, stacktrace => Stacktrace}),
                {internal_server_error, "Internal server error"}
            end;
        _ ->
            {not_found, "Unknown domain"}
    end.

-spec create_token(User) -> Result when
    User :: jid:jid(),
    Result :: {ok, #{binary() => string()}} | {not_found | internal_server_error, string()}.
create_token(User) ->
    #jid{lserver = LServer} = User,
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            case {token(HostType, User, access), token(HostType, User, refresh)} of
                {#token{} = AccessToken, #token{} = RefreshToken} ->
                    {ok, #{<<"access">> => serialize(AccessToken),
                           <<"refresh">> => serialize(RefreshToken)}};
                _ -> {internal_server_error, "Internal server error"}
            end;
        _ ->
            {not_found, "Unknown domain"}
    end.
