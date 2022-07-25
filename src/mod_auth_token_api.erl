-module(mod_auth_token_api).

-include("jlib.hrl").
-include("mod_auth_token.hrl").
-include("mongoose.hrl").

-export([revoke_token_command/1, create_token/1]).

-import(mod_auth_token, [token/3, serialize/1]).

-spec revoke_token_command(User) -> Result when
    User :: jid:jid(),
    Reason :: {not_found | internal_server_error, string()},
    Result :: {ok, string()} | {error, Reason}.
revoke_token_command(User) ->
    #jid{lserver = LServer} = Jid = convert_user(User),
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            try mod_auth_token:revoke(HostType, Jid) of
                not_found ->
                    {error, {not_found, "User or token not found."}};
                ok ->
                    {ok, "Revoked."};
                error ->
                    {error, {internal_server_error, "Internal server error."}}
            catch Class:Reason:Stacktrace ->
                ?LOG_ERROR(#{what => auth_token_revoke_failed,
                             class => Class, reason => Reason, stacktrace => Stacktrace}),
                {error, {internal_server_error, "Internal server error."}}
            end;
        _ ->
            {error, {not_found, "Unknown domain"}}
    end.

-spec create_token(User) -> Result when
    User :: jid:jid(),
    Reason :: {not_found | internal_server_error, string()},
    Result :: {ok, #{binary() => string()}} | {error, Reason}.
create_token(User) ->
    #jid{lserver = LServer} = Jid = convert_user(User),
    case mongoose_domain_api:get_domain_host_type(LServer) of
        {ok, HostType} ->
            case {token(HostType, Jid, access), token(HostType, Jid, refresh)} of
                {#token{} = AccessToken, #token{} = RefreshToken} ->
                    {ok, #{<<"access">> => serialize(AccessToken),
                           <<"refresh">> => serialize(RefreshToken)}};
                _ -> {error, {internal_server_error, "Internal server errror."}}
            end;
        _ ->
            {error, {not_found, "Unknown domain"}}
    end.

convert_user(User) when is_binary(User) ->
    jid:from_binary(User);
convert_user(User) ->
    User.
