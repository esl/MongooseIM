%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_http.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Authentication via HTTP request
%%% Created : 23 Sep 2013 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(ejabberd_auth_http).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(ejabberd_gen_auth).

%% External exports
-export([start/1,
         set_password/3,
         authorize/1,
         try_register/3,
         dirty_get_registered_users/0,
         get_vh_registered_users/1,
         get_vh_registered_users/2,
         get_vh_registered_users_number/1,
         get_vh_registered_users_number/2,
         get_password/2,
         get_password_s/2,
         does_user_exist/2,
         remove_user/2,
         remove_user/3,
         supports_sasl_module/2,
         stop/1]).

%% Pre-mongoose_credentials API
-export([check_password/3,
         check_password/5]).

-include("mongoose.hrl").
-include("scram.hrl").

-type http_error_atom() :: conflict | not_found | not_authorized | not_allowed.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(binary()) -> ok.
start(_Host) ->
    ok.

-spec supports_sasl_module(jid:lserver(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_, cyrsasl_plain) -> true;
supports_sasl_module(_, cyrsasl_scram) -> true;
supports_sasl_module(Host, cyrsasl_digest) -> not mongoose_scram:enabled(Host);
supports_sasl_module(_, _) -> false.

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    case mongoose_credentials:get(Creds, der_cert, undefined) of
        undefined -> ejabberd_auth:authorize_with_check_password(?MODULE, Creds);
        DerCert ->
            {LServer, LUser} = get_server_and_user_name(Creds),
            case verify_cert(LUser, LServer, DerCert) of
                true -> {ok, mongoose_credentials:set(Creds, auth_module, ?MODULE)};
                false -> {error, not_authorized}
            end
    end.


-spec check_password(jid:luser(), jid:lserver(), binary()) -> boolean().
check_password(LUser, LServer, Password) ->
    case mongoose_scram:enabled(LServer) of
        false ->
            case make_req(get, <<"check_password">>, LUser, LServer, Password) of
                {ok, <<"true">>} -> true;
                _ -> false
            end;
        true ->
            {ok, true} =:= verify_scram_password(LUser, LServer, Password)
    end.

-spec check_password(jid:luser(), jid:lserver(), binary(), binary(), fun()) -> boolean().
check_password(LUser, LServer, Password, Digest, DigestGen) ->
    case make_req(get, <<"get_password">>, LUser, LServer, <<"">>) of
        {error, _} ->
            false;
        {ok, GotPasswd} ->
            case mongoose_scram:enabled(LServer) of
                true ->
                    check_scram_password(GotPasswd, Password, Digest, DigestGen);
                false ->
                    ejabberd_auth:check_digest(Digest, DigestGen, Password, GotPasswd)
            end
    end.

-spec set_password(jid:luser(), jid:lserver(), binary()) -> ok | {error, not_allowed}.
set_password(LUser, LServer, Password) ->
    PasswordFinal = case mongoose_scram:enabled(LServer) of
                        true -> mongoose_scram:serialize(mongoose_scram:password_to_scram(
                                                  Password, mongoose_scram:iterations(LServer)));
                        false -> Password
                    end,
    case make_req(post, <<"set_password">>, LUser, LServer, PasswordFinal) of
        {ok, _} -> ok;
        {error, invalid_jid} = Error -> Error;
        {error, _} -> {error, not_allowed}
    end.

-spec try_register(jid:luser(), jid:lserver(), binary()) ->
    ok | {error, exists | not_allowed}.
try_register(LUser, LServer, Password) ->
    PasswordFinal = case mongoose_scram:enabled(LServer) of
                        true -> mongoose_scram:serialize(mongoose_scram:password_to_scram(
                                                  Password, mongoose_scram:iterations(LServer)));
                        false -> Password
                    end,
    case make_req(post, <<"register">>, LUser, LServer, PasswordFinal) of
        {ok, created} -> ok;
        {error, conflict} -> {error, exists};
        _Error -> {error, not_allowed}
    end.

-spec dirty_get_registered_users() -> [].
dirty_get_registered_users() ->
    [].

-spec get_vh_registered_users(jid:lserver()) -> [].
get_vh_registered_users(_Server) ->
    [].

-spec get_vh_registered_users(jid:lserver(), list()) -> [].
get_vh_registered_users(_Server, _Opts) ->
    [].

-spec get_vh_registered_users_number(binary()) -> 0.
get_vh_registered_users_number(_Server) ->
    0.

-spec get_vh_registered_users_number(jid:lserver(), list()) -> 0.
get_vh_registered_users_number(_Server, _Opts) ->
    0.

-spec get_password(jid:luser(), jid:lserver()) -> ejabberd_auth:passterm() | false.
get_password(LUser, LServer) ->
    case make_req(get, <<"get_password">>, LUser, LServer, <<"">>) of
        {error, _} ->
            false;
        {ok, Password} ->
            case mongoose_scram:enabled(LServer) of
                true ->
                    convert_scram_to_tuple(Password);
                false ->
                    Password
            end
    end.

-spec get_password_s(jid:luser(), jid:lserver()) -> binary().
get_password_s(User, Server) ->
    case get_password(User, Server) of
        Pass when is_binary(Pass) -> Pass;
        _ -> <<>>
    end.

-spec does_user_exist(jid:luser(), jid:lserver()) -> boolean().
does_user_exist(LUser, LServer) ->
    case make_req(get, <<"user_exists">>, LUser, LServer, <<"">>) of
        {ok, <<"true">>} -> true;
        _ -> false
    end.

-spec remove_user(jid:luser(), jid:lserver()) ->
    ok | {error, not_allowed}.
remove_user(LUser, LServer) ->
    case remove_user_req(LUser, LServer, <<"">>, <<"remove_user">>) of
        ok -> ok;
        _ -> {error, not_allowed}
    end.

-spec remove_user(jid:luser(), jid:lserver(), binary()) ->
    ok | {error, not_allowed | not_exists | bad_request}.
remove_user(LUser, LServer, Password) ->
    case mongoose_scram:enabled(LServer) of
        false ->
            remove_user_req(LUser, LServer, Password, <<"remove_user_validate">>);
        true ->
            case verify_scram_password(LUser, LServer, Password) of
                {ok, true} ->
                    remove_user_req(LUser, LServer, <<"">>, <<"remove_user">>);
                {ok, false} ->
                    {error, not_allowed};
                {error, _} = Error ->
                    Error
            end
    end.

-spec remove_user_req(binary(), binary(), binary(), binary()) ->
    ok | {error, not_exists | not_allowed | bad_request}.
remove_user_req(LUser, LServer, Password, Method) ->
    case make_req(post, Method, LUser, LServer, Password) of
        {error, not_allowed} -> {error, not_allowed};
        {error, not_found} -> {error, not_exists};
        {error, _} -> {error, bad_request};
        {ok, _} -> ok
    end.

%%%----------------------------------------------------------------------
%%% Request maker
%%%----------------------------------------------------------------------

-spec make_req(post | get, binary(), binary(), binary(), binary()) ->
    {ok, BodyOrCreated :: binary() | created} | {error, invalid_jid | http_error_atom() | binary()}.
make_req(_, _, LUser, LServer, _) when LUser == error orelse LServer == error ->
    {error, invalid_jid};
make_req(Method, Path, LUser, LServer, Password) ->
    LUserE = list_to_binary(http_uri:encode(binary_to_list(LUser))),
    LServerE = list_to_binary(http_uri:encode(binary_to_list(LServer))),
    PasswordE = list_to_binary(http_uri:encode(binary_to_list(Password))),
    Query = <<"user=", LUserE/binary, "&server=", LServerE/binary, "&pass=", PasswordE/binary>>,
    Header =  case ejabberd_auth:get_opt(LServer, basic_auth) of
                  undefined ->
                      [];
                  BasicAuth ->
                      BasicAuth64 = base64:encode(BasicAuth),
                      [{<<"Authorization">>, <<"Basic ", BasicAuth64/binary>>}]
              end,

    ?DEBUG("Making request '~s' for user ~s@~s...", [Path, LUser, LServer]),
    {ok, {Code, RespBody}} = case Method of
        get -> mongoose_http_client:get(LServer, auth, <<Path/binary, "?", Query/binary>>, Header);
        post -> mongoose_http_client:post(LServer, auth, Path, Header, Query)
    end,

    ?DEBUG("Request result: ~s: ~p", [Code, RespBody]),
    case Code of
        <<"409">> -> {error, conflict};
        <<"404">> -> {error, not_found};
        <<"401">> -> {error, not_authorized};
        <<"403">> -> {error, not_allowed};
        <<"400">> -> {error, RespBody};
        <<"204">> -> {ok, <<"">>};
        <<"201">> -> {ok, created};
        <<"200">> -> {ok, RespBody}
    end.

%%%----------------------------------------------------------------------
%%% Other internal functions
%%%----------------------------------------------------------------------

-spec verify_scram_password(binary(), binary(), binary()) ->
    {ok, boolean()} | {error, bad_request | not_exists}.
verify_scram_password(LUser, LServer, Password) ->
    case make_req(get, <<"get_password">>, LUser, LServer, <<"">>) of
        {ok, RawPassword} ->
            case mongoose_scram:deserialize(RawPassword) of
                {ok, #scram{} = ScramRecord} ->
                    {ok, mongoose_scram:check_password(Password, ScramRecord)};
                _ ->
                    {error, bad_request}
            end;
        _ ->
            {error, not_exists}
    end.

verify_cert(LUser, LServer, DerCert) ->
    case make_req(get, <<"get_certs">>, LUser, LServer, <<"">>) of
        {ok, PemCert} ->
            UserCert = {'Certificate', DerCert, not_encrypted},
            [] =/= [Cert || Cert <- public_key:pem_decode(PemCert), Cert =:= UserCert];
        _ -> false
    end.

get_server_and_user_name(Creds) ->
    User = mongoose_credentials:get(Creds, username),
    LUser = jid:nodeprep(User),
    LUser == error andalso error({nodeprep_error, User}),
    LServer = mongoose_credentials:lserver(Creds),
    {LServer, LUser}.

stop(_Host) ->
    ok.

-spec check_scram_password(binary(), binary(), binary(), fun()) -> boolean().
check_scram_password(OriginalPassword, GotPassword, Digest, DigestGen) ->
    case mongoose_scram:deserialize(GotPassword) of
        {ok, #scram{} = Scram} ->
            mongoose_scram:check_digest(Scram, Digest, DigestGen, OriginalPassword);
        _ ->
            false
    end.

-spec convert_scram_to_tuple(binary()) -> ejabberd_auth:passterm() | false.
convert_scram_to_tuple(Password) ->
    case mongoose_scram:deserialize(Password) of
        {ok, #scram{} = Scram} ->
            mongoose_scram:scram_to_tuple(Scram);
        _ ->
            false
    end.
