%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_http.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Authentication via HTTP request
%%% Created : 23 Sep 2013 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(ejabberd_auth_http).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(mongoose_gen_auth).

%% External exports
-export([start/1,
         stop/1,
         config_spec/0,
         supports_sasl_module/2,
         set_password/4,
         authorize/1,
         try_register/4,
         get_password/3,
         get_password_s/3,
         does_user_exist/3,
         remove_user/3,
         supported_features/0]).

%% Pre-mongoose_credentials API
-export([check_password/4,
         check_password/6]).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

-type http_error_atom() :: conflict | not_found | not_authorized | not_allowed.
-type params() :: #{luser := jid:luser(),
                    lserver := jid:lserver(),
                    host_type := mongooseim:host_type(),
                    password => binary()}.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(mongooseim:host_type()) -> ok.
start(_HostType) ->
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{<<"basic_auth">> => #option{type = string}}}.

-spec supports_sasl_module(mongooseim:host_type(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_HostType, cyrsasl_plain) -> true;
supports_sasl_module(HostType, cyrsasl_digest) ->  not mongoose_scram:enabled(HostType);
supports_sasl_module(HostType, Mechanism) -> mongoose_scram:enabled(HostType, Mechanism).

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    case mongoose_credentials:get(Creds, der_cert, undefined) of
        undefined -> ejabberd_auth:authorize_with_check_password(?MODULE, Creds);
        DerCert ->
            case verify_cert(Creds, DerCert) of
                true -> {ok, mongoose_credentials:set(Creds, auth_module, ?MODULE)};
                false -> {error, not_authorized}
            end
    end.


-spec check_password(mongooseim:host_type(), jid:luser(), jid:lserver(), binary()) -> boolean().
check_password(HostType, LUser, LServer, Password) ->
    Params = #{luser => LUser, lserver => LServer, host_type => HostType, password => Password},
    case mongoose_scram:enabled(HostType) of
        false ->
            case make_req(get, <<"check_password">>, Params) of
                {ok, <<"true">>} -> true;
                _ -> false
            end;
        true ->
            {ok, true} =:= verify_scram_password(Params)
    end.

-spec check_password(mongooseim:host_type(), jid:luser(), jid:lserver(),
                     binary(), binary(), fun()) -> boolean().
check_password(HostType, LUser, LServer, Password, Digest, DigestGen) ->
    case make_req(get, <<"get_password">>, #{luser => LUser,
                                             lserver => LServer,
                                             host_type => HostType}) of
        {error, _} ->
            false;
        {ok, GotPasswd} ->
            case mongoose_scram:enabled(HostType) of
                true ->
                    check_scram_password(GotPasswd, Password, Digest, DigestGen);
                false ->
                    ejabberd_auth:check_digest(Digest, DigestGen, Password, GotPasswd)
            end
    end.

-spec set_password(mongooseim:host_type(), jid:luser(), jid:lserver(),
                   binary()) -> ok | {error, not_allowed}.
set_password(HostType, LUser, LServer, Password) ->
    PasswordFinal = case mongoose_scram:enabled(HostType) of
                        true -> mongoose_scram:serialize(mongoose_scram:password_to_scram(HostType,
                                                  Password, mongoose_scram:iterations(HostType)));
                        false -> Password
                    end,
    case make_req(post, <<"set_password">>, #{luser => LUser,
                                             lserver => LServer,
                                             host_type => HostType,
                                             password => PasswordFinal}) of
        {ok, _} -> ok;
        {error, invalid_jid} = Error -> Error;
        {error, _} -> {error, not_allowed}
    end.

-spec try_register(mongooseim:host_type(), jid:luser(), jid:lserver(), binary()) ->
    ok | {error, exists | not_allowed}.
try_register(HostType, LUser, LServer, Password) ->
    PasswordFinal = case mongoose_scram:enabled(HostType) of
                        true -> mongoose_scram:serialize(mongoose_scram:password_to_scram(HostType,
                                                  Password, mongoose_scram:iterations(HostType)));
                        false -> Password
                    end,
    case make_req(post, <<"register">>, #{luser => LUser,
                                          lserver => LServer,
                                          host_type => HostType,
                                          password => PasswordFinal}) of
        {ok, created} -> ok;
        {error, conflict} -> {error, exists};
        _Error -> {error, not_allowed}
    end.

-spec get_password(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
          ejabberd_auth:passterm() | false.
get_password(HostType, LUser, LServer) ->
    case make_req(get, <<"get_password">>, #{luser => LUser,
                                             lserver => LServer,
                                             host_type => HostType}) of
        {error, _} ->
            false;
        {ok, Password} ->
            case mongoose_scram:enabled(HostType) of
                true ->
                    convert_scram_to_tuple(Password);
                false ->
                    Password
            end
    end.

-spec get_password_s(mongooseim:host_type(), jid:luser(), jid:lserver()) -> binary().
get_password_s(HostType, User, Server) ->
    case get_password(HostType, User, Server) of
        Pass when is_binary(Pass) -> Pass;
        _ -> <<>>
    end.

-spec does_user_exist(mongooseim:host_type(), jid:luser(), jid:lserver()) -> boolean().
does_user_exist(HostType, LUser, LServer) ->
    case make_req(get, <<"user_exists">>, #{luser => LUser,
                                            lserver => LServer,
                                            host_type => HostType}) of
        {ok, <<"true">>} -> true;
        _ -> false
    end.

-spec remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    ok | {error, not_allowed}.
remove_user(HostType, LUser, LServer) ->
    case make_req(post, <<"remove_user">>,  #{luser => LUser,
                                              lserver => LServer,
                                              host_type => HostType}) of
        {ok, _} -> ok;
        {error, _} -> {error, not_allowed}
    end.

-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].

%%%----------------------------------------------------------------------
%%% Request maker
%%%----------------------------------------------------------------------

-spec make_req(post | get, binary(), params()) ->
    {ok, BodyOrCreated :: binary() | created} | {error, invalid_jid
        | http_error_atom() | binary()}.
make_req(_, _, #{luser := LUser, lserver := LServer}) when LUser == error orelse LServer == error ->
    {error, invalid_jid};
make_req(Method, Path, Params = #{luser := LUser, lserver := LServer, host_type := HostType}) ->
    Password = maps:get(password, Params, <<>>),
    Query = uri_string:compose_query([{<<"user">>, LUser},
                                      {<<"server">>, LServer},
                                      {<<"pass">>, Password}]),
    Header = case mongoose_config:lookup_opt([{auth, HostType}, http, basic_auth]) of
                 {error, not_found} ->
                     [];
                 {ok, BasicAuth} ->
                     BasicAuth64 = base64:encode(BasicAuth),
                     [{<<"Authorization">>, <<"Basic ", BasicAuth64/binary>>}]
             end,

    Result = case Method of
        get -> mongoose_http_client:get(HostType, auth, <<Path/binary, "?", Query/binary>>, Header);
        post -> mongoose_http_client:post(HostType, auth, Path, Header, Query)
    end,
    handle_result(Result, Method, Path, LUser, HostType).

handle_result({ok, {Code, RespBody}}, Method, Path, LUser, HostType) ->
    ?LOG_DEBUG(#{what => http_auth_request, text => <<"Received HTTP request result">>,
                 path => Path, method => Method, user => LUser, host_type => HostType,
                 code => Code, result => RespBody}),
    case Code of
        <<"409">> -> {error, conflict};
        <<"404">> -> {error, not_found};
        <<"401">> -> {error, not_authorized};
        <<"403">> -> {error, not_allowed};
        <<"400">> -> {error, RespBody};
        <<"204">> -> {ok, <<>>};
        <<"201">> -> {ok, created};
        <<"200">> -> {ok, RespBody}
    end;
handle_result({error, Reason}, Method, Path, LUser, HostType) ->
    ?LOG_DEBUG(#{what => http_auth_request_failed, text => <<"HTTP request failed">>,
                 path => Path, method => Method, user => LUser, host_type => HostType,
                 reason => Reason}),
    {error, bad_request}.

%%%----------------------------------------------------------------------
%%% Other internal functions
%%%----------------------------------------------------------------------

-spec verify_scram_password(params()) -> {ok, boolean()} | {error, bad_request | not_exists}.
verify_scram_password(Params) ->
    {Password, ParamsWithoutPass} = maps:take(password, Params),
    case make_req(get, <<"get_password">>, ParamsWithoutPass) of
        {ok, RawPassword} ->
            case mongoose_scram:deserialize(RawPassword) of
                {ok, DeserializedScramMap} ->
                    {ok, mongoose_scram:check_password(Password, DeserializedScramMap)};
                {error, Reason} ->
                    ?LOG_WARNING(#{what => scram_serialisation_incorrect, reason => Reason,
                                   params => Params}),
                    {error, bad_request}
            end;
        _ ->
            {error, not_exists}
    end.

verify_cert(Creds, DerCert) ->
    Params = creds_to_params(Creds),
    case make_req(get, <<"get_certs">>, Params) of
        {ok, PemCert} ->
            UserCert = {'Certificate', DerCert, not_encrypted},
            [] =/= [Cert || Cert <- public_key:pem_decode(PemCert), Cert =:= UserCert];
        _ -> false
    end.

creds_to_params(Creds) ->
    User = mongoose_credentials:get(Creds, username),
    LUser = jid:nodeprep(User),
    LUser == error andalso error({nodeprep_error, User}),
    HostType = mongoose_credentials:host_type(Creds),
    LServer = mongoose_credentials:lserver(Creds),
    #{host_type => HostType, luser => LUser, lserver => LServer}.

-spec check_scram_password(binary(), binary(), binary(), fun()) -> boolean().
check_scram_password(OriginalPassword, GotPassword, Digest, DigestGen) ->
    case mongoose_scram:deserialize(GotPassword) of
        {ok, Scram} ->
            mongoose_scram:check_digest(Scram, Digest, DigestGen, OriginalPassword);
        _ ->
            false
    end.

-spec convert_scram_to_tuple(binary()) -> ejabberd_auth:passterm() | false.
convert_scram_to_tuple(Password) ->
    case mongoose_scram:deserialize(Password) of
        {ok, Scram} ->
           Scram;
        _ ->
            false
    end.
