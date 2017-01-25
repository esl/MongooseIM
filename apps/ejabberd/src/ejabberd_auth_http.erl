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
         make_http_req/5,
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
         store_type/1,
         stop/1]).

%% Pre-mongoose_credentials API
-export([check_password/3,
         check_password/5]).

-include("ejabberd.hrl").

-type http_error_atom() :: conflict | not_found | not_authorized | not_allowed.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(binary()) -> ok.
start(Host) ->
    AuthOpts = ejabberd_config:get_local_option(auth_opts, Host),
    {_, AuthHost} = lists:keyfind(host, 1, AuthOpts),
    PoolSize = proplists:get_value(connection_pool_size, AuthOpts, 10),
    Opts = proplists:get_value(connection_opts, AuthOpts, []),
    ChildMods = [fusco],
    ChildMF = {fusco, start_link},
    ChildArgs = {for_all, [AuthHost, Opts]},

    {ok, _} = supervisor:start_child(ejabberd_sup,
                                     {{ejabberd_auth_http_sup, Host},
                                      {cuesport, start_link,
                                       [pool_name(Host), PoolSize, ChildMods, ChildMF, ChildArgs]},
                                      transient, 2000, supervisor, [cuesport | ChildMods]}),
    ok.


-spec store_type(binary()) -> plain | scram.
store_type(Server) ->
    case scram:enabled(Server) of
        false ->
            case is_external(Server) of
                true -> external;
                _ -> plain
            end;
        true -> scram
    end.

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    ejabberd_auth:authorize_with_check_password(?MODULE, Creds).

-spec check_password(ejabberd:luser(), ejabberd:lserver(), binary()) -> boolean().
check_password(LUser, LServer, Password) ->
    case scram:enabled(LServer) of
        false ->
            case make_req(get, <<"check_password">>, LUser, LServer, Password) of
                {ok, <<"true">>} -> true;
                _ -> false
            end;
        true ->
            {ok, true} =:= verify_scram_password(LUser, LServer, Password)
    end.

-spec check_password(ejabberd:luser(), ejabberd:lserver(), binary(), binary(), fun()) -> boolean().
check_password(LUser, LServer, Password, Digest, DigestGen) ->
    case make_req(get, <<"get_password">>, LUser, LServer, <<"">>) of
        {error, _} ->
            false;
        {ok, GotPasswd} ->
            case scram:enabled(LServer) of
                true ->
                    case scram:deserialize(GotPasswd) of
                        {ok, #scram{} = Scram} ->
                            scram:check_digest(Scram, Digest, DigestGen, Password);
                        _ ->
                            false
                    end;
                false ->
                    ejabberd_auth:check_digest(Digest, DigestGen, Password, GotPasswd)
            end
    end.

-spec set_password(ejabberd:luser(), ejabberd:lserver(), binary()) -> ok | {error, not_allowed}.
set_password(LUser, LServer, Password) ->
    PasswordFinal = case scram:enabled(LServer) of
                        true -> scram:serialize(scram:password_to_scram(
                                                  Password, scram:iterations(LServer)));
                        false -> Password
                    end,
    case make_req(post, <<"set_password">>, LUser, LServer, PasswordFinal) of
        {ok, _} -> ok;
        {error, invalid_jid} = Error -> Error;
        {error, _} -> {error, not_allowed}
    end.

-spec try_register(ejabberd:luser(), ejabberd:lserver(), binary()) ->
    ok | {error, exists | not_allowed}.
try_register(LUser, LServer, Password) ->
    PasswordFinal = case scram:enabled(LServer) of
                        true -> scram:serialize(scram:password_to_scram(
                                                  Password, scram:iterations(LServer)));
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

-spec get_vh_registered_users(ejabberd:lserver()) -> [].
get_vh_registered_users(_Server) ->
    [].

-spec get_vh_registered_users(ejabberd:lserver(), list()) -> [].
get_vh_registered_users(_Server, _Opts) ->
    [].

-spec get_vh_registered_users_number(binary()) -> 0.
get_vh_registered_users_number(_Server) ->
    0.

-spec get_vh_registered_users_number(ejabberd:lserver(), list()) -> 0.
get_vh_registered_users_number(_Server, _Opts) ->
    0.

-spec get_password(ejabberd:luser(), ejabberd:lserver()) -> ejabberd_auth:passwordlike() | false.
get_password(LUser, LServer) ->
    case make_req(get, <<"get_password">>, LUser, LServer, <<"">>) of
        {error, _} ->
            false;
        {ok, Password} ->
            case scram:enabled(LServer) of
                true ->
                    case scram:deserialize(Password) of
                        {ok, #scram{} = Scram} ->
                            scram:scram_to_tuple(Scram);
                        _ ->
                            false
                    end;
                false ->
                    Password
            end
    end.

-spec get_password_s(ejabberd:luser(), ejabberd:lserver()) -> binary().
get_password_s(User, Server) ->
    case get_password(User, Server) of
        Pass when is_binary(Pass) -> Pass;
        _ -> <<>>
    end.

-spec does_user_exist(ejabberd:luser(), ejabberd:lserver()) -> boolean().
does_user_exist(LUser, LServer) ->
    case make_req(get, <<"user_exists">>, LUser, LServer, <<"">>) of
        {ok, <<"true">>} -> true;
        _ -> false
    end.

-spec remove_user(ejabberd:luser(), ejabberd:lserver()) ->
    ok | {error, not_allowed}.
remove_user(LUser, LServer) ->
    case remove_user_req(LUser, LServer, <<"">>, <<"remove_user">>) of
        ok -> ok;
        _ -> {error, not_allowed}
    end.

-spec remove_user(ejabberd:luser(), ejabberd:lserver(), binary()) ->
    ok | {error, not_allowed | not_exists | bad_request}.
remove_user(LUser, LServer, Password) ->
    case scram:enabled(LServer) of
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
    AuthOpts = ejabberd_config:get_local_option(auth_opts, LServer),
    PathPrefix = case lists:keyfind(path_prefix, 1, AuthOpts) of
                     {_, Prefix} -> ejabberd_binary:string_to_binary(Prefix);
                     false -> <<"/">>
                 end,
    LUserE = list_to_binary(http_uri:encode(binary_to_list(LUser))),
    LServerE = list_to_binary(http_uri:encode(binary_to_list(LServer))),
    PasswordE = list_to_binary(http_uri:encode(binary_to_list(Password))),
    Query = <<"user=", LUserE/binary, "&server=", LServerE/binary, "&pass=", PasswordE/binary>>,
    Header = case lists:keyfind(basic_auth, 1, AuthOpts) of
                 {_, BasicAuth} ->
                     BasicAuth64 = base64:encode(BasicAuth),
                     [{<<"Authorization">>, <<"Basic ", BasicAuth64/binary>>}];
                 _ -> []
             end,
    FullPath = <<PathPrefix/binary, Path/binary>>,
    ?DEBUG("Making request '~s' for user ~s@~s...", [Path, LUser, LServer]),
    make_http_req(LServer, Method, FullPath, Query, Header).

make_http_req(LServer, Method, FullPath, Query, Header) ->
    Connection = cuesport:get_worker(existing_pool_name(LServer)),

    {ok, {{Code, _Reason}, _RespHeaders, RespBody, _, _}} = case Method of
        get -> fusco:request(Connection, <<FullPath/binary, "?", Query/binary>>,
                             "GET", Header, "", 2, 5000);
        post -> fusco:request(Connection, FullPath,
                              "POST", Header, Query, 2, 5000)
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
-spec pool_name(binary()) -> atom().
pool_name(Host) ->
    list_to_atom("ejabberd_auth_http_" ++ binary_to_list(Host)).

-spec existing_pool_name(binary()) -> atom().
existing_pool_name(Host) ->
    list_to_existing_atom("ejabberd_auth_http_" ++ binary_to_list(Host)).

-spec verify_scram_password(binary(), binary(), binary()) ->
    {ok, boolean()} | {error, bad_request | not_exists}.
verify_scram_password(LUser, LServer, Password) ->
    case make_req(get, <<"get_password">>, LUser, LServer, <<"">>) of
        {ok, RawPassword} ->
            case scram:deserialize(RawPassword) of
                {ok, #scram{} = ScramRecord} ->
                    {ok, scram:check_password(Password, ScramRecord)};
                _ ->
                    {error, bad_request}
            end;
        _ ->
            {error, not_exists}
    end.

stop(Host) ->
    Id = {ejabberd_auth_http_sup, Host},
    supervisor:terminate_child(ejabberd_sup, Id),
    supervisor:delete_child(ejabberd_sup, Id),
    ok.

is_external(Host) ->
    case ejabberd_config:get_local_option(auth_opts, Host) of
        undefined ->
            false;
        AuthOpts ->
            {is_external, true} == lists:keyfind(is_external, 1, AuthOpts)
    end.
