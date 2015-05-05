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
         check_password/3,
         check_password/5,
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
         plain_password_required/0,
         store_type/1,
         login/2,
         get_password/3,
         stop/1]).

-include("ejabberd.hrl").


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
    ChildMFA = {fusco, start_link, [AuthHost, Opts]},

    {ok, _} = supervisor:start_child(ejabberd_sup,
                                     {{ejabberd_auth_http_sup, Host},
                                      {cuesport, start_link,
                                       [pool_name(Host), PoolSize, ChildMods, ChildMFA]},
                                      transient, 2000, supervisor, [cuesport | ChildMods]}),
    ok.

-spec plain_password_required() -> false.
plain_password_required() ->
    false.

-spec store_type(binary()) -> plain | scram.
store_type(Server) ->
    case scram:enabled(Server) of
        false -> plain;
        true -> scram
    end.

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

-spec set_password(ejabberd:luser(), ejabberd:lserver(), binary()) -> ok | {error, term()}.
set_password(LUser, LServer, Password) ->
    PasswordFinal = case scram:enabled(LServer) of
                        true -> scram:serialize(scram:password_to_scram(
                                                  Password, scram:iterations(LServer)));
                        false -> Password
                    end,
    case make_req(post, <<"set_password">>, LUser, LServer, PasswordFinal) of
        {error, _} = Err -> Err;
        _ -> ok
    end.

-spec try_register(ejabberd:luser(), ejabberd:lserver(), binary()) -> {atomic, ok | exists} | {error, term()}.
try_register(LUser, LServer, Password) ->
    PasswordFinal = case scram:enabled(LServer) of
                        true -> scram:serialize(scram:password_to_scram(
                                                  Password, scram:iterations(LServer)));
                        false -> Password
                    end,
    case make_req(post, <<"register">>, LUser, LServer, PasswordFinal) of
        {ok, created} -> ok;
        {error, conflict} -> {error, exists};
        Error -> Error
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

-spec get_password(ejabberd:luser(), ejabberd:lserver()) -> false | binary() |
                                          {binary(), binary(), binary(), integer()}.
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

-spec remove_user(ejabberd:luser(), ejabberd:lserver()) -> ok | not_exists | not_allowed | bad_request.
remove_user(LUser, LServer) ->
    remove_user_req(LUser, LServer, <<"">>, <<"remove_user">>).

-spec remove_user(ejabberd:luser(), ejabberd:lserver(), binary()) -> ok | not_exists | not_allowed | bad_request.
remove_user(LUser, LServer, Password) ->
    case scram:enabled(LServer) of
        false ->
            remove_user_req(LUser, LServer, Password, <<"remove_user_validate">>);
        true ->
            case verify_scram_password(LUser, LServer, Password) of
                {ok, false} ->
                    not_allowed;
                {ok, true} ->
                    remove_user_req(LUser, LServer, <<"">>, <<"remove_user">>);
                {error, Error} ->
                    Error
            end
    end.

-spec remove_user_req(binary(), binary(), binary(), binary()) ->
    ok | not_exists | not_allowed | bad_request.
remove_user_req(LUser, LServer, Password, Method) ->
    case make_req(post, Method, LUser, LServer, Password) of
        {error, not_allowed} -> not_allowed;
        {error, not_found} -> not_exists;
        {error, _} -> bad_request;
        _ -> ok
    end.

%%%----------------------------------------------------------------------
%%% Request maker
%%%----------------------------------------------------------------------

-spec make_req(post | get, binary(), binary(), binary(), binary()) ->
    {ok, Body :: binary()} | {error, term()}.
make_req(_, _, LUser, LServer, _) when LUser == error orelse LServer == error ->
    {error, {prep_failed, LUser, LServer}};
make_req(Method, Path, LUser, LServer, Password) -> 
    AuthOpts = ejabberd_config:get_local_option(auth_opts, LServer),
    BasicAuth = case lists:keyfind(basic_auth, 1, AuthOpts) of
                    {_, BasicAuth0} -> BasicAuth0;
                    _ -> ""
                end,
    PathPrefix = case lists:keyfind(path_prefix, 1, AuthOpts) of
                     {_, Prefix} -> ejabberd_binary:string_to_binary(Prefix);
                     false -> <<"/">>
                 end,
    BasicAuth64 = base64:encode(BasicAuth),
    LUserE = list_to_binary(http_uri:encode(binary_to_list(LUser))),
    LServerE = list_to_binary(http_uri:encode(binary_to_list(LServer))),
    PasswordE = list_to_binary(http_uri:encode(binary_to_list(Password))),
    Query = <<"user=", LUserE/binary, "&server=", LServerE/binary, "&pass=", PasswordE/binary>>,
    Header = [{<<"Authorization">>, <<"Basic ", BasicAuth64/binary>>}],
    Connection = cuesport:get_worker(existing_pool_name(LServer)),

    ?DEBUG("Making request '~s' for user ~s@~s...", [Path, LUser, LServer]),
    {ok, {{Code, _Reason}, _RespHeaders, RespBody, _, _}} = case Method of
        get -> fusco:request(Connection, <<PathPrefix/binary, Path/binary, "?", Query/binary>>,
                             "GET", Header, "", 2, 5000);
        post -> fusco:request(Connection, <<PathPrefix/binary, Path/binary>>,
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

login(_User, _Server) ->
    erlang:error(not_implemented).

get_password(_User, _Server, _DefaultValue) ->
    erlang:error(not_implemented).

stop(_Host) ->
    ok.
