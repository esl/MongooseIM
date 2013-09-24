%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_http.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : Authentification via HTTP request
%%% Created : 23 Sep 2013 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(ejabberd_auth_http).
-author('piotr.nosek@erlang-solutions.com').

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
         is_user_exists/2,
         remove_user/2,
         remove_user/3,
         plain_password_required/0
        ]).

-include("ejabberd.hrl").

-define(btoea(X), list_to_existing_atom(binary_to_list(X))).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(_Host) ->
    application:ensure_started(lhttpc),
    ok.

plain_password_required() ->
    true.

check_password(User, Server, Password) ->
    ?btoea(make_req(get, "check_password", undefined, User, Server, Password)).

check_password(_User, _Server, _Password, _Digest, _DigestGen) ->
    false.

%% @spec (User::string(), Server::string(), Password::string()) -> ok | {error, Reason}
set_password(User, Server, Password) ->
    case make_req(post, "set_password", {set_password, false}, User, Server, Password) of
        {error, _} = Err -> Err;
        _ -> ok
    end.

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, Reason}
try_register(User, Server, Password) ->
    case make_req(post, "register", {register, false}, User, Server, Password) of
        created -> {atomic, ok};
        conflict -> {atomic, exists};
        Error -> Error
    end.

dirty_get_registered_users() ->
    [].

get_vh_registered_users(_Server) ->
    [].

get_vh_registered_users(_Server, _) ->
    [].

get_vh_registered_users_number(_Server) ->
    0.

get_vh_registered_users_number(_Server, _) ->
    0.

get_password(User, Server) ->
    case make_req(get, "get_password", {get_password, false}, User, Server, <<"">>) of
        not_found -> false;
        PassOrErr -> PassOrErr
    end.

get_password_s(User, Server) ->
    case get_password(User, Server) of
        false -> <<>>;
        PassOrErr -> PassOrErr
    end.

%% @spec (User, Server) -> true | false
is_user_exists(User, Server) ->
    ?btoea(make_req(get, "user_exists", undefined, User, Server, <<"">>)).

%% @spec (User, Server) -> ok | not_exists | not_allowed | bad_request
remove_user(User, Server) ->
    remove_user(User, Server, <<"">>).

%% @spec (User, Server, Password) -> ok | not_exists | not_allowed | bad_request
remove_user(User, Server, Password) ->
    case make_req(post, "remove_user", {remove_user, false}, User, Server, Password) of
        {error, not_allowed} -> not_allowed;
        not_found -> not_exists;
        {error, _} -> bad_request;
        _ -> ok
    end.

make_req(Method, Suffix, undefined, User, Server, Password) ->
    LServer = jlib:nameprep(Server),
    make_req(Method, Suffix, User, LServer, Password);
make_req(Method, Suffix, {Condition, Default}, User, Server, Password) ->
    LServer = jlib:nameprep(Server),
    case {lists:keyfind(Condition, 1,
            ejabberd_config:get_local_option(auth_opts, LServer)), Default} of
        {{_, true}, _} -> make_req(Method, Suffix, User, LServer, Password);
        {_, true} -> make_req(Method, Suffix, User, LServer, Password);
        _ -> {error, not_allowed}
    end.

make_req(Method, Suffix, User, LServer, Password) ->
    LUser = jlib:nodeprep(User),

    AuthOpts = ejabberd_config:get_local_option(auth_opts, LServer),
    {_, BaseURL} = lists:keyfind(base_url, 1, AuthOpts),
    {_, BasicAuth} = lists:keyfind(basic_auth, 1, AuthOpts),
    LUserE = http_uri:encode(binary_to_list(LUser)),
    LServerE = http_uri:encode(binary_to_list(LServer)),
    PasswordE = http_uri:encode(binary_to_list(Password)),
    Query = "user=" ++ LUserE ++ "&server=" ++ LServerE ++ "&pass=" ++ PasswordE,
    Header = [{"Authorization", "Basic " ++ base64:encode_to_string(BasicAuth)}],

    {ok, {{Code, _Reason}, _RespHdr, RespBody}} = case Method of
        get -> lhttpc:request(BaseURL ++ Suffix ++ "?" ++ Query, "GET", Header, "", 5000);
        post -> lhttpc:request(BaseURL ++ Suffix, "POST", Header, Query, 5000)
    end,
    case Code of
        409 -> conflict;
        404 -> not_found;
        401 -> {error, not_authorized};
        403 -> {error, not_allowed};
        400 -> {error, RespBody};
        204 -> "";
        201 -> created;
        200 -> RespBody
    end.
