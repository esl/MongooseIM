%%% File    : ejabberd_auth.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification
%%% Created : 23 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth).
-author('alexey@process-one.net').

%% External exports
-export([start/0,
         start/1,
         stop/1,
         set_opts/2,
         get_opt/3,
         get_opt/2,
         authorize/1,
         set_password/2,
         check_password/2,
         check_password/4,
         try_register/2,
         dirty_get_registered_users/0,
         get_vh_registered_users/1,
         get_vh_registered_users/2,
         get_vh_registered_users_number/1,
         get_vh_registered_users_number/2,
         get_password_s/1,
         get_passterm_with_authmodule/1,
         does_user_exist/1,
         does_user_exist_in_other_modules/3,
         does_method_support/2,
         remove_user/1,
         supports_sasl_module/2,
         entropy/1
        ]).

-export([check_digest/4]).

-export([auth_modules/1,
         auth_methods/1]).

%% Library functions for reuse in ejabberd_auth_* modules
-export([authorize_with_check_password/2]).

-include("mongoose.hrl").
-include("jlib.hrl").

-export_type([authmodule/0,
              passterm/0]).

-type authmodule() :: module().
-type passterm() :: binary() | mongoose_scram:scram_tuple() | mongoose_scram:scram_map().

-define(METRIC(Name), [backends, auth, Name]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-spec start() -> 'ok'.
start() ->
    lists:foreach(fun start/1, ?ALL_HOST_TYPES).

-spec start(HostType :: binary()) -> 'ok'.
start(HostType) ->
    ensure_metrics(HostType),
    lists:foreach(
      fun(M) ->
              M:start(HostType)
      end, auth_modules_for_host_type(HostType)).

-spec stop(HostType :: binary()) -> 'ok'.
stop(HostType) ->
    lists:foreach(
      fun(M) ->
              M:stop(HostType)
      end, auth_modules_for_host_type(HostType)).

-spec set_opts(HostType :: binary(),
               KVs :: [tuple()]) ->  {atomic|aborted, _}.
set_opts(HostType, KVs) ->
    OldOpts = ejabberd_config:get_local_option(auth_opts, HostType),
    AccFunc = fun({K, V}, Acc) ->
                  lists:keystore(K, 1, Acc, {K, V})
              end,
    NewOpts = lists:foldl(AccFunc, OldOpts, KVs),
    ejabberd_config:add_local_option({auth_opts, HostType}, NewOpts).

-spec get_opt(HostType :: binary(),
              Opt :: atom(),
              Default :: ejabberd:value()) -> undefined | ejabberd:value().
get_opt(HostType, Opt, Default) ->
    case ejabberd_config:get_local_option(auth_opts, HostType) of
        undefined ->
            Default;
        Opts ->
            case lists:keyfind(Opt, 1, Opts) of
                {Opt, Value} ->
                    Value;
                false ->
                    Default
            end
    end.

get_opt(HostType, Opt) ->
    get_opt(HostType, Opt, undefined).

-spec supports_sasl_module(binary(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(HostType, Module) ->
    lists:any(fun(M) -> M:supports_sasl_module(HostType, Module) end,
              auth_modules_for_host_type(HostType)).

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    HostType = mongoose_credentials:host_type(Creds),
    timed_call(HostType, authorize, fun authorize_loop/2,
               [ auth_modules_for_host_type(HostType), Creds]).

-spec authorize_loop([AuthM], Creds) -> {ok, R}
                                      | {error, any()} when
      AuthM :: ejabberd_gen_auth:t(),
      Creds :: mongoose_credentials:t(),
      R     :: mongoose_credentials:t().
authorize_loop([], Creds) -> {error, {no_auth_modules, Creds}};
authorize_loop(Modules, Creds) -> do_authorize_loop(Modules, Creds).

do_authorize_loop([], _Creds) -> {error, not_authorized};
do_authorize_loop([M | Modules], Creds) ->
    try
        {ok, NewCreds} = M:authorize(Creds),
        {ok, mongoose_credentials:register(NewCreds, M, success)}
    catch
        _:R -> do_authorize_loop(Modules,
                              mongoose_credentials:register(Creds, M, {failure, R}))
    end.


%% @doc Check if the user and password can login in server.
-spec check_password(JID :: jid:jid() | error, Password :: binary()) -> boolean().
check_password(error, _Password) ->
    false;
check_password(#jid{luser = LUser, lserver = LServer}, Password) ->
    CheckPasswordArgsWithoutHostType = [LUser, LServer, Password],
    case check_password_for_domain(LServer, CheckPasswordArgsWithoutHostType) of
        {true, _AuthModule} -> true;
        false -> false
    end.

%% @doc Check if the user and password can login in server.
-spec check_password(JID :: jid:jid() | error,
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun((binary()) -> binary())) -> boolean().
check_password(error, _, _, _) ->
    false;
check_password(#jid{luser = LUser, lserver = LServer}, Password, Digest, DigestGen) ->
    CheckPasswordArgsWithoutHostType = [LUser, LServer, Password, Digest, DigestGen],
    case check_password_for_domain(LServer, CheckPasswordArgsWithoutHostType) of
        {true, _AuthModule} -> true;
        false -> false
    end.

%% @doc Check if the user and password can login in server.
%% The user can login if at least an authentication method accepts the user
%% and the password.
-spec check_password_for_domain(LServer :: jid:lserver(),
                                ArgsWithoutHostType :: [any(), ...]) ->
                                   'false' | {'true', authmodule()}.
check_password_for_domain(LServer, ArgsWithoutHostType) ->
    case mongoose_domain_api:get_host_type(LServer) of
        {error, not_found} -> false;
        {ok, HostType} ->
            Args = [HostType | ArgsWithoutHostType],
            check_password_for_host_type(HostType, Args)
    end.

check_password_for_host_type(HostType, Args) when length(Args) =:= 4;
                                                  length(Args) =:= 6 ->
    case auth_modules_for_host_type(HostType) of
        [] -> false;
        AuthModules ->
            timed_call(HostType, check_password,
                       fun do_check_password_loop/2,
                       [AuthModules, Args])
    end.

do_check_password_loop([], _) ->
    false;
do_check_password_loop([AuthModule | AuthModules], Args) ->
    case apply(AuthModule, check_password, Args) of
        true ->
            {true, AuthModule};
        false ->
            do_check_password_loop(AuthModules, Args)
    end.

-spec check_digest(binary(), fun((binary()) -> binary()), binary(), binary()) -> boolean().
check_digest(<<>>, _, <<>>, _) ->
    false; %%empty digest and password
check_digest(Digest, DigestGen, _Password, Passwd) ->
    Digest == DigestGen(Passwd).

-spec set_password(jid:jid() | error, binary()) ->
    ok | {error, empty_password | not_allowed | invalid_jid}.
set_password(_, <<"">>) ->
    {error, empty_password};
set_password(error, _) ->
    {error, invalid_jid};
set_password(#jid{luser = LUser, lserver = LServer}, Password) ->
    case mongoose_domain_api:get_host_type(LServer) of
        {ok, HostType} ->
            lists:foldl(
                fun(M, {error, _}) ->
                    M:set_password(HostType, LUser, LServer, Password);
                   (_M, Res) ->
                       Res
                end, {error, not_allowed}, auth_modules_for_host_type(HostType));
        {error, not_found} -> {error, not_allowed}
    end.


-spec try_register(jid:jid() | error, binary()) ->
    ok | {error, exists | not_allowed | invalid_jid | null_password}.
try_register(_, <<"">>) ->
    {error, null_password};
try_register(error, _) ->
    {error, invalid_jid};
try_register(JID, Password) ->
    Exists = does_user_exist(JID),
    do_try_register_if_does_not_exist(Exists, JID, Password).

-spec do_try_register_if_does_not_exist(boolean(), jid:jid(), binary()) ->
    ok | {error, exists | not_allowed | invalid_jid | null_password}.
do_try_register_if_does_not_exist(true, _, _) ->
    {error, exists};
do_try_register_if_does_not_exist(_, JID, Password) ->
    {LUser, LServer} = jid:to_lus(JID),
    case mongoose_domain_api:get_host_type(LServer) of
        {ok, HostType} ->
            timed_call(HostType, try_register,
                       fun do_try_register_if_does_not_exist_timed/4, [HostType, LUser, LServer, Password]);
        {error, not_found} ->
            {error, not_allowed}
    end.

do_try_register_if_does_not_exist_timed(HostType, LUser, LServer, Password) ->
    Backends = auth_modules_for_host_type(HostType),
    do_try_register_in_backend(Backends, HostType, LUser, LServer, Password).

do_try_register_in_backend([], _, _, _, _) ->
    {error, not_allowed};
do_try_register_in_backend([M | Backends], HostType, LUser, LServer, Password) ->
    case M:try_register(HostType, LUser, LServer, Password) of
        ok ->
            mongoose_hooks:register_user(LServer, ok, LUser);
        _ ->
            do_try_register_in_backend(Backends, HostType, LUser, LServer, Password)
    end.

%% @doc Registered users list do not include anonymous users logged
-spec dirty_get_registered_users() -> [jid:simple_bare_jid()].
dirty_get_registered_users() ->
    lists:flatmap(
      fun(M) ->
              M:dirty_get_registered_users()
      end, auth_modules()).


%% @doc Registered users list do not include anonymous users logged
-spec get_vh_registered_users(Server :: jid:server()) -> [jid:simple_bare_jid()].
get_vh_registered_users(Server) ->
    LServer = jid:nameprep(Server),
    do_get_vh_registered_users(LServer).

do_get_vh_registered_users(error) ->
    [];
do_get_vh_registered_users(LServer) ->
    lists:flatmap(
      fun(M) ->
              M:get_vh_registered_users(LServer)
      end, auth_modules(LServer)).


-spec get_vh_registered_users(Server :: jid:server(), Opts :: [any()]) ->
    [jid:simple_bare_jid()].
get_vh_registered_users(Server, Opts) ->
    LServer = jid:nameprep(Server),
    do_get_vh_registered_users(LServer, Opts).

do_get_vh_registered_users(error, _) ->
    [];
do_get_vh_registered_users(LServer, Opts) ->
    lists:flatmap(
        fun(M) ->
            M:get_vh_registered_users(LServer, Opts)
        end, auth_modules(LServer)).


-spec get_vh_registered_users_number(Server :: jid:server()) -> integer().
get_vh_registered_users_number(Server) ->
    LServer = jid:nameprep(Server),
    do_get_vh_registered_users_number(LServer).

do_get_vh_registered_users_number(error) ->
    0;
do_get_vh_registered_users_number(LServer) ->
    lists:sum(
        lists:map(
            fun(M) ->
                M:get_vh_registered_users_number(LServer)
            end, auth_modules(LServer))).


-spec get_vh_registered_users_number(Server :: jid:server(), Opts :: list()) -> integer().
get_vh_registered_users_number(Server, Opts) ->
    LServer = jid:nameprep(Server),
    do_get_vh_registered_users_number(LServer, Opts).

do_get_vh_registered_users_number(error, _) ->
    0;
do_get_vh_registered_users_number(LServer, Opts) ->
    lists:sum(
        lists:map(
            fun(M) ->
                M:get_vh_registered_users_number(LServer, Opts)
            end, auth_modules(LServer))).


-spec get_password_s(JID :: jid:jid() | error) -> binary().
get_password_s(#jid{luser = LUser, lserver = LServer}) ->
    lists:foldl(
        fun(M, <<"">>) ->
            M:get_password_s(LUser, LServer);
            (_M, Password) ->
                Password
        end, <<"">>, auth_modules(LServer));
get_password_s(error) ->
    <<"">>.

%% @doc Get the password(like thing) of the user and the auth module.
-spec get_passterm_with_authmodule(error | jid:jid()) -> R when
      R :: {passterm(), authmodule()} | {false, none}.
get_passterm_with_authmodule(#jid{luser = LUser, lserver = LServer}) ->
    lists:foldl(
        fun(M, {false, _}) ->
                {M:get_password(LUser, LServer), M};
           (_M, {Password, AuthModule}) ->
                {Password, AuthModule}
        end, {false, none}, auth_modules(LServer)).

%% @doc Returns true if the user exists in the DB or if an anonymous user is
%% logged under the given name
-spec does_user_exist(JID :: jid:jid() | error) -> boolean().
does_user_exist(#jid{luser = LUser, lserver = LServer}) ->
    timed_call(LServer, does_user_exist, fun does_user_exist_timed/2, [LUser, LServer]);
does_user_exist(error) ->
    false.

does_user_exist_timed(LUser, LServer) ->
    Modules = auth_modules(LServer),
    does_user_exist_in_given_modules(Modules, LUser, LServer, false).

-spec does_method_support(AuthMethod :: atom(), Feature :: atom()) -> boolean().
does_method_support(AuthMethod, Feature) ->
    Module = auth_method_to_module(AuthMethod),
    lists:member(Feature, get_supported_features(Module)).

-spec get_supported_features(Module :: authmodule()) -> [Feature :: atom()].
get_supported_features(Module) ->
    %% if module is not loaded, erlang:function_exported/3 returns false.
    %% we can safely ignore any error returned from code:ensure_loaded/1.
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, supported_features, 0) of
        true -> apply(Module, supported_features, []);
        false -> []
    end.

%% Check if the user exists in all authentications module
%% except the module passed as parameter
-spec does_user_exist_in_other_modules(HostType :: binary(),
                                       Module :: authmodule(),
                                       JID :: jid:jid() | error) ->
    boolean() | maybe.
does_user_exist_in_other_modules(HostType, Module, #jid{luser = LUser, lserver = LServer}) ->
    Modules = auth_modules_for_host_type(HostType) -- [Module],
    does_user_exist_in_given_modules(Modules, LUser, LServer, maybe);
does_user_exist_in_other_modules(_HostType, _M, error) ->
    false.

does_user_exist_in_given_modules([], _, _, _) ->
    false;
does_user_exist_in_given_modules(_, LUser, LServer, Default)
  when LUser =:= error; LServer =:= error -> Default;
does_user_exist_in_given_modules([Mod | Modules], LUser, LServer, Default) ->
    case Mod:does_user_exist(LUser, LServer) of
        true ->
            true;
        false ->
            does_user_exist_in_given_modules(Modules, LUser, LServer, Default);
        {error, Error} ->
            ?LOG_ERROR(#{what => does_user_exist_failed,
                         text => <<"The authentication module returned an error">>,
                         auth_module => Mod, reason => Error,
                         user => LUser, server => LServer}),
            Default
    end.

%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
-spec remove_user(JID :: jid:jid()) -> ok | {error, not_allowed};
                 (error) -> error.
remove_user(#jid{luser = LUser, lserver = LServer}) ->
    AuthModules = auth_modules(LServer),
    RemoveResult = [M:remove_user(LUser, LServer) || M <- AuthModules ],
    case lists:any(fun(El) -> El == ok end, RemoveResult) of
        true ->
            Acc = mongoose_acc:new(#{ location => ?LOCATION,
                                      lserver => LServer,
                                      element => undefined }),
            mongoose_hooks:remove_user(LServer, Acc, LUser),
            ok;
        false ->
            ?LOG_ERROR(#{what => backends_disallow_user_removal,
                         user => LUser, server => LServer,
                         auth_modules => AuthModules}),
            {error, not_allowed}
    end;
remove_user(error) -> error.

%% @doc Calculate informational entropy.
-spec entropy(iolist()) -> float().
entropy(IOList) ->
    case binary_to_list(iolist_to_binary(IOList)) of
        "" ->
            0.0;
        InputList ->
            Set = lists:foldl(
                    fun(IOContent, Acc) ->
                            get_type_information(IOContent, Acc)
                    end, [0, 0, 0, 0, 0], InputList),
            length(InputList) * math:log(lists:sum(Set))/math:log(2)
    end.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
%% @doc Return the lists of all the auth modules actually used in the
%% configuration
-spec auth_modules() -> [authmodule()].
auth_modules() ->
    lists:usort(
      lists:flatmap(
        fun(HostType) ->
                auth_modules_for_host_type(HostType)
        end, ?ALL_HOST_TYPES)).

%% Return the list of authenticated modules for a given domain
%% TODO: rework is_anonymous_user/1 at ejabberd_users module,
%% so there is no need for exporting auth_modules/1 function.
%% after that completely get rid of this interface, we should
%% use auth_modules_for_host_type/1 function instead.
-spec auth_modules(Server :: jid:lserver()) -> [authmodule()].
auth_modules(LServer) ->
    case mongoose_domain_api:get_host_type(LServer) of
        {ok, HostType} -> auth_modules_for_host_type(HostType);
        {error, not_found} -> []
    end.

%% Return the list of authenticated modules for a given host type
-spec auth_modules_for_host_type(HostType :: binary()) -> [authmodule()].
auth_modules_for_host_type(HostType) ->
    Methods = auth_methods(HostType),
    [auth_method_to_module(M) || M <- Methods].

-spec auth_methods(binary()) -> [atom()].
auth_methods(HostType) ->
    Method = ejabberd_config:get_local_option({auth_method, HostType}),
    get_auth_method_as_a_list(Method).

-spec auth_method_to_module(atom()) -> authmodule().
auth_method_to_module(Method) ->
    list_to_atom("ejabberd_auth_" ++ atom_to_list(Method)).

get_auth_method_as_a_list(undefined) -> [];
get_auth_method_as_a_list(AuthMethod) when is_list(AuthMethod) -> AuthMethod;
get_auth_method_as_a_list(AuthMethod) when is_atom(AuthMethod) -> [AuthMethod].

ensure_metrics(Host) ->
    Metrics = [authorize, check_password, try_register, does_user_exist],
    [mongoose_metrics:ensure_metric(Host, ?METRIC(Metric), histogram)
     || Metric <- Metrics].

-spec timed_call(jid:lserver(), term(), fun(), list()) -> term().
timed_call(LServer, Metric, Fun, Args) ->
    {Time, Result} = timer:tc(Fun, Args),
    mongoose_metrics:update(LServer, ?METRIC(Metric), Time),
    Result.

%% Library functions for reuse in ejabberd_auth_* modules
-spec authorize_with_check_password(Module, Creds) -> {ok, Creds}
                                                    | {error, any()} when
      Module :: authmodule(),
      Creds :: mongoose_credentials:t().
authorize_with_check_password(Module, Creds) ->
    User      = mongoose_credentials:get(Creds, username),
    LUser     = jid:nodeprep(User),
    LUser == error andalso error({nodeprep_error, User}),
    LServer   = mongoose_credentials:lserver(Creds),
    HostType  = mongoose_credentials:host_type(Creds),
    Password  = mongoose_credentials:get(Creds, password),
    Digest    = mongoose_credentials:get(Creds, digest, undefined),
    DigestGen = mongoose_credentials:get(Creds, digest_gen, undefined),
    Args = case {Digest, DigestGen} of
               _ when Digest /= undefined andalso DigestGen /= undefined ->
                   [HostType, LUser, LServer, Password, Digest, DigestGen];
               _  ->
                   [HostType, LUser, LServer, Password]
           end,
    case erlang:apply(Module, check_password, Args) of
        true -> {ok, mongoose_credentials:set(Creds, auth_module, Module)};
        false -> {error, not_authorized}
    end.

-spec get_type_information(integer(), list()) -> list().
get_type_information(IOContent, [Digit, Printable, _, HiLetter, Other])
  when IOContent >= $a andalso IOContent =< $z ->
    [Digit, Printable, 26, HiLetter, Other];
get_type_information(IOContent, [_, Printable, LowLetter, HiLetter, Other])
  when IOContent >= $0 andalso IOContent =< $9 ->
    [9, Printable, LowLetter, HiLetter, Other];
get_type_information(IOContent, [Digit, Printable, LowLetter, _, Other])
  when IOContent >= $A andalso IOContent =< $Z ->
    [Digit, Printable, LowLetter, 26, Other];
get_type_information(IOContent, [Digit, _, LowLetter, HiLetter, Other])
  when IOContent >= 16#21 andalso IOContent =< 16#7e ->
    [Digit, 33, LowLetter, HiLetter, Other];
get_type_information(_IOContent, [Digit, Printable, LowLetter, HiLetter, _Other]) ->
    [Digit, Printable, LowLetter, HiLetter, 128].
