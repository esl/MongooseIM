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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth).
-author('alexey@process-one.net').

%% External exports
-export([start/0,
         start/1,
         stop/1,
         authorize/1,
         set_password/3,
         check_password/3,
         check_password/5,
         check_password_with_authmodule/3,
         check_password_with_authmodule/5,
         try_register/3,
         dirty_get_registered_users/0,
         get_vh_registered_users/1,
         get_vh_registered_users/2,
         get_vh_registered_users_number/1,
         get_vh_registered_users_number/2,
         get_password/2,
         get_password_s/2,
         get_password_with_authmodule/2,
         is_user_exists/2,
         is_user_exists_in_other_modules/3,
         remove_user/2,
         remove_user/3,
         plain_password_required/1,
         store_type/1,
         entropy/1
        ]).

-export([check_digest/4]).

-export([auth_modules/1]).

%% Library functions for reuse in ejabberd_auth_* modules
-export([authorize_with_check_password/2]).

-include("ejabberd.hrl").

-export_type([authmodule/0,
              passwordlike/0]).

-type authmodule() :: module().
%% TODO: TBH this name smells.
-type passwordlike() :: binary() | scram:scram_tuple().

-define(METRIC(Name), [backends, auth, Name]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-spec start() -> 'ok'.
start() ->
    lists:foreach(fun start/1, ?MYHOSTS).

-spec start(Host :: ejabberd:server()) -> 'ok'.
start(Host) ->
    ensure_metrics(Host),
    lists:foreach(
      fun(M) ->
              M:start(Host)
      end, auth_modules(Host)).

-spec stop(Host :: ejabberd:server()) -> 'ok'.
stop(Host) ->
    lists:foreach(
      fun(M) ->
              M:stop(Host)
      end, auth_modules(Host)).

%% This is only executed by ejabberd_c2s for non-SASL auth client
-spec plain_password_required(Server :: ejabberd:server()) -> boolean().
plain_password_required(Server) ->
    lists:any(
      fun(M) ->
              M:plain_password_required()
      end, auth_modules(Server)).

store_type(Server) ->
    lists:foldl(
      fun(_, external) ->
              external;
         (M, scram) ->
              case M:store_type(Server) of
                  external ->
                      external;
                  _Else ->
                      scram
              end;
         (M, plain) ->
              M:store_type(Server)
      end, plain, auth_modules(Server)).

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    LServer = mongoose_credentials:lserver(Creds),
    timed_call(LServer, authorize, fun authorize_loop/2,
               [auth_modules(LServer), Creds]).

-spec authorize_loop([AuthM], Creds) -> {ok, R}
                                      | {error, any()} when
      AuthM :: ejabberd_gen_auth:t(),
      Creds :: mongoose_credentials:t(),
      R     :: mongoose_credentials:t().
authorize_loop([], Creds) -> {error, {no_auth_modules, Creds}};
authorize_loop([M | Modules], Creds) ->
    try
        {ok, NewCreds} = M:authorize(Creds),
        {ok, mongoose_credentials:register(NewCreds, M, success)}
    catch
        _:R -> authorize_loop(Modules,
                              mongoose_credentials:register(Creds, M, {failure, R}))
    end.

%% @doc Check if the user and password can login in server.
-spec check_password(User :: ejabberd:user(),
                     Server :: ejabberd:server(),
                     Password :: binary() ) -> boolean().
check_password(User, Server, Password) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    do_check_password(LUser, LServer, Password).

-spec do_check_password(ejabberd:luser(), ejabberd:lserver(), binary()) -> boolean().
do_check_password(LUser, LServer, _) when LUser =:= error; LServer =:= error ->
    false;
do_check_password(LUser, LServer, Password) ->
    case check_password_with_authmodule(LUser, LServer, Password) of
        {true, _AuthModule} -> true;
        false -> false
    end.

%% @doc Check if the user and password can login in server.
-spec check_password(User :: ejabberd:user(),
                     Server :: ejabberd:server(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
check_password(User, Server, Password, Digest, DigestGen) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    do_check_password(LUser, LServer, Password, Digest, DigestGen).

-spec do_check_password(User :: ejabberd:luser(),
                     Server :: ejabberd:lserver(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
do_check_password(LUser, LServer, _,_,_)
    when LUser =:= error; LServer =:= error ->
    false;
do_check_password(LUser, LServer, Password, Digest, DigestGen) ->
    case check_password_with_authmodule(LUser, LServer, Password, Digest, DigestGen) of
        {true, _AuthModule} -> true;
        false -> false
    end.

%% @doc Check if the user and password can login in server.
%% The user can login if at least an authentication method accepts the user
%% and the password.
-spec check_password_with_authmodule(User :: binary(),
                                     Server :: binary(),
                                     Password :: binary()
                                     ) -> 'false' | {'true', authmodule()}.
check_password_with_authmodule(User, Server, Password) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    do_check_password_with_authmodule(LUser, LServer, Password).

-spec do_check_password_with_authmodule(LUser :: ejabberd:luser(),
                                        LServer :: ejabberd:lserver(),
                                        Password :: binary()
                                       ) -> 'false' | {'true', authmodule()}.
do_check_password_with_authmodule(LUser, LServer, _)
    when LUser =:= error; LServer =:= error ->
    false;
do_check_password_with_authmodule(LUser, LServer, Password) ->
    check_password_loop(auth_modules(LServer), [LUser, LServer, Password]).

-spec check_password_with_authmodule(User :: binary(),
                                     Server :: binary(),
                                     Password :: binary(),
                                     Digest :: binary(),
                                     DigestGen :: fun()
                                     ) -> 'false' | {'true', authmodule()}.
check_password_with_authmodule(User, Server, Password, Digest, DigestGen) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    do_check_password_with_authmodule(LUser, LServer, Password, Digest, DigestGen).

-spec do_check_password_with_authmodule(LUser :: ejabberd:luser(),
                                        LServer :: ejabberd:lserver(),
                                        Password :: binary(),
                                        Digest :: binary(),
                                        DigestGen :: fun()
                                     ) -> 'false' | {'true', authmodule()}.
do_check_password_with_authmodule(LUser, LServer, _, _, _)
    when LUser =:= error; LServer =:= error ->
    false;
do_check_password_with_authmodule(LUser, LServer, Password, Digest, DigestGen) ->
    check_password_loop(auth_modules(LServer), [LUser, LServer, Password,
                                               Digest, DigestGen]).

-spec check_password_loop(AuthModules :: [authmodule()],
                          Args :: [any(),...]
                          ) -> 'false' | {'true', authmodule()}.
check_password_loop([], _Args) ->
    false;
check_password_loop(AuthModules, [_, LServer | _] = Args) ->
    timed_call(LServer, check_password, fun do_check_password_loop/2, [AuthModules, Args]).

do_check_password_loop([], _) ->
    false;
do_check_password_loop([AuthModule | AuthModules], Args) ->
    case apply(AuthModule, check_password, Args) of
        true ->
            {true, AuthModule};
        false ->
            do_check_password_loop(AuthModules, Args)
    end.

-spec check_digest(binary(), fun(), binary(), binary()) -> boolean().
check_digest(<<>>, _, <<>>, _) ->
    false; %%empty digest and password
check_digest(Digest, DigestGen, _Password, Passwd) ->
    Digest == DigestGen(Passwd).

-spec set_password(User :: ejabberd:user(),
                   Server :: ejabberd:server(),
                   Password :: binary()
                  ) -> ok | {error, empty_password | not_allowed | invalid_jid}.
set_password(_,_, <<"">>) ->
    {error, empty_password};
set_password(User, Server, Password) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nodeprep(Server),
    do_set_password(LUser, LServer, Password).

do_set_password(LUser, LServer, _) when LUser =:= error; LServer =:= error ->
    {error, invalid_jid};
do_set_password(LUser, LServer, Password) ->
    lists:foldl(
      fun(M, {error, _}) ->
              M:set_password(LUser, LServer, Password);
         (_M, Res) ->
              Res
      end, {error, not_allowed}, auth_modules(LServer)).


-spec try_register(User :: ejabberd:user(),
                   Server :: ejabberd:server(),
                   Password :: binary()
                   ) -> ok | {error, exists | not_allowed | invalid_jid | null_password}.
try_register(_,_,<<"">>) ->
    {error, null_password};
try_register(User, Server, Password) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nodeprep(Server),
    do_try_register(LUser, LServer, Password).

-spec do_try_register(ejabberd:luser(), ejabberd:lserver(),binary())
        -> ok | {error, exists | not_allowed | invalid_jid}.
do_try_register(LUser, LServer, _) when LUser =:= error; LServer =:= error ->
    {error, invalid_jid};
do_try_register(LUser, LServer, Password) ->
    Exists = is_user_exists(LUser,LServer),
    do_try_register_if_does_not_exist(Exists, LUser, LServer, Password).

do_try_register_if_does_not_exist(true, _, _, _) ->
    {error, exists};
do_try_register_if_does_not_exist(_, LUser, LServer, Password) ->
    case lists:member(LServer, ?MYHOSTS) of
        true ->
            timed_call(LServer, try_register,
                       fun do_try_register_if_does_not_exist_timed/3, [LUser, LServer, Password]);
        false ->
            {error, not_allowed}
    end.

do_try_register_if_does_not_exist_timed(LUser, LServer, Password) ->
    Backends = auth_modules(LServer),
    do_try_register_in_backend(Backends, LUser, LServer, Password).

do_try_register_in_backend([], _, _, _) ->
    {error, not_allowed};
do_try_register_in_backend([M | Backends], LUser, LServer, Password) ->
    case M:try_register(LUser, LServer, Password) of
        ok ->
            ejabberd_hooks:run(register_user, LServer,
                [LUser, LServer]);
        _ ->
            do_try_register_in_backend(Backends, LUser, LServer, Password)
    end.

%% @doc Registered users list do not include anonymous users logged
-spec dirty_get_registered_users() -> [ejabberd:simple_bare_jid()].
dirty_get_registered_users() ->
    lists:flatmap(
      fun(M) ->
              M:dirty_get_registered_users()
      end, auth_modules()).


%% @doc Registered users list do not include anonymous users logged
-spec get_vh_registered_users(Server :: ejabberd:server()
                             ) -> [ejabberd:simple_bare_jid()].
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


-spec get_vh_registered_users(Server :: ejabberd:server(),
                              Opts :: [any()]) -> [ejabberd:simple_bare_jid()].
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


-spec get_vh_registered_users_number(Server :: ejabberd:server()
                                    ) -> integer().
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


-spec get_vh_registered_users_number(Server :: ejabberd:server(),
                                     Opts :: list()) -> integer().
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


%% @doc Get the password of the user.
-spec get_password(User :: ejabberd:user(),
                   Server :: ejabberd:server()) -> binary() | false.
get_password(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    do_get_password(LUser, LServer).

do_get_password(LUser, LServer) when LUser =:= error; LServer =:= error ->
    false;
do_get_password(LUser, LServer) ->
    lists:foldl(
        fun(M, false) ->
            M:get_password(LUser, LServer);
            (_M, Password) ->
                Password
        end, false, auth_modules(LServer)).


-spec get_password_s(User :: ejabberd:user(),
                     Server :: ejabberd:server()) -> binary().
get_password_s(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    do_get_password_s(LUser, LServer).

do_get_password_s(LUser, LServer) when LUser =:= error; LServer =:= error ->
    <<"">>;
do_get_password_s(LUser, LServer) ->
    lists:foldl(
        fun(M, <<"">>) ->
            M:get_password_s(LUser, LServer);
            (_M, Password) ->
                Password
        end, <<"">>, auth_modules(LServer)).

%% @doc Get the password(like thing) of the user and the auth module.
-spec get_password_with_authmodule(ejabberd:user(), ejabberd:server()) -> R when
      R :: {passwordlike(), authmodule()}
         | {'false', 'none'}.
get_password_with_authmodule(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    do_get_password_with_authmodule(LUser, LServer).

do_get_password_with_authmodule(LUser, LServer)
    when LUser =:= error; LServer =:= error ->
    {false, none};
do_get_password_with_authmodule(LUser, LServer) ->
    lists:foldl(
        fun(M, {false, _}) ->
            {M:get_password(LUser, LServer), M};
            (_M, {Password, AuthModule}) ->
                {Password, AuthModule}
        end, {false, none}, auth_modules(LServer)).

%% @doc Returns true if the user exists in the DB or if an anonymous user is
%% logged under the given name
-spec is_user_exists(User :: ejabberd:user(),
                     Server :: ejabberd:server()) -> boolean().
is_user_exists(<<"">>, _) ->
    false;
is_user_exists(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    do_does_user_exist(LUser, LServer).

do_does_user_exist(LUser, LServer) when LUser =:= error; LServer =:= error ->
    false;
do_does_user_exist(LUser, LServer) ->
    timed_call(LServer, does_user_exist, fun does_user_exist_timed/2, [LUser, LServer]).

does_user_exist_timed(LUser, LServer) ->
    lists:any(
        fun(M) ->
            case M:does_user_exist(LUser, LServer) of
                {error, Error} ->
                    ?ERROR_MSG("The authentication module ~p returned an "
                    "error~nwhen checking user ~p in server ~p~n"
                    "Error message: ~p",
                        [M, LUser, LServer, Error]),
                    false;
                Else ->
                    Else
            end
        end, auth_modules(LServer)).

%% Check if the user exists in all authentications module except the module
%% passed as parameter
-spec is_user_exists_in_other_modules(Module :: authmodule(),
                                      User :: ejabberd:user(),
                                      Server :: ejabberd:server()
                                      ) -> boolean() | 'maybe'.
is_user_exists_in_other_modules(Module, User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    do_does_user_exist_in_other_modules(Module, LUser, LServer).

do_does_user_exist_in_other_modules(_, LUser, LServer)
    when LUser =:= error; LServer =:= error ->
    false;
do_does_user_exist_in_other_modules(Module, LUser, LServer) ->
    does_user_exist_in_other_modules_loop(
        auth_modules(LServer)--[Module],
        LUser, LServer).


does_user_exist_in_other_modules_loop([], _User, _Server) ->
    false;
does_user_exist_in_other_modules_loop([AuthModule|AuthModules], User, Server) ->
    case AuthModule:does_user_exist(User, Server) of
        true ->
            true;
        false ->
            does_user_exist_in_other_modules_loop(AuthModules, User, Server);
        {error, Error} ->
            ?DEBUG("The authentication module ~p returned an error~nwhen "
                   "checking user ~p in server ~p~nError message: ~p",
                   [AuthModule, User, Server, Error]),
            maybe
    end.


%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
-spec remove_user(User :: ejabberd:user(),
                  Server :: ejabberd:server()) -> ok | error | {error, not_allowed}.
remove_user(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    do_remove_user(LUser, LServer).

do_remove_user(LUser, LServer) when LUser =:= error; LServer =:= error ->
    error;
do_remove_user(LUser, LServer) ->
    [M:remove_user(LUser, LServer) || M <- auth_modules(LServer)],
    ejabberd_hooks:run(remove_user, LServer, [LUser, LServer]),
    ok.

%% @doc Try to remove user if the provided password is correct.
%% The removal is attempted in each auth method provided:
%% when one returns 'ok' the loop stops;
%% if no method returns 'ok' then it returns the error message indicated by the last method attempted.
-spec remove_user(User :: ejabberd:user(),
                  Server :: ejabberd:server(),
                  Password :: binary()
                  ) -> ok | not_exists | not_allowed | bad_request | error.
remove_user(User, Server, Password) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    do_remove_user(LUser, LServer, Password).

do_remove_user(LUser, LServer, _) when LUser =:= error; LServer =:= error ->
    error;
do_remove_user(LUser, LServer, Password) ->
    R = lists:foldl(
        fun(_M, ok = Res) ->
            Res;
            (M, _) ->
                M:remove_user(LUser, LServer, Password)
        end, error, auth_modules(LServer)),
    case R of
        ok -> ejabberd_hooks:run(remove_user, LServer, [LUser, LServer]);
        _ -> none
    end,
    R.

%% @doc Calculate informational entropy.
-spec entropy(iolist()) -> float().
entropy(IOList) ->
    case binary_to_list(iolist_to_binary(IOList)) of
        "" ->
            0.0;
        S ->
            Set = lists:foldl(
                    fun(C, [Digit, Printable, LowLetter, HiLetter, Other]) ->
                            if C >= $a, C =< $z ->
                                    [Digit, Printable, 26, HiLetter, Other];
                               C >= $0, C =< $9 ->
                                    [9, Printable, LowLetter, HiLetter, Other];
                               C >= $A, C =< $Z ->
                                    [Digit, Printable, LowLetter, 26, Other];
                               C >= 16#21, C =< 16#7e ->
                                    [Digit, 33, LowLetter, HiLetter, Other];
                               true ->
                                    [Digit, Printable, LowLetter, HiLetter, 128]
                            end
                    end, [0, 0, 0, 0, 0], S),
            length(S) * math:log(lists:sum(Set))/math:log(2)
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
        fun(Server) ->
                auth_modules(Server)
        end, ?MYHOSTS)).


%% Return the list of authenticated modules for a given host
-spec auth_modules(Server :: ejabberd:lserver()) -> [authmodule()].
auth_modules(LServer) ->
    Method = ejabberd_config:get_local_option({auth_method, LServer}),
    Methods = if
                  Method == undefined -> [];
                  is_list(Method) -> Method;
                  is_atom(Method) -> [Method]
              end,
    [list_to_atom("ejabberd_auth_" ++ atom_to_list(M)) || M <- Methods].

ensure_metrics(Host) ->
    Metrics = [authorize, check_password, try_register, does_user_exist],
    [mongoose_metrics:ensure_metric(Host, ?METRIC(Metric), histogram)
     || Metric <- Metrics].

-spec timed_call(ejabberd:lserver(), term(), fun(), list()) -> term().
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
    Password  = mongoose_credentials:get(Creds, password),
    Digest    = mongoose_credentials:get(Creds, digest, undefined),
    DigestGen = mongoose_credentials:get(Creds, digest_gen, undefined),
    Args = if
               Digest /= undefined andalso DigestGen /= undefined ->
                   [LUser, LServer, Password, Digest, DigestGen];
               Digest == undefined orelse DigestGen == undefined ->
                   [LUser, LServer, Password]
           end,
    case erlang:apply(Module, check_password, Args) of
        true -> {ok, mongoose_credentials:set(Creds, auth_module, Module)};
        false -> {error, not_authorized}
    end.
