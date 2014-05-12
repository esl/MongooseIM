%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via ODBC
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_auth_odbc).
-author('alexey@process-one.net').

%% External exports
-behaviour(gen_auth).
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

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(_Host) ->
    ok.

plain_password_required() ->
    false.

-spec check_password(User :: binary(),
                     Server :: ejabberd:server(),
                     Password :: binary()) -> boolean().
check_password(User, Server, Password) ->
    case jlib:nodeprep(User) of
        error ->
            false;
        LUser ->
            Username = ejabberd_odbc:escape(LUser),
            LServer = jlib:nameprep(Server),
            PasswdStr = Password,
            try odbc_queries:get_password(LServer, Username) of
                {selected, [<<"password">>], [{PasswdStr}]} ->  
                    Password /= <<"">>; %% Password is correct, and not empty
                {selected, [<<"password">>], [{_Password2}]} ->
                    false; %% Password is not correct
                {selected, [<<"password">>], []} ->
                    false; %% Account does not exist
                {error, _Error} ->
                    false %% Typical error is that table doesn't exist
            catch
                _:_ ->
                    false %% Typical error is database not accessible
            end
    end.

-spec check_password(User :: binary(),
                     Server :: ejabberd:server(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
check_password(User, Server, Password, Digest, DigestGen) ->
    case jlib:nodeprep(User) of
        error ->
            false;
        LUser ->
            Username = ejabberd_odbc:escape(LUser),
            LServer = jlib:nameprep(Server),
            try odbc_queries:get_password(LServer, Username) of
                %% Account exists, check if password is valid

                {selected, [<<"password">>], [{Passwd}]} ->
                    DigRes = if
                                 Digest /= <<"">> ->
                                     Digest == DigestGen(Passwd);
                                 true ->
                                     false
                             end,
                    if DigRes ->
                            true;
                       true ->
                            (Passwd == Password) and (Password /= <<"">>)
                    end;
                {selected, [<<"password">>], []} ->
                    false; %% Account does not exist
                {error, _Error} ->
                    false %% Typical error is that table doesn't exist
            catch
                _:_ ->
                    false %% Typical error is database not accessible
            end
    end.

-spec set_password(User :: binary(),
                   Server :: ejabberd:server(),
                   Password :: binary()
                   ) -> ok | {error, not_allowed | invalid_jid}.
set_password(User, Server, Password) ->
    case jlib:nodeprep(User) of
        error ->
            {error, invalid_jid};
        LUser ->
            Username = ejabberd_odbc:escape(LUser),
            Pass = ejabberd_odbc:escape(Password),
            LServer = jlib:nameprep(Server),
            case catch odbc_queries:set_password_t(LServer, Username, Pass) of
                {atomic, ok} -> ok;
                Other -> {error, Other}
            end
    end.


-spec try_register(User :: binary(),
                   Server :: ejabberd:server(),
                   Password :: binary()
                   ) -> {atomic, ok | exists}
                      | {error, invalid_jid | not_allowed} | {aborted, _}.
try_register(User, Server, Password) ->
    case jlib:nodeprep(User) of
        error ->
            {error, invalid_jid};
        LUser ->
            Username = ejabberd_odbc:escape(LUser),
            Pass = ejabberd_odbc:escape(Password),
            LServer = jlib:nameprep(Server),
            case catch odbc_queries:add_user(LServer, Username, Pass) of
                {updated, 1} ->
                    {atomic, ok};
                _ ->
                    {atomic, exists}
            end
    end.

-spec dirty_get_registered_users() -> [ejabberd:simple_jid()].
dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(odbc),
    lists:flatmap(
      fun(Server) ->
              get_vh_registered_users(Server)
      end, Servers).

-spec get_vh_registered_users(Server :: ejabberd:server()
                             ) -> [ejabberd:simple_jid()].
get_vh_registered_users(Server) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:list_users(LServer) of
        {selected, [<<"username">>], Res} ->
            [{U, LServer} || {U} <- Res];
        _ ->
            []
    end.

-spec get_vh_registered_users(Server :: ejabberd:server(), Opts :: list()
                             ) -> [ejabberd:simple_jid()].
get_vh_registered_users(Server, Opts) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:list_users(LServer, Opts) of
        {selected, [<<"username">>], Res} ->
            [{U, LServer} || {U} <- Res];
        _ ->
            []
    end.

-spec get_vh_registered_users_number(Server :: ejabberd:server()
                                    ) -> integer().
get_vh_registered_users_number(Server) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:users_number(LServer) of
        {selected, [_], [{Res}]} ->
            list_to_integer(binary_to_list(Res));
        _ ->
            0
    end.

-spec get_vh_registered_users_number(Server :: ejabberd:server(),
                                     Opts :: list()) -> integer().
get_vh_registered_users_number(Server, Opts) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:users_number(LServer, Opts) of
        {selected, [_], [{Res}]} ->
            list_to_integer(Res);
        _Other ->
            0
    end.

-spec get_password(User :: binary(),
                   Server :: ejabberd:server()) -> binary() | false.
get_password(User, Server) ->
    case jlib:nodeprep(User) of
        error ->
            false;
        LUser ->
            Username = ejabberd_odbc:escape(LUser),
            LServer = jlib:nameprep(Server),
            case catch odbc_queries:get_password(LServer, Username) of
                {selected, [<<"password">>], [{Password}]} ->
                    Password;
                _ ->
                    false
            end
    end.

-spec get_password_s(User :: binary(),
                     Server :: ejabberd:server()) -> binary().
get_password_s(User, Server) ->
    case jlib:nodeprep(User) of
        error ->
            <<"">>;
        LUser ->
            Username = ejabberd_odbc:escape(LUser),
            LServer = jlib:nameprep(Server),
            case catch odbc_queries:get_password(LServer, Username) of
                {selected, [<<"password">>], [{Password}]} ->
                    Password;
                _ ->
                    <<"">>
            end
    end.

-spec is_user_exists(User :: binary(),
                     Server :: ejabberd:server()
                    ) -> boolean() | {error, atom()}.
is_user_exists(User, Server) ->
    case jlib:nodeprep(User) of
        error ->
            false;
        LUser ->
            Username = ejabberd_odbc:escape(LUser),
            LServer = jlib:nameprep(Server),
            try odbc_queries:get_password(LServer, Username) of
                {selected, [<<"password">>], [{_Password}]} ->
                    true; %% Account exists
                {selected, [<<"password">>], []} ->
                    false; %% Account does not exist
                {error, Error} ->
                    {error, Error} %% Typical error is that table doesn't exist
            catch
                _:B ->
                    {error, B} %% Typical error is database not accessible
            end
    end.

%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
-spec remove_user(User :: binary(),
                  Server :: ejabberd:server()
                  ) -> ok | error | {error, not_allowed}.
remove_user(User, Server) ->
    case jlib:nodeprep(User) of
        error ->
            error;
        LUser ->
            Username = ejabberd_odbc:escape(LUser),
            LServer = jlib:nameprep(Server),
            catch odbc_queries:del_user(LServer, Username),
            ok
    end.

%% @doc Remove user if the provided password is correct.
-spec remove_user(User :: binary(),
                  Server :: ejabberd:server(),
                  Password :: binary()
                 ) -> ok | not_exists | not_allowed | bad_request | error.
remove_user(User, Server, Password) ->
    case jlib:nodeprep(User) of
        error ->
            error;
        LUser ->
            Username = ejabberd_odbc:escape(LUser),
            Pass = ejabberd_odbc:escape(Password),
            LServer = jlib:nameprep(Server),
            PasswdStr = Password,
            F = fun() ->
                        Result = odbc_queries:del_user_return_password(
                                   LServer, Username, Pass),
                        case Result of
                            {selected, [<<"password">>], [{PasswdStr}]} ->
                                ok;
                            {selected, [<<"password">>], []} ->
                                not_exists;
                            _ ->
                                not_allowed
                        end
                end,
            {atomic, Result} = odbc_queries:sql_transaction(LServer, F),
            Result
    end.
