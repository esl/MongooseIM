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
-behaviour(ejabberd_gen_auth).
-export([start/1,
         stop/1,
         authorize/1,
         set_password/3,
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
         store_type/1
        ]).

%% Internal
-export([check_password/3,
         check_password/5]).

-export([scram_passwords/2, scram_passwords/4]).

-include("mongoose.hrl").
-include("scram.hrl").

-define(DEFAULT_SCRAMMIFY_COUNT, 10000).
-define(DEFAULT_SCRAMMIFY_INTERVAL, 1000).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start(_Host) ->
    ok.

stop(_Host) ->
    ok.

store_type(Server) ->
    case scram:enabled(Server) of
        false -> plain;
        true -> scram
    end.

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    ejabberd_auth:authorize_with_check_password(?MODULE, Creds).

-spec check_password(LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary()) -> boolean().
check_password(LUser, LServer, Password) ->
    Username = mongoose_rdbms:escape(LUser),
    true == check_password_wo_escape(Username, LServer, Password).


-spec check_password(LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
check_password(LUser, LServer, Password, Digest, DigestGen) ->
    Username = mongoose_rdbms:escape(LUser),
    try rdbms_queries:get_password(LServer, Username) of
        %% Account exists, check if password is valid
        {selected, [{Passwd, null}]} ->
            ejabberd_auth:check_digest(Digest, DigestGen, Password, Passwd);
        {selected, [{_Passwd, PassDetails}]} ->
            case scram:deserialize(PassDetails) of
                {ok, #scram{} = Scram} ->
                    scram:check_digest(Scram, Digest, DigestGen, Password);
                _ ->
                    false
            end;
        {selected, []} ->
            false; %% Account does not exist
        {error, _Error} ->
            false %% Typical error is that table doesn't exist
    catch
        _:_ ->
            false %% Typical error is database not accessible
    end.

-spec check_password_wo_escape(LUser::jid:luser(),
                               LServer::jid:lserver(),
                               Password::binary()) -> boolean() | not_exists.
check_password_wo_escape(LUser, LServer, Password) ->
    try rdbms_queries:get_password(LServer, LUser) of
        {selected, [{Password, null}]} ->
            Password /= <<"">>; %% Password is correct, and not empty
        {selected, [{_Password2, null}]} ->
            false;
        {selected, [{_Password2, PassDetails}]} ->
            case scram:deserialize(PassDetails) of
                {ok, Scram} ->
                    scram:check_password(Password, Scram);
                _ ->
                    false %% Password is not correct
            end;
        {selected, []} ->
            not_exists; %% Account does not exist
        {error, _Error} ->
            false %% Typical error is that table doesn't exist
    catch
        _:_ ->
            false %% Typical error is database not accessible
    end.


-spec set_password(LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()
                   ) -> ok | {error, not_allowed}.
set_password(LUser, LServer, Password) ->
    Username = mongoose_rdbms:escape(LUser),
    PreparedPass = prepare_password(LServer, Password),
    case catch rdbms_queries:set_password_t(LServer, Username, PreparedPass) of
        {atomic, ok} ->
            ok;
        Error ->
            ?WARNING_MSG("Failed SQL request: ~p", [Error]),
            {error, not_allowed}
    end.

-spec try_register(LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()
                   ) -> ok | {error, exists}.
try_register(LUser, LServer, Password) ->
    Username = mongoose_rdbms:escape(LUser),
    PreparedPass = prepare_password(LServer, Password),
    case catch rdbms_queries:add_user(LServer, Username, PreparedPass) of
        {updated, 1} ->
            ok;
        _ ->
            {error, exists}
    end.

-spec dirty_get_registered_users() -> [jid:simple_bare_jid()].
dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(odbc),
    lists:flatmap(
      fun(Server) ->
              get_vh_registered_users(Server)
      end, Servers).


-spec get_vh_registered_users(LServer :: jid:lserver()) -> [jid:simple_bare_jid()].
get_vh_registered_users(LServer) ->
    case catch rdbms_queries:list_users(LServer) of
        {selected, Res} ->
            [{U, LServer} || {U} <- Res];
        _ ->
            []
    end.


-spec get_vh_registered_users(LServer :: jid:lserver(), Opts :: list()
                             ) -> [jid:simple_bare_jid()].
get_vh_registered_users(LServer, Opts) ->
    case catch rdbms_queries:list_users(LServer, Opts) of
        {selected, Res} ->
            [{U, LServer} || {U} <- Res];
        _ ->
            []
    end.


-spec get_vh_registered_users_number(LServer :: jid:lserver()
                                    ) -> integer().
get_vh_registered_users_number(LServer) ->
    case catch rdbms_queries:users_number(LServer) of
        {selected, [{Res}]} when is_integer(Res) ->
            Res;
        {selected, [{Res}]} ->
            mongoose_rdbms:result_to_integer(Res);
        _ ->
            0
    end.


-spec get_vh_registered_users_number(LServer :: jid:lserver(),
                                     Opts :: list()) -> integer().
get_vh_registered_users_number(LServer, Opts) ->
    case catch rdbms_queries:users_number(LServer, Opts) of
        {selected, [{Res}]} ->
            list_to_integer(Res);
        _Other ->
            0
    end.


-spec get_password(jid:luser(), jid:lserver()) -> ejabberd_auth:passterm() | false.
get_password(LUser, LServer) ->
    Username = mongoose_rdbms:escape(LUser),
    case catch rdbms_queries:get_password(LServer, Username) of
        {selected, [{Password, null}]} ->
            Password; %%Plain password
        {selected, [{_Password, PassDetails}]} ->
            case scram:deserialize(PassDetails) of
                {ok, Scram} ->
                    scram:scram_to_tuple(Scram);
                _ ->
                    false
            end;
        _ ->
            false
    end.


-spec get_password_s(LUser :: jid:user(),
                     LServer :: jid:server()) -> binary().
get_password_s(LUser, LServer) ->
    Username = mongoose_rdbms:escape(LUser),
    case catch rdbms_queries:get_password(LServer, Username) of
        {selected, [{Password, _}]} ->
            Password;
        _ ->
            <<"">>
    end.


-spec does_user_exist(LUser :: jid:luser(),
                     LServer :: jid:lserver()
                    ) -> boolean() | {error, atom()}.
does_user_exist(LUser, LServer) ->
    Username = mongoose_rdbms:escape(LUser),
    try rdbms_queries:get_password(LServer, Username) of
        {selected, [{_Password, _}]} ->
            true; %% Account exists
        {selected, []} ->
            false; %% Account does not exist
        {error, Error} ->
            {error, Error} %% Typical error is that table doesn't exist
    catch
        _:B ->
            {error, B} %% Typical error is database not accessible
    end.


%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
-spec remove_user(LUser :: jid:luser(),
                  LServer :: jid:lserver()
                  ) -> ok.
remove_user(LUser, LServer) ->
    Username = mongoose_rdbms:escape(LUser),
    catch rdbms_queries:del_user(LServer, Username),
    ok.


%% @doc Remove user if the provided password is correct.
-spec remove_user(LUser :: jid:luser(),
                  LServer :: jid:lserver(),
                  Password :: binary()
                 ) -> ok | {error, not_exists | not_allowed}.
remove_user(LUser, LServer, Password) ->
    Username = mongoose_rdbms:escape(LUser),
    Pass = mongoose_rdbms:escape(Password),
    case check_password_wo_escape(Username, LServer, Pass) of
        true ->
            case catch rdbms_queries:del_user(LServer, Username) of
                {'EXIT', Error} ->
                    ?WARNING_MSG("Failed SQL query: ~p", [Error]),
                    {error, not_allowed};
                _ ->
                    ok
            end;
        not_exists ->
            {error, not_exists};
        false ->
            {error, not_allowed}
    end.

%%%------------------------------------------------------------------
%%% SCRAM
%%%------------------------------------------------------------------

-spec prepare_scrammed_password(Iterations :: pos_integer(), Password :: binary()) ->
    {PreparedPassword :: binary(), ExtendedPassword :: binary()}.
prepare_scrammed_password(Iterations, Password) when is_integer(Iterations) ->
    Scram = scram:password_to_scram(Password, Iterations),
    PassDetails = scram:serialize(Scram),
    PassDetailsEscaped = mongoose_rdbms:escape(PassDetails),
    {<<>>, PassDetailsEscaped}.

-spec prepare_password(Server :: jid:server(), Password :: binary()) ->
    PreparedPassword :: {binary(), binary()} | binary().
prepare_password(Server, Password) ->
    case scram:enabled(Server) of
        true ->
            prepare_scrammed_password(scram:iterations(Server), Password);
        _ ->
            mongoose_rdbms:escape(Password)
    end.

scram_passwords(Server, ScramIterationCount) ->
    scram_passwords(Server, ?DEFAULT_SCRAMMIFY_COUNT,
                    ?DEFAULT_SCRAMMIFY_INTERVAL, ScramIterationCount).

scram_passwords(Server, Count, Interval, ScramIterationCount) ->
    LServer = jid:nameprep(Server),
    ?INFO_MSG("Converting the stored passwords into SCRAM bits", []),
    ToConvertCount = case catch rdbms_queries:get_users_without_scram_count(LServer) of
        {selected, [{Res}]} -> binary_to_integer(Res);
        _ -> 0
    end,

    ?INFO_MSG("Users to scrammify: ~p", [ToConvertCount]),
    scram_passwords1(LServer, Count, Interval, ScramIterationCount).

scram_passwords1(LServer, Count, Interval, ScramIterationCount) ->
    case rdbms_queries:get_users_without_scram(LServer, Count) of
        {selected, []} ->
            ?INFO_MSG("All users scrammed.", []);
        {selected, Results} ->
            ?INFO_MSG("Scramming ~p users...", [length(Results)]),
            lists:foreach(
              fun({Username, Password}) ->
                      ScrammedPassword = prepare_scrammed_password(ScramIterationCount,
                                                                   Password),
                      write_scrammed_password_to_rdbms(LServer, Username, ScrammedPassword)
              end, Results),
            ?INFO_MSG("Scrammed. Waiting for ~pms", [Interval]),
            timer:sleep(Interval),
            scram_passwords1(LServer, Count, Interval, ScramIterationCount);
        Other ->
            ?ERROR_MSG("Interrupted scramming because: ~p", [Other])
    end.

write_scrammed_password_to_rdbms(LServer, Username, ScrammedPassword) ->
    case catch rdbms_queries:set_password_t(LServer, Username,
                                            ScrammedPassword) of
        {atomic, ok} -> ok;
        Other -> ?ERROR_MSG("Could not scrammify user ~s@~s because: ~p",
                            [Username, LServer, Other])
    end.
