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
	 store_type/0,
	 plain_password_required/0
	]).

-include("ejabberd.hrl").

-define(SCRAM_ODBC_PREFIX, "==SCRAM==,").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(_Host) ->
    ok.

plain_password_required() ->
    ejabberd_auth:plain_password_required().

store_type() ->
    ejabberd_auth:store_type().

-spec check_password(binary(), binary(), binary()) -> true | false | {error, any()}.
check_password(User, Server, Password) ->
    case jlib:nodeprep(User) of
	error ->
	    false;
	LUser ->
	    Username = ejabberd_odbc:escape(LUser),
	    LServer = jlib:nameprep(Server),
	    try odbc_queries:get_password(LServer, Username) of
		{selected, [<<"password", "pass_details">>], [{Password, null}]} ->
		    Password /= <<"">>; %% Password is correct, and not empty
		{selected, [<<"password">>, <<"pass_details">>], [{_Password2, null}]} ->
		    false;
		{selected, [<<"password">>, <<"pass_details">>], [{_Password2, PassDetails}]} ->
		    case decode_pass_details(PassDetails) of
			#scram{} = Scram ->
			    ejabberd_auth:is_password_scram_valid(Password, Scram);
			_ ->
			    false %% Password is not correct
		    end;
		{selected, [<<"password">>, <<"pass_details">>], []} ->
		    false; %% Account does not exist
		{error, _Error} ->
		    false %% Typical error is that table doesn't exist
	    catch
		_:_ ->
		    false %% Typical error is database not accessible
	    end
    end.

%% @spec (User, Server, Password, Digest, DigestGen) -> true | false | {error, Error}
check_password(User, Server, Password, Digest, DigestGen) ->
    case jlib:nodeprep(User) of
        error ->
            false;
        LUser ->
            Username = ejabberd_odbc:escape(LUser),
            LServer = jlib:nameprep(Server),
            try odbc_queries:get_password(LServer, Username) of
                %% Account exists, check if password is valid
                {selected, [<<"password">>, <<"pass_details">>], [{Passwd, null}]} ->
                    ejabberd_auth:check_digest(Digest, DigestGen, Password, Passwd);
                {selected, [<<"password">>, <<"pass_details">>], [{_Passwd, PassDetails}]} ->
                    case decode_pass_details(PassDetails) of
                        #scram{storedkey = StoredKey} ->
                            Passwd = base64:decode(StoredKey),
                            ejabberd_auth:check_digest(Digest, DigestGen, Password, Passwd);
                        _ ->
                            false
                    end;
                {selected, [<<"password">>, <<"pass_details">>], []} ->
                    false; %% Account does not exist
                {error, _Error} ->
                    false %% Typical error is that table doesn't exist
            catch
                _:_ ->
                    false %% Typical error is database not accessible
            end
    end.

-spec set_password(binary(), binary(), binary()) -> ok | {error, any()}.
set_password(User, Server, Password) ->
    case jlib:nodeprep(User) of
        error ->
            {error, invalid_jid};
        LUser ->
            Username = ejabberd_odbc:escape(LUser),
            LServer = jlib:nameprep(Server),
            case prepare_password(Password) of
                false ->
                    {error, invalid_password};
                Pass ->
                    case catch odbc_queries:set_password_t(LServer, Username, Pass) of
                        {atomic, ok} ->
                            ok;
                        Other ->
                            {error, Other}
                    end
            end
    end.

-spec try_register(binary(), binary(), binary()) -> {atomic, ok} | {error, any()}.
try_register(User, Server, Password) ->
    case jlib:nodeprep(User) of
        error ->
            {error, invalid_jid};
        LUser ->
            Username = ejabberd_odbc:escape(LUser),
            case prepare_password(Password) of
                false ->
                    {error, invalid_password};
                Pass ->
                    LServer = jlib:nameprep(Server),
                    case catch odbc_queries:add_user(LServer, Username, Pass) of
                        {updated, 1} ->
                            {atomic, ok};
                        _ ->
                            {atomic, exists}
                    end
            end
    end.

dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(odbc),
    lists:flatmap(
      fun(Server) ->
	      get_vh_registered_users(Server)
      end, Servers).

get_vh_registered_users(Server) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:list_users(LServer) of
	{selected, [<<"username">>], Res} ->
	    [{U, LServer} || {U} <- Res];
	_ ->
	    []
    end.

get_vh_registered_users(Server, Opts) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:list_users(LServer, Opts) of
	{selected, [<<"username">>], Res} ->
	    [{U, LServer} || {U} <- Res];
	_ ->
	    []
    end.

get_vh_registered_users_number(Server) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:users_number(LServer) of
	{selected, [_], [{Res}]} ->
	    list_to_integer(binary_to_list(Res));
	_ ->
	    0
    end.

get_vh_registered_users_number(Server, Opts) ->
    LServer = jlib:nameprep(Server),
    case catch odbc_queries:users_number(LServer, Opts) of
	{selected, [_], [{Res}]} ->
	    list_to_integer(Res);
	_Other ->
	    0
    end.

get_password(User, Server) ->
    case jlib:nodeprep(User) of
        error ->
            false;
        LUser ->
            Username = ejabberd_odbc:escape(LUser),
            LServer = jlib:nameprep(Server),
            case catch odbc_queries:get_password(LServer, Username) of
                {selected, [<<"password">>, <<"pass_details">>], [{Password, null}]} ->
                    Password; %%Plain password
                {selected, [<<"password">>, <<"pass_details">>], [{_Password, PassDetails}]} ->
                    case decode_pass_details(PassDetails) of
                        #scram{} = Scram ->
                            {base64:decode(Scram#scram.storedkey),
                             base64:decode(Scram#scram.serverkey),
                             base64:decode(Scram#scram.salt),
                             Scram#scram.iterationcount};
                        _ ->
                            false
                    end;
                _ ->
                    false
            end
    end.

get_password_s(User, Server) ->
    case jlib:nodeprep(User) of
        error ->
            <<"">>;
        LUser ->
            Username = ejabberd_odbc:escape(LUser),
            LServer = jlib:nameprep(Server),
            case catch odbc_queries:get_password(LServer, Username) of
                {selected, [<<"password">>, <<"pass_details">>], [{Password, _}]} ->
                    Password;
                _ ->
                    <<"">>
            end
    end.

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
    case jlib:nodeprep(User) of
        error ->
            false;
        LUser ->
            Username = ejabberd_odbc:escape(LUser),
            LServer = jlib:nameprep(Server),
            try odbc_queries:get_password(LServer, Username) of
                {selected, [<<"password">>, <<"pass_details">>], [{_Password, _}]} ->
                    true; %% Account exists
                {selected, [<<"password">>, <<"pass_details">>], []} ->
                    false; %% Account does not exist
                {error, Error} ->
                    {error, Error} %% Typical error is that table doesn't exist
            catch
                _:B ->
                    {error, B} %% Typical error is database not accessible
            end
    end.

%% @spec (User, Server) -> ok | error
%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
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

%% @spec (User, Server, Password) -> ok | error | not_exists | not_allowed
%% @doc Remove user if the provided password is correct.
%% TODO do sth with scramed password
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

decode_pass_details(<<?SCRAM_ODBC_PREFIX, PassDetails/binary>>) ->
    case odbc_to_scram(PassDetails) of
        {ok, Decoded} ->
            Decoded;
        Other ->
            ?WARNING_MSG("Incorrect pass_details ~p, ~p", [Other, PassDetails]),
            false
    end;
decode_pass_details(PassDetails) ->
    ?WARNING_MSG("Uknown pass_details ~p", [PassDetails]),
    false.

encode_pass_details(#scram{} = Scram) ->
    {ok, scram_to_odbc(Scram)};
encode_pass_details(PassDetails) ->
    ?WARNING_MSG("Uknown pass_details ~p", [PassDetails]),
    {error, {unknown, PassDetails}}.

%%%------------------------------------------------------------------
%%% SCRAM
%%%------------------------------------------------------------------

scram_to_odbc(#scram{storedkey = StoredKey, serverkey = ServerKey,
                     salt = Salt, iterationcount = IterationCount})->
    IterationCountBin = integer_to_binary(IterationCount),
    << <<?SCRAM_ODBC_PREFIX>>/binary,
       StoredKey/binary,$,,ServerKey/binary,
       $,,Salt/binary,$,,IterationCountBin/binary>>.

odbc_to_scram(PasswordOdbc) ->
    case catch binary:split(PasswordOdbc, <<",">>, [global]) of
        [StoredKey, ServerKey,Salt,IterationCount] ->
            {ok, #scram{storedkey = StoredKey,
                   serverkey = ServerKey,
                   salt = Salt,
                   iterationcount = binary_to_integer(IterationCount)}};
        _ ->
            {error, incorrect_scram}
    end.

prepare_password(Password) ->
    case ejabberd_auth:is_scrammed() of
        true ->
            Scram = ejabberd_auth:password_to_scram(Password),
            case encode_pass_details(Scram) of
                {ok, PassDetails} ->
                    PassDetailsEscaped = ejabberd_odbc:escape(PassDetails),
                    {<<"">>, PassDetailsEscaped};
                _ ->
                    false
            end;
        _ ->
            ejabberd_odbc:escape(Password)
    end.

