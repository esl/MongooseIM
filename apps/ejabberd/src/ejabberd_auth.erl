%%%----------------------------------------------------------------------
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

%% TODO: Use the functions in ejabberd auth to add and remove users.

-module(ejabberd_auth).
-author('alexey@process-one.net').

%% External exports
-export([start/0,
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
	 entropy/1
	]).

-export([auth_modules/1]).

-include("ejabberd.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    lists:foreach(
      fun(Host) ->
	      lists:foreach(
		fun(M) ->
			M:start(Host)
		end, auth_modules(Host))
      end, ?MYHOSTS).

plain_password_required(Server) ->
    lists:any(
      fun(M) ->
	      M:plain_password_required()
      end, auth_modules(Server)).

%% @doc Check if the user and password can login in server.
%% @spec (User::string(), Server::string(), Password::string()) ->
%%     true | false
check_password(User, Server, Password) 
  when is_list(User), is_list(Server), is_list(Password) ->
    check_password(list_to_binary(User), 
                   list_to_binary(Server), 
                   list_to_binary(Password));
check_password(User, Server, Password) ->
    case check_password_with_authmodule(User, Server, Password) of
	{true, _AuthModule} -> true;
	false -> false
    end.

%% @doc Check if the user and password can login in server.
%% @spec (User::string(), Server::string(), Password::string(),
%%        Digest::string(), DigestGen::function()) ->
%%     true | false
check_password(User, Server, Password, Digest, DigestGen) 
  when is_list(User), is_list(Server), is_list(Password)->
    check_password(list_to_binary(User), 
                   list_to_binary(Server), 
                   list_to_binary(Password),
                   Digest,
                   DigestGen);
check_password(User, Server, Password, Digest, DigestGen) 
  when is_list(Digest) ->
    check_password(User, Server, Password, list_to_binary(Digest), DigestGen);    
check_password(User, Server, Password, Digest, DigestGen) ->
    case check_password_with_authmodule(User, Server, Password,
					Digest, DigestGen) of
	{true, _AuthModule} -> true;
	false -> false
    end.

%% @doc Check if the user and password can login in server.
%% The user can login if at least an authentication method accepts the user
%% and the password.
%% The first authentication method that accepts the credentials is returned.
%% @spec (User::string(), Server::string(), Password::string()) ->
%%     {true, AuthModule} | false
%% where
%%   AuthModule = ejabberd_auth_anonymous | ejabberd_auth_external
%%                 | ejabberd_auth_internal | ejabberd_auth_ldap
%%                 | ejabberd_auth_odbc | ejabberd_auth_pam
check_password_with_authmodule(User, Server, Password)
  when is_list(User), is_list(Server), is_list(Password) ->
    check_password_with_authmodule(list_to_binary(User), 
                                   list_to_binary(Server), 
                                   list_to_binary(Password));
check_password_with_authmodule(User, Server, Password) ->
    check_password_loop(auth_modules(Server), [User, Server, Password]).

check_password_with_authmodule(User, Server, Password, Digest, DigestGen) 
  when is_list(User), is_list(Server), is_list(Password) ->
    check_password_with_authmodule(list_to_binary(User), 
                                   list_to_binary(Server), 
                                   list_to_binary(Password),
                                   Digest,
                                   DigestGen);
check_password_with_authmodule(User, Server, Password, Digest, DigestGen) ->
    check_password_loop(auth_modules(Server), [User, Server, Password,
					       Digest, DigestGen]).

check_password_loop([], Args) ->
    [User, Server, Password | _] = Args,
    ejabberd_hooks:run(auth_failed, Server, [User, Server, Password]),
    false;
check_password_loop([AuthModule | AuthModules], Args) ->
    case apply(AuthModule, check_password, Args) of
	true ->
	    {true, AuthModule};
	false ->
	    check_password_loop(AuthModules, Args)
    end.


%% @spec (User::string(), Server::string(), Password::string()) ->
%%       ok | {error, ErrorType}
%% where ErrorType = empty_password | not_allowed | invalid_jid
set_password(User, Server, Password)
  when is_list(User), is_list(Server), is_list(Password) ->
    set_password(list_to_binary(User), 
                 list_to_binary(Server), 
                 list_to_binary(Password));
set_password(_User, _Server, "") ->
    %% We do not allow empty password
    {error, empty_password};
set_password(User, Server, Password) ->
    lists:foldl(
      fun(M, {error, _}) ->
	      M:set_password(User, Server, Password);
	 (_M, Res) ->
	      Res
      end, {error, not_allowed}, auth_modules(Server)).

%% @spec (User, Server, Password) -> {atomic, ok} | {atomic, exists} | {error, not_allowed}
try_register(User, Server, Password)
  when is_list(User), is_list(Server), is_list(Password) ->
    try_register(list_to_binary(User), 
                 list_to_binary(Server), 
                 list_to_binary(Password));
try_register(_User, _Server, "") ->
    %% We do not allow empty password
    {error, not_allowed};    
try_register(User, Server, Password) ->
    case is_user_exists(User,Server) of
	true ->
	    {atomic, exists};
	false ->
	    case lists:member(jlib:nameprep(Server), ?MYHOSTS) of
		true ->
		    Res = lists:foldl(
		      fun(_M, {atomic, ok} = Res) ->
			      Res;
			 (M, _) ->
			      M:try_register(User, Server, Password)
		      end, {error, not_allowed}, auth_modules(Server)),
		    case Res of
			{atomic, ok} ->
			    ejabberd_hooks:run(register_user, Server,
					       [User, Server]),
			    {atomic, ok};
			_ -> Res
		    end;
		false ->
		    {error, not_allowed}
	    end
    end.

%% Registered users list do not include anonymous users logged
dirty_get_registered_users() ->
    lists:flatmap(
      fun(M) ->
	      M:dirty_get_registered_users()
      end, auth_modules()).

%% Registered users list do not include anonymous users logged
get_vh_registered_users(Server) when is_list(Server) ->
    get_vh_registered_users(list_to_binary(Server));
get_vh_registered_users(Server) ->
    lists:flatmap(
      fun(M) ->
	      M:get_vh_registered_users(Server)
      end, auth_modules(Server)).

get_vh_registered_users(Server, Opts) when is_list(Server) ->
    get_vh_registered_users(list_to_binary(Server), Opts);
get_vh_registered_users(Server, Opts) ->
    lists:flatmap(
      fun(M) ->
		case erlang:function_exported(
		       M, get_vh_registered_users, 2) of
		    true ->
			M:get_vh_registered_users(Server, Opts);
		    false ->
			M:get_vh_registered_users(Server)
		end
      end, auth_modules(Server)).

get_vh_registered_users_number(Server) when is_list(Server) ->
    get_vh_registered_users_number(list_to_binary(Server));
get_vh_registered_users_number(Server) ->
    lists:sum(
      lists:map(
	fun(M) ->
		case erlang:function_exported(
		       M, get_vh_registered_users_number, 1) of
		    true ->
			M:get_vh_registered_users_number(Server);
		    false ->
			length(M:get_vh_registered_users(Server))
		end
	end, auth_modules(Server))).

get_vh_registered_users_number(Server, Opts) when is_list(Server) ->
    get_vh_registered_users_number(list_to_binary(Server), Opts);
get_vh_registered_users_number(Server, Opts) ->
    lists:sum(
      lists:map(
	fun(M) ->
		case erlang:function_exported(
		       M, get_vh_registered_users_number, 2) of
		    true ->
			M:get_vh_registered_users_number(Server, Opts);
		    false ->
			length(M:get_vh_registered_users(Server))
		end
	end, auth_modules(Server))).

%% @doc Get the password of the user.
%% @spec (User::string(), Server::string()) -> Password::string()
get_password(User, Server) when is_list(User), is_list(Server) ->
    list_to_binary(get_password(list_to_binary(User), 
                                list_to_binary(Server)));
get_password(User, Server) ->
    lists:foldl(
      fun(M, false) ->
	      M:get_password(User, Server);
	 (_M, Password) ->
	      Password
      end, false, auth_modules(Server)).

get_password_s(User, Server) when is_list(User), is_list(Server) ->
    list_to_binary(get_password_s(list_to_binary(User), 
                                  list_to_binary(Server)));
get_password_s(User, Server) ->
    case get_password(User, Server) of
	false ->
	    <<"">>;
	Password ->
	    Password
    end.

%% @doc Get the password of the user and the auth module.
-spec get_password_with_authmodule(User::binary(), Server::binary()) ->
                        {Password::binary(), AuthModule::atom()}
                        | {false, none}.
get_password_with_authmodule(User, Server) ->
    lists:foldl(
      fun(M, {false, _}) ->
	      {M:get_password(User, Server), M};
	 (_M, {Password, AuthModule}) ->
	      {Password, AuthModule}
      end, {false, none}, auth_modules(Server)).

%% Returns true if the user exists in the DB or if an anonymous user is logged
%% under the given name
is_user_exists(User, Server) when is_list(User), is_list(Server) ->
    is_user_exists(list_to_binary(User), 
                   list_to_binary(Server));
is_user_exists(User, Server) ->
    lists:any(
      fun(M) ->
	      case M:is_user_exists(User, Server) of
		  {error, Error} ->
		      ?ERROR_MSG("The authentication module ~p returned an "
				 "error~nwhen checking user ~p in server ~p~n"
				 "Error message: ~p",
				 [M, User, Server, Error]),
		      false;
		  Else ->
		      Else
	      end
      end, auth_modules(Server)).

%% Check if the user exists in all authentications module except the module
%% passed as parameter
%% @spec (Module::atom(), User, Server) -> true | false | maybe
is_user_exists_in_other_modules(Module, User, Server) when is_list(User), is_list(Server) ->
    is_user_exists_in_other_modules(Module,
                                    list_to_binary(User), 
                                    list_to_binary(Server));
is_user_exists_in_other_modules(Module, User, Server) ->
    is_user_exists_in_other_modules_loop(
      auth_modules(Server)--[Module],
      User, Server).

is_user_exists_in_other_modules_loop([], _User, _Server) ->
    false;
is_user_exists_in_other_modules_loop([AuthModule|AuthModules], User, Server) ->
    case AuthModule:is_user_exists(User, Server) of
	true ->
	    true;
	false ->
	    is_user_exists_in_other_modules_loop(AuthModules, User, Server);
	{error, Error} ->
	    ?DEBUG("The authentication module ~p returned an error~nwhen "
		   "checking user ~p in server ~p~nError message: ~p",
		   [AuthModule, User, Server, Error]),
	    maybe
    end.


%% @spec (User, Server) -> ok | error | {error, not_allowed}
%% @doc Remove user.
%% Note: it may return ok even if there was some problem removing the user.
remove_user(User, Server) when is_list(User), is_list(Server) ->
    remove_user(list_to_binary(User), 
                list_to_binary(Server));
remove_user(User, Server) ->
    [M:remove_user(User, Server) || M <- auth_modules(Server)],
    ejabberd_hooks:run(remove_user, jlib:nameprep(Server), [User, Server]),
    ok.

%% @spec (User, Server, Password) -> ok | not_exists | not_allowed | bad_request | error
%% @doc Try to remove user if the provided password is correct.
%% The removal is attempted in each auth method provided:
%% when one returns 'ok' the loop stops;
%% if no method returns 'ok' then it returns the error message indicated by the last method attempted.
remove_user(User, Server, Password) 
  when is_list(User), is_list(Server), is_list(Password)->
    remove_user(list_to_binary(User), 
                list_to_binary(Server),
                list_to_binary(Password));
remove_user(User, Server, Password) ->
    R = lists:foldl(
      fun(_M, ok = Res) ->
	      Res;
	 (M, _) ->
	      M:remove_user(User, Server, Password)
      end, error, auth_modules(Server)),
    case R of
		ok -> ejabberd_hooks:run(remove_user, jlib:nameprep(Server), [User, Server]);
		_ -> none
    end,
    R.

%% @spec (IOList) -> non_negative_float()
%% @doc Calculate informational entropy.
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
%% Return the lists of all the auth modules actually used in the
%% configuration
auth_modules() ->
    lists:usort(
      lists:flatmap(
	fun(Server) ->
		auth_modules(Server)
	end, ?MYHOSTS)).

%% Return the list of authenticated modules for a given host
auth_modules(Server) ->
    LServer = jlib:nameprep(Server),
    Method = ejabberd_config:get_local_option({auth_method, LServer}),
    Methods = if
		  Method == undefined -> [];
		  is_list(Method) -> Method;
		  is_atom(Method) -> [Method]
	      end,
    [list_to_atom("ejabberd_auth_" ++ atom_to_list(M)) || M <- Methods].
