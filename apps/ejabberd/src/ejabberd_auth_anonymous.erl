%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_anonymous.erl
%%% Author  : Mickael Remond <mickael.remond@process-one.net>
%%% Purpose : Anonymous feature support in ejabberd
%%% Created : 17 Feb 2006 by Mickael Remond <mremond@process-one.net>
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

-module(ejabberd_auth_anonymous).
-author('mickael.remond@process-one.net').

-export([start/1,
  allow_anonymous/1,
  is_sasl_anonymous_enabled/1,
  is_login_anonymous_enabled/1,
  anonymous_user_exist/2,
  allow_multiple_connections/1,
  register_connection/3,
  unregister_connection/3
  ]).

-behaviour(ejabberd_gen_auth).
%% Function used by ejabberd_auth:
-export([login/2,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_password/2,
	 get_password/3,
	 is_user_exists/2,
	 remove_user/2,
	 remove_user/3,
	 store_type/1,
	 plain_password_required/0,
	 get_vh_registered_users/2,       
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2,
	 get_password_s/2                  % not impl
]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-record(anonymous, {us  :: ejabberd:simple_bare_jid(),
                    sid :: ejabberd_sm:sid()
                   }).

%% @doc Create the anonymous table if at least one virtual host has anonymous
%% features enabled. Register to login / logout events
start(Host) ->
    %% TODO: Check cluster mode
    mnesia:create_table(anonymous, [{ram_copies, [node()]},
                                    {type, bag},
                                    {attributes, record_info(fields, anonymous)}]),
    %% The hooks are needed to add / remove users from the anonymous tables
    ejabberd_hooks:add(sm_register_connection_hook, Host,
                       ?MODULE, register_connection, 100),
    ejabberd_hooks:add(sm_remove_connection_hook, Host,
                       ?MODULE, unregister_connection, 100),
    ok.


%% @doc Return true if anonymous is allowed for host or false otherwise
-spec allow_anonymous(Host :: ejabberd:server()) -> boolean().
allow_anonymous(Host) ->
    lists:member(?MODULE, ejabberd_auth:auth_modules(Host)).


%% @doc Return true if anonymous mode is enabled and if anonymous protocol is
%% SASL anonymous protocol can be: sasl_anon|login_anon|both
-spec is_sasl_anonymous_enabled(Host :: ejabberd:server()) -> boolean().
is_sasl_anonymous_enabled(Host) ->
    case allow_anonymous(Host) of
        false -> false;
        true ->
            case anonymous_protocol(Host) of
                sasl_anon -> true;
                both      -> true;
                _Other    -> false
            end
    end.


%% @doc Return true if anonymous login is enabled on the server
%% anonymous login can be use using standard authentication method (i.e. with
%% clients that do not support anonymous login)
-spec is_login_anonymous_enabled(Host :: ejabberd:server()) -> boolean().
is_login_anonymous_enabled(Host) ->
    case allow_anonymous(Host) of
        false -> false;
        true  ->
            case anonymous_protocol(Host) of
                login_anon -> true;
                both       -> true;
                _Other     -> false
            end
    end.


%% @doc Return the anonymous protocol to use: sasl_anon|login_anon|both
%% defaults to login_anon
-spec anonymous_protocol(Host :: ejabberd:server()) ->
                                      'both' | 'login_anon' | 'sasl_anon'.
anonymous_protocol(Host) ->
    case ejabberd_config:get_local_option({anonymous_protocol, Host}) of
        sasl_anon  -> sasl_anon;
        login_anon -> login_anon;
        both       -> both;
        _Other     -> sasl_anon
    end.


%% @doc Return true if multiple connections have been allowed in the config file
%% defaults to false
-spec allow_multiple_connections(Host :: ejabberd:server()) -> boolean().
allow_multiple_connections(Host) ->
    ejabberd_config:get_local_option({allow_multiple_connections, Host}) =:= true.


%% @doc Check if user exist in the anonymus database
-spec anonymous_user_exist(User :: ejabberd:user(),
                           Server :: ejabberd:server()) -> boolean().
anonymous_user_exist(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    US = {LUser, LServer},
    case catch mnesia:dirty_read({anonymous, US}) of
        [] ->
            false;
        [_H|_T] ->
            true
    end.


%% @doc Remove connection from Mnesia tables
-spec remove_connection(SID :: ejabberd_sm:sid(),
                        LUser :: ejabberd:luser(),
                        LServer :: ejabberd:lserver()
                        ) -> {atomic|aborted|error, _}.
remove_connection(SID, LUser, LServer) ->
    US = {LUser, LServer},
    F = fun() ->
                mnesia:delete_object({anonymous, US, SID})
        end,
    mnesia:transaction(F).


%% @doc Register connection
-spec register_connection(SID :: ejabberd_sm:sid(),
                          JID :: ejabberd:jid(),
                          Info :: list()) -> ok.
register_connection(SID, #jid{luser = LUser, lserver = LServer}, Info) ->
    AuthModule = xml:get_attr_s(auth_module, Info),
    case AuthModule == ?MODULE of
        true ->
            ejabberd_hooks:run(register_user, LServer, [LUser, LServer]),
            US = {LUser, LServer},
            mnesia:sync_dirty(
              fun() -> mnesia:write(#anonymous{us = US, sid=SID})
              end);
        false ->
            ok
    end.


%% @doc Remove an anonymous user from the anonymous users table
-spec unregister_connection(SID :: ejabberd_sm:sid(),
                            JID :: ejabberd:jid(),
                            any()) -> {atomic|error|aborted, _}.
unregister_connection(SID, #jid{luser = LUser, lserver = LServer}, _) ->
    purge_hook(anonymous_user_exist(LUser, LServer),
               LUser, LServer),
    remove_connection(SID, LUser, LServer).


%% @doc Launch the hook to purge user data only for anonymous users
-spec purge_hook(boolean(), ejabberd:luser(), ejabberd:lserver()) -> 'ok'.
purge_hook(false, _LUser, _LServer) ->
    ok;
purge_hook(true, LUser, LServer) ->
    ejabberd_hooks:run(anonymous_purge_hook, LServer, [LUser, LServer]).

%% ---------------------------------
%% Specific anonymous auth functions
%% ---------------------------------

%% @doc When anonymous login is enabled, check the password for permenant users
%% before allowing access
-spec check_password(User :: ejabberd:user(),
                     Server :: ejabberd:server(),
                     Password :: binary()) -> boolean().
check_password(User, Server, Password) ->
    check_password(User, Server, Password, undefined, undefined).
check_password(User, Server, _Password, _Digest, _DigestGen) ->
    %% We refuse login for registered accounts (They cannot logged but
    %% they however are "reserved")
    case ejabberd_auth:is_user_exists_in_other_modules(?MODULE,
                                                       User, Server) of
        %% If user exists in other module, reject anonnymous authentication
        true  -> false;
        %% If we are not sure whether the user exists in other module, reject anon auth
        maybe  -> false;
        false -> login(User, Server)
    end.


-spec login(User :: ejabberd:user(),
            Server :: ejabberd:server()) -> boolean().
login(User, Server) ->
    case is_login_anonymous_enabled(Server) of
        false -> false;
        true  ->
            case anonymous_user_exist(User, Server) of
                %% Reject the login if an anonymous user with the same login
                %% is already logged and if multiple login has not been enable
                %% in the config file.
                true  -> allow_multiple_connections(Server);
                %% Accept login and add user to the anonymous table
                false -> true
            end
    end.


%% @doc When anonymous login is enabled, check that the user is permanent before
%% changing its password
-spec set_password(User :: ejabberd:user(),
                   Server :: ejabberd:server(),
                   Password :: binary()) -> ok | {error, not_allowed}.
set_password(User, Server, _Password) ->
    case anonymous_user_exist(User, Server) of
        true ->
            ok;
        false ->
            {error, not_allowed}
    end.

%% @doc When anonymous login is enabled, check if permanent users are allowed on
%% the server:
-spec try_register(User :: ejabberd:user(),
                   Server :: ejabberd:server(),
                   Password :: binary()) -> {error, not_allowed}.
try_register(_User, _Server, _Password) ->
    {error, not_allowed}.

-spec dirty_get_registered_users() -> [].
dirty_get_registered_users() ->
    [].

-spec get_vh_registered_users(Server :: ejabberd:server()
                             ) -> [ejabberd:simple_jid()].
get_vh_registered_users(Server) ->
    [{U, S} || {{U, S, _R}, _, _, _} <- ejabberd_sm:get_vh_session_list(Server)].

get_vh_registered_users(Server, _Opts) ->
  get_vh_registered_users(Server).


%% @doc Return password of permanent user or false for anonymous users
-spec get_password(User :: ejabberd:user(),
                   Server :: ejabberd:server()) -> binary() | false.
get_password(User, Server) ->
    get_password(User, Server, "").


-spec get_password(User :: ejabberd:user(),
                   Server :: ejabberd:server(),
                   DefaultValue :: binary()) -> binary() | false.
get_password(User, Server, DefaultValue) ->
    case anonymous_user_exist(User, Server) or login(User, Server) of
        %% We return the default value if the user is anonymous
        true ->
            DefaultValue;
        %% We return the permanent user password otherwise
        false ->
            false
    end.


%% @doc Returns true if the user exists in the DB or if an anonymous user is
%% logged under the given name
-spec is_user_exists(User :: ejabberd:user(),
                     Server :: ejabberd:server()) -> boolean().
is_user_exists(User, Server) ->
    anonymous_user_exist(User, Server).


-spec remove_user(User :: ejabberd:user(),
                  Server :: ejabberd:server()) -> {error, not_allowed}.
remove_user(_User, _Server) ->
    {error, not_allowed}.


-spec remove_user(User :: ejabberd:user(),
                  Server :: ejabberd:server(),
                  Password :: binary()) -> 'not_allowed'.
remove_user(_User, _Server, _Password) ->
    not_allowed.


-spec plain_password_required() -> 'false'.
plain_password_required() ->
    false.

store_type(_) ->
    plain.

get_vh_registered_users_number(_Server) -> 0.

get_vh_registered_users_number(_Server, _Opts) -> 0.

%% @doc gen_auth unimplemented callbacks
get_password_s(_User, _Server) -> erlang:error(not_implemented).
