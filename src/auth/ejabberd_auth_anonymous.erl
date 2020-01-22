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

-export([
         start/1,
         stop/1,
         anonymous_user_exist/2,
         allow_multiple_connections/1,
         register_connection/4,
         unregister_connection/5,
         session_cleanup/5
        ]).

-behaviour(ejabberd_gen_auth).
%% Function used by ejabberd_auth:
-export([login/2,
         set_password/3,
         authorize/1,
         try_register/3,
         dirty_get_registered_users/0,
         get_vh_registered_users/1,
         get_password/2,
         get_password/3,
         does_user_exist/2,
         remove_user/2,
         remove_user/3,
         supports_sasl_module/2,
         get_vh_registered_users/2,
         get_vh_registered_users_number/1,
         get_vh_registered_users_number/2,
         get_password_s/2                  % not impl
        ]).

%% Internal
-export([check_password/3,
         check_password/5]).

-include("mongoose.hrl").
-include("jlib.hrl").
-record(anonymous, {us  :: jid:simple_bare_jid(),
                    sid :: ejabberd_sm:sid()
                   }).

%% @doc Create the anonymous table if at least one virtual host has anonymous
%% features enabled. Register to login / logout events
start(Host) ->
    %% TODO: Check cluster mode
    mnesia:create_table(anonymous, [{ram_copies, [node()]},
                                    {type, bag},
                                    {attributes, record_info(fields, anonymous)}]),
    mnesia:add_table_copy(anonymous, node(), ram_copies),
    %% The hooks are needed to add / remove users from the anonymous tables
    ejabberd_hooks:add(sm_register_connection_hook, Host, ?MODULE, register_connection, 100),
    ejabberd_hooks:add(sm_remove_connection_hook, Host, ?MODULE, unregister_connection, 100),
    ejabberd_hooks:add(session_cleanup, Host, ?MODULE, session_cleanup, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(sm_register_connection_hook, Host, ?MODULE, register_connection, 100),
    ejabberd_hooks:delete(sm_remove_connection_hook, Host, ?MODULE, unregister_connection, 100),
    ejabberd_hooks:delete(session_cleanup, Host, ?MODULE, session_cleanup, 50),
    ok.

%% @doc Return true if SASL ANONYMOUS mechanism is enabled one the server
-spec is_sasl_anonymous_enabled(Host :: jid:lserver()) -> boolean().
is_sasl_anonymous_enabled(Host) ->
    case anonymous_protocol(Host) of
        sasl_anon -> true;
        both      -> true;
        _Other    -> false
    end.

%% @doc Return true if anonymous login is enabled on the server
%% anonymous login can be used with a standard authentication method
%% (i.e. with clients that do not support anonymous login)
-spec is_login_anonymous_enabled(Host :: jid:lserver()) -> boolean().
is_login_anonymous_enabled(Host) ->
    case anonymous_protocol(Host) of
        login_anon -> true;
        both       -> true;
        _Other     -> false
    end.

%% @doc Return the anonymous protocol to use: sasl_anon|login_anon|both
%% defaults to login_anon
-spec anonymous_protocol(Host :: jid:lserver()) ->
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
-spec allow_multiple_connections(Host :: jid:lserver()) -> boolean().
allow_multiple_connections(Host) ->
    ejabberd_config:get_local_option({allow_multiple_connections, Host}) =:= true.


%% @doc Check if user exist in the anonymus database
-spec anonymous_user_exist(LUser :: jid:luser(),
                           LServer :: jid:lserver()) -> boolean().
anonymous_user_exist(LUser, LServer) ->
    US = {LUser, LServer},
    case catch mnesia:dirty_read({anonymous, US}) of
        [] ->
            false;
        [_H|_T] ->
            true
    end.


%% @doc Remove connection from Mnesia tables
-spec remove_connection(SID :: ejabberd_sm:sid(),
                        LUser :: jid:luser(),
                        LServer :: jid:lserver()
                        ) -> {atomic|aborted|error, _}.
remove_connection(SID, LUser, LServer) ->
    US = {LUser, LServer},
    F = fun() ->
                mnesia:delete_object({anonymous, US, SID})
        end,
    mnesia:transaction(F).


%% @doc Register connection
-spec register_connection(Acc :: map(),
                          SID :: ejabberd_sm:sid(),
                          JID :: jid:jid(),
                          Info :: list()) -> map().
register_connection(Acc, SID, #jid{luser = LUser, lserver = LServer}, Info) ->
    case lists:keyfind(auth_module, 1, Info) of
        {_, ?MODULE} ->
            ejabberd_hooks:run(register_user, LServer, [LUser, LServer]),
            US = {LUser, LServer},
            mnesia:sync_dirty(
              fun() -> mnesia:write(#anonymous{us = US, sid=SID})
              end);
        _ ->
            ok
    end,
    Acc.


%% @doc Remove an anonymous user from the anonymous users table
-spec unregister_connection(Acc :: map(),
                            SID :: ejabberd_sm:sid(),
                            JID :: jid:jid(),
                            any(), ejabberd_sm:close_reason()) -> {atomic|error|aborted, _}.
unregister_connection(Acc, SID, #jid{luser = LUser, lserver = LServer}, _, _) ->
    purge_hook(anonymous_user_exist(LUser, LServer),
               LUser, LServer),
    remove_connection(SID, LUser, LServer),
    Acc.


%% @doc Launch the hook to purge user data only for anonymous users
-spec purge_hook(boolean(), jid:luser(), jid:lserver()) -> 'ok'.
purge_hook(false, _LUser, _LServer) ->
    ok;
purge_hook(true, LUser, LServer) ->
    ejabberd_hooks:run(anonymous_purge_hook, LServer, [LUser, LServer]).

-spec session_cleanup(Acc :: map(), LUser :: jid:luser(),
                      LServer :: jid:lserver(),
                      LResource :: jid:lresource(),
                      SID :: ejabberd_sm:sid()) -> any().
session_cleanup(Acc, LUser, LServer, _LResource, SID) ->
    remove_connection(SID, LUser, LServer),
    Acc.

%% ---------------------------------
%% Specific anonymous auth functions
%% ---------------------------------

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    ejabberd_auth:authorize_with_check_password(?MODULE, Creds).

%% @doc When anonymous login is enabled, check the password for permenant users
%% before allowing access
-spec check_password(LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary()) -> boolean().
check_password(LUser, LServer, Password) ->
    check_password(LUser, LServer, Password, undefined, undefined).
check_password(LUser, LServer, _Password, _Digest, _DigestGen) ->
    %% We refuse login for registered accounts (They cannot logged but
    %% they however are "reserved")
    case ejabberd_auth:is_user_exists_in_other_modules(?MODULE,
                                                       LUser, LServer) of
        %% If user exists in other module, reject anonnymous authentication
        true  -> false;
        %% If we are not sure whether the user exists in other module, reject anon auth
        maybe  -> false;
        false -> login(LUser, LServer)
    end.


-spec login(LUser :: jid:luser(),
            LServer :: jid:lserver()) -> boolean().
login(LUser, LServer) ->
    case is_login_anonymous_enabled(LServer) of
        false -> false;
        true  ->
            case anonymous_user_exist(LUser, LServer) of
                %% Reject the login if an anonymous user with the same login
                %% is already logged and if multiple login has not been enable
                %% in the config file.
                true  -> allow_multiple_connections(LServer);
                %% Accept login and add user to the anonymous table
                false -> true
            end
    end.


%% @doc When anonymous login is enabled, check that the user is permanent before
%% changing its password
-spec set_password(LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()) -> ok | {error, not_allowed}.
set_password(LUser, LServer, _Password) ->
    case anonymous_user_exist(LUser, LServer) of
        true ->
            ok;
        false ->
            {error, not_allowed}
    end.

%% @doc When anonymous login is enabled, check if permanent users are allowed on
%% the server:
-spec try_register(LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()) -> {error, not_allowed}.
try_register(_LUser, _LServer, _Password) ->
    {error, not_allowed}.

-spec dirty_get_registered_users() -> [].
dirty_get_registered_users() ->
    [].

-spec get_vh_registered_users(LServer :: jid:lserver()) -> [jid:simple_bare_jid()].
get_vh_registered_users(LServer) ->
    [{U, S} || {{U, S, _R}, _, _, _} <- ejabberd_sm:get_vh_session_list(LServer)].

-spec get_vh_registered_users(LServer :: jid:lserver(), Opts :: list()) ->
    [jid:simple_bare_jid()].
get_vh_registered_users(LServer, _Opts) ->
  get_vh_registered_users(LServer).


%% @doc Return password of permanent user or false for anonymous users
-spec get_password(LUser :: jid:luser(),
                   LServer :: jid:lserver()) -> binary() | false.
get_password(LUser, LServer) ->
    get_password(LUser, LServer, <<"">>).


-spec get_password(LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   DefaultValue :: binary()) -> binary() | false.
get_password(LUser, LServer, DefaultValue) ->
    case anonymous_user_exist(LUser, LServer) or login(LUser, LServer) of
        %% We return the default value if the user is anonymous
        true ->
            DefaultValue;
        %% We return the permanent user password otherwise
        false ->
            false
    end.


%% @doc Returns true if the user exists in the DB or if an anonymous user is
%% logged under the given name
-spec does_user_exist(LUser :: jid:luser(),
                     LServer :: jid:lserver()) -> boolean().
does_user_exist(LUser, LServer) ->
    anonymous_user_exist(LUser, LServer).


-spec remove_user(LUser :: jid:luser(),
                  LServer :: jid:lserver()) -> {error, not_allowed}.
remove_user(_LUser, _LServer) ->
    {error, not_allowed}.


-spec remove_user(LUser :: jid:luser(),
                  LServer :: jid:lserver(),
                  Password :: binary()) -> {error, not_allowed}.
remove_user(_LUser, _LServer, _Password) ->
    {error, not_allowed}.

-spec supports_sasl_module(jid:lserver(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(Host, cyrsasl_anonymous) -> is_sasl_anonymous_enabled(Host);
supports_sasl_module(Host, cyrsasl_plain) -> is_login_anonymous_enabled(Host);
supports_sasl_module(Host, cyrsasl_scram) -> is_login_anonymous_enabled(Host);
supports_sasl_module(Host, cyrsasl_digest) -> is_login_anonymous_enabled(Host);
supports_sasl_module(_, _) -> false.

get_vh_registered_users_number(_LServer) -> 0.

get_vh_registered_users_number(_LServer, _Opts) -> 0.

%% @doc gen_auth unimplemented callbacks
get_password_s(_LUser, _LServer) -> erlang:error(not_implemented).


