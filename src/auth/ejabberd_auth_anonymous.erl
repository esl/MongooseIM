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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth_anonymous).
-author('mickael.remond@process-one.net').

-export([start/1,
         stop/1,
         config_spec/0,
         register_connection/5,
         unregister_connection/5,
         session_cleanup/5
        ]).

-behaviour(mongoose_gen_auth).

%% Function used by ejabberd_auth:
-export([login/3,
         set_password/4,
         authorize/1,
         get_password/3,
         does_user_exist/3,
         supports_sasl_module/2,
         get_registered_users/3,
         supported_features/0
        ]).

%% Internal
-export([check_password/4,
         check_password/6]).

-ignore_xref([register_connection/5, session_cleanup/5, unregister_connection/5,
              login/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("session.hrl").
-include("mongoose_config_spec.hrl").

-record(anonymous, {us  :: jid:simple_bare_jid(),
                    sid :: ejabberd_sm:sid()
                   }).

%% @doc Create the anonymous table if at least one host type has anonymous
%% features enabled. Register to login / logout events
-spec start(mongooseim:host_type()) -> ok.
start(HostType) ->
    %% TODO: Check cluster mode
    mnesia:create_table(anonymous, [{ram_copies, [node()]},
                                    {type, bag},
                                    {attributes, record_info(fields, anonymous)}]),
    mnesia:add_table_copy(anonymous, node(), ram_copies),
    %% The hooks are needed to add / remove users from the anonymous tables
    ejabberd_hooks:add(sm_register_connection_hook, HostType, ?MODULE, register_connection, 100),
    ejabberd_hooks:add(sm_remove_connection_hook, HostType, ?MODULE, unregister_connection, 100),
    ejabberd_hooks:add(session_cleanup, HostType, ?MODULE, session_cleanup, 50),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ejabberd_hooks:delete(sm_register_connection_hook, HostType, ?MODULE, register_connection, 100),
    ejabberd_hooks:delete(sm_remove_connection_hook, HostType, ?MODULE, unregister_connection, 100),
    ejabberd_hooks:delete(session_cleanup, HostType, ?MODULE, session_cleanup, 50),
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"allow_multiple_connections">> => #option{type = boolean},
                 <<"protocol">> => #option{type = atom,
                                           validate = {enum, [sasl_anon, login_anon, both]}}
                },
       defaults = #{<<"allow_multiple_connections">> => false,
                    <<"protocol">> => sasl_anon}
      }.

%% @doc Return true if multiple connections have been allowed in the config file
%% defaults to false
-spec allow_multiple_connections(mongooseim:host_type()) -> boolean().
allow_multiple_connections(HostType) ->
    mongoose_config:get_opt([{auth, HostType}, anonymous, allow_multiple_connections]).

does_user_exist(_, LUser, LServer) ->
    does_anonymous_user_exist(LUser, LServer).

%% @doc Check if user exist in the anonymous database
-spec does_anonymous_user_exist(LUser :: jid:luser(),
                           LServer :: jid:lserver()) -> boolean().
does_anonymous_user_exist(LUser, LServer) ->
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
-spec register_connection(Acc,
                          HostType :: mongooseim:host_type(),
                          SID :: ejabberd_sm:sid(),
                          JID :: jid:jid(),
                          Info :: ejabberd_sm:info()) -> Acc when Acc :: any().
register_connection(Acc, HostType, SID, #jid{luser = LUser, lserver = LServer},
                    #{auth_module := cyrsasl_anonymous}) ->
    mongoose_hooks:register_user(HostType, LServer, LUser),
    US = {LUser, LServer},
    mnesia:sync_dirty(fun() -> mnesia:write(#anonymous{us = US, sid = SID}) end),
    Acc;
register_connection(Acc, _HostType, _SID, _JID, _Info) ->
    Acc.

%% @doc Remove an anonymous user from the anonymous users table
-spec unregister_connection(Acc, SID :: ejabberd_sm:sid(), JID :: jid:jid(),
                            any(), ejabberd_sm:close_reason()) -> Acc
              when Acc :: mongoose_acc:t().
unregister_connection(Acc, SID, #jid{luser = LUser, lserver = LServer}, _, _) ->
    purge_hook(does_anonymous_user_exist(LUser, LServer),
               mongoose_acc:host_type(Acc),
               LUser, LServer),
    remove_connection(SID, LUser, LServer),
    Acc.


%% @doc Launch the hook to purge user data only for anonymous users.
-spec purge_hook(boolean(), mongooseim:host_type(), jid:luser(), jid:lserver()) -> 'ok'.
purge_hook(false, _HostType, _LUser, _LServer) ->
    ok;
purge_hook(true, HostType, LUser, LServer) ->
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              host_type => HostType,
                              lserver => LServer,
                              element => undefined }),
    mongoose_hooks:anonymous_purge_hook(LServer, Acc, LUser).

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

%% @doc When anonymous login is enabled, check the password for permanent users
%% before allowing access
-spec check_password(HostType :: mongooseim:host_type(),
                     LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary()) -> boolean().
check_password(HostType, LUser, LServer, Password) ->
    check_password(HostType, LUser, LServer, Password, undefined, undefined).

check_password(HostType, LUser, LServer, _Password, _Digest, _DigestGen) ->
    %% We refuse login for registered accounts (They cannot logged but
    %% they however are "reserved")
    case ejabberd_auth:does_stored_user_exist(
           HostType, jid:make_noprep(LUser, LServer, <<>>)) of
        %% If user exists in other module, reject anonymous authentication
        true  -> false;
        %% If we are not sure whether the user exists in other module, reject anon auth
        {error, _Error}  -> false;
        false -> login(HostType, LUser, LServer)
    end.


-spec login(HostType :: mongoooseim:host_type(), LUser :: jid:luser(),
            LServer :: jid:lserver()) -> boolean().
login(HostType, LUser, LServer) ->
    case is_protocol_enabled(HostType, login_anon) of
        false -> false;
        true  ->
            case does_anonymous_user_exist(LUser, LServer) of
                %% Reject the login if an anonymous user with the same login
                %% is already logged and if multiple login has not been enable
                %% in the config file.
                true  -> allow_multiple_connections(HostType);
                %% Accept login and add user to the anonymous table
                false -> true
            end
    end.


%% @doc When anonymous login is enabled, check that the user is permanent before
%% changing its password
-spec set_password(HostType :: mongooseim:host_type(),
                   LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()) -> ok | {error, not_allowed}.
set_password(_HostType, LUser, LServer, _Password) ->
    case does_anonymous_user_exist(LUser, LServer) of
        true ->
            ok;
        false ->
            {error, not_allowed}
    end.

-spec get_registered_users(mongooseim:host_type(), jid:lserver(), list()) ->
          [jid:simple_bare_jid()].
get_registered_users(_HostType, LServer, _) ->
    [{U, S} || #session{us = {U, S}} <- ejabberd_sm:get_vh_session_list(LServer)].

%% @doc Return password of permanent user or false for anonymous users
-spec get_password(HostType :: mongooseim:host_type(),
                   LUser :: jid:luser(),
                   LServer :: jid:lserver()) -> binary() | false.
get_password(HostType, LUser, LServer) ->
    case does_anonymous_user_exist(LUser, LServer) orelse login(HostType, LUser, LServer) of
        %% We return the default value if the user is anonymous
        true ->
            <<>>;
        %% We return the permanent user password otherwise
        false ->
            false
    end.

%% @doc Returns true if the SASL mechanism is supportedon the server
%% Anonymous login can be used with a standard authentication method
%% (i.e. with clients that do not support SASL ANONYMOUS)
-spec supports_sasl_module(mongooseim:host_type(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(HostType, cyrsasl_anonymous) ->
    is_protocol_enabled(HostType, sasl_anon);
supports_sasl_module(HostType, cyrsasl_plain) ->
    is_protocol_enabled(HostType, login_anon);
supports_sasl_module(HostType, cyrsasl_digest) ->
    is_protocol_enabled(HostType, login_anon);
supports_sasl_module(HostType, Mechanism) ->
   case mongoose_scram:enabled(HostType, Mechanism) of
      true ->
          is_protocol_enabled(HostType, login_anon);
      _ ->
          false
end.

%% @doc Returns true if the requested anonymous protocol is enabled
-spec is_protocol_enabled(mongooseim:host_type(), sasl_anon | login_anon) -> boolean().
is_protocol_enabled(HostType, Protocol) ->
    case anonymous_protocol(HostType) of
        both -> true;
        Protocol -> true;
        _ -> false
    end.

%% @doc Returns the anonymous protocol to use, defaults to sasl_anon
-spec anonymous_protocol(mongooseim:host_type()) -> sasl_anon | login_anon | both.
anonymous_protocol(HostType) ->
    mongoose_config:get_opt([{auth, HostType}, anonymous, protocol]).

-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].
