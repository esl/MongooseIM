%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_external.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via LDAP external script
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

-module(ejabberd_auth_external).
-author('alexey@process-one.net').

%% External exports
-behaviour(ejabberd_gen_auth).
-export([start/1,
         stop/1,
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
         store_type/1,
         plain_password_required/0
        ]).

%% Exported for behaviour but not implemented
-export([login/2, get_password/3]).

-include("ejabberd.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(Host :: ejabberd:server()) -> 'ok'.
start(Host) ->
    AuthOpts = ejabberd_config:get_local_option(auth_opts, Host),
    {_, ExtauthProgram} = lists:keyfind(extauth_program, 1, AuthOpts),
    extauth:start(Host, ExtauthProgram),
    case check_cache_last_options(Host) of
        cache ->
            ok = ejabberd_auth_internal:start(Host);
        no_cache ->
            ok
    end.

stop(Host) ->
    extauth:stop(Host).


-spec check_cache_last_options(Server :: ejabberd:server()
                              ) -> 'cache' | 'no_cache'.
check_cache_last_options(Server) ->
    %% if extauth_cache is enabled, then a mod_last module must also be enabled
    case get_cache_option(Server) of
        false -> no_cache;
        {true, _CacheTime} ->
            case get_mod_last_configured(Server) of
                no_mod_last ->
                    ?ERROR_MSG("In host ~p extauth is used, extauth_cache is enabled but "
                               "mod_last is not enabled.", [Server]),
                    no_cache;
                _ -> cache
            end
    end.


plain_password_required() ->
    true.

store_type(_) ->
	external.

-spec check_password(User :: ejabberd:user(),
                     Server :: ejabberd:server(),
                     Password :: binary()) -> boolean().
check_password(User, Server, Password) ->
    do_check_password(jlib:nodeprep(User), Server, Password).

do_check_password(LUser, Server, Password) ->
    case get_cache_option(Server) of
        false -> check_password_extauth(LUser, Server, Password);
        {true, CacheTime} -> check_password_cache(LUser, Server, Password, CacheTime)
    end.


-spec check_password(User :: ejabberd:user(),
                     Server :: ejabberd:server(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
check_password(User, Server, Password, _Digest, _DigestGen) ->
    check_password(User, Server, Password).


-spec set_password(User :: ejabberd:user(),
                   Server :: ejabberd:server(),
                   Password :: binary()) -> ok | {error, not_allowed}.
set_password(User, Server, Password) ->
    do_set_password(jlib:nodeprep(User), Server, Password).

do_set_password(LUser, Server, Password) ->
    case extauth:set_password(LUser, Server, Password) of
        true -> set_password_internal(LUser, Server, Password),
                ok;
        _ -> {error, unknown_problem}
    end.


-spec try_register(User :: ejabberd:user(),
                   Server :: ejabberd:server(),
                   Password :: binary()
                   ) -> {atomic, ok | exists} | {error, not_allowed}.
try_register(User, Server, Password) ->
    do_try_register(jlib:nodeprep(User), Server, Password).

-spec do_try_register(User :: error | ejabberd:luser(),
                      Server :: ejabberd:server(),
                      Password :: binary()
                      ) -> {atomic, ok | exists} | {error, not_allowed}.
do_try_register(error, _, _) ->
    {error, invalid_jid};
do_try_register(LUser, Server, Password) ->
    case get_cache_option(Server) of
        false -> try_register_extauth(LUser, Server, Password);
        {true, _CacheTime} -> try_register_external_cache(LUser, Server, Password)
    end.


-spec dirty_get_registered_users() -> [ejabberd:simple_jid()].
dirty_get_registered_users() ->
    ejabberd_auth_internal:dirty_get_registered_users().


-spec get_vh_registered_users(Server :: ejabberd:server()) -> [ejabberd:simple_jid()].
get_vh_registered_users(Server) ->
    ejabberd_auth_internal:get_vh_registered_users(Server).


-spec get_vh_registered_users(Server :: ejabberd:server(),
                              Opts :: list()) -> [ejabberd:simple_jid()].
get_vh_registered_users(Server, Opts)  ->
    ejabberd_auth_internal:get_vh_registered_users(Server, Opts).


-spec get_vh_registered_users_number(Server :: ejabberd:server()) -> integer().
get_vh_registered_users_number(Server) ->
    ejabberd_auth_internal:get_vh_registered_users_number(Server).


-spec get_vh_registered_users_number(Server :: ejabberd:server(),
                                     Opts :: list()) -> integer().
get_vh_registered_users_number(Server, Opts) ->
    ejabberd_auth_internal:get_vh_registered_users_number(Server, Opts).


%% @doc The password can only be returned if cache is enabled, cached info
%% exists and is fresh enough.
-spec get_password(User :: ejabberd:user(),
                   Server :: ejabberd:server()) -> binary() | false.
get_password(User, Server) ->
    LUser = jlib:nodeprep(User),
    case get_cache_option(Server) of
        false -> false;
        {true, CacheTime} -> get_password_cache(LUser, Server, CacheTime)
    end.


-spec get_password_s(User :: ejabberd:user(),
                     Server :: ejabberd:server()) -> binary().
get_password_s(User, Server) ->
    case get_password(User, Server) of
        false -> [];
        Other -> Other
    end.


-spec is_user_exists(User :: ejabberd:user(),
                     Server :: ejabberd:server()) -> boolean() | {error, atom()}.
is_user_exists(User, Server) ->
    LUser = jlib:nodeprep(User),
    try extauth:is_user_exists(LUser, Server) of
        Res -> Res
    catch
        _:Error -> {error, Error}
    end.


-spec remove_user(User :: ejabberd:user(),
                  Server :: ejabberd:server()
                  ) -> ok | error | {error, not_allowed}.
remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    case extauth:remove_user(LUser, Server) of
        false -> false;
        true ->
            case get_cache_option(Server) of
                false -> false;
                {true, _CacheTime} ->
                    ejabberd_auth_internal:remove_user(LUser, Server)
            end
    end.


-spec remove_user(User :: ejabberd:user(),
                  Server :: ejabberd:server(),
                  Password :: binary()
                  ) -> ok | not_exists | not_allowed | bad_request | error.
remove_user(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    case extauth:remove_user(LUser, Server, Password) of
        false -> false;
        true ->
            case get_cache_option(Server) of
                false -> false;
                {true, _CacheTime} ->
                    ejabberd_auth_internal:remove_user(LUser, Server, Password)
            end
    end.

%%%
%%% Extauth cache management
%%%

-spec get_cache_option(Host :: ejabberd:server()
                      ) -> false | {true, CacheTime::integer()}.
get_cache_option(Host) ->
    case ejabberd_config:get_local_option({extauth_cache, Host}) of
        CacheTime when is_integer(CacheTime) -> {true, CacheTime};
        _ -> false
    end.


-spec check_password_extauth(User :: ejabberd:user(),
                             Server :: ejabberd:server(),
                             Password :: binary()) -> boolean().
check_password_extauth(User, Server, Password) ->
    extauth:check_password(User, Server, Password) andalso Password /= "".


-spec try_register_extauth(User :: ejabberd:user(),
                           Server :: ejabberd:server(),
                           Password :: binary()) -> boolean().
try_register_extauth(User, Server, Password) ->
    extauth:try_register(User, Server, Password).


-spec check_password_cache(User :: ejabberd:user(),
                           Server :: ejabberd:server(),
                           Password :: binary(),
                           CacheTime :: integer()) -> boolean().
check_password_cache(User, Server, Password, CacheTime) ->
    case get_last_access(User, Server) of
        online ->
            check_password_internal(User, Server, Password);
        never ->
            check_password_external_cache(User, Server, Password);
        mod_last_required ->
            ?ERROR_MSG("extauth is used, extauth_cache is enabled but mod_last is not enabled in that host", []),
            check_password_external_cache(User, Server, Password);
        TimeStamp ->
            %% If last access exists, compare last access with cache refresh time
            case is_fresh_enough(TimeStamp, CacheTime) of
                %% If no need to refresh, check password against Mnesia
                true ->
                    case check_password_internal(User, Server, Password) of
                        %% If password valid in Mnesia, accept it
                        true ->
                            true;
                        %% Else (password nonvalid in Mnesia), check in extauth and cache result
                        false ->
                            check_password_external_cache(User, Server, Password)
                    end;
                %% Else (need to refresh), check in extauth and cache result
                false ->
                    check_password_external_cache(User, Server, Password)
            end
    end.


get_password_internal(User, Server) ->
    ejabberd_auth_internal:get_password(User, Server).


-spec get_password_cache(User :: ejabberd:user(),
                         Server :: ejabberd:server(),
                         CacheTime :: integer()) -> false | string().
get_password_cache(User, Server, CacheTime) ->
    case get_last_access(User, Server) of
        online ->
            get_password_internal(User, Server);
        never ->
            false;
        mod_last_required ->
            ?ERROR_MSG("extauth is used, extauth_cache is enabled but mod_last is not enabled in that host", []),
            false;
        TimeStamp ->
            case is_fresh_enough(TimeStamp, CacheTime) of
                true ->
                    get_password_internal(User, Server);
                false ->
                    false
            end
    end.


%% @doc Check the password using extauth; if success then cache it
check_password_external_cache(User, Server, Password) ->
    case check_password_extauth(User, Server, Password) of
        true ->
            set_password_internal(User, Server, Password), true;
        false ->
            false
    end.


%% @doc Try to register using extauth; if success then cache it
try_register_external_cache(User, Server, Password) ->
    case try_register_extauth(User, Server, Password) of
        {atomic, ok} = R ->
            set_password_internal(User, Server, Password),
            R;
        _ -> {error, not_allowed}
    end.


-spec check_password_internal(User :: ejabberd:user(),
                              Server :: ejabberd:server(),
                              Password :: binary()) -> boolean().
check_password_internal(User, Server, Password) ->
    ejabberd_auth_internal:check_password(User, Server, Password).


-spec set_password_internal(User :: ejabberd:user(),
                            Server :: ejabberd:server(),
                            Password :: binary()) -> ok | {error, invalid_jid}.
set_password_internal(User, Server, Password) ->
    ejabberd_auth_internal:set_password(User, Server, Password).


-spec is_fresh_enough(TimeLast :: online | never | integer(),
                      CacheTime :: integer() | false) -> boolean().
is_fresh_enough(online, _CacheTime) ->
    true;
is_fresh_enough(never, _CacheTime) ->
    false;
is_fresh_enough(TimeStampLast, CacheTime) ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    Now = MegaSecs * 1000000 + Secs,
    (TimeStampLast + CacheTime > Now).


%% @doc Code copied from mod_configure.erl
%% Code copied from web/ejabberd_web_admin.erl
%% TODO: Update time format to XEP-0202: Entity Time
-spec get_last_access(User :: ejabberd:user(),
                      Server :: ejabberd:server()
                      ) -> online | never | mod_last_required | integer().
get_last_access(User, Server) ->
    case ejabberd_sm:get_user_resources(User, Server) of
        [] ->
            _US = {User, Server},
            case get_last_info(User, Server) of
                mod_last_required ->
                    mod_last_required;
                not_found ->
                    never;
                {ok, Timestamp, _Status} ->
                    Timestamp
            end;
        _ ->
            online
    end.


-spec get_last_info(User :: ejabberd:user(),
                    Server :: ejabberd:server()
                    ) -> {ok, Timestamp :: integer(), Status :: binary()}
                         | not_found | mod_last_required.
get_last_info(User, Server) ->
    case get_mod_last_enabled(Server) of
        mod_last -> mod_last:get_last_info(User, Server);
        mod_last_odbc -> mod_last_odbc:get_last_info(User, Server);
        no_mod_last -> mod_last_required
    end.


-spec get_mod_last_enabled(Server :: ejabberd:server()
                          ) -> mod_last | mod_last_odbc | no_mod_last.
get_mod_last_enabled(Server) ->
    ML = gen_mod:is_loaded(Server, mod_last),
    MLO = gen_mod:is_loaded(Server, mod_last_odbc),
    case {ML, MLO} of
        {true, _} -> mod_last;
        {false, true} -> mod_last_odbc;
        {false, false} -> no_mod_last
    end.


-spec get_mod_last_configured(Server :: ejabberd:server()
                             ) -> mod_last | mod_last_odbc | no_mod_last.
get_mod_last_configured(Server) ->
    ML = is_configured(Server, mod_last),
    MLO = is_configured(Server, mod_last_odbc),
    case {ML, MLO} of
        {true, _} -> mod_last;
        {false, true} -> mod_last_odbc;
        {false, false} -> no_mod_last
    end.

is_configured(Host, Module) ->
    lists:keymember(Module, 1, ejabberd_config:get_local_option({modules, Host})).


%% @doc gen_auth unimplemented callbacks
login(_User, _Server) -> erlang:error(not_implemented).
get_password(_User, _Server, _DefaultValue) -> erlang:error(not_implemented).
