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
         authorize/1,
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
         supports_sasl_module/2
        ]).

%% Internal
-export([check_password/3,
         check_password/5]).

-include("mongoose.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(Host :: jid:server()) -> 'ok'.
start(Host) ->
    ExtauthProgram = ejabberd_auth:get_opt(Host, extauth_program),
    extauth:start(Host, ExtauthProgram),
    case check_cache_last_options(Host) of
        cache ->
            ok = ejabberd_auth_internal:start(Host);
        no_cache ->
            ok
    end.

stop(Host) ->
    extauth:stop(Host).


-spec check_cache_last_options(Server :: jid:server()
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

-spec supports_sasl_module(jid:lserver(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_, Module) -> Module =:= cyrsasl_plain.

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    ejabberd_auth:authorize_with_check_password(?MODULE, Creds).

-spec check_password(LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary()) -> boolean().
check_password(LUser, LServer, Password) ->
    case get_cache_option(LServer) of
        false -> check_password_extauth(LUser, LServer, Password);
        {true, CacheTime} -> check_password_cache(LUser, LServer, Password, CacheTime)
    end.


-spec check_password(LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
check_password(LUser, LServer, Password, _Digest, _DigestGen) ->
    check_password(LUser, LServer, Password).


-spec set_password(LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()) -> ok | {error, not_allowed}.
set_password(LUser, LServer, Password) ->
    case extauth:set_password(LUser, LServer, Password) of
        true ->
            UseCache = get_cache_option(LServer),
            maybe_set_password_internal(UseCache, LUser, LServer, Password);
        _ -> {error, unknown_problem}
    end.

maybe_set_password_internal(false, _, _, _) ->
    ok;
maybe_set_password_internal({true, _}, LUser, LServer, Password) ->
    set_password_internal(LUser, LServer, Password).


-spec try_register(LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()
                   ) -> ok | {error, not_allowed}.
try_register(LUser, LServer, Password) ->
    case get_cache_option(LServer) of
        false -> try_register_extauth(LUser, LServer, Password);
        {true, _CacheTime} -> try_register_external_cache(LUser, LServer, Password)
    end.


-spec dirty_get_registered_users() -> [jid:simple_bare_jid()].
dirty_get_registered_users() ->
    ejabberd_auth_internal:dirty_get_registered_users().


-spec get_vh_registered_users(LServer :: jid:lserver()) -> [jid:simple_bare_jid()].
get_vh_registered_users(LServer) ->
    ejabberd_auth_internal:get_vh_registered_users(LServer).


-spec get_vh_registered_users(LServer :: jid:lserver(),
                              Opts :: list()) -> [jid:simple_bare_jid()].
get_vh_registered_users(LServer, Opts)  ->
    ejabberd_auth_internal:get_vh_registered_users(LServer, Opts).


-spec get_vh_registered_users_number(LServer :: jid:lserver()) -> integer().
get_vh_registered_users_number(LServer) ->
    ejabberd_auth_internal:get_vh_registered_users_number(LServer).


-spec get_vh_registered_users_number(LServer :: jid:lserver(),
                                     Opts :: list()) -> integer().
get_vh_registered_users_number(LServer, Opts) ->
    ejabberd_auth_internal:get_vh_registered_users_number(LServer, Opts).


%% @doc The password can only be returned if cache is enabled, cached info
%% exists and is fresh enough.
-spec get_password(LUser :: jid:luser(),
                   LServer :: jid:lserver()) -> binary() | false.
get_password(LUser, LServer) ->
    case get_cache_option(LServer) of
        false -> false;
        {true, CacheTime} -> get_password_cache(LUser, LServer, CacheTime)
    end.


-spec get_password_s(LUser :: jid:luser(),
                     LServer :: jid:lserver()) -> binary().
get_password_s(LUser, LServer) ->
    case get_password(LUser, LServer) of
        false -> <<"">>;
        Other -> Other
    end.


-spec does_user_exist(LUser :: jid:luser(),
                     LServer :: jid:lserver()) -> boolean() | {error, atom()}.
does_user_exist(LUser, LServer) ->
    try extauth:is_user_exists(LUser, LServer) of
        Res -> Res
    catch
        _:Error -> {error, Error}
    end.


-spec remove_user(User :: jid:luser(),
                  Server :: jid:lserver()
                  ) -> ok | {error, not_allowed}.
remove_user(LUser, LServer) ->
    case extauth:remove_user(LUser, LServer) of
        false -> {error, not_allowed};
        true ->
            case get_cache_option(LServer) of
                false -> ok;
                {true, _CacheTime} ->
                    ejabberd_auth_internal:remove_user(LUser, LServer)
            end,
            ok
    end.


-spec remove_user(LUser :: jid:luser(),
                  LServer :: jid:lserver(),
                  Password :: binary()
                  ) -> ok | {error, not_allowed}.
remove_user(LUser, LServer, Password) ->
    case extauth:remove_user(LUser, LServer, Password) of
        false -> {error, not_allowed};
        true ->
            case get_cache_option(LServer) of
                false -> ok;
                {true, _CacheTime} ->
                    ejabberd_auth_internal:remove_user(LUser, LServer, Password)
            end,
            ok
    end.

%%%
%%% Extauth cache management
%%%

-spec get_cache_option(Host :: jid:lserver()
                      ) -> false | {true, CacheTime::integer()}.
get_cache_option(Host) ->
    case ejabberd_config:get_local_option({extauth_cache, Host}) of
        CacheTime when is_integer(CacheTime) -> {true, CacheTime};
        _ -> false
    end.


-spec check_password_extauth(LUser :: jid:luser(),
                             LServer :: jid:lserver(),
                             Password :: binary()) -> boolean().
check_password_extauth(LUser, LServer, Password) ->
    extauth:check_password(LUser, LServer, Password) andalso Password /= "".


-spec try_register_extauth(LUser :: jid:luser(),
                           LServer :: jid:lserver(),
                           Password :: binary()) -> ok | {error, not_allowed}.
try_register_extauth(LUser, LServer, Password) ->
    extauth:try_register(LUser, LServer, Password).


-spec check_password_cache(LUser :: jid:luser(),
                           LServer :: jid:lserver(),
                           Password :: binary(),
                           CacheTime :: integer()) -> boolean().
check_password_cache(LUser, LServer, Password, CacheTime) ->
    case get_last_access(LUser, LServer) of
        online ->
            check_password_internal(LUser, LServer, Password);
        never ->
            check_password_external_cache(LUser, LServer, Password);
        mod_last_required ->
            ?ERROR_MSG("extauth is used, extauth_cache is enabled but mod_last"
                       " is not enabled in that host", []),
            check_password_external_cache(LUser, LServer, Password);
        TimeStamp ->
            %% If last access exists, compare last access with cache refresh time
            case is_fresh_enough(TimeStamp, CacheTime) of
                %% If no need to refresh, check password against Mnesia
                true ->
                    check_caches(LUser, LServer, Password);
                %% Else (need to refresh), check in extauth and cache result
                false ->
                    check_password_external_cache(LUser, LServer, Password)
            end
    end.

check_caches(LUser, LServer, Password) ->
    case check_password_internal(LUser, LServer, Password) of
        true -> true;
        false -> check_password_external_cache(LUser, LServer, Password)
    end.

get_password_internal(LUser, LServer) ->
    ejabberd_auth_internal:get_password(LUser, LServer).


-spec get_password_cache(LUser :: jid:luser(),
                         LServer :: jid:lserver(),
                         CacheTime :: integer()) -> false | binary().
get_password_cache(LUser, LServer, CacheTime) ->
    case get_last_access(LUser, LServer) of
        online ->
            get_password_internal(LUser, LServer);
        never ->
            false;
        mod_last_required ->
            ?ERROR_MSG("extauth is used, extauth_cache is enabled but mod_last"
                       " is not enabled in that host", []),
            false;
        TimeStamp ->
            case is_fresh_enough(TimeStamp, CacheTime) of
                true ->
                    get_password_internal(LUser, LServer);
                false ->
                    false
            end
    end.


%% @doc Check the password using extauth; if success then cache it
check_password_external_cache(LUser, LServer, Password) ->
    case check_password_extauth(LUser, LServer, Password) of
        true ->
            set_password_internal(LUser, LServer, Password), true;
        false ->
            false
    end.


%% @doc Try to register using extauth; if success then cache it
-spec try_register_external_cache(LUser :: jid:luser(),
                                  LServer :: jid:lserver(),
                                  Password :: binary()) -> ok | {error, not_allowed}.
try_register_external_cache(LUser, LServer, Password) ->
    case try_register_extauth(LUser, LServer, Password) of
        ok = R ->
            set_password_internal(LUser, LServer, Password),
            R;
        _ -> {error, not_allowed}
    end.


-spec check_password_internal(LUser :: jid:luser(),
                              LServer :: jid:lserver(),
                              Password :: binary()) -> boolean().
check_password_internal(LUser, LServer, Password) ->
    ejabberd_auth_internal:check_password(LUser, LServer, Password).


-spec set_password_internal(LUser :: jid:luser(),
                            LServer :: jid:lserver(),
                            Password :: binary()) -> ok | {error, invalid_jid}.
set_password_internal(LUser, LServer, Password) ->
    ejabberd_auth_internal:set_password(LUser, LServer, Password).


-spec is_fresh_enough(TimeLast :: integer(),
                      CacheTime :: integer()) -> boolean().
is_fresh_enough(TimeStampLast, CacheTime) ->
    Now = erlang:system_time(second),
    (TimeStampLast + CacheTime > Now).


%% @doc Code copied from mod_configure.erl
%% Code copied from web/ejabberd_web_admin.erl
-spec get_last_access(User :: jid:user(),
                      Server :: jid:server()
                      ) -> online | never | mod_last_required | integer().
get_last_access(User, Server) ->
    JID = jid:make(User, Server, <<>>),
    case ejabberd_sm:get_user_resources(JID) of
        [] ->
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


-spec get_last_info(User :: jid:user(),
                    Server :: jid:server()
                    ) -> {ok, Timestamp :: integer(), Status :: binary()}
                         | not_found | mod_last_required.
get_last_info(User, Server) ->
    case gen_mod:is_loaded(Server, mod_last) of
        true -> mod_last:get_last_info(User, Server);
        _ -> mod_last_required
    end.

-spec get_mod_last_configured(Server :: jid:server()
                             ) -> mod_last | mod_last_rdbms | no_mod_last.
get_mod_last_configured(Server) ->
    ML = is_configured(Server, mod_last),
    MLO = is_configured(Server, mod_last_rdbms),
    case {ML, MLO} of
        {true, _} -> mod_last;
        {false, true} -> mod_last_rdbms;
        {false, false} -> no_mod_last
    end.

is_configured(Host, Module) ->
    lists:keymember(Module, 1, ejabberd_config:get_local_option({modules, Host})).


