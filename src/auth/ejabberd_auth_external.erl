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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth_external).
-author('alexey@process-one.net').

%% External exports
-behaviour(mongoose_gen_auth).

-export([start/1,
         stop/1,
         config_spec/0,
         set_password/4,
         authorize/1,
         try_register/4,
         get_registered_users/3,
         get_registered_users_number/3,
         get_password/3,
         get_password_s/3,
         does_user_exist/3,
         remove_user/3,
         supports_sasl_module/2,
         supported_features/0
        ]).

%% Internal
-export([check_password/4,
         check_password/6]).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(HostType :: mongooseim:host_type()) -> 'ok'.
start(HostType) ->
    Program = mongoose_config:get_opt([{auth, HostType}, external, program]),
    extauth:start(HostType, Program),
    case check_cache_last_options(HostType) of
        cache ->
            ok = ejabberd_auth_internal:start(HostType);
        no_cache ->
            ok
    end.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    extauth:stop(HostType).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"instances">> => #option{type = integer,
                                            validate = positive},
                 <<"program">> => #option{type = string,
                                          validate = non_empty}
                },
       required = [<<"program">>],
       defaults = #{<<"instances">> => 1}
      }.

-spec check_cache_last_options(mongooseim:host_type()) -> 'cache' | 'no_cache'.
check_cache_last_options(HostType) ->
    %% if extauth_cache is enabled, then a mod_last module must also be enabled
    case get_cache_option(HostType) of
        false -> no_cache;
        {true, _CacheTime} ->
            case get_mod_last_configured(HostType) of
                no_mod_last ->
                    ?LOG_ERROR(#{what => mod_last_required_by_extauth,
                                 text => <<"extauth configured with extauth_cache,"
                                          " but mod_last is not enabled">>,
                                 host_type => HostType}),
                    no_cache;
                _ -> cache
            end
    end.

-spec supports_sasl_module(binary(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_, Module) -> Module =:= cyrsasl_plain.

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    ejabberd_auth:authorize_with_check_password(?MODULE, Creds).

-spec check_password(HostType :: mongooseim:host_type(),
                     LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary()) -> boolean().
check_password(HostType, LUser, LServer, Password) ->
    case get_cache_option(HostType) of
        false -> check_password_extauth(HostType, LUser, LServer, Password);
        {true, CacheTime} -> check_password_cache(HostType, LUser, LServer, Password, CacheTime)
    end.


-spec check_password(HostType :: mongooseim:host_type(),
                     LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
check_password(HostType, LUser, LServer, Password, _Digest, _DigestGen) ->
    check_password(HostType, LUser, LServer, Password).


-spec set_password(HostType :: mongooseim:host_type(),
                   LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()) -> ok | {error, not_allowed}.
set_password(HostType, LUser, LServer, Password) ->
    case extauth:set_password(HostType, LUser, LServer, Password) of
        true ->
            UseCache = get_cache_option(HostType),
            maybe_set_password_internal(UseCache, HostType, LUser, LServer, Password);
        _ -> {error, unknown_problem}
    end.

maybe_set_password_internal(false, _, _, _, _) ->
    ok;
maybe_set_password_internal({true, _}, HostType, LUser, LServer, Password) ->
    set_password_internal(HostType, LUser, LServer, Password).


-spec try_register(HostType :: mongooseim:host_type(),
                   LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()
                   ) -> ok | {error, not_allowed}.
try_register(HostType, LUser, LServer, Password) ->
    case get_cache_option(HostType) of
        false -> extauth:try_register(HostType, LUser, LServer, Password);
        {true, _CacheTime} -> try_register_external_cache(HostType, LUser, LServer, Password)
    end.


-spec get_registered_users(HostType :: mongooseim:host_type(),
                           LServer :: jid:lserver(),
                           Opts :: list()) -> [jid:simple_bare_jid()].
get_registered_users(HostType, LServer, Opts)  ->
    ejabberd_auth_internal:get_registered_users(HostType, LServer, Opts).


-spec get_registered_users_number(HostType :: mongooseim:host_type(),
                                     LServer :: jid:lserver(),
                                     Opts :: list()) -> non_neg_integer().
get_registered_users_number(HostType, LServer, Opts) ->
    ejabberd_auth_internal:get_registered_users_number(HostType, LServer, Opts).


%% @doc The password can only be returned if cache is enabled, cached info
%% exists and is fresh enough.
-spec get_password(HostType :: mongooseim:host_type(),
                   LUser :: jid:luser(),
                   LServer :: jid:lserver()) -> binary() | false.
get_password(HostType, LUser, LServer) ->
    case get_cache_option(HostType) of
        false -> false;
        {true, CacheTime} -> get_password_cache(HostType, LUser, LServer, CacheTime)
    end.


-spec get_password_s(HostType :: mongooseim:host_type(),
                     LUser :: jid:luser(),
                     LServer :: jid:lserver()) -> binary().
get_password_s(HostType, LUser, LServer) ->
    case get_password(HostType, LUser, LServer) of
        false -> <<"">>;
        Other -> Other
    end.


-spec does_user_exist(HostType :: mongooseim:host_type(),
                      LUser :: jid:luser(),
                      LServer :: jid:lserver()) -> boolean() | {error, atom()}.
does_user_exist(HostType, LUser, LServer) ->
    try extauth:does_user_exist(HostType, LUser, LServer) of
        Res -> Res
    catch
        _:Error -> {error, Error}
    end.


-spec remove_user(HostType :: mongooseim:host_type(),
                  User :: jid:luser(),
                  Server :: jid:lserver()) -> ok | {error, not_allowed}.
remove_user(HostType, LUser, LServer) ->
    case extauth:remove_user(HostType, LUser, LServer) of
        false -> {error, not_allowed};
        true ->
            case get_cache_option(HostType) of
                false -> ok;
                {true, _CacheTime} ->
                    ejabberd_auth_internal:remove_user(HostType, LUser, LServer)
            end,
            ok
    end.

-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].

%%%
%%% Extauth cache management
%%%

%% FIXME there is no such option in config spec
-spec get_cache_option(mongooseim:host_type()) -> false | {true, CacheTime::integer()}.
get_cache_option(HostType) ->
    case mongoose_config:lookup_opt({extauth_cache, HostType}) of
        {ok, CacheTime} -> {true, CacheTime};
        {error, not_found} -> false
    end.

-spec check_password_extauth(HostType :: mongooseim:host_type(),
                             LUser :: jid:luser(),
                             LServer :: jid:lserver(),
                             Password :: binary()) -> boolean().
check_password_extauth(HostType, LUser, LServer, Password) ->
    extauth:check_password(HostType, LUser, LServer, Password) andalso Password =/= <<>>.

-spec check_password_cache(HostType :: mongooseim:host_type(),
                           LUser :: jid:luser(),
                           LServer :: jid:lserver(),
                           Password :: binary(),
                           CacheTime :: integer()) -> boolean().
check_password_cache(HostType, LUser, LServer, Password, CacheTime) ->
    case get_last_access(HostType, LUser, LServer) of
        online ->
            check_password_internal(HostType, LUser, LServer, Password);
        never ->
            check_password_external_cache(HostType, LUser, LServer, Password);
        mod_last_required ->
            ?LOG_ERROR(#{what => mod_last_required_by_extauth,
                         text => <<"extauth configured with extauth_cache but "
                                  "mod_last is not enabled">>,
                         user => LUser, server => LServer}),
            check_password_external_cache(HostType, LUser, LServer, Password);
        TimeStamp ->
            %% If last access exists, compare last access with cache refresh time
            case is_fresh_enough(TimeStamp, CacheTime) of
                %% If no need to refresh, check password against Mnesia
                true ->
                    check_caches(HostType, LUser, LServer, Password);
                %% Else (need to refresh), check in extauth and cache result
                false ->
                    check_password_external_cache(HostType, LUser, LServer, Password)
            end
    end.

check_caches(HostType, LUser, LServer, Password) ->
    case check_password_internal(HostType, LUser, LServer, Password) of
        true -> true;
        false -> check_password_external_cache(HostType, LUser, LServer, Password)
    end.

get_password_internal(HostType, LUser, LServer) ->
    ejabberd_auth_internal:get_password(HostType, LUser, LServer).


-spec get_password_cache(HostType :: mongooseim:host_type(),
                         LUser :: jid:luser(),
                         LServer :: jid:lserver(),
                         CacheTime :: integer()) -> false | binary().
get_password_cache(HostType, LUser, LServer, CacheTime) ->
    case get_last_access(HostType, LUser, LServer) of
        online ->
            get_password_internal(HostType, LUser, LServer);
        never ->
            false;
        mod_last_required ->
            ?LOG_ERROR(#{what => mod_last_required_by_extauth,
                         text => <<"extauth configured with extauth_cache but "
                                  "mod_last is not enabled">>,
                         user => LUser, server => LServer}),
            false;
        TimeStamp ->
            case is_fresh_enough(TimeStamp, CacheTime) of
                true ->
                    get_password_internal(HostType, LUser, LServer);
                false ->
                    false
            end
    end.


%% @doc Check the password using extauth; if success then cache it
check_password_external_cache(HostType, LUser, LServer, Password) ->
    case check_password_extauth(HostType, LUser, LServer, Password) of
        true ->
            %% FIXME: here we must provide a host type as a first argument
            %% for set_password_internal/4, current implementation will
            %% not work with dynamic domains.
            set_password_internal(LServer, LUser, LServer, Password), true;
        false ->
            false
    end.


%% @doc Try to register using extauth; if success then cache it
-spec try_register_external_cache(HostType :: mongooseim:host_type(),
                                  LUser :: jid:luser(),
                                  LServer :: jid:lserver(),
                                  Password :: binary()) -> ok | {error, not_allowed}.
try_register_external_cache(HostType, LUser, LServer, Password) ->
    case extauth:try_register(HostType, LUser, LServer, Password) of
        ok = R ->
            set_password_internal(HostType, LUser, LServer, Password),
            R;
        _ -> {error, not_allowed}
    end.


-spec check_password_internal(HostType :: mongooseim:host_type(),
                              LUser :: jid:luser(),
                              LServer :: jid:lserver(),
                              Password :: binary()) -> boolean().
check_password_internal(HostType, LUser, LServer, Password) ->
    %% FIXME: here we must provide a host type as a first argument
    %% for ejabberd_auth_internal:check_password/4, current implementation
    %% will not work with dynamic domains.
    ejabberd_auth_internal:check_password(HostType, LUser, LServer, Password).


-spec set_password_internal(HostType :: mongooseim:host_type(),
                            LUser :: jid:luser(),
                            LServer :: jid:lserver(),
                            Password :: binary()) -> ok | {error, invalid_jid}.
set_password_internal(HostType, LUser, LServer, Password) ->
    ejabberd_auth_internal:set_password(HostType, LUser, LServer, Password).


-spec is_fresh_enough(TimeLast :: integer(),
                      CacheTime :: integer()) -> boolean().
is_fresh_enough(TimeStampLast, CacheTime) ->
    Now = erlang:system_time(second),
    (TimeStampLast + CacheTime > Now).


%% @doc Code copied from mod_configure.erl
%% Code copied from web/ejabberd_web_admin.erl
-spec get_last_access(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
          online | never | mod_last_required | integer().
get_last_access(HostType, LUser, LServer) ->
    JID = jid:make_noprep(LUser, LServer, <<>>),
    case ejabberd_sm:get_user_resources(JID) of
        [] ->
            case get_last_info(HostType, LUser, LServer) of
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

-spec get_last_info(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
          {ok, mod_last:timestamp(), mod_last:status()} | not_found | mod_last_required.
get_last_info(HostType, LUser, LServer) ->
    case gen_mod:is_loaded(HostType, mod_last) of
        true -> mod_last:get_last_info(HostType, LUser, LServer);
        _ -> mod_last_required
    end.

-spec get_mod_last_configured(mongooseim:host_type()) -> mod_last | mod_last_rdbms | no_mod_last.
get_mod_last_configured(HostType) ->
    ML = is_configured(HostType, mod_last),
    MLO = is_configured(HostType, mod_last_rdbms),
    case {ML, MLO} of
        {true, _} -> mod_last;
        {false, true} -> mod_last_rdbms;
        {false, false} -> no_mod_last
    end.

is_configured(HostType, Module) ->
    maps:is_key(Module, mongoose_config:get_opt({modules, HostType})).
