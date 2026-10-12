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
         does_user_exist/3,
         remove_user/3,
         supports_sasl_module/2,
         supported_features/0
        ]).

%% Internal
-export([check_password/4,
         check_password/6]).

-include("mongoose_config_spec.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(HostType :: mongooseim:host_type()) -> 'ok'.
start(HostType) ->
    Program = mongoose_config:get_opt([{auth, HostType}, external, program]),
    extauth:start(HostType, Program).

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
    extauth:check_password(HostType, LUser, LServer, Password) andalso Password =/= ~"".


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
        true -> ok;
        _ -> {error, unknown_problem}
    end.


-spec try_register(HostType :: mongooseim:host_type(),
                   LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()
                   ) -> ok | {error, not_allowed}.
try_register(HostType, LUser, LServer, Password) ->
    extauth:try_register(HostType, LUser, LServer, Password).


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
        true -> ok
    end.

-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].
