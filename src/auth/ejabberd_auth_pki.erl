%%%=============================================================================
%%% @copyright (C) 1999-2018, Erlang Solutions Ltd
%%% @author Denys Gonchar <denys.gonchar@erlang-solutions.com>
%%% @doc demo PKI auth backend.
%%%
%%% it authorises all the certificates with Common Name (used as client's
%%% "username"), assuming that all of the certificates are valid.
%%%
%%% certificate verification can be configured for c2s listener.
%%%
%%% as we cannot track properly the list of valid user, does_user_exist/2
%%% function is stubbed to true (this one is used by MAM)
%%% @end
%%%=============================================================================
-module(ejabberd_auth_pki).
-copyright("2018, Erlang Solutions Ltd.").
-author('denys.gonchar@erlang-solutions.com').

-include("jlib.hrl").

-behaviour(ejabberd_gen_auth).

%% ejabberd_gen_auth API
-export([start/1,
         stop/1,
         supports_sasl_module/2,
         set_password/4,
         authorize/1,
         try_register/4,
         dirty_get_registered_users/0,
         get_vh_registered_users/1,
         get_vh_registered_users/2,
         get_vh_registered_users_number/1,
         get_vh_registered_users_number/2,
         get_password/2,
         get_password_s/2,
         does_user_exist/2,
         remove_user/2
        ]).

-export([check_password/4,
         check_password/6]).

-spec start(HostType :: mongooseim:host_type()) -> ok.
start(_) -> ok.

-spec stop(HostType :: mongooseim:host_type()) -> ok.
stop(_) -> ok.

-spec supports_sasl_module(binary(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_, Module) -> Module =:= cyrsasl_external.

-spec set_password( HostType :: mongooseim:host_type(),
                    User :: jid:luser(),
                    Server :: jid:lserver(),
                    Password :: binary()
                  ) -> ok | {error, not_allowed | invalid_jid}.
set_password(_, _, _, _) -> {error, not_allowed}.

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()} | {error, any()}.
authorize(Creds) ->
    {ok, mongoose_credentials:extend(Creds, [{auth_module, ?MODULE}])}.

check_password(_, _, _, _) -> false.

check_password(_, _, _, _, _, _) -> false.

-spec try_register( HostType :: mongooseim:host_type(),
                    User :: jid:luser(),
                    Server :: jid:lserver(),
                    Password :: binary()
                  ) -> ok | {error, exists | not_allowed | term()}.
try_register(_, _, _, _) -> {error, not_allowed}.

-spec dirty_get_registered_users() -> [jid:simple_bare_jid()].
dirty_get_registered_users() -> [].

-spec get_vh_registered_users(Server :: jid:lserver()) -> [jid:simple_bare_jid()].
get_vh_registered_users(_) -> [].

-spec get_vh_registered_users( Server :: jid:lserver(),
                               Opts :: list()
                             ) -> [jid:simple_bare_jid()].
get_vh_registered_users(_, _) -> [].

-spec get_vh_registered_users_number(Server :: jid:lserver()) -> integer().
get_vh_registered_users_number(_) -> 0.

-spec get_vh_registered_users_number( Server :: jid:lserver(),
                                      Opts :: list()
                                    ) -> integer().
get_vh_registered_users_number(_, _) -> 0.

-spec get_password( User :: jid:luser(),
                    Server :: jid:lserver()
                  ) -> ejabberd_auth:passterm() | false.
get_password(_, _) -> false.

-spec get_password_s( User :: jid:luser(),
                      Server :: jid:lserver()
                    ) -> binary().
get_password_s(_, _) -> <<"">>.

-spec does_user_exist( User :: jid:luser(),
                       Server :: jid:lserver()
                     ) -> boolean() | {error, atom()}.
does_user_exist(_, _) -> true.

-spec remove_user( User :: jid:luser(),
                   Server :: jid:lserver()
                 ) -> ok | {error, not_allowed}.
remove_user(_, _) -> {error, not_allowed}.
