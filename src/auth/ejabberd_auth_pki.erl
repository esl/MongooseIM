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
         remove_user/3
        ]).

-export([check_password/3,
         check_password/5]).

-spec start(Host :: ejabberd:lserver()) -> ok.
start(_) -> ok.

-spec stop(Host :: ejabberd:lserver()) -> ok.
stop(_) -> ok.

-spec supports_sasl_module(jid:lserver(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_, Module) -> Module =:= cyrsasl_external.

-spec set_password( User :: ejabberd:luser(),
                    Server :: ejabberd:lserver(),
                    Password :: binary()
                  ) -> ok | {error, not_allowed | invalid_jid}.
set_password(_, _, _) -> {error, not_allowed}.

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()} | {error, any()}.
authorize(Creds) ->
            {ok, mongoose_credentials:extend(Creds, [{auth_module, ?MODULE}])}.

-spec try_register( User :: ejabberd:luser(),
                    Server :: ejabberd:lserver(),
                    Password :: binary()
                  ) -> ok | {error, exists | not_allowed | term()}.
try_register(_, _, _) -> {error, not_allowed}.

-spec dirty_get_registered_users() -> [jid:simple_bare_jid()].
dirty_get_registered_users() -> [].

-spec get_vh_registered_users(Server :: ejabberd:lserver()) -> [jid:simple_bare_jid()].
get_vh_registered_users(_) -> [].

-spec get_vh_registered_users( Server :: ejabberd:lserver(),
                               Opts :: list()
                             ) -> [jid:simple_bare_jid()].
get_vh_registered_users(_, _) -> [].

-spec get_vh_registered_users_number(Server :: ejabberd:lserver()) -> integer().
get_vh_registered_users_number(_) -> 0.

-spec get_vh_registered_users_number( Server :: ejabberd:lserver(),
                                      Opts :: list()
                                    ) -> integer().
get_vh_registered_users_number(_, _) -> 0.

-spec get_password( User :: ejabberd:luser(),
                    Server :: ejabberd:lserver()
                  ) -> ejabberd_auth:passterm() | false.
get_password(_, _) -> false.

-spec get_password_s( User :: ejabberd:luser(),
                      Server :: ejabberd:lserver()
                    ) -> binary().
get_password_s(_, _) -> <<"">>.

-spec does_user_exist( User :: ejabberd:luser(),
                       Server :: ejabberd:lserver()
                     ) -> boolean() | {error, atom()}.
does_user_exist(_, _) -> true.

-spec remove_user( User :: ejabberd:luser(),
                   Server :: ejabberd:lserver()
                 ) -> ok | {error, not_allowed}.
remove_user(_, _) -> {error, not_allowed}.

-spec remove_user( User :: ejabberd:luser(),
                   Server :: ejabberd:lserver(),
                   Password :: binary()
                 ) -> ok | {error, not_exists | not_allowed | bad_request}.
remove_user(_, _, _) -> {error, not_allowed}.

check_password(_, _, _) -> false.

check_password(_, _, _, _, _) -> false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
