%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@erlang-solutions.com>
%%% @copyright (C) 2014, Erlang Solutions Ltd.
%%% @doc Generic behaviour for Authentication modules
%%% @end
%%% Created : 06. Mar 2014 11:44 PM
%%%-------------------------------------------------------------------
-module(ejabberd_gen_auth).

-callback start(HostType :: binary()) -> ok.

-callback stop(HostType :: binary()) -> ok.

-callback supports_sasl_module(HostType :: binary(),
                               Module :: cyrsasl:sasl_module()) -> boolean().

-callback set_password(HostType :: binary(),
                       User :: jid:luser(),
                       Server :: jid:lserver(),
                       Password :: binary()
                      ) -> ok | {error, not_allowed | invalid_jid}.

%% credentials already contain host type
-callback authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                               | {error, any()}.

-callback try_register(HostType :: binary(),
                       User :: jid:luser(),
                       Server :: jid:lserver(),
                       Password :: binary()
                       ) -> ok | {error, exists | not_allowed | term()}.

-callback dirty_get_registered_users() -> [jid:simple_bare_jid()].

-callback get_vh_registered_users(Server :: jid:lserver()) -> [jid:simple_bare_jid()].

-callback get_vh_registered_users(Server :: jid:lserver(), Opts :: list()) ->
    [jid:simple_bare_jid()].

-callback get_vh_registered_users_number(Server :: jid:lserver()) -> integer().

-callback get_vh_registered_users_number(Server :: jid:lserver(), Opts :: list()) -> integer().

-callback get_password(User :: jid:luser(),
                       Server :: jid:lserver()) -> ejabberd_auth:passterm() | false.

-callback get_password_s(User :: jid:luser(),
                         Server :: jid:lserver()) -> binary().

-callback does_user_exist(User :: jid:luser(),
                          Server :: jid:lserver()
                         ) -> boolean() | {error, atom()}.

-callback remove_user(User :: jid:luser(),
                      Server :: jid:lserver()
                      ) -> ok | {error, not_allowed}.

-callback remove_domain(Server :: jid:lserver()) -> ok | {error, term()}.

-callback supported_features() -> [Feature::atom()].


%% implementation of check_password callbacks is required only if
%% auth module uses ejabberd_auth:authorize_with_check_password/2
%% helper interface.
-callback check_password(HostType :: binary(),
                         LUser :: jid:luser(),
                         LServer :: jid:lserver(),
                         Password :: binary()) -> boolean().

-callback check_password(HostType :: binary(),
                         LUser :: jid:luser(),
                         LServer :: jid:lserver(),
                         Password :: binary(),
                         Digest :: binary(),
                         DigestGen :: fun()) -> boolean().

-optional_callbacks([remove_domain/1, supported_features/0,
                     check_password/4, check_password/6]).

-export_type([t/0]).

-type t() :: module().
