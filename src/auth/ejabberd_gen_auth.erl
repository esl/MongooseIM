%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@erlang-solutions.com>
%%% @copyright (C) 2014, Erlang Solutions Ltd.
%%% @doc Generic behaviour for Authentication modules
%%% @end
%%% Created : 06. Mar 2014 11:44 PM
%%%-------------------------------------------------------------------
-module(ejabberd_gen_auth).

-callback start(Host :: jid:lserver()) -> ok.

-callback stop(Host :: jid:lserver()) -> ok.

-callback store_type(Host :: jid:lserver()) -> scram | plain | external.

-callback set_password(User :: jid:luser(),
                       Server :: jid:lserver(),
                       Password :: binary()
                      ) -> ok | {error, not_allowed | invalid_jid}.

-callback authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                               | {error, any()}.

-callback try_register(User :: jid:luser(),
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

-callback remove_user(User :: jid:luser(),
                      Server :: jid:lserver(),
                      Password :: binary()
                      ) -> ok | {error, not_exists | not_allowed | bad_request}.

-export_type([t/0]).

-type t() :: module().
