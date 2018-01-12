%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@erlang-solutions.com>
%%% @copyright (C) 2014, Erlang Solutions Ltd.
%%% @doc Generic behaviour for Authentication modules
%%% @end
%%% Created : 06. Mar 2014 11:44 PM
%%%-------------------------------------------------------------------
-module(ejabberd_gen_auth).

-callback start(Host :: jlib:lserver()) -> ok.

-callback stop(Host :: jlib:lserver()) -> ok.

-callback store_type(Host :: jlib:lserver()) -> scram | plain | external.

-callback set_password(User :: jlib:luser(),
                       Server :: jlib:lserver(),
                       Password :: binary()
                      ) -> ok | {error, not_allowed | invalid_jid}.

-callback authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                               | {error, any()}.

-callback try_register(User :: jlib:luser(),
                       Server :: jlib:lserver(),
                       Password :: binary()
                       ) -> ok | {error, exists | not_allowed | term()}.

-callback dirty_get_registered_users() -> [jlib:simple_bare_jid()].

-callback get_vh_registered_users(Server :: jlib:lserver()) -> [jlib:simple_bare_jid()].

-callback get_vh_registered_users(Server :: jlib:lserver(), Opts :: list()) ->
    [jlib:simple_bare_jid()].

-callback get_vh_registered_users_number(Server :: jlib:lserver()) -> integer().

-callback get_vh_registered_users_number(Server :: jlib:lserver(), Opts :: list()) -> integer().

-callback get_password(User :: jlib:luser(),
                       Server :: jlib:lserver()) -> ejabberd_auth:passterm() | false.

-callback get_password_s(User :: jlib:luser(),
                         Server :: jlib:lserver()) -> binary().

-callback does_user_exist(User :: jlib:luser(),
                          Server :: jlib:lserver()
                         ) -> boolean() | {error, atom()}.

-callback remove_user(User :: jlib:luser(),
                      Server :: jlib:lserver()
                      ) -> ok | {error, not_allowed}.

-callback remove_user(User :: jlib:luser(),
                      Server :: jlib:lserver(),
                      Password :: binary()
                      ) -> ok | {error, not_exists | not_allowed | bad_request}.

-export_type([t/0]).

-type t() :: module().
