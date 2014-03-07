%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@erlang-solutions.com>
%%% @copyright (C) 2014, Erlang Solutions Ltd.
%%% @doc Generic behaviour for Authentication modules
%%% @end
%%% Created : 06. Mar 2014 11:44 PM
%%%-------------------------------------------------------------------
-module(gen_auth).

-callback login(User :: binary(),
                Server :: ejabberd:server()) -> boolean().
-callback set_password(User :: binary(),
                       Server :: ejabberd:server(),
                       Password :: binary())
      -> ok | {error, not_allowed | invalid_jid}.
-callback check_password(User :: binary(),
                         Server :: ejabberd:server(),
                         Password :: binary()) -> boolean().
-callback check_password(User :: binary(),
                         Server :: ejabberd:server(),
                         Password :: binary(),
                         Digest :: binary(),
                         DigestGen :: fun()) -> boolean().
-callback try_register(User :: binary(),
                       Server :: ejabberd:server(),
                       Password :: binary()
                       ) -> {atomic, ok | exists}
                         | {error, invalid_jid | not_allowed} | {aborted, _}.
-callback dirty_get_registered_users() -> [ejabberd:simple_jid()].

-callback get_vh_registered_users(Server :: ejabberd:server()
                                 ) -> [ejabberd:simple_jid()].
-callback get_vh_registered_users(Server :: ejabberd:server(),
                                  Opts :: list()) -> [ejabberd:simple_jid()].
-callback get_vh_registered_users_number(Server :: ejabberd:server()
                                        ) -> integer().
-callback get_vh_registered_users_number(Server :: ejabberd:server(),
                                         Opts :: list()) -> integer().

-callback get_password(User :: binary(),
                       Server :: ejabberd:server()) -> binary() | false.
-callback get_password_s(User :: binary(),
                         Server :: ejabberd:server()) -> binary().
-callback get_password(User :: binary(),
                       Server :: ejabberd:server(),
                       DefaultValue :: binary()) -> binary() | false.
-callback is_user_exists(User :: binary(),
                         Server :: ejabberd:server()
                         ) -> boolean() | {error, atom()}.
-callback remove_user(User :: binary(),
                      Server :: ejabberd:server()
                      ) -> ok | error | {error, not_allowed}.
-callback remove_user(User :: binary(),
                      Server :: ejabberd:server(),
                      Password :: binary()
                      ) -> ok | not_exists | not_allowed | bad_request | error.
-callback plain_password_required() -> boolean().
