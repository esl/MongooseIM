%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@erlang-solutions.com>
%%% @copyright (C) 2014, Erlang Solutions Ltd.
%%% @doc Generic behaviour for Authentication modules
%%% @end
%%% Created : 06. Mar 2014 11:44 PM
%%%-------------------------------------------------------------------
-module(ejabberd_gen_auth).

-callback start(Host :: ejabberd:lserver()) -> ok.

-callback stop(Host :: ejabberd:lserver()) -> ok.

-callback store_type(Host :: ejabberd:lserver()) -> scram | plain.

-callback login(User :: ejabberd:luser(),
                Server :: ejabberd:lserver()) -> boolean().
-callback set_password(User :: ejabberd:luser(),
                       Server :: ejabberd:lserver(),
                       Password :: binary()
                      ) -> ok | {error, not_allowed | invalid_jid}.
-callback check_password(User :: ejabberd:luser(),
                         Server :: ejabberd:lserver(),
                         Password :: binary()) -> boolean().
-callback check_password(User :: ejabberd:luser(),
                         Server :: ejabberd:lserver(),
                         Password :: binary(),
                         Digest :: binary(),
                         DigestGen :: fun()) -> boolean().
-callback try_register(User :: ejabberd:luser(),
                       Server :: ejabberd:lserver(),
                       Password :: binary()
                       ) -> ok
                          | {error, invalid_jid | exists | not_allowed | {aborted, _}}.
-callback dirty_get_registered_users() -> [ejabberd:simple_jid()].

-callback get_vh_registered_users(Server :: ejabberd:lserver()
                                 ) -> [ejabberd:simple_jid()].
-callback get_vh_registered_users(Server :: ejabberd:lserver(),
                                  Opts :: list()) -> [ejabberd:simple_jid()].
-callback get_vh_registered_users_number(Server :: ejabberd:lserver()
                                        ) -> integer().
-callback get_vh_registered_users_number(Server :: ejabberd:lserver(),
                                         Opts :: list()) -> integer().

-callback get_password(User :: ejabberd:luser(),
                       Server :: ejabberd:lserver()) -> binary() | false.
-callback get_password_s(User :: ejabberd:luser(),
                         Server :: ejabberd:lserver()) -> binary().
-callback get_password(User :: ejabberd:luser(),
                       Server :: ejabberd:lserver(),
                       DefaultValue :: binary()) -> binary() | false.
-callback does_user_exist(User :: ejabberd:luser(),
                          Server :: ejabberd:lserver()
                         ) -> boolean() | {error, atom()}.
-callback remove_user(User :: ejabberd:luser(),
                      Server :: ejabberd:lserver()
                      ) -> ok | error | {error, not_allowed}.
-callback remove_user(User :: ejabberd:luser(),
                      Server :: ejabberd:lserver(),
                      Password :: binary()
                      ) -> ok | not_exists | not_allowed | bad_request | error.
-callback plain_password_required() -> boolean().
