%%%-------------------------------------------------------------------
%%% @author Dmytro Lytovchenko <dmytro.lytovchenko@erlang-solutions.com>
%%% @copyright (C) 2014, Erlang Solutions Ltd.
%%% @doc Generic behaviour for Authentication modules
%%% @end
%%% Created : 06. Mar 2014 11:44 PM
%%%-------------------------------------------------------------------
-module(gen_auth).

-type server() :: binary().
-export_type([ server/0 ]).

-callback login( User :: binary()
               , Server :: server()) -> boolean().
-callback set_password( User :: binary()
                      , Server :: server()
                      , Password :: binary()) -> ok | {error, not_allowed}.
-callback check_password( User :: binary()
                        , Server :: server()
                        , Password :: binary()) -> boolean().
-callback check_password( User :: binary()
                        , Server :: server()
                        , Password :: binary()
                        , Digest :: binary()
                        , DigestGen :: fun()) -> boolean().
-callback try_register( User :: binary()
                      , Server :: server()
                      , Password :: binary()
                      ) -> {atomic, ok | exists} | {error, not_allowed}.
-callback dirty_get_registered_users() -> [ejabberd:user()].
-callback get_vh_registered_users(Server :: binary()) -> [ejabberd:user()].
-callback get_password( User :: binary()
                      , Server :: server()) -> binary() | false.
-callback get_password( User :: binary()
                      , Server :: server()
                      , DefaultValue :: binary()) -> binary() | false.
-callback is_user_exists( User :: binary()
                        , Server :: server()) -> boolean() | {error, atom()}.
-callback remove_user( User :: binary()
                     , Server :: server()) -> ok | error | {error, not_allowed}.
-callback remove_user( User :: binary()
                     , Server :: server()
                     , Password :: binary()
                     ) -> ok | not_exists | not_allowed | bad_request | error.
-callback plain_password_required() -> boolean().
