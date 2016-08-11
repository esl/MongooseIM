%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_internal.erl
%%% Author  : Astro <astro@spaceboyz.net>
%%% Purpose : Authentification with JSON Web Tokens
%%% Created : 02 Aug 2016 by Stephan Maka <stephan@spaceboyz.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2016   CostaDigital
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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth_jwt).
-author('astro@spaceboyz.net').

%% External exports
-behaviour(ejabberd_gen_auth).
-export([start/1,
         stop/1,
         set_password/3,
         authorize/1,
         check_password/3,
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
         remove_user/3,
         store_type/1
        ]).


-include("ejabberd.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(Host :: ejabberd:server()) -> ok.
start(Host) ->
    ok.

-spec stop(Host :: ejabberd:server()) -> ok.
stop(_Host) ->
    ok.

store_type(Server) ->
    external.

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    ejabberd_auth:authorize_with_check_password(?MODULE, Creds).

-spec check_password(LUser :: ejabberd:luser(),
                     LServer :: ejabberd:lserver(),
                     Password :: binary()) -> boolean().
check_password(LUser, LServer, Password) ->
    Key = case get_auth_opt(LServer, jwt_key) of
              Key1 when is_binary(Key1) ->
                  Key1;
              Key1 when is_list(Key1) ->
                  list_to_binary(Key1);
              {env, Var} ->
                  list_to_binary(os:getenv(Var))
          end,
    Options = #{key => Key, alg => <<"HS256">>},
    case jwerl:verify(Password, Options) of
        {ok, TokenData} ->
            %% Example: bookingNumber => <<"10857838">>
            UserKey = get_auth_opt(LServer, token_user_key),
            case maps:find(UserKey, TokenData) of
                {ok, LUser} ->
                    %% Login username matches $token_user_key in TokenData
                    ?INFO_MSG("Successfully authenticated with JWT.~nTokenData: ~p~n", [TokenData]),
                    true;
                {ok, ExpectedUser} ->
                    ?WARNING_MSG("Wrong JWT for user ~p /= expected ~p~n", [LUser, ExpectedUser]),
                    false;
                error ->
                    ?WARNING_MSG("Missing key ~p in JWT ~p~n", [UserKey, TokenData]),
                    false
            end;
        {error, Reason} ->
            ?WARNING_MSG("Cannot verify JWT for user ~s: ~p~n", [LUser, Reason]),
            false
    end.


-spec check_password(LUser :: ejabberd:luser(),
                     LServer :: ejabberd:lserver(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
check_password(LUser, LServer, Password, _Digest, _DigestGen) ->
    check_password(LUser, LServer, Password).


-spec set_password(LUser :: ejabberd:luser(),
                   LServer :: ejabberd:lserver(),
                   Password :: binary()) -> ok | {error, not_allowed | invalid_jid}.
set_password(_LUser, _LServer, _Password) ->
    {error, not_allowed}.


-spec try_register(LUser :: ejabberd:luser(),
                   LServer :: ejabberd:lserver(),
                   Password :: binary()
                   ) -> ok | {error, exists | not_allowed}.
try_register(_LUser, _LServer, _Password) ->
    {error, not_allowed}.


-spec dirty_get_registered_users() -> [ejabberd:simple_bare_jid()].
dirty_get_registered_users() ->
    [].


-spec get_vh_registered_users(LServer :: ejabberd:lserver()
                             ) -> [ejabberd:simple_bare_jid()].
get_vh_registered_users(_LServer) ->
    [].


-type query_keyword() :: from | to | limit | offset | prefix.
-type query_value() :: integer() | binary().
-spec get_vh_registered_users(LServer :: ejabberd:lserver(),
                              Query :: [{query_keyword(), query_value()}]
                              ) -> [ejabberd:simple_bare_jid()].
get_vh_registered_users(LServer, _) ->
    get_vh_registered_users(LServer).


-spec get_vh_registered_users_number(LServer :: ejabberd:server()
                                    ) -> non_neg_integer().
get_vh_registered_users_number(_LServer) ->
    0.


-spec get_vh_registered_users_number(LServer :: ejabberd:lserver(),
                                     Query :: [{prefix, binary()}]
                                     ) -> integer().
get_vh_registered_users_number(LServer, _) ->
    get_vh_registered_users_number(LServer).


-spec get_password(LUser :: ejabberd:luser(),
                   LServer :: ejabberd:lserver()) -> binary() | false.
get_password(_LUser, _LServer) ->
    false.


-spec get_password_s(LUser :: ejabberd:luser(),
                     LServer :: ejabberd:lserver()) -> binary().
get_password_s(_LUser, _LServer) ->
    <<"">>.

-spec does_user_exist(LUser :: ejabberd:luser(),
                     LServer :: ejabberd:lserver()
                     ) -> boolean() | {error, atom()}.
does_user_exist(_LUser, _LServer) ->
    true.


%% @doc Remove user.
%% Note: it returns ok even if there was some problem removing the user.
-spec remove_user(LUser :: ejabberd:luser(),
                  LServer :: ejabberd:lserver()
                  ) -> ok | {error, not_allowed}.
remove_user(_LUser, _LServer) ->
    ok.


%% @doc Remove user if the provided password is correct.
-spec remove_user(LUser :: ejabberd:luser(),
                  LServer :: ejabberd:lserver(),
                  Password :: binary()
                  ) -> ok | {error, not_exists | not_allowed | bad_request}.
remove_user(_LUser, _LServer, _Password) ->
    {error, not_allowed}.


%% Internal helper
get_auth_opt(Host, Key) ->
    case ejabberd_config:get_local_option(auth_opts, Host) of
        undefined ->
            false;
        AuthOpts ->
            case lists:keyfind(Key, 1, AuthOpts) of
                {Key, Value} ->
                    Value;
                false ->
                    undefined
            end
    end.
