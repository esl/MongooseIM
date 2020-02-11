%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_jwt.erl
%%% Author  : Astro <astro@spaceboyz.net>
%%% Purpose : Authentification with JSON Web Tokens
%%% Created : 02 Aug 2016 by Stephan Maka <stephan@spaceboyz.net>
%%%
%%%
%%% MongooseIM, Copyright (C) 2016   CostaDigital
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

-module(ejabberd_auth_jwt).
-author('astro@spaceboyz.net').

%% External exports
-behaviour(ejabberd_gen_auth).
-export([start/1,
         stop/1,
         set_password/3,
         authorize/1,
         check_password/3,
         check_password/5,
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
         supports_sasl_module/2
        ]).


-include("mongoose.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(Host :: jid:server()) -> ok.
start(Host) ->
    UsernameKey = ejabberd_auth:get_opt(Host, jwt_username_key),
    true = is_atom(UsernameKey) andalso UsernameKey /= undefined,

    JWTSecret = get_jwt_secret(Host),
    JWTAlgorithm = ejabberd_auth:get_opt(Host, jwt_algorithm),
    ejabberd_auth:set_opts(Host,
                           [{jwt_secret, JWTSecret},
                           {jwt_algorithm, list_to_binary(JWTAlgorithm)}]),
    ok.

-spec stop(Host :: jid:server()) -> ok.
stop(_Host) ->
    ok.

-spec supports_sasl_module(jid:lserver(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_, Module) -> Module =:= cyrsasl_plain.

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    ejabberd_auth:authorize_with_check_password(?MODULE, Creds).

-spec check_password(LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary()) -> boolean().
check_password(LUser, LServer, Password) ->
    Key = case ejabberd_auth:get_opt(LServer, jwt_secret) of
              Key1 when is_binary(Key1) -> Key1;
              {env, Var} -> list_to_binary(os:getenv(Var))
          end,
    BinAlg = ejabberd_auth:get_opt(LServer, jwt_algorithm),
    Alg = binary_to_atom(stringprep:tolower(BinAlg), utf8),
    case jwerl:verify(Password, Alg, Key) of
        {ok, TokenData} ->
            UserKey = ejabberd_auth:get_opt(LServer, jwt_username_key),
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


-spec check_password(LUser :: jid:luser(),
                     LServer :: jid:lserver(),
                     Password :: binary(),
                     Digest :: binary(),
                     DigestGen :: fun()) -> boolean().
check_password(LUser, LServer, Password, _Digest, _DigestGen) ->
    check_password(LUser, LServer, Password).


-spec set_password(LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()) -> ok | {error, not_allowed | invalid_jid}.
set_password(_LUser, _LServer, _Password) ->
    {error, not_allowed}.


-spec try_register(LUser :: jid:luser(),
                   LServer :: jid:lserver(),
                   Password :: binary()
                   ) -> ok | {error, exists | not_allowed}.
try_register(_LUser, _LServer, _Password) ->
    {error, not_allowed}.


-spec dirty_get_registered_users() -> [jid:simple_bare_jid()].
dirty_get_registered_users() ->
    [].


-spec get_vh_registered_users(LServer :: jid:lserver()
                             ) -> [jid:simple_bare_jid()].
get_vh_registered_users(_LServer) ->
    [].


-type query_keyword() :: from | to | limit | offset | prefix.
-type query_value() :: integer() | binary().
-spec get_vh_registered_users(LServer :: jid:lserver(),
                              Query :: [{query_keyword(), query_value()}]
                              ) -> [jid:simple_bare_jid()].
get_vh_registered_users(LServer, _) ->
    get_vh_registered_users(LServer).


-spec get_vh_registered_users_number(LServer :: jid:server()
                                    ) -> non_neg_integer().
get_vh_registered_users_number(_LServer) ->
    0.


-spec get_vh_registered_users_number(LServer :: jid:lserver(),
                                     Query :: [{prefix, binary()}]
                                     ) -> integer().
get_vh_registered_users_number(LServer, _) ->
    get_vh_registered_users_number(LServer).


-spec get_password(LUser :: jid:luser(),
                   LServer :: jid:lserver()) -> binary() | false.
get_password(_LUser, _LServer) ->
    false.


-spec get_password_s(LUser :: jid:luser(),
                     LServer :: jid:lserver()) -> binary().
get_password_s(_LUser, _LServer) ->
    <<"">>.

-spec does_user_exist(LUser :: jid:luser(),
                     LServer :: jid:lserver()
                     ) -> boolean() | {error, atom()}.
does_user_exist(_LUser, _LServer) ->
    true.


%% @doc Remove user.
%% Note: it returns ok even if there was some problem removing the user.
-spec remove_user(LUser :: jid:luser(),
                  LServer :: jid:lserver()
                  ) -> ok | {error, not_allowed}.
remove_user(_LUser, _LServer) ->
    ok.


%% @doc Remove user if the provided password is correct.
-spec remove_user(LUser :: jid:luser(),
                  LServer :: jid:lserver(),
                  Password :: binary()
                  ) -> ok | {error, not_exists | not_allowed | bad_request}.
remove_user(_LUser, _LServer, _Password) ->
    {error, not_allowed}.

%%%----------------------------------------------------------------------
%%% Internal helpers
%%%----------------------------------------------------------------------

% A direct path to a file is read only once during startup,
% a path in environment variable is read on every auth request.
get_jwt_secret(Host) ->
   JWTSource = ejabberd_auth:get_opt(Host, jwt_secret_source),
   JWTSecret = ejabberd_auth:get_opt(Host, jwt_secret),

   case {JWTSource, JWTSecret} of
       {undefined, JWTSecret0} when is_list(JWTSecret0) ->
           list_to_binary(JWTSecret0);
       {undefined, JWTSecret0} when is_binary(JWTSecret0) ->
           JWTSecret0;
       {{env, _} = Env, _} ->
           Env;
       {JWTSecretPath, _} when is_list(JWTSecretPath) ->
           {ok, JWTSecretBin} = file:read_file(JWTSecretPath),
           JWTSecretBin
   end.
