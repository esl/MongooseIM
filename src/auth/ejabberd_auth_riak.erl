%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(ejabberd_auth_riak).

-behaviour(ejabberd_gen_auth).

-include("mongoose.hrl").
-include("scram.hrl").

%% API
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

-export([bucket_type/1]).

%% Internal
-export([check_password/3,
         check_password/5]).

-spec start(jid:lserver()) -> ok.
start(_Host) ->
    ok.

-spec stop(jid:lserver()) -> ok.
stop(_Host) ->
    ok.

-spec supports_sasl_module(jid:lserver(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_, cyrsasl_plain) -> true;
supports_sasl_module(_, cyrsasl_scram) -> true;
supports_sasl_module(Host, cyrsasl_digest) -> not mongoose_scram:enabled(Host);
supports_sasl_module(_, _) -> false.

-spec set_password(jid:luser(), jid:lserver(), binary())
        -> ok | {error, not_allowed | invalid_jid}.
set_password(LUser, LServer, Password) ->
    case prepare_password(LServer, Password) of
        false ->
            {error, invalid_password};
        Password ->
            User = mongoose_riak:fetch_type(bucket_type(LServer), LUser),
            do_set_password(User, LUser, LServer, Password)
    end.

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    ejabberd_auth:authorize_with_check_password(?MODULE, Creds).

-spec check_password(jid:luser(), jid:lserver(), binary()) -> boolean().
check_password(LUser, LServer, Password) ->
    case do_get_password(LUser, LServer) of
        false ->
            false;
        #scram{} = Scram ->
            mongoose_scram:check_password(Password, Scram);
        Password when is_binary(Password) ->
            Password /= <<"">>;
        _ ->
            false
    end.

-spec check_password(jid:luser(),
                     jid:lserver(),
                     binary(),
                     binary(),
                     fun()) -> boolean().
check_password(LUser, LServer, Password, Digest, DigestGen) ->
    case do_get_password(LUser, LServer) of
        false ->
            false;
        #scram{} = Scram ->
            mongoose_scram:check_digest(Scram, Digest, DigestGen, Password);
        PassRiak when is_binary(PassRiak) ->
            ejabberd_auth:check_digest(Digest, DigestGen, Password, PassRiak)
    end.

-spec try_register(User :: jid:luser(),
                   Server :: jid:lserver(),
                   Password :: binary()
                  ) -> ok | {error, term()}.
try_register(LUser, LServer, Password) ->
    try_register_if_does_not_exist(LUser, LServer, Password).

-spec dirty_get_registered_users() -> [jid:simple_bare_jid()].
dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(riak),
    lists:flatmap(
        fun(Server) ->
            get_vh_registered_users(Server)
        end, Servers).

-spec get_vh_registered_users(jid:lserver()) ->
    [jid:simple_bare_jid()].
get_vh_registered_users(LServer) ->
    case mongoose_riak:list_keys(bucket_type(LServer)) of
        {ok, Users} ->
            [{User, LServer} || User <- Users];
        _ ->
            []
    end.

-spec get_vh_registered_users(jid:lserver(), list()) ->
    [jid:simple_bare_jid()].
get_vh_registered_users(LServer, _Opts) ->
    get_vh_registered_users(LServer).

-spec get_vh_registered_users_number(jid:lserver()) -> non_neg_integer().
get_vh_registered_users_number(LServer) ->
    length(get_vh_registered_users(LServer)).

-spec get_vh_registered_users_number(jid:lserver(), list()) -> non_neg_integer().
get_vh_registered_users_number(LServer, _Opts) ->
    get_vh_registered_users_number(LServer).

-spec get_password(jid:luser(), jid:lserver()) -> ejabberd_auth:passterm() | false.
get_password(LUser, LServer) ->
    case do_get_password(LUser, LServer) of
        false ->
            false;
        #scram{} = Scram ->
            mongoose_scram:scram_to_tuple(Scram);
        Password ->
            Password
    end.

get_password_s(LUser, LServer) ->
    case get_password(LUser, LServer) of
        Password when is_binary(Password) ->
            Password;
        _ ->
            <<"">>
    end.

-spec does_user_exist(jid:luser(), jid:lserver()) -> boolean().
does_user_exist(LUser, LServer) ->
    case mongoose_riak:fetch_type(bucket_type(LServer), LUser) of
        {ok, _} ->
            true;
        {error, {notfound, map}} ->
            false
    end.

-spec remove_user(jid:luser(), jid:lserver()) ->
    ok | {error, not_allowed}.
remove_user(LUser, LServer) ->
    case mongoose_riak:delete(bucket_type(LServer), LUser) of
        ok -> ok;
        Error ->
            ?WARNING_MSG("Failed Riak query: ~p", [Error]),
            {error, not_allowed}
    end.

remove_user(_LUser, _LServer, _Password) ->
    erlang:error(not_implemented).

-spec bucket_type(jid:lserver()) -> {binary(), jid:lserver()}.
bucket_type(LServer) ->
    Opts = ejabberd_config:get_local_option_or_default({auth_opts, LServer}, []),
    BucketType = proplists:get_value(bucket_type, Opts, <<"users">>),
    {BucketType, LServer}.

%% -----------------------------------------------------------------------------
%% Internal functions
%% -----------------------------------------------------------------------------

try_register_if_does_not_exist(LUser, LServer, _)
    when LUser =:= error; LServer =:= error ->
    {error, invalid_jid};
try_register_if_does_not_exist(LUser, LServer, PasswordIn) ->
   case does_user_exist(LUser, LServer) of
       false ->
           Password = prepare_password(LServer, PasswordIn),
           try_register_with_password(LUser, LServer, Password);
       true ->
           {error, exists}
   end.

try_register_with_password(LUser, LServer, Password) ->
    Now = integer_to_binary(os:system_time(second)),
    Ops = [{{<<"created">>, register},
            fun(R) -> riakc_register:set(Now, R) end},
           set_password_map_op(Password)],
    UserMap = mongoose_riak:create_new_map(Ops),
    mongoose_riak:update_type(bucket_type(LServer), LUser, riakc_map:to_op(UserMap)).

do_get_password(LUser, LServer) ->
    case mongoose_riak:fetch_type(bucket_type(LServer), LUser) of
        {ok, Map} ->
            extract_password(Map);
        _ ->
            false
    end.

do_set_password({ok, Map}, LUser, LServer, Password) ->
    Ops = [set_password_map_op(Password)],
    UpdateMap = mongoose_riak:update_map(Map, Ops),
    mongoose_riak:update_type(bucket_type(LServer), LUser, riakc_map:to_op(UpdateMap)).

prepare_password(Iterations, Password) when is_integer(Iterations) ->
    Scram = mongoose_scram:password_to_scram(Password, Iterations),
    PassDetails = mongoose_scram:serialize(Scram),
    {<<"">>, PassDetails};

prepare_password(Server, Password) ->
    case mongoose_scram:enabled(Server) of
        true ->
            prepare_password(mongoose_scram:iterations(Server), Password);
        _ ->
            Password
    end.

set_password_map_op({_, Scram}) ->
    {{<<"scram">>, register}, fun(R) -> riakc_register:set(Scram, R) end};
set_password_map_op(Password) ->
    {{<<"password">>, register}, fun(R) -> riakc_register:set(Password, R) end}.

extract_password(Map) ->
    case riakc_map:find({<<"password">>, register}, Map) of
        error ->
            maybe_extract_scram_password(riakc_map:find({<<"scram">>, register}, Map));
        {ok, Password} ->
            Password
    end.

-spec maybe_extract_scram_password({ok, binary()} | error) -> mongoose_scram:scram() | false.
maybe_extract_scram_password({ok, ScramSerialised}) ->
    case mongoose_scram:deserialize(ScramSerialised) of
        {ok, Scram} ->
            Scram;
        _ ->
            false
    end;
maybe_extract_scram_password(_) ->
    false.
