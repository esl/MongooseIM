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

-behaviour(mongoose_gen_auth).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").
-include("scram.hrl").

%% API
-export([start/1,
         stop/1,
         config_spec/0,
         supports_sasl_module/2,
         supported_features/0,
         set_password/4,
         authorize/1,
         try_register/4,
         get_registered_users/3,
         get_registered_users_number/3,
         get_password/3,
         get_password_s/3,
         does_user_exist/3,
         remove_user/3
        ]).

%% Internal
-export([check_password/4,
         check_password/6]).

-spec start(mongooseim:host_type()) -> ok.
start(_HostType) ->
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"bucket_type">> => #option{type = binary,
                                              validate = non_empty}},
       defaults = #{<<"bucket_type">> => <<"users">>}
      }.

-spec supports_sasl_module(mongooseim:host_type(), cyrsasl:sasl_module()) -> boolean().
supports_sasl_module(_HostType, cyrsasl_plain) -> true;
supports_sasl_module(HostType, cyrsasl_digest) -> not mongoose_scram:enabled(HostType);
supports_sasl_module(HostType, Mechanism) -> mongoose_scram:enabled(HostType, Mechanism).

-spec supported_features() -> [atom()].
supported_features() -> [dynamic_domains].

-spec set_password(mongooseim:host_type(), jid:luser(), jid:lserver(), binary())
        -> ok | {error, not_allowed | invalid_jid}.
set_password(HostType, LUser, LServer, Password) ->
    case prepare_password(HostType, Password) of
        false ->
            {error, invalid_password};
        Password ->
            User = mongoose_riak:fetch_type(bucket_type(HostType, LServer), LUser),
            do_set_password(User, HostType, LUser, LServer, Password);
        {<<>>, Scram} ->
            User = mongoose_riak:fetch_type(bucket_type(HostType, LServer), LUser),
            do_set_password(User, HostType, LUser, LServer, {<<>>, Scram})
    end.

-spec authorize(mongoose_credentials:t()) -> {ok, mongoose_credentials:t()}
                                           | {error, any()}.
authorize(Creds) ->
    ejabberd_auth:authorize_with_check_password(?MODULE, Creds).

-spec check_password(mongooseim:host_type(), jid:luser(), jid:lserver(), binary()) -> boolean().
check_password(HostType, LUser, LServer, Password) ->
    case do_get_password(HostType, LUser, LServer) of
        false ->
            false;
        Scram when is_record(Scram, scram) orelse is_map(Scram)->
            mongoose_scram:check_password(Password, Scram);
        Password when is_binary(Password) ->
            Password /= <<"">>;
        _ ->
            false
    end.

-spec check_password(mongooseim:host_type(),
                     jid:luser(),
                     jid:lserver(),
                     binary(),
                     binary(),
                     fun()) -> boolean().
check_password(HostType, LUser, LServer, Password, Digest, DigestGen) ->
    case do_get_password(HostType, LUser, LServer) of
        false ->
            false;
        Scram when is_record(Scram, scram) orelse is_map(Scram) ->
            mongoose_scram:check_digest(Scram, Digest, DigestGen, Password);
        PassRiak when is_binary(PassRiak) ->
            ejabberd_auth:check_digest(Digest, DigestGen, Password, PassRiak)
    end.

-spec try_register(HostType :: mongooseim:host_type(),
                   User :: jid:luser(),
                   Server :: jid:lserver(),
                   Password :: binary()
                  ) -> ok | {error, term()}.
try_register(HostType, LUser, LServer, Password) ->
    try_register_if_does_not_exist(HostType, LUser, LServer, Password).

-spec get_registered_users(mongooseim:host_type(), jid:lserver(), list()) ->
    [jid:simple_bare_jid()].
get_registered_users(HostType, LServer, _Opts) ->
    case mongoose_riak:list_keys(bucket_type(HostType, LServer)) of
        {ok, Users} ->
            [{User, LServer} || User <- Users];
        _ ->
            []
    end.

-spec get_registered_users_number(mongooseim:host_type(), jid:lserver(), list()) ->
          non_neg_integer().
get_registered_users_number(HostType, LServer, Opts) ->
    length(get_registered_users(HostType, LServer, Opts)).

-spec get_password(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
          ejabberd_auth:passterm() | false.
get_password(HostType, LUser, LServer) ->
    case do_get_password(HostType, LUser, LServer) of
        false ->
            false;
        Scram when is_map(Scram) ->
            Scram;
        #scram{} = Scram ->
            mongoose_scram:scram_record_to_map(Scram);
        Password ->
            Password
    end.

-spec get_password_s(mongooseim:host_type(), jid:luser(), jid:lserver()) -> binary().
get_password_s(HostType, LUser, LServer) ->
    case get_password(HostType, LUser, LServer) of
        Password when is_binary(Password) ->
            Password;
        _ ->
            <<"">>
    end.

-spec does_user_exist(mongooseim:host_type(), jid:luser(), jid:lserver()) -> boolean().
does_user_exist(HostType, LUser, LServer) ->
    case mongoose_riak:fetch_type(bucket_type(HostType, LServer), LUser) of
        {ok, _} ->
            true;
        {error, {notfound, map}} ->
            false
    end.

-spec remove_user(mongooseim:host_type(), jid:luser(), jid:lserver()) ->
    ok | {error, not_allowed}.
remove_user(HostType, LUser, LServer) ->
    case mongoose_riak:delete(bucket_type(HostType, LServer), LUser) of
        ok -> ok;
        Error ->
            ?LOG_WARNING(#{what => remove_user_failed, reason => Error,
                           user => LUser, server => LServer}),
            {error, not_allowed}
    end.

-spec bucket_type(mongooseim:host_type(), jid:lserver()) -> {binary(), jid:lserver()}.
bucket_type(HostType, LServer) ->
    BucketType = mongoose_config:get_opt([{auth, HostType}, riak, bucket_type]),
    {BucketType, LServer}.

%% -----------------------------------------------------------------------------
%% Internal functions
%% -----------------------------------------------------------------------------

try_register_if_does_not_exist(_, LUser, LServer, _)
    when LUser =:= error; LServer =:= error ->
    {error, invalid_jid};
try_register_if_does_not_exist(HostType, LUser, LServer, PasswordIn) ->
   case does_user_exist(HostType, LUser, LServer) of
       false ->
           Password = prepare_password(HostType, PasswordIn),
           try_register_with_password(HostType, LUser, LServer, Password);
       true ->
           {error, exists}
   end.

try_register_with_password(HostType, LUser, LServer, Password) ->
    Now = integer_to_binary(os:system_time(second)),
    Ops = [{{<<"created">>, register},
            fun(R) -> riakc_register:set(Now, R) end},
           set_password_map_op(Password)],
    UserMap = mongoose_riak:create_new_map(Ops),
    mongoose_riak:update_type(bucket_type(HostType, LServer), LUser, riakc_map:to_op(UserMap)).

do_get_password(HostType, LUser, LServer) ->
    case mongoose_riak:fetch_type(bucket_type(HostType, LServer), LUser) of
        {ok, Map} ->
            case extract_password(Map) of
                false ->
                    ?LOG_WARNING(#{what => scram_serialisation_incorrect,
                                   user => LUser, server => LServer});
                Pwd -> Pwd
            end;
        _ ->
            false
    end.

do_set_password({ok, Map}, HostType, LUser, LServer, Password) ->
    Ops = [set_password_map_op(Password)],
    UpdateMap = mongoose_riak:update_map(Map, Ops),
    mongoose_riak:update_type(bucket_type(HostType, LServer), LUser, riakc_map:to_op(UpdateMap)).

prepare_password(HostType, Iterations, Password) when is_integer(Iterations) ->
    Scram = mongoose_scram:password_to_scram(HostType, Password, Iterations),
    PassDetails = mongoose_scram:serialize(Scram),
    {<<"">>, PassDetails}.

prepare_password(HostType, Password) ->
    case mongoose_scram:enabled(HostType) of
        true ->
            prepare_password(HostType, mongoose_scram:iterations(HostType), Password);
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
