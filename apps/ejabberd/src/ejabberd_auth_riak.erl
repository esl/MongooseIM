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

-include("ejabberd.hrl").

%% API
-export([start/1,
         stop/1,
         store_type/1,
         login/2,
         set_password/3,
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
         get_password/3,
         is_user_exists/2,
         remove_user/2,
         remove_user/3,
         plain_password_required/0]).

-export([bucket_type/1]).

-spec start(ejabberd:server()) -> ok.
start(_Host) ->
    ok.

-spec stop(ejabberd:server()) -> ok.
stop(_Host) ->
    ok.

-spec store_type(ejabberd:server()) -> plain | scram.
store_type(Host) ->
    case scram:enabled(Host) of
        false -> plain;
        true -> scram
    end.

-spec set_password(User :: ejabberd:user(),
    Server :: ejabberd:server(),
    Password :: binary()
) -> ok | {error, not_allowed | invalid_jid}.
set_password(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    do_set_password(LUser, LServer, Password).

-spec check_password(User :: ejabberd:user(),
    Server :: ejabberd:server(),
    Password :: binary()) -> boolean().
check_password(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    do_check_password(LUser, LServer, Password).

-spec check_password(ejabberd:user(),
                     ejabberd:server(),
                     binary(),
                     binary(),
                     fun()) -> boolean().
check_password(User, Server, Password, Digest, DigestGen) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    do_check_password(LUser, LServer, Password, Digest, DigestGen).

try_register(User, Server, Password) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    try_register_if_does_not_exist(LUser, LServer, Password).

-spec dirty_get_registered_users() -> [ejabberd:simple_jid()].
dirty_get_registered_users() ->
    Servers = ejabberd_config:get_vh_by_auth_method(riak),
    lists:flatmap(
        fun(Server) ->
            get_vh_registered_users(Server)
        end, Servers).

-spec get_vh_registered_users(ejabberd:server()) ->
    [ejabberd:simple_jid()].
get_vh_registered_users(Server) ->
    LServer = jlib:nameprep(Server),
    do_get_vh_registered_users(LServer).

get_vh_registered_users(Server, Opts) ->
    erlang:error(not_implemented).

-spec get_vh_registered_users_number(ejabberd:server()) -> non_neg_integer().
get_vh_registered_users_number(Server) ->
    length(get_vh_registered_users(Server)).

get_vh_registered_users_number(Server, Opts) ->
    erlang:error(not_implemented).

-spec get_password(ejabberd:user(), ejabberd:server()) -> binary() | false | scram().
get_password(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    case do_get_password(LUser, LServer) of
        false ->
            false;
        #scram{} = Scram ->
            scram:scram_to_tuple(Scram);
        Password ->
            Password
    end.

get_password_s(User, Server) ->
    erlang:error(not_implemented).

get_password(User, Server, DefaultValue) ->
    erlang:error(not_implemented).

-spec is_user_exists(ejabberd:user(), ejabberd:server()) -> boolean().
is_user_exists(User, Server) ->
    does_user_exist_escaped(jlib:nodeprep(User), Server).

-spec remove_user(ejabberd:user(), ejabberd:server()) ->
    ok | {error, term()}.
remove_user(User, Server) ->
    LUser = jlib:nodeprep(User),
    LServer = jlib:nameprep(Server),
    do_remove_user(LUser, LServer).

remove_user(User, Server, Password) ->
    erlang:error(not_implemented).

plain_password_required() ->
    erlang:error(not_implemented).

login(_User, _Server) ->
    erlang:error(not_implemented).

-spec bucket_type(ejabberd:server()) -> {binary(), ejabberd:server()}.
bucket_type(LServer) ->
    {<<"users">>, LServer}.

%% -----------------------------------------------------------------------------
%% Internal functions
%% -----------------------------------------------------------------------------

try_register_if_does_not_exist(LUser, LServer, _)
    when LUser =:= error; LServer =:= error ->
    {error, invalid_jid};
try_register_if_does_not_exist(LUser, LServer, PasswordIn) ->
   case does_user_exist_escaped(LUser, LServer) of
       false ->
           Password = prepare_password(LServer, PasswordIn),
           try_register_with_password(LUser, LServer, Password);
       true ->
           {error, exists}
   end.

try_register_with_password(LUser, LServer, Password) ->
    Now = integer_to_binary(now_to_seconds(os:timestamp())),
    Ops = [{{<<"created">>, register},
            fun(R) -> riakc_register:set(Now, R) end},
           set_password_map_op(Password)],
    UserMap = mongoose_riak:create_new_map(Ops),
    case mongoose_riak:update_type(bucket_type(LServer), LUser,
                                   riakc_map:to_op(UserMap)) of
        {ok, _Map} ->
            ok;
        Error ->
            Error
    end.

do_remove_user(LUser, LServer)
    when LUser =:= error; LServer =:= error->
    {error, invalid_jid};
do_remove_user(LUser, LServer) ->
    mongoose_riak:delete(bucket_type(LServer), LUser).


do_check_password(LUser, LServer, _)
    when LUser =:= error; LServer =:= error->
    false;
do_check_password(LUser, LServer, Password) ->
    case do_get_password(LUser, LServer) of
        false ->
            false;
        #scram{} = Scram ->
            scram:check_password(Password, Scram);
        Password when is_binary(Password) ->
            Password /= <<"">>;
        _ ->
            false
    end.

do_check_password(LUser, LServer, _, _, _)
    when LUser =:= error; LServer =:= error->
    false;
do_check_password(LUser, LServer, Password, Digest, DigestGen) ->
    case do_get_password(LUser, LServer) of
        false ->
            false;
        #scram{} = Scram ->
            scram:check_digest(Scram, Digest, DigestGen, Password);
        PassRiak when is_binary(PassRiak) ->
            ejabberd_auth:check_digest(Digest, DigestGen, Password, PassRiak)
    end.


do_get_password(LUser, LServer)
    when LUser =:= error; LServer =:= error->
    false;
do_get_password(LUser, LServer) ->
    case mongoose_riak:fetch_type(bucket_type(LServer), LUser) of
        {ok, Map} ->
            extract_password(Map);
        _ ->
            false
    end.

do_set_password(LUser, LServer, _)
    when LUser =:= error; LServer =:= error ->
    {error, invalid_jid};
do_set_password(LUser, LServer, Password) ->
    case prepare_password(LServer, Password) of
        false ->
            {error, invalid_password};
        Password ->
            User = mongoose_riak:fetch_type(bucket_type(LServer), LUser),
            do_set_password(User, LUser, LServer, Password)
    end.

do_set_password({error, {notfound, map}}, _, _, _) ->
    {error, not_exists};
do_set_password({ok, Map}, LUser, LServer, Password) ->
    Ops = [set_password_map_op(Password)],
    UpdateMap = mongoose_riak:update_map(Map, Ops),
    case mongoose_riak:update_type(bucket_type(LServer), LUser,
                                   riakc_map:to_op(UpdateMap)) of
        ok ->
            ok;
        Reason ->
            Reason
    end.

do_get_vh_registered_users(LServer) ->
    case mongoose_riak:list_keys(bucket_type(LServer)) of
        {ok, Users} ->
            [{User, LServer} || User <- Users];
        _ ->
            []
    end.

prepare_password(Iterations, Password) when is_integer(Iterations) ->
    Scram = scram:password_to_scram(Password, Iterations),
    PassDetails = scram:serialize(Scram),
    {<<"">>, PassDetails};

prepare_password(Server, Password) ->
    case scram:enabled(Server) of
        true ->
            prepare_password(scram:iterations(Server), Password);
        _ ->
            Password
    end.

does_user_exist_escaped(LUser, LServer) ->
    case mongoose_riak:fetch_type(bucket_type(LServer), LUser) of
        {ok, _} ->
            true;
        {error, {notfound, map}} ->
            false
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

maybe_extract_scram_password(false) ->
    false;
maybe_extract_scram_password({ok, ScramSerialised}) ->
    case scram:deserialize(ScramSerialised) of
        {ok, Scram} ->
            Scram;
        _ ->
            false
    end.

now_to_seconds({MegaSecs, Secs, _MicroSecs}) ->
    MegaSecs * 1000000 + Secs.