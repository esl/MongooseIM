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

%%% NS is namespace or key.
%%% XML is #xmlel{} or value.
-module(mod_private_cassandra).
-author('arcusfelis@gmail.com').
-behaviour(mod_private).

-export([init/2,
         multi_set_data/3,
         multi_get_data/3,
         remove_user/2]).

-export([prepared_queries/0]).

-include("ejabberd.hrl").
-include("jlib.hrl").

prepared_queries() ->
    [{remove_user, "DELETE FROM private_storage WHERE username = ?"},
     {set_data, "INSERT INTO private_storage (username, namespace, data) VALUES (?,?,?)"},
     {get_data, "SELECT data FROM private_storage WHERE username = ? AND namespace = ?"}].

init(_Host, Opts) ->
    compile_params_module(Opts),
    ok.

multi_set_data(LUser, LServer, [{NS, XML}]) ->
    PoolName = pool_name(LServer, LUser),
    UserJID = jid:make(LUser, LServer, <<>>),
    Params = [LUser, NS, exml:to_binary(XML)],
    mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, set_data, Params),
    ok;
multi_set_data(LUser, LServer, NS2XML) ->
    PoolName = pool_name(LServer, LUser),
    UserJID = jid:make(LUser, LServer, <<>>),
    Queries = [ {set_data, [LUser, NS, exml:to_binary(XML)]} || {NS, XML} <- NS2XML ],
    mongoose_cassandra_worker:cql_batch_pool(PoolName, UserJID, ?MODULE, Queries),
    ok.

multi_get_data(LUser, LServer, NS2Def) ->
    [get_data(LUser, LServer, NS, Default) || {NS, Default} <- NS2Def].

%% @doc Return stored value or default.
get_data(LUser, LServer, NS, Default) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    Params = [LUser, NS],
    {ok, Rows} = mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, get_data, Params),
    case Rows of
        [[SData]] ->
            {ok, Elem} = exml:parse(SData),
            Elem;
        _ ->
            Default
    end.

remove_user(LUser, LServer) ->
    UserJID = jid:make(LUser, LServer, <<>>),
    PoolName = pool_name(LServer, LUser),
    Params = [LUser],
    mongoose_cassandra_worker:cql_query_pool(PoolName, UserJID, ?MODULE, remove_user, Params),
    ok.

%% ----------------------------------------------------------------------
%% Dynamic params module

compile_params_module(Params) ->
    CodeStr = params_helper(Params),
    {Mod, Code} = dynamic_compile:from_string(CodeStr),
    code:load_binary(Mod, "mod_private_cassandra_params.erl", Code).

params_helper(Params) ->
    binary_to_list(iolist_to_binary(io_lib:format(
        "-module(mod_private_cassandra_params).~n"
        "-compile(export_all).~n"
        "pool_name() -> ~p.~n",
        [proplists:get_value(pool_name, Params, default)
        ]))).

-spec pool_name(binary(), binary()) -> term().
pool_name(_LServer, _LUser) ->
    mod_private_cassandra_params:pool_name().
