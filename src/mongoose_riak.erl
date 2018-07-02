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
-module(mongoose_riak).

-include("mongoose.hrl").
-include_lib("riakc/include/riakc.hrl").

-behaviour(gen_server).

%% API
-export([start/0]).
-export([start_pool/1]).
-export([stop/0]).

-export([put/1, put/2]).
-export([get/2, get/3]).
-export([delete/2, delete/3]).
-export([update_type/3, update_type/4]).
-export([fetch_type/2, fetch_type/3]).
-export([list_keys/1]).
-export([list_buckets/1]).
-export([create_new_map/1]).
-export([update_map/2]).
-export([mapred/2]).
-export([search/2]).
-export([search/3]).
-export([get_index/4]).
-export([get_index_range/5]).

-export([pool_name/0]).

-compile({no_auto_import, [put/2]}).

-define(CALL(F, Args), call_riak(F, Args)).

-type riakc_map_op() :: {{binary(), MapDataType :: atom()},
                          fun((riakc_datatype:datatype()) -> riakc_datatype:datatype())}.

%% proxy worker API
-export([start_link/3]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {conn}).

%%%%

-spec start() -> {ok, pid()} | ignore.
start() ->
    mongoose_wpool:setup_env(),
    case ejabberd_config:get_local_option(riak_server) of
        undefined ->
            ignore;
        RiakOpts ->
            start_pool(RiakOpts)
    end.

start_pool(RiakOpts) ->
    {_, RiakAddr} = get_riak_opt(address, RiakOpts),
    {_, RiakPort} = get_riak_opt(port, RiakOpts),
    {_, Workers} = get_riak_opt(pool_size, RiakOpts, {pool_size, 20}),
    SecurityOptsKeys = [credentials, cacertfile, ssl_opts],
    SecurityOpts = [get_riak_opt(OptKey, RiakOpts) ||
        OptKey <- SecurityOptsKeys],
    RiakPBOpts = [auto_reconnect, keepalive],
    WorkerArgs =
        maybe_add_additional_opts(RiakPBOpts, SecurityOpts),
    Worker = {?MODULE, [RiakAddr, RiakPort, WorkerArgs]},
    PoolOpts = [{workers, Workers},
        {worker, Worker}]
        ++  proplists:get_value(pool_options, RiakOpts, []),

    PoolTimeout = proplists:get_value(pool_timeout, RiakOpts, 5000),
    mongoose_wpool:save_pool_settings(pool_name(), #mongoose_worker_pool{pool_timeout = PoolTimeout}),
    wpool:start_sup_pool(pool_name(), PoolOpts).

-spec stop() -> _.
stop() ->
    wpool:stop_sup_pool(pool_name()).

-spec put(riakc_obj()) ->
    ok | {ok, riakc_obj()} | {ok, key()} | {error, term()}.
put(Obj) ->
    put(Obj, []).

-spec put(riakc_obj(), timeout() | put_options()) ->
    ok | {ok, riakc_obj()} | {ok, key()} | {error, term()}.
put(Obj, OptsOrTimeout) ->
    ?CALL(put, [Obj, OptsOrTimeout]).

-spec get(bucket() | {binary(), bucket()}, key()) -> {ok, riakc_obj()} | {error, term()}.
get(Bucket, Key) ->
    get(Bucket, Key, []).

-spec get(bucket() | {binary(), bucket()}, key(), get_options() | timeout()) ->
    {ok, riakc_obj()} | {error, term()}.
get(Bucket, Key, OptsOrTimeout) ->
    ?CALL(get, [Bucket, Key, OptsOrTimeout]).


-spec update_type({binary(), binary()}, binary(), riakc_datatype:update(term())) ->
    ok | {error, term()}.
update_type(Bucket, Key, Update) ->
    update_type(Bucket, Key, Update, []).

-spec update_type({binary(), binary()}, binary(),
    riakc_datatype:update(term()), [proplists:property()]) ->
    ok | {error, term()}.
update_type(Bucket, Key, Update, Options) ->
    ?CALL(update_type, [Bucket, Key, Update, Options]).

-spec delete(bucket() | {binary(), bucket()}, key()) ->
    ok | {error, term()}.
delete(Bucket, Key) ->
    delete(Bucket, Key, []).

-spec delete(bucket() | {binary(), bucket()}, key(), delete_options() | timeout()) ->
    ok | {error, term()}.
delete(Bucket, Key, OptsOrTimeout) ->
    ?CALL(delete, [Bucket, Key, OptsOrTimeout]).

-spec fetch_type({binary(), binary()}, binary()) ->
    {ok, riakc_datatype:datatype()} | {error, term()}.
fetch_type(Bucket, Key) ->
    fetch_type(Bucket, Key, []).

-spec fetch_type({binary(), binary()}, binary(), [proplists:property()]) ->
    {ok, riakc_datatype:datatype()} | {error, term()}.
fetch_type(Bucket, Key, Opts) ->
    ?CALL(fetch_type, [Bucket, Key, Opts]).

-spec list_keys({binary(), binary()}) ->
    {ok, [binary()]} | {error, term()}.
list_keys(Bucket) ->
    ?CALL(list_keys, [Bucket]).

-spec list_buckets(binary()) -> list().
list_buckets(Type) ->
    ?CALL(list_buckets, [Type]).

-spec create_new_map([riakc_map_op()]) -> riakc_map:crdt_map().
create_new_map(Ops) ->
    update_map(riakc_map:new(), Ops).

-spec update_map(riakc_map:crdt_map(), [riakc_map_op()]) -> riakc_map:crdt_map().
update_map(Map, Ops) ->
    lists:foldl(fun update_map_op/2, Map, Ops).

-type mapred_bucket_type_idx_input() :: {index, riakc_obj:bucket(),
                                         binary()|secondary_index_id(),
                                         StartKey::key()|integer(),
                                         EndKey::key()|integer()}.

-spec mapred(mapred_inputs() |  mapred_bucket_type_idx_input(), [mapred_queryterm()]) ->
    {ok, mapred_result()} | {error, term()}.
mapred(KeyFileters, MapRed) ->
    ?CALL(mapred, [KeyFileters, MapRed]).

search(Index, Query) ->
    search(Index, Query, []).

search(Index, Query, Opts) ->
    ?CALL(search, [Index, Query, Opts]).

-spec get_index(Bucket :: riakc_obj:bucket(),
                Index :: binary() | secondary_index_id(),
                Key :: key() | integer(),
                Opts :: [term()]) ->
    {ok, index_results()} | {error, term()}.
get_index(BucketType, Index, Value, Opts) ->
    ?CALL(get_index_eq, [BucketType, Index, Value, Opts]).

-spec get_index_range(Bucket :: riakc_obj:bucket(),
                      Index :: binary() | secondary_index_id(),
                      StartKey :: key() | integer() | list(),
                      EndKey :: key() | integer() | list(),
                      Opts :: [term()]) ->
    {ok, index_results()} | {error, term()}.
get_index_range(Bucket, Index, StartKey, EndKey, Opts) ->
    ?CALL(get_index_range, [Bucket, Index, StartKey, EndKey, Opts]).

-spec pool_name() ->  atom().
pool_name() -> riak_pool.

update_map_op({Field, Fun}, Map) ->
    riakc_map:update(Field, Fun, Map).

call_riak(F, ArgsIn) ->
    PoolName = pool_name(),
    case mongoose_wpool:get_pool_settings(PoolName) of
        undefined ->
            {error, pool_not_started};
        #mongoose_worker_pool{pool_timeout = PoolTimeout} ->
            wpool:call(PoolName, {F, ArgsIn}, available_worker, PoolTimeout)
    end.

%% @doc Gets a particular option from `Opts`. They're expressed as a list
%% of tuples where the first element is `OptKey`. If provided `OptKey` doesn't
%% exist the `Default` is returned.
-spec get_riak_opt(OptKey :: atom(), Opts :: [tuple()], Default :: tuple()) ->
                                   tuple().
get_riak_opt(OptKey, Opts, Default) ->
    Opt = get_riak_opt(OptKey, Opts),
    verify_if_riak_opt_exists(Opt, Default).

get_riak_opt(OptKey, Opts) ->
    lists:keyfind(OptKey, 1, Opts).

verify_if_riak_opt_exists(false, Default) -> Default;
verify_if_riak_opt_exists(Opt, _) -> Opt.

%% @docs Merges `AdditionalOpts` into `Opts` if a particular additional option
%% exists.
-spec maybe_add_additional_opts(Opts :: [term()],
                                   AdditionalOpts :: [tuple()]) -> [term()].
maybe_add_additional_opts(Opts, AdditionalOpts) ->
    Opts2 = [verify_if_riak_opt_exists(Opt, []) || Opt <- AdditionalOpts],
    lists:flatten(Opts ++ Opts2).


%%%===================================================================
%%% proxy worker
%%%===================================================================

%% It is required here because we want to use available_worker strategy, while:
%%
%% - worker_pool works by calling gen_server:call(WorkerPid, ...), which means the worker must
%%   provide all its functionality via handle_call
%% - riakc_pb_socket has plenty of api methods, there is no way to get straight to handle_call
%% - we could get a worker from a pool and use it to directly call riakc_pb_socket api
%% - ...but you can't do it if you want to use available_worker strategy

start_link(RiakAddr, RiakPort, WorkerArgs) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [RiakAddr, RiakPort, WorkerArgs], []).

init([RiakAddr, RiakPort, WorkerArgs]) ->
    {ok, Conn} = riakc_pb_socket:start_link(RiakAddr, RiakPort, WorkerArgs),
    {ok, #state{conn = Conn}}.

handle_call({F, ArgsIn}, _From, #state{conn = Worker} = State) ->
    Args = [Worker | ArgsIn],
    Res = apply(riakc_pb_socket, F, Args),
    {reply, Res, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
