%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
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
-module(mongoose_http_client).
-include("mongoose.hrl").

%% API
-export([start/0, stop/0, start_pool/2, stop_pool/1, get_pool/1, get/3, post/4]).

%% Exported for testing
-export([start/1]).

-record(pool, {name :: atom(),
               server :: string(),
               size :: pos_integer(),
               max_overflow :: pos_integer(),
               path_prefix :: binary(),
               pool_timeout :: pos_integer(),
               request_timeout :: pos_integer()}).

-opaque pool() :: #pool{}.

-export_type([pool/0]).

%%------------------------------------------------------------------------------
%% API

-spec start() -> any().
start() ->
    case ejabberd_config:get_local_option(http_connections) of
        undefined -> ok;
        Opts -> start(Opts)
    end.

-spec stop() -> any().
stop() ->
    stop_supervisor(),
    ets:delete(tab_name()).

-spec start_pool(atom(), list()) -> ok.
start_pool(Name, Opts) ->
    Pool = make_pool(Name, Opts),
    do_start_pool(Pool),
    ets:insert(tab_name(), Pool),
    ok.

-spec stop_pool(atom()) -> ok.
stop_pool(Name) ->
    SupProc = sup_proc_name(),
    ok = supervisor:terminate_child(SupProc, pool_proc_name(Name)),
    ok = supervisor:delete_child(SupProc, pool_proc_name(Name)),
    ets:delete(tab_name(), Name),
    ok.

-spec get_pool(atom()) -> pool().
get_pool(Name) ->
    [Pool] = ets:lookup(tab_name(), Name),
    Pool.

-spec get(pool(), binary(), list()) ->
                 {ok, {binary(), binary()}} | {error, any()}.
get(Pool, Path, Headers) ->
    make_request(Pool, Path, <<"GET">>, Headers, <<>>).

-spec post(pool(), binary(), list(), binary()) ->
                 {ok, {binary(), binary()}} | {error, any()}.
post(Pool, Path, Headers, Query) ->
    make_request(Pool, Path, <<"POST">>, Headers, Query).

%%------------------------------------------------------------------------------
%% exported for testing

start(Opts) ->
    {ok, _SupProc} = start_supervisor(),
    EjdSupPid = whereis(ejabberd_sup),
    HeirOpt = case self() =:= EjdSupPid of
                  true -> [];
                  false -> [{heir, EjdSupPid, self()}] % for dynamic start
              end,
    ets:new(tab_name(), [set, public, named_table, {keypos, 2},
                         {read_concurrency, true} | HeirOpt]),
    [start_pool(Name, PoolOpts) || {Name, PoolOpts} <- Opts],
    ok.

%%------------------------------------------------------------------------------
%% internal functions

make_pool(Name, Opts) ->
    #pool{name = Name,
          server = gen_mod:get_opt(server, Opts, "http://localhost"),
          size = gen_mod:get_opt(pool_size, Opts, 20),
          max_overflow = gen_mod:get_opt(max_overflow, Opts, 5),
          path_prefix = list_to_binary(gen_mod:get_opt(path_prefix, Opts, "/")),
          pool_timeout = gen_mod:get_opt(pool_timeout, Opts, 200),
          request_timeout = gen_mod:get_opt(request_timeout, Opts, 2000)}.

do_start_pool(#pool{name = Name,
                    server = Server,
                    size = PoolSize,
                    max_overflow = MaxOverflow}) ->
    ProcName = pool_proc_name(Name),
    PoolOpts = [{name, {local, ProcName}},
                {size, PoolSize},
                {max_overflow, MaxOverflow},
                {worker_module, mongoose_http_client_worker}],
    {ok, _} = supervisor:start_child(
                sup_proc_name(),
                poolboy:child_spec(ProcName, PoolOpts, [Server, []])),
    ok.

pool_proc_name(PoolName) ->
    list_to_atom("mongoose_http_client_pool_" ++ atom_to_list(PoolName)).

sup_proc_name() ->
    mongoose_http_client_sup.

tab_name() ->
    mongoose_http_client_pools.

make_request(Pool, Path, Method, Headers, Query) ->
    #pool{path_prefix = PathPrefix,
          name = PoolName,
          pool_timeout = PoolTimeout,
          request_timeout = RequestTimeout} = Pool,
    FullPath = <<PathPrefix/binary, Path/binary>>,
    case catch poolboy:transaction(
                 pool_proc_name(PoolName),
                 fun(WorkerPid) ->
                         fusco:request(WorkerPid, FullPath, Method, Headers, Query, RequestTimeout)
                 end,
                 PoolTimeout) of
        {'EXIT', {timeout, _}} ->
            {error, pool_timeout};
        {ok, {{Code, _Reason}, _RespHeaders, RespBody, _, _}} ->
            {ok, {Code, RespBody}};
        {error, timeout} ->
            {error, request_timeout};
        {error, Reason} ->
            {error, Reason}
    end.

start_supervisor() ->
    Proc = sup_proc_name(),
    ChildSpec =
        {Proc,
         {Proc, start_link, [Proc]},
         permanent,
         infinity,
         supervisor,
         [Proc]},
    {ok, _} = supervisor:start_child(ejabberd_sup, ChildSpec).

stop_supervisor() ->
    Proc = sup_proc_name(),
    ok = supervisor:terminate_child(ejabberd_sup, Proc),
    ok = supervisor:delete_child(ejabberd_sup, Proc).

