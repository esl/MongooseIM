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
-module(mod_http_client).

-behaviour(gen_mod).

%% API
-export([start_pool/3, stop_pool/2, get_pool/2, make_request/5]).

%% gen_mod callbacks
-export([start/2, stop/1]).

-record(pool, {name :: atom(),
               host :: ejabberd:server(),
               http_host :: string(),
               size :: pos_integer(),
               max_overflow :: pos_integer(),
               path_prefix :: binary(),
               pool_timeout :: pos_integer(),
               request_timeout :: pos_integer()}).

-opaque pool() :: #pool{}.

-export_type([pool/0]).

%%------------------------------------------------------------------------------
%% API

-spec start_pool(ejabberd:server(), atom(), list()) -> ok.
start_pool(Host, Name, Opts) ->
    Pool = make_pool(Host, Name, Opts),
    do_start_pool(Pool),
    ets:insert(tab_name(Host), Pool),
    ok.

-spec get_pool(ejabberd:server(), atom()) -> pool().
get_pool(Host, Name) ->
    [Pool] = ets:lookup(tab_name(Host), Name),
    Pool.

-spec stop_pool(ejabberd:server(), atom()) -> ok.
stop_pool(Host, Name) ->
    SupProc = gen_mod:get_module_proc(Host, ejabberd_mod_http_client_sup),
    ok = supervisor:terminate_child(SupProc, pool_proc_name(Host, Name)),
    ok = supervisor:delete_child(SupProc, pool_proc_name(Host, Name)),
    ets:delete(tab_name(Host), Name),
    ok.

-spec make_request(pool(), binary(), binary(), list(), binary()) ->
                          {ok, {binary(), binary()}} | {error, any()}.
make_request(Pool, Path, Method, Header, Query) ->
    #pool{path_prefix = PathPrefix,
          name = Name,
          host = Host,
          pool_timeout = PoolTimeout,
          request_timeout = RequestTimeout} = Pool,
    FullPath = <<PathPrefix/binary, Path/binary>>,
    case catch poolboy:transaction(
                 pool_proc_name(Host, Name),
                 fun(WorkerPid) ->
                         fusco:request(WorkerPid, FullPath, Method, Header, Query, RequestTimeout)
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

%%------------------------------------------------------------------------------
%% gen_mod callbacks

-spec start(ejabberd:server(), list()) -> any().
start(Host, Opts) ->
    {ok, _SupProc} = start_supervisor(Host),
    EjdSupPid = whereis(ejabberd_sup),
    HeirOpt = case self() =:= EjdSupPid of
                  true -> [];
                  false -> [{heir, EjdSupPid, self()}] % for dynamic start
              end,
    ets:new(tab_name(Host), [set, public, named_table, {keypos, 2},
                             {read_concurrency, true} | HeirOpt]),
    PoolSpec = gen_mod:get_opt(pools, Opts, []),
    [start_pool(Host, Name, PoolOpts) || {Name, PoolOpts} <- PoolSpec],
    ok.

-spec stop(ejabberd:server()) -> any().
stop(Host) ->
    stop_supervisor(Host),
    ets:delete(tab_name(Host)).

%%------------------------------------------------------------------------------
%% internal functions

make_pool(Host, Name, Opts) ->
    #pool{name = Name,
          host = Host,
          http_host = gen_mod:get_opt(host, Opts, "http://localhost"),
          size = gen_mod:get_opt(pool_size, Opts, 20),
          max_overflow = gen_mod:get_opt(max_overflow, Opts, 5),
          path_prefix = list_to_binary(gen_mod:get_opt(path_prefix, Opts, "")),
          pool_timeout = gen_mod:get_opt(pool_timeout, Opts, 200),
          request_timeout = gen_mod:get_opt(request_timeout, Opts, 2000)}.

do_start_pool(#pool{name = Name,
                    host = Host,
                    http_host = HttpHost,
                    size = PoolSize,
                    max_overflow = MaxOverflow}) ->
    ProcName = pool_proc_name(Host, Name),
    PoolOpts = [{name, {local, ProcName}},
                {size, PoolSize},
                {max_overflow, MaxOverflow},
                {worker_module, mod_http_client_worker}],
    SupProc = gen_mod:get_module_proc(Host, ejabberd_mod_http_client_sup),
    {ok, _} = supervisor:start_child(
                SupProc,
                poolboy:child_spec(ProcName, PoolOpts, [HttpHost, []])),
    ok.

pool_proc_name(Host, PoolName) ->
    gen_mod:get_module_proc(Host, PoolName).

tab_name(Host) ->
    %% NOTE: The naming scheme for processes is reused for ETS table
    gen_mod:get_module_proc(Host, mod_http_client_pools).

start_supervisor(Host) ->
    Proc = gen_mod:get_module_proc(Host, ejabberd_mod_http_client_sup),
    ChildSpec =
        {Proc,
         {mod_http_client_sup, start_link, [Proc]},
         permanent,
         infinity,
         supervisor,
         [mod_http_client_sup]},
    {ok, _} = supervisor:start_child(ejabberd_sup, ChildSpec).

stop_supervisor(Host) ->
    Proc = gen_mod:get_module_proc(Host, ejabberd_mod_http_client_sup),
    ok = supervisor:terminate_child(ejabberd_sup, Proc),
    ok = supervisor:delete_child(ejabberd_sup, Proc).
