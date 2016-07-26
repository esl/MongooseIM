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
-behaviour(gen_server).

%% API
-export([start_pool/3, stop_pool/2, get_pool/2, make_request/5]).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% gen_server callbacks
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

%% other exports
-export([start_link/1]).

-record(pool, {name :: atom(),
               host :: ejabberd:server(),
               http_host :: string(),
               size :: pos_integer(),
               path_prefix :: binary(),
               pool_timeout :: pos_integer(),
               request_timeout :: pos_integer()}).

-record(state, {pools :: dict:dict(atom(), #pool{})}).

-opaque pool() :: #pool{}.

-export_type([pool/0]).

%%------------------------------------------------------------------------------
%% API

-spec start_pool(ejabberd:server(), atom(), list()) -> ok.
start_pool(Host, Name, Opts) ->
    Proc = gen_mod:get_module_proc(Host, mod_http_client),
    gen_server:call(Proc, {start_pool, Host, Name, Opts}).

-spec get_pool(ejabberd:server(), atom()) -> pool().
get_pool(Host, Name) ->
    Proc = gen_mod:get_module_proc(Host, mod_http_client),
    gen_server:call(Proc, {get_pool, Name}).

-spec stop_pool(ejabberd:server(), atom()) -> ok.
stop_pool(Host, Name) ->
    Proc = gen_mod:get_module_proc(Host, mod_http_client),
    gen_server:call(Proc, {stop_pool, Host, Name}).

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
            {error, poolbusy};
        {ok, {{Code, _Reason}, _RespHeaders, RespBody, _, _}} ->
            {ok, {Code, RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% gen_mod callbacks

-spec start(ejabberd:server(), list()) -> any().
start(Host, Opts) ->
    {ok, SupProc} = start_supervisor(Host),
    Proc = gen_mod:get_module_proc(Host, mod_http_client),
    ChildSpec = {Proc,
                 {?MODULE, start_link, [Host]},
                 permanent,
                 1000,
                 worker,
                 [?MODULE]},
    {ok, _} = supervisor:start_child(SupProc, ChildSpec),
    PoolSpec = gen_mod:get_opt(pools, Opts, []),
    [start_pool(Host, Name, PoolOpts) || {Name, PoolOpts} <- PoolSpec],
    ok.

-spec stop(ejabberd:server()) -> any().
stop(Host) ->
    stop_supervisor(Host).

%%------------------------------------------------------------------------------
%% gen_server callbacks

-spec init(any()) -> {ok, #state{}}.
init(_) ->
    {ok, #state{pools = dict:new()}}.

-spec handle_call(tuple(), {pid(), any()}, #state{}) -> {reply, ok | pool(), #state{}}.
handle_call({start_pool, Host, Name, Opts}, _From, State = #state{pools = Pools}) ->
    Pool = make_pool(Host, Name, Opts),
    do_start_pool(Pool),
    {reply, ok, State#state{pools = dict:store(Name, Pool, Pools)}};
handle_call({get_pool, Name}, _From, State = #state{pools = Pools}) ->
    {reply, dict:fetch(Name, Pools), State};
handle_call({stop_pool, Host, Name}, _From, State = #state{pools = Pools}) ->
    SupProc = gen_mod:get_module_proc(Host, ejabberd_mod_http_client_sup),
    ok = supervisor:terminate_child(SupProc, Name),
    ok = supervisor:delete_child(SupProc, Name),
    {reply, ok, State#state{pools = dict:erase(Name, Pools)}}.

-spec handle_cast(any(), #state{}) -> no_return().
handle_cast(_Request, _State) ->
    erlang:error(undef).

-spec handle_info(any(), #state{}) -> no_return().
handle_info(_Info, _State) ->
    erlang:error(undef).

-spec code_change(any(), #state{}, any()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

-spec terminate(any(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% other exports

-spec start_link(ejabberd:server()) -> {ok, pid()} | {error, any()}.
start_link(Host) ->
    Proc = gen_mod:get_module_proc(Host, mod_http_client),
    gen_server:start_link({local, Proc}, ?MODULE, Host, []).

%%------------------------------------------------------------------------------
%% internal functions

make_pool(Host, Name, Opts) ->
    #pool{name = Name,
          host = Host,
          http_host = gen_mod:get_opt(host, Opts, "http://localhost"),
          size = gen_mod:get_opt(pool_size, Opts, 5),
          path_prefix = list_to_binary(gen_mod:get_opt(path_prefix, Opts, "")),
          pool_timeout = gen_mod:get_opt(pool_timeout, Opts, 200),
          request_timeout = gen_mod:get_opt(request_timeout, Opts, 5000)}.

do_start_pool(#pool{name = Name,
                    host = Host,
                    http_host = HttpHost,
                    size = PoolSize}) ->
    ProcName = pool_proc_name(Host, Name),
    PoolOpts = [{name, {local, ProcName}},
                {size, PoolSize},
                {max_overflow, 5},
                {worker_module, mod_http_client_worker}],
    SupProc = gen_mod:get_module_proc(Host, ejabberd_mod_http_client_sup),
    {ok, _} = supervisor:start_child(
                SupProc,
                poolboy:child_spec(ProcName, PoolOpts, [HttpHost, []])),
    ok.

pool_proc_name(Host, PoolName) ->
    gen_mod:get_module_proc(Host, PoolName).

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
