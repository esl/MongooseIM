%%%==============================================================================
%% Copyright 2017 Erlang Solutions Ltd.
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


-module(mod_global_distrib_hosts_refresher).
-author("dominik.stanaszek@erlang-solutions.com").

-behavior(gen_server).
-behavior(gen_mod).
-include("ejabberd.hrl").

%% API
-export([start_link/0]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/3]).

%% gen_mod API
-export([deps/2, start/2, stop/1]).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link() -> pid() | term().
start_link() ->
    start_link(default_interval()).

-spec start_link(Milliseconds :: non_neg_integer()) -> pid() | term().
start_link(RefreshInterval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [RefreshInterval], []).


%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------

-spec start(Host :: ejabberd:lserver(), Opts :: proplists:proplist()) -> any().
start(Host, Opts) ->
    ?DEBUG("Opts in refresher: ~p~n", [Opts]),
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

-spec stop(Host :: ejabberd:lserver()) -> any().
stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

-spec deps(Host :: ejabberd:server(), Opts :: proplists:proplist()) -> gen_mod:deps_list().
deps(Host, Opts) ->
    mod_global_distrib_utils:deps(?MODULE, Host, Opts, fun deps/1).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([RefreshInterval]) ->
    self() ! refresh,
    {ok, [{refresh_interval, RefreshInterval}]}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(refresh, [{refresh_interval, Int}] = State) ->
    ?DEBUG("Refreshing hosts, checking if there exists a supervisor for all of them~n", []),
    Hosts = mod_global_distrib_mapping:hosts(),
    ?DEBUG("Discovered hosts: ~p~n", [Hosts]),
    lists:map(fun maybe_add/1, Hosts),
    timer:send_after(Int, self(), refresh),
    {noreply, State}.

terminate(Reason, _State, _Data) ->
    ?DEBUG("REFRESHER has shutdown with reason ~p", [Reason]),
    ok.


%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

-spec deps(Opts :: proplists:proplist()) -> gen_mod:deps_list().
deps(Opts) ->
    [{mod_global_distrib_mapping, Opts, hard}].

-spec start() -> any().
start() ->
    ok.

-spec stop() -> any().
stop() ->
    ok.

maybe_add(Host) ->
    ?DEBUG("Checking host ~p (on host ~p)~n", [Host, local_host()]),
    case local_host() of
        Host ->
            ok;
        _ ->
            mod_global_distrib_outgoing_conns_sup:ensure_server_started(Host)
    end.

default_interval() ->
    3000.

local_host() ->
    [{local_host, Host}] = ets:lookup(mod_global_distrib, local_host),
    Host.
