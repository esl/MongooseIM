%%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
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
-include("mongoose.hrl").

%% API
-export([start_link/1]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% gen_mod API
-export([deps/2, start/2, stop/1]).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

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
    ?DEBUG("refresher starting with interval ~p~n", [RefreshInterval]),
    schedule_refresh(RefreshInterval),
    {ok, RefreshInterval}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(refresh, Interval) ->
    refresh(),
    schedule_refresh(Interval),
    {noreply, Interval}.

terminate(Reason, _State) ->
    ?ERROR_MSG("mod_global_distrib_refresher terminated with reason: ~p", [Reason]),
    ok.


%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------


-spec deps(Opts :: proplists:proplist()) -> gen_mod:deps_list().
deps(Opts) ->
    [{mod_global_distrib_mapping, Opts, hard}].

-spec start() -> any().
start() ->
    start_outgoing_conns_sup(),
    Interval = mod_global_distrib_utils:opt(?MODULE, hosts_refresh_interval, default_interval()),
    Child = #{
      id => mod_global_distrib_hosts_refresher,
      start => {?MODULE, start_link, [Interval]},
      restart => transient,
      shutdown => 5000,
      modules => [mod_global_distrib_outgoing_conns_sup]
    },
    {ok, _} = supervisor:start_child(mod_global_distrib_outgoing_conns_sup, Child),
    ok.

-spec stop() -> any().
stop() ->
    stop_outgoing_conns_sup(),
    ok.

refresh() ->
    ?DEBUG("Refreshing hosts, checking if there exists a supervisor for all of them~n", []),
    Hosts = mod_global_distrib_mapping:hosts(),
    ?DEBUG("Discovered hosts: ~p~n", [Hosts]),
    lists:map(fun maybe_add_host/1, Hosts),
    ok.

schedule_refresh(Interval) ->
  erlang:send_after(Interval, self(), refresh).

maybe_add_host(Host) ->
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
    mod_global_distrib_utils:opt(?MODULE, local_host).

start_outgoing_conns_sup() ->
  ConnsSup = mod_global_distrib_outgoing_conns_sup,
    ChildSpec = #{
      id => ConnsSup,
      start => {ConnsSup, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => supervisor,
      modules => [ConnsSup]
     },
  supervisor:start_child(ejabberd_sup, ChildSpec),
  ok.


stop_outgoing_conns_sup() ->
  ConnsSup = mod_global_distrib_outgoing_conns_sup,
  supervisor:terminate_child(ejabberd_sup, ConnsSup),
  supervisor:delete_child(ejabberd_sup, ConnsSup).
