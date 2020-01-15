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

-behaviour(gen_server).
-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

-include("mongoose.hrl").

%% API
-export([start_link/1]).

%% gen_server API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% gen_mod API
-export([deps/2, start/2, stop/1]).

%% Test & debug API
-export([pause/0, unpause/0]).

-record(state, {
          refresh_interval :: pos_integer(),
          tref :: reference() | undefined
         }).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link(Milliseconds :: non_neg_integer()) -> pid() | term().
start_link(RefreshInterval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [RefreshInterval], []).

-spec pause() -> ok.
pause() ->
    gen_server:call(?MODULE, pause).

-spec unpause() -> ok.
unpause() ->
    gen_server:call(?MODULE, unpause).

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------

-spec start(Host :: ejabberd:lserver(), Opts :: proplists:proplist()) -> any().
start(Host, Opts0) ->
    Opts = [{hosts_refresh_interval, default_refresh_interval()} | Opts0],
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
    NState = schedule_refresh(#state{ refresh_interval = RefreshInterval }),
    {ok, NState}.

handle_call(pause, _From, State = #state{tref = undefined}) ->
    ?ERROR_MSG("event=already_paused", []),
    {reply, ok, State};
handle_call(pause, _From, State) ->
    erlang:cancel_timer(State#state.tref),
    {reply, ok, State#state{ tref = undefined }};
handle_call(unpause, _From, State = #state{tref = undefined}) ->
    {reply, ok, schedule_refresh(State)};
handle_call(unpause, _From, State = #state{tref = TRef}) ->
    ?ERROR_MSG("event=not_paused, timer_ref=~p", [TRef]),
    {reply, ok, State};
handle_call(Request, From, State) ->
    ?ERROR_MSG("issue=unknown_call request=~p from=~p", [Request, From]),
    {reply, {error, unknown_request}, State}.

handle_cast(Request, State) ->
    ?ERROR_MSG("issue=unknown_cast request=~p", [Request]),
    {noreply, State}.

handle_info({timeout, TRef, refresh}, #state{ tref = TRef } = State) ->
    refresh(),
    NState = schedule_refresh(State),
    {noreply, NState, hibernate};
handle_info({timeout, _, refresh}, State) ->
    %% We got refresh signal from outdated timer
    {noreply, State, hibernate};
handle_info(Msg, _State) ->
    ?WARNING_MSG("Unknown message: ~p", Msg).

terminate(Reason, _State) ->
    ?INFO_MSG("mod_global_distrib_refresher has terminated with reason: ~p", [Reason]).

code_change(_, State, _) -> {ok, State}.

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------


-spec deps(Opts :: proplists:proplist()) -> gen_mod:deps_list().
deps(Opts) ->
    [{mod_global_distrib_mapping, Opts, hard}].

-spec start() -> any().
start() ->
    start_outgoing_conns_sup(),
    Interval = mod_global_distrib_utils:opt(?MODULE, hosts_refresh_interval),
    Child = #{
      id => ?MODULE,
      start => {?MODULE, start_link, [Interval]},
      restart => transient,
      shutdown => 5000,
      modules => [?MODULE]
    },
    {ok, _} = supervisor:start_child(mod_global_distrib_outgoing_conns_sup, Child),
    ok.

-spec stop() -> any().
stop() ->
    stop_outgoing_conns_sup(),
    ok.

refresh() ->
    Hosts = mod_global_distrib_mapping:hosts(),
    LocalHost = local_host(),
    ?DEBUG("event=fetched_hosts,hosts='~p',local_host='~s'", [Hosts, LocalHost]),
    lists:foreach(fun mod_global_distrib_outgoing_conns_sup:ensure_server_started/1,
                  lists:delete(LocalHost, Hosts)).

schedule_refresh(#state{ refresh_interval = Interval } = State) ->
    TRef = erlang:start_timer(Interval, self(), refresh),
    State#state{ tref = TRef }.

default_refresh_interval() ->
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
    ejabberd_sup:start_child(ChildSpec).


stop_outgoing_conns_sup() ->
    ConnsSup = mod_global_distrib_outgoing_conns_sup,
    ejabberd_sup:stop_child(ConnsSup).
