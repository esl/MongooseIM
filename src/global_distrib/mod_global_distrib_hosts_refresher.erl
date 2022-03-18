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

-ignore_xref([pause/0, start_link/1, unpause/0]).

-record(state, {local_host :: binary(),
                refresh_interval :: pos_integer(),
                tref :: reference() | undefined}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link(gen_mod:module_opts()) -> pid() | term().
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-spec pause() -> ok.
pause() ->
    gen_server:call(?MODULE, pause).

-spec unpause() -> ok.
unpause() ->
    gen_server:call(?MODULE, unpause).

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(_HostType, Opts) ->
    start_outgoing_conns_sup(),
    Child = #{
      id => ?MODULE,
      start => {?MODULE, start_link, [Opts]},
      restart => transient,
      shutdown => 5000,
      modules => [?MODULE]
    },
    {ok, _} = supervisor:start_child(mod_global_distrib_outgoing_conns_sup, Child),
    ok.

-spec stop(mongooseim:host_type()) -> any().
stop(_HostType) ->
    stop_outgoing_conns_sup(),
    ok.

-spec deps(mongooseim:host_type(), gen_mod:module_opts()) -> gen_mod_deps:deps().
deps(_HostType, Opts) ->
    [{mod_global_distrib_utils, Opts, hard},
     {mod_global_distrib_mapping, Opts, hard}].

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init(#{local_host := LocalHost, hosts_refresh_interval := RefreshInterval}) ->
    ?LOG_DEBUG(#{what => gd_refresher_started, refresh_interval => RefreshInterval}),
    NState = schedule_refresh(#state{local_host = LocalHost,
                                     refresh_interval = RefreshInterval}),
    {ok, NState}.

handle_call(pause, _From, State = #state{tref = undefined}) ->
    ?LOG_ERROR(#{what => gd_refresher_already_paused}),
    {reply, ok, State};
handle_call(pause, _From, State) ->
    erlang:cancel_timer(State#state.tref),
    {reply, ok, State#state{ tref = undefined }};
handle_call(unpause, _From, State = #state{tref = undefined}) ->
    {reply, ok, schedule_refresh(State)};
handle_call(unpause, _From, State = #state{tref = TRef}) ->
    ?LOG_ERROR(#{what => gd_refresher_already_running, timer_ref => TRef,
                 text => <<"GD Refresher received unpause, when already unpaused. Ignore.">>}),
    {reply, ok, State};
handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, {error, unknown_request}, State}.

handle_cast(Request, State) ->
    ?UNEXPECTED_CAST(Request),
    {noreply, State}.

handle_info({timeout, TRef, refresh}, #state{local_host = LocalHost, tref = TRef} = State) ->
    refresh(LocalHost),
    NState = schedule_refresh(State),
    {noreply, NState, hibernate};
handle_info({timeout, _, refresh}, State) ->
    %% We got refresh signal from outdated timer
    {noreply, State, hibernate};
handle_info(Msg, State) ->
    ?UNEXPECTED_INFO(Msg),
    {noreply, State, hibernate}.

terminate(Reason, _State) ->
    ?LOG_INFO(#{what => gd_refresher_stopped, reason => Reason,
                text => <<"mod_global_distrib_refresher has terminated">>}).

code_change(_, State, _) -> {ok, State}.

%%--------------------------------------------------------------------
%% Helper functions
%%--------------------------------------------------------------------

refresh(LocalHost) ->
    Hosts = mod_global_distrib_mapping:hosts(),
    ?LOG_DEBUG(#{what => gd_refresher_fetched_hosts,
                 hosts => Hosts, local_host => LocalHost}),
    lists:foreach(fun mod_global_distrib_outgoing_conns_sup:ensure_server_started/1,
                  lists:delete(LocalHost, Hosts)).

schedule_refresh(#state{ refresh_interval = Interval } = State) ->
    TRef = erlang:start_timer(Interval, self(), refresh),
    State#state{ tref = TRef }.

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
