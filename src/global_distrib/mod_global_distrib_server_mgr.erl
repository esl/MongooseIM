%%==============================================================================
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
-module(mod_global_distrib_server_mgr).
-author('piotr.nosek@erlang-solutions.com').

-include("mongoose.hrl").

-behaviour(gen_server).

-export([start_link/2]).
-export([get_connection/1, ping_proc/1, get_state_info/1]).
-export([force_refresh/1, close_disabled/1]).
-export([init/1, handle_continue/2, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% Debug
-export([get_enabled_endpoints/1, get_disabled_endpoints/1]).

-ignore_xref([close_disabled/1, force_refresh/1, get_disabled_endpoints/1,
              get_enabled_endpoints/1, get_state_info/1, start_link/2]).

-type endpoint() :: mod_global_distrib_utils:endpoint().
-type endpoint_pid_tuple() :: {endpoint(), pid()}.

-record(endpoint_info, {
          endpoint :: endpoint(),
          conn_pool_ref :: atom(),
          conn_pool_pid :: pid(),
          monitor_ref :: reference()
         }).

-type endpoint_info() :: #endpoint_info{}.
-type endpoints_changes() :: [{enable | disable, endpoint()}].

-record(state, {
          server :: jid:lserver(),
          supervisor :: pid(),
          enabled :: [endpoint_info()],
          disabled :: [endpoint_info()],
          pending_endpoints :: endpoints_changes(),
          pending_gets :: queue:queue(tuple()),
          refresh_interval :: pos_integer(),
          refresh_interval_when_disconnected :: pos_integer(),
          gc_interval :: pos_integer(),
          %% Used by force_refresh to block until refresh is fully done.
          %% Listeners are notified only once and then this list is cleared.
          pending_endpoints_listeners = [] :: [pid()],
          %% Containts last result of get_endpoints
          last_endpoints :: [endpoint()] | undefined,
          conn_opts :: map()
         }).

-type state() :: #state{}.
%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link(Server :: jid:lserver(), ServerSup :: pid()) -> {ok, pid()} | {error, any()}.
start_link(Server, ServerSup) when is_binary(Server) ->
    Name = mod_global_distrib_utils:server_to_mgr_name(Server),
    gen_server:start_link({local, Name}, ?MODULE, [Server, ServerSup], []).

%% Will return only when endpoints changes (if there are any) are fully applied.
%% If force refresh is already in progress, new concurrent calls won't trigger another refresh
%% but will return 'ok' when the current one is finished.
-spec force_refresh(Server :: jid:lserver()) -> ok.
force_refresh(Server) ->
    do_call(Server, force_refresh).

-spec close_disabled(Server :: jid:lserver()) -> ok.
close_disabled(Server) ->
    do_call(Server, close_disabled).

-spec get_connection(Server :: jid:lserver()) -> {ok, pid()} | no_return().
get_connection(Server) ->
    {ok, CPoolRef} = do_call(Server, get_connection_pool),
    {ok, cpool:get_connection(CPoolRef)}.

%% `ping_proc` instead of just `ping` to emphasize that this call does not ping
%% some remote server but the manager process instead
-spec ping_proc(Server :: jid:lserver()) -> pong | pang.
ping_proc(Server) ->
    case catch do_call(Server, ping_proc) of
        pong -> pong;
        _Error -> pang
    end.

-spec get_state_info(Server :: jid:lserver()) -> map().
get_state_info(Server) ->
    do_call(Server, get_state_info).

%%--------------------------------------------------------------------
%% Debug API
%%--------------------------------------------------------------------

-spec get_enabled_endpoints(Server :: jid:lserver()) -> [endpoint()].
get_enabled_endpoints(Server) ->
    do_call(Server, get_enabled_endpoints).

-spec get_disabled_endpoints(Server :: jid:lserver()) -> [endpoint()].
get_disabled_endpoints(Server) ->
    do_call(Server, get_disabled_endpoints).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init([Server, Supervisor]) ->
    process_flag(trap_exit, true),

    HostType = mod_global_distrib_utils:host_type(),
    ConnOpts = gen_mod:get_module_opt(HostType, mod_global_distrib_hosts_refresher, connections),
    #{endpoint_refresh_interval := RefreshInterval,
      endpoint_refresh_interval_when_empty := DisRefreshInterval,
      disabled_gc_interval := GCInterval} = ConnOpts,

    State = #state{
               server = Server,
               supervisor = Supervisor,
               enabled = [],
               disabled = [],
               pending_endpoints = [],
               pending_gets = queue:new(),
               refresh_interval = RefreshInterval,
               refresh_interval_when_disconnected = DisRefreshInterval,
               gc_interval = GCInterval,
               conn_opts = ConnOpts
              },

    ?LOG_INFO(ls(#{what => gd_mgr_started}, State)),
    {ok, State, {continue, initial_refresh}}.

handle_continue(initial_refresh, State) ->
    State2 = refresh_connections(State),
    schedule_refresh(State2),
    schedule_gc(State2),
    {noreply, State2}.

handle_call(get_connection_pool, From, #state{ enabled = [],
                                               pending_gets = PendingGets } = State) ->
    {noreply, State#state{ pending_gets = queue:in(From, PendingGets) }};
handle_call(get_connection_pool, _From, #state{ enabled = Enabled } = State) ->
    {reply, {ok, pick_connection_pool(Enabled)}, State};
handle_call(force_refresh, From, #state{ pending_endpoints_listeners = [] } = State) ->
    State2 = refresh_connections(State),
    case State2#state.pending_endpoints of
        [] -> {reply, ok, State2};
        _ -> {noreply, State2#state{ pending_endpoints_listeners = [From] }}
    end;
handle_call(force_refresh, From, #state{ pending_endpoints_listeners = Listeners } = State) ->
    {noreply, State#state{ pending_endpoints_listeners = [From | Listeners] }};
handle_call(close_disabled, _From, #state{ disabled = Disabled } = State) ->
    lists:foreach(
      fun(#endpoint_info{ endpoint = Endpoint }) ->
              stop_disabled(Endpoint, State)
      end, Disabled),
    {reply, ok, State};
handle_call(get_enabled_endpoints, _From, State) ->
    {reply, [ CI#endpoint_info.endpoint || CI <- State#state.enabled ], State};
handle_call(get_disabled_endpoints, _From, State) ->
    {reply, [ CI#endpoint_info.endpoint || CI <- State#state.disabled ], State};
handle_call(ping_proc, _From, State) ->
    {reply, pong, State};
handle_call(get_state_info, _From, State) ->
    {reply, state_info(State), State}.

handle_cast({call_timeout, FromPid, Msg}, State) ->
    ?LOG_WARNING(ls(#{what => gd_mgr_call_timeout,
                      caller_pid => FromPid, caller_msg => Msg}, State)),
    {noreply, State};
handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

handle_info(refresh, State) ->
    State2 = case State#state.pending_endpoints of
                 [] -> refresh_connections(State);
                 _ -> State % May occur if we are in the middle of force_refresh
             end,
    case State#state.pending_endpoints == State2#state.pending_endpoints of
        true ->
            ok;
        _ ->
            ?LOG_INFO(ls(#{what => gd_mgr_refresh,
                           text => <<"A list of pending_endpoints has changed in GD server manager">>,
                           pending_endpoints_before => State#state.pending_endpoints,
                           pending_endpoints_after => State2#state.pending_endpoints}, State))
    end,
    schedule_refresh(State2),
    {noreply, State2};
handle_info(disabled_gc, #state{ disabled = Disabled } = State) ->
    StoppedEndpoints = lists:flatmap(
      fun(#endpoint_info{ endpoint = Endpoint, conn_pool_ref = ConnPool }) ->
              try cpool:get_connection(ConnPool, no_wait_for_reconnect) of
                  _ConnPid ->
                      []
              catch error:no_connections ->
                      stop_disabled(Endpoint, State),
                      [Endpoint]
              end
      end, Disabled),
    case StoppedEndpoints of
        [] ->
            ok;
        _ ->
            ?LOG_INFO(ls(#{what => gd_mgr_disabled_gc,
                           text => <<"GD server manager stops some inactive endpoints">>,
                           stopped_endpoints => StoppedEndpoints}, State))
    end,
    schedule_gc(State),
    {noreply, State};
handle_info(process_pending_get, #state{ enabled = [] } = State) ->
    {noreply, State};
handle_info(process_pending_get, #state{ pending_gets = PendingGets,
                                         enabled = Enabled } = State) ->
    NState =
    case queue:out(PendingGets) of
        {{value, From}, NewPendingGets} ->
            CPoolRef = pick_connection_pool(Enabled),
            gen_server:reply(From, {ok, CPoolRef}),
            State#state{ pending_gets = NewPendingGets };
        {empty, _} ->
            State
    end,
    maybe_schedule_process_get(NState),
    {noreply, NState};
handle_info(process_pending_endpoint,
            #state{ pending_endpoints = [{enable, Endpoint} | RPendingEndpoints] } = State) ->
    State2 =
    case catch enable(Endpoint, State) of
        {ok, NState0} ->
            ?LOG_INFO(ls(#{what => gd_endpoint_enabled,
                           text => <<"GD server manager enables pending endpoint">>,
                           endpoint => Endpoint}, State)),
            NState0;
        Error ->
            ?LOG_ERROR(ls(#{what => gd_endpoint_enabling_failed,
                            text => <<"GD server manager cannot enable endpoint">>,
                            endpoint => Endpoint, reason => Error}, State)),
            State
    end,

    maybe_schedule_process_get(State2),
    maybe_schedule_process_endpoint(RPendingEndpoints),
    State3 = State2#state{ pending_endpoints = RPendingEndpoints },
    State4 = maybe_notify_endpoints_listeners(State3),
    {noreply, State4};
handle_info(process_pending_endpoint,
            #state{ pending_endpoints = [{disable, Endpoint} | RPendingEndpoints] } = State) ->
    State2 =
    case catch disable(Endpoint, State) of
        {ok, NState0} ->
            ?LOG_INFO(ls(#{what => gd_endpoint_disabled,
                           text => <<"GD server manager disables pending endpoint">>,
                           endpoint => Endpoint}, State)),
            NState0;
        Error ->
            ?LOG_ERROR(ls(#{what => gd_endpoint_disabling_failed,
                            text => <<"GD server manager cannot disable endpoint">>,
                            endpoint => Endpoint, reason => Error}, State)),
            State
    end,

    maybe_schedule_process_endpoint(RPendingEndpoints),
    State3 = State2#state{ pending_endpoints = RPendingEndpoints },
    State4 = maybe_notify_endpoints_listeners(State3),
    {noreply, State4};
handle_info({'DOWN', MonitorRef, _Type, Pid, Reason}, #state{ enabled = Enabled,
                                                              disabled = Disabled } = State) ->
    {Endpoint, Type, NState} =
    case lists:keytake(MonitorRef, #endpoint_info.monitor_ref, Enabled) of
        {value, #endpoint_info{ endpoint = Endpoint0 }, NEnabled} ->
            {Endpoint0, enabled, State#state{ enabled = NEnabled }};
        false ->
            case lists:keytake(MonitorRef, #endpoint_info.monitor_ref, Disabled) of
                {value, #endpoint_info{ endpoint = Endpoint0 }, NDisabled} ->
                    {Endpoint0, disabled, State#state{ disabled = NDisabled }};
                false ->
                    {Pid, unknown, State}
            end
    end,

    Reason2 =
        case Reason of
            shutdown -> normal;
            _Other -> Reason
        end,
    ?LOG_INFO(ls(#{what => gd_endpoint_closed,
                   text => <<"Disconnected from a GD endpoint">>,
                   type => Type, endpoint => Endpoint, reason => Reason2}, State)),
    {noreply, NState};
handle_info(Msg, State) ->
    ?UNEXPECTED_INFO(Msg),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    %% TODO: Cleanup
    ok.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

-spec do_call(Server :: jid:lserver(), Msg :: any()) -> any().
do_call(Server, Msg) ->
    MgrName = mod_global_distrib_utils:server_to_mgr_name(Server),
    try
        gen_server:call(MgrName, Msg)
    catch exit:{timeout,_} = Reason:Stacktrace ->
        catch gen_server:cast(MgrName, {call_timeout, self(), Msg}),
        erlang:raise(exit, Reason, Stacktrace)
    end.

-spec schedule_refresh(State :: state()) -> state().
schedule_refresh(#state{ refresh_interval = Interval, last_endpoints = [_|_] } = State) ->
    do_schedule_refresh(State, Interval);
schedule_refresh(#state{ refresh_interval_when_disconnected = Interval } = State) ->
    %% Try more often by default when get_endpoints returns empty list
    do_schedule_refresh(State, Interval).

do_schedule_refresh(State, Interval) ->
    erlang:send_after(timer:seconds(Interval), self(), refresh),
    State.

-spec schedule_gc(State :: state()) -> state().
schedule_gc(#state{ gc_interval = Interval } = State) ->
    erlang:send_after(timer:seconds(Interval), self(), disabled_gc),
    State.

-spec maybe_schedule_process_endpoint(PendingEndpoints :: list()) -> any().
maybe_schedule_process_endpoint([]) ->
    nop;
maybe_schedule_process_endpoint(_) ->
    self() ! process_pending_endpoint.

-spec maybe_schedule_process_get(state()) -> any().
maybe_schedule_process_get(#state{ pending_gets = PendingGets, enabled = Enabled }) ->
    case PendingGets == queue:new() of
        true -> nop;
        false ->
            Enabled =/= [] orelse error(enabled_is_empty),
            self() ! process_pending_get
    end.

-spec maybe_notify_endpoints_listeners(state()) -> state().
maybe_notify_endpoints_listeners(#state{ pending_endpoints = [],
                                         pending_endpoints_listeners = Listeners } = State) ->
    lists:foreach(fun(Listener) -> gen_server:reply(Listener, ok) end, Listeners),
    State#state{ pending_endpoints_listeners = [] };
maybe_notify_endpoints_listeners(State) ->
    State.

-spec pick_connection_pool(Enabled :: [endpoint_info()]) -> pid() | no_connections.
pick_connection_pool(Enabled) ->
    #endpoint_info{ conn_pool_ref = PoolRef } = lists:nth(rand:uniform(length(Enabled)), Enabled),
    PoolRef.

-spec refresh_connections(State :: state()) -> state().
refresh_connections(#state{ server = Server, pending_endpoints = PendingEndpoints,
                            last_endpoints = LastEndpoints } = State) ->
    ?LOG_DEBUG(ls(#{what => gd_refreshing_endpoints}, State)),
    NewEndpoints = get_endpoints(Server),
    case NewEndpoints of
        LastEndpoints ->
            nothing_new;
        _ ->
            ?LOG_INFO(ls(#{what => gd_endpoints_change,
                           old_endpoints => LastEndpoints,
                           new_endpoints => NewEndpoints}, State))
    end,
    ?LOG_DEBUG(ls(#{what => gd_fetched_endpoints, fetched_endpoints => NewEndpoints}, State)),

    NPendingEndpoints = resolve_pending(NewEndpoints, State#state.enabled),
    log_endpoints_changes(Server, NPendingEndpoints, State),

    case PendingEndpoints of
        [] -> maybe_schedule_process_endpoint(NPendingEndpoints);
        _ -> already_scheduled
    end,

    FinalPendingEndpoints = PendingEndpoints ++ NPendingEndpoints,

    case FinalPendingEndpoints of
        [] ->
            no_log;
        _ ->
            ?LOG_DEBUG(ls(#{what => gd_endpoints_update_scheduled,
                            new_changes => NPendingEndpoints,
                            pending_changes => FinalPendingEndpoints,
                            new_changes_length => length(NPendingEndpoints),
                            pending_changes_length => length(FinalPendingEndpoints)},
                          State))
    end,
    State#state{ pending_endpoints = FinalPendingEndpoints, last_endpoints = NewEndpoints }.

-spec get_endpoints(Server :: jid:lserver()) -> [mod_global_distrib_utils:endpoint()].
get_endpoints(Server) ->
    EndpointsToResolve = mod_global_distrib_mapping:endpoints(Server),
    mod_global_distrib_utils:resolve_endpoints(EndpointsToResolve).

-spec resolve_pending(NewEndpointList :: [mod_global_distrib_utils:endpoint()],
                      OldEnabled :: [endpoint_pid_tuple()]) ->
    endpoints_changes().
resolve_pending([], []) ->
    [];
resolve_pending([], [#endpoint_info{ endpoint = ToDisable } | RToDisable]) ->
    [{disable, ToDisable} | resolve_pending([], RToDisable)];
resolve_pending([MaybeToEnable | RNewEndpoints], OldEnabled) ->
    case lists:keytake(MaybeToEnable, #endpoint_info.endpoint, OldEnabled) of
        false -> [{enable, MaybeToEnable} | resolve_pending(RNewEndpoints, OldEnabled)];
        {value, _, NOldEnabled} -> resolve_pending(RNewEndpoints, NOldEnabled)
    end.

-spec log_endpoints_changes(Server :: jid:lserver(),
                            EndpointsChanges :: endpoints_changes(), term()) -> any().
log_endpoints_changes(Server, [], State) ->
    ?LOG_DEBUG(ls(#{what => gd_same_endpoints, server => Server,
                    text => <<"No endpoint changes">>}, State));
log_endpoints_changes(Server, EndpointsChanges, State) ->
    ?LOG_INFO(ls(#{what => gd_endpoints_changes, server => Server,
                   to_enable => [ E || {enable, E} <- EndpointsChanges ],
                   to_disable => [ E || {disable, E} <- EndpointsChanges ]}, State)).

-spec enable(Endpoint :: endpoint(), State :: state()) -> {ok, state()} | {error, any()}.
enable(Endpoint, #state{ disabled = Disabled, supervisor = Supervisor,
                         enabled = Enabled, server = Server, conn_opts = ConnOpts } = State) ->
    case lists:keytake(Endpoint, #endpoint_info.endpoint, Disabled) of
        false ->
            case catch mod_global_distrib_server_sup:start_pool(Supervisor, Endpoint,
                                                                Server, ConnOpts) of
                {ok, ConnPoolRef, ConnPoolPid} ->
                    MonitorRef = monitor(process, ConnPoolPid),
                    EndpointInfo = #endpoint_info{
                                      endpoint = Endpoint,
                                      conn_pool_ref = ConnPoolRef,
                                      conn_pool_pid = ConnPoolPid,
                                      monitor_ref = MonitorRef
                                     },
                    {ok, State#state{ enabled = [EndpointInfo | Enabled] }};
                Error ->
                    {error, Error}
            end;
        {value, EndpointInfo, NewDisabled} ->
            {ok, State#state{ enabled = [EndpointInfo | Enabled], disabled = NewDisabled }}
    end.

-spec disable(Endpoint :: endpoint(), State :: state()) -> {ok, state()} | {error, any()}.
disable(Endpoint, #state{ disabled = Disabled, enabled = Enabled } = State) ->
    {value, EndpointInfo, NewEnabled} = lists:keytake(Endpoint, #endpoint_info.endpoint, Enabled),
    {ok, State#state{ enabled = NewEnabled, disabled = [EndpointInfo | Disabled] }}.

-spec stop_disabled(Endpoint :: endpoint(), State :: state()) -> any().
stop_disabled(Endpoint, State) ->
    case catch mod_global_distrib_server_sup:stop_pool(
                 State#state.supervisor, Endpoint) of
        ok ->
            ok;
        Error ->
            ?LOG_ERROR(ls(#{what => gd_cannot_close_disabled_connection,
                            reason => Error, endpoint => Endpoint},
                          State))
    end.

state_info(#state{
          server = Server,
          supervisor = Supervisor,
          enabled = Enabled,
          disabled = Disabled,
          pending_endpoints = PendingEndpoints,
          pending_gets = PendingGets,
          refresh_interval = RefreshInterval,
          refresh_interval_when_disconnected = DisRefreshInterval,
          gc_interval = GCInterval,
          pending_endpoints_listeners = PendingEndpointsListeners,
          last_endpoints = LastEndpoints
         }) ->
    #{server => Server,
      supervisor => Supervisor,
      enabled => Enabled,
      disabled => Disabled,
      pending_endpoints => PendingEndpoints,
      pending_gets => PendingGets,
      refresh_interval => RefreshInterval,
      refresh_interval_when_disconnected => DisRefreshInterval,
      gc_interval => GCInterval,
      pending_endpoints_listeners => PendingEndpointsListeners,
      last_endpoints => LastEndpoints}.


%% Log State
ls(LogMeta, State) ->
    maps:merge(state_info(State), LogMeta).
