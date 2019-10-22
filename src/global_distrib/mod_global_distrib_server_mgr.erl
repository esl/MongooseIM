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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Debug
-export([get_enabled_endpoints/1, get_disabled_endpoints/1]).

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
          last_endpoints :: [endpoint()] | undefined
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

    RefreshInterval = mod_global_distrib_utils:opt(mod_global_distrib_sender,
                                                   endpoint_refresh_interval),
    DisRefreshInterval = mod_global_distrib_utils:opt(mod_global_distrib_sender,
                                                      endpoint_refresh_interval_when_empty),
    GCInterval = mod_global_distrib_utils:opt(mod_global_distrib_sender, disabled_gc_interval),
    State = #state{
               server = Server,
               supervisor = Supervisor,
               enabled = [],
               disabled = [],
               pending_endpoints = [],
               pending_gets = queue:new(),
               refresh_interval = RefreshInterval,
               refresh_interval_when_disconnected = DisRefreshInterval,
               gc_interval = GCInterval
              },

    State2 = refresh_connections(State),
    schedule_refresh(State2),
    schedule_gc(State2),

    ?INFO_MSG("event=mgr_started,pid='~p',server='~p',supervisor='~p',refresh_interval=~p",
              [self(), Server, Supervisor, RefreshInterval]),
    {ok, State2}.

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
    ?WARNING_MSG("event=mgr_timeout, caller_pid=~p, caller_msg=~p state_info=~1000p",
                 [FromPid, Msg, state_info(State)]),
    {noreply, State};
handle_cast(Msg, State) ->
    ?WARNING_MSG("event=unknown_msg,msg='~p'", [Msg]),
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
            ?INFO_MSG("event=gd_mgr_refresh refresh_interval=~p "
                      "pending_endpoints_before=~p pending_endpoints_after=~p",
                      [State#state.refresh_interval,
                       State#state.pending_endpoints, State2#state.pending_endpoints])
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
            ?INFO_MSG("event=gd_mgr_disabled_gc stopped_endpoints=~p gc_interval=~p",
                      [StoppedEndpoints, State#state.gc_interval])
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
            ?INFO_MSG("event=endpoint_enabled,endpoint='~p',server='~s'",
                      [Endpoint, State#state.server]),
            NState0;
        Error ->
            ?ERROR_MSG("event=cannot_enable_endpoint,endpoint='~p',server='~s',error='~p'",
                       [Endpoint, State#state.server, Error]),
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
            ?INFO_MSG("event=endpoint_disabled,endpoint='~p',server='~s'",
                      [Endpoint, State#state.server]),
            NState0;
        Error ->
            ?ERROR_MSG("event=cannot_disable_endpoint,endpoint='~p',server='~s',error='~p'",
                       [Endpoint, State#state.server, Error]),
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

    case Reason of
        ItsFine when ItsFine == normal; ItsFine == shutdown ->
            ?INFO_MSG("event=endpoint_closed,endpoint='~p',type=~p,reason='normal'",
                      [Endpoint, Type]);
        _Other ->
            ?ERROR_MSG("event=endpoint_closed,endpoint='~p',type=~p,reason='~p'",
                       [Endpoint, Type, Reason])
    end,
    {noreply, NState};
handle_info(Msg, State) ->
    ?WARNING_MSG("event=unknown_msg,msg='~p'", [Msg]),
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
    ?DEBUG("event=refreshing_endpoints,server='~s'", [Server]),
    {ok, NewEndpoints} = get_endpoints(Server),
    case NewEndpoints of
        LastEndpoints ->
            nothing_new;
        _ ->
            ?INFO_MSG("event=endpoints_change old_endpoints=~p new_endpoints=~p",
                      [LastEndpoints, NewEndpoints])
    end,
    ?DEBUG("event=fetched_endpoints,server='~s',result='~p'", [Server, NewEndpoints]),

    NPendingEndpoints = resolve_pending(NewEndpoints, State#state.enabled),
    log_endpoints_changes(Server, NPendingEndpoints),

    case PendingEndpoints of
        [] -> maybe_schedule_process_endpoint(NPendingEndpoints);
        _ -> already_scheduled
    end,

    FinalPendingEndpoints = PendingEndpoints ++ NPendingEndpoints,

    case FinalPendingEndpoints of
        [] ->
            no_log;
        _ ->
            ?DEBUG("event=endpoints_update_scheduled,server='~s',new_changes=~p,pending_changes=~p",
                   [Server, length(NPendingEndpoints), length(FinalPendingEndpoints)])
    end,
    State#state{ pending_endpoints = FinalPendingEndpoints, last_endpoints = NewEndpoints }.

-spec get_endpoints(Server :: jid:lserver()) -> {ok, [mod_global_distrib_utils:endpoint()]}.
get_endpoints(Server) ->
    {ok, EndpointsToResolve} =
    case ejabberd_config:get_local_option({global_distrib_addr, Server}) of
        undefined -> mod_global_distrib_mapping:endpoints(Server);
        Endpoints -> {ok, Endpoints}
    end,
    Resolved = mod_global_distrib_utils:resolve_endpoints(EndpointsToResolve),
    {ok, Resolved}.

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
                            EndpointsChanges :: endpoints_changes()) -> any().
log_endpoints_changes(Server, []) ->
    ?DEBUG("event=endpoints_changes,server='~s',to_enable='[]',to_disable='[]'", [Server]);
log_endpoints_changes(Server, EndpointsChanges) ->
    ?INFO_MSG("event=endpoints_changes,server='~s',to_enable='~p',to_disable='~p'",
              [Server, [ E || {enable, E} <- EndpointsChanges ],
                       [ E || {disable, E} <- EndpointsChanges ]]).

-spec enable(Endpoint :: endpoint(), State :: state()) -> {ok, state()} | {error, any()}.
enable(Endpoint, #state{ disabled = Disabled, supervisor = Supervisor,
                         enabled = Enabled, server = Server } = State) ->
    case lists:keytake(Endpoint, #endpoint_info.endpoint, Disabled) of
        false ->
            case catch mod_global_distrib_server_sup:start_pool(Supervisor, Endpoint, Server) of
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
            ?ERROR_MSG("event=cannot_close_disabled_connection,endpoint='~p',error='~p'",
                       [Endpoint, Error])
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
    [{server, Server},
     {supervisor, Supervisor},
     {enabled, Enabled},
     {disabled, Disabled},
     {pending_endpoints, PendingEndpoints},
     {pending_gets, PendingGets},
     {refresh_interval, RefreshInterval},
     {refresh_interval_when_disconnected, DisRefreshInterval},
     {gc_interval, GCInterval},
     {pending_endpoints_listeners, PendingEndpointsListeners},
     {last_endpoints, LastEndpoints}].

