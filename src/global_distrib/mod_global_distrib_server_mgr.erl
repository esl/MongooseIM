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
-export([get_connection/1]).
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

-record(state, {
          server :: jid:lserver(),
          supervisor :: pid(),
          enabled :: [endpoint_info()],
          disabled :: [endpoint_info()],
          pending_endpoints :: [{enable | disable, endpoint()}],
          pending_gets :: queue:queue(tuple()),
          refresh_interval :: pos_integer(),
          gc_interval :: pos_integer()
         }).

-type state() :: #state{}.

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link(Server :: jid:lserver(), ServerSup :: pid()) -> {ok, pid()} | {error, any()}.
start_link(Server, ServerSup) ->
    Name = mod_global_distrib_utils:server_to_mgr_name(Server),
    gen_server:start_link({local, Name}, ?MODULE, [Server, ServerSup], []).

-spec force_refresh(Server :: jid:lserver()) -> ok.
force_refresh(Server) ->
    do_call(Server, force_refresh).

-spec close_disabled(Server :: jid:lserver()) -> ok.
close_disabled(Server) ->
    do_call(Server, close_disabled).

-spec get_connection(Server :: jid:lserver()) -> pid() | {error, any()}.
get_connection(Server) ->
    do_call(Server, get_connection).

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
    GCInterval = mod_global_distrib_utils:opt(mod_global_distrib_sender, disabled_gc_interval),
    State = #state{
               server = Server,
               supervisor = Supervisor,
               enabled = [],
               disabled = [],
               pending_endpoints = [],
               pending_gets = queue:new(),
               refresh_interval = RefreshInterval,
               gc_interval = GCInterval
              },

    State2 = refresh_connections(State),
    schedule_refresh(State2),
    schedule_gc(State2),

    {ok, State2}.

handle_call(get_connection, From, #state{ enabled = [], pending_gets = PendingGets } = State) ->
    {noreply, State#state{ pending_gets = queue:in(From, PendingGets) }};
handle_call(get_connection, _From, #state{ enabled = Enabled } = State) ->
    Connection = pick_connection(Enabled),
    {reply, Connection, State};
handle_call(force_refresh, _From, State) ->
    {reply, ok, refresh_connections(State)};
handle_call(close_disabled, _From, #state{ disabled = Disabled } = State) ->
    lists:foreach(
      fun(#endpoint_info{ endpoint = Endpoint }) ->
              stop_disabled(Endpoint, State)
      end, Disabled),
    {reply, ok, State};
handle_call(get_enabled_endpoints, _From, State) ->
    {reply, [ CI#endpoint_info.endpoint || CI <- State#state.enabled ], State};
handle_call(get_disabled_endpoints, _From, State) ->
    {reply, [ CI#endpoint_info.endpoint || CI <- State#state.disabled ], State}.

handle_cast(Msg, State) ->
    ?WARNING_MSG("event=unknown_msg,msg='~p'", [Msg]),
    {noreply, State}.

handle_info(refresh, State) ->
    State2 = refresh_connections(State),
    schedule_refresh(State2),
    {noreply, State2};
handle_info(disabled_gc, #state{ disabled = Disabled } = State) ->
    lists:foreach(
      fun(#endpoint_info{ endpoint = Endpoint, conn_pool_ref = ConnPool }) ->
              case catch cpool:get_connection(ConnPool, no_wait_for_reconnect) of
                  {'EXIT', {no_connections, _}} -> stop_disabled(Endpoint, State);
                  _OK -> ok
              end
      end, Disabled),
    schedule_gc(State),
    {noreply, State};
handle_info(process_pending_get, #state{ enabled = [] } = State) ->
    {noreply, State};
handle_info(process_pending_get, #state{ pending_gets = PendingGets,
                                         enabled = Enabled } = State) ->
    NState =
    case queue:out(PendingGets) of
        {{value, From}, NewPendingGets} ->
            Connection = pick_connection(Enabled),
            gen_server:reply(From, Connection),

            NState0 = State#state{ pending_gets = NewPendingGets },
            maybe_schedule_process_get(NState0);
        {empty, _} ->
            State
    end,
    {noreply, NState};
handle_info(process_pending_endpoint,
            #state{ pending_endpoints = [{enable, Endpoint} | RPendingEndpoints] } = State) ->
    NState =
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

    maybe_schedule_process_get(NState),
    maybe_schedule_process_endpoint(RPendingEndpoints),
    {noreply, NState#state{ pending_endpoints = RPendingEndpoints }};
handle_info(process_pending_endpoint,
            #state{ pending_endpoints = [{disable, Endpoint} | RPendingEndpoints] } = State) ->
    NState =
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
    {noreply, NState#state{ pending_endpoints = RPendingEndpoints }};
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
    gen_server:call(MgrName, Msg).

-spec schedule_refresh(State :: state()) -> state().
schedule_refresh(#state{ refresh_interval = Interval } = State) ->
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

-spec pick_connection(Enabled :: [endpoint_info()]) -> pid().
pick_connection(Enabled) ->
    #endpoint_info{ conn_pool_ref = PoolRef } = lists:nth(rand:uniform(length(Enabled)), Enabled),
    cpool:get_connection(PoolRef).

-spec refresh_connections(State :: state()) -> state().
refresh_connections(#state{ server = Server, pending_endpoints = PendingEndpoints } = State) ->
    ?INFO_MSG("event=refreshing_endpoints,server='~s'", [Server]),
    {ok, NewEndpoints} = get_endpoints(Server),

    NPendingEndpoints = resolve_pending(NewEndpoints, State#state.enabled),

    case PendingEndpoints of
        [] -> maybe_schedule_process_endpoint(NPendingEndpoints);
        _ -> already_scheduled
    end,

    FinalPendingEndpoints = PendingEndpoints ++ NPendingEndpoints,

    ?INFO_MSG("event=endpoints_update_scheduled,server='~s',new_changes=~p,pending_changes=~p",
              [Server, length(NPendingEndpoints), length(FinalPendingEndpoints)]),
    State#state{ pending_endpoints = FinalPendingEndpoints }.

-spec get_endpoints(Server :: jid:lserver()) -> {ok, [mod_global_distrib_utils:endpoint()]}.
get_endpoints(Server) ->
    {ok, EndpointsToResolve} =
    case ejabberd_config:get_local_option({global_distrib_addr, Server}) of
        undefined -> mod_global_distrib_mapping:endpoints(Server);
        Endpoints -> {ok, Endpoints}
    end,
    {ok, mod_global_distrib_utils:resolve_endpoints(EndpointsToResolve)}.

-spec resolve_pending(NewEndpointList :: [mod_global_distrib_utils:endpoint()],
                      OldEnabled :: [endpoint_pid_tuple()]) ->
    [{enable | disable, mod_global_distrib_utils:endpoint()}].
resolve_pending([], []) ->
    [];
resolve_pending([], [#endpoint_info{ endpoint = ToDisable } | RToDisable]) ->
    [{disable, ToDisable} | resolve_pending([], RToDisable)];
resolve_pending([MaybeToEnable | RNewEndpoints], OldEnabled) ->
    case lists:keytake(MaybeToEnable, #endpoint_info.endpoint, OldEnabled) of
        false -> [{enable, MaybeToEnable} | resolve_pending(RNewEndpoints, OldEnabled)];
        {value, _, NOldEnabled} -> resolve_pending(RNewEndpoints, NOldEnabled)
    end.

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
