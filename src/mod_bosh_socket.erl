-module(mod_bosh_socket).

-behaviour(gen_fsm_compat).
-behaviour(mongoose_xmpp_socket).

%% API
-export([start/5,
         start_link/5,
         start_supervisor/0,
         is_supervisor_started/0,
         stop_supervisor/0,
         handle_request/2,
         pause/2]).

%% Private API
-export([get_handlers/1,
         get_pending/1,
         get_client_acks/1,
         set_client_acks/2,
         get_cached_responses/1]).

%% mongoose_xmpp_socket callbacks
-export([peername/1,
         tcp_to_tls/3,
         handle_data/2,
         activate/1,
         send_xml/2,
         close/1,
         get_peer_certificate/1,
         has_peer_cert/2,
         is_channel_binding_supported/1,
         export_key_materials/5,
         is_ssl/1]).

%% gen_fsm callbacks
-export([init/1,
         accumulate/2, accumulate/3,
         normal/2, normal/3,
         closing/2, closing/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-ignore_xref([{mod_bosh_backend, delete_session, 1},
              accumulate/2, accumulate/3, closing/2,
              closing/3, get_cached_responses/1,
              get_client_acks/1, get_handlers/1, get_peer_certificate/1,
              get_pending/1, normal/2, normal/3,
              pause/2, set_client_acks/2, start_link/5]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include("mod_bosh.hrl").
-define(ACCUMULATE_PERIOD, 10).
-define(DEFAULT_HOLD, 1).
-define(CONCURRENT_REQUESTS, 2).
-define(DEFAULT_WAIT, 60).
-define(DEFAULT_CLIENT_ACKS, false).

-type cached_response() :: {rid(), TStamp :: integer(), exml:element()}.
-type rid() :: pos_integer().

-record(state, {from            :: binary() | undefined,
                to              :: binary() | undefined,
                c2s_pid         :: pid(),
                handlers = []   :: [{rid(), reference(), pid()}],
                %% Elements buffered for sending to the client.
                pending = []    :: [jlib:xmlstreamel()],
                sid             :: mod_bosh:sid(),
                wait = ?DEFAULT_WAIT :: integer(),
                hold = ?DEFAULT_HOLD :: integer(),
                rid             :: rid() | undefined,
                %% Requests deferred for later processing because
                %% of having Rid greater than expected.
                deferred = []   :: [{rid(), {mod_bosh:event_type(), exml:element()}}],
                client_acks = ?DEFAULT_CLIENT_ACKS :: boolean(),
                sent = []       :: [cached_response()],
                %% Allowed inactivity period in seconds.
                inactivity      :: pos_integer() | infinity,
                inactivity_tref :: reference() | 'undefined',
                %% Max pause period in seconds.
                max_pause        :: pos_integer() | undefined,
                max_wait        :: pos_integer() | infinity,
                %% Are acknowledgements used?
                server_acks     :: boolean(),
                last_processed  :: rid() | 'undefined',
                %% Report scheduled for sending at the earliest
                %% possible occasion.
                report = false  :: {rid(), Time :: non_neg_integer()} | 'false'}).
-type state() :: #state{}.

-type statename() :: 'accumulate' | 'normal' | 'closing'.
-type fsm_return() :: {'next_state', statename(), state()}.

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start(mongooseim:host_type(), mod_bosh:sid(), mongoose_transport:peer(),
            binary() | undefined, map()) ->
    {'error', _} | {'ok', 'undefined' | pid()} | {'ok', 'undefined' | pid(), _}.
start(HostType, Sid, Peer, PeerCert, Opts) ->
    supervisor:start_child(?BOSH_SOCKET_SUP, [HostType, Sid, Peer, PeerCert, Opts]).

-spec start_link(mongooseim:host_type(), mod_bosh:sid(), mongoose_transport:peer(),
                 binary() | undefined, map()) ->
    'ignore' | {'error', _} | {'ok', pid()}.
start_link(HostType, Sid, Peer, PeerCert, Opts) ->
    gen_fsm_compat:start_link(?MODULE, [{HostType, Sid, Peer, PeerCert, Opts}], []).

-spec start_supervisor() -> {ok, pid()} | {error, any()}.
start_supervisor() ->
    ChildId = ?BOSH_SOCKET_SUP,
    ChildSpec = ejabberd_sup:template_supervisor_spec(ChildId, ?MODULE),
    case supervisor:start_child(ejabberd_sup, ChildSpec) of
        {ok, undefined} ->
            {error, undefined};
        {ok, Child} ->
            {ok, Child};
        {ok, Child, _Info} ->
            {ok, Child};
        {error, {already_started, Child}} ->
            {ok, Child};
        {error, Reason} ->
            {error, Reason}
    end.

-spec is_supervisor_started() -> boolean().
is_supervisor_started() ->
    is_pid(whereis(?BOSH_SOCKET_SUP)).

-spec stop_supervisor() -> ok | {error, any()}.
stop_supervisor() ->
    ejabberd_sup:stop_child(?BOSH_SOCKET_SUP).

-spec handle_request(Pid :: pid(),
                    {EventTag :: mod_bosh:event_type(),
                     Handler :: pid(),
                     Body :: exml:element()}) -> ok.
handle_request(Pid, Request) ->
    gen_fsm_compat:send_all_state_event(Pid, Request).


%% @doc TODO: no handler for this call is present!
%% No check for violating maxpause is made when calling this!
-spec pause(Pid :: pid(), Seconds :: pos_integer()) -> ok.
pause(Pid, Seconds) ->
    gen_fsm_compat:send_all_state_event(Pid, {pause, Seconds}).

%%--------------------------------------------------------------------
%% Private API
%%--------------------------------------------------------------------

get_handlers(Pid) ->
    gen_fsm_compat:sync_send_all_state_event(Pid, get_handlers).


get_pending(Pid) ->
    gen_fsm_compat:sync_send_all_state_event(Pid, get_pending).


-spec get_client_acks(pid()) -> boolean().
get_client_acks(Pid) ->
    gen_fsm_compat:sync_send_all_state_event(Pid, get_client_acks).


-spec set_client_acks(pid(), boolean()) -> any().
set_client_acks(Pid, Enabled) ->
    gen_fsm_compat:sync_send_all_state_event(Pid, {set_client_acks, Enabled}).


-spec get_cached_responses(pid()) -> [cached_response()].
get_cached_responses(Pid) ->
    gen_fsm_compat:sync_send_all_state_event(Pid, get_cached_responses).

%%--------------------------------------------------------------------
%% gen_fsm callbacks
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @private
%% init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
-spec init([{mongooseim:host_type(), mod_bosh:sid(), mongoose_transport:peer(), undefined |
             binary(), map()}]) ->
    {ok, accumulate, state()}.
init([{HostType, Sid, Peer, PeerCert, ListenerOpts}]) ->
    BoshSocket = #bosh_socket{sid = Sid, pid = self(), peer = Peer, peercert = PeerCert},
    C2SOpts = ListenerOpts#{access => all,
                            shaper => none,
                            max_stanza_size => 0,
                            hibernate_after => 0,
                            state_timeout => 5000,
                            backwards_compatible_session => true,
                            proto => tcp},
    {ok, C2SPid} = mongoose_c2s:start({?MODULE, BoshSocket, C2SOpts}, []),
    Opts = gen_mod:get_loaded_module_opts(HostType, mod_bosh),
    State = new_state(Sid, C2SPid, Opts),
    ?LOG_DEBUG(ls(#{what => bosh_socket_init, peer => Peer}, State)),
    {ok, accumulate, State}.

new_state(Sid, C2SPid, #{inactivity := Inactivity, max_wait := MaxWait,
                         server_acks := ServerAcks, max_pause := MaxPause}) ->
    #state{sid = Sid,
           c2s_pid = C2SPid,
           inactivity = Inactivity,
           max_pause = MaxPause,
           max_wait = MaxWait,
           server_acks = ServerAcks}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm_compat:send_event/2, the instance of this function with the same
%% name as the current state name StateName is called to handle
%% the event. It is also called if a timeout occurs.
%%
%% state_name(Event, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

-spec accumulate(_, state()) -> fsm_return().
accumulate(acc_off, #state{pending = Pending} = S) ->
    NS = S#state{pending = []},
    {next_state, normal, send_or_store(Pending, NS)};
accumulate(Event, State) ->
    ?LOG_DEBUG(ls(#{what => bosh_socket_unhandled_event, state_name => accumulate,
                    event => Event}, State)),
    {next_state, accumulate, State}.


-spec normal(_, state()) -> fsm_return().
normal(acc_off, #state{} = S) ->
    {next_state, normal, S};
normal(Event, State) ->
    ?LOG_DEBUG(ls(#{what => bosh_socket_unhandled_event, state_name => normal,
                    event => Event}, State)),
    {next_state, normal, State}.

closing(Event, State) ->
    ?LOG_DEBUG(ls(#{what => bosh_socket_unhandled_event, state_name => closing,
                    event => Event}, State)),
    {next_state, closing, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm_compat:sync_send_event/[2, 3], the instance of this function with
%% the same name as the current state name StateName is called to
%% handle the event.
%%
%% state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
accumulate(Event, _From, State) ->
    ?LOG_DEBUG(ls(#{what => bosh_socket_unhandled_sync_event, state_name => accumulate,
                    event => Event}, State)),
    {reply, ok, accumulate, State}.

normal(Event, _From, State) ->
    ?LOG_DEBUG(ls(#{what => bosh_socket_unhandled_sync_event, state_name => normal,
                    event => Event}, State)),
    {reply, ok, normal, State}.

closing(Event, _From, State) ->
    ?LOG_DEBUG(ls(#{what => bosh_socket_unhandled_sync_event, state_name => closing,
                    event => Event}, State)),
    {reply, ok, closing, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm_compat:send_all_state_event/2, this function is called to handle
%% the event.
%%
%% handle_event(Event, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

handle_event({EventTag, Handler, #xmlel{} = Body}, SName, S) ->
    NS = cancel_inactivity_timer(S),
    Rid = binary_to_integer(exml_query:attr(Body, <<"rid">>)),
    try
        NNS = handle_stream_event({EventTag, Body, Rid}, Handler, SName, NS),
        %% TODO: it's the event which determines the next state,
        %%       this ought to be returned from handle_stream_event
        determine_next_state(EventTag, SName, NNS)
    catch
        throw:{invalid_rid, TState} ->
            {stop, {shutdown, invalid_rid}, TState};
        throw:{invalid_pause, TState} ->
            {stop, {shutdown, policy_violation}, TState}
    end;

handle_event(Event, StateName, State) ->
    ?LOG_DEBUG(ls(#{what => bosh_socket_unhandled_all_state,
                   state_name => StateName, event => Event}, State)),
    {next_state, StateName, State}.


determine_next_state(_EventTag, closing, NNS) ->
    {stop, normal, NNS};
determine_next_state(EventTag, SName, NNS) ->
    case EventTag of
        _ when EventTag == streamstart; EventTag == restart ->
            timer:apply_after(?ACCUMULATE_PERIOD,
                gen_fsm_compat, send_event, [self(), acc_off]),
            {next_state, accumulate, NNS};
        _ ->
            {next_state, SName, NNS}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm receives an event sent using
%% gen_fsm_compat:sync_send_all_state_event/[2, 3], this function is called
%% to handle the event.
%%
%% handle_sync_event(Event, From, StateName, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {reply, Reply, NextStateName, NextState} |
%%                   {reply, Reply, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState} |
%%                   {stop, Reason, Reply, NewState}
%% @end
%%--------------------------------------------------------------------
handle_sync_event(get_handlers, _From, StateName,
                  #state{handlers = Handlers} = S) ->
    {reply, Handlers, StateName, S};
handle_sync_event(get_pending, _From, StateName,
                  #state{pending = Pending} = S) ->
    {reply, Pending, StateName, S};
handle_sync_event(get_client_acks, _From, StateName,
                  #state{client_acks = ClientAcks} = S) ->
    {reply, ClientAcks, StateName, S};
handle_sync_event({set_client_acks, ClientAcks}, _From, StateName,
                  #state{} = S) ->
    NS = S#state{client_acks = ClientAcks},
    {reply, ok, StateName, NS};
handle_sync_event(get_cached_responses, _From, StateName,
                  #state{sent = CachedResponses} = S) ->
    {reply, CachedResponses, StateName, S};
handle_sync_event(Event, _From, StateName, State) ->
    ?LOG_DEBUG(ls(#{what => bosh_socket_unhandled_sync_all_state,
                    state_name => StateName, event => Event}, State)),
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_fsm when it receives any
%% message other than a synchronous or asynchronous event
%% (or a system message).
%%
%% handle_info(Info, StateName, State)->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Timeout} |
%%                   {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------

handle_info({send, #xmlstreamend{} = StreamEnd}, normal = SName,
            #state{pending = Pending} = S) ->
    NS = send_or_store(Pending ++ [StreamEnd], S#state{pending = []}),
    {next_state, SName, NS};
handle_info({send, Data}, accumulate = SName, #state{} = S) ->
    {next_state, SName, store([Data], S)};
handle_info({send, Data}, normal = SName, #state{} = S) ->
    NS = send_or_store(Data, S),
    {next_state, SName, NS};
handle_info(close, _SName, #state{pending = []} = State) ->
    {stop, normal, State};
handle_info(close, _SName, State) ->
    {next_state, closing, State};
handle_info(inactivity_timeout, _SName, State) ->
    ?LOG_INFO(ls(#{what => bosh_socket_terminating, reason => inactivity_timeout}, State)),
    {stop, {shutdown, inactivity_timeout}, State};
handle_info({wait_timeout, {Rid, Pid}}, SName,
            #state{handlers = Handlers} = S) ->
    ?LOG_INFO(ls(#{what => bosh_socket_wait_timeout,
                   handler_rid => Rid, handler_pid => Pid}, S)),
    %% In case some message was being handled when the timer fired
    %% it may turn out that Pid is no longer available in Handlers.
    case lists:keytake(Rid, 1, Handlers) of
        false ->
            {next_state, SName, S};
        {value, {Rid, _, Pid}, NewHandlers} ->
            NS = send_to_handler({Rid, Pid}, [],
                                 S#state{handlers = NewHandlers}),
            {next_state, SName, NS}
    end;
handle_info(Info, SName, State) ->
    ?UNEXPECTED_INFO(Info),
    {next_state, SName, State}.

terminate(Reason, StateName, #state{sid = Sid, handlers = Handlers} = S) ->
    [Pid ! {close, Sid} || {_, _, Pid} <- lists:sort(Handlers)],
    mod_bosh_backend:delete_session(Sid),
    catch mongoose_c2s:stop(S#state.c2s_pid, normal),
    ?LOG_DEBUG(ls(#{what => bosh_socket_closing_session, reason => Reason,
                    state_name => StateName, handlers => Handlers,
                    pending => S#state.pending}, S)).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%% callback implementations
%%--------------------------------------------------------------------

handle_stream_event({EventTag, Body, Rid} = Event, Handler,
                    SName, #state{rid = OldRid} = S) ->
    ExpectedRid = maybe_add(1, OldRid),
    NS = maybe_add_handler(Handler, Rid, S),
    NNS = case {EventTag,
                maybe_is_retransmission(Rid, OldRid, S#state.sent),
                is_expected_rid(Rid, ExpectedRid),
                is_acceptable_rid(Rid, ExpectedRid)}
    of
        {_, {true, CachedResponse}, _, _} when Handler /= none ->
            case CachedResponse of
                none ->
                    NS;
                _ ->
                    resend_cached(CachedResponse, NS)
            end;
        {streamstart, _, _, _} ->
            process_acked_stream_event(Event, SName, NS);
        {_, _, true, _} ->
            process_acked_stream_event(Event, SName, NS);
        {_, _, false, true} ->
            ?LOG_INFO(ls(#{what => bosh_socket_deferring,
                           event_rid => Rid, body => Body,
                           expected_rid => ExpectedRid, event_tag => EventTag}, S)),
            NS#state{deferred = [Event | NS#state.deferred]};
        {_, _, false, false} ->
            ?LOG_ERROR(#{what => bosh_socket_invalid_rid,
                         event_rid => Rid, body => Body,
                         expected_rid => ExpectedRid, event_tag => EventTag,
                         difference => maybe_diff(Rid, ExpectedRid)}),
            [Pid ! item_not_found
             || {_, _, Pid} <- lists:sort(NS#state.handlers)],
            throw({invalid_rid, NS#state{handlers = []}})
    end,
    return_surplus_handlers(SName, NNS).

-spec maybe_is_retransmission(rid(), rid(), [cached_response()])
    -> false | {true, none} | {true, cached_response()}.
maybe_is_retransmission(Rid, OldRid, Sent) ->
    case {lists:keyfind(Rid, 1, Sent), Rid =:= OldRid} of
        {false, false} ->
            false;
        {false, true} ->
            ?LOG_INFO(#{what => bosh_socket_request_repeated,
                        text => <<"Request repeated but no response found in cache">>,
                        event_rid => Rid, old_rid => OldRid, sent => Sent}),
            {true, none};
        {CachedResponse, _} ->
            {true, CachedResponse}
    end.

-spec maybe_add(rid(), rid() | undefined)
  -> rid() | undefined.
maybe_add(_, undefined) -> undefined;
maybe_add(Rid1, Rid2) when is_integer(Rid1),
                           is_integer(Rid2) -> Rid1 + Rid2.

-spec maybe_diff(rid(), rid() | undefined)
  -> non_neg_integer() | undefined.
maybe_diff(_, undefined) -> undefined;
maybe_diff(Rid, Expected) -> abs(Rid-Expected).

-spec resend_cached(cached_response(), state()) -> state().
resend_cached({_Rid, _, CachedBody}, S) ->
    send_to_handler(CachedBody, S).


-spec process_acked_stream_event({EventTag :: mod_bosh:event_type(),
                                    Body :: exml:element(),
                                    Rid :: 'undefined' | rid()},
                                SName :: any(),
                                S :: state() ) -> state().
process_acked_stream_event({EventTag, Body, Rid}, SName,
                           #state{} = S) ->
    MaybeBAck = exml_query:attr(Body, <<"ack">>),
    {Action, Ack} = determine_report_action(MaybeBAck, S#state.client_acks,
                                            Rid, S#state.last_processed),
    NS = maybe_trim_cache(Ack, S),
    case Action of
        noreport ->
            process_stream_event(EventTag, Body, SName, rid(NS, Rid));
        report ->
            NS2 = schedule_report(Ack, NS),
            NS3 = process_stream_event(EventTag, Body, SName, rid(NS2, Rid)),
            maybe_send_report(NS3)
    end.

rid(#state{} = S, Rid) when is_integer(Rid), Rid > 0 ->
    S#state{rid = Rid}.


-spec determine_report_action(BinAck :: 'undefined' | binary(),
                              boolean(),
                              Rid :: rid(),
                              LastProcessed :: 'undefined' | pos_integer()
                            ) -> {'noreport', _} | {'report', _}.
determine_report_action(undefined, false, _, _) ->
    {noreport, undefined};
determine_report_action(undefined, true, Rid, LastProcessed) ->
    ?WARNING_MSG_IF(Rid+1 /= LastProcessed, "expected 'ack' attribute on ~p~n", [Rid]),
    {noreport, undefined};
determine_report_action(BinAck, _, _, LastProcessed) ->
    Ack = binary_to_integer(BinAck),
    case {LastProcessed, is_valid_ack(Ack, LastProcessed)} of
        {undefined, _} ->
            {noreport, Ack};
        {_, true} ->
            {noreport, Ack};
        {_, false} ->
            {report, Ack}
    end.


-spec is_valid_ack(Ack :: rid(), 'undefined' | pos_integer()) -> boolean().
is_valid_ack(Ack, LastProcessed)
        when Ack < LastProcessed ->
    false;
is_valid_ack(_, _) ->
    true.


-spec maybe_trim_cache(undefined | any(), state()) -> state().
maybe_trim_cache(undefined, S) ->
    S;
maybe_trim_cache(Ack, S) ->
    UpToAck = fun({R, _, _}) when R =< Ack ->
                    true;
                 (_) ->
                    false
              end,
    NewSent = lists:dropwhile(UpToAck, S#state.sent),
    S#state{sent = NewSent}.


-spec schedule_report(rid(), state()) -> state().
schedule_report(Ack, #state{sent = Sent} = S) ->
    ReportRid = Ack + 1,
    try
        {ReportRid, TimeSent, _} = lists:keyfind(ReportRid, 1, Sent),
        ElapsedTimeMillis = erlang:monotonic_time(millisecond) - TimeSent,
        Report = {ReportRid, ElapsedTimeMillis},
        case S#state.report of
            false ->
                S#state{report = Report};
            OldReport when OldReport < Report ->
                S#state{report = OldReport};
            _ ->
                S#state{report = Report}
        end
    catch
        error:{badmatch, {resp, false}} ->
            ?LOG_ERROR(ls(#{what => bosh_socket_no_cached_response,
                            responses => Sent, rid_offender => ReportRid}, S)),
            S
    end.


-spec maybe_send_report(state()) -> state().
maybe_send_report(#state{report = false} = S) ->
    S;
maybe_send_report(#state{} = S) ->
    send_or_store([], S).


-spec process_stream_event(mod_bosh:event_type(), exml:element(), _SName,
                           state()) -> state().
process_stream_event(pause, Body, SName, State) ->
    Seconds = binary_to_integer(exml_query:attr(Body, <<"pause">>)),
    NewState = process_pause_event(Seconds, State),
    process_deferred_events(SName, NewState);
process_stream_event(EventTag, Body, SName, #state{c2s_pid = C2SPid} = State) ->
    {Els, NewState} = bosh_unwrap(EventTag, Body, State),
    [forward_to_c2s(C2SPid, El) || El <- Els],
    process_deferred_events(SName, NewState).


-spec process_pause_event('infinity' | 'undefined' | pos_integer(),
                          state()) -> state().
process_pause_event(Seconds, #state{max_pause = MaxPause} = S)
        when MaxPause == undefined;
             Seconds > MaxPause ->
    [Pid ! policy_violation || {_, _, Pid} <- S#state.handlers],
    throw({invalid_pause, S#state{handlers = []}});
process_pause_event(Seconds, State) ->
    NS = State#state{inactivity = Seconds},
    F = fun(_, S) ->
            send_to_handler([], S)
    end,
    lists:foldl(F, NS, lists:seq(1, length(State#state.handlers))).


-spec process_deferred_events(_SName, state()) -> state().
process_deferred_events(SName, #state{deferred = Deferred} = S) ->
    lists:foldl(fun(Event, State) ->
                    ?LOG_DEBUG(ls(#{what => bosh_socket_processing_deferred_event,
                                    event => Event}, S)),
                    handle_stream_event(Event, none, SName, State)
                end,
                S#state{deferred = []},
                lists:sort(Deferred)).


-spec is_expected_rid(rid(), rid() | undefined) -> boolean().
is_expected_rid(Rid, ExpectedRid) when Rid == ExpectedRid ->
    true;
is_expected_rid(_, _) ->
    false.

-spec is_acceptable_rid(rid(), rid() | undefined) -> boolean().
is_acceptable_rid(Rid, ExpectedRid)
  when Rid > ExpectedRid,
       Rid < ExpectedRid + ?CONCURRENT_REQUESTS ->
    true;
is_acceptable_rid(_, _) ->
    false.

%% @doc Send data to the client if a request handler is available, that matches next RID.
%% Otherwise, store for sending later.
-spec send_or_store(_Data, state()) -> state().
send_or_store(Data, State) when not is_list(Data) ->
    send_or_store([Data], State);
send_or_store(Data, State) ->
    case send_to_handler(Data, State) of
        no_valid_handler ->
            store(Data, State);
        NewState ->
            NewState
    end.


%% @doc send_to_handler() assumes that Handlers is not empty!
%% Be sure that's the case if calling it.
-spec send_to_handler([any()] | exml:element(), state()) -> state() | no_valid_handler.
send_to_handler(Data, State) ->
    case pick_handler(State) of
        {Handler, NS} ->
            send_to_handler(Handler, Data, NS);
        false ->
            no_valid_handler
    end.


%% Return handler and new state if a handler is available
%% or `false` otherwise.
-spec pick_handler(state()) -> {{rid(), pid()}, state()} | false.
pick_handler(#state{ handlers = [] }) ->
    false;
pick_handler(#state{ handlers = Handlers, rid = Rid } = S) ->
    case lists:sort(Handlers) of
        [{HandlerRid, TRef, Pid} | HRest] when HandlerRid =< Rid->
            %% The cancellation might fail if the timer already fired.
            %% Don't worry, it's handled on receiving the timeout message.
            erlang:cancel_timer(TRef),
            {{HandlerRid, Pid}, S#state{handlers = HRest}};
        _ ->
            false
    end.


-spec send_to_handler({_, atom() | pid() | port() | {atom(), atom()}},
                      Wrapped :: [any()] | exml:element(),
                      State :: state() ) -> state().
send_to_handler({_, Pid}, #xmlel{name = <<"body">>} = Wrapped, State) ->
    send_wrapped_to_handler(Pid, Wrapped, State);
send_to_handler({Rid, Pid}, Data, State) ->
    {Wrapped, NS} = bosh_wrap(Data, Rid, State),
    NS2 = cache_response({Rid, erlang:monotonic_time(millisecond), Wrapped}, NS),
    send_wrapped_to_handler(Pid, Wrapped, NS2).


%% @doc This is the most specific variant of send_to_handler()
%% and the *only one* actually performing a send
%% to the cowboy_loop_handler serving a HTTP request.
-spec send_wrapped_to_handler(atom() | pid() | port() | {atom(), atom()},
                              Wrapped :: exml:element(),
                              State :: state()) -> state().
send_wrapped_to_handler(Pid, Wrapped, #state{handlers = []} = State) ->
    Pid ! {bosh_reply, Wrapped},
    setup_inactivity_timer(State);
send_wrapped_to_handler(Pid, Wrapped, State) ->
    Pid ! {bosh_reply, Wrapped},
    State.


-spec maybe_add_ack(rid(), state(), exml:attrs()) -> exml:attrs().
maybe_add_ack(HandlerRid, #state{rid = Rid} = S, Attrs) ->
    case Rid > HandlerRid of
        true ->
            server_ack(S#state.server_acks, Rid, Attrs);
        false ->
            Attrs
    end.


-spec maybe_add_report(state(), exml:attrs()) -> {exml:attrs(), state()}.
maybe_add_report(#state{report = false} = S, Attrs) ->
    {Attrs, S};
maybe_add_report(#state{report = Report} = S, Attrs) ->
    {ReportRid, ElapsedTime} = Report,
    NewAttrs = Attrs#{<<"report">> => integer_to_binary(ReportRid),
                      <<"time">> => integer_to_binary(ElapsedTime)},
    {NewAttrs, S#state{report = false}}.


-spec cache_response(cached_response(), state()) -> state().
cache_response({Rid, _, _} = Response, #state{sent = Sent} = S) ->
    NewSent = lists:keymerge(1, [Response], Sent),
    CacheUpTo = case S#state.client_acks of
        true ->
            %% Acknowledgements are on - there's no limit on the number
            %% of cached responses.
            infinity;
        false ->
            %% Leave up to ?CONCURRENT_REQUESTS responses in cache.
            ?CONCURRENT_REQUESTS
    end,
    S#state{sent = cache_up_to(CacheUpTo, NewSent),
            last_processed = last_processed(Rid, S#state.last_processed)}.


-spec cache_up_to('infinity' | 2, Responses :: [cached_response()])
            -> [cached_response()].
cache_up_to(infinity, Responses) ->
    Responses;
cache_up_to(N, Responses) ->
    lists:nthtail(max(0, length(Responses) - N), Responses).


-spec last_processed(rid(), 'undefined' | pos_integer()) -> rid().
last_processed(Rid, undefined) ->
    Rid;
last_processed(Rid1, Rid2) ->
    max(Rid1, Rid2).


-spec setup_inactivity_timer(state()) -> state().
setup_inactivity_timer(#state{inactivity = infinity} = S) ->
    S;
setup_inactivity_timer(S) ->
    cancel_inactivity_timer(S),
    TRef = erlang:send_after(timer:seconds(S#state.inactivity), self(),
                             inactivity_timeout),
    S#state{inactivity_tref = TRef}.


-spec cancel_inactivity_timer(state()) -> state().
cancel_inactivity_timer(#state{inactivity_tref = undefined} = S) ->
    S;
cancel_inactivity_timer(S) ->
    erlang:cancel_timer(S#state.inactivity_tref),
    S#state{inactivity_tref = undefined}.


%% @doc Store data for sending later.
-spec store([jlib:xmlstreamel()], state()) -> state().
store(Data, #state{pending = Pending} = S) ->
    S#state{pending = Pending ++ Data}.


-spec forward_to_c2s(pid() | undefined, jlib:xmlstreamel()) -> ok.
forward_to_c2s(C2SPid, StreamElement) ->
    C2SPid ! {tcp, undefined, StreamElement},
    ok.


-spec maybe_add_handler(_, rid(), state()) -> state().
maybe_add_handler(Handler, Rid, S) when is_pid(Handler) ->
    add_handler({Rid, Handler}, S);
maybe_add_handler(_, _, S) ->
    S.


-spec add_handler({rid(), pid()}, state()) -> state().
add_handler({Rid, Pid}, #state{handlers = Handlers} = S) ->
    TRef = erlang:send_after(timer:seconds(S#state.wait), self(),
                             {wait_timeout, {Rid, Pid}}),
    S#state{handlers = [{Rid, TRef, Pid} | Handlers]}.


%% @doc Keep in mind the hardcoding for hold == 1.
-spec return_surplus_handlers('accumulate' | 'normal' | 'closing', state()) -> state().
return_surplus_handlers(SName, #state{handlers = []} = State)
        when SName == accumulate; SName == normal; SName == closing ->
    State;
return_surplus_handlers(SName, #state{handlers = []} = State)
        when SName == normal; SName == closing ->
    State;
return_surplus_handlers(accumulate, #state{handlers = [_]} = State) ->
    State;
return_surplus_handlers(SName, #state{handlers = [_], pending = []} = State)
    when SName == normal; SName == closing ->
    State;
return_surplus_handlers(accumulate, #state{handlers = _} = S) ->
    case send_to_handler([], S) of
        no_valid_handler -> S;
        NS -> return_surplus_handlers(accumulate, NS)
    end;
return_surplus_handlers(SName, #state{pending = Pending} = S)
    when SName == normal; SName == closing ->
    send_or_store(Pending, S#state{pending = []}).


-spec bosh_unwrap(EventTag :: mod_bosh:event_type(), exml:element(), state())
   -> {[jlib:xmlstreamel()], state()}.
bosh_unwrap(StreamEvent, Body, #state{} = S)
  when StreamEvent =:= streamstart ->
    Wait = min(get_attr(<<"wait">>, Body, S#state.wait), S#state.max_wait),
    Hold = get_attr(<<"hold">>, Body, S#state.hold),
    ClientAcks = get_client_acks(StreamEvent, Body, S#state.client_acks, S),
    From = exml_query:attr(Body, <<"from">>),
    To = exml_query:attr(Body, <<"to">>),
    E = stream_start(From, To),
    S2 = S#state{wait = Wait, hold = Hold, client_acks = ClientAcks,
                 from = From, to = To},
    {[E], S2};

bosh_unwrap(StreamEvent, _Body, #state{} = S)
  when StreamEvent =:= restart ->
    {[stream_start(S#state.from, S#state.to)], S};

bosh_unwrap(streamend, Body, State) ->
    {Els, NewState} = bosh_unwrap(normal, Body, State),
    {Els ++ [#xmlstreamend{name = <<>>}], NewState};

bosh_unwrap(normal, Body, #state{sid = Sid} = State) ->
    Sid = exml_query:attr(Body, <<"sid">>),
    ?NS_HTTPBIND = exml_query:attr(Body, <<"xmlns">>),

    {[El || El <- Body#xmlel.children,
            %% Ignore whitespace keepalives.
            El /= #xmlcdata{content = <<" ">>}],
     State}.


-spec get_client_acks(streamstart, exml:element(), boolean(), #state{}) -> boolean().
get_client_acks(streamstart, Element, Default, State) ->
    case exml_query:attr(Element, <<"ack">>) of
        undefined ->
            Default;
        <<"1">> ->
            true;
        _ ->
            ?LOG_INFO(ls(#{what => bosh_socket_ignore_ack,
                           text => <<"Ignoring invalid client ack on stream start">>}, State)),
            false
    end.


-spec get_attr(Attr :: binary(), exml:element(), integer()) -> any().
get_attr(Attr, Element, Default) ->
    case exml_query:attr(Element, Attr) of
        undefined ->
            Default;
        Value ->
            binary_to_integer(Value)
    end.


-spec stream_start(binary(), binary()) -> exml_stream:start().
stream_start(From, To) ->
    #xmlstreamstart{name = <<"stream:stream">>,
                    attrs = #{<<"from">> => From,
                              <<"to">> => To,
                              <<"version">> => <<"1.0">>,
                              <<"xml:lang">> => <<"en">>,
                              <<"xmlns">> => ?NS_CLIENT,
                              <<"xmlns:stream">> => ?NS_STREAM}}.


-spec bosh_wrap([any()], rid(), state()) -> {exml:element(), state()}.
bosh_wrap(Elements, Rid, #state{} = S) ->
    EventsStanzas = lists:partition(fun is_stream_event/1, Elements),
    {{Body, Children}, NS} = case EventsStanzas of
        {[], Stanzas} ->
            {{bosh_body(S), Stanzas}, S};
        {[#xmlstreamstart{} = StreamStart], Stanzas} ->
            {{bosh_stream_start_body(StreamStart, S), Stanzas}, S};
        {[#xmlstreamend{}], []} ->
            %% No stanzas except stream end - OK.
            {{bosh_stream_end_body(), []}, S};
        {[#xmlstreamend{} = StreamEnd], Stanzas} ->
            %% Can't wrap remaining stanzas in a stream end body.
            %% Send Stanzas and forfeit sending stream end.
            ?LOG_DEBUG(#{what => bosh_socket_cannot_send_stream_end,
                         text => <<"Can't send stream end yet. Still have pending stanzas">>,
                         stanzas => Stanzas}),
            Pending = S#state.pending,
            {{bosh_body(S), Stanzas},
             S#state{pending = Pending ++ [StreamEnd]}}
    end,
    Attrs1 = maybe_add_ack(Rid, NS, Body#xmlel.attrs),
    {Attrs2, NNS} = maybe_add_report(NS, Attrs1),
    HasStreamPrefix = (exml_query:attr(Body, <<"xmlns:stream">>) /= undefined),
    Attrs3 = maybe_add_stream_prefix(HasStreamPrefix, Children, Attrs2),
    {Body#xmlel{attrs = Attrs3,
                children = maybe_add_default_ns_to_children(Children)}, NNS}.


-spec is_stream_event(jlib:xmlstreamel()) -> boolean().
is_stream_event(#xmlstreamstart{}) ->
    true;
is_stream_event(#xmlstreamend{}) ->
    true;
is_stream_event(_) ->
    false.


%% @doc Bosh body for a session creation response.
-spec bosh_stream_start_body(exml_stream:start(), state()) -> exml:element().
bosh_stream_start_body(#xmlstreamstart{attrs = Attrs}, #state{} = S) ->
    Attrs1 = #{<<"wait">> => integer_to_binary(S#state.wait),
               <<"requests">> => integer_to_binary(?CONCURRENT_REQUESTS),
               <<"hold">> => integer_to_binary(S#state.hold),
               <<"from">> => maps:get(<<"from">>, Attrs,undefined),
               % TODO: how to support these with cowbo?
               <<"accept">> => <<"deflate, gzip">>,
               <<"sid">> => S#state.sid,
               <<"xmpp:restartlogic">> => <<"true">>,
               <<"xmpp:version">> => <<"1.0">>,
               <<"xmlns">> => ?NS_HTTPBIND,
               <<"xmlns:xmpp">> => <<"urn:xmpp:xbosh">>,
               <<"xmlns:stream">> => ?NS_STREAM},

    Attrs2 = inactivity(S#state.inactivity, Attrs1),
    Attrs3 = maxpause(S#state.max_pause, Attrs2),

    %% TODO: shouldn't an ack be sent on restart?
    Attrs4 = server_ack(S#state.server_acks, S#state.rid, Attrs3),

    #xmlel{name = <<"body">>,  attrs = Attrs4, children = []}.


-spec inactivity('infinity' | 'undefined' | pos_integer(), exml:attrs()) -> exml:attrs().
inactivity(I, Attrs) when is_integer(I)->
    Attrs#{<<"inactivity">> => integer_to_binary(I)};
inactivity(_, Attrs) ->
    Attrs.


-spec maxpause('undefined' | pos_integer(), exml:attrs()) -> exml:attrs().
maxpause(MP, Attrs) when is_integer(MP)->
    Attrs#{<<"maxpause">> =>  integer_to_binary(MP)};
maxpause(_MP, Attrs)->
    Attrs.

-spec server_ack('false' | 'true' | 'undefined', 'undefined' | rid(), exml:attrs())
            -> exml:attrs().
server_ack(true, Rid, Attrs) ->
    Attrs#{<<"ack">> => integer_to_binary(Rid)};
server_ack(_, _, Attrs) ->
    Attrs.


%% @doc Bosh body for an ordinary stream element(s).
-spec bosh_body(state()) -> exml:element().
bosh_body(#state{} = S) ->
    #xmlel{name = <<"body">>,
           attrs = #{<<"sid">> => S#state.sid,
                     <<"xmlns">> => ?NS_HTTPBIND},
           children = []}.


-spec bosh_stream_end_body() -> exml:element().
bosh_stream_end_body() ->
    #xmlel{name = <<"body">>,
           attrs = #{<<"type">> => <<"terminate">>,
                     <<"xmlns">> => ?NS_HTTPBIND},
           children = []}.

maybe_add_stream_prefix(true, _, Attrs) ->
    Attrs;
maybe_add_stream_prefix(_, Stanzas, Attrs) ->
    case lists:any(fun is_stream_prefix/1, Stanzas) of
        false ->
            Attrs;
        true ->
            Attrs#{<<"xmlns:stream">> => ?NS_STREAM}
    end.

is_stream_prefix(#xmlel{name = <<"stream:error">>}) -> true;
is_stream_prefix(#xmlel{name = <<"stream:features">>}) -> true;
is_stream_prefix(_) -> false.


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

maybe_add_default_ns_to_children(Children) ->
    lists:map(fun maybe_add_default_ns/1, Children).

maybe_add_default_ns(#xmlel{name = Name, attrs = Attrs} = El)
  when Name =:= <<"message">>; Name =:= <<"presence">>; Name =:= <<"iq">> ->
    case exml_query:attr(El, <<"xmlns">>) of
        undefined ->
            El#xmlel{attrs = Attrs#{<<"xmlns">> => ?NS_CLIENT}};
        _ ->
            El
    end;
maybe_add_default_ns(El) ->
    El.

ls(LogMap, State) ->
    S = #{sid => State#state.sid,
          c2s_pid => State#state.c2s_pid,
          from_jid => State#state.from,
          to_jid => State#state.to,
          rid => State#state.rid},
    maps:merge(LogMap, ignore_undefined(S)).

ignore_undefined(Map) ->
    maps:filter(fun(_, V) -> V =/= undefined end, Map).

%% mongoose_xmpp_socket callbacks

-spec peername(mod_bosh:socket()) -> mongoose_transport:peer().
peername(#bosh_socket{peer = Peer}) ->
    Peer.

-spec tcp_to_tls(mod_bosh:socket(), mongoose_listener:options(), mongoose_xmpp_socket:side()) ->
  {ok, mod_bosh:socket()} | {error, term()}.
tcp_to_tls(_Socket, _LOpts, server) ->
    {error, tcp_to_tls_not_allowed_on_bosh}.

-spec handle_data(mod_bosh:socket(), {tcp | ssl, term(), iodata()}) ->
  iodata() | {raw, [exml:element()]} | {error, term()}.
handle_data(_Socket, {_Kind, _Term, Packet}) ->
    {raw, [Packet]}.

-spec activate(mod_bosh:socket()) -> ok.
activate(_Socket) ->
    ok.

-spec send_xml(mod_bosh:socket(),
                      iodata() | exml_stream:element() | [exml_stream:element()]) ->
    ok | {error, term()}.
send_xml(#bosh_socket{pid = Pid}, XMLs) when is_list(XMLs) ->
    [Pid ! {send, XML} || XML <- XMLs],
    ok;
send_xml(#bosh_socket{pid = Pid}, XML) ->
    Pid ! {send, XML},
    ok.

-spec close(mod_bosh:socket()) -> ok.
close(#bosh_socket{pid = Pid}) ->
    Pid ! close,
    ok.

-spec get_peer_certificate(mod_bosh:socket()) ->
    mongoose_transport:peercert_return().
get_peer_certificate(#bosh_socket{peercert = undefined}) ->
    no_peer_cert;
get_peer_certificate(#bosh_socket{peercert = PeerCert}) ->
    Decoded = public_key:pkix_decode_cert(PeerCert, plain),
    {ok, Decoded}.

-spec has_peer_cert(mod_bosh:socket(), mongoose_listener:options()) -> boolean().
has_peer_cert(Socket, _) ->
    get_peer_certificate(Socket) /= no_peer_cert.

-spec is_channel_binding_supported(mod_bosh:socket()) -> boolean().
is_channel_binding_supported(_Socket) ->
    false.

-spec export_key_materials(mod_bosh:socket(), _, _, _, _) -> {error, atom()}.
export_key_materials(_Socket, _, _, _, _) ->
    {error, export_key_materials_not_allowed_on_bosh}.

-spec is_ssl(mod_bosh:socket()) -> boolean().
is_ssl(_Socket) ->
    false.

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

cache_up_to_test_() ->
    [?_test(?assertEqual( [4, 5], cache_up_to(2, [1, 2, 3, 4, 5]) ))].

-endif.
