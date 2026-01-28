%%%-------------------------------------------------------------------
%%% @doc Broadcast job worker process.
%%%
%%% Implements the broadcast worker as a gen_statem that:
%%% - Reads job metadata and resumes from persisted state
%%% - Loads recipients in batches
%%% - Sends messages with rate limiting (monotonic time)
%%% - Persists progress after each batch
%%% - Aborts on DB errors, skips on per-recipient errors
%%%-------------------------------------------------------------------

-module(broadcast_worker).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(gen_statem).

%% API
-export([start_link/2,
         stop/1]).

%% gen_statem callbacks
-export([callback_mode/0,
         init/1,
         terminate/3,
         code_change/4]).

%% gen_statem states
-export([loading_batch/3,
         sending_batch/3,
         finished/3,
         aborted/3]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_broadcast.hrl").

-record(data, {
    host_type :: mongooseim:host_type(),
    %% Note: worker cares only about static data in #broadcast_job{},
    %% so recipients_processed in job record is not updated, only in worker state.
    job :: broadcast_job(),
    state :: broadcast_worker_state(),
    batch_size :: pos_integer(),
    batch_t0 :: integer() | undefined,  % monotonic time when batch started
    current_batch :: [jid:jid()]  % recipients remaining in current batch
}).

-type state() :: loading_batch | sending_batch | finished | aborted.
-type data() :: #data{}.

%%====================================================================
%% API
%%====================================================================

-spec start_link(mongooseim:host_type(), JobId :: integer()) -> gen_statem:start_ret().
start_link(HostType, JobId) ->
    gen_statem:start_link(?MODULE, {HostType, JobId}, []).

-spec stop(pid()) -> ok.
stop(WorkerPid) ->
    try gen_statem:call(WorkerPid, stop, 5000) of
        ok -> ok
    catch
        exit:{timeout, _} ->
            %% Worker didn't respond in time, forcibly kill it
            exit(WorkerPid, kill),
            ok
    end.

%%====================================================================
%% gen_statem callbacks
%%====================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    state_functions.

-spec init({mongooseim:host_type(), integer()}) -> gen_statem:init_result(state(), data()).
init({HostType, JobId}) ->
    process_flag(trap_exit, true),
    case load_job(HostType, JobId) of
        {ok, Job, WorkerStateFromDB} ->
            WorkerState = maybe_init_worker_state(HostType, JobId, WorkerStateFromDB),
            BatchSize = Job#broadcast_job.message_rate * 10,
            Data = #data{host_type = HostType,
                         job = Job,
                         state = WorkerState,
                         batch_size = BatchSize,
                         batch_t0 = undefined,
                         current_batch = []},
            ?LOG_INFO(#{what => broadcast_worker_started,
                        job_id => JobId,
                        domain => Job#broadcast_job.domain,
                        total_recipients => Job#broadcast_job.recipient_count,
                        processed => WorkerState#broadcast_worker_state.recipients_processed}),
            {ok, loading_batch, Data, [{next_event, internal, load_batch}]};
        {error, Reason} ->
            ?LOG_ERROR(#{what => broadcast_worker_load_failed,
                         job_id => JobId, reason => Reason}),
            {stop, {load_failed, Reason}}
    end.

%%====================================================================
%% State functions
%%====================================================================

-spec loading_batch(gen_statem:event_type(), term(), data()) ->
    gen_statem:state_function_result(state()).
loading_batch(Origin, load_batch, Data)
  when Origin == internal; Origin == state_timeout->
    #data{host_type = HostType, job = Job, state = WorkerState} = Data,
    %% Persist current state before attempting to load next batch
    %% (ensures we can retry same batch if loading/sending fails)
    case persist_worker_state(HostType, Job#broadcast_job.id, WorkerState) of
        ok ->
            case load_next_batch(Job, WorkerState) of
                {ok, Recipients, NewCursor} ->
                    NewState = WorkerState#broadcast_worker_state{cursor = NewCursor},
                    T0 = erlang:monotonic_time(millisecond),
                    NewData = Data#data{state = NewState,
                                        batch_t0 = T0,
                                        current_batch = Recipients},
                    {next_state, sending_batch, NewData,
                     [{next_event, internal, send_one}]};
                {error, Reason} ->
                    FinalizeEvent = {finalize, {"load_batch failed: ~p", [Reason]}},
                    {next_state, aborted, Data, [{next_event, internal, FinalizeEvent}]}
            end;
        {error, Reason} ->
            FinalizeEvent = {finalize, {"persist_state failed: ~p", [Reason]}},
            {next_state, aborted, Data, [{next_event, internal, FinalizeEvent}]}
    end;
loading_batch({call, From}, stop, Data) ->
    persist_abort_admin(Data),
    {stop_and_reply, normal, [{reply, From, ok}]};
loading_batch(EventType, Event, Data) ->
    handle_common(EventType, Event, loading_batch, Data).

-spec sending_batch(gen_statem:event_type(), term(), data()) ->
    gen_statem:state_function_result(state()).
sending_batch(EventType, send_one, #data{current_batch = [], state = WorkerState} = Data)
  when EventType == internal; EventType == state_timeout ->
    %% Batch complete, check if more batches needed
    NewData = Data#data{batch_t0 = undefined, current_batch = []},
    case WorkerState#broadcast_worker_state.cursor of
        undefined ->
            %% No more recipients: finalize job
            {next_state, finished, NewData, [{next_event, internal, finalize}]};
        _Cursor ->
            %% More recipients to process: load next batch
            {next_state, loading_batch, NewData, [{next_event, internal, load_batch}]}
    end;
sending_batch(EventType, send_one, #data{current_batch = [RecipientJid | Rest]} = Data)
  when EventType == internal; EventType == state_timeout ->
    #data{job = Job, state = WorkerState, batch_t0 = T0} = Data,
    %% Send message to this recipient (log on failure but continue)
    case send_message(Job, RecipientJid) of
        ok -> ok;
        {error, Reason} ->
            ?LOG_WARNING(#{what => broadcast_send_failed,
                            job_id => Job#broadcast_job.id,
                            recipient => jid:to_binary(RecipientJid),
                            reason => Reason})
    end,

    %% Update progress
    Processed = WorkerState#broadcast_worker_state.recipients_processed,
    NewProcessed = Processed + 1,
    NewState = WorkerState#broadcast_worker_state{recipients_processed = NewProcessed},
    NewData = Data#data{state = NewState, current_batch = Rest},

    %% Calculate delay for rate limiting
    TargetTime = T0 + (NewProcessed * 1000) div Job#broadcast_job.message_rate,
    CurrentTime = erlang:monotonic_time(millisecond),
    DelayMs = max(0, TargetTime - CurrentTime),

    %% Schedule next recipient via state_timeout (allows handling other events)
    {keep_state, NewData, [{state_timeout, DelayMs, send_one}]};
sending_batch({call, From}, stop, Data) ->
    persist_abort_admin(Data),
    {stop_and_reply, normal, [{reply, From, ok}]};
sending_batch(EventType, Event, Data) ->
    handle_common(EventType, Event, sending_batch, Data).

-spec finished(gen_statem:event_type(), term(), data()) ->
    gen_statem:state_function_result(state()).
finished(internal, finalize, Data) ->
    #data{host_type = HostType, job = Job, state = WorkerState} = Data,
    %% Persist final worker state (best effort)
    case persist_worker_state(HostType, Job#broadcast_job.id, WorkerState) of
        ok -> ok;
        {error, Reason} ->
            ?LOG_WARNING(#{what => broadcast_final_state_persist_failed,
                           job_id => Job#broadcast_job.id, reason => Reason})
    end,
    persist_completion(HostType, Job),
    ?LOG_INFO(#{what => broadcast_worker_finished,
                job_id => Job#broadcast_job.id,
                domain => Job#broadcast_job.domain,
                total_processed => WorkerState#broadcast_worker_state.recipients_processed}),
    {stop, normal, Data};
finished(EventType, Event, Data) ->
    handle_common(EventType, Event, finished, Data).

-spec aborted(gen_statem:event_type(), term(), data()) ->
    gen_statem:state_function_result(state()).
aborted(internal, {finalize, {Fmt, Args}}, Data) ->
    #data{host_type = HostType, job = Job} = Data,
    AbortReason = abort_job(HostType, Job#broadcast_job.id, Fmt, Args),
    ?LOG_ERROR(#{what => broadcast_worker_aborted,
                 job_id => Job#broadcast_job.id,
                 domain => Job#broadcast_job.domain,
                 reason => AbortReason}),
    {stop, {abort, AbortReason}, Data};
aborted(EventType, Event, Data) ->
    handle_common(EventType, Event, aborted, Data).

%% Common handler for unexpected events
handle_common(EventType, Event, State, Data) ->
    ?LOG_WARNING(#{what => broadcast_worker_unexpected_event,
                   event_type => EventType, event => Event, state => State}),
    {keep_state, Data}.

-spec terminate(term(), state(), data()) -> ok.
terminate(Reason, State, #data{job = Job}) ->
    ?LOG_INFO(#{what => broadcast_worker_terminated,
                job_id => Job#broadcast_job.id,
                state => State,
                reason => Reason}),
    ok.

-spec code_change(term(), state(), data(), term()) -> {ok, state(), data()}.
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec load_job(mongooseim:host_type(), integer()) ->
    {ok, broadcast_job(), broadcast_worker_state() | undefined} | {error, term()}.
load_job(HostType, JobId) ->
    case mod_broadcast_backend:get_job(HostType, JobId) of
        {ok, Job} ->
            WorkerState = case mod_broadcast_backend:get_worker_state(HostType, JobId) of
                {ok, WS} -> WS;
                {error, not_found} -> undefined
            end,
            {ok, Job, WorkerState};
        {error, _} = Error ->
            Error
    end.

-spec load_next_batch(broadcast_job(), broadcast_worker_state()) ->
    {ok, [jid:jid()], binary() | undefined} | {error, term()}.
load_next_batch(_Job, _WorkerState) ->
    %% TODO: Implement recipient paging via auth backend
    %% Returns {ok, Recipients, NewCursor} or {ok, [], undefined} when done
    {error, not_implemented}.

-spec send_message(broadcast_job(), jid:jid()) -> ok | {error, term()}.
send_message(Job, RecipientJid) ->
    try
        MessageId = make_message_id(Job#broadcast_job.id, RecipientJid),
        Stanza = build_message_stanza(Job, RecipientJid, MessageId),
        HostType = Job#broadcast_job.host_type,
        Acc = mongoose_acc:new(#{location => ?LOCATION,
                                 host_type => HostType,
                                 lserver => Job#broadcast_job.domain,
                                 element => Stanza}),
        ejabberd_router:route(Job#broadcast_job.sender, RecipientJid, Acc, Stanza),
        ok
    catch
        Class:Reason:_Stacktrace ->
            ?LOG_WARNING(#{what => broadcast_route_exception,
                           class => Class, reason => Reason}),
            {error, {Class, Reason}}
    end.

-spec make_message_id(integer(), jid:jid()) -> binary().
make_message_id(JobId, RecipientJid) ->
    %% Deterministic message ID: mb- + hex(first 16 bytes of sha256("JobId:recipient"))
    JobIdBin = integer_to_binary(JobId),
    Input = <<JobIdBin/binary, ":", (jid:to_binary(RecipientJid))/binary>>,
    Hash = crypto:hash(sha256, Input),
    <<HashPrefix:16/binary, _/binary>> = Hash,
    <<"mb-", (binary:encode_hex(HashPrefix))/binary>>.

-spec build_message_stanza(broadcast_job(), jid:jid(), binary()) -> exml:element().
build_message_stanza(Job, RecipientJid, MessageId) ->
    BodyEl = #xmlel{name = <<"body">>,
                    children = [#xmlcdata{content = Job#broadcast_job.body}]},

    SubjectEl = #xmlel{name = <<"subject">>,
                       children = [#xmlcdata{content = Job#broadcast_job.subject}]},

    %% XEP-0359 origin-id
    OriginIdEl = #xmlel{name = <<"origin-id">>,
                        attrs = #{<<"xmlns">> => <<"urn:xmpp:sid:0">>,
                                  <<"id">> => MessageId}},

    #xmlel{name = <<"message">>,
           attrs = #{<<"type">> => <<"chat">>,
                     <<"id">> => MessageId,
                     <<"to">> => jid:to_binary(RecipientJid),
                     <<"from">> => jid:to_binary(Job#broadcast_job.sender)},
           children = [SubjectEl, BodyEl, OriginIdEl]}.

-spec persist_worker_state(mongooseim:host_type(), integer(), broadcast_worker_state()) ->
    ok | {error, term()}.
persist_worker_state(HostType, JobId, State) ->
    mod_broadcast_backend:update_worker_state(HostType, JobId, State).

-spec persist_completion(mongooseim:host_type(), broadcast_job()) -> ok | {error, term()}.
persist_completion(HostType, Job) ->
    mod_broadcast_backend:set_job_finished(HostType, Job#broadcast_job.id).

-spec abort_job(mongooseim:host_type(), integer(), string(), [term()]) -> binary().
abort_job(HostType, JobId, Fmt, Args) ->
    Reason = iolist_to_binary(io_lib:format(Fmt, Args)),
    case mod_broadcast_backend:set_job_aborted(HostType, JobId, Reason) of
        ok -> ok;
        {error, PersistError} ->
            ?LOG_WARNING(#{what => broadcast_abort_persist_failed,
                           job_id => JobId, reason => PersistError})
    end,
    Reason.

-spec persist_abort_admin(data()) -> ok.
persist_abort_admin(#data{host_type = HostType, job = Job, state = WorkerState}) ->
    JobId = Job#broadcast_job.id,
    ?LOG_INFO(#{what => broadcast_job_aborted_by_admin,
                job_id => JobId,
                domain => Job#broadcast_job.domain,
                processed => WorkerState#broadcast_worker_state.recipients_processed,
                total_recipients => Job#broadcast_job.recipient_count}),
    case mod_broadcast_backend:set_job_aborted_admin(HostType, JobId) of
        ok -> ok;
        {error, Reason} ->
            ?LOG_WARNING(#{what => broadcast_abort_admin_persist_failed,
                           job_id => JobId, reason => Reason})
    end,
    ok.

-spec mark_job_started(mongooseim:host_type(), integer()) -> ok.
mark_job_started(HostType, JobId) ->
    case mod_broadcast_backend:set_job_started(HostType, JobId) of
        ok -> ok;
        {error, Reason} ->
            ?LOG_WARNING(#{what => broadcast_start_persist_failed,
                           job_id => JobId, reason => Reason})
    end,
    ok.

-spec maybe_init_worker_state(mongooseim:host_type(), integer(), broadcast_worker_state() | undefined) ->
    broadcast_worker_state().
maybe_init_worker_state(_HostType, _JobId, WorkerState) when WorkerState =/= undefined ->
    WorkerState;
maybe_init_worker_state(HostType, JobId, undefined) ->
    mark_job_started(HostType, JobId),
    #broadcast_worker_state{cursor = undefined, recipients_processed = 0}.
