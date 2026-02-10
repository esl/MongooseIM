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

-ifdef(TEST).
-export([make_message_id/2]).
-endif.

-record(data, {
    host_type :: mongooseim:host_type(),
    %% Note: worker cares only about static data in #broadcast_job{},
    %% so recipients_processed in job record is not updated, only in worker state.
    job :: broadcast_job(),
    state :: broadcast_worker_state(),
    batch_recipients_processed :: non_neg_integer(),
    batch_t0 :: integer() | undefined,  % monotonic time when batch started
    current_batch :: [jid:jid()]  % recipients remaining in current batch
}).

-type state() :: loading_batch | sending_batch | finished | aborted.
-type data() :: #data{}.

%%====================================================================
%% API
%%====================================================================

-spec start_link(mongooseim:host_type(), JobId :: broadcast_job_id()) -> gen_statem:start_ret().
start_link(HostType, JobId) ->
    gen_statem:start_link(?MODULE, {HostType, JobId}, []).

-spec stop(pid()) -> ok.
stop(WorkerPid) ->
    try gen_statem:call(WorkerPid, stop, 5000) of
        ok -> ok
    catch
        exit:{timeout, _} ->
            %% Worker didn't respond in time, forcibly kill it
            ?LOG_WARNING(#{what => broadcast_worker_killed_timeout,
                           worker_pid => WorkerPid}),
            exit(WorkerPid, kill),
            ok;
        exit:{normal, _} ->
            %% Worker was already stopping, treat as success
            ok;
        exit:{{error, _} = Error, _} ->
            %% Worker was already stopping with error, ignore but log
            %% TODO: Persist as abort_error execution state
            ?LOG_WARNING(#{what => broadcast_worker_killed_error,
                           worker_pid => WorkerPid,
                           error => Error}),
            ok
    end.

%%====================================================================
%% gen_statem callbacks
%%====================================================================

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    state_functions.

-spec init({mongooseim:host_type(), broadcast_job_id()}) -> gen_statem:init_result(state(), data()).
init({HostType, JobId}) ->
    process_flag(trap_exit, true),
    case load_job(HostType, JobId) of
        {ok, Job, WorkerStateFromDB} ->
            WorkerState = maybe_init_worker_state(WorkerStateFromDB),
            Data = #data{host_type = HostType,
                         job = Job,
                         state = WorkerState,
                         batch_recipients_processed = 0,
                         batch_t0 = undefined,
                         current_batch = []},
            ?LOG_INFO(#{what => broadcast_worker_started,
                        job_id => JobId,
                        domain => Job#broadcast_job.domain,
                        total_recipients => Job#broadcast_job.recipient_count,
                        processed => WorkerState#broadcast_worker_state.recipients_processed}),
            case WorkerState#broadcast_worker_state.finished of
                true ->
                    %% Job was already finished, go directly to finished state
                    %% This can happen if job execution state was not persisted properly
                    %% e.g. due to crash after persisting worker state but before updating job state
                    ?LOG_INFO(#{what => broadcast_worker_already_finished,
                                job_id => JobId,
                                domain => Job#broadcast_job.domain,
                                host_type => HostType}),
                    {ok, finished, Data, [{next_event, internal, finalize}]};
                false ->
                    {ok, loading_batch, Data, [{next_event, internal, load_batch}]}
            end;
        {error, Reason} ->
            ?LOG_ERROR(#{what => broadcast_worker_load_failed,
                         job_id => JobId,
                         host_type => HostType,
                         reason => Reason}),
            {stop, {load_failed, Reason}}
    end.

%%====================================================================
%% State functions
%%====================================================================

-spec loading_batch(gen_statem:event_type(), term(), data()) ->
    gen_statem:state_function_result().
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
                    BatchT0 = erlang:monotonic_time(millisecond),
                    NewData = Data#data{state = NewState,
                                        batch_t0 = BatchT0,
                                        batch_recipients_processed = 0,
                                        current_batch = Recipients},
                    {next_state, sending_batch, NewData,
                     [{next_event, internal, send_one}]};
                {error, Reason} ->
                    FinalizeEvent = {finalize, {load_batch_failed, Reason}},
                    {next_state, aborted, Data, [{next_event, internal, FinalizeEvent}]}
            end;
        {error, Reason} ->
            FinalizeEvent = {finalize, {persist_state_failed, Reason}},
            {next_state, aborted, Data, [{next_event, internal, FinalizeEvent}]}
    end.

-spec sending_batch(gen_statem:event_type(), term(), data()) ->
    gen_statem:state_function_result().
sending_batch(EventType, send_one, #data{current_batch = [], state = WorkerState} = Data)
  when EventType == internal; EventType == state_timeout ->
    NewData = Data#data{batch_t0 = undefined, current_batch = []},
    case WorkerState#broadcast_worker_state.cursor of
        undefined ->
            {next_state, finished, NewData, [{next_event, internal, finalize}]};
        _Cursor ->
            {next_state, loading_batch, NewData, [{next_event, internal, load_batch}]}
    end;
sending_batch(EventType, send_one, #data{current_batch = [RecipientJid | Rest]} = Data)
  when EventType == internal; EventType == state_timeout ->
    #data{host_type = HostType,
          job = Job,
          state = WorkerState,
          batch_t0 = BatchT0,
          batch_recipients_processed = BatchRecipientsProcessed} = Data,
    SendResult = send_message(Job, RecipientJid),
    mongoose_instrument:execute(mod_broadcast_recipients_processed,
                                #{host_type => HostType}, #{count => 1}),
    case SendResult of
        ok ->
            mongoose_instrument:execute(mod_broadcast_recipients_success,
                                        #{host_type => HostType}, #{count => 1});
        {error, Reason} ->
            mongoose_instrument:execute(mod_broadcast_recipients_skipped,
                                        #{host_type => HostType}, #{count => 1}),
            %% Bump to WARNING when we implement some throttling, as these may happen in bursts
            ?LOG_DEBUG(#{what => broadcast_send_failed,
                           job_id => Job#broadcast_job.id,
                           recipient => jid:to_binary(RecipientJid),
                           reason => Reason})
    end,

    %% Update progress
    OldProcessed = WorkerState#broadcast_worker_state.recipients_processed,
    NewState = WorkerState#broadcast_worker_state{recipients_processed = OldProcessed + 1},
    NewBatchRecipientsProcessed = BatchRecipientsProcessed + 1,
    NewData = Data#data{state = NewState,
                        current_batch = Rest,
                        batch_recipients_processed = NewBatchRecipientsProcessed},

    %% Calculate delay for rate limiting
    TargetTime = BatchT0 + (NewBatchRecipientsProcessed * 1000) div Job#broadcast_job.message_rate,
    CurrentTime = erlang:monotonic_time(millisecond),
    DelayMs = max(0, TargetTime - CurrentTime),

    %% Schedule next recipient via state_timeout (allows handling other events)
    {keep_state, NewData, [{state_timeout, DelayMs, send_one}]};
sending_batch({call, From}, stop, _Data) ->
    {stop_and_reply, normal, [{reply, From, ok}]};
sending_batch(EventType, Event, Data) ->
    ?LOG_WARNING(#{what => broadcast_worker_unexpected_event,
                   event_type => EventType, event => Event}),
    {keep_state, Data}.

-spec finished(gen_statem:event_type(), term(), data()) ->
    gen_statem:state_function_result().
finished(internal, finalize, Data) ->
    #data{host_type = HostType, job = Job, state = WorkerState} = Data,
    %% Mark worker state as finished and persist (best effort)
    FinalState = WorkerState#broadcast_worker_state{finished = true},
    case persist_worker_state(HostType, Job#broadcast_job.id, FinalState) of
        ok -> ok;
        {error, Reason} ->
            ?LOG_WARNING(#{what => broadcast_final_state_persist_failed,
                           job_id => Job#broadcast_job.id,
                           domain => Job#broadcast_job.domain,
                           host_type => HostType,
                           reason => Reason})
    end,
    ?LOG_INFO(#{what => broadcast_worker_finished,
                job_id => Job#broadcast_job.id,
                domain => Job#broadcast_job.domain,
                host_type => HostType}),
    {stop, normal, Data}.

-spec aborted(gen_statem:event_type(), {finalize, term()}, data()) ->
    gen_statem:state_function_result().
aborted(internal, {finalize, Error}, #data{job = Job, host_type = HostType} = Data) ->
    ?LOG_ERROR(#{what => broadcast_worker_aborted,
                 job_id => Job#broadcast_job.id,
                 domain => Job#broadcast_job.domain,
                 host_type => HostType,
                 reason => Error}),
    {stop, {error, Error}, Data}.

-spec terminate(term(), state(), data()) -> ok.
terminate(_Reason, _State, #data{job = Job, host_type = HostType}) ->
    ?LOG_INFO(#{what => broadcast_worker_terminated,
                job_id => Job#broadcast_job.id,
                domain => Job#broadcast_job.domain,
                host_type => HostType}),
    ok.

-spec code_change(term(), state(), data(), term()) -> {ok, state(), data()}.
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec load_job(mongooseim:host_type(), broadcast_job_id()) ->
    {ok, broadcast_job(), broadcast_worker_state() | undefined} | {error, term()}.
load_job(HostType, JobId) ->
    try mod_broadcast_backend:get_job(HostType, JobId) of
        {ok, Job} ->
            try mod_broadcast_backend:get_worker_state(HostType, JobId) of
                {ok, WS} ->
                    {ok, Job, WS};
                {error, not_found} ->
                    {ok, Job, undefined}
            catch
                Class:Reason ->
                    ?LOG_WARNING(#{what => broadcast_worker_state_load_failed,
                                   job_id => JobId, host_type => HostType,
                                   class => Class, reason => Reason}),
                    {error, {Class, Reason}}
            end
    catch
        Class:Reason ->
            {error, {Class, Reason}}
    end.

-spec load_next_batch(broadcast_job(), broadcast_worker_state()) ->
    {ok, [jid:jid()], binary() | undefined} | {error, term()}.
load_next_batch(Job, WorkerState) ->
    #broadcast_job{host_type = HostType, domain = Domain, created_at = CreatedAt} = Job,
    #broadcast_worker_state{cursor = Cursor} = WorkerState,

    BatchSize = Job#broadcast_job.message_rate * 10,
    Params = case Cursor of
        undefined ->
            #{limit => BatchSize, snapshot_timestamp => CreatedAt};
        _ ->
            #{limit => BatchSize, cursor => Cursor}
    end,

    case mongoose_gen_auth:get_registered_users_snapshot(ejabberd_auth_rdbms, HostType, Domain, Params) of
        {ok, {Usernames, NewCursor}} ->
            Recipients = [jid:make_bare(Username, Domain) || Username <- Usernames],
            {ok, Recipients, NewCursor};
        {error, _} = Error ->
            Error
    end.

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
            {error, {Class, Reason}}
    end.

-spec make_message_id(broadcast_job_id(), jid:jid()) -> binary().
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

-spec persist_worker_state(mongooseim:host_type(), broadcast_job_id(), broadcast_worker_state()) ->
    ok | {error, term()}.
persist_worker_state(HostType, JobId, State) ->
    try mod_broadcast_backend:update_worker_state(HostType, JobId, State) of
        ok -> ok
    catch
        Class:Reason ->
            ?LOG_WARNING(#{what => broadcast_worker_state_persist_failed,
                           job_id => JobId, host_type => HostType,
                           class => Class, reason => Reason}),
            {error, {Class, Reason}}
    end.

-spec maybe_init_worker_state(broadcast_worker_state() | undefined) ->
    broadcast_worker_state().
maybe_init_worker_state(WorkerState) when WorkerState =/= undefined ->
    WorkerState;
maybe_init_worker_state(undefined) ->
    #broadcast_worker_state{cursor = undefined, recipients_processed = 0, finished = false}.
