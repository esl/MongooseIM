%%%-------------------------------------------------------------------
%%% @doc Broadcast manager process (per host type).
%%%
%%% Manages broadcast job lifecycle
%%%-------------------------------------------------------------------

-module(broadcast_manager).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(gen_server).

-include_lib("jid/include/jid.hrl").

%% API
-export([start_link/1,
         stop/1,
         start_job/2,
         stop_job/3,
         get_live_job_count/1,
         abort_running_jobs_for_domain/3]).

-ignore_xref([start_link/1, stop/1]).

%% Debug API
-export([does_worker_for_job_exist/2,
         get_worker_map/1,
         get_supervisor_children/1,
         get_sync_mode/1]).
-ignore_xref([does_worker_for_job_exist/2,
              get_worker_map/1,
              get_supervisor_children/1,
              get_sync_mode/1]).

%% Error types
-export_type([create_job_error/0,
              stop_job_error/0,
              validation_error/0]).

-type create_job_error() :: running_job_limit_exceeded | validation_error() |
                            temporarily_unavailable | term().
-type stop_job_error() :: not_live.
-type validation_error() :: sender_not_found
                         | no_recipients
                         | cannot_count_recipients
                         | {bad_parameter, atom()}.

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_continue/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("mongoose.hrl").
-include("mod_broadcast.hrl").

-type worker_info() :: #{pid := pid(), domain := jid:lserver()}.
-type worker_map() :: #{broadcast_job_id() => worker_info()}.

-type sync_mode() :: normal | emergency.

-record(job_sync, {
    mode = normal :: sync_mode(),
    tref :: reference() | undefined
}).

-record(state, {
    host_type :: mongooseim:host_type(),
    worker_map :: worker_map(),
    job_sync :: #job_sync{}
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link(HostType :: mongooseim:host_type()) -> {ok, pid()} | {error, term()}.
start_link(HostType) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:start_link({local, ProcName}, ?MODULE, HostType, []).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:stop(ProcName).

% In future iterations there will be a clear distinction between creating a job
% and actually starting its workers.
-spec start_job(mongooseim:host_type(), job_spec()) ->
    {ok, JobId :: broadcast_job_id()} | {error, create_job_error() | term()}.
start_job(HostType, JobSpec) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:call(ProcName, {start_job, JobSpec}).

-spec stop_job(Node :: node(), mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    ok | {error, stop_job_error()}.
stop_job(Node, HostType, JobId) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:call({ProcName, Node}, {stop_job, JobId}).

-spec get_live_job_count(mongooseim:host_type()) -> non_neg_integer().
get_live_job_count(HostType) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:call(ProcName, get_live_job_count).

-spec abort_running_jobs_for_domain(node(), mongooseim:host_type(), jid:lserver()) -> ok.
abort_running_jobs_for_domain(Node, HostType, Domain) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:call({ProcName, Node}, {abort_running_jobs_for_domain, Domain}).

%% Debug API

-spec does_worker_for_job_exist(mongooseim:host_type(), broadcast_job_id()) -> boolean().
does_worker_for_job_exist(HostType, JobId) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:call(ProcName, {does_worker_for_job_exist, JobId}).

-spec get_worker_map(mongooseim:host_type()) -> worker_map().
get_worker_map(HostType) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:call(ProcName, get_worker_map).

-spec get_supervisor_children(mongooseim:host_type()) ->
    [{term(), undefined | pid() | restarting, worker | supervisor, [module()] | dynamic}].
get_supervisor_children(HostType) ->
    supervisor:which_children(get_sup_name(HostType)).

-spec get_sync_mode(mongooseim:host_type()) -> sync_mode().
get_sync_mode(HostType) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:call(ProcName, get_sync_mode).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(mongooseim:host_type()) -> {ok, #state{}, {continue, initial_sync}}.
init(HostType) ->
    process_flag(trap_exit, true),
    JobSync = #job_sync{mode = normal,
                        tref = undefined},
    State = #state{host_type = HostType,
                   worker_map = #{},
                   job_sync = JobSync},
    {ok, State, {continue, initial_sync}}.

handle_continue({start_worker_for, JobId, Domain}, #state{host_type = HostType, worker_map = WorkerMap0} = State) ->
    case start_and_monitor_worker(HostType, JobId) of
        {ok, WorkerPid} ->
            NewState = State#state{worker_map = WorkerMap0#{JobId => #{pid => WorkerPid,
                                                                     domain => Domain}}},
            persist_job_started(HostType, Domain, JobId),
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR(#{what => broadcast_start_worker_failed,
                         job_id => JobId,
                         host_type => HostType,
                         domain => Domain,
                         reason => Reason}),
            persist_job_aborted_error(HostType, Domain, JobId, Reason),
            {noreply, State}
    end;
handle_continue(initial_sync, #state{host_type = HostType} = State) ->
    WorkerMap = map_workers(HostType),
    maps:foreach(fun monitor_worker/2, WorkerMap),
    NewState = synchronize_jobs(State#state{worker_map = WorkerMap}),
    {noreply, NewState}.

handle_call({start_job, _JobSpec}, _From,
            #state{job_sync = #job_sync{mode = emergency}} = State) ->
    {reply, {error, temporarily_unavailable}, State};
handle_call({start_job, #{domain := Domain} = JobSpec}, _From, #state{host_type = HostType} = State) ->
    %% Get recipient count from auth backend
    %% Small race condition risk: a user may be created between getting the count
    %% and creating the job (which sets the snapshot timestamp).
    %% It means the total count may be slightly off, but this is acceptable for now
    RecipientCount = catch ejabberd_auth:get_vh_registered_users_number(Domain),
    JobSpecWithCount = JobSpec#{recipient_count => RecipientCount},
    LeaseTime = mod_broadcast:lease_time(HostType),
    %% If RecipientCount is an error tuple, it will be handled in validation
    case do_create_job(HostType, validate_job_spec(HostType, JobSpecWithCount), LeaseTime) of
        {ok, JobId} ->
            {reply, {ok, JobId}, State, {continue, {start_worker_for, JobId, Domain}}};
        {error, _Reason} = Error ->
            {reply, Error, State}
    end;

handle_call({stop_job, JobId}, _From, State) ->
    case maps:get(JobId, State#state.worker_map, undefined) of
        #{pid := WorkerPid, domain := Domain} when is_pid(WorkerPid) ->
            stop_worker_by_admin(State#state.host_type, Domain, JobId, WorkerPid),
            NewState = State#state{worker_map = maps:remove(JobId, State#state.worker_map)},
            {reply, ok, NewState};
        _ ->
            {reply, {error, not_live}, State}
    end;
handle_call(get_live_job_count, _From, State) ->
    Count = maps:size(State#state.worker_map),
    {reply, Count, State};
handle_call({abort_running_jobs_for_domain, Domain}, _From, State) ->
    NewState = do_abort_running_jobs_for_domain(State, Domain),
    {reply, ok, NewState};
handle_call(get_worker_map, _From, State) ->
    {reply, State#state.worker_map, State};
handle_call({does_worker_for_job_exist, JobId}, _From, State) ->
    Reply = maps:is_key(JobId, State#state.worker_map),
    {reply, Reply, State};
handle_call(get_sync_mode, _From, State) ->
    {reply, (State#state.job_sync)#job_sync.mode, State};
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({{'DOWN', JobId}, _Ref, process, Pid, Reason}, State) ->
    handle_worker_down(JobId, Pid, Reason, State);
handle_info({timeout, TRef, sync_jobs},
            #state{job_sync = #job_sync{tref = TRef}} = State) ->
    NewState = synchronize_jobs(State),
    {noreply, NewState};
handle_info({timeout, _OldTRef, sync_jobs}, State) ->
    %% Stale timer from a previous scheduling cycle, ignore
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Handler implementations
%%====================================================================

-spec do_create_job(mongooseim:host_type(), job_spec() | {error, validation_error()},
                    LeaseTime :: non_neg_integer()) ->
    {ok, JobId :: broadcast_job_id()} | {error, create_job_error()}.
do_create_job(HostType, #{domain := Domain} = JobSpec, LeaseTime) ->
    try mod_broadcast_backend:create_job(HostType, JobSpec, LeaseTime) of
        {ok, JobId} ->
            ?LOG_INFO(#{what => broadcast_job_created,
                                job_id => JobId,
                                domain => Domain,
                                host_type => HostType}),
            {ok, JobId};
        {error, running_job_limit_exceeded} = Error ->
            ?LOG_WARNING(#{what => broadcast_job_running_limit_exceeded,
                            domain => Domain,
                            host_type => HostType}),
            Error
    catch
        Class:Reason ->
            ?LOG_ERROR(#{what => broadcast_create_job_failed,
                         host_type => HostType,
                         domain => Domain,
                         class => Class,
                         reason => Reason}),
            {error, Reason}
    end;
do_create_job(_HostType, {error, _} = ValidationError, _LeaseTime) ->
    ValidationError.

-spec handle_worker_down(broadcast_job_id(), pid(), term(), #state{}) ->
    {noreply, #state{}}.
handle_worker_down(JobId, Pid, Reason, #state{host_type = HostType, worker_map = WorkerMap} = State) ->
    case maps:get(JobId, WorkerMap, undefined) of
        #{pid := Pid, domain := Domain} ->
            NewWorkerMap = maps:remove(JobId, WorkerMap),
            case Reason of
                normal ->
                    persist_job_finished(HostType, Domain, JobId);
                _ ->
                    %% TODO: implement proper retry mechanism
                    persist_job_aborted_error(HostType, Domain, JobId, Reason)
            end,
            {noreply, State#state{worker_map = NewWorkerMap}};
        _ ->
            ?LOG_WARNING(#{what => broadcast_unknown_worker_down,
                           job_id => JobId,
                           pid => Pid,
                           host_type => HostType}),
            {noreply, State}
    end.

-spec do_abort_running_jobs_for_domain(#state{}, jid:lserver()) -> #state{}.
do_abort_running_jobs_for_domain(#state{host_type = HostType, worker_map = WorkerMap0} = State, Domain) ->
    NewWorkerMap = maps:filter(fun(JobId, #{pid := Pid, domain := JobDomain}) when JobDomain =:= Domain ->
            stop_worker_by_admin(HostType, Domain, JobId, Pid),
            false;
        (_JobId, _WorkerInfo) ->
            true
        end, WorkerMap0),
    State#state{worker_map = NewWorkerMap}.

%%====================================================================
%% Job execution state persistence
%%====================================================================

-spec persist_job_started(mongooseim:host_type(), jid:lserver(), broadcast_job_id()) -> ok.
persist_job_started(HostType, Domain, JobId) ->
    mongoose_instrument:execute(mod_broadcast_jobs_started,
                                #{host_type => HostType}, #{count => 1}),
    ?LOG_INFO(#{what => broadcast_job_started,
                job_id => JobId,
                domain => Domain,
                host_type => HostType}),
    try mod_broadcast_backend:set_job_started(HostType, JobId) of
        _ -> ok
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => broadcast_set_job_started_failed,
                         job_id => JobId,
                         domain => Domain,
                         host_type => HostType,
                         class => Class,
                         reason => Reason,
                         stacktrace => Stacktrace})
    end.

-spec persist_job_aborted_by_admin(mongooseim:host_type(), jid:lserver(), broadcast_job_id()) -> ok.
persist_job_aborted_by_admin(HostType, Domain, JobId) ->
    mongoose_instrument:execute(mod_broadcast_jobs_aborted_admin,
                                #{host_type => HostType}, #{count => 1}),
    ?LOG_INFO(#{what => broadcast_job_aborted_by_admin,
                job_id => JobId,
                domain => Domain,
                host_type => HostType}),
    try
        mod_broadcast_backend:set_job_aborted_admin(HostType, JobId),
        mod_broadcast_backend:remove_ownership(HostType, JobId)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => broadcast_set_job_aborted_admin_failed,
                         job_id => JobId,
                         domain => Domain,
                         host_type => HostType,
                         class => Class,
                         reason => Reason,
                         stacktrace => Stacktrace})
    end.

-spec persist_job_finished(mongooseim:host_type(), jid:lserver(), broadcast_job_id()) -> ok.
persist_job_finished(HostType, Domain, JobId) ->
    mongoose_instrument:execute(mod_broadcast_jobs_finished,
                                #{host_type => HostType}, #{count => 1}),
    ?LOG_INFO(#{what => broadcast_job_finished,
                job_id => JobId,
                domain => Domain,
                host_type => HostType}),
    try
        mod_broadcast_backend:set_job_finished(HostType, JobId),
        mod_broadcast_backend:remove_ownership(HostType, JobId)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => broadcast_set_job_finished_failed,
                         job_id => JobId,
                         domain => Domain,
                         host_type => HostType,
                         class => Class,
                         reason => Reason,
                         stacktrace => Stacktrace})
    end.

-spec persist_job_aborted_error(mongooseim:host_type(), jid:lserver(), broadcast_job_id(), term()) -> ok.
persist_job_aborted_error(HostType, Domain, JobId, Reason) ->
    mongoose_instrument:execute(mod_broadcast_jobs_aborted_error,
                                #{host_type => HostType}, #{count => 1}),
    ?LOG_ERROR(#{what => broadcast_job_aborted_error,
                 job_id => JobId,
                 domain => Domain,
                 host_type => HostType}),
    ReasonBin = iolist_to_binary(io_lib:format("~p", [Reason])),
    try
        mod_broadcast_backend:set_job_aborted_error(HostType, JobId, ReasonBin),
        mod_broadcast_backend:remove_ownership(HostType, JobId)
    catch
        Class:BackendReason:Stacktrace ->
            ?LOG_ERROR(#{what => broadcast_set_job_aborted_error_failed,
                         job_id => JobId,
                         domain => Domain,
                         host_type => HostType,
                         class => Class,
                         reason => BackendReason,
                         stacktrace => Stacktrace})
    end.

%% ===================================================================
%% Worker lifecycle management
%% ===================================================================

-spec start_and_monitor_worker(mongooseim:host_type(), broadcast_job_id()) ->
    {ok, pid()} | {error, term()}.
start_and_monitor_worker(HostType, JobId) ->
    SupName = get_sup_name(HostType),
    ChildSpec = #{id => JobId,
                  start => {broadcast_worker, start_link, [HostType, JobId]},
                  restart => temporary,
                  shutdown => 5000,
                  type => worker,
                  modules => [broadcast_worker]},
    case supervisor:start_child(SupName, ChildSpec) of
        {ok, WorkerPid} ->
            monitor_worker(JobId, WorkerPid),
            {ok, WorkerPid};
        {error, Reason} ->
            {error, Reason}
    end.

-spec monitor_worker(broadcast_job_id(), pid() | worker_info()) -> reference().
monitor_worker(JobId, #{pid := Pid}) ->
    monitor_worker(JobId, Pid);
monitor_worker(JobId, Pid) ->
    erlang:monitor(process, Pid, [{tag, {'DOWN', JobId}}]).

-spec stop_worker_by_admin(mongooseim:host_type(), jid:lserver(), broadcast_job_id(), pid()) -> ok.
stop_worker_by_admin(HostType, Domain, JobId, WorkerPid) ->
    stop_worker(HostType, JobId, WorkerPid),
    persist_job_aborted_by_admin(HostType, Domain, JobId),
    ok.

-spec stop_worker(mongooseim:host_type(), broadcast_job_id(), pid()) -> ok.
stop_worker(HostType, JobId, WorkerPid) ->
    broadcast_worker:stop(WorkerPid),
    receive
        {{'DOWN', JobId}, _Ref, process, WorkerPid, _Reason} ->
            ok
    after 6000 ->
        ?LOG_WARNING(#{what => broadcast_worker_stop_timeout,
                        job_id => JobId,
                        host_type => HostType,
                        pid => WorkerPid}),
        ok
    end.

-spec get_sup_name(mongooseim:host_type()) -> atom().
get_sup_name(HostType) ->
    gen_mod:get_module_proc(HostType, broadcast_jobs_sup).

%%====================================================================
%% Job synchronization
%%====================================================================

-spec map_workers(mongooseim:host_type()) -> worker_map().
map_workers(HostType) ->
    Children = get_supervisor_children(HostType),
    lists:foldl(fun({Id, Pid, _Type, _Modules}, Acc) when is_pid(Pid) ->
            case broadcast_worker:get_domain(Pid) of
                {ok, Domain} ->
                    Acc#{Id => #{pid => Pid, domain => Domain}};
                noproc ->
                    Acc
            end;
        (_Other, Acc) ->
            Acc
    end, #{}, Children).

-spec synchronize_jobs(#state{}) -> #state{}.
synchronize_jobs(#state{host_type = HostType} = State) ->
    case try_take_renew_and_fetch(HostType) of
        {ok, RunningJobs, TakenJobIDs} ->
            ?LOG_INFO(#{what => broadcast_taken_oven_jobs,
                        host_type => HostType,
                        taken_job_ids => TakenJobIDs}),
            %% We need to stop workers for taken jobs in case we were experiencing problems
            %% with the database and another node took over our jobs while we were in emergency mode.
            %% They are paused and their state may be outdated.
            stop_workers_for_jobs(HostType, State#state.worker_map, TakenJobIDs),
            NewWorkerMap = maps:without(TakenJobIDs, State#state.worker_map),
            handle_sync_success(State#state{worker_map = NewWorkerMap}, RunningJobs);
        {error, Reason} ->
            handle_sync_failure(State, Reason)
    end.

-spec try_take_renew_and_fetch(mongooseim:host_type()) ->
    {ok, [broadcast_job()], [broadcast_job_id()]} | {error, term()}.
try_take_renew_and_fetch(HostType) ->
    LeaseTime = mod_broadcast:lease_time(HostType),
    try
        {ok, TakenJobIDs} = mod_broadcast_backend:take_expired_jobs(HostType, LeaseTime),
        mod_broadcast_backend:renew_ownership(HostType, LeaseTime),
        {ok, RunningJobs} = mod_broadcast_backend:get_running_jobs(HostType),
        {ok, RunningJobs, TakenJobIDs}
    catch
        Class:Reason ->
            ?LOG_ERROR(#{what => broadcast_job_synchronization_failed,
                         host_type => HostType,
                         class => Class,
                         reason => Reason}),
        {error, Reason}
    end.

-spec handle_sync_success(#state{}, [broadcast_job()]) -> #state{}.
handle_sync_success(#state{host_type = HostType,
                           worker_map = WorkerMap0,
                           job_sync = JobSync0} = State, Jobs) ->
    PreviousMode = JobSync0#job_sync.mode,
    WorkerMap = reconcile_workers(HostType, Jobs, WorkerMap0),
    ?LOG_DEBUG(#{what => broadcast_job_synchronization_succeeded,
                 host_type => HostType,
                 owned_jobs => maps:size(WorkerMap)}),
    JobSync1 = JobSync0#job_sync{mode = normal},
    State1 = State#state{worker_map = WorkerMap, job_sync = JobSync1},
    NewWorkerMap = case PreviousMode of
        emergency ->
            ?LOG_INFO(#{what => broadcast_emergency_recovery_succeeded,
                        host_type => HostType}),
            resume_owned_workers(WorkerMap);
        normal ->
            WorkerMap
    end,
    schedule_sync(State1#state{worker_map = NewWorkerMap}).

-spec handle_sync_failure(#state{}, term()) -> #state{}.
handle_sync_failure(#state{host_type = HostType, job_sync = #job_sync{mode = emergency}} = State, _Reason) ->
    ?LOG_WARNING(#{what => broadcast_emergency_sync_failed, host_type => HostType}),
    schedule_sync(State);
handle_sync_failure(#state{host_type = HostType, job_sync = JobSync0} = State, _Reason) ->
    ?LOG_WARNING(#{what => broadcast_entering_emergency_mode,
                    host_type => HostType}),
    NewWorkerMap = pause_all_workers(State#state.worker_map),
    schedule_sync(State#state{job_sync = JobSync0#job_sync{mode = emergency}, worker_map = NewWorkerMap}).

-spec reconcile_workers(mongooseim:host_type(), [broadcast_job()], worker_map()) -> worker_map().
reconcile_workers(HostType, OwnedJobs, OldWorkerMap) ->
    NewWorkerMap = lists:foldl(fun(Job, AccWorkerMap) ->
        resume_job_if_necessary(HostType, Job, OldWorkerMap, AccWorkerMap)
    end, #{}, OwnedJobs),
    %% Stop workers that are locally running but no longer owned
    NoLongerOwnedJobIDs = maps:keys(OldWorkerMap) -- maps:keys(NewWorkerMap),
    ?LOG_INFO(#{what => broadcast_jobs_no_longer_owned,
                host_type => HostType,
                lost_job_ids => NoLongerOwnedJobIDs}),
    stop_workers_for_jobs(HostType, OldWorkerMap, NoLongerOwnedJobIDs),
    NewWorkerMap.

-spec resume_job_if_necessary(mongooseim:host_type(), broadcast_job(),
                               worker_map(), worker_map()) ->
    worker_map().
resume_job_if_necessary(HostType, #broadcast_job{id = JobId} = Job, OldWorkerMap, WorkerMap0) ->
    case maps:get(JobId, OldWorkerMap, undefined) of
        undefined ->
            case start_and_monitor_worker(HostType, JobId) of
                {ok, WorkerPid} ->
                    ?LOG_INFO(#{what => broadcast_job_resumed,
                                job_id => JobId,
                                domain => Job#broadcast_job.domain,
                                host_type => HostType}),
                    WorkerMap0#{JobId => #{pid => WorkerPid, domain => Job#broadcast_job.domain}};
                {error, Reason} ->
                    ?LOG_ERROR(#{what => broadcast_resume_worker_failed,
                                 job_id => JobId,
                                 host_type => HostType,
                                 reason => Reason}),
                    WorkerMap0
            end;
        #{pid := Pid} ->
            ?LOG_DEBUG(#{what => broadcast_job_already_live, job_id => JobId}),
            WorkerMap0#{JobId => #{pid => Pid, domain => Job#broadcast_job.domain}}
    end.

-spec stop_workers_for_jobs(mongooseim:host_type(), worker_map(), [broadcast_job_id()]) -> ok.
stop_workers_for_jobs(HostType, WorkerMap, JobIds) ->
    maps:foreach(fun(JobId, #{pid := WorkerPid}) ->
        stop_worker(HostType, JobId, WorkerPid)
    end, maps:with(JobIds, WorkerMap)).

%%====================================================================
%% Pause / resume orchestration
%%====================================================================

-spec pause_all_workers(worker_map()) -> worker_map().
pause_all_workers(WorkerMap) ->
    maps:filter(fun(_JobId, #{pid := Pid}) ->
        broadcast_worker:pause(Pid) == ok
    end, WorkerMap).

-spec resume_owned_workers(worker_map()) -> worker_map().
resume_owned_workers(WorkerMap) ->
    maps:filter(fun(_JobId, #{pid := Pid}) ->
        broadcast_worker:resume(Pid) == ok
    end, WorkerMap).

%%====================================================================
%% Timer scheduling
%%====================================================================

-spec schedule_sync(#state{}) -> #state{}.
schedule_sync(#state{host_type = HostType, job_sync = JobSync0} = State) ->
    #job_sync{mode = Mode, tref = OldTRef} = JobSync0,
    cancel_timer(OldTRef),
    Interval = compute_sync_interval(HostType, Mode),
    NewTRef = erlang:start_timer(Interval, self(), sync_jobs),
    JobSync1 = JobSync0#job_sync{tref = NewTRef},
    State#state{job_sync = JobSync1}.

-spec cancel_timer(reference() | undefined) -> ok.
cancel_timer(undefined) ->
    ok;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef, [{async, true}, {info, false}]),
    ok.

-spec compute_sync_interval(mongooseim:host_type(), sync_mode()) -> pos_integer().
compute_sync_interval(HostType, normal) ->
    LeaseTimeSec = mod_broadcast:lease_time(HostType),
    trunc(LeaseTimeSec * 1000 * 2 / 3);
compute_sync_interval(HostType, emergency) ->
    LeaseTimeSec = mod_broadcast:lease_time(HostType),
    min(30000, trunc(LeaseTimeSec * 1000 / 10)).

%%====================================================================
%% Validation
%%====================================================================

-spec validate_job_spec(mongooseim:host_type(), job_spec()) ->
    job_spec() | {error, validation_error()}.
validate_job_spec(HostType, JobSpec) ->
    Validations = [
        fun() -> validate_message_rate(JobSpec) end,
        fun() -> validate_sender(HostType, JobSpec) end,
        fun() -> validate_recipient_count(JobSpec) end
    ]
    ++
    [
        fun() -> validate_string_param(Param, Min, Max, JobSpec) end
        || {Param, Min, Max} <- [
            {name, 1, 250},
            {subject, 0, 1024},
            {body, 1, 16000} % Arbitrary limit, may become configurable in the future
        ]
    ],
    case run_validations(Validations) of
        ok -> JobSpec;
        {error, _} = Error -> Error
    end.

-spec run_validations([fun(() -> ok | {error, validation_error()})]) ->
    ok | {error, validation_error()}.
run_validations([]) ->
    ok;
run_validations([Validate | Rest]) ->
    case Validate() of
        ok -> run_validations(Rest);
        {error, _Reason} = Error -> Error
    end.

-spec validate_message_rate(job_spec()) -> ok | {error, {bad_parameter, message_rate}}.
validate_message_rate(#{message_rate := Rate}) when is_integer(Rate), Rate >= 1, Rate =< 1000 ->
    ok;
validate_message_rate(_) ->
    {error, {bad_parameter, message_rate}}.

-spec validate_sender(mongooseim:host_type(), job_spec()) ->
    ok | {error, sender_not_found}.
validate_sender(HostType, #{sender := SenderJid = #jid{lserver = Domain}, domain := Domain}) ->
    case ejabberd_auth:does_user_exist(HostType, SenderJid, stored) of
        true ->
            ok;
        false ->
            {error, sender_not_found}
    end;
validate_sender(_HostType, _JobSpec) ->
    {error, sender_not_found}.

-spec validate_recipient_count(job_spec()) ->
    ok | {error, no_recipients | cannot_count_recipients}.
validate_recipient_count(#{recipient_count := Count}) when is_integer(Count), Count > 0 ->
    ok;
validate_recipient_count(#{recipient_count := 0}) ->
    {error, no_recipients};
validate_recipient_count(_) ->
    {error, cannot_count_recipients}.

-spec validate_string_param(atom(), non_neg_integer(), pos_integer(), job_spec()) ->
    ok | {error, {bad_parameter, atom()}}.
validate_string_param(Param, Min, Max, JobSpec) ->
    case string:length(maps:get(Param, JobSpec)) of
        Len when Len >= Min, Len =< Max ->
            ok;
        _ ->
            {error, {bad_parameter, Param}}
    end.
