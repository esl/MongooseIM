%%%-------------------------------------------------------------------
%%% @doc Broadcast manager process (per host type).
%%%
%%% Manages broadcast job lifecycle
%%%-------------------------------------------------------------------

-module(broadcast_manager).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(gen_server).

%% API
-export([start_link/1,
         start_job/2,
         stop_job/2]).

%% Debug API
-export([abort_running_jobs_for_domain/2,
         does_worker_for_job_exist/2,
         get_worker_map/1,
         get_supervisor_children/1]).

%% Error types
-export_type([create_job_error/0,
              stop_job_error/0,
              validation_error/0]).

-type create_job_error() :: already_running | validation_error() | term().
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

-type worker_map() :: #{broadcast_job_id() => pid()}.

-record(state, {
    host_type :: mongooseim:host_type(),
    worker_map :: worker_map()
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link(HostType :: mongooseim:host_type()) -> {ok, pid()} | {error, term()}.
start_link(HostType) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:start_link({local, ProcName}, ?MODULE, HostType, []).

% In future iterations there will be a clear distinction between creating a job
% and actually starting its workers.
-spec start_job(mongooseim:host_type(), job_spec()) ->
    {ok, JobId :: broadcast_job_id()} | {error, create_job_error() | term()}.
start_job(HostType, JobSpec) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:call(ProcName, {start_job, JobSpec}).

-spec stop_job(mongooseim:host_type(), JobId :: broadcast_job_id()) -> ok | {error, stop_job_error()}.
stop_job(HostType, JobId) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:call(ProcName, {stop_job, JobId}).

%% @doc Abort all running broadcast jobs for a specific domain.
%% This is currently test-only but will become a proper user-facing API
%% in a future iteration to allow domain administrators to abort all
%% broadcasts in their domain at once.
-spec abort_running_jobs_for_domain(mongooseim:host_type(), jid:lserver()) -> ok.
abort_running_jobs_for_domain(HostType, Domain) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:call(ProcName, {abort_running_jobs_for_domain, Domain}).

-spec does_worker_for_job_exist(mongooseim:host_type(), broadcast_job_id()) -> boolean().
does_worker_for_job_exist(HostType, JobId) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:call(ProcName, {does_worker_for_job_exist, JobId}).

-spec get_worker_map(mongooseim:host_type()) -> worker_map().
get_worker_map(HostType) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:call(ProcName, get_worker_map).

-spec get_supervisor_children(mongooseim:host_type()) -> [supervisor:child_spec()].
get_supervisor_children(HostType) ->
    supervisor:which_children(get_sup_name(HostType)).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(mongooseim:host_type()) -> {ok, #state{}, {continue, resume_jobs}}.
init(HostType) ->
    process_flag(trap_exit, true),
    State = #state{host_type = HostType, worker_map = #{}},
    {ok, State, {continue, resume_jobs}}.

handle_continue({start_worker_for, JobId}, #state{host_type = HostType, worker_map = WorkerMap0} = State) ->
    case start_and_monitor_worker(HostType, JobId) of
        {ok, WorkerPid} ->
            NewState = State#state{worker_map = WorkerMap0#{JobId => WorkerPid}},
            persist_job_started(HostType, JobId),
            ?LOG_INFO(#{what => broadcast_job_started, job_id => JobId}),
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR(#{what => broadcast_start_worker_failed,
                         job_id => JobId,
                         reason => Reason}),
            {noreply, State}
    end;
handle_continue(resume_jobs, State) ->
    NewState = resume_running_jobs(State),
    {noreply, NewState}.

handle_call({start_job, #{domain := Domain} = JobSpec}, _From, #state{host_type = HostType} = State) ->
    %% Get recipient count from auth backend
    %% Small race condition risk: a user may be created between getting the count
    %% and creating the job (which sets the snapshot timestamp).
    %% It means the total count may be slightly off, but this is acceptable for now
    RecipientCount = catch ejabberd_auth:get_vh_registered_users_number(Domain),
    JobSpecWithCount = JobSpec#{recipient_count => RecipientCount},
    %% If RecipientCount is an error tuple, it will be handled in validation
    case do_create_job(HostType, validate_job_spec(HostType, JobSpecWithCount)) of
        {ok, JobId} ->
            {reply, {ok, JobId}, State, {continue, {start_worker_for, JobId}}};
        {error, _Reason} = Error ->
            {reply, Error, State}
    end;

handle_call({stop_job, JobId}, _From, State) ->
    case maps:get(JobId, State#state.worker_map, undefined) of
        WorkerPid when is_pid(WorkerPid) ->
            NewState = stop_worker_by_admin(State#state.host_type, JobId, WorkerPid, State),
            {reply, ok, NewState};
        _ ->
            {reply, {error, not_live}, State}
    end;
handle_call({abort_running_jobs_for_domain, Domain}, _From, State) ->
    NewState = do_abort_running_jobs_for_domain(State, Domain),
    {reply, ok, NewState};
handle_call(get_worker_map, _From, State) ->
    {reply, State#state.worker_map, State};
handle_call({does_worker_for_job_exist, JobId}, _From, State) ->
    Reply = maps:is_key(JobId, State#state.worker_map),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({{'DOWN', JobId}, _Ref, process, Pid, Reason}, State) ->
    handle_worker_down(JobId, Pid, Reason, State);
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Handler implementations
%%====================================================================

-spec do_create_job(mongooseim:host_type(), job_spec() | {error, validation_error()}) ->
    {ok, JobId :: broadcast_job_id()} | {error, create_job_error()}.
do_create_job(HostType, #{domain := Domain, sender := Sender} = JobSpec) ->
    try mod_broadcast_backend:create_job(HostType, JobSpec) of
        {ok, JobId} ->
            ?LOG_INFO(#{what => broadcast_job_created,
                                job_id => JobId,
                                domain => Domain,
                                host_type => HostType,
                                sender => jid:to_binary(Sender)}),
            {ok, JobId};
        {error, already_running} = Error ->
            ?LOG_WARNING(#{what => broadcast_job_already_running,
                            domain => Domain,
                            host_type => HostType}),
            Error
    catch
        Class:Reason ->
            ?LOG_ERROR(#{what => broadcast_create_job_failed,
                         host_type => HostType,
                         class => Class, reason => Reason}),
            {error, Reason}
    end;
do_create_job(_HostType, {error, _} = ValidationError) ->
    ValidationError.

-spec handle_worker_down(broadcast_job_id(), pid(), term(), #state{}) ->
    {noreply, #state{}}.
handle_worker_down(JobId, Pid, Reason, #state{host_type = HostType, worker_map = WorkerMap} = State) ->
    case maps:get(JobId, WorkerMap, undefined) of
        Pid ->
            NewWorkerMap = maps:remove(JobId, WorkerMap),
            case Reason of
                normal ->
                    persist_job_finished(HostType, JobId);
                _ ->
                    %% TODO: implement proper retry mechanism
                    persist_job_aborted_error(HostType, JobId, Reason)
            end,
            {noreply, State#state{worker_map = NewWorkerMap}};
        _ ->
            ?LOG_WARNING(#{what => broadcast_unknown_worker_down,
                           job_id => JobId, pid => Pid}),
            {noreply, State}
    end.

-spec do_abort_running_jobs_for_domain(#state{}, jid:lserver()) -> #state{}.
do_abort_running_jobs_for_domain(#state{host_type = HostType, worker_map = WorkerMap} = State, Domain) ->
    try mod_broadcast_backend:get_running_jobs(HostType) of
        {ok, Jobs} ->
            DomainJobs = [Job#broadcast_job.id || Job <- Jobs, Job#broadcast_job.domain =:= Domain],
            lists:foldl(fun({WorkerId, WorkerPid}, AccState) ->
                    case lists:member(WorkerId, DomainJobs) of
                        true ->
                            ?LOG_INFO(#{what => aborting_broadcast_job,
                                        job_id => WorkerId,
                                        domain => Domain}),
                            stop_worker_by_admin(HostType, WorkerId, WorkerPid, AccState);
                        false ->
                            AccState
                    end
                end, State, maps:to_list(WorkerMap))
    catch
        Class:Reason ->
            ?LOG_ERROR(#{what => broadcast_abort_get_jobs_failed,
                         domain => Domain, class => Class, reason => Reason}),
            State
    end.

%%====================================================================
%% Job execution state persistence
%%====================================================================

-spec persist_job_started(mongooseim:host_type(), broadcast_job_id()) -> ok.
persist_job_started(HostType, JobId) ->
    mongoose_instrument:execute(mod_broadcast_jobs_started,
                                #{host_type => HostType}, #{count => 1}),
    catch mod_broadcast_backend:set_job_started(HostType, JobId).

-spec persist_job_aborted_by_admin(mongooseim:host_type(), broadcast_job_id()) -> ok.
persist_job_aborted_by_admin(HostType, JobId) ->
    mongoose_instrument:execute(mod_broadcast_jobs_aborted_admin,
                                #{host_type => HostType}, #{count => 1}),
    ?LOG_INFO(#{what => broadcast_job_aborted_by_admin,
                job_id => JobId}),
    catch mod_broadcast_backend:set_job_aborted_admin(HostType, JobId).

-spec persist_job_finished(mongooseim:host_type(), broadcast_job_id()) -> ok.
persist_job_finished(HostType, JobId) ->
    mongoose_instrument:execute(mod_broadcast_jobs_finished,
                                #{host_type => HostType}, #{count => 1}),
    ?LOG_INFO(#{what => broadcast_job_finished,
                job_id => JobId}),
    catch mod_broadcast_backend:set_job_finished(HostType, JobId).

-spec persist_job_aborted_error(mongooseim:host_type(), broadcast_job_id(), binary()) -> ok.
persist_job_aborted_error(HostType, JobId, Reason) ->
    mongoose_instrument:execute(mod_broadcast_jobs_aborted_error,
                                #{host_type => HostType}, #{count => 1}),
    ?LOG_ERROR(#{what => broadcast_job_aborted_error,
                 job_id => JobId,
                 reason => Reason}),
    ReasonBin = iolist_to_binary(io_lib:format("~p", [Reason])),
    catch mod_broadcast_backend:set_job_aborted_error(HostType, JobId, ReasonBin).

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

-spec monitor_worker(broadcast_job_id(), pid()) -> reference().
monitor_worker(JobId, Pid) ->
    erlang:monitor(process, Pid, [{tag, {'DOWN', JobId}}]).

-spec stop_worker_by_admin(mongooseim:host_type(), broadcast_job_id(), pid(), #state{}) -> #state{}.
stop_worker_by_admin(HostType, JobId, WorkerPid, #state{worker_map = WorkerMap0} = State) ->
    ok = broadcast_worker:stop(WorkerPid),
    NewState = State#state{worker_map = maps:remove(JobId, WorkerMap0)},
    receive
        {{'DOWN', JobId}, _Ref, process, WorkerPid, _Reason} ->
            ok
    after 6000 ->
            ?LOG_WARNING(#{what => broadcast_worker_stop_timeout,
                           job_id => JobId, worker_pid => WorkerPid})
    end,
    persist_job_aborted_by_admin(HostType, JobId),
    NewState.

-spec get_sup_name(mongooseim:host_type()) -> atom().
get_sup_name(HostType) ->
    gen_mod:get_module_proc(HostType, broadcast_jobs_sup).

%%====================================================================
%% Resumption on init
%%====================================================================

-spec resume_running_jobs(#state{}) -> #state{}.
resume_running_jobs(#state{host_type = HostType} = State) ->
    WorkerMap0 = build_worker_map(HostType),
    maps:foreach(fun monitor_worker/2, WorkerMap0),
    try mod_broadcast_backend:get_running_jobs(HostType) of
        {ok, Jobs} ->
            RunningIds = sets:from_list(maps:keys(WorkerMap0)),
            WorkerMap = lists:foldl(fun(Job, AccWorkerMap) ->
                resume_job_if_needed(HostType, Job, RunningIds, AccWorkerMap)
            end, WorkerMap0, Jobs),
            State#state{worker_map = WorkerMap}
    catch
        Class:Reason ->
            ?LOG_ERROR(#{what => broadcast_resume_failed,
                         host_type => HostType,
                         class => Class, reason => Reason}),
            State#state{worker_map = WorkerMap0}
    end.

-spec resume_job_if_needed(mongooseim:host_type(), broadcast_job(),
                           sets:set(broadcast_job_id()), worker_map()) ->
    worker_map().
resume_job_if_needed(HostType, #broadcast_job{id = JobId} = Job, LiveIds, WorkerMap0) ->
    case sets:is_element(JobId, LiveIds) of
        true ->
            ?LOG_DEBUG(#{what => broadcast_job_already_live, job_id => JobId}),
            WorkerMap0;
        false ->
            case start_and_monitor_worker(HostType, JobId) of
                {ok, WorkerPid} ->
                    NewWorkerMap = WorkerMap0#{JobId => WorkerPid},
                    ?LOG_INFO(#{what => broadcast_job_resumed,
                                job_id => JobId,
                                domain => Job#broadcast_job.domain,
                                total_recipients => Job#broadcast_job.recipient_count}),
                    NewWorkerMap;
                {error, Reason} ->
                    ?LOG_ERROR(#{what => broadcast_resume_worker_failed,
                                 job_id => JobId,
                                 reason => Reason}),
                    WorkerMap0
            end
    end.

-spec build_worker_map(mongooseim:host_type()) -> worker_map().
build_worker_map(HostType) ->
    Children = get_supervisor_children(HostType),
    maps:from_list([{Id, Pid} || {Id, Pid, _Type, _Modules} <- Children, is_pid(Pid)]).

%%====================================================================
%% Validation
%%====================================================================

-spec validate_job_spec(mongooseim:host_type(), job_spec()) ->
    job_spec() | {error, validation_error()}.
validate_job_spec(HostType, JobSpec) ->
    Validations = [
        fun() -> validate_message_rate(JobSpec) end,
        fun() -> validate_sender_exists(HostType, JobSpec) end,
        fun() -> validate_recipient_count(JobSpec) end
    ]
    ++
    [
        fun() -> validate_string_param(Param, Min, Max, JobSpec) end
        || {Param, Min, Max} <- [
            {name, 1, 250},
            {subject, 0, 1024},
            {body, 1, 16000} % Arbitraty limit, may become configurable in the future
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

-spec validate_sender_exists(mongooseim:host_type(), job_spec()) ->
    ok | {error, sender_not_found}.
validate_sender_exists(HostType, #{sender := SenderJid}) ->
    case ejabberd_auth:does_user_exist(HostType, SenderJid, stored) of
        true ->
            ok;
        false ->
            {error, sender_not_found}
    end.

-spec validate_recipient_count(job_spec()) ->
    ok | {error, no_recipients | cannot_count_recipients}.
validate_recipient_count(#{recipient_count := Count}) when is_integer(Count), Count > 0 ->
    ok;
validate_recipient_count(#{recipient_count := 0}) ->
    {error, no_recipients};
validate_recipient_count(_) ->
    {error, cannot_count_recipients}.

-spec validate_string_param(atom(), non_neg_integer(), pos_integer(), job_spec()) -> ok | {error, {bad_parameter, atom()}}.
validate_string_param(Param, Min, Max, JobSpec) ->
    case string:length(maps:get(Param, JobSpec)) of
        Len when Len >= Min, Len =< Max ->
            ok;
        _ ->
            {error, {bad_parameter, Param}}
    end.