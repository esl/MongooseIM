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

%% Test-only API (will become proper user API in future iteration)
-export([abort_running_jobs_for_domain/2,
         does_worker_for_job_exist/2,
         get_supervisor_children/1]).

%% Error types
-export_type([create_job_error/0,
              stop_job_error/0,
              validation_error/0]).

-type create_job_error() :: already_running | validation_error() | term().
-type stop_job_error() :: not_found.
-type validation_error() :: sender_not_found | {bad_parameter, atom()}.

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

-record(state, {
    host_type :: mongooseim:host_type()
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

-spec get_supervisor_children(mongooseim:host_type()) -> [supervisor:child_spec()].
get_supervisor_children(HostType) ->
    supervisor:which_children(get_sup_name(HostType)).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(mongooseim:host_type()) -> {ok, #state{}, {continue, resume_jobs}}.
init(HostType) ->
    State = #state{host_type = HostType},
    {ok, State, {continue, resume_jobs}}.

handle_continue({start_worker_for, JobId}, State) ->
    case start_worker(State#state.host_type, JobId) of
        {ok, _WorkerPid} ->
            persist_job_started(State#state.host_type, JobId),
            ?LOG_INFO(#{what => broadcast_job_started,
                        job_id => JobId});
        {error, Reason} ->
            ?LOG_ERROR(#{what => broadcast_start_worker_failed,
                         job_id => JobId,
                         reason => Reason})
    end,
    {noreply, State};
handle_continue(resume_jobs, State) ->
    resume_running_jobs(State),
    {noreply, State}.

handle_call({start_job, JobSpec}, _From, State) ->
    case do_create_job(State#state.host_type, JobSpec) of
        {ok, JobId} ->
            {reply, {ok, JobId}, State, {continue, {start_worker_for, JobId}}};
        {error, _Reason} = Error ->
            {reply, Error, State}
    end;

handle_call({stop_job, JobId}, _From, State) ->
    SupName = get_sup_name(State#state.host_type),
    case find_worker_pid(SupName, JobId) of
        {ok, WorkerPid} ->
            broadcast_worker:stop(WorkerPid),
            persist_job_aborted_by_admin(State#state.host_type, JobId),
            {reply, ok, State};
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call({abort_running_jobs_for_domain, Domain}, _From, State) ->
    do_abort_running_jobs_for_domain(State#state.host_type, Domain),
    {reply, ok, State};
handle_call({does_worker_for_job_exist, JobId}, _From, State) ->
    SupName = get_sup_name(State#state.host_type),
    Reply = case find_worker_pid(SupName, JobId) of
                {ok, _Pid} -> true;
                error -> false
            end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec do_create_job(mongooseim:host_type(), job_spec()) ->
    {ok, JobId :: broadcast_job_id()} | {error, create_job_error() | term()}.
do_create_job(HostType, JobSpec) ->
    case validate_job_spec(HostType, JobSpec) of
        ok ->
            %% Create job in database (unique constraint enforces one running per domain)
            case mod_broadcast_backend:create_job(HostType, JobSpec) of
                {ok, JobId} ->
                    ?LOG_INFO(#{what => broadcast_job_created,
                                        job_id => JobId,
                                        domain => maps:get(domain, JobSpec),
                                        host_type => HostType,
                                        sender => jid:to_binary(maps:get(sender, JobSpec))}),
                    {ok, JobId};
                {error, already_running} = Error ->
                    ?LOG_WARNING(#{what => broadcast_job_already_running,
                                   domain => maps:get(domain, JobSpec),
                                   host_type => HostType}),
                    Error;
                {error, _Reason} = Error ->
                    Error
            end;
        {error, _Reason} = Error ->
            Error
    end.

-spec start_worker(mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    {ok, pid()} | {error, term()}.
start_worker(HostType, JobId) ->
    SupName = get_sup_name(HostType),
    ChildSpec = #{id => JobId,
                  start => {broadcast_worker, start_link, [HostType, JobId]},
                  restart => temporary,
                  shutdown => 5000,
                  type => worker,
                  modules => [broadcast_worker]},
    supervisor:start_child(SupName, ChildSpec).

-spec persist_job_started(mongooseim:host_type(), broadcast_job_id()) -> ok.
persist_job_started(HostType, JobId) ->
    mongoose_instrument:execute(mod_broadcast_jobs_started,
                                #{host_type => HostType}, #{count => 1}),
    case mod_broadcast_backend:set_job_started(HostType, JobId) of
        ok -> ok;
        {error, Reason} ->
            ?LOG_WARNING(#{what => broadcast_start_persist_failed,
                           job_id => JobId, reason => Reason})
    end,
    ok.

-spec persist_job_aborted_by_admin(mongooseim:host_type(), broadcast_job_id()) -> ok.
persist_job_aborted_by_admin(HostType, JobId) ->
    mongoose_instrument:execute(mod_broadcast_jobs_aborted_admin,
                                #{host_type => HostType}, #{count => 1}),
    ?LOG_INFO(#{what => broadcast_job_aborted_by_admin,
                job_id => JobId}),
    case mod_broadcast_backend:set_job_aborted_admin(HostType, JobId) of
        ok -> ok;
        {error, Reason} ->
            ?LOG_WARNING(#{what => broadcast_abort_admin_persist_failed,
                           job_id => JobId, reason => Reason})
    end,
    ok.

-spec find_worker_pid(atom(), broadcast_job_id()) -> {ok, pid()} | error.
find_worker_pid(SupName, JobId) ->
    Children = supervisor:which_children(SupName),
    case lists:keyfind(JobId, 1, Children) of
        {JobId, Pid, _, _} when is_pid(Pid) -> {ok, Pid};
        _ -> error
    end.

-spec do_abort_running_jobs_for_domain(mongooseim:host_type(), jid:lserver()) -> ok.
do_abort_running_jobs_for_domain(HostType, Domain) ->
    case mod_broadcast_backend:get_running_jobs(HostType) of
        {ok, Jobs} ->
            DomainJobs = [Job#broadcast_job.id || Job <- Jobs, Job#broadcast_job.domain =:= Domain],
            %% TODO: Take into account that Pid may be 'restarting' or 'undefined'
            lists:foreach(fun({WorkerId, WorkerPid, _, _}) ->
                    case lists:member(WorkerId, DomainJobs) of
                        true ->
                            ?LOG_INFO(#{what => aborting_broadcast_job,
                                        job_id => WorkerId,
                                        domain => Domain}),
                            % TODO: Proper error handling
                            catch broadcast_worker:stop(WorkerPid),
                            persist_job_aborted_by_admin(HostType, WorkerId);
                        false ->
                            ok
                    end
                end, get_supervisor_children(HostType));
        {error, Reason} ->
            ?LOG_ERROR(#{what => broadcast_abort_get_jobs_failed,
                         domain => Domain, reason => Reason})
    end,
    ok.

%%====================================================================
%% Resumption on init
%%====================================================================

-spec resume_running_jobs(#state{}) -> ok.
resume_running_jobs(#state{host_type = HostType}) ->
    case mod_broadcast_backend:get_running_jobs(HostType) of
        {ok, Jobs} ->
            RunningIds = get_live_job_ids(HostType),
            lists:foreach(fun(Job) ->
                resume_job_if_needed(Job, RunningIds, HostType)
            end, Jobs);
        {error, Reason} ->
            ?LOG_ERROR(#{what => broadcast_resume_failed,
                         host_type => HostType,
                         reason => Reason})
    end,
    ok.

-spec get_live_job_ids(atom()) -> sets:set(broadcast_job_id()).
get_live_job_ids(HostType) ->
    Children = get_supervisor_children(HostType),
    sets:from_list([Id || {Id, _Pid, _Type, _Modules} <- Children]).

-spec resume_job_if_needed(broadcast_job(), sets:set(broadcast_job_id()), mongooseim:host_type()) -> ok.
resume_job_if_needed(#broadcast_job{id = JobId} = Job, LiveIds, HostType) ->
    case sets:is_element(JobId, LiveIds) of
        true ->
            ?LOG_DEBUG(#{what => broadcast_job_already_live, job_id => JobId});
        false ->
            case start_worker(HostType, JobId) of
                {ok, _WorkerPid} ->
                    ?LOG_INFO(#{what => broadcast_job_resumed,
                                job_id => JobId,
                                domain => Job#broadcast_job.domain,
                                total_recipients => Job#broadcast_job.recipient_count});
                {error, Reason} ->
                    ?LOG_ERROR(#{what => broadcast_resume_worker_failed,
                                 job_id => JobId,
                                 reason => Reason})
            end
    end,
    ok.

-spec get_sup_name(mongooseim:host_type()) -> atom().
get_sup_name(HostType) ->
    gen_mod:get_module_proc(HostType, broadcast_jobs_sup).

%%====================================================================
%% Validation
%%====================================================================

-spec validate_job_spec(mongooseim:host_type(), job_spec()) ->
    ok | {error, validation_error()}.
validate_job_spec(HostType, JobSpec) ->
    Validations = [
        fun() -> validate_message_rate(JobSpec) end,
        fun() -> validate_sender_exists(HostType, JobSpec) end
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
    run_validations(Validations).

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

-spec validate_string_param(atom(), non_neg_integer(), pos_integer(), job_spec()) -> ok | {error, {bad_parameter, atom()}}.
validate_string_param(Param, Min, Max, JobSpec) ->
    case string:length(maps:get(Param, JobSpec)) of
        Len when Len >= Min, Len =< Max ->
            ok;
        _ ->
            {error, {bad_parameter, Param}}
    end.