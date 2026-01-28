%%%-------------------------------------------------------------------
%%% @doc Broadcast manager process (per host type).
%%%
%%% Manages broadcast job lifecycle:
%%% - Starting new jobs (creates DB entry + spawns worker)
%%% - Stopping running jobs
%%% - Tracks active jobs per domain
%%%-------------------------------------------------------------------

-module(broadcast_manager).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(gen_server).

%% API
-export([start_link/1,
         start_job/2,
         stop_job/2]).

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

-spec start_job(mongooseim:host_type(), job_spec()) ->
    {ok, JobId :: integer()} | {error, term()}.
start_job(HostType, JobSpec) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:call(ProcName, {start_job, JobSpec}).

-spec stop_job(mongooseim:host_type(), JobId :: integer()) -> ok | {error, term()}.
stop_job(HostType, JobId) ->
    ProcName = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:call(ProcName, {stop_job, JobId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(mongooseim:host_type()) -> {ok, #state{}, {continue, resume_jobs}}.
init(HostType) ->
    State = #state{host_type = HostType},
    {ok, State, {continue, resume_jobs}}.

handle_continue(resume_jobs, State) ->
    resume_running_jobs(State),
    {noreply, State}.

handle_call({start_job, JobSpec}, _From, State) ->
    case do_start_job(State#state.host_type, JobSpec) of
        {ok, JobId} ->
            {reply, {ok, JobId}, State};
        {error, _Reason} = Error ->
            {reply, Error, State}
    end;

handle_call({stop_job, JobId}, _From, State) ->
    SupName = gen_mod:get_module_proc(State#state.host_type, broadcast_jobs_sup),
    case find_worker_pid(SupName, JobId) of
        {ok, WorkerPid} ->
            broadcast_worker:stop(WorkerPid),
            {reply, ok, State};
        error ->
            {reply, {error, not_found}, State}
    end;

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

-spec do_start_job(mongooseim:host_type(), job_spec()) ->
    {ok, JobId :: integer()} | {error, term()}.
do_start_job(HostType, JobSpec) ->
    %% Validate job spec before creating
    case validate_job_spec(HostType, JobSpec) of
        ok ->
            %% Create job in database (unique constraint enforces one running per domain)
            case create_job(HostType, JobSpec) of
                {ok, JobId} ->
                    %% Start worker process under jobs supervisor
                    case start_worker(HostType, JobId) of
                        {ok, _WorkerPid} ->
                            ?LOG_INFO(#{what => broadcast_job_created,
                                        job_id => JobId,
                                        domain => maps:get(domain, JobSpec),
                                        host_type => HostType,
                                        sender => jid:to_binary(maps:get(sender, JobSpec))}),
                            {ok, JobId};
                        {error, _Reason} = Error ->
                            %% TODO: Clean up DB entry on worker start failure
                            Error
                    end;
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

-spec create_job(mongooseim:host_type(), job_spec()) ->
    {ok, JobId :: integer()} | {error, term()}.
create_job(HostType, JobSpec) ->
    mod_broadcast_backend:create_job(HostType, JobSpec).

-spec start_worker(mongooseim:host_type(), JobId :: integer()) ->
    {ok, pid()} | {error, term()}.
start_worker(HostType, JobId) ->
    SupName = gen_mod:get_module_proc(HostType, broadcast_jobs_sup),
    ChildSpec = #{id => JobId,
                  start => {broadcast_worker, start_link, [HostType, JobId]},
                  restart => temporary,
                  shutdown => 5000,
                  type => worker,
                  modules => [broadcast_worker]},
    supervisor:start_child(SupName, ChildSpec).

-spec find_worker_pid(atom(), integer()) -> {ok, pid()} | error.
find_worker_pid(SupName, JobId) ->
    Children = supervisor:which_children(SupName),
    case lists:keyfind(JobId, 1, Children) of
        {JobId, Pid, _, _} when is_pid(Pid) -> {ok, Pid};
        _ -> error
    end.

%%====================================================================
%% Resumption on init
%%====================================================================

-spec resume_running_jobs(#state{}) -> ok.
resume_running_jobs(#state{host_type = HostType}) ->
    case mod_broadcast_backend:get_running_jobs(HostType) of
        {ok, Jobs} ->
            %% Get already running workers from supervisor
            SupName = gen_mod:get_module_proc(HostType, broadcast_jobs_sup),
            RunningIds = get_running_job_ids(SupName),
            %% Start workers for jobs that don't have a running worker
            lists:foreach(fun(Job) ->
                resume_job_if_needed(Job, RunningIds, HostType)
            end, Jobs);
        {error, Reason} ->
            ?LOG_ERROR(#{what => broadcast_resume_failed,
                         host_type => HostType,
                         reason => Reason})
    end,
    ok.

-spec get_running_job_ids(atom()) -> sets:set(integer()).
get_running_job_ids(SupName) ->
    Children = supervisor:which_children(SupName),
    lists:foldl(fun({Id, _Pid, _Type, _Modules}, Acc) when is_integer(Id) ->
        sets:add_element(Id, Acc);
    (_, Acc) ->
        Acc
    end, sets:new(), Children).

-spec resume_job_if_needed(broadcast_job(), sets:set(integer()), mongooseim:host_type()) -> ok.
resume_job_if_needed(Job, RunningIds, HostType) ->
    JobId = Job#broadcast_job.id,
    case sets:is_element(JobId, RunningIds) of
        true ->
            %% Already running, skip
            ?LOG_DEBUG(#{what => broadcast_job_already_running, job_id => JobId});
        false ->
            %% Need to resume
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

%%====================================================================
%% Validation
%%====================================================================

-spec validate_job_spec(mongooseim:host_type(), job_spec()) -> ok | {error, term()}.
validate_job_spec(HostType, JobSpec) ->
    Validations = [
        fun() -> validate_message_rate(JobSpec) end,
        fun() -> validate_sender_exists(HostType, JobSpec) end
    ],
    run_validations(Validations).

-spec run_validations([fun(() -> ok | {error, term()})]) -> ok | {error, term()}.
run_validations([]) ->
    ok;
run_validations([Validate | Rest]) ->
    case Validate() of
        ok -> run_validations(Rest);
        {error, _Reason} = Error -> Error
    end.

-spec validate_message_rate(job_spec()) -> ok | {error, term()}.
validate_message_rate(#{message_rate := Rate}) when is_integer(Rate), Rate >= 1, Rate =< 1000 ->
    ok;
validate_message_rate(#{message_rate := Rate}) ->
    ?LOG_WARNING(#{what => broadcast_invalid_rate, rate => Rate}),
    {error, {invalid_message_rate, Rate}}.

-spec validate_sender_exists(mongooseim:host_type(), job_spec()) -> ok | {error, term()}.
validate_sender_exists(HostType, #{sender := SenderJid}) ->
    case ejabberd_auth:does_user_exist(HostType, SenderJid, stored) of
        true ->
            ok;
        false ->
            ?LOG_WARNING(#{what => broadcast_sender_not_found,
                           sender => jid:to_binary(SenderJid)}),
            {error, sender_not_found}
    end.