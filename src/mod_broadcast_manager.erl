-module(mod_broadcast_manager).
-behaviour(gen_server).

-export([start_link/2]).

-export([ensure_started/1,
         start_job/2,
         abort_job/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("mongoose_logger.hrl").

-record(state, {host_type :: mongooseim:host_type(),
                runners = #{} :: #{pos_integer() => pid()}}).

%% Public API

start_link(HostType, _Opts) ->
    Proc = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, [HostType], []).

-spec ensure_started(mongooseim:host_type()) -> ok.
ensure_started(HostType) ->
    %% Ensures the manager exists; safe even if already started.
    ok = mod_broadcast:start_broadcast_worker(HostType, #{}).

-spec start_job(mongooseim:host_type(), pos_integer()) -> ok.
start_job(HostType, JobId) ->
    Proc = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:cast(Proc, {start_job, JobId}).

-spec abort_job(mongooseim:host_type(), pos_integer()) -> ok.
abort_job(HostType, JobId) ->
    Proc = gen_mod:get_module_proc(HostType, ?MODULE),
    gen_server:cast(Proc, {abort_job, JobId}).

%% gen_server

init([HostType]) ->
    process_flag(trap_exit, true),
    State0 = #state{host_type = HostType, runners = #{}},
    %% Schedule periodic cleanup of stale jobs (every 5 minutes)
    erlang:send_after(300000, self(), cleanup_stale_jobs),
    {ok, resume_jobs(State0)}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({start_job, JobId}, State) ->
    {noreply, ensure_runner(JobId, State)};
handle_cast({abort_job, JobId}, State = #state{runners = Runners}) ->
    case maps:get(JobId, Runners, undefined) of
        undefined -> ok;
        Pid -> exit(Pid, abort)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, _Reason}, State = #state{runners = Runners}) ->
    Runners2 = maps:filter(fun(_JobId, RP) -> RP =/= Pid end, Runners),
    {noreply, State#state{runners = Runners2}};
handle_info(cleanup_stale_jobs, State = #state{host_type = HostType}) ->
    %% Cleanup jobs that have been running for > 1 hour without heartbeat
    case cleanup_stale(HostType) of
        ok -> ok;
        {error, Reason} ->
            ?LOG_WARNING(#{what => mod_broadcast_cleanup_failed,
                          host_type => HostType,
                          reason => Reason})
    end,
    %% Schedule next cleanup
    erlang:send_after(300000, self(), cleanup_stale_jobs),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Internal

resume_jobs(State = #state{host_type = HostType0}) ->
    %% Resume jobs owned by this node (best-effort) and still running.
    %% We list globally and then filter by host_type to avoid a dedicated query.
    case mod_broadcast_rdbms:list_jobs(HostType0, undefined, 500, 0) of
        {ok, #{items := Jobs}} ->
            Owned = [J || J = #{host_type := HostTypeJob, status := running} <- Jobs,
                          HostTypeJob =:= HostType0,
                          maps:get(owner_node, J, undefined) =:= atom_to_binary(node(), utf8)],
            lists:foldl(fun(#{id := Id}, St) -> ensure_runner(Id, St) end, State, Owned);
        _ ->
            State
    end.

ensure_runner(JobId, State = #state{host_type = HostType, runners = Runners}) ->
    case maps:is_key(JobId, Runners) of
        true -> State;
        false ->
            case mod_broadcast_rdbms:claim_job(HostType, JobId) of
                ok ->
                    Pid = spawn_link(fun() -> mod_broadcast_runner:run(HostType, JobId) end),
                    State#state{runners = Runners#{JobId => Pid}};
                _ ->
                    ?LOG_INFO(#{what => mod_broadcast_job_not_claimed, id => JobId, host_type => HostType}),
                    State
            end
    end.

cleanup_stale(HostType) ->
    %% Mark stale jobs as aborted_errors if heartbeat is > 1 hour old
    Now = erlang:system_time(second),
    StaleThreshold = Now - 3600,
    mod_broadcast_rdbms:cleanup_stale_jobs(HostType, StaleThreshold).
