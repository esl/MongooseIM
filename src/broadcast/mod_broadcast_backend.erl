%%%-------------------------------------------------------------------
%%% @doc Backend proxy module for mod_broadcast.
%%%
%%% Provides backend abstraction for broadcast job persistence.
%%% Currently only RDBMS backend is supported.
%%%-------------------------------------------------------------------

-module(mod_broadcast_backend).
-author('piotr.nosek@erlang-solutions.com').

-export([init/2,
         create_job/2,
         get_job/2,
         get_jobs_by_domain/4,
         get_running_jobs/1,
         get_worker_state/2,
         set_job_started/2,
         update_worker_state/3,
         set_job_finished/2,
         set_job_aborted_error/3,
         set_job_aborted_admin/2,
         delete_job/2,
         delete_inactive_jobs_by_domain/2]).

-include("mod_broadcast.hrl").

%%====================================================================
%% Callbacks
%%====================================================================

-callback init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.

-callback create_job(mongooseim:host_type(), job_spec()) ->
    {ok, JobId :: broadcast_job_id()} | {error, already_running}.

-callback get_job(mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    {ok, broadcast_job()} | {error, not_found}.

-callback get_running_jobs(mongooseim:host_type()) ->
    {ok, [broadcast_job()]}.

-callback get_worker_state(mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    {ok, broadcast_worker_state()} | {error, not_found}.

-callback set_job_started(mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    ok.

-callback update_worker_state(mongooseim:host_type(), JobId :: broadcast_job_id(),
                              WorkerState :: broadcast_worker_state()) ->
    ok.

-callback set_job_finished(mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    ok.

-callback set_job_aborted_error(mongooseim:host_type(), JobId :: broadcast_job_id(), Reason :: binary()) ->
    ok.

-callback set_job_aborted_admin(mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    ok.

-callback get_jobs_by_domain(mongooseim:host_type(), jid:lserver(),
                             Limit :: pos_integer(), Offset :: non_neg_integer()) ->
    {ok, [broadcast_job()]}.

-callback delete_job(mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    ok.

-callback delete_inactive_jobs_by_domain(mongooseim:host_type(), jid:lserver()) ->
    {ok, [broadcast_job_id()]}.

%%====================================================================
%% API
%%====================================================================

-spec init(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
init(HostType, Opts) ->
    TrackedFuns = [get_job, update_worker_state],
    mongoose_backend:init(HostType, main_module(), TrackedFuns, Opts),
    Args = [HostType, Opts],
    mongoose_backend:call(HostType, main_module(), ?FUNCTION_NAME, Args).

-spec create_job(mongooseim:host_type(), job_spec()) ->
    {ok, JobId :: broadcast_job_id()} | {error, already_running}.
create_job(HostType, JobSpec) ->
    Args = [HostType, JobSpec],
    mongoose_backend:call(HostType, main_module(), ?FUNCTION_NAME, Args).

-spec get_job(mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    {ok, broadcast_job()} | {error, not_found}.
get_job(HostType, JobId) ->
    Args = [HostType, JobId],
    mongoose_backend:call_tracked(HostType, main_module(), ?FUNCTION_NAME, Args).

-spec get_running_jobs(mongooseim:host_type()) ->
    {ok, [broadcast_job()]}.
get_running_jobs(HostType) ->
    Args = [HostType],
    mongoose_backend:call(HostType, main_module(), ?FUNCTION_NAME, Args).

-spec get_worker_state(mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    {ok, broadcast_worker_state()} | {error, not_found}.
get_worker_state(HostType, JobId) ->
    Args = [HostType, JobId],
    mongoose_backend:call(HostType, main_module(), ?FUNCTION_NAME, Args).

-spec set_job_started(mongooseim:host_type(), JobId :: broadcast_job_id()) -> ok | {error, not_found}.
set_job_started(HostType, JobId) ->
    Args = [HostType, JobId],
    mongoose_backend:call(HostType, main_module(), ?FUNCTION_NAME, Args).

-spec update_worker_state(mongooseim:host_type(), JobId :: broadcast_job_id(),
                          WorkerState :: broadcast_worker_state()) ->
    ok.
update_worker_state(HostType, JobId, WorkerState) ->
    Args = [HostType, JobId, WorkerState],
    mongoose_backend:call_tracked(HostType, main_module(), ?FUNCTION_NAME, Args).

-spec set_job_finished(mongooseim:host_type(), JobId :: broadcast_job_id()) -> ok.
set_job_finished(HostType, JobId) ->
    Args = [HostType, JobId],
    mongoose_backend:call(HostType, main_module(), ?FUNCTION_NAME, Args).

-spec set_job_aborted_error(mongooseim:host_type(), JobId :: broadcast_job_id(), Reason :: binary()) ->
    ok.
set_job_aborted_error(HostType, JobId, Reason) ->
    Args = [HostType, JobId, Reason],
    mongoose_backend:call(HostType, main_module(), ?FUNCTION_NAME, Args).

-spec set_job_aborted_admin(mongooseim:host_type(), JobId :: broadcast_job_id()) ->
    ok.
set_job_aborted_admin(HostType, JobId) ->
    Args = [HostType, JobId],
    mongoose_backend:call(HostType, main_module(), ?FUNCTION_NAME, Args).

-spec get_jobs_by_domain(mongooseim:host_type(), jid:lserver(),
                         Limit :: pos_integer(), Offset :: non_neg_integer()) ->
    {ok, [broadcast_job()]}.
get_jobs_by_domain(HostType, Domain, Limit, Offset) ->
    Args = [HostType, Domain, Limit, Offset],
    mongoose_backend:call(HostType, main_module(), ?FUNCTION_NAME, Args).

-spec delete_job(mongooseim:host_type(), JobId :: broadcast_job_id()) -> ok.
delete_job(HostType, JobId) ->
    Args = [HostType, JobId],
    mongoose_backend:call(HostType, main_module(), ?FUNCTION_NAME, Args).

-spec delete_inactive_jobs_by_domain(mongooseim:host_type(), jid:lserver()) ->
    {ok, [broadcast_job_id()]}.
delete_inactive_jobs_by_domain(HostType, Domain) ->
    Args = [HostType, Domain],
    mongoose_backend:call(HostType, main_module(), ?FUNCTION_NAME, Args).

%% =====================================================================
%% Internal functions
%% =====================================================================

-spec main_module() -> module().
main_module() -> mod_broadcast.
