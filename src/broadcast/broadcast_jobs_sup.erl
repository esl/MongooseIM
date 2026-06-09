%%%-------------------------------------------------------------------
%%% @doc Broadcast jobs supervisor (per host type).
%%%
%%% In MVP this will supervise broadcast workers keyed by broadcast id.
%%%-------------------------------------------------------------------

-module(broadcast_jobs_sup).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(supervisor).

-include("mod_broadcast.hrl").

-export([start_link/1, start_worker/2, get_children/1]).
-export([init/1]).

-ignore_xref([start_link/1]).

-spec start_link(HostType :: mongooseim:host_type()) -> {ok, pid()} | {error, term()}.
start_link(HostType) ->
    SupName = gen_mod:get_module_proc(HostType, ?MODULE),
    supervisor:start_link({local, SupName}, ?MODULE, [HostType]).

-spec start_worker(mongooseim:host_type(), broadcast_job_id()) -> {ok, pid()} | {error, term()}.
start_worker(HostType, JobId) ->
    SupName = gen_mod:get_module_proc(HostType, ?MODULE),
    ChildSpec = #{id => JobId,
                  start => {broadcast_worker, start_link, [HostType, JobId]},
                  restart => temporary,
                  shutdown => 5000,
                  type => worker,
                  modules => [broadcast_worker]},
    supervisor:start_child(SupName, ChildSpec).

-spec get_children(mongooseim:host_type()) ->
    [{term(), undefined | pid() | restarting, worker | supervisor, [module()] | dynamic}].
get_children(HostType) ->
    SupName = gen_mod:get_module_proc(HostType, ?MODULE),
    supervisor:which_children(SupName).

-spec init([mongooseim:host_type()]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([_HostType]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 5},
    {ok, {SupFlags, []}}.
