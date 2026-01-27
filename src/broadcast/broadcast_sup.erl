%%%-------------------------------------------------------------------
%%% @doc Message Broadcast per-host-type supervision tree.
%%%-------------------------------------------------------------------

-module(broadcast_sup).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-spec start_link(HostType :: mongooseim:host_type()) -> {ok, pid()} | {error, term()}.
start_link(HostType) ->
    SupName = gen_mod:get_module_proc(HostType, ?MODULE),
    supervisor:start_link({local, SupName}, ?MODULE, [HostType]).

-spec init([mongooseim:host_type()]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([HostType]) ->
    SupFlags = #{strategy => rest_for_one,
                 intensity => 5,
                 period => 5},

    JobsSupName = gen_mod:get_module_proc(HostType, broadcast_jobs_sup),
    JobsSupSpec = #{id => JobsSupName,
                    start => {broadcast_jobs_sup, start_link, [HostType]},
                    restart => permanent,
                    shutdown => infinity,
                    type => supervisor,
                    modules => [broadcast_jobs_sup]},

    ManagerName = gen_mod:get_module_proc(HostType, broadcast_manager),
    ManagerSpec = #{id => ManagerName,
                    start => {broadcast_manager, start_link, [HostType]},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [broadcast_manager]},

    {ok, {SupFlags, [JobsSupSpec, ManagerSpec]}}.
