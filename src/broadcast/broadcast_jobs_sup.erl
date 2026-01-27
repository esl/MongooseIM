%%%-------------------------------------------------------------------
%%% @doc Broadcast jobs supervisor (per host type).
%%%
%%% In MVP this will supervise broadcast workers keyed by broadcast id.
%%%-------------------------------------------------------------------

-module(broadcast_jobs_sup).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-ignore_xref([start_link/1]).

-spec start_link(HostType :: mongooseim:host_type()) -> {ok, pid()} | {error, term()}.
start_link(HostType) ->
    SupName = gen_mod:get_module_proc(HostType, ?MODULE),
    supervisor:start_link({local, SupName}, ?MODULE, [HostType]).

-spec init([mongooseim:host_type()]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([_HostType]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 5,
                 period => 5},
    {ok, {SupFlags, []}}.
