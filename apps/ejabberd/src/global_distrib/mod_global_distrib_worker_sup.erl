-module(mod_global_distrib_worker_sup).

-behaviour(supervisor).

-export([start_link/0, get_worker/1, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

get_worker(From) when is_binary(From) ->
    Name = mod_global_distrib_utils:any_binary_to_atom(From),
    case whereis(Name) of
        undefined -> supervisor:start_child(?MODULE, [Name]);
        _ -> ok
    end,
    Name.

init(_) ->
    Module = mod_global_distrib_worker,
    Child = {Module, {Module, start_link, []}, transient, 5000, worker, [Module]},
    {ok, {{simple_one_for_one, 100, 1}, [Child]}}.
