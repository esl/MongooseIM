-module(mongoose_cassandra_sup).
-author('arcusfelis@gmail.com').

-behaviour(supervisor).

-export([start/2, stop/1, stop/0, start_link/2, init/1]).

%% Registration API
-export([register_worker/2,
         select_worker/2,
         get_all_workers/0,
         get_all_workers/1]).

list_pools() ->
    Children = supervisor:which_children(ejabberd_sup),
    [PoolName || {_Name={mongoose_cassandra_sup, PoolName}, _Pid, supervisor, _} <- Children].

stop() ->
    [stop(PoolName) || PoolName <- list_pools()].

start(PoolName, WorkerCount) ->
    create_worker_pool(PoolName),
    supervisor:start_child(ejabberd_sup, supervisor_spec(PoolName, WorkerCount)).

stop(PoolName) ->
    Tag = {mongoose_cassandra_sup, PoolName},
    supervisor:terminate_child(ejabberd_sup, Tag),
    supervisor:delete_child(ejabberd_sup, Tag),
    delete_worker_pool(PoolName).

start_link(PoolName, WorkerCount) ->
    supervisor2:start_link({local, PoolName}, ?MODULE, [PoolName, WorkerCount]).

supervisor_spec(PoolName, WorkerCount) ->
    {
      {?MODULE, PoolName},
      {?MODULE, start_link, [PoolName, WorkerCount]},
      permanent,
      infinity,
      supervisor,
      [?MODULE]
    }.

worker_spec(PoolName, WorkerNumber) ->
    {
      {PoolName, WorkerNumber},
      {mongoose_cassandra_worker, start_link, [PoolName]},
      {permanent, 10}, %% Delay is 10 seconds
      infinity,
      worker,
      [mongoose_cassandra_worker]
    }.

worker_specs(PoolName, WorkerCount) ->
    [worker_spec(PoolName, WorkerNumber) || WorkerNumber <- lists:seq(1, WorkerCount)].

init([PoolName, WorkerCount]) ->
    Specs = worker_specs(PoolName, WorkerCount),
    {ok, {{one_for_one, 10, 1}, Specs}}.


%%====================================================================
%% Registration
%%====================================================================

create_worker_pool(PoolName) ->
    pg2:create(group_name(PoolName)).

delete_worker_pool(PoolName) ->
    pg2:delete(group_name(PoolName)).

register_worker(PoolName, WorkerPid) ->
    pg2:join(group_name(PoolName), WorkerPid).

select_worker(PoolName, UserJID) ->
    case pg2:get_local_members(group_name(PoolName)) of
        [] ->
            error({no_worker, PoolName});
        Workers ->
            N = erlang:phash2(UserJID, length(Workers)) + 1,
            lists:nth(N, Workers)
    end.

get_all_workers() ->
    [{PoolName, get_all_workers(PoolName)} || PoolName <- list_pools()].

get_all_workers(PoolName) ->
    pg2:get_local_members(group_name(PoolName)).

group_name(PoolName) ->
    {mam_ca, PoolName, node()}.
