-module(cassandra_sup).
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
    [PoolName || {_Name={cassandra_sup, PoolName}, _Pid, supervisor, _} <- Children].

stop() ->
    [stop(PoolName) || PoolName <- list_pools()].

start(PoolName, Config) ->
    create_worker_pool(PoolName),
    supervisor:start_child(ejabberd_sup, supervisor_spec(PoolName, Config)).

stop(PoolName) ->
    Tag = {cassandra_sup, PoolName},
    supervisor:terminate_child(ejabberd_sup, Tag),
    supervisor:delete_child(ejabberd_sup, Tag),
    delete_worker_pool(PoolName).

start_link(PoolName, Config) ->
    supervisor2:start_link({local, ?MODULE}, ?MODULE, [PoolName, Config]).

supervisor_spec(PoolName, Config) ->
    {{cassandra_sup, PoolName},
     {?MODULE, start_link, [PoolName, Config]},
     permanent,
     infinity,
     supervisor,
     [?MODULE]}.

worker_spec(PoolName, Addr, Port, WorkerNumber, ClientOptions) ->
    {{PoolName, Addr, Port, WorkerNumber},
     {cassandra_worker, start_link, [PoolName, Addr, Port, ClientOptions]},
     {permanent, 10}, %% Delay is 10 seconds
     infinity,
     worker,
     [cassandra_worker]}.

worker_specs(PoolName, Servers, ClientOptions) ->
    [worker_spec(PoolName, Addr, Port, WorkerNumber, ClientOptions)
     || {Addr, Port, WorkerCount} <- Servers,
        WorkerNumber <- lists:seq(1, WorkerCount)].

init([PoolName, ClientOptions]) ->
    Servers = proplists:get_value(servers, ClientOptions),
    Specs = worker_specs(PoolName, Servers, ClientOptions),
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
