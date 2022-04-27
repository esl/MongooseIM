-module(mongoose_shaper_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, get_workers/0, select_worker/1]).
-ignore_xref([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {#{strategy => one_for_one, intensity => 100, period => 5},
                        [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 100, period => 5},
    WorkerNames = worker_names(),
    TupleNames = list_to_tuple(WorkerNames),
    persistent_term:put(?MODULE, TupleNames),
    Shapers = [child_spec(Name) || Name <- WorkerNames],
    {ok, { SupFlags, Shapers }}.

-spec child_spec(atom()) -> supervisor:child_spec().
child_spec(ProcName) ->
    #{id => ProcName,
      start => {shaper_srv, start_link, [ProcName]},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [shaper_srv]}.

-spec get_workers() -> [atom()].
get_workers() ->
    tuple_to_list(persistent_term:get(?MODULE)).

-spec select_worker(term()) -> atom().
select_worker(Key) ->
    N = 1 + erlang:phash2(Key, worker_count()),
    Workers = persistent_term:get(?MODULE),
    element(N, Workers).

-spec worker_names() -> [atom()].
worker_names() ->
    [build_worker_name(N) || N <- lists:seq(1, worker_count())].

-spec build_worker_name(integer()) -> atom().
build_worker_name(N) ->
    list_to_atom(worker_prefix() ++ integer_to_list(N)).

-spec worker_prefix() -> string().
worker_prefix() ->
    "mongoose_shaper_".

worker_count() ->
    10.
