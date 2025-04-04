-module(mongoose_domain_sup).

-behaviour(supervisor).

-type pair() :: {mongooseim:domain_name(), mongooseim:host_type()}.

-export([start_link/0, init/1]).
-ignore_xref([start_link/0]).

-export([start_link/2, restart_core/1]).
-ignore_xref([start_link/2, restart_core/1]).

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(Pairs, AllowedHostTypes) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Pairs, AllowedHostTypes]).

restart_core(Args) ->
    supervisor:terminate_child(?MODULE, mongoose_domain_core),
    supervisor:delete_child(?MODULE, mongoose_domain_core),
    supervisor:start_child(?MODULE, worker_spec(mongoose_domain_core, fill_args(Args))).

-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Args) ->
    DomainCore = worker_spec(mongoose_domain_core, fill_args(Args)),
    SubdomainCore = worker_spec(mongoose_subdomain_core, []),
    LazyRouting = worker_spec(mongoose_lazy_routing, []),
    {ok, {{one_for_one, 10, 1},
          [DomainCore, SubdomainCore, LazyRouting]}}.

worker_spec(Mod, Args) ->
    {Mod, {Mod, start_link, Args}, permanent, timer:seconds(5), worker, [Mod]}.

%% Domains should be nameprepped using `jid:nameprep'
-spec get_static_pairs() -> [pair()].
get_static_pairs() ->
    [{H, H} || H <- mongoose_config:get_opt(hosts)].

fill_args([]) ->
    Pairs = get_static_pairs(),
    AllowedHostTypes = mongoose_config:get_opt(host_types),
    [Pairs, AllowedHostTypes];
fill_args([_, _] = Args) ->
    Args.
