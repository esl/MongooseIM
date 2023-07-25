-module(mongoose_domain_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).
-ignore_xref([start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    DomainCore =
        {mongoose_domain_core,
         {mongoose_domain_core, start_link, []},
         permanent, brutal_kill, worker, [mongoose_domain_core]},
    SubdomainCore =
        {mongoose_subdomain_core,
         {mongoose_subdomain_core, start_link, []},
         permanent, brutal_kill, worker, [mongoose_subdomain_core]},
    LazyRouting =
        {mongoose_lazy_routing,
         {mongoose_lazy_routing, start_link, []},
         permanent, brutal_kill, worker, [mongoose_lazy_routing]},
    {ok, {{one_for_one, 10, 1},
          [DomainCore, SubdomainCore, LazyRouting]}}.
