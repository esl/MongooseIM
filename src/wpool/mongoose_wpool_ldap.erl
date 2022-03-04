-module(mongoose_wpool_ldap).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4, stop/2]).

%% --------------------------------------------------------------
%% mongoose_wpool callbacks
init() ->
    ok.

start(HostType, Tag, WpoolOpts, ConnOptsIn) ->
    ConnOpts = maps:to_list(ConnOptsIn),
    WorkerSpec = {mongoose_ldap_worker, ConnOpts},
    ProcName = mongoose_wpool:make_pool_name(ldap, HostType, Tag),
    mongoose_wpool:start_sup_pool(ldap, ProcName, [{worker, WorkerSpec} | WpoolOpts]).

stop(_HostType, _Tag) ->
    ok.
