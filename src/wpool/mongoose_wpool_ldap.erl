-module(mongoose_wpool_ldap).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4, stop/2]).

%% --------------------------------------------------------------
%% mongoose_wpool callbacks
-spec init() -> ok.
init() ->
    ok.

-spec start(mongooseim:host_type_or_global(), mongoose_wpool:tag(),
            mongoose_wpool:pool_opts(), mongoose_wpool:conn_opts()) -> {ok, pid()} | {error, any()}.
start(HostType, Tag, WpoolOpts, ConnOpts) ->
    WorkerSpec = {mongoose_ldap_worker, ConnOpts},
    ProcName = mongoose_wpool:make_pool_name(ldap, HostType, Tag),
    mongoose_wpool:start_sup_pool(ldap, ProcName, [{worker, WorkerSpec} | WpoolOpts]).

-spec stop(mongooseim:host_type_or_global(), mongoose_wpool:tag()) -> ok.
stop(_HostType, _Tag) ->
    ok.
