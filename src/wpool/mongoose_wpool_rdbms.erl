-module(mongoose_wpool_rdbms).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).

%% --------------------------------------------------------------
%% mongoose_wpool callbacks
-spec init() -> ok.
init() ->
    ejabberd_sup:create_ets_table(
      prepared_statements, [named_table, public, {read_concurrency, true}]).

-spec start(mongooseim:host_type_or_global(), mongoose_wpool:tag(),
            mongoose_wpool:pool_opts(), mongoose_wpool:conn_opts()) -> {ok, pid()} | {error, any()}.
start(HostType, Tag, WpoolOpts, RdbmsOpts) ->
    try do_start(HostType, Tag, WpoolOpts, RdbmsOpts)
    catch
        Err -> {error, Err}
    end.

-spec stop(mongooseim:host_type_or_global(), mongoose_wpool:tag()) -> ok.
stop(_, _) ->
    ok.

%% --------------------------------------------------------------
%% Helper functions
do_start(HostType, Tag, WpoolOpts0, RdbmsOpts) when is_list(WpoolOpts0), is_map(RdbmsOpts) ->
    #{driver := BackendName} = RdbmsOpts,
    mongoose_backend:init(global, mongoose_rdbms, [query, execute], #{backend => BackendName}),
    mongoose_metrics:ensure_db_pool_metric({rdbms, HostType, Tag}),
    WpoolOpts = make_wpool_opts(WpoolOpts0, RdbmsOpts),
    ProcName = mongoose_wpool:make_pool_name(rdbms, HostType, Tag),
    mongoose_wpool:start_sup_pool(rdbms, ProcName, WpoolOpts).

make_wpool_opts(WpoolOpts0, RdbmsOpts) ->
    Worker = {mongoose_rdbms, RdbmsOpts},
    [{worker, Worker}, {pool_sup_shutdown, infinity} | WpoolOpts0].
