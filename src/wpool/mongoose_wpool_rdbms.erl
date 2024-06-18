-module(mongoose_wpool_rdbms).
-behaviour(mongoose_wpool).
-behaviour(mongoose_instrument_probe).

-include("mongoose_metrics_definitions.hrl").

-export([init/0, start/4, stop/2]).
-export([probe/2, instrumentation/2]).

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
    mongoose_rdbms_backend:init(BackendName),
    WpoolOpts = make_wpool_opts(WpoolOpts0, RdbmsOpts),
    ProcName = mongoose_wpool:make_pool_name(rdbms, HostType, Tag),
    mongoose_wpool:start_sup_pool(rdbms, ProcName, WpoolOpts).

make_wpool_opts(WpoolOpts0, RdbmsOpts) ->
    Worker = {mongoose_rdbms, RdbmsOpts},
    [{worker, Worker}, {pool_sup_shutdown, infinity} | WpoolOpts0].

-spec instrumentation(mongooseim:host_type_or_global(), mongoose_wpool:tag()) ->
    [mongoose_instrument:spec()].
instrumentation(global, Tag) ->
    % Services use global pools. Since the same number of labels for a metric is expected, for an
    % event, global pool has to emit an event under a different name.
    [{rdbms_stats_global, #{pool_tag => Tag},
      #{probe => #{module => ?MODULE}, metrics => ?INET_STATS_METRICS#{workers => counter}}}];
instrumentation(HostType, Tag) ->
    [{rdbms_stats, #{host_type => HostType, pool_tag => Tag},
      #{probe => #{module => ?MODULE}, metrics => ?INET_STATS_METRICS#{workers => counter}}}].

-spec probe(mongoose_instrument:event_name(), mongoose_instrument:labels()) ->
    mongoose_instrument:measurements().
probe(rdbms_stats_global, #{pool_tag := Tag} = _Labels) ->
    Stats = mongoose_metrics:get_rdbms_data_stats([{rdbms, global, Tag}]),
    proplists:to_map(Stats);
probe(rdbms_stats, #{host_type := HostType, pool_tag := Tag} = _Labels) ->
    Stats = mongoose_metrics:get_rdbms_data_stats([{rdbms, HostType, Tag}]),
    proplists:to_map(Stats).
