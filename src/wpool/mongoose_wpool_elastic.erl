-module(mongoose_wpool_elastic).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).

%% --------------------------------------------------------------
%% mongoose_wpool callbacks
-spec init() -> ok.
init() ->
    tirerl:start(),
    ok.

-spec start(mongooseim:host_type_or_global(), mongoose_wpool:tag(),
            mongoose_wpool:pool_opts(), mongoose_wpool:conn_opts()) -> {ok, pid()} | {error, any()}.
start(HostType, Tag, WpoolOptsIn, ConnOpts) ->
    ProcName = mongoose_wpool:make_pool_name(elastic, HostType, Tag),
    WPoolOptions  = [{overrun_warning, infinity},
                     {overrun_handler, {error_logger, warning_report}},
                     {worker, {tirerl_worker, maps:to_list(ConnOpts)}}
                    | WpoolOptsIn],
    mongoose_wpool:start_sup_pool(elastic, ProcName, WPoolOptions).

-spec stop(mongooseim:host_type_or_global(), mongoose_wpool:tag()) -> ok.
stop(_, _) ->
    ok.
