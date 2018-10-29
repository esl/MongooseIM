-module(mongoose_wpool_elastic).
-behaviour(mongoose_wpool).

-export([init/0]).
-export([start/4]).
-export([stop/2]).

init() ->
    tirerl:start(),
    ok.

start(Host, Tag, WpoolOptsIn, ConnOpts) ->
    ElasticHost = proplists:get_value(host, ConnOpts, "localhost"),
    Port = proplists:get_value(port, ConnOpts, 9200),
    PoolName = mongoose_wpool:make_pool_name(elastic, Host, Tag),
    Opts = [{host, list_to_binary(ElasticHost)}, {port, Port}],
    WPoolOptions  = [{overrun_warning, infinity},
                     {overrun_handler, {error_logger, warning_report}},
                     {worker, {tirerl_worker, Opts}}
                    | WpoolOptsIn],
    case mongoose_wpool:start_sup_pool(elastic, PoolName, WPoolOptions) of
        {ok, Pid} ->
            {external, Pid};
        Other ->
            Other
    end.

stop(_, _) ->
    ok.

