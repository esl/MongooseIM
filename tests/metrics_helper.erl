-module(metrics_helper).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

reset_counters(_Config) ->
    StopModMetrics =
        fun(Host) -> rpc:call(ct:get_config(ejabberd_node),
                              mod_metrics, stop, [Host]) end,
    RestartModMetrics =
        fun(Host) -> rpc:call(ct:get_config(ejabberd_node),
                              mod_metrics, start, [Host, []]) end,
    Hosts = escalus_ejabberd:get_global_option(hosts),

    rpc:call(ct:get_config(ejabberd_node), application, stop, [folsom]),
    lists:foreach(StopModMetrics, Hosts),

    rpc:call(ct:get_config(ejabberd_node), application, start, [folsom]),
    lists:foreach(RestartModMetrics, Hosts).


get_counter_value({_, _} = Counter) ->
    Result = rpc:call(ct:get_config(ejabberd_node),
                      folsom_metrics,
                      get_metric_value,
                      [Counter]),
    case Result of
        [{count, Total}, {one, _}] ->
            {value, Total};
        Value when is_integer(Value) ->
            {value, Value};
        _ ->
            {error, unknown_counter}
    end;
get_counter_value(CounterName) ->
    get_counter_value({ct:get_config(ejabberd_domain), CounterName}).

assert_counter(Value, CounterName) ->
    {value, Value} = get_counter_value(CounterName).
