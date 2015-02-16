-module(metrics_helper).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).


get_counter_value({_, _} = Counter) ->
    Result = rpc:call(ct:get_config(ejabberd_node),
                      mongoose_metrics,
                      get_metric_value,
                      [Counter]),
    case Result of
        {ok, [{count, Total}, {one, _}]} ->
            {value, Total};
        {ok, [{value, Value} | _]} when is_integer(Value) ->
            {value, Value};
        {ok, Value} ->
            {value, Value};
        _ ->
            {error, unknown_counter}
    end;
get_counter_value([Host, Metric | _ ]) ->
    get_counter_value({Host, Metric});
get_counter_value(CounterName) ->
    get_counter_value({ct:get_config(ejabberd_domain), CounterName}).


assert_counter(Value, CounterName) ->
    {value, Value} = get_counter_value(CounterName).
