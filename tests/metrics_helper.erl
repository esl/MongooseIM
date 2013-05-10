-module(metrics_helper).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

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

get_rest_counter_values(CounterName) ->
    {value, ValueHost} = get_rest_counter_value(
            get_url(host_metric, CounterName), CounterName),
    {value, ValueTotal} = get_rest_counter_value(
            get_url(sum_metric, CounterName), CounterName),
    {value, ValueHost, ValueTotal}.

get_rest_counter_value(URL, CounterName) ->
    {ok, {{200, _}, _, Json}} = lhttpc:request(URL, 'GET', [], infinity),
    case parse_json(Json) of
        {struct, [{<<"metric">>, Metric}]} ->
            extract_metric(Metric);
        {struct, [{<<"metrics">>, {struct, Metrics}}]} ->
            CounterNameB = atom_to_binary(CounterName, utf8),
            {CounterNameB, Metric} = lists:keyfind(CounterNameB, 1, Metrics),
            extract_metric(Metric)
    end.

get_url(Type, CounterName) ->
    Port = rpc:call(ct:get_config(ejabberd_node),
                    gen_mod, get_module_opt,
                    [global, mod_metrics, port, undefined]),
    Host = ct:get_config(ejabberd_domain),
    "http://localhost:" ++ integer_to_list(Port) ++ "/metrics" ++
    get_url_suffix(Type, Host, CounterName).

get_url_suffix(host_metric, Host, CounterName) ->
    "/host/" ++ binary_to_list(Host) ++ "/" ++ atom_to_list(CounterName);
get_url_suffix(host_metrics, Host, _) ->
    "/host/" ++ binary_to_list(Host);
get_url_suffix(sum_metrics, _, _) ->
    "/m/";
get_url_suffix(sum_metric, _, CounterName) ->
    "/m/" ++ atom_to_list(CounterName).

extract_metric({struct, [{<<"count">>, Total}, {<<"one">>, _}]}) ->
    {value, Total};
extract_metric(Value) when is_integer(Value) ->
    {value, Value};
extract_metric(_) ->
    {error, unknown_counter}.

assert_counter(Value, CounterName) ->
    {value, Value} = get_counter_value(CounterName).

assert_rest_counters(ValueHost, ValueTotal, CounterName) ->
    ValueTuples = [{value, ValueHost}, {value, ValueHost},
                   {value, ValueTotal}, {value, ValueTotal}],
    Types = [host_metric, host_metrics, sum_metric, sum_metrics],
    ValueTuples = [get_rest_counter_value(get_url(Type, CounterName),
                                          CounterName) || Type <- Types].

parse_json(Json) ->
    rpc:call(ct:get_config(ejabberd_node), mochijson2, decode, [Json]).

start_lhttpc() ->
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(lhttpc).

stop_lhttpc() ->
    ok = application:stop(lhttpc),
    ok = application:stop(ssl),
    ok = application:stop(public_key),
    ok = application:stop(crypto).
