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

get_rest_counter_values(CounterName) ->
    {value, ValueHost} = get_rest_counter_value(
            get_url(host_metric, CounterName), CounterName),
    {value, ValueTotal} = get_rest_counter_value(
            get_url(sum_metric, CounterName), CounterName),
    {value, ValueHost, ValueTotal}.

get_rest_counter_value({URL,Path}, CounterName) ->
    {ok, {{<<"200">>, <<"OK">>}, _, Json,_,_}} = fusco_request(URL, Path, "GET", [], infinity),
    case parse_json(Json) of
        {struct, [{<<"metric">>, Metric}]} ->
            extract_metric(Metric);
        {struct, [{<<"metrics">>, {struct, Metrics}}]} ->
            CounterNameB = atom_to_binary(CounterName, utf8),
            {CounterNameB, Metric} = lists:keyfind(CounterNameB, 1, Metrics),
            extract_metric(Metric)
    end.

get_url(Type, CounterName) ->
    Port = ct:get_config(ejabberd_metrics_rest_port),
    Host = ct:get_config(ejabberd_domain),
    {"http://localhost:" ++ integer_to_list(Port),
     list_to_binary("/metrics" ++ get_url_suffix(Type, Host, CounterName))}.

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

start_fusco() ->
    %% In R16B01 and above, we could use application:ensure_started/1
    %% instead.
    lists:foreach(
      fun(App) ->
	      case application:start(App) of
		  ok ->
		      ok;
		  {error, {already_started, App}} ->
		      ok;
		  {error, E} ->
		      error({cannot_start, E}, [App])
	      end
      end, [asn1,crypto,public_key,ssl,fusco]).
stop_fusco() ->
    [ok,ok,ok,ok,ok] = lists:map(fun application:stop/1,
                                 [fusco,ssl,public_key,crypto,asn1]).

fusco_request(URL, Path , Method, Hdrs, Timeout) ->
    {ok, P}=fusco:start(URL,[]),
    ok = fusco:connect(P),
    Result = fusco:request(P, Path, Method, Hdrs, [], Timeout),
    ok = fusco:disconnect(P),
    Result.
