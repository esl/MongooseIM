-module(prometheus_endpoint_SUITE).

-include_lib("eunit/include/eunit.hrl").

-compile([export_all, nowarn_export_all]).

-import(config_parser_helper, [config/2]).

-define(HOST_TYPE, <<"test type">>).
-define(PORT, 9099).

%% Setup and teardown

all() ->
    [{group, all_tests}].

groups() ->
    [{all_tests, [parallel], [global_metrics_are_exposed,
                              host_type_metrics_are_exposed]}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(required_apps()),
    mongoose_config:set_opts(opts()),
    Config1 = async_helper:start(Config, [{mongoose_instrument, start_link, []},
                                          {mim_ct_sup, start_link, [ejabberd_sup]},
                                          {mongoose_listener_sup, start_link, []}]),
    mongoose_listener:start(),
    Config1.

end_per_suite(Config) ->
    mongoose_listener:stop(),
    async_helper:stop_all(Config),
    mongoose_config:erase_opts(),
    lists:foreach(fun(App) -> ok = application:stop(App) end, lists:reverse(required_apps())).

%% Test cases

global_metrics_are_exposed(_Config) ->
    test_metrics(global_event, #{}).

host_type_metrics_are_exposed(_Config) ->
    test_metrics(host_type_event, #{host_type => ?HOST_TYPE}).

test_metrics(Event, Labels) ->
    ok = mongoose_instrument:set_up(Event, Labels, #{metrics => #{requests => spiral,
                                                                  sessions => counter,
                                                                  seconds => gauge,
                                                                  time => histogram}}),
    ok = mongoose_instrument:execute(Event, Labels, #{requests => 1,
                                                      sessions => 5,
                                                      seconds => 10,
                                                      time => 2}),
    Scraped = scrape(),
    check_counter(Event, requests, Labels, 1, Scraped), % 'spiral' is a Prometheus counter
    check_gauge(Event, sessions, Labels, 5, Scraped), % 'counter' is a Prometheus gauge
    check_gauge(Event, seconds, Labels, 10, Scraped),
    check_histogram(Event, time, Labels, #{count => 1, sum => 2, bucket_num => 32}, Scraped).

%% Checks for the parsed metrics

check_counter(Event, Metric, Labels, ExpValue, Scraped) ->
    [Type, Help, Value] = get_metric([Event, Metric], Scraped),
    ?assertEqual({<<"TYPE">>, <<"counter">>}, Type),
    ?assertEqual({<<"HELP">>, help(Event, Metric)}, Help),
    ?assertEqual({Labels, ExpValue}, Value).

check_gauge(Event, Metric, Labels, ExpValue, Scraped) ->
    [Type, Help, Value] = get_metric([Event, Metric], Scraped),
    ?assertEqual({<<"TYPE">>, <<"gauge">>}, Type),
    ?assertEqual({<<"HELP">>, help(Event, Metric)}, Help),
    ?assertEqual({Labels, ExpValue}, Value).

check_histogram(Event, Metric, Labels, ExpValues, Scraped) ->
    #{count := ExpCount, sum := ExpSum, bucket_num := ExpBucketNum} = ExpValues,
    [Type, Help] = get_metric([Event, Metric], Scraped),
    ?assertEqual({<<"TYPE">>, <<"histogram">>}, Type),
    ?assertEqual({<<"HELP">>, help(Event, Metric)}, Help),
    [Count] = get_metric([Event, Metric, count], Scraped),
    ?assertEqual({Labels, ExpCount}, Count),
    [Sum] = get_metric([Event, Metric, sum], Scraped),
    ?assertEqual({Labels, ExpSum}, Sum),
    Buckets = get_metric([Event, Metric, bucket], Scraped),
    ?assertEqual(ExpBucketNum, length(Buckets)),
    check_buckets(ExpCount, Labels, Buckets).

%% Check that the histogram buckets have growing thresholds and counts,
%% and that the last bucket has the expected total count (because they are cumulative).
check_buckets(ExpCount, Labels, Buckets) ->
    InitState = #{labels => Labels, last_count => 0, last_threshold => 0},
    #{final_count := LastCount} = lists:foldl(fun check_bucket/2, InitState, Buckets),
    ?assertEqual(ExpCount, LastCount).

check_bucket({Labels, Count}, State) ->
    #{labels := BaseLabels, last_count := LastCount, last_threshold := LastThreshold} = State,
    {ThresholdBin, Labels1} = maps:take(le, Labels),
    ?assertEqual(Labels1, BaseLabels),
    ?assert(Count >= LastCount),
    case ThresholdBin of
        <<"+Inf">> ->
            #{final_count => Count};
        _ ->
            Threshold = binary_to_integer(ThresholdBin),
            ?assert(Threshold > LastThreshold),
            State#{last_count => Count, last_threshold => Threshold}
    end.

help(Event, Metric) ->
    <<"Event: ", (atom_to_binary(Event))/binary, ", Metric: ", (atom_to_binary(Metric))/binary>>.

get_metric(Parts, Scraped) when is_list(Parts) ->
    get_metric(binary_name(Parts), Scraped);
get_metric(Name, Scraped) when is_binary(Name) ->
    [{LabelsOrKey, Value} || {N, LabelsOrKey, Value} <- Scraped, N =:= Name].

binary_name(Atoms) ->
    list_to_binary(string:join([atom_to_list(Atom) || Atom <- Atoms], "_")).

%% Scrape and parse all metrics from the Prometheus endpoint
%% All metrics are parsed to ensure that the whole document is well-formed.
scrape() ->
    {ok, ConnPid} = gun:open("127.0.0.1", ?PORT),
    {ok, _} = gun:await_up(ConnPid),
    StreamRef = gun:get(ConnPid, "/metrics"),
    {ok, Body} = gun:await_body(ConnPid, StreamRef),
    ct:log("Scraping prometheus metrics. Received body:~n~s", [Body]),
    Metrics = parse_body(Body),
    gun:close(ConnPid),
    Metrics.

parse_body(Body) ->
    Lines = binary:split(Body, <<"\n">>, [global]),
    RegexMap = #{metric => metric_regex(), metadata => metadata_regex()},
    [parse_line(Line, RegexMap) || Line <- Lines, Line =/= <<>>].

parse_line(Line, #{metric := MetricRegex, metadata := MetadataRegex}) ->
    case re:run(Line, MetricRegex, [{capture, all_but_first}]) of
        nomatch ->
            parse_metadata_line(Line, MetadataRegex);
        {match, Matches} ->
            [MetricName | Rest] = [binary:part(Line, Start, Len) || {Start, Len} <- Matches,
                                                                    Start =/= -1],
            {Labels, MetricValue} = group_labels(Rest, []),
            {MetricName, maps:from_list(Labels), parse_metric_value(MetricValue)}
    end.

metric_regex() ->
    KVRegex = "(\\w+)=\"(.+?)\"",
    Regex = "^(\\w+)(?:\{" ++ KVRegex ++ "(?:," ++ KVRegex ++ ")*\})? (\\d+(?:\.\\d+)?)$",
    {ok, MP} = re:compile(Regex),
    MP.

metadata_regex() ->
    {ok, MP} = re:compile("^# (TYPE|HELP) (\\w+) (.+)$"),
    MP.

parse_metadata_line(Line, Regex) ->
    case re:run(Line, Regex, [{capture, all_but_first}]) of
        nomatch ->
            ct:fail("Unable to parse line ~s", [Line]);
        {match, Matches} ->
            [Key, MetricName, Data] = [binary:part(Line, Start, Len) || {Start, Len} <- Matches],
            {MetricName, Key, Data}
    end.

parse_metric_value(Value) ->
    try binary_to_integer(Value)
    catch error:badarg -> binary_to_float(Value)
    end.

group_labels([LabelKey, LabelValue | Rest], AccLabels) ->
    group_labels(Rest, [{binary_to_atom(LabelKey), LabelValue} | AccLabels]);
group_labels([MetricValue], AccLabels) ->
    {AccLabels, MetricValue}.

%% Helpers for setup and teardown

required_apps() ->
    [gun, cowboy, prometheus, prometheus_httpd, prometheus_cowboy].

opts() ->
    #{hosts => [],
      host_types => [?HOST_TYPE],
      internal_databases => #{},
      instrumentation => config([instrumentation], #{prometheus => #{}}),
      listen => [config([listen, http],
                        #{port => ?PORT,
                          handlers => [config([listen, http, handlers, mongoose_prometheus_handler],
                                              #{host => '_', path => "/metrics"})]
                         })]
     }.
