-module(mongoose_instrument_metrics_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(LABELS, #{host_type => <<"localhost">>}).
-define(LABELS2, #{host_type => <<"test type">>}).
-define(HOST_TYPE, <<"localhost">>).
-define(HOST_TYPE2, <<"test type">>).

%% Setup and teardown

all() ->
    [{group, prometheus},
     {group, exometer},
     {group, exometer_global},
     {group, prometheus_and_exometer}
    ].

groups() ->
    [{prometheus, [parallel], [prometheus_skips_non_metric_event,
                               prometheus_counter_is_created_but_not_initialized,
                               prometheus_counter_is_updated_separately_for_different_labels,
                               prometheus_histogram_is_created_but_not_initialized,
                               prometheus_histogram_is_updated_separately_for_different_labels,
                               multiple_prometheus_metrics_are_updated]},
     {exometer, [parallel], [exometer_skips_non_metric_event,
                             exometer_spiral_is_created_and_initialized,
                             exometer_spiral_is_updated_separately_for_different_labels,
                             exometer_histogram_is_created_and_initialized,
                             exometer_histogram_is_updated_separately_for_different_labels,
                             multiple_exometer_metrics_are_updated]},
     {exometer_global, [parallel], [multiple_exometer_metrics_are_updated]},
     {prometheus_and_exometer, [parallel], [prometheus_and_exometer_metrics_are_updated]}
    ].

init_per_group(Group, Config) ->
    [application:ensure_all_started(App) || App <- apps(Group)],
    mongoose_config:set_opts(#{hosts => [?HOST_TYPE],
                               host_types => [?HOST_TYPE2],
                               instrumentation => opts(Group)}),
    Config1 = async_helper:start(Config, mongoose_instrument, start_link, []),
    mongoose_instrument:persist(),
    Config1 ++ extra_config(Group).

end_per_group(_Group, Config) ->
    async_helper:stop_all(Config),
    mongoose_config:erase_opts().

init_per_testcase(Case, Config) ->
    [{event, join_atoms(Case, event)} | Config].

end_per_testcase(_Case, _Config) ->
    ok.

apps(prometheus) -> [prometheus];
apps(exometer) -> [exometer_core];
apps(exometer_global) -> [exometer_core];
apps(prometheus_and_exometer) -> apps(prometheus) ++ apps(exometer).

opts(prometheus) -> #{prometheus => #{}};
opts(exometer) -> #{exometer => #{all_metrics_are_global => false}};
opts(exometer_global) -> #{exometer => #{all_metrics_are_global => true}};
opts(prometheus_and_exometer) -> maps:merge(opts(prometheus), opts(exometer)).

extra_config(exometer) -> [{prefix, ?HOST_TYPE}];
extra_config(exometer_global) -> [{prefix, global}];
extra_config(_Group) -> [].

%% Test cases

prometheus_skips_non_metric_event(Config) ->
    Event = ?config(event, Config),
    false = mongoose_instrument_prometheus:set_up(Event, ?LABELS, #{}),
    false = mongoose_instrument_prometheus:set_up(Event, ?LABELS, #{loglevel => error}).

prometheus_counter_is_created_but_not_initialized(Config) ->
    Event = ?config(event, Config),
    Metric = prom_name(Event, count),
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => spiral}}),
    ?assertEqual(undefined, prometheus_counter:value(Metric, [?HOST_TYPE])).

prometheus_counter_is_updated_separately_for_different_labels(Config) ->
    Event = ?config(event, Config),
    Metric = prom_name(Event, count),
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => spiral}}),
    ok = mongoose_instrument:set_up(Event, ?LABELS2, #{metrics => #{count => spiral}}),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 1}),
    ok = mongoose_instrument:execute(Event, ?LABELS2, #{count => 2}),
    ?assertEqual(1, prometheus_counter:value(Metric, [?HOST_TYPE])),
    ?assertEqual(2, prometheus_counter:value(Metric, [?HOST_TYPE2])).

prometheus_histogram_is_created_but_not_initialized(Config) ->
    Event = ?config(event, Config),
    Metric = prom_name(Event, time),
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{time => histogram}}),
    ?assertEqual(undefined, prometheus_histogram:value(Metric, [?HOST_TYPE])).

prometheus_histogram_is_updated_separately_for_different_labels(Config) ->
    Event = ?config(event, Config),
    Metric = prom_name(Event, time),
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{time => histogram}}),
    ok = mongoose_instrument:set_up(Event, ?LABELS2, #{metrics => #{time => histogram}}),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{time => 1}),
    ok = mongoose_instrument:execute(Event, ?LABELS2, #{time => 2}),
    ?assertMatch({[1, 0|_], 1}, prometheus_histogram:value(Metric, [?HOST_TYPE])),
    ?assertMatch({[0, 1|_], 2}, prometheus_histogram:value(Metric, [?HOST_TYPE2])).

multiple_prometheus_metrics_are_updated(Config) ->
    Event = ?config(event, Config),
    Counter = prom_name(Event, count),
    Histogram = prom_name(Event, time),
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => spiral,
                                                                   time => histogram}}),
    %% Update both metrics
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 1, time => 2}),
    ?assertEqual(1, prometheus_counter:value(Counter, [?HOST_TYPE])),
    HistogramValue = prometheus_histogram:value(Histogram, [?HOST_TYPE]),
    ?assertMatch({[0, 1|_], 2}, HistogramValue),

    %% Update only one metric
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 2}),
    ?assertEqual(3, prometheus_counter:value(Counter, [?HOST_TYPE])),
    ?assertEqual(HistogramValue, prometheus_histogram:value(Histogram, [?HOST_TYPE])),

    %% No update
    ok = mongoose_instrument:execute(Event, ?LABELS, #{something => irrelevant}),
    ?assertEqual(3, prometheus_counter:value(Counter, [?HOST_TYPE])),
    ?assertEqual(HistogramValue, prometheus_histogram:value(Histogram, [?HOST_TYPE])).

exometer_skips_non_metric_event(Config) ->
    Event = ?config(event, Config),
    false = mongoose_instrument_exometer:set_up(Event, ?LABELS, #{}),
    false = mongoose_instrument_exometer:set_up(Event, ?LABELS, #{loglevel => error}).

exometer_spiral_is_created_and_initialized(Config) ->
    Event = ?config(event, Config),
    Metric = [?HOST_TYPE, Event, count],
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => spiral}}),
    ?assertEqual({ok, [{count, 0}]}, exometer:get_value(Metric, count)).

exometer_spiral_is_updated_separately_for_different_labels(Config) ->
    Event = ?config(event, Config),
    Metric1 = [?HOST_TYPE, Event, count],
    Metric2 = [<<"test_type">>, Event, count],
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => spiral}}),
    ok = mongoose_instrument:set_up(Event, ?LABELS2, #{metrics => #{count => spiral}}),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 1}),
    ok = mongoose_instrument:execute(Event, ?LABELS2, #{count => 2}),
    ?assertEqual({ok, [{count, 1}]}, exometer:get_value(Metric1, count)),
    ?assertEqual({ok, [{count, 2}]}, exometer:get_value(Metric2, count)).

exometer_histogram_is_created_and_initialized(Config) ->
    Event = ?config(event, Config),
    Metric = [?HOST_TYPE, Event, time],
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{time => histogram}}),
    ?assertEqual({ok, [{mean, 0}]}, exometer:get_value(Metric, mean)).

exometer_histogram_is_updated_separately_for_different_labels(Config) ->
    Event = ?config(event, Config),
    Metric1 = [?HOST_TYPE, Event, time],
    Metric2 = [<<"test_type">>, Event, time],
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{time => histogram}}),
    ok = mongoose_instrument:set_up(Event, ?LABELS2, #{metrics => #{time => histogram}}),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{time => 1}),
    ok = mongoose_instrument:execute(Event, ?LABELS2, #{time => 3}),
    ?assertEqual({ok, [{mean, 1}]}, exometer:get_value(Metric1, mean)),
    ?assertEqual({ok, [{mean, 3}]}, exometer:get_value(Metric2, mean)).

multiple_exometer_metrics_are_updated(Config) ->
    Event = ?config(event, Config),
    Prefix = ?config(prefix, Config),
    Counter = [Prefix, Event, count],
    Histogram = [Prefix, Event, time],
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => spiral,
                                                                   time => histogram}}),
    %% Update both metrics
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 1, time => 2}),
    ?assertEqual({ok, [{count, 1}]}, exometer:get_value(Counter, count)),
    ?assertEqual({ok, [{mean, 2}]}, exometer:get_value(Histogram, mean)),

    %% Update only one metric
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 5}),
    ?assertEqual({ok, [{count, 6}]}, exometer:get_value(Counter, count)),
    ?assertEqual({ok, [{mean, 2}]}, exometer:get_value(Histogram, mean)),

    %% No update
    ok = mongoose_instrument:execute(Event, ?LABELS, #{something => irrelevant}),
    ?assertEqual({ok, [{count, 6}]}, exometer:get_value(Counter, count)),
    ?assertEqual({ok, [{mean, 2}]}, exometer:get_value(Histogram, mean)).

prometheus_and_exometer_metrics_are_updated(Config) ->
    Event = ?config(event, Config),
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => spiral,
                                                                   time => histogram}}),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 1, time => 2}),
    ?assertEqual({ok, [{count, 1}]}, exometer:get_value([?HOST_TYPE, Event, count], count)),
    ?assertEqual({ok, [{mean, 2}]}, exometer:get_value([?HOST_TYPE, Event, time], mean)),
    ?assertEqual(1, prometheus_counter:value(prom_name(Event, count), [?HOST_TYPE])),
    ?assertMatch({[0, 1|_], 2}, prometheus_histogram:value(prom_name(Event, time), [?HOST_TYPE])).

%% Helpers

join_atoms(A1, A2) ->
    list_to_atom(join_atoms_to_list(A1, A2)).

prom_name(EventName, MetricName) ->
    join_atoms_to_list(EventName, MetricName).

join_atoms_to_list(A1, A2) ->
    atom_to_list(A1) ++ "_" ++ atom_to_list(A2).
