-module(mongoose_instrument_metrics_SUITE).
-compile([export_all, nowarn_export_all]).

-include("log_helper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(LABELS, #{host_type => <<"localhost">>}).
-define(LABELS2, #{host_type => <<"test type">>}).
-define(HOST_TYPE, <<"localhost">>).
-define(HOST_TYPE2, <<"test type">>).

-import(mongoose_instrument_exometer, [exometer_metric_name/3]).
%% Setup and teardown

all() ->
    [{group, prometheus},
     {group, exometer},
     {group, exometer_global},
     {group, prometheus_and_exometer}
    ].

groups() ->
    [{prometheus, [parallel], [prometheus_skips_non_metric_event,
                               prometheus_gauge_is_created_and_updated,
                               prometheus_gauge_is_updated_separately_for_different_labels,
                               prometheus_gauge_counter_is_created_and_updated,
                               prometheus_gauge_counter_is_updated_separately_for_different_labels,
                               prometheus_counter_is_created_and_updated,
                               prometheus_counter_cannot_be_decreased,
                               prometheus_counter_is_updated_separately_for_different_labels,
                               prometheus_histogram_is_created_and_updated,
                               prometheus_histogram_is_updated_separately_for_different_labels,
                               multiple_prometheus_metrics_are_updated]},
     {exometer, [parallel], [exometer_skips_non_metric_event,
                             exometer_gauge_is_created_and_updated,
                             exometer_gauge_is_updated_separately_for_different_labels,
                             exometer_counter_is_created_and_updated,
                             exometer_counter_is_updated_separately_for_different_labels,
                             exometer_spiral_is_created_and_updated,
                             exometer_spiral_cannot_be_decreased,
                             exometer_spiral_is_updated_separately_for_different_labels,
                             exometer_histogram_is_created_and_updated,
                             exometer_histogram_is_updated_separately_for_different_labels,
                             exometer_metric_name_supports_random_labels,
                             multiple_exometer_metrics_are_updated]},
     {exometer_global, [parallel], [multiple_exometer_metrics_are_updated,
                                    exometer_metric_name_supports_random_labels]},
     {prometheus_and_exometer, [parallel], [prometheus_and_exometer_metrics_are_updated]}
    ].

init_per_suite(Config) ->
    log_helper:set_up(),
    Config.

end_per_suite(_Config) ->
    log_helper:tear_down().

init_per_group(Group, Config) ->
    [{ok, _} = application:ensure_all_started(App) || App <- apps(Group)],
    mongoose_config:set_opts(#{hosts => [?HOST_TYPE],
                               host_types => [?HOST_TYPE2],
                               instrumentation => opts(Group)}),
    Config1 = async_helper:start(Config, mongoose_instrument, start_link, []),
    mongoose_instrument:persist(),
    Config1 ++ extra_config(Group).

end_per_group(Group, Config) ->
    async_helper:stop_all(Config),
    mongoose_config:erase_opts(),
    lists:foreach(fun(App) -> ok = application:stop(App) end, apps(Group)).

init_per_testcase(Case, Config) ->
    log_helper:subscribe(),
    [{event, join_atoms(Case, event)} | Config].

end_per_testcase(_Case, _Config) ->
    log_helper:unsubscribe().

apps(prometheus) -> [prometheus, prometheus_httpd, prometheus_cowboy];
apps(exometer) -> [exometer_core];
apps(exometer_global) -> [exometer_core];
apps(prometheus_and_exometer) -> apps(prometheus) ++ apps(exometer).

opts(prometheus) ->
    #{prometheus => #{}};
opts(exometer) ->
    #{exometer => #{all_metrics_are_global => false, report => #{}}};
opts(exometer_global) ->
    #{exometer => #{all_metrics_are_global => true, report => #{}}};
opts(prometheus_and_exometer) ->
    maps:merge(opts(prometheus), opts(exometer)).

extra_config(exometer) -> [{prefix, ?HOST_TYPE}];
extra_config(exometer_global) -> [{prefix, global}];
extra_config(_Group) -> [].

%% Test cases

prometheus_skips_non_metric_event(Config) ->
    Event = ?config(event, Config),
    false = mongoose_instrument_prometheus:set_up(Event, ?LABELS, #{}),
    false = mongoose_instrument_prometheus:set_up(Event, ?LABELS, #{loglevel => error}).

prometheus_gauge_is_created_and_updated(Config) ->
    Event = ?config(event, Config),
    Metric = prom_name(Event, count),

    %% Prometheus gauge has no initial value, and reports the last registered value
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => gauge}}),
    ?assertEqual(undefined, prometheus_gauge:value(Metric, [?HOST_TYPE])),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 1}),
    ?assertEqual(1, prometheus_gauge:value(Metric, [?HOST_TYPE])),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => -2}),
    ?assertEqual(-2, prometheus_gauge:value(Metric, [?HOST_TYPE])).

prometheus_gauge_is_updated_separately_for_different_labels(Config) ->
    Event = ?config(event, Config),
    Metric = prom_name(Event, count),
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => gauge}}),
    ok = mongoose_instrument:set_up(Event, ?LABELS2, #{metrics => #{count => gauge}}),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 1}),
    ok = mongoose_instrument:execute(Event, ?LABELS2, #{count => 2}),
    ?assertEqual(1, prometheus_gauge:value(Metric, [?HOST_TYPE])),
    ?assertEqual(2, prometheus_gauge:value(Metric, [?HOST_TYPE2])).

prometheus_gauge_counter_is_created_and_updated(Config) ->
    Event = ?config(event, Config),
    Metric = prom_name(Event, count),

    %% Prometheus gauge used as a counter starts at zero and can be incremented/decremented
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => counter}}),
    ?assertEqual(0, prometheus_gauge:value(Metric, [?HOST_TYPE])),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 1}),
    ?assertEqual(1, prometheus_gauge:value(Metric, [?HOST_TYPE])),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => -2}),
    ?assertEqual(-1, prometheus_gauge:value(Metric, [?HOST_TYPE])).

prometheus_gauge_counter_is_updated_separately_for_different_labels(Config) ->
    Event = ?config(event, Config),
    Metric = prom_name(Event, count),
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => counter}}),
    ok = mongoose_instrument:set_up(Event, ?LABELS2, #{metrics => #{count => counter}}),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 1}),
    ok = mongoose_instrument:execute(Event, ?LABELS2, #{count => 2}),
    ?assertEqual(1, prometheus_gauge:value(Metric, [?HOST_TYPE])),
    ?assertEqual(2, prometheus_gauge:value(Metric, [?HOST_TYPE2])).

prometheus_counter_is_created_and_updated(Config) ->
    Event = ?config(event, Config),
    Metric = prom_name(Event, count),

    %% Prometheus counter starts at zero, and reports the sum of all values
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => spiral}}),
    ?assertEqual(0, prometheus_counter:value(Metric, [?HOST_TYPE])),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 1}),
    ?assertEqual(1, prometheus_counter:value(Metric, [?HOST_TYPE])),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 2}),
    ?assertEqual(3, prometheus_counter:value(Metric, [?HOST_TYPE])).

prometheus_counter_cannot_be_decreased(Config) ->
    Event = ?config(event, Config),
    Metric = prom_name(Event, count),

    %% Prometheus counter starts at zero, and cannot be decreased
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => spiral}}),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => -1}),
    ?assertEqual(0, prometheus_counter:value(Metric, [?HOST_TYPE])),
    ?assertLog(error, #{what := event_handler_failed}).

prometheus_counter_is_updated_separately_for_different_labels(Config) ->
    Event = ?config(event, Config),
    Metric = prom_name(Event, count),
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => spiral}}),
    ok = mongoose_instrument:set_up(Event, ?LABELS2, #{metrics => #{count => spiral}}),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 1}),
    ok = mongoose_instrument:execute(Event, ?LABELS2, #{count => 2}),
    ?assertEqual(1, prometheus_counter:value(Metric, [?HOST_TYPE])),
    ?assertEqual(2, prometheus_counter:value(Metric, [?HOST_TYPE2])).

prometheus_histogram_is_created_and_updated(Config) ->
    Event = ?config(event, Config),
    Metric = prom_name(Event, time),
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{time => histogram}}),

    %% Prometheus histogram shows no value if there is no data
    ?assertEqual(undefined, prometheus_histogram:value(Metric, [?HOST_TYPE])),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{time => 1}),
    ?assertMatch({[1, 0|_], 1}, prometheus_histogram:value(Metric, [?HOST_TYPE])),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{time => 1}),
    ?assertMatch({[2, 0|_], 2}, prometheus_histogram:value(Metric, [?HOST_TYPE])),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{time => 2}),
    ?assertMatch({[2, 1|_], 4}, prometheus_histogram:value(Metric, [?HOST_TYPE])).

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

exometer_gauge_is_created_and_updated(Config) ->
    Event = ?config(event, Config),
    Metric = [?HOST_TYPE, Event, count],

    %% Exometer gauge starts at zero, and reports the last registered value
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => gauge}}),
    ?assertEqual({ok, [{value, 0}]}, exometer:get_value(Metric, value)),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 1}),
    ?assertEqual({ok, [{value, 1}]}, exometer:get_value(Metric, value)),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 2}),
    ?assertEqual({ok, [{value, 2}]}, exometer:get_value(Metric, value)).

exometer_gauge_is_updated_separately_for_different_labels(Config) ->
    Event = ?config(event, Config),
    Metric1 = [?HOST_TYPE, Event, count],
    Metric2 = [<<"test_type">>, Event, count],
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => gauge}}),
    ok = mongoose_instrument:set_up(Event, ?LABELS2, #{metrics => #{count => gauge}}),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 1}),
    ok = mongoose_instrument:execute(Event, ?LABELS2, #{count => 2}),
    ?assertEqual({ok, [{value, 1}]}, exometer:get_value(Metric1, value)),
    ?assertEqual({ok, [{value, 2}]}, exometer:get_value(Metric2, value)).

exometer_counter_is_created_and_updated(Config) ->
    Event = ?config(event, Config),
    Metric = [?HOST_TYPE, Event, count],

    %% Exometer counter starts at zero, and can be incremented/decremented
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => counter}}),
    ?assertEqual({ok, [{value, 0}]}, exometer:get_value(Metric, value)),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 1}),
    ?assertEqual({ok, [{value, 1}]}, exometer:get_value(Metric, value)),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => -2}),
    ?assertEqual({ok, [{value, -1}]}, exometer:get_value(Metric, value)).

exometer_counter_is_updated_separately_for_different_labels(Config) ->
    Event = ?config(event, Config),
    Metric1 = [?HOST_TYPE, Event, count],
    Metric2 = [<<"test_type">>, Event, count],
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => counter}}),
    ok = mongoose_instrument:set_up(Event, ?LABELS2, #{metrics => #{count => counter}}),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 1}),
    ok = mongoose_instrument:execute(Event, ?LABELS2, #{count => 2}),
    ?assertEqual({ok, [{value, 1}]}, exometer:get_value(Metric1, value)),
    ?assertEqual({ok, [{value, 2}]}, exometer:get_value(Metric2, value)).

exometer_spiral_is_created_and_updated(Config) ->
    Event = ?config(event, Config),
    Metric = [?HOST_TYPE, Event, count],

    %% Exometer spiral starts at zero, and reports the sum of all values
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => spiral}}),
    ?assertEqual({ok, [{count, 0}]}, exometer:get_value(Metric, count)),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 1}),
    ?assertEqual({ok, [{count, 1}]}, exometer:get_value(Metric, count)),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => 2}),
    ?assertEqual({ok, [{count, 3}]}, exometer:get_value(Metric, count)).

exometer_spiral_cannot_be_decreased(Config) ->
    Event = ?config(event, Config),
    Metric = [?HOST_TYPE, Event, count],

    %% Exometer spiral starts at zero, and cannot be decreased
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{count => spiral}}),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{count => -1}),
    ?assertEqual({ok, [{count, 0}]}, exometer:get_value(Metric, count)),
    ?assertLog(error, #{what := event_handler_failed}).

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

exometer_histogram_is_created_and_updated(Config) ->
    Event = ?config(event, Config),
    Metric = [?HOST_TYPE, Event, time],

    %% Exometer mean value is zero if there is no data
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{time => histogram}}),
    ?assertEqual({ok, [{mean, 0}]}, exometer:get_value(Metric, mean)),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{time => 1}),
    ?assertEqual({ok, [{mean, 1}]}, exometer:get_value(Metric, mean)),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{time => 3}),
    ?assertEqual({ok, [{mean, 2}]}, exometer:get_value(Metric, mean)).

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

exometer_metric_name_supports_random_labels(Config) ->
    Prefix = ?config(prefix, Config),
    HostTypeLabel = #{host_type => ?HOST_TYPE},
    logger_ct_backend:start(local()),

    logger_ct_backend:capture(warning, local()),
    assert_exometer_metric_name(global, [c2s], #{connection_type => c2s}),
    assert_exometer_metric_name(Prefix, [c2s], HostTypeLabel#{connection_type => c2s}),
    logger_ct_backend:stop_capture(local()),
    FilterFun = fun(_, Msg) -> re:run(Msg, "what: default_exometer_labels_conversion") /= nomatch end,
    NoWarnings = logger_ct_backend:recv(FilterFun),
    ?assertEqual(0, length(NoWarnings)),

    logger_ct_backend:capture(warning, local()),
    UnknownLabels = #{unexpected_label1 => value1, unexpected_label2 => value2},
    Suffix = [maps:get(K, UnknownLabels) || K <- lists:sort(maps:keys(UnknownLabels))],
    assert_exometer_metric_name(global, Suffix, UnknownLabels),
    assert_exometer_metric_name(Prefix, Suffix, maps:merge(HostTypeLabel, UnknownLabels)),
    logger_ct_backend:stop_capture(local()),
    Warnings = logger_ct_backend:recv(FilterFun),
    ?assertEqual(2, length(Warnings)),

    logger_ct_backend:stop(local()),
    ok.

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

local() ->
    #{node => node()}.

assert_exometer_metric_name(Prefix, Suffix, Labels) ->
    MetricName = metric_name,
    EventName = event_name,
    ?assertEqual([Prefix, EventName] ++ Suffix ++ [MetricName],
                 exometer_metric_name(EventName, Labels, MetricName)).

join_atoms(A1, A2) ->
    list_to_atom(join_atoms_to_list(A1, A2)).

prom_name(EventName, MetricName) ->
    join_atoms_to_list(EventName, MetricName).

join_atoms_to_list(A1, A2) ->
    atom_to_list(A1) ++ "_" ++ atom_to_list(A2).
