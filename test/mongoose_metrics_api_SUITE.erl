-module(mongoose_metrics_api_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("log_helper.hrl").

-define(HOST_TYPE, <<"localhost">>).

all() ->
    [every_metric_type_resolves_to_a_graphql_type,
     unknown_metric_type_is_reported_as_an_error,
     unknown_metric_type_does_not_hide_the_remaining_metrics].

init_per_suite(Config) ->
    log_helper:set_up(),
    mongoose_config:set_opts(#{hosts => [?HOST_TYPE], host_types => []}),
    meck:new(mongoose_instrument_exometer, [no_link]),
    Config.

end_per_suite(_Config) ->
    meck:unload(mongoose_instrument_exometer),
    mongoose_config:erase_opts(),
    log_helper:tear_down().

init_per_testcase(_CaseName, Config) ->
    log_helper:subscribe(),
    Config.

end_per_testcase(_CaseName, _Config) ->
    log_helper:unsubscribe().

every_metric_type_resolves_to_a_graphql_type(_Config) ->
    [begin
         mock_metric_values([{metric_name(Type), Dict}]),
         {ok, [{ok, Metric}]} = mongoose_metrics_api:get_metrics(metric_name(Type)),
         ?assertEqual({ok, Type}, mongoose_graphql_union:execute(Metric))
     end || {Type, Dict} <- metric_dicts()].

unknown_metric_type_is_reported_as_an_error(_Config) ->
    Name = metric_name(<<"unknown">>),
    mock_metric_values([{Name, [{unexpected_key, 1}]}]),
    ?assertEqual({ok, [{error, unknown_metric_type}]},
                 mongoose_metrics_api:get_metrics(Name)),
    ?assertLog(error, #{what := unknown_metric_type}).

unknown_metric_type_does_not_hide_the_remaining_metrics(_Config) ->
    GaugeName = metric_name(<<"GaugeMetric">>),
    mock_metric_values([{metric_name(<<"unknown">>), [{unexpected_key, 1}]},
                        {GaugeName, [{value, 3}]}]),
    ?assertMatch({ok, [{error, unknown_metric_type},
                       {ok, #{<<"type">> := <<"gauge">>, <<"value">> := 3}}]},
                 mongoose_metrics_api:get_metrics([localhost])),
    ?assertLog(error, #{what := unknown_metric_type}).

metric_dicts() ->
    [{<<"SpiralMetric">>, [{count, 10}, {one, 1}]},
     {<<"CounterMetric">>, [{value, 5}, {ms_since_reset, 100}]},
     {<<"GaugeMetric">>, [{value, 3}]},
     {<<"HistogramMetric">>, [{n, 1}, {mean, 2}, {min, 1}, {max, 3}, {median, 2},
                              {50, 2}, {75, 3}, {90, 3}, {95, 3}, {99, 3}, {999, 3}]}].

metric_name(Type) ->
    [localhost, binary_to_atom(Type)].

mock_metric_values(Values) ->
    meck:expect(mongoose_instrument_exometer, get_metric_values, fun(_) -> Values end).
