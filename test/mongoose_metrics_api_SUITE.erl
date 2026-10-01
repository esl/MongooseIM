-module(mongoose_metrics_api_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("log_helper.hrl").

-define(HOST_TYPE, ~"localhost").
-define(HOST_TYPE_WITH_SPACE, ~"test host type").

all() ->
    [every_metric_type_resolves_to_a_graphql_type,
     unknown_metric_type_is_reported_as_an_error,
     unknown_metric_type_does_not_hide_the_remaining_metrics,
     prepare_name_normalizes_host_type_with_space,
     prepare_name_keeps_host_type_without_space_as_binary,
     prepare_name_does_not_match_already_normalized_host_type,
     prepare_name_matches_already_registered_atom,
     prepare_name_gives_empty_result_for_unregistered_segment,
     prepare_name_gives_empty_result_for_unregistered_segment_among_valid_ones,
     unregistered_segment_gives_empty_result_in_all_queries,
     registered_name_without_metrics_returns_empty_result,
     cluster_query_accepts_known_node,
     cluster_query_without_nodes_uses_all_nodes,
     keys_select_atom_datapoint,
     keys_select_integer_datapoint,
     empty_keys_select_all_datapoints,
     unknown_key_selects_nothing].

init_per_suite(Config) ->
    log_helper:set_up(),
    mongoose_config:set_opts(#{hosts => [?HOST_TYPE, ?HOST_TYPE_WITH_SPACE], host_types => []}),
    meck:new(mongoose_instrument_exometer, [no_link]),
    Config.

end_per_suite(_Config) ->
    meck:unload(mongoose_instrument_exometer),
    mongoose_config:erase_opts(),
    log_helper:tear_down().

init_per_testcase(_CaseName, Config) ->
    log_helper:subscribe(),
    meck:reset(mongoose_instrument_exometer),
    Config.

end_per_testcase(_CaseName, _Config) ->
    log_helper:unsubscribe().

every_metric_type_resolves_to_a_graphql_type(_Config) ->
    [begin
         mock_metric_values([{metric_name(Type), Dict}]),
         {ok, [{ok, Metric}]} = mongoose_metrics_api:get_metrics([?HOST_TYPE]),
         ?assertEqual({ok, Type}, mongoose_graphql_union:execute(Metric))
     end || {Type, Dict} <- metric_dicts()].

unknown_metric_type_is_reported_as_an_error(_Config) ->
    Name = metric_name(~"unknown"),
    mock_metric_values([{Name, [{unexpected_key, 1}]}]),
    ?assertEqual({ok, [{error, unknown_metric_type}]},
                 mongoose_metrics_api:get_metrics([?HOST_TYPE])),
    ?assertLog(error, #{what := unknown_metric_type}).

unknown_metric_type_does_not_hide_the_remaining_metrics(_Config) ->
    GaugeName = metric_name(~"GaugeMetric"),
    mock_metric_values([{metric_name(~"unknown"), [{unexpected_key, 1}]},
                        {GaugeName, [{value, 3}]}]),
    ?assertMatch({ok, [{error, unknown_metric_type},
                       {ok, #{~"type" := ~"gauge", ~"value" := 3}}]},
                 mongoose_metrics_api:get_metrics([?HOST_TYPE])),
    ?assertLog(error, #{what := unknown_metric_type}).

prepare_name_normalizes_host_type_with_space(_Config) ->
    assert_gauge_found_for([~"test_host_type"], [?HOST_TYPE_WITH_SPACE]).

prepare_name_keeps_host_type_without_space_as_binary(_Config) ->
    assert_gauge_found_for([?HOST_TYPE], [?HOST_TYPE]).

%% The already-underscored form must not match, unlike the correctly configured spelling.
prepare_name_does_not_match_already_normalized_host_type(_Config) ->
    Name = [~"test_host_type"],
    mock_metric_values_for(Name, [{Name, [{value, 3}]}]),
    ?assertMatch({ok, [{ok, #{~"type" := ~"gauge"}}]},
                 mongoose_metrics_api:get_metrics([?HOST_TYPE_WITH_SPACE])),
    ?assertEqual({ok, []}, mongoose_metrics_api:get_metrics(Name)).

prepare_name_matches_already_registered_atom(_Config) ->
    assert_gauge_found_for([xmpp_element_in], [~"xmpp_element_in"]).

%% An unknown segment gives an empty result without creating an atom or querying exometer.
prepare_name_gives_empty_result_for_unregistered_segment(_Config) ->
    Segment = ~"zzz_never_registered_metric_segment_9f3a1b",
    mock_metric_values([]),
    ?assertEqual({ok, []}, mongoose_metrics_api:get_metrics([Segment])),
    ?assertError(badarg, binary_to_existing_atom(Segment)),
    ?assertEqual(0, meck:num_calls(mongoose_instrument_exometer, get_metric_values, '_')).

prepare_name_gives_empty_result_for_unregistered_segment_among_valid_ones(_Config) ->
    mock_metric_values([]),
    ?assertEqual({ok, []},
                 mongoose_metrics_api:get_metrics([~"localhost", ~"xmpp_element_in",
                                                   ~"zzz_never_registered_segment_2d8c4e"])),
    ?assertEqual(0, meck:num_calls(mongoose_instrument_exometer, get_metric_values, '_')).

unregistered_segment_gives_empty_result_in_all_queries(_Config) ->
    Name = [~"zzz_never_registered_segment_71ab3f"],
    mock_metric_values([]),
    ?assertEqual({ok, []}, mongoose_metrics_api:get_metrics_as_dicts(Name, [])),
    Node = node(),
    ?assertMatch({ok, [{ok, #{~"node" := Node, ~"result" := []}}]},
                 mongoose_metrics_api:get_cluster_metrics_as_dicts(Name, [], [Node])),
    ?assertEqual(0, meck:num_calls(mongoose_instrument_exometer, get_metric_values, '_')).

%% A resolvable name that matches no metric is not an error, only the segments are validated.
registered_name_without_metrics_returns_empty_result(_Config) ->
    mock_metric_values([]),
    ?assertEqual({ok, []}, mongoose_metrics_api:get_metrics([~"xmpp_element_in"])),
    ?assertEqual(1, meck:num_calls(mongoose_instrument_exometer, get_metric_values, '_')).

cluster_query_accepts_known_node(_Config) ->
    mock_metric_values([{metric_name(~"GaugeMetric"), [{value, 3}]}]),
    Node = node(),
    ?assertMatch({ok, [{ok, #{~"node" := Node,
                              ~"result" := [{ok, #{~"dict" := [{ok, #{~"key" := value,
                                                                      ~"value" := 3}}]}}]}}]},
                 mongoose_metrics_api:get_cluster_metrics_as_dicts([?HOST_TYPE], [], [Node])).

cluster_query_without_nodes_uses_all_nodes(_Config) ->
    mock_metric_values([]),
    Nodes = [node() | nodes()],
    {ok, Results} = mongoose_metrics_api:get_cluster_metrics_as_dicts([?HOST_TYPE], [], []),
    ?assertEqual(Nodes, [Node || {ok, #{~"node" := Node}} <- Results]).

keys_select_atom_datapoint(_Config) ->
    ?assertEqual([{count, 10}], dict_entries([~"count"], [{count, 10}, {one, 1}])).

keys_select_integer_datapoint(_Config) ->
    ?assertEqual([{50, 2}], dict_entries([~"50"], [{50, 2}, {75, 3}, {mean, 2}])).

empty_keys_select_all_datapoints(_Config) ->
    ?assertEqual([{count, 10}, {one, 1}], dict_entries([], [{count, 10}, {one, 1}])).

%% An unknown key yields an empty dict rather than an error, and must not create an atom.
unknown_key_selects_nothing(_Config) ->
    Key = ~"zzz_never_registered_key_5c2e7d",
    ?assertEqual([], dict_entries([Key], [{count, 10}])),
    ?assertError(badarg, binary_to_existing_atom(Key)).

%% Helpers

metric_dicts() ->
    [{~"SpiralMetric", [{count, 10}, {one, 1}]},
     {~"CounterMetric", [{value, 5}, {ms_since_reset, 100}]},
     {~"GaugeMetric", [{value, 3}]},
     {~"HistogramMetric", [{n, 1}, {mean, 2}, {min, 1}, {max, 3}, {median, 2},
                              {50, 2}, {75, 3}, {90, 3}, {95, 3}, {99, 3}, {999, 3}]}].

metric_name(Type) ->
    [~"localhost", Type].

%% Queries for RegisteredName find the gauge, any other name finds nothing.
assert_gauge_found_for(RegisteredName, QueryName) ->
    mock_metric_values_for(RegisteredName, [{RegisteredName, [{value, 3}]}]),
    ?assertMatch({ok, [{ok, #{~"type" := ~"gauge", ~"value" := 3}}]},
                 mongoose_metrics_api:get_metrics(QueryName)).

mock_metric_values_for(RegisteredName, Values) ->
    meck:expect(mongoose_instrument_exometer, get_metric_values,
                fun(Name) when Name =:= RegisteredName -> Values;
                   (_) -> [] end).

dict_entries(Keys, Dict) ->
    mock_metric_values([{metric_name(~"SpiralMetric"), Dict}]),
    {ok, [{ok, #{~"dict" := Entries}}]} =
        mongoose_metrics_api:get_metrics_as_dicts([?HOST_TYPE], Keys),
    [{K, V} || {ok, #{~"key" := K, ~"value" := V}} <- Entries].

mock_metric_values(Values) ->
    meck:expect(mongoose_instrument_exometer, get_metric_values, fun(_) -> Values end).
