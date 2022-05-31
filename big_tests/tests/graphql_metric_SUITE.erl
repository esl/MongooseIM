-module(graphql_metric_SUITE).

 -include_lib("common_test/include/ct.hrl").
 -include_lib("eunit/include/eunit.hrl").
 -include_lib("exml/include/exml.hrl").

 -compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute_auth/2, init_admin_handler/1]).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
     [{group, metrics}].

groups() ->
     [{metrics, [], metrics_handler()}].

metrics_handler() ->
    [get_metrics,
     get_metrics_as_dicts,
     get_metrics_as_dicts_with_key_one].

init_per_suite(Config) ->
    escalus:init_per_suite(init_admin_handler(Config)).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
     escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
     escalus:end_per_testcase(CaseName, Config).

get_metrics(Config) ->
    Vars = #{},
    Result = execute_auth(#{query => get_all_metrics_call(), variables => Vars,
                            operationName => <<"Q1">>}, Config),
    ct:fail(Result),
    ParsedResult = ok_result(<<"metric">>, <<"getMetrics">>, Result),
    ?assertEqual([], ParsedResult).

get_metrics_as_dicts(Config) ->
    Vars = #{},
    Result = execute_auth(#{query => get_all_metrics_as_dicts_call(), variables => Vars,
                            operationName => <<"Q1">>}, Config),
    ct:fail(Result),
    ParsedResult = ok_result(<<"metric">>, <<"getMetricsAsDicts">>, Result),
    ?assertEqual([], ParsedResult).

get_metrics_as_dicts_with_key_one(Config) ->
    Vars = #{},
    Result = execute_auth(#{query => get_all_metrics_as_dicts_with_key_one_call(),
                            variables => Vars,
                            operationName => <<"Q1">>}, Config),
    ct:fail(Result),
    ParsedResult = ok_result(<<"metric">>, <<"getMetricsAsDicts">>, Result),
    ?assertEqual([], ParsedResult).

get_all_metrics_call() ->
    <<"query Q1
           {metric
               {getMetrics {
                     ... on HistogramMetric
                     { name type n mean min max median p50 p75 p90 p95 p99 p999 }
                     ... on CounterMetric
                     { name type value ms_since_reset }
                     ... on SpiralMetric
                     { name type one count }
                     ... on GaugeMetric
                     { name type value }
                     ... on MergedInetStatsMetric
                     { name type connections recv_cnt recv_max recv_oct
                       send_cnt send_max send_oct send_pend }
                     ... on VMStatsMemoryMetric
                     { name type total processes_used atom_used binary ets system }
                     ... on VMSystemInfoMetric
                     { name type port_count port_limit process_count process_limit ets_limit }
                     ... on ProbeQueuesMetric
                     { name type type fsm regular total }
                 }
               }
           }">>.

get_all_metrics_as_dicts_call() ->
    <<"query Q1
           {metric
               {getMetricsAsDicts { name dict { key value }}}}">>.

get_all_metrics_as_dicts_with_key_one_call() ->
    <<"query Q1
           {metric
               {getMetricsAsDicts(filterKeys: [\"one\"]) { name dict { key value }}}}">>.

%% Helpers
ok_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"data">> := Data}}) ->
    maps:get(What2, maps:get(What1, Data)).

error_result(ErrorNumber, {{<<"200">>, <<"OK">>}, #{<<"errors">> := Errors}}) ->
    lists:nth(ErrorNumber, Errors).
