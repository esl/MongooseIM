-module(graphql_stats_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [require_rpc_nodes/1]).
-import(domain_helper, [host_type/0, domain/0]).
-import(graphql_helper, [execute_user/3, execute_auth/2, user_to_bin/1]).
-import(config_parser_helper, [mod_config/2]).
-import(mongooseimctl_helper, [mongooseimctl/3, rpc_call/3]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include("../../include/mod_roster.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, admin_stats}].

groups() ->
    [{admin_stats, [], admin_stats_handler()}].

admin_stats_handler() ->
    [admin_get_incoming_s2s_number_test,
     admin_get_outgoing_s2s_number_test,
     admin_stats_no_stat_name_test,
     admin_stats_domain_no_stat_name_test,
     admin_stats_test,
     admin_stats_domain_test].

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    graphql_helper:init_admin_handler(Config).

end_per_group(_, _Config) ->
    escalus_fresh:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

% Admin test cases

admin_get_incoming_s2s_number_test(Config) ->
    GraphQlRequest = admin_get_incoming_s2s(Config, #{}),
    Number = ok_result(<<"stats">>, <<"getIncomingS2SNumber">>, GraphQlRequest),
    ?assertEqual(0, Number).

admin_get_outgoing_s2s_number_test(Config) ->
    GraphQlRequest = admin_get_outgoing_s2s(Config, #{}),
    Number = ok_result(<<"stats">>, <<"getOutgoingS2SNumber">>, GraphQlRequest),
    ?assertEqual(0, Number).

admin_stats_no_stat_name_test(Config) ->
    GraphQlRequest = admin_get_stats(Config, #{<<"statName">> => <<"AAA">>}),
    ParsedResult = error_result(<<"extensions">>, <<"code">>, GraphQlRequest),
    ?assertEqual(<<"no_command_error">>, ParsedResult).

admin_stats_domain_no_stat_name_test(Config) ->
    Vars = #{<<"statName">> => <<"AAA">>, <<"domain">> => domain()},
    GraphQlRequest = admin_get_stats_domain(Config, Vars),
    ParsedResult = error_result(<<"extensions">>, <<"code">>, GraphQlRequest),
    ?assertEqual(<<"no_command_error">>, ParsedResult).

admin_stats_test(Config) ->
    GraphQlRequest = admin_get_stats(Config, #{<<"statName">> => <<"onlineusers">>}),
    Number = ok_result(<<"stats">>, <<"stats">>, GraphQlRequest),
    ?assertEqual(0, Number).

admin_stats_domain_test(Config) ->
    Vars = #{<<"statName">> => <<"onlineusers">>, <<"domain">> => domain()},
    GraphQlRequest = admin_get_stats_domain(Config, Vars),
    Number = ok_result(<<"stats">>, <<"stats">>, GraphQlRequest),
    ?assertEqual(0, Number).

% Helpers

admin_get_incoming_s2s(Config, Vars) ->
    Query = <<"query Q1
                   {stats{getIncomingS2SNumber}}">>,
    admin_send_mutation(Config, Vars, Query).

admin_get_outgoing_s2s(Config, Vars) ->
    Query = <<"query Q1
                   {stats{getOutgoingS2SNumber}}">>,
    admin_send_mutation(Config, Vars, Query).

admin_get_stats(Config, Vars) ->
    Query = <<"query Q1($statName: String!)
                   {stats{stats(statName: $statName)}}">>,
    admin_send_mutation(Config, Vars, Query).

admin_get_stats_domain(Config, Vars) ->
    Query = <<"query Q1($statName: String!, $domain: String)
                   {stats{stats(statName: $statName, domain: $domain)}}">>,
    admin_send_mutation(Config, Vars, Query).

admin_send_mutation(Config, Vars, Query) ->
    Body = #{query => Query, operationName => <<"Q1">>, variables => Vars},
    execute_auth(Body, Config).

error_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"errors">> := [Data]}}) ->
    maps:get(What2, maps:get(What1, Data)).

ok_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"data">> := Data}}) ->
    maps:get(What2, maps:get(What1, Data)).
