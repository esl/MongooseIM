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
    [admin_stats_test,
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

admin_stats_test(Config) ->
    GraphQlRequest = admin_get_stats(Config, #{}),
    Result = ok_result(<<"stats">>, <<"stats">>, GraphQlRequest),
    #{<<"uptimeSeconds">> := UptimeSeconds, <<"registeredUsers">> := RegisteredUsers,
      <<"onlineUsersNode">> := OnlineUsersNode, <<"onlineUsers">> := OnlineUsers,
      <<"incomingS2S">> := IncomingS2S, <<"outgoingS2S">> := OutgoingS2S} = Result,
    ?assertEqual(true, is_integer(UptimeSeconds)),
    ?assertEqual(0, RegisteredUsers),
    ?assertEqual(0, OnlineUsersNode),
    ?assertEqual(0, OnlineUsers),
    ?assertEqual(0, IncomingS2S),
    ?assertEqual(0, OutgoingS2S).

admin_stats_domain_test(Config) ->
    Vars = #{<<"domain">> => domain()},
    GraphQlRequest = admin_get_stats_domain(Config, Vars),
    Result = ok_result(<<"stats">>, <<"domainStats">>, GraphQlRequest),
    #{<<"registeredUsers">> := RegisteredUsers, <<"onlineUsers">> := OnlineUsers} = Result,
    ?assertEqual(0, RegisteredUsers),
    ?assertEqual(0, OnlineUsers).

% Helpers

admin_get_stats(Config, Vars) ->
    Query = <<"query Q1
                   {stats{stats{uptimeSeconds registeredUsers onlineUsersNode
                                onlineUsers incomingS2S outgoingS2S}}}">>,
    admin_send_mutation(Config, Vars, Query).

admin_get_stats_domain(Config, Vars) ->
    Query = <<"query Q1($domain: String!)
                   {stats{domainStats(domain: $domain){registeredUsers onlineUsers}}}">>,
    admin_send_mutation(Config, Vars, Query).

admin_send_mutation(Config, Vars, Query) ->
    Body = #{query => Query, operationName => <<"Q1">>, variables => Vars},
    execute_auth(Body, Config).

error_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"errors">> := [Data]}}) ->
    maps:get(What2, maps:get(What1, Data)).

ok_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"data">> := Data}}) ->
    maps:get(What2, maps:get(What1, Data)).
