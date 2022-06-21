-module(graphql_offline_SUITE).

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

-record(offline_msg, {us, timestamp, expire, from, to, packet, permanent_fields = []}).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, admin_offline},
      {group, admin_offline_not_configured}].

groups() ->
    [{admin_offline, [], admin_offline_handler()},
     {admin_offline_not_configured, [], admin_offline_not_configured_handler()}].

admin_offline_handler() ->
    [admin_delete_expired_messages_test,
     admin_delete_old_messages_test,
     admin_delete_expired_messages2_test,
     admin_delete_old_messages2_test,
     admin_delete_expired_messages_no_domain_test,
     admin_delete_old_messages_no_domain_test].

admin_offline_not_configured_handler() ->
    [admin_delete_expired_messages_offline_not_configured_test,
     admin_delete_old_messages_offline_not_configured_test].

init_per_suite(Config) ->
    Config1 = dynamic_modules:save_modules(host_type(), Config),
    escalus:init_per_suite(Config1).

-spec create_config(atom()) -> [{mod_offline, gen_mod:module_opts()}].
create_config(riak) ->
    [{mod_offline, mod_config(mod_offline, #{backend => riak,
        riak => #{bucket_type => <<"offline">>}})}];
create_config(Backend) ->
    [{mod_offline, mod_config(mod_offline, #{backend => Backend})}].


end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_offline, Config) ->
    HostType = host_type(),
    Backend = mongoose_helper:get_backend_mnesia_rdbms_riak(HostType),
    ModConfig = create_config(Backend),
    dynamic_modules:ensure_modules(HostType, ModConfig),
    Config1 = [{backend, Backend} | escalus:init_per_suite(Config)],
    graphql_helper:init_admin_handler(Config1);
init_per_group(admin_offline_not_configured, Config) ->
    dynamic_modules:ensure_modules(host_type(), [{mod_offline, stopped}]),
    graphql_helper:init_admin_handler(Config).

end_per_group(_, _Config) ->
    escalus_fresh:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

% Admin test cases

admin_delete_expired_messages_test(Config) ->
    Vars = #{<<"domain">> => domain()},
    GraphQlRequest = admin_send_request(Config, Vars, fun admin_delete_expired_messages/0),
    Message = ok_result(<<"offline">>, <<"deleteExpiredMessages">>, GraphQlRequest),
    ?assertEqual(<<"Removed 0 messages">>, Message).

admin_delete_old_messages_test(Config) ->
    Vars = #{<<"domain">> => domain(), <<"days">> => 2},
    GraphQlRequest = admin_send_request(Config, Vars, fun admin_delete_old_messages/0),
    Message = ok_result(<<"offline">>, <<"deleteOldMessages">>, GraphQlRequest),
    ?assertEqual(<<"Removed 0 messages">>, Message).

admin_delete_expired_messages2_test(Config) ->
    escalus:fresh_story_with_config(Config, [{mike, 1}, {kate, 1}], fun admin_delete_expired_messages2_test/3).

admin_delete_expired_messages2_test(Config, JidMike, JidKate) ->
    generate_message(JidMike, JidKate, 10, 2),
    generate_message(JidMike, JidKate, 10, 2),
    Vars = #{<<"domain">> => domain()},
    GraphQlRequest = admin_send_request(Config, Vars, fun admin_delete_expired_messages/0),
    Message = ok_result(<<"offline">>, <<"deleteExpiredMessages">>, GraphQlRequest),
    ?assertEqual(<<"Removed 2 messages">>, Message).

admin_delete_old_messages2_test(Config) ->
    escalus:fresh_story_with_config(Config, [{mike, 1}, {kate, 1}], fun admin_delete_old_messages2_test/3).

admin_delete_old_messages2_test(Config, JidMike, JidKate) ->
    generate_message(JidMike, JidKate, 2, 10),
    generate_message(JidMike, JidKate, 2, 10),
    Vars = #{<<"domain">> => domain(), <<"days">> => 2},
    GraphQlRequest = admin_send_request(Config, Vars, fun admin_delete_old_messages/0),
    Message = ok_result(<<"offline">>, <<"deleteOldMessages">>, GraphQlRequest),
    ?assertEqual(<<"Removed 2 messages">>, Message).

admin_delete_expired_messages_no_domain_test(Config) ->
    Vars = #{<<"domain">> => <<"AAAA">>},
    GraphQlRequest = admin_send_request(Config, Vars, fun admin_delete_expired_messages/0),
    ParsedResult = error_result(<<"extensions">>, <<"code">>, GraphQlRequest),
    ?assertEqual(<<"domain_not_found">>, ParsedResult).

admin_delete_old_messages_no_domain_test(Config) ->
    Vars = #{<<"domain">> => <<"AAAA">>, <<"days">> => 2},
    GraphQlRequest = admin_send_request(Config, Vars, fun admin_delete_old_messages/0),
    ParsedResult = error_result(<<"extensions">>, <<"code">>, GraphQlRequest),
    ?assertEqual(<<"domain_not_found">>, ParsedResult).

admin_delete_expired_messages_offline_not_configured_test(Config) ->
    Vars = #{<<"domain">> => domain()},
    GraphQlRequest = admin_send_request(Config, Vars, fun admin_delete_expired_messages/0),
    ParsedResult = error_result(<<"extensions">>, <<"code">>, GraphQlRequest),
    ?assertEqual(<<"module_not_loaded_error">>, ParsedResult).

admin_delete_old_messages_offline_not_configured_test(Config) ->
    Vars = #{<<"domain">> => domain(), <<"days">> => 2},
    GraphQlRequest = admin_send_request(Config, Vars, fun admin_delete_old_messages/0),
    ParsedResult = error_result(<<"extensions">>, <<"code">>, GraphQlRequest),
    ?assertEqual(<<"module_not_loaded_error">>, ParsedResult).

% Helpers

admin_delete_expired_messages() ->
    <<"mutation M1($domain: String!)
           {offline{deleteExpiredMessages(domain: $domain)}}">>.

admin_delete_old_messages() ->
    <<"mutation M1($domain: String!, $days: Int!)
           {offline{deleteOldMessages(domain: $domain, days: $days)}}">>.

admin_send_request(Config, Vars, Function) ->
    Body = #{query => Function(), operationName => <<"M1">>, variables => Vars},
    execute_auth(Body, Config).

error_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"errors">> := [Data]}}) ->
    maps:get(What2, maps:get(What1, Data)).

ok_result(What1, What2, {{<<"200">>, <<"OK">>}, #{<<"data">> := Data}}) ->
    maps:get(What2, maps:get(What1, Data)).

generate_message(JidMike, JidKate, TimestampDaysAgo, TimestampExpiringDaysAgo) ->
    JidRecordMike = jid:from_binary(user_to_bin(JidMike)),
    JidRecordKate = jid:from_binary(user_to_bin(JidKate)),
    Domain = domain(),
    Msg1 = escalus_stanza:chat_to(<<"kate@", Domain/binary>>, "Rolling stones"),
    OldTimestamp = fallback_timestamp(TimestampDaysAgo, os:system_time(microsecond)),
    ExpirationTime = fallback_timestamp(TimestampExpiringDaysAgo, os:system_time(microsecond)),
    OfflineOld = generate_offline_expired_message(JidRecordMike,
                                                  JidRecordKate, Msg1,
                                                  OldTimestamp,
                                                  ExpirationTime),
    {LUser, LServer} = jid:to_lus(JidRecordKate),
    rpc_call(mod_offline_backend, write_messages, [host_type(), LUser, LServer, [OfflineOld]]).

generate_offline_expired_message(From, To, Msg, TimeStamp, ExpirationTime) ->
    {LUser, LServer} = jid:to_lus(To),
    #offline_msg{us = {LUser, LServer}, timestamp = TimeStamp,
                 expire = ExpirationTime, from = From, to = To, packet = Msg}.

fallback_timestamp(HowManyDays, TS_MicroSeconds) ->
    HowManySeconds = HowManyDays * 86400,
    HowManyMicroSeconds = erlang:convert_time_unit(HowManySeconds, second, microsecond),
    TS_MicroSeconds - HowManyMicroSeconds.
