-module(graphql_offline_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1]).
-import(domain_helper, [host_type/0, domain/0]).
-import(graphql_helper, [execute_command/4, get_ok_value/2, get_err_code/1, user_to_bin/1,
                         get_unauthorized/1]).
-import(config_parser_helper, [mod_config/2]).
-import(mongooseimctl_helper, [mongooseimctl/3, rpc_call/3]).

-include_lib("eunit/include/eunit.hrl").

-record(offline_msg, {us, timestamp, expire, from, to, packet, permanent_fields = []}).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, admin_http},
     {group, admin_cli},
     {group, domain_admin}].

groups() ->
    [{admin_http, [], admin_groups()},
     {admin_cli, [], admin_groups()},
     {domain_admin, [], domain_admin_groups()},
     {admin_offline, [], admin_offline_tests()},
     {admin_offline_not_configured, [], admin_offline_not_configured_tests()},
     {domain_admin_offline, [], domain_admin_offline_tests()},
     {domain_admin_offline_not_configured, [], domain_admin_offline_not_configured_tests()}].

admin_groups() ->
    [{group, admin_offline},
     {group, admin_offline_not_configured}].

domain_admin_groups() ->
    [{group, domain_admin_offline},
     {group, domain_admin_offline_not_configured}].

admin_offline_tests() ->
    [admin_delete_expired_messages_test,
     admin_delete_old_messages_test,
     admin_delete_expired_messages2_test,
     admin_delete_old_messages2_test,
     admin_delete_expired_messages_no_domain_test,
     admin_delete_old_messages_no_domain_test].

admin_offline_not_configured_tests() ->
    [admin_delete_expired_messages_offline_not_configured_test,
     admin_delete_old_messages_offline_not_configured_test].

domain_admin_offline_tests() ->
    [admin_delete_expired_messages_test,
     admin_delete_old_messages_test,
     admin_delete_expired_messages2_test,
     admin_delete_old_messages2_test,
     domain_admin_delete_expired_messages_no_permission_test,
     domain_admin_delete_old_messages_no_permission_test].

domain_admin_offline_not_configured_tests() ->
    [admin_delete_expired_messages_offline_not_configured_test,
     admin_delete_old_messages_offline_not_configured_test].

init_per_suite(Config) ->
    Config1 = dynamic_modules:save_modules(host_type(), Config),
    Config2 = ejabberd_node_utils:init(mim(), Config1),
    escalus:init_per_suite(Config2).

-spec create_config(atom()) -> [{mod_offline, gen_mod:module_opts()}].
create_config(riak) ->
    [{mod_offline, mod_config(mod_offline, #{backend => riak,
        riak => #{bucket_type => <<"offline">>}})}];
create_config(Backend) ->
    [{mod_offline, mod_config(mod_offline, #{backend => Backend})}].

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin, Config) ->
    graphql_helper:init_domain_admin_handler(Config);
init_per_group(GroupName, Config) when GroupName =:= admin_offline;
                                       GroupName =:= domain_admin_offline ->
    HostType = host_type(),
    Backend = mongoose_helper:get_backend_mnesia_rdbms_riak(HostType),
    ModConfig = create_config(Backend),
    dynamic_modules:ensure_modules(HostType, ModConfig),
    [{backend, Backend} | escalus:init_per_suite(Config)];
init_per_group(admin_offline_not_configured, Config) ->
    dynamic_modules:ensure_modules(host_type(), [{mod_offline, stopped}]),
    Config;
init_per_group(domain_admin_offline_not_configured, Config) ->
    dynamic_modules:ensure_modules(host_type(), [{mod_offline, stopped}]),
    Config.

end_per_group(GroupName, _Config) when GroupName =:= admin_http;
                                       GroupName =:= admin_cli;
                                       GroupName =:= domain_admin ->
    graphql_helper:clean();
end_per_group(_, _Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config),
    escalus_fresh:clean().

% Admin test cases

admin_delete_expired_messages_test(Config) ->
    Result = delete_expired_messages(domain(), Config),
    ParsedResult = get_ok_value([data, offline, deleteExpiredMessages], Result),
    ?assertEqual(<<"Removed 0 messages">>, ParsedResult).

admin_delete_old_messages_test(Config) ->
    Result = delete_old_messages(domain(), 2, Config),
    ParsedResult = get_ok_value([data, offline, deleteOldMessages], Result),
    ?assertEqual(<<"Removed 0 messages">>, ParsedResult).

admin_delete_expired_messages2_test(Config) ->
    escalus:fresh_story_with_config(Config, [{mike, 1}, {kate, 1}],
                                    fun admin_delete_expired_messages2_test/3).

admin_delete_expired_messages2_test(Config, JidMike, JidKate) ->
    generate_message(JidMike, JidKate, 2, 1),
    generate_message(JidMike, JidKate, 5, -1), % not expired yet
    Result = delete_expired_messages(domain(), Config),
    ParsedResult = get_ok_value([data, offline, deleteExpiredMessages], Result),
    ?assertEqual(<<"Removed 1 messages">>, ParsedResult).

admin_delete_old_messages2_test(Config) ->
    escalus:fresh_story_with_config(Config, [{mike, 1}, {kate, 1}],
                                    fun admin_delete_old_messages2_test/3).

admin_delete_old_messages2_test(Config, JidMike, JidKate) ->
    generate_message(JidMike, JidKate, 2, 1), % not old enough
    generate_message(JidMike, JidKate, 5, -1),
    generate_message(JidMike, JidKate, 7, 5),
    Result = delete_old_messages(domain(), 3, Config),
    ParsedResult = get_ok_value([data, offline, deleteOldMessages], Result),
    ?assertEqual(<<"Removed 2 messages">>, ParsedResult).

admin_delete_expired_messages_no_domain_test(Config) ->
    Result = delete_expired_messages(<<"AAAA">>, Config),
    ?assertEqual(<<"domain_not_found">>, get_err_code(Result)).

admin_delete_old_messages_no_domain_test(Config) ->
    Result = delete_old_messages(<<"AAAA">>, 2, Config),
    ?assertEqual(<<"domain_not_found">>, get_err_code(Result)).

admin_delete_expired_messages_offline_not_configured_test(Config) ->
    Result = delete_expired_messages(domain(), Config),
    ?assertEqual(<<"module_not_loaded_error">>, get_err_code(Result)).

admin_delete_old_messages_offline_not_configured_test(Config) ->
    Result = delete_old_messages(domain(), 2, Config),
    ?assertEqual(<<"module_not_loaded_error">>, get_err_code(Result)).

%% Domain admin test cases

domain_admin_delete_expired_messages_no_permission_test(Config) ->
    get_unauthorized(delete_expired_messages(<<"AAAA">>, Config)),
    get_unauthorized(delete_expired_messages(domain_helper:secondary_domain(), Config)).

domain_admin_delete_old_messages_no_permission_test(Config) ->
    get_unauthorized(delete_old_messages(<<"AAAA">>, 2, Config)),
    get_unauthorized(delete_old_messages(domain_helper:secondary_domain(), 2, Config)).

%% Commands

delete_expired_messages(Domain, Config) ->
    Vars = #{<<"domain">> => Domain},
    execute_command(<<"offline">>, <<"deleteExpiredMessages">>, Vars, Config).

delete_old_messages(Domain, Days, Config) ->
    Vars = #{<<"domain">> => Domain, <<"days">> => Days},
    execute_command(<<"offline">>, <<"deleteOldMessages">>, Vars, Config).

%% Helpers

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
