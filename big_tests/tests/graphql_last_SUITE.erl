-module(graphql_last_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute_user/3, execute_auth/2, user_to_bin/1, user_to_jid/1,
                         get_ok_value/2, get_err_msg/1, get_err_code/1]).

-include_lib("eunit/include/eunit.hrl").

-define(assertErrMsg(Res, ContainsPart), assert_err_msg(ContainsPart, Res)).
-define(assertErrCode(Res, Code), assert_err_code(Code, Res)).

-define(NONEXISTENT_JID, <<"user@user.com">>).
-define(DEFAULT_DT, <<"2022-04-17T12:58:30.000000Z">>).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user_last},
     {group, admin_last},
     {group, admin_last_old_users}].

groups() ->
    [{user_last, [parallel], user_last_handler()},
     {admin_last, [parallel], admin_last_handler()},
     {admin_last_old_users, [], admin_old_users_handler()}].

user_last_handler() ->
    [user_set_last,
     user_get_last,
     user_get_other_user_last].

admin_last_handler() ->
    [admin_set_last,
     admin_try_set_nonexistent_user_last,
     admin_get_last,
     admin_get_nonexistent_user_last,
     admin_try_get_nonexistent_last,
     admin_count_active_users,
     admin_try_count_nonexistent_domain_active_users].

admin_old_users_handler() ->
    [admin_list_old_users_domain,
     admin_try_list_old_users_nonexistent_domain,
     admin_list_old_users_global,
     admin_remove_old_users_domain,
     admin_try_remove_old_users_nonexistent_domain,
     admin_remove_old_users_global,
     admin_user_without_last_info_is_old_user,
     admin_logged_user_is_not_old_user].

init_per_suite(Config) ->
    HostType = domain_helper:host_type(),
    SecHostType = domain_helper:secondary_host_type(),
    Config1 = escalus:init_per_suite(Config),
    Config2 = dynamic_modules:save_modules([HostType, SecHostType], Config1),
    Backend = mongoose_helper:get_backend_mnesia_rdbms_riak(HostType),
    SecBackend = mongoose_helper:get_backend_mnesia_rdbms_riak(SecHostType),
    dynamic_modules:ensure_modules(HostType, required_modules(Backend)),
    dynamic_modules:ensure_modules(SecHostType, required_modules(SecBackend)),
    escalus:init_per_suite(Config2).

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_last, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_last_old_users, Config) ->
    AuthMods = mongoose_helper:auth_modules(),
    case lists:member(ejabberd_auth_ldap, AuthMods) of
        true -> {skip, not_fully_supported_with_ldap};
        false -> graphql_helper:init_admin_handler(Config)
    end;
init_per_group(user_last, Config) ->
    [{schema_endpoint, user} | Config].

end_per_group(_, _Config) ->
    escalus_fresh:clean().

init_per_testcase(C, Config) when C =:= admin_remove_old_users_domain;
                                  C =:= admin_remove_old_users_global;
                                  C =:= admin_list_old_users_domain;
                                  C =:= admin_list_old_users_global;
                                  C =:= admin_user_without_last_info_is_old_user ->
    Config1 = escalus:create_users(Config, escalus:get_users([alice, bob, alice_bis])),
    escalus:init_per_testcase(C, Config1);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(C, Config) when C =:= admin_remove_old_users_domain;
                                 C =:= admin_remove_old_users_global;
                                 C =:= admin_list_old_users_domain;
                                 C =:= admin_list_old_users_global;
                                 C =:= admin_user_without_last_info_is_old_user ->
    escalus:delete_users(Config, escalus:get_users([alice, bob, alice_bis])),
    escalus:end_per_testcase(C, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

required_modules(riak) ->
    [{mod_last, #{backend => riak,
                  iqdisc => one_queue,
                  riak => #{bucket_type => <<"last">>}}}];
required_modules(Backend) ->
    [{mod_last, #{backend => Backend,
                  iqdisc => one_queue}}].

%% Admin test cases

admin_set_last(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_set_last/2).

admin_set_last(Config, Alice) ->
    Status = <<"First status">>,
    JID = escalus_utils:jid_to_lower(user_to_bin(Alice)),
    % With timestamp provided
    Res = execute_auth(admin_set_last_body(Alice, Status, ?DEFAULT_DT), Config),
    #{<<"user">> := JID, <<"status">> := Status, <<"timestamp">> := ?DEFAULT_DT} =
        get_ok_value(p(setLast), Res),
    % Without timestamp provided
    Status2 = <<"Second status">>,
    Res2 = execute_auth(admin_set_last_body(Alice, Status2, null), Config),
    #{<<"user">> := JID, <<"status">> := Status2, <<"timestamp">> := DateTime2} =
        get_ok_value(p(setLast), Res2),
    ?assert(os:system_time(second) - dt_to_unit(DateTime2, second) < 2).

admin_try_set_nonexistent_user_last(Config) ->
    Res = execute_auth(admin_set_last_body(?NONEXISTENT_JID, <<"status">>, null), Config),
    ?assertErrMsg(Res, <<"not exist">>),
    ?assertErrCode(Res, user_does_not_exist).

admin_get_last(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_get_last/2).

admin_get_last(Config, Alice) ->
    Status = <<"I love ducks">>,
    JID = escalus_utils:jid_to_lower(user_to_bin(Alice)),
    execute_auth(admin_set_last_body(Alice, Status, ?DEFAULT_DT), Config),
    Res = execute_auth(admin_get_last_body(Alice), Config),
    #{<<"user">> := JID, <<"status">> := Status, <<"timestamp">> := ?DEFAULT_DT} =
        get_ok_value(p(getLast), Res).

admin_get_nonexistent_user_last(Config) ->
    Res = execute_auth(admin_get_last_body(?NONEXISTENT_JID), Config),
    ?assertErrMsg(Res, <<"not exist">>),
    ?assertErrCode(Res, user_does_not_exist).

admin_try_get_nonexistent_last(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_try_get_nonexistent_last/2).

admin_try_get_nonexistent_last(Config, Alice) ->
    Res = execute_auth(admin_get_last_body(Alice), Config),
    ?assertErrMsg(Res, <<"not found">>),
    ?assertErrCode(Res, last_not_found).

admin_count_active_users(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_count_active_users/3).

admin_count_active_users(Config, Alice, Bob) ->
    Domain = domain_helper:domain(),
    set_last(Alice, now_dt_with_offset(5), Config),
    set_last(Bob, now_dt_with_offset(10), Config),
    Res = execute_auth(admin_count_active_users_body(Domain, null), Config),
    ?assertEqual(2, get_ok_value(p(countActiveUsers), Res)),
    Res2 = execute_auth(admin_count_active_users_body(Domain, now_dt_with_offset(30)), Config),
    ?assertEqual(0, get_ok_value(p(countActiveUsers), Res2)).

admin_try_count_nonexistent_domain_active_users(Config) ->
    Res = execute_auth(admin_count_active_users_body(<<"unknown-domain.com">>, null), Config),
    ?assertErrMsg(Res, <<"not found">>),
    ?assertErrCode(Res, domain_not_found).

%% Admin old users test cases

admin_remove_old_users_domain(Config) ->
    jids_with_config(Config, [alice, alice_bis, bob], fun admin_remove_old_users_domain/4).

admin_remove_old_users_domain(Config, Alice, AliceBis, Bob) ->
    Domain = domain_helper:domain(),
    ToRemoveDateTime = now_dt_with_offset(100),

    set_last(Bob, ToRemoveDateTime, Config),
    set_last(AliceBis, ToRemoveDateTime, Config),
    set_last(Alice, now_dt_with_offset(200), Config),

    Resp = execute_auth(admin_remove_old_users_body(Domain, now_dt_with_offset(150)), Config),
    [#{<<"jid">> := Bob, <<"timestamp">> := BobDateTimeRes}] = get_ok_value(p(removeOldUsers), Resp),
    ?assertEqual(dt_to_unit(ToRemoveDateTime, second), dt_to_unit(BobDateTimeRes, second)),
    ?assertMatch({user_does_not_exist, _}, check_account(Bob)),
    ?assertMatch({ok, _}, check_account(Alice)),
    ?assertMatch({ok, _}, check_account(AliceBis)).

admin_try_remove_old_users_nonexistent_domain(Config) ->
    Res = execute_auth(admin_remove_old_users_body(<<"nonexistent">>, now_dt_with_offset(0)), Config),
    ?assertErrMsg(Res, <<"not found">>),
    ?assertErrCode(Res, domain_not_found).

admin_remove_old_users_global(Config) ->
    jids_with_config(Config, [alice, alice_bis, bob], fun admin_remove_old_users_global/4).

admin_remove_old_users_global(Config, Alice, AliceBis, Bob) ->
    ToRemoveDateTime = now_dt_with_offset(100),
    ToRemoveTimestamp = dt_to_unit(ToRemoveDateTime, second),

    set_last(Bob, ToRemoveDateTime, Config),
    set_last(AliceBis, ToRemoveDateTime, Config),
    set_last(Alice, now_dt_with_offset(200), Config),

    Resp = execute_auth(admin_remove_old_users_body(null, now_dt_with_offset(150)), Config),
    [#{<<"jid">> := AliceBis, <<"timestamp">> := AliceBisDateTime},
     #{<<"jid">> := Bob, <<"timestamp">> := BobDateTime}] =
        lists:sort(get_ok_value(p(removeOldUsers), Resp)),
    ?assertEqual(ToRemoveTimestamp, dt_to_unit(BobDateTime, second)),
    ?assertEqual(ToRemoveTimestamp, dt_to_unit(AliceBisDateTime, second)),
    ?assertMatch({user_does_not_exist, _}, check_account(Bob)),
    ?assertMatch({user_does_not_exist, _}, check_account(AliceBis)),
    ?assertMatch({ok, _}, check_account(Alice)).

admin_list_old_users_domain(Config) ->
    jids_with_config(Config, [alice, bob], fun admin_list_old_users_domain/3).

admin_list_old_users_domain(Config, Alice, Bob) ->
    Domain = domain_helper:domain(),
    OldDateTime = now_dt_with_offset(100),

    set_last(Bob, OldDateTime, Config),
    set_last(Alice, now_dt_with_offset(200), Config),

    Res = execute_auth(admin_list_old_users_body(Domain, now_dt_with_offset(150)), Config),
    [#{<<"jid">> := Bob, <<"timestamp">> := BobDateTime}] = get_ok_value(p(listOldUsers), Res),
    ?assertEqual(dt_to_unit(OldDateTime, second), dt_to_unit(BobDateTime, second)).

admin_try_list_old_users_nonexistent_domain(Config) ->
    Res = execute_auth(admin_list_old_users_body(<<"nonexistent">>, now_dt_with_offset(0)), Config),
    ?assertErrMsg(Res, <<"not found">>),
    ?assertErrCode(Res, domain_not_found).

admin_list_old_users_global(Config) ->
    jids_with_config(Config, [alice, alice_bis, bob], fun admin_list_old_users_global/4).

admin_list_old_users_global(Config, Alice, AliceBis, Bob) ->
    OldDateTime = now_dt_with_offset(100),

    set_last(Bob, OldDateTime, Config),
    set_last(AliceBis, OldDateTime, Config),
    set_last(Alice, now_dt_with_offset(200), Config),

    Res = execute_auth(admin_list_old_users_body(null, now_dt_with_offset(150)), Config),
    [#{<<"jid">> := AliceBis, <<"timestamp">> := AliceBisDateTime},
     #{<<"jid">> := Bob, <<"timestamp">> := BobDateTime}] =
        lists:sort(get_ok_value(p(listOldUsers), Res)),
    ?assertEqual(dt_to_unit(OldDateTime, second), dt_to_unit(BobDateTime, second)),
    ?assertEqual(dt_to_unit(OldDateTime, second), dt_to_unit(AliceBisDateTime, second)).

admin_user_without_last_info_is_old_user(Config) ->
    Res = execute_auth(admin_list_old_users_body(null, now_dt_with_offset(150)), Config),
    OldUsers = get_ok_value(p(listOldUsers), Res),
    ?assertEqual(3, length(OldUsers)),
    [?assertEqual(null, TS) || #{<<"timestamp">> := TS} <- OldUsers].

admin_logged_user_is_not_old_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_logged_user_is_not_old_user/2).

admin_logged_user_is_not_old_user(Config, _Alice) ->
    Res = execute_auth(admin_list_old_users_body(null, now_dt_with_offset(100)), Config),
    ?assertEqual([], get_ok_value(p(listOldUsers), Res)).

%% User test cases

user_set_last(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_set_last/2).

user_set_last(Config, Alice) ->
    Status = <<"My first status">>,
    JID = escalus_utils:jid_to_lower(user_to_bin(Alice)),
    Res = execute_user(user_set_last_body(Status, ?DEFAULT_DT), Alice, Config),
    #{<<"user">> := JID, <<"status">> := Status, <<"timestamp">> := ?DEFAULT_DT} =
        get_ok_value(p(setLast), Res),
    Status2 = <<"Quack Quack">>,
    Res2 = execute_user(user_set_last_body(Status2, null), Alice, Config),
    #{<<"user">> := JID, <<"status">> := Status2, <<"timestamp">> := DateTime2} =
        get_ok_value(p(setLast), Res2),
    ?assert(os:system_time(second) - dt_to_unit(DateTime2, second) < 2).

user_get_last(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_get_last/2).
user_get_last(Config, Alice) ->
    Status = <<"I love ducks">>,
    JID = escalus_utils:jid_to_lower(user_to_bin(Alice)),
    execute_user(user_set_last_body(Status, ?DEFAULT_DT), Alice, Config),
    Res = execute_user(user_get_last_body(null), Alice, Config),
    #{<<"user">> := JID, <<"status">> := Status, <<"timestamp">> := ?DEFAULT_DT} =
        get_ok_value(p(getLast), Res).

user_get_other_user_last(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_get_other_user_last/3).

user_get_other_user_last(Config, Alice, Bob) ->
    Status = <<"In good mood">>,
    JID = escalus_utils:jid_to_lower(user_to_bin(Bob)),
    execute_user(user_set_last_body(Status, ?DEFAULT_DT), Bob, Config),
    Res = execute_user(admin_get_last_body(Bob), Alice, Config),
    #{<<"user">> := JID, <<"status">> := Status, <<"timestamp">> := ?DEFAULT_DT} =
        get_ok_value(p(getLast), Res).

%% Helpers

jids_with_config(Config, Users, Fun) ->
    Args = [escalus_utils:jid_to_lower(escalus_users:get_jid(Config, User)) || User <- Users],
    apply(Fun, [Config | Args]).

set_last(UserJID, DateTime, Config) ->
    execute_auth(admin_set_last_body(UserJID, <<>>, DateTime), Config).

check_account(User) ->
    {Username, LServer} = jid:to_lus(user_to_jid(User)),
    rpc(mim(), mongoose_account_api, check_account, [Username, LServer]).

assert_err_msg(Contains, Res) ->
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), Contains)).

assert_err_code(Code, Res) ->
    ?assertEqual(atom_to_binary(Code), get_err_code(Res)).

p(Cmd) when is_atom(Cmd) ->
    [data, last, Cmd];
p(Path) when is_list(Path) ->
    [data, last] ++ Path.

now_dt_with_offset(SecondsOffset) ->
    Seconds = erlang:system_time(second) + SecondsOffset,
    list_to_binary(calendar:system_time_to_rfc3339(Seconds, [{unit, second}, {offset, "Z"}])).

dt_to_unit(ISODateTime, Unit) ->
    calendar:rfc3339_to_system_time(binary_to_list(ISODateTime), [{unit, Unit}]).

%% Request bodies

admin_set_last_body(User, Status, DateTime) ->
    Query = <<"mutation M1($user: JID!, $timestamp: DateTime, $status: String!)
              { last { setLast (user: $user, timestamp: $timestamp, status: $status)
              { user timestamp status } } }">>,
    OpName = <<"M1">>,
    Vars = #{user => user_to_bin(User), timestamp => DateTime, status => Status},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_get_last_body(User) ->
    Query = <<"query Q1($user: JID!)
              { last { getLast(user: $user)
              { user timestamp status } } }">>,
    OpName = <<"Q1">>,
    Vars = #{user => user_to_bin(User)},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_count_active_users_body(Domain, Timestamp) ->
    Query = <<"query Q1($domain: String!, $timestamp: DateTime)
              { last { countActiveUsers(domain: $domain, timestamp: $timestamp) } }">>,
    OpName = <<"Q1">>,
    Vars = #{domain => Domain, timestamp => Timestamp},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_remove_old_users_body(Domain, Timestamp) ->
    Query = <<"mutation M1($domain: String, $timestamp: DateTime!)
              { last { removeOldUsers(domain: $domain, timestamp: $timestamp) { jid timestamp } } }">>,
    OpName = <<"M1">>,
    Vars = #{domain => Domain, timestamp => Timestamp},
    #{query => Query, operationName => OpName, variables => Vars}.

admin_list_old_users_body(Domain, Timestamp) ->
    Query = <<"query Q1($domain: String, $timestamp: DateTime!)
              { last { listOldUsers(domain: $domain, timestamp: $timestamp) { jid timestamp } } }">>,
    OpName = <<"Q1">>,
    Vars = #{domain => Domain, timestamp => Timestamp},
    #{query => Query, operationName => OpName, variables => Vars}.

user_set_last_body(Status, DateTime) ->
    Query = <<"mutation M1($timestamp: DateTime, $status: String!)
              { last { setLast (timestamp: $timestamp, status: $status)
              { user timestamp status } } }">>,
    OpName = <<"M1">>,
    Vars = #{timestamp => DateTime, status => Status},
    #{query => Query, operationName => OpName, variables => Vars}.

user_get_last_body(User) ->
    Query = <<"query Q1($user: JID)
              { last { getLast(user: $user)
              { user timestamp status } } }">>,
    OpName = <<"Q1">>,
    Vars = #{user => user_to_bin(User)},
    #{query => Query, operationName => OpName, variables => Vars}.
