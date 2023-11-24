-module(graphql_last_SUITE).

-compile([export_all, nowarn_export_all]).

-import(common_helper, [unprep/1]).
-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute_command/4, execute_user_command/5, user_to_bin/1, user_to_jid/1,
                         get_ok_value/2, get_err_msg/1, get_err_code/1, get_unauthorized/1,
                         get_not_loaded/1, get_coercion_err_msg/1]).

-include_lib("eunit/include/eunit.hrl").

-define(assertErrMsg(Res, ContainsPart), assert_err_msg(ContainsPart, Res)).
-define(assertErrCode(Res, Code), assert_err_code(Code, Res)).

-define(NONEXISTENT_JID, <<"user@user.com">>).
-define(NONEXISTENT_NAME, <<"user@", (domain_helper:domain())/binary>>).
-define(EMPTY_NAME_JID, <<"@", (domain_helper:domain())/binary>>).
-define(DEFAULT_DT, <<"2022-04-17T12:58:30.000000Z">>).
-define(INVALID_TIMESTAMP, <<"20222-04-17T12:58:30.000000Z">>).
-define(NONEXISTENT_DOMAIN, <<"nonexistent">>).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user},
     {group, admin_http},
     {group, admin_cli},
     {group, domain_admin}].

groups() ->
    [{user, [], user_groups()},
     {user_last, [parallel], user_tests()},
     {admin_http, [], admin_groups()},
     {admin_cli, [], admin_groups()},
     {domain_admin, [], domain_admin_groups()},
     {admin_last, [], admin_last_tests()},
     {domain_admin_last, [], domain_admin_last_tests()},
     {admin_old_users, [], admin_old_users_tests()},
     {domain_admin_old_users, [], domain_admin_old_users_tests()},
     {admin_last_not_configured, [], admin_last_not_configured()},
     {admin_last_not_configured_old_users, [], admin_last_not_configured_old_users()},
     {admin_last_not_configured_group, [], admin_last_not_configured_groups()},
     {admin_last_configured, [], admin_last_configured()},
     {user_last_not_configured, [], user_last_not_configured()}].

user_groups() ->
    [{group, user_last},
     {group, user_last_not_configured}].

admin_groups() ->
    [{group, admin_last_configured},
     {group, admin_last_not_configured_group}].

admin_last_configured() ->
    [{group, admin_last},
     {group, admin_old_users}].

domain_admin_groups() ->
    [{group, domain_admin_last},
     {group, domain_admin_old_users}].

admin_last_not_configured_groups() ->
    [{group, admin_last_not_configured},
     {group, admin_last_not_configured_old_users}].

user_tests() ->
    [user_set_last,
     user_get_last,
     user_get_other_user_last].

user_last_not_configured() ->
    [user_set_last_not_configured,
     user_get_last_not_configured].

admin_last_tests() ->
    [admin_set_last,
     admin_try_set_nonexistent_user_last,
     admin_try_set_last_invalid_timestamp,
     admin_get_last,
     admin_get_nonexistent_user_last,
     admin_try_get_nonexistent_last,
     admin_count_active_users,
     admin_try_count_nonexistent_domain_active_users,
     admin_try_count_active_users_invalid_timestamp].

admin_old_users_tests() ->
    [admin_list_old_users_domain,
     admin_try_list_old_users_nonexistent_domain,
     admin_try_list_old_users_invalid_timestamp,
     admin_list_old_users_global,
     admin_remove_old_users_domain,
     admin_try_remove_old_users_nonexistent_domain,
     admin_try_remove_old_users_invalid_timestamp,
     admin_remove_old_users_global,
     admin_user_without_last_info_is_old_user,
     admin_logged_user_is_not_old_user].

domain_admin_last_tests() ->
    [admin_set_last,
     domain_admin_set_user_last_no_permission,
     admin_try_set_last_invalid_timestamp,
     admin_get_last,
     domain_admin_get_user_last_no_permission,
     admin_try_get_nonexistent_last,
     admin_count_active_users,
     domain_admin_try_count_external_domain_active_users,
     admin_try_count_active_users_invalid_timestamp].

domain_admin_old_users_tests() ->
    [admin_list_old_users_domain,
     admin_try_list_old_users_invalid_timestamp,
     admin_try_remove_old_users_invalid_timestamp,
     domain_admin_try_list_old_users_external_domain,
     domain_admin_list_old_users_global,
     domain_admin_remove_old_users_global,
     domain_admin_try_remove_old_users_external_domain,
     domain_admin_remove_old_users_global,
     domain_admin_user_without_last_info_is_old_user,
     domain_admin_logged_user_is_not_old_user].

admin_last_not_configured() ->
    [admin_set_last_not_configured,
     admin_get_last_not_configured,
     admin_count_active_users_last_not_configured].

admin_last_not_configured_old_users() ->
     [admin_remove_old_users_domain_last_not_configured,
      admin_remove_old_users_global_last_not_configured,
      admin_list_old_users_domain_last_not_configured,
      admin_list_old_users_global_last_not_configured].

init_per_suite(Config) ->
    HostType = domain_helper:host_type(),
    SecHostType = domain_helper:secondary_host_type(),
    Config1 = dynamic_modules:save_modules([HostType, SecHostType], Config),
    Config2 = ejabberd_node_utils:init(mim(), Config1),
    escalus:init_per_suite(Config2).

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(user, Config) ->
    graphql_helper:init_user(Config);
init_per_group(admin_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin, Config) ->
    configure_last(Config),
    graphql_helper:init_domain_admin_handler(Config);
init_per_group(domain_admin_last, Config) ->
    Config;
init_per_group(user_last, Config) ->
    configure_last(Config);
init_per_group(user_last_not_configured, Config) ->
    stop_last(Config);
init_per_group(admin_last, Config) ->
    Config;
init_per_group(admin_last_configured, Config) ->
    configure_last(Config);
init_per_group(admin_last_not_configured_group, Config) ->
    stop_last(Config);
init_per_group(Group, Config) when Group =:= admin_old_users;
                                   Group =:= domain_admin_old_users;
                                   Group =:= admin_last_not_configured_old_users ->
    AuthMods = mongoose_helper:auth_modules(),
    case lists:member(ejabberd_auth_ldap, AuthMods) of
        true -> {skip, not_fully_supported_with_ldap};
        false -> Config
    end;
init_per_group(admin_last_not_configured, Config) ->
    Config.

configure_last(Config) ->
    HostType = domain_helper:host_type(),
    SecHostType = domain_helper:secondary_host_type(),
    Backend = mongoose_helper:get_backend_mnesia_rdbms(HostType),
    SecBackend = mongoose_helper:get_backend_mnesia_rdbms(SecHostType),
    dynamic_modules:ensure_modules(HostType, required_modules(Backend)),
    dynamic_modules:ensure_modules(SecHostType, required_modules(SecBackend)),
    Config.

stop_last(Config) ->
    HostType = domain_helper:host_type(),
    SecHostType = domain_helper:secondary_host_type(),
    dynamic_modules:ensure_modules(HostType, [{mod_last, stopped}]),
    dynamic_modules:ensure_modules(SecHostType, [{mod_last, stopped}]),
    Config.

end_per_group(GroupName, _Config) when GroupName =:= admin_http;
                                       GroupName =:= admin_cli ->
    graphql_helper:clean();
end_per_group(user, _Config) ->
    graphql_helper:clean(),
    escalus_fresh:clean();
end_per_group(_GroupName, _Config) ->
    escalus_fresh:clean().

init_per_testcase(C, Config) when C =:= admin_remove_old_users_domain;
                                  C =:= admin_remove_old_users_global;
                                  C =:= admin_list_old_users_domain;
                                  C =:= admin_list_old_users_global;
                                  C =:= admin_user_without_last_info_is_old_user;
                                  C =:= domain_admin_list_old_users_global;
                                  C =:= domain_admin_remove_old_users_global;
                                  C =:= domain_admin_user_without_last_info_is_old_user ->
    Config1 = escalus:create_users(Config, escalus:get_users([alice, bob, alice_bis])),
    escalus:init_per_testcase(C, Config1);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(C, Config) when C =:= admin_remove_old_users_domain;
                                 C =:= admin_remove_old_users_global;
                                 C =:= admin_list_old_users_domain;
                                 C =:= admin_list_old_users_global;
                                 C =:= admin_user_without_last_info_is_old_user;
                                 C =:= domain_admin_list_old_users_global;
                                 C =:= domain_admin_remove_old_users_global;
                                 C =:= domain_admin_user_without_last_info_is_old_user->
    escalus:delete_users(Config, escalus:get_users([alice, bob, alice_bis])),
    escalus:end_per_testcase(C, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

required_modules(Backend) ->
    [{mod_last, #{backend => Backend,
                  iqdisc => one_queue}}].

%% Admin test cases

admin_set_last(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_set_last_story/2).

admin_set_last_story(Config, Alice) ->
    Status = <<"First status">>,
    JID = escalus_utils:jid_to_lower(user_to_bin(Alice)),
    % With timestamp provided
    Res = admin_set_last(Alice, Status, ?DEFAULT_DT, Config),
    #{<<"user">> := JID, <<"status">> := Status, <<"timestamp">> := ?DEFAULT_DT} =
        get_ok_value(p(setLast), Res),
    % Without timestamp provided
    Status2 = <<"Second status">>,
    Res2 = admin_set_last(Alice, Status2, null, Config),
    #{<<"user">> := JID, <<"status">> := Status2, <<"timestamp">> := DateTime2} =
        get_ok_value(p(setLast), Res2),
    ?assert(os:system_time(second) - dt_to_unit(DateTime2, second) < 2).

admin_try_set_nonexistent_user_last(Config) ->
    Res = admin_set_last(?NONEXISTENT_JID, <<"status">>, null, Config),
    ?assertErrMsg(Res, <<"not exist">>),
    ?assertErrCode(Res, user_does_not_exist),
    Res2 = admin_set_last(?NONEXISTENT_NAME, <<"status">>, null, Config),
    ?assertErrMsg(Res2, <<"not exist">>),
    ?assertErrCode(Res2, user_does_not_exist),
    Res3 = admin_set_last(?EMPTY_NAME_JID, <<"status">>, null, Config),
    get_coercion_err_msg(Res3).

admin_try_set_last_invalid_timestamp(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_try_set_last_invalid_timestamp_story/2).

admin_try_set_last_invalid_timestamp_story(Config, Alice) ->
    Res = admin_set_last(Alice, <<"status">>, ?INVALID_TIMESTAMP, Config),
    get_coercion_err_msg(Res).

admin_get_last(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_get_last_story/2).

admin_get_last_story(Config, Alice) ->
    Status = <<"I love ducks">>,
    JID = escalus_utils:jid_to_lower(user_to_bin(Alice)),
    admin_set_last(Alice, Status, ?DEFAULT_DT, Config),
    Res = admin_get_last(Alice, Config),
    #{<<"user">> := JID, <<"status">> := Status, <<"timestamp">> := ?DEFAULT_DT} =
        get_ok_value(p(getLast), Res).

admin_get_nonexistent_user_last(Config) ->
    Res = admin_get_last(?NONEXISTENT_JID, Config),
    ?assertErrMsg(Res, <<"not exist">>),
    ?assertErrCode(Res, user_does_not_exist),
    Res2 = admin_get_last(?NONEXISTENT_NAME, Config),
    ?assertErrMsg(Res2, <<"not exist">>),
    ?assertErrCode(Res2, user_does_not_exist),
    Res3 = admin_get_last(?EMPTY_NAME_JID, Config),
    get_coercion_err_msg(Res3).

admin_try_get_nonexistent_last(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_try_get_nonexistent_last_story/2).

admin_try_get_nonexistent_last_story(Config, Alice) ->
    Res = admin_get_last(Alice, Config),
    ?assertErrMsg(Res, <<"not found">>),
    ?assertErrCode(Res, last_not_found).

admin_count_active_users(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_count_active_users_story/3).

admin_count_active_users_story(Config, Alice, Bob) ->
    Domain = domain_helper:domain(),
    set_last(Alice, now_dt_with_offset(5), Config),
    set_last(Bob, now_dt_with_offset(10), Config),
    Res = admin_count_active_users(Domain, null, Config),
    ?assertEqual(2, get_ok_value(p(countActiveUsers), Res)),
    Res1 = admin_count_active_users(unprep(Domain), null, Config),
    ?assertEqual(2, get_ok_value(p(countActiveUsers), Res1)),
    Res2 = admin_count_active_users(Domain, now_dt_with_offset(30), Config),
    ?assertEqual(0, get_ok_value(p(countActiveUsers), Res2)).

admin_try_count_nonexistent_domain_active_users(Config) ->
    Res = admin_count_active_users(<<"unknown-domain.com">>, null, Config),
    ?assertErrMsg(Res, <<"not found">>),
    ?assertErrCode(Res, domain_not_found).

admin_try_count_active_users_invalid_timestamp(Config) ->
    Domain = domain_helper:domain(),
    Res = admin_count_active_users(Domain, ?INVALID_TIMESTAMP, Config),
    get_coercion_err_msg(Res).

%% Admin old users test cases

admin_remove_old_users_domain(Config) ->
    jids_with_config(Config, [alice, alice_bis, bob], fun admin_remove_old_users_domain_story/4).

admin_remove_old_users_domain_story(Config, Alice, AliceBis, Bob) ->
    Domain = domain_helper:domain(),
    ToRemoveDateTime = now_dt_with_offset(100),

    set_last(Bob, ToRemoveDateTime, Config),
    set_last(AliceBis, ToRemoveDateTime, Config),
    set_last(Alice, now_dt_with_offset(200), Config),

    Resp = admin_remove_old_users(Domain, now_dt_with_offset(150), Config),
    [#{<<"jid">> := Bob, <<"timestamp">> := BobDateTimeRes}] = get_ok_value(p(removeOldUsers), Resp),
    ?assertEqual(dt_to_unit(ToRemoveDateTime, second), dt_to_unit(BobDateTimeRes, second)),
    ?assertMatch({user_does_not_exist, _}, check_account(Bob)),
    ?assertMatch({ok, _}, check_account(Alice)),
    ?assertMatch({ok, _}, check_account(AliceBis)).

admin_try_remove_old_users_nonexistent_domain(Config) ->
    Res = admin_remove_old_users(?NONEXISTENT_DOMAIN, now_dt_with_offset(0), Config),
    ?assertErrMsg(Res, <<"not found">>),
    ?assertErrCode(Res, domain_not_found).

admin_try_remove_old_users_invalid_timestamp(Config) ->
    Domain = domain_helper:domain(),
    Res = admin_remove_old_users(Domain, ?INVALID_TIMESTAMP, Config),
    get_coercion_err_msg(Res).

admin_remove_old_users_global(Config) ->
    jids_with_config(Config, [alice, alice_bis, bob], fun admin_remove_old_users_global_story/4).

admin_remove_old_users_global_story(Config, Alice, AliceBis, Bob) ->
    ToRemoveDateTime = now_dt_with_offset(100),
    ToRemoveTimestamp = dt_to_unit(ToRemoveDateTime, second),

    set_last(Bob, ToRemoveDateTime, Config),
    set_last(AliceBis, ToRemoveDateTime, Config),
    set_last(Alice, now_dt_with_offset(200), Config),

    Resp = admin_remove_old_users(null, now_dt_with_offset(150), Config),
    [#{<<"jid">> := AliceBis, <<"timestamp">> := AliceBisDateTime},
     #{<<"jid">> := Bob, <<"timestamp">> := BobDateTime}] =
        lists:sort(get_ok_value(p(removeOldUsers), Resp)),
    ?assertEqual(ToRemoveTimestamp, dt_to_unit(BobDateTime, second)),
    ?assertEqual(ToRemoveTimestamp, dt_to_unit(AliceBisDateTime, second)),
    ?assertMatch({user_does_not_exist, _}, check_account(Bob)),
    ?assertMatch({user_does_not_exist, _}, check_account(AliceBis)),
    ?assertMatch({ok, _}, check_account(Alice)).

admin_list_old_users_domain(Config) ->
    jids_with_config(Config, [alice, bob], fun admin_list_old_users_domain_story/3).

admin_list_old_users_domain_story(Config, Alice, Bob) ->
    Domain = domain_helper:domain(),
    OldDateTime = now_dt_with_offset(100),

    set_last(Bob, OldDateTime, Config),
    set_last(Alice, now_dt_with_offset(200), Config),

    Res = admin_list_old_users(Domain, now_dt_with_offset(150), Config),
    [#{<<"jid">> := Bob, <<"timestamp">> := BobDateTime}] = get_ok_value(p(listOldUsers), Res),
    ?assertEqual(dt_to_unit(OldDateTime, second), dt_to_unit(BobDateTime, second)).

admin_try_list_old_users_nonexistent_domain(Config) ->
    Res = admin_list_old_users(?NONEXISTENT_DOMAIN, now_dt_with_offset(0), Config),
    ?assertErrMsg(Res, <<"not found">>),
    ?assertErrCode(Res, domain_not_found).

admin_try_list_old_users_invalid_timestamp(Config) ->
    Domain = domain_helper:domain(),
    Res = admin_list_old_users(Domain, ?INVALID_TIMESTAMP, Config),
    get_coercion_err_msg(Res).

admin_list_old_users_global(Config) ->
    jids_with_config(Config, [alice, alice_bis, bob], fun admin_list_old_users_global_story/4).

admin_list_old_users_global_story(Config, Alice, AliceBis, Bob) ->
    OldDateTime = now_dt_with_offset(100),

    set_last(Bob, OldDateTime, Config),
    set_last(AliceBis, OldDateTime, Config),
    set_last(Alice, now_dt_with_offset(200), Config),

    Res = admin_list_old_users(null, now_dt_with_offset(150), Config),
    [#{<<"jid">> := AliceBis, <<"timestamp">> := AliceBisDateTime},
     #{<<"jid">> := Bob, <<"timestamp">> := BobDateTime}] =
        lists:sort(get_ok_value(p(listOldUsers), Res)),
    ?assertEqual(dt_to_unit(OldDateTime, second), dt_to_unit(BobDateTime, second)),
    ?assertEqual(dt_to_unit(OldDateTime, second), dt_to_unit(AliceBisDateTime, second)).

admin_user_without_last_info_is_old_user(Config) ->
    Res = admin_list_old_users(null, now_dt_with_offset(150), Config),
    OldUsers = get_ok_value(p(listOldUsers), Res),
    ?assertEqual(3, length(OldUsers)),
    [?assertEqual(null, TS) || #{<<"timestamp">> := TS} <- OldUsers].

admin_logged_user_is_not_old_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_logged_user_is_not_old_user_story/2).

admin_logged_user_is_not_old_user_story(Config, _Alice) ->
    Res = admin_list_old_users(null, now_dt_with_offset(100), Config),
    ?assertEqual([], get_ok_value(p(listOldUsers), Res)).

%% Domain admin test cases

domain_admin_set_user_last_no_permission(Config) ->
    get_unauthorized(admin_set_last(?NONEXISTENT_JID, <<"status">>, null, Config)),
    escalus:fresh_story(Config, [{alice_bis, 1}], fun(AliceBis) ->
        BinJID = escalus_client:full_jid(AliceBis),
        Resp = admin_set_last(BinJID, <<"status">>, null, Config),
        get_unauthorized(Resp)
    end).

domain_admin_get_user_last_no_permission(Config) ->
    get_unauthorized(admin_get_last(?NONEXISTENT_JID, Config)),
    escalus:fresh_story(Config, [{alice_bis, 1}], fun(AliceBis) ->
        BinJID = escalus_client:full_jid(AliceBis),
        Resp = admin_get_last(BinJID, Config),
        get_unauthorized(Resp)
    end).

domain_admin_try_count_external_domain_active_users(Config) ->
    get_unauthorized(admin_count_active_users(?NONEXISTENT_DOMAIN, null, Config)),
    get_unauthorized(admin_count_active_users(domain_helper:secondary_domain(), null, Config)).

%% Domain admin old users test cases

domain_admin_try_list_old_users_external_domain(Config) ->
    ExternalDomain = domain_helper:secondary_domain(),
    get_unauthorized(admin_list_old_users(?NONEXISTENT_DOMAIN, now_dt_with_offset(0), Config)),
    get_unauthorized(admin_list_old_users(ExternalDomain, now_dt_with_offset(0), Config)).

domain_admin_try_remove_old_users_external_domain(Config) ->
    ExternalDomain = domain_helper:secondary_domain(),
    get_unauthorized(admin_remove_old_users(?NONEXISTENT_DOMAIN, now_dt_with_offset(0), Config)),
    get_unauthorized(admin_remove_old_users(ExternalDomain, now_dt_with_offset(0), Config)).

domain_admin_list_old_users_global(Config) ->
    jids_with_config(Config, [alice, alice_bis, bob],
                     fun domain_admin_list_old_users_global_story/4).

domain_admin_list_old_users_global_story(Config, Alice, AliceBis, Bob) ->
    OldDateTime = now_dt_with_offset(100),

    set_last(Bob, OldDateTime, Config),
    set_last(AliceBis, OldDateTime, Config),
    set_last(Alice, now_dt_with_offset(200), Config),

    get_unauthorized(admin_list_old_users(null, now_dt_with_offset(150), Config)).

domain_admin_remove_old_users_global(Config) ->
    jids_with_config(Config, [alice, alice_bis, bob],
                     fun domain_admin_remove_old_users_global_story/4).

domain_admin_remove_old_users_global_story(Config, Alice, AliceBis, Bob) ->
    ToRemoveDateTime = now_dt_with_offset(100),

    set_last(Bob, ToRemoveDateTime, Config),
    set_last(AliceBis, ToRemoveDateTime, Config),
    set_last(Alice, now_dt_with_offset(200), Config),

    get_unauthorized(admin_remove_old_users(null, now_dt_with_offset(150), Config)).

domain_admin_user_without_last_info_is_old_user(Config) ->
    get_unauthorized(admin_list_old_users(null, now_dt_with_offset(150), Config)).

domain_admin_logged_user_is_not_old_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun domain_admin_logged_user_is_not_old_user_story/2).

domain_admin_logged_user_is_not_old_user_story(Config, _Alice) ->
    get_unauthorized(admin_list_old_users(null, now_dt_with_offset(100), Config)).

%% User test cases

user_set_last(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_set_last_story/2).

user_set_last_story(Config, Alice) ->
    Status = <<"My first status">>,
    JID = escalus_utils:jid_to_lower(user_to_bin(Alice)),
    Res = user_set_last(Alice, Status, ?DEFAULT_DT, Config),
    #{<<"user">> := JID, <<"status">> := Status, <<"timestamp">> := ?DEFAULT_DT} =
        get_ok_value(p(setLast), Res),
    Status2 = <<"Quack Quack">>,
    Res2 = user_set_last(Alice, Status2, null, Config),
    #{<<"user">> := JID, <<"status">> := Status2, <<"timestamp">> := DateTime2} =
        get_ok_value(p(setLast), Res2),
    ?assert(os:system_time(second) - dt_to_unit(DateTime2, second) < 2).

user_get_last(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_get_last_story/2).
user_get_last_story(Config, Alice) ->
    Status = <<"I love ducks">>,
    JID = escalus_utils:jid_to_lower(user_to_bin(Alice)),
    user_set_last(Alice, Status, ?DEFAULT_DT, Config),
    Res = user_get_last(Alice, Alice, Config),
    #{<<"user">> := JID, <<"status">> := Status, <<"timestamp">> := ?DEFAULT_DT} =
        get_ok_value(p(getLast), Res).

user_get_other_user_last(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_get_other_user_last_story/3).

user_get_other_user_last_story(Config, Alice, Bob) ->
    Status = <<"In good mood">>,
    JID = escalus_utils:jid_to_lower(user_to_bin(Bob)),
    user_set_last(Bob, Status, ?DEFAULT_DT, Config),
    Res = user_get_last(Alice, Bob, Config),
    #{<<"user">> := JID, <<"status">> := Status, <<"timestamp">> := ?DEFAULT_DT} =
        get_ok_value(p(getLast), Res).


admin_set_last_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_set_last_not_configured_story/2).

admin_set_last_not_configured_story(Config, Alice) ->
    Status = <<"First status">>,
    Res = admin_set_last(Alice, Status, ?DEFAULT_DT, Config),
    get_not_loaded(Res).

admin_get_last_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_get_last_not_configured_story/2).

admin_get_last_not_configured_story(Config, Alice) ->
    Res = admin_get_last(Alice, Config),
    get_not_loaded(Res).

admin_count_active_users_last_not_configured(Config) ->
    Domain = domain_helper:domain(),
    get_not_loaded(admin_count_active_users(Domain, null, Config)),
    get_not_loaded(admin_count_active_users(unprep(Domain), null, Config)).

admin_remove_old_users_domain_last_not_configured(Config) ->
    Domain = domain_helper:domain(),
    get_not_loaded(admin_remove_old_users(Domain, now_dt_with_offset(150), Config)),
    get_not_loaded(admin_remove_old_users(unprep(Domain), now_dt_with_offset(150), Config)).

admin_remove_old_users_global_last_not_configured(Config) ->
    Res = admin_remove_old_users(null, now_dt_with_offset(150), Config),
    get_ok_value([], Res).

admin_list_old_users_domain_last_not_configured(Config) ->
    Domain = domain_helper:domain(),
    get_not_loaded(admin_list_old_users(Domain, now_dt_with_offset(150), Config)),
    get_not_loaded(admin_list_old_users(unprep(Domain), now_dt_with_offset(150), Config)).

admin_list_old_users_global_last_not_configured(Config) ->
    Res = admin_list_old_users(null, now_dt_with_offset(150), Config),
    get_ok_value([], Res).

user_set_last_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_set_last_not_configured_story/2).

user_set_last_not_configured_story(Config, Alice) ->
    Status = <<"My first status">>,
    Res = user_set_last(Alice, Status, ?DEFAULT_DT, Config),
    get_not_loaded(Res).

user_get_last_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_get_last_not_configured_story/2).
user_get_last_not_configured_story(Config, Alice) ->
    Res = user_get_last(Alice, Alice, Config),
    get_not_loaded(Res).

%% Helpers

jids_with_config(Config, Users, Fun) ->
    Args = [escalus_utils:jid_to_lower(escalus_users:get_jid(Config, User)) || User <- Users],
    apply(Fun, [Config | Args]).

set_last(UserJID, DateTime, Config) ->
    admin_set_last(UserJID, <<>>, DateTime, Config).

check_account(User) ->
    {Username, LServer} = jid:to_lus(user_to_jid(User)),
    rpc(mim(), mongoose_account_api, check_account, [mongoose_helper:make_jid(Username, LServer)]).

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

%% Commands

admin_set_last(User, Status, DateTime, Config) ->
    Vars = #{user => user_to_bin(User), timestamp => DateTime, status => Status},
    execute_command(<<"last">>, <<"setLast">>, Vars, Config).

admin_get_last(User, Config) ->
    Vars = #{user => user_to_bin(User)},
    execute_command(<<"last">>, <<"getLast">>, Vars, Config).

admin_count_active_users(Domain, Timestamp, Config) ->
    Vars = #{domain => Domain, timestamp => Timestamp},
    execute_command(<<"last">>, <<"countActiveUsers">>, Vars, Config).

admin_remove_old_users(Domain, Timestamp, Config) ->
    Vars = #{domain => Domain, timestamp => Timestamp},
    execute_command(<<"last">>, <<"removeOldUsers">>, Vars, Config).

admin_list_old_users(Domain, Timestamp, Config) ->
    Vars = #{domain => Domain, timestamp => Timestamp},
    execute_command(<<"last">>, <<"listOldUsers">>, Vars, Config).

user_set_last(User, Status, DateTime, Config) ->
    Vars = #{timestamp => DateTime, status => Status},
    execute_user_command(<<"last">>, <<"setLast">>, User, Vars, Config).

user_get_last(User, QueriedUser, Config) ->
    Vars = #{user => user_to_bin(QueriedUser)},
    execute_user_command(<<"last">>, <<"getLast">>, User, Vars, Config).
