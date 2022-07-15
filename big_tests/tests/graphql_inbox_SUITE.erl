-module(graphql_inbox_SUITE).

-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(graphql_helper, [execute_user_command/5, execute_command/4, user_to_bin/1,
                         get_ok_value/2, get_err_msg/1, get_err_code/1]).

-include_lib("eunit/include/eunit.hrl").
-include("inbox.hrl").

-define(assertErrMsg(Res, ContainsPart), assert_err_msg(ContainsPart, Res)).
-define(assertErrCode(Res, Code), assert_err_code(Code, Res)).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    inbox_helper:skip_or_run_inbox_tests(tests()).

tests() ->
    [{group, user_inbox},
     {group, admin_inbox_http},
     {group, admin_inbox_cli}].

groups() ->
    [{user_inbox, [], user_inbox_tests()},
     {admin_inbox_http, [], admin_inbox_tests()},
     {admin_inbox_cli, [], admin_inbox_tests()}].

user_inbox_tests() ->
    [user_flush_own_bin].

admin_inbox_tests() ->
    [admin_flush_user_bin,
     admin_try_flush_nonexistent_user_bin,
     admin_flush_domain_bin,
     admin_try_flush_nonexistent_domain_bin,
     admin_flush_global_bin,
     admin_flush_global_bin_after_days,
     admin_try_flush_nonexistent_host_type_bin].

init_per_suite(Config) ->
    HostType = domain_helper:host_type(),
    SecHostType = domain_helper:secondary_host_type(),
    Config1 = dynamic_modules:save_modules([HostType, SecHostType], Config),
    Modules = [{mod_inbox, inbox_helper:inbox_opts(async_pools)} | inbox_helper:muclight_modules()],
    ok = dynamic_modules:ensure_modules(HostType, Modules),
    ok = dynamic_modules:ensure_modules(SecHostType, Modules),
    Config2 = ejabberd_node_utils:init(mim(), Config1),
    escalus:init_per_suite(Config2).

end_per_suite(Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_inbox_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_inbox_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(user_inbox, Config) ->
    graphql_helper:init_user(Config).

end_per_group(_, _) ->
    graphql_helper:clean().

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    %% Clean users after each test case to keep inbox empty
    escalus_fresh:clean(),
    escalus:end_per_testcase(CaseName, Config).

%% Admin test cases

admin_flush_user_bin(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}, {kate, 1}],
                                    fun admin_flush_user_bin/4).

admin_flush_user_bin(Config, Alice, Bob, Kate) ->
    RoomBinJID = create_room_and_make_users_leave(Alice, Bob, Kate),
    Res = flush_user_bin(Bob, Config),
    NumOfRows = get_ok_value(p(flushUserBin), Res),
    ?assertEqual(1, NumOfRows),
    inbox_helper:check_inbox(Bob, [], #{box => bin}),
    check_aff_msg_in_inbox_bin(Kate, RoomBinJID).

admin_flush_domain_bin(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}, {kate, 1}],
                                    fun admin_flush_domain_bin/4).

admin_flush_domain_bin(Config, Alice, AliceBis, Kate) ->
    RoomBinJID = create_room_and_make_users_leave(Alice, AliceBis, Kate),
    Domain = domain_helper:domain(),
    Res = flush_domain_bin(Domain, Config),
    NumOfRows = get_ok_value(p(flushDomainBin), Res),
    ?assertEqual(1, NumOfRows),
    inbox_helper:check_inbox(Kate, [], #{box => bin}),
    check_aff_msg_in_inbox_bin(AliceBis, RoomBinJID).

admin_flush_global_bin(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}, {kate, 1}],
                                    fun admin_flush_global_bin/4).

admin_flush_global_bin(Config, Alice, AliceBis, Kate) ->
    SecHostType = domain_helper:host_type(),
    create_room_and_make_users_leave(Alice, AliceBis, Kate),
    Res = flush_global_bin(SecHostType, null, Config),
    NumOfRows = get_ok_value(p(flushGlobalBin), Res),
    ?assertEqual(2, NumOfRows),
    inbox_helper:check_inbox(AliceBis, [], #{box => bin}),
    inbox_helper:check_inbox(Kate, [], #{box => bin}).

admin_flush_global_bin_after_days(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}, {kate, 1}],
                                    fun admin_flush_global_bin_after_days/4).

admin_flush_global_bin_after_days(Config, Alice, AliceBis, Kate) ->
    SecHostType = domain_helper:host_type(),
    RoomBinJID = create_room_and_make_users_leave(Alice, AliceBis, Kate),
    Res = flush_global_bin(SecHostType, 1, Config),
    NumOfRows = get_ok_value(p(flushGlobalBin), Res),
    ?assertEqual(0, NumOfRows),
    check_aff_msg_in_inbox_bin(AliceBis, RoomBinJID),
    check_aff_msg_in_inbox_bin(Kate, RoomBinJID).

admin_try_flush_nonexistent_user_bin(Config) ->
    %% Check nonexistent domain error
    Res = flush_user_bin(<<"user@user.com">>, Config),
    ?assertErrMsg(Res, <<"not found">>),
    ?assertErrCode(Res, domain_not_found),
    %% Check nonexistent user error
    User = <<"nonexistent-user@", (domain_helper:domain())/binary>>,
    Res2 = flush_user_bin(User, Config),
    ?assertErrMsg(Res2, <<"does not exist">>),
    ?assertErrCode(Res2, user_does_not_exist).

admin_try_flush_nonexistent_domain_bin(Config) ->
    Res = flush_domain_bin(<<"unknown-domain">>, Config),
    ?assertErrMsg(Res, <<"not found">>),
    ?assertErrCode(Res, domain_not_found).

admin_try_flush_nonexistent_host_type_bin(Config) ->
    Res = flush_global_bin(<<"nonexistent host type">>, null, Config),
    ?assertErrMsg(Res, <<"not found">>),
    ?assertErrCode(Res, host_type_not_found).

%% User test cases

user_flush_own_bin(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}, {kate, 1}],
                                    fun user_flush_own_bin/4).

user_flush_own_bin(Config, Alice, Bob, Kate) ->
    create_room_and_make_users_leave(Alice, Bob, Kate),
    Res = user_flush_own_bin(Bob, Config),
    NumOfRows = get_ok_value(p(flushBin), Res),
    ?assertEqual(1, NumOfRows),
    inbox_helper:check_inbox(Bob, [], #{box => bin}).

%% Helpers

create_room_and_make_users_leave(Alice, Bob, Kate) ->
    RoomName = inbox_SUITE:create_room_and_make_users_leave(Alice, Bob, Kate),
    muc_light_helper:room_bin_jid(RoomName).

check_aff_msg_in_inbox_bin(User, RoomBinJID) ->
    UserShort = escalus_client:short_jid(User),
    Convs = [#conv{unread = 1, from = RoomBinJID, to = UserShort, content = <<>>}],
    inbox_helper:check_inbox(User, Convs, #{box => bin}).

assert_err_msg(Contains, Res) ->
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), Contains)).

assert_err_code(Code, Res) ->
    ?assertEqual(atom_to_binary(Code), get_err_code(Res)).

p(Cmd) when is_atom(Cmd) ->
    [data, inbox, Cmd];
p(Path) when is_list(Path) ->
    [data, inbox] ++ Path.

%% Commands

flush_user_bin(User, Config) ->
    Vars = #{user => user_to_bin(User)},
    graphql_helper:execute_command(<<"inbox">>, <<"flushUserBin">>, Vars, Config).

flush_domain_bin(Domain, Config) ->
    Vars = #{domain => Domain},
    graphql_helper:execute_command(<<"inbox">>, <<"flushDomainBin">>, Vars, Config).

flush_global_bin(HostType, Days, Config) ->
    Vars = #{hostType => HostType, days => Days},
    graphql_helper:execute_command(<<"inbox">>, <<"flushGlobalBin">>, Vars, Config).

user_flush_own_bin(User, Config) ->
    graphql_helper:execute_user_command(<<"inbox">>, <<"flushBin">>, User, #{}, Config).
