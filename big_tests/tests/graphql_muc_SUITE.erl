-module(graphql_muc_SUITE).

-compile([export_all, nowarn_export_all]).

-import(common_helper, [unprep/1]).
-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4, subhost_pattern/1]).
-import(graphql_helper, [execute_command/4, execute_user_command/5, get_ok_value/2, get_err_msg/1,
                         get_coercion_err_msg/1, user_to_bin/1, user_to_full_bin/1, user_to_jid/1,
                         get_unauthorized/1, get_not_loaded/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("jid/include/jid.hrl").

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user},
     {group, admin_http},
     {group, admin_cli},
     {group, domain_admin_muc}].

groups() ->
    [{user, [], user_groups()},
     {admin_http, [], admin_groups()},
     {admin_cli, [], admin_groups()},
     {admin_muc_configured, [], admin_muc_tests()},
     {admin_muc_and_mam_configured, [], admin_muc_with_mam_tests()},
     {admin_muc_not_configured, [], admin_muc_not_configured_tests()},
     {user_muc_configured, [parallel], user_muc_tests()},
     {user_muc_and_mam_configured, [parallel], user_muc_with_mam_tests()},
     {user_muc_not_configured, [parallel], user_muc_not_configured_tests()},
     {domain_admin_muc, [], domain_admin_muc_tests()}].

user_groups() ->
    [{group, user_muc_configured},
     {group, user_muc_and_mam_configured},
     {group, user_muc_not_configured}].

admin_groups() ->
    [{group, admin_muc_configured},
     {group, admin_muc_and_mam_configured},
     {group, admin_muc_not_configured}].

user_muc_tests() ->
    [user_create_and_delete_room,
     user_create_room_with_unprepped_name,
     user_try_delete_nonexistent_room,
     user_try_delete_room_by_not_owner,
     user_try_create_instant_room_with_nonexistent_domain,
     user_try_create_instant_room_with_nonexistent_subdomain,
     user_try_create_instant_room_with_invalid_args,
     user_list_rooms,
     user_try_list_rooms_for_nonexistent_domain,
     user_list_room_users,
     user_list_room_users_without_anonymous_mode,
     user_try_list_room_users_without_permission,
     user_try_list_nonexistent_room_users,
     user_change_room_config,
     user_try_change_nonexistent_room_config,
     user_get_room_config,
     user_try_get_nonexistent_room_config,
     user_invite_user,
     user_kick_user,
     user_try_kick_user_from_nonexistent_room,
     user_try_kick_user_without_moderator_resource,
     user_send_message_to_room,
     user_send_message_to_room_with_specified_res,
     user_send_private_message,
     user_without_session_send_message_to_room,
     user_owner_set_user_affiliation,
     user_admin_set_user_affiliation,
     user_member_set_user_affiliation,
     user_try_set_nonexistent_room_affiliation,
     user_moderator_set_user_role,
     user_participant_set_user_role,
     user_try_set_nonexistent_room_role,
     user_can_enter_room,
     user_cannot_enter_room_with_invalid_resource,
     user_can_enter_room_with_password,
     user_can_exit_room,
     user_list_room_affiliations,
     user_try_list_room_affiliations_without_permission,
     user_try_list_nonexistent_room_affiliations,
     user_get_room_messages_muc_or_mam_not_configured
    ].

user_muc_with_mam_tests() ->
    [user_get_room_messages,
     user_shouldnt_store_messages_in_muc_light,
     user_try_get_nonexistent_room_messages,
     user_try_get_room_messages_without_permission].

user_muc_not_configured_tests() ->
    [user_delete_room_muc_not_configured,
     user_list_room_users_muc_not_configured,
     user_change_room_config_muc_not_configured,
     user_get_room_config_muc_not_configured,
     user_invite_user_muc_not_configured,
     user_kick_user_muc_not_configured,
     user_send_message_to_room_muc_not_configured,
     user_send_private_message_muc_not_configured,
     user_get_room_messages_muc_or_mam_not_configured,
     user_owner_set_user_affiliation_muc_not_configured,
     user_moderator_set_user_role_muc_not_configured,
     user_can_enter_room_muc_not_configured,
     user_can_exit_room_muc_not_configured,
     user_list_room_affiliations_muc_not_configured].

admin_muc_tests() ->
    [admin_list_rooms,
     admin_list_rooms_with_invalid_args,
     admin_create_and_delete_room,
     admin_create_room_with_unprepped_name,
     admin_try_create_instant_room_with_nonexistent_domain,
     admin_try_create_instant_room_with_nonexistent_subdomain,
     admin_try_create_instant_room_with_nonexistent_user,
     admin_try_create_instant_room_with_invalid_args,
     admin_try_delete_nonexistent_room,
     admin_try_delete_room_with_nonexistent_domain,
     admin_try_list_rooms_for_nonexistent_domain,
     admin_list_room_users,
     admin_try_list_users_from_nonexistent_room,
     admin_change_room_config,
     admin_try_change_nonexistent_room_config,
     admin_get_room_config,
     admin_try_get_nonexistent_room_config,
     admin_invite_user,
     admin_invite_user_with_password,
     admin_try_invite_user_to_nonexistent_room,
     admin_kick_user,
     admin_try_kick_user_from_nonexistent_room,
     admin_try_kick_user_from_room_without_moderators,
     admin_send_message_to_room,
     admin_send_private_message,
     admin_set_user_affiliation,
     admin_try_set_nonexistent_room_user_affiliation,
     admin_set_user_role,
     admin_try_set_nonexistent_room_user_role,
     admin_try_set_nonexistent_nick_role,
     admin_try_set_user_role_in_room_without_moderators,
     admin_make_user_enter_room,
     admin_make_user_enter_room_with_password,
     admin_make_user_enter_room_bare_jid,
     admin_make_user_exit_room,
     admin_make_user_exit_room_bare_jid,
     admin_list_room_affiliations,
     admin_try_list_nonexistent_room_affiliations,
     admin_get_room_messages_muc_or_mam_not_configured
    ].

admin_muc_with_mam_tests() ->
    [admin_get_room_messages,
     admin_try_get_nonexistent_room_messages].

admin_muc_not_configured_tests() ->
    [admin_delete_room_muc_not_configured,
     admin_list_room_users_muc_not_configured,
     admin_change_room_config_muc_not_configured,
     admin_get_room_config_muc_not_configured,
     admin_invite_user_muc_not_configured,
     admin_kick_user_muc_not_configured,
     admin_send_message_to_room_muc_not_configured,
     admin_send_private_message_muc_not_configured,
     admin_get_room_messages_muc_or_mam_not_configured,
     admin_set_user_affiliation_muc_not_configured,
     admin_set_user_role_muc_not_configured,
     admin_make_user_enter_room_muc_not_configured,
     admin_make_user_exit_room_muc_not_configured,
     admin_list_room_affiliations_muc_not_configured].

domain_admin_muc_tests() ->
    [admin_list_rooms,
     admin_create_and_delete_room,
     admin_create_room_with_unprepped_name,
     admin_try_create_instant_room_with_nonexistent_domain,
     admin_try_delete_nonexistent_room,
     domain_admin_create_and_delete_room_no_permission,
     domain_admin_list_rooms_no_permission,
     admin_list_room_users,
     domain_admin_list_room_users_no_permission,
     admin_change_room_config,
     domain_admin_change_room_config_no_permission,
     admin_get_room_config,
     domain_admin_get_room_config_no_permission,
     admin_invite_user,
     admin_invite_user_with_password,
     admin_try_invite_user_to_nonexistent_room,
     domain_admin_invite_user_no_permission,
     admin_kick_user,
     admin_try_kick_user_from_room_without_moderators,
     domain_admin_kick_user_no_permission,
     admin_send_message_to_room,
     domain_admin_send_message_to_room_no_permission,
     admin_send_private_message,
     domain_admin_send_private_message_no_permission,
     admin_get_room_messages,
     domain_admin_get_room_messages_no_permission,
     admin_set_user_affiliation,
     domain_admin_set_user_affiliation_no_permission,
     admin_set_user_role,
     admin_try_set_nonexistent_nick_role,
     admin_try_set_user_role_in_room_without_moderators,
     domain_admin_set_user_role_no_permission,
     admin_make_user_enter_room,
     admin_make_user_enter_room_with_password,
     admin_make_user_enter_room_bare_jid,
     domain_admin_make_user_enter_room_no_permission,
     admin_make_user_exit_room,
     admin_make_user_exit_room_bare_jid,
     domain_admin_make_user_exit_room_no_permission,
     admin_list_room_affiliations,
     domain_admin_list_room_affiliations_no_permission
    ].

init_per_suite(Config) ->
    HostType = domain_helper:host_type(),
    SecondaryHostType = domain_helper:secondary_host_type(),
    Config2 = escalus:init_per_suite(Config),
    Config3 = dynamic_modules:save_modules(HostType, Config2),
    Config4 = dynamic_modules:save_modules(SecondaryHostType, Config3),
    Config5 = ejabberd_node_utils:init(mim(), Config4),
    dynamic_modules:restart(HostType, mod_disco,
                            config_parser_helper:default_mod_config(mod_disco)),
    Config5.

end_per_suite(Config) ->
    escalus_fresh:clean(),
    mongoose_helper:ensure_muc_clean(),
    ensure_muc_stopped(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(admin_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin_muc, Config) ->
    maybe_enable_mam(),
    ensure_muc_started(),
    graphql_helper:init_domain_admin_handler(Config);
init_per_group(user, Config) ->
    graphql_helper:init_user(Config);
init_per_group(Group, Config) when Group =:= admin_muc_configured;
                                   Group =:= user_muc_configured ->
    disable_mam(),
    ensure_muc_started(),
    Config;
init_per_group(Group, Config) when Group =:= admin_muc_and_mam_configured;
                                   Group =:= user_muc_and_mam_configured ->
    case maybe_enable_mam() of
        true ->
            ensure_muc_started(),
            ensure_muc_light_started(Config);
        false ->
            {skip, "No MAM backend available"}
    end;
init_per_group(Group, Config) when Group =:= admin_muc_not_configured;
                                   Group =:= user_muc_not_configured ->
    maybe_enable_mam(),
    ensure_muc_stopped(),
    Config.

disable_mam() ->
    dynamic_modules:ensure_modules(domain_helper:host_type(), [{mod_mam, stopped}]).

maybe_enable_mam() ->
    case mam_helper:backend() of
        disabled ->
            false;
        Backend ->
            MAMOpts = mam_helper:config_opts(
                        #{backend => Backend,
                          muc => #{host => subhost_pattern(muc_helper:muc_host_pattern())},
                          async_writer => #{enabled => false}}),
            dynamic_modules:ensure_modules(domain_helper:host_type(), [{mod_mam, MAMOpts}]),
            true
    end.

ensure_muc_started() ->
    SecondaryHostType = domain_helper:secondary_host_type(),
    muc_helper:load_muc(),
    muc_helper:load_muc(SecondaryHostType),
    mongoose_helper:ensure_muc_clean().

ensure_muc_stopped() ->
    SecondaryHostType = domain_helper:secondary_host_type(),
    muc_helper:unload_muc(),
    muc_helper:unload_muc(SecondaryHostType).

ensure_muc_light_started(Config) ->
    Backend = mongoose_helper:mnesia_or_rdbms_backend(),
    MucLightOpts = config_parser_helper:mod_config(mod_muc_light,
        #{backend => Backend, rooms_in_rosters => true, config_schema => custom_schema()}),
    HostType = domain_helper:host_type(),
    dynamic_modules:ensure_modules(HostType, [{mod_muc_light, MucLightOpts}]),
    [{muc_light_host, muc_light_helper:muc_host()} | Config].

ensure_muc_light_stopped() ->
    HostType = domain_helper:host_type(),
    dynamic_modules:ensure_modules(HostType, [{mod_muc_light, stopped}]).

custom_schema() ->
    %% Should be sorted
    [{<<"background">>, <<>>, background, binary},
     {<<"music">>, <<>>, music, binary},
     %% Default fields
     {<<"roomname">>, <<>>, roomname, binary},
     {<<"subject">>, <<"Test">>, subject, binary}].

end_per_group(Group, _Config) when Group =:= user;
                                   Group =:= admin_http;
                                   Group =:= domain_admin_muc;
                                   Group =:= admin_cli ->
    graphql_helper:clean();
end_per_group(Group, _Config) when Group =:= admin_muc_and_mam_configured;
                                   Group =:= user_muc_and_mam_configured ->
    ensure_muc_light_stopped(),
    escalus_fresh:clean();
end_per_group(_Group, _Config) ->
    escalus_fresh:clean().

init_per_testcase(TC, Config) ->
    escalus:init_per_testcase(TC, Config).

end_per_testcase(TC, Config) ->
    escalus:end_per_testcase(TC, Config).

-define(CREATE_INSTANT_ROOM_PATH, [data, muc, createInstantRoom]).
-define(LIST_ROOMS_PATH, [data, muc, listRooms]).
-define(INVITE_USER_PATH, [data, muc, inviteUser]).
-define(KICK_USER_PATH, [data, muc, kickUser]).
-define(DELETE_ROOM_PATH, [data, muc, deleteRoom]).
-define(SEND_MESSAGE_PATH, [data, muc, sendMessageToRoom]).
-define(SEND_PRIV_MESG_PATH, [data, muc, sendPrivateMessage]).
-define(GET_MESSAGES_PATH, [data, muc, getRoomMessages]).
-define(LIST_ROOM_USERS_PATH, [data, muc, listRoomUsers]).
-define(LIST_ROOM_AFFILIATIONS_PATH, [data, muc, listRoomAffiliations]).
-define(CHANGE_ROOM_CONFIG_PATH, [data, muc, changeRoomConfiguration]).
-define(GET_ROOM_CONFIG_PATH, [data, muc, getRoomConfig]).
-define(SET_AFFILIATION_PATH, [data, muc, setUserAffiliation]).
-define(SET_ROLE_PATH, [data, muc, setUserRole]).
-define(ENTER_ROOM_PATH, [data, muc, enterRoom]).
-define(EXIT_ROOM_PATH, [data, muc, exitRoom]).

-define(NONEXISTENT_ROOM, <<"room@room">>).
-define(NONEXISTENT_ROOM2, <<"room@", (muc_helper:muc_host())/binary>>).
-define(EXTERNAL_DOMAIN_ROOM, <<"external_room@muc.", (domain_helper:secondary_domain())/binary>>).
-define(PASSWORD, <<"pa5sw0rd">>).

admin_list_rooms(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun admin_list_rooms_story/3).

admin_list_rooms_story(Config, Alice, Bob) ->
    Res0 = list_rooms(muc_helper:muc_host(), Alice, null, null, Config),
    ?assertEqual([], extract_rooms(get_ok_value(?LIST_ROOMS_PATH, Res0))),
    AliceJID = jid:from_binary(escalus_client:short_jid(Alice)),
    BobJID = jid:from_binary(escalus_client:short_jid(Bob)),
    AliceRoom = rand_name(),
    BobRoom = rand_name(),
    muc_helper:create_instant_room(AliceRoom, AliceJID, <<"Ali">>, []),
    muc_helper:create_instant_room(BobRoom, BobJID, <<"Bob">>, [{public_list, false}]),
    Res1 = list_rooms(muc_helper:muc_host(), Alice, null, null, Config),
    Rooms1 = [_, RoomB] = extract_rooms(get_ok_value(?LIST_ROOMS_PATH, Res1)),
    ?assertEqual(lists:sort([AliceRoom, BobRoom]), lists:sort(Rooms1)),
    Res2 = list_rooms(unprep(muc_helper:muc_host()), Alice, null, null, Config),
    ?assertEqual(Rooms1, extract_rooms(get_ok_value(?LIST_ROOMS_PATH, Res2))),
    Res3 = list_rooms(muc_helper:muc_host(), Alice, 1, 1, Config),
    ?assertEqual([RoomB], extract_rooms(get_ok_value(?LIST_ROOMS_PATH, Res3))).

admin_list_rooms_with_invalid_args(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}]),
    AliceJid = escalus_users:get_jid(Config1, alice),
    AliceDomain = escalus_users:get_host(Config1, alice),
    Res1 = list_rooms(muc_helper:muc_host(), AliceDomain, null, null, Config1),
    assert_coercion_err(Res1, <<"jid_without_local_part">>),
    Res2 = list_rooms(muc_helper:muc_host(), AliceJid, 0, null, Config1),
    assert_coercion_err(Res2, <<"Value is not a positive integer">>),
    Res3 = list_rooms(muc_helper:muc_host(), AliceJid, null, -1, Config1),
    assert_coercion_err(Res3, <<"Value is not a non-negative integer">>).

admin_try_list_rooms_for_nonexistent_domain(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}]),
    AliceJID = escalus_users:get_jid(Config1, alice),
    Res1 = list_rooms(<<"baddomain">>, AliceJID, null, null, Config1),
    ?assertMatch({_, _}, binary:match(get_err_msg(Res1), <<"not found">>)),
    %% Domain instead of the MUC subdomain
    Res2 = list_rooms(domain_helper:domain(), AliceJID, null, null, Config1),
    ?assertMatch({_, _}, binary:match(get_err_msg(Res2), <<"not found">>)).

admin_create_and_delete_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_create_and_delete_room_story/2).

admin_create_and_delete_room_story(Config, Alice) ->
    Name = <<"first-alice-room">>,
    MUCServer = muc_helper:muc_host(),
    RoomJID = jid:make_bare(Name, MUCServer),
    % Create instant room
    Res = create_instant_room(RoomJID, Alice, <<"Ali">>, Config),
    ?assertMatch(#{<<"title">> := Name, <<"private">> := false, <<"usersNumber">> := 0},
                 get_ok_value(?CREATE_INSTANT_ROOM_PATH, Res)),
    Res2 = list_rooms(MUCServer, Alice, null, null, Config),
    ?assert(contain_room(Name, get_ok_value(?LIST_ROOMS_PATH, Res2))),
    % Delete room
    Res3 = delete_room(RoomJID, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?DELETE_ROOM_PATH, Res3),
                                          <<"successfully">>)),
    Res4 = list_rooms(MUCServer, Alice, null, null, Config),
    ?assertNot(contain_room(Name, get_ok_value(?LIST_ROOMS_PATH, Res4))).

admin_create_room_with_unprepped_name(Config) ->
    FreshConfig = escalus_fresh:create_users(Config, [{alice, 1}]),
    AliceJid = escalus_users:get_jid(FreshConfig, alice),
    Name = <<$a, (rand_name())/binary>>, % make it start with a letter
    MUCServer = muc_helper:muc_host(),
    RoomJID = jid:make_noprep(unprep(Name), unprep(MUCServer), <<>>),
    Res = create_instant_room(RoomJID, AliceJid, <<"Ali">>, FreshConfig),
    ?assertMatch(#{<<"title">> := Name, <<"private">> := false, <<"usersNumber">> := 0},
                 get_ok_value(?CREATE_INSTANT_ROOM_PATH, Res)),
    Res2 = list_rooms(MUCServer, AliceJid, null, null, Config),
    ?assert(contain_room(Name, get_ok_value(?LIST_ROOMS_PATH, Res2))).

admin_try_create_instant_room_with_nonexistent_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_try_create_instant_room_with_nonexistent_domain_story/2).

admin_try_create_instant_room_with_nonexistent_domain_story(Config, Alice) ->
    Res = create_instant_room(jid:make_bare(rand_name(), <<"unknown">>), Alice, <<"Ali">>, Config),
    ?assertEqual(<<"Error while creating a room">>, get_err_msg(Res)).

admin_try_create_instant_room_with_nonexistent_subdomain(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_try_create_instant_room_with_nonexistent_subdomain_story/2).

admin_try_create_instant_room_with_nonexistent_subdomain_story(Config, Alice) ->
    TopDomain = escalus_client:server(Alice),
    RoomJID = jid:make_bare(rand_name(), <<"unknown_muc.", TopDomain/binary>>),
    Res = create_instant_room(RoomJID, Alice, <<"Ali">>, Config),
    ?assertEqual(<<"Could not create room due to incorrect domain">>, get_err_msg(Res)).

admin_try_create_instant_room_with_nonexistent_user(Config) ->
    RoomJID = jid:make_bare(rand_name(), muc_helper:muc_host()),
    JID = <<(rand_name())/binary, "@localhost">>,
    Res = create_instant_room(RoomJID, JID, <<"Ali">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_try_create_instant_room_with_invalid_args(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}]),
    AliceJid = escalus_users:get_jid(Config1, alice),
    AliceDomain = escalus_users:get_host(Config1, alice),
    Domain = muc_helper:muc_host(),
    Res1 = create_instant_room(<<"test room@", Domain/binary>>, AliceJid, <<"Ali">>, Config1),
    assert_coercion_err(Res1, <<"failed_to_parse_jid">>),
    Res2 = create_instant_room(<<"testroom@", Domain/binary, "/res1">>, AliceJid, <<"Ali">>, Config1),
    assert_coercion_err(Res2, <<"jid_with_resource">>),
    Res3 = create_instant_room(Domain, AliceJid, <<"Ali">>, Config1),
    assert_coercion_err(Res3, <<"jid_without_local_part">>),
    Res4 = create_instant_room(<<"testroom@", Domain/binary>>, AliceDomain, <<"Ali">>, Config1),
    assert_coercion_err(Res4, <<"jid_without_local_part">>),
    Res5 = create_instant_room(<<"testroom@", Domain/binary>>, AliceJid, <<>>, Config1),
    assert_coercion_err(Res5, <<"empty_resource_name">>).

admin_try_delete_nonexistent_room(Config) ->
    RoomJID = jid:make_bare(<<"unknown">>, muc_helper:muc_host()),
    Res = delete_room(RoomJID, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"non-existent">>)).

admin_try_delete_room_with_nonexistent_domain(Config) ->
    RoomJID = jid:make_bare(<<"unknown">>, <<"unknown">>),
    Res = delete_room(RoomJID, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"non-existent">>)).

admin_invite_user(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}], fun admin_invite_user_story/3).

admin_invite_user_story(Config, Alice, Bob) ->
    RoomJIDBin = ?config(room_jid, Config),
    RoomJID = jid:from_binary(RoomJIDBin),
    Res = invite_user(RoomJID, Alice, Bob, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?INVITE_USER_PATH, Res),
                                          <<"successfully">>)),
    Stanza = escalus:wait_for_stanza(Bob),
    escalus:assert(is_message, Stanza),
    ?assertEqual(RoomJIDBin,
                 exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"jid">>}])),
    ?assertEqual(undefined, exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"password">>}])).

admin_invite_user_with_password(Config) ->
    muc_helper:story_with_room(Config, [{password_protected, true}, {password, ?PASSWORD}],
                               [{alice, 1}, {bob, 1}], fun admin_invite_user_with_password/3).

admin_invite_user_with_password(Config, Alice, Bob) ->
    RoomJIDBin = ?config(room_jid, Config),
    RoomJID = jid:from_binary(RoomJIDBin),
    Res = invite_user(RoomJID, Alice, Bob, null, Config),
    assert_success(?INVITE_USER_PATH, Res),
    Stanza = escalus:wait_for_stanza(Bob),
    escalus:assert(is_message, Stanza),
    ?assertEqual(RoomJIDBin,
                 exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"jid">>}])),
    ?assertEqual(?PASSWORD, exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"password">>}])).

admin_try_invite_user_to_nonexistent_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_try_invite_user_to_nonexistent_room_story/3).

admin_try_invite_user_to_nonexistent_room_story(Config, Alice, Bob) ->
    Res = invite_user(?NONEXISTENT_ROOM, Alice, Bob, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_kick_user(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}], fun admin_kick_user_story/3).

admin_kick_user_story(Config, Alice, Bob) ->
    RoomJIDBin = ?config(room_jid, Config),
    RoomJID = jid:from_binary(RoomJIDBin),
    BobNick = <<"Bobek">>,
    Reason = <<"You are too laud">>,
    enter_room(RoomJID, Alice, <<"ali">>),
    enter_room(RoomJID, Bob, BobNick),
    Res = kick_user(RoomJID, BobNick, Reason, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?KICK_USER_PATH, Res),
                                          <<"successfully">>)),
    escalus:wait_for_stanzas(Bob, 2),
    KickStanza = escalus:wait_for_stanza(Bob),
    escalus:assert(is_presence_with_type, [<<"unavailable">>], KickStanza),
    ?assertEqual(Reason,
                 exml_query:path(KickStanza, [{element, <<"x">>}, {element, <<"item">>},
                                              {element, <<"reason">>}, cdata])).

admin_try_kick_user_from_room_without_moderators(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_try_kick_user_from_room_without_moderators/3).

admin_try_kick_user_from_room_without_moderators(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Bobek">>,
    enter_room(RoomJID, Bob, BobNick),
    Res = kick_user(RoomJID, BobNick, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_try_kick_user_from_nonexistent_room(Config) ->
    Res = kick_user(?NONEXISTENT_ROOM, <<"ali">>, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_send_message_to_room(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_send_message_to_room_story/3).

admin_send_message_to_room_story(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Message = <<"Hello All!">>,
    BobNick = <<"Bobek">>,
    enter_room(RoomJID, Bob, BobNick),
    escalus:wait_for_stanza(Bob),
    % Try send message from bare JID,
    BareBob = escalus_client:short_jid(Bob),
    Res = send_message_to_room(RoomJID, BareBob, Message, Config),
    assert_no_full_jid(Res),
    % Send message
    Res1 = send_message_to_room(RoomJID, Bob, Message, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SEND_MESSAGE_PATH, Res1),
                                          <<"successfully">>)),
    assert_is_message_correct(RoomJID, BobNick, <<"groupchat">>, Message,
                              escalus:wait_for_stanza(Bob)).

admin_send_private_message(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_send_private_message/3).

admin_send_private_message(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Message = <<"Hello Bob!">>,
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Alice, AliceNick),
    enter_room(RoomJID, Bob, BobNick),
    escalus:wait_for_stanzas(Bob, 2),
    % Try send private message from bare JID,
    BareAlice = escalus_client:short_jid(Alice),
    Res = send_private_message(RoomJID, BareAlice, BobNick, Message, Config),
    assert_no_full_jid(Res),
    % Try send private message to empty nick
    Res1 = send_private_message(RoomJID, Alice, <<>>, Message, Config),
    assert_coercion_err(Res1, <<"empty_resource_name">>),
    % Send message
    Res2 = send_private_message(RoomJID, Alice, BobNick, Message, Config),
    assert_success(?SEND_PRIV_MESG_PATH, Res2),
    assert_is_message_correct(RoomJID, AliceNick, <<"chat">>, Message,
                              escalus:wait_for_stanza(Bob)).

admin_get_room_config(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun admin_get_room_config_story/2).

admin_get_room_config_story(Config, _Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = get_room_config(RoomJID, Config),
    assert_default_room_config(Res).

admin_try_get_nonexistent_room_config(Config) ->
    Res = get_room_config(?NONEXISTENT_ROOM, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_change_room_config(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun admin_change_room_config_story/2).

admin_change_room_config_story(Config, _Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Title = <<"aloes">>,
    Description = <<"The chat about aloes">>,
    Public = false,
    RoomConfig = #{title => Title, description => Description, public => Public},
    Res = change_room_config(RoomJID, RoomConfig, Config),
    ?assertMatch(#{<<"title">> := Title,
                   <<"description">> := Description,
                   <<"public">> := Public}, get_ok_value(?CHANGE_ROOM_CONFIG_PATH, Res)).

admin_try_change_nonexistent_room_config(Config) ->
    RoomConfig = #{title => <<"NewTitle">>},
    Res = change_room_config(?NONEXISTENT_ROOM, RoomConfig, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_list_room_users(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_list_room_users_story/3).

admin_list_room_users_story(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Alice, AliceNick),
    Res = list_room_users(RoomJID, Config),
    ExpectedUsers = [{escalus_client:full_jid(Bob), BobNick, <<"PARTICIPANT">>},
                     {escalus_client:full_jid(Alice), AliceNick, <<"MODERATOR">>}],
    assert_room_users(ExpectedUsers, get_ok_value(?LIST_ROOM_USERS_PATH, Res)).

admin_try_list_users_from_nonexistent_room(Config) ->
    Res = list_room_users(?NONEXISTENT_ROOM, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_get_room_messages(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_get_room_messages_story/3).

admin_get_room_messages_story(Config, Alice, Bob) ->
    RoomJID = #jid{luser = RoomName, lserver = MUCDomain} =
        jid:from_binary(?config(room_jid, Config)),
    enter_room(RoomJID, Bob, <<"Bobek">>),
    enter_room(RoomJID, Alice, <<"Ali">>),
    escalus:wait_for_stanzas(Bob, 2),
    send_message_to_room(RoomJID, Bob, <<"Hi!">>, Config),
    escalus:wait_for_stanzas(Bob, 1),
    mam_helper:wait_for_room_archive_size(MUCDomain, RoomName, 1),
    Res = get_room_messages(RoomJID, 50, null, Config),
    #{<<"stanzas">> := [#{<<"stanza">> := StanzaXML}], <<"limit">> := 50} =
        get_ok_value(?GET_MESSAGES_PATH, Res),
    ?assertMatch({ok, #xmlel{name = <<"message">>}}, exml:parse(StanzaXML)).

admin_try_get_nonexistent_room_messages(Config) ->
    Res = get_room_messages(?NONEXISTENT_ROOM, null, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).


admin_set_user_affiliation(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_set_user_affiliation/3).

admin_set_user_affiliation(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    % Grant member affiliation
    Res = set_user_affiliation(RoomJID, Bob, member, Config),
    assert_success(?SET_AFFILIATION_PATH, Res),
    assert_user_affiliation(RoomJID, Bob, member),
    % Grant admin affiliation
    Res1 = set_user_affiliation(RoomJID, Bob, admin, Config),
    assert_success(?SET_AFFILIATION_PATH, Res1),
    assert_user_affiliation(RoomJID, Bob, admin),
    % Grant owner affiliation
    Res2 = set_user_affiliation(RoomJID, Bob, owner, Config),
    assert_success(?SET_AFFILIATION_PATH, Res2),
    assert_user_affiliation(RoomJID, Bob, owner),
    % Revoke affiliation
    Res3 = set_user_affiliation(RoomJID, Bob, none, Config),
    assert_success(?SET_AFFILIATION_PATH, Res3),
    assert_user_affiliation(RoomJID, Bob, none),
    % Ban user
    Res4 = set_user_affiliation(RoomJID, Bob, outcast, Config),
    assert_success(?SET_AFFILIATION_PATH, Res4),
    assert_user_affiliation(RoomJID, Bob, outcast).

admin_try_set_nonexistent_room_user_affiliation(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_try_set_nonexistent_room_user_affiliation/2).

admin_try_set_nonexistent_room_user_affiliation(Config, Alice) ->
    Res = set_user_affiliation(?NONEXISTENT_ROOM, Alice, admin, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_set_user_role(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}], fun admin_set_user_role/3).

admin_set_user_role(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Boobek">>,
    enter_room(RoomJID, Alice, escalus_client:username(Alice)),
    enter_room(RoomJID, Bob, BobNick),
    % Change from participant to visitor
    Res = set_user_role(RoomJID, BobNick, visitor, Config),
    assert_success(?SET_ROLE_PATH, Res),
    assert_user_role(RoomJID, Bob, visitor),
    % Change from visitor to participant
    Res1 = set_user_role(RoomJID, BobNick, participant, Config),
    assert_success(?SET_ROLE_PATH, Res1),
    assert_user_role(RoomJID, Bob, participant),
    % Change from participant to moderator
    Res2 = set_user_role(RoomJID, BobNick, moderator, Config),
    assert_success(?SET_ROLE_PATH, Res2),
    assert_user_role(RoomJID, Bob, moderator).

admin_try_set_nonexistent_nick_role(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun admin_try_set_nonexistent_nick_role/2).

admin_try_set_nonexistent_nick_role(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    enter_room(RoomJID, Alice, escalus_client:username(Alice)),
    Res = set_user_role(RoomJID, <<"kik">>, visitor, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not exist">>)).

admin_try_set_user_role_in_room_without_moderators(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_try_set_user_role_in_room_without_moderators/3).

admin_try_set_user_role_in_room_without_moderators(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Boobek">>,
    enter_room(RoomJID, Bob, BobNick),
    Res = set_user_role(RoomJID, BobNick, visitor, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_try_set_nonexistent_room_user_role(Config) ->
    Res = set_user_role(?NONEXISTENT_ROOM, <<"Alice">>, moderator, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_make_user_enter_room(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun admin_make_user_enter_room/2).

admin_make_user_enter_room(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    JID = jid:from_binary(escalus_client:full_jid(Alice)),
    % Alice enter room with password
    Res = enter_room(RoomJID, Alice, Nick, null, Config),
    assert_success(?ENTER_ROOM_PATH, Res),
    ?assertMatch([#{nick := Nick, jid := JID}], get_room_users(RoomJID)).

admin_make_user_enter_room_with_password(Config) ->
    muc_helper:story_with_room(Config, [{password_protected, true}, {password, ?PASSWORD}],
                               [{alice, 1}, {bob, 1}],
                               fun admin_make_user_enter_room_with_password/3).

admin_make_user_enter_room_with_password(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    JID = jid:from_binary(escalus_client:full_jid(Alice)),
    % Alice enter room with password
    Res = enter_room(RoomJID, Alice, Nick, ?PASSWORD, Config),
    assert_success(?ENTER_ROOM_PATH, Res),
    ?assertMatch([#{nick := Nick, jid := JID}], get_room_users(RoomJID)),
    % Bob try enter room without password
    Res1 = enter_room(RoomJID, Bob, <<"Bobek">>, null, Config),
    assert_success(?ENTER_ROOM_PATH, Res1),
    ?assertMatch([_], get_room_users(RoomJID)),
    % Bob enter room with password
    Res2 = enter_room(RoomJID, Bob, <<"Bobek">>, ?PASSWORD, Config),
    assert_success(?ENTER_ROOM_PATH, Res2),
    ?assertMatch([_, _], get_room_users(RoomJID)).

admin_make_user_enter_room_bare_jid(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun admin_make_user_enter_room_bare_jid/2).

admin_make_user_enter_room_bare_jid(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BareAlice = escalus_client:short_jid(Alice),
    Res = enter_room(RoomJID, BareAlice, <<"Ali">>, null, Config),
    assert_no_full_jid(Res).

admin_make_user_exit_room(Config) ->
    muc_helper:story_with_room(Config, [{persistent, true}], [{alice, 1}],
                               fun admin_make_user_exit_room/2).

admin_make_user_exit_room(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    enter_room(RoomJID, Alice, Nick),
    ?assertMatch([_], get_room_users(RoomJID)),
    Res = exit_room(RoomJID, Alice, Nick, Config),
    assert_success(?EXIT_ROOM_PATH, Res),
    ?assertMatch([], get_room_users(RoomJID)).

admin_make_user_exit_room_bare_jid(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun admin_make_user_exit_room_bare_jid/2).

admin_make_user_exit_room_bare_jid(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BareAlice = escalus_client:short_jid(Alice),
    Nick = <<"ali">>,
    enter_room(RoomJID, Alice, Nick),
    Res = exit_room(RoomJID, BareAlice, Nick, Config),
    assert_no_full_jid(Res).

admin_list_room_affiliations(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun admin_list_room_affiliations/3).

admin_list_room_affiliations(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Alice, AliceNick),
    AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
    BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    % List all owners
    Res = list_room_affiliations(RoomJID, owner, Config),
    ?assertMatch([#{<<"jid">> := AliceJID, <<"affiliation">> := <<"OWNER">>}],
                 get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res)),
    % List all members
    set_user_affiliation(RoomJID, Bob, member, Config),
    Res1 = list_room_affiliations(RoomJID, member, Config),
    ?assertMatch([#{<<"jid">> := BobJID, <<"affiliation">> := <<"MEMBER">>}],
                 get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res1)),
    % List all
    Res2 = list_room_affiliations(RoomJID, null, Config),
    ?assertMatch([_, _], get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res2)).

admin_try_list_nonexistent_room_affiliations(Config) ->
    Res = list_room_affiliations(?NONEXISTENT_ROOM, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

%% Admin MUC not configured test cases

admin_delete_room_muc_not_configured(Config) ->
    Res = delete_room(get_room_name(), null, Config),
    get_not_loaded(Res).

admin_list_room_users_muc_not_configured(Config) ->
    Res = list_room_users(get_room_name(), Config),
    get_not_loaded(Res).

admin_change_room_config_muc_not_configured(Config) ->
    RoomConfig = #{title => <<"NewTitle">>},
    Res = change_room_config(get_room_name(), RoomConfig, Config),
    get_not_loaded(Res).

admin_get_room_config_muc_not_configured(Config) ->
    Res = get_room_config(get_room_name(), Config),
    get_not_loaded(Res).

admin_invite_user_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
        fun admin_invite_user_muc_not_configured_story/3).

admin_invite_user_muc_not_configured_story(Config, Alice, Bob) ->
    Res = invite_user(get_room_name(), Alice, Bob, null, Config),
    get_not_loaded(Res).

admin_kick_user_muc_not_configured(Config) ->
    Res = kick_user(get_room_name(), <<"nick">>, <<"reason">>, Config),
    get_not_loaded(Res).

admin_send_message_to_room_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun admin_send_message_to_room_muc_not_configured_story/2).

admin_send_message_to_room_muc_not_configured_story(Config, Alice) ->
    Res = send_message_to_room(get_room_name(), Alice, <<"body">>, Config),
    get_not_loaded(Res).

admin_send_private_message_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun admin_send_private_message_muc_not_configured_story/2).

admin_send_private_message_muc_not_configured_story(Config, Alice) ->
    Nick = <<"ali">>,
    Res = send_private_message(get_room_name(), Alice, Nick, <<"body">>, Config),
    get_not_loaded(Res).

admin_get_room_messages_muc_or_mam_not_configured(Config) ->
    Res = get_room_messages(get_room_name(), 4, null, Config),
    get_not_loaded(Res).

admin_set_user_affiliation_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun admin_set_user_affiliation_muc_not_configured_story/2).

admin_set_user_affiliation_muc_not_configured_story(Config, Alice) ->
    Res = set_user_affiliation(get_room_name(), Alice, member, Config),
    get_not_loaded(Res).

admin_set_user_role_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun admin_set_user_role_muc_not_configured_story/2).

admin_set_user_role_muc_not_configured_story(Config, Alice) ->
    Res = set_user_role(get_room_name(), Alice, moderator, Config),
    get_not_loaded(Res).

admin_make_user_enter_room_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun admin_make_user_enter_room_muc_light_not_configured_story/2).

admin_make_user_enter_room_muc_light_not_configured_story(Config, Alice) ->
    Nick = <<"ali">>,
    Res = enter_room(get_room_name(), Alice, Nick, null, Config),
    get_not_loaded(Res).

admin_make_user_exit_room_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun admin_make_user_exit_room_muc_not_configured_story/2).

admin_make_user_exit_room_muc_not_configured_story(Config, Alice) ->
    Nick = <<"ali">>,
    Res = exit_room(get_room_name(), Alice, Nick, Config),
    get_not_loaded(Res).

admin_list_room_affiliations_muc_not_configured(Config) ->
    Res = list_room_affiliations(get_room_name(), member, Config),
    get_not_loaded(Res).

%% Domain admin test cases

domain_admin_try_delete_room_with_nonexistent_domain(Config) ->
    RoomJID = jid:make_bare(<<"unknown">>, <<"unknown">>),
    get_unauthorized(delete_room(RoomJID, null, Config)).

domain_admin_create_and_delete_room_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_create_and_delete_room_no_permission_story/2).

domain_admin_create_and_delete_room_no_permission_story(Config, AliceBis) ->
    ExternalDomain = domain_helper:secondary_domain(),
    UnknownDomain = <<"unknown">>,
    RoomJid = jid:make_bare(rand_name(), muc_helper:muc_host()),
    ExternalServer = <<"muc.", ExternalDomain/binary>>,
    % Create instant room with a non-existent domain
    UnknownJID = <<(rand_name())/binary, "@", UnknownDomain/binary>>,
    Res = create_instant_room(RoomJid, UnknownJID, <<"Ali">>, Config),
    get_unauthorized(Res),
    % Create instant room with an external domain
    Res2 = create_instant_room(RoomJid, AliceBis, <<"Ali">>, Config),
    get_unauthorized(Res2),
    % Delete instant room with a non-existent domain
    UnknownRoomJID = jid:make_bare(<<"unknown_room">>, UnknownDomain),
    Res3 = delete_room(UnknownRoomJID, null, Config),
    get_unauthorized(Res3),
    % Delete instant room with an external domain
    ExternalRoomJID = jid:make_bare(<<"external_room">>, ExternalServer),
    Res4 = delete_room(ExternalRoomJID, null, Config),
    get_unauthorized(Res4).

domain_admin_list_rooms_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_list_rooms_no_permission_story/2).

domain_admin_list_rooms_no_permission_story(Config, AliceBis) ->
    AliceBisJID = jid:from_binary(escalus_client:short_jid(AliceBis)),
    AliceBisRoom = rand_name(),
    muc_helper:create_instant_room(AliceBisRoom, AliceBisJID, <<"Ali">>, []),
    Res = list_rooms(muc_helper:muc_host(), AliceBis, null, null, Config),
    get_unauthorized(Res).

domain_admin_list_room_users_no_permission(Config) ->
    get_unauthorized(list_room_users(?NONEXISTENT_ROOM, Config)),
    get_unauthorized(list_room_users(?EXTERNAL_DOMAIN_ROOM, Config)).

domain_admin_change_room_config_no_permission(Config) ->
    RoomConfig = #{title => <<"NewTitle">>},
    get_unauthorized(change_room_config(?NONEXISTENT_ROOM, RoomConfig, Config)),
    get_unauthorized(change_room_config(?EXTERNAL_DOMAIN_ROOM, RoomConfig, Config)).

domain_admin_get_room_config_no_permission(Config) ->
    get_unauthorized(get_room_config(?NONEXISTENT_ROOM, Config)),
    get_unauthorized(get_room_config(?EXTERNAL_DOMAIN_ROOM, Config)).

domain_admin_invite_user_no_permission(Config) ->
    muc_helper:story_with_room(Config, [], [{alice_bis, 1}, {bob, 1}],
                               fun domain_admin_invite_user_no_permission_story/3).

domain_admin_invite_user_no_permission_story(Config, Alice, Bob) ->
    RoomJIDBin = ?config(room_jid, Config),
    RoomJID = jid:from_binary(RoomJIDBin),
    Res = invite_user(RoomJID, Alice, Bob, null, Config),
    get_unauthorized(Res).

domain_admin_kick_user_no_permission(Config) ->
    get_unauthorized(kick_user(?NONEXISTENT_ROOM, <<"ali">>, null, Config)),
    get_unauthorized(kick_user(?EXTERNAL_DOMAIN_ROOM, <<"ali">>, null, Config)).

domain_admin_send_message_to_room_no_permission(Config) ->
    muc_helper:story_with_room(Config, [], [{alice_bis, 1}],
                                fun domain_admin_send_message_to_room_no_permission_story/2).

domain_admin_send_message_to_room_no_permission_story(Config, AliceBis) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Message = <<"Hello All!">>,
    AliceNick = <<"Bobek">>,
    enter_room(RoomJID, AliceBis, AliceNick),
    escalus:wait_for_stanza(AliceBis),
    % Send message
    Res = send_message_to_room(RoomJID, AliceBis, Message, Config),
    get_unauthorized(Res).

domain_admin_send_private_message_no_permission(Config) ->
    muc_helper:story_with_room(Config, [], [{alice_bis, 1}, {bob, 1}],
                               fun domain_admin_send_private_message_no_permission_story/3).

domain_admin_send_private_message_no_permission_story(Config, AliceBis, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Message = <<"Hello Bob!">>,
    BobNick = <<"Bobek">>,
    AliceBisNick = <<"Ali">>,
    enter_room(RoomJID, AliceBis, AliceBisNick),
    enter_room(RoomJID, Bob, BobNick),
    escalus:wait_for_stanzas(Bob, 2),
    % Send message
    Res = send_private_message(RoomJID, AliceBis, BobNick, Message, Config),
    get_unauthorized(Res).

domain_admin_get_room_messages_no_permission(Config) ->
    get_unauthorized(get_room_messages(?NONEXISTENT_ROOM, null, null, Config)),
    get_unauthorized(get_room_messages(?EXTERNAL_DOMAIN_ROOM, null, null, Config)).

domain_admin_set_user_affiliation_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun domain_admin_set_user_affiliation_no_permission_story/2).

domain_admin_set_user_affiliation_no_permission_story(Config, Alice) ->
    get_unauthorized(set_user_affiliation(?NONEXISTENT_ROOM, Alice, admin, Config)),
    get_unauthorized(set_user_affiliation(?EXTERNAL_DOMAIN_ROOM, Alice, admin, Config)).

domain_admin_set_user_role_no_permission(Config) ->
    get_unauthorized(set_user_role(?NONEXISTENT_ROOM, <<"Alice">>, moderator, Config)),
    get_unauthorized(set_user_role(?EXTERNAL_DOMAIN_ROOM, <<"Alice">>, moderator, Config)).

domain_admin_list_room_affiliations_no_permission(Config) ->
    get_unauthorized(list_room_affiliations(?NONEXISTENT_ROOM, null, Config)),
    get_unauthorized(list_room_affiliations(?EXTERNAL_DOMAIN_ROOM, null, Config)).

domain_admin_make_user_enter_room_no_permission(Config) ->
    muc_helper:story_with_room(Config, [], [{alice_bis, 1}],
                               fun domain_admin_make_user_enter_room_no_permission_story/2).

domain_admin_make_user_enter_room_no_permission_story(Config, AliceBis) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    % Enter room without password
    Res = enter_room(RoomJID, AliceBis, Nick, null, Config),
    get_unauthorized(Res),
    % Enter room with password
    Res2 = enter_room(RoomJID, AliceBis, Nick, ?PASSWORD, Config),
    get_unauthorized(Res2).

domain_admin_make_user_exit_room_no_permission(Config) ->
    muc_helper:story_with_room(Config, [{persistent, true}], [{alice_bis, 1}],
                               fun domain_admin_make_user_exit_room_no_permission_story/2).

domain_admin_make_user_exit_room_no_permission_story(Config, AliceBis) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    enter_room(RoomJID, AliceBis, Nick),
    ?assertMatch([_], get_room_users(RoomJID)),
    Res = exit_room(RoomJID, AliceBis, Nick, Config),
    get_unauthorized(Res).

%% User test cases

user_list_rooms(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun user_list_rooms_story/3).

user_list_rooms_story(Config, Alice, Bob) ->
    AliceJID = jid:from_binary(escalus_client:short_jid(Alice)),
    BobJID = jid:from_binary(escalus_client:short_jid(Bob)),
    AliceRoom = rand_name(),
    BobRoom = rand_name(),
    muc_helper:create_instant_room(AliceRoom, AliceJID, <<"Ali">>, []),
    muc_helper:create_instant_room(BobRoom, BobJID, <<"Bob">>, []),

    Res = user_list_rooms(Alice, muc_helper:muc_host(), null, null, Config),
    #{<<"rooms">> := Rooms } = get_ok_value(?LIST_ROOMS_PATH, Res),
    ?assert(contain_room(AliceRoom, Rooms)),
    ?assert(contain_room(BobRoom, Rooms)).

user_try_list_rooms_for_nonexistent_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_list_rooms_for_nonexistent_domain_story/2).

user_try_list_rooms_for_nonexistent_domain_story(Config, Alice) ->
    Res1 = user_list_rooms(Alice, <<"baddomain">>, null, null, Config),
    ?assertMatch({_, _}, binary:match(get_err_msg(Res1), <<"not found">>)),
    %% Domain instead of the MUC subdomain
    Res2 = user_list_rooms(Alice, domain_helper:domain(), null, null, Config),
    ?assertMatch({_, _}, binary:match(get_err_msg(Res2), <<"not found">>)).

user_create_and_delete_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_create_and_delete_room_story/2).

user_create_and_delete_room_story(Config, Alice) ->
    Name = rand_name(),
    MUCServer = muc_helper:muc_host(),
    RoomJID = jid:make_bare(Name, MUCServer),
    % Create instant room
    Res = user_create_instant_room(Alice, RoomJID, <<"Ali">>, Config),
    ?assertMatch(#{<<"title">> := Name, <<"private">> := false, <<"usersNumber">> := 0},
                 get_ok_value(?CREATE_INSTANT_ROOM_PATH, Res)),
    Res2 = user_list_rooms(Alice, MUCServer, null, null, Config),
    ?assert(contain_room(Name, get_ok_value(?LIST_ROOMS_PATH, Res2))),
    % Delete room
    Res3 = user_delete_room(Alice, RoomJID, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?DELETE_ROOM_PATH, Res3),
                                          <<"successfully">>)),
    Res4 = user_list_rooms(Alice, MUCServer, null, null, Config),
    ?assertNot(contain_room(Name, get_ok_value(?LIST_ROOMS_PATH, Res4))).

user_create_room_with_unprepped_name(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_create_room_with_unprepped_name_story/2).

user_create_room_with_unprepped_name_story(Config, Alice) ->
    Name = <<$a, (rand_name())/binary>>, % make it start with a letter
    MUCServer = muc_helper:muc_host(),
    RoomJid = jid:make_noprep(unprep(Name), unprep(MUCServer), <<>>),
    Res = user_create_instant_room(Alice, RoomJid, <<"Ali">>, Config),
    ?assertMatch(#{<<"title">> := Name, <<"private">> := false, <<"usersNumber">> := 0},
                 get_ok_value(?CREATE_INSTANT_ROOM_PATH, Res)).

user_try_create_instant_room_with_nonexistent_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_create_instant_room_with_nonexistent_domain_story/2).

user_try_create_instant_room_with_nonexistent_domain_story(Config, Alice) ->
    RoomJID = jid:make_bare(rand_name(), <<"unknown">>),
    Res = user_create_instant_room(Alice, RoomJID, <<"Ali">>, Config),
    ?assertEqual(<<"Error while creating a room">>, get_err_msg(Res)).

user_try_create_instant_room_with_nonexistent_subdomain(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_create_instant_room_with_nonexistent_subdomain_story/2).

user_try_create_instant_room_with_nonexistent_subdomain_story(Config, Alice) ->
    TopDomain = escalus_client:server(Alice),
    RoomJID = jid:make_bare(rand_name(), <<"unknown_muc.", TopDomain/binary>>),
    Res = user_create_instant_room(Alice, RoomJID, <<"Ali">>, Config),
    ?assertEqual(<<"Could not create room due to incorrect domain">>, get_err_msg(Res)).

user_try_create_instant_room_with_invalid_args(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_create_instant_room_with_invalid_args_story/2).

user_try_create_instant_room_with_invalid_args_story(Config, Alice) ->
    Domain = muc_helper:muc_host(),
    Res1 = user_create_instant_room(Alice, <<"test room@", Domain/binary>>, <<"Ali">>, Config),
    assert_coercion_err(Res1, <<"failed_to_parse_jid">>),
    Res2 = user_create_instant_room(Alice, <<"testroom@", Domain/binary, "/res1">>, <<"Ali">>, Config),
    assert_coercion_err(Res2, <<"jid_with_resource">>),
    Res3 = user_create_instant_room(Alice, <<"testroom@", Domain/binary>>, <<>>, Config),
    assert_coercion_err(Res3, <<"empty_resource_name">>).

user_try_delete_nonexistent_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_delete_nonexistent_room_story/2).

user_try_delete_nonexistent_room_story(Config, Alice) ->
    RoomJID = jid:make_bare(<<"unknown">>, muc_helper:muc_host()),
    Res = user_delete_room(Alice, RoomJID, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"non-existent">>)).

user_try_delete_room_by_not_owner(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_try_delete_room_by_not_owner_story/3).

user_try_delete_room_by_not_owner_story(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = user_delete_room(Bob, RoomJID, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not have permission">>)).

user_invite_user(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}], fun user_invite_user_story/3).

user_invite_user_story(Config, Alice, Bob) ->
    RoomJIDBin = ?config(room_jid, Config),
    RoomJID = jid:from_binary(RoomJIDBin),
    Res = user_invite_user(Alice, RoomJID, Bob, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?INVITE_USER_PATH, Res),
                                          <<"successfully">>)),
    Stanza = escalus:wait_for_stanza(Bob),
    escalus:assert(is_message, Stanza),
    ?assertEqual(RoomJIDBin,
                 exml_query:path(Stanza, [{element, <<"x">>}, {attr, <<"jid">>}])).

user_kick_user(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}], fun user_kick_user_story/3).

user_kick_user_story(Config, Alice, Bob) ->
    RoomJIDBin = ?config(room_jid, Config),
    RoomJID = jid:from_binary(RoomJIDBin),
    BobNick = <<"Bobek">>,
    Reason = <<"You are too loud">>,
    enter_room(RoomJID, Alice, <<"ali">>),
    enter_room(RoomJID, Bob, BobNick),
    Res = user_kick_user(Alice, RoomJID, BobNick, Reason, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?KICK_USER_PATH, Res),
                                          <<"successfully">>)),
    escalus:wait_for_stanzas(Bob, 2),
    KickStanza = escalus:wait_for_stanza(Bob),
    escalus:assert(is_presence_with_type, [<<"unavailable">>], KickStanza),
    ?assertEqual(Reason,
                 exml_query:path(KickStanza, [{element, <<"x">>}, {element, <<"item">>},
                                              {element, <<"reason">>}, cdata])).

user_try_kick_user_without_moderator_resource(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_try_kick_user_without_moderator_resource/3).

user_try_kick_user_without_moderator_resource(Config, Alice, Bob) ->
    RoomJIDBin = ?config(room_jid, Config),
    RoomJID = jid:from_binary(RoomJIDBin),
    BobNick = <<"Bobek">>,
    enter_room(RoomJID, Bob, BobNick),
    Res = user_kick_user(Alice, RoomJID, BobNick, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_try_kick_user_from_nonexistent_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_kick_user_from_nonexistent_room/2).

user_try_kick_user_from_nonexistent_room(Config, Alice) ->
    Res = user_kick_user(Alice, ?NONEXISTENT_ROOM, <<"bobi">>, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_send_message_to_room(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_send_message_to_room_story/3).

user_send_message_to_room_story(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Message = <<"Hello All!">>,
    BobNick = <<"Bobek">>,
    enter_room(RoomJID, Bob, BobNick),
    escalus:wait_for_stanza(Bob),
    % Send message
    Res = user_send_message_to_room(Bob, RoomJID, Message, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SEND_MESSAGE_PATH, Res),
                                          <<"successfully">>)),
    assert_is_message_correct(RoomJID, BobNick, <<"groupchat">>, Message,
                              escalus:wait_for_stanza(Bob)).

user_send_message_to_room_with_specified_res(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 2}],
                               fun user_send_message_to_room_with_specified_res_story/4).

user_send_message_to_room_with_specified_res_story(Config, _Alice, Bob, Bob2) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Message = <<"Hello All!">>,
    BobNick = <<"Bobek">>,
    enter_room(RoomJID, Bob2, BobNick),
    escalus:wait_for_stanza(Bob2),
    % Send message, the resource should be normalized to "res2"
    Res = user_send_message_to_room(Bob, RoomJID, Message, <<"res₂"/utf8>>, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SEND_MESSAGE_PATH, Res),
                                          <<"successfully">>)),
    assert_is_message_correct(RoomJID, BobNick, <<"groupchat">>, Message,
                              escalus:wait_for_stanza(Bob2)).

user_send_private_message(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_send_private_message/3).

user_send_private_message(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Message = <<"Hello Bob!">>,
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Alice, AliceNick),
    escalus:wait_for_stanzas(Bob, 2),
    % Send message
    Res = user_send_private_message(Alice, RoomJID, Message, BobNick, null, Config),
    assert_success(?SEND_PRIV_MESG_PATH, Res),
    assert_is_message_correct(RoomJID, AliceNick, <<"chat">>, Message,
                              escalus:wait_for_stanza(Bob)).

user_send_private_message_with_specified_res(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 2}, {bob, 1}],
                               fun user_send_private_message/3).

user_send_private_message_with_specified_res(Config, Alice, Alice2, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Message = <<"Hello Bob!">>,
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Alice2, AliceNick),
    escalus:wait_for_stanzas(Bob, 2),
    % Send message, the resource should be normalized to "res2"
    Res = user_send_private_message(Alice, RoomJID, Message, BobNick, <<"res₂"/utf8>>, Config),
    assert_success(?SEND_PRIV_MESG_PATH, Res),
    assert_is_message_correct(RoomJID, AliceNick, <<"chat">>, Message,
                              escalus:wait_for_stanza(Bob)).

user_without_session_send_message_to_room(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}],
                               fun user_without_session_send_message_to_room_story/2).

user_without_session_send_message_to_room_story(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    JID = jid:from_binary(escalus_client:full_jid(Alice)),
    terminate_user_session(JID),
    % Send message
    Res = user_send_message_to_room(Alice, RoomJID, <<"Hello!">>, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not have any session">>)).

terminate_user_session(Jid) ->
    ?assertEqual(ok, rpc(mim(), ejabberd_sm, terminate_session, [Jid, <<"Kicked">>])),
    F = fun() -> rpc(mim(), ejabberd_sm, get_user_resources, [Jid]) end,
    wait_helper:wait_until(F, [], #{time_left => timer:seconds(5)}).

user_get_room_config(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_get_room_config_story/3).

user_get_room_config_story(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = user_get_room_config(Alice, RoomJID, Config),
    assert_default_room_config(Res),
    % Not an owner tries to get room config
    Res2 = user_get_room_config(Bob, RoomJID, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not have permission">>)).

user_try_get_nonexistent_room_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_get_nonexistent_room_config_story/2).

user_try_get_nonexistent_room_config_story(Config, Alice) ->
    Res = user_get_room_config(Alice, ?NONEXISTENT_ROOM, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_change_room_config(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_change_room_config_story/3).

user_change_room_config_story(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Title = <<"aloes">>,
    Description = <<"The chat about aloes">>,
    Public = false,
    RoomConfig = #{title => Title, description => Description, public => Public},
    Res = user_change_room_config(Alice, RoomJID, RoomConfig, Config),
    ?assertMatch(#{<<"title">> := Title,
                   <<"description">> := Description,
                   <<"public">> := Public}, get_ok_value(?CHANGE_ROOM_CONFIG_PATH, Res)),
    % Not an owner tries to change the room config
    Res2 = user_change_room_config(Bob, RoomJID, RoomConfig, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not have permission">>)).

user_try_change_nonexistent_room_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_change_nonexistent_room_config_story/2).

user_try_change_nonexistent_room_config_story(Config, Alice) ->
    RoomConfig = #{title => <<"NewTitle">>},
    Res = user_change_room_config(Alice, ?NONEXISTENT_ROOM, RoomConfig, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_list_room_users(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_list_room_users_story/3).

user_list_room_users_story(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Alice, AliceNick),
    Res = user_list_room_users(Alice, RoomJID, Config),
    ExpectedUsers = [{null, BobNick, <<"PARTICIPANT">>},
                     {null, AliceNick, <<"MODERATOR">>}],
    assert_room_users(ExpectedUsers, get_ok_value(?LIST_ROOM_USERS_PATH, Res)).

user_list_room_users_without_anonymous_mode(Config) ->
    muc_helper:story_with_room(Config, [{anonymous, false}], [{alice, 1}, {bob, 1}],
                               fun user_list_room_users_without_anonymous_mode_story/3).

user_list_room_users_without_anonymous_mode_story(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Alice, AliceNick),
    Res = user_list_room_users(Alice, RoomJID, Config),
    ExpectedUsers = [{escalus_client:full_jid(Bob), BobNick, <<"PARTICIPANT">>},
                     {escalus_client:full_jid(Alice), AliceNick, <<"MODERATOR">>}],
    assert_room_users(ExpectedUsers, get_ok_value(?LIST_ROOM_USERS_PATH, Res)).

user_try_list_nonexistent_room_users(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_list_nonexistent_room_users_story/2).

user_try_list_nonexistent_room_users_story(Config, Alice) ->
    Res = user_list_room_users(Alice, ?NONEXISTENT_ROOM, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_try_list_room_users_without_permission(Config) ->
    muc_helper:story_with_room(Config, [{members_only, true}], [{alice, 1}, {bob, 1}],
                               fun user_try_list_room_users_without_permission_story/3).

user_try_list_room_users_without_permission_story(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = user_list_room_users(Bob, RoomJID, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not have permission">>)).

user_get_room_messages(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_get_room_messages_story/3).

user_get_room_messages_story(Config, Alice, Bob) ->
    RoomJID = #jid{luser = RoomName, lserver = MUCDomain} =
        jid:from_binary(?config(room_jid, Config)),
    enter_room(RoomJID, Bob, <<"Bobek">>),
    enter_room(RoomJID, Alice, <<"Ali">>),
    escalus:wait_for_stanzas(Bob, 2),
    user_send_message_to_room(Bob, RoomJID, <<"Hi!">>, null, Config),
    escalus:wait_for_stanzas(Bob, 1),
    mam_helper:wait_for_room_archive_size(MUCDomain, RoomName, 1),
    Res = user_get_room_messages(Alice, RoomJID, 50, null, Config),
    #{<<"stanzas">> := [#{<<"stanza">> := StanzaXML}], <<"limit">> := 50} =
        get_ok_value(?GET_MESSAGES_PATH, Res),
    ?assertMatch({ok, #xmlel{name = <<"message">>}}, exml:parse(StanzaXML)).

user_shouldnt_store_messages_in_muc_light(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_shouldnt_store_messages_in_muc_light_story/2).

user_shouldnt_store_messages_in_muc_light_story(Config, Alice) ->
    %% Create a MUC Light room
    MUCServer = ?config(muc_light_host, Config),
    AliceBin = escalus_client:short_jid(Alice),
    {ok, #{jid := RoomJID}} =
        create_muc_light_room(MUCServer, <<"first room">>, <<"subject">>, AliceBin),
    %% Send a message
    Message = <<"Hello friends">>,
    send_message_to_muc_light_room(RoomJID, jid:from_binary(AliceBin), Message),
    escalus:wait_for_stanza(Alice),
    %% Try to get a MUC Light message
    Limit = 1,
    Res = user_get_muc_light_room_messages(Alice, jid:to_binary(RoomJID), Limit, null, Config),
    #{<<"stanzas">> := [], <<"limit">> := Limit} =
        get_ok_value([data, muc_light, getRoomMessages], Res).

user_try_get_nonexistent_room_messages(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_get_nonexistent_room_messages_story/2).

user_try_get_nonexistent_room_messages_story(Config, Alice) ->
    % Non-existent room with non-existent domain
    Res = user_get_room_messages(Alice, ?NONEXISTENT_ROOM, null, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)),
    % Non-existent room with existent domain
    Res2 = user_get_room_messages(Alice, ?NONEXISTENT_ROOM2, null, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)).

user_try_get_room_messages_without_permission(Config) ->
    muc_helper:story_with_room(Config, [{members_only, true}], [{alice, 1}, {bob, 1}],
                               fun user_try_get_room_messages_without_permission/3).

user_try_get_room_messages_without_permission(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = user_get_room_messages(Bob, RoomJID, null, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not have permission">>)).

user_owner_set_user_affiliation(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_owner_set_user_affiliation/3).

user_owner_set_user_affiliation(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    % Grant a member affiliation
    Res = user_set_user_affiliation(Alice, RoomJID, Bob, member, Config),
    assert_success(?SET_AFFILIATION_PATH, Res),
    assert_user_affiliation(RoomJID, Bob, member),
    % Grant a member affiliation
    Res1 = user_set_user_affiliation(Alice, RoomJID, Bob, admin, Config),
    assert_success(?SET_AFFILIATION_PATH, Res1),
    assert_user_affiliation(RoomJID, Bob, admin),
    % Grant a owner affiliation
    Res2 = user_set_user_affiliation(Alice, RoomJID, Bob, owner, Config),
    assert_success(?SET_AFFILIATION_PATH, Res2),
    assert_user_affiliation(RoomJID, Bob, owner),
    % Revoke affiliation
    Res3 = user_set_user_affiliation(Alice, RoomJID, Bob, none, Config),
    assert_success(?SET_AFFILIATION_PATH, Res3),
    assert_user_affiliation(RoomJID, Bob, none),
    % Ban user
    Res4 = user_set_user_affiliation(Alice, RoomJID, Bob, outcast, Config),
    assert_success(?SET_AFFILIATION_PATH, Res4),
    assert_user_affiliation(RoomJID, Bob, outcast).


user_admin_set_user_affiliation(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}, {kate, 1}],
                               fun user_admin_set_user_affiliation/4).

user_admin_set_user_affiliation(Config, Alice, Bob, Kate) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    user_set_user_affiliation(Alice, RoomJID, Bob, admin, Config),
    % Grant member affiliation
    Res = user_set_user_affiliation(Bob, RoomJID, Kate, member, Config),
    assert_success(?SET_AFFILIATION_PATH, Res),
    assert_user_affiliation(RoomJID, Kate, member),
    % Revoke affiliation
    Res1 = user_set_user_affiliation(Bob, RoomJID, Kate, none, Config),
    assert_success(?SET_AFFILIATION_PATH, Res1),
    assert_user_affiliation(RoomJID, Kate, none),
    % Admin cannot grant admin affiliation
    Res2 = user_set_user_affiliation(Bob, RoomJID, Kate, admin, Config),
    assert_no_permission(Res2),
    % Admin cannot grant owner affiliation
    Res3 = user_set_user_affiliation(Bob, RoomJID, Kate, owner, Config),
    assert_no_permission(Res3),
    % Admin can ban member
    Res4 = user_set_user_affiliation(Bob, RoomJID, Kate, outcast, Config),
    assert_success(?SET_AFFILIATION_PATH, Res4),
    assert_user_affiliation(RoomJID, Kate, outcast),
    % Admin cannot ban owner
    Res5 = user_set_user_affiliation(Bob, RoomJID, Alice, outcast, Config),
    assert_no_permission(Res5).

user_member_set_user_affiliation(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}, {kate, 1}],
                               fun user_member_set_user_affiliation/4).

user_member_set_user_affiliation(Config, Alice, Bob, Kate) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    user_set_user_affiliation(Alice, RoomJID, Bob, member, Config),
    % Member cannot grant member affiliation
    Res = user_set_user_affiliation(Bob, RoomJID, Kate, member, Config),
    assert_no_permission(Res),
    % Member cannot grant member admin affiliation
    Res1 = user_set_user_affiliation(Bob, RoomJID, Kate, admin, Config),
    assert_no_permission(Res1),
    % Member cannot grant member owner affiliation
    Res2 = user_set_user_affiliation(Bob, RoomJID, Kate, owner, Config),
    assert_no_permission(Res2),
    % Member cannot revoke member affiliation
    user_set_user_affiliation(Alice, RoomJID, Kate, member, Config),
    Res3 = user_set_user_affiliation(Bob, RoomJID, Kate, none, Config),
    assert_no_permission(Res3).

user_try_set_nonexistent_room_affiliation(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_set_nonexistent_room_affiliation/2).

user_try_set_nonexistent_room_affiliation(Config, Alice) ->
    Res = user_set_user_affiliation(Alice, ?NONEXISTENT_ROOM, Alice, none, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_moderator_set_user_role(Config) ->
    muc_helper:story_with_room(Config, [{anonymous, false}, {persistent, true}],
                               [{alice, 1}, {bob, 1}],
                               fun user_moderator_set_user_role/3).

user_moderator_set_user_role(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Boobek">>,
    enter_room(RoomJID, Alice, escalus_client:username(Alice)),
    enter_room(RoomJID, Bob, BobNick),
    % Change from participant to visitor
    Res = user_set_user_role(Alice, RoomJID, BobNick, visitor, Config),
    assert_success(?SET_ROLE_PATH, Res),
    assert_user_role(RoomJID, Bob, visitor),
    % Change from visitor to participant
    Res1 = user_set_user_role(Alice, RoomJID, BobNick, participant, Config),
    assert_success(?SET_ROLE_PATH, Res1),
    assert_user_role(RoomJID, Bob, participant),
    % Change from participant to moderator
    Res2 = user_set_user_role(Alice, RoomJID, BobNick, moderator, Config),
    assert_success(?SET_ROLE_PATH, Res2),
    assert_user_role(RoomJID, Bob, moderator).

user_participant_set_user_role(Config) ->
    muc_helper:story_with_room(Config, [{anonymous, false}, {persistent, true}],
                               [{alice, 1}, {bob, 1}, {kate, 1}],
                               fun user_participant_set_user_role/4).

user_participant_set_user_role(Config, _Alice, Bob, Kate) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Boobek">>,
    KateNick = <<"Katek">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Kate, KateNick),
    % Try change from participant to visitor
    Res = user_set_user_role(Bob, RoomJID, KateNick, visitor, Config),
    assert_no_permission(Res),
    % Change from participant to participant with success response
    Res1 = user_set_user_role(Bob, RoomJID, KateNick, participant, Config),
    assert_success(?SET_ROLE_PATH, Res1),
    assert_user_role(RoomJID, Bob, participant),
    % Try change from participant to moderator
    Res2 = user_set_user_role(Bob, RoomJID, KateNick, moderator, Config),
    assert_no_permission(Res2).

user_try_set_nonexistent_room_role(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_set_nonexistent_room_role/2).

user_try_set_nonexistent_room_role(Config, Alice) ->
    Res = user_set_user_role(Alice, ?NONEXISTENT_ROOM, <<"Ali">>, participant, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

user_can_enter_room(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}], fun user_can_enter_room/2).

user_can_enter_room(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    JID = jid:from_binary(escalus_utils:jid_to_lower(escalus_client:full_jid(Alice))),
    % Resource should be normalized to "res1", which is Alice's connected resource
    Res = user_enter_room(Alice, RoomJID, Nick, <<"res₁"/utf8>>, null, Config),
    assert_success(?ENTER_ROOM_PATH, Res),
    ?assertMatch([#{nick := Nick, jid := JID}], get_room_users(RoomJID)).

user_cannot_enter_room_with_invalid_resource(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}],
                               fun user_cannot_enter_room_with_invalid_resource/2).

user_cannot_enter_room_with_invalid_resource(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    Res1 = user_enter_room(Alice, RoomJID, Nick, <<"\n">>, null, Config),
    assert_coercion_err(Res1, <<"failed_to_parse_resource_name">>),
    Res2 = user_enter_room(Alice, RoomJID, Nick, <<>>, null, Config),
    assert_coercion_err(Res2, <<"empty_resource_name">>).

user_can_enter_room_with_password(Config) ->
    muc_helper:story_with_room(Config, [{password_protected, true}, {password, ?PASSWORD}],
                               [{alice, 1}, {bob, 1}],
                               fun user_can_enter_room_with_password/3).

user_can_enter_room_with_password(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    JID = jid:from_binary(escalus_utils:jid_to_lower(escalus_client:full_jid(Alice))),
    Resource = escalus_client:resource(Alice),
    % Alice enter room with password
    Res = user_enter_room(Alice, RoomJID, Nick, Resource, ?PASSWORD, Config),
    assert_success(?ENTER_ROOM_PATH, Res),
    ?assertMatch([#{nick := Nick, jid := JID}], get_room_users(RoomJID)),
    % Bob try enter room without password
    Res1 = user_enter_room(Bob, RoomJID, <<"Bobek">>, Resource, null, Config),
    assert_success(?ENTER_ROOM_PATH, Res1),
    ?assertMatch([_], get_room_users(RoomJID)),
    % Bob enter room with password
    Res2 = user_enter_room(Bob, RoomJID, <<"Bobek">>, Resource, ?PASSWORD, Config),
    assert_success(?ENTER_ROOM_PATH, Res2),
    ?assertMatch([_, _], get_room_users(RoomJID)).

user_can_exit_room(Config) ->
    muc_helper:story_with_room(Config, [{persistent, true}], [{alice, 1}],
                               fun user_can_exit_room/2).

user_can_exit_room(Config, Alice) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Nick = <<"ali">>,
    enter_room(RoomJID, Alice, Nick),
    ?assertMatch([_], get_room_users(RoomJID)),
    % Resource should be normalized to "res1", which is Alice's connected resource
    Res = user_exit_room(Alice, RoomJID, Nick, <<"res₁"/utf8>>, Config),
    assert_success(?EXIT_ROOM_PATH, Res),
    ?assertMatch([], get_room_users(RoomJID)).

user_list_room_affiliations(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}, {bob, 1}],
                               fun user_list_room_affiliations/3).

user_list_room_affiliations(Config, Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    BobNick = <<"Bobek">>,
    AliceNick = <<"Ali">>,
    enter_room(RoomJID, Bob, BobNick),
    enter_room(RoomJID, Alice, AliceNick),
    AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
    BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    % List all owners
    Res = user_list_room_affiliations(Alice, RoomJID, owner, Config),
    ?assertMatch([#{<<"jid">> := AliceJID, <<"affiliation">> := <<"OWNER">>}],
                 get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res)),
    % List all members
    user_set_user_affiliation(Alice, RoomJID, Bob, member, Config),
    Res1 = user_list_room_affiliations(Alice, RoomJID, member, Config),
    ?assertMatch([#{<<"jid">> := BobJID, <<"affiliation">> := <<"MEMBER">>}],
                 get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res1)),
    % List all
    Res2 = user_list_room_affiliations(Alice, RoomJID, null, Config),
    ?assertMatch([_, _], get_ok_value(?LIST_ROOM_AFFILIATIONS_PATH, Res2)).

user_try_list_room_affiliations_without_permission(Config) ->
    muc_helper:story_with_room(Config, [{members_only, true}], [{alice, 1}, {bob, 1}],
                               fun user_try_list_room_affiliations_without_permission/3).

user_try_list_room_affiliations_without_permission(Config, _Alice, Bob) ->
    RoomJID = jid:from_binary(?config(room_jid, Config)),
    Res = user_list_room_affiliations(Bob, RoomJID, null, Config),
    assert_no_permission(Res).

user_try_list_nonexistent_room_affiliations(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_try_list_nonexistent_room_affiliations/2).

user_try_list_nonexistent_room_affiliations(Config, Alice) ->
    Res = user_list_room_affiliations(Alice, ?NONEXISTENT_ROOM, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

%% User MUC not configured test cases

user_delete_room_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_delete_room_muc_not_configured_story/2).

user_delete_room_muc_not_configured_story(Config, Alice) ->
    Res = user_delete_room(Alice, get_room_name(), null, Config),
    get_not_loaded(Res).

user_list_room_users_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_list_room_users_muc_not_configured_story/2).

user_list_room_users_muc_not_configured_story(Config, Alice) ->
    Res = user_list_room_users(Alice, get_room_name(), Config),
    get_not_loaded(Res).

user_change_room_config_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_change_room_config_muc_not_configured_story/2).

user_change_room_config_muc_not_configured_story(Config, Alice) ->
    RoomConfig = #{title => <<"NewTitle">>},
    Res = user_change_room_config(Alice, get_room_name(), RoomConfig, Config),
    get_not_loaded(Res).

user_get_room_config_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_get_room_config_muc_not_configured_story/2).

user_get_room_config_muc_not_configured_story(Config, Alice) ->
    Res = user_get_room_config(Alice, get_room_name(), Config),
    get_not_loaded(Res).

user_invite_user_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
        fun user_invite_user_muc_not_configured_story/3).

user_invite_user_muc_not_configured_story(Config, Alice, Bob) ->
    Res = user_invite_user(Alice, get_room_name(), Bob, null, Config),
    get_not_loaded(Res).

user_kick_user_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_kick_user_muc_not_configured_story/2).

user_kick_user_muc_not_configured_story(Config, Alice) ->
    Res = user_kick_user(Alice, get_room_name(), <<"nick">>, <<"reason">>, Config),
    get_not_loaded(Res).

user_send_message_to_room_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_send_message_to_room_muc_not_configured_story/2).

user_send_message_to_room_muc_not_configured_story(Config, Alice) ->
    Res = user_send_message_to_room(Alice, get_room_name(), <<"Body">>, null, Config),
    get_not_loaded(Res).

user_send_private_message_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_send_private_message_muc_not_configured_story/2).

user_send_private_message_muc_not_configured_story(Config, Alice) ->
    Message = <<"Hello Bob!">>,
    BobNick = <<"Bobek">>,
    Res = user_send_private_message(Alice, get_room_name(), Message, BobNick, null, Config),
    get_not_loaded(Res).

user_get_room_messages_muc_or_mam_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_get_room_messages_muc_or_mam_not_configured_story/2).

user_get_room_messages_muc_or_mam_not_configured_story(Config, Alice) ->
    Res = user_get_room_messages(Alice, get_room_name(), 10, null, Config),
    get_not_loaded(Res).

user_owner_set_user_affiliation_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_owner_set_user_affiliation_muc_not_configured_story/2).

user_owner_set_user_affiliation_muc_not_configured_story(Config, Alice) ->
    Res = user_set_user_affiliation(Alice, get_room_name(), Alice, member, Config),
    get_not_loaded(Res).

user_moderator_set_user_role_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_moderator_set_user_role_muc_not_configured_story/2).

user_moderator_set_user_role_muc_not_configured_story(Config, Alice) ->
    Res = user_set_user_role(Alice, get_room_name(), Alice, moderator, Config),
    get_not_loaded(Res).

user_can_enter_room_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_can_enter_room_muc_not_configured_story/2).

user_can_enter_room_muc_not_configured_story(Config, Alice) ->
    Nick = <<"ali">>,
    Resource = escalus_client:resource(Alice),
    Res = user_enter_room(Alice, get_room_name(), Nick, Resource, null, Config),
    get_not_loaded(Res).

user_can_exit_room_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_can_exit_room_muc_not_configured_story/2).

user_can_exit_room_muc_not_configured_story(Config, Alice) ->
    Resource = escalus_client:resource(Alice),
    Res = user_exit_room(Alice, get_room_name(), <<"ali">>, Resource, Config),
    get_not_loaded(Res).

user_list_room_affiliations_muc_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_list_room_affiliations_muc_not_configured_story/2).

user_list_room_affiliations_muc_not_configured_story(Config, Alice) ->
    Res = user_list_room_affiliations(Alice, get_room_name(), owner, Config),
    get_not_loaded(Res).

%% Helpers

assert_coercion_err(Res, Code) ->
    ?assertNotEqual(nomatch, binary:match(get_coercion_err_msg(Res), Code)).

assert_no_full_jid(Res) ->
    assert_coercion_err(Res, <<"jid_without_resource">>).

assert_no_permission(Res) ->
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not have permission">>)).

assert_success(Path, Res) ->
    ?assertNotEqual(nomatch, binary:match(get_ok_value(Path, Res), <<"successfully">>)).

get_room_affiliation(RoomJID, Aff) ->
    {ok, Affs} = rpc(mim(), mod_muc_api, get_room_affiliations, [RoomJID, Aff]),
    Affs.

get_room_users(RoomJID) ->
    {ok, Users} = rpc(mim(), mod_muc_api, get_room_users, [RoomJID]),
    Users.

assert_user_role(RoomJID, User, Role) ->
    UserJID = jid:from_binary(escalus_client:full_jid(User)),
    ?assert(lists:any(fun(#{jid := JID, role := Role2}) ->
                              Role =:= Role2 andalso jid:are_bare_equal(JID, UserJID) end,
                      get_room_users(RoomJID))).

assert_user_affiliation(RoomJID, User, none) ->
    Affs = get_room_affiliation(RoomJID, undefined),
    UserSimpleJID = jid:to_lower(user_to_jid(User)),
    ?assertNot(lists:any(fun({U, _}) -> U == UserSimpleJID end, Affs));
assert_user_affiliation(RoomJID, User, Aff) ->
    Affs = get_room_affiliation(RoomJID, Aff),
    Elem = {jid:to_lower(user_to_jid(User)), Aff},
    ?assert(lists:member(Elem, Affs)).

rand_name() ->
    rpc(mim(), mongoose_bin, gen_from_crypto, []).

-spec assert_room_users([{jid:jid(), binary(), binary()}], [map()]) -> ok.
assert_room_users(Expected, Actual) ->
    ActualTuples = [{JID, Nick, Role} || #{<<"jid">> := JID, <<"role">> := Role, <<"nick">> := Nick} <- Actual],
    ?assertEqual(lists:sort(Expected), lists:sort(ActualTuples)).

assert_is_message_correct(RoomJID, SenderNick, Type, Text, ReceivedMessage) ->
    escalus_pred:is_message(ReceivedMessage),
    From = jid:to_binary(jid:replace_resource(RoomJID, SenderNick)),
    From = exml_query:attr(ReceivedMessage, <<"from">>),
    Type = exml_query:attr(ReceivedMessage, <<"type">>),
    Body = #xmlel{name = <<"body">>, children = [#xmlcdata{content=Text}]},
    Body = exml_query:subelement(ReceivedMessage, <<"body">>).

enter_room(RoomJID, User, Nick) ->
    JID = jid:to_binary(jid:replace_resource(RoomJID, Nick)),
    Pres = escalus_stanza:to(escalus_stanza:presence(<<"available">>,
        [#xmlel{ name = <<"x">>, attrs=#{<<"xmlns">> => <<"http://jabber.org/protocol/muc">>}}]),
        JID),
    escalus:send(User, Pres),
    escalus:wait_for_stanza(User).

contain_room(Name, #{<<"rooms">> := Rooms}) ->
    contain_room(Name, Rooms);
contain_room(Name, Rooms) when is_list(Rooms) ->
    lists:any(fun(#{<<"title">> := T}) -> T =:= Name end, Rooms).

extract_rooms(#{<<"rooms">> := [], <<"index">> := null, <<"count">> := 0,
                <<"first">> := null, <<"last">> := null}) ->
    [];
extract_rooms(#{<<"rooms">> := Rooms, <<"index">> := Index, <<"count">> := Count,
                <<"first">> := First, <<"last">> := Last}) ->
    Titles = lists:map(fun(#{<<"title">> := Title}) -> Title end, Rooms),
    ?assertEqual(hd(Titles), First),
    ?assertEqual(lists:last(Titles), Last),
    ?assert(is_integer(Count) andalso Count >= length(Titles)),
    ?assert(is_integer(Index) andalso Index >= 0),
    Titles.

returned_rooms(#{<<"rooms">> := Rooms}) ->
    [Title || #{<<"title">> := Title} <- Rooms].

assert_default_room_config(Response) ->
    ?assertMatch(#{<<"title">> := <<>>,
                   <<"description">> := <<>>,
                   <<"allowChangeSubject">> := true,
                   <<"allowQueryUsers">> := true,
                   <<"allowPrivateMessages">> := true,
                   <<"allowVisitorStatus">> := true,
                   <<"allowVisitorNickchange">> := true,
                   <<"public">> := true,
                   <<"publicList">> := true,
                   <<"persistent">> := false,
                   <<"moderated">> := true,
                   <<"membersByDefault">> := true,
                   <<"membersOnly">> := false,
                   <<"allowUserInvites">> := false,
                   <<"allowMultipleSession">> := false,
                   <<"passwordProtected">> := false,
                   <<"password">> := <<>>,
                   <<"anonymous">> := true,
                   <<"mayGetMemberList">> := [],
                   <<"maxUsers">> := 200,
                   <<"logging">> := false}, get_ok_value(?GET_ROOM_CONFIG_PATH, Response)).

atom_to_enum_item(null) -> null;
atom_to_enum_item(Atom) -> string:uppercase(atom_to_binary(Atom)).

get_room_name() ->
    Domain = domain_helper:domain(),
    <<"NON_EXISTING@", Domain/binary>>.

%% Commands

create_muc_light_room(Domain, Name, Subject, CreatorBin) ->
    CreatorJID = jid:from_binary(CreatorBin),
    Config = #{<<"roomname">> => Name, <<"subject">> => Subject},
    rpc(mim(), mod_muc_light_api, create_room, [Domain, CreatorJID, Config]).

create_instant_room(Room, Owner, Nick, Config) ->
    Vars = #{room => room_to_bin(Room), owner => user_to_bin(Owner), nick => Nick},
    execute_command(<<"muc">>, <<"createInstantRoom">>, Vars, Config).

invite_user(Room, Sender, Recipient, Reason, Config) ->
    Vars = #{room => jid:to_binary(Room), sender => user_to_bin(Sender),
             recipient => user_to_bin(Recipient), reason => Reason},
    execute_command(<<"muc">>, <<"inviteUser">>, Vars, Config).

kick_user(Room, Nick, Reason, Config) ->
    Vars = #{room => jid:to_binary(Room), nick => Nick, reason => Reason},
    execute_command(<<"muc">>, <<"kickUser">>, Vars, Config).

send_message_to_room(Room, From, Body, Config) ->
    Vars = #{room => jid:to_binary(Room), from => user_to_full_bin(From), body => Body},
    execute_command(<<"muc">>, <<"sendMessageToRoom">>, Vars, Config).

send_private_message(Room, From, ToNick, Body, Config) ->
    Vars = #{room => jid:to_binary(Room), from => user_to_full_bin(From),
             toNick => ToNick, body => Body},
    execute_command(<<"muc">>, <<"sendPrivateMessage">>, Vars, Config).

send_message_to_muc_light_room(RoomJID, SenderJID, Message) ->
    rpc(mim(), mod_muc_light_api, send_message, [RoomJID, SenderJID, Message]).

enter_room(Room, User, Nick, Password, Config) ->
    Vars = #{room => jid:to_binary(Room), user => user_to_full_bin(User),
             nick => Nick, password => Password},
    execute_command(<<"muc">>, <<"enterRoom">>, Vars, Config).

delete_room(Room, Reason, Config) ->
    Vars = #{room => jid:to_binary(Room), reason => Reason},
    execute_command(<<"muc">>, <<"deleteRoom">>, Vars, Config).

change_room_config(Room, RoomConfig, Config) ->
    Vars = #{room => jid:to_binary(Room), config => RoomConfig},
    execute_command(<<"muc">>, <<"changeRoomConfiguration">>, Vars, Config).

list_rooms(MUCDomain, From, Limit, Index, Config) ->
    Vars = #{mucDomain => MUCDomain, from => user_to_bin(From), limit => Limit, index => Index},
    execute_command(<<"muc">>, <<"listRooms">>, Vars, Config).

get_room_config(Room, Config) ->
    Vars = #{room => jid:to_binary(Room)},
    execute_command(<<"muc">>, <<"getRoomConfig">>, Vars, Config).

list_room_users(RoomJID, Config) ->
    Vars = #{room => jid:to_binary(RoomJID)},
    execute_command(<<"muc">>, <<"listRoomUsers">>, Vars, Config).

get_room_messages(RoomJID, PageSize, Before, Config) ->
    Vars = #{<<"room">> => jid:to_binary(RoomJID), <<"pageSize">> => PageSize,
             <<"before">> => Before},
    execute_command(<<"muc">>, <<"getRoomMessages">>, Vars, Config).

set_user_affiliation(Room, User, Aff, Config) ->
    Vars = #{room => jid:to_binary(Room), user => user_to_bin(User),
             affiliation => atom_to_enum_item(Aff)},
    execute_command(<<"muc">>, <<"setUserAffiliation">>, Vars, Config).

set_user_role(Room, User, Role, Config) ->
    Vars = #{room => jid:to_binary(Room), nick => user_to_bin(User),
             role => atom_to_enum_item(Role)},
    execute_command(<<"muc">>, <<"setUserRole">>, Vars, Config).

exit_room(Room, User, Nick, Config) ->
    Vars = #{room => jid:to_binary(Room), user => user_to_full_bin(User), nick => Nick},
    execute_command(<<"muc">>, <<"exitRoom">>, Vars, Config).

list_room_affiliations(Room, Aff, Config) ->
    Vars = #{room => jid:to_binary(Room), affiliation => atom_to_enum_item(Aff)},
    execute_command(<<"muc">>, <<"listRoomAffiliations">>, Vars, Config).

user_kick_user(User, Room, Nick, Reason, Config) ->
    Vars = #{room => jid:to_binary(Room), nick => Nick, reason => Reason},
    execute_user_command(<<"muc">>, <<"kickUser">>, User, Vars, Config).

user_enter_room(User, Room, Nick, Resource, Password, Config) ->
    Vars = #{room => jid:to_binary(Room), nick => Nick, resource => Resource, password => Password},
    execute_user_command(<<"muc">>, <<"enterRoom">>, User, Vars, Config).

user_get_room_messages(User, RoomJID, PageSize, Before, Config) ->
    Vars = #{<<"room">> => jid:to_binary(RoomJID), <<"pageSize">> => PageSize,
             <<"before">> => Before},
    execute_user_command(<<"muc">>, <<"getRoomMessages">>, User, Vars, Config).

user_get_muc_light_room_messages(User, RoomJID, PageSize, Before, Config) ->
    Vars = #{<<"room">> => RoomJID, <<"pageSize">> => PageSize, <<"before">> => Before},
    execute_user_command(<<"muc_light">>, <<"getRoomMessages">>, User, Vars, Config).

user_delete_room(User, Room, Reason, Config) ->
    Vars = #{room => jid:to_binary(Room), reason => Reason},
    execute_user_command(<<"muc">>, <<"deleteRoom">>, User, Vars, Config).

user_change_room_config(User, Room, RoomConfig, Config) ->
    Vars = #{room => jid:to_binary(Room), config => RoomConfig},
    execute_user_command(<<"muc">>, <<"changeRoomConfiguration">>, User, Vars, Config).

user_list_rooms(User, MUCDomain, Limit, Index, Config) ->
    Vars = #{mucDomain => MUCDomain, limit => Limit, index => Index},
    execute_user_command(<<"muc">>, <<"listRooms">>, User, Vars, Config).

user_get_room_config(User, Room, Config) ->
    Vars = #{room => jid:to_binary(Room)},
    execute_user_command(<<"muc">>, <<"getRoomConfig">>, User, Vars, Config).

user_list_room_users(User, RoomJID, Config) ->
    Vars = #{room => jid:to_binary(RoomJID)},
    execute_user_command(<<"muc">>, <<"listRoomUsers">>, User, Vars, Config).

user_send_message_to_room(User, Room, Body, Resource, Config) ->
    Vars = #{room => jid:to_binary(Room), body => Body, resource => Resource},
    execute_user_command(<<"muc">>, <<"sendMessageToRoom">>, User, Vars, Config).

user_send_private_message(User, Room, Body, ToNick, Resource, Config) ->
    Vars = #{room => jid:to_binary(Room), body => Body, toNick => ToNick, resource => Resource},
    execute_user_command(<<"muc">>, <<"sendPrivateMessage">>, User, Vars, Config).

user_create_instant_room(User, Room, Nick, Config) ->
    Vars = #{room => room_to_bin(Room), nick => Nick},
    execute_user_command(<<"muc">>, <<"createInstantRoom">>, User, Vars, Config).

user_invite_user(User, Room, Recipient, Reason, Config) ->
    Vars = #{room => jid:to_binary(Room), recipient => user_to_bin(Recipient), reason => Reason},
    execute_user_command(<<"muc">>, <<"inviteUser">>, User, Vars, Config).

user_set_user_affiliation(User, Room, QueriedUser, Aff, Config) ->
    Vars = #{room => jid:to_binary(Room), user => user_to_bin(QueriedUser),
             affiliation => atom_to_enum_item(Aff)},
    execute_user_command(<<"muc">>, <<"setUserAffiliation">>, User, Vars, Config).

user_set_user_role(User, Room, QueriedUser, Role, Config) ->
    Vars = #{room => jid:to_binary(Room), nick => user_to_bin(QueriedUser),
             role => atom_to_enum_item(Role)},
    execute_user_command(<<"muc">>, <<"setUserRole">>, User, Vars, Config).

user_exit_room(User, Room, Nick, Resource, Config) ->
    Vars = #{room => jid:to_binary(Room), resource => Resource, nick => Nick},
    execute_user_command(<<"muc">>, <<"exitRoom">>, User, Vars, Config).

user_list_room_affiliations(User, Room, Aff, Config) ->
    Vars = #{room => jid:to_binary(Room), affiliation => atom_to_enum_item(Aff)},
    execute_user_command(<<"muc">>, <<"listRoomAffiliations">>, User, Vars, Config).

room_to_bin(RoomJIDBin) when is_binary(RoomJIDBin) ->RoomJIDBin;
room_to_bin(RoomJID) -> jid:to_binary(RoomJID).
