-module(graphql_muc_light_SUITE).

-compile([export_all, nowarn_export_all]).

-import(common_helper, [unprep/1]).
-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4, subhost_pattern/1]).
-import(graphql_helper, [execute_user_command/5, execute_command/4, get_listener_port/1,
                         get_listener_config/1, get_ok_value/2, get_err_msg/1,
                         get_coercion_err_msg/1, make_creds/1, get_unauthorized/1,
                         get_err_code/1, get_not_loaded/1]).

-import(config_parser_helper, [mod_config/2]).

-include_lib("common_test/include/ct.hrl").
-include_lib("jid/include/jid.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").

-define(UNKNOWN_DOMAIN, <<"not-existing-domain">>).
-define(UNKNOWN, <<"not-existing">>).

%% GraphQL Paths
-define(CREATE_ROOM_PATH, [data, muc_light, createRoom]).
-define(CHANGE_CONFIG_PATH, [data, muc_light, changeRoomConfiguration]).
-define(INVITE_USER_PATH, [data, muc_light, inviteUser]).
-define(KICK_USER_PATH, [data, muc_light, kickUser]).
-define(DELETE_ROOM_PATH, [data, muc_light, deleteRoom]).
-define(SEND_MESSAGE_PATH, [data, muc_light, sendMessageToRoom]).
-define(GET_MESSAGES_PATH, [data, muc_light, getRoomMessages]).
-define(LIST_USER_ROOMS_PATH, [data, muc_light, listUserRooms]).
-define(USER_LIST_ROOMS_PATH, [data, muc_light, listRooms]).
-define(LIST_ROOM_USERS_PATH, [data, muc_light, listRoomUsers]).
-define(GET_ROOM_CONFIG_PATH, [data, muc_light, getRoomConfig]).
-define(GET_BLOCKING_LIST_PATH, [data, muc_light, getBlockingList]).
-define(SET_BLOCKING_LIST_PATH, [data, muc_light, setBlockingList]).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, user},
     {group, admin_http},
     {group, admin_cli},
     {group, domain_admin}].

groups() ->
    [{user, [], user_groups()},
     {admin_http, [], admin_groups()},
     {admin_cli, [], admin_groups()},
     {domain_admin, domain_admin_groups()},
     {user_muc_light, [parallel], user_muc_light_tests()},
     {user_muc_light_with_mam, [parallel], user_muc_light_with_mam_tests()},
     {user_muc_light_not_configured, [], user_muc_light_not_configured_tests()},
     {admin_muc_light, [], admin_muc_light_tests()},
     {admin_muc_light_with_mam, [], admin_muc_light_with_mam_tests()},
     {domain_admin_muc_light, [], domain_admin_muc_light_tests()},
     {domain_admin_muc_light_with_mam, [], domain_admin_muc_light_with_mam_tests()},
     {admin_muc_light_not_configured, [], admin_muc_light_not_configured_tests()}].

user_groups() ->
    [{group, user_muc_light},
     {group, user_muc_light_with_mam},
     {group, user_muc_light_not_configured}].

admin_groups() ->
    [{group, admin_muc_light},
     {group, admin_muc_light_with_mam},
     {group, admin_muc_light_not_configured}].

domain_admin_groups() ->
    [{group, domain_admin_muc_light},
     {group, domain_admin_muc_light_with_mam}].

user_muc_light_tests() ->
    [user_create_room,
     user_create_room_with_unprepped_domain,
     user_create_room_with_bad_config,
     user_create_room_with_custom_fields,
     user_create_identified_room,
     user_create_room_with_unprepped_id,
     user_change_room_config,
     user_change_room_config_errors,
     user_invite_user,
     user_invite_user_errors,
     user_delete_room,
     user_kick_user,
     user_send_message_to_room,
     user_send_message_to_room_errors,
     user_get_room_messages_muc_light_or_mam_not_configured,
     user_list_rooms,
     user_list_room_users,
     user_get_room_config,
     user_blocking_list
    ].

user_muc_light_with_mam_tests() ->
    [user_get_room_messages,
     user_shouldnt_store_messages_in_muc].

user_muc_light_not_configured_tests() ->
    [user_create_room_muc_light_not_configured,
     user_change_room_config_muc_light_not_configured,
     user_invite_user_muc_light_not_configured,
     user_delete_room_muc_light_not_configured,
     user_kick_user_muc_light_not_configured,
     user_send_message_to_room_muc_light_not_configured,
     user_get_room_messages_muc_light_or_mam_not_configured,
     user_list_rooms_muc_light_not_configured,
     user_list_room_users_muc_light_not_configured,
     user_get_room_config_muc_light_not_configured,
     user_blocking_list_muc_light_not_configured].

admin_muc_light_tests() ->
    [admin_create_room,
     admin_create_room_non_existent_domain,
     admin_create_room_with_unprepped_domain,
     admin_create_room_with_custom_fields,
     admin_create_identified_room,
     admin_create_identified_room_non_existent_domain,
     admin_create_room_with_unprepped_id,
     admin_change_room_config,
     admin_change_room_config_with_custom_fields,
     admin_change_room_config_errors,
     admin_change_room_config_non_existent_domain,
     admin_invite_user,
     admin_invite_user_errors,
     admin_delete_room,
     admin_delete_room_non_existent_domain,
     admin_kick_user,
     admin_send_message_to_room,
     admin_send_message_to_room_errors,
     admin_get_room_messages_muc_light_or_mam_not_configured,
     admin_list_user_rooms,
     admin_list_user_rooms_non_existent_domain,
     admin_list_room_users,
     admin_list_room_users_non_existent_domain,
     admin_get_room_config,
     admin_get_room_config_non_existent_domain,
     admin_blocking_list,
     admin_blocking_list_null,
     admin_blocking_list_errors
    ].

admin_muc_light_with_mam_tests() ->
    [admin_get_room_messages,
     admin_get_room_messages_non_existent_domain].

domain_admin_muc_light_tests() ->
    [admin_create_room,
     admin_create_room_with_unprepped_domain,
     admin_create_room_with_custom_fields,
     domain_admin_create_room_no_permission,
     admin_create_identified_room,
     admin_create_room_with_unprepped_id,
     domain_admin_create_identified_room_no_permission,
     admin_change_room_config,
     admin_change_room_config_with_custom_fields,
     admin_change_room_config_errors,
     domain_admin_change_room_config_no_permission,
     admin_invite_user,
     admin_invite_user_errors,
     domain_admin_invite_user_no_permission,
     admin_delete_room,
     domain_admin_delete_room_no_permission,
     admin_kick_user,
     domain_admin_kick_user_no_permission,
     admin_send_message_to_room,
     admin_send_message_to_room_errors,
     domain_admin_send_message_to_room_no_permission,
     admin_get_room_messages_muc_light_or_mam_not_configured,
     admin_list_user_rooms,
     domain_admin_list_user_rooms_no_permission,
     admin_list_room_users,
     domain_admin_list_room_users_no_permission,
     admin_get_room_config,
     domain_admin_get_room_config_no_permission,
     admin_blocking_list,
     domain_admin_blocking_list_no_permission
    ].

domain_admin_muc_light_with_mam_tests() ->
    [admin_get_room_messages,
     domain_admin_get_room_messages_no_permission].

admin_muc_light_not_configured_tests() ->
    [admin_create_room_muc_light_not_configured,
     admin_change_room_config_muc_light_not_configured,
     admin_invite_user_muc_light_not_configured,
     admin_delete_room_muc_light_not_configured,
     admin_kick_user_muc_light_not_configured,
     admin_send_message_to_room_muc_light_not_configured,
     admin_get_room_messages_muc_light_or_mam_not_configured,
     admin_list_user_rooms_muc_light_not_configured,
     admin_list_room_users_muc_light_not_configured,
     admin_get_room_config_muc_light_not_configured,
     admin_blocking_list_muc_light_not_configured].

init_per_suite(Config) ->
    HostType = domain_helper:host_type(),
    SecondaryHostType = domain_helper:secondary_host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config),
    Config2 = dynamic_modules:save_modules(SecondaryHostType, Config1),
    Config3 = ejabberd_node_utils:init(mim(), Config2),
    escalus:init_per_suite(Config3).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

required_modules(_) ->
    Backend = mongoose_helper:mnesia_or_rdbms_backend(),
    MucLightOpts = mod_config(mod_muc_light, #{backend => Backend,
                                               rooms_in_rosters => true,
                                               config_schema => custom_schema()}),
    [{mod_muc_light, MucLightOpts}].

custom_schema() ->
    %% Should be sorted
    [{<<"background">>, <<>>, background, binary},
     {<<"music">>, <<>>, music, binary},
     %% Default fields
     {<<"roomname">>, <<>>, roomname, binary},
     {<<"subject">>, <<"Test">>, subject, binary}].

init_per_group(admin_http, Config) ->
    graphql_helper:init_admin_handler(Config);
init_per_group(admin_cli, Config) ->
    graphql_helper:init_admin_cli(Config);
init_per_group(domain_admin, Config) ->
    graphql_helper:init_domain_admin_handler(Config);
init_per_group(Group, Config) when Group =:= user_muc_light;
                                   Group =:= admin_muc_light;
                                   Group =:= domain_admin_muc_light ->
    disable_mam(),
    ensure_muc_light_started(Config);
init_per_group(Group, Config) when Group =:= user_muc_light_with_mam;
                                   Group =:= admin_muc_light_with_mam;
                                   Group =:= domain_admin_muc_light_with_mam ->
    case maybe_enable_mam() of
        true ->
            ensure_muc_started(),
            ensure_muc_light_started(Config);
        false ->
            {skip, "No MAM backend available"}
    end;
init_per_group(Group, Config) when Group =:= user_muc_light_not_configured;
                                   Group =:= admin_muc_light_not_configured ->
    maybe_enable_mam(),
    ensure_muc_light_stopped(Config);
init_per_group(user, Config) ->
    graphql_helper:init_user(Config).

disable_mam() ->
    dynamic_modules:ensure_modules(domain_helper:host_type(), [{mod_mam, stopped}]).

maybe_enable_mam() ->
    case mam_helper:backend() of
        disabled ->
            false;
        Backend ->
            MAMOpts = mam_helper:config_opts(
                        #{backend => Backend,
                          muc => #{host => subhost_pattern(muc_light_helper:muc_host_pattern())},
                          async_writer => #{enabled => false}}),
            dynamic_modules:ensure_modules(domain_helper:host_type(), [{mod_mam, MAMOpts}]),
            true
    end.

ensure_muc_light_started(Config) ->
    HostType = domain_helper:host_type(),
    SecondaryHostType = domain_helper:secondary_host_type(),
    dynamic_modules:ensure_modules(HostType, required_modules(suite)),
    dynamic_modules:ensure_modules(SecondaryHostType, required_modules(suite)),
    [{muc_light_host, muc_light_helper:muc_host()},
     {secondary_muc_light_host, <<"muclight.", (domain_helper:secondary_domain())/binary>>}
     | Config].

ensure_muc_light_stopped(Config) ->
    HostType = domain_helper:host_type(),
    SecondaryHostType = domain_helper:secondary_host_type(),
    dynamic_modules:ensure_modules(HostType, [{mod_muc_light, stopped}]),
    dynamic_modules:ensure_modules(SecondaryHostType, [{mod_muc_light, stopped}]),
    [{muc_light_host, <<"NON_EXISTING">>} | Config].

ensure_muc_started() ->
    muc_helper:load_muc(),
    mongoose_helper:ensure_muc_clean().

ensure_muc_stopped() ->
    muc_helper:unload_muc().

end_per_group(Group, _Config) when Group =:= user;
                                   Group =:= admin_http;
                                   Group =:= admin_cli;
                                   Group =:= domain_admin ->
    maybe_clear_db(),
    graphql_helper:clean();
end_per_group(Group, _Config) when Group =:= user_muc_light_with_mam;
                                   Group =:= admin_muc_light_with_mam;
                                   Group =:= domain_admin_muc_light_with_mam ->
    maybe_clear_db(),
    ensure_muc_stopped(),
    escalus_fresh:clean();
end_per_group(_Group, _Config) ->
    maybe_clear_db(),
    escalus_fresh:clean().

maybe_clear_db() ->
    case mongoose_helper:is_rdbms_enabled(domain_helper:host_type()) of
        true ->
            ok = rpc(mim(), mod_muc_light_db_rdbms, force_clear, []);
        false ->
            ok
    end.

init_per_testcase(TC, Config) ->
    rest_helper:maybe_skip_mam_test_cases(TC, [user_get_room_messages,
                                               admin_get_room_messages], Config).

end_per_testcase(TC, Config) ->
    escalus:end_per_testcase(TC, Config).

%% User test cases

user_create_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_create_room_story/2).

user_create_room_story(Config, Alice) ->
    MucServer = ?config(muc_light_host, Config),
    AliceBinLower = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    Res = user_create_room(Alice, MucServer, Name, Subject, null, Config),
    #{<<"jid">> := JID, <<"name">> := Name, <<"subject">> := Subject,
      <<"participants">> := Participants} = get_ok_value(?CREATE_ROOM_PATH, Res),
    ?assertMatch(#jid{lserver = MucServer}, jid:from_binary(JID)),
    ?assertEqual([#{<<"jid">> => AliceBinLower, <<"affiliation">> => <<"OWNER">>}], Participants),
    % Try with a non-existent domain
    Res2 = user_create_room(Alice, ?UNKNOWN_DOMAIN, Name, Subject, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)).

user_create_room_with_unprepped_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_create_room_with_unprepped_domain_story/2).

user_create_room_with_unprepped_domain_story(Config, Alice) ->
    MucServer = ?config(muc_light_host, Config),
    Name = <<"room with unprepped domain">>,
    Subject = <<"testing">>,
    Res = user_create_room(Alice, unprep(MucServer), Name, Subject, null, Config),
    #{<<"jid">> := JID, <<"name">> := Name, <<"subject">> := Subject} =
        get_ok_value(?CREATE_ROOM_PATH, Res),
    ?assertMatch(#jid{lserver = MucServer}, jid:from_binary_noprep(JID)).

user_create_room_with_bad_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_create_room_with_bad_config_story/2).

user_create_room_with_bad_config_story(Config, Alice) ->
    MucServer = ?config(muc_light_host, Config),
    Name = <<"room with custom fields">>,
    Subject = <<"testing_custom">>,
    Opts = #{<<"badkey">> => <<"badvalue">>},
    Res =  user_create_room_with_options(Alice, MucServer, Name, Subject, null, Opts, Config),
    ?assertMatch({_, _}, binary:match(get_err_msg(Res), <<"Validation failed for key: badkey">>)).

user_create_room_with_custom_fields(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_create_room_with_custom_fields_story/2).

user_create_room_with_custom_fields_story(Config, Alice) ->
    MucServer = ?config(muc_light_host, Config),
    AliceBinLower = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
    Name = <<"room with custom fields">>,
    Subject = <<"testing_custom">>,
    Opts = [#{<<"key">> => <<"background">>, <<"value">> => <<"red">>},
            #{<<"key">> => <<"roomname">>, <<"value">> => Name},
            #{<<"key">> => <<"subject">>, <<"value">> => Subject}],
    Res = user_create_room_with_options(Alice, MucServer, Name, Subject, null,
                                        #{<<"background">> => <<"red">>}, Config),
    #{<<"jid">> := JID, <<"name">> := Name, <<"subject">> := Subject,
      <<"participants">> := Participants, <<"options">> := Opts}
        = get_ok_value(?CREATE_ROOM_PATH, Res),
    ?assertMatch(#jid{lserver = MucServer}, jid:from_binary(JID)),
    ?assertEqual([#{<<"jid">> => AliceBinLower, <<"affiliation">> => <<"OWNER">>}], Participants).

user_create_identified_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_create_identified_room_story/2).

user_create_identified_room_story(Config, Alice) ->
    MucServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    Id = <<"my_user_room">>,
    Res = user_create_room(Alice, MucServer, Name, Subject, Id, Config),
    #{<<"jid">> := JID, <<"name">> := Name, <<"subject">> := Subject} =
        get_ok_value(?CREATE_ROOM_PATH, Res),
    ?assertMatch(#jid{luser = Id, lserver = MucServer}, jid:from_binary(JID)),
    % Create a room with an existing ID
    Res2 = user_create_room(Alice, MucServer, <<"snd room">>, Subject, Id, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"already exists">>)),
    % Try with a non-existent domain
    Res3 = user_create_room(Alice, ?UNKNOWN_DOMAIN, <<"name">>, Subject, Id, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)),
    % Try with an empty string passed as ID
    Res4 = user_create_room(Alice, MucServer, <<"name">>, Subject, <<>>, Config),
    ?assertMatch({_, _}, binary:match(get_coercion_err_msg(Res4), <<"empty_room_name">>)).

user_create_room_with_unprepped_id(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun user_create_room_with_unprepped_id_story/2).

user_create_room_with_unprepped_id_story(Config, Alice) ->
    MucServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    Id = <<"user_room_with_unprepped_id">>,
    Res = user_create_room(Alice, unprep(MucServer), Name, Subject, unprep(Id), Config),
    #{<<"jid">> := JID, <<"name">> := Name, <<"subject">> := Subject} =
        get_ok_value(?CREATE_ROOM_PATH, Res),
    ?assertMatch(#jid{luser = Id, lserver = MucServer}, jid:from_binary_noprep(JID)).

user_change_room_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_change_room_config_story/2).

user_change_room_config_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MUCServer = ?config(muc_light_host, Config),
    % Create a new room
    {ok, #{jid := RoomJID}} = create_room(MUCServer, <<"ornithology">>, <<"birds">>, AliceBin),
    % Try to change the room configuration
    Name2 = <<"changed room">>,
    Subject2 = <<"not testing">>,
    Res = user_change_room_configuration(Alice, jid:to_binary(RoomJID), Name2, Subject2, Config),
    ?assertMatch(#{<<"name">> := Name2, <<"subject">> := Subject2}, get_ok_value(?CHANGE_CONFIG_PATH, Res)).

user_change_room_config_with_custom_fields(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_change_room_config_with_custom_fields_story/2).

user_change_room_config_with_custom_fields_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MUCServer = ?config(muc_light_host, Config),
    % Create a new room
    {ok, #{jid := RoomJID}} = create_room(MUCServer, <<"ornithology">>, <<"birds">>, AliceBin),
    % Try to change the room configuration
    Name2 = <<"changed room">>,
    Subject2 = <<"not testing">>,
    Opts2 = #{<<"music">> => <<"sad">>},
    Res = user_change_room_configuration_with_custom_fields(Alice, jid:to_binary(RoomJID), Name2, Subject2, Config, Opts2),
    Opts3 = [#{<<"key">> => <<"music">>, <<"value">> => <<"sad">>},
             #{<<"key">> => <<"roomname">>, <<"value">> => Name2},
             #{<<"key">> => <<"subject">>, <<"value">> => Subject2}],
    ?assertMatch(#{<<"name">> := Name2, <<"subject">> := Subject2, <<"options">> := Opts3}, get_ok_value(?CHANGE_CONFIG_PATH, Res)).

user_change_room_config_errors(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_change_room_config_errors_story/3).

user_change_room_config_errors_story(Config, Alice, Bob) ->
    MUCServer = ?config(muc_light_host, Config),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    RoomName = <<"first room">>,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    % Try to change the config with a non-existent domain
    Res = user_change_room_configuration(
            Alice, make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), RoomName, <<"subject2">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)),
    % Try to change the config of the non-existent room
    Res2 = user_change_room_configuration(
             Alice, make_bare_jid(?UNKNOWN, MUCServer), RoomName, <<"subject2">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)),
    % Try to change the config by the user that does not occupy this room
    Res3 = user_change_room_configuration(
             Bob, jid:to_binary(RoomJID), RoomName, <<"subject2">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not occupy this room">>)),
    % Try to change a config by the user without permission
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    Res4 = user_change_room_configuration(
             Bob, jid:to_binary(RoomJID), RoomName, <<"subject2">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4),
                                          <<"does not have permission to change">>)).

user_invite_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun user_invite_user_story/3).

user_invite_user_story(Config, Alice, Bob) ->
    MUCServer = ?config(muc_light_host, Config),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    Domain = escalus_client:server(Alice),
    Name = <<"first room">>,
    {ok, #{jid := RoomJID}} = create_room(MUCServer, Name, <<"subject2">>, AliceBin),
    % Room owner can invite a user
    Res = user_invite_user(Alice, jid:to_binary(RoomJID), BobBin, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?INVITE_USER_PATH, Res),
                                          <<"successfully">>)),
    escalus:wait_for_stanza(Bob),
    BobName = escalus_utils:jid_to_lower(escalus_client:username(Bob)),
    AliceName = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
    ExpectedAff = lists:sort([{{AliceName, Domain}, owner},
                              {{BobName, Domain}, member}]),
    ?assertMatch(ExpectedAff, lists:sort(get_room_aff(RoomJID))).

user_invite_user_errors(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_invite_user_errors_story/3).

user_invite_user_errors_story(Config, Alice, Bob) ->
    MUCServer = ?config(muc_light_host, Config),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(MUCServer, <<"first room">>, <<"subject">>, AliceBin),
    % Try to invite a user to not existing room
    Res = user_invite_user(Alice, make_bare_jid(?UNKNOWN, MUCServer), BobBin, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)),
    % User without rooms tries to invite a user
    Res2 = user_invite_user(Bob, jid:to_binary(RoomJID), AliceBin, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not occupy this room">>)),
    % Try with a non-existent domain
    Res3 = user_invite_user(Alice, make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), BobBin, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)).

user_delete_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun user_delete_room_story/3).

user_delete_room_story(Config, Alice, Bob) ->
    MUCServer = ?config(muc_light_host, Config),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    Name = <<"first room">>,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID} = RoomInfo} =
        create_room(MUCServer, Name, <<"subject">>, AliceBin),
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    % Member cannot delete room
    Res = user_delete_room(Bob, jid:to_binary(RoomJID), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res),
                                          <<"cannot delete this room">>)),
    % Owner can delete own room
    Res2 = user_delete_room(Alice, jid:to_binary(RoomJID), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?DELETE_ROOM_PATH, Res2),
                                          <<"successfully">>)),
    ?assertEqual({error, not_exists}, get_room_info(RoomJID), RoomInfo),
    % Try with a non-existent domain
    Res3 = user_delete_room(Alice, make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)),
    % Try with a non-existent room
    Res4 = user_delete_room(Alice, make_bare_jid(?UNKNOWN, MUCServer), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4), <<"not found">>)).

user_kick_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun user_kick_user_story/3).

user_kick_user_story(Config, Alice, Bob) ->
    MUCServer = ?config(muc_light_host, Config),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    RoomName = <<"first room">>,
    {ok, #{jid := RoomJID}} = create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    % Member kicks himself from a room
    ?assertEqual(2, length(get_room_aff(RoomJID))),
    Res = user_kick_user(Bob, jid:to_binary(RoomJID), null, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?KICK_USER_PATH, Res),
                                          <<"successfully">>)),
    ?assertEqual(1, length(get_room_aff(RoomJID))),
    % Member cannot kick the room owner.
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    Res2 = user_kick_user(Bob, jid:to_binary(RoomJID), AliceBin, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not have permission">>)),
    ?assertEqual(2, length(get_room_aff(RoomJID))),
    % Owner kicks the member from a room
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    Res3 = user_kick_user(Alice, jid:to_binary(RoomJID), BobBin, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?KICK_USER_PATH, Res3), <<"successfully">>)),
    ?assertEqual(1, length(get_room_aff(RoomJID))).

user_send_message_to_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_message_to_room_story/3).

user_send_message_to_room_story(Config, Alice, Bob) ->
    MUCServer = ?config(muc_light_host, Config),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    RoomName = <<"first room">>,
    MsgBody = <<"Hello there!">>,
    {ok, #{jid := RoomJID}} = create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    Res = user_send_message_to_room(Alice, jid:to_binary(RoomJID), MsgBody, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SEND_MESSAGE_PATH, Res), <<"successfully">>)),
    [_, Msg] = escalus:wait_for_stanzas(Bob, 2),
    escalus:assert(is_message, Msg).

user_send_message_to_room_errors(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun user_send_message_to_room_errors_story/3).

user_send_message_to_room_errors_story(Config, Alice, Bob) ->
    MUCServer = ?config(muc_light_host, Config),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    MsgBody = <<"Hello there!">>,
    {ok, #{jid := #jid{luser = ARoomID} = ARoomJID}} =
        create_room(MUCServer, <<"alice room">>, <<"subject">>, AliceBin),
    % Try with a non-existent domain
    Res = user_send_message_to_room(
            Alice, make_bare_jid(ARoomID, ?UNKNOWN_DOMAIN), MsgBody, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)),
    % Try with a user without rooms
    Res2 = user_send_message_to_room(Bob, jid:to_binary(ARoomJID), MsgBody, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not occupy this room">>)),
    % Try with a room not occupied by this user
    {ok, #{jid := _RoomJID2}} = create_room(MUCServer, <<"bob room">>, <<"subject">>, BobBin),
    Res3 = user_send_message_to_room(Bob, jid:to_binary(ARoomJID), MsgBody, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not occupy this room">>)).

user_get_room_messages(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun user_get_room_messages_story/3).

user_get_room_messages_story(Config, Alice, Bob) ->
    MUCServer = ?config(muc_light_host, Config),
    AliceBin = escalus_client:short_jid(Alice),
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(MUCServer, <<"first room">>, <<"subject">>, AliceBin),
    Message = <<"Hello friends">>,
    send_message_to_room(RoomJID, jid:from_binary(AliceBin), Message),
    mam_helper:wait_for_room_archive_size(MUCServer, RoomID, 1),
    % Get messages so far
    Limit = 40,
    Res = user_get_room_messages(Alice, jid:to_binary(RoomJID), Limit, null, Config),
    #{<<"stanzas">> :=[#{<<"stanza">> := StanzaXML}], <<"limit">> := Limit} =
        get_ok_value(?GET_MESSAGES_PATH, Res),
    ?assertMatch({ok, #xmlel{name = <<"message">>}}, exml:parse(StanzaXML)),
    % Get messages before the given date and time
    Before = <<"2022-02-17T04:54:13+00:00">>,
    Res2 = user_get_room_messages(Alice, jid:to_binary(RoomJID), null, Before, Config),
    ?assertMatch(#{<<"stanzas">> := [], <<"limit">> := 50}, get_ok_value(?GET_MESSAGES_PATH, Res2)),
    % Try to pass too big page size value
    Res3 = user_get_room_messages(Alice, jid:to_binary(RoomJID), 51, Before, Config),
    ?assertMatch(#{<<"limit">> := 50}, get_ok_value(?GET_MESSAGES_PATH, Res3)),
    % Try with a non-existent domain
    Res4 = user_get_room_messages(
             Alice, make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), Limit, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4), <<"server not found">>)),
    % Try with a user that is not a room member
    Res5 = user_get_room_messages(Bob, jid:to_binary(RoomJID), Limit, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res5), <<"not occupy this room">>)).

user_shouldnt_store_messages_in_muc(Config) ->
    muc_helper:story_with_room(Config, [], [{alice, 1}],
                               fun user_shouldnt_store_messages_in_muc_story/2).

user_shouldnt_store_messages_in_muc_story(Config, Alice) ->
    %% Enter a MUC room
    MUCRoomJID = jid:from_binary(?config(room_jid, Config)),
    enter_muc_room(MUCRoomJID, Alice, <<"Ali">>),
    escalus:wait_for_stanza(Alice),
    %% Send a message
    Message = <<"Hello friends">>,
    send_message_to_muc_room(Alice, MUCRoomJID, Message, null, Config),
    escalus:wait_for_stanza(Alice),
    %% Try to get a MUC message
    Limit = 1,
    Res3 = user_get_muc_room_messages(Alice, jid:to_binary(MUCRoomJID), Limit, null, Config),
    #{<<"stanzas">> := [], <<"limit">> := Limit} = get_ok_value([data, muc, getRoomMessages], Res3).

user_list_rooms(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun user_list_rooms_story/2).

user_list_rooms_story(Config, Alice) ->
    MUCServer = ?config(muc_light_host, Config),
    AliceBin = escalus_client:short_jid(Alice),
    {ok, #{jid := RoomJID}} = create_room(MUCServer, <<"room a">>, <<"subject">>, AliceBin),
    {ok, #{jid := RoomJID2}} = create_room(MUCServer, <<"room b">>, <<"subject">>, AliceBin),
    Res = user_list_rooms(Alice, Config),
    ?assertEqual(lists:sort([jid:to_binary(RoomJID), jid:to_binary(RoomJID2)]),
                 lists:sort(get_ok_value(?USER_LIST_ROOMS_PATH, Res))).

user_list_room_users(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun user_list_room_users_story/3).

user_list_room_users_story(Config, Alice, Bob) ->
    MUCServer = ?config(muc_light_host, Config),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    AliceLower = escalus_utils:jid_to_lower(AliceBin),
    {ok, #{jid := RoomJID}} = create_room(MUCServer, <<"room a">>, <<"subject">>, AliceBin),
    % Owner can list room users
    Res = user_list_room_users(Alice, jid:to_binary(RoomJID), Config),
    ?assertEqual([#{<<"jid">> => AliceLower, <<"affiliation">> => <<"OWNER">>}],
                 get_ok_value(?LIST_ROOM_USERS_PATH, Res)),
    % Try with a non-existent domain
    Res2 = user_list_room_users(Alice, make_bare_jid(RoomJID#jid.luser, ?UNKNOWN_DOMAIN), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)),
    % Try with a non-existent room
    Res3 = user_list_room_users(Alice, make_bare_jid(?UNKNOWN, MUCServer), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)),
    % User that is not a member cannot get aff
    Res4 = user_list_room_users(Bob, jid:to_binary(RoomJID), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4), <<"not occupy this room">>)),
    % Member can get aff
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    escalus:wait_for_stanza(Bob),
    Res5 = user_list_room_users(Bob, jid:to_binary(RoomJID), Config),
    ?assertMatch([_, _], get_ok_value(?LIST_ROOM_USERS_PATH, Res5)).

user_get_room_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun user_get_room_config_story/3).

user_get_room_config_story(Config, Alice, Bob) ->
    MUCServer = ?config(muc_light_host, Config),
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    AliceLower = escalus_utils:jid_to_lower(AliceBin),
    BobLower = escalus_utils:jid_to_lower(BobBin),
    RoomName = <<"first room">>,
    RoomSubject = <<"Room about nothing">>,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(MUCServer, RoomName, RoomSubject, AliceBin),
    RoomJIDBin = jid:to_binary(RoomJID),
    Res = user_get_room_config(Alice, jid:to_binary(RoomJID), Config),
    % Owner can get a config
    ?assertEqual(#{<<"jid">> => RoomJIDBin, <<"subject">> => RoomSubject, <<"name">> => RoomName,
                    <<"options">> => [#{<<"key">> => <<"background">>, <<"value">> => <<>>},
                                      #{<<"key">> => <<"music">>, <<"value">> => <<>>},
                                      #{<<"key">> => <<"roomname">>, <<"value">> => RoomName},
                                      #{<<"key">> => <<"subject">>, <<"value">> => RoomSubject}],
                    <<"participants">> => [#{<<"jid">> => AliceLower,
                                             <<"affiliation">> => <<"OWNER">>}]},
                 get_ok_value(?GET_ROOM_CONFIG_PATH, Res)),
    % Try with a non-existent domain
    Res2 = user_get_room_config(Alice, make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)),
    % Try with a non-existent room
    Res3 = user_get_room_config(Alice, make_bare_jid(?UNKNOWN, MUCServer), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)),
    % User that is not a member cannot get a room config
    Res4 = user_get_room_config(Bob, jid:to_binary(RoomJID), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4), <<"not occupy this room">>)),
    % Member can get a config
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    Res5 = user_get_room_config(Bob, jid:to_binary(RoomJID), Config),
    ?assertEqual(#{<<"jid">> => RoomJIDBin, <<"subject">> => RoomSubject, <<"name">> => RoomName,
                    <<"options">> => [#{<<"key">> => <<"background">>, <<"value">> => <<>>},
                                      #{<<"key">> => <<"music">>, <<"value">> => <<>>},
                                      #{<<"key">> => <<"roomname">>, <<"value">> => RoomName},
                                      #{<<"key">> => <<"subject">>, <<"value">> => RoomSubject}],
                    <<"participants">> => [#{<<"jid">> => AliceLower,
                                             <<"affiliation">> => <<"OWNER">>},
                                           #{<<"jid">> => BobLower,
                                             <<"affiliation">> => <<"MEMBER">>}]},
                 get_ok_value(?GET_ROOM_CONFIG_PATH, Res5)).

user_blocking_list(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun user_blocking_list_story/3).

user_blocking_list_story(Config, Alice, Bob) ->
    BobBin = escalus_client:full_jid(Bob),
    BobShortBin = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    {ok, #{jid := RoomJID}} = create_room(?config(muc_light_host, Config),
                                          <<"room">>, <<"subject">>, BobBin),
    RoomBin = jid:to_binary(RoomJID),
    Res = user_get_blocking(Alice, Config),
    ?assertMatch([], get_ok_value(?GET_BLOCKING_LIST_PATH, Res)),
    Res2 = user_set_blocking(Alice, [{<<"USER">>, <<"DENY">>, BobBin}], Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SET_BLOCKING_LIST_PATH, Res2),
                                          <<"successfully">>)),
    Res3 = user_get_blocking(Alice, Config),
    ?assertEqual([#{<<"entityType">> => <<"USER">>,
                    <<"action">> => <<"DENY">>,
                    <<"entity">> => BobShortBin}],
                 get_ok_value(?GET_BLOCKING_LIST_PATH, Res3)),
    Res4 = user_set_blocking(Alice, [{<<"USER">>, <<"ALLOW">>, BobBin},
                                     {<<"ROOM">>, <<"DENY">>, jid:to_binary(RoomJID)}], Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SET_BLOCKING_LIST_PATH, Res4),
                                          <<"successfully">>)),
    Res5 = user_get_blocking(Alice, Config),
    ?assertEqual([#{<<"entityType">> => <<"ROOM">>,
                    <<"action">> => <<"DENY">>,
                    <<"entity">> => RoomBin}],
                 get_ok_value(?GET_BLOCKING_LIST_PATH, Res5)).

%% Domain admin test cases

domain_admin_create_room_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {alice_bis, 1}],
                                    fun domain_admin_create_room_no_permission_story/3).

domain_admin_create_room_no_permission_story(Config, Alice, AliceBis) ->
    AliceBin = escalus_client:short_jid(Alice),
    AliceBisBin = escalus_client:short_jid(AliceBis),
    InvalidUser = make_bare_jid(?UNKNOWN, ?UNKNOWN_DOMAIN),
    MucServer = ?config(muc_light_host, Config),
    ExternalMucServer = ?config(secondary_muc_light_host, Config),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    % Try with a non-existent domain
    Res = create_room(MucServer, Name, InvalidUser, Subject, null, Config),
    get_unauthorized(Res),
    % Try with an external domain
    Res2 = create_room(MucServer, Name, AliceBisBin, Subject, null, Config),
    get_unauthorized(Res2),
    % Try to create a room at an external domain
    Res3 = create_room(ExternalMucServer, Name, AliceBin, Subject, null, Config),
    get_unauthorized(Res3).

domain_admin_create_identified_room_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_create_identified_room_no_permission_story/2).

domain_admin_create_identified_room_no_permission_story(Config, AliceBis) ->
    AliceBisBin = escalus_client:short_jid(AliceBis),
    InvalidUser = make_bare_jid(?UNKNOWN, ?UNKNOWN_DOMAIN),
    MucServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    SchemaEndpoint = atom_to_binary(?config(schema_endpoint, Config)),
    Protocol = atom_to_binary(?config(protocol, Config)),
    Id = <<"my_room_", SchemaEndpoint/binary, "_", Protocol/binary>>,
    % Try with a non-existent domain
    Res = create_room(MucServer, Name, InvalidUser, Subject, Id, Config),
    get_unauthorized(Res),
    % Try with an external domain
    Res2 = create_room(MucServer, Name, AliceBisBin, Subject, Id, Config),
    get_unauthorized(Res2).

domain_admin_change_room_config_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun domain_admin_change_room_config_no_permission_story/3).

domain_admin_change_room_config_no_permission_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    MUCServer = ?config(secondary_muc_light_host, Config),
    RoomName = <<"first room">>,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    RoomJIDBin = jid:to_binary(RoomJID),
    Res = change_room_configuration(RoomJIDBin, AliceBin, RoomName, <<"subject">>, Config),
    get_unauthorized(Res),
    % Try to change the config with an external domain
    Res2 = change_room_configuration(
            make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), AliceBin, RoomName, <<"subject2">>, Config),
    get_unauthorized(Res2).

domain_admin_invite_user_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}, {bob, 1}],
                                    fun domain_admin_invite_user_no_permission_story/3).

domain_admin_invite_user_no_permission_story(Config, AliceBis, Bob) ->
    AliceBisBin = escalus_client:short_jid(AliceBis),
    BobBin = escalus_client:short_jid(Bob),
    MUCServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    {ok, #{jid := RoomJID}} = create_room(MUCServer, Name, <<"subject2">>, AliceBisBin),
    Res = invite_user(jid:to_binary(RoomJID), AliceBisBin, BobBin, Config),
    get_unauthorized(Res).

domain_admin_delete_room_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun domain_admin_delete_room_no_permission_story/2).

domain_admin_delete_room_no_permission_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    RoomName = <<"first room">>,
    MUCServer = ?config(secondary_muc_light_host, Config),
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    RoomJIDBin = jid:to_binary(RoomJID),
    Res = delete_room(RoomJIDBin, Config),
    get_unauthorized(Res),
    % Try with a non-existent domain
    Res2 = delete_room(make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), Config),
    get_unauthorized(Res2).

domain_admin_kick_user_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun domain_admin_kick_user_no_permission_story/3).

domain_admin_kick_user_no_permission_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    RoomName = <<"first room">>,
    MUCServer = ?config(secondary_muc_light_host, Config),
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    RoomJIDBin = jid:to_binary(RoomJID),
    ?assertEqual(2, length(get_room_aff(RoomJID))),
    Res = kick_user(RoomJIDBin, BobBin, Config),
    get_unauthorized(Res),
    % Try with a non-existent domain
    Res2 = kick_user(make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), BobBin, Config),
    get_unauthorized(Res2).

domain_admin_send_message_to_room_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}, {bob, 1}],
                                    fun domain_admin_send_message_to_room_no_permission_story/3).

domain_admin_send_message_to_room_no_permission_story(Config, AliceBis, Bob) ->
    AliceBisBin = escalus_client:short_jid(AliceBis),
    InvalidUser = make_bare_jid(?UNKNOWN, ?UNKNOWN_DOMAIN),
    BobBin = escalus_client:short_jid(Bob),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    MsgBody = <<"Hello there!">>,
    {ok, #{jid := RoomJID}} = create_room(MUCServer, RoomName, <<"subject">>, AliceBisBin),
    {ok, _} = invite_user(RoomJID, AliceBisBin, BobBin),
    % Try with a non-existent domain
    Res = send_message_to_room(jid:to_binary(RoomJID), InvalidUser, MsgBody, Config),
    get_unauthorized(Res),
    % Try with an external domain room
    Res2 = send_message_to_room(jid:to_binary(RoomJID), AliceBisBin, MsgBody, Config),
    get_unauthorized(Res2).

domain_admin_get_room_messages_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun domain_admin_get_room_messages_no_permission_story/2).

domain_admin_get_room_messages_no_permission_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MUCServer = ?config(secondary_muc_light_host, Config),
    RoomName = <<"first room">>,
    Limit = 40,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    RoomJIDBin = jid:to_binary(RoomJID),
    Res = get_room_messages(RoomJIDBin, Limit, null, Config),
    get_unauthorized(Res),
    % Try with a non-existent domain
    Res2 = get_room_messages(make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), Limit, null, Config),
    get_unauthorized(Res2).

domain_admin_list_user_rooms_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_list_user_rooms_no_permission_story/2).

domain_admin_list_user_rooms_no_permission_story(Config, AliceBis) ->
    AliceBisBin = escalus_client:short_jid(AliceBis),
    InvalidUser = make_bare_jid(?UNKNOWN, ?UNKNOWN_DOMAIN),
    % Try with a non-existent domain
    Res = list_user_rooms(InvalidUser, Config),
    get_unauthorized(Res),
    % Try with an external domain
    Res2 = list_user_rooms(AliceBisBin, Config),
    get_unauthorized(Res2).

domain_admin_list_room_users_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun domain_admin_list_room_users_story_no_permission/2).

domain_admin_list_room_users_story_no_permission(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MUCServer = ?config(secondary_muc_light_host, Config),
    RoomName = <<"first room">>,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    RoomJIDBin = jid:to_binary(RoomJID),
    Res = list_room_users(RoomJIDBin, Config),
    get_unauthorized(Res),
    % Try with an external domain
    Res2 = list_room_users(make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), Config),
    get_unauthorized(Res2).

domain_admin_get_room_config_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun domain_admin_get_room_config_no_permission_story/2).

domain_admin_get_room_config_no_permission_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MUCServer = ?config(secondary_muc_light_host, Config),
    RoomName = <<"first room">>,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    RoomJIDBin = jid:to_binary(RoomJID),
    Res = get_room_config(RoomJIDBin, Config),
    get_unauthorized(Res),
    % Try with a non-existent domain
    Res2 = get_room_config(make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), Config),
    get_unauthorized(Res2).

domain_admin_blocking_list_no_permission(Config) ->
    escalus:fresh_story_with_config(Config, [{alice_bis, 1}],
                                    fun domain_admin_blocking_list_no_permission_story/2).

domain_admin_blocking_list_no_permission_story(Config, AliceBis) ->
    AliceBisBin = escalus_client:full_jid(AliceBis),
    InvalidUser = make_bare_jid(?UNKNOWN, ?UNKNOWN_DOMAIN),
    % Try with a non-existent user
    Res = get_user_blocking(InvalidUser, Config),
    get_unauthorized(Res),
    Res2 = set_blocking(InvalidUser, [], Config),
    get_unauthorized(Res2),
    % Try with an external domain user
    Res3 = get_user_blocking(AliceBisBin, Config),
    get_unauthorized(Res3),
    Res4 = set_blocking(AliceBisBin, [], Config),
    get_unauthorized(Res4).

%% Admin test cases

admin_blocking_list(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun admin_blocking_list_story/3).

admin_blocking_list_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:full_jid(Alice),
    BobBin = escalus_client:full_jid(Bob),
    BobShortBin = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    Res = get_user_blocking(AliceBin, Config),
    ?assertMatch([], get_ok_value(?GET_BLOCKING_LIST_PATH, Res)),
    Res2 = set_blocking(AliceBin, [{<<"USER">>, <<"DENY">>, BobBin}], Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SET_BLOCKING_LIST_PATH, Res2),
                                          <<"successfully">>)),
    Res3 = get_user_blocking(AliceBin, Config),
    ?assertEqual([#{<<"entityType">> => <<"USER">>,
                    <<"action">> => <<"DENY">>,
                    <<"entity">> => BobShortBin}],
                 get_ok_value(?GET_BLOCKING_LIST_PATH, Res3)),
    Res4 = set_blocking(AliceBin, [{<<"USER">>, <<"ALLOW">>, BobBin}], Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SET_BLOCKING_LIST_PATH, Res4),
                                          <<"successfully">>)),
    Res5 = get_user_blocking(AliceBin, Config),
    ?assertMatch([], get_ok_value(?GET_BLOCKING_LIST_PATH, Res5)).

admin_blocking_list_null(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_blocking_list_null_story/3).

admin_blocking_list_null_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:full_jid(Alice),
    BobBin = escalus_client:full_jid(Bob),
    BobShortBin = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
    Res = get_user_blocking(AliceBin, Config),
    ?assertMatch([], get_ok_value(?GET_BLOCKING_LIST_PATH, Res)),
    Res2 = set_blocking(AliceBin, [{<<"USER">>, null, BobBin}], Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SET_BLOCKING_LIST_PATH, Res2),
                                            <<"successfully">>)),
    Res3 = get_user_blocking(AliceBin, Config),
    ?assertEqual([#{<<"entityType">> => <<"USER">>,
                    <<"action">> => <<"DENY">>,
                    <<"entity">> => BobShortBin}],
                    get_ok_value(?GET_BLOCKING_LIST_PATH, Res3)),
    Res4 = set_blocking(AliceBin, [{<<"USER">>, <<"ALLOW">>, BobBin}], Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SET_BLOCKING_LIST_PATH, Res4),
                                            <<"successfully">>)),
    Res5 = get_user_blocking(AliceBin, Config),
    ?assertMatch([], get_ok_value(?GET_BLOCKING_LIST_PATH, Res5)).

admin_blocking_list_errors(Config) ->
    InvalidUser = make_bare_jid(?UNKNOWN, ?UNKNOWN_DOMAIN),
    Res = get_user_blocking(InvalidUser, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"domain does not exist">>)),
    Res2 = set_blocking(InvalidUser, [], Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"domain does not exist">>)),
    BadUser = make_bare_jid(<<"baduser">>, domain_helper:domain()),
    Res3 = get_user_blocking(BadUser, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"user does not exist">>)),
    Res4 = set_blocking(BadUser, [], Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4), <<"user does not exist">>)).

admin_create_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_create_room_story/2).

admin_create_room_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    AliceBinLower = escalus_utils:jid_to_lower(AliceBin),
    MucServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    Res = create_room(MucServer, Name, AliceBin, Subject, null, Config),
    #{<<"jid">> := JID, <<"name">> := Name, <<"subject">> := Subject,
      <<"participants">> := Participants} = get_ok_value(?CREATE_ROOM_PATH, Res),
    ?assertMatch(#jid{lserver = MucServer}, jid:from_binary(JID)),
    ?assertEqual([#{<<"jid">> => AliceBinLower, <<"affiliation">> => <<"OWNER">>}], Participants),
    % Try with a non-existent user
    BadUser = make_bare_jid(<<"baduser">>, domain_helper:domain()),
    Res1 = create_room(?config(muc_light_host, Config), null, BadUser, null, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res1), <<"user does not exist">>)).

admin_create_room_non_existent_domain(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}]),
    AliceBin = escalus_users:get_jid(Config1, alice),
    MucServer = ?config(muc_light_host, Config),
    Res1 = create_room(?UNKNOWN_DOMAIN, null, AliceBin, null, null, Config1),
    ?assertMatch({_, _}, binary:match(get_err_msg(Res1), <<"MUC Light server not found">>)),
    BadUser = make_bare_jid(<<"baduser">>, ?UNKNOWN_DOMAIN),
    Res2 = create_room(MucServer, null, BadUser, null, null, Config1),
    ?assertMatch({_, _}, binary:match(get_err_msg(Res2), <<"User's domain does not exist">>)).

admin_create_room_with_unprepped_domain(Config) ->
    FreshConfig = escalus_fresh:create_users(Config, [{alice, 1}]),
    AliceBin = escalus_users:get_jid(FreshConfig, alice),
    MucServer = ?config(muc_light_host, Config),
    Name = <<"room with unprepped domain">>,
    Subject = <<"testing">>,
    Res = create_room(unprep(MucServer), Name, AliceBin, Subject, null, Config),
    #{<<"jid">> := JID, <<"name">> := Name, <<"subject">> := Subject} =
        get_ok_value(?CREATE_ROOM_PATH, Res),
    ?assertMatch(#jid{lserver = MucServer}, jid:from_binary_noprep(JID)).

admin_create_room_with_custom_fields(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_create_room_with_custom_fields_story/2).

admin_create_room_with_custom_fields_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    AliceBinLower = escalus_utils:jid_to_lower(AliceBin),
    MucServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    Options = #{<<"background">> => <<"red">>},
    Opts = [#{<<"key">> => <<"background">>, <<"value">> => <<"red">>},
            #{<<"key">> => <<"roomname">>, <<"value">> => Name},
            #{<<"key">> => <<"subject">>, <<"value">> => Subject}],
    Res = create_room_with_custom_fields(MucServer, Name, AliceBin, Subject, null, Config, Options),
    #{<<"jid">> := JID, <<"name">> := Name, <<"subject">> := Subject,
      <<"participants">> := Participants, <<"options">> := Opts} = get_ok_value(?CREATE_ROOM_PATH, Res),
    ?assertMatch(#jid{lserver = MucServer}, jid:from_binary(JID)),
    ?assertEqual([#{<<"jid">> => AliceBinLower, <<"affiliation">> => <<"OWNER">>}], Participants).

admin_create_identified_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_create_identified_room_story/2).

admin_create_identified_room_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MucServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    SchemaEndpoint = atom_to_binary(?config(schema_endpoint, Config)),
    Protocol = atom_to_binary(?config(protocol, Config)),
    Id = <<"my_room_", SchemaEndpoint/binary, "_", Protocol/binary>>,
    Res = create_room(MucServer, Name, AliceBin, Subject, Id, Config),
    #{<<"jid">> := JID, <<"name">> := Name, <<"subject">> := Subject} =
        get_ok_value(?CREATE_ROOM_PATH, Res),
    ?assertMatch(#jid{luser = Id, lserver = MucServer}, jid:from_binary(JID)),
    % Create a room with an existing ID
    Res1 = create_room(MucServer, <<"snd room">>, AliceBin, Subject, Id, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res1), <<"already exists">>)),
    % Try with a non-existent user
    BadUser = make_bare_jid(<<"baduser">>, domain_helper:domain()),
    Res2 = create_room(?config(muc_light_host, Config), null, BadUser, null, Id, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"user does not exist">>)).

admin_create_identified_room_non_existent_domain(Config) ->
    Config1 = escalus_fresh:create_users(Config, [{alice, 1}]),
    AliceBin = escalus_users:get_jid(Config1, alice),
    Id = <<"bad_room">>,
    MucServer = ?config(muc_light_host, Config),
    Res1 = create_room(?UNKNOWN_DOMAIN, null, AliceBin, null, Id, Config1),
    ?assertMatch({_, _}, binary:match(get_err_msg(Res1), <<"MUC Light server not found">>)),
    BadUser = make_bare_jid(<<"baduser">>, ?UNKNOWN_DOMAIN),
    Res2 = create_room(MucServer, null, BadUser, null, Id, Config1),
    ?assertMatch({_, _}, binary:match(get_err_msg(Res2), <<"User's domain does not exist">>)).

admin_create_room_with_unprepped_id(Config) ->
    FreshConfig = escalus_fresh:create_users(Config, [{alice, 1}]),
    AliceBin = escalus_users:get_jid(FreshConfig, alice),
    MucServer = ?config(muc_light_host, FreshConfig),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    Id = <<"room_with_unprepped_id">>,
    Res = create_room(unprep(MucServer), Name, AliceBin, Subject, unprep(Id), FreshConfig),
    #{<<"jid">> := JID, <<"name">> := Name, <<"subject">> := Subject} =
        get_ok_value(?CREATE_ROOM_PATH, Res),
    ?assertMatch(#jid{luser = Id, lserver = MucServer}, jid:from_binary_noprep(JID)).

admin_change_room_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_change_room_config_story/2).

admin_change_room_config_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MUCServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    % Create a new room
    {ok, #{jid := RoomJID}} = create_room(MUCServer, Name, Subject, AliceBin),
    % Try to change the room configuration
    Name2 = <<"changed room">>,
    Subject2 = <<"not testing">>,
    Res = change_room_configuration(jid:to_binary(RoomJID), AliceBin, Name2, Subject2, Config),
    ?assertMatch(#{<<"name">> := Name2, <<"subject">> := Subject2},
                 get_ok_value(?CHANGE_CONFIG_PATH, Res)).

admin_change_room_config_with_custom_fields(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_change_room_config_with_custom_fields_story/2).

admin_change_room_config_with_custom_fields_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MUCServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    Opts = #{<<"background">> => <<"red">>},
    % Create a new room
    Res = create_room_with_custom_fields(MUCServer, Name, AliceBin, Subject, null, Config, Opts),
    #{<<"jid">> := RoomJID} = get_ok_value(?CREATE_ROOM_PATH, Res),
    % Try to change the room configuration
    Name2 = <<"changed room">>,
    Subject2 = <<"not testing">>,
    Opts2 = #{<<"music">> => <<"sad">>},
    Res2 = change_room_configuration_with_custom_fields(jid:to_binary(RoomJID), AliceBin, Name2, Subject2, Config, Opts2),
    %% It overwrites old config for all fields
    Opts3 = [% #{<<"key">> => <<"background">>, <<"value">> => <<"red">>},
             #{<<"key">> => <<"music">>, <<"value">> => <<"sad">>},
             #{<<"key">> => <<"roomname">>, <<"value">> => Name2},
             #{<<"key">> => <<"subject">>, <<"value">> => Subject2}],
    ?assertMatch(#{<<"name">> := Name2, <<"subject">> := Subject2, <<"options">> := Opts3},
                 get_ok_value(?CHANGE_CONFIG_PATH, Res2)).

admin_change_room_config_errors(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}, {kate, 1}],
                                    fun admin_change_room_config_errors_story/4).

admin_change_room_config_errors_story(Config, Alice, Bob, Kate) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    {ok, #{jid := RoomJID}} = create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    % Try to change the config of the non-existent room
    Res = change_room_configuration(
             make_bare_jid(<<"unknown">>, MUCServer), AliceBin, RoomName, <<"subject2">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)),
    % Try to change the config by the non-existent user
    Res2 = change_room_configuration(
             jid:to_binary(RoomJID), <<"wrong-user@wrong-domain">>, RoomName, <<"subject2">>,
             Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"User's domain does not exist">>)),
    % Try to change a config by the user without permission
    Res3 = change_room_configuration(
             jid:to_binary(RoomJID), BobBin, RoomName, <<"subject2">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3),
                                          <<"does not have permission to change">>)),
    % Try to change a config by a user not occupying the room
    KateBin = escalus_client:short_jid(Kate),
    Res4 = change_room_configuration(
             jid:to_binary(RoomJID), KateBin, RoomName, <<"subject2">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4), <<"does not occupy">>)).

admin_change_room_config_non_existent_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_change_room_config_non_existent_domain_story/3).

admin_change_room_config_non_existent_domain_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    Res = change_room_configuration(
            make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), AliceBin, RoomName, <<"subject2">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_invite_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun admin_invite_user_story/3).

admin_invite_user_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    Domain = escalus_client:server(Alice),
    MUCServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    {ok, #{jid := RoomJID}} = create_room(MUCServer, Name, <<"subject2">>, AliceBin),

    Res = invite_user(jid:to_binary(RoomJID), AliceBin, BobBin, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?INVITE_USER_PATH, Res),
                                          <<"successfully">>)),
    BobName = escalus_utils:jid_to_lower(escalus_client:username(Bob)),
    AliceName = escalus_utils:jid_to_lower(escalus_client:username(Alice)),
    ExpectedAff = lists:sort([{{AliceName, Domain}, owner},
                              {{BobName, Domain}, member}]),
    ?assertMatch(ExpectedAff, lists:sort(get_room_aff(RoomJID))).

admin_invite_user_errors(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_invite_user_errors_story/3).

admin_invite_user_errors_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    MUCServer = ?config(muc_light_host, Config),
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(MUCServer, <<"first room">>, <<"subject">>, AliceBin),
    % Try to invite a user to not existing room
    Res = invite_user(make_bare_jid(?UNKNOWN, MUCServer), AliceBin, BobBin, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)),
    % User without rooms tries to invite a user
    Res2 = invite_user(jid:to_binary(RoomJID), BobBin, AliceBin, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"does not occupy this room">>)),
    % Try with a non-existent domain
    Res3 = invite_user(make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), AliceBin, BobBin, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"not found">>)).

admin_delete_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_delete_room_story/2).

admin_delete_room_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MUCServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    {ok, #{jid := RoomJID} = RoomInfo} = create_room(MUCServer, Name, <<"subject">>, AliceBin),
    Res = delete_room(jid:to_binary(RoomJID), Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?DELETE_ROOM_PATH, Res),
                                          <<"successfully">>)),
    ?assertEqual({error, not_exists}, get_room_info(RoomJID), RoomInfo),
    % Try with a non-existent room
    Res2 = delete_room(make_bare_jid(?UNKNOWN, MUCServer), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)).

admin_delete_room_non_existent_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_delete_room_non_existent_domain_story/2).

admin_delete_room_non_existent_domain_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MUCServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    {ok, #{jid := #jid{luser = RoomID}}} =
        create_room(MUCServer, Name, <<"subject">>, AliceBin),
    Res = delete_room(make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_kick_user(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}], fun admin_kick_user_story/3).

admin_kick_user_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    {ok, #{jid := RoomJID}} = create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    ?assertEqual(2, length(get_room_aff(RoomJID))),
    Res = kick_user(jid:to_binary(RoomJID), BobBin, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?KICK_USER_PATH, Res),
                                          <<"successfully">>)),
    ?assertEqual(1, length(get_room_aff(RoomJID))).

admin_send_message_to_room(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_send_message_to_room_story/3).

admin_send_message_to_room_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    MsgBody = <<"Hello there!">>,
    {ok, #{jid := RoomJID}} = create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, _} = invite_user(RoomJID, AliceBin, BobBin),
    Res = send_message_to_room(jid:to_binary(RoomJID), AliceBin, MsgBody, Config),
    ?assertNotEqual(nomatch, binary:match(get_ok_value(?SEND_MESSAGE_PATH, Res),
                                          <<"successfully">>)),
    [_, Msg] = escalus:wait_for_stanzas(Bob, 2),
    escalus:assert(is_message, Msg).

admin_send_message_to_room_errors(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
                                    fun admin_send_message_to_room_errors_story/3).

admin_send_message_to_room_errors_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    MUCServer = ?config(muc_light_host, Config),
    MsgBody = <<"Hello there!">>,
    {ok, #{jid := #jid{luser = ARoomID} = ARoomJID}} =
        create_room(MUCServer, <<"alice room">>, <<"subject">>, AliceBin),
    % Try with a non-existent domain
    Res2 = send_message_to_room(make_bare_jid(ARoomID, ?UNKNOWN_DOMAIN), AliceBin, MsgBody, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)),
    % Try with a user without rooms
    Res3 = send_message_to_room(jid:to_binary(ARoomJID), BobBin, MsgBody, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res3), <<"does not occupy this room">>)),
    % Try with a room not occupied by this user
    {ok, #{jid := _RoomJID2}} = create_room(MUCServer, <<"bob room">>, <<"subject">>, BobBin),
    Res4 = send_message_to_room(jid:to_binary(ARoomJID), BobBin, MsgBody, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4), <<"does not occupy this room">>)).

admin_get_room_messages(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_get_room_messages_story/2).

admin_get_room_messages_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    %Domain = escalus_client:server(Alice),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    RoomName2 = <<"second room">>,
    {ok, #{jid := #jid{luser = RoomID} = RoomJID}} =
        create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, _} = create_room(MUCServer, RoomName2, <<"subject">>, AliceBin),
    Message = <<"Hello friends">>,
    send_message_to_room(RoomJID, jid:from_binary(AliceBin), Message),
    mam_helper:wait_for_room_archive_size(MUCServer, RoomID, 1),
    % Get messages so far
    Limit = 40,
    Res = get_room_messages(jid:to_binary(RoomJID), Limit, null, Config),
    #{<<"stanzas">> := [#{<<"stanza">> := StanzaXML}], <<"limit">> := Limit} =
        get_ok_value(?GET_MESSAGES_PATH, Res),
    ?assertMatch({ok, #xmlel{name = <<"message">>}}, exml:parse(StanzaXML)),
    % Get messages before the given date and time
    Before = <<"2022-02-17T04:54:13+00:00">>,
    Res2 = get_room_messages(jid:to_binary(RoomJID), null, Before, Config),
    ?assertMatch(#{<<"stanzas">> := [], <<"limit">> := 50}, get_ok_value(?GET_MESSAGES_PATH, Res2)),
    % Try to pass too big page size value
    Res3 = get_room_messages(jid:to_binary(RoomJID), 51, Before, Config),
    ?assertMatch(#{<<"limit">> := 50}, get_ok_value(?GET_MESSAGES_PATH, Res3)),
    % Try with a non-existent room
    Res4 = get_room_messages(make_bare_jid(<<"badroom">>, MUCServer), null, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res4), <<"Room not found">>)).

admin_get_room_messages_non_existent_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_get_room_messages_non_existent_domain_story/2).

admin_get_room_messages_non_existent_domain_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    {ok, #{jid := #jid{luser = RoomID}}} =
    create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    Limit = 40,
    Res = get_room_messages(make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), Limit, null, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_list_user_rooms(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_list_user_rooms_story/2).

admin_list_user_rooms_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    Domain = escalus_client:server(Alice),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    RoomName2 = <<"second room">>,
    {ok, #{jid := RoomJID}} = create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    {ok, #{jid := RoomJID2}} = create_room(MUCServer, RoomName2, <<"subject">>, AliceBin),
    Res = list_user_rooms(AliceBin, Config),
    ?assertEqual(lists:sort([jid:to_binary(RoomJID), jid:to_binary(RoomJID2)]),
                 lists:sort(get_ok_value(?LIST_USER_ROOMS_PATH, Res))),
    % Try with a non-existent user
    Res2 = list_user_rooms(<<"not-exist@", Domain/binary>>, Config),
    ?assertMatch({_, _}, binary:match(get_err_msg(Res2), <<"does not exist">>)).

admin_list_user_rooms_non_existent_domain(Config) ->
    Res = list_user_rooms(<<"not-exist@not-exist">>, Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"does not exist">>)).

admin_list_room_users(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_list_room_users_story/2).

admin_list_room_users_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    AliceLower = escalus_utils:jid_to_lower(AliceBin),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    {ok, #{jid := RoomJID}} = create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    Res = list_room_users(jid:to_binary(RoomJID), Config),
    ?assertEqual([#{<<"jid">> => AliceLower, <<"affiliation">> => <<"OWNER">>}],
                 get_ok_value(?LIST_ROOM_USERS_PATH, Res)),
    % Try with a non-existent room
    Res2 = list_room_users(make_bare_jid(?UNKNOWN, MUCServer), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)).

admin_list_room_users_non_existent_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_list_room_users_non_existent_domain_story/2).

admin_list_room_users_non_existent_domain_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    {ok, #{jid := RoomJID}} = create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    Res = list_room_users(make_bare_jid(RoomJID#jid.luser, ?UNKNOWN_DOMAIN), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

admin_get_room_config(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}], fun admin_get_room_config_story/2).

admin_get_room_config_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    AliceLower = escalus_utils:jid_to_lower(AliceBin),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    RoomSubject = <<"Room about nothing">>,
    {ok, #{jid := #jid{luser = _RoomID} = RoomJID}} =
        create_room(MUCServer, RoomName, RoomSubject, AliceBin),
    RoomJIDBin = jid:to_binary(RoomJID),
    Res = get_room_config(RoomJIDBin, Config),
    ?assertEqual(#{<<"jid">> => RoomJIDBin, <<"subject">> => RoomSubject, <<"name">> => RoomName,
                    <<"options">> => [#{<<"key">> => <<"background">>, <<"value">> => <<>>},
                                      #{<<"key">> => <<"music">>, <<"value">> => <<>>},
                                      #{<<"key">> => <<"roomname">>, <<"value">> => RoomName},
                                      #{<<"key">> => <<"subject">>, <<"value">> => RoomSubject}],
                    <<"participants">> => [#{<<"jid">> => AliceLower,
                                             <<"affiliation">> => <<"OWNER">>}]},
                 get_ok_value([data, muc_light, getRoomConfig], Res)),
    % Try with a non-existent room
    Res2 = get_room_config(make_bare_jid(?UNKNOWN, MUCServer), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res2), <<"not found">>)).

admin_get_room_config_non_existent_domain(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
                                    fun admin_get_room_config_non_existent_domain_story/2).

admin_get_room_config_non_existent_domain_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MUCServer = ?config(muc_light_host, Config),
    RoomName = <<"first room">>,
    {ok, #{jid := #jid{luser = RoomID}}} =
        create_room(MUCServer, RoomName, <<"subject">>, AliceBin),
    Res = get_room_config(make_bare_jid(RoomID, ?UNKNOWN_DOMAIN), Config),
    ?assertNotEqual(nomatch, binary:match(get_err_msg(Res), <<"not found">>)).

%% User mod_muc_light not configured test cases
user_create_room_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_create_room_muc_light_not_configured_story/2).

user_create_room_muc_light_not_configured_story(Config, Alice) ->
    MucServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    Res = user_create_room(Alice, MucServer, Name, Subject, null, Config),
    ?assertEqual(<<"muc_server_not_found">>, get_err_code(Res)).

user_change_room_config_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_change_room_config_muc_light_not_configured_story/2).

user_change_room_config_muc_light_not_configured_story(Config, Alice) ->
    Name = <<"changed room">>,
    Subject = <<"not testing">>,
    Res = user_change_room_configuration(Alice, get_room_name(), Name, Subject, Config),
    get_not_loaded(Res).

user_invite_user_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
        fun user_invite_user_muc_light_not_configured_story/3).

user_invite_user_muc_light_not_configured_story(Config, Alice, Bob) ->
    BobBin = escalus_client:short_jid(Bob),
    Res = user_invite_user(Alice, get_room_name(), BobBin, Config),
    get_not_loaded(Res).

user_delete_room_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_delete_room_muc_light_not_configured_story/2).

user_delete_room_muc_light_not_configured_story(Config, Alice) ->
    Res = user_delete_room(Alice, get_room_name(), Config),
    get_not_loaded(Res).

user_kick_user_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_kick_user_muc_light_not_configured_story/2).

user_kick_user_muc_light_not_configured_story(Config, Alice) ->
    Res = user_kick_user(Alice, get_room_name(), null, Config),
    get_not_loaded(Res).

user_send_message_to_room_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_send_message_to_room_muc_light_not_configured_story/2).

user_send_message_to_room_muc_light_not_configured_story(Config, Alice) ->
    MsgBody = <<"Hello there!">>,
    Res = user_send_message_to_room(Alice, get_room_name(), MsgBody, Config),
    get_not_loaded(Res).

user_get_room_messages_muc_light_or_mam_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_get_room_messages_muc_light_or_mam_not_configured_story/2).

user_get_room_messages_muc_light_or_mam_not_configured_story(Config, Alice) ->
    Before = <<"2022-02-17T04:54:13+00:00">>,
    Res = user_get_room_messages(Alice, get_room_name(), 51, Before, Config),
    get_not_loaded(Res).

user_list_rooms_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_list_rooms_muc_light_not_configured_story/2).

user_list_rooms_muc_light_not_configured_story(Config, Alice) ->
    Res = user_list_rooms(Alice, Config),
    get_not_loaded(Res).

user_list_room_users_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_list_room_users_muc_light_not_configured_story/2).

user_list_room_users_muc_light_not_configured_story(Config, Alice) ->
    Res = user_list_room_users(Alice, get_room_name(), Config),
    get_not_loaded(Res).

user_get_room_config_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun user_get_room_config_muc_light_not_configured_story/2).

user_get_room_config_muc_light_not_configured_story(Config, Alice) ->
    Res = user_get_room_config(Alice, get_room_name(), Config),
    get_not_loaded(Res).

user_blocking_list_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
         fun user_blocking_list_muc_light_not_configured_story/3).

user_blocking_list_muc_light_not_configured_story(Config, Alice, Bob) ->
    BobBin = escalus_client:full_jid(Bob),
    Res = user_get_blocking(Alice, Config),
    get_not_loaded(Res),
    Res2 = user_set_blocking(Alice, [{<<"USER">>, <<"DENY">>, BobBin}], Config),
    get_not_loaded(Res2).

%% Admin mod_muc_light not configured test cases

admin_create_room_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun admin_create_room_muc_light_not_configured_story/2).

admin_create_room_muc_light_not_configured_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MucServer = ?config(muc_light_host, Config),
    Name = <<"first room">>,
    Subject = <<"testing">>,
    Res = create_room(MucServer, Name, AliceBin, Subject, null, Config),
    ?assertEqual(<<"muc_server_not_found">>, get_err_code(Res)).

admin_invite_user_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
        fun admin_invite_user_muc_light_not_configured_story/3).

admin_invite_user_muc_light_not_configured_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:short_jid(Alice),
    BobBin = escalus_client:short_jid(Bob),
    Res = invite_user(get_room_name(), AliceBin, BobBin, Config),
    get_not_loaded(Res).

admin_change_room_config_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun admin_change_room_config_muc_light_not_configured_story/2).

admin_change_room_config_muc_light_not_configured_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    Name = <<"changed room">>,
    Subject = <<"not testing">>,
    Res = change_room_configuration(get_room_name(), AliceBin, Name, Subject, Config),
    get_not_loaded(Res).

admin_delete_room_muc_light_not_configured(Config) ->
    Res = delete_room(get_room_name(), Config),
    get_not_loaded(Res).

admin_kick_user_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{bob, 1}],
        fun admin_kick_user_muc_light_not_configured_story/2).

admin_kick_user_muc_light_not_configured_story(Config, Bob) ->
    BobBin = escalus_client:short_jid(Bob),
    Res = kick_user(get_room_name(), BobBin, Config),
    get_not_loaded(Res).

admin_send_message_to_room_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun admin_send_message_to_room_muc_light_not_configured_story/2).

admin_send_message_to_room_muc_light_not_configured_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    MsgBody = <<"Hello there!">>,
    Res = send_message_to_room(get_room_name(), AliceBin, MsgBody, Config),
    get_not_loaded(Res).

admin_get_room_messages_muc_light_or_mam_not_configured(Config) ->
    Limit = 40,
    Res = get_room_messages(get_room_name(), Limit, null, Config),
    get_not_loaded(Res).

admin_list_user_rooms_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}],
        fun admin_list_user_rooms_muc_light_not_configured_story/2).

admin_list_user_rooms_muc_light_not_configured_story(Config, Alice) ->
    AliceBin = escalus_client:short_jid(Alice),
    Res = list_user_rooms(AliceBin, Config),
    get_not_loaded(Res).

admin_list_room_users_muc_light_not_configured(Config) ->
    Res = list_room_users(get_room_name(), Config),
    get_not_loaded(Res).

admin_get_room_config_muc_light_not_configured(Config) ->
    Res = get_room_config(get_room_name(), Config),
    get_not_loaded(Res).

%% Helpers

get_room_name() ->
    Domain = domain_helper:domain(),
    <<"NON_EXISTING@", Domain/binary>>.

admin_blocking_list_muc_light_not_configured(Config) ->
    escalus:fresh_story_with_config(Config, [{alice, 1}, {bob, 1}],
        fun admin_blocking_list_muc_light_not_configured_story/3).

admin_blocking_list_muc_light_not_configured_story(Config, Alice, Bob) ->
    AliceBin = escalus_client:full_jid(Alice),
    BobBin = escalus_client:full_jid(Bob),
    Res = get_user_blocking(AliceBin, Config),
    get_not_loaded(Res),
    Res2 = set_blocking(AliceBin, [{<<"USER">>, <<"DENY">>, BobBin}], Config),
    get_not_loaded(Res2).

make_bare_jid(User, Server) ->
    JID = jid:make_bare(User, Server),
    jid:to_binary(JID).

send_message_to_room(RoomJID, SenderJID, Message) ->
    rpc(mim(), mod_muc_light_api, send_message, [RoomJID, SenderJID, Message]).

get_room_messages(ID, Domain) ->
    {ok, Messages} = rpc(mim(), mod_muc_light_api, get_room_messages, [Domain, ID]),
    Messages.

create_room(Domain, Name, Subject, CreatorBin) ->
    CreatorJID = jid:from_binary(CreatorBin),
    Config = #{<<"roomname">> => Name, <<"subject">> => Subject},
    rpc(mim(), mod_muc_light_api, create_room, [Domain, CreatorJID, Config]).

invite_user(RoomJID, SenderBin, RecipientBin) ->
    SenderJID = jid:from_binary(SenderBin),
    RecipientJID = jid:from_binary(RecipientBin),
    rpc(mim(), mod_muc_light_api, invite_to_room, [RoomJID, SenderJID, RecipientJID]).

get_room_info(error) ->
    arg_is_not_jid;
get_room_info(#jid{} = JID) ->
    HostType = domain_helper:host_type(),
    RoomUS = jid:to_lus(JID),
    rpc(mim(), mod_muc_light_db_backend, get_info, [HostType, RoomUS]).

get_room_aff(JID) ->
    {ok, _, Aff, _} = get_room_info(JID),
    Aff.

prepare_blocking_items_for_query(Items) ->
    [#{<<"entity">> => Who, <<"entityType">> => What,
       <<"action">> => Action} || {What, Action, Who} <- Items].

%% Commands

create_room(MUCDomain, Name, Owner, Subject, Id, Config) ->
    Vars = #{<<"mucDomain">> => MUCDomain, <<"name">> => Name, <<"owner">> => Owner,
             <<"subject">> => Subject, <<"id">> => Id},
    execute_command(<<"muc_light">>, <<"createRoom">>, Vars, Config).

create_room_with_custom_fields(MUCDomain, Name, Owner, Subject,
                               Id, Config, CustomFields) ->
    Vars = #{<<"mucDomain">> => MUCDomain, <<"name">> => Name, <<"owner">> => Owner,
             <<"subject">> => Subject, <<"id">> => Id,
             <<"options">> => format_options(CustomFields)},
    execute_command(<<"muc_light">>, <<"createRoom">>, Vars, Config).

change_room_configuration(RoomJID, OwnerJID, Name, Subject, Config) ->
    Vars = #{<<"room">> => RoomJID, <<"name">> => Name, <<"owner">> => OwnerJID,
             <<"subject">> => Subject},
    execute_command(<<"muc_light">>, <<"changeRoomConfiguration">>, Vars, Config).

change_room_configuration_with_custom_fields(RoomJID, OwnerJID, Name, Subject, Config, Opts) ->
    Vars = #{<<"room">> => RoomJID, <<"name">> => Name, <<"owner">> => OwnerJID,
             <<"subject">> => Subject, <<"options">> => format_options(Opts)},
    execute_command(<<"muc_light">>, <<"changeRoomConfiguration">>, Vars, Config).

invite_user(RoomJID, Sender, Recipient, Config) ->
    Vars = #{<<"room">> => RoomJID, <<"sender">> => Sender, <<"recipient">> => Recipient},
    execute_command(<<"muc_light">>, <<"inviteUser">>, Vars, Config).

kick_user(RoomJID, User, Config) ->
    Vars = #{<<"room">> => RoomJID, <<"user">> => User},
    execute_command(<<"muc_light">>, <<"kickUser">>, Vars, Config).

send_message_to_room(RoomJID, From, Body, Config) ->
    Vars = #{<<"room">> => RoomJID, <<"from">> => From, <<"body">> => Body},
    execute_command(<<"muc_light">>, <<"sendMessageToRoom">>, Vars, Config).

list_user_rooms(User, Config) ->
    Vars = #{<<"user">> => User},
    execute_command(<<"muc_light">>, <<"listUserRooms">>, Vars, Config).

delete_room(RoomJID, Config) ->
    Vars = #{<<"room">> => RoomJID},
    execute_command(<<"muc_light">>, <<"deleteRoom">>, Vars, Config).

get_room_messages(RoomJID, PageSize, Before, Config) ->
    Vars = #{<<"room">> => RoomJID, <<"pageSize">> => PageSize, <<"before">> => Before},
    execute_command(<<"muc_light">>, <<"getRoomMessages">>, Vars, Config).

list_room_users(RoomJID, Config) ->
    Vars = #{<<"room">> => RoomJID},
    execute_command(<<"muc_light">>, <<"listRoomUsers">>, Vars, Config).

get_room_config(RoomJID, Config) ->
    Vars = #{<<"room">> => RoomJID},
    execute_command(<<"muc_light">>, <<"getRoomConfig">>, Vars, Config).

get_user_blocking(UserJID, Config) ->
    Vars = #{<<"user">> => UserJID},
    execute_command(<<"muc_light">>, <<"getBlockingList">>, Vars, Config).

set_blocking(UserJID, Items, Config) ->
    Vars = #{<<"user">> => UserJID, <<"items">> => prepare_blocking_items_for_query(Items)},
    execute_command(<<"muc_light">>, <<"setBlockingList">>, Vars, Config).

user_create_room(User, MUCDomain, Name, Subject, Id, Config) ->
    Vars = #{<<"mucDomain">> => MUCDomain, <<"name">> => Name, <<"subject">> => Subject,
             <<"id">> => Id},
    execute_user_command(<<"muc_light">>, <<"createRoom">>, User, Vars, Config).

user_create_room_with_options(User, MUCDomain, Name, Subject, Id, CustomFields, Config) ->
    Vars = #{<<"mucDomain">> => MUCDomain, <<"name">> => Name, <<"subject">> => Subject,
             <<"id">> => Id, <<"options">> => format_options(CustomFields)},
    execute_user_command(<<"muc_light">>, <<"createRoom">>, User, Vars, Config).

user_change_room_configuration(User, RoomJID, Name, Subject, Config) ->
    Vars = #{<<"room">> => RoomJID, <<"name">> => Name, <<"subject">> => Subject},
    execute_user_command(<<"muc_light">>, <<"changeRoomConfiguration">>, User, Vars, Config).

user_change_room_configuration_with_custom_fields(User, RoomJID, Name, Subject, Config, Options) ->
    Vars = #{<<"room">> => RoomJID, <<"name">> => Name, <<"subject">> => Subject,
             <<"options">> => format_options(Options)},
    execute_user_command(<<"muc_light">>, <<"changeRoomConfiguration">>, User, Vars, Config).

format_options(Map) ->
    [#{<<"key">> => K, <<"value">> => V} || {K, V} <- maps:to_list(Map)].

user_invite_user(User, RoomJID, Recipient, Config) ->
    Vars = #{<<"room">> => RoomJID, <<"recipient">> => Recipient},
    execute_user_command(<<"muc_light">>, <<"inviteUser">>, User, Vars, Config).

user_delete_room(User, RoomJID, Config) ->
    Vars = #{<<"room">> => RoomJID},
    execute_user_command(<<"muc_light">>, <<"deleteRoom">>, User, Vars, Config).

user_kick_user(User, RoomJID, QueriedUser, Config) ->
    Vars = #{<<"room">> => RoomJID, <<"user">> => QueriedUser},
    execute_user_command(<<"muc_light">>, <<"kickUser">>, User, Vars, Config).

user_send_message_to_room(User, RoomJID, Body, Config) ->
    Vars = #{<<"room">> => RoomJID, <<"body">> => Body},
    execute_user_command(<<"muc_light">>, <<"sendMessageToRoom">>, User, Vars, Config).

user_get_room_messages(User, RoomJID, PageSize, Before, Config) ->
    Vars = #{<<"room">> => RoomJID, <<"pageSize">> => PageSize, <<"before">> => Before},
    execute_user_command(<<"muc_light">>, <<"getRoomMessages">>, User, Vars, Config).

user_list_rooms(User, Config) ->
    execute_user_command(<<"muc_light">>, <<"listRooms">>, User, #{}, Config).

user_list_room_users(User, RoomJID, Config) ->
    Vars = #{<<"room">> => RoomJID},
    execute_user_command(<<"muc_light">>, <<"listRoomUsers">>, User, Vars, Config).

user_get_room_config(User, RoomJID, Config) ->
    Vars = #{<<"room">> => RoomJID},
    execute_user_command(<<"muc_light">>, <<"getRoomConfig">>, User, Vars, Config).

user_get_blocking(User, Config) ->
    execute_user_command(<<"muc_light">>, <<"getBlockingList">>, User, #{}, Config).

user_set_blocking(User, Items, Config) ->
    Vars = #{<<"items">> => prepare_blocking_items_for_query(Items)},
    execute_user_command(<<"muc_light">>, <<"setBlockingList">>, User, Vars, Config).

user_get_muc_room_messages(User, RoomJID, PageSize, Before, Config) ->
    Vars = #{<<"room">> => jid:to_binary(RoomJID), <<"pageSize">> => PageSize,
                <<"before">> => Before},
    execute_user_command(<<"muc">>, <<"getRoomMessages">>, User, Vars, Config).

send_message_to_muc_room(User, Room, Body, Resource, Config) ->
    Vars = #{room => jid:to_binary(Room), body => Body, resource => Resource},
    execute_user_command(<<"muc">>, <<"sendMessageToRoom">>, User, Vars, Config).

enter_muc_room(RoomJID, User, Nick) ->
    JID = jid:to_binary(jid:replace_resource(RoomJID, Nick)),
    Pres = escalus_stanza:to(escalus_stanza:presence(<<"available">>,
        [#xmlel{ name = <<"x">>, attrs=[{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]}]),
        JID),
    escalus:send(User, Pres),
    escalus:wait_for_stanza(User).
