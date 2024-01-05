-module(rest_client_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-import(rest_helper,
        [decode_maplist/1,
         gett/3,
         post/4,
         putt/4,
         delete/3,
         delete/4]
         ).

-import(domain_helper, [host_type/0]).
-import(config_parser_helper, [mod_config/2]).

-define(OK, {<<"200">>, <<"OK">>}).
-define(CREATED, {<<"201">>, <<"Created">>}).
-define(NOCONTENT, {<<"204">>, <<"No Content">>}).
-define(NOT_FOUND, {<<"404">>, <<"Not Found">>}).
-define(BAD_REQUEST, {<<"400">>, <<"Bad Request">>}).
-define(UNAUTHORIZED, {<<"401">>, <<"Unauthorized">>}).
-define(FORBIDDEN, {<<"403">>, <<"Forbidden">>}).

%% --------------------------------------------------------------------
%% Common Test stuff
%% --------------------------------------------------------------------

all() ->
    [{group, messages},
     {group, muc},
     {group, muc_config},
     {group, muc_disabled},
     {group, roster},
     {group, messages_with_props},
     {group, messages_with_thread},
     {group, security},
     {group, sse_timeout}].

groups() ->
    [{messages_with_props, [parallel], message_with_props_test_cases()},
     {messages_with_thread, [parallel], message_with_thread_test_cases()},
     {messages, [parallel], message_test_cases()},
     {muc, [parallel], muc_test_cases()},
     {muc_config, [], muc_config_cases()},
     {muc_disabled, [parallel], muc_disabled_cases()},
     {roster, [parallel], roster_test_cases()},
     {security, [], security_test_cases()},
     {sse_timeout, [], [sse_should_not_get_timeout]}].

message_test_cases() ->
    [msg_is_sent_and_delivered_over_xmpp,
     msg_is_sent_and_delivered_over_sse,
     message_sending_errors,
     all_messages_are_archived,
     messages_with_user_are_archived,
     messages_can_be_paginated,
     message_query_errors].

muc_test_cases() ->
     [room_is_created,
      room_is_created_with_given_identifier,
      room_creation_errors,
      room_query_errors,
      user_is_invited_to_a_room,
      user_is_removed_from_a_room,
      user_removal_errors,
      rooms_can_be_listed,
      owner_can_leave_a_room_and_auto_select_owner,
      user_can_leave_a_room,
      invitation_to_room_is_forbidden_for_non_member,
      msg_is_sent_and_delivered_in_room,
      room_message_sending_errors,
      messages_are_archived_in_room,
      chat_markers_are_archived_in_room,
      room_message_query_errors,
      markable_property_is_archived_in_room,
      only_room_participant_can_read_messages,
      messages_can_be_paginated_in_room,
      room_msg_is_sent_and_delivered_over_sse,
      aff_change_msg_is_delivered_over_sse,
      room_is_created_with_given_jid,
      room_is_not_created_with_jid_not_matching_hostname,
      room_can_be_fetched_by_jid,
      messages_can_be_sent_and_fetched_by_room_jid,
      user_can_be_added_and_removed_by_room_jid
     ].

muc_config_cases() ->
    [
      config_can_be_changed_by_owner,
      config_cannot_be_changed_by_member,
      config_cannot_be_changed_by_non_member,
      config_change_errors,
      config_can_be_changed_by_all
    ].

muc_disabled_cases() ->
    [muc_disabled_errors].

roster_test_cases() ->
    [add_contact_and_invite,
     add_contact_and_be_invited,
     add_and_remove,
     add_and_remove_some_contacts_properly,
     add_and_remove_some_contacts_with_nonexisting,
     roster_errors].

message_with_props_test_cases() ->
    [
     msg_with_props_is_sent_and_delivered_over_xmpp,
     msg_with_props_can_be_parsed,
     msg_with_malformed_props_can_be_parsed,
     msg_with_malformed_props_is_sent_and_delivered_over_xmpp
     ].

message_with_thread_test_cases() ->
    [msg_with_thread_is_sent_and_delivered_over_xmpp,
     msg_with_thread_can_be_parsed,
     msg_with_thread_and_parent_is_sent_and_delivered_over_xmpp,
     msg_with_thread_and_parent_can_be_parsed,
     msg_without_thread_can_be_parsed,
     msg_without_thread_is_sent_and_delivered_over_xmpp].

security_test_cases() ->
    [
     default_http_server_name_is_returned_if_not_changed,
     non_default_http_server_name_is_returned_if_configured
    ].

init_per_suite(Config) ->
    Config1 = init_modules(Config),
    [{muc_light_host, muc_light_helper:muc_host()}
     | escalus:init_per_suite(Config1)].

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_modules(Config) ->
    HostType = host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config),
    Config2 = rest_helper:maybe_enable_mam(mam_helper:backend(), HostType, Config1),
    dynamic_modules:ensure_modules(HostType, required_modules(suite)),
    Config2.

init_per_group(muc_disabled = GN, Config) ->
    HostType = host_type(),
    Config1 = dynamic_modules:save_modules(HostType, Config),
    dynamic_modules:ensure_modules(HostType, required_modules(GN)),
    Config1;
init_per_group(sse_timeout, Config) ->
    % Change the default idle_timeout for the listener to 1s to test if sse will override it
    Listener = get_client_api_listener(),
    mongoose_helper:change_listener_idle_timeout(Listener, 1000),
    Config;
init_per_group(_GN, Config) ->
    Config.

end_per_group(muc_disabled, Config) ->
    dynamic_modules:restore_modules(Config);
end_per_group(sse_timeout, _Config) ->
    Listener = get_client_api_listener(),
    mongoose_helper:restart_listener(distributed_helper:mim(), Listener);
end_per_group(_GN, _Config) ->
    ok.

init_per_testcase(config_can_be_changed_by_all = TC, Config) ->
    HostType = host_type(),
    DefaultConfig = dynamic_modules:save_modules(HostType, Config),
    dynamic_modules:ensure_modules(HostType, required_modules(TC)),
    escalus:init_per_testcase(config_can_be_changed_by_all, DefaultConfig);
init_per_testcase(TC, Config) ->
    MAMTestCases = [all_messages_are_archived,
                    messages_with_user_are_archived,
                    messages_can_be_paginated,
                    messages_are_archived_in_room,
                    chat_markers_are_archived_in_room,
                    markable_property_is_archived_in_room,
                    only_room_participant_can_read_messages,
                    messages_can_be_paginated_in_room,
                    messages_can_be_sent_and_fetched_by_room_jid,
                    msg_with_props_is_sent_and_delivered_over_xmpp,
                    msg_with_props_can_be_parsed,
                    msg_with_malformed_props_can_be_parsed,
                    msg_with_malformed_props_is_sent_and_delivered_over_xmpp,
                    msg_with_thread_is_sent_and_delivered_over_xmpp,
                    msg_with_thread_can_be_parsed,
                    msg_with_thread_and_parent_is_sent_and_delivered_over_xmpp,
                    msg_with_thread_and_parent_can_be_parsed,
                    msg_without_thread_can_be_parsed,
                    msg_without_thread_is_sent_and_delivered_over_xmpp
                   ],
    rest_helper:maybe_skip_mam_test_cases(TC, MAMTestCases, Config).

end_per_testcase(config_can_be_changed_by_all, Config) ->
    dynamic_modules:restore_modules(Config),
    escalus:end_per_testcase(config_can_be_changed_by_all, Config);
end_per_testcase(TC, C) ->
    escalus:end_per_testcase(TC, C).

%% Module configuration - set up per suite and for special test cases
%% TODO: include MAM configuration here

required_modules(muc_disabled) ->
    [{mod_muc_light, stopped}];
required_modules(SuiteOrTC) ->
    Opts = maps:merge(common_muc_light_opts(), muc_light_opts(SuiteOrTC)),
    [{mod_muc_light, mod_config(mod_muc_light, Opts)}].

muc_light_opts(config_can_be_changed_by_all) ->
    #{all_can_configure => true};
muc_light_opts(suite) ->
    #{}.

common_muc_light_opts() ->
    #{rooms_in_rosters => true,
      backend => mongoose_helper:mnesia_or_rdbms_backend()}.

%% --------------------------------------------------------------------
%% Test cases
%% --------------------------------------------------------------------

msg_is_sent_and_delivered_over_xmpp(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        M = send_message(alice, Alice, Bob),
        Msg = escalus:wait_for_stanza(Bob),
        escalus:assert(is_chat_message, [maps:get(body, M)], Msg)
    end).

msg_is_sent_and_delivered_over_sse(ConfigIn) ->
    Config = escalus_fresh:create_users(ConfigIn, [{alice, 1}, {bob, 1}]),
    Bob = escalus_users:get_userspec(Config, bob),
    Alice = escalus_users:get_userspec(Config, alice),

    {200, Conn} = connect_to_sse({alice, Alice}),
    M = send_message(bob, Bob, Alice),

    Event = sse_helper:wait_for_event(Conn),
    assert_json_message(M, Event),
    sse_helper:stop_sse(Conn).

message_sending_errors(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        BobJID = user_jid(Bob),
        M = #{to => BobJID, body => <<"hello, ", BobJID/binary, " it's me">>},
        Cred = credentials({alice, Alice}),
        {?BAD_REQUEST, <<"Missing message body">>} =
            post(client, <<"/messages">>, maps:remove(body, M), Cred),
        {?BAD_REQUEST, <<"Missing recipient JID">>} =
            post(client, <<"/messages">>, maps:remove(to, M), Cred),
        {?BAD_REQUEST, <<"Invalid recipient JID">>} =
            post(client, <<"/messages">>, M#{to => <<"@invalid">>}, Cred)
    end).

room_msg_is_sent_and_delivered_over_sse(ConfigIn) ->
    Config = escalus_fresh:create_users(ConfigIn, [{alice, 1}, {bob, 1}]),
    Bob = escalus_users:get_userspec(Config, bob),
    Alice = escalus_users:get_userspec(Config, alice),
    RoomID = given_new_room_with_users({alice, Alice}, [{bob, Bob}]),
    RoomInfo = get_room_info({alice, Alice}, RoomID),
    true = is_participant(Bob, <<"member">>, RoomInfo),
    {200, Conn} = connect_to_sse({bob, Bob}),
    Message = given_message_sent_to_room(RoomID, {alice, Alice}),
    Event = sse_helper:wait_for_event(Conn),
    assert_json_room_sse_message(Message#{room => RoomID, type => <<"message">>}, Event),
    sse_helper:stop_sse(Conn).

aff_change_msg_is_delivered_over_sse(ConfigIn) ->
    Config = escalus_fresh:create_users(ConfigIn, [{alice, 1}, {bob, 1}]),
    Bob = escalus_users:get_userspec(Config, bob),
    Alice = escalus_users:get_userspec(Config, alice),
    RoomID = given_new_room({alice, Alice}),
    {200, Conn} = connect_to_sse({bob, Bob}),
    given_user_invited({alice, Alice}, RoomID, Bob),
    Event = sse_helper:wait_for_event(Conn),
    BobJID = user_jid(Bob),
    RoomJID = room_jid(RoomID, Config),
    assert_json_room_sse_message(#{room => RoomID,
                                   from => RoomJID,
                                   type => <<"affiliation">>,
                                   user => BobJID}, Event),
    sse_helper:stop_sse(Conn).

all_messages_are_archived(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        Sent = [M1 | _] = send_messages(Config, Alice, Bob, Kate),
        AliceJID = maps:get(to, M1),
        AliceCreds = {AliceJID, user_password(alice)},
        GetPath = lists:flatten("/messages/"),
        {?OK, Msgs} = rest_helper:gett(client, GetPath, AliceCreds),
        Received = [_Msg1, _Msg2, _Msg3] = rest_helper:decode_maplist(Msgs),
        assert_messages(Sent, Received)

    end).

messages_with_user_are_archived(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        [M1, _M2, M3] = send_messages(Config, Alice, Bob, Kate),
        AliceJID = maps:get(to, M1),
        KateJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Kate)),
        AliceCreds = {AliceJID, user_password(alice)},
        GetPath = lists:flatten(["/messages/", binary_to_list(KateJID)]),
        {?OK, Msgs} = rest_helper:gett(client, GetPath, AliceCreds),
        Recv = [_Msg2] = rest_helper:decode_maplist(Msgs),
        assert_messages([M3], Recv)

    end).

messages_can_be_paginated(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
        BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
        rest_helper:fill_archive(Alice, Bob),
        mam_helper:maybe_wait_for_archive(Config),
        AliceCreds = {AliceJID, user_password(alice)},
        % recent msgs with a limit
        M1 = get_messages(AliceCreds, BobJID, 10),
        6 = length(M1),
        M2 = get_messages(AliceCreds, BobJID, 3),
        3 = length(M2),
        % older messages - earlier then the previous midnight
        PriorTo = rest_helper:make_timestamp(-1, {0, 0, 1}),
        M3 = get_messages(AliceCreds, BobJID, PriorTo, 10),
        4 = length(M3),
        [Oldest|_] = M3,
        <<"A">> = maps:get(body, Oldest),
        % same with limit
        M4 = get_messages(AliceCreds, BobJID, PriorTo, 2),
        2 = length(M4),
        [Oldest2|_] = M4,
        <<"B">> = maps:get(body, Oldest2)
    end).

message_query_errors(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
        Creds = credentials({alice, Alice}),
        Path = <<"/messages/", BobJID/binary>>,
        {?BAD_REQUEST, <<"Invalid interlocutor JID">>} =
            rest_helper:gett(client, <<"/messages/@invalid">>, Creds),
        {?BAD_REQUEST, <<"Invalid limit">>} =
            rest_helper:gett(client, <<Path/binary, "?limit=x">>, Creds),
        {?BAD_REQUEST, <<"Invalid value of 'before'">>} =
            rest_helper:gett(client, <<Path/binary, "?before=x">>, Creds),
        {?BAD_REQUEST, <<"Invalid query string">>} =
            rest_helper:gett(client, <<Path/binary, "?kuropatwa">>, Creds)
    end).

room_is_created(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        RoomID = given_new_room({alice, Alice}),
        RoomInfo = get_room_info({alice, Alice}, RoomID),
        assert_room_info(Alice, RoomInfo)
    end).

room_query_errors(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room_with_users({alice, Alice}, []),
        Creds = credentials({bob, Bob}),
        {?NOT_FOUND, <<"Room not found">>} =
            rest_helper:gett(client, <<"/rooms/badroom">>, Creds),
        {?FORBIDDEN, _} =
            rest_helper:gett(client, <<"/rooms/", RoomID/binary>>, Creds)
    end).

muc_disabled_errors(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Creds = credentials({alice, Alice}),
        {?NOT_FOUND, <<"MUC Light server not found">>} =
            rest_helper:gett(client, <<"/rooms/badroom">>, Creds)
    end).

room_is_created_with_given_identifier(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        GivenRoomID = muc_helper:fresh_room_name(),
        GivenRoomID = given_new_room({alice, Alice}, GivenRoomID),
        RoomInfo = get_room_info({alice, Alice}, GivenRoomID),
        assert_room_info(Alice, RoomInfo)
    end).

room_creation_errors(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        RoomID = muc_helper:fresh_room_name(),
        Room = #{name => <<"My Room">>, subject => <<"My Secrets">>},
        Path = <<"/rooms/", RoomID/binary>>,
        Creds = credentials({alice, Alice}),
        {?BAD_REQUEST, <<"Missing room ID">>} =
            putt(client, "/rooms", Room, Creds),
        {?BAD_REQUEST, <<"Invalid room ID">>} =
            putt(client, "/rooms/@invalid", Room, Creds),
        {?BAD_REQUEST, <<"Missing room name">>} =
            putt(client, Path, maps:remove(name, Room), Creds),
        {?BAD_REQUEST, <<"Missing room subject">>} =
            putt(client, Path, maps:remove(subject, Room), Creds),
        {?CREATED, _} =
            putt(client, Path, Room, Creds),
        {?FORBIDDEN, _} =
            putt(client, Path, Room, Creds)
    end).

config_can_be_changed_by_owner(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        RoomID = muc_helper:fresh_room_name(),
        RoomJID = room_jid(RoomID, Config),
        RoomID = given_new_room({alice, Alice}, RoomJID, <<"old_name">>),
        RoomInfo = get_room_info({alice, Alice}, RoomID),
        assert_property_value(<<"name">>, <<"old_name">>, RoomInfo),

        {{<<"204">>,<<"No Content">>},<<>>} =
            when_config_change({alice, Alice}, RoomJID, <<"new_name">>, <<"new_subject">>),
        NewRoomInfo = get_room_info({alice, Alice}, RoomID),
        assert_property_value(<<"name">>, <<"new_name">>, NewRoomInfo),
        assert_property_value(<<"subject">>, <<"new_subject">>, NewRoomInfo)
    end).

config_cannot_be_changed_by_member(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room_with_users({alice, Alice}, [{bob, Bob}]),
        RoomJID = room_jid(RoomID, Config),
        {?FORBIDDEN, _} =
            when_config_change({bob, Bob}, RoomJID, <<"other_name">>, <<"other_subject">>),
        NewRoomInfo = get_room_info({bob, Bob}, RoomID),
        assert_property_value(<<"name">>, <<"new_room_name">>, NewRoomInfo)
    end).

config_cannot_be_changed_by_non_member(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room_with_users({alice, Alice}, []),
        RoomJID = room_jid(RoomID, Config),
        {?FORBIDDEN, _} =
            when_config_change({bob, Bob}, RoomJID, <<"other_name">>, <<"other_subject">>),
        NewRoomInfo = get_room_info({alice, Alice}, RoomID),
        assert_property_value(<<"name">>, <<"new_room_name">>, NewRoomInfo)
    end).

config_change_errors(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        RoomID = given_new_room_with_users({alice, Alice}, []),
        RoomJID = room_jid(RoomID, Config),
        {?NOT_FOUND, _} =
            when_config_change({alice, Alice}, <<"badroom">>, <<"other_name">>, <<"other_subject">>),
        {?BAD_REQUEST, <<"Validation failed ", _/binary>>} =
            when_config_change({alice, Alice}, RoomJID, <<"other_name">>, 123),
        NewRoomInfo = get_room_info({alice, Alice}, RoomID),
        assert_property_value(<<"name">>, <<"new_room_name">>, NewRoomInfo)
    end).

config_can_be_changed_by_all(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room_with_users({alice, Alice}, [{bob, Bob}]),
        RoomJID = room_jid(RoomID, Config),
        RoomInfo = get_room_info({alice, Alice}, RoomID),
        assert_property_value(<<"name">>,<<"new_room_name">>,RoomInfo),
        {{<<"204">>,<<"No Content">>},<<>>} =
            when_config_change({bob, Bob}, RoomJID, <<"other_name">>, <<"other_subject">>),
        NewRoomInfo = get_room_info({alice, Alice}, RoomID),
        assert_property_value(<<"name">>,<<"other_name">>,NewRoomInfo)
    end).

rooms_can_be_listed(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        [] = get_my_rooms({alice, Alice}),
        RoomID = given_new_room_with_users({alice, Alice}, [{bob, Bob}]),
        [{Room}] = get_my_rooms({alice, Alice}),
        RoomMap = maps:from_list(Room),
        RoomID = maps:get(<<"id">>, RoomMap),
        true = maps:is_key(<<"name">>, RoomMap),
        true = maps:is_key(<<"subject">>, RoomMap),
        [{Room}] = get_my_rooms({bob, Bob})
    end).

user_is_invited_to_a_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room_with_users({alice, Alice}, [{bob, Bob}]),
        RoomInfo = get_room_info({alice, Alice}, RoomID),
        true = is_participant(Bob, <<"member">>, RoomInfo),
        IQ = escalus_stanza:iq_get(<<"urn:xmpp:muclight:0#affiliations">>, []),
        RoomJID = room_jid(RoomID, Config),
        escalus:send(Alice, escalus_stanza:to(IQ, RoomJID)),
        escalus:assert(is_iq_result, [IQ], escalus:wait_for_stanza(Alice))

    end).

user_is_removed_from_a_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room_with_users({alice, Alice}, [{bob, Bob}]),
        {{<<"204">>, _}, _} = remove_user_from_a_room({alice, Alice}, RoomID, Bob),
        Stanza = escalus:wait_for_stanza(Bob),
        assert_aff_change_stanza(Stanza, Bob, <<"none">>)
    end).

user_removal_errors(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room_with_users({alice, Alice}, [{bob, Bob}]),
        Path = <<"/rooms/", RoomID/binary, "/users/">>,
        BobJid = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
        AliceJid = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
        Creds = credentials({alice, Alice}),
        {?BAD_REQUEST, <<"Invalid user JID: @invalid">>} =
            rest_helper:delete(client, <<Path/binary, "@invalid">>, Creds),
        {?BAD_REQUEST, <<"Missing JID">>} =
            rest_helper:delete(client, Path, Creds),
        {?FORBIDDEN, <<"Given user does not have permission", _/binary>>} =
            rest_helper:delete(client, <<Path/binary, AliceJid/binary>>, credentials({bob, Bob})),
        {?NOT_FOUND, <<"Room not found">>} =
            rest_helper:delete(client, <<"/rooms/badroom/users/", BobJid/binary>>, Creds)
    end).

owner_can_leave_a_room_and_auto_select_owner(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room_with_users({alice, Alice}, [{bob, Bob}]),
        {{<<"204">>, _}, _} = remove_user_from_a_room({alice, Alice}, RoomID, Alice),
        Stanza = escalus:wait_for_stanza(Bob),
        assert_aff_change_stanza(Stanza, Alice, <<"none">>),
        assert_aff_change_stanza(Stanza, Bob, <<"owner">>)
    end).

user_can_leave_a_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room_with_users({alice, Alice}, [{bob, Bob}]),
        {{<<"204">>, _}, _} = remove_user_from_a_room({bob, Bob}, RoomID, Bob),
        Stanza = escalus:wait_for_stanza(Bob),
        assert_aff_change_stanza(Stanza, Bob, <<"none">>)
    end).

invitation_to_room_is_forbidden_for_non_member(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room({alice, Alice}),
        {?FORBIDDEN, _ } = invite_to_room({bob, Bob}, RoomID, <<"auser@domain.com">>)
    end).

msg_is_sent_and_delivered_in_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        given_new_room_with_users_and_msgs({alice, Alice}, [{bob, Bob}])
    end).

room_message_sending_errors(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        Sender = {alice, Alice},
        RoomID = given_new_room_with_users(Sender, []),
        InvalidMarker = #{type => <<"bad">>, id => <<"some_id">>},
        {?BAD_REQUEST, <<"Invalid message body">>} =
            given_message_sent_to_room(RoomID, Sender, #{body => #{body => <<"Too nested">>}}),
        {?BAD_REQUEST, <<"No valid message elements">>} =
            given_message_sent_to_room(RoomID, Sender, #{no_body => <<"This should be in body">>}),
        {?BAD_REQUEST, <<"No valid message elements">>} =
            given_message_sent_to_room(RoomID, Sender, #{markable => true}),
        {?BAD_REQUEST, <<"Invalid chat marker">>} =
            given_message_sent_to_room(RoomID, Sender, #{chat_marker => InvalidMarker}),
        {?BAD_REQUEST, <<"Invalid request body">>} =
            given_message_sent_to_room(RoomID, Sender, <<"This is not JSON object">>),
        {?FORBIDDEN, _} =
            given_message_sent_to_room(RoomID, {bob, Bob}, #{body => <<"Hi">>}),
        {?NOT_FOUND, <<"Room not found">>} =
            given_message_sent_to_room(<<"badroom">>, Sender, #{body => <<"Hi">>})
    end).

messages_are_archived_in_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {RoomID, _Msgs} = given_new_room_with_users_and_msgs({alice, Alice}, [{bob, Bob}]),
        mam_helper:maybe_wait_for_archive(Config),
        {?OK, Result} = get_room_messages({alice, Alice}, RoomID),
        [Aff, _Msg1, _Msg2] = rest_helper:decode_maplist(Result),
        %% The oldest message is aff change
        <<"affiliation">> = maps:get(type, Aff),
        <<"member">> = maps:get(affiliation, Aff),
        BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
        BobJID = maps:get(user, Aff)
    end).

chat_markers_are_archived_in_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % GIVEN 3 different chat markers that are sent via HTTP and received via XMPP
        MarkedID = <<"RagnarokIsComing">>,
        MarkerTypes = [<<"received">>, <<"displayed">>, <<"acknowledged">>],
        Markers = [#{ chat_marker => #{ type => Type, id => MarkedID } } || Type <- MarkerTypes ],
        RoomID = given_new_room_with_users({alice, Alice}, [{bob, Bob}]),
        lists:foreach(fun(Marker) ->
                              {?OK, {_Result}} =
                              given_message_sent_to_room(RoomID, {bob, Bob}, Marker),
                              [ escalus:wait_for_stanza(Client) || Client <- [Alice, Bob] ]
                      end, Markers),
        mam_helper:maybe_wait_for_archive(Config),

        % WHEN an archive is queried via HTTP
        {?OK, Result} = get_room_messages({alice, Alice}, RoomID),

        % THEN these markers are retrieved and in proper order and with valid payload
        % (we discard remaining msg fields, they are tested by other cases)
        [_Aff | ReceivedMarkers] = rest_helper:decode_maplist(Result),
        Markers = [ maps:with([chat_marker], RecvMarker) || RecvMarker <- ReceivedMarkers ]
    end).

% Combo test case which verifies both the translation of "markable" element
% (JSON -> XML -> JSON) and if it's preserved properly in the archive
markable_property_is_archived_in_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        % GIVEN a markable message is sent in the room
        MarkableMsg = #{ markable => true, body => <<"Floor is lava!">> },
        RoomID = given_new_room_with_users({alice, Alice}, [{bob, Bob}]),
        {?OK, {_Result}}
        = given_message_sent_to_room(RoomID, {bob, Bob}, MarkableMsg),
        [ escalus:wait_for_stanza(Client) || Client <- [Alice, Bob] ],
        mam_helper:maybe_wait_for_archive(Config),

        % WHEN an archive is queried via HTTP
        {?OK, Result} = get_room_messages({alice, Alice}, RoomID),

        % THEN the retrieved message has markable property
        [_Aff, Msg] = rest_helper:decode_maplist(Result),
        true = maps:get(markable, Msg, undefined)
    end).

only_room_participant_can_read_messages(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room({alice, Alice}),
        {?FORBIDDEN, _} = get_room_messages({bob, Bob}, RoomID),
        ok
    end).

get_room_messages(Caller, RoomID) ->
    Path = <<"/rooms/", RoomID/binary, "/messages">>,
    Creds = credentials(Caller),
    rest_helper:gett(client, Path, Creds).

messages_can_be_paginated_in_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room_with_users({alice, Alice}, [{bob, Bob}]),
        %% GenMsgs1 is older than GenMsgs2
        %% One message is already in the archive
        [GenMsgs1, GenMsgs2 | _] = rest_helper:fill_room_archive(RoomID, [Alice, Bob], 1),
        mam_helper:maybe_wait_for_archive(Config),
        Msgs10 = get_room_messages({alice, Alice}, RoomID, 10),
        Msgs10Len = length(Msgs10),
        true = Msgs10Len > 0 andalso Msgs10Len =< 10,
        Msgs3 = get_room_messages({alice, Alice}, RoomID, 3),
        [_, _, _] = Msgs3,
        {_, Time} = calendar:now_to_datetime(os:timestamp()),
        PriorTo = rest_helper:make_timestamp(-1, Time) - timer:seconds(10),
        [OldestMsg1 | _] = get_room_messages({alice, Alice}, RoomID, 4, PriorTo),
        assert_room_messages(OldestMsg1, hd(lists:keysort(1, GenMsgs1))),
        [OldestMsg2 | _] = get_room_messages({alice, Alice}, RoomID, 2, PriorTo),
        assert_room_messages(OldestMsg2, hd(lists:keysort(1, GenMsgs2)))
    end).

room_message_query_errors(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        RoomID = given_new_room_with_users({alice, Alice}, []),
        Creds = credentials({alice, Alice}),
        Path = <<"/rooms/", RoomID/binary, "/messages">>,
        {?BAD_REQUEST, <<"Invalid limit">>} =
            rest_helper:gett(client, <<Path/binary, "?limit=x">>, Creds),
        {?BAD_REQUEST, <<"Invalid value of 'before'">>} =
            rest_helper:gett(client, <<Path/binary, "?before=x">>, Creds),
        {?BAD_REQUEST, <<"Invalid query string">>} =
            rest_helper:gett(client, <<Path/binary, "?kuropatwa">>, Creds),
        {?NOT_FOUND, <<"Room not found">>} =
            rest_helper:gett(client, <<"/rooms/badroom/messages">>, Creds)
    end).

room_is_created_with_given_jid(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        RoomID = muc_helper:fresh_room_name(),
        RoomJID = room_jid(RoomID, Config),
        RoomID = given_new_room({alice, Alice}, RoomJID),
        RoomInfo = get_room_info({alice, Alice}, RoomID),
        assert_room_info(Alice, RoomInfo)
    end).

room_is_not_created_with_jid_not_matching_hostname(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        RoomID = muc_helper:fresh_room_name(),
        RoomJID = <<RoomID/binary, "@muclight.wrongdomain">>,
        Creds = credentials({alice, Alice}),
        {{Status, _}, _} = create_room_with_id_request(Creds,
                                                       <<"some_name">>,
                                                       <<"some subject">>,
                                                       RoomJID),
        ?assertEqual(<<"400">>, Status)
    end).

room_can_be_fetched_by_jid(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        RoomID = muc_helper:fresh_room_name(),
        RoomJID = room_jid(RoomID, Config),
        RoomID = given_new_room({alice, Alice}, RoomJID),
        RoomInfo = get_room_info({alice, Alice}, RoomJID),
        assert_room_info(Alice, RoomInfo)
    end).

messages_can_be_sent_and_fetched_by_room_jid(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, _Bob) ->
        RoomID = given_new_room({alice, Alice}),
        RoomJID = room_jid(RoomID, Config),
        given_message_sent_to_room(RoomJID, {alice, Alice}),
        mam_helper:maybe_wait_for_archive(Config),
        [_] = get_room_messages({alice, Alice}, RoomJID, 10)
    end).

user_can_be_added_and_removed_by_room_jid(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room({alice, Alice}),
        RoomJID = room_jid(RoomID, Config),
        given_user_invited({alice, Alice}, RoomJID, Bob),
        {{Status, _}, _} = remove_user_from_a_room({alice, Alice}, RoomJID, Bob),
        ?assertEqual(<<"204">>, Status)
    end).

msg_with_props_is_sent_and_delivered_over_xmpp(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        BobJID = user_jid(Bob),
        MsgID = base16:encode(crypto:strong_rand_bytes(5)),
        M1 = rest_helper:make_msg_stanza_with_props(BobJID,MsgID),

        escalus:send(Alice, M1),

        M2 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, M2)
    end).

msg_with_props_can_be_parsed(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
        BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
        MsgID = base16:encode(crypto:strong_rand_bytes(5)),
        M1 = rest_helper:make_msg_stanza_with_props(BobJID,MsgID),

        escalus:send(Alice, M1),

        escalus:wait_for_stanza(Bob),
        mam_helper:wait_for_archive_size(Bob, 1),
        mam_helper:wait_for_archive_size(Alice, 1),

        AliceCreds = {AliceJID, user_password(alice)},

        % recent msgs with a limit
        M2 = get_messages_with_props(AliceCreds, BobJID, 1),

        [{MsgWithProps} | _] = M2,

        Data = maps:from_list(MsgWithProps),

        #{<<"properties">> := {Props},
          <<"id">> := ReceivedMsgID} = Data,

        %we are expecting two properties:"some_string" and "some_number" for this test message
        %test message defined in rest_helper:make_msg_stanza_with_props
        2 = length(Props),
        ReceivedMsgID = MsgID

    end).

msg_with_malformed_props_is_sent_and_delivered_over_xmpp(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        BobJID = user_jid(Bob),
        MsgID = base16:encode(crypto:strong_rand_bytes(5)),

        M1 = rest_helper:make_malformed_msg_stanza_with_props(BobJID, MsgID),

        escalus:send(Alice, M1),

        M2 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_message, M2)
    end).

msg_with_malformed_props_can_be_parsed(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
        AliceCreds = {AliceJID, user_password(alice)},
        BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
        MsgID = base16:encode(crypto:strong_rand_bytes(5)),

        M1 = rest_helper:make_malformed_msg_stanza_with_props(BobJID,MsgID),
        escalus:send(Alice, M1),

        escalus:wait_for_stanza(Bob),
        mam_helper:wait_for_archive_size(Bob, 1),
        mam_helper:wait_for_archive_size(Alice, 1),

        % recent msgs with a limit
        M2 = get_messages_with_props(AliceCreds, BobJID, 1),
        [_Msg] = rest_helper:decode_maplist(M2),

        MsgID = maps:get(id, _Msg)

    end).

msg_with_thread_is_sent_and_delivered_over_xmpp(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
				BobJID = user_jid(Bob),
				MsgID = base16:encode(crypto:strong_rand_bytes(5)),
				ThreadID = base16:encode(crypto:strong_rand_bytes(5)),
				M1 = rest_helper:make_msg_stanza_with_thread(BobJID, MsgID, ThreadID),
				escalus:send(Alice, M1),
				M2 = escalus:wait_for_stanza(Bob),
				escalus:assert(is_message, M2)
		end).

msg_with_thread_can_be_parsed(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
				AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
				BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
				MsgID = base16:encode(crypto:strong_rand_bytes(5)),
				ThreadID = base16:encode(crypto:strong_rand_bytes(5)),
				M1 = rest_helper:make_msg_stanza_with_thread(BobJID, MsgID, ThreadID),
				escalus:send(Alice, M1),
				escalus:wait_for_stanza(Bob),
				mam_helper:wait_for_archive_size(Bob, 1),
				mam_helper:wait_for_archive_size(Alice, 1),
				AliceCreds = {AliceJID, user_password(alice)},
				% recent msgs with a limit
				M2 = get_messages_with_props(AliceCreds, BobJID, 1),
				[{MsgWithProps} | _] = M2,
				Data = maps:from_list(MsgWithProps),
				#{<<"thread">> := ReceivedThreadID,
				  <<"id">> := ReceivedMsgID} = Data,
				%we are expecting thread and parent thread for this test message
				%test message defined in rest_helper:make_msg_stanza_with_thread
				ReceivedThreadID = ThreadID,
				ReceivedMsgID = MsgID
		end).

msg_with_thread_and_parent_is_sent_and_delivered_over_xmpp(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
				BobJID = user_jid(Bob),
				MsgID = base16:encode(crypto:strong_rand_bytes(5)),
				ThreadID = base16:encode(crypto:strong_rand_bytes(5)),
				ThreadParentID = base16:encode(crypto:strong_rand_bytes(5)),
				M1 = rest_helper:make_msg_stanza_with_thread_and_parent(BobJID, MsgID, ThreadID, ThreadParentID),
				escalus:send(Alice, M1),
				M2 = escalus:wait_for_stanza(Bob),
				escalus:assert(is_message, M2)
		end).

msg_with_thread_and_parent_can_be_parsed(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
				AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
				BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
				MsgID = base16:encode(crypto:strong_rand_bytes(5)),
				ThreadID = base16:encode(crypto:strong_rand_bytes(5)),
				ThreadParentID = base16:encode(crypto:strong_rand_bytes(5)),
				M1 = rest_helper:make_msg_stanza_with_thread_and_parent(BobJID, MsgID, ThreadID, ThreadParentID),
				escalus:send(Alice, M1),
				escalus:wait_for_stanza(Bob),
				mam_helper:wait_for_archive_size(Bob, 1),
				mam_helper:wait_for_archive_size(Alice, 1),
				AliceCreds = {AliceJID, user_password(alice)},
				% recent msgs with a limit
				M2 = get_messages_with_props(AliceCreds, BobJID, 1),
				[{MsgWithProps} | _] = M2,
				Data = maps:from_list(MsgWithProps),
				#{<<"thread">> := ReceivedThreadID,
				  <<"parent">> := ReceivedThreadParentID,
				  <<"id">> := ReceivedMsgID} = Data,
				%we are expecting thread and parent thread for this test message
				%test message defined in rest_helper:make_msg_stanza_with_thread
				ReceivedThreadID = ThreadID,
				ReceivedThreadParentID = ThreadParentID,
				ReceivedMsgID = MsgID
		end).

msg_without_thread_is_sent_and_delivered_over_xmpp(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
				BobJID = user_jid(Bob),
				MsgID = base16:encode(crypto:strong_rand_bytes(5)),
				M1 = rest_helper:make_msg_stanza_without_thread(BobJID, MsgID),
				escalus:send(Alice, M1),
				M2 = escalus:wait_for_stanza(Bob),
				escalus:assert(is_message, M2)
		end).

msg_without_thread_can_be_parsed(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
				AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
				AliceCreds = {AliceJID, user_password(alice)},
				BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
				MsgID = base16:encode(crypto:strong_rand_bytes(5)),
				M1 = rest_helper:make_msg_stanza_without_thread(BobJID, MsgID),
				escalus:send(Alice, M1),
				escalus:wait_for_stanza(Bob),
				mam_helper:wait_for_archive_size(Bob, 1),
				mam_helper:wait_for_archive_size(Alice, 1),
				% recent msgs with a limit
				M2 = get_messages_with_props(AliceCreds, BobJID, 1),
				[_Msg] = rest_helper:decode_maplist(M2),
				MsgID = maps:get(id, _Msg)
		end).

sse_should_not_get_timeout(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun (Alice, Bob) ->
        From = escalus_client:full_jid(Bob),
        To = escalus_client:short_jid(Alice),
        {200, Stream} = connect_to_sse({alice, Alice}),
        escalus:send(Bob, escalus_stanza:chat(From, To, <<"Hello!">>)),
        sse_helper:wait_for_event(Stream),
        timer:sleep(2000),
        escalus:send(Bob, escalus_stanza:chat(From, To, <<"Hello again!">>)),
        sse_helper:wait_for_event(Stream),
        sse_helper:stop_sse(Stream)
    end).

assert_room_messages(RecvMsg, {_ID, _GenFrom, GenMsg}) ->
    escalus:assert(is_chat_message, [maps:get(body, RecvMsg)], GenMsg),
    ok.

get_room_info(User, RoomID) ->
    Creds = credentials(User),
    {?OK, {Result}} = rest_helper:gett(client, <<"/rooms/", RoomID/binary>>,
                                                         Creds),
    Result.

given_new_room_with_users_and_msgs(Owner, Users) ->
    RoomID = given_new_room_with_users(Owner, Users),
    Msgs = [given_message_sent_to_room(RoomID, Sender) || Sender <- [Owner | Users]],
    wait_for_room_msgs(Msgs, [Owner | Users]),
    {RoomID, Msgs}.

wait_for_room_msgs([], _) ->
    ok;
wait_for_room_msgs([Msg | Rest], Users) ->
    [wait_for_room_msg(Msg, User) || {_, User} <- Users],
    wait_for_room_msgs(Rest, Users).

wait_for_room_msg(Msg, User) ->
    Stanza = escalus:wait_for_stanza(User),
    escalus:assert(is_groupchat_message, [maps:get(body, Msg)], Stanza).

given_message_sent_to_room(RoomID, Sender) ->
    Body = #{body => <<"Hi all!">>},
    HTTPResult = given_message_sent_to_room(RoomID, Sender, Body),
    {?OK, {Result}} = HTTPResult,
    MsgId = proplists:get_value(<<"id">>, Result),
    true = is_binary(MsgId),
    {UserJID, _} = credentials(Sender),

    Body#{id => MsgId, from => UserJID}.

given_message_sent_to_room(RoomID, Sender, Body) ->
    Creds = credentials(Sender),
    Path = <<"/rooms/", RoomID/binary, "/messages">>,
    rest_helper:post(client, Path, Body, Creds).

given_new_room_with_users(Owner, Users) ->
    RoomID = given_new_room(Owner),
    [given_user_invited(Owner, RoomID, User) || {_, User} <- Users],
    RoomID.

given_new_room(Owner) ->
    Creds = credentials(Owner),
    RoomName = <<"new_room_name">>,
    create_room(Creds, RoomName, <<"This room subject">>).

given_new_room(Owner, RoomID) ->
    Creds = credentials(Owner),
    RoomName = <<"new_room_name">>,
    create_room_with_id(Creds, RoomName, <<"This room subject">>, RoomID).

given_new_room(Owner, RoomID, RoomName) ->
    Creds = credentials(Owner),
    create_room_with_id(Creds, RoomName, <<"This room subject">>, RoomID).

given_user_invited({_, Inviter} = Owner, RoomID, Invitee) ->
    JID = user_jid(Invitee),
    {?NOCONTENT, _} = invite_to_room(Owner, RoomID, JID),
    maybe_wait_for_aff_stanza(Invitee, Invitee),
    maybe_wait_for_aff_stanza(Inviter, Invitee).

when_config_change(User, RoomID, NewName, NewSubject) ->
    Creds = credentials(User),
    Config = #{name => NewName, subject => NewSubject},
    Path = <<"/rooms/", RoomID/binary, "/config">>,
    putt(client, Path, Config, Creds).

maybe_wait_for_aff_stanza(#client{} = Client, Invitee) ->
    Stanza = escalus:wait_for_stanza(Client),
    assert_aff_change_stanza(Stanza, Invitee, <<"member">>);
maybe_wait_for_aff_stanza(_, _) ->
    ok.

invite_to_room(Inviter, RoomID, Invitee) ->
    Body = #{user => Invitee},
    Creds = credentials(Inviter),
    rest_helper:post(client, <<"/rooms/", RoomID/binary, "/users">>, Body, Creds).

remove_user_from_a_room(Inviter, RoomID, Invitee) ->
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Invitee)),
    Creds = credentials(Inviter),
    Path = <<"/rooms/", RoomID/binary, "/users/", JID/binary>>,
    rest_helper:delete(client, Path, Creds).

credentials({User, ClientOrSpec}) ->
    {user_jid(ClientOrSpec), user_password(User)}.

user_jid(#client{} = UserClient) ->
    escalus_utils:jid_to_lower(escalus_client:short_jid(UserClient));
user_jid(Spec) ->
    U = proplists:get_value(username, Spec),
    S = proplists:get_value(server, Spec),
    escalus_utils:jid_to_lower(<<U/binary, $@, S/binary>>).

user_password(User) ->
    [{User, Props}] = escalus:get_users([User]),
    proplists:get_value(password, Props).

send_message(User, From, To) ->
    AliceJID = user_jid(From),
    BobJID = user_jid(To),
    M = #{to => BobJID, body => <<"hello, ", BobJID/binary, " it's me">>},
    Cred = credentials({User, From}),
    {?OK, {Result}} = post(client, <<"/messages">>, M, Cred),
    ID = proplists:get_value(<<"id">>, Result),
    M#{id => ID, from => AliceJID}.

get_messages(MeCreds, Other, Count) ->
    GetPath = lists:flatten(["/messages/",
                             binary_to_list(Other),
                             "?limit=", integer_to_list(Count)]),
    get_messages(GetPath, MeCreds).

get_messages(Path, Creds) ->
    {?OK, Msgs} = rest_helper:gett(client, Path, Creds),
    rest_helper:decode_maplist(Msgs).

get_messages(MeCreds, Other, Before, Count) ->
    GetPath = lists:flatten(["/messages/",
                             binary_to_list(Other),
                             "?before=", integer_to_list(Before),
                             "&limit=", integer_to_list(Count)]),
    get_messages(GetPath, MeCreds).

get_messages_with_props(MeCreds, Other, Count) ->
    GetPath = lists:flatten(["/messages/",
                             binary_to_list(Other),
                             "?limit=", integer_to_list(Count)]),
    get_messages_with_props(GetPath, MeCreds).

get_messages_with_props(Path, Creds) ->
    {?OK, Msgs} = rest_helper:gett(client, Path, Creds),
    Msgs.

get_messages_with_props(MeCreds, Other, Before, Count) ->
    GetPath = lists:flatten(["/messages/",
                             binary_to_list(Other),
                             "?before=", integer_to_list(Before),
                             "&limit=", integer_to_list(Count)]),
    get_messages_with_props(GetPath, MeCreds).

get_room_messages(Client, RoomID, Count) ->
    get_room_messages(Client, RoomID, Count, undefined).

get_room_messages(Client, RoomID, Count, Before) ->
    Creds = credentials(Client),
    BasePathList = ["/rooms/", RoomID, "/messages?limit=", integer_to_binary(Count)],
    PathList = BasePathList ++ [["&before=", integer_to_binary(Before)] || Before /= undefined],
    Path = erlang:iolist_to_binary(PathList),
    get_messages(Path, Creds).

create_room({_AliceJID, _} = Creds, RoomName, Subject) ->
    Room = #{name => RoomName,
             subject => Subject},
    {?CREATED, {Result}} = rest_helper:post(client, <<"/rooms">>, Room, Creds),
    proplists:get_value(<<"id">>, Result).

create_room_with_id({_AliceJID, _} = Creds, RoomName, Subject, RoomID) ->
    Res = create_room_with_id_request(Creds, RoomName, Subject, RoomID),
    case Res of
        {?CREATED, {Result}} ->
            proplists:get_value(<<"id">>, Result);
        _ ->
            ct:fail(#{issue => create_room_with_id_failed,
                      result => Res,
                      creds => Creds,
                      room_name => RoomName,
                      subject => Subject,
                      room_id => RoomID})
    end.

create_room_with_id_request(Creds, RoomName, Subject, RoomID) ->
    Room = #{name => RoomName,
             subject => Subject},
    Path = <<"/rooms/", RoomID/binary>>,
    putt(client, Path, Room, Creds).

get_my_rooms(User) ->
    Creds = credentials(User),
    {?OK, Rooms} = rest_helper:gett(client, <<"/rooms">>, Creds),
    Rooms.

assert_messages([], []) ->
    ok;
assert_messages([SentMsg | SentRest], [RecvMsg | RecvRest]) ->
    FromJID = maps:get(from, SentMsg),
    FromJID = maps:get(from, RecvMsg),
    MsgId = maps:get(id, SentMsg),
    MsgId = maps:get(id, RecvMsg), %checks if there is an ID
    _ = maps:get(timestamp, RecvMsg), %checks if there ia timestamp
    MsgBody = maps:get(body, SentMsg),
    MsgBody = maps:get(body, RecvMsg),
    assert_messages(SentRest, RecvRest);
assert_messages(_Sent, _Recv) ->
    ct:fail("Send and Recv messages are not equal").

send_messages(Config, Alice, Bob, Kate) ->
    M1 = send_message(bob, Bob, Alice),
    M2 = send_message(alice, Alice, Bob),
    M3 = send_message(kate, Kate, Alice),
    mam_helper:maybe_wait_for_archive(Config),
    [M1, M2, M3].

assert_aff_change_stanza(Stanza, Target, Change) ->
    TargetJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Target)),
    ID = exml_query:attr(Stanza, <<"id">>),
    true = is_binary(ID) andalso ID /= <<>>,
    Users = exml_query:paths(Stanza, [{element, <<"x">>}, {element, <<"user">>}]),
    [User] = [User || User <- Users, TargetJID == exml_query:cdata(User)],
    Change = exml_query:attr(User, <<"affiliation">>),
    TargetJID = exml_query:cdata(User).

assert_room_info(Owner, RoomInfo) ->
    true = is_property_present(<<"subject">>, RoomInfo),
    true = is_property_present(<<"name">>, RoomInfo),
    true = is_property_present(<<"participants">>, RoomInfo),
    true = is_participant(Owner, <<"owner">>, RoomInfo).

is_property_present(Name, Proplist) ->
    Val = proplists:get_value(Name, Proplist),
    Val /= undefined.

assert_property_value(Name, Value, Proplist) ->
    Val = proplists:get_value(Name, Proplist),
    ?assertEqual(Value, Val).

is_participant(User, Role, RoomInfo) ->
    Participants = proplists:get_value(<<"participants">>, RoomInfo),
    JID = user_jid(User),
    Fun = fun({Props}) ->
                  UserJID = proplists:get_value(<<"user">>, Props),
                  UserRole = proplists:get_value(<<"role">>, Props),
                  UserJID == JID andalso UserRole == Role
          end,
    lists:any(Fun, Participants).

connect_to_sse(User) ->
    Port = ct:get_config({hosts, mim, http_api_client_endpoint_port}),
    sse_helper:connect_to_sse(Port, "/api/sse", credentials(User), #{transport => tls,
        tls_opts => [{verify, verify_none}]}).

assert_json_message(Sent, Received) ->
    #{<<"body">> := Body,
      <<"to">> := To,
      <<"from">> := From,
      <<"id">> := Id} = Received,

    Body = maps:get(body, Sent),
    To = maps:get(to, Sent),
    From = maps:get(from, Sent),
    Id = maps:get(id, Sent).

assert_json_room_sse_message(Expected, Received) ->
    #{<<"from">> := From,
      <<"room">> := Room,
      <<"id">> := _Id,
      <<"type">> := Type} = Received,

    Room = maps:get(room, Expected),
    Type = maps:get(type, Expected),
    From = maps:get(from, Expected),
    case Type of
        <<"message">> ->
            Body = maps:get(<<"body">>, Received),
            Body = maps:get(body, Expected);
        _ ->
            User = maps:get(<<"user">>, Received),
            User = maps:get(user, Expected)
    end.


add_contact_and_invite(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            AliceJID = escalus_utils:jid_to_lower(
                            escalus_client:short_jid(Alice)),
            BCred = credentials({bob, Bob}),
            % bob has empty roster
            {?OK, R} = gett(client, "/contacts", BCred),
            Res = decode_maplist(R),
            [] = Res,
            % adds Alice
            add_contact_check_roster_push(Alice, {bob, Bob}),
            % and she is in his roster, with empty status
            {?OK, R2} = gett(client, "/contacts", BCred),
            Result = decode_maplist(R2),
            [Res2] = Result,
            #{jid := AliceJID, subscription := <<"none">>,
              ask := <<"none">>} = Res2,
            % he invites her
            PutPath = lists:flatten(["/contacts/", binary_to_list(AliceJID)]),
            {?NOCONTENT, _} = putt(client, PutPath,
                                   #{action => <<"invite">>},
                                   BCred),
            % another roster push
            Push2 = escalus:wait_for_stanza(Bob),
            escalus:assert(is_roster_set, Push2),
            ct:log("Push2: ~p", [Push2]),
            % she receives  a subscription request
            Sub = escalus:wait_for_stanza(Alice),
            escalus:assert(is_presence_with_type, [<<"subscribe">>], Sub),
            % in his roster she has a changed 'ask' status
            {?OK, R3} = gett(client, "/contacts", BCred),
            Result3 = decode_maplist(R3),
            [Res3] = Result3,
            #{jid := AliceJID, subscription := <<"none">>,
              ask := <<"out">>} = Res3,
            % adds him to her contacts
            escalus:send(Alice, escalus_stanza:roster_add_contact(Bob,
                         [], <<"Bob">>)),
            PushReqB = escalus:wait_for_stanza(Alice),
            escalus:assert(is_roster_set, PushReqB),
            escalus:send(Alice, escalus_stanza:iq_result(PushReqB)),
            escalus:assert(is_iq_result, escalus:wait_for_stanza(Alice)),
            %% Alice sends subscribed presence
            escalus:send(Alice,
                         escalus_stanza:presence_direct(
                             escalus_client:short_jid(Bob),
                             <<"subscribed">>)),
            %% Wait for push before trying to query endpoint
            %% If we just call endpoint,
            %% the "subscribed" stanza can not yet be processed.
            Push3 = escalus:wait_for_stanza(Bob),
            ct:log("Push3 ~p", [Push3]),
            escalus:assert(is_roster_set, Push3),

            % now check Bob's roster
            {?OK, R4} = gett(client, "/contacts", BCred),
            Result4 = decode_maplist(R4),
            [Res4] = Result4,
            #{jid := AliceJID, subscription := <<"to">>,
                ask := <<"none">>} = Res4,
            ok
        end
    ),
    ok.

add_contact_and_be_invited(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            AliceJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Alice)),
            BCred = credentials({bob, Bob}),
            % bob has empty roster
            {?OK, R} = gett(client, "/contacts", BCred),
            Res = decode_maplist(R),
            [] = Res,
            % adds Alice
            add_contact_check_roster_push(Alice, {bob, Bob}),
            % and she is in his roster, with empty status
            {?OK, R2} = gett(client, "/contacts", BCred),
            Result = decode_maplist(R2),
            [Res2] = Result,
            #{jid := AliceJID, subscription := <<"none">>,
              ask := <<"none">>} = Res2,
            %% she adds him and invites
            escalus:send(Alice, escalus_stanza:roster_add_contact(Bob,
                         [],
                         <<"Bobek">>)),
            escalus:assert_many([is_roster_set, is_iq_result],
                                escalus:wait_for_stanzas(Alice, 2)),
            escalus:send(Alice,
                         escalus_stanza:presence_direct(
                             escalus_client:short_jid(Bob),
                             <<"subscribe">>)),
            escalus:assert(is_roster_set, escalus:wait_for_stanza(Alice)),
            escalus:assert(is_presence_with_type, [<<"subscribe">>],
                           escalus:wait_for_stanza(Bob)),
            % now check Bob's roster, and it is the same...
            {?OK, R4} = gett(client, "/contacts", BCred),
            [Res4] = decode_maplist(R4),
            #{jid := AliceJID, subscription := <<"none">>,
                ask := <<"in">>} = Res4,
            % because although it is stated in RFC3921, 8.2.6 that {none, in}
            % should be hidden from user, we changed it in REST API
            % he accepts
            PutPath = lists:flatten(["/contacts/", binary_to_list(AliceJID)]),
            {?NOCONTENT, _} = putt(client, PutPath,
                                   #{action => <<"accept">>},
                                   BCred),
            escalus:assert(is_roster_set, escalus:wait_for_stanza(Bob)),
            IsSub = fun(S) ->
                        escalus_pred:is_presence_with_type(<<"subscribed">>, S)
                    end,
            escalus:assert_many([is_roster_set, IsSub,
                                 is_presence],
                                escalus:wait_for_stanzas(Alice, 3)),
            ok
        end
    ),
    ok.

is_subscription_remove(User) ->
    IsSubscriptionRemove = fun(El) ->
                Sub = exml_query:paths(El, [{element, <<"query">>},
                                            {element, <<"item">>},
                                            {attr, <<"subscription">>}]),
                Sub == [<<"remove">>]
                end,
    escalus:assert(IsSubscriptionRemove, escalus:wait_for_stanza(User)).



add_and_remove(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            AliceJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Alice)),
            BCred = credentials({bob, Bob}),
            % adds Alice
            add_contact_check_roster_push(Alice, {bob, Bob}),
            % Check if Contact is in Bob's roster
            {?OK, R2} = gett(client, "/contacts", BCred),
            Result = decode_maplist(R2),
            [Res2] = Result,
            #{jid := AliceJID, subscription := <<"none">>,
              ask := <<"none">>} = Res2,
            % delete user
            DelPath = lists:flatten(["/contacts/", binary_to_list(AliceJID)]),
            {?NOCONTENT, _} = delete(client, DelPath, BCred),
            % Bob's roster is empty again
            {?OK, R3} = gett(client, "/contacts", BCred),
            [] = decode_maplist(R3),
            is_subscription_remove(Bob),
            ok
        end
    ),
    ok.


add_and_remove_some_contacts_properly(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}, {mike, 1}],
        fun(Alice, Bob, Kate, Mike) ->
            BCred = credentials({bob, Bob}),
            % adds all the other users
            lists:foreach(fun(AddContact) ->
                                  add_contact_check_roster_push(AddContact, {bob, Bob}) end,
                         [Alice, Kate, Mike]),
            AliceJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Alice)),
            KateJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Kate)),
            MikeJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Mike)),
            _AliceContact = create_contact(AliceJID),
            _KateContact = create_contact(KateJID),
            MikeContact = create_contact(MikeJID),
            % delete Alice and Kate
            Body = jiffy:encode(#{<<"to_delete">> => [AliceJID, KateJID]}),
            {?OK, {[{<<"not_deleted">>,[]}]}} = delete(client, "/contacts", BCred, Body),
            % Bob's roster consists now of only Mike
            {?OK, R4} = gett(client, "/contacts", BCred),
            [MikeContact] = decode_maplist(R4),
            is_subscription_remove(Bob),
            ok
        end
    ),
    ok.


add_and_remove_some_contacts_with_nonexisting(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}, {mike, 1}],
        fun(Alice, Bob, Kate, Mike) ->
            BCred = credentials({bob, Bob}),
            % adds all the other users
            lists:foreach(fun(AddContact) ->
                                  add_contact_check_roster_push(AddContact, {bob, Bob}) end,
                         [Alice, Kate]),
            AliceJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Alice)),
            KateJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Kate)),
            MikeJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Mike)),
            _AliceContact = create_contact(AliceJID),
            _KateContact = create_contact(KateJID),
            _MikeContact = create_contact(MikeJID),
            % delete Alice, Kate and Mike (who is absent)
            Body = jiffy:encode(#{<<"to_delete">> => [AliceJID, KateJID, MikeJID]}),
            {?OK, {[{<<"not_deleted">>,[MikeJID]}]}} = delete(client, "/contacts", BCred, Body),
            % Bob's roster is empty now
            {?OK, R4} = gett(client, "/contacts", BCred),
            [] = decode_maplist(R4),
            is_subscription_remove(Bob),
            ok
        end
    ),
    ok.

create_contact(JID) ->
    #{jid => JID, subscription => <<"none">>,
                             ask => <<"none">>}.

add_contact_check_roster_push(Contact, {_, RosterOwnerSpec} = RosterOwner) ->
    ContactJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Contact)),
    RosterOwnerCreds = credentials(RosterOwner),
    {?NOCONTENT, _} = post(client, <<"/contacts">>, #{jid => ContactJID},
                            RosterOwnerCreds),
    Push = escalus:wait_for_stanza(RosterOwnerSpec),
    escalus:assert(is_roster_set, Push),
    ok.

roster_errors(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun roster_errors_story/2).

roster_errors_story(Alice, Bob) ->
    AliceJID = user_jid(Alice),
    BCred = credentials({bob, Bob}),
    {?BAD_REQUEST, <<"Missing JID">>} =
        post(client, <<"/contacts">>, #{}, BCred),
    {?BAD_REQUEST, <<"Invalid JID: @invalid">>} =
        post(client, <<"/contacts">>, #{jid => <<"@invalid">>}, BCred),
    {?BAD_REQUEST, <<"Invalid action">>} =
        putt(client, <<"/contacts/", AliceJID/binary>>, #{action => <<"nosuchaction">>}, BCred),
    {?BAD_REQUEST, <<"Missing action">>} =
        putt(client, <<"/contacts/", AliceJID/binary>>, #{}, BCred),
    {?NOT_FOUND, _} =
        post(client, <<"/contacts">>, #{jid => <<"zorro@localhost">>}, BCred),
    {?NOT_FOUND, _} =
        putt(client, <<"/contacts/zorro@localhost">>, #{action => <<"invite">>}, BCred),
    {?NOT_FOUND, _} =
        gett(client, <<"/contacts/zorro@localhost">>, BCred),
    {?NOT_FOUND, _} =
        delete(client, <<"/contacts/zorro@localhost">>, BCred).

-spec room_jid(RoomID :: binary(), Config :: list()) -> RoomJID :: binary().
room_jid(RoomID, Config) ->
    MUCLightHost = config_to_muc_host(Config),
    <<RoomID/binary, "@", MUCLightHost/binary>>.

default_http_server_name_is_returned_if_not_changed(_Config) ->
    %% GIVEN MIM1 uses default name
    verify_server_name_in_header(distributed_helper:mim(), <<"Cowboy">>).

non_default_http_server_name_is_returned_if_configured(_Config) ->
    %% GIVEN MIM2 uses name "Classified"
    verify_server_name_in_header(distributed_helper:mim2(), <<"Classified">>).

verify_server_name_in_header(Server, ExpectedName) ->
    % WHEN unathenticated user makes a request to nonexistent path
    ReqParams = #{
      role => client,
      method => <<"GET">>,
      path => "/contacts/zorro@localhost",
      body => <<>>,
      return_headers => true,
      server => Server
     },
    {?UNAUTHORIZED, Headers2, _} = rest_helper:make_request(ReqParams),
    % THEN expected server name is returned
    ExpectedName = proplists:get_value(<<"server">>, Headers2).

config_to_muc_host(Config) ->
    ?config(muc_light_host, Config).

get_client_api_listener() ->
    Handler = #{module => mongoose_client_api},
    ListenerOpts = #{handlers => [Handler]},
    [Listener] = mongoose_helper:get_listeners(distributed_helper:mim(), ListenerOpts),
    Listener.
