-module(rest_client_SUITE).
-compile(export_all).

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

-import(muc_light_helper,
        [set_mod_config/3]).

-define(PRT(X, Y), ct:pal("~p: ~p", [X, Y])).
-define(OK, {<<"200">>, <<"OK">>}).
-define(CREATED, {<<"201">>, <<"Created">>}).
-define(NOCONTENT, {<<"204">>, <<"No Content">>}).
-define(ERROR, {<<"500">>, _}).
-define(NOT_FOUND, {<<"404">>, _}).
-define(NOT_IMPLEMENTED, {<<"501">>, _}).
-define(UNAUTHORIZED, {<<"401">>, <<"Unauthorized">>}).
-define(MUCHOST, <<"muclight.localhost">>).

%% --------------------------------------------------------------------
%% Common Test stuff
%% --------------------------------------------------------------------

all() ->
    [{group, messages},
     {group, muc},
     {group, muc_config},
     {group, roster},
     {group, messages_with_props},
     {group, security}].

groups() ->
    G = [{messages_with_props, [parallel], message_with_props_test_cases()},
         {messages, [parallel], message_test_cases()},
         {muc, [pararell], muc_test_cases()},
         {muc_config, [], muc_config_cases()},
         {roster, [parallel], roster_test_cases()},
         {security, [], security_test_cases()}],
    ct_helper:repeat_all_until_all_ok(G).

message_test_cases() ->
    [msg_is_sent_and_delivered_over_xmpp,
     msg_is_sent_and_delivered_over_sse,
     all_messages_are_archived,
     messages_with_user_are_archived,
     messages_can_be_paginated].

muc_test_cases() ->
     [room_is_created,
      room_is_created_with_given_identifier,
      user_is_invited_to_a_room,
      user_is_removed_from_a_room,
      rooms_can_be_listed,
      owner_can_leave_a_room_and_auto_select_owner,
      user_can_leave_a_room,
      invitation_to_room_is_forbidden_for_non_memeber,
      msg_is_sent_and_delivered_in_room,
      sending_message_with_wrong_body_results_in_bad_request,
      sending_message_with_no_body_results_in_bad_request,
      sending_markable_message_with_no_body_results_in_bad_request,
      sending_message_not_in_JSON_results_in_bad_request,
      sending_message_by_not_room_member_results_in_forbidden,
      messages_are_archived_in_room,
      chat_markers_are_archived_in_room,
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
      config_can_be_changed_by_all
    ].

roster_test_cases() ->
    [add_contact_and_invite,
     add_contact_and_be_invited,
     add_and_remove,
     add_and_remove_some_contacts_properly,
     add_and_remove_some_contacts_with_nonexisting,
     break_stuff].

message_with_props_test_cases() ->
    [
     msg_with_props_is_sent_and_delivered_over_xmpp,
     msg_with_props_can_be_parsed,
     msg_with_malformed_props_can_be_parsed,
     msg_with_malformed_props_is_sent_and_delivered_over_xmpp
     ].

security_test_cases() ->
    [
     default_http_server_name_is_returned_if_not_changed,
     non_default_http_server_name_is_returned_if_configured
    ].

init_per_suite(C) ->
    application:ensure_all_started(shotgun),
    Host = ct:get_config({hosts, mim, domain}),
    MUCLightHost = <<"muclight.", Host/binary>>,
    C1 = rest_helper:maybe_enable_mam(mam_helper:backend(), Host, C),
    dynamic_modules:start(Host, mod_muc_light,
                          [{host, binary_to_list(MUCLightHost)},
                           {rooms_in_rosters, true}]),
    [{muc_light_host, MUCLightHost} | escalus:init_per_suite(C1)].

end_per_suite(Config) ->
    escalus_fresh:clean(),
    Host = ct:get_config({hosts, mim, domain}),
    rest_helper:maybe_disable_mam(mam_helper:backend(), Host),
    dynamic_modules:stop(Host, mod_muc_light),
    application:stop(shotgun),
    escalus:end_per_suite(Config).

init_per_group(_GN, C) ->
    C.

end_per_group(_GN, C) ->
    C.

init_per_testcase(config_can_be_changed_by_all = CaseName, Config) ->
    DefaultConfig = dynamic_modules:save_modules(domain(Config), Config),
    set_mod_config(all_can_configure, true, ?MUCHOST),
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
                    msg_with_malformed_props_is_sent_and_delivered_over_xmpp
                   ],
    rest_helper:maybe_skip_mam_test_cases(TC, MAMTestCases, Config).

end_per_testcase(config_can_be_changed_by_all = CaseName, Config) ->
    set_mod_config(all_can_configure, false, ?MUCHOST),
    dynamic_modules:restore_modules(domain(Config), Config),
    escalus:end_per_testcase(config_can_be_changed_by_all, Config);

end_per_testcase(TC, C) ->
    escalus:end_per_testcase(TC, C).

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

    Conn = connect_to_sse({alice, Alice}),
    M = send_message(bob, Bob, Alice),

    Event = wait_for_event(Conn),
    Data = jiffy:decode(maps:get(data, Event), [return_maps]),

    assert_json_message(M, Data),

    stop_sse(Conn).

room_msg_is_sent_and_delivered_over_sse(ConfigIn) ->
    Config = escalus_fresh:create_users(ConfigIn, [{alice, 1}, {bob, 1}]),
    Bob = escalus_users:get_userspec(Config, bob),
    Alice = escalus_users:get_userspec(Config, alice),
    RoomID = given_new_room_with_users({alice, Alice}, [{bob, Bob}]),
    RoomInfo = get_room_info({alice, Alice}, RoomID),
    true = is_participant(Bob, <<"member">>, RoomInfo),
    Conn = connect_to_sse({bob, Bob}),
    Message = given_message_sent_to_room(RoomID, {alice, Alice}),
    Event = wait_for_event(Conn),
    Data = jiffy:decode(maps:get(data, Event), [return_maps]),
    assert_json_room_sse_message(Message#{room => RoomID, type => <<"message">>},
                                 Data),
    stop_sse(Conn).

aff_change_msg_is_delivered_over_sse(ConfigIn) ->
    Config = escalus_fresh:create_users(ConfigIn, [{alice, 1}, {bob, 1}]),
    Bob = escalus_users:get_userspec(Config, bob),
    Alice = escalus_users:get_userspec(Config, alice),
    RoomID = given_new_room({alice, Alice}),
    Conn = connect_to_sse({bob, Bob}),
    given_user_invited({alice, Alice}, RoomID, Bob),
    Event = wait_for_event(Conn),
    Data = jiffy:decode(maps:get(data, Event), [return_maps]),
    BobJID = user_jid(Bob),
    Host = ct:get_config({hosts, mim, domain}),
    RoomJID = <<RoomID/binary, "@muclight.", Host/binary>>,
    assert_json_room_sse_message(#{room => RoomID,
                                   from => RoomJID,
                                   type => <<"affiliation">>,
                                   user => BobJID},
                                 Data),
    stop_sse(Conn).

all_messages_are_archived(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        Sent = [M1 | _] = send_messages(Config, Alice, Bob, Kate),
        AliceJID = maps:get(to, M1),
        AliceCreds = {AliceJID, user_password(alice)},
        GetPath = lists:flatten("/messages/"),
        {{<<"200">>, <<"OK">>}, Msgs} = rest_helper:gett(client, GetPath, AliceCreds),
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
        {{<<"200">>, <<"OK">>}, Msgs} = rest_helper:gett(client, GetPath, AliceCreds),
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

room_is_created(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        RoomID = given_new_room({alice, Alice}),
        RoomInfo = get_room_info({alice, Alice}, RoomID),
        assert_room_info(Alice, RoomInfo)
    end).

room_is_created_with_given_identifier(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        GivenRoomID = <<"just_an_id">>,
        GivenRoomID = given_new_room({alice, Alice}, GivenRoomID),
        RoomInfo = get_room_info({alice, Alice}, GivenRoomID),
        assert_room_info(Alice, RoomInfo)
    end).

config_can_be_changed_by_owner(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        RoomID = <<"som_othere_id">>,
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
        {{<<"403">>,<<"Forbidden">>},<<>>} =
            when_config_change({bob, Bob}, RoomJID, <<"other_name">>, <<"other_subject">>),
        NewRoomInfo = get_room_info({bob, Bob}, RoomID),
        assert_property_value(<<"name">>,<<"new_room_name">>, NewRoomInfo)
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
        Host = ct:get_config({hosts, mim, domain}),
        RoomJID = <<RoomID/binary, "@muclight.", Host/binary>>,
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

invitation_to_room_is_forbidden_for_non_memeber(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room({alice, Alice}),
        {{<<"403">>, <<"Forbidden">>}, _ } = invite_to_room({bob, Bob}, RoomID,
                                                            <<"auser@domain.com">>)
    end).

msg_is_sent_and_delivered_in_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        given_new_room_with_users_and_msgs({alice, Alice}, [{bob, Bob}])
    end).


sending_message_by_not_room_member_results_in_forbidden(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        Sender = {alice, Alice},
        RoomID = given_new_room_with_users(Sender, []),
        Result = given_message_sent_to_room(RoomID, {bob, Bob}, #{body => <<"Hello, I'm not member">>}),
        ?assertMatch({{<<"403">>, <<"Forbidden">>}, _}, Result)

    end).

sending_message_with_wrong_body_results_in_bad_request(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Sender = {alice, Alice},
        RoomID = given_new_room_with_users(Sender, []),
        Result = given_message_sent_to_room(RoomID, Sender, #{body => #{nested => <<"structure">>}}),
        ?assertMatch({{<<"400">>, <<"Bad Request">>}, <<"Invalid body, it must be a string">>}, Result)
    end).

sending_message_with_no_body_results_in_bad_request(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Sender = {alice, Alice},
        RoomID = given_new_room_with_users(Sender, []),
        Result = given_message_sent_to_room(RoomID, Sender, #{no_body => <<"This should be in body element">>}),
        ?assertMatch({{<<"400">>, <<"Bad Request">>}, <<"No valid message elements">>}, Result)
    end).

sending_markable_message_with_no_body_results_in_bad_request(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Sender = {alice, Alice},
        RoomID = given_new_room_with_users(Sender, []),
        Result = given_message_sent_to_room(RoomID, Sender, #{markable => true}),
        ?assertMatch({{<<"400">>, <<"Bad Request">>}, <<"No valid message elements">>}, Result)
    end).

sending_message_not_in_JSON_results_in_bad_request(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        Sender = {alice, Alice},
        RoomID = given_new_room_with_users(Sender, []),
        Result = given_message_sent_to_room(RoomID, Sender, <<"This is not JSON object">>),
        ?assertMatch({{<<"400">>, <<"Bad Request">>}, <<"Request body is not a valid JSON">>}, Result)
    end).

messages_are_archived_in_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {RoomID, Msgs} = given_new_room_with_users_and_msgs({alice, Alice}, [{bob, Bob}]),
        mam_helper:maybe_wait_for_archive(Config),
        {{<<"200">>, <<"OK">>}, Result} = get_room_messages({alice, Alice}, RoomID),
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
                              {{<<"200">>, <<"OK">>}, {_Result}} =
                              given_message_sent_to_room(RoomID, {bob, Bob}, Marker),
                              [ escalus:wait_for_stanza(Client) || Client <- [Alice, Bob] ]
                      end, Markers),
        mam_helper:maybe_wait_for_archive(Config),

        % WHEN an archive is queried via HTTP
        {{<<"200">>, <<"OK">>}, Result} = get_room_messages({alice, Alice}, RoomID),

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
        {{<<"200">>, <<"OK">>}, {_Result}}
        = given_message_sent_to_room(RoomID, {bob, Bob}, MarkableMsg),
        [ escalus:wait_for_stanza(Client) || Client <- [Alice, Bob] ],
        mam_helper:maybe_wait_for_archive(Config),

        % WHEN an archive is queried via HTTP
        {{<<"200">>, <<"OK">>}, Result} = get_room_messages({alice, Alice}, RoomID),

        % THEN the retrieved message has markable property
        [_Aff, Msg] = rest_helper:decode_maplist(Result),
        true = maps:get(markable, Msg, undefined)
    end).

only_room_participant_can_read_messages(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room({alice, Alice}),
        {{<<"403">>, <<"Forbidden">>}, _} = get_room_messages({bob, Bob}, RoomID),
        ok
    end).

get_room_messages(Caller, RoomID) ->
    Path = <<"/rooms/", RoomID/binary, "/messages">>,
    Creds = credentials(Caller),
    rest_helper:gett(client, Path, Creds).

messages_can_be_paginated_in_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room_with_users({alice, Alice}, [{bob, Bob}]),
        [GenMsgs1, GenMsgs2 | _] = Msgs = rest_helper:fill_room_archive(RoomID, [Alice, Bob]),
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

room_is_created_with_given_jid(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        RoomID = <<"some_id">>,
        RoomJID = room_jid(RoomID, Config),
        RoomID = given_new_room({alice, Alice}, RoomJID),
        RoomInfo = get_room_info({alice, Alice}, RoomID),
        assert_room_info(Alice, RoomInfo)
    end).

room_is_not_created_with_jid_not_matching_hostname(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        RoomID = <<"some_id">>,
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
        RoomID = <<"yet_another_id">>,
        RoomJID = room_jid(RoomID, Config),
        RoomID = given_new_room({alice, Alice}, RoomJID),
        RoomInfo = get_room_info({alice, Alice}, RoomJID),
        assert_room_info(Alice, RoomInfo)
    end).

messages_can_be_sent_and_fetched_by_room_jid(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
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
        Recv = [_Msg] = rest_helper:decode_maplist(M2),

        MsgID = maps:get(id, _Msg)

    end).

assert_room_messages(RecvMsg, {_ID, _GenFrom, GenMsg}) ->
    escalus:assert(is_chat_message, [maps:get(body, RecvMsg)], GenMsg),
    ok.

get_room_info(User, RoomID) ->
    Creds = credentials(User),
    {{<<"200">>, <<"OK">>}, {Result}} = rest_helper:gett(client, <<"/rooms/", RoomID/binary>>,
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
    {{<<"200">>, <<"OK">>}, {Result}} = HTTPResult,
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
    {{<<"204">>, <<"No Content">>}, _} = invite_to_room(Owner, RoomID, JID),
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
    {{<<"200">>, <<"OK">>}, {Result}} = post(client, <<"/messages">>, M, Cred),
    ID = proplists:get_value(<<"id">>, Result),
    M#{id => ID, from => AliceJID}.

get_messages(MeCreds, Other, Count) ->
    GetPath = lists:flatten(["/messages/",
                             binary_to_list(Other),
                             "?limit=", integer_to_list(Count)]),
    get_messages(GetPath, MeCreds).

get_messages(Path, Creds) ->
    {{<<"200">>, <<"OK">>}, Msgs} = rest_helper:gett(client, Path, Creds),
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
    {{<<"200">>, <<"OK">>}, Msgs} = rest_helper:gett(client, Path, Creds),
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
    {{<<"200">>, <<"OK">>}, {Result}} = rest_helper:post(client, <<"/rooms">>, Room, Creds),
    proplists:get_value(<<"id">>, Result).

create_room_with_id({_AliceJID, _} = Creds, RoomName, Subject, RoomID) ->
    Res = create_room_with_id_request(Creds, RoomName, Subject, RoomID),
    case Res of
        {{<<"201">>, <<"Created">>}, {Result}} ->
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
    {{<<"200">>, <<"OK">>}, Rooms} = rest_helper:gett(client, <<"/rooms">>, Creds),
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
    %% By default, gun prefers http2 protocol.
    %%
    %% The error occures with HTTP/2 enabled both on the client and the server:
    %% "{error,{stream_error,protocol_error, 'Stream reset by server.'}}"
    %%
    %% Disable HTTP/2 on the client side:
    GunOpts = #{protocols => [http]},
    ShotGunOpts = #{gun_opts => GunOpts},
    Port = ct:get_config({hosts, mim, http_api_client_endpoint_port}),
    {ok, Conn} = shotgun:open("localhost", Port, https, ShotGunOpts),
    Me = self(),
    EventFun = fun(State, Ref, Bin) ->
        Me ! {sse, State, Ref, Bin}
    end,

    {U, P} = credentials(User),
    Options = #{async => true, async_mode => sse, handle_event => EventFun},
    Headers = #{basic_auth => {binary_to_list(U), binary_to_list(P)}},
    case shotgun:get(Conn, "/api/sse", Headers, Options) of
        {ok, Ref} ->
            {Conn, Ref};
        Other ->
            ct:fail(#{issue => connect_to_sse_failed,
                      headers => Headers,
                      reason => Other,
                      url => "https://localhost:8089/api/sse"})
    end.

wait_for_event({_Conn, Ref}) ->
    receive
        {sse, _State, Ref, Bin} ->
            shotgun:parse_event(Bin)
    after
        5000 ->
            ct:fail("timeout waiting for SSE event")
    end.

stop_sse({Conn, _Ref}) ->
    shotgun:close(Conn).

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
        Config, [{alice, 1}, {bob, 1}, {kate, 1}, {carol, 1}],
        fun(Alice, Bob, Kate, Carol) ->
            BCred = credentials({bob, Bob}),
            % adds all the other users
            lists:foreach(fun(AddContact) ->
                                  add_contact_check_roster_push(AddContact, {bob, Bob}) end,
                         [Alice, Kate, Carol]),
            AliceJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Alice)),
            KateJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Kate)),
            CarolJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Carol)),
            AliceContact = create_contact(AliceJID),
            KateContact = create_contact(KateJID),
            CarolContact = create_contact(CarolJID),
            % delete Alice and Kate
            Body = jiffy:encode(#{<<"to_delete">> => [AliceJID, KateJID]}),
            {?OK, {[{<<"not_deleted">>,[]}]}} = delete(client, "/contacts", BCred, Body),
            % Bob's roster consists now of only Carol
            {?OK, R4} = gett(client, "/contacts", BCred),
            [CarolContact] = decode_maplist(R4),
            is_subscription_remove(Bob),
            ok
        end
    ),
    ok.


add_and_remove_some_contacts_with_nonexisting(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}, {kate, 1}, {carol, 1}],
        fun(Alice, Bob, Kate, Carol) ->
            BCred = credentials({bob, Bob}),
            % adds all the other users
            lists:foreach(fun(AddContact) ->
                                  add_contact_check_roster_push(AddContact, {bob, Bob}) end,
                         [Alice, Kate]),
            AliceJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Alice)),
            KateJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Kate)),
            CarolJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Carol)),
            AliceContact = create_contact(AliceJID),
            KateContact = create_contact(KateJID),
            CarolContact = create_contact(CarolJID),
            % delete Alice, Kate and Carol (who is absent)
            Body = jiffy:encode(#{<<"to_delete">> => [AliceJID, KateJID, CarolJID]}),
            {?OK, {[{<<"not_deleted">>,[CarolJID]}]}} = delete(client, "/contacts", BCred, Body),
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


break_stuff(Config) ->
    escalus:fresh_story(
        Config, [{alice, 1}, {bob, 1}],
        fun(Alice, Bob) ->
            AliceJID = escalus_utils:jid_to_lower(
                escalus_client:short_jid(Alice)),
            BCred = credentials({bob, Bob}),
            AddContact = #{jid => AliceJID},
            {?NOCONTENT, _} = post(client, <<"/contacts">>, AddContact,
                BCred),
            PutPath = lists:flatten(["/contacts/", binary_to_list(AliceJID)]),
            {?NOT_IMPLEMENTED, _} = putt(client, PutPath,
                                         #{action => <<"nosuchaction">>},
                                         BCred),
            BadPutPath = "/contacts/zorro@localhost",
            {?NOT_FOUND, _} = putt(client, BadPutPath,
                                   #{action => <<"invite">>},
                                   BCred),
            BadGetPath = "/contacts/zorro@localhost",
            {?NOT_FOUND, _} = gett(client, BadGetPath, BCred),
            ok
        end
    ),
    ok.

-spec room_jid(RoomID :: binary(), Config :: list()) -> RoomJID :: binary().
room_jid(RoomID, Config) ->
    MUCLightHost = ?config(muc_light_host, Config),
    <<RoomID/binary, "@", MUCLightHost/binary>>.

domain(Config) ->
    ?config(muc_light_host, Config).

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

