-module(rest_client_SUITE).
-compile(export_all).

all() ->
    [{group, all}].

groups() ->
    [{all, [parallel], test_cases()}].

test_cases() ->
    [msg_is_sent_and_delivered,
     messages_are_archived,
%     messages_can_be_paginated,
     room_is_created,
     user_is_invited_to_a_room].

init_per_suite(C) ->
    Host = ct:get_config({hosts, mim, domain}),
    C1 = rest_helper:maybe_enable_mam(mam_helper:backend(), Host, C),
    MUCLightHost = <<"muclight.", Host/binary>>,
    dynamic_modules:start(Host, mod_muc_light,
                          [{host, binary_to_list(MUCLightHost)},
                           {rooms_in_rosters, true}]),
    escalus:init_per_suite(C1).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    Host = ct:get_config({hosts, mim, domain}),
    rest_helper:maybe_disable_mam(proplists:get_value(mam_enabled, Config), Host),
    dynamic_modules:stop(Host, mod_muc_light),
    escalus:end_per_suite(Config).

init_per_group(_GN, C) ->
    C.

end_per_group(_GN, C) ->
    C.

init_per_testcase(TC, Config) ->
    MAMTestCases = [messages_are_archived, messages_can_be_paginated],
    rest_helper:maybe_skip_mam_test_cases(TC, MAMTestCases, Config).

end_per_testcase(TC, C) ->
    escalus:end_per_testcase(TC, C).

msg_is_sent_and_delivered(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        M = send_message(alice, Alice, Bob),
        Msg = escalus:wait_for_stanza(Bob),
        escalus:assert(is_chat_message, [maps:get(body, M)], Msg)
    end).

messages_are_archived(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        M1 = send_message(bob, Bob, Alice),
        M2 = send_message(alice, Alice, Bob),
        mam_helper:maybe_wait_for_yz(Config),
        AliceJID = maps:get(to, M1),
        BobJID = maps:get(to, M2),
        AliceCreds = {AliceJID, user_password(alice)},
        GetPath = lists:flatten("/messages/"),
        {{<<"200">>, <<"OK">>}, Msgs} = rest_helper:gett(GetPath, AliceCreds),
        [Msg1, _Msg2] = rest_helper:decode_maplist(Msgs),
        BobJID = maps:get(from, Msg1),
        BobMsgId = maps:get(id, M1),
        BobMsgId = maps:get(id, Msg1), %checks if there is an ID
        _ = maps:get(timestamp, Msg1), %checks if there ia timestamp
        BobMsgBody = maps:get(body, M1),
        BobMsgBody = maps:get(body, Msg1)

    end).

messages_can_be_paginated(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
        BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
        rest_helper:fill_archive(Alice, Bob),
        mam_helper:maybe_wait_for_yz(Config),
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
        [Oldest|_] = rest_helper:decode_maplist(M3),
        <<"A">> = maps:get(body, Oldest),
        % same with limit
        M4 = get_messages(AliceCreds, BobJID, PriorTo, 2),
        2 = length(M4),
        [Oldest2|_] = rest_helper:decode_maplist(M4),
        <<"B">> = maps:get(body, Oldest2)
    end).

room_is_created(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
        Creds = {AliceJID, user_password(alice)},
        RoomName = <<"new_room_name">>,
        RoomID = create_room(Creds, RoomName, <<"This room subject">>),
        {{<<"200">>, <<"OK">>}, Result} = rest_helper:gett(<<"/rooms/", RoomID/binary>>,
                                                          Creds),
        ct:print("~p", [Result])
    end).

user_is_invited_to_a_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
        BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
        Creds = {AliceJID, user_password(alice)},
        RoomName = <<"new_room_id2">>,
        RoomID = create_room(Creds, RoomName, <<"This room subject 2">>),
        Body = #{user => BobJID},
        {{<<"204">>, <<"No Content">>}, _} = rest_helper:putt(<<"/rooms/", RoomID/binary>>,
                                                              Body, Creds),
        Stanza = escalus:wait_for_stanza(Bob),
        ct:print("~p", [Stanza])
    end).

user_password(User) ->
    [{User, Props}] = escalus:get_users([User]),
    proplists:get_value(password, Props).

send_message(User, From, To) ->
    AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(From)),
    BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(To)),
    M = #{to => BobJID, body => <<"hello, ", BobJID/binary," it's me">>},
    Cred = {AliceJID, user_password(User)},
    {{<<"200">>, <<"OK">>}, {Result}} = rest_helper:post(<<"/messages">>, M, Cred),
    ID = proplists:get_value(<<"id">>, Result),
    M#{id => ID}.

get_messages(MeCreds, Other, Count) ->
    GetPath = lists:flatten(["/messages/",
                             binary_to_list(Other),
                             "/", integer_to_list(Count)]),
    {{<<"200">>, <<"OK">>}, Msgs} = rest_helper:gett(GetPath, MeCreds),
    Msgs.

get_messages(MeCreds, Other, Before, Count) ->
    GetPath = lists:flatten(["/messages/",
                             binary_to_list(Other),
                             "/", integer_to_list(Before),
                             "/", integer_to_list(Count)]),
    {{<<"200">>, <<"OK">>}, Msgs} = rest_helper:gett(GetPath, MeCreds),
    Msgs.

create_room({AliceJID, _} = Creds, RoomID, Subject) ->
    Room = #{name => RoomID,
             subject => Subject},
    {{<<"200">>, <<"OK">>}, {Result}} = rest_helper:post(<<"/rooms">>, Room, Creds),
    proplists:get_value(<<"id">>, Result).

