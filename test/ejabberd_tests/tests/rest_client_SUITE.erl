-module(rest_client_SUITE).
-compile(export_all).

all() ->
    [{group, all}].

groups() ->
    [{all, [parallel], test_cases()}].

test_cases() ->
    [msg_is_sent_and_delivered,
     all_messages_are_archived,
     messages_with_user_are_archived,
     messages_can_be_paginated,
     room_is_created,
     user_is_invited_to_a_room,
     user_is_removed_from_a_room,
     rooms_can_be_listed,
     owner_can_leave_a_room_and_auto_select_owner,
     user_can_leave_a_room,
     invitation_to_room_is_forbidden_for_non_memeber,
     msg_is_sent_and_delivered_in_room,
     messages_are_archived_in_room,
     only_room_participant_can_read_messages,
     messages_can_be_paginated_in_room
     ].

init_per_suite(C) ->
    Host = ct:get_config({hosts, mim, domain}),
    MUCLightHost = <<"muclight.", Host/binary>>,
    C1 = rest_helper:maybe_enable_mam(mam_helper:backend(), Host, C),
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
    MAMTestCases = [all_messages_are_archived,
                    messages_with_user_are_archived,
                    messages_can_be_paginated,
                    messages_are_archived_in_room,
                    only_room_participant_can_read_messages,
                    messages_can_be_paginated_in_room
                   ],
    rest_helper:maybe_skip_mam_test_cases(TC, MAMTestCases, Config).

end_per_testcase(TC, C) ->
    escalus:end_per_testcase(TC, C).

msg_is_sent_and_delivered(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        M = send_message(alice, Alice, Bob),
        Msg = escalus:wait_for_stanza(Bob),
        escalus:assert(is_chat_message, [maps:get(body, M)], Msg)
    end).

all_messages_are_archived(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}, {kate, 1}], fun(Alice, Bob, Kate) ->
        Sent = [M1 | _] = send_messages(Config, Alice, Bob, Kate),
        AliceJID = maps:get(to, M1),
        AliceCreds = {AliceJID, user_password(alice)},
        GetPath = lists:flatten("/messages/"),
        {{<<"200">>, <<"OK">>}, Msgs} = rest_helper:gett(GetPath, AliceCreds),
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
        {{<<"200">>, <<"OK">>}, Msgs} = rest_helper:gett(GetPath, AliceCreds),
        Recv = [_Msg2] = rest_helper:decode_maplist(Msgs),
        assert_messages([M3], Recv)

    end).

messages_can_be_paginated(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
        BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
        rest_helper:fill_archive(Alice, Bob),
        mam_helper:maybe_wait_for_backend(Config),
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
        RoomJID = <<RoomID/binary, "@muclight.localhost">>,
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

messages_are_archived_in_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        {RoomID, Msgs} = given_new_room_with_users_and_msgs({alice, Alice}, [{bob, Bob}]),
        mam_helper:maybe_wait_for_backend(Config),
        {{<<"200">>, <<"OK">>}, Result} = get_room_messages({alice, Alice}, RoomID),
        [Aff, _Msg1, _Msg2] = MsgsRecv = rest_helper:decode_maplist(Result),
        %% The oldest message is aff change
        <<"affiliation">> = maps:get(type, Aff),
        <<"member">> = maps:get(affiliation, Aff),
        BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
        BobJID = maps:get(user, Aff)
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
    rest_helper:gett(Path, Creds).

messages_can_be_paginated_in_room(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        RoomID = given_new_room_with_users({alice, Alice}, [{bob, Bob}]),
        [GenMsgs1, GenMsgs2 | _] = Msgs = rest_helper:fill_room_archive(RoomID, [Alice, Bob]),
        mam_helper:maybe_wait_for_backend(Config),
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

assert_room_messages(RecvMsg, {_ID, _GenFrom, GenMsg}) ->
    escalus:assert(is_chat_message, [maps:get(body, RecvMsg)], GenMsg),
    ok.

get_room_info(User, RoomID) ->
    Creds = credentials(User),
    {{<<"200">>, <<"OK">>}, {Result}} = rest_helper:gett(<<"/rooms/", RoomID/binary>>,
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
    Creds = credentials(Sender),
    Path = <<"/rooms/", RoomID/binary, "/messages">>,
    Body = #{body => <<"Hi all!">>},
    {{<<"200">>, <<"OK">>}, {Result}} = rest_helper:post(Path, Body, Creds),
    MsgId = proplists:get_value(<<"id">>, Result),
    true = is_binary(MsgId),
    Body#{id => MsgId}.

given_new_room_with_users(Owner, Users) ->
    RoomID = given_new_room(Owner),
    [given_user_invited(Owner, RoomID, User) || {_, User} <- Users],
    RoomID.

given_new_room(Owner) ->
    Creds = credentials(Owner),
    RoomName = <<"new_room_name">>,
    create_room(Creds, RoomName, <<"This room subject">>).

given_user_invited({_, Inviter} = Owner, RoomID, Invitee) ->
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Invitee)),
    {{<<"204">>, <<"No Content">>}, _} = invite_to_room(Owner, RoomID, JID),
    Stanza = escalus:wait_for_stanza(Invitee),
    assert_aff_change_stanza(Stanza, Invitee, <<"member">>),
    Stanza2 = escalus:wait_for_stanza(Inviter),
    assert_aff_change_stanza(Stanza2, Invitee, <<"member">>).

invite_to_room(Inviter, RoomID, Invitee) ->
    Body = #{user => Invitee},
    Creds = credentials(Inviter),
    rest_helper:post(<<"/rooms/", RoomID/binary, "/users">>, Body, Creds).

remove_user_from_a_room(Inviter, RoomID, Invitee) ->
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(Invitee)),
    Creds = credentials(Inviter),
    Path = <<"/rooms/", RoomID/binary, "/users/", JID/binary>>,
    rest_helper:delete(Path, Creds).

credentials({User, UserClient}) ->
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(UserClient)),
    {JID, user_password(User)}.


user_password(User) ->
    [{User, Props}] = escalus:get_users([User]),
    proplists:get_value(password, Props).

send_message(User, From, To) ->
    AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(From)),
    BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(To)),
    M = #{to => BobJID, body => <<"hello, ", BobJID/binary, " it's me">>},
    Cred = {AliceJID, user_password(User)},
    {{<<"200">>, <<"OK">>}, {Result}} = rest_helper:post(<<"/messages">>, M, Cred),
    ID = proplists:get_value(<<"id">>, Result),
    M#{id => ID, from => AliceJID}.

get_messages(MeCreds, Other, Count) ->
    GetPath = lists:flatten(["/messages/",
                             binary_to_list(Other),
                             "?limit=", integer_to_list(Count)]),
    get_messages(GetPath, MeCreds).

get_messages(Path, Creds) ->
    {{<<"200">>, <<"OK">>}, Msgs} = rest_helper:gett(Path, Creds),
    rest_helper:decode_maplist(Msgs).

get_messages(MeCreds, Other, Before, Count) ->
    GetPath = lists:flatten(["/messages/",
                             binary_to_list(Other),
                             "?before=", integer_to_list(Before),
                             "&limit=", integer_to_list(Count)]),
    get_messages(GetPath, MeCreds).


get_room_messages(Client, RoomID, Count) ->
    get_room_messages(Client, RoomID, Count, undefined).

get_room_messages(Client, RoomID, Count, Before) ->
    Creds = credentials(Client),
    BasePathList = ["/rooms/", RoomID, "/messages?limit=", integer_to_binary(Count)],
    PathList = BasePathList ++ [["&before=", integer_to_binary(Before)] || Before /= undefined],
    Path = erlang:iolist_to_binary(PathList),
    get_messages(Path, Creds).

create_room({_AliceJID, _} = Creds, RoomID, Subject) ->
    Room = #{name => RoomID,
             subject => Subject},
    {{<<"200">>, <<"OK">>}, {Result}} = rest_helper:post(<<"/rooms">>, Room, Creds),
    proplists:get_value(<<"id">>, Result).

get_my_rooms(User) ->
    Creds = credentials(User),
    {{<<"200">>, <<"OK">>}, Rooms} = rest_helper:gett(<<"/rooms">>, Creds),
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
    mam_helper:maybe_wait_for_backend(Config),
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

is_participant(User, Role, RoomInfo) ->
    Participants = proplists:get_value(<<"participants">>, RoomInfo),
    JID = escalus_utils:jid_to_lower(escalus_client:short_jid(User)),
    Fun = fun({Props}) ->
                  UserJID = proplists:get_value(<<"user">>, Props),
                  UserRole = proplists:get_value(<<"role">>, Props),
                  UserJID == JID andalso UserRole == Role
          end,
    lists:any(Fun, Participants).

