-module(rest_client_SUITE).
-compile(export_all).

all() ->
    [{group, all}].

groups() ->
    [{all, [], test_cases()}].

test_cases() ->
    [msg_is_sent_and_delivered,
     messages_are_archived].

init_per_suite(C) ->
    Host = ct:get_config({hosts, mim, domain}),
    C1 = rest_helper:maybe_enable_mam(mam_helper:backend(), Host, C),
    escalus:init_per_suite(C1).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    Host = ct:get_config({hosts, mim, domain}),
    rest_helper:maybe_disable_mam(proplists:get_value(mam_enabled, Config), Host),
    escalus:end_per_suite(Config).

init_per_group(_GN, C) ->
    C.

end_per_group(_GN, C) ->
    C.

init_per_testcase(TC, Config) ->
    MAMTestCases = [messages_are_archived],
    rest_helper:maybe_skip_mam_test_cases(TC, MAMTestCases, Config).

end_per_testcase(TC, C) ->
    escalus:end_per_testcase(TC, C).

msg_is_sent_and_delivered(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        M = send_message(alice, Alice, Bob),
        Msg = escalus:wait_for_stanza(Bob),
        escalus:assert(is_chat_message, [maps:get(msg, M)], Msg)
    end).

messages_are_archived(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        M1 = send_message(bob, Bob, Alice),
        M2 = send_message(alice, Alice, Bob),
        mam_helper:maybe_wait_for_yz(Config),
        AliceJID = maps:get(to, M1),
        BobJID = maps:get(to, M2),
        AliceCreds = {AliceJID, user_password(alice)},
        GetPath = lists:flatten(["/messages/", binary_to_list(BobJID), "/10"]),
        {{<<"200">>, <<"OK">>}, Msgs} = rest_helper:gett(GetPath, AliceCreds),
        [Msg1, _Msg2] = rest_helper:decode_maplist(Msgs),
        BobJID = maps:get(sender, Msg1),
        _ = maps:get(message_id, Msg1), %checks if there is an ID
        _ = maps:get(timestamp, Msg1), %checks if there ia timestamp
        BobMsgBody = maps:get(msg, M1),
        BobMsgBody = maps:get(body, Msg1)

    end).

user_password(User) ->
    [{User, Props}] = escalus:get_users([User]),
    proplists:get_value(password, Props).

send_message(User, From, To) ->
    AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(From)),
    BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(To)),
    M = #{to => BobJID, msg => <<"hello, ", BobJID/binary," it's me">>},
    Cred = {AliceJID, user_password(User)},
    {{<<"200">>, <<"OK">>}, _} = rest_helper:post(<<"/messages">>, M, Cred),
    M.

