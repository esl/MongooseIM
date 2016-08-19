-module(rest_client_SUITE).
-compile(export_all).

all() ->
    [{group, all}].

groups() ->
    [{all, [], test_cases()}].

test_cases() ->
    [msg_is_sent_and_delivered].

init_per_suite(C) ->
    escalus:init_per_suite(C).

end_per_suite(C) ->
    escalus_fresh:clean(),
    C.

init_per_group(_GN, C) ->
    C.

end_per_group(_GN, C) ->
    C.

init_per_testcase(TC, C) ->
    escalus:init_per_testcase(TC, C).

end_per_testcase(TC, C) ->
    escalus:end_per_testcase(TC, C).

msg_is_sent_and_delivered(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        AliceJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Alice)),
        BobJID = escalus_utils:jid_to_lower(escalus_client:short_jid(Bob)),
        M = #{to => BobJID, msg => <<"hello Bob, it's Alice">>},
        Cred = {AliceJID, user_password(alice)},
        {{<<"200">>, <<"OK">>}, _} = rest_helper:post(<<"/messages">>, M, Cred),
        Msg = escalus:wait_for_stanza(Bob),
        escalus:assert(is_chat_message, [maps:get(msg, M)], Msg)
    end).

user_password(User) ->
    [{User, Props}] = escalus:get_users([User]),
    proplists:get_value(password, Props).
