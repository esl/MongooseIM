-module(xep_0352_csi_SUITE).

-include_lib("exml/include/exml.hrl").

-compile([export_all]).

all() ->
    [{group, basic}].

groups() ->
    [{basic, [], [alice_is_inactive_and_no_stanza_arrived,
                  alice_gets_msgs_after_activate]}].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_Group, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob])).

end_per_group(_Group, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob])).


init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

alice_is_inactive_and_no_stanza_arrived(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        given_client_is_inactive_and_message_sent(Alice, Bob),

        escalus_assert:has_no_stanzas(Alice)
    end).

alice_gets_msgs_after_activate(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %%Given
        given_client_is_inactive_and_message_sent(Alice, Bob),

        %%When client becomes active again
        escalus:send(Alice, csi_stanza(<<"active">>)),

        Msg = escalus:wait_for_stanza(Alice),
        escalus:assert(is_chat_message, Msg)
    end).

given_client_is_inactive_and_message_sent(Alice, Bob) ->
    %%Given
    given_client_is_inactive(Alice),

    %%When
    escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, Alice">>)),
    timer:sleep(timer:seconds(1)).


given_client_is_inactive(Alice) ->
    escalus:send(Alice, csi_stanza(<<"inactive">>)).


csi_stanza(Name) ->
    #xmlel{name = Name,
           attrs = [{<<"xmlns">>, <<"urn:xmpp:csi:0">>}]}.
