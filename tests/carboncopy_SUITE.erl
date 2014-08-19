%%%===================================================================
%%% @copyright (C) 2011, Erlang Solutions Ltd.
%%% @doc Suite for testing mod_carboncopy module
%%% @end
%%%===================================================================

-module(carboncopy_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml_stream.hrl").

%%%===================================================================
%%% Suite configuration
%%%===================================================================


all() ->
    [{group, mod_message_carbons_tests}].

all_tests() ->
    [carbon_tests_full_jid, carbon_tests_bare_jid, multiswitch_test, private_message].

groups() ->
    [{mod_message_carbons_tests, [sequence], all_tests()}].

suite() ->
    escalus:suite().

%%%===================================================================
%%% Init & teardown
%%%===================================================================

init_per_suite(Config0) ->
    escalus:init_per_suite(Config0).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, {by_name, [alice, bob]}).

end_per_group(_GroupName, _Config) ->
    escalus:delete_users(_Config, {by_name, [alice, bob]}).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% mod_carboncopy tests
%%--------------------------------------------------------------------

carbon_tests_full_jid(Config) ->

    escalus:story(Config, [3, 3], fun(Alice1, Alice2, Alice3, Bob1, Bob2, Bob3) ->

    send_iq_carbon(Bob1, <<"enable">>),
    send_iq_carbon(Bob2, <<"enable">>),
    send_iq_carbon(Bob3, <<"disable">>),
    send_iq_carbon(Alice1, <<"enable">>),
    send_iq_carbon(Alice2, <<"enable">>),
    send_iq_carbon(Alice3, <<"disable">>),

    escalus:send(Alice1, escalus_stanza:chat_to(escalus_client:full_jid(Bob1), <<"Hi Bob1!">>)),

    Bob_Stanza1 = escalus:wait_for_stanza(Bob1),
    Bob_Stanza2 = escalus:wait_for_stanza(Bob2),
    Alice_Stanza2 = escalus:wait_for_stanza(Alice2),

    escalus_assert:has_no_stanzas(Bob3),
    escalus_assert:has_no_stanzas(Alice3),
    escalus:assert(is_chat_message, [<<"Hi Bob1!">>], Bob_Stanza1),

    is_carbon(Bob_Stanza2, [<<"Hi Bob1!">>], <<"received">>),
    is_carbon(Alice_Stanza2, [<<"Hi Bob1!">>], <<"sent">>),

    escalus_assert:has_no_stanzas(Bob1),
    escalus_assert:has_no_stanzas(Bob2),
    escalus_assert:has_no_stanzas(Bob3),
    escalus_assert:has_no_stanzas(Alice1),
    escalus_assert:has_no_stanzas(Alice2),
    escalus_assert:has_no_stanzas(Alice3)

  end).

carbon_tests_bare_jid(Config) ->

  escalus:story(Config, [3, 3], fun(Alice1, Alice2, Alice3, Bob1, Bob2, Bob3) ->

    send_iq_carbon(Bob1, <<"enable">>),
    send_iq_carbon(Bob2, <<"enable">>),
    send_iq_carbon(Bob3, <<"disable">>),
    send_iq_carbon(Alice1, <<"enable">>),
    send_iq_carbon(Alice2, <<"enable">>),
    send_iq_carbon(Alice3, <<"disable">>),

    escalus:send(Alice1, escalus_stanza:chat_to(escalus_client:short_jid(Bob1), <<"Hi Bob1!">>)),

    Bob_Stanza1 = escalus:wait_for_stanza(Bob1),
    Bob_Stanza2 = escalus:wait_for_stanza(Bob2),
    Bob_Stanza3 = escalus:wait_for_stanza(Bob3),
    Alice_Stanza2 = escalus:wait_for_stanza(Alice2),

    escalus_assert:has_no_stanzas(Alice3),
    escalus:assert(is_chat_message, [<<"Hi Bob1!">>], Bob_Stanza1),
    escalus:assert(is_chat_message, [<<"Hi Bob1!">>], Bob_Stanza2),
    escalus:assert(is_chat_message, [<<"Hi Bob1!">>], Bob_Stanza3),

    is_carbon(Alice_Stanza2, [<<"Hi Bob1!">>], <<"sent">>),

    escalus_assert:has_no_stanzas(Bob1),
    escalus_assert:has_no_stanzas(Bob2),
    escalus_assert:has_no_stanzas(Bob3),
    escalus_assert:has_no_stanzas(Alice1),
    escalus_assert:has_no_stanzas(Alice2),
    escalus_assert:has_no_stanzas(Alice3)

  end).

multiswitch_test(Config) ->
    escalus:story(Config, [1, 2], fun(Alice1, Bob1, Bob2) ->

      send_iq_carbon(Bob1, <<"enable">>),
      send_iq_carbon(Bob2, <<"enable">>),
      send_iq_carbon(Bob2, <<"enable">>),

      escalus:send(Alice1, escalus_stanza:chat_to(escalus_client:full_jid(Bob1), <<"Hi Bob1!">>)),

      Bob_Stanza1 = escalus:wait_for_stanza(Bob1),
      Bob_Stanza2 = escalus:wait_for_stanza(Bob2),

      escalus:assert(is_chat_message, [<<"Hi Bob1!">>], Bob_Stanza1),

      is_carbon(Bob_Stanza2, [<<"Hi Bob1!">>], <<"received">>),

      escalus_assert:has_no_stanzas(Bob1),
      escalus_assert:has_no_stanzas(Bob2),

      send_iq_carbon(Bob2, <<"disable">>),
      send_iq_carbon(Bob2, <<"disable">>),

      escalus:send(Alice1, escalus_stanza:chat_to(escalus_client:full_jid(Bob1), <<"Hi Bob1!">>)),

      Bob_Stanza3 = escalus:wait_for_stanza(Bob1),

      escalus:assert(is_chat_message, [<<"Hi Bob1!">>], Bob_Stanza3),

      escalus_assert:has_no_stanzas(Bob1),
      escalus_assert:has_no_stanzas(Bob2)

  end).

private_message(Config) ->

  escalus:story(Config, [2, 2], fun(Alice1, Alice2, Bob1, Bob2) ->

    send_iq_carbon(Alice1, <<"enable">>),
    send_iq_carbon(Alice2, <<"enable">>),
    send_iq_carbon(Bob1, <<"enable">>),
    send_iq_carbon(Bob2, <<"enable">>),

    send_private_carbon_message(Alice1, escalus_client:full_jid(Bob1), <<"Hi Bob1!">>),

    Bob_Stanza1 = escalus:wait_for_stanza(Bob1),

    escalus:assert(is_chat_message, [<<"Hi Bob1!">>], Bob_Stanza1),

    escalus_assert:has_no_stanzas(Bob1),
    escalus_assert:has_no_stanzas(Bob2),
    escalus_assert:has_no_stanzas(Alice2),

    send_private_carbon_message(Alice1, escalus_client:short_jid(Bob1), <<"Hi Bob1!">>),

    Bob_Stanza2 = escalus:wait_for_stanza(Bob2),
    Bob_Stanza3 = escalus:wait_for_stanza(Bob1),

    escalus:assert(is_chat_message, [<<"Hi Bob1!">>], Bob_Stanza3),
    escalus:assert(is_chat_message, [<<"Hi Bob1!">>], Bob_Stanza2),

    escalus_assert:has_no_stanzas(Bob1),
    escalus_assert:has_no_stanzas(Bob2),
    escalus_assert:has_no_stanzas(Alice2)

  end).

%%%===================================================================
%%% Custom predicates
%%%===================================================================

is_carbon(Stanza, Content, Message_Type) ->
  escalus:assert(is_chat_message, Stanza),
  Stanza_received = exml_query:subelement(Stanza, Message_Type),
  Stanza_forwarded = exml_query:subelement(Stanza_received, <<"forwarded">>),
  Stanza_carbon_message = exml_query:subelement(Stanza_forwarded, <<"message">>),
  escalus:assert(has_ns, [<<"urn:xmpp:carbons:2">>], Stanza_received),
  escalus:assert(is_chat_message, Content, Stanza_carbon_message).

%%%===================================================================
%%% Helpers
%%%===================================================================

send_iq_carbon(User, Iq) ->
  Stanza = escalus_client:send_and_wait(User, #xmlel{name = <<"iq">>,
                            attrs = [{<<"xmlns">>,<<"jabber:client">>},{<<"id">>,<<"iq_carbon">>},
                            {<<"from">>,escalus_client:full_jid(User)},{<<"type">>,<<"set">>}],
                            children = #xmlel{name = Iq,attrs = [{<<"xmlns">>, <<"urn:xmpp:carbons:2">>}]}}),
  escalus:assert(is_iq_result, Stanza).

send_private_carbon_message(Sender, Receipient, Message) ->
  escalus_client:send(Sender, #xmlel{name = <<"message">>,
                            attrs = [{<<"xmlns">>,<<"jabber:client">>},{<<"to">>,escalus_utils:get_jid(Receipient)},
                            {<<"type">>,<<"chat">>}],
                            children = [#xmlel{name = <<"private">>, attrs = [{<<"xmlns">>, <<"urn:xmpp:carbons:2">>}]},
                            #xmlel{name = <<"body">>, children = [exml:escape_cdata(Message)]}]}).
