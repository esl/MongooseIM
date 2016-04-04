-module(xep_0352_csi_SUITE).

-include_lib("exml/include/exml.hrl").

-compile([export_all]).

-define(CSI_BUFFER_MAX, 10).

all() ->
    [{group, basic}].


groups() ->
    [{basic,
      [parallel, shuffle],
      all_tests()
     }].

all_tests() ->
    [
     server_announces_csi,
     alice_is_inactive_and_no_stanza_arrived,
     alice_gets_msgs_after_activate,
     alice_gets_msgs_after_activate_in_order,
     alice_gets_message_after_buffer_overflow,
     bob_gets_msgs_from_inactive_alice,
     alice_is_inactive_but_sends_sm_req_and_recives_ack
    ].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    Domain = escalus_ct:get_config(ejabberd_domain),
    dynamic_modules:start(Domain, mod_csi, [{buffer_max, ?CSI_BUFFER_MAX}]),
    [{escalus_user_db, {module, escalus_ejabberd}} | escalus:init_per_suite(Config)].

end_per_suite(Config) ->
    Domain = escalus_ct:get_config(ejabberd_domain),
    dynamic_modules:stop(Domain, mod_csi),
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    escalus_users:update_userspec(Config, alice, stream_management, true).

end_per_group(_Group, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

server_announces_csi(Config) ->
    NewConfig = escalus_fresh:create_users(Config, [{alice, 1}]),
    Spec = escalus_users:get_userspec(NewConfig, alice),
    Steps = [start_stream,
             stream_features,
             maybe_use_ssl,
             maybe_use_compression,
             authenticate,
             bind,
             session],
    {ok, _Client, _Props, Features} = escalus_connection:start(Spec, Steps),
    true = proplists:get_value(client_state_indication, Features).

alice_is_inactive_and_no_stanza_arrived(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        given_client_is_inactive_and_messages_sent(Alice, Bob, 1),

        escalus_assert:has_no_stanzas(Alice)
    end).

alice_gets_msgs_after_activate(Config) ->
    alice_gets_msgs_after_activate(Config, 1).

alice_gets_msgs_after_activate_in_order(Config) ->
    alice_gets_msgs_after_activate(Config, 3).

alice_gets_msgs_after_activate(Config, N) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %%Given
        Msgs = given_client_is_inactive_and_messages_sent(Alice, Bob, N),

        %%When client becomes active again
        escalus:send(Alice, csi_stanza(<<"active">>)),

        then_client_receives_message(Alice, Msgs)
    end).

alice_gets_message_after_buffer_overflow(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        Msgs = given_client_is_inactive_and_messages_sent(Alice, Bob, ?CSI_BUFFER_MAX+5),

        {Flushed, Awaiting} = lists:split(?CSI_BUFFER_MAX+1, Msgs),

        then_client_receives_message(Alice, Flushed),
        %% and no other stanza
        escalus_assert:has_no_stanzas(Alice),
        %% Alice activates
        escalus:send(Alice, csi_stanza(<<"active">>)),
        %% ands gets remaining stanzas
        then_client_receives_message(Alice, Awaiting)
    end).



bob_gets_msgs_from_inactive_alice(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        given_client_is_inactive_but_sends_messages(Alice, Bob, 1),

        escalus:assert(is_chat_message, escalus:wait_for_stanza(Bob))
    end).

alice_is_inactive_but_sends_sm_req_and_recives_ack(Config) ->
    escalus:fresh_story(Config, [{alice,1}], fun(Alice) ->
        given_client_is_inactive(Alice),

        escalus:send(Alice, escalus_stanza:sm_request()),

        escalus:assert(is_sm_ack, escalus:wait_for_stanza(Alice))

    end).

given_client_is_inactive_but_sends_messages(Alice, Bob, N) ->
    %%Given
    MsgsToAlice = given_client_is_inactive_and_messages_sent(Alice, Bob, N),

    MsgsToBob = gen_msgs(<<"Hi, Bob">>, N),
    send_msgs(Alice, Bob, MsgsToBob),
    timer:sleep(1),
    {MsgsToAlice, MsgsToBob}.


then_client_receives_message(Alice, Msgs) ->
    [escalus:assert(is_chat_message, [Msg], escalus:wait_for_stanza(Alice)) ||
     Msg <- Msgs].


given_client_is_inactive_and_messages_sent(Alice, Bob, N) ->
    %%Given
    given_client_is_inactive(Alice),

    timer:sleep(1000),

    %%When
    Msgs = gen_msgs(<<"Hi, Alice">>, N),
    send_msgs(Bob, Alice, Msgs),
    timer:sleep(timer:seconds(1)),
    Msgs.

send_msgs(From, To, Msgs) ->
    [escalus:send(From, escalus_stanza:chat_to(To, Msg)) ||
     Msg <- Msgs].


gen_msgs(Prefix, N) ->
    [<<Prefix/binary, (integer_to_binary(I))/binary>> || I <- lists:seq(1, N)].

given_client_is_inactive(Alice) ->
    escalus:send(Alice, csi_stanza(<<"inactive">>)).


csi_stanza(Name) ->
    #xmlel{name = Name,
           attrs = [{<<"xmlns">>, <<"urn:xmpp:csi:0">>}]}.

