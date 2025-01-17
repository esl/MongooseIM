-module(csi_helper).
-compile([export_all, nowarn_export_all]).

-include_lib("exml/include/exml.hrl").

-define(NS_CSI, <<"urn:xmpp:csi:0">>).

given_client_is_active(Alice) ->
    escalus:send(Alice, csi_helper:csi_stanza(<<"active">>)).

given_client_is_inactive_and_no_messages_arrive(Alice) ->
    escalus:send(Alice, csi_stanza(<<"inactive">>)),
    then_client_does_not_receive_any_message(Alice).

given_messages_are_sent(Alice, Bob, N) ->
    Msgs = gen_msgs(<<"Hi, Alice">>, N),
    send_msgs(Bob, Alice, Msgs),
    Msgs.

then_client_does_not_receive_any_message(Alice) ->
    [] = escalus:wait_for_stanzas(Alice, 1, 100),
    escalus_assert:has_no_stanzas(Alice).

then_client_receives_message(Alice, Msgs) ->
    [escalus:assert(is_chat_message, [Msg], escalus:wait_for_stanza(Alice)) ||
     Msg <- Msgs].

gen_msgs(Prefix, N) ->
    [<<Prefix/binary, ": ", (integer_to_binary(I))/binary>> || I <- lists:seq(1, N)].

send_msgs(From, To, Msgs) ->
    [escalus:send(From, escalus_stanza:chat_to(To, Msg)) || Msg <- Msgs].

csi_stanza(Name) ->
    #xmlel{name = Name,
           attrs = #{<<"xmlns">> => ?NS_CSI}}.
