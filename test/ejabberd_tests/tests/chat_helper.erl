-module(chat_helper).
-export([chat_w_id/2
    ,chat_w_id/3
    ,carbon_received/5
    ,chat_state_delivered/3
    ,message_delivered/4
    ,message_sent/3
    ,message_sent/4
    ,random_alpha_binary/1
]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml.hrl").

chat_state_delivered(From, To, State) ->
    C = #xmlel{name = State
        ,attrs = [{<<"xmlns">>, <<"http://jabber.org/protocol/chatstates">>}]},
    Payload =
    #xmlel{name = <<"message">>
        ,attrs = [{<<"from">>,  escalus_client:full_jid(From)}
            ,{<<"to">>, escalus_client:full_jid(To)}
            ,{<<"type">>, <<"chat">>}]
        ,children = [C]},
    escalus_client:send(From, Payload).

message_delivered(From, To, MsgText, MsgId) ->
    message_sent(From, To, MsgText, MsgId),
    escalus:assert(is_chat_message, [MsgText], escalus_client:wait_for_stanza(To, 3000)).

message_sent(From, To, MsgText) ->
    message_sent(From, To, MsgText, escalus_stanza:id()).

message_sent(From, To, MsgText, MsgId) ->
    escalus_client:send(From, chat_w_id(To, MsgText, MsgId)).

carbon_received(outgoing, Resource, From, To, Text) ->
    escalus:assert(is_forwarded_sent_message,
                   [escalus_client:full_jid(From),
                    escalus_client:full_jid(To),
                    Text],
                   escalus_client:wait_for_stanza(Resource));
carbon_received(incoming, Resource, From, To, Text) ->
    escalus:assert(is_forwarded_received_message,
                   [escalus_client:full_jid(From),
                    escalus_client:full_jid(To),
                    Text],
                   escalus_client:wait_for_stanza(Resource)).

%% TODO move these two into escalus
chat_w_id(Target, Msg) ->
    Id = random_alpha_binary(10),
    escalus_stanza:set_id(escalus_stanza:chat_to(Target, Msg), Id).
chat_w_id(Target, Msg, Id) ->
    escalus_stanza:set_id(escalus_stanza:chat_to(Target, Msg), Id).

random_alpha_binary(Length) ->
    [random:uniform($z - $a + 1) + $a - 1 || _X <- lists:seq(1, Length)].
