-module(carboncopy_helper).
-export([
    wait_for_carbon_chat_with_body/3,
    wait_for_carbon_message/2,
    normal_message_with_body/2,
    normal_message_with_receipt/2,
    normal_message_with_csn/2,
    normal_message_with_chat_marker/2
]).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

wait_for_carbon_chat_with_body(Client, Body, #{from := From, to := To}) ->
    escalus:assert(
        is_forwarded_received_message,
        [escalus_client:full_jid(From), escalus_client:full_jid(To), Body],
        escalus_client:wait_for_stanza(Client)
    ).

wait_for_carbon_message(Client, #{from := From, to := To}) ->
    escalus:assert(
        fun is_forwarded_received_message/3,
        [escalus_client:full_jid(From), escalus_client:full_jid(To)],
        escalus_client:wait_for_stanza(Client)
    ).

is_forwarded_received_message(From, To, Stanza) ->
    Carbon = exml_query:subelement(Stanza, <<"received">>),
    escalus_pred:has_ns(?NS_CARBONS_2, Carbon) andalso
        is_forwarded_message(From, To, exml_query:subelement(Carbon, <<"forwarded">>)).

is_forwarded_message(From, To, Stanza) ->
    escalus_pred:has_ns(?NS_FORWARD_0, Stanza) andalso
        is_message_from_to(From, To, exml_query:subelement(Stanza, <<"message">>)).

is_message_from_to(From, To, #xmlel{attrs = Attrs} = Stanza) ->
    escalus_pred:is_message(Stanza) andalso
        escalus_compat:bin(From) == proplists:get_value(<<"from">>, Attrs) andalso
        escalus_compat:bin(To) == proplists:get_value(<<"to">>, Attrs).

normal_message_with_body(User, Body) ->
    escalus_stanza:message(Body, #{type => <<"normal">>, to => User}).

normal_message_with_receipt(User, _Body) ->
    Msg = #xmlel{
        name = <<"message">>,
        attrs = [
            {<<"type">>, <<"normal">>},
            {<<"from">>, escalus_utils:get_jid(User)},
            {<<"id">>, escalus_stanza:id()}
        ]
    },
    escalus_stanza:receipt_conf(Msg).

normal_message_with_csn(User, _Body) ->
    #xmlel{
        name = <<"message">>,
        attrs = [
            {<<"type">>, <<"normal">>},
            {<<"to">>, escalus_utils:get_jid(User)}
        ],
        children = [
            #xmlel{
                name = <<"stateName">>,
                attrs = [{<<"xmlns">>, ?NS_CHATSTATES}]
            }
        ]
    }.

normal_message_with_chat_marker(User, _Body) ->
    Msg = escalus_stanza:chat_marker(User, <<"received">>, escalus_stanza:id()),
    escalus_stanza:setattr(Msg, <<"type">>, <<"normal">>).
