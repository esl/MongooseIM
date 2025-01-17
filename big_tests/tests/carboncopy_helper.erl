-module(carboncopy_helper).
-compile([export_all, nowarn_export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").

wait_for_carbon_chat_with_body(Client, Body, #{from := From, to := To}) when is_binary(From) ->
    escalus:assert(
        is_forwarded_received_message,
        [From, escalus_client:full_jid(To), Body],
        escalus_client:wait_for_stanza(Client)
    );

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
        escalus_compat:bin(From) == maps:get(<<"from">>, Attrs, undefined) andalso
        escalus_compat:bin(To) == maps:get(<<"to">>, Attrs, undefined).

chat_message_with_body(#{to := User, body := Body}) ->
    escalus_stanza:chat_to(User, Body).

normal_message_with_body(#{to := User, body := Body}) ->
    escalus_stanza:message(Body, #{type => <<"normal">>, to => User}).

normal_message_with_receipt(#{to := User}) ->
    Msg = #xmlel{
        name = <<"message">>,
        attrs = #{
            <<"type">> => <<"normal">>,
            <<"from">> => escalus_utils:get_jid(User),
            <<"id">> => escalus_stanza:id()
        }
    },
    escalus_stanza:receipt_conf(Msg).

normal_message_with_csn(#{to := User}) ->
    #xmlel{
        name = <<"message">>,
        attrs = #{
            <<"type">> => <<"normal">>,
            <<"to">> => escalus_utils:get_jid(User)
        },
        children = [
            #xmlel{
                name = <<"stateName">>,
                attrs = #{<<"xmlns">> => ?NS_CHATSTATES}
            }
        ]
    }.

normal_message_with_chat_marker(#{to := User}) ->
    Msg = escalus_stanza:chat_marker(User, <<"received">>, escalus_stanza:id()),
    escalus_stanza:setattr(Msg, <<"type">>, <<"normal">>).
