-module(special_chars_helper).

-include_lib("exml/include/exml_stream.hrl").

%% API
-export([check_attr_from_to/2,
         check_cdata_from_to/3]).

check_attr_from_to(From, To) ->
    AttrWithSpecialChars = <<"font-family: 'Helvetica';font-size: 12px;">>,
    SpecialCharsInAttr = create_special_chars_attr_xmlel(AttrWithSpecialChars),
    Message = add_special_chars_attr_xmlel(escalus_stanza:chat_to(To, <<"Hi there!">>),
        SpecialCharsInAttr),
    escalus_client:send(From, Message),
    Stanza = escalus_client:wait_for_stanza(To),

    escalus:assert(is_chat_message, [<<"Hi there!">>], Stanza),
    AttrWithSpecialChars = exml_query:path(Stanza,
        [{element, <<"span">>}, {attr, <<"style">>}]).

check_cdata_from_to(From, To, Message) ->
    escalus_client:send(From, escalus_stanza:chat_to(To, Message)),
    escalus:assert(is_chat_message, [Message], escalus_client:wait_for_stanza(To)).

add_special_chars_attr_xmlel(BaseMessage, SpecialCharsChild) ->
    BaseMessage#xmlel{children =
    [SpecialCharsChild | BaseMessage#xmlel.children]}.

create_special_chars_attr_xmlel(AttrWithSpecialChars) ->
    #xmlel{name = <<"span">>,
        attrs = [{<<"style">>, AttrWithSpecialChars}]}.
