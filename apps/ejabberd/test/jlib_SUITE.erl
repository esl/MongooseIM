-module(jlib_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").
-compile([export_all]).


all() -> [ make_iq_reply_switch_to_from ].

init_per_suite(C) -> C.

make_iq_reply_switch_to_from(_C) ->
    ToJid = <<"test@esl.com/res">>,
    FromJid = <<"test2@esl.com/res2">>,

    #xmlel{attrs = Attrs} = BaseIQ = make_iq(),

    BaseIQReply = jlib:make_result_iq_reply(BaseIQ),
    <<"result">> = exml_query:attr(BaseIQReply, <<"type">>),

    IQWithTo = BaseIQ#xmlel{attrs = [{<<"to">>, ToJid} | Attrs]},
    WithToReply = jlib:make_result_iq_reply(IQWithTo),
    <<"result">> = exml_query:attr(WithToReply, <<"type">>),
    ToJid = exml_query:attr(WithToReply, <<"from">>),

    IQWithFrom = BaseIQ#xmlel{attrs = [{<<"from">>, FromJid} | Attrs]},
    WithFromReply = jlib:make_result_iq_reply(IQWithFrom),
    <<"result">> = exml_query:attr(WithFromReply, <<"type">>),
    FromJid = exml_query:attr(WithFromReply, <<"to">>),


    IQWithToAndFrom = BaseIQ#xmlel{attrs = [{<<"to">>, ToJid},
                                            {<<"from">>, FromJid} | Attrs]},

    WithToFromReply = jlib:make_result_iq_reply(IQWithToAndFrom),
    <<"result">> = exml_query:attr(WithToFromReply, <<"type">>),
    FromJid = exml_query:attr(WithToFromReply, <<"to">>),
    ToJid = exml_query:attr(WithToFromReply, <<"from">>),

    ok.




make_iq() ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"id">>, base64:encode(crypto:rand_bytes(4))},
                    {<<"xmlns">>, <<"jabber:client">>},
                    {<<"type">>, <<"set">>}],
           children = [#xmlel{name = <<"session">>,
                              attrs = [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-session">>}]}
                      ]}.
