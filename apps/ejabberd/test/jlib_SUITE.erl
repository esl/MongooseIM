-module(jlib_SUITE).
-include_lib("exml/include/exml.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-compile([export_all]).

-import(prop_helper, [prop/2]).

all() -> [
          make_iq_reply_changes_type_to_result,
          make_iq_reply_changes_to_to_from,
          make_iq_reply_switches_from_to_to,
          make_iq_reply_switches_to_and_from_attrs
         ].

init_per_suite(C) ->
    ok = stringprep:start(),
    C.

end_per_suite(C) ->
    C.


make_iq_reply_switches_to_and_from_attrs(_C) ->
    ToJid = <<"test@esl.com/res">>,
    FromJid = <<"test2@esl.com/res2">>,
    #xmlel{attrs = Attrs} = BaseIQ = base_iq(),

    IQWithToAndFrom = BaseIQ#xmlel{attrs = [{<<"to">>, ToJid},
                                            {<<"from">>, FromJid} | Attrs]},

    WithToFromReply = jlib:make_result_iq_reply(IQWithToAndFrom),

    <<"result">> = exml_query:attr(WithToFromReply, <<"type">>),
    FromJid = exml_query:attr(WithToFromReply, <<"to">>),
    ToJid = exml_query:attr(WithToFromReply, <<"from">>).

make_iq_reply_switches_from_to_to(_C) ->
    FromJid = <<"test2@esl.com/res2">>,
    #xmlel{attrs = Attrs} = BaseIQ = base_iq(),
    IQWithFrom = BaseIQ#xmlel{attrs = [{<<"from">>, FromJid} | Attrs]},

    WithFromReply = jlib:make_result_iq_reply(IQWithFrom),

    <<"result">> = exml_query:attr(WithFromReply, <<"type">>),
    FromJid = exml_query:attr(WithFromReply, <<"to">>).

make_iq_reply_changes_to_to_from(_C) ->
    ToJid = <<"test@esl.com/res">>,
    #xmlel{attrs = Attrs} = BaseIQ = base_iq(),
    IQWithTo = BaseIQ#xmlel{attrs = [{<<"to">>, ToJid} | Attrs]},

    WithToReply = jlib:make_result_iq_reply(IQWithTo),

    <<"result">> = exml_query:attr(WithToReply, <<"type">>),
    ToJid = exml_query:attr(WithToReply, <<"from">>).

make_iq_reply_changes_type_to_result(_) ->
    BaseIQReply = jlib:make_result_iq_reply(base_iq()),
    <<"result">> = exml_query:attr(BaseIQReply, <<"type">>).

base_iq() ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"id">>, base64:encode(crypto:rand_bytes(4))},
                    {<<"xmlns">>, <<"jabber:client">>},
                    {<<"type">>, <<"set">>}],
           children = [#xmlel{name = <<"session">>,
                              attrs = [{<<"xmlns">>, <<"urn:ietf:params:xml:ns:xmpp-session">>}]}
                      ]}.

element_length_is_too_big(Els) ->
    lists:any(fun(El) -> size(El) >= 1024 end, Els).

run_property(Prop, NumTest, StartSize, StopSize) ->
    ?assert(proper:quickcheck(Prop, [verbose, long_result,
                                     {numtests, NumTest},
                                     {start_size, StartSize},
                                     {max_size, StopSize}])).

