-module(jlib_SUITE).
-include_lib("exml/include/exml.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("jlib.hrl").
-include("mongoose.hrl").
-include_lib("common_test/include/ct.hrl").
-compile([export_all]).

all() -> [
          make_iq_reply_changes_type_to_result,
          make_iq_reply_changes_to_to_from,
          make_iq_reply_switches_from_to_to,
          make_iq_reply_switches_to_and_from_attrs,
          error_reply_check
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

error_reply_check(_) ->
    BaseIQReply = jlib:make_result_iq_reply(base_iq()),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => <<"localhost">>,
                              element => BaseIQReply,
                              from_jid => jid:make_noprep(<<"a">>, <<"localhost">>, <<>>),
                              to_jid => jid:make_noprep(<<>>, <<"localhost">>, <<>>) }),
    {Acc1, ErrorReply1} = jlib:make_error_reply(Acc, BaseIQReply, #xmlel{name = <<"testerror">>}),
    ?assertMatch(#xmlel{}, ErrorReply1),
    {_Acc2, ErrorReply2} = jlib:make_error_reply(Acc1, ErrorReply1, #xmlel{name = <<"testerror">>}),
    ?assertMatch({error, {already_an_error, #xmlel{}, #xmlel{}}}, ErrorReply2),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


base_iq() ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"id">>, base64:encode(crypto:strong_rand_bytes(4))},
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

