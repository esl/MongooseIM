-module(jlib_SUITE).
-include_lib("exml/include/exml.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-compile([export_all]).

-import(prop_helper, [prop/2]).

prohibited_output_node() ->
    [$", $&, $', $/, $:, $<, $>, $@, " "].

prohibited_c_1_2() ->
    [<<"\x{00A0}"/utf8>>,% NO-BREAK SPACE
     <<"\x{1680}"/utf8>>,% OGHAM SPACE MARK
     <<"\x{2000}"/utf8>>,% EN QUAD
     <<"\x{2001}"/utf8>>,% EM QUAD
     <<"\x{2002}"/utf8>>,% EN SPACE
     <<"\x{2003}"/utf8>>,% EM SPACE
     <<"\x{2004}"/utf8>>,% THREE-PER-EM SPACE
     <<"\x{2005}"/utf8>>,% FOUR-PER-EM SPACE
     <<"\x{2006}"/utf8>>,% SIX-PER-EM SPACE
     <<"\x{2007}"/utf8>>,% FIGURE SPACE
     <<"\x{2008}"/utf8>>,% PUNCTUATION SPACE
     <<"\x{2009}"/utf8>>,% THIN SPACE
     <<"\x{200A}"/utf8>>,% HAIR SPACE
     <<"\x{200B}"/utf8>>,% ZERO WIDTH SPACE
     <<"\x{202F}"/utf8>>,% NARROW NO-BREAK SPACE
     <<"\x{205F}"/utf8>>,% MEDIUM MATHEMATICAL SPACE
     <<"\x{3000}"/utf8>>].% IDEOGRAPHIC SPACE

all() -> [make_iq_reply_switch_to_from,
          correct_jid,
          incorrect_username,
          incorrect_domain].

init_per_suite(C) ->
    application:start(p1_stringprep),
    C.

end_per_suite(C) ->
    application:stop(p1_stringprep),
    C.


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

correct_jid(_C) ->
    prop(correct_jid_property,
         ?FORALL(BinJid, valid_jid(),
                   is_valid_jid_record(jlib:binary_to_jid(BinJid)))).

incorrect_username(_) ->
    prop(incorrect_username_property,
         ?FORALL(Bin, invalid_username(),
                error == jlib:nodeprep(Bin))).

incorrect_resource(_) ->
    prop(incorrect_resource_property,
         ?FORALL(Bin, invalid_resource(),
                error == jlib:resourceprep(Bin))).

incorrect_domain(_) ->
    prop(incorrect_resource_property,
         ?FORALL(Bin, invalid_domain(),
                error == jlib:nameprep(Bin))).


is_valid_jid_record(#jid{}) ->
    true;
is_valid_jid_record(_) ->
    false.

valid_jid() ->
    oneof([valid_full_jid(), valid_bare_jid()]).

valid_bare_jid() ->
    ?LET({Username, Domain}, {valid_username(), valid_domain()},
         <<Username/binary, $@, Domain/binary>>).

valid_full_jid() ->
    ?LET({BareJid, Resource}, {valid_bare_jid(), valid_resource()},
         <<BareJid/binary, $/, Resource/binary>>).

valid_username() ->
    always_correct_xmpp_binary().

invalid_username() ->
    invalid_xmpp_binary(prohibited_output_node() ++ prohibited_c_1_2()).

invalid_resource() ->
    invalid_xmpp_binary(prohibited_c_1_2()).

invalid_domain() ->
    invalid_xmpp_binary(prohibited_c_1_2()).

valid_domain() ->
    always_correct_xmpp_binary().

valid_resource() ->
    always_correct_xmpp_binary().

always_correct_xmpp_binary() ->
    ?LET(Str, always_correct_xmpp_string(), list_to_binary(Str)).

allowed_output() ->
    oneof([choose($a, $z),
           choose($A, $Z),
           choose($0, $9)]).

always_correct_xmpp_string() ->
    non_empty(
      list(allowed_output())).

invalid_xmpp_binary(ProhibitedOutput) ->
    non_empty(
      list(
        oneof([allowed_output(),
               oneof(ProhibitedOutput)]))).


