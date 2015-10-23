-module(jlib_SUITE).
-include_lib("exml/include/exml.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("common_test/include/ct.hrl").
-compile([export_all]).

-import(prop_helper, [prop/2]).

prohibited_output_node() ->
    [$", $&, $', $/, $:, $<, $>, $@, " "].

all() -> [make_iq_reply_switch_to_from,
          binary_to_jid,
          binary_to_jid_incorrect,
          empty_binary_to_jid,
          make_jid,
          correct_but_too_long_username,
          correct_but_too_long_domain,
          correct_but_too_long_resource,
          incorrect_username].

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

binary_to_jid(_C) ->
    Prop = ?FORALL(BinJid, valid_jid(),
                   is_valid_jid_record(jlib:binary_to_jid(BinJid))),
    prop(correct_binary_to_jid, Prop).


binary_to_jid_incorrect(_C) ->
    Prop = ?FORALL(BinJid, invalid_jid(),
                   error == jlib:binary_to_jid(BinJid)),
    big_size_property(Prop, 100, 1, 42).

empty_binary_to_jid(_) ->
    error = jlib:binary_to_jid(<<>>).

make_jid(_) ->
    Prop = ?FORALL({U, S, R}, {valid_username(), valid_domain(), valid_resource()},
                   check_output(U, S, R, jlib:make_jid(U, S, R))),
    big_size_property(Prop, 100, 500, 1500).


correct_but_too_long_username(_C) ->
    Prop = ?FORALL(Bin, valid_username(),
                   error == jlib:nodeprep(Bin)),
    big_size_property(Prop, 5, 1024, 2048).

correct_but_too_long_domain(_C) ->
    Prop = ?FORALL(Bin, valid_domain(),
                   error == jlib:nameprep(Bin)),
    big_size_property(Prop, 5, 1024, 2048).

correct_but_too_long_resource(_C) ->
    Prop = ?FORALL(Bin, valid_resource(),
                   error == jlib:resourceprep(Bin)),
    big_size_property(Prop, 5, 1024, 2048).


big_size_property(Prop, NumTest, StartSize, StopSize) ->
    ?assert(proper:quickcheck(Prop, [verbose, long_result,
                                     {numtests, NumTest},
                                     {start_size, StartSize},
                                     {max_size, StopSize}])).

incorrect_username(_) ->
    prop(incorrect_username_property,
         ?FORALL(Bin, invalid_username(),
                error == jlib:nodeprep(Bin))).


is_valid_jid_record(#jid{}) ->
    true;
is_valid_jid_record(_) ->
    false.

check_output(U, S, R, #jid{})
  when size(U) < 1024, size(S) < 1024, size(R) < 1024 ->
    true;
check_output(_, _, _, error) ->
    true;
check_output(_, _, _, _) ->
    false.

valid_jid() ->
    oneof([valid_full_jid(), valid_bare_jid(), valid_domain()]).

valid_bare_jid() ->
    ?LET({Username, Domain}, {valid_username(), valid_domain()},
         <<Username/binary, $@, Domain/binary>>).

valid_full_jid() ->
    ?LET({BareJid, Resource}, {valid_bare_jid(), valid_resource()},
         <<BareJid/binary, $/, Resource/binary>>).

valid_username() ->
    ?SIZED(S, always_correct_xmpp_binary(S)).

valid_domain() ->
    ?SIZED(S, always_correct_xmpp_binary(round(S*1.5))).

valid_resource() ->
    ?SIZED(S, always_correct_xmpp_binary(round(S*1.7))).

invalid_jid() ->
    oneof([invalid_full_jid(), invalid_bare_jid()]).

invalid_bare_jid() ->
    %%Oh yes, jids like domain/resource are allowed in both ejabberd and MongooseIM
    ?LET({U, S}, {?SUCHTHAT(E, invalid_username(), size(E) == 1 orelse binary:matches(E, <<"/">>) == []),
                  valid_domain()},
         <<U/binary, $@, S/binary>>).

invalid_full_jid() ->
    ?LET({BareJid, R}, {invalid_bare_jid(), valid_resource()},
         <<BareJid/binary, $/, R/binary>>).

invalid_username() ->
    invalid_xmpp_binary(prohibited_output_node()).


always_correct_xmpp_binary(S) ->
    ?LET(Str, always_correct_xmpp_string(S), list_to_binary(Str)).

allowed_output() ->
    oneof([choose($a, $z),
           choose($A, $Z),
           choose($0, $9)]).

always_correct_xmpp_string(S) ->
    [allowed_output() || _ <- lists:seq(1, S)].

invalid_xmpp_binary(ProhibitedOutput) ->
    ?LET({NotAllowed, Str},
         {oneof(ProhibitedOutput),
          frequency([{1, []}, {5, maybe_invalid_xmpp_string(ProhibitedOutput)}])},
         erlang:iolist_to_binary([NotAllowed | Str])).

maybe_invalid_xmpp_string(ProhibitedOutput) ->
      list(
        oneof([allowed_output(),
               oneof(ProhibitedOutput)])).


