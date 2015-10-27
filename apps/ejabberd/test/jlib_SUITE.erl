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

all() -> [make_iq_reply_changes_type_to_result,
          make_iq_reply_changes_to_to_from,
          make_iq_reply_switches_from_to_to,
          make_iq_reply_switches_to_and_from_attrs,
          binary_to_jid_succeeds_with_valid_binaries,
          binary_to_jid_fails_with_invalid_binaries,
          binary_to_jid_fails_with_empty_binary,
          make_jid_fails_on_binaries_that_are_too_long,
          jid_to_lower_fails_if_any_binary_is_invalid,
          jid_replace_resource_failes_for_invalid_resource,
          nodeprep_fails_with_too_long_username,
          nameprep_fails_with_too_long_domain,
          resourceprep_fails_with_too_long_resource,
          nodeprep_fails_with_incorrect_username,
          resourceprep_fails_with_incorrect_resource,
          nameprep_fails_with_incorrect_domain].

init_per_suite(C) ->
    application:start(p1_stringprep),
    C.

end_per_suite(C) ->
    application:stop(p1_stringprep),
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

binary_to_jid_succeeds_with_valid_binaries(_C) ->
    Prop = ?FORALL(BinJid, valid_jid(),
                   is_record(jlib:binary_to_jid(BinJid), jid)),
    prop(binary_to_jid_succeeds_with_valid_binaries, Prop).


binary_to_jid_fails_with_invalid_binaries(_C) ->
    Prop = ?FORALL(BinJid, invalid_jid(),
                   error == jlib:binary_to_jid(BinJid)),
    run_property(Prop, 100, 1, 42).

binary_to_jid_fails_with_empty_binary(_) ->
    error = jlib:binary_to_jid(<<>>).

make_jid_fails_on_binaries_that_are_too_long(_) ->
    Prop = ?FORALL({U, S, R}, {valid_username(), valid_domain(), valid_resource()},
                   case element_length_is_too_big([U,S,R]) of
                        true -> error == jlib:make_jid(U,S,R);
                        false -> is_record(jlib:make_jid(U,S,R), jid)
                   end),
    run_property(Prop, 100, 500, 1500).

element_length_is_too_big(Els) ->
    lists:any(fun(El) -> size(El) >= 1024 end, Els).

jid_to_lower_fails_if_any_binary_is_invalid(_) ->
    Prop = ?FORALL({U, S, R}, {maybe_valid_username(), maybe_valid_domain(), maybe_valid_resource()},
                   case jlib:jid_to_lower({U, S, R}) of
                       {LU, LS, LR} ->
                           jlib:nodeprep(U) == LU andalso
                           jlib:nameprep(S) == LS andalso
                           jlib:resourceprep(R) == LR;
                       error ->
                           jlib:nodeprep(U) == error orelse
                           jlib:nameprep(S) == error orelse
                           jlib:resourceprep(R) == error
                   end),

    run_property(Prop, 150, 1, 42).

nodeprep_fails_with_too_long_username(_C) ->
    Prop = ?FORALL(Bin, valid_username(),
                   error == jlib:nodeprep(Bin)),
    run_property(Prop, 5, 1024, 2048).

nameprep_fails_with_too_long_domain(_C) ->
    Prop = ?FORALL(Bin, valid_domain(),
                   error == jlib:nameprep(Bin)),
    run_property(Prop, 5, 1024, 2048).

resourceprep_fails_with_too_long_resource(_C) ->
    Prop = ?FORALL(Bin, valid_resource(),
                   error == jlib:resourceprep(Bin)),
    run_property(Prop, 5, 1024, 2048).

jid_replace_resource_failes_for_invalid_resource(_) ->
    Prop = ?FORALL({BinJid, MaybeCorrectRes},
                   {valid_bare_jid(), maybe_valid_resource()},
                   jid_replace_resource(BinJid, MaybeCorrectRes)),
    prop(jid_replace_resource, Prop).

jid_replace_resource(BinJid, Res) ->
    Jid = jlib:binary_to_jid(BinJid),
    Jid2 = jlib:jid_replace_resource(Jid, Res),
    check_jid_replace_resource_output(Res, Jid2).

check_jid_replace_resource_output(Resource, error) ->
    jlib:resourceprep(Resource) == error;
check_jid_replace_resource_output(Resource, #jid{}) ->
    jlib:resourceprep(Resource) =/= error.


run_property(Prop, NumTest, StartSize, StopSize) ->
    ?assert(proper:quickcheck(Prop, [verbose, long_result,
                                     {numtests, NumTest},
                                     {start_size, StartSize},
                                     {max_size, StopSize}])).

nodeprep_fails_with_incorrect_username(_) ->
    prop(incorrect_username_property,
         ?FORALL(Bin, invalid_username(),
                 error == jlib:nodeprep(Bin))).

resourceprep_fails_with_incorrect_resource(_) ->
    prop(incorrect_resource_property,
         ?FORALL(Bin, invalid_resource(),
                 error == jlib:resourceprep(Bin))).

nameprep_fails_with_incorrect_domain(_) ->
    prop(incorrect_domain_property,
         ?FORALL(Bin, invalid_domain(),
                 error == jlib:nameprep(Bin))).

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
                  maybe_valid_domain()},
         <<U/binary, $@, S/binary>>).

invalid_full_jid() ->
    ?LET({BareJid, R}, {invalid_bare_jid(), valid_resource()},
         <<BareJid/binary, $/, R/binary>>).

maybe_valid_username() ->
    oneof([valid_username, <<>>, invalid_username()]).

invalid_username() ->
    invalid_xmpp_binary(prohibited_output_node()).

maybe_valid_resource() ->
    oneof([valid_resource(), <<>>, invalid_resource()]).

invalid_resource() ->
    invalid_xmpp_binary([<<238,190,187>>]). %<<"\x{EFBB}"/utf8>>

maybe_valid_domain() ->
    oneof([valid_domain(), <<>>, invalid_domain()]).

invalid_domain() ->
    invalid_resource().


always_correct_xmpp_binary(S) ->
    ?LET(Str, always_correct_xmpp_string(S), list_to_binary(Str)).

allowed_output() ->
    oneof([choose($a, $z),
           choose($A, $Z),
           oneof([$., $-, $_]),
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


