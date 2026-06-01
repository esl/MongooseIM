-module(mam_misc_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("exml/include/exml.hrl").
-include("mongoose_ns.hrl").

-import(mod_mam_utils,
        [
         is_archivable_message/4,
         is_valid_message/4
        ]).

all() ->
    [
     test_encode_decode_functionality,
     {group, should_archive},
     {group, retract_unit}
    ].

groups() ->
    [
     {should_archive, [parallel],
      [
       non_messages_are_always_false,
       messages_type_error_false,
       other_message_types_return_false,
       must_be_rejected,
       must_be_accepted,
       must_be_accepted_v1_retract
      ]},
     {retract_unit, [parallel],
      [
       get_retract_id_v0,
       get_retract_id_esl,
       get_retract_id_v1,
       get_retract_id_v1_missing_message_id,
       get_retract_id_none,
       retracted_element_v1_shape
      ]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    Config.

end_per_suite(Config) ->
    Config.

test_encode_decode_functionality(_Config) ->
    PossibleDomainNames = [<<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"e">>, <<"f">>],
    PossibleUserNames = [<<"">> | PossibleDomainNames],
    PossibleResourceNames = [<<"just:some@random/text here">> | PossibleUserNames],
    PossibleJIDs = [{U, D, R, jid:make(U, D, R)} || U <- PossibleUserNames,
                                                    D <- PossibleDomainNames,
                                                    R <- PossibleResourceNames],
    FailedJIDs = [[{U1, D1, R1}, {U2, D2, R2}, EncodedJID, DecodedJID]
                  || {U1, D1, R1, JID1} <- PossibleJIDs,
                     {U2, D2, R2, JID2} <- PossibleJIDs,
                     EncodedJID <- [catch mam_jid_mini:encode(JID1, JID2)],
                     DecodedJID <- [catch mam_jid_mini:decode(JID1, EncodedJID)],
                     DecodedJID =/= JID2],
    case lists:sublist(FailedJIDs, 100) of
        [] -> ok;
        First100FailedJIDs ->
            [ct:log("~nJID encoding/decoding failed:~n"
                    "\tbase JID      - ~p~n"
                    "\tJID to encode - ~p~n"
                    "\tencoded JID   - ~p~n"
                    "\tdecoded JID   - ~p~n", Params) || Params <- First100FailedJIDs],
            ct:fail("Failed to encode/decode some of the JIDs,"
                    " see test suite logs for more details", [])
    end.

non_messages_are_always_false(_) ->
    NonMsgPacket = ?LET({T, Attrs, Children},
                        {oneof([<<"iq">>, <<"presence">>]), gen_attrs(all), gen_children()},
                        packet(T, Attrs, Children)),
    Prop = ?FORALL({Mod, Dir, Packet, Acm},
                   {gen_modules(), gen_directions(), NonMsgPacket, gen_arc_markers()},
                   not is_archivable_message(Mod, Dir, Packet, Acm)),
    run_prop(?FUNCTION_NAME, Prop).

messages_type_error_false(_) ->
    ErrorMsgDef = ?LET({Attrs, Children},
                       {gen_attrs(<<"error">>), gen_children()},
                       packet(<<"message">>, Attrs, Children)),
    Prop = ?FORALL({Mod, Dir, ErrorMessage, Acm},
                   {gen_modules(), gen_directions(), ErrorMsgDef, gen_arc_markers()},
                   not is_archivable_message(Mod, Dir, ErrorMessage, Acm)),
    run_prop(?FUNCTION_NAME, Prop).

other_message_types_return_false(_) ->
    StrangeTypeDef = ?LET({Attrs, Children},
                          {gen_attrs(strange), gen_children()},
                          packet(<<"message">>, Attrs, Children)),
    Prop = ?FORALL({Mod, Dir, StrangeType, Acm},
                   {gen_modules(), gen_directions(), StrangeTypeDef, gen_arc_markers()},
                   not is_archivable_message(Mod, Dir, StrangeType, Acm)),
    run_prop(?FUNCTION_NAME, Prop).

must_be_rejected(_) ->
    MustContain = oneof([no_store, delay, result]),
    MsgDef = ?LET({Attrs, Children},
                  {gen_attrs(good), gen_children(MustContain, [])},
                  packet(<<"message">>, Attrs, Children)),
    Prop = ?FORALL({Mod, Dir, Msg, Acm},
                   {gen_modules(), gen_directions(), MsgDef, gen_arc_markers()},
                   not is_valid_message(Mod, Dir, Msg, Acm)),
    run_prop(?FUNCTION_NAME, Prop).

must_be_accepted(_) ->
    MustContain = oneof([body, store, retraction]),
    MustNotContain = [no_store, delay, result],
    MsgDef = ?LET({Attrs, Children},
                  {gen_attrs(good), gen_children(MustContain, MustNotContain)},
                  packet(<<"message">>, Attrs, Children)),
    Prop = ?FORALL({Mod, Dir, Msg, Acm},
                   {gen_modules(), gen_directions(), MsgDef, gen_arc_markers()},
                   is_valid_message(Mod, Dir, Msg, Acm)),
    run_prop(?FUNCTION_NAME, Prop).

must_be_accepted_v1_retract(_) ->
    Packet = packet(<<"message">>,
                    #{<<"from">> => alice(), <<"to">> => bob(),
                      <<"type">> => <<"chat">>, <<"id">> => <<"R1">>},
                    [retraction_v1(<<"orig-1">>)]),
    ?assert(is_archivable_message(mod_mam_pm, outgoing, Packet, false)),
    ?assert(is_archivable_message(mod_mam_pm, incoming, Packet, false)).

%% Direct unit tests for mod_mam_utils:get_retract_id/1 and retracted_element/2.

get_retract_id_v0(_) ->
    Packet = packet(<<"message">>,
                    #{<<"from">> => alice(), <<"to">> => bob(), <<"type">> => <<"chat">>},
                    [retraction()]),
    {retract_0, _OriginId} = mod_mam_utils:get_retract_id(Packet).

get_retract_id_esl(_) ->
    Packet = packet(<<"message">>,
                    #{<<"from">> => alice(), <<"to">> => bob(), <<"type">> => <<"chat">>},
                    [#xmlel{name = <<"apply-to">>,
                            attrs = #{<<"id">> => <<"S">>, <<"xmlns">> => ?NS_FASTEN},
                            children = [#xmlel{name = <<"retract">>,
                                               attrs = #{<<"xmlns">> => ?NS_ESL_RETRACT}}]}]),
    ?assertEqual({retract_esl, <<"S">>}, mod_mam_utils:get_retract_id(Packet)).

get_retract_id_v1(_) ->
    Packet = packet(<<"message">>,
                    #{<<"from">> => alice(), <<"to">> => bob(),
                      <<"type">> => <<"chat">>, <<"id">> => <<"R">>},
                    [retraction_v1(<<"X">>)]),
    ?assertEqual({retract_1, <<"X">>, <<"R">>}, mod_mam_utils:get_retract_id(Packet)).

%% A v1 retraction message lacking the required @id yields a retraction_ref
%% with an undefined message id. The retraction is later skipped when
%% decoding it into a retraction_info map (which requires a binary id),
%% because XEP-0424 defines this attribute as mandatory.
get_retract_id_v1_missing_message_id(_) ->
    Packet = packet(<<"message">>,
                    #{<<"from">> => alice(), <<"to">> => bob(), <<"type">> => <<"chat">>},
                    [retraction_v1(<<"X">>)]),
    ?assertEqual({retract_1, <<"X">>, undefined}, mod_mam_utils:get_retract_id(Packet)).

get_retract_id_none(_) ->
    Packet = packet(<<"message">>,
                    #{<<"from">> => alice(), <<"to">> => bob(), <<"type">> => <<"chat">>},
                    [body()]),
    ?assertEqual(none, mod_mam_utils:get_retract_id(Packet)).

retracted_element_v1_shape(_) ->
    Env = #{retract_type => retract_1,
            packet => packet(<<"message">>, #{}, []),
            message_id => 0,
            origin_id => <<"orig">>,
            retraction_message_id => <<"R">>},
    LocJid = jid:from_binary(alice()),
    Tombstone = mod_mam_utils:tombstone(Env, LocJid),
    [Retracted] = Tombstone#xmlel.children,
    #xmlel{name = <<"retracted">>, attrs = Attrs, children = []} = Retracted,
    ?assertEqual(?NS_RETRACT_1, maps:get(<<"xmlns">>, Attrs)),
    ?assertEqual(<<"R">>, maps:get(<<"id">>, Attrs)),
    ?assert(is_binary(maps:get(<<"stamp">>, Attrs))).

%% Generators
gen_modules() ->
    oneof([mod_mam_pm, mod_inbox]).

gen_directions() ->
    oneof([outgoing, incoming]).

gen_arc_markers() ->
    boolean().

gen_attrs(Bin) when is_binary(Bin) ->
    ?LET({From, To}, {gen_jid(), gen_jid()}, attrs(From, To, Bin));
gen_attrs(Type) when is_atom(Type) ->
    ?LET({From, To, MsgType}, {gen_jid(), gen_jid(), gen_msg_type(Type)}, attrs(From, To, MsgType)).

gen_msg_type(all) ->
    oneof([<<"normal">>, <<"chat">>, <<"groupchat">>, <<"error">>]);
gen_msg_type(good) ->
    oneof([<<"normal">>, <<"chat">>, <<"groupchat">>]);
gen_msg_type(strange) ->
    ?SUCHTHAT(B, non_empty(binary()),
              not lists:member(B, [<<"normal">>, <<"chat">>, <<"groupchat">>, <<"error">>])).

gen_jid() ->
    oneof([alice(), bob(), room()]).

gen_children() ->
    do_gen_children([undefined], []).

gen_children([], F) ->
    do_gen_children([undefined], F);
gen_children(F1, F2) ->
    do_gen_children(F1, F2).

do_gen_children(ForceOneYes, MustNotContain) ->
    ?LET({B1, B2, B3, B4, B5, B6, B7, OneYes},
         {boolean(), boolean(), boolean(), boolean(), boolean(), boolean(), boolean(), ForceOneYes},
         begin
             Elems = [{body, body(), B1},
                      {store, store(), B2},
                      {marker, chat_marker(), B3},
                      {retraction, retraction(), B4},
                      {result, mam_result(), B5},
                      {delay, offline_delay(), B6},
                      {no_store, no_store(), B7}],
             Children = [ maybe_get(Val, OneYes, MustNotContain) || Val <- Elems ],
             lists:filter(fun(El) -> El =/= false end, Children)
         end).

maybe_get({Elem, XmlElem, _}, Elem, _) ->
    XmlElem;
maybe_get({Elem, XmlElem, Maybe}, _, MustNotContain) ->
    Maybe andalso not lists:member(Elem, MustNotContain) andalso XmlElem.

%% Possible XML elements
attrs(From, To, Type) ->
    #{<<"from">> => From, <<"to">> => To, <<"type">> => Type}.
body() ->
    #xmlel{name = <<"body">>, children = [#xmlcdata{content = bin()}]}.
chat_marker() ->
    #xmlel{name = <<"displayed">>, attrs = #{<<"xmlmn">> => ?NS_CHAT_MARKERS, <<"id">> => bin()}}.
retraction() ->
    #xmlel{name = <<"apply-to">>,
           attrs = #{<<"id">> => bin(), <<"xmlns">> => ?NS_FASTEN},
           children = [#xmlel{name = <<"retract">>, attrs = #{<<"xmlns">> => ?NS_RETRACT_0}}]}.
retraction_v1(Id) ->
    #xmlel{name = <<"retract">>,
           attrs = #{<<"id">> => Id, <<"xmlns">> => ?NS_RETRACT_1}}.
mam_result() ->
    #xmlel{name = <<"result">>,
           attrs = #{<<"id">> => bin(), <<"queryid">> => bin(), <<"xmlns">> => ?NS_MAM_06}}.
offline_delay() ->
    #xmlel{name = <<"delay">>, attrs = #{<<"stamp">> => bin(), <<"xmlns">> => ?NS_DELAY}}.
no_store() ->
    #xmlel{name = <<"no-store">>, attrs = #{<<"xmlns">> => ?NS_HINTS}}.
store() ->
    #xmlel{name = <<"store">>, attrs = #{<<"xmlns">> => ?NS_HINTS}}.
packet(Name, Attrs, Children) ->
    #xmlel{name = Name, attrs = Attrs, children = Children}.

%% Helpers
bin() ->
    binary:encode_hex(crypto:strong_rand_bytes(8)).
alice() ->
    <<"alice@localhost">>.
bob() ->
    <<"bob@localhost">>.
room() ->
    <<"room@muclight.localhost">>.

run_prop(PropName, Property) ->
    Opts = [quiet, long_result, {start_size, 2}, {numtests, 1000},
            {numworkers, erlang:system_info(schedulers_online)}],
    case proper:quickcheck(proper:conjunction([{PropName, Property}]), Opts) of
        true -> ok;
        Res -> ct:fail(Res)
    end.
