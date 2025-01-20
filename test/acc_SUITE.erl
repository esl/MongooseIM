%% @doc This suite tests API of accumulator encapsulated in mongoose_acc module
-module(acc_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("jlib.hrl").
-include("mongoose.hrl").

-define(PRT(X, Y), ct:pal("~p: ~p", [X, Y])).
-define(ACC_PARAMS, #{location => ?LOCATION,
                      host_type => <<"local host">>,
                      lserver => <<"localhost">>}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% suite configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    [
     {group, basic}
    ].

groups() ->
    [
     {basic, [sequence],
      [
       store_retrieve_and_delete,
       store_retrieve_and_delete_many,
       init_from_element,
       produce_iq_meta_automatically,
       strip,
       strip_with_params,
       parse_with_cdata
      ]
     }
    ].

init_per_suite(C) ->
    application:ensure_all_started(jid),
    C.

end_per_suite(C) ->
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% test methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

store_retrieve_and_delete(_C) ->
    Acc = mongoose_acc:new(?ACC_PARAMS),
    Acc2 = mongoose_acc:set(ns, check, 1, Acc),
    ?assertEqual(1, mongoose_acc:get(ns, check, Acc2)),
    Acc3 = mongoose_acc:set(ns, check, 2, Acc2),
    ?assertEqual(2, mongoose_acc:get(ns, check, Acc3)),
    Acc4 = mongoose_acc:delete(ns, check, Acc3),
    ?assertError(_, mongoose_acc:get(ns, check, Acc4)),
    ok.

store_permanent_retrieve_and_delete(_C) ->
    Acc = mongoose_acc:new(?ACC_PARAMS),
    Acc2 = mongoose_acc:set_permanent(ns, check, 1, Acc),
    ?assertEqual(1, mongoose_acc:get(ns, check, Acc2)),
    Acc3 = mongoose_acc:set_permanent(ns, check, 2, Acc2),
    ?assertEqual(2, mongoose_acc:get(ns, check, Acc3)),
    ?assertEqual([{ns, check}], mongoose_acc:get_permanent_keys(Acc3)),
    Acc4 = mongoose_acc:delete(ns, check, Acc3),
    ?assertError(_, mongoose_acc:get(ns, check, Acc4)),
    ?assertEqual([], mongoose_acc:get_permanent_keys(Acc4)),
    ok.

store_retrieve_and_delete_many(_C) ->
    Acc = mongoose_acc:new(?ACC_PARAMS),
    KV = [{check, 1}, {check2, 2}, {check3, 3}],
    Acc2 = mongoose_acc:set(ns, [{check3, 0} | KV], Acc),
    [?assertEqual(V, mongoose_acc:get(ns, K, Acc2)) || {K, V} <- KV],
    NS = mongoose_acc:get(ns, Acc2),
    ?assertEqual(lists:sort(NS), lists:sort(KV)),
    Acc3 = mongoose_acc:delete_many(ns, [K || {K, _} <- KV], Acc2),
    [?assertError(_, mongoose_acc:get(ns, K, Acc3)) || {K, _} <- KV],
    ?assertEqual([], mongoose_acc:get(ns, Acc3)),
    Acc4 = mongoose_acc:delete(ns, Acc2),
    [?assertError(_, mongoose_acc:get(ns, K, Acc4)) || {K, _} <- KV],
    ?assertEqual([], mongoose_acc:get(ns, Acc4)),
    ok.

store_permanent_retrieve_and_delete_many(_C) ->
    Acc = mongoose_acc:new(?ACC_PARAMS),
    KV = [{check, 1}, {check2, 2}, {check3, 3}],
    NK = [{ns, K} || {K, _} <- KV],
    Acc2 = mongoose_acc:set_permanent(ns, [{check3, 0} | KV], Acc),
    [?assertEqual(V, mongoose_acc:get(ns, K, Acc2)) || {K, V} <- KV],
    NS = mongoose_acc:get(ns, Acc2),
    ?assertEqual(lists:sort(NS), lists:sort(KV)),
    ?assertEqual(lists:sort(NK), lists:sort(mongoose_acc:get_permanent_keys(Acc2))),
    Acc3 = mongoose_acc:delete_many(ns, [K || {K, _} <- KV], Acc2),
    [?assertError(_, mongoose_acc:get(ns, K, Acc3)) || {K, _} <- KV],
    ?assertEqual([], mongoose_acc:get(ns, Acc3)),
    ?assertEqual([], mongoose_acc:get_permanent_keys(Acc3)),
    Acc4 = mongoose_acc:delete(ns, Acc2),
    [?assertError(_, mongoose_acc:get(ns, K, Acc4)) || {K, _} <- KV],
    ?assertEqual([], mongoose_acc:get(ns, Acc4)),
    ?assertEqual([], mongoose_acc:get_permanent_keys(Acc4)),
    ok.

init_from_element(_C) ->
    Acc = mongoose_acc:new(?ACC_PARAMS#{element => sample_stanza()}),
    ?PRT("Acc", Acc),
    ?assertEqual(<<"iq">>, mongoose_acc:stanza_name(Acc)),
    ?assertEqual(<<"set">>, mongoose_acc:stanza_type(Acc)),
    ok.


produce_iq_meta_automatically(_C) ->
    Acc = mongoose_acc:new(?ACC_PARAMS#{element => sample_stanza()}),
    {Command, Acc1} = mongoose_iq:command(Acc),
    ?assertEqual(<<"block">>, Command),
    % We check for exactly the same Acc, as there is no need to update the cache
    {XMLNS, Acc1} = mongoose_iq:xmlns(Acc1),
    ?assertEqual(<<"urn:xmpp:blocking">>, XMLNS),
    Iq = mongoose_acc:update_stanza(#{ element => iq_stanza() }, Acc1),
    {IqData, _Iq1} = mongoose_iq:info(Iq),
    ?assertEqual(set, IqData#iq.type),
    ?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-session">>, IqData#iq.xmlns),
    ok.

parse_with_cdata(_C) ->
    Acc = mongoose_acc:new(?ACC_PARAMS#{element => stanza_with_cdata()}),
    {XMLNS, _} = mongoose_iq:xmlns(Acc),
    ?assertEqual(<<"jabber:iq:roster">>, XMLNS).

strip(_C) ->
    El = iq_stanza(),
    FromJID = jid:make(<<"jajid">>, <<"localhost">>, <<>>),
    ToJID = jid:make(<<"tyjid">>, <<"localhost">>, <<>>),
    Server = maps:get(lserver, ?ACC_PARAMS),
    HostType = maps:get(host_type, ?ACC_PARAMS),
    Acc = mongoose_acc:new(?ACC_PARAMS#{element => El,
                                        from_jid => FromJID,
                                        to_jid => ToJID}),
    {XMLNS1, Acc1} = mongoose_iq:xmlns(Acc),
    ?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-session">>, XMLNS1),
    ?assertEqual(<<"set">>, mongoose_acc:stanza_type(Acc1)),
    ?assertEqual(undefined, mongoose_acc:get(ns, ppp, undefined, Acc1)),
    Acc2 = mongoose_acc:set_permanent(ns, ppp, 997, Acc1),
    Acc3 = mongoose_acc:set(ns2, [{a, 1}, {b, 2}], Acc2),
    ?assertMatch([_, _], mongoose_acc:get(ns2, Acc3)),
    ?assertEqual(Server, mongoose_acc:lserver(Acc3)),
    ?assertEqual(HostType, mongoose_acc:host_type(Acc3)),
    ?assertEqual({FromJID, ToJID, El}, mongoose_acc:packet(Acc3)),
    Ref = mongoose_acc:ref(Acc3),
    %% strip stanza and check that only non-permanent fields are missing
    NAcc1 = mongoose_acc:strip(Acc3),
    {XMLNS3, NAcc2} = mongoose_iq:xmlns(NAcc1),
    ?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-session">>, XMLNS3),
    ?assertEqual(<<"set">>, mongoose_acc:stanza_type(NAcc2)),
    ?assertEqual(Server, mongoose_acc:lserver(NAcc2)),
    ?assertEqual(HostType, mongoose_acc:host_type(NAcc2)),
    ?assertEqual({FromJID, ToJID, El}, mongoose_acc:packet(NAcc2)),
    ?assertEqual(Ref, mongoose_acc:ref(NAcc2)),
    ?assertEqual(997, mongoose_acc:get(ns, ppp, NAcc2)),
    ?assertEqual([], mongoose_acc:get(ns2, NAcc2)).

strip_with_params(_Config) ->
    FromJID = jid:make(<<"jajid">>, <<"localhost">>, <<>>),
    ToJID = jid:make(<<"tyjid">>, <<"localhost">>, <<>>),
    Server = maps:get(lserver, ?ACC_PARAMS),
    HostType = maps:get(host_type, ?ACC_PARAMS),
    Acc = mongoose_acc:new(?ACC_PARAMS#{element => iq_stanza(),
                                        from_jid => FromJID,
                                        to_jid => ToJID}),
    {XMLNS1, Acc1} = mongoose_iq:xmlns(Acc),
    ?assertEqual(<<"urn:ietf:params:xml:ns:xmpp-session">>, XMLNS1),
    ?assertEqual(<<"set">>, mongoose_acc:stanza_type(Acc1)),
    ?assertEqual(undefined, mongoose_acc:get(ns, ppp, undefined, Acc1)),
    Acc2 = mongoose_acc:set_permanent(ns, ppp, 997, Acc1),
    Acc3 = mongoose_acc:set(ns2, [{a, 1}, {b, 2}], Acc2),
    ?assertMatch([_, _], mongoose_acc:get(ns2, Acc3)),
    ?assertEqual(Server, mongoose_acc:lserver(Acc3)),
    ?assertEqual(HostType, mongoose_acc:host_type(Acc3)),
    Ref = mongoose_acc:ref(Acc3),
    %% strip stanza with params and check that new params are applied
    %% and non-permanent fields are missing
    NewServer = <<"test.", Server/binary>>,
    NewHostType = <<"new ", HostType/binary>>,
    NewStanza = sample_stanza(),
    StripParams = #{lserver => NewServer,
                    host_type => NewHostType,
                    element => NewStanza},
    NAcc1 = mongoose_acc:strip(StripParams, Acc3),
    {XMLNS2, NAcc2} = mongoose_iq:xmlns(NAcc1),
    ?assertEqual(<<"urn:xmpp:blocking">>, XMLNS2),
    ?assertEqual(jid:from_binary(<<"a@localhost">>), mongoose_acc:to_jid(NAcc2)),
    ?assertEqual(Ref, mongoose_acc:ref(NAcc2)),
    ?assertEqual(997, mongoose_acc:get(ns, ppp, NAcc2)),
    ?assertEqual([], mongoose_acc:get(ns2, NAcc2)),
    ?assertEqual(sample_stanza(), mongoose_acc:element(NAcc2)),
    ?assertEqual(NewServer, mongoose_acc:lserver(NAcc2)),
    ?assertEqual(NewHostType, mongoose_acc:host_type(NAcc2)).

sample_stanza() ->
    #xmlel{name = <<"iq">>,
           attrs = #{<<"xml:lang">> => <<"en">>,
                     <<"type">> => <<"set">>,
                     <<"from">> => <<"a@localhost">>,
                     <<"to">> => <<"a@localhost">>},
           children = [
            #xmlel{name = <<"block">>,
                   attrs = #{<<"xmlns">> => <<"urn:xmpp:blocking">>},
                   children = [
                    #xmlel{name = <<"item">>,
                           attrs = #{<<"jid">> => <<"bob37.814302@localhost">>}}
                   ]}
           ]}.


stanza_with_cdata() ->
    Txt = <<"<iq type=\"get\" id=\"aab9a\" from=\"a@localhost\" to=\"a@localhost\">"
            "<query xmlns=\"jabber:iq:roster\"/>\" \"</iq>">>,
    {ok, X} = exml:parse(Txt),
    X.


iq_stanza() ->
    #xmlel{ name = <<"iq">>,
            attrs = #{<<"type">> => <<"set">>,
                      <<"id">> => <<"a31baa4c478896af19b76bac799b65ed">>,
                      <<"from">> => <<"a@localhost">>,
                      <<"to">> => <<"localhost">>},
            children = [
                #xmlel{ name = <<"session">>,
                        attrs = #{<<"xmlns">> => <<"urn:ietf:params:xml:ns:xmpp-session">>}}
            ]}.

another_iq_stanza() ->
    #xmlel{ name = <<"iq">>,
            attrs = #{<<"type">> => <<"pet">>,
                      <<"id">> => <<"a31baa4c478896af19b76bac799b65ed">>,
                      <<"from">> => <<"a@localhost">>,
                      <<"to">> => <<"localhost">>},
            children = [
                #xmlel{ name = <<"session">>,
                        attrs = #{<<"xmlns">> => <<"urn:ietf:params:xml:ns:xmpp-session">>}}
            ]}.
