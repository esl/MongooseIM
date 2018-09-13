%% @doc This suite tests API of accumulator encapsulated in mongoose_acc module
-module(acc_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("ejabberd_commands.hrl").
-include("jlib.hrl").
-include("mongoose.hrl").

-define(PRT(X, Y), ct:pal("~p: ~p", [X, Y])).

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
       store_and_retrieve,
       init_from_element,
       produce_iq_meta_automatically,
       strip,
       parse_with_cdata
      ]
     }
    ].

init_per_suite(C) ->
    application:ensure_all_started(stringprep),
    C.

end_per_suite(C) ->
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% test methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


store_and_retrieve(_C) ->
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => <<"localhost">>,
                              element => undefined }),
    Acc2 = mongoose_acc:set(ns, check, 1, Acc),
    ?assertEqual(mongoose_acc:get(ns, check, Acc2), 1),
    ok.


init_from_element(_C) ->
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => <<"localhost">>,
                              element => sample_stanza() }),
    mongoose_acc:dump(Acc),
    ?PRT("Acc", Acc),
    ?assertEqual(mongoose_acc:stanza_name(Acc), <<"iq">>),
    ?assertEqual(mongoose_acc:stanza_type(Acc), <<"set">>),
    ok.


produce_iq_meta_automatically(_C) ->
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => <<"localhost">>,
                              element => sample_stanza() }),
    {Command, Acc1} = mongoose_iq:command(Acc),
    ?assertEqual(Command, <<"block">>),
    % We check for exactly the same Acc, as there is no need to update the cache
    {XMLNS, Acc1} = mongoose_iq:xmlns(Acc1),
    ?assertEqual(XMLNS, <<"urn:xmpp:blocking">>),
    Iq = mongoose_acc:update_stanza(#{ element => iq_stanza() }, Acc1),
    {IqData, _Iq1} = mongoose_iq:record(Iq),
    ?assertEqual(IqData#iq.type, set),
    ?assertEqual(IqData#iq.xmlns, <<"urn:ietf:params:xml:ns:xmpp-session">>),
    ok.

parse_with_cdata(_C) ->
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => <<"localhost">>,
                              element => stanza_with_cdata() }),
    {XMLNS, _} = mongoose_iq:xmlns(Acc),
    ?assertEqual(XMLNS, <<"jabber:iq:roster">>).

strip(_C) ->
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => <<"localhost">>,
                              element => iq_stanza(),
                              from_jid => <<"jajid">>,
                              to_jid => <<"tyjid">> }),
    {XMLNS1, Acc1} = mongoose_iq:xmlns(Acc),
    ?assertEqual(XMLNS1, <<"urn:ietf:params:xml:ns:xmpp-session">>),
    ?assertEqual(mongoose_acc:stanza_type(Acc1), <<"set">>),
    ?assertEqual(undefined, mongoose_acc:get(ns, ppp, undefined, Acc1)),
    Acc2 = mongoose_acc:set(ns, ppp, 997, false, Acc1),
    ?assertEqual(997, mongoose_acc:get(ns, ppp, Acc2)),
    Ref = mongoose_acc:ref(Acc2),
    NAcc = mongoose_acc:strip(#{ lserver => <<"localhost">>, element => undefined }, Acc2),
    {XMLNS2, _} = mongoose_iq:xmlns(NAcc),
    ?assertEqual(XMLNS2, undefined),
    ?assertEqual(mongoose_acc:to_jid(NAcc), undefined),
    ?assertEqual(mongoose_acc:ref(NAcc), Ref),
    ?assertEqual(997, mongoose_acc:get(ns, ppp, NAcc)).


sample_stanza() ->
    {xmlel, <<"iq">>,
        [{<<"xml:lang">>, <<"en">>},
         {<<"type">>, <<"set">>},
         {<<"from">>, <<"a@localhost">>},
         {<<"to">>, <<"a@localhost">>}],
        [{xmlel, <<"block">>,
            [{<<"xmlns">>, <<"urn:xmpp:blocking">>}],
            [{xmlel, <<"item">>,
                [{<<"jid">>, <<"bob37.814302@localhost">>}],
                []}]}]}.


stanza_with_cdata() ->
    Txt = <<"<iq type=\"get\" id=\"aab9a\" from=\"a@localhost\" to=\"a@localhost\">"
            "<query xmlns=\"jabber:iq:roster\"/>\" \"</iq>">>,
    {ok, X} = exml:parse(Txt),
    X.


iq_stanza() ->
    {xmlel,<<"iq">>,
        [{<<"type">>,<<"set">>},
         {<<"id">>,<<"a31baa4c478896af19b76bac799b65ed">>},
         {<<"from">>, <<"a@localhost">>},
         {<<"to">>, <<"localhost">>}],
        [{xmlel,<<"session">>,
            [{<<"xmlns">>,<<"urn:ietf:params:xml:ns:xmpp-session">>}],
            []}]}.

another_iq_stanza() ->
    {xmlel,<<"iq">>,
        [{<<"type">>,<<"pet">>},
         {<<"id">>,<<"a31baa4c478896af19b76bac799b65ed">>},
         {<<"from">>, <<"a@localhost">>},
         {<<"to">>, <<"localhost">>}],
        [{xmlel,<<"session">>,
            [{<<"xmlns">>,<<"urn:ietf:params:xml:ns:xmpp-session">>}],
            []}]}.
