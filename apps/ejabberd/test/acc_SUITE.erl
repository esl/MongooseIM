%% @doc This suite tests API of accumulator encapsulated in mongoose_acc module
-module(acc_SUITE).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/include/ejabberd_commands.hrl").
-include_lib("ejabberd/include/jlib.hrl").

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
      [store_and_retrieve, init_from_element, get_and_require, strip,
          parse_with_cdata]
     }
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% test methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


store_and_retrieve(_C) ->
    Acc = mongoose_acc:new(),
    Acc2 = mongoose_acc:put(check, 1, Acc),
    ?assertEqual(mongoose_acc:get(check, Acc2), 1),
    ok.


init_from_element(_C) ->
    Acc = mongoose_acc:from_element(sample_stanza()),
    mongoose_acc:dump(Acc),
    ?PRT("Acc", Acc),
    ?assertEqual(mongoose_acc:get(name, Acc), <<"iq">>),
    ?assertEqual(mongoose_acc:get(type, Acc), <<"set">>),
    ok.


get_and_require(_C) ->
    Acc = mongoose_acc:from_element(sample_stanza()),
    ?assertEqual(mongoose_acc:get(command, Acc, nope), nope),
    ?assertEqual(mongoose_acc:get(xmlns, Acc, nope), nope),
    Acc1 = mongoose_acc:require(command, Acc),
    ?assertEqual(mongoose_acc:get(command, Acc1), <<"block">>),
    ?assertEqual(mongoose_acc:get(xmlns, Acc, nope), nope),
    Acc2 = mongoose_acc:require([command, xmlns], Acc),
    ?assertEqual(mongoose_acc:get(xmlns, Acc2), <<"urn:xmpp:blocking">>),
    ?assertEqual(mongoose_acc:get(iq_query_info, Acc2, nope), nope),
    Iq = mongoose_acc:from_element(iq_stanza()),
    Iq1 = mongoose_acc:require([iq_query_info], Iq),
    IqData = mongoose_acc:get(iq_query_info, Iq1),
    ?assertEqual(IqData#iq.type, set),
    ?assertEqual(IqData#iq.xmlns, <<"urn:ietf:params:xml:ns:xmpp-session">>),
    ok.

parse_with_cdata(_C) ->
    Acc = mongoose_acc:from_element(stanza_with_cdata()),
    Acc1 = mongoose_acc:require(xmlns, Acc),
    ?assertEqual(mongoose_acc:get(xmlns, Acc1), <<"jabber:iq:roster">>).

strip(_C) ->
    Acc = mongoose_acc:from_element(iq_stanza()),
    Acc1 = mongoose_acc:update(#{from => <<"ja">>, from_jid => <<"jajid">>,
        to => <<"ty">>, to_jid => <<"tyjid">>}, Acc),
    Acc2 = mongoose_acc:require([command, xmlns], Acc1),
    ?assertEqual(mongoose_acc:get(xmlns, Acc2), <<"urn:ietf:params:xml:ns:xmpp-session">>),
    ?assertEqual(mongoose_acc:get(type, Acc2), <<"set">>),
    Ref = mongoose_acc:get(ref, Acc2),
    NAcc = mongoose_acc:strip(Acc2),
    ?assertEqual(mongoose_acc:get(xmlns, NAcc, niema), niema),
    ?assertEqual(mongoose_acc:get(to_jid, NAcc), <<"tyjid">>),
    ?assertEqual(mongoose_acc:get(ref, NAcc, ref), Ref).


sample_stanza() ->
    {xmlel, <<"iq">>,
        [{<<"xml:lang">>, <<"en">>}, {<<"type">>, <<"set">>}],
        [{xmlel, <<"block">>,
            [{<<"xmlns">>, <<"urn:xmpp:blocking">>}],
            [{xmlel, <<"item">>,
                [{<<"jid">>, <<"bob37.814302@localhost">>}],
                []}]}]}.


stanza_with_cdata() ->
    Txt = <<"<iq type=\"get\" id=\"aab9a\"><query xmlns=\"jabber:iq:roster\"/>\" </iq>\">">>,
    {ok, X} = exml:parse(Txt),
    X.


iq_stanza() ->
    {xmlel,<<"iq">>,
        [{<<"type">>,<<"set">>},
            {<<"id">>,<<"a31baa4c478896af19b76bac799b65ed">>}],
        [{xmlel,<<"session">>,
            [{<<"xmlns">>,<<"urn:ietf:params:xml:ns:xmpp-session">>}],
            []}]}.

another_iq_stanza() ->
    {xmlel,<<"iq">>,
        [{<<"type">>,<<"pet">>},
            {<<"id">>,<<"a31baa4c478896af19b76bac799b65ed">>}],
        [{xmlel,<<"session">>,
            [{<<"xmlns">>,<<"urn:ietf:params:xml:ns:xmpp-session">>}],
            []}]}.
