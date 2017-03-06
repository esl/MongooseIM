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
      [store_and_retrieve, init_from_element, get_and_require ]
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
    ok.


sample_stanza() ->
    {xmlel, <<"iq">>,
        [{<<"xml:lang">>, <<"en">>}, {<<"type">>, <<"set">>}],
        [{xmlel, <<"block">>,
            [{<<"xmlns">>, <<"urn:xmpp:blocking">>}],
            [{xmlel, <<"item">>,
                [{<<"jid">>, <<"bob37.814302@localhost">>}],
                []}]}]}.


