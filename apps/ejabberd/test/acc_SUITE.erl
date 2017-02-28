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
      [store_and_retrieve, init_from_element ]
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
    Elem = {xmlel, <<"iq">>,
        [{<<"xml:lang">>, <<"en">>}, {<<"type">>, <<"set">>}],
        [{xmlel, <<"block">>,
            [{<<"xmlns">>, <<"urn:xmpp:blocking">>}],
            [{xmlel, <<"item">>,
                [{<<"jid">>, <<"bob37.814302@localhost">>}],
                []}]}]},
    Acc = mongoose_acc:from_element(Elem),
    mongoose_acc:dump(Acc),
    ?PRT("Acc", Acc),
    ?assertEqual(mongoose_acc:get(name, Acc), <<"iq">>),
    ?assertEqual(mongoose_acc:get(type, Acc), <<"set">>),
    ?assertEqual(mongoose_acc:get(command, Acc), <<"block">>),
    ?assertEqual(mongoose_acc:get(xmlns, Acc), <<"urn:xmpp:blocking">>),
    ok.
