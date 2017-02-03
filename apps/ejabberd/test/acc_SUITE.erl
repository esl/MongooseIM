%% @doc This suite tests both old ejabberd_commands module, which is slowly getting deprecated,
%% and the new mongoose_commands implementation.
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
      [store_and_retrieve ]
     }
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% test methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


store_and_retrieve(_C) ->
    Acc = mongoose_acc:new(),
    Acc2 = mongoose_acc:put(check, 1, Acc),
    ?assertEqual(mongoose_acc:get(check, Acc2), 1),
    PDef = <<"one@jid">>,
    PDef2 = <<"two@jid">>,
    Acc3 = mongoose_acc:store(privacy, PDef, allow, Acc2),
    Acc4 = mongoose_acc:store(privacy, PDef2, deny, Acc3),
    ?assertEqual(mongoose_acc:retrieve(privacy, PDef, Acc4), allow),
    ?assertEqual(mongoose_acc:retrieve(privacy, PDef2, Acc4), deny),
    ?assertEqual(mongoose_acc:retrieve(privacy, sthgelse, Acc4), undefined),
    ?assertEqual(mongoose_acc:retrieve(shmivacy, PDef, Acc4), undefined),
    ok.


