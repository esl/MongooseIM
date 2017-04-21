%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Apr 2017 16:09
%%%-------------------------------------------------------------------
-module(roster_SUITE).
-author("bartek").

-include_lib("eunit/include/eunit.hrl").
-include_lib("ejabberd/src/ejabberd_c2s.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include_lib("mod_roster.hrl").
-compile([export_all]).

-define(_eq(E, I), ?_assertEqual(E, I)).
-define(eq(E, I), ?assertEqual(E, I)).
-define(am(E, I), ?assertMatch(E, I)).
-define(ne(E, I), ?assert(E =/= I)).


all() -> [
    roster_old,
    roster_old_with_filter
].

init_per_suite(C) ->
    init_ets(),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    ok = stringprep:start(),
    {ok, _} = application:ensure_all_started(exometer),
    C.

end_per_suite(C) ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    C.

init_per_testcase(_TC, C) ->
    init_ets(),
    ejabberd_hooks:start_link(),
    meck:new(gen_iq_handler),
    meck:expect(gen_iq_handler, add_iq_handler, fun(_, _, _, _, _, _) -> ok end),
    meck:expect(gen_iq_handler, remove_iq_handler, fun(_, _, _) -> ok end),
    gen_mod:start(),
    gen_mod:start_module(host(), mod_roster, []),
    C.

end_per_testcase(_TC, C) ->
    mod_roster:remove_user(<<"alice">>, host()),
    gen_mod:stop_module(host(), mod_roster),
    meck:unload(gen_iq_handler),
    C.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


roster_old(_C) ->
    R1 = get_roster(),
    ?assertEqual(length(R1), 0),
    mod_roster:set_items(<<"alice">>, host(), addbob_stanza()),
    assert_state(none, none),
    subscription(out, subscribe),
    assert_state(none, out),
    ok.

roster_old_with_filter(_C) ->
    R1 = get_roster(),
    ?assertEqual(0, length(R1)),
    mod_roster:set_items(<<"alice">>, host(), addbob_stanza()),
    assert_state(none, none),
    subscription(in, subscribe),
    R2 = get_roster(),
    ?assertEqual(0, length(R2)),
    R3 = get_full_roster(),
    ?assertEqual(1, length(R3)),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% HELPERS %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


subscription(Direction, Type) ->
    LBob = jid:to_lower(jid:from_binary(bob())),
    TFun = fun() -> mod_roster:process_subscription_transaction(Direction,
                                                                <<"alice">>,
                                                                host(),
                                                                LBob,
                                                                Type,
                                                                <<"">>)
           end,
    {atomic, _} = mod_roster:transaction(host(), TFun).

get_roster() ->
    Acc = mongoose_acc:new(),
    Acc1 = mod_roster:get_user_roster(Acc, {<<"alice">>, host()}),
    mongoose_acc:get(roster, Acc1).

get_full_roster() ->
    Acc = mongoose_acc:from_kv(show_full_roster, true),
    Acc1 = mod_roster:get_user_roster(Acc, {<<"alice">>, host()}),
    mongoose_acc:get(roster, Acc1).

assert_state(Subscription, Ask) ->
%%    ct:pal("get_roster(): ~p", [get_roster()]),
    [Rentry] = get_roster(),
%%    ct:pal("Rentry: ~p", [Rentry]),
    ?assertEqual(Subscription, Rentry#roster.subscription),
    ?assertEqual(Ask, Rentry#roster.ask).

init_ets() ->
    catch ets:new(local_config, [named_table]),
    ok.

host() -> <<"localhost">>.

bob() -> <<"bob@localhost">>.

addbob_stanza() ->
    #xmlel{children = [
        #xmlel{attrs = [{<<"jid">>, bob()}]}
        ]
    }.
