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
-include("ejabberd_c2s.hrl").
-include("mongoose.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include_lib("mod_roster.hrl").
-compile([export_all]).

-define(_eq(E, I), ?_assertEqual(E, I)).
-define(eq(E, I), ?assertEqual(E, I)).
-define(am(E, I), ?assertMatch(E, I)).
-define(ne(E, I), ?assert(E =/= I)).


all() -> [
    roster_old,
    roster_old_with_filter,
    roster_new,
    roster_case_insensitive
].

init_per_suite(C) ->
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    ok = stringprep:start(),
    {ok, _} = application:ensure_all_started(exometer_core),
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
    mod_roster:remove_user(a(), host()),
    gen_mod:stop_module(host(), mod_roster),
    delete_ets(),
    meck:unload(gen_iq_handler),
    C.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


roster_old(_C) ->
    R1 = get_roster_old(),
    ?assertEqual(length(R1), 0),
    mod_roster:set_items(a(), host(), addbob_stanza()),
    assert_state_old(none, none),
    subscription(out, subscribe),
    assert_state_old(none, out),
    ok.

roster_old_with_filter(_C) ->
    R1 = get_roster_old(),
    ?assertEqual(0, length(R1)),
    mod_roster:set_items(a(), host(), addbob_stanza()),
    assert_state_old(none, none),
    subscription(in, subscribe),
    R2 = get_roster_old(),
    ?assertEqual(0, length(R2)),
    R3 = get_full_roster(),
    ?assertEqual(1, length(R3)),
    ok.

roster_new(_C) ->
    R1 = mod_roster:get_roster_entry(a(), host(), bob()),
    ?assertEqual(does_not_exist, R1),
    mod_roster:set_items(a(), host(), addbob_stanza()),
    assert_state_old(none, none),
    ct:pal("get_roster_old(): ~p", [get_roster_old()]),
    R2 = mod_roster:get_roster_entry(a(), host(), bob()),
    ?assertMatch(#roster{}, R2), % is not guaranteed to contain full info
    R3 = mod_roster:get_roster_entry(a(), host(), bob(), full),
    assert_state(R3, none, none, [<<"friends">>]),
    subscription(out, subscribe),
    R4 = mod_roster:get_roster_entry(a(), host(), bob(), full),
    assert_state(R4, none, out, [<<"friends">>]).


roster_case_insensitive(_C) ->
    mod_roster:set_items(a(), host(), addbob_stanza()),
    R1 = get_roster_old(),
    ?assertEqual(1, length(R1)),
    R2 = get_roster_old(ae()),
    ?assertEqual(1, length(R2)),
    R3 = mod_roster:get_roster_entry(a(), host(), bob(), full),
    assert_state(R3, none, none, [<<"friends">>]),
    R3 = mod_roster:get_roster_entry(ae(), host(), bob(), full),
    assert_state(R3, none, none, [<<"friends">>]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%% HELPERS %%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assert_state(Rentry, Subscription, Ask, Groups) ->
    ?assertEqual(Subscription, Rentry#roster.subscription),
    ?assertEqual(Ask, Rentry#roster.ask),
    ?assertEqual(Groups, Rentry#roster.groups).

subscription(Direction, Type) ->
    LBob = jid:to_lower(jid:from_binary(bob())),
    TFun = fun() -> mod_roster:process_subscription_transaction(Direction,
                                                                a(),
                                                                host(),
                                                                LBob,
                                                                Type,
                                                                <<"">>)
           end,
    {atomic, _} = mod_roster:transaction(host(), TFun).

get_roster_old() ->
    get_roster_old(a()).

get_roster_old(User) ->
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => <<"localhost">>,
                              element => undefined }),
    Acc1 = mod_roster:get_user_roster(Acc, {User, host()}),
    mongoose_acc:get(roster, items, Acc1).

get_full_roster() ->
    Acc0 = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => <<"localhost">>,
                              element => undefined }),
    Acc1 = mongoose_acc:set(roster, show_full_roster, true, Acc0),
    Acc2 = mod_roster:get_user_roster(Acc1, {a(), host()}),
    mongoose_acc:get(roster, items, Acc2).

assert_state_old(Subscription, Ask) ->
    [Rentry] = get_roster_old(),
    ?assertEqual(Subscription, Rentry#roster.subscription),
    ?assertEqual(Ask, Rentry#roster.ask).

init_ets() ->
    catch ets:new(local_config, [named_table]),
    catch ets:new(mongoose_services, [named_table]),
    ok.

delete_ets() ->
    catch ets:delete(local_config),
    catch ets:delete(mongoose_services),
    ok.

a() -> <<"alice">>.
ae() -> <<"alicE">>.

host() -> <<"localhost">>.

bob() -> <<"bob@localhost">>.

addbob_stanza() ->
    #xmlel{children = [
        #xmlel{
            attrs = [{<<"jid">>, bob()}],
            children = [
                #xmlel{name = <<"group">>,
                    children = [
                        #xmlcdata{content = <<"friends">>}
                    ]}
            ]}
        ]
    }.
