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
-include("mongoose.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include_lib("mod_roster.hrl").
-compile([export_all, nowarn_export_all]).

-define(ACC_PARAMS, #{location => ?LOCATION,
                      lserver => domain(),
                      host_type => host_type(),
                      element => undefined}).

all() -> [
    roster_old,
    roster_old_with_filter,
    roster_new,
    roster_case_insensitive
].

init_per_suite(Config) ->
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    {ok, _} = application:ensure_all_started(jid),
    {ok, _} = application:ensure_all_started(exometer_core),
    meck:new(gen_iq_handler, [no_link]),
    meck:expect(gen_iq_handler, add_iq_handler_for_domain, fun(_, _, _, _, _, _) -> ok end),
    meck:expect(gen_iq_handler, remove_iq_handler_for_domain, fun(_, _, _) -> ok end),
    meck:new(mongoose_domain_api, [no_link]),
    meck:expect(mongoose_domain_api, get_domain_host_type, fun(_) -> {ok, host_type()} end),
    mongoose_config:set_opts(opts()),
    async_helper:start(Config, mongoose_instrument, start_link, []).

end_per_suite(Config) ->
    async_helper:stop_all(Config),
    mongoose_config:erase_opts(),
    meck:unload(),
    mnesia:stop(),
    mnesia:delete_schema([node()]).

init_per_testcase(_TC, C) ->
    init_ets(),
    mongooseim_helper:start_link_loaded_hooks(),
    mongoose_modules:start(),
    C.

end_per_testcase(_TC, C) ->
    Acc = mongoose_acc:new(?ACC_PARAMS),
    JID = jid:make_bare(a(), domain()),
    mod_roster:remove_user(Acc, #{jid => JID}, #{host_type => mongoose_acc:host_type(Acc)}),
    mongoose_modules:stop(),
    delete_ets(),
    C.

opts() ->
    #{hosts => [host_type()],
      host_types => [],
      {modules, host_type()} => #{mod_roster => config_parser_helper:default_mod_config(mod_roster)},
      instrumentation => config_parser_helper:default_config([instrumentation])}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


roster_old(_C) ->
    R1 = get_roster_old(),
    ?assertEqual(length(R1), 0),
    ok = mod_roster:set_items(host_type(), alice_jid(), addbob_stanza()),
    assert_state_old(none, none),
    subscription(out, subscribe),
    assert_state_old(none, out),
    ok.

roster_old_with_filter(_C) ->
    R1 = get_roster_old(),
    ?assertEqual(0, length(R1)),
    ok = mod_roster:set_items(host_type(), alice_jid(), addbob_stanza()),
    assert_state_old(none, none),
    subscription(in, subscribe),
    R2 = get_roster_old(),
    ?assertEqual(0, length(R2)),
    R3 = get_full_roster(),
    ?assertEqual(1, length(R3)),
    ok.

roster_new(_C) ->
    R1 = mod_roster:get_roster_entry(host_type(), alice_jid(), bob_ljid(), short),
    ?assertEqual(does_not_exist, R1),
    ok = mod_roster:set_items(host_type(), alice_jid(), addbob_stanza()),
    assert_state_old(none, none),
    ct:pal("get_roster_old(): ~p", [get_roster_old()]),
    R2 = mod_roster:get_roster_entry(host_type(), alice_jid(), bob_ljid(), short),
    ?assertMatch(#roster{}, R2), % is not guaranteed to contain full info
    R3 = mod_roster:get_roster_entry(host_type(), alice_jid(), bob_ljid(), full),
    assert_state(R3, none, none, [<<"friends">>]),
    subscription(out, subscribe),
    R4 = mod_roster:get_roster_entry(host_type(), alice_jid(), bob_ljid(), full),
    assert_state(R4, none, out, [<<"friends">>]).


roster_case_insensitive(_C) ->
    ok = mod_roster:set_items(host_type(), alice_jid(), addbob_stanza()),
    R1 = get_roster_old(),
    ?assertEqual(1, length(R1)),
    R2 = get_roster_old(ae()),
    ?assertEqual(1, length(R2)),
    R3 = mod_roster:get_roster_entry(host_type(), alice_jid(), bob_ljid(), full),
    assert_state(R3, none, none, [<<"friends">>]),
    R3 = mod_roster:get_roster_entry(host_type(), alicE_jid(), bob_ljid(), full),
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
    TFun = fun() ->
                   mod_roster:process_subscription_t(host_type(), Direction, alice_jid(),
                                                     bob_jid(), Type, <<>>)
           end,
    {atomic, _} = mod_roster:transaction(host_type(), TFun).

get_roster_old() ->
    get_roster_old(a()).

get_roster_old(User) ->
    Acc = mongoose_acc:new(?ACC_PARAMS),
    Params = #{mongoose_acc => Acc, show_full_roster => false, jid => jid:make_bare(User, domain())},
    Extra = #{host_type => mongoose_acc:host_type(Acc)},
    {ok, Roster} = mod_roster:get_user_roster([], Params, Extra),
    Roster.

get_full_roster() ->
    Acc = mongoose_acc:new(?ACC_PARAMS),
    Params = #{mongoose_acc => Acc, show_full_roster => true, jid => alice_jid()},
    Extra = #{host_type => mongoose_acc:host_type(Acc)},
    {ok, Roster} = mod_roster:get_user_roster([], Params, Extra),
    Roster.

assert_state_old(Subscription, Ask) ->
    [Rentry] = get_roster_old(),
    ?assertEqual(Subscription, Rentry#roster.subscription),
    ?assertEqual(Ask, Rentry#roster.ask).

init_ets() ->
    catch ets:new(mongoose_services, [named_table]),
    ok.

delete_ets() ->
    catch ets:delete(mongoose_services),
    ok.

alice_jid() ->
    jid:make(a(), domain(), <<>>).

alicE_jid() ->
    jid:make(ae(), domain(), <<>>).

a() -> <<"alice">>.
ae() -> <<"alicE">>.

domain() -> <<"localhost">>.

bob() -> <<"bob@localhost">>.

bob_jid() ->
    jid:make(<<"bob">>, domain(), <<>>).

bob_ljid() ->
    jid:to_lower(bob_jid()).

host_type() -> <<"test type">>.

addbob_stanza() ->
    #xmlel{children = [
        #xmlel{
            attrs = #{<<"jid">> => bob()},
            children = [
                #xmlel{name = <<"group">>,
                    children = [
                        #xmlcdata{content = <<"friends">>}
                    ]}
            ]}
        ]
    }.
