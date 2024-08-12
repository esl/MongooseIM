%%==============================================================================
%% Copyright 2013 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(metrics_roster_SUITE).
-compile([export_all, nowarn_export_all]).

-import(distributed_helper, [mim/0, require_rpc_nodes/1, rpc/4]).
-import(domain_helper, [host_type/0]).
-import(roster_helper, [assert_roster_event/2, assert_subscription_event/3]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, roster},
     {group, subscriptions}
    ].

groups() ->
    [{roster, [parallel], roster_tests()},
     {subscriptions, [parallel], subscription_tests()}].

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

roster_tests() -> [get_roster,
                   add_contact,
                   roster_push].

subscription_tests() -> [subscribe,
                         unsubscribe,
                         decline_subscription].
%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    instrument_helper:start(declared_events()),
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config),
    instrument_helper:stop().

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob])).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob])).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(add_contact, Config) ->
    [{_, UserSpec} | _] = escalus_config:get_config(escalus_users, Config),
    remove_roster(Config, UserSpec),
    escalus:end_per_testcase(add_contact, Config);
end_per_testcase(roster_push, Config) ->
    [{_, UserSpec} | _] = escalus_config:get_config(escalus_users, Config),
    remove_roster(Config, UserSpec),
    escalus:end_per_testcase(roster_push, Config);
end_per_testcase(subscribe, Config) ->
    end_rosters_remove(Config);
end_per_testcase(decline_subscription, Config) ->
    end_rosters_remove(Config);
end_per_testcase(unsubscribe, Config) ->
    end_rosters_remove(Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

end_rosters_remove(Config) ->
    [{_, UserSpec1}, {_, UserSpec2} | _] =
        escalus_config:get_config(escalus_users, Config),
    remove_roster(Config, UserSpec1),
    remove_roster(Config, UserSpec2),
    escalus:end_per_testcase(subscription, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

get_roster(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% Presences trigger 'get_subscription_lists' events
        assert_backend_event(Alice, get_subscription_lists),
        assert_backend_event(Bob, get_subscription_lists),
        escalus_client:send(Alice, escalus_stanza:roster_get()),
        escalus_client:wait_for_stanza(Alice),
        assert_backend_event(Alice, get_roster),
        assert_roster_event(Alice, mod_roster_get)
        end).

add_contact(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% add contact
        escalus_client:send(Alice,
                            escalus_stanza:roster_add_contact(Bob,
                                                              [<<"friends">>],
                                                              <<"Bobby">>)),
        Received = escalus_client:wait_for_stanza(Alice),
        assert_roster_event(Alice, mod_roster_set),
        escalus_client:send(Alice, escalus_stanza:iq_result(Received)),
        escalus_client:wait_for_stanza(Alice)

        end).

roster_push(Config) ->
    escalus:fresh_story(Config, [{alice, 2}, {bob, 1}], fun(Alice1, Alice2, Bob) ->

        %% add contact
        escalus_client:send(Alice1,
                            escalus_stanza:roster_add_contact(Bob,
                                                              [<<"friends">>],
                                                              <<"Bobby">>)),
        Received = escalus_client:wait_for_stanza(Alice1),
        assert_roster_event(Alice1, mod_roster_set),
        assert_roster_event(Alice1, mod_roster_push),
        escalus_client:send(Alice1, escalus_stanza:iq_result(Received)),
        escalus_client:wait_for_stanza(Alice1),

        Received2 = escalus_client:wait_for_stanza(Alice2),
        assert_roster_event(Alice2, mod_roster_push),
        escalus_client:send(Alice2, escalus_stanza:iq_result(Received2))

        end).

subscribe(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice,Bob) ->
        BobJid = escalus_client:short_jid(Bob),
        AliceJid = escalus_client:short_jid(Alice),

        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus_client:send(Alice, escalus_stanza:presence_direct(BobJid, <<"subscribe">>)),
        PushReq = escalus_client:wait_for_stanza(Alice),
        escalus_client:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription request
        escalus_client:wait_for_stanza(Bob),

        %% Bob adds new contact to his roster
        escalus_client:send(Bob,
                            escalus_stanza:roster_add_contact(Alice,
                                                              [<<"enemies">>],
                                                              <<"Alice">>)),
        PushReqB = escalus_client:wait_for_stanza(Bob),
        escalus_client:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus_client:wait_for_stanza(Bob),

        %% Bob sends subscribed presence
        escalus_client:send(Bob, escalus_stanza:presence_direct(AliceJid, <<"subscribed">>)),

        %% Alice receives subscribed
        escalus_client:wait_for_stanzas(Alice, 3),
        assert_subscription_event(Bob, Alice, fun(#{subscription_count := 1}) -> true end),

        %% Bob receives roster push
        escalus_client:wait_for_stanza(Bob)

        end).

decline_subscription(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice,Bob) ->
        BobJid = escalus_client:short_jid(Bob),
        AliceJid = escalus_client:short_jid(Alice),

        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus_client:send(Alice, escalus_stanza:presence_direct(BobJid, <<"subscribe">>)),
        PushReq = escalus_client:wait_for_stanza(Alice),
        escalus_client:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription request
        escalus_client:wait_for_stanza(Bob),

        %% Bob refuses subscription
        escalus_client:send(Bob, escalus_stanza:presence_direct(AliceJid, <<"unsubscribed">>)),

        %% Alice receives unsubscribed
        escalus_client:wait_for_stanzas(Alice, 2),
        assert_subscription_event(Bob, Alice, fun(#{unsubscription_count := 1}) -> true end)

        end).


unsubscribe(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice,Bob) ->
        BobJid = escalus_client:short_jid(Bob),
        AliceJid = escalus_client:short_jid(Alice),

        %% add contact
        add_sample_contact(Alice, Bob),
        %% subscribe
        escalus_client:send(Alice, escalus_stanza:presence_direct(BobJid, <<"subscribe">>)),
        PushReq = escalus_client:wait_for_stanza(Alice),

        escalus_client:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription request
        escalus_client:wait_for_stanza(Bob),
        %% Bob adds new contact to his roster
        escalus_client:send(Bob,
                            escalus_stanza:roster_add_contact(Alice,
                                                              [<<"enemies">>],
                                                              <<"Alice">>)),
        PushReqB = escalus_client:wait_for_stanza(Bob),
        escalus_client:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus_client:wait_for_stanza(Bob),

        %% Bob sends subscribed presence
        escalus_client:send(Bob, escalus_stanza:presence_direct(AliceJid, <<"subscribed">>)),

        %% Alice receives subscribed
        escalus_client:wait_for_stanzas(Alice, 2),

        escalus_client:wait_for_stanza(Alice),

        %% Bob receives roster push
        PushReqB1 = escalus_client:wait_for_stanza(Bob),
        escalus_assert:is_roster_set(PushReqB1),

        %% Alice sends unsubscribe
        escalus_client:send(Alice, escalus_stanza:presence_direct(BobJid, <<"unsubscribe">>)),

        PushReqA2 = escalus_client:wait_for_stanza(Alice),
        escalus_client:send(Alice, escalus_stanza:iq_result(PushReqA2)),

        %% Bob receives unsubscribe
        escalus_client:wait_for_stanzas(Bob, 2),
        assert_subscription_event(Bob, Alice, fun(#{unsubscription_count := 1}) -> true end)

    end).

%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

add_sample_contact(Alice, Bob) ->
    add_sample_contact(Alice, Bob, [<<"friends">>], <<"generic :p name">>).

add_sample_contact(Alice, Bob, Groups, Name) ->
    escalus_client:send(Alice,
        escalus_stanza:roster_add_contact(Bob, Groups, Name)),
    RosterPush = escalus_client:wait_for_stanza(Alice),
    escalus:assert(is_roster_set, RosterPush),
    escalus_client:send(Alice, escalus_stanza:iq_result(RosterPush)),
    escalus_client:wait_for_stanza(Alice).


remove_roster(Config, UserSpec) ->
    [Username, Server, _Pass] = escalus_users:get_usp(Config, UserSpec),
    Acc = mongoose_helper:new_mongoose_acc(Server),
    Extra = #{host_type => domain_helper:host_type()},
    Params = #{jid => jid:make_bare(Username, Server)},
    rpc(mim(), mod_roster, remove_user, [Acc, Params, Extra]).

declared_events() ->
    declared_backend_events() ++ declared_sm_events() ++ instrument_helper:declared_events(mod_roster).

declared_sm_events() ->
    [{sm_presence_subscription, #{host_type => host_type()}}].

declared_backend_events() ->
    BackendMod = backend_mod(),
    HostType = host_type(),
    Functions = [get_roster, get_subscription_lists],
    [{BackendMod, #{host_type => HostType, function => Function}} || Function <- Functions].

%% This works only for get_roster and get_subscription_lists because of the function arguments
assert_backend_event(Client, Function) ->
    ClientJid = jid:from_binary(escalus_utils:get_short_jid(Client)),
    instrument_helper:assert_one(
      backend_mod(), #{host_type => host_type(), function => Function},
      fun(#{count := 1, time := T, args := [_, User, Server]}) when T > 0 ->
              ClientJid =:= jid:make_bare(User, Server)
      end).

backend_mod() ->
    rpc(mim(), mongoose_backend, get_backend_module, [host_type(), mod_roster]).
