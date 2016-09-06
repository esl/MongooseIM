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
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").


-import(metrics_helper, [assert_counter/2,
                      get_counter_value/1]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, roster},
     {group, subscriptions}
    ].

groups() ->
    [{roster, [sequence], roster_tests()},
     {subscriptions, [sequence], subscription_tests()}
    ].

suite() ->
    [{required, ejabberd_node} | escalus:suite()].

roster_tests() -> [get_roster,
                   add_contact,
                   roster_push].

%%  WARNING: Side-effects & test interference
%%  subscribe affects subsequent tests
%%  by sending a directed presence before the roster push
%%  in add_sample_contact/2
%%  TODO: investigate, fix.

subscription_tests() -> [unsubscribe,
                         decline_subscription,
                         subscribe].
%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------


init_per_suite(Config) ->

    MongooseMetrics = [{[global, data, xmpp, received, xml_stanza_size], changed},
                       {[global, data, xmpp, sent, xml_stanza_size], changed},
                       {fun roster_odbc_precondition/0, [global, data, odbc, regular],
                        [{recv_oct, '>'}, {send_oct, '>'}]},
                       {[global, backends, mod_roster, get_subscription_lists], changed}
                       ],
    [{mongoose_metrics, MongooseMetrics} | escalus:init_per_suite(Config)].

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

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

get_roster(ConfigIn) ->
    Metrics =
        [{['_', modRosterGets], 1},
         {[global, backends, mod_roster, get_roster], changed}
        ],
    Config = mongoose_metrics(ConfigIn, Metrics),

    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice,_Bob) ->
        escalus_client:send(Alice, escalus_stanza:roster_get()),
        escalus_client:wait_for_stanza(Alice)

        end).

add_contact(ConfigIn) ->
    Config = mongoose_metrics(ConfigIn, [{['_', modRosterSets], 1}]),

    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% add contact
        escalus_client:send(Alice,
                            escalus_stanza:roster_add_contact(Bob,
                                                              [<<"friends">>],
                                                              <<"Bobby">>)),
        Received = escalus_client:wait_for_stanza(Alice),
        escalus_client:send(Alice, escalus_stanza:iq_result(Received)),
        escalus_client:wait_for_stanza(Alice)

        end).

roster_push(ConfigIn) ->
    Config = mongoose_metrics(ConfigIn, [{['_', modRosterSets], 1},
                                         {['_', modRosterPush], 2}]),

    escalus:story(Config, [{alice, 2}, {bob, 1}], fun(Alice1, Alice2, Bob) ->

        %% add contact
        escalus_client:send(Alice1,
                            escalus_stanza:roster_add_contact(Bob,
                                                              [<<"friends">>],
                                                              <<"Bobby">>)),
        Received = escalus_client:wait_for_stanza(Alice1),
        escalus_client:send(Alice1, escalus_stanza:iq_result(Received)),
        escalus_client:wait_for_stanza(Alice1),

        Received2 = escalus_client:wait_for_stanza(Alice2),
        escalus_client:send(Alice2, escalus_stanza:iq_result(Received2))

        end).


subscribe(ConfigIn) ->
    Config = mongoose_metrics(ConfigIn, [{['_', modPresenceSubscriptions], 1}]),

    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice,Bob) ->
        BobJid = escalus_client:short_jid(Bob),
        AliceJid = escalus_client:short_jid(Alice),

        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus_client:send(Alice, escalus_stanza:presence_direct(BobJid, <<"subscribe">>)),
        PushReq = escalus_client:wait_for_stanza(Alice),
        escalus_client:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
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

        %% Bob receives roster push
        escalus_client:wait_for_stanza(Bob)


        end).

decline_subscription(ConfigIn) ->
    Config = mongoose_metrics(ConfigIn, [{['_', modPresenceUnsubscriptions], 1}]),

    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice,Bob) ->
        BobJid = escalus_client:short_jid(Bob),
        AliceJid = escalus_client:short_jid(Alice),

        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus_client:send(Alice, escalus_stanza:presence_direct(BobJid, <<"subscribe">>)),
        PushReq = escalus_client:wait_for_stanza(Alice),
        escalus_client:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        escalus_client:wait_for_stanza(Bob),

        %% Bob refuses subscription
        escalus_client:send(Bob, escalus_stanza:presence_direct(AliceJid, <<"unsubscribed">>)),

        %% Alice receives subscribed
        escalus_client:wait_for_stanzas(Alice, 2)

        end).


unsubscribe(ConfigIn) ->
    Config = mongoose_metrics(ConfigIn, [{['_', modPresenceUnsubscriptions], 1}]),

    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice,Bob) ->
        BobJid = escalus_client:short_jid(Bob),
        AliceJid = escalus_client:short_jid(Alice),

        %% add contact
        add_sample_contact(Alice, Bob),
        %% subscribe
        escalus_client:send(Alice, escalus_stanza:presence_direct(BobJid, <<"subscribe">>)),
        PushReq = escalus_client:wait_for_stanza(Alice),

        escalus_client:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
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

        escalus_client:wait_for_stanzas(Bob, 2)

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
    escalus_ejabberd:rpc(mod_roster_odbc, remove_user, [Username, Server]),
    escalus_ejabberd:rpc(mod_roster, remove_user, [Username, Server]).

mongoose_metrics(ConfigIn, Metrics) ->
    Predefined = proplists:get_value(mongoose_metrics, ConfigIn, []),
    MongooseMetrics = Predefined ++ Metrics,
    [{mongoose_metrics, MongooseMetrics} | ConfigIn].

roster_odbc_precondition() ->
    mod_roster_odbc == escalus_ejabberd:rpc(mod_roster_backend, backend, []).
