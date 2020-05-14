%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
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

-module(presence_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, presence},
     {group, presence_priority},
     {group, roster},
     {group, roster_versioning},
     {group, subscribe_group}].

groups() ->
    G = [{presence, [sequence], [available,
                                 available_direct,
                                 available_direct_then_unavailable,
                                 available_direct_then_disconnect,
                                 additions,
                                 invisible_presence]},
         {presence_priority, [sequence], [negative_priority_presence]},
         {roster, [sequence], [get_roster,
                               add_contact,
                               remove_contact]},
         {roster_versioning, [sequence], [versioning,
                                          versioning_no_store]},
         {subscribe_group, [sequence], [subscribe,
                                        subscribe_decline,
                                        subscribe_relog,
                                        subscribe_preserves_extra_info,
                                        unsubscribe,
                                        remove_unsubscribe]}],
    ct_helper:repeat_all_until_all_ok(G).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(Config).

init_per_group(_GroupName, Config) ->
    escalus:create_users(Config, escalus:get_users([alice, bob])).

end_per_group(_GroupName, Config) ->
    escalus:delete_users(Config, escalus:get_users([alice, bob])).

init_per_testcase(versioning, ConfigIn) ->
    Config = set_versioning(true, true, ConfigIn),
    escalus:init_per_testcase(versioning, Config);
init_per_testcase(versioning_no_store, ConfigIn) ->
    Config = set_versioning(true, false, ConfigIn),
    escalus:init_per_testcase(versioning_no_store, Config);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(add_contact, Config) ->
    [{_, UserSpec} | _] = escalus_config:get_config(escalus_users, Config),
    remove_roster(Config, UserSpec),
    escalus:end_per_testcase(add_contact, Config);
end_per_testcase(subscribe, Config) ->
    end_rosters_remove(Config);
end_per_testcase(subscribe_decline, Config) ->
    end_rosters_remove(Config);
end_per_testcase(unsubscribe, Config) ->
    end_rosters_remove(Config);
end_per_testcase(VersionCases, Config)
      when VersionCases =:= versioning; VersionCases =:= versioning_no_store ->
    restore_versioning(Config),
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

available(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice,_Bob) ->

        escalus:send(Alice, escalus_stanza:presence(<<"available">>)),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice))

        end).

available_direct(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice,Bob) ->

        BobJid = escalus_users:get_jid(Config, bob),
        escalus:send(Alice, escalus_stanza:presence_direct(BobJid, <<"available">>)),
        Received = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence, Received),
        escalus_assert:is_stanza_from(Alice, Received)

        end).

available_direct_then_unavailable(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice,Bob) ->
        %% given Alice has sent direct presence to Bob
        escalus:send(Alice, escalus_stanza:presence_direct(Bob, <<"available">>)),
        Received1 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence, Received1),
        escalus_assert:is_stanza_from(Alice, Received1),

        %% when Alice sends presence unavailable
        escalus:send(Alice, escalus_stanza:presence(<<"unavailable">>)),

        %% then Bob receives presence unavailable
        Received2 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence, Received2),
        escalus_assert:is_stanza_from(Alice, Received2)
        end).

available_direct_then_disconnect(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice,Bob) ->
        %% given Alice has sent direct presence to Bob
        escalus:send(Alice, escalus_stanza:presence_direct(Bob, <<"available">>)),
        Received1 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence, Received1),
        escalus_assert:is_stanza_from(Alice, Received1),

        %% when Alice suddenly disconnects
        escalus_client:kill_connection(Config, Alice),

        %% then Bob receives presence unavailable
        Received2 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence, Received2),
        escalus_assert:is_stanza_from(Alice, Received2)
        end).

additions(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice,Bob) ->

        Tags = escalus_stanza:tags([
            {<<"show">>, <<"dnd">>},
            {<<"priority">>, <<"1">>},
            {<<"status">>, <<"Short break">>}
        ]),
        BobJid = escalus_users:get_jid(Config, bob),
        Presence = escalus_stanza:presence_direct(BobJid, <<"available">>, Tags),
        escalus:send(Alice, Presence),

        Received = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence, Received),
        escalus:assert(is_presence_with_show, [<<"dnd">>], Received),
        escalus:assert(is_presence_with_status, [<<"Short break">>], Received),
        escalus:assert(is_presence_with_priority, [<<"1">>], Received)

        end).

negative_priority_presence(Config) ->
    escalus:story(Config, [{alice, 2}, {bob, 1}], fun(Alice1, Alice2, Bob) ->

        %% Alice1 updates presense priority
        Tags = escalus_stanza:tags([
            {<<"priority">>, <<"-10">>}
        ]),
        Presence = escalus_stanza:presence(<<"available">>, Tags),
        escalus:send(Alice1, Presence),

        Received1 = escalus:wait_for_stanza(Alice1),
        Received2 = escalus:wait_for_stanza(Alice2),
        escalus:assert(is_presence, Received1),
        escalus:assert(is_presence, Received2),
        escalus:assert(is_presence_with_priority, [<<"-10">>], Received1),
        escalus:assert(is_presence_with_priority, [<<"-10">>], Received2),

        %% Bob sends to the Alice's bare JID.
        escalus:send(Bob, escalus_stanza:chat_to_short_jid(Alice1, <<"Hi.">>)),

        %% If priority is negative, than the client does not want to receive
        %% any messages.
        timer:sleep(1000),
        escalus_assert:has_no_stanzas(Alice1)

        end).

invisible_presence(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice,Bob) ->
        BobJid = escalus_users:get_jid(Config, bob),
        AliceJid = escalus_users:get_jid(Config, alice),

        %% Alice adds Bob as a contact
        add_sample_contact(Alice, Bob),

        %% She subscribes to his presences
        escalus:send(Alice, escalus_stanza:presence_direct(BobJid, <<"subscribe">>)),
        PushReq = escalus:wait_for_stanza(Alice),
        escalus:assert(is_roster_set, PushReq),
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        Received = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence_with_type, [<<"subscribe">>], Received),

        %% Bob adds new contact to his roster
        escalus:send(Bob, escalus_stanza:roster_add_contact(Alice,
                                                            [<<"enemies">>],
                                                             <<"Alice">>)),
        PushReqB = escalus:wait_for_stanza(Bob),
        escalus:assert(is_roster_set, PushReqB),
        escalus:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

        %% Bob sends subscribed presence
        escalus:send(Bob, escalus_stanza:presence_direct(AliceJid, <<"subscribed">>)),

        %% Alice receives subscribed
        Stanzas = escalus:wait_for_stanzas(Alice, 2),

        check_subscription_stanzas(Stanzas, <<"subscribed">>),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob receives roster push
        PushReqB1 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_roster_set, PushReqB1),

        %% Bob sends presence
        escalus:send(Bob, escalus_stanza:presence(<<"available">>)),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob becomes invisible
        escalus:send(Bob, escalus_stanza:presence(<<"invisible">>)),

        escalus:assert(is_presence_with_type, [<<"unavailable">>],
                       escalus:wait_for_stanza(Alice)),

        %% Return everything back
        escalus:send(Bob, escalus_stanza:presence(<<"available">>)),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice))

        end).

get_roster(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice,_Bob) ->
        escalus:send(Alice, escalus_stanza:roster_get()),
        escalus_assert:is_roster_result(escalus:wait_for_stanza(Alice))

        end).

add_contact(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% add contact
        Stanza = escalus_stanza:roster_add_contact(Bob, bobs_default_groups(),
                                                   bobs_default_name()),
        escalus:send(Alice, Stanza),
        Received = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([is_roster_set, is_iq_result], Received),

        Result = hd([R || R <- Received, escalus_pred:is_roster_set(R)]),
        escalus:assert(count_roster_items, [1], Result),
        escalus:send(Alice, escalus_stanza:iq_result(Result)),

        %% check roster
        escalus:send(Alice, escalus_stanza:roster_get()),
        Received2 = escalus:wait_for_stanza(Alice),

        escalus:assert(is_roster_result, Received2),
        BobJid = escalus_users:get_jid(Config, bob),
        escalus:assert(roster_contains, [BobJid], Received2)

        end).

remove_contact(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        %% add contact
        add_sample_contact(Alice, Bob),

        %% check roster
        escalus:send(Alice, escalus_stanza:roster_get()),
        escalus:assert(count_roster_items, [1], escalus:wait_for_stanza(Alice)),

        %% remove contact
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Bob)),
        IsSubscriptionRemove = fun(El) ->
            Sub = exml_query:paths(El, [{element, <<"query">>},
                                  {element, <<"item">>},
                                  {attr, <<"subscription">>}]),
            Sub == [<<"remove">>]
            end,
        escalus:assert_many([IsSubscriptionRemove, is_iq_result],
                            escalus:wait_for_stanzas(Alice, 2)),

        %% check roster
        escalus:send(Alice, escalus_stanza:roster_get()),
        escalus:assert(count_roster_items, [0], escalus:wait_for_stanza(Alice))

    end).

versioning(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->

        escalus:send(Alice, escalus_stanza:roster_get(<<"">>)),
        RosterResult = escalus:wait_for_stanza(Alice),

        escalus_assert:is_roster_result(RosterResult),
        Ver = exml_query:path(RosterResult, [{element, <<"query">>}, {attr, <<"ver">>}]),

        true = Ver /= undefined,

        %% add contact
        Stanza = escalus_stanza:roster_add_contact(Bob, bobs_default_groups(),
                                                   bobs_default_name()),
        escalus:send(Alice, Stanza),
        Received = escalus:wait_for_stanzas(Alice, 2),

        escalus:assert_many([is_roster_set, is_iq_result], Received),

        RosterSet = hd(Received),

        Ver2 = exml_query:path(RosterSet, [{element, <<"query">>}, {attr, <<"ver">>}]),

        true = Ver2 /= undefined,

        Result = hd([R || R <- Received, escalus_pred:is_roster_set(R)]),
        escalus:assert(count_roster_items, [1], Result),
        escalus:send(Alice, escalus_stanza:iq_result(Result)),

        %% check roster, send old ver
        escalus:send(Alice, escalus_stanza:roster_get(Ver)),
        Received2 = escalus:wait_for_stanza(Alice),

        escalus:assert(is_roster_result, Received2),
        BobJid = escalus_users:get_jid(Config, bob),
        escalus:assert(roster_contains, [BobJid], Received2),

        %% check version

        Ver2 = exml_query:path(Received2, [{element, <<"query">>}, {attr, <<"ver">>}]),

        %% check roster, send correct Ver

        escalus:send(Alice, escalus_stanza:roster_get(Ver2)),
        Received3 = escalus:wait_for_stanza(Alice),

        escalus:assert(is_iq_result, Received3),

        %% There is no content as version matches

        undefined = exml_query:path(Received3, [{element, <<"query">>}])

    end).

versioning_no_store(Config) ->
    versioning(Config).

subscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        BobJid = escalus_users:get_jid(Config, bob),
        AliceJid = escalus_users:get_jid(Config, alice),

        %% Alice adds Bob as a contact
        add_sample_contact(Alice, Bob),

        %% She subscribes to his presences
        escalus:send(Alice, escalus_stanza:presence_direct(BobJid,
                     <<"subscribe">>)),
        PushReq = escalus:wait_for_stanza(Alice),
        escalus:assert(is_roster_set, PushReq),
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        Received = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence_with_type, [<<"subscribe">>], Received),

        check_roster_count(Bob, 0), % she is in his roster but invisible
        % because she is {none, in}

        %% Bob adds new contact to his roster
        escalus:send(Bob, escalus_stanza:roster_add_contact(Alice,
                                                            [<<"enemies">>],
                                                             <<"Alice">>)),
        PushReqB = escalus:wait_for_stanza(Bob),
        escalus:assert(is_roster_set, PushReqB),
        escalus:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

        %% Bob sends subscribed presence
        escalus:send(Bob, escalus_stanza:presence_direct(AliceJid,
                     <<"subscribed">>)),

        %% Alice receives subscribed
        Stanzas = escalus:wait_for_stanzas(Alice, 2),

        check_subscription_stanzas(Stanzas, <<"subscribed">>),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob receives roster push
        PushReqB1 = escalus:wait_for_stanza(Bob),
        escalus:assert(is_roster_set, PushReqB1),

        check_roster_count(Bob, 1),
        %% Bob sends presence
        escalus:send(Bob, escalus_stanza:presence(<<"available">>)),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob sends presence
        escalus:send(Bob, escalus_stanza:presence(<<"unavailable">>)),
        escalus:assert(is_presence_with_type, [<<"unavailable">>],
                       escalus:wait_for_stanza(Alice))

        end).

subscribe_decline(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice,Bob) ->
        BobJid = escalus_users:get_jid(Config, bob),
        AliceJid = escalus_users:get_jid(Config, alice),

        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus:send(Alice, escalus_stanza:presence_direct(BobJid, <<"subscribe">>)),
        PushReq = escalus:wait_for_stanza(Alice),
        escalus_assert:is_roster_set(PushReq),
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        Received = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence_with_type, [<<"subscribe">>], Received),

        %% Bob refuses subscription
        escalus:send(Bob, escalus_stanza:presence_direct(AliceJid, <<"unsubscribed">>)),

        %% Alice receives subscribed
        Stanzas = escalus:wait_for_stanzas(Alice, 2),

        check_subscription_stanzas(Stanzas, <<"unsubscribed">>)

    end).

subscribe_relog(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        BobJid = escalus_users:get_jid(Config, bob),
        AliceJid = escalus_users:get_jid(Config, alice),

        %% Alice adds Bob as a contact
        add_sample_contact(Alice, Bob),

        %% She subscribes to his presences
        escalus:send(Alice, escalus_stanza:presence_direct(BobJid, <<"subscribe">>)),
        PushReq = escalus:wait_for_stanza(Alice),
        escalus:assert(is_roster_set, PushReq),
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        Received = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence_with_type, [<<"subscribe">>], Received),

        %% New Bob resource connects, should receive subscription request again
        {ok, NewBob} = escalus_client:start_for(Config, bob, <<"newbob">>),
        escalus:send(NewBob,
            escalus_stanza:presence(<<"available">>)),

        escalus:assert(is_presence_with_type, [<<"available">>],
                       escalus:wait_for_stanza(Bob)),

        Stanzas = escalus:wait_for_stanzas(NewBob, 3),
        3 = length(Stanzas),

        escalus_new_assert:mix_match([
                fun(S) ->
                    escalus_pred:is_presence_with_type(<<"available">>, S)
                    andalso escalus_pred:is_stanza_from(Bob, S)
                end,
                fun(S) ->
                    escalus_pred:is_presence_with_type(<<"available">>, S)
                    andalso escalus_pred:is_stanza_from(NewBob, S)
                end,
                fun(S) ->
                    escalus_pred:is_presence_with_type(<<"subscribe">>, S)
                    andalso escalus_pred:is_stanza_from(AliceJid, S)
                end
            ], Stanzas),

        escalus_client:stop(Config, NewBob),

        escalus:send(Bob, escalus_stanza:presence_direct(AliceJid, <<"unsubscribed">>))

        end).

%% This test verifies that a subscription request doesn't remove nickname of a contact
%% and doesn't remove them from a group.
subscribe_preserves_extra_info(Config) ->
    escalus_fresh:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        %% Alice adds Bob as a contact
        add_sample_contact(Alice, Bob),

        %% Subscription without confirmation to prevent unavailable presence exchange
        %% after the test; TODO: escalus_story should ignore them automatically
        BobJid = escalus_client:short_jid(Bob),
        escalus:send(Alice, escalus_stanza:presence_direct(BobJid, <<"subscribe">>)),
        PushReq = escalus:wait_for_stanza(Alice), % Roster set
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),
        Received = escalus:wait_for_stanza(Bob),
        escalus:assert(is_presence_with_type, [<<"subscribe">>], Received),
        
        %% Alice gets current roster
        escalus:send(Alice, escalus_stanza:roster_get()),
        RosterResult = escalus:wait_for_stanza(Alice),
        escalus_assert:is_roster_result(RosterResult),

        %% Actual verification
        [BobItem] = exml_query:paths(RosterResult, [{element, <<"query">>}, {element, <<"item">>}]),
        
        ValidName = bobs_default_name(),
        ValidGroups = lists:sort(bobs_default_groups()),

        {ValidName, ValidGroups}
        = {exml_query:attr(BobItem, <<"name">>),
           lists:sort(exml_query:paths(BobItem, [{element, <<"group">>}, cdata]))}
        end).

unsubscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice,Bob) ->
        BobJid = escalus_users:get_jid(Config, bob),
        AliceJid = escalus_users:get_jid(Config, alice),

        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus:send(Alice, escalus_stanza:presence_direct(BobJid, <<"subscribe">>)),
        PushReq = escalus:wait_for_stanza(Alice),
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        escalus:assert(is_presence_with_type, [<<"subscribe">>],
                       escalus:wait_for_stanza(Bob)),
        %% Bob adds new contact to his roster
        escalus:send(Bob, escalus_stanza:roster_add_contact(Alice,
                                                            [<<"enemies">>],
                                                             <<"Alice">>)),
        PushReqB = escalus:wait_for_stanza(Bob),
        escalus:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

        %% Bob sends subscribed presence
        escalus:send(Bob, escalus_stanza:presence_direct(AliceJid, <<"subscribed">>)),

        %% Alice receives subscribed
        Stanzas = escalus:wait_for_stanzas(Alice, 2),

        check_subscription_stanzas(Stanzas, <<"subscribed">>),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob receives roster push
        PushReqB1 = escalus:wait_for_stanza(Bob),
        escalus_assert:is_roster_set(PushReqB1),

        %% Alice sends unsubscribe
        escalus:send(Alice, escalus_stanza:presence_direct(BobJid, <<"unsubscribe">>)),

        PushReqA2 = escalus:wait_for_stanza(Alice),
        escalus_assert:is_roster_set(PushReqA2),
        escalus:send(Alice, escalus_stanza:iq_result(PushReqA2)),

        %% Bob receives unsubscribe

        StanzasB = escalus:wait_for_stanzas(Bob, 2),

        check_subscription_stanzas(StanzasB, <<"unsubscribe">>),

        %% Alice receives unsubscribed
        escalus:assert(is_presence_with_type, [<<"unavailable">>],
                       escalus:wait_for_stanza(Alice))
    end).

remove_unsubscribe(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice,Bob) ->
        BobJid = escalus_users:get_jid(Config, bob),
        AliceJid = escalus_users:get_jid(Config, alice),

        %% add contact
        add_sample_contact(Alice, Bob),

        %% subscribe
        escalus:send(Alice, escalus_stanza:presence_direct(BobJid, <<"subscribe">>)),
        PushReq = escalus:wait_for_stanza(Alice),
        escalus:send(Alice, escalus_stanza:iq_result(PushReq)),

        %% Bob receives subscription reqest
        escalus:assert(is_presence_with_type, [<<"subscribe">>],
                       escalus:wait_for_stanza(Bob)),
        %% Bob adds new contact to his roster
        escalus:send(Bob, escalus_stanza:roster_add_contact(Alice,
                                                            [<<"enemies">>],
                                                            <<"Alice">>)),
        PushReqB = escalus:wait_for_stanza(Bob),
        escalus:send(Bob, escalus_stanza:iq_result(PushReqB)),
        escalus:assert(is_iq_result, escalus:wait_for_stanza(Bob)),

        %% Bob sends subscribed presence
        escalus:send(Bob, escalus_stanza:presence_direct(AliceJid, <<"subscribed">>)),

        %% Alice receives subscribed
        Stanzas = [escalus:wait_for_stanza(Alice),
                   escalus:wait_for_stanza(Alice)],

        check_subscription_stanzas(Stanzas, <<"subscribed">>),
        escalus:assert(is_presence, escalus:wait_for_stanza(Alice)),

        %% Bob receives roster push
        PushReqB1 = escalus:wait_for_stanza(Bob),
        escalus_assert:is_roster_set(PushReqB1),

        %% remove contact
        escalus:send(Alice, escalus_stanza:roster_remove_contact(Bob)),

        IsPresUnavailable =
                fun(S) ->
                    escalus_pred:is_presence_with_type(<<"unavailable">>, S)
                end,
        escalus:assert_many([is_roster_set, is_iq_result, IsPresUnavailable],
                            escalus:wait_for_stanzas(Alice, 3)),
        check_subscription_stanzas(escalus:wait_for_stanzas(Bob, 2),
                                   <<"unsubscribe">>)

    end).


%%-----------------------------------------------------------------
%% Helpers
%%-----------------------------------------------------------------

bobs_default_groups() -> [<<"friends">>].

bobs_default_name() -> <<"Bobby">>.

add_sample_contact(Alice, Bob) ->
    escalus:send(Alice, escalus_stanza:roster_add_contact(Bob,
                                                          bobs_default_groups(),
                                                          bobs_default_name())),

    Received = escalus:wait_for_stanzas(Alice, 2),
    escalus:assert_many([is_roster_set, is_iq_result], Received),

    Result = hd([R || R <- Received, escalus_pred:is_roster_set(R)]),
    escalus:assert(count_roster_items, [1], Result),
    escalus:send(Alice, escalus_stanza:iq_result(Result)).

check_subscription_stanzas(Stanzas, Type) ->
    IsPresWithType = fun(S) ->
                         escalus_pred:is_presence_with_type(Type, S)
                     end,
    escalus:assert_many([is_roster_set, IsPresWithType], Stanzas).

remove_roster(Config, UserSpec) ->
    [Username, Server, _Pass] = [escalus_ejabberd:unify_str_arg(Item) ||
                                 Item <- escalus_users:get_usp(Config, UserSpec)],
    Mods = rpc(mim(), gen_mod, loaded_modules, [Server]),
    case lists:member(mod_roster, Mods) of
        true ->
            rpc(mim(), mod_roster, remove_user, [Username, Server]);
        false ->
            case lists:member(mod_roster_rdbms, Mods) of
                true ->
                    rpc(mim(), mod_roster_rdbms, remove_user, [Username, Server]);
                false ->
                    throw(roster_not_loaded)
            end
    end.

set_versioning(Versioning, VersionStore, Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    RosterVersioning = rpc(mim(), gen_mod, get_module_opt,
                           [Host, mod_roster, versioning, false]),
    RosterVersionOnDb = rpc(mim(), gen_mod, get_module_opt,
                            [Host, mod_roster, store_current_id, false]),
    rpc(mim(), gen_mod, set_module_opt, [Host, mod_roster, versioning, Versioning]),
    rpc(mim(), gen_mod, set_module_opt, [Host, mod_roster, store_current_id, VersionStore]),
    [{versioning, RosterVersioning},
     {store_current_id, RosterVersionOnDb} | Config].

restore_versioning(Config) ->
    Host = ct:get_config({hosts, mim, domain}),
    RosterVersioning = proplists:get_value(versioning, Config),
    RosterVersionOnDb = proplists:get_value(store_current_id, Config),
    rpc(mim(), gen_mod, get_module_opt, [Host, mod_roster, versioning, RosterVersioning]),
    rpc(mim(), gen_mod, get_module_opt, [Host, mod_roster, store_current_id, RosterVersionOnDb]).


check_roster_count(User, ExpectedCount) ->
    % the user sends get_roster iq
    escalus_client:send(User, escalus_stanza:roster_get()),
    Roster = escalus_client:wait_for_stanza(User),
    ct:pal("Roster: ~p", [Roster]),
    % Roster contains all created users excluding user
    escalus:assert(is_roster_result, Roster),
    escalus:assert(count_roster_items, [ExpectedCount], Roster).

