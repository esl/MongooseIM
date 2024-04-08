-module(carboncopy_SUITE).

-compile([export_all, nowarn_export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(AE(Expected, Actual), ?assertEqual(Expected, Actual)).
-define(BODY, <<"And pious action">>).

-import(mongoose_helper, [enable_carbons/1, disable_carbons/1]).
-import(domain_helper, [domain/0]).

all() ->
    [{group, one2one},
     {group, muc}].

groups() ->
    [{one2one, [parallel],
        [discovering_support,
         enabling_carbons,
         disabling_carbons,
         avoiding_carbons,
         non_enabled_clients_dont_get_sent_carbons,
         non_enabled_clients_dont_get_received_carbons,
         enabled_single_resource_doesnt_get_carbons,
         unavailable_resources_dont_get_carbons,
         dropped_client_doesnt_create_duplicate_carbons,
         prop_forward_received_chat_messages,
         prop_forward_sent_chat_messages,
         prop_normal_routing_to_bare_jid,
         chat_message_is_carbon_copied,
         normal_message_with_body_is_carbon_copied,
         normal_message_with_receipt_is_carbon_copied,
         normal_message_with_csn_is_carbon_copied,
         normal_message_with_chat_marker_is_carbon_copied]},
     {muc, [parallel],
        [group_chat_is_not_carbon_copied,
         local_user_to_muc_participant_is_carbon_copied,
         muc_participant_to_local_user_is_not_carbon_copied]}].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(C) ->
    escalus:init_per_suite(C).

end_per_suite(C) ->
    escalus_fresh:clean(),
    escalus:end_per_suite(C).

init_per_group(muc, Config) ->
    muc_helper:load_muc(),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(muc, Config) ->
    muc_helper:unload_muc(),
    Config;
end_per_group(_GroupName, Config) ->
    Config.

%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(Name, C) ->
    escalus:init_per_testcase(Name, C).

end_per_testcase(Name, C) ->
    escalus:end_per_testcase(Name, C).

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================
discovering_support(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}],
      fun(Alice) ->
              IqGet = escalus_stanza:disco_info(domain()),
              escalus_client:send(Alice, IqGet),
              Result = escalus_client:wait_for_stanza(Alice),
              escalus:assert(is_iq_result, [IqGet], Result),
              escalus:assert(has_feature, [<<"urn:xmpp:carbons:2">>], Result)
      end).

enabling_carbons(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun mongoose_helper:enable_carbons/1).

disabling_carbons(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                        fun(Alice) -> enable_carbons(Alice),
                                      disable_carbons(Alice) end).

avoiding_carbons(Config) ->
    escalus:fresh_story(
      Config, [{alice, 2}, {bob, 1}],
      fun(Alice1, Alice2, Bob) ->
              enable_carbons([Alice1, Alice2]),
              Msg = escalus_stanza:chat_without_carbon_to(Bob, ?BODY),
              escalus_client:send(Alice1, Msg),
              BobReceived = escalus_client:wait_for_stanza(Bob),
              escalus:assert(is_chat_message, [?BODY], BobReceived),
              ?assertEqual([], escalus_client:wait_for_stanzas(Alice2, 1, 500)),
              ?assertEqual([], escalus_client:peek_stanzas(Alice2))
      end).

non_enabled_clients_dont_get_sent_carbons(Config) ->
    escalus:fresh_story(
      Config, [{alice, 2}, {bob, 1}],
      fun(Alice1, Alice2, Bob) ->
              Msg = escalus_stanza:chat_to(Bob, ?BODY),
              escalus_client:send(Alice1, Msg),
              BobReceived = escalus_client:wait_for_stanza(Bob),
              escalus:assert(is_chat_message, [?BODY], BobReceived),
              ?assertEqual([], escalus_client:wait_for_stanzas(Alice2, 1, 500)),
              ?assertEqual([], escalus_client:peek_stanzas(Alice2))
      end).

non_enabled_clients_dont_get_received_carbons(Config) ->
    escalus:fresh_story(
      Config, [{alice, 2}, {bob, 1}],
      fun(Alice1, Alice2, Bob) ->
              Msg = escalus_stanza:chat_to(Alice1, ?BODY),
              escalus_client:send(Bob, Msg),
              AliceReceived = escalus_client:wait_for_stanza(Alice1),
              escalus:assert(is_chat_message, [?BODY], AliceReceived),
              ?assertEqual([], escalus_client:wait_for_stanzas(Alice2, 1, 500)),
              ?assertEqual([], escalus_client:peek_stanzas(Alice2))
      end).

enabled_single_resource_doesnt_get_carbons(Config) ->
    BobsMessages = [
                    <<"There's such a thing as dwelling">>,
                    <<"On the thought ourselves have nursed,">>,
                    <<"And with scorn and courage telling">>,
                    <<"The world to do its worst.">>
                   ],
    escalus:fresh_story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              enable_carbons(Alice),
              [ escalus_client:send(Bob, escalus_stanza:chat_to(Alice, M))
                || M <- BobsMessages ],
              [ escalus:assert(is_chat_message, [M], escalus_client:wait_for_stanza(Alice))
                || M <- BobsMessages ]
      end).

unavailable_resources_dont_get_carbons(Config) ->
    escalus:fresh_story(
      Config, [{alice, 2}, {bob, 1}],
      fun(Alice1, Alice2, Bob) ->
          enable_carbons([Alice1, Alice2]),
          client_unsets_presence(Alice1),
          escalus:assert(is_presence_with_type, [<<"unavailable">>],
                         escalus_client:wait_for_stanza(Alice2)),

          escalus_client:send(Bob, escalus_stanza:chat_to(Alice2, ?BODY)),
          %% Alice2 receives the message, no carbons for Alice1
          wait_for_message_with_body(Alice2, ?BODY),
          client_sets_presence(Alice1),
          %% still no carbons for Alice1, only presences
          escalus_new_assert:mix_match([is_presence, is_presence],
                                       escalus:wait_for_stanzas(Alice1, 2)),
          escalus:assert(is_presence, [],
                         escalus_client:wait_for_stanza(Alice2)),
          enable_carbons(Alice1),
          %% Send a message with a different body and wait for it.
          %% If we receive it, we can be sure, that our first message has
          %% been fully processed by the routing pipeline.
          Body2 = <<"carbonated">>,
          escalus_client:send(Bob, escalus_stanza:chat_to(Alice2, Body2)),
          wait_for_message_with_body(Alice2, Body2),
          carboncopy_helper:wait_for_carbon_chat_with_body(Alice1, Body2, #{from => Bob, to => Alice2})
      end).

dropped_client_doesnt_create_duplicate_carbons(Config) ->
    escalus:fresh_story(
      Config, [{alice, 2}, {bob, 1}],
      fun(Alice1, Alice2, Bob) ->
              enable_carbons([Alice1, Alice2]),
              Msg = escalus_stanza:chat_to(Bob, ?BODY),
              escalus_client:stop(Config, Alice2),
              escalus:assert(is_presence_with_type, [<<"unavailable">>],
                             escalus_client:wait_for_stanza(Alice1)),
              escalus_client:send(Alice1, Msg),
              escalus:assert(is_chat_message, [?BODY],
                             escalus_client:wait_for_stanza(Bob)),
              [] = escalus_client:peek_stanzas(Alice1)
      end).

prop_forward_received_chat_messages(Config) ->
    run_prop
      (forward_received,
    ?FORALL({N, Msg}, {no_of_resources(), utterance()},
               true_story
                 (Config, [{alice, 1}, {bob, N}],
                       fun(Users) ->
                           all_bobs_other_resources_get_received_carbons(Users,
                                                                         Msg)
                  end))).

prop_forward_sent_chat_messages(Config) ->
    run_prop
        (forward_sent,
    ?FORALL({N, Msg}, {no_of_resources(), utterance()},
                 true_story
                   (Config, [{alice, 1}, {bob, N}],
                       fun(Users) ->
                               all_bobs_other_resources_get_sent_carbons(Users,
                                                                         Msg)
                    end))).

prop_normal_routing_to_bare_jid(Config) ->
    run_prop
        (normal_routing,
    ?FORALL({N, Msg}, {no_of_resources(), utterance()},
                 true_story
                   (Config, [{alice, 1}, {bob, N}],
                       fun(Users) ->
                               all_bobs_resources_get_message_to_bare_jid(Users,
                                                                          Msg)
                    end))).

chat_message_is_carbon_copied(Config) ->
    message_is_carbon_copied(Config, fun carboncopy_helper:chat_message_with_body/1).

normal_message_with_body_is_carbon_copied(Config) ->
    message_is_carbon_copied(Config, fun carboncopy_helper:normal_message_with_body/1).

normal_message_with_receipt_is_carbon_copied(Config) ->
    message_is_carbon_copied(Config, fun carboncopy_helper:normal_message_with_receipt/1).

normal_message_with_csn_is_carbon_copied(Config) ->
    message_is_carbon_copied(Config, fun carboncopy_helper:normal_message_with_csn/1).

normal_message_with_chat_marker_is_carbon_copied(Config) ->
    message_is_carbon_copied(Config, fun carboncopy_helper:normal_message_with_chat_marker/1).

message_is_carbon_copied(Config, StanzaFun) ->
    escalus:fresh_story(
        Config, [{alice, 2}, {bob, 1}],
        fun(Alice1, Alice2, Bob) ->
            enable_carbons([Alice1, Alice2]),
            Body = <<"carbonated">>,
            escalus_client:send(Bob, StanzaFun(#{to => Alice2, body => Body})),
            AliceReceived = escalus_client:wait_for_stanza(Alice2),
            escalus:assert(is_message, AliceReceived),
            carboncopy_helper:wait_for_carbon_message(Alice1, #{from => Bob, to => Alice2})
        end).

group_chat_is_not_carbon_copied(Config) ->
    escalus:fresh_story(Config, [{alice, 2}, {bob, 1}],
        fun(Alice1, Alice2, Bob) ->
            enable_carbons([Alice1, Alice2]),
            RoomCfg = muc_helper:start_fresh_room(Config, inbox_helper:extract_user_specs(Bob), <<"some_friendly_name">>, default),
            muc_helper:enter_room(RoomCfg, [{Alice1, <<"cool_alice">>}, {Bob, <<"cool_bob">>}]),

            Msg = <<"Hi Room!">>,
            muc_helper:send_to_room(RoomCfg, Bob, Msg),
            muc_helper:verify_message_received(RoomCfg, [Alice1, Bob], <<"cool_bob">>, Msg),
            ?assertEqual([], escalus_client:peek_stanzas(Alice2))
      end).

local_user_to_muc_participant_is_carbon_copied(Config) ->
    escalus:fresh_story(Config, [{alice, 2}, {bob, 1}],
        fun(Alice1, Alice2, Bob) ->
            enable_carbons([Alice1, Alice2]),
            RoomCfg = muc_helper:start_fresh_room(Config, inbox_helper:extract_user_specs(Bob), <<"some_friendly_name">>, default),
            muc_helper:enter_room(RoomCfg, [{Alice1, <<"cool_alice">>}, {Bob, <<"cool_bob">>}]),
            RoomJid = proplists:get_value(room_jid, RoomCfg),
            Body = <<"Hello!">>,
            Stanza = escalus_stanza:chat_to(<<RoomJid/binary, "/cool_alice">>, Body),

            escalus:send(Bob, Stanza),
            escalus:wait_for_stanza(Alice1),
            carboncopy_helper:wait_for_carbon_chat_with_body(Alice2, Body, #{from => <<RoomJid/binary, "/cool_bob">>, to => Alice1})
      end).

muc_participant_to_local_user_is_not_carbon_copied(Config) ->
    escalus:fresh_story(Config, [{alice, 2}, {bob, 1}],
        fun(Alice1, Alice2, Bob) ->
            enable_carbons([Alice1, Alice2]),
            RoomCfg = muc_helper:start_fresh_room(Config, inbox_helper:extract_user_specs(Bob), <<"some_friendly_name">>, default),
            muc_helper:enter_room(RoomCfg, [{Alice1, <<"cool_alice">>}, {Bob, <<"cool_bob">>}]),
            RoomJid = proplists:get_value(room_jid, RoomCfg),
            Body = <<"Hello!">>,
            Stanza = escalus_stanza:chat(<<RoomJid/binary, "/cool_bob">>, Alice1, Body),

            escalus:send(Bob, Stanza),
            escalus:wait_for_stanza(Alice1),
            ?assertEqual([], escalus_client:peek_stanzas(Alice2))
      end).

%%
%% Test scenarios w/assertions
%%

all_bobs_resources_get_message_to_bare_jid([Alice, Bob1 | Bobs], Msg) ->
    %% All connected resources receive messages sent
    %% to the user's bare JID without carbon wrappers.
    enable_carbons([Bob1|Bobs]),
    escalus_client:send(
      Alice, escalus_stanza:chat_to(escalus_client:short_jid(Bob1), Msg)),
    GotMsg = fun(BobsResource) ->
                     escalus:assert(
                       is_chat_message,
                       [Msg],
                       escalus_client:wait_for_stanza(BobsResource)),
                     escalus_assert:has_no_stanzas(BobsResource)
             end,
    lists:foreach(GotMsg, [Bob1|Bobs]).

all_bobs_other_resources_get_received_carbons([Alice, Bob1 | Bobs], Msg) ->
    enable_carbons([Bob1|Bobs]),
    escalus_client:send(Alice, escalus_stanza:chat_to(Bob1, Msg)),
    escalus_client:wait_for_stanza(Bob1),
    GotForward = fun(BobsResource) ->
                         escalus:assert(
                           is_forwarded_received_message,
                           [escalus_client:full_jid(Alice),
                            escalus_client:full_jid(Bob1),
                            Msg],
                           escalus_client:wait_for_stanza(BobsResource)),
                         escalus_assert:has_no_stanzas(BobsResource) end,
    lists:foreach(GotForward, Bobs).

all_bobs_other_resources_get_sent_carbons([Alice, Bob1 | Bobs], Msg) ->
    enable_carbons([Bob1|Bobs]),
    escalus_client:send(Bob1, escalus_stanza:chat_to(Alice, Msg)),
    escalus:assert(is_chat_message, [Msg], escalus_client:wait_for_stanza(Alice)),
    GotCarbon = fun(BobsResource) ->
                        escalus:assert(
                          is_forwarded_sent_message,
                          [escalus_client:full_jid(Bob1),
                           escalus_client:full_jid(Alice),
                           Msg],
                          escalus_client:wait_for_stanza(BobsResource)),
                        escalus_assert:has_no_stanzas(BobsResource) end,
    lists:foreach(GotCarbon, Bobs).

%%
%% Internal helpers
%%

%% Wrapper around escalus:story. Returns PropEr result.
true_story(Config, UserSpecs, TestFun) ->
    try escalus_fresh:story_with_client_list(Config, UserSpecs, TestFun), true
    catch E ->
              {error, E}
    end.

%% Number of resources per users
no_of_resources() -> 1 + rand:uniform(4).

%% A sample chat message
utterance() ->
    proper_types:oneof(
      [<<"Now, fair Hippolyta, our nuptial hour">>,
       <<"Draws on apace; four happy days bring in">>,
       <<"Another moon: but, O, methinks, how slow">>,
       <<"This old moon wanes! she lingers my desires">>,
       <<"Like to a step-dame or a dowager">>,
       <<"Long withering out a young man revenue.">>]).


client_unsets_presence(Client) ->
    escalus_client:send(Client, escalus_stanza:presence(<<"unavailable">>)).

client_sets_presence(Client) ->
    escalus_client:send(Client, escalus_stanza:presence(<<"available">>)).

run_prop(PropName, Property) ->
    ?AE(true, proper:quickcheck(proper:conjunction([{PropName, Property}]),
                                [verbose, long_result, {numtests, 3}])).

wait_for_message_with_body(Alice, Body) ->
    AliceReceived = escalus_client:wait_for_stanza(Alice),
    escalus:assert(is_chat_message, [Body], AliceReceived).
