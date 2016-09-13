-module(carboncopy_SUITE).


-compile([export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(ae(Expected, Actual), ?assertEqual(Expected, Actual)).

all() -> [{group, all}].


groups() ->
    [{all, [parallel,shuffle],
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
       prop_normal_routing_to_bare_jid
      ]}].

init_per_suite(C) -> escalus:init_per_suite(C).
end_per_suite(C) -> escalus_fresh:clean(), escalus:end_per_suite(C).
init_per_testcase(Name,C) -> escalus:init_per_testcase(Name,C).
end_per_testcase(Name,C) -> escalus:end_per_testcase(Name,C).
run_prop(PropName, Property) ->
    ?ae(true, proper:quickcheck(proper:conjunction([{PropName, Property}]),
                                [verbose,long_result, {numtests, 3}])).

discovering_support(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}],
      fun(Alice) ->
              IqGet = escalus_stanza:disco_info(<<"localhost">>),
              escalus_client:send(Alice, IqGet),
              Result = escalus_client:wait_for_stanza(Alice),
              escalus:assert(is_iq_result, [IqGet], Result),
              escalus:assert(has_feature, [<<"urn:xmpp:carbons:2">>], Result)
      end).

enabling_carbons(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun carbons_get_enabled/1).

disabling_carbons(Config) ->
    escalus:fresh_story(Config, [{alice, 1}],
                  fun(Alice) -> carbons_get_enabled(Alice),
                                carbons_get_disabled(Alice) end).

avoiding_carbons(Config) ->
    escalus:fresh_story(
      Config, [{alice, 2}, {bob, 1}],
      fun(Alice1, Alice2, Bob) ->
              carbons_get_enabled([Alice1,Alice2]),
              Msg = escalus_stanza:chat_without_carbon_to(Bob,
                                                          <<"And pious action">>),
              escalus_client:send(Alice1, Msg),
              escalus:assert(
                is_chat_message, [<<"And pious action">>],
                escalus_client:wait_for_stanza(Bob)),
              escalus_client:wait_for_stanzas(Alice2, 1),
              [] = escalus_client:peek_stanzas(Alice2)
      end).

non_enabled_clients_dont_get_sent_carbons(Config) ->
    escalus:fresh_story(
      Config, [{alice, 2}, {bob, 1}],
      fun(Alice1, Alice2, Bob) ->
              Msg = escalus_stanza:chat_to(Bob, <<"And pious action">>),
              escalus_client:send(Alice1, Msg),
              escalus:assert(
                is_chat_message, [<<"And pious action">>],
                escalus_client:wait_for_stanza(Bob)),
              escalus_client:wait_for_stanzas(Alice2, 1),
              [] = escalus_client:peek_stanzas(Alice2)
      end).

non_enabled_clients_dont_get_received_carbons(Config) ->
    escalus:fresh_story(
      Config, [{alice, 2}, {bob, 1}],
      fun(Alice1, Alice2, Bob) ->
              Msg = escalus_stanza:chat_to(Alice1, <<"And pious action">>),
              escalus_client:send(Bob, Msg),
              escalus:assert(
                is_chat_message, [<<"And pious action">>],
                escalus_client:wait_for_stanza(Alice1)),
              escalus_client:wait_for_stanzas(Alice2, 1),
              [] = escalus_client:peek_stanzas(Alice2)
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
      fun(Alice,Bob) ->
              carbons_get_enabled(Alice),
              [ escalus_client:send(Bob, escalus_stanza:chat_to(Alice, M))
                || M <- BobsMessages ],
              [ escalus:assert(is_chat_message, [M], escalus_client:wait_for_stanza(Alice))
                || M <- BobsMessages ]
      end).

unavailable_resources_dont_get_carbons(Config) ->
    escalus:fresh_story(
      Config, [{alice, 2}, {bob, 1}],
      fun(Alice1, Alice2, Bob) ->
        carbons_get_enabled(Alice1),
        carbons_get_enabled(Alice2),
        client_unsets_presence(Alice1),
        _unavailable = escalus_client:wait_for_stanza(Alice2),

        escalus_client:send(Bob, escalus_stanza:chat_to(Alice2, <<"one">>)),

        client_sets_presence(Alice1),
        %% no carbons for Alice1, only presences
        escalus_new_assert:mix_match([is_presence, is_presence],
                                     escalus:peek_stanzas(Alice1))
      end).

client_unsets_presence(Client) ->
    escalus_client:send(Client, escalus_stanza:presence(<<"unavailable">>)),
    timer:sleep(100).

client_sets_presence(Client) ->
    escalus_client:send(Client, escalus_stanza:presence(<<"available">>)),
    timer:sleep(100).


dropped_client_doesnt_create_duplicate_carbons(Config) ->
    escalus:fresh_story(
      Config, [{alice, 2}, {bob, 1}],
      fun(Alice1, Alice2, Bob) ->
              Msg = escalus_stanza:chat_to(Bob, <<"And pious action">>),
              given_client_logs_out(Alice2),
              _Pres = escalus_client:wait_for_stanza(Alice1),

              escalus_client:send(Alice1, Msg),
              escalus:assert(
                is_chat_message, [<<"And pious action">>],
                escalus_client:wait_for_stanza(Bob)),
              [] = escalus_client:peek_stanzas(Alice1)
      end).

prop_forward_received_chat_messages(Config) ->
    run_prop
      (forward_received,
    ?FORALL({N,Msg}, {no_of_resources(), utterance()},
               true_story
                 (Config, [{alice, 1}, {bob, N}],
                       fun(Users) ->
                               all_bobs_other_resources_get_received_carbons(Users,Msg)
                  end))).

prop_forward_sent_chat_messages(Config) ->
    run_prop
        (forward_sent,
    ?FORALL({N,Msg}, {no_of_resources(),utterance()},
                 true_story
                   (Config, [{alice, 1}, {bob, N}],
                       fun(Users) ->
                               all_bobs_other_resources_get_sent_carbons(Users,Msg)
                    end))).

prop_normal_routing_to_bare_jid(Config) ->
    run_prop
        (normal_routing,
    ?FORALL({N,Msg}, {no_of_resources(),utterance()},
                 true_story
                   (Config, [{alice, 1}, {bob, N}],
                       fun(Users) ->
                               all_bobs_resources_get_message_to_bare_jid(Users,Msg)
                    end))).



%%
%% Test scenarios w/assertions
%%

all_bobs_resources_get_message_to_bare_jid([Alice,Bob1|Bobs], Msg) ->
    %% All connected resources receive messages sent
    %% to the user's bare JID without carbon wrappers.
    carbons_get_enabled([Bob1|Bobs]),
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

all_bobs_other_resources_get_received_carbons([Alice,Bob1|Bobs], Msg) ->
    carbons_get_enabled([Bob1|Bobs]),
    escalus_client:send(Alice, escalus_stanza:chat_to(Bob1, Msg)),
    GotForward = fun(BobsResource) ->
                         escalus:assert(
                           is_forwarded_received_message,
                           [escalus_client:full_jid(Alice),
                            escalus_client:full_jid(Bob1),
                            Msg],
                           escalus_client:wait_for_stanza(BobsResource)),
                         escalus_assert:has_no_stanzas(BobsResource) end,
    lists:foreach(GotForward, Bobs).

all_bobs_other_resources_get_sent_carbons([Alice,Bob1|Bobs], Msg) ->
    carbons_get_enabled([Bob1|Bobs]),
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

carbons_get_disabled(ClientOrClients) ->
    disable_carbons(ClientOrClients).

carbons_get_enabled(ClientOrClients) ->
    enable_carbons(ClientOrClients).


%%
%% Internal helpers
%%

given_client_logs_out(Client) ->
    escalus_client:stop(Client),
    timer:sleep(300).

%% Wrapper around escalus:story. Returns PropEr result.
true_story(Config, UserSpecs, TestFun) ->
    try   escalus:fresh_story(Config, UserSpecs, TestFun), true
    catch E ->
              ct:pal("~p", [E]),
              {error, E}
    end.

%% Number of resources per users
no_of_resources() -> random:uniform(4).

%% A sample chat message
utterance() ->
    proper_types:oneof(
      [<<"Now, fair Hippolyta, our nuptial hour">>,
       <<"Draws on apace; four happy days bring in">>,
       <<"Another moon: but, O, methinks, how slow">>,
       <<"This old moon wanes! she lingers my desires">>,
       <<"Like to a step-dame or a dowager">>,
       <<"Long withering out a young man revenue.">>]).


enable_carbons(Clients) when is_list(Clients) ->
    lists:foreach(fun enable_carbons/1, Clients);
enable_carbons(Client) ->
    IqSet = escalus_stanza:carbons_enable(),
    escalus_client:send(Client, IqSet),
    Result = escalus_client:wait_for_stanza(Client),
    escalus:assert(is_iq, [<<"result">>], Result).


disable_carbons(Clients) when is_list(Clients) ->
    lists:foreach(fun disable_carbons/1, Clients);
disable_carbons(Client) ->
    IqSet = escalus_stanza:carbons_disable(),
    escalus_client:send(Client, IqSet),
    Result = escalus_client:wait_for_stanza(Client),
    escalus:assert(is_iq, [<<"result">>], Result).
