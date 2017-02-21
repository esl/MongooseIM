-module(amp_big_SUITE).
%% @doc Tests for XEP-0079 Advanced Message Processing support
%% @reference <a href="http://xmpp.org/extensions/xep-0079.html">XEP-0079</a>
%% @author <simon.zelazny@erlang-solutions.com>
%% @copyright 2014 Erlang Solutions, Ltd.
%% This work was sponsored by Grindr.com

-compile([export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

all() -> [{group, Group} || Group <- enabled_group_names()].

enabled_group_names() ->
    [basic, offline] ++
    case mongoose_helper:is_odbc_enabled(domain()) of
        true -> [mam];
        false -> []
    end.

groups() ->
    [{basic, [parallel], [{group, G} || G <- subgroup_names()] ++ basic_test_cases()},
     {mam, [], [{group, mam_success},
                {group, mam_failure}]},
     {mam_success, [], [{group, G} || G <- subgroup_names()]},
     {mam_failure, [], [{group, G} || G <- subgroup_names()]},
     {offline, [], [{group, offline_success},
                    {group, offline_failure}]},
     {offline_success, [], [{group, G} || G <- subgroup_names()]},
     {offline_failure, [], [{group, G} || G <- subgroup_names()]}
    ] ++
        [{G, [parallel, shuffle], notify_deliver_test_cases()}
         || G <- notify_deliver_group_names()] ++
        [{G, [parallel, shuffle], error_deliver_test_cases()}
         || G <- error_deliver_group_names()] ++
        [{G, [parallel, shuffle], drop_deliver_test_cases()}
         || G <- drop_deliver_group_names()].

subgroup_names() -> notify_deliver_group_names() ++
                        error_deliver_group_names() ++
                        drop_deliver_group_names().

notify_deliver_group_names() ->
    [notify_deliver_none,
     notify_deliver_direct,
     notify_deliver_stored,
     notify_deliver_none_direct_stored].

error_deliver_group_names() ->
    [error_deliver_none,
     error_deliver_direct,
     error_deliver_stored,
     error_deliver_none_direct_stored].

drop_deliver_group_names() ->
    [drop_deliver_none,
     drop_deliver_direct,
     drop_deliver_stored,
     drop_deliver_none_direct_stored].

basic_test_cases() ->
    [initial_service_discovery_test,
     actions_and_conditions_discovery_test,

     unsupported_actions_test,
     unsupported_conditions_test,
     unacceptable_rules_test,

     notify_match_resource_any_test,
     notify_match_resource_exact_test,
     notify_match_resource_other_test,
     notify_match_resource_other_bare_test,

     last_rule_applies_test].

notify_deliver_test_cases() ->
    [notify_deliver_to_online_user_test,
     notify_deliver_to_online_user_bare_jid_test,
     notify_deliver_to_online_user_recipient_privacy_test,
     notify_deliver_to_offline_user_test,
     notify_deliver_to_offline_user_recipient_privacy_test,
     notify_deliver_to_stranger_test,
     notify_deliver_to_malformed_jid_test].

error_deliver_test_cases() ->
    [error_deliver_to_online_user_test,
     error_deliver_to_offline_user_test,
     error_deliver_to_stranger_test].

drop_deliver_test_cases() ->
    [drop_deliver_to_online_user_test,
     drop_deliver_to_offline_user_test,
     drop_deliver_to_stranger_test].

init_per_suite(C) ->
    escalus_ejabberd:rpc(ejabberd_config, add_local_option,
                         [outgoing_s2s_options, {[ipv4, ipv6], 1000}]),
    escalus:init_per_suite(C).
end_per_suite(C) ->
    escalus_ejabberd:rpc(ejabberd_config, del_local_option,
                         [outgoing_s2s_options]),

    escalus_fresh:clean(),
    escalus:end_per_suite(C).

init_per_group(GroupName, Config) ->
    ConfigWithModules = dynamic_modules:save_modules(domain(), Config),
    ConfigWithRules = setup_rules(GroupName, ConfigWithModules),
    dynamic_modules:ensure_modules(domain(), required_modules(GroupName)),
    setup_meck(GroupName),
    save_offline_status(GroupName, ConfigWithRules).

setup_meck(mam_failure) ->
    ok = escalus_ejabberd:rpc(meck, expect, [mod_mam_odbc_arch, archive_message, 9,
                                             {error, simulated}]);
setup_meck(offline_failure) ->
    ok = escalus_ejabberd:rpc(meck, expect, [mod_offline_mnesia, write_messages, 3,
                                             {error, simulated}]);
setup_meck(_) -> ok.

save_offline_status(mam_success, Config) -> [{offline_storage, mam} | Config];
save_offline_status(mam_failure, Config) -> [{offline_storage, mam_failure} | Config];
save_offline_status(offline_success, Config) -> [{offline_storage, offline} | Config];
save_offline_status(offline_failure, Config) -> [{offline_storage, offline_failure} | Config];
save_offline_status(basic, Config) -> [{offline_storage, none} | Config];
save_offline_status(_GN, Config) -> Config.

end_per_group(GroupName, Config) ->
    teardown_meck(GroupName),
    dynamic_modules:restore_modules(domain(), Config).

teardown_meck(G) when G == mam_failure;
                      G == offline_failure ->
    escalus_ejabberd:rpc(meck, unload, []);
teardown_meck(_) -> ok.

init_per_testcase(Name, C) -> escalus:init_per_testcase(Name, C).
end_per_testcase(Name, C) -> escalus:end_per_testcase(Name, C).

initial_service_discovery_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}],
      fun(Alice) ->
              escalus_client:send(Alice, disco_info(Config)),
              Response = escalus_client:wait_for_stanza(Alice),
              escalus:assert(has_feature, [ns_amp()], Response)
      end).

actions_and_conditions_discovery_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}],
      fun(Alice) ->
          Args = [ns_amp(),
                  <<"http://jabber.org/protocol/amp?action=notify">>,
                  <<"http://jabber.org/protocol/amp?action=error">>,
                  <<"http://jabber.org/protocol/amp?condition=deliver">>,
                  <<"http://jabber.org/protocol/amp?condition=match-resource">>
                  ],
          escalus_client:send(Alice, disco_info_amp_node(Config)),
          Response = escalus_client:wait_for_stanza(Alice),
          assert_has_features(Response, Args)
      end).


unsupported_actions_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% given
              Msg = amp_message_to(Bob, [{deliver, direct, alert}],  % alert is unsupported
                                   <<"A paradoxical payload!">>),
              %% when
              client_sends_message(Alice, Msg),

              % then
              client_receives_amp_error(Alice, {deliver, direct, alert}, <<"unsupported-actions">>)
      end).

unsupported_conditions_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% given
              %% expire-at is unsupported
              Msg = amp_message_to(Bob, [{'expire-at', <<"2020-06-06T12:20:20Z">>, notify}],
                                   <<"Never fade away!">>),
              %% when
              client_sends_message(Alice, Msg),

              % then
              client_receives_amp_error(Alice, {'expire-at', <<"2020-06-06T12:20:20Z">>, notify},
                                   <<"unsupported-conditions">>)
      end).

unacceptable_rules_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% given
              Msg = amp_message_to(Bob, [{broken, rule, spec}
                                        , {also_broken, rule, spec}
                                        ],
                                   <<"Break all the rules!">>),
              %% when
              client_sends_message(Alice, Msg),

              % then
              client_receives_amp_error(Alice, [{broken, rule, spec}
                                           , {also_broken, rule, spec}],
                                    <<"not-acceptable">>)
      end).

notify_deliver_to_online_user_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% given
              Rule = {deliver, direct, notify},
              Rules = rules(Config, [Rule]),
              Msg = amp_message_to(Bob, Rules, <<"I want to be sure you get this!">>),

              %% when
              client_sends_message(Alice, Msg),

              % then
              case lists:member(Rule, Rules) of
                  true -> client_receives_notification(Alice, Bob, Rule);
                  false -> ok
              end,
              client_receives_message(Bob, <<"I want to be sure you get this!">>),
              client_receives_nothing(Alice)
      end).



notify_deliver_to_online_user_bare_jid_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% given
              Message = <<"One of your resources needs to get this!">>,
              Rule = {deliver, direct, notify},
              Rules = rules(Config, [Rule]),
              BobsBareJid = escalus_client:short_jid(Bob),
              Msg = amp_message_to(BobsBareJid, Rules, Message),
              %% when
              client_sends_message(Alice, Msg),
              % then
              case lists:member(Rule, Rules) of
                  true -> client_receives_notification(Alice, BobsBareJid, Rule);
                  false -> ok
              end,
              client_receives_message(Bob, Message),
              client_receives_nothing(Alice)
      end).

notify_deliver_to_online_user_recipient_privacy_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% given
              Rule = {deliver, none, notify},
              Rules = rules(Config, [Rule]),
              Msg = amp_message_to(Bob, Rules, <<"Should be filtered by Bob's privacy list">>),
              privacy_helper:set_and_activate(Bob, <<"deny_all_message">>),

              %% when
              client_sends_message(Alice, Msg),

              % then
              case lists:member(Rule, Rules) of
                  true -> client_receives_notification(Alice, Bob, Rule);
                  false -> ok
              end,
              client_receives_generic_error(Alice, <<"503">>, <<"cancel">>),
              client_receives_nothing(Alice),
              client_receives_nothing(Bob)
      end).

notify_deliver_to_offline_user_test(Config) ->
    FreshConfig = escalus_fresh:create_users(Config, [alice, bob]),
    escalus:story(
      FreshConfig, [{alice, 1}],
      fun(Alice) ->
              %% given
              Rule = {deliver, case is_offline_storage_working(Config) of
                                   true -> stored;
                                   false -> none
                               end, notify},
              Rules = rules(Config, [Rule]),
              BobJid = escalus_users:get_jid(FreshConfig, bob),
              Msg = amp_message_to(BobJid, Rules, <<"A message in a bottle...">>),

              %% when
              client_sends_message(Alice, Msg),

              % then
              case lists:member(Rule, Rules) of
                  true -> client_receives_notification(Alice, BobJid, Rule);
                  false -> ok
              end,
              case ?config(offline_storage, Config) of
                  offline_failure -> client_receives_generic_error(Alice, <<"500">>, <<"wait">>);
                  _ -> client_receives_nothing(Alice)
              end
      end),
    case is_offline_storage_working(Config) of
        true -> user_has_incoming_offline_message(FreshConfig, bob, <<"A message in a bottle...">>);
        false -> user_has_no_incoming_offline_messages(FreshConfig, bob)
    end.

is_offline_storage_working(Config) ->
    Status = ?config(offline_storage, Config),
    Status == mam orelse Status == offline.

notify_deliver_to_offline_user_recipient_privacy_test(Config) ->
    case is_module_loaded(mod_mam) of
        true -> skipped; %% MAM does not support privacy lists
        false -> do_notify_deliver_to_offline_user_recipient_privacy_test(Config)
    end.

do_notify_deliver_to_offline_user_recipient_privacy_test(Config) ->
    FreshConfig = escalus_fresh:create_users(Config, [alice, bob]),
    escalus:story(
      FreshConfig, [{bob, 1}],
      fun(Bob) ->
              %% given
              privacy_helper:set_and_activate(Bob, <<"deny_all_message">>),
              privacy_helper:set_default_list(Bob, <<"deny_all_message">>)
      end),
    escalus:story(
      FreshConfig, [{alice, 1}],
      fun(Alice) ->
              %% given
              Rule = {deliver, none, notify},
              Rules = rules(Config, [Rule]),
              BobJid = escalus_users:get_jid(FreshConfig, bob),
              Msg = amp_message_to(BobJid, Rules, <<"A message in a bottle...">>),

              %% when
              client_sends_message(Alice, Msg),

              % then
              case lists:member(Rule, Rules) of
                  true -> client_receives_notification(Alice, BobJid, Rule);
                  false -> ok
              end,
              client_receives_nothing(Alice)
      end),
    user_has_no_incoming_offline_messages(FreshConfig, bob).

notify_deliver_to_stranger_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}],
      fun(Alice) ->
              %% given
              Rule = {deliver, none, notify},
              Rules = rules(Config, [Rule]),
              Domain = domain(),
              StrangerJid = <<"stranger@", Domain/binary>>,
              Msg = amp_message_to(StrangerJid, Rules, <<"A message in a bottle...">>),

              %% when
              client_sends_message(Alice, Msg),

              % then
              case lists:member(Rule, Rules) of
                  true -> client_receives_notification(Alice, StrangerJid, Rule);
                  false -> ok
              end,
              client_receives_generic_error(Alice, <<"503">>, <<"cancel">>),
              client_receives_nothing(Alice)
      end).

notify_deliver_to_malformed_jid_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}],
      fun(Alice) ->
              %% given
              StrangerJid = <<"not a jid">>,
              Rule = {deliver, none, notify},
              Rules = rules(Config, [Rule]),
              Msg = amp_message_to(StrangerJid, Rules, <<"Msg to malformed jid">>),
              %% when
              client_sends_message(Alice, Msg),

              % then
              case lists:member(Rule, Rules) of
                  true -> client_receives_notification(Alice, StrangerJid, Rule);
                  false -> ok
              end,
              % Error codes may vary because of s2s config - accept any code
              client_receives_generic_error(Alice, any, <<"cancel">>),
              client_receives_nothing(Alice)
      end).

notify_match_resource_any_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {bob, 4}],
      fun(Alice, Bob, _, _, _) ->
              %% given
              Msg = amp_message_to(Bob, [{'match-resource', any, notify}],
                                   <<"Church-encoded hot-dogs">>),
              %% when
              client_sends_message(Alice, Msg),

              % then
              client_receives_notification(Alice, Bob, {'match-resource', any, notify}),
              client_receives_message(Bob, <<"Church-encoded hot-dogs">>)
      end).

notify_match_resource_exact_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {bob, 4}],
      fun(Alice, _, _, Bob3, _) ->
              %% given
              Msg = amp_message_to(Bob3, [{'match-resource', exact, notify}],
                                   <<"Resource three, your battery is on fire!">>),
              %% when
              client_sends_message(Alice, Msg),

              % then
              client_receives_notification(Alice, Bob3, {'match-resource', exact, notify}),
              client_receives_message(Bob3, <<"Resource three, your battery is on fire!">>)
      end).

notify_match_resource_other_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% given
              NonmatchingJid = << (escalus_client:short_jid(Bob))/binary,
                                  "/blahblahblah_resource" >>,
              Msg = amp_message_to(NonmatchingJid,
                                   [{'match-resource', other, notify}],
                                    <<"A Bob by any other name!">>),

              %% when
              client_sends_message(Alice, Msg),

              % then
              client_receives_notification(Alice, NonmatchingJid,
                                           {'match-resource', other, notify}),
              client_receives_message(Bob, <<"A Bob by any other name!">>)
      end).

notify_match_resource_other_bare_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% given
              BareJid = escalus_client:short_jid(Bob),
              Msg = amp_message_to(BareJid,
                                   [{'match-resource', other, notify}],
                                    <<"A Bob by any other name!">>),

              %% when
              client_sends_message(Alice, Msg),

              % then
              client_receives_notification(Alice, BareJid, {'match-resource', other, notify}),
              client_receives_message(Bob, <<"A Bob by any other name!">>)
      end).

error_deliver_to_online_user_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% given
              Rule = {deliver, direct, error},
              Rules = rules(Config, [Rule]),
              Msg = amp_message_to(Bob, Rules, <<"It might cause an error">>),

              %% when
              client_sends_message(Alice, Msg),

              % then
              case lists:member(Rule, Rules) of
                  true ->
                      client_receives_amp_error(Alice, Bob, Rule, <<"undefined-condition">>),
                      client_receives_nothing(Bob);
                  false ->
                      client_receives_message(Bob, <<"It might cause an error">>)
              end,
              client_receives_nothing(Alice)
      end).

error_deliver_to_offline_user_test(Config) ->
    FreshConfig = escalus_fresh:create_users(Config, [alice, bob]),
    Rule = {deliver, case ?config(offline_storage, Config) of
                         none -> none;
                         _ -> stored
                     end, error},
    Rules = rules(Config, [Rule]),
    escalus:story(
      FreshConfig, [{alice, 1}],
      fun(Alice) ->
              %% given
              BobJid = escalus_users:get_jid(FreshConfig, bob),
              Msg = amp_message_to(BobJid, Rules, <<"A message in a bottle...">>),

              %% when
              client_sends_message(Alice, Msg),

              % then
              case lists:member(Rule, Rules) of
                  true ->
                      client_receives_amp_error(Alice, BobJid, Rule, <<"undefined-condition">>);
                  false ->
                      check_offline_storage(Alice, Config)
              end
      end),
    case is_offline_storage_working(Config) andalso not lists:member(Rule, Rules) of
        true -> user_has_incoming_offline_message(FreshConfig, bob, <<"A message in a bottle...">>);
        false -> user_has_no_incoming_offline_messages(FreshConfig, bob)
    end.

check_offline_storage(User, Config) ->
    case ?config(offline_storage, Config) of
        offline_failure ->
            client_receives_generic_error(User, <<"500">>, <<"wait">>);
        _ -> client_receives_nothing(User)
    end.

error_deliver_to_stranger_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}],
      fun(Alice) ->
              %% given
              Rule = {deliver, none, error},
              Rules = rules(Config, [Rule]),
              Domain = domain(),
              StrangerJid = <<"stranger@", Domain/binary>>,
              Msg = amp_message_to(StrangerJid, Rules, <<"This cannot possibly succeed">>),

              %% when
              client_sends_message(Alice, Msg),

              % then
              case lists:member(Rule, Rules) of
                  true -> client_receives_amp_error(Alice, StrangerJid, Rule,
                                                    <<"undefined-condition">>);
                  false -> client_receives_generic_error(Alice, <<"503">>, <<"cancel">>)
              end,
              client_receives_nothing(Alice)

      end).

drop_deliver_to_online_user_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% given
              Rule = {deliver, direct, drop},
              Rules = rules(Config, [Rule]),
              Msg = amp_message_to(Bob, Rules, <<"It might get dropped">>),

              %% when
              client_sends_message(Alice, Msg),

              % then
              case lists:member(Rule, Rules) of
                  true -> client_receives_nothing(Bob);
                  false -> client_receives_message(Bob, <<"It might get dropped">>)
              end,
              client_receives_nothing(Alice)
      end).

drop_deliver_to_offline_user_test(Config) ->
    FreshConfig = escalus_fresh:create_users(Config, [alice, bob]),
    Rule = {deliver, case ?config(offline_storage, Config) of
                         none -> none;
                         _ -> stored
                     end, drop},
    Rules = rules(Config, [Rule]),
    escalus:story(
      FreshConfig, [{alice, 1}],
      fun(Alice) ->
              %% given
              Message = <<"A message in a bottle...">>,
              BobJid = escalus_users:get_jid(FreshConfig, bob),
              Msg = amp_message_to(BobJid, Rules, Message),

              %% when
              client_sends_message(Alice, Msg),

              % then
              case lists:member(Rule, Rules) orelse
                   ?config(offline_storage, Config) /= offline_failure of
                  true -> client_receives_nothing(Alice);
                  false -> client_receives_generic_error(Alice, <<"500">>, <<"wait">>)
              end,

              % then
              case is_offline_storage_working(Config) andalso not lists:member(Rule, Rules) of
                  true -> user_has_incoming_offline_message(FreshConfig, bob,
                                                            Message);
                  false -> user_has_no_incoming_offline_messages(FreshConfig, bob)
              end
      end).

drop_deliver_to_stranger_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}],
      fun(Alice) ->
              %% given
              Rule = {deliver, none, drop},
              Rules = rules(Config, [Rule]),
              Domain = domain(),
              StrangerJid = <<"stranger@", Domain/binary>>,
              Msg = amp_message_to(StrangerJid, Rules, <<"This cannot possibly succeed">>),

              %% when
              client_sends_message(Alice, Msg),

              % then
              case lists:member(Rule, Rules) of
                  true -> ok;
                  false -> client_receives_generic_error(Alice, <<"503">>, <<"cancel">>)
              end,
              client_receives_nothing(Alice)

      end).

last_rule_applies_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% given
              BobsBareJid = escalus_client:short_jid(Bob),
              Msg = amp_message_to(BobsBareJid, [{deliver, none, error},
                                                 {deliver, stored, error},
                                                 {deliver, direct, notify}],
                                   <<"One of your resources needs to get this!">>),
              %% when
              client_sends_message(Alice, Msg),

              % then
              client_receives_notification(Alice, BobsBareJid, {deliver, direct, notify}),
              client_receives_message(Bob, <<"One of your resources needs to get this!">>)
      end).

%% Internal

user_has_no_incoming_offline_messages(FreshConfig, UserName) ->
    escalus:story(
      FreshConfig, [{UserName, 1}],
      fun(User) ->
              client_receives_nothing(User),
              case is_module_loaded(mod_mam) of
                  true -> client_has_no_mam_messages(User);
                  false -> ok
              end
      end).

user_has_incoming_offline_message(FreshConfig, UserName, MsgText) ->
    true = is_module_loaded(mod_mam) orelse is_module_loaded(mod_offline),
    {ok, Client} = escalus_client:start(FreshConfig, UserName, <<"new-session">>),
    escalus:send(Client, escalus_stanza:presence(<<"available">>)),
    case is_module_loaded(mod_offline) of
        true -> client_receives_message(Client, MsgText);
        false -> ok
    end,
    Presence = escalus:wait_for_stanza(Client),
    escalus:assert(is_presence, Presence),
    case is_module_loaded(mod_mam) of
        true -> client_has_mam_message(Client);
        false -> ok
    end,
    escalus_cleaner:clean(FreshConfig).

client_has_no_mam_messages(User) ->
    P = mam_helper:mam03_props(),
    escalus:send(User, mam_helper:stanza_archive_request(P, <<"q1">>)),
    Res = mam_helper:wait_archive_respond(P, User),
    mam_helper:assert_respond_size(0, Res).

client_has_mam_message(User) ->
    P = mam_helper:mam03_props(),
    escalus:send(User, mam_helper:stanza_archive_request(P, <<"q1">>)),
    Res = mam_helper:wait_archive_respond(P, User),
    mam_helper:assert_respond_size(1, Res).

setup_rules(notify_deliver_none, Config) -> [{rules, [{deliver, none, notify}]} | Config];
setup_rules(error_deliver_none, Config) -> [{rules, [{deliver, none, error}]} | Config];
setup_rules(drop_deliver_none, Config) -> [{rules, [{deliver, none, drop}]} | Config];
setup_rules(notify_deliver_stored, Config) -> [{rules, [{deliver, stored, notify}]} | Config];
setup_rules(error_deliver_stored, Config) -> [{rules, [{deliver, stored, error}]} | Config];
setup_rules(drop_deliver_stored, Config) -> [{rules, [{deliver, stored, drop}]} | Config];
setup_rules(notify_deliver_direct, Config) -> [{rules, [{deliver, direct, notify}]} | Config];
setup_rules(error_deliver_direct, Config) -> [{rules, [{deliver, direct, error}]} | Config];
setup_rules(drop_deliver_direct, Config) -> [{rules, [{deliver, direct, drop}]} | Config];
setup_rules(notify_deliver_none_direct_stored, Config) ->
    [{rules, [{deliver, none, notify},
              {deliver, direct, notify},
              {deliver, stored, notify}]} | Config];
setup_rules(error_deliver_none_direct_stored, Config) ->
    [{rules, [{deliver, none, error},
              {deliver, direct, error},
              {deliver, stored, error}]} | Config];
setup_rules(drop_deliver_none_direct_stored, Config) ->
    [{rules, [{deliver, none, drop},
              {deliver, direct, drop},
              {deliver, stored, drop}]} | Config];
setup_rules(_, Config) -> Config.

rules(Config, Default) ->
    case lists:keysearch(rules, 1, Config) of
        {value, {rules, Val}} -> Val;
        _ -> Default
    end.

ns_amp() ->
    <<"http://jabber.org/protocol/amp">>.

client_goes_offline(Client) ->
    escalus_client:stop(Client).

client_sends_message(Client, Msg) ->
    escalus_client:send(Client, Msg).

client_receives_amp_error(Client, Rules, AmpErrorKind) when is_list(Rules) ->
    Received = escalus_client:wait_for_stanza(Client),
    assert_amp_error(Client, Received, Rules, AmpErrorKind);
client_receives_amp_error(Client, Rule, AmpErrorKind) ->
    client_receives_amp_error(Client, [Rule], AmpErrorKind).

client_receives_amp_error(Client, IntendedRecipient, Rule, AmpErrorKind) ->
    Received = escalus_client:wait_for_stanza(Client),
    assert_amp_error_with_full_amp(Client, IntendedRecipient,
                                   Received, Rule, AmpErrorKind).

client_receives_generic_error(Client, Code, Type) ->
    Received = escalus_client:wait_for_stanza(Client, 5000),
    escalus:assert(fun contains_error/3, [Code, Type], Received).

client_receives_nothing(Client) ->
    timer:sleep(300),
    escalus_assert:has_no_stanzas(Client).

client_receives_message(Client, MsgText) ->
    Received = escalus_client:wait_for_stanza(Client),
    escalus:assert(is_chat_message, [MsgText], Received).

client_receives_notification(Client, IntendedRecipient, Rule) ->
    Msg = escalus_client:wait_for_stanza(Client),
    assert_notification(Client, IntendedRecipient, Msg, Rule).

disco_info(Config) ->
    Server = ct:get_config({hosts, mim, domain}),
    escalus_stanza:disco_info(Server).
disco_info_amp_node(Config) ->
    Server = ct:get_config({hosts, mim, domain}),
    escalus_stanza:disco_info(Server, ns_amp()).

assert_amp_error(Client, Response, Rules, AmpErrorKind) when is_list(Rules) ->
    ClientJID = escalus_client:full_jid(Client),
    Server = escalus_client:server(Client),
    Server = exml_query:attr(Response, <<"from">>),
    ClientJID = exml_query:attr(Response, <<"to">>),
    escalus:assert(fun contains_amp/5,
                   [amp_status_attr(AmpErrorKind), no_to_attr, no_from_attr, Rules],
                   Response),
    escalus:assert(fun contains_amp_error/3,
                   [AmpErrorKind, Rules],
                   Response);

assert_amp_error(Client, Response, Rule, AmpErrorKind) ->
    assert_amp_error(Client, Response, [Rule], AmpErrorKind).

assert_amp_error_with_full_amp(Client, IntendedRecipient, Response,
                               {_C, _V, _A} = Rule, AmpErrorKind) ->
    ClientJID = escalus_client:full_jid(Client),
    RecipientJID = full_jid(IntendedRecipient),
    Server = escalus_client:server(Client),
    Server = exml_query:attr(Response, <<"from">>),
    ClientJID = exml_query:attr(Response, <<"to">>),
    escalus:assert(fun contains_amp/5,
                   [amp_status_attr(AmpErrorKind), RecipientJID, ClientJID, [Rule]],
                   Response),
    escalus:assert(fun contains_amp_error/3,
                   [AmpErrorKind, [Rule]],
                   Response).


assert_notification(Client, IntendedRecipient, Response, {_C, _V, A} = Rule) ->
    ClientJID = escalus_client:full_jid(Client),
    RecipientJID = full_jid(IntendedRecipient),
    Server = escalus_client:server(Client),
    Action = a2b(A),
    Server = exml_query:attr(Response, <<"from">>),
    ClientJID = exml_query:attr(Response, <<"to">>),
    escalus:assert(fun contains_amp/5,
                   [Action, RecipientJID, ClientJID, [Rule]],
                   Response).

assert_has_features(Response, Features) ->
    CheckF = fun(F) -> escalus:assert(has_feature, [F], Response) end,
    lists:foreach(CheckF, Features).

full_jid(#client{} = Client) ->
    escalus_client:full_jid(Client);
full_jid(B) when is_binary(B) ->
    B.

%% @TODO: Move me out to escalus_stanza %%%%%%%%%
%%%%%%%%% Element constructors %%%%%%%%%%%%%%%%%%
amp_message_to(To, Rules, MsgText) ->
    Msg0 = #xmlel{children=C} = escalus_stanza:chat_to(To, MsgText),
    Msg = escalus_stanza:set_id(Msg0, escalus_stanza:id()),
    Amp = amp_el(Rules),
    Msg#xmlel{children = C ++ [Amp]}.

amp_el([]) ->
    throw("cannot build <amp> with no rules!");
amp_el(Rules) ->
    #xmlel{name = <<"amp">>,
           attrs = [{<<"xmlns">>, ns_amp()}],
           children = [ rule_el(R) || R <- Rules ]}.

rule_el({Condition, Value, Action}) ->
    check_rules(Condition, Value, Action),
    #xmlel{name = <<"rule">>
          , attrs = [{<<"condition">>, a2b(Condition)}
                    , {<<"value">>, a2b(Value)}
                    , {<<"action">>, a2b(Action)}]}.

%% @TODO: Move me out to escalus_pred %%%%%%%%%%%%
%%%%%%%%% XML predicates %%%%% %%%%%%%%%%%%%%%%%%%
contains_amp(Status, To, From, ExpectedRules, Stanza) when is_list(ExpectedRules)->
    Amp = exml_query:subelement(Stanza, <<"amp">>),
    undefined =/= Amp andalso
        To == exml_query:attr(Amp, <<"to">>, no_to_attr) andalso
        From == exml_query:attr(Amp, <<"from">>, no_from_attr) andalso
        Status == exml_query:attr(Amp, <<"status">>, no_status_attr) andalso
        all_present([ rule_el(R) || R <- ExpectedRules ], exml_query:subelements(Amp, <<"rule">>)).

contains_amp_error(AmpErrorKind, Rules, Response) ->
    ErrorEl = exml_query:subelement(Response, <<"error">>),
    <<"modify">> == exml_query:attr(ErrorEl, <<"type">>)
        andalso
        amp_error_code(AmpErrorKind) == exml_query:attr(ErrorEl, <<"code">>)
        andalso
        undefined =/= (Marker = exml_query:subelement(ErrorEl, amp_error_marker(AmpErrorKind)))
        andalso
        ns_stanzas() == exml_query:attr(Marker, <<"xmlns">>)
        andalso
        undefined =/= (Container = exml_query:subelement(ErrorEl,
                                            amp_error_container(AmpErrorKind)))
        andalso
        all_present([ rule_el(R) || R <- Rules ], exml_query:subelements(Container, <<"rule">>)).

contains_error(Code, Type, Response) ->
    ErrorEl = exml_query:subelement(Response, <<"error">>),
    Type == exml_query:attr(ErrorEl, <<"type">>)
        andalso (Code == any orelse Code == exml_query:attr(ErrorEl, <<"code">>)).

all_present(Needles, Haystack) ->
    list_and([ lists:member(Needle, Haystack)
               || Needle <- Needles ]).


list_and(List) ->
    lists:all(fun(X) -> X =:= true end, List).

ns_stanzas() ->
    <<"urn:ietf:params:xml:ns:xmpp-stanzas">>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_rules(deliver, direct, notify) -> ok;
check_rules(deliver, stored, notify) -> ok;
check_rules(deliver, none, notify) -> ok;

check_rules(deliver, direct, error) -> ok;
check_rules(deliver, stored, error) -> ok;
check_rules(deliver, none, error) -> ok;

check_rules(deliver, direct, drop) -> ok;
check_rules(deliver, stored, drop) -> ok;
check_rules(deliver, none, drop) -> ok;

check_rules('match-resource', any, notify) -> ok;
check_rules('match-resource', exact, notify) -> ok;
check_rules('match-resource', other, notify) -> ok;

check_rules(deliver, direct, alert) -> ok;       %% for testing unsupported rules
check_rules('expire-at', _binary, notify) -> ok; %% for testing unsupported conditions
check_rules(broken, rule, spec) -> ok;           %% for testing unacceptable rules
check_rules(also_broken, rule, spec) -> ok;      %% for testing unacceptable rules

check_rules(C, V, A) -> throw({illegal_amp_rule, {C, V, A}}).

a2b(B) when is_binary(B) -> B;
a2b(A) -> atom_to_binary(A, utf8).

%% Undefined-condition errors return a fully-fledged amp element with status=error
%% The other errors have 'thin' <amp>s with no status attribute
amp_status_attr(<<"undefined-condition">>) -> <<"error">>;
amp_status_attr(_)                         -> no_status_attr.

amp_error_code(<<"undefined-condition">>) -> <<"500">>;
amp_error_code(<<"not-acceptable">>) -> <<"405">>;
amp_error_code(<<"unsupported-actions">>) -> <<"400">>;
amp_error_code(<<"unsupported-conditions">>) -> <<"400">>.

amp_error_marker(<<"not-acceptable">>) -> <<"not-acceptable">>;
amp_error_marker(<<"unsupported-actions">>) -> <<"bad-request">>;
amp_error_marker(<<"unsupported-conditions">>) -> <<"bad-request">>;
amp_error_marker(<<"undefined-condition">>) -> <<"undefined-condition">>.

amp_error_container(<<"not-acceptable">>) -> <<"invalid-rules">>;
amp_error_container(<<"unsupported-actions">>) -> <<"unsupported-actions">>;
amp_error_container(<<"unsupported-conditions">>) -> <<"unsupported-conditions">>;
amp_error_container(<<"undefined-condition">>) -> <<"failed-rules">>.

is_module_loaded(Mod) ->
    escalus_ejabberd:rpc(gen_mod, is_loaded, [domain(), Mod]).

required_modules(basic) ->
    mam_modules(off) ++ offline_modules(off);
required_modules(mam) ->
    mam_modules(on) ++ offline_modules(off);
required_modules(offline) ->
    mam_modules(off) ++ offline_modules(on);
required_modules(mam_and_offline) ->
    mam_modules(on) ++ offline_modules(on);
required_modules(_) ->
    [].

mam_modules(on) ->
    [{mod_mam_odbc_user, [pm]},
     {mod_mam_odbc_prefs, [pm]},
     {mod_mam_odbc_arch, [pm]},
     {mod_mam, []}];
mam_modules(off) ->
    [{mod_mam, stopped}].

offline_modules(on) ->
    [{mod_offline, [{access_max_user_messages, max_user_offline_messages}]}];
offline_modules(off) ->
    [{mod_offline, stopped},
     {mod_offline_stub, []}].

domain() ->
    ct:get_config({hosts, mim, domain}).
