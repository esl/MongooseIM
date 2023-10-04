-module(amp_big_SUITE).
%% @doc Tests for XEP-0079 Advanced Message Processing support
%% @reference <a href="http://xmpp.org/extensions/xep-0079.html">XEP-0079</a>
%% @author <simon.zelazny@erlang-solutions.com>
%% @copyright 2014 Erlang Solutions, Ltd.
%% This work was sponsored by Grindr.com

-compile([export_all, nowarn_export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).
-import(muc_light_helper, [lbin/1]).
-import(domain_helper, [host_type/0, domain/0]).

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

all() ->
    [{group, G} || G <- main_group_names(), is_enabled(G)].

groups() ->
    group_spec(main_group_names()).

is_enabled(mam) -> mongoose_helper:is_rdbms_enabled(host_type());
is_enabled(_) -> true.

%% Group definitions

main_group_names() ->
    [basic, mam, offline].

subgroups(mam) -> [mam_success, mam_failure];
subgroups(offline) -> [offline_success, offline_failure];
subgroups(_) -> [].

group_spec(Groups) when is_list(Groups) ->
    lists:flatmap(fun group_spec/1, Groups);
group_spec(Group) ->
    case subgroups(Group) of
        [] -> [{Group, [parallel], test_cases(Group)}];
        SubGroups -> [{Group, [{group, SubG} || SubG <- SubGroups]} | group_spec(SubGroups)]
    end.

test_cases(Group) ->
    regular_tests(Group) ++ multiple_config_cth:flatten_and_strip_config(tests_with_config(Group)).

regular_tests(basic) -> basic_test_cases();
regular_tests(_) -> [].

%% This function is called by multiple_config_cth for each group
%% to get a list of configs for each test case
-spec tests_with_config(_GroupName :: atom()) -> [{TestCase :: atom(),
                                                  [Config :: [{Key :: atom(), Value :: term()}]]}].
tests_with_config(_GroupName) ->
    lists:append([deliver_tests_with_config(notify),
                  deliver_tests_with_config(error),
                  deliver_tests_with_config(drop)]).

%% Each of the 'deliver' tests is repeated several times, each time with a different config
deliver_tests_with_config(Action) ->
    multiple_config_cth:add_config(deliver_rule_configs(Action), deliver_test_cases(Action)).

%% Each config tests different rules in the AMP message
deliver_rule_configs(Action) ->
    [
     [{rules, [{deliver, direct, Action}]}],
     [{rules, [{deliver, stored, Action}]}],
     [{rules, [{deliver, none, Action}]}],
     [{rules, [{deliver, direct, Action},
               {deliver, stored, Action},
               {deliver, none, Action}]}]
    ].

%% Test case list, each test has to be listed exactly once

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

deliver_test_cases(notify) ->
    [notify_deliver_to_online_user_test,
     notify_deliver_to_online_user_bare_jid_test,
     notify_deliver_to_online_user_recipient_privacy_test,
     notify_deliver_to_offline_user_test,
     notify_deliver_to_offline_user_recipient_privacy_test,
     notify_deliver_to_online_user_broken_connection_test,
     notify_deliver_to_stranger_test,
     notify_deliver_to_unknown_domain_test];
deliver_test_cases(error) ->
    [error_deliver_to_online_user_test,
     error_deliver_to_offline_user_test,
     error_deliver_to_stranger_test];
deliver_test_cases(drop) ->
    [drop_deliver_to_online_user_test,
     drop_deliver_to_offline_user_test,
     drop_deliver_to_stranger_test].

%% Setup and teardown

init_per_suite(Config) ->
    ConfigWithHooks = [{ct_hooks, [{multiple_config_cth, fun tests_with_config/1}]} | Config],
    {Mod, Code} = rpc(mim(), dynamic_compile, from_string, [amp_test_helper_code()]),
    rpc(mim(), code, load_binary, [Mod, "amp_test_helper.erl", Code]),
    setup_meck(suite),
    escalus:init_per_suite(ConfigWithHooks).

amp_test_helper_code() ->
    "-module(amp_test_helper).\n"
    "-compile([export_all, nowarn_export_all]).\n"
    "setup_meck() ->\n"
    "  meck:expect(ranch_tcp, send, fun ranch_tcp_send/2).\n"
    "ranch_tcp_send(Socket, Data) ->\n"
    "  case catch binary:match(Data, <<\"Recipient connection breaks\">>) of\n"
    "    {N, _} when is_integer(N) -> {error, simulated};\n"
    "    _ -> meck:passthrough([Socket, Data])\n"
    "  end.\n".

end_per_suite(C) ->
    teardown_meck(suite),
    escalus_fresh:clean(),
    escalus:end_per_suite(C).

init_per_group(GroupName, Config) ->
    Config1 = case lists:member(GroupName, main_group_names()) of
                  true ->
                      ConfigWithModules = dynamic_modules:save_modules(host_type(), Config),
                      dynamic_modules:ensure_modules(host_type(), required_modules(GroupName)),
                      ConfigWithModules;
                  false ->
                      Config
              end,
    setup_meck(GroupName),
    save_offline_status(GroupName, Config1).

setup_meck(suite) ->
    ok = rpc(mim(), meck, new, [ranch_tcp, [passthrough, no_link]]),
    ok = rpc(mim(), amp_test_helper, setup_meck, []);
setup_meck(mam_failure) ->
    ok = rpc(mim(), meck, expect, [mod_mam_rdbms_arch, archive_message, 3, {ok, {error, simulated}}]);
setup_meck(offline_failure) ->
    ok = rpc(mim(), meck, expect, [mod_offline_backend_module(), write_messages, 4, {error, simulated}]);
setup_meck(_) -> ok.

save_offline_status(mam_success, Config) -> [{offline_storage, mam} | Config];
save_offline_status(mam_failure, Config) -> [{offline_storage, mam_failure} | Config];
save_offline_status(offline_success, Config) -> [{offline_storage, offline} | Config];
save_offline_status(offline_failure, Config) -> [{offline_storage, offline_failure} | Config];
save_offline_status(basic, Config) -> [{offline_storage, none} | Config];
save_offline_status(_GN, Config) -> Config.

end_per_group(GroupName, Config) ->
    teardown_meck(GroupName),
    case lists:member(GroupName, main_group_names()) of
        true -> dynamic_modules:restore_modules(Config);
        false -> ok
    end.

teardown_meck(mam_failure) ->
    rpc(mim(), meck, unload, [mod_mam_rdbms_arch]);
teardown_meck(offline_failure) ->
    rpc(mim(), meck, unload, [mod_offline_backend_module()]);
teardown_meck(suite) ->
    rpc(mim(), meck, unload, []);
teardown_meck(_) -> ok.

init_per_testcase(Name, C) -> escalus:init_per_testcase(Name, C).
end_per_testcase(Name, C) -> escalus:end_per_testcase(Name, C).

%% Test cases

initial_service_discovery_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}],
      fun(Alice) ->
              escalus_client:send(Alice, disco_info()),
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
          escalus_client:send(Alice, disco_info_amp_node()),
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
    case is_module_loaded(mod_mam_pm) of
        true -> {skip, "MAM does not support privacy lists"};
        false -> do_notify_deliver_to_online_user_recipient_privacy_test(Config)
    end.

do_notify_deliver_to_online_user_recipient_privacy_test(Config) ->
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

notify_deliver_to_online_user_broken_connection_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->
              %% given
              Rule = {deliver, case ?config(offline_storage, Config) of
                                   mam -> stored;
                                   _ -> none
                               end, notify},
              Rules = rules(Config, [Rule]),
              %% This special message is matched by the
              %% amp_test_helper:ranch_tcp_send/2 mock,
              %% (see amp_test_helper_code/0)
              Msg = amp_message_to(Bob, Rules, <<"Recipient connection breaks">>),

              %% when
              client_sends_message(Alice, Msg),

              % then
              case lists:member(Rule, Rules) of
                  true -> client_receives_notification(Alice, Bob, Rule);
                  false -> ok
              end,
              client_receives_nothing(Alice),

              %% Kill Bob's connection to avoid errors with closing the stream
              %% while the session is being resumed after the simulated error
              escalus_connection:kill(Bob)
      end),
    ok.

notify_deliver_to_offline_user_test(Config) ->
    FreshConfig = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
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
    wait_until_no_session(FreshConfig, alice),
    case is_offline_storage_working(Config) of
        true -> user_has_incoming_offline_message(FreshConfig, bob, <<"A message in a bottle...">>);
        false -> user_has_no_incoming_offline_messages(FreshConfig, bob)
    end.

is_offline_storage_working(Config) ->
    Status = ?config(offline_storage, Config),
    Status == mam orelse Status == offline.

notify_deliver_to_offline_user_recipient_privacy_test(Config) ->
    case is_module_loaded(mod_mam_pm) of
        true -> {skip, "MAM does not support privacy lists"};
        false -> do_notify_deliver_to_offline_user_recipient_privacy_test(Config)
    end.

do_notify_deliver_to_offline_user_recipient_privacy_test(Config) ->
    FreshConfig = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    escalus:story(
      FreshConfig, [{alice, 1}, {bob, 1}],
      fun(Alice, Bob) ->

              privacy_helper:set_and_activate(Bob, <<"deny_all_message">>),
              privacy_helper:set_default_list(Bob, <<"deny_all_message">>),
              mongoose_helper:logout_user(Config, Bob),
              %% given
              Rule = {deliver, none, notify},
              Rules = rules(Config, [Rule]),
              BobJid = lbin(escalus_client:short_jid(Bob)),

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

notify_deliver_to_unknown_domain_test(Config) ->
    escalus:fresh_story(
      Config, [{alice, 1}],
      fun(Alice) ->
              %% given
              StrangerJid = <<"stranger@unknown.domain">>,
              Rule = {deliver, none, notify},
              Rules = rules(Config, [Rule]),
              Msg = amp_message_to(StrangerJid, Rules, <<"Msg to unknown domain">>),
              %% when
              client_sends_message(Alice, Msg),

              % then
              case lists:member(Rule, Rules) of
                  true -> client_receives_notification(Alice, StrangerJid, Rule);
                  false -> ok
              end,
              % error 404: 'remote server not found' is expected
              client_receives_generic_error(Alice, <<"404">>, <<"cancel">>),
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
    FreshConfig = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
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
    wait_until_no_session(FreshConfig, alice),
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
    FreshConfig = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    Rule = {deliver, case ?config(offline_storage, Config) of
                         none -> none;
                         _ -> stored
                     end, drop},
    Rules = rules(Config, [Rule]),
    Message = <<"A message in a bottle...">>,
    escalus:story(
      FreshConfig, [{alice, 1}],
      fun(Alice) ->
              %% given
              BobJid = escalus_users:get_jid(FreshConfig, bob),
              Msg = amp_message_to(BobJid, Rules, Message),

              %% when
              client_sends_message(Alice, Msg),

              % then
              case lists:member(Rule, Rules) orelse
                   ?config(offline_storage, Config) /= offline_failure of
                  true -> client_receives_nothing(Alice);
                  false -> client_receives_generic_error(Alice, <<"500">>, <<"wait">>)
              end
      end),
    wait_until_no_session(FreshConfig, alice),
    case is_offline_storage_working(Config) andalso not lists:member(Rule, Rules) of
        true -> user_has_incoming_offline_message(FreshConfig, bob, Message);
        false -> user_has_no_incoming_offline_messages(FreshConfig, bob)
    end.

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

wait_until_no_session(FreshConfig, User) ->
    U = escalus_users:get_username(FreshConfig, User),
    S = escalus_users:get_server(FreshConfig, User),
    JID = jid:make(U, S, <<>>),
    mongoose_helper:wait_until(
      fun() -> rpc(mim(), ejabberd_sm, get_user_resources, [JID]) end, []).

user_has_no_incoming_offline_messages(FreshConfig, UserName) ->
    escalus:fresh_story(
      FreshConfig, [{UserName, 1}],
      fun(User) ->
              client_receives_nothing(User),
              case is_module_loaded(mod_mam_pm) of
                  true -> client_has_no_mam_messages(User);
                  false -> ok
              end
      end).

user_has_incoming_offline_message(FreshConfig, UserName, MsgText) ->
    true = is_module_loaded(mod_mam_pm) orelse is_module_loaded(mod_offline),
    {ok, Client} = escalus_client:start(FreshConfig, UserName, <<"new-session">>),
    escalus:send(Client, escalus_stanza:presence(<<"available">>)),
    case is_module_loaded(mod_offline) of
        true -> client_receives_message(Client, MsgText);
        false -> ok
    end,
    Presence = escalus:wait_for_stanza(Client),
    escalus:assert(is_presence, Presence),
    case is_module_loaded(mod_mam_pm) of
        true -> client_has_mam_message(Client);
        false -> ok
    end,
    escalus_client:stop(FreshConfig, Client).

client_has_no_mam_messages(User) ->
    P = mam_helper:mam04_props(),
    escalus:send(User, mam_helper:stanza_archive_request(P, <<"q1">>)),
    Res = mam_helper:wait_archive_respond(User),
    mam_helper:assert_respond_size(0, Res).

client_has_mam_message(User) ->
    P = mam_helper:mam04_props(),
    escalus:send(User, mam_helper:stanza_archive_request(P, <<"q1">>)),
    Res = mam_helper:wait_archive_respond(User),
    mam_helper:assert_respond_size(1, Res).

rules(Config, Default) ->
    case lists:keysearch(rules, 1, Config) of
        {value, {rules, Val}} -> Val;
        _ -> Default
    end.

ns_amp() ->
    <<"http://jabber.org/protocol/amp">>.

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

disco_info() ->
    escalus_stanza:disco_info(domain()).

disco_info_amp_node() ->
    escalus_stanza:disco_info(domain(), ns_amp()).

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
    rpc(mim(), gen_mod, is_loaded, [host_type(), Mod]).

required_modules(basic) ->
    mam_modules(off) ++ offline_modules(off) ++ privacy_modules(on);
required_modules(mam) ->
    mam_modules(on) ++ offline_modules(off) ++ privacy_modules(off);
required_modules(offline) ->
    mam_modules(off) ++ offline_modules(on) ++ privacy_modules(on);
required_modules(_) ->
    [].

mam_modules(on) ->
    [{mod_mam, mam_helper:config_opts(#{pm => #{}, async_writer => #{enabled => false}})}];
mam_modules(off) ->
    [{mod_mam, stopped}].

offline_modules(on) ->
    Backend = mongoose_helper:mnesia_or_rdbms_backend(),
    [{mod_offline, config_parser_helper:mod_config(mod_offline,
        #{backend => Backend,
          access_max_user_messages => max_user_offline_messages})}];
offline_modules(off) ->
    [{mod_offline, stopped},
     {mod_offline_stub, []}].

privacy_modules(on) ->
    Backend = mongoose_helper:mnesia_or_rdbms_backend(),
    [{mod_privacy, config_parser_helper:mod_config(mod_privacy, #{backend => Backend})},
     {mod_blocking, config_parser_helper:mod_config(mod_blocking, #{backend => Backend})}];
privacy_modules(off) ->
    [{mod_privacy, stopped},
     {mod_blocking, stopped}].

mod_offline_backend_module() ->
    Backend = mongoose_helper:mnesia_or_rdbms_backend(),
    case Backend of
        mnesia ->
            mod_offline_mnesia;
        rdbms ->
            mod_offline_rdbms
    end.
