-module(sm_SUITE).
-compile(export_all).

-include_lib("exml/include/exml.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-define(MOD_SM, mod_stream_management).
-define(CONSTRAINT_CHECK_TIMEOUT, 5000).

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).

-import(vcard_update, [discard_vcard_update/1,
                       server_string/1]).

-import(escalus_stanza, [setattr/3]).

-define(BIG_BIG_BIG_TIMEOUT, 3600).
-define(SHORT_RESUME_TIMEOUT, 3).
-define(SMALL_SM_BUFFER, 3).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, parallel},
     {group, parallel_manual_ack_freq_1},
     {group, stale_h},
     {group, stream_mgmt_disabled},
     server_requests_ack_freq_2,
     {group, unacknowledged_message_hook}
     ].

groups() ->
    G = [{parallel, [parallel], parallel_test_cases()},
         {parallel_manual_ack_freq_1, [parallel], parallel_manual_ack_test_cases()},
         {stale_h, [], stale_h_test_cases()},
         {stream_mgmt_disabled, [], stream_mgmt_disabled_cases()},
         {manual_ack_freq_long_session_timeout, [parallel], [preserve_order]},
         {unacknowledged_message_hook, [parallel], unacknowledged_message_hook()}],
    ct_helper:repeat_all_until_all_ok(G).


parallel_test_cases() ->
    [server_announces_sm,
     server_enables_sm_before_session,
     server_enables_sm_after_session,
     server_returns_failed_after_start,
     server_returns_failed_after_auth,
     server_enables_resumption,
     client_enables_sm_twice_fails_with_correct_error_stanza,
     session_resumed_then_old_session_is_closed_gracefully_with_correct_error_stanza,
     session_resumed_and_old_session_dead_doesnt_route_error_to_new_session,
     basic_ack,
     h_ok_before_session,
     h_ok_after_session_enabled_before_session,
     h_ok_after_session_enabled_after_session,
     h_ok_after_a_chat,
     h_non_given_closes_stream_gracefully,
     resend_unacked_on_reconnection,
     session_established,
     wait_for_resumption,
     resume_session,
     resume_session_with_wrong_h_does_not_leak_sessions,
     resume_session_with_wrong_sid_returns_item_not_found,
     resume_session_with_wrong_namespace_is_a_noop,
     resume_dead_session_results_in_item_not_found,
     aggressively_pipelined_resume,
     replies_are_processed_by_resumed_session,
     subscription_requests_are_buffered_properly,
     messages_are_properly_flushed_during_resumption,
     messages_are_properly_flushed_during_resumption_p1_fsm_old
    ].

parallel_manual_ack_test_cases() ->
    [client_acks_more_than_sent,
     too_many_unacked_stanzas,
     resend_unacked_after_resume_timeout,
     resume_session_state_send_message,
     resume_session_state_stop_c2s,
     server_requests_ack_after_session,
     resend_more_offline_messages_than_buffer_size,
     server_requests_ack
     ].

stale_h_test_cases() ->
    [
     resume_expired_session_returns_correct_h,
     gc_repeat_after_never_means_no_cleaning,
     gc_repeat_after_timeout_does_clean
    ].

stream_mgmt_disabled_cases() ->
    [
     no_crash_if_stream_mgmt_disabled_but_client_requests_stream_mgmt,
     no_crash_if_stream_mgmt_disabled_but_client_requests_stream_mgmt_with_resumption
    ].

unacknowledged_message_hook() ->
    [unacknowledged_message_hook_bounce,
     unacknowledged_message_hook_offline,
     unacknowledged_message_hook_resume].

suite() ->
    require_rpc_nodes([mim]) ++ escalus:suite().

domain() ->
    ct:get_config({hosts, mim, domain}).

stream_management_with_stale_h(RepeatAfter, Geriatric) ->
  [{mod_stream_management,
    [
     {ack_freq, 1},
     {resume_timeout, ?SHORT_RESUME_TIMEOUT},
     {stale_h, [{enabled, true},
                {stale_h_repeat_after, RepeatAfter},
                {stale_h_geriatric, Geriatric}]}]}].

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    NewConfig1 = escalus_ejabberd:setup_option(ack_freq(never), Config),
    NewConfig = escalus_ejabberd:setup_option(buffer_max(?SMALL_SM_BUFFER), NewConfig1),
    NewConfigWithSM = escalus_users:update_userspec(NewConfig, alice, stream_management, true),
    mongoose_helper:inject_module(?MODULE),
    escalus:init_per_suite(NewConfigWithSM).

end_per_suite(Config) ->
    NewConfig = escalus_ejabberd:reset_option(ack_freq(never), Config),
    NewConfig1 = escalus_ejabberd:reset_option(buffer_max(?SMALL_SM_BUFFER), NewConfig),
    escalus_fresh:clean(),
    escalus:end_per_suite(NewConfig1).


init_per_group(G, Config) when G =:= unacknowledged_message_hook;
                               G =:= manual_ack_freq_long_session_timeout ->
    true = rpc(mim(), ?MOD_SM, set_ack_freq, [1]),
    escalus_users:update_userspec(Config, alice, manual_ack, true);
init_per_group(parallel_manual_ack_freq_1, Config) ->
    true = rpc(mim(), ?MOD_SM, set_ack_freq, [1]),
    rpc(mim(), ?MOD_SM, set_resume_timeout, [?SHORT_RESUME_TIMEOUT]),
    escalus_users:update_userspec(Config, alice, manual_ack, true);
init_per_group(stale_h, Config) ->
    escalus_users:update_userspec(Config, alice, manual_ack, true);
init_per_group(stream_mgmt_disabled, Config) ->
    Config2 = dynamic_modules:save_modules(domain(), Config),
    dynamic_modules:stop(domain(), ?MOD_SM),
    rpc(mim(), mnesia, delete_table, [sm_session]),
    escalus_users:update_userspec(Config2, alice, manual_ack, true);
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(stream_mgmt_disabled, Config) ->
    dynamic_modules:restore_modules(domain(), Config);
end_per_group(G, Config) when G =:= unacknowledged_message_hook;
                              G =:= manual_ack_freq_long_session_timeout ->
    true = rpc(mim(), ?MOD_SM, set_ack_freq, [never]),
    Config;
end_per_group(parallel_manual_ack_freq_1, Config) ->
    true = rpc(mim(), ?MOD_SM, set_ack_freq, [never]),
    rpc(mim(), ?MOD_SM, set_resume_timeout, [600]),
    Config;
end_per_group(_GroupName, Config) ->
    Config.


set_gc_parameters(RepeatAfter, Geriatric, Config) ->
    Config2 = dynamic_modules:save_modules(domain(), Config),
    dynamic_modules:ensure_modules(
      domain(), stream_management_with_stale_h(RepeatAfter, Geriatric)),
    Config2.

register_smid(IntSmidId) ->
    S = {SMID = make_smid(), IntSmidId},
    ok = rpc(mim(), ?MOD_SM, register_stale_smid_h, [SMID, IntSmidId]),
    S.

register_some_smid_h(Config) ->
    TestSmids = lists:map(fun register_smid/1, lists:seq(1, 3)),
    [{smid_test, TestSmids} | Config].

init_per_testcase(resume_expired_session_returns_correct_h = CN, Config) ->
    Config2 = set_gc_parameters(?BIG_BIG_BIG_TIMEOUT, ?BIG_BIG_BIG_TIMEOUT, Config),
    rpc(mim(), ?MOD_SM, set_resume_timeout, [?SHORT_RESUME_TIMEOUT]),
    true = rpc(mim(), ?MOD_SM, set_ack_freq, [1]),
    escalus:init_per_testcase(CN, Config2);
init_per_testcase(gc_repeat_after_never_means_no_cleaning = CN, Config) ->
    Config2 = set_gc_parameters(?BIG_BIG_BIG_TIMEOUT, ?SHORT_RESUME_TIMEOUT, Config),
    Config3 = register_some_smid_h(Config2),
    escalus:init_per_testcase(CN, Config3);
init_per_testcase(gc_repeat_after_timeout_does_clean = CN, Config) ->
    Config2 = set_gc_parameters(?SHORT_RESUME_TIMEOUT, ?SHORT_RESUME_TIMEOUT, Config),
    Config3 = register_some_smid_h(Config2),
    escalus:init_per_testcase(CN, Config3);
init_per_testcase(server_requests_ack_freq_2 = CN, Config) ->
    true = rpc(mim(), ?MOD_SM, set_ack_freq, [2]),
    escalus:init_per_testcase(CN, Config);
init_per_testcase(replies_are_processed_by_resumed_session = CN, Config) ->
    register_handler(<<"localhost">>),
    escalus:init_per_testcase(CN, Config);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CN, Config) when CN =:= resume_expired_session_returns_correct_h;
                                  CN =:= gc_repeat_after_never_means_no_cleaning;
                                  CN =:= gc_repeat_after_timeout_does_clean
                                   ->
    dynamic_modules:stop(domain(), ?MOD_SM),
    rpc(mim(), ejabberd_sup, stop_child, [stream_management_stale_h]),
    dynamic_modules:restore_modules(domain(), Config),
    escalus:end_per_testcase(CN, Config);
end_per_testcase(server_requests_ack_freq_2 = CN, Config) ->
    true = rpc(mim(), ?MOD_SM, set_ack_freq, [never]),
    escalus:end_per_testcase(CN, Config);
end_per_testcase(replies_are_processed_by_resumed_session = CN, Config) ->
    unregister_handler(<<"localhost">>),
    escalus:end_per_testcase(CN, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

server_announces_sm(Config) ->
    AliceSpec = escalus_fresh:freshen_spec(Config, alice),
    {ok, #client{props = Props}, Features} = escalus_connection:start(AliceSpec,
                                                                      [start_stream]),
    true = escalus_session:can_use_stream_management(Props, Features).


server_enables_sm_before_session(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Steps = connection_steps_to_enable_stream_mgmt(after_bind),
    {ok, _, _} = escalus_connection:start(AliceSpec, Steps).

server_enables_sm_after_session(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Steps = connection_steps_to_enable_stream_mgmt(after_session),
    {ok, _, _} = escalus_connection:start(AliceSpec, Steps).

server_returns_failed_after_start(Config) ->
    server_returns_failed(Config, []).

server_returns_failed_after_auth(Config) ->
    server_returns_failed(Config, [authenticate]).

server_enables_resumption(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    %% Assert matches {ok, _, _, _}
    Steps = connection_steps_to_enable_stream_resumption(),
    {ok, Alice, _} = escalus_connection:start(AliceSpec, Steps),
    escalus_connection:stop(Alice).

server_returns_failed(Config, ConnActions) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    {ok, Alice, _} = escalus_connection:start(AliceSpec,
                                                 [start_stream,
                                                  stream_features,
                                                  maybe_use_ssl]
                                                 ++ ConnActions),
    escalus_connection:send(Alice, escalus_stanza:enable_sm()),
    escalus:assert(is_sm_failed, [<<"unexpected-request">>],
                   escalus_connection:get_stanza(Alice, enable_sm_failed)).

client_enables_sm_twice_fails_with_correct_error_stanza(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Steps = connection_steps_to_session(),
    {ok, Alice, Features} = escalus_connection:start(AliceSpec, Steps),
    escalus_session:stream_management(Alice, Features),
    escalus_connection:send(Alice, escalus_stanza:enable_sm()),
    escalus:assert(is_sm_failed, [<<"unexpected-request">>],
                   escalus_connection:get_stanza(Alice, enable_sm_failed)),
    escalus:assert(is_stream_end,
                   escalus_connection:get_stanza(Alice, enable_sm_failed)),
    true = escalus_connection:wait_for_close(Alice,timer:seconds(5)).

session_resumed_then_old_session_is_closed_gracefully_with_correct_error_stanza(Config) ->
    %% GIVEN USER WITH STREAM RESUMPTION ENABLED
    {Alice, AliceSpec, SMID, SMH} =
        get_stream_resumption_enabled_fresh_user_smid_and_h(Config, alice),
    %% WHEN USER RESUMES SESSION FROM NEW CLIENT
    Steps2 = connection_steps_to_stream_resumption(SMID, SMH),
    {ok, Alice2, _} = escalus_connection:start(AliceSpec, Steps2),
    process_initial_stanza(Alice2),
    %% THEN: Old session is gracefully closed with the correct error stanza
    escalus:assert(is_stream_error, [<<"conflict">>, <<>>],
                   escalus_connection:get_stanza(Alice, close_old_stream)),
    escalus:assert(is_stream_end,
                   escalus_connection:get_stanza(Alice, close_old_stream)),
    true = escalus_connection:wait_for_close(Alice,timer:seconds(5)),
    true = escalus_connection:is_connected(Alice2),
    escalus_connection:stop(Alice2).

session_resumed_and_old_session_dead_doesnt_route_error_to_new_session(Config) ->
    %% GIVEN USER WITH STREAM RESUMPTION ENABLED
    {Alice, AliceSpec, SMID, SMH} =
        get_stream_resumption_enabled_fresh_user_smid_and_h(Config, alice),
    %% WHEN FIRST SESSION DIES AND USER RESUMES FROM NEW CLIENT
    escalus_connection:kill(Alice),
    Steps2 = connection_steps_to_stream_resumption(SMID, SMH),
    {ok, Alice2, _} = escalus_connection:start(AliceSpec, Steps2),
    process_initial_stanza(Alice2),
    %% THEN new session does not have any message rerouted
    false = escalus_client:has_stanzas(Alice2),
    true = escalus_connection:is_connected(Alice2),
    escalus_connection:stop(Alice2).

basic_ack(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Steps = connection_steps_to_enable_stream_mgmt(after_session),
    {ok, Alice, _} = escalus_connection:start(AliceSpec, Steps),
    escalus_connection:send(Alice, escalus_stanza:roster_get()),
    escalus:assert(is_roster_result,
                   escalus_connection:get_stanza(Alice, roster_result)),
    escalus_connection:send(Alice, escalus_stanza:sm_request()),
    escalus:assert(is_sm_ack,
                   escalus_connection:get_stanza(Alice, stream_mgmt_ack)).

%% Test that "h" value is valid when:
%% - SM is enabled *before* the session is established
%% - <r/> is sent *before* the session is established
h_ok_before_session(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Steps = connection_steps_to_enable_stream_mgmt(after_bind),
    {ok, Alice, _} = escalus_connection:start(AliceSpec, Steps),
    escalus_connection:send(Alice, escalus_stanza:sm_request()),
    escalus:assert(is_sm_ack, [0],
                   escalus_connection:get_stanza(Alice, stream_mgmt_ack)).

%% Test that "h" value is valid when:
%% - SM is enabled *before* the session is established
%% - <r/> is sent *after* the session is established
h_ok_after_session_enabled_before_session(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Steps = connection_steps_to_enable_stream_mgmt(after_bind) ++ [session],
    {ok, Alice, _} = escalus_connection:start(AliceSpec, Steps),
    escalus_connection:send(Alice, escalus_stanza:sm_request()),
    escalus:assert(is_sm_ack, [1],
                   escalus_connection:get_stanza(Alice, stream_mgmt_ack)).

%% Test that "h" value is valid when:
%% - SM is enabled *after* the session is established
%% - <r/> is sent *after* the session is established
h_ok_after_session_enabled_after_session(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Steps = connection_steps_to_enable_stream_mgmt(after_session),
    {ok, Alice, _} = escalus_connection:start(AliceSpec, Steps),
    escalus_connection:send(Alice, escalus_stanza:roster_get()),
    escalus:assert(is_roster_result,
                   escalus_connection:get_stanza(Alice, roster_result)),
    escalus_connection:send(Alice, escalus_stanza:sm_request()),
    escalus:assert(is_sm_ack, [1],
                   escalus_connection:get_stanza(Alice, stream_mgmt_ack)).

%% Test that "h" value is valid after exchanging a few messages.
h_ok_after_a_chat(ConfigIn) ->
    Config = escalus_users:update_userspec(ConfigIn, alice,
                                           stream_management, true),
    escalus:fresh_story(Config, [{alice,1}, {bob,1}], fun(Alice, Bob) ->
        NDiscarded = discard_vcard_update(Alice),
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Hi, Bob!">>)),
        escalus:assert(is_chat_message, [<<"Hi, Bob!">>],
                       escalus:wait_for_stanza(Bob)),
        escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, Alice!">>)),
        escalus:assert(is_chat_message, [<<"Hi, Alice!">>],
                       escalus:wait_for_stanza(Alice)),
        escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"How's life?">>)),
        escalus:assert(is_chat_message, [<<"How's life?">>],
                       escalus:wait_for_stanza(Alice)),
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Pretty !@#$%^$">>)),
        escalus:assert(is_chat_message, [<<"Pretty !@#$%^$">>],
                       escalus:wait_for_stanza(Bob)),
        escalus:send(Alice, escalus_stanza:sm_request()),
        escalus:assert(is_sm_ack, [3], escalus:wait_for_stanza(Alice)),
        %% Ack, so that unacked messages don't go into offline store.
        escalus:send(Alice, escalus_stanza:sm_ack(3 + NDiscarded))
    end).

h_non_given_closes_stream_gracefully(ConfigIn) ->
    AStanza = #xmlel{name = <<"a">>,
               attrs = [{<<"xmlns">>, <<"urn:xmpp:sm:3">>}]},
    Config = escalus_users:update_userspec(ConfigIn, alice,
                                           stream_management, true),
    escalus:fresh_story(Config, [{alice,1}], fun(Alice) ->
        C2SPid = mongoose_helper:get_session_pid(Alice, mim()),
        escalus:send(Alice, AStanza),
        escalus:assert(is_stream_error,
                       [<<"policy-violation">>, <<>>],
                       escalus:wait_for_stanza(Alice)),
        mongoose_helper:wait_for_pid_to_die(C2SPid),
        escalus:assert(is_stream_end, escalus_connection:get_stanza(Alice, stream_end)),
        true = escalus_connection:wait_for_close(Alice,timer:seconds(5))
    end).

client_acks_more_than_sent(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    {ok, Alice, _} = escalus_connection:start(AliceSpec),
    escalus:send(Alice, escalus_stanza:sm_ack(5)),
    StreamErrorStanza = escalus:wait_for_stanza(Alice),
    %% Assert "undefined-condition" children
    escalus:assert(is_stream_error, [<<"undefined-condition">>, <<>>], StreamErrorStanza),
    %% Assert "handled-count-too-high" children with correct attributes
    HandledCountSubElement = exml_query:path(StreamErrorStanza,
                                             [{element_with_ns,
                                               <<"handled-count-too-high">>,
                                               <<"urn:xmpp:sm:3">>}]),
    <<"5">> = exml_query:attr(HandledCountSubElement, <<"h">>),
    <<"0">> = exml_query:attr(HandledCountSubElement, <<"send-count">>),
    %% Assert graceful stream end
    escalus:assert(is_stream_end, escalus_connection:get_stanza(Alice, stream_end)),
    true = escalus_connection:wait_for_close(Alice,timer:seconds(5)).

too_many_unacked_stanzas(Config) ->
    {Bob, _} = given_fresh_user(Config, bob),
    {Alice, _} = given_fresh_user(Config, alice),
    escalus:wait_for_stanza(Alice), %% wait for ack request
    [escalus:send(Bob, escalus_stanza:chat_to(Alice,
        <<(integer_to_binary(N))/binary, ": Hi, Alice!">>))
     || N <- lists:seq(1,?SMALL_SM_BUFFER)],
    escalus:wait_for_stanzas(Alice, ?SMALL_SM_BUFFER * 2), % messages and ack requests
    escalus:assert(is_stream_error, [<<"resource-constraint">>,
                                     <<"too many unacked stanzas">>],
                   %% wait for deffered buffer check
                   escalus:wait_for_stanza(Alice, ?CONSTRAINT_CHECK_TIMEOUT + 1000)).

server_requests_ack(Config) ->
    server_requests_ack(Config, 1).

server_requests_ack(Config, N) ->
    {Bob, _} = given_fresh_user(Config, bob),
    {Alice, _} = given_fresh_user(Config, alice),
    %% ack request after initial presence
    maybe_assert_ack_request(1, N, Alice),
    StanzasRec = maybe_discard_vcard_update(1, N, Alice),
    ct:print("discarded"),
    escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, Alice!">>)),
    escalus:assert(is_chat_message, [<<"Hi, Alice!">>],
                   escalus:wait_for_stanza(Alice)),
    maybe_assert_ack_request(StanzasRec + 1, N, Alice).

maybe_assert_ack_request(StanzasRec, AckRequests, Alice) ->
    ct:print("StanzasRec: ~p, AckRequests: ~p", [StanzasRec, AckRequests]),
    case StanzasRec rem AckRequests of
        0 ->
            escalus:assert(is_sm_ack_request, escalus:wait_for_stanza(Alice));
        _ ->
            ok
    end,
    StanzasRec.

maybe_discard_vcard_update(StanzasRec, AckFreq, Alice) ->
    case discard_vcard_update(Alice) of
        0 ->
            StanzasRec;
        1 ->
            maybe_assert_ack_request(StanzasRec + 1, AckFreq, Alice)
    end.

server_requests_ack_freq_2(Config) ->
    Config1 = escalus_users:update_userspec(Config, alice, manual_ack, true),
    server_requests_ack(Config1, 2).

server_requests_ack_after_session(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Steps = connection_steps_to_enable_stream_mgmt(after_bind) ++ [session],
    {ok, Alice, _} = escalus_connection:start(AliceSpec, Steps),
    escalus:assert(is_sm_ack_request, escalus_connection:get_stanza(Alice, stream_mgmt_req)).


resend_more_offline_messages_than_buffer_size(Config) ->
    ConnSteps = connection_steps_to_session(),

    %% connect bob and alice
    BobSpec = escalus_fresh:create_fresh_user(Config, bob),
    {ok, Bob, _} = escalus_connection:start(BobSpec),
    escalus_connection:send(Bob, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Bob, presence),
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),

    % sent some messages - more than unacked buffer size
    MessagesToSend = ?SMALL_SM_BUFFER + 1,
    JID = common_helper:get_bjid(AliceSpec),
    [escalus_connection:send(Bob, escalus_stanza:chat_to(JID, integer_to_binary(I)))
     || I <- lists:seq(1, MessagesToSend)],

    % connect alice who wants to receive all messages from offline storage
    {ok, Alice, _} = escalus_connection:start(AliceSpec, ConnSteps++[stream_management]),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),

    escalus:wait_for_stanzas(Alice, MessagesToSend * 2), %messages and ack requests

    escalus_connection:get_stanza(Alice, presence),
    escalus:wait_for_stanza(Alice), % ack request

    % confirm messages + presence
    escalus_connection:send(Alice, escalus_stanza:sm_ack(4)),
    % wait for check constraint message on server side

    ct:sleep(?CONSTRAINT_CHECK_TIMEOUT + 1000),
    false = escalus_client:has_stanzas(Alice),
    % should not receive anything especially any stream errors

    escalus_connection:stop(Alice),
    escalus_connection:stop(Bob).

resend_unacked_on_reconnection(Config) ->
    Messages = [<<"msg-1">>, <<"msg-2">>, <<"msg-3">>],
    {Bob, _} = given_fresh_user(Config, bob),
    {Alice, AliceSpec0} = given_fresh_user(Config, alice),
    discard_vcard_update(Alice),
    %% Bob sends some messages to Alice.
    [escalus:send(Bob, escalus_stanza:chat_to(Alice, Msg))
     || Msg <- Messages],
    %% Alice receives the messages.
    Stanzas = escalus:wait_for_stanzas(Alice, length(Messages)),
    [escalus:assert(is_chat_message, [Msg], Stanza)
     || {Msg, Stanza} <- lists:zip(Messages, Stanzas)],
    %% Alice disconnects without acking the messages.
    C2SRef = monitor_session(Alice),
    escalus_connection:stop(Alice),
    %% TODO There's a race condition between the C2S process and mod_offline,
    %% so we have to wait for C2S termination.
    %% For details please see https://github.com/esl/MongooseIM/pull/2007
    ok = wait_for_process_termination(C2SRef),
    escalus_connection:stop(Bob),
    %% Messages go to the offline store.
    %% Alice receives the messages from the offline store.
    AliceSpec = [{manual_ack, true} | AliceSpec0],
    {ok, NewAlice, _} = escalus_connection:start(AliceSpec),
    escalus_connection:send(NewAlice, escalus_stanza:presence(<<"available">>)),
    OfflineMsgs = [escalus_connection:get_stanza(NewAlice, {msg, I})
                   || I <- lists:seq(1, length(Messages))],
    [escalus:assert(is_chat_message, [Msg], Stanza)
     || {Msg, Stanza} <- lists:zip(Messages, OfflineMsgs)],
    %% Alice acks the delayed messages so they won't go again
    %% to the offline store.
    escalus_connection:send(NewAlice, escalus_stanza:sm_ack(3)).

preserve_order(Config) ->
    ConnSteps = connection_steps_to_session(),

    %% connect bob and alice
    BobSpec = escalus_fresh:create_fresh_user(Config, bob),
    {ok, Bob, _} = escalus_connection:start(BobSpec),
    escalus_connection:send(Bob, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Bob, presence),

    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    {ok, Alice, _} = escalus_connection:start(AliceSpec, ConnSteps++[stream_resumption]),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Alice, presence),

    escalus:assert(is_sm_ack_request, escalus_connection:get_stanza(Alice, ack)),
    escalus_connection:send(Bob, escalus_stanza:chat_to(common_helper:get_bjid(AliceSpec), <<"1">>)),

    %% kill alice connection
    escalus_connection:kill(Alice),
    wait_until_disconnected(AliceSpec),

    escalus_connection:send(Bob, escalus_stanza:chat_to(common_helper:get_bjid(AliceSpec), <<"2">>)),
    escalus_connection:send(Bob, escalus_stanza:chat_to(common_helper:get_bjid(AliceSpec), <<"3">>)),

    {ok, NewAlice, _} = escalus_connection:start(AliceSpec, ConnSteps),
    escalus_connection:send(NewAlice, escalus_stanza:enable_sm([resume])),

    escalus_connection:send(Bob, escalus_stanza:chat_to(common_helper:get_bjid(AliceSpec), <<"4">>)),
    escalus_connection:send(Bob, escalus_stanza:chat_to(common_helper:get_bjid(AliceSpec), <<"5">>)),

    escalus_connection:send(NewAlice, escalus_stanza:presence(<<"available">>)),
    escalus_connection:send(Bob, escalus_stanza:chat_to(common_helper:get_bjid(AliceSpec), <<"6">>)),

    receive_all_ordered(NewAlice,1),

    % replace connection
    {ok, NewAlice2, _} = escalus_connection:start(AliceSpec, ConnSteps),
    % allow messages to go to the offline storage
    ct:sleep(1000),

    escalus_connection:send(NewAlice2, escalus_stanza:presence(<<"available">>)),

    % receves messages in correct order
    receive_all_ordered(NewAlice2, 1),

    escalus_connection:stop(Bob),
    escalus_connection:stop(NewAlice2).

receive_all_ordered(Conn, N) ->
    case catch escalus_connection:get_stanza(Conn, msg) of
        #xmlel{} = Stanza ->
            NN = case Stanza#xmlel.name of
                     <<"message">> ->
                         %ct:pal("~p~n", [Stanza]),
                         escalus:assert(is_chat_message, [integer_to_binary(N)], Stanza),
                         N + 1;
                     _ ->
                         N
                 end,
            receive_all_ordered(Conn, NN);
        _Error ->
            ok
    end.

resend_unacked_after_resume_timeout(Config) ->
    ConnSteps = connection_steps_to_session(),

    %% connect bob and alice
    BobSpec = escalus_fresh:create_fresh_user(Config, bob),
    {ok, Bob, _} = escalus_connection:start(BobSpec),
    escalus_connection:send(Bob, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Bob, presence),

    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    {ok, Alice, _} = escalus_connection:start(AliceSpec, ConnSteps++[stream_resumption]),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Alice, presence),

    escalus:assert(is_sm_ack_request, escalus_connection:get_stanza(Alice, ack)),

    escalus_connection:send(Bob, escalus_stanza:chat_to(common_helper:get_bjid(AliceSpec), <<"msg-1">>)),
    %% kill alice connection
    escalus_connection:kill(Alice),

    %% ensure there is no session
    wait_until_disconnected(AliceSpec),
    0 = length(get_user_alive_resources(AliceSpec)),

    %% alice come back and receives unacked message
    {ok, NewAlice, _} = escalus_connection:start(AliceSpec, ConnSteps),
    escalus_connection:send(NewAlice, escalus_stanza:presence(<<"available">>)),

    Stanzas =[escalus_connection:get_stanza(NewAlice, msg),
              escalus_connection:get_stanza(NewAlice, msg)],

    escalus_new_assert:mix_match([is_presence,
                                  is_chat(<<"msg-1">>)],
                                 Stanzas),

    escalus_connection:stop(Bob),
    escalus_connection:stop(NewAlice).

resume_expired_session_returns_correct_h(Config) ->
    %% connect bob and alice
    {Bob, _, _, _} =
        get_stream_resumption_enabled_fresh_user_smid_and_h(Config, bob),
    {Alice, AliceSpec, SMID, SMH} =
        get_stream_resumption_enabled_fresh_user_smid_and_h(Config, alice),
    escalus:assert(is_sm_ack_request, escalus_connection:get_stanza(Alice, ack)),
    %% Bob sends a message to Alice, and Alice receives it but doesn't acknowledge
    escalus_connection:send(Bob, escalus_stanza:chat_to(common_helper:get_bjid(AliceSpec), <<"msg-1">>)),
    escalus:wait_for_stanza(Alice),
    %% kill alice connection
    escalus_connection:kill(Alice),
    %% ensure there is no session
    wait_until_disconnected(AliceSpec),
    %% alice comes back, but too late, so resumption doesn't work,
    %% but she receives the previous h = 1 anyway
    {ok, NewAlice, _} = escalus_connection:start(AliceSpec, connection_steps_to_authenticate()),
    escalus_connection:send(NewAlice, escalus_stanza:resume(SMID, SMH)),
    FailedResumption = escalus_connection:get_stanza(NewAlice, failed_resumption),
    <<"1">> = exml_query:attr(FailedResumption, <<"h">>),
    %% And we can continue with bind and session
    escalus_session:session(escalus_session:bind(NewAlice)),
    escalus_connection:send(NewAlice, escalus_stanza:presence(<<"available">>)),
    Stanzas = [escalus_connection:get_stanza(NewAlice, msg),
               escalus_connection:get_stanza(NewAlice, msg)],
    escalus_new_assert:mix_match([is_presence, is_chat(<<"msg-1">>)], Stanzas),
    escalus_connection:stop(Bob),
    escalus_connection:stop(NewAlice).

gc_repeat_after_never_means_no_cleaning(Config) ->
    true = rpc(mim(), ?MOD_SM, set_stale_h_repeat_after, [?BIG_BIG_BIG_TIMEOUT]),
    [{SMID1, _}, {SMID2, _}, {SMID3, _}] = ?config(smid_test, Config),
    {stale_h, 1} = rpc(mim(), ?MOD_SM, get_session_from_smid, [SMID1]),
    {stale_h, 2} = rpc(mim(), ?MOD_SM, get_session_from_smid, [SMID2]),
    {stale_h, 3} = rpc(mim(), ?MOD_SM, get_session_from_smid, [SMID3]).
gc_repeat_after_timeout_does_clean(Config) ->
    [{SMID1, _} | _ ] = ?config(smid_test, Config),
    mongoose_helper:wait_until(fun() ->
                                       rpc(mim(), ?MOD_SM, get_stale_h, [SMID1])
                               end,
                               {error, smid_not_found},
                               #{name => smid_garbage_collected}).

resume_session_state_send_message(Config) ->
    ConnSteps = connection_steps_to_session(),

    %% connect bob and alice

    BobSpec = escalus_fresh:create_fresh_user(Config, bob),
    {ok, Bob, _} = escalus_connection:start(BobSpec),
    escalus_connection:send(Bob, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Bob, presence),

    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    {ok, Alice, _} = escalus_connection:start(AliceSpec, ConnSteps++[stream_resumption]),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Alice, presence),
    %% Ack the presence stanza
    escalus:assert(is_sm_ack_request, escalus_connection:get_stanza(Alice, ack)),
    escalus:send(Alice, escalus_stanza:sm_ack(1)),

    escalus_connection:send(Bob, escalus_stanza:chat_to(common_helper:get_bjid(AliceSpec), <<"msg-1">>)),
    %% kill alice connection
    {ok, C2SPid} = get_session_pid(AliceSpec, escalus_client:resource(Alice)),
    escalus_connection:kill(Alice),
    wait_for_c2s_state_change(C2SPid, resume_session),

    1 = length(get_user_alive_resources(AliceSpec)),

    %% send some messages and check if c2s can handle it
    escalus_connection:send(Bob, escalus_stanza:chat_to(common_helper:get_bjid(AliceSpec), <<"msg-2">>)),
    escalus_connection:send(Bob, escalus_stanza:chat_to(common_helper:get_bjid(AliceSpec), <<"msg-3">>)),

    %% alice comes back and receives unacked message
    {ok, NewAlice, _} = escalus_connection:start(AliceSpec, ConnSteps),
    escalus_connection:send(NewAlice, escalus_stanza:presence(<<"available">>)),

    Stanzas = [escalus_connection:get_stanza(NewAlice, msg) || _ <- lists:seq(1,4) ],

    % what about order ?
    % alice receive presence from herself and 3 unacked messages from bob
    escalus_new_assert:mix_match([is_presence,
                                  is_chat(<<"msg-1">>),
                                  is_chat(<<"msg-2">>),
                                  is_chat(<<"msg-3">>)],
                                 Stanzas),
    escalus_connection:stop(Bob),
    escalus_connection:stop(NewAlice).

%%for instance it can be done by mod ping
resume_session_state_stop_c2s(Config) ->
    ConnSteps = connection_steps_to_session(),

    %% connect bob and alice
    BobSpec = escalus_fresh:create_fresh_user(Config, bob),
    {ok, Bob, _} = escalus_connection:start(BobSpec, ConnSteps),
    escalus_connection:send(Bob, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Bob, presence),

    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    {ok, Alice, _} = escalus_connection:start(AliceSpec, ConnSteps++[stream_resumption]),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Alice, presence),

    escalus:assert(is_sm_ack_request, escalus_connection:get_stanza(Alice, ack)),
    %% Ack presence
    escalus_connection:send(Alice, escalus_stanza:sm_ack(1)),

    escalus_connection:send(Bob, escalus_stanza:chat_to(common_helper:get_bjid(AliceSpec), <<"msg-1">>)),

    %% get pid of c2s
    {ok, C2SPid} = get_session_pid(AliceSpec, escalus_client:resource(Alice)),
    %% Wait c2s process to process our presence ack.
    %% Otherwise, we can receive two initial presences sometimes.
    wait_for_c2s_unacked_count(C2SPid, 1),

    % kill alice connection
    escalus_connection:kill(Alice),
    % session should be alive
    1 = length(get_user_alive_resources(AliceSpec)),
    rpc(mim(), ejabberd_c2s, stop, [C2SPid] ),
    wait_for_c2s_state_change(C2SPid, resume_session),

    %% alice comes back and receives unacked message
    {ok, NewAlice, _} = escalus_connection:start(AliceSpec, ConnSteps),
    escalus_connection:send(NewAlice, escalus_stanza:presence(<<"available">>)),

    Stanzas = [escalus_connection:get_stanza(NewAlice, msg),
               escalus_connection:get_stanza(NewAlice, msg)],
    escalus_new_assert:mix_match([is_presence, is_chat(<<"msg-1">>)], Stanzas),

    escalus_connection:stop(NewAlice),
    escalus_connection:stop(Bob).

%% This test only verifies the validity of helpers (get_session_pid,
%% assert_no_offline_msgs, assert_c2s_state) written for wait_for_resumption
%% testcase.
session_established(Config) ->
    AliceSpec = [{manual_ack, true}
                 | escalus_fresh:create_fresh_user(Config, alice)],
    {Alice, _} = given_fresh_user_with_spec(AliceSpec),
    {ok, C2SPid} = get_session_pid(AliceSpec, server_string("escalus-default-resource")),
    assert_no_offline_msgs(AliceSpec),
    assert_c2s_state(C2SPid, session_established),
    escalus_connection:stop(Alice).

%% Ensure that after a violent disconnection,
%% the c2s waits for resumption (but don't resume yet).
wait_for_resumption(Config) ->
    AliceSpec = [{manual_ack, true}
                 | escalus_fresh:create_fresh_user(Config, alice)],
    {Bob, _} = given_fresh_user(Config, bob),
    Messages = [<<"msg-1">>, <<"msg-2">>, <<"msg-3">>],
    {C2SPid, _} = buffer_unacked_messages_and_die(Config, AliceSpec, Bob, Messages),
    %% Ensure the c2s process is waiting for resumption.
    assert_no_offline_msgs(AliceSpec),
    wait_for_c2s_state_change(C2SPid, resume_session).

unacknowledged_message_hook_resume(Config) ->
    unacknowledged_message_hook_common(fun unacknowledged_message_hook_resume/4, Config).

unacknowledged_message_hook_resume(AliceSpec, Resource, SMID, _C2SPid) ->
    {ok, NewAlice, _} = escalus_connection:start(AliceSpec, connection_steps_to_stream_resumption(SMID, 1)),
    escalus_connection:send(NewAlice, escalus_stanza:presence(<<"available">>)),
    {Resource, NewAlice}.

unacknowledged_message_hook_bounce(Config) ->
    unacknowledged_message_hook_common(fun unacknowledged_message_hook_bounce/4, Config).

unacknowledged_message_hook_bounce(AliceSpec, Resource, _SMID, C2SPid) ->
    ConnSteps = connection_steps_to_session() ++ [stream_resumption],
    NewResource = <<"new_", Resource/binary>>,
    NewSpec = lists:keystore(resource, 1, AliceSpec, {resource, NewResource}),
    {ok, NewAlice, _} = escalus_connection:start(NewSpec, ConnSteps),
    escalus_connection:send(NewAlice, escalus_stanza:presence(<<"available">>)),
    %% ensure second C2S is registered so all the messages are bounced properly
    mongoose_helper:wait_until(fun() -> length(get_user_alive_resources(AliceSpec)) end, 2),
    ok = rpc(mim(), sys, terminate, [C2SPid, normal]),
    {NewResource, NewAlice}.

unacknowledged_message_hook_offline(Config) ->
    unacknowledged_message_hook_common(fun unacknowledged_message_hook_offline/4, Config).

unacknowledged_message_hook_offline(AliceSpec, Resource, _SMID, C2SPid) ->
    ConnSteps = connection_steps_to_session() ++ [stream_resumption],
    C2SRef = erlang:monitor(process, C2SPid),
    %%reset the session, so old C2S process is stopped
    {ok, NewAlice, _} = escalus_connection:start(AliceSpec, ConnSteps),
    %% wait for old C2S termination before send presence. other way
    %% some of the latest unacknowledged messages can be bounced to
    %% the new C2S process instead of going to the mod_offline storage.
    %% looks like all the unacknowledged messages arrive to the new
    %% C2S, but the message sequence is broken (the bounced messages
    %% delivered before the messages from the mod_offline storage)
    wait_for_process_termination(C2SRef),
    escalus_connection:send(NewAlice, escalus_stanza:presence(<<"available">>)),
    {Resource, NewAlice}.

unacknowledged_message_hook_common(RestartConnectionFN, Config) ->
    ConnSteps = connection_steps_to_session(),

    %% connect bob and alice
    BobSpec = escalus_fresh:create_fresh_user(Config, bob),
    {ok, Bob, _} = escalus_connection:start(BobSpec),
    escalus_connection:send(Bob, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Bob, presence),

    AliceSpec0 = escalus_fresh:create_fresh_user(Config, alice),
    Resource = proplists:get_value(username, AliceSpec0),
    AliceSpec = [{resource, Resource} | AliceSpec0],
    start_hook_listener(Resource),
    {ok, Alice, _} = escalus_connection:start(AliceSpec, ConnSteps ++ [stream_resumption]),
    SMID = proplists:get_value(smid, Alice#client.props),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Alice, presence),
    %% Ack the presence stanza
    escalus:assert(is_sm_ack_request, escalus_connection:get_stanza(Alice, ack)),
    escalus:send(Alice, escalus_stanza:sm_ack(1)),

    escalus_connection:send(Bob, escalus_stanza:chat_to(common_helper:get_bjid(AliceSpec), <<"msg-1">>)),
    escalus_connection:send(Bob, escalus_stanza:chat_to(common_helper:get_bjid(AliceSpec), <<"msg-2">>)),
    %% kill alice connection
    {ok, C2SPid} = get_session_pid(AliceSpec, Resource),
    escalus_connection:kill(Alice),
    wait_for_c2s_state_change(C2SPid, resume_session),
    1 = length(get_user_alive_resources(AliceSpec)),

    escalus:assert(is_chat_message, [<<"msg-1">>], wait_for_unacked_msg_hook(0, Resource, 100)),
    escalus:assert(is_chat_message, [<<"msg-2">>], wait_for_unacked_msg_hook(0, Resource, 100)),
    ?assertEqual(timeout, wait_for_unacked_msg_hook(0, Resource, 100)),

    %% send some messages and check if c2s can handle it
    escalus_connection:send(Bob, escalus_stanza:chat_to(common_helper:get_bjid(AliceSpec), <<"msg-3">>)),
    escalus_connection:send(Bob, escalus_stanza:chat_to(common_helper:get_bjid(AliceSpec), <<"msg-4">>)),
    escalus:assert(is_chat_message, [<<"msg-3">>], wait_for_unacked_msg_hook(0, Resource, 100)),
    escalus:assert(is_chat_message, [<<"msg-4">>], wait_for_unacked_msg_hook(0, Resource, 100)),
    ?assertEqual(timeout, wait_for_unacked_msg_hook(0, Resource, 100)),

    %% alice comes back and receives unacked message
    {NewResource, NewAlice} = RestartConnectionFN(AliceSpec, Resource, SMID, C2SPid),

    mongoose_helper:wait_until(
        fun() ->
            Stanza = escalus_connection:get_stanza(NewAlice, msg),
            escalus:assert(is_chat_message, [<<"msg-4">>], Stanza),
            ok
        end, ok),

    {ok, NewC2SPid} = get_session_pid(AliceSpec, NewResource),
    escalus_connection:kill(NewAlice),
    wait_for_c2s_state_change(NewC2SPid, resume_session),

    escalus:assert(is_chat_message, [<<"msg-1">>], wait_for_unacked_msg_hook(1, NewResource, 100)),
    escalus:assert(is_chat_message, [<<"msg-2">>], wait_for_unacked_msg_hook(1, NewResource, 100)),
    escalus:assert(is_chat_message, [<<"msg-3">>], wait_for_unacked_msg_hook(1, NewResource, 100)),
    escalus:assert(is_chat_message, [<<"msg-4">>], wait_for_unacked_msg_hook(1, NewResource, 100)),
    ?assertEqual(timeout, wait_for_unacked_msg_hook(0, Resource, 100)),

    escalus_connection:stop(Bob).

resume_session(Config) ->
    AliceSpec = [{manual_ack, true}
                 | escalus_fresh:create_fresh_user(Config, alice)],
    Messages = [<<"msg-1">>, <<"msg-2">>, <<"msg-3">>],
    escalus:fresh_story(Config, [{bob, 1}], fun(Bob) ->
        {_, SMID} = buffer_unacked_messages_and_die(Config, AliceSpec, Bob, Messages),
        %% Resume the session.
        Steps = connection_steps_to_stream_resumption(SMID, 2),
        {ok, Alice, _} = escalus_connection:start(AliceSpec, Steps),
        NDiscarded = discard_vcard_update(Alice),
        %% Alice receives the unacked messages from the previous
        %% interrupted session.
        Stanzas = [escalus_connection:get_stanza(Alice, {msg, I})
                   || I <- lists:seq(1, 3)],
        [escalus:assert(is_chat_message, [Msg], Stanza)
         || {Msg, Stanza} <- lists:zip(Messages, Stanzas)],
        %% Alice acks the received messages.
        escalus_connection:send(Alice, escalus_stanza:sm_ack(5 + NDiscarded)),
        escalus_connection:stop(Alice)
    end).

resume_session_with_wrong_h_does_not_leak_sessions(Config) ->
    AliceSpec = [{manual_ack, true}
                 | escalus_fresh:create_fresh_user(Config, alice)],
    Messages = [<<"msg-1">>, <<"msg-2">>, <<"msg-3">>],
    escalus:fresh_story(Config, [{bob, 1}], fun(Bob) ->

        {_, SMID} = buffer_unacked_messages_and_die(Config, AliceSpec, Bob, Messages),
        %% Resume the session.
        Steps = connection_steps_to_authenticate(),
        {ok, Alice, _} = escalus_connection:start(AliceSpec, Steps),
        Resumed = try_to_resume_stream(Alice, SMID, 30),
        escalus:assert(is_stream_error, [<<"undefined-condition">>, <<>>], Resumed),

        [] = get_user_present_resources(AliceSpec),
        {error, smid_not_found} = get_sid_by_stream_id(SMID),
        escalus_connection:wait_for_close(Alice, timer:seconds(5))
    end).

resume_session_with_wrong_sid_returns_item_not_found(Config) ->
    session_resumption_expects_item_not_found(Config, <<"wrong-sid">>).

resume_session_with_wrong_namespace_is_a_noop(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Steps = connection_steps_to_authenticate(),
    {ok, Alice, _} = escalus_connection:start(AliceSpec, Steps),
    #xmlel{attrs = Attrs} = Resume = escalus_stanza:resume(<<"doesnt_matter">>, 4),
    Attrs2 = lists:keyreplace(<<"xmlns">>, 1, Attrs, {<<"xmlns">>, <<"not-stream-mgnt">>}),
    escalus_connection:send(Alice, Resume#xmlel{attrs = Attrs2}),
    escalus_assert:has_no_stanzas(Alice),
    [] = get_user_present_resources(AliceSpec),
    true = escalus_connection:is_connected(Alice),
    escalus_connection:stop(Alice).

resume_dead_session_results_in_item_not_found(Config) ->
    SMID = base64:encode(crypto:strong_rand_bytes(21)),
    SID = {os:timestamp(), undefined},
    rpc(mim(), ?MOD_SM, register_smid, [SMID, SID]),
    session_resumption_expects_item_not_found(Config, SMID).

session_resumption_expects_item_not_found(Config, SMID) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Steps = connection_steps_to_authenticate(),
    {ok, Alice, _} = escalus_connection:start(AliceSpec, Steps),
    Resumed = try_to_resume_stream(Alice, SMID, 2),
    escalus:assert(is_sm_failed, [<<"item-not-found">>], Resumed),
    [] = get_user_present_resources(AliceSpec),
    true = escalus_connection:is_connected(Alice),
    escalus_connection:stop(Alice).


connection_steps_to_authenticate() ->
    [start_stream,
     stream_features,
     maybe_use_ssl,
     authenticate].

connection_steps_to_bind() ->
    connection_steps_to_authenticate() ++ [bind].

connection_steps_to_session() ->
    connection_steps_to_bind() ++ [session].

connection_steps_to_enable_stream_mgmt(after_session) ->
    connection_steps_to_session() ++ [stream_management];
connection_steps_to_enable_stream_mgmt(after_bind) ->
    connection_steps_to_bind() ++ [stream_management].

connection_steps_to_enable_stream_resumption() ->
    connection_steps_to_bind() ++ [session, stream_resumption].

connection_steps_to_stream_resumption(SMID, H) ->
    connection_steps_to_authenticate() ++ [mk_resume_stream(SMID, H)].

mk_resume_stream(SMID, PrevH) ->
    fun (Conn = #client{props = Props}, Features) ->
            Resumed = try_to_resume_stream(Conn, SMID, PrevH),
            true = escalus_pred:is_sm_resumed(SMID, Resumed),
            {Conn#client{props = [{smid, SMID} | Props]}, Features}
    end.

try_to_resume_stream(Conn, SMID, PrevH) ->
    escalus_connection:send(Conn, escalus_stanza:resume(SMID, PrevH)),
    escalus_connection:get_stanza(Conn, get_resumed).

buffer_unacked_messages_and_die(Config, AliceSpec, Bob, Messages) ->
    Steps = connection_steps_to_enable_stream_resumption(),
    {ok, Alice = #client{props = Props}, _} = escalus_connection:start(AliceSpec, Steps),
    JID = common_helper:get_bjid(Props),
    InitialPresence = setattr(escalus_stanza:presence(<<"available">>),
                              <<"id">>, <<"presence1">>),
    escalus_connection:send(Alice, InitialPresence),
    Presence = escalus_connection:get_stanza(Alice, presence1),
    escalus:assert(is_presence, Presence),
    Res = server_string("escalus-default-resource"),
    {ok, C2SPid} = get_session_pid(AliceSpec, Res),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    _Presence = escalus_connection:get_stanza(Alice, presence2),
    discard_vcard_update(Alice),
    %% Bobs sends some messages to Alice.
    [escalus:send(Bob, escalus_stanza:chat_to(JID, Msg))
     || Msg <- Messages],
    %% Alice receives them, but doesn't ack.
    Stanzas = [escalus_connection:get_stanza(Alice, {msg, I})
               || I <- lists:seq(1, 3)],
    [escalus:assert(is_chat_message, [Msg], Stanza)
     || {Msg, Stanza} <- lists:zip(Messages, Stanzas)],
    %% Alice's connection is violently terminated.
    escalus_client:kill_connection(Config, Alice),
    {C2SPid, proplists:get_value(smid, Props)}.

aggressively_pipelined_resume(Config) ->
    AliceSpec = [{manual_ack, true}, {parser_opts, [{start_tag, <<"stream:stream">>}]}
                 | escalus_fresh:create_fresh_user(Config, alice)],
    UnackedMessages = [<<"msg-1">>, <<"msg-2">>, <<"msg-3">>],
    escalus:fresh_story(Config, [{bob, 1}], fun(Bob) ->
        {_, SMID} = buffer_unacked_messages_and_die(Config, AliceSpec, Bob, UnackedMessages),
        %% Resume the session.
        Alice = escalus_connection:connect(AliceSpec),

        Username = proplists:get_value(username, AliceSpec),
        Password = proplists:get_value(password, AliceSpec),
        Payload = <<0:8,Username/binary,0:8,Password/binary>>,
        Server = proplists:get_value(server, AliceSpec),

        Stream = escalus_stanza:stream_start(Server, <<"jabber:client">>),
        Auth = escalus_stanza:auth(<<"PLAIN">>, [#xmlcdata{content = base64:encode(Payload)}]),
        AuthStream = escalus_stanza:stream_start(Server, <<"jabber:client">>),
        Resume = escalus_stanza:resume(SMID, 2),

        escalus_client:send(Alice, [Stream, Auth, AuthStream, Resume]),
        Messages = [escalus_connection:get_stanza(Alice, {get_resumed, I}) || I <- lists:seq(1, 6)],
        escalus:assert(is_sm_resumed, [SMID], lists:last(Messages)),

        escalus_connection:stop(Alice)
    end).

%% This is a regression test for a case when a session processes a request, which will
%% receive a response from the server, i.e. will have the same origin SID in mongoose_acc.
%% Without proper handling, the reply would be rejected because the resumed session
%% has new SID.
replies_are_processed_by_resumed_session(Config) ->
    %% GIVEN a session and registered special IQ handler (added in init_per_testcase),
    %% that waits for old session process to terminate (at this point new process
    %% has fully taken over) and then actually sends the reply.
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Steps = connection_steps_to_enable_stream_resumption(),
    {ok, Alice = #client{props = Props}, _} = escalus_connection:start(AliceSpec, Steps),
    SMID = proplists:get_value(smid, Props),
    SMH = escalus_connection:get_sm_h(Alice),

    %% WHEN a client sends IQ request to the special handler...
    IQReq = escalus_stanza:iq_get(regression_ns(), []),
    escalus:send(Alice, IQReq),

    %% ... goes down and session is resumed.
    escalus_client:kill_connection(Config, Alice),
    Steps2 = connection_steps_to_stream_resumption(SMID, SMH),
    {ok, Alice2, _} = escalus_connection:start(AliceSpec, Steps2),

    %% THEN the client receives the reply properly.
    IQReply = escalus:wait_for_stanza(Alice2),
    escalus:assert(is_iq_result, [IQReq], IQReply),
    escalus_connection:stop(Alice2).

%% This is a regression test for a bug, which manifested in following scenario
%% (due to improper presence sub requests buffering):
%% 1. A is online, B is offline
%% 2. A subscribes to B's presence;
%% 3. B becomes online
%% 4. A sends a message to B
%% 5. B doesn't SM-ack the request or message, terminates the connection
%% 6. B reconnects but with session *replace*, not resume
%% 7. Packet rerouting crashes on the buffered sub request, preventing resending whole buffer
%% 8. B doesn't receive the buffered message
subscription_requests_are_buffered_properly(Config) ->
    AliceSpec = [{manual_ack, true} | escalus_fresh:create_fresh_user(Config, alice)],
    MsgBody = <<"buffered">>,
    SubPredFun = fun(S) -> escalus_pred:is_presence_with_type(<<"subscribe">>, S) end,
    AvailablePredFun = fun(S) -> escalus_pred:is_presence_with_type(<<"available">>, S) end,
    MsgPredFun = fun(S) -> escalus_pred:is_chat_message(MsgBody, S) end,

    escalus:fresh_story(Config, [{bob, 1}], fun(Bob) ->
        % GIVEN Bob's pending subscription to Alice's presence
        AliceUser = proplists:get_value(username, AliceSpec),
        AliceServer = proplists:get_value(server, AliceSpec),
        AliceJid = <<AliceUser/binary, $@, AliceServer/binary>>,
        escalus:send(Bob, escalus_stanza:presence_direct(AliceJid, <<"subscribe">>)),
        _RosterPushReq = escalus:wait_for_stanza(Bob),

        % WHEN Alice becomes online...
        Steps = connection_steps_to_enable_stream_resumption(),
        {ok, Alice, _} = escalus_connection:start(AliceSpec, Steps),
        InitialPresence = escalus_stanza:presence(<<"available">>),
        escalus_connection:send(Alice, InitialPresence),
        AvailableAndSubPresences = escalus:wait_for_stanzas(Alice, 2),
        escalus:assert_many([SubPredFun, AvailablePredFun], AvailableAndSubPresences),

        % ...and Bob sends a message to Alice...
        escalus:send(Bob, escalus_stanza:chat_to(Alice, MsgBody)),
        MsgStanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_chat_message, [MsgBody], MsgStanza),

        % ...and Alice terminates connection without acking anything...
        escalus_client:kill_connection(Config, Alice),

        % ...and reconnects with session replacement.
        {ok, Alice2, _} = escalus_connection:start(AliceSpec, connection_steps_to_session()),

        % THEN Alice receives (without sending initial presence):
        % * buffered available presence (because it's addressed to full JID)
        % * buffered Bob's message (like above)
        % Alice DOESN'T receive:
        % * buffered subscription request because it is dropped by ejabberd_sm
        %   because it's treated like repeated sub request to bare JID, so it's not
        %   processed by any sub req handler (like mod_roster)
        SubReqAndInitialPresence = escalus:wait_for_stanzas(Alice2, 2),
        escalus:assert_many([AvailablePredFun, MsgPredFun], SubReqAndInitialPresence),

        escalus_connection:stop(Alice2)
    end).

%% This is a regression test for a bug, due to which messages sent to old session
%% in a middle of state handover were not appended properly to SM buffer.
%% Scenario to reproduce:
%% 1. Online Bob and Alice
%% 2. Alice kills the connecion
%% 3. Alice's session is suspended
%% 4. Alice resumes session with new connection. At this moment new session is still not
%%    present in session table. `resume` request is stuck in old proc mailbox.
%% 5. Bob sends a message to Alice. Only old proc is present in session table so now
%%    old session has two messages in mailbox: `resume` and XML from Bob
%% 6. We resume old process and it begins session handover
%% 7. Bob's message is appended to SM buffer in "flush" step
%% 8. With bug fixed, the message is retransmitted properly
messages_are_properly_flushed_during_resumption(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    escalus:fresh_story(Config, [{bob, 1}], fun(Bob) ->
        % GIVEN (online Bob) and (Alice in resume state); Alice's session is suspended
        Steps = connection_steps_to_enable_stream_resumption(),
        {ok, Alice = #client{props = Props}, _} = escalus_connection:start(AliceSpec, Steps),
        SMID = proplists:get_value(smid, Props),
        InitialPresence = escalus_stanza:presence(<<"available">>),
        escalus_connection:send(Alice, InitialPresence),
        Presence = escalus:wait_for_stanza(Alice),
        escalus:assert(is_presence, Presence),
        SMH = escalus_connection:get_sm_h(Alice),
        escalus_client:kill_connection(Config, Alice),
        {ok, C2SPid} = get_session_pid(AliceSpec, server_string("escalus-default-resource")),
        ok = rpc(mim(), sys, suspend, [C2SPid]),

        % WHEN new session requests resumption
        % we wait until that old session has resumption request enqueued;
        % we need it to ensure the order of messages: resume first, Bob's chat second.
        % Actual wait and message sent by Bob is done in separate process
        % because new client start will block until old process is resumed

        MsgBody = <<"flush-regression">>,
        spawn(fun() ->
                      wait_for_queue_length(C2SPid, 1),

                      % Bob sends a message...
                      escalus:send(Bob, escalus_stanza:chat_to(Alice, MsgBody)),

                      % ...we ensure that a message is enqueued in Alice's session...
                      % (2 messages = resume request + Bob's message)
                      wait_for_queue_length(C2SPid, 2),

                      % ...and old process is resumed.
                      ok = rpc(mim(), sys, resume, [C2SPid])
              end),

        Steps2 = connection_steps_to_stream_resumption(SMID, SMH),
        {ok, Alice2, _} = escalus_connection:start(AliceSpec, Steps2),

        % THEN Alice's new session receives Bob's message
        RecvMsg = escalus:wait_for_stanza(Alice2),
        escalus:assert(is_chat_message, [MsgBody], RecvMsg)
      end).

messages_are_properly_flushed_during_resumption_p1_fsm_old(Config) ->
    %% the same as messages_are_properly_flushed_during_resumption,
    %% but tests that buffered by p1_fsm_old messages are delivered
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    escalus:fresh_story(Config, [{bob, 1}], fun(Bob) ->
        Steps = connection_steps_to_enable_stream_resumption(),
        {ok, Alice = #client{props = Props}, _} = escalus_connection:start(AliceSpec, Steps),
        SMID = proplists:get_value(smid, Props),
        InitialPresence = escalus_stanza:presence(<<"available">>),
        escalus_connection:send(Alice, InitialPresence),
        Presence = escalus:wait_for_stanza(Alice),
        escalus:assert(is_presence, Presence),
        SMH = escalus_connection:get_sm_h(Alice),
        escalus_client:kill_connection(Config, Alice),
        {ok, C2SPid} = get_session_pid(AliceSpec, server_string("escalus-default-resource")),
        ok = rpc(mim(), sys, suspend, [C2SPid]),

        %% send some dummy event. ignored by c2s but ensures that
        %% p1_old_fsm buffers the messages, sent after this one
        rpc(mim(), p1_fsm_old, send_all_state_event, [C2SPid, dummy_event]),

        MsgBody = <<"flush-regression">>,
        spawn(fun() ->
                    wait_for_queue_length(C2SPid, 2),

                    % Bob sends a message...
                    escalus:send(Bob, escalus_stanza:chat_to(Alice, MsgBody)),

                    % ...we ensure that a message is enqueued in Alice's session...
                    % (2 messages = resume request + Bob's message)
                    wait_for_queue_length(C2SPid, 3),

                    % ...and old process is resumed.
                    ok = rpc(mim(), sys, resume, [C2SPid])
              end),

        Steps2 = connection_steps_to_stream_resumption(SMID, SMH),
        {ok, Alice2, _} = escalus_connection:start(AliceSpec, Steps2),

        % THEN Alice's new session receives Bob's message
        RecvMsg = escalus:wait_for_stanza(Alice2),
        escalus:assert(is_chat_message, [MsgBody], RecvMsg)
      end).

no_crash_if_stream_mgmt_disabled_but_client_requests_stream_mgmt(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Steps = connection_steps_to_session(),
    {ok, Alice, _Features} = escalus_connection:start(AliceSpec, Steps),
    %% Should not crash anything!
    escalus_connection:send(Alice, escalus_stanza:enable_sm()),
    Response = escalus_connection:get_stanza(Alice, service_unavailable),
    escalus:assert(is_sm_failed, [<<"feature-not-implemented">>], Response),
    escalus_connection:stop(Alice).

no_crash_if_stream_mgmt_disabled_but_client_requests_stream_mgmt_with_resumption(Config) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    Steps = connection_steps_to_session(),
    {ok, Alice, _Features} = escalus_connection:start(AliceSpec, Steps),
    %% Should not crash anything!
    escalus_connection:send(Alice, escalus_stanza:enable_sm([resume])),
    Response = escalus_connection:get_stanza(Alice, service_unavailable),
    escalus:assert(is_sm_failed, [<<"feature-not-implemented">>], Response),
    escalus_connection:stop(Alice).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
start_hook_listener(Resource) ->
    TestCasePid = self(),
    rpc(mim(), ?MODULE, rpc_start_hook_handler, [TestCasePid, Resource]).

rpc_start_hook_handler(TestCasePid, User) ->
    LUser=jid:nodeprep(User),
    Handler = fun(Acc, Jid) ->
                {U, _S, R} = jid:to_lower(Jid),
                case U of
                    LUser ->
                        Counter = mongoose_acc:get(sm_test, counter, 0, Acc),
                        El = mongoose_acc:element(Acc),
                        TestCasePid ! {sm_test, Counter, R, El},
                        mongoose_acc:set_permanent(sm_test, counter, Counter + 1, Acc);
                    _ -> Acc
                end
              end,
    ejabberd_hooks:add(unacknowledged_message, <<"localhost">>, Handler, 50).

wait_for_unacked_msg_hook(Counter, Res, Timeout) ->
    receive
        {sm_test, AccCounter, Resource, Stanza} = Msg ->
            ?assertEqual(Counter, AccCounter, Msg),
            ?assertEqual(Res, Resource, Msg),
            Stanza
    after Timeout ->
        timeout
    end.

get_stream_resumption_enabled_fresh_user_smid_and_h(Config, UserAtom) ->
    UserSpec = escalus_fresh:create_fresh_user(Config, UserAtom),
    Steps = connection_steps_to_enable_stream_resumption(),
    {ok, User = #client{props = Props}, _} = escalus_connection:start(UserSpec, Steps),
    process_initial_stanza(User),
    SMID = proplists:get_value(smid, Props),
    SMH = escalus_connection:get_sm_h(User),
    {User, UserSpec, SMID, SMH}.

process_initial_stanza(User) ->
    escalus:send(User, escalus_stanza:presence(<<"available">>)),
    escalus:assert(is_presence, escalus:wait_for_stanza(User)).

discard_offline_messages(Config, UserName) ->
    discard_offline_messages(Config, UserName, 1).

discard_offline_messages(Config, UserName, H) when is_atom(UserName) ->
    Spec = escalus_users:get_options(Config, UserName),
    {ok, User, _} = escalus_connection:start(Spec),
    escalus_connection:send(User, escalus_stanza:presence(<<"available">>)),
    discard_offline_messages(Config, User, H);
discard_offline_messages(Config, User, H) ->
    Stanza = escalus_connection:get_stanza(User, maybe_offline_msg),
    escalus_connection:send(User, escalus_stanza:sm_ack(H)),
    case escalus_pred:is_presence(Stanza) of
        true ->
            ok;
        false ->
            discard_offline_messages(Config, User, H+1)
    end.

buffer_max(BufferMax) ->
    {buffer_max,
     fun () ->
             rpc(mim(), ?MOD_SM, get_buffer_max, [unset])
     end,
     fun (unset) ->
             ct:pal("buffer_max was not set - setting to 'undefined'"),
             rpc(mim(), ?MOD_SM, set_buffer_max, [undefined]);
         (V) ->
             rpc(mim(), ?MOD_SM, set_buffer_max, [V])
     end,
     BufferMax}.

ack_freq(AckFreq) ->
    {ack_freq,
     fun () ->
             rpc(mim(), ?MOD_SM, get_ack_freq, [unset])
     end,
     fun (unset) ->
             ct:pal("ack_freq was not set - setting to 'undefined'"),
             rpc(mim(), ?MOD_SM, set_ack_freq, [undefined]);
         (V) ->
             rpc(mim(), ?MOD_SM, set_ack_freq, [V])
     end,
     AckFreq}.

assert_no_offline_msgs(Spec) ->
    Username = escalus_utils:jid_to_lower(proplists:get_value(username, Spec)),
    Server = proplists:get_value(server, Spec),
    0 = mongoose_helper:total_offline_messages({Username, Server}).

assert_no_offline_msgs() ->
    0 = mongoose_helper:total_offline_messages().

wait_for_c2s_state_change(C2SPid, NewStateName) ->
    mongoose_helper:wait_until(fun() -> get_c2s_state(C2SPid) end, NewStateName,
                                #{name => get_c2s_state, time_left => timer:seconds(5)}).

wait_for_c2s_unacked_count(C2SPid, Count) ->
    mongoose_helper:wait_until(fun() -> get_c2s_unacked_count(C2SPid) end, Count,
                                #{name => get_c2s_state, time_left => timer:seconds(5)}).

get_c2s_unacked_count(C2SPid) ->
     Info = rpc(mim(), ejabberd_c2s, get_info, [C2SPid]),
     maps:get(stream_mgmt_buffer_size, Info).

assert_c2s_state(C2SPid, StateName) ->
    StateName = get_c2s_state(C2SPid).

get_c2s_state(C2SPid) when is_pid(C2SPid) ->
    SysStatus = rpc(mim(), sys, get_status, [C2SPid]),
    extract_state_name(SysStatus).

extract_state_name(SysStatus) ->
    {status, _Pid, {module, _},
     [_, _, _, _, [_, {data, FSMData} | _]]} = SysStatus,
    proplists:get_value("StateName", FSMData).

wait_until_disconnected(UserSpec) ->
    mongoose_helper:wait_until(fun() -> get_user_alive_resources(UserSpec) =:= [] end, true,
                               #{name => get_user_alive_resources}).

monitor_session(Client) ->
    UserSpec = Client#client.props,
    {resource, Res} = lists:keyfind(resource, 1, UserSpec),
    {ok, C2SPid} = get_session_pid(UserSpec, Res),
    erlang:monitor(process, C2SPid).

wait_for_process_termination(C2SRef) ->
    receive
        {'DOWN', _MRef, _Type, C2SRef, _Info} ->
            ok
    after timer:seconds(1) ->
              ok
    end,
    ok.

get_session_pid(UserSpec, Resource) ->
    {U, S} = get_us_from_spec(UserSpec),
    JID = mongoose_helper:make_jid(U, S, Resource),
    case rpc(mim(), ejabberd_sm, get_session_pid, [JID]) of
        none ->
            {error, no_found};
        C2SPid ->
            {ok, C2SPid}
    end.

get_user_alive_resources(UserSpec) ->
    {U, S} = get_us_from_spec(UserSpec),
    JID = mongoose_helper:make_jid(U, S, <<>>),
    rpc(mim(), ejabberd_sm, get_user_resources, [JID]).

get_user_present_resources(UserSpec) ->
    {U, S} = get_us_from_spec(UserSpec),
    JID = mongoose_helper:make_jid(U, S, <<>>),
    rpc(mim(), ejabberd_sm, get_user_present_resources, [JID]).

get_sid_by_stream_id(SMID) ->
    rpc(mim(), ?MOD_SM, get_sid, [SMID]).

get_us_from_spec(UserSpec) ->
    ConfigUS = [proplists:get_value(username, UserSpec),
                proplists:get_value(server, UserSpec)],
    [U, S] = [server_string(V) || V <- ConfigUS],
    {U, S}.

clear_session_table() ->
    Node = ct:get_config({hosts, mim, node}),
    SessionBackend  = rpc(mim(), ejabberd_sm_backend, backend, []),
    rpc(mim(), SessionBackend, cleanup, [Node]).

clear_sm_session_table() ->
    rpc(mim(), mnesia, clear_table, [sm_session]).

is_chat(Content) ->
    fun(Stanza) -> escalus_pred:is_chat_message(Content, Stanza) end.

given_fresh_user(Config, UserName) ->
    Spec = escalus_fresh:create_fresh_user(Config, UserName),
    given_fresh_user_with_spec(Spec).

given_fresh_user_with_spec(Spec) ->
    {ok, User = #client{props = Props}, _} = escalus_connection:start(Spec),
    escalus:send(User, escalus_stanza:presence(<<"available">>)),
    escalus:wait_for_stanza(User),
    JID = common_helper:get_bjid(Props),
    {User#client{jid  = JID}, Spec}.

wait_for_queue_length(Pid, Length) ->
    mongoose_helper:wait_until(
      fun() ->
              rpc(mim(), erlang, process_info, [Pid, message_queue_len])
      end, {message_queue_len, Length}).

%%--------------------------------------------------------------------
%% IQ handler necessary for reproducing "replies_are_processed_by_resumed_session"
%%--------------------------------------------------------------------

regression_ns() ->
    <<"regression">>.

register_handler(Host) ->
    rpc(mim(), gen_iq_handler, add_iq_handler,
        [ejabberd_sm, Host, regression_ns(), ?MODULE, regression_handler, one_queue]).

unregister_handler(Host) ->
    rpc(mim(), gen_iq_handler, remove_iq_handler, [ejabberd_sm, Host, regression_ns()]).

regression_handler(_From, _To, Acc, IQ) ->
    %% A bit of a hack - will no longer work when the SID format changes
    {_, Pid} = mongoose_acc:get(c2s, origin_sid, undefined, Acc),
    erlang:monitor(process, Pid),
    receive
        {'DOWN', _, _, _, _} ->
            ok
    after
        10000 ->
            error({c2s_not_stopped_after_timeout, Pid})
    end,
    %% We avoid another race condition - there is a short window where user session
    %% is not registered in ejabberd_sm: between old process termination and the moment
    %% when the new process stores new session in memory. It should be fixed separately.
    wait_for_session(mongoose_acc:get(c2s, origin_jid, undefined, Acc), 50, 100),
    {Acc, jlib:make_result_iq_reply(IQ)}.

wait_for_session(JID, Retries, SleepTime) ->
    case ejabberd_sm:get_session(JID) of
        offline ->
            timer:sleep(SleepTime),
            wait_for_session(JID, Retries - 1, SleepTime);
        _ ->
            ok
    end.

make_smid() ->
    base64:encode(crypto:strong_rand_bytes(21)).
