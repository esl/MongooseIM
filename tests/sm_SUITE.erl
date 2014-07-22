-module(sm_SUITE).
-compile(export_all).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-define(MOD_SM, mod_stream_management).
-define(CONSTRAINT_CHECK_TIMEOUT, 5000).

-import(vcard_update, [discard_vcard_update/1,
                       has_mod_vcard_xupdate/0,
                       server_string/1]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [ server_string_type,
      {group, negotiation},
      {group, server_acking},
      {group, client_acking},
      {group, reconnection},
      {group, resumption},
      {group, stanza_cache},
      {group, mod_offline_dependent}
    ].

groups() ->
    [{negotiation, [shuffle, {repeat, 5}], [server_announces_sm,
                                            server_enables_sm_before_session,
                                            server_enables_sm_after_session,
                                            server_returns_failed_after_start,
                                            server_returns_failed_after_auth,
                                            server_enables_resumption]},
     {server_acking,
      [shuffle, {repeat, 5}], [basic_ack,
                               h_ok_before_session,
                               h_ok_after_session_enabled_before_session,
                               h_ok_after_session_enabled_after_session,
                               h_ok_after_a_chat]},
     {client_acking,
      [shuffle, {repeat, 5}], [client_acks_more_than_sent,
                               too_many_unacked_stanzas,
                               server_requests_ack
                              ]},
     {reconnection, [shuffle, {repeat, 5}], [
                                    preserve_order
                                   ]},
     {resumption, [shuffle, {repeat, 5}], [session_established,
                                           wait_for_resumption,
                                           resume_session]},
     {stanza_cache, [         resume_session_state_stop_c2s,
                              resend_unacked_after_resume_timeout,
                              resend_unacked_on_reconnection,
                              resume_session_state_send_message ]},
     {mod_offline_dependent, [
                              resume_session_state_send_message,
                              resend_more_offline_messages_than_buffer_size
                             ]}
    ].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    NewConfig = escalus_ejabberd:setup_option(ack_freq(never), Config),
    Config1 = escalus:init_per_suite(NewConfig),
    escalus:create_users(Config1, {by_name, [alice, bob]}).

end_per_suite(Config) ->
    NewConfig = escalus_ejabberd:reset_option(ack_freq(never), Config),
    NewConfig1 = escalus:delete_users(NewConfig, config),
    escalus:end_per_suite(NewConfig1).

init_per_group(client_acking, Config) ->
    config_with_sm_manual_ack(Config, alice);
init_per_group(reconnection, Config) ->
    config_with_sm_manual_ack(Config, alice);
init_per_group(mod_offline_dependent, Config) ->
    config_without_sm(Config, alice);
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(reconnection, Config)->
    sm_helpers:discard_offline_messages(Config, alice),
    clear_session_table(),
    clear_sm_session_table(),
    true = escalus_ejabberd:rpc(?MOD_SM, set_ack_freq, [never]),
    Config;

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(h_ok_after_a_chat = CaseName, Config) ->
    NewConfig = config_with_sm_manual_ack(Config, alice),
    escalus:init_per_testcase(CaseName, NewConfig);
init_per_testcase(too_many_unacked_stanzas = CaseName, Config) ->
    clear_db(),
    NewConfig = escalus_ejabberd:setup_option(buffer_max(2), Config),
    escalus:init_per_testcase(CaseName, NewConfig);
init_per_testcase(resend_more_offline_messages_than_buffer_size = CaseName, Config) ->
    clear_db(),
    true = escalus_ejabberd:rpc(?MOD_SM, set_ack_freq, [never]),
    NewConfig = escalus_ejabberd:setup_option(buffer_max(2), Config),
    escalus:init_per_testcase(CaseName, NewConfig);
init_per_testcase(resend_unacked_on_reconnection = CaseName, Config) ->
    clear_db(),
    Config1 = config_with_sm_manual_ack(Config, alice),
    escalus:init_per_testcase(CaseName, Config1);
init_per_testcase(server_requests_ack = CaseName, Config) ->
    clear_db(),
    AckFreq = 2 + case has_mod_vcard_xupdate() of
                      true -> 1;
                      _ -> 0
                  end,
    NewConfig = escalus_ejabberd:setup_option(ack_freq(AckFreq), Config),
    escalus:init_per_testcase(CaseName, NewConfig);
init_per_testcase(CaseName, Config) ->
    clear_db(),
    escalus:init_per_testcase(CaseName, Config).


end_per_testcase(too_many_unacked_stanzas = CaseName, Config) ->
    NewConfig = escalus_ejabberd:reset_option(buffer_max(2), Config),
    clear_db(),
    escalus:end_per_testcase(CaseName, NewConfig);
end_per_testcase(resend_more_offline_messages_than_buffer_size = CaseName, Config) ->
    NewConfig = escalus_ejabberd:reset_option(buffer_max(2), Config),
    clear_db(),
    escalus:end_per_testcase(CaseName, NewConfig);
end_per_testcase(server_requests_ack = CaseName, Config) ->
    AckFreq = 2 + case has_mod_vcard_xupdate() of
                      true -> 1;
                      _ -> 0
                  end,
    NewConfig = escalus_ejabberd:reset_option(ack_freq(AckFreq), Config),
    clear_db(),
    escalus:end_per_testcase(CaseName, NewConfig);
end_per_testcase(CaseName, Config) ->
    clear_db(),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

server_string_type(_) ->
    ct:log("server string type: ~p~n", [vcard_update:server_string_type()]).

server_announces_sm(Config) ->
    AliceSpec = spec_with_sm_manual_ack(Config, alice),
    {ok, _, Props, Features} = escalus_connection:start(AliceSpec,
                                                        [start_stream]),
    true = escalus_session:can_use_stream_management(Props, Features).

server_enables_sm_before_session(Config) ->
    AliceSpec = spec_with_sm_manual_ack(Config, alice),
    {ok, _, _, _} = escalus_connection:start(AliceSpec, [start_stream,
                                                         maybe_use_ssl,
                                                         authenticate,
                                                         bind,
                                                         stream_management]).

server_enables_sm_after_session(Config) ->
    AliceSpec = spec_with_sm_manual_ack(Config, alice),
    {ok, _, _, _} = escalus_connection:start(AliceSpec, [start_stream,
                                                         maybe_use_ssl,
                                                         authenticate,
                                                         bind,
                                                         session,
                                                         stream_management]).

server_returns_failed_after_start(Config) ->
    server_returns_failed(Config, []).

server_returns_failed_after_auth(Config) ->
    server_returns_failed(Config, [authenticate]).

server_enables_resumption(Config) ->
    AliceSpec = spec_with_sm_manual_ack(Config, alice),
    %% Assert matches {ok, _, _, _}
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec,
                                                 [start_stream,
                                                  maybe_use_ssl,
                                                  authenticate,
                                                  bind,
                                                  session,
                                                  stream_resumption]),
    escalus_connection:stop(Alice).

server_returns_failed(Config, ConnActions) ->
    AliceSpec = spec_with_sm_manual_ack(Config,alice),
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec,
                                                 [start_stream,
                                                  maybe_use_ssl]
                                                 ++ ConnActions),
    escalus_connection:send(Alice, escalus_stanza:enable_sm()),
    escalus:assert(is_failed, [<<"unexpected-request">>],
                   escalus_connection:get_stanza(Alice, enable_sm_failed)).

basic_ack(Config) ->
    AliceSpec = spec_with_sm_manual_ack(Config,alice),
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec,
                                                 [start_stream,
                                                  maybe_use_ssl,
                                                  authenticate,
                                                  bind,
                                                  session,
                                                  stream_management]),
    escalus_connection:send(Alice, escalus_stanza:roster_get()),
    escalus:assert(is_roster_result,
                   escalus_connection:get_stanza(Alice, roster_result)),
    escalus_connection:send(Alice, escalus_stanza:sm_request()),
    escalus:assert(is_ack,
                   escalus_connection:get_stanza(Alice, stream_mgmt_ack)).

%% Test that "h" value is valid when:
%% - SM is enabled *before* the session is established
%% - <r/> is sent *before* the session is established
h_ok_before_session(Config) ->
    AliceSpec = spec_with_sm_manual_ack(Config,alice),
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec,
                                                 [start_stream,
                                                  maybe_use_ssl,
                                                  authenticate,
                                                  bind,
                                                  stream_management]),
    escalus_connection:send(Alice, escalus_stanza:sm_request()),
    escalus:assert(is_ack, [0],
                   escalus_connection:get_stanza(Alice, stream_mgmt_ack)).

%% Test that "h" value is valid when:
%% - SM is enabled *before* the session is established
%% - <r/> is sent *after* the session is established
h_ok_after_session_enabled_before_session(Config) ->
    AliceSpec = spec_with_sm_manual_ack(Config,alice),
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec,
                                                 [start_stream,
                                                  maybe_use_ssl,
                                                  authenticate,
                                                  bind,
                                                  stream_management,
                                                  session]),
    escalus_connection:send(Alice, escalus_stanza:sm_request()),
    escalus:assert(is_ack, [1],
                   escalus_connection:get_stanza(Alice, stream_mgmt_ack)).

%% Test that "h" value is valid when:
%% - SM is enabled *after* the session is established
%% - <r/> is sent *after* the session is established
h_ok_after_session_enabled_after_session(Config) ->
    AliceSpec = spec_with_sm_manual_ack(Config,alice),
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec,
                                                 [start_stream,
                                                  maybe_use_ssl,
                                                  authenticate,
                                                  bind,
                                                  session,
                                                  stream_management]),
    escalus_connection:send(Alice, escalus_stanza:roster_get()),
    escalus:assert(is_roster_result,
                   escalus_connection:get_stanza(Alice, roster_result)),
    escalus_connection:send(Alice, escalus_stanza:sm_request()),
    escalus:assert(is_ack, [1],
                   escalus_connection:get_stanza(Alice, stream_mgmt_ack)).

%% Test that "h" value is valid after exchanging a few messages.
h_ok_after_a_chat(Config) ->
    escalus:story(Config, [{alice, 1}, {bob,1}], fun(Alice, Bob) ->
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
        escalus:assert(is_ack, [3], escalus:wait_for_stanza(Alice)),
        %% Ack, so that unacked messages don't go into offline store.
        escalus:send(Alice, escalus_stanza:sm_ack(3 + NDiscarded))
    end).

client_acks_more_than_sent(Config) ->
    escalus:story(Config, [{alice,1}], fun(Alice) ->
        escalus:send(Alice, escalus_stanza:sm_ack(5)),
        escalus:assert(is_stream_error, [<<"policy-violation">>,
                                         <<"h attribute too big">>],
                       escalus:wait_for_stanza(Alice))
    end).

too_many_unacked_stanzas(Config) ->
    escalus:story(Config, [{alice,1}, {bob,1}], fun(Alice, Bob) ->
        Msg = escalus_stanza:chat_to(Alice, <<"Hi, Alice!">>),
        [escalus:send(Bob, Msg) || _ <- lists:seq(1,2)],
        escalus:wait_for_stanzas(Alice, 2),
        escalus:assert(is_stream_error, [<<"resource-constraint">>,
                                         <<"too many unacked stanzas">>],
                       %% wait for deffered buffer check
                       escalus:wait_for_stanza(Alice, ?CONSTRAINT_CHECK_TIMEOUT + 1000))
    end),
    sm_helpers:discard_offline_messages(Config, alice).

server_requests_ack(Config) ->
    escalus:story(Config, [{alice,1}, {bob,1}], fun(Alice, Bob) ->
        discard_vcard_update(Alice),
        escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, Alice!">>)),
        escalus:assert(is_chat_message, [<<"Hi, Alice!">>],
                       escalus:wait_for_stanza(Alice)),
        escalus:assert(is_ack_request, escalus:wait_for_stanza(Alice))
    end),
    sm_helpers:discard_offline_messages(Config, alice).

resend_more_offline_messages_than_buffer_size(Config) ->
    ConnSteps =  [start_stream,
                  authenticate,
                  bind,
                  session],

    %% connect bob and alice
    BobSpec = escalus_users:get_options(Config, bob),
    {ok, Bob, _, _} = escalus_connection:start(BobSpec),
    escalus_connection:send(Bob, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Bob, presence),
    AliceSpec = escalus_users:get_options(Config, alice),

    % sent some messages - more than unacked buffer size
    escalus_connection:send(Bob, escalus_stanza:chat_to(get_bjid(AliceSpec), <<"1">>)),
    escalus_connection:send(Bob, escalus_stanza:chat_to(get_bjid(AliceSpec), <<"2">>)),
    escalus_connection:send(Bob, escalus_stanza:chat_to(get_bjid(AliceSpec), <<"3">>)),

    % connect alice who wants to receive all messages from offline storage
    {ok, Alice, _Props, _} = escalus_connection:start(AliceSpec, ConnSteps++[stream_management]),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),

    escalus_connection:get_stanza(Alice, msg_1),
    escalus_connection:get_stanza(Alice, msg_2),
    escalus_connection:get_stanza(Alice, msg_3),
    escalus_connection:get_stanza(Alice, presence),

    % confirm messages + presence
    escalus_connection:send(Alice, escalus_stanza:sm_ack(4)),
    % wait for check constraint message on server side
    ct:sleep(?CONSTRAINT_CHECK_TIMEOUT+1000),

    % should not receive anything especially any stream errors
    false = escalus_client:has_stanzas(Alice),

    escalus_connection:stop(Alice),
    escalus_connection:stop(Bob),
    sm_helpers:discard_offline_messages(Config, alice).

resend_unacked_on_reconnection(Config) ->
    escalus_ejabberd:rpc(?MOD_SM, set_ack_freq, [never]),
    Messages = [<<"msg-1">>, <<"msg-2">>, <<"msg-3">>],
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        discard_vcard_update(Alice),
        %% Bob sends some messages to Alice.
        [escalus:send(Bob, escalus_stanza:chat_to(Alice, Msg))
         || Msg <- Messages],
        %% Alice receives the messages.
        Stanzas = escalus:wait_for_stanzas(Alice, 3),
        [escalus:assert(is_chat_message, [Msg], Stanza)
         || {Msg, Stanza} <- lists:zip(Messages, Stanzas)]
        %% Alice disconnects without acking the messages.
    end),
    %% Messages go to the offline store.
    %% Alice receives the messages from the offline store.
    %% This is done without escalus:story() as a story() performs
    %% an is_presence assertion on the first stanza after connection
    %% initiation which fails as the message from offline store will come
    %% before that presence.
    AliceSpec = escalus_users:get_options(Config, alice),
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    Stanzas = [escalus_connection:get_stanza(Alice, {msg, I})
               || I <- lists:seq(1, 3)],
    [escalus:assert(is_chat_message, [Msg], Stanza)
     || {Msg, Stanza} <- lists:zip(Messages, Stanzas)],
    %% Alice acks the delayed messages so they won't go again
    %% to the offline store.
    escalus_connection:send(Alice, escalus_stanza:sm_ack(3)).

preserve_order(Config) ->
    TIMEOUT = 60,
    escalus_ejabberd:rpc(?MOD_SM, set_resume_timeout, [TIMEOUT]),
    escalus_ejabberd:rpc(?MOD_SM, set_ack_freq, [1]),
    ConnSteps =  [start_stream,
                  authenticate,
                  bind,
                  session],

    %% connect bob and alice
    BobSpec = escalus_users:get_options(Config, bob),
    {ok, Bob, _, _} = escalus_connection:start(BobSpec),
    escalus_connection:send(Bob, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Bob, presence),

    AliceSpec = escalus_users:get_options(Config, alice),
    {ok, Alice, _Props, _} = escalus_connection:start(AliceSpec, ConnSteps++[stream_resumption]),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Alice, presence),

    escalus:assert(is_ack_request, escalus_connection:get_stanza(Alice, ack)),
    escalus_connection:send(Bob, escalus_stanza:chat_to(get_bjid(AliceSpec), <<"1">>)),

    %% kill alice connection
    escalus_connection:kill(Alice),

    escalus_connection:send(Bob, escalus_stanza:chat_to(get_bjid(AliceSpec), <<"2">>)),
    escalus_connection:send(Bob, escalus_stanza:chat_to(get_bjid(AliceSpec), <<"3">>)),

    {ok, NewAlice, _, _} = escalus_connection:start(AliceSpec, ConnSteps),
    escalus_connection:send(NewAlice, escalus_stanza:enable_sm([resume])),

    escalus_connection:send(Bob, escalus_stanza:chat_to(get_bjid(AliceSpec), <<"4">>)),
    escalus_connection:send(Bob, escalus_stanza:chat_to(get_bjid(AliceSpec), <<"5">>)),

    escalus_connection:send(NewAlice, escalus_stanza:presence(<<"available">>)),
    escalus_connection:send(Bob, escalus_stanza:chat_to(get_bjid(AliceSpec), <<"6">>)),


    receive_all_ordered(NewAlice,1),

    % replace connection
    {ok, NewAlice2, _, _} = escalus_connection:start(AliceSpec, ConnSteps),
    % allow messages to go to the offline storage
    ct:sleep(1000),

    escalus_connection:send(NewAlice2, escalus_stanza:presence(<<"available">>)),

    % receves messages in correct order
    receive_all_ordered(NewAlice2, 1),

    %revert changes
    escalus_ejabberd:rpc(?MOD_SM, set_resume_timeout, [600]),
    escalus_connection:stop(Bob),
    escalus_connection:stop(NewAlice2),
    sm_helpers:discard_offline_messages(Config, alice).

receive_all_ordered(Conn, N) ->
    case catch escalus_connection:get_stanza(Conn, msg) of
        #xmlel{} = Stanza ->
	    NN = case Stanza#xmlel.name of
        <<"message">> ->
%% 		    ct:pal("~p~n", [Stanza]),
            escalus:assert(is_chat_message, [list_to_binary(integer_to_list(N))], Stanza),
            N+1;
		_ ->
		    N
	    end,
            receive_all_ordered(Conn, NN);
        _Error ->
            ok
    end.

resend_unacked_after_resume_timeout(Config) ->
    TIMEOUT = 5,
    escalus_ejabberd:rpc(?MOD_SM, set_resume_timeout, [TIMEOUT]),
    escalus_ejabberd:rpc(?MOD_SM, set_ack_freq, [1]),
    ConnSteps =  [start_stream,
                  authenticate,
                  bind,
                  session],

    %% connect bob and alice
    BobSpec = escalus_users:get_options(Config, bob),
    {ok, Bob, _, _} = escalus_connection:start(BobSpec),
    escalus_connection:send(Bob, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Bob, presence),

    AliceSpec = escalus_users:get_options(Config, alice),
    {ok, Alice, _Props, _} = escalus_connection:start(AliceSpec, ConnSteps++[stream_resumption]),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Alice, presence),

    escalus:assert(is_ack_request, escalus_connection:get_stanza(Alice, ack)),

    escalus_connection:send(Bob, escalus_stanza:chat_to(get_bjid(AliceSpec), <<"msg-1">>)),
    %% kill alice connection
    escalus_connection:kill(Alice),
    U = proplists:get_value(username, AliceSpec),
    S = proplists:get_value(server, AliceSpec),
    1 = length(escalus_ejabberd:rpc(ejabberd_sm, get_user_resources, [U, S])),
    %% wait 2 times longer to be sure that c2s is dead
    ct:sleep({seconds, 2*TIMEOUT}),
    %% ensure there is no session
    0 = length(escalus_ejabberd:rpc(ejabberd_sm, get_user_resources, [U, S])),

    %% alice come back and receives unacked message
    {ok, NewAlice, _, _} = escalus_connection:start(AliceSpec, ConnSteps),
    escalus_connection:send(NewAlice, escalus_stanza:presence(<<"available">>)),

    Stanzas =[escalus_connection:get_stanza(NewAlice, msg),
              escalus_connection:get_stanza(NewAlice, msg)],

    escalus_new_assert:mix_match([is_presence,
                                  is_chat(<<"msg-1">>)],
                                 Stanzas),
    %% revert timeout change
    escalus_ejabberd:rpc(?MOD_SM, set_resume_timeout, [600]),

    escalus_connection:stop(Bob),
    escalus_connection:stop(Alice),
    sm_helpers:discard_offline_messages(Config, alice).

resume_session_state_send_message(Config) ->
    escalus_ejabberd:rpc(?MOD_SM, set_ack_freq, [1]),
    ConnSteps =  [
                  start_stream,
                  authenticate,
                  bind,
                  session],

    %% connect bob and alice
    BobSpec = escalus_users:get_options(Config, bob),
    {ok, Bob, _, _} = escalus_connection:start(BobSpec),
    escalus_connection:send(Bob, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Bob, presence),

    AliceSpec = escalus_users:get_options(Config, alice),
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec, ConnSteps++[stream_resumption]),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    escalus_connection:get_stanza(Alice, presence),

    escalus:assert(is_ack_request, escalus_connection:get_stanza(Alice, ack)),

    escalus_connection:send(Bob, escalus_stanza:chat_to(get_bjid(AliceSpec), <<"msg-1">>)),
    %% kill alice connection
    escalus_connection:kill(Alice),
    ct:sleep(1000), %% alice should be in resume_session_state

    U = proplists:get_value(username, AliceSpec),
    S = proplists:get_value(server, AliceSpec),
    1 = length(escalus_ejabberd:rpc(ejabberd_sm, get_user_resources, [U, S])),

    %% send some messages and check if c2s can handle it
    escalus_connection:send(Bob, escalus_stanza:chat_to(get_bjid(AliceSpec), <<"msg-2">>)),
    escalus_connection:send(Bob, escalus_stanza:chat_to(get_bjid(AliceSpec), <<"msg-3">>)),

    %% alice comes back and receives unacked message
    {ok, NewAlice, _, _} = escalus_connection:start(AliceSpec, ConnSteps),
    escalus_connection:send(NewAlice, escalus_stanza:presence(<<"available">>)),

    Stanzas = [escalus_connection:get_stanza(NewAlice, msg) || _ <- lists:seq(1,4) ],

    % what about order ?
    escalus_new_assert:mix_match([is_presence,
                                  is_chat(<<"msg-1">>),
                                  is_chat(<<"msg-2">>),
                                  is_chat(<<"msg-3">>)],
                                 Stanzas),
    escalus_connection:stop(Bob),
    escalus_connection:stop(NewAlice),
    sm_helpers:discard_offline_messages(Config, alice).

%%for instance it can be done by mod ping
resume_session_state_stop_c2s(Config) ->
    true = escalus_ejabberd:rpc(?MOD_SM, set_ack_freq, [1]),
    ConnSteps =  [start_stream,
                  authenticate,
                  bind,
                  session],

    %% connect bob and alice
    BobSpec = escalus_users:get_options(Config, bob),
    {ok, Bob, _, _} = escalus_connection:start(BobSpec, ConnSteps),
    escalus_connection:send(Bob, escalus_stanza:presence(<<"available">>)),
    escalus:assert(is_presence,
                   escalus_connection:get_stanza(Bob, presence)),

    AliceSpec = escalus_users:get_options(Config, alice),
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec, ConnSteps++[stream_resumption]),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    escalus:assert(is_presence,
                   escalus_connection:get_stanza(Alice, presence)),

    escalus:assert(is_ack_request, escalus_connection:get_stanza(Alice, ack)),
    escalus_connection:send(Bob, escalus_stanza:chat_to(get_bjid(AliceSpec), <<"msg-1">>)),

    % kill alice connection
    escalus_connection:kill(Alice),
    ct:sleep(1000), %% alice should be in resume_session_state
    % session should be alive
    U = proplists:get_value(username, AliceSpec),
    S = proplists:get_value(server, AliceSpec),
    [Res] = escalus_ejabberd:rpc(ejabberd_sm, get_user_resources, [U, S]),
    %% get pid of c2s and stop him !
    C2SRef = escalus_ejabberd:rpc(ejabberd_sm, get_session_pid, [U, S, Res]),
    escalus_ejabberd:rpc(ejabberd_c2s, stop, [C2SRef] ),
    ct:sleep(1000), %% c2s should be in resume_session_state

    %% alice comes back and receives unacked message
    {ok, NewAlice, _, _} = escalus_connection:start(AliceSpec, ConnSteps),
    escalus_connection:send(NewAlice, escalus_stanza:presence(<<"available">>)),

    Stanzas = [escalus_connection:get_stanza(NewAlice, msg),
               escalus_connection:get_stanza(NewAlice, msg)
              ],

    escalus_new_assert:mix_match([is_presence,
                                  is_chat(<<"msg-1">>)],
                                 Stanzas),

    escalus_connection:stop(NewAlice),
    escalus_connection:stop(Bob),
    sm_helpers:discard_offline_messages(Config, alice).

%% This test only verifies the validity of helpers (get_session_pid,
%% assert_no_offline_msgs, assert_c2s_state) written for wait_for_resumption
%% testcase.
session_established(Config) ->
    AliceSpec = spec_with_sm_manual_ack(Config,alice),
    escalus:story(Config, [{alice, 1}], fun(_Alice) ->
        {ok, C2SPid} = get_session_pid(AliceSpec, server_string("res1")),
        assert_no_offline_msgs(),
        assert_c2s_state(C2SPid, session_established)
    end).

%% Ensure that after a violent disconnection,
%% the c2s waits for resumption (but don't resume yet).
wait_for_resumption(Config) ->
    AliceSpec = spec_with_sm_manual_ack(Config,alice),
    Messages = [<<"msg-1">>, <<"msg-2">>, <<"msg-3">>],
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        {C2SPid, _} = sm_helpers:buffer_unacked_messages_and_die(AliceSpec, Bob, Messages),
        %% Ensure the c2s process is waiting for resumption.
        assert_no_offline_msgs(),
        assert_c2s_state(C2SPid, resume_session)
    end).

resume_session(Config) ->
    AliceSpec = spec_with_sm_manual_ack(Config,alice),
    Messages = [<<"msg-1">>, <<"msg-2">>, <<"msg-3">>],
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        {_, SMID} = sm_helpers:buffer_unacked_messages_and_die(AliceSpec, Bob, Messages),
        %% Resume the session.
        Steps = [start_stream,
                 maybe_use_ssl,
                 authenticate,
                 sm_helpers:mk_resume_stream(SMID, 2)],
        {ok, Alice, _, _} = escalus_connection:start(AliceSpec, Steps),
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


%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

buffer_max(BufferMax) ->
    {buffer_max,
     fun () ->
             escalus_ejabberd:rpc(?MOD_SM, get_buffer_max, [unset])
     end,
     fun (unset) ->
             ct:log("buffer_max was not set - setting to 'undefined'"),
             escalus_ejabberd:rpc(?MOD_SM, set_buffer_max, [undefined]);
         (V) ->
             escalus_ejabberd:rpc(?MOD_SM, set_buffer_max, [V])
     end,
     BufferMax}.

ack_freq(AckFreq) ->
    {ack_freq,
     fun () ->
             escalus_ejabberd:rpc(?MOD_SM, get_ack_freq, [unset])
     end,
     fun (unset) ->
             ct:log("ack_freq was not set - setting to 'undefined'"),
             escalus_ejabberd:rpc(?MOD_SM, set_ack_freq, [undefined]);
         (V) ->
             escalus_ejabberd:rpc(?MOD_SM, set_ack_freq, [V])
     end,
     AckFreq}.

assert_no_offline_msgs() ->
    Backend = escalus_ejabberd:rpc(mod_offline_backend,backend,[]),
    case Backend of
        mod_offline_mnesia ->
            Pattern = escalus_ejabberd:rpc(mnesia, table_info,
                                           [offline_msg, wild_pattern]),
            0 = length(escalus_ejabberd:rpc(mnesia, dirty_match_object, [Pattern]));
        mod_offline_odbc ->
            {selected, _, [{<<"0">>}]} =
            escalus_ejabberd:rpc(ejabberd_odbc,sql_query, [<<"localhost">>,<<"select count(*) from offline_message;">>]);
        {badrpc, {'EXIT', {undef, _}}} ->
            mod_offline_not_active
    end.


assert_c2s_state(C2SPid, StateName) when is_pid(C2SPid) ->
    SysStatus = escalus_ejabberd:rpc(sys, get_status, [C2SPid]),
    StateName = extract_state_name(SysStatus).

extract_state_name(SysStatus) ->
    {status, _Pid, {module, _},
     [_, _, _, _, [_, {data, FSMData} | _]]} = SysStatus,
    proplists:get_value("StateName", FSMData).

get_session_pid(UserSpec, Resource) ->
    ConfigUS = [proplists:get_value(username, UserSpec),
                proplists:get_value(server, UserSpec)],
    [U, S] = [server_string(V) || V <- ConfigUS],
    case escalus_ejabberd:rpc(ejabberd_sm, get_session_pid, [U, S, server_string(Resource)]) of
        none ->
            {error, no_found};
        C2SPid ->
            {ok, C2SPid}
    end.

%% Copy'n'paste from github.com/lavrin/ejabberd-trace

match_session_pid({_User, _Domain, _Resource} = UDR) ->
    [{%% match pattern
      set(session(), [{2, {'_', '$1'}},
                      {3, UDR}]),
      %% guards
      [],
      %% return
      ['$1']}];

match_session_pid({User, Domain}) ->
    [{%% match pattern
      set(session(), [{2, {'_', '$1'}},
                      {3, '$2'},
                      {4, {User, Domain}}]),
      %% guards
      [],
      %% return
      [{{'$2', '$1'}}]}].

set(Record, FieldValues) ->
    F = fun({Field, Value}, Rec) ->
                setelement(Field, Rec, Value)
        end,
    lists:foldl(F, Record, FieldValues).

session() ->
    set(erlang:make_tuple(6, '_'), [{1, session}]).

%% End of copy'n'paste from github.com/lavrin/ejabberd-trace

clear_db() ->
    %% This will break if mod_offline stores its data
    %% someplace else than mnesia.
    clear_session_table(),
    clear_sm_session_table(),
    clear_offline_table().

clear_session_table() ->
    Node = escalus_ct:get_config(ejabberd_node),
    SessionBackend  = escalus_ejabberd:rpc(ejabberd_sm_backend, backend, []),
    escalus_ejabberd:rpc(SessionBackend, cleanup, [Node]).

clear_sm_session_table() ->
    escalus_ejabberd:rpc(mnesia, clear_table, [sm_session]).

clear_offline_table() ->
    escalus_ejabberd:rpc(mnesia, clear_table, [offline_msg]).

is_chat(Content) ->
    fun(Stanza) -> escalus_pred:is_chat_message(Content, Stanza) end.

get_bjid(UserSpec) ->
    User = proplists:get_value(username, UserSpec),
    Server = proplists:get_value(server, UserSpec),
    <<User/binary,"@",Server/binary>>.



%%
%%  Configuring manual stream managment
%%

config_with_sm_manual_ack(Config, Who) ->
    C0 = escalus_users:update_userspec(Config, Who, stream_management, true),
    escalus_users:update_userspec(C0, Who, manual_ack, true).

config_without_sm(Config, Who) ->
    escalus_users:update_userspec(Config, Who, stream_management, false).

spec_with_sm_manual_ack(Config, Who) ->
    [{stream_management, true}, {manual_ack, true} |
     escalus_users:get_options(Config, Who)].
