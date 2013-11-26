-module(sm_SUITE).
-compile(export_all).

-include_lib("exml/include/exml.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-define(MOD_SM, mod_stream_management).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, negotiation},
     {group, server_acking},
     {group, client_acking}].

groups() ->
    [{negotiation, [], [server_announces_sm,
                        server_enables_sm_before_session,
                        server_enables_sm_after_session,
                        server_returns_failed_after_start,
                        server_returns_failed_after_auth]},
     {server_acking,
      [shuffle, {repeat,5}], [basic_ack,
                              h_ok_before_session,
                              h_ok_after_session_enabled_before_session,
                              h_ok_after_session_enabled_after_session,
                              h_ok_after_a_chat]},
     {client_acking,
      [shuffle, {repeat, 5}], [client_acks_more_than_sent,
                               too_many_unacked_stanzas,
                               server_requests_ack]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_group(client_acking, Config) ->
    NewConfig = escalus_ejabberd:setup_option(ack_freq(200), Config),
    escalus_users:update_userspec(NewConfig, alice, stream_management, true);
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(client_acking, Config) ->
    escalus_ejabberd:reset_option(ack_freq(200), Config);
end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(h_ok_after_a_chat = CaseName, Config) ->
    NewConfig = escalus_users:update_userspec(Config, alice,
                                              stream_management, true),
    escalus:init_per_testcase(CaseName, NewConfig);
init_per_testcase(too_many_unacked_stanzas = CaseName, Config) ->
    NewConfig = escalus_ejabberd:setup_option(buffer_max(2), Config),
    escalus:init_per_testcase(CaseName, NewConfig);
init_per_testcase(server_requests_ack = CaseName, Config) ->
    NewConfig = escalus_ejabberd:setup_option(ack_freq(2), Config),
    escalus:init_per_testcase(CaseName, NewConfig);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(too_many_unacked_stanzas = CaseName, Config) ->
    NewConfig = escalus_ejabberd:reset_option(buffer_max(2), Config),
    escalus:end_per_testcase(CaseName, NewConfig);
end_per_testcase(server_requests_ack = CaseName, Config) ->
    NewConfig = escalus_ejabberd:reset_option(ack_freq(2), Config),
    escalus:end_per_testcase(CaseName, NewConfig);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

server_announces_sm(Config) ->
    AliceSpec = [{stream_management, true}
                 | escalus_users:get_options(Config, alice)],
    {ok, _, Props, Features} = escalus_connection:start(AliceSpec,
                                                        [start_stream]),
    true = escalus_session:can_use_stream_management(Props, Features).

server_enables_sm_before_session(Config) ->
    AliceSpec = [{stream_management, true}
                 | escalus_users:get_options(Config, alice)],
    {ok, _, _, _} = escalus_connection:start(AliceSpec, [start_stream,
                                                         authenticate,
                                                         bind,
                                                         stream_management]).

server_enables_sm_after_session(Config) ->
    AliceSpec = [{stream_management, true}
                 | escalus_users:get_options(Config, alice)],
    {ok, _, _, _} = escalus_connection:start(AliceSpec, [start_stream,
                                                         authenticate,
                                                         bind,
                                                         session,
                                                         stream_management]).

server_returns_failed_after_start(Config) ->
    server_returns_failed(Config, []).

server_returns_failed_after_auth(Config) ->
    server_returns_failed(Config, [authenticate]).

server_returns_failed(Config, ConnActions) ->
    AliceSpec = [{stream_management, true}
                 | escalus_users:get_options(Config, alice)],
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec,
                                                 [start_stream]
                                                 ++ ConnActions),
    escalus_connection:send(Alice, escalus_stanza:enable_sm()),
    escalus:assert(is_failed,
                   escalus_connection:get_stanza(Alice, enable_sm_failed)).

basic_ack(Config) ->
    AliceSpec = [{stream_management, true}
                 | escalus_users:get_options(Config, alice)],
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec,
                                                 [start_stream,
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
    AliceSpec = [{stream_management, true}
                 | escalus_users:get_options(Config, alice)],
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec,
                                                 [start_stream,
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
    AliceSpec = [{stream_management, true}
                 | escalus_users:get_options(Config, alice)],
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec,
                                                 [start_stream,
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
    AliceSpec = [{stream_management, true}
                 | escalus_users:get_options(Config, alice)],
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec,
                                                 [start_stream,
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
    escalus:story(Config, [{alice,1}, {bob,1}], fun(Alice, Bob) ->
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
        escalus:assert(is_ack, [3], escalus:wait_for_stanza(Alice))
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
                       escalus:wait_for_stanza(Alice))
    end).

server_requests_ack(Config) ->
    escalus:story(Config, [{alice,1}, {bob,1}], fun(Alice, Bob) ->
        escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, Alice!">>)),
        escalus:assert(is_chat_message, [<<"Hi, Alice!">>],
                       escalus:wait_for_stanza(Alice)),
        escalus:assert(is_ack_request, escalus:wait_for_stanza(Alice))
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
             ct:pal("buffer_max was not set - setting to 'undefined'"),
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
             ct:pal("ack_freq was not set - setting to 'undefined'"),
             escalus_ejabberd:rpc(?MOD_SM, set_ack_freq, [undefined]);
         (V) ->
             escalus_ejabberd:rpc(?MOD_SM, set_ack_freq, [V])
     end,
     AckFreq}.
