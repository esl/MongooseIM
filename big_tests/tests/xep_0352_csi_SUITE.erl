-module(xep_0352_csi_SUITE).

-include_lib("escalus/include/escalus.hrl").

-compile([export_all, nowarn_export_all]).

-import(domain_helper, [host_type/0]).
-import(config_parser_helper, [default_mod_config/1, mod_config/2]).

-define(CSI_BUFFER_MAX, 10).

all() ->
    [{group, basic}].


groups() ->
    [{basic, [parallel], all_tests()}].

all_tests() ->
    [
     server_announces_csi,
     alice_is_inactive_and_no_stanza_arrived,
     inactive_twice_does_not_reset_buffer,
     alice_gets_msgs_after_activate,
     alice_gets_msgs_after_activate_in_order,
     alice_gets_message_after_buffer_overflow,
     alice_gets_buffered_messages_after_reconnection_with_sm,
     alice_gets_buffered_messages_after_stream_resumption,
     bob_gets_msgs_from_inactive_alice,
     alice_is_inactive_but_sends_sm_req_and_recives_ack,
     invalid_csi_request_returns_error
    ].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    NewConfig = dynamic_modules:save_modules(host_type(), Config),
    Backend = mongoose_helper:mnesia_or_rdbms_backend(),
    dynamic_modules:ensure_modules(
      host_type(), [{mod_offline, mod_config(mod_offline, #{backend => Backend})},
                    {mod_csi, mod_config(mod_csi, #{buffer_max => ?CSI_BUFFER_MAX})}]),
    [{escalus_user_db, {module, escalus_ejabberd}} | escalus:init_per_suite(NewConfig)].

end_per_suite(Config) ->
    escalus_fresh:clean(),
    dynamic_modules:restore_modules(Config),
    escalus:end_per_suite(Config).

init_per_group(_, Config) ->
    escalus_users:update_userspec(Config, alice, stream_management, true).

end_per_group(_Group, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

server_announces_csi(Config) ->
    NewConfig = escalus_fresh:create_users(Config, [{alice, 1}]),
    Spec = escalus_users:get_userspec(NewConfig, alice),
    Steps = [start_stream, stream_features, maybe_use_ssl, authenticate, bind, session],
    {ok, _Client, Features} = escalus_connection:start(Spec, Steps),
    true = proplists:get_value(client_state_indication, Features).

invalid_csi_request_returns_error(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice) ->
        escalus:send(Alice, csi_helper:csi_stanza(<<"invalid">>)),
        Stanza = escalus:wait_for_stanza(Alice),
        escalus:assert(is_error, [<<"modify">>, <<"bad-request">>], Stanza)
    end).

alice_is_inactive_and_no_stanza_arrived(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        csi_helper:given_client_is_inactive_and_no_messages_arrive(Alice),
        csi_helper:given_messages_are_sent(Alice, Bob, 1),
        csi_helper:then_client_does_not_receive_any_message(Alice)
    end).

alice_gets_msgs_after_activate(Config) ->
    alice_gets_msgs_after_activate(Config, 1).

alice_gets_msgs_after_activate_in_order(Config) ->
    alice_gets_msgs_after_activate(Config, 3).

alice_gets_msgs_after_activate(Config, N) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        csi_helper:given_client_is_inactive_and_no_messages_arrive(Alice),
        Msgs = csi_helper:given_messages_are_sent(Alice, Bob, N),
        csi_helper:given_client_is_active(Alice),
        csi_helper:then_client_receives_message(Alice, Msgs)
    end).

inactive_twice_does_not_reset_buffer(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        csi_helper:given_client_is_inactive_and_no_messages_arrive(Alice),
        Msgs = csi_helper:given_messages_are_sent(Alice, Bob, 2),
        csi_helper:given_client_is_inactive_and_no_messages_arrive(Alice),
        csi_helper:given_client_is_active(Alice),
        csi_helper:then_client_receives_message(Alice, Msgs)
    end).

alice_gets_buffered_messages_after_reconnection_with_sm(Config) ->
    NewConfig = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    AliceSpec = escalus_users:get_userspec(NewConfig, alice),
    BobSpec = escalus_users:get_userspec(NewConfig, bob),
    {ok, Alice0 = #client{props = AliceProps}, _} = escalus_connection:start(AliceSpec),
    JID = make_jid_from_spec(AliceProps),
    Alice = Alice0#client{jid = JID},
    {ok, Bob, _} = escalus_connection:start(BobSpec),

    csi_helper:given_client_is_inactive_and_no_messages_arrive(Alice),

    MsgsToAlice = csi_helper:given_messages_are_sent(Alice, Bob, 5),

    %% then Alice disconnects

    escalus_connection:kill(Alice),

    ConnSteps = [start_stream, stream_features, authenticate, bind, session],
    {ok, Alice2, _} = escalus_connection:start(AliceSpec, ConnSteps),

    csi_helper:then_client_receives_message(Alice2, MsgsToAlice),
    escalus_connection:stop(Alice2),
    escalus_connection:stop(Bob).

alice_gets_buffered_messages_after_stream_resumption(Config) ->
    ConnSteps = [start_stream, stream_features, authenticate, bind, session, stream_resumption],
    NewConfig = escalus_fresh:create_users(Config, [{alice, 1}, {bob, 1}]),
    AliceSpec = escalus_users:get_userspec(NewConfig, alice),
    BobSpec = escalus_users:get_userspec(NewConfig, bob),
    {ok, Alice0 = #client{props = AliceProps}, _} = escalus_connection:start(AliceSpec, ConnSteps),
    JID = make_jid_from_spec(AliceProps),
    Alice = Alice0#client{jid = JID},

    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    escalus:wait_for_stanza(Alice),
    {ok, Bob, _} = escalus_connection:start(BobSpec),

    csi_helper:given_client_is_inactive_and_no_messages_arrive(Alice),
    MsgsToAlice = csi_helper:given_messages_are_sent(Alice, Bob, 5),
    csi_helper:then_client_does_not_receive_any_message(Alice),

    %% then Alice loses connection and resumes it
    escalus_connection:kill(Alice),
    SMID = proplists:get_value(smid, AliceProps),
    ResumeSession = [start_stream, stream_features, authenticate, mk_resume_stream(SMID, 1)],
    {ok, Alice2, _} = escalus_connection:start(AliceSpec, ResumeSession),

    csi_helper:then_client_receives_message(Alice2, MsgsToAlice),
    escalus_connection:stop(Alice2),
    escalus_connection:stop(Bob).

make_jid_from_spec(AliceProps) ->
    AliceUsername = proplists:get_value(username, AliceProps),
    AliceServer = proplists:get_value(server, AliceProps),
    <<AliceUsername/binary, "@", AliceServer/binary>>.

mk_resume_stream(SMID, PrevH) ->
    fun (Conn = #client{props = Props}, Features) ->
            escalus_connection:send(Conn, escalus_stanza:resume(SMID, PrevH)),
            Resumed = escalus_connection:get_stanza(Conn, get_resumed),
            true = escalus_pred:is_sm_resumed(SMID, Resumed),
            {Conn#client{props = [{smid, SMID} | Props]}, Features}
    end.

alice_gets_message_after_buffer_overflow(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        csi_helper:given_client_is_inactive_and_no_messages_arrive(Alice),
        Msgs = csi_helper:given_messages_are_sent(Alice, Bob, ?CSI_BUFFER_MAX + 5),
        {Flushed, Awaiting} = lists:split(?CSI_BUFFER_MAX+1, Msgs),
        csi_helper:then_client_receives_message(Alice, Flushed),
        %% and no other stanza
        csi_helper:then_client_does_not_receive_any_message(Alice),
        %% Alice activates
        csi_helper:given_client_is_active(Alice),
        %% ands gets remaining stanzas
        csi_helper:then_client_receives_message(Alice, Awaiting)
    end).

bob_gets_msgs_from_inactive_alice(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        given_client_is_inactive_but_sends_messages(Alice, Bob, 1),
        escalus:assert(is_chat_message, escalus:wait_for_stanza(Bob))
    end).

alice_is_inactive_but_sends_sm_req_and_recives_ack(Config) ->
    escalus:fresh_story(Config, [{alice,1}], fun(Alice) ->
        csi_helper:given_client_is_inactive_and_no_messages_arrive(Alice),
        escalus:send(Alice, escalus_stanza:sm_request()),
        escalus:assert(is_sm_ack, escalus:wait_for_stanza(Alice))

    end).

given_client_is_inactive_but_sends_messages(Alice, Bob, N) ->
    %%Given
    csi_helper:given_client_is_inactive_and_no_messages_arrive(Alice),
    MsgsToAlice = csi_helper:given_messages_are_sent(Alice, Bob, N),
    MsgsToBob = csi_helper:gen_msgs(<<"Hi, Bob">>, N),
    csi_helper:send_msgs(Alice, Bob, MsgsToBob),
    timer:sleep(1),
    {MsgsToAlice, MsgsToBob}.
