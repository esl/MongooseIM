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
     {group, client_acking},
     {group, reconnection},
     {group, resumption}].

groups() ->
    [{negotiation, [], [server_announces_sm,
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
                               server_requests_ack]},
     {reconnection, [], [resend_unacked_on_reconnection]},
     {resumption, [], [session_established,
                       wait_for_resumption,
                       resume_session]}].

suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    NewConfig = escalus_ejabberd:setup_option(ack_freq(never), Config),
    escalus:init_per_suite(NewConfig).

end_per_suite(Config) ->
    NewConfig = escalus_ejabberd:reset_option(ack_freq(never), Config),
    escalus:end_per_suite(NewConfig).

init_per_group(client_acking, Config) ->
    escalus_users:update_userspec(Config, alice, stream_management, true);
init_per_group(reconnection, Config) ->
    escalus_users:update_userspec(Config, alice, stream_management, true);
init_per_group(_GroupName, Config) ->
    Config.

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
end_per_testcase(wait_for_resumption = CaseName, Config) ->
    discard_offline_messages(Config, alice),
    clear_session_table(),
    escalus:end_per_testcase(CaseName, Config);
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

server_enables_resumption(Config) ->
    AliceSpec = [{stream_management, true}
                 | escalus_users:get_options(Config, alice)],
    {ok, _, _, _} = escalus_connection:start(AliceSpec, [start_stream,
                                                         authenticate,
                                                         bind,
                                                         session,
                                                         stream_resumption]).

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
        escalus:assert(is_ack, [3], escalus:wait_for_stanza(Alice)),
        %% Ack, so that unacked messages don't go into offline store.
        escalus:send(Alice, escalus_stanza:sm_ack(3))
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
    end),
    discard_offline_messages(Config, alice).

server_requests_ack(Config) ->
    escalus:story(Config, [{alice,1}, {bob,1}], fun(Alice, Bob) ->
        escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"Hi, Alice!">>)),
        escalus:assert(is_chat_message, [<<"Hi, Alice!">>],
                       escalus:wait_for_stanza(Alice)),
        escalus:assert(is_ack_request, escalus:wait_for_stanza(Alice))
    end),
    discard_offline_messages(Config, alice).

resend_unacked_on_reconnection(Config) ->
    Messages = [<<"msg-1">>, <<"msg-2">>, <<"msg-3">>],
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
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

%% This test only verifies the validity of helpers (get_session_pid,
%% assert_no_offline_msgs, assert_c2s_state) written for wait_for_resumption
%% testcase.
session_established(Config) ->
    AliceSpec = [{stream_management, true}
                 | escalus_users:get_options(Config, alice)],
    escalus:story(Config, [{alice, 1}], fun(_Alice) ->
        {ok, C2SPid} = get_session_pid(AliceSpec, "res1"),
        assert_no_offline_msgs(),
        assert_c2s_state(C2SPid, session_established)
    end).

%% Ensure that after a violent disconnection,
%% the c2s waits for resumption (but don't resume yet).
wait_for_resumption(Config) ->
    AliceSpec = [{stream_management, true}
                 | escalus_users:get_options(Config, alice)],
    Messages = [<<"msg-1">>, <<"msg-2">>, <<"msg-3">>],
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        {C2SPid, _} = buffer_unacked_messages_and_die(AliceSpec, Bob, Messages),
        %% Ensure the c2s process is waiting for resumption.
        assert_no_offline_msgs(),
        assert_c2s_state(C2SPid, resume_session)
    end).

resume_session(Config) ->
    AliceSpec = [{stream_management, true}
                 | escalus_users:get_options(Config, alice)],
    Messages = [<<"msg-1">>, <<"msg-2">>, <<"msg-3">>],
    escalus:story(Config, [{bob, 1}], fun(Bob) ->
        {C2SPid, SMID} = buffer_unacked_messages_and_die(AliceSpec, Bob, Messages),
        %% Resume the session.
        Steps = [start_stream,
                 authenticate,
                 mk_resume_stream(SMID, 2)],
        {ok, Alice, _, _} = escalus_connection:start(AliceSpec, Steps),
        %% Alice receives the unacked messages from the previous
        %% interrupted session.
        Stanzas = [escalus_connection:get_stanza(Alice, {msg, I})
                   || I <- lists:seq(1, 3)],
        [escalus:assert(is_chat_message, [Msg], Stanza)
         || {Msg, Stanza} <- lists:zip(Messages, Stanzas)],
        %% Alice acks the received messages.
        escalus_connection:send(Alice, escalus_stanza:sm_ack(3))
    end).

mk_resume_stream(SMID, PrevH) ->
    fun (Conn, Props, Features) ->
            escalus_connection:send(Conn, escalus_stanza:resume(SMID, PrevH)),
            Resumed = escalus_connection:get_stanza(Conn, get_resumed),
            true = escalus_pred:is_resumed(SMID, Resumed),
            {Conn, [{smid, SMID} | Props], Features}
    end.

buffer_unacked_messages_and_die(AliceSpec, Bob, Messages) ->
    Steps = [start_stream,
             authenticate,
             bind,
             session,
             stream_resumption],
    {ok, Alice, Props, _} = escalus_connection:start(AliceSpec, Steps),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    Presence = escalus_connection:get_stanza(Alice, presence),
    escalus:assert(is_presence, Presence),
    {ok, C2SPid} = get_session_pid(AliceSpec, "escalus-default-resource"),
    escalus_connection:send(Alice, escalus_stanza:presence(<<"available">>)),
    _Presence = escalus_connection:get_stanza(Alice, presence),
    %% Bobs sends some messages to Alice.
    [escalus:send(Bob, escalus_stanza:chat_to(alice, Msg))
     || Msg <- Messages],
    %% Alice receives them, but doesn't ack.
    Stanzas = [escalus_connection:get_stanza(Alice, {msg, I})
               || I <- lists:seq(1, 3)],
    [escalus:assert(is_chat_message, [Msg], Stanza)
     || {Msg, Stanza} <- lists:zip(Messages, Stanzas)],
    %% Alice's connection is violently terminated.
    kill_connection(Alice),
    {C2SPid, proplists:get_value(smid, Props)}.

kill_connection(#transport{module = escalus_tcp, ssl = SSL,
                           socket = Socket} = Conn) ->
    %% Ugly, but there's no API for killing the connection
    %% without sending </stream:stream>.
    case SSL of
        true ->
            ssl:close(Socket);
        false ->
            gen_tcp:close(Socket)
    end,
    %% There might be open zlib streams left...
    catch escalus_connection:stop(Conn).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

discard_offline_messages(Config, UserName) ->
    discard_offline_messages(Config, UserName, 1).

discard_offline_messages(Config, UserName, H) when is_atom(UserName) ->
    Spec = escalus_users:get_options(Config, UserName),
    {ok, User, _, _} = escalus_connection:start(Spec),
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

assert_no_offline_msgs() ->
    Pattern = escalus_ejabberd:rpc(mnesia, table_info,
                                   [offline_msg, wild_pattern]),
    0 = length(escalus_ejabberd:rpc(mnesia, dirty_match_object, [Pattern])).

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
    [U, S] = case string_type() of
                 list ->
                     [binary_to_list(V) || V <- ConfigUS];
                 binary ->
                     ConfigUS
             end,
    MatchSpec = match_session_pid({U, S, Resource}),
    case escalus_ejabberd:rpc(ets, select, [session, MatchSpec]) of
        [] ->
            {error, not_found};
        [{_, C2SPid}] ->
            {ok, C2SPid};
        [C2SPid] ->
            {ok, C2SPid};
        [_|_] = Sessions ->
            {error, {multiple_sessions, Sessions}}
    end.

-spec string_type() -> list | binary.
string_type() ->
    [{config, hosts,
      [XMPPDomain | _]}] = escalus_ejabberd:rpc(ets, lookup, [config, hosts]),
    case XMPPDomain of
        BString when is_binary(BString) ->
            binary;
        String when is_list(String) ->
            list
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

clear_session_table() ->
    escalus_ejabberd:rpc(mnesia, clear_table, [session]).
