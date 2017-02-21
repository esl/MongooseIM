-module(bind2_SUITE).

-compile(export_all).

all() ->
    [{group, all}].

groups() ->
    [{all, [parallel], [bind_2_0_succeeds,
                        client_with_bind_2_0_receives_messages,
                        client_with_bind_2_0_receives_carbon_copies]}].
init_per_suite(Config) ->
    escalus_fresh:start(Config),
    Config.

end_per_suite(_Config) ->
    escalus_fresh:clean().

bind_2_0_succeeds(Config0) ->
    Config = escalus_fresh:create_users(Config0, [alice]),
    {ok, Conn, _, _} = connect_with_bind_2_0(Config, alice),
    escalus_connection:stop(Conn).

client_with_bind_2_0_receives_messages(Config0) ->
    Config = escalus_fresh:create_users(Config0, [alice, bob]),
    {ok, Conn, _, _} = connect_with_bind_2_0(Config, alice),
    BobSpec = escalus_users:get_userspec(Config, bob),
    {ok, Bob, _, _} = escalus_connection:start(BobSpec),
    Msg = <<"Hi Alice!">>,
    escalus:send(Bob, escalus_stanza:chat_to(Conn, Msg)),
    RecvStanza = escalus:wait_for_stanza(Conn),
    escalus:assert(is_chat_message, [Msg], RecvStanza),
    escalus_connection:stop(Bob),
    escalus_connection:stop(Conn).

client_with_bind_2_0_receives_carbon_copies(Config0) ->
    Config = escalus_fresh:create_users(Config0, [alice, bob]),
    AliceSpec = escalus_users:get_userspec(Config, alice),
    BobSpec = escalus_users:get_userspec(Config, bob),
    {ok, Alice20, _, _} = connect_with_bind_2_0(AliceSpec),
    {ok, Bob, _, _} = escalus_connection:start(BobSpec),
    {ok, Alice, _, _} = escalus_connection:start(AliceSpec),
    Msg = <<"Hi Alice!">>,
    escalus:send(Bob, escalus_stanza:chat_to(Alice, Msg)),
    RecvAlice = escalus:wait_for_stanza(Alice),
    escalus:assert(is_chat_message, [Msg], RecvAlice),
    RecvAlice20 = escalus:wait_for_stanza(Alice20),
    ct:print("~p", [RecvAlice20]),
    BobJID = escalus_client:full_jid(Bob),
    AliceJID = escalus_client:full_jid(Alice),
    escalus:assert(is_forwarded_received_message, [BobJID, AliceJID, Msg], RecvAlice20),
    escalus_connection:stop(Bob),
    escalus_connection:stop(Alice),
    escalus_connection:stop(Alice20).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

connect_with_bind_2_0(Config, User) ->
    UserSpec = escalus_users:get_userspec(Config, User),
    connect_with_bind_2_0(UserSpec).

connect_with_bind_2_0(UserSpec) ->
    ConnectionSteps = [start_stream, stream_features, authenticate, bind_2_0],
    escalus_connection:start(UserSpec, ConnectionSteps).

