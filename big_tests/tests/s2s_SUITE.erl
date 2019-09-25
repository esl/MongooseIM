%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Suite for testing s2s connection
%%% @end
%%%===================================================================

-module(s2s_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Suite configuration
%%%===================================================================

all() ->
    [
     {group, both_plain},
     {group, both_tls_optional}, %% default MongooseIM config
     {group, both_tls_required},

     {group, node1_tls_optional_node2_tls_required},
     {group, node1_tls_required_node2_tls_optional},

     {group, node1_tls_required_trusted_node2_tls_optional},
     {group, node1_tls_optional_node2_tls_required_trusted_with_cachain},

     {group, node1_tls_false_node2_tls_optional},
     {group, node1_tls_optional_node2_tls_false},

     {group, node1_tls_false_node2_tls_required},
     {group, node1_tls_required_node2_tls_false}

    ].

groups() ->
    G = [{both_plain, [sequence], all_tests()},
         {both_tls_optional, [], essentials()},
         {both_tls_required, [], essentials()},

         {node1_tls_optional_node2_tls_required, [], essentials()},
         {node1_tls_required_node2_tls_optional, [], essentials()},

         %% Node1 closes connection from nodes with invalid certs
         {node1_tls_required_trusted_node2_tls_optional, [], negative()},

         %% Node1 accepts connection provided the cert can be verified
         {node1_tls_optional_node2_tls_required_trusted_with_cachain, [],
          essentials() ++ connection_cases()},

         {node1_tls_false_node2_tls_optional, [], essentials()},
         {node1_tls_optional_node2_tls_false, [], essentials()},

         {node1_tls_false_node2_tls_required, [], negative()},
         {node1_tls_required_node2_tls_false, [], negative()}],
    ct_helper:repeat_all_until_all_ok(G).

essentials() ->
    [simple_message].

all_tests() ->
    essentials() ++ [connections_info, nonexistent_user, unknown_domain, malformed_jid].

negative() ->
    [timeout_waiting_for_message].

connection_cases() ->
    [successful_external_auth_with_valid_cert,
     only_messages_from_authenticated_domain_users_are_accepted,
     auth_with_valid_cert_fails_when_requested_name_is_not_in_the_cert,
     auth_with_valid_cert_fails_for_other_mechanism_than_external].

suite() ->
    s2s_helper:suite(escalus:suite()).

users() ->
    [alice2, alice, bob].



%%%===================================================================
%%% Init & teardown
%%%===================================================================

init_per_suite(Config) ->
    Config1 = s2s_helper:init_s2s(escalus:init_per_suite(Config)),
    escalus:create_users(Config1, escalus:get_users(users())).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    s2s_helper:end_s2s(Config),
    escalus:delete_users(Config, escalus:get_users(users())),
    escalus:end_per_suite(Config).

init_per_group(GroupName, Config) ->
    s2s_helper:configure_s2s(GroupName, Config).

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%%===================================================================
%%% Server-to-server communication test
%%%===================================================================

simple_message(Config) ->
    escalus:fresh_story(Config, [{alice2, 1}, {alice, 1}], fun(Alice2, Alice1) ->

        %% User on the main server sends a message to a user on a federated server
        escalus:send(Alice1, escalus_stanza:chat_to(Alice2, <<"Hi, foreign Alice!">>)),

        %% User on the federated server receives the message
        Stanza = escalus:wait_for_stanza(Alice2, 10000),
        escalus:assert(is_chat_message, [<<"Hi, foreign Alice!">>], Stanza),

        %% User on the federated server sends a message to the main server
        escalus:send(Alice2, escalus_stanza:chat_to(Alice1, <<"Nice to meet you!">>)),

        %% User on the main server receives the message
        Stanza2 = escalus:wait_for_stanza(Alice1, 10000),
        escalus:assert(is_chat_message, [<<"Nice to meet you!">>], Stanza2)

    end).

timeout_waiting_for_message(Config) ->
    try
        simple_message(Config),
        ct:fail("got message but shouldn't")
    catch
        error:timeout_when_waiting_for_stanza ->
            ok
    end.

connections_info(Config) ->
    simple_message(Config),
    Node = ct:get_config({hosts, mim, node}),
    FedDomain = ct:get_config({hosts, fed, domain}),
    S2SIn = distributed_helper:rpc(Node, ejabberd_s2s, get_info_s2s_connections, [in]),
    ct:pal("S2sIn: ~p", [S2SIn]),
    true = lists:any(fun(PropList) -> [FedDomain] =:= proplists:get_value(domains, PropList) end,
                     S2SIn),
    S2SOut = distributed_helper:rpc(Node, ejabberd_s2s, get_info_s2s_connections, [out]),
    ct:pal("S2sOut: ~p", [S2SOut]),
    true = lists:any(fun(PropList) -> FedDomain =:= proplists:get_value(server, PropList) end,
                     S2SOut),

    ok.


nonexistent_user(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {alice2, 1}], fun(Alice1, Alice2) ->

        %% Alice@localhost1 sends message to Xyz@localhost2
        RemoteServer = escalus_client:server(Alice2),
        Fake = <<"xyz@", RemoteServer/binary>>,
        escalus:send(Alice1, escalus_stanza:chat_to(Fake,
                                                    <<"Hello, nonexistent!">>)),

        %% Alice@localhost1 receives stanza error: service-unavailable
        Stanza = escalus:wait_for_stanza(Alice1),
        escalus:assert(is_error, [<<"cancel">>, <<"service-unavailable">>], Stanza)

    end).

unknown_domain(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice1) ->

        %% Alice@localhost1 sends message to Xyz@localhost3
        escalus:send(Alice1, escalus_stanza:chat_to(
            <<"xyz@somebogushost">>,
            <<"Hello, unreachable!">>)),

        %% Alice@localhost1 receives stanza error: remote-server-not-found
        Stanza = escalus:wait_for_stanza(Alice1, 10000),
        escalus:assert(is_error, [<<"cancel">>, <<"remote-server-not-found">>], Stanza)

    end).

malformed_jid(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice1) ->

        %% Alice@localhost1 sends message to Xyz@localhost3
        escalus:send(Alice1, escalus_stanza:chat_to(
            <<"not a jid">>,
            <<"Hello, unreachable!">>)),

        %% Alice@localhost1 receives stanza error: remote-server-not-found
        Stanza = escalus:wait_for_stanza(Alice1, 10000),
        escalus:assert(is_error, [<<"cancel">>, <<"remote-server-not-found">>], Stanza)

    end).

nonascii_addr(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob2, 1}], fun(Alice, Bob) ->

        %% Bob@localhost2 sends message to Alice@localhost1
        escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"Cześć Alice!">>)),

        %% Alice@localhost1 receives message from Bob@localhost2
        Stanza = escalus:wait_for_stanza(Alice, 10000),
        escalus:assert(is_chat_message, [<<"Cześć Alice!">>], Stanza),

        %% Alice@localhost1 sends message to Bob@localhost2
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Miło Cię poznać">>)),

        %% Bob@localhost2 receives message from Alice@localhost1
        Stanza2 = escalus:wait_for_stanza(Bob, 10000),
        escalus:assert(is_chat_message, [<<"Miło Cię poznać">>], Stanza2)

    end).

successful_external_auth_with_valid_cert(Config) ->
    {KeyFile, CertFile} = get_main_key_and_cert_files(Config),
    ConnectionArgs = [{host, "localhost"},
                      {to_server, "fed1"},
                      {from_server, "localhost_bis"},
                      {requested_name, <<"localhost">>},
                      {starttls, required},
                      {port, ct:get_config({hosts, fed, incoming_s2s_port})},
                      {ssl_opts, [{certfile, CertFile},
                                  {keyfile, KeyFile}]}
                     ],
    {ok, Client, _Features} = escalus_connection:start(ConnectionArgs,
                                                       [fun s2s_start_stream/2,
                                                        fun s2s_starttls/2,
                                                        fun s2s_external_auth/2]),
    escalus_connection:stop(Client).

only_messages_from_authenticated_domain_users_are_accepted(Config) ->
    {KeyFile, CertFile} = get_main_key_and_cert_files(Config),
    ConnectionArgs = [{host, "localhost"},
                      {to_server, "fed1"},
                      {from_server, "localhost_bis"},
                      {requested_name, <<"localhost">>},
                      {starttls, required},
                      {port, ct:get_config({hosts, fed, incoming_s2s_port})},
                      {ssl_opts, [{certfile, CertFile},
                                  {keyfile, KeyFile}]}
                     ],
    {ok, Client, _Features} = escalus_connection:start(ConnectionArgs,
                                                       [fun s2s_start_stream/2,
                                                        fun s2s_starttls/2,
                                                        fun s2s_external_auth/2]),
    escalus:fresh_story(Config, [{alice2, 1}], fun(Alice) ->

        UserInWrongDomain = <<"a_user@this_is_not_my.domain.com">>,
        ChatToAliceFromUserInWrongDomain = escalus_stanza:chat(UserInWrongDomain,
                                                               Alice, <<"Miło Cię poznać">>),
        %% Client is a s2s connection established and authenticated for domain "localhost"
        %% Now we try to send a message from other domain than "localhost"
        %% over the established s2s connection
        escalus:send(Client, ChatToAliceFromUserInWrongDomain),

        %% Alice@fed1 does not receives message from a_user@this_is_not_my.domain.com
        timer:sleep(timer:seconds(5)),
        escalus_assert:has_no_stanzas(Alice)

    end),

    escalus_connection:stop(Client).

auth_with_valid_cert_fails_when_requested_name_is_not_in_the_cert(Config) ->
    {KeyFile, CertFile} = get_main_key_and_cert_files(Config),
    ConnectionArgs = [{host, "localhost"},
                      {to_server, "fed1"},
                      {from_server, "some_not_in_cert_domain"},
                      {requested_name, <<"some_not_in_cert_domain">>},
                      {starttls, required},
                      {port, ct:get_config({hosts, fed, incoming_s2s_port})},
                      {ssl_opts, [{certfile, CertFile},
                                  {keyfile, KeyFile}]}
                     ],
    {ok, Client, _Features} = escalus_connection:start(ConnectionArgs,
                                                       [fun s2s_start_stream/2,
                                                        fun s2s_starttls/2]),

    try
        escalus_auth:auth_sasl_external(Client, Client#client.props),
        ct:fail("Authenitcated but MUST NOT")
    catch throw:{auth_failed, _, _} ->
              escalus_connection:wait_for_close(Client, timer:seconds(5))
    end.

auth_with_valid_cert_fails_for_other_mechanism_than_external(Config) ->
    {KeyFile, CertFile} = get_main_key_and_cert_files(Config),
    ConnectionArgs = [{host, "localhost"},
                      {to_server, "fed1"},
                      {from_server, "localhost"},
                      {requested_name, <<"localhost">>},
                      {starttls, required},
                      {port, ct:get_config({hosts, fed, incoming_s2s_port})},
                      {ssl_opts, [{certfile, CertFile},
                                  {keyfile, KeyFile}]}
                     ],
    {ok, Client, _Features} = escalus_connection:start(ConnectionArgs,
                                                       [fun s2s_start_stream/2,
                                                        fun s2s_starttls/2
                                                       ]),

    Stanza = escalus_stanza:auth(<<"ANONYMOUS">>),
    ok = escalus_connection:send(Client, Stanza),
    #xmlel{name = <<"failure">>} = escalus_connection:get_stanza(Client, wait_for_auth_reply),

    escalus_connection:wait_for_close(Client, timer:seconds(5)).

s2s_start_stream(Conn = #client{props = Props}, []) ->
    StreamStartRep = s2s_start_stream_and_wait_for_response(Conn),

    #xmlstreamstart{attrs = Attrs} = StreamStartRep,
    Id = proplists:get_value(<<"id">>, Attrs),

    escalus_session:stream_features(Conn#client{props = [{sid, Id} | Props]}, []).

s2s_start_stream_and_wait_for_response(Conn = #client{props = Props}) ->
    {to_server, To} = lists:keyfind(to_server, 1, Props),
    {from_server, From} = lists:keyfind(from_server, 1, Props),

    StreamStart = s2s_stream_start_stanza(To, From),
    ok = escalus_connection:send(Conn, StreamStart),
    escalus_connection:get_stanza(Conn, wait_for_stream).

s2s_stream_start_stanza(To, From) ->
    Attrs = [{<<"to">>, To},
             {<<"from">>, From},
             {<<"xmlns">>, <<"jabber:server">>},
             {<<"xmlns:stream">>,
              <<"http://etherx.jabber.org/streams">>},
             {<<"version">>, <<"1.0">>}],
    #xmlstreamstart{name = <<"stream:stream">>, attrs = Attrs}.

s2s_starttls(Client, Features) ->
    case proplists:get_value(starttls, Features) of
        false ->
            ct:fail("The server does not offer STARTTLS");
        _ ->
            ok
    end,

    escalus_connection:send(Client, escalus_stanza:starttls()),
    escalus_connection:get_stanza(Client, proceed),
    escalus_connection:upgrade_to_tls(Client),
    s2s_start_stream(Client, []).

s2s_external_auth(Client = #client{props = Props}, Features) ->
    case proplists:get_value(sasl_mechanisms, Features) of
        [<<"EXTERNAL">>] ->
            ok;
        SASL ->
            ct:fail("Server does not provide EXTERNAL auth: ~p", [SASL])
    end,
    escalus_auth:auth_sasl_external(Client, Props),
    s2s_start_stream(Client, []).

get_main_key_and_cert_files(Config) ->
    CertFile = get_main_file_path(Config, "cert.pem"),
    KeyFile = get_main_file_path(Config, "key.pem"),
    {KeyFile, CertFile}.

get_main_file_path(Config, File) ->
    filename:join([path_helper:repo_dir(Config),
                   "tools", "ssl", "mongooseim", File]).

