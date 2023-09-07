%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Suite for testing s2s connection
%%% @end
%%%===================================================================

-module(s2s_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Module aliases
-define(dh, distributed_helper).
-import(distributed_helper, [mim/0, rpc_spec/1, rpc/4]).

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
     {group, node1_tls_required_node2_tls_false},

     {group, dialback}
    ].

groups() ->
    [{both_plain, [sequence], all_tests()},
     {both_tls_optional, [], essentials()},
     {both_tls_required, [], essentials()},

     {node1_tls_optional_node2_tls_required, [], essentials()},
     {node1_tls_required_node2_tls_optional, [], essentials()},

     %% Node1 closes connection from nodes with invalid certs
     {node1_tls_required_trusted_node2_tls_optional, [], negative()},

     %% Node1 accepts connection provided the cert can be verified
     {node1_tls_optional_node2_tls_required_trusted_with_cachain, [parallel],
      essentials() ++ connection_cases()},

     {node1_tls_false_node2_tls_optional, [], essentials()},
     {node1_tls_optional_node2_tls_false, [], essentials()},

     {node1_tls_false_node2_tls_required, [], negative()},
     {node1_tls_required_node2_tls_false, [], negative()},
     {dialback, [], [dialback_key_is_synchronized_on_different_nodes]}].

essentials() ->
    [simple_message].

all_tests() ->
    [connections_info, nonexistent_user, unknown_domain, malformed_jid,
     dialback_with_wrong_key | essentials()].

negative() ->
    [timeout_waiting_for_message].

connection_cases() ->
    [successful_external_auth_with_valid_cert,
     start_stream_fails_for_wrong_namespace,
     start_stream_fails_for_wrong_version,
     start_stream_fails_without_version,
     start_stream_fails_without_host,
     start_stream_fails_for_unknown_host,
     starttls_fails_for_unknown_host,
     only_messages_from_authenticated_domain_users_are_accepted,
     auth_with_valid_cert_fails_when_requested_name_is_not_in_the_cert,
     auth_with_valid_cert_fails_for_other_mechanism_than_external].

suite() ->
    distributed_helper:require_rpc_nodes([mim, mim2, fed]) ++ escalus:suite().

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

init_per_group(dialback, Config) ->
    %% Tell mnesia that mim and mim2 nodes are clustered
    distributed_helper:add_node_to_cluster(distributed_helper:mim2(), Config);
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
    %% check that metrics are bounced
    MongooseMetrics = [{[global, data, xmpp, received, s2s], changed},
                       {[global, data, xmpp, sent, s2s], changed}],
    escalus:fresh_story([{mongoose_metrics, MongooseMetrics} | Config],
                        [{alice2, 1}, {alice, 1}], fun(Alice2, Alice1) ->

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
    FedDomain = ct:get_config({hosts, fed, domain}),
    %% there should be at least one in and at least one out connection
    [_ | _] = get_s2s_connections(?dh:mim(), FedDomain, in),
    [_ | _] = get_s2s_connections(?dh:mim(), FedDomain, out),
    ok.

get_s2s_connections(RPCSpec, Domain, Type) ->
    AllS2SConnections = ?dh:rpc(RPCSpec, mongoose_s2s_info, get_connections, [Type]),
    DomainS2SConnections = 
        [Connection || Connection <- AllS2SConnections,
                       Type =/= in orelse [Domain] =:= maps:get(domains, Connection),
                       Type =/= out orelse Domain =:= maps:get(server, Connection)],
    ct:pal("Node = ~p,  ConnectionType = ~p, Domain = ~s~nDomainS2SConnections(~p): ~p",
           [maps:get(node, RPCSpec), Type, Domain, length(DomainS2SConnections),
            DomainS2SConnections]),
    DomainS2SConnections.

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

dialback_with_wrong_key(_Config) ->
    HostType = domain_helper:host_type(mim),
    MimDomain = domain_helper:domain(mim),
    FedDomain = domain_helper:domain(fed),
    FromTo = {MimDomain, FedDomain},
    Key = <<"123456">>, %% wrong key
    StreamId = <<"sdfdsferrr">>,
    StartType = {verify, self(), Key, StreamId},
    {ok, _} = rpc(rpc_spec(mim), ejabberd_s2s_out, start, [FromTo, StartType]),
    receive
        %% Remote server (fed1) rejected out request
        {'$gen_event', {validity_from_s2s_out, false, FromTo}} ->
            ok
        after 5000 ->
            ct:fail(timeout)
    end.

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
    ConnectionArgs = connection_args("localhost.bis", <<"localhost">>, Config),
    {ok, Client, _Features} = escalus_connection:start(ConnectionArgs,
                                                       [fun s2s_start_stream/2,
                                                        fun s2s_starttls/2,
                                                        fun s2s_external_auth/2]),
    escalus_connection:stop(Client).

start_stream_fails_for_wrong_namespace(Config) ->
    start_stream_fails(Config, <<"invalid-namespace">>,
                       [fun s2s_start_stream_with_wrong_namespace/2]).

start_stream_fails_for_wrong_version(Config) ->
    %% TLS authentication requires version 1.0
    start_stream_fails(Config, <<"invalid-xml">>,
                       [fun s2s_start_stream_with_wrong_version/2]).

start_stream_fails_without_version(Config) ->
    %% TLS authentication requires version 1.0
    start_stream_fails(Config, <<"invalid-xml">>,
                       [fun s2s_start_stream_without_version/2]).

start_stream_fails_without_host(Config) ->
    start_stream_fails(Config, <<"improper-addressing">>,
                       [fun s2s_start_stream_without_host/2]).

start_stream_fails_for_unknown_host(Config) ->
    start_stream_fails(Config, <<"host-unknown">>,
                       [fun s2s_start_stream_to_wrong_host/2]).

starttls_fails_for_unknown_host(Config) ->
    start_stream_fails(Config, <<"host-unknown">>,
                       [fun s2s_start_stream/2,
                        fun s2s_starttls_to_wrong_host/2]).

start_stream_fails(Config, ErrorType, ConnectionSteps) ->
    ConnectionArgs = connection_args("localhost.bis", <<"localhost">>, Config),
    {ok, Client, _} = escalus_connection:start(ConnectionArgs, ConnectionSteps),
    [Start, Error, End] = escalus:wait_for_stanzas(Client, 3),
    escalus:assert(is_stream_start, Start),
    escalus:assert(is_stream_error, [ErrorType, <<>>], Error),
    escalus:assert(is_stream_end, End).

only_messages_from_authenticated_domain_users_are_accepted(Config) ->
    ConnectionArgs = connection_args("localhost.bis", <<"localhost">>, Config),
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
    ConnectionArgs = connection_args("not_in_cert_domain", <<"not_in_cert_domain">>, Config),
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
    ConnectionArgs = connection_args("localhost", <<"localhost">>, Config),
    {ok, Client, _Features} = escalus_connection:start(ConnectionArgs,
                                                       [fun s2s_start_stream/2,
                                                        fun s2s_starttls/2
                                                       ]),

    Stanza = escalus_stanza:auth(<<"ANONYMOUS">>),
    ok = escalus_connection:send(Client, Stanza),
    #xmlel{name = <<"failure">>} = escalus_connection:get_stanza(Client, wait_for_auth_reply),

    escalus_connection:wait_for_close(Client, timer:seconds(5)).

connection_args(FromServer, RequestedName, Config) ->
    {KeyFile, CertFile} = get_main_key_and_cert_files(Config),
    [{host, "localhost"},
     {to_server, "fed1"},
     {from_server, FromServer},
     {requested_name, RequestedName},
     {starttls, required},
     {port, ct:get_config({hosts, fed, incoming_s2s_port})},
     {ssl_opts, [{versions, ['tlsv1.2']}, {verify, verify_none}, {certfile, CertFile}, {keyfile, KeyFile}]}].

s2s_start_stream_with_wrong_namespace(Conn = #client{props = Props}, Features) ->
    Start = s2s_stream_start_stanza(Props, fun(Attrs) -> Attrs#{<<"xmlns">> => <<"42">>} end),
    ok = escalus_connection:send(Conn, Start),
    {Conn, Features}.

s2s_start_stream_with_wrong_version(Conn = #client{props = Props}, Features) ->
    Start = s2s_stream_start_stanza(Props, fun(Attrs) -> Attrs#{<<"version">> => <<"42">>} end),
    ok = escalus_connection:send(Conn, Start),
    {Conn, Features}.

s2s_start_stream_without_version(Conn = #client{props = Props}, Features) ->
    Start = s2s_stream_start_stanza(Props, fun(Attrs) -> maps:remove(<<"version">>, Attrs) end),
    ok = escalus_connection:send(Conn, Start),
    {Conn, Features}.

s2s_start_stream_without_host(Conn = #client{props = Props}, Features) ->
    Start = s2s_stream_start_stanza(Props, fun(Attrs) -> maps:remove(<<"to">>, Attrs) end),
    ok = escalus_connection:send(Conn, Start),
    {Conn, Features}.

s2s_start_stream_to_wrong_host(Conn = #client{props = Props}, Features) ->
    Start = s2s_stream_start_stanza(Props, fun(Attrs) -> Attrs#{<<"to">> => <<"42">>} end),
    ok = escalus_connection:send(Conn, Start),
    {Conn, Features}.

s2s_start_stream(Conn = #client{props = Props}, []) ->
    StreamStartRep = s2s_start_stream_and_wait_for_response(Conn),

    #xmlstreamstart{attrs = Attrs} = StreamStartRep,
    Id = proplists:get_value(<<"id">>, Attrs),

    escalus_session:stream_features(Conn#client{props = [{sid, Id} | Props]}, []).

s2s_start_stream_and_wait_for_response(Conn = #client{props = Props}) ->
    StreamStart = s2s_stream_start_stanza(Props, fun(Attrs) -> Attrs end),
    ok = escalus_connection:send(Conn, StreamStart),
    escalus_connection:get_stanza(Conn, wait_for_stream).

s2s_stream_start_stanza(Props, F) ->
    Attrs = (stream_start_attrs())#{<<"to">> => proplists:get_value(to_server, Props),
                                    <<"from">> => proplists:get_value(from_server, Props)},
    #xmlstreamstart{name = <<"stream:stream">>, attrs = maps:to_list(F(Attrs))}.

stream_start_attrs() ->
    #{<<"xmlns">> => <<"jabber:server">>,
      <<"xmlns:stream">> => <<"http://etherx.jabber.org/streams">>,
      <<"version">> => <<"1.0">>}.

s2s_starttls(Client, Features, StartStreamF) ->
    case proplists:get_value(starttls, Features) of
        false ->
            ct:fail("The server does not offer STARTTLS");
        _ ->
            ok
    end,

    escalus_connection:send(Client, escalus_stanza:starttls()),
    escalus_connection:get_stanza(Client, proceed),
    escalus_connection:upgrade_to_tls(Client),
    StartStreamF(Client, []).

s2s_starttls(Client, Features) ->
    s2s_starttls(Client, Features, fun s2s_start_stream/2).

s2s_starttls_to_wrong_host(Client, Features) ->
    s2s_starttls(Client, Features, fun s2s_start_stream_to_wrong_host/2).

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

dialback_key_is_synchronized_on_different_nodes(_Config) ->
    configure_secret_and_restart_s2s(mim),
    configure_secret_and_restart_s2s(mim2),
    Key1 = get_shared_secret(mim),
    Key2 = get_shared_secret(mim2),
    ?assertEqual(Key1, Key2),
    %% Node 2 is restarted later, so both nodes should have the key.
    ?assertEqual(Key2, {ok, <<"9e438f25e81cf347100b">>}).

get_shared_secret(NodeKey) ->
    HostType = domain_helper:host_type(mim),
    rpc(rpc_spec(NodeKey), mongoose_s2s_backend, get_shared_secret, [HostType]).

set_opt(Spec, Opt, Value) ->
    rpc(Spec, mongoose_config, set_opt, [Opt, Value]).

configure_secret_and_restart_s2s(NodeKey) ->
    HostType = domain_helper:host_type(mim),
    Spec = rpc_spec(NodeKey),
    set_opt(Spec, [{s2s, HostType}, shared], shared_secret(NodeKey)),
    ok = rpc(Spec, supervisor, terminate_child, [ejabberd_sup, ejabberd_s2s]),
    {ok, _} = rpc(Spec, supervisor, restart_child, [ejabberd_sup, ejabberd_s2s]).

shared_secret(mim) -> <<"f623e54a0741269be7dd">>; %% Some random key
shared_secret(mim2) -> <<"9e438f25e81cf347100b">>.
