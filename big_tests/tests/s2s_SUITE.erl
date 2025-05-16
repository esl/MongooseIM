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

-import(distributed_helper, [mim/0, rpc_spec/1, rpc/4]).
-import(domain_helper, [host_type/0]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Suite configuration
%%%===================================================================

all() ->
    [
     {group, both_plain},
     {group, both_tls_optional}, %% default MongooseIM config
     {group, both_tls_required},
     {group, both_tls_enforced},

     {group, node1_tls_optional_node2_tls_required},
     {group, node1_tls_required_node2_tls_optional},

     {group, node1_tls_required_trusted_node2_tls_optional},
     {group, node1_tls_optional_node2_tls_required_trusted_with_cachain},

     {group, node1_tls_false_node2_tls_required},
     {group, node1_tls_required_node2_tls_false},

     {group, dialback}
    ].

groups() ->
    [{both_plain, [], all_tests()},
     {both_tls_optional, [], essentials()},
     {both_tls_required, [], essentials()},
     {both_tls_enforced, [], essentials()},

     {node1_tls_optional_node2_tls_required, [], essentials()},
     {node1_tls_required_node2_tls_optional, [], essentials()},

     %% Node1 closes connection from nodes with invalid certs
     {node1_tls_required_trusted_node2_tls_optional, [], negative()},

     %% Node1 accepts connection provided the cert can be verified
     {node1_tls_optional_node2_tls_required_trusted_with_cachain, [parallel],
      essentials() ++ connection_cases() ++ start_stream_error_groups()},

     {node1_tls_false_node2_tls_required, [], negative()},
     {node1_tls_required_node2_tls_false, [], negative()},
     {dialback, [], [dialback_key_is_synchronized_on_different_nodes]},

     {start_stream_errors, [parallel], start_stream_error_cases()},
     {start_stream_errors_after_starttls, [parallel], start_stream_error_cases()},
     {start_stream_errors_after_auth, [parallel], start_stream_error_cases()}
    ].

essentials() ->
    [simple_message].

all_tests() ->
    [connections_info,
     dns_srv_discovery,
     dns_ip_discovery,
     dns_discovery_fail,
     nonexistent_user,
     unknown_domain,
     malformed_jid,
     dialback_with_wrong_key].

negative() ->
    [timeout_waiting_for_message].

connection_cases() ->
    [successful_external_auth_with_valid_cert,
     only_messages_from_authenticated_domain_users_are_accepted,
     auth_with_valid_cert_fails_when_requested_name_is_not_in_the_cert,
     auth_with_valid_cert_fails_for_other_mechanism_than_external,
     simple_message_auth_failed].

start_stream_error_groups() ->
    [{group, start_stream_errors},
     {group, start_stream_errors_after_starttls},
     {group, start_stream_errors_after_auth}].

start_stream_error_cases() ->
    [start_stream_fails_for_wrong_namespace,
     start_stream_fails_for_wrong_version,
     start_stream_fails_without_version,
     start_stream_fails_without_host,
     start_stream_fails_for_unknown_host].

suite() ->
    distributed_helper:require_rpc_nodes([mim, mim2, fed]) ++ escalus:suite().

%%%===================================================================
%%% Init & teardown
%%%===================================================================

init_per_suite(Config0) ->
    ct:pal("Tested events: ~p~n", [tested_events()]),
    instrument_helper:start(tested_events()),
    mongoose_helper:inject_module(?MODULE, reload),
    Config1 = escalus:init_per_suite(Config0),
    s2s_helper:init_s2s(Config1).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    s2s_helper:end_s2s(Config),
    escalus:end_per_suite(Config),
    instrument_helper:stop().

init_per_group(dialback, Config) ->
    %% Tell mnesia that mim and mim2 nodes are clustered
    distributed_helper:add_node_to_cluster(distributed_helper:mim2(), Config);
init_per_group(both_tls_enforced, Config) ->
    meck_dns_srv_lookup("fed1", srv_ssl),
    Config1 = s2s_helper:configure_s2s(both_tls_enforced, Config),
    [{requires_tls, group_with_tls(both_tls_enforced)}, {group, both_tls_enforced} | Config1];
init_per_group(start_stream_errors, Config) ->
    [{initial_steps, []} | Config];
init_per_group(start_stream_errors_after_starttls, Config) ->
    [{initial_steps, [fun s2s_start_stream/2,
                      fun s2s_starttls/2]} | Config];
init_per_group(start_stream_errors_after_auth, Config) ->
    [{initial_steps, [fun s2s_start_stream/2,
                      fun s2s_starttls/2,
                      fun s2s_start_stream/2,
                      fun s2s_external_auth/2]} | Config];
init_per_group(GroupName, Config) ->
    Config1 = s2s_helper:configure_s2s(GroupName, Config),
    [{requires_tls, group_with_tls(GroupName)}, {group, GroupName} | Config1].

end_per_group(both_tls_enforced, _Config) ->
    rpc(mim(), meck, unload, []);
end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(dns_srv_discovery = CaseName, Config) ->
    meck_dns_srv_lookup("fed2", srv),
    Config1 = escalus_users:update_userspec(Config, alice2, server, <<"fed2">>),
    escalus:init_per_testcase(CaseName, Config1);
init_per_testcase(dns_ip_discovery = CaseName, Config) ->
    case is_github_actions() of
        true ->
            {skip, "Test skipped for GH Actions"};
        false ->
            meck_dns_srv_lookup("fed2", ip),
            Config1 = escalus_users:update_userspec(Config, alice2, server, <<"fed2">>),
            escalus:init_per_testcase(CaseName, Config1)
    end;
init_per_testcase(dns_discovery_fail = CaseName, Config) ->
    case is_github_actions() of
        true ->
            {skip, "Test skipped for GH Actions"};
        false ->
            meck_dns_srv_lookup("fed3", none),
            escalus:init_per_testcase(CaseName, Config)
    end;
init_per_testcase(unknown_domain = CaseName, Config) ->
    case is_github_actions() of
        true ->
            {skip, "Test skipped for GH Actions"};
        false ->
            escalus:init_per_testcase(CaseName, Config)
    end;
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

meck_dns_srv_lookup(Domain, Which) ->
    FedPort = ct:get_config({hosts, fed, incoming_s2s_port}),
    ok = rpc(mim(), meck, new, [inet_res, [no_link, unstick, passthrough]]),
    ok = rpc(mim(), meck, expect, [inet_res, lookup, inet_res_lookup_fun(Domain, FedPort, Which)]).

inet_res_lookup_fun(Domain, FedPort, srv_ssl) ->
    fun("_xmpps-server._tcp." ++ Domain1, in, srv, _Opts, _Timeout) when Domain1 =:= Domain ->
            [{30, 0, FedPort, "localhost"}];
       (Name, Class, Type, Opts, Timeout) ->
            meck:passthrough([Name, Class, Type, Opts, Timeout])
    end;
inet_res_lookup_fun(Domain, FedPort, srv) ->
    fun("_xmpp-server._tcp." ++ Domain1, in, srv, _Opts, _Timeout) when Domain1 =:= Domain ->
            [{30, 0, FedPort, "localhost"}];
       (Name, Class, Type, Opts, Timeout) ->
            meck:passthrough([Name, Class, Type, Opts, Timeout])
    end;
inet_res_lookup_fun(Domain, _FedPort, ip) ->
    fun(Domain1, in, a, _Opts, _Timeout) when Domain1 =:= Domain ->
            [{127, 0, 0, 1}];
       (Name, Class, Type, Opts, Timeout) ->
            meck:passthrough([Name, Class, Type, Opts, Timeout])
    end;
inet_res_lookup_fun(Domain, _FedPort, none) ->
    fun("_xmpp-server._tcp." ++ Domain1, in, srv, _Opts, _Timeout) when Domain1 =:= Domain ->
            {error, nxdomain};
       (Domain1, in, inet, _Opts, _Timeout) when Domain1 =:= Domain ->
            {error, nxdomain};
       (Name, Class, Type, Opts, Timeout) ->
            meck:passthrough([Name, Class, Type, Opts, Timeout])
    end.

is_github_actions() ->
    case os:getenv("GITHUB_ACTIONS") of
        "true" -> true;
        _ -> false
    end.

end_per_testcase(CaseName, Config) when CaseName =:= dns_srv_discovery;
                                        CaseName =:= dns_ip_discovery;
                                        CaseName =:= dns_discovery_fail ->
    rpc(mim(), meck, unload, []),
    s2s_helper:reset_s2s_connections(),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

%%%===================================================================
%%% Server-to-server communication test
%%%===================================================================

simple_message(Config) ->
    escalus:fresh_story(Config, [{alice2, 1}, {alice, 1}], fun(Alice2, Alice1) ->
        TS = instrument_helper:timestamp(),

        %% User on the main server sends a message to a user on a federated server
        escalus:send(Alice1, escalus_stanza:chat_to(Alice2, <<"Hi, foreign Alice!">>)),

        %% User on the federated server receives the message
        Stanza = escalus:wait_for_stanza(Alice2, 5000),
        escalus:assert(is_chat_message, [<<"Hi, foreign Alice!">>], Stanza),

        %% User on the federated server sends a message to the main server
        escalus:send(Alice2, escalus_stanza:chat_to(Alice1, <<"Nice to meet you!">>)),

        %% User on the main server receives the message
        Stanza2 = escalus:wait_for_stanza(Alice1, 5000),
        escalus:assert(is_chat_message, [<<"Nice to meet you!">>], Stanza2),

        % Instrumentation events are executed
        assert_events(TS, Config)
    end).

simple_message_auth_failed(Config) ->
    escalus:fresh_story(Config, [{alice2, 1}, {alice_bis, 1}], fun(Alice2, Alice1) ->
        TS = instrument_helper:timestamp(),

        % User on the main server sends a message to a user on a federated server
        escalus:send(Alice1, escalus_stanza:chat_to(Alice2, <<"Hi, foreign Alice!">>)),

        % Cert is for localhost, and the domain is localhost.bis
        % As a result, s2s auth fails, and Alice2 receives nothing
        BisDomain = ct:get_config({hosts, mim, secondary_domain}),
        FedDomain = ct:get_config({hosts, fed, domain}),
        Filter = fun(#{direction := out, count := 1, local_domain := LD, remote_domain := RD}) ->
                         LD =:= BisDomain andalso RD =:= FedDomain
                 end,
        Opts = #{retries => 50, delay => 100, min_timestamp => TS, expected_count => 1},
        instrument_helper:assert(s2s_auth_failed, #{}, Filter, Opts),
        timer:sleep(500),
        escalus_assert:has_no_stanzas(Alice2)
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
    [_ | _] = get_s2s_connections(mim(), FedDomain, in),
    [_ | _] = get_s2s_connections(mim(), FedDomain, out),
    ok.

dns_srv_discovery(Config) ->
    simple_message(Config),
    %% Ensure that the mocked DNS discovery for connecting to the other server
    History = rpc(mim(), meck, history, [inet_res]),
    ?assert(0 < length(History), History),
    ?assert(s2s_helper:has_xmpp_server(History, "_xmpp-server._tcp.fed2", srv), History),
    ok.

dns_ip_discovery(Config) ->
    simple_message(Config),
    %% Ensure that the mocked DNS discovery for connecting to the other server
    History = rpc(mim(), meck, history, [inet_res]),
    ?assert(0 < length(History), History),
    ?assert(s2s_helper:has_xmpp_server(History, "fed2", a), History),
    ok.


dns_discovery_fail(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice1) ->
        escalus:send(Alice1, escalus_stanza:chat_to(<<"alice2@fed3">>, <<"Hello, second Alice!">>)),
        Stanza = escalus:wait_for_stanza(Alice1, 5000),
        escalus:assert(is_error, [<<"cancel">>, <<"remote-server-not-found">>], Stanza),
        History = rpc(mim(), meck, history, [inet_res]),
        ?assert(s2s_helper:has_inet_errors(History, "fed3"), History)
    end).

get_s2s_connections(RPCSpec, Domain, Type) ->
    AllS2SConnections = rpc(RPCSpec, mongoose_s2s_info, get_connections, [Type]),
    DomainS2SConnections =
        [Connection || Connection <- AllS2SConnections,
                       Type =/= in orelse [Domain] =:= maps:get(domains, Connection),
                       Type =/= out orelse Domain =:= maps:get(server, Connection)],
    ct:pal("Node = ~p,  ConnectionType = ~p, Domain = ~s~nDomainS2SConnections(~p): ~p~nAll Connections: ~p",
           [maps:get(node, RPCSpec), Type, Domain, length(DomainS2SConnections),
            DomainS2SConnections, AllS2SConnections]),
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
        Stanza = escalus:wait_for_stanza(Alice1, 5000),
        escalus:assert(is_error, [<<"cancel">>, <<"remote-server-not-found">>], Stanza)

    end).

malformed_jid(Config) ->
    escalus:fresh_story(Config, [{alice, 1}], fun(Alice1) ->

        %% Alice@localhost1 sends message to Xyz@localhost3
        escalus:send(Alice1, escalus_stanza:chat_to(
            <<"not a jid">>,
            <<"Hello, unreachable!">>)),

        %% Alice@localhost1 receives stanza error: remote-server-not-found
        Stanza = escalus:wait_for_stanza(Alice1, 5000),
        escalus:assert(is_error, [<<"cancel">>, <<"remote-server-not-found">>], Stanza)

    end).

dialback_with_wrong_key(_Config) ->
    MimDomain = domain_helper:domain(mim),
    FedDomain = domain_helper:domain(fed),
    FromTo = {MimDomain, FedDomain},
    Key = <<"123456">>, %% wrong key
    StreamId = <<"sdfdsferrr">>,
    StartType = {verify, self(), Key, StreamId},
    {ok, _} = rpc(rpc_spec(mim), mongoose_s2s_out, start_connection, [FromTo, StartType]),
    receive
        %% Remote server (fed1) rejected out request
        {'$gen_cast', {validity_from_s2s_out, false, FromTo}} ->
            ok
    after 5000 ->
              ct:fail(timeout)
    end.

nonascii_addr(Config) ->
    escalus:fresh_story(Config, [{alice, 1}, {bob2, 1}], fun(Alice, Bob) ->

        %% Bob@localhost2 sends message to Alice@localhost1
        escalus:send(Bob, escalus_stanza:chat_to(Alice, <<"Cześć Alice!">>)),

        %% Alice@localhost1 receives message from Bob@localhost2
        Stanza = escalus:wait_for_stanza(Alice, 5000),
        escalus:assert(is_chat_message, [<<"Cześć Alice!">>], Stanza),

        %% Alice@localhost1 sends message to Bob@localhost2
        escalus:send(Alice, escalus_stanza:chat_to(Bob, <<"Miło Cię poznać">>)),

        %% Bob@localhost2 receives message from Alice@localhost1
        Stanza2 = escalus:wait_for_stanza(Bob, 5000),
        escalus:assert(is_chat_message, [<<"Miło Cię poznać">>], Stanza2)

    end).

successful_external_auth_with_valid_cert(Config) ->
    ConnectionArgs = connection_args("localhost.bis", <<"localhost">>, Config),
    {ok, Client, _Features} = escalus_connection:start(ConnectionArgs,
                                                       [fun s2s_start_stream/2,
                                                        fun s2s_starttls/2,
                                                        fun s2s_start_stream/2,
                                                        fun s2s_external_auth/2,
                                                        fun s2s_start_stream/2]),
    escalus_connection:stop(Client).

start_stream_fails_for_wrong_namespace(Config) ->
    Steps = ?config(initial_steps, Config) ++ [fun s2s_start_stream_with_wrong_namespace/2],
    start_stream_fails(Config, <<"invalid-namespace">>, Steps).

start_stream_fails_for_wrong_version(Config) ->
    Steps = ?config(initial_steps, Config) ++ [fun s2s_start_stream_with_wrong_version/2],
    start_stream_fails(Config, <<"invalid-xml">>, Steps).

start_stream_fails_without_version(Config) ->
    Steps = ?config(initial_steps, Config) ++ [fun s2s_start_stream_without_version/2],
    start_stream_fails(Config, <<"invalid-xml">>, Steps).

start_stream_fails_without_host(Config) ->
    Steps = ?config(initial_steps, Config) ++ [fun s2s_start_stream_without_host/2],
    start_stream_fails(Config, <<"improper-addressing">>, Steps).

start_stream_fails_for_unknown_host(Config) ->
    Steps = ?config(initial_steps, Config) ++ [fun s2s_start_stream_to_wrong_host/2],
    start_stream_fails(Config, <<"host-unknown">>, Steps).

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
                                                        fun s2s_start_stream/2,
                                                        fun s2s_external_auth/2,
                                                        fun s2s_start_stream/2]),
    escalus:fresh_story(Config, [{alice2, 1}], fun(Alice) ->

        UserInWrongDomain = <<"a_user@this_is_not_my.domain.com">>,
        ChatToAliceFromUserInWrongDomain = escalus_stanza:chat(UserInWrongDomain,
                                                               Alice, <<"Miło Cię poznać">>),
        %% Client is a s2s connection established and authenticated for domain "localhost"
        %% Now we try to send a message from other domain than "localhost"
        %% over the established s2s connection
        escalus:send(Client, ChatToAliceFromUserInWrongDomain),

        %% Alice@fed1 does not receives message from a_user@this_is_not_my.domain.com
        timer:sleep(500),
        escalus_assert:has_no_stanzas(Alice)

    end),

    escalus_connection:stop(Client).

auth_with_valid_cert_fails_when_requested_name_is_not_in_the_cert(Config) ->
    ConnectionArgs = connection_args("not_in_cert_domain", <<"not_in_cert_domain">>, Config),
    {ok, Client, _Features} = escalus_connection:start(ConnectionArgs,
                                                       [fun s2s_start_stream/2,
                                                        fun s2s_starttls/2,
                                                        fun s2s_start_stream/2]),

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
                                                        fun s2s_starttls/2,
                                                        fun s2s_start_stream/2]),

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
    Id = maps:get(<<"id">>, Attrs, undefined),

    escalus_session:stream_features(Conn#client{props = [{sid, Id} | Props]}, []).

s2s_start_stream_and_wait_for_response(Conn = #client{props = Props}) ->
    StreamStart = s2s_stream_start_stanza(Props, fun(Attrs) -> Attrs end),
    ok = escalus_connection:send(Conn, StreamStart),
    escalus_connection:get_stanza(Conn, wait_for_stream).

s2s_stream_start_stanza(Props, F) ->
    Attrs0 = stream_start_attrs(),
    Attrs = Attrs0#{<<"to">> => proplists:get_value(to_server, Props),
                    <<"from">> => proplists:get_value(from_server, Props)},
    #xmlstreamstart{name = <<"stream:stream">>, attrs = F(Attrs)}.

stream_start_attrs() ->
    #{<<"xmlns">> => <<"jabber:server">>,
      <<"xmlns:stream">> => <<"http://etherx.jabber.org/streams">>,
      <<"version">> => <<"1.0">>}.

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
    {Client, []}.

s2s_external_auth(Client = #client{props = Props}, Features) ->
    case proplists:get_value(sasl_mechanisms, Features) of
        [<<"EXTERNAL">>] ->
            ok;
        SASL ->
            ct:fail("Server does not provide EXTERNAL auth: ~p", [SASL])
    end,
    escalus_auth:auth_sasl_external(Client, Props),
    escalus_connection:reset_parser(Client),
    {Client, []}.

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

assert_events(TS, Config) ->
    TLS = proplists:get_value(requires_tls, Config, false),
    {DataInEvent, DataOutEvent} = data_events(TLS),
    Labels = #{connection_type => s2s},
    Opts = #{min_timestamp => TS},

    SizeFilter = fun(#{byte_size := S}) -> S > 0 end,
    instrument_helper:assert(DataInEvent, Labels, SizeFilter, Opts),
    instrument_helper:assert(DataOutEvent, Labels, SizeFilter, Opts),

    Opts2 = Opts#{expected_count => element_count(TLS)},
    MimDomain = domain_helper:domain(),
    DomainFilter = fun(#{lserver := LServer}) -> LServer =:= MimDomain end,
    CombinedFilter = fun(M) -> DomainFilter(M) andalso SizeFilter(M) end,
    Labels2 = Labels#{host_type => host_type()},
    instrument_helper:assert(xmpp_element_in, Labels2, CombinedFilter, Opts2),
    instrument_helper:assert(xmpp_element_out, Labels2, CombinedFilter, Opts2).

data_events(true) -> {tls_data_in, tls_data_out};
data_events(false) -> {tcp_data_in, tcp_data_out}.

% Some of these steps happen asynchronously, so the order may be different.
% Since S2S connections are unidirectional, mim1 acts both as initiating,
% and receiving (and authoritative) server in the dialback procedure.
%
% We also test that both users on each side of federation can text each other,
% hence both servers will run the dialback each.
%
% When a user in mim1 writes to a user in fed1, from the perspective of mim1:
% - Open an outgoing connection from mim1 to fed1:
%   Outgoing stream starts
%   Incoming stream starts
%   1. Incoming stream features
%   2. Outgoing dialback key (step 1)
% - Open an incoming connection from fed1 to mim1:
%   Incoming stream starts
%   Outgoing stream starts
%   3. Outgoing stream features
%   4. Incoming dialback verification request (step 2, as authoritative server)
%   5. Outgoing dialback verification response (step 3, as receiving server)
% - Original outgoing connection
%   6. Incoming dialback result (step 4, as initiating server)
%   7. Outgoing message from user in mim1 to user in fed1
%
% Likewise, when a user in fed1 writes to a user in mim1, from the perspective of mim1:
% - Open an incoming connection from fed1 to mim1:
%   Incoming stream starts
%   Outgoing stream starts
%   1. Outgoing stream features
%   2. Incoming dialback key (step 1)
% - Open an outgoing connection from mim1 to fed1:
%   Outgoing stream starts
%   Incoming stream starts
%   3. Incoming stream features
%   4. Outgoing dialback verification request (step 2, as authoritative server)
%   5. Incoming dialback verification response (step 3, as receiving server)
% - Original incoming connection
%   6. Outgoing dialback result (step 4, as initiating server)
%   7. Incoming message from user in fed1 to user in mim1
%
% The number can be seen as the sum of all arrows from the dialback diagram, since mim
% acts as all three roles in the two dialback procedures that occur:
% https://xmpp.org/extensions/xep-0220.html#intro-howitworks
% (6 arrows) + one for the actual message
element_count(true) ->
    % TLS tests are not checking a specific number of events, because the numbers are flaky
    positive;
element_count(false) ->
    7.

group_with_tls(both_tls_optional) -> true;
group_with_tls(both_tls_required) -> true;
group_with_tls(both_tls_enforced) -> true;
group_with_tls(node1_tls_optional_node2_tls_required) -> true;
group_with_tls(node1_tls_required_node2_tls_optional) -> true;
group_with_tls(node1_tls_required_trusted_node2_tls_optional) -> true;
group_with_tls(node1_tls_optional_node2_tls_required_trusted_with_cachain) -> true;
group_with_tls(_GN) -> false.

tested_events() ->
    lists:filter(fun is_event_tested/1,
                 instrument_helper:declared_events(mongoose_s2s_listener, [#{}])).

is_event_tested({_Event, #{host_type := HostType}}) -> HostType =:= host_type();
is_event_tested({_Event, _Labels}) -> true.
