%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% @doc Suite for testing s2s connection
%%% @end
%%%===================================================================

-module(s2s_SUITE).
-compile(export_all).

-import(distributed_helper, [rpc/4, rpc/5]).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

-record(s2s_opts, {
          node1_s2s_certfile = undefined,
          node1_s2s_use_starttls = undefined,
          node2_s2s_certfile = undefined,
          node2_s2s_use_starttls = undefined
         }).

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

     {group, node1_tls_false_node2_tls_optional},
     {group, node1_tls_optional_node2_tls_false},

     {group, node1_tls_false_node2_tls_required},
     {group, node1_tls_required_node2_tls_false}

    ].

groups() ->
    [{both_plain, [sequence], all_tests()},
     {both_tls_optional, [], essentials()},
     {both_tls_required, [], essentials()},

     {node1_tls_optional_node2_tls_required, [], essentials()},
     {node1_tls_required_node2_tls_optional, [], essentials()},

     %% Node1 closes connection with "self-signed certificate" reason
     {node1_tls_required_trusted_node2_tls_optional, [], negative()},

     {node1_tls_false_node2_tls_optional, [], essentials()},
     {node1_tls_optional_node2_tls_false, [], essentials()},

     {node1_tls_false_node2_tls_required, [], negative()},
     {node1_tls_required_node2_tls_false, [], negative()}
    ].

essentials() ->
    [simple_message].

all_tests() ->
    essentials() ++ [nonexistent_user, unknown_domain].

negative() ->
    [timeout_waiting_for_message].

suite() ->
    require_s2s_nodes() ++
    escalus:suite().

require_s2s_nodes() ->
    [{require, mim_node, {hosts, mim, node}},
     {require, fed_node, {hosts, fed, node}}].

%%%===================================================================
%%% Init & teardown
%%%===================================================================

init_per_suite(Config0) ->
    Node1S2SCertfile = rpc(mim(), ejabberd_config, get_local_option, [s2s_certfile]),
    Node1S2SUseStartTLS = rpc(mim(), ejabberd_config, get_local_option, [s2s_use_starttls]),

    rpc(fed(), mongoose_cover_helper, start, [[ejabberd]]),

    Node2S2SCertfile = rpc(fed(), ejabberd_config, get_local_option, [s2s_certfile]),
    Node2S2SUseStartTLS = rpc(fed(), ejabberd_config, get_local_option, [s2s_use_starttls]),
    S2S = #s2s_opts{node1_s2s_certfile = Node1S2SCertfile,
                    node1_s2s_use_starttls = Node1S2SUseStartTLS,
                    node2_s2s_certfile = Node2S2SCertfile,
                    node2_s2s_use_starttls = Node2S2SUseStartTLS},

    Config1 = [{s2s_opts, S2S} | escalus:init_per_suite(Config0)],
    Config2 = [{escalus_user_db, xmpp} | Config1],
    escalus:create_users(Config2, escalus:get_users([alice2, alice, bob])).

end_per_suite(Config) ->
    S2SOrig = ?config(s2s_opts, Config),
    configure_s2s(S2SOrig),
    rpc(fed(), mongoose_cover_helper, analyze, []),
    escalus:delete_users(Config, escalus:get_users([alice, bob])),
    escalus:end_per_suite(Config).

init_per_group(both_plain, Config) ->
    configure_s2s(#s2s_opts{}),
    Config;
init_per_group(both_tls_optional, Config) ->
    S2S = ?config(s2s_opts, Config), %The initial config assumes that both nodes are configured to use encrypted s2s
    configure_s2s(S2S),
    Config;
init_per_group(both_tls_required, Config) ->
    S2S = ?config(s2s_opts, Config),
    configure_s2s(S2S#s2s_opts{node1_s2s_use_starttls = required,
                               node2_s2s_use_starttls = required}),
    Config;
init_per_group(node1_tls_optional_node2_tls_required, Config) ->
    S2S = ?config(s2s_opts, Config),
    configure_s2s(S2S#s2s_opts{node2_s2s_use_starttls = required}),
    Config;
init_per_group(node1_tls_required_node2_tls_optional, Config) ->
    S2S = ?config(s2s_opts, Config),
    configure_s2s(S2S#s2s_opts{node1_s2s_use_starttls = required}),
    Config;
init_per_group(node1_tls_required_trusted_node2_tls_optional, Config) ->
    S2S = ?config(s2s_opts, Config),
    configure_s2s(S2S#s2s_opts{node1_s2s_use_starttls = required_trusted}),
    Config;
init_per_group(node1_tls_false_node2_tls_optional, Config) ->
    S2S = ?config(s2s_opts, Config),
    configure_s2s(S2S#s2s_opts{node1_s2s_use_starttls = false}),
    Config;
init_per_group(node1_tls_optional_node2_tls_false, Config) ->
    S2S = ?config(s2s_opts, Config),
    configure_s2s(S2S#s2s_opts{node2_s2s_use_starttls = false}),
    Config;
init_per_group(node1_tls_false_node2_tls_required, Config) ->
    S2S = ?config(s2s_opts, Config),
    configure_s2s(S2S#s2s_opts{node1_s2s_use_starttls = false,
                               node2_s2s_use_starttls = required}),
    Config;
init_per_group(node1_tls_required_node2_tls_false, Config) ->
    S2S = ?config(s2s_opts, Config),
    configure_s2s(S2S#s2s_opts{node1_s2s_use_starttls = required,
                               node2_s2s_use_starttls = false}),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

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
    escalus:story(Config, [{alice2, 1}, {alice, 1}], fun(Alice2, Alice1) ->

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

nonexistent_user(Config) ->
    escalus:story(Config, [{alice, 1}, {alice2, 1}], fun(Alice1, Alice2) ->

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
    escalus:story(Config, [{alice, 1}], fun(Alice1) ->

        %% Alice@localhost1 sends message to Xyz@localhost3
        escalus:send(Alice1, escalus_stanza:chat_to(
            <<"xyz@somebogushost">>,
            <<"Hello, unreachable!">>)),

        %% Alice@localhost1 receives stanza error: remote-server-not-found
        Stanza = escalus:wait_for_stanza(Alice1, 10000),
        escalus:assert(is_error, [<<"cancel">>, <<"remote-server-not-found">>], Stanza)

    end).

nonascii_addr(Config) ->
    escalus:story(Config, [{alice, 1}, {bob2, 1}], fun(Alice, Bob) ->

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

configure_s2s(#s2s_opts{node1_s2s_certfile = Certfile1,
                        node1_s2s_use_starttls = StartTLS1,
                        node2_s2s_certfile = Certfile2,
                        node2_s2s_use_starttls = StartTLS2}) ->
    configure_s2s(mim(), Certfile1, StartTLS1),
    configure_s2s(fed(), Certfile2, StartTLS2),
    restart_s2s().

configure_s2s(Node, Certfile, StartTLS) ->
    rpc(Node, ejabberd_config, add_local_option, [s2s_certfile, Certfile]),
    rpc(Node, ejabberd_config, add_local_option, [s2s_use_starttls, StartTLS]).

restart_s2s() ->
    restart_s2s(mim()),
    restart_s2s(fed()).

restart_s2s(Node) ->
    Children = rpc(Node, supervisor, which_children, [ejabberd_s2s_out_sup]),
    [rpc(Node, ejabberd_s2s_out, stop_connection, [Pid]) ||
     {_, Pid, _, _} <- Children],

    ChildrenIn = rpc(Node, supervisor, which_children, [ejabberd_s2s_in_sup]),
    [rpc(Node, erlang, exit, [Pid, kill]) ||
     {_, Pid, _, _} <- ChildrenIn].

mim() ->
    get_or_fail(mim_node).

fed() ->
    get_or_fail(fed_node).

get_or_fail(Key) ->
    Val = ct:get_config(Key),
    Val == undefined andalso error({undefined, Key}),
    Val.
