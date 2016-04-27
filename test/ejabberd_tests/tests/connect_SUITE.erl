%%==============================================================================
%% Copyright 2014 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================
-module(connect_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("exml/include/exml.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-define(assert_equal(E, V), (
    [ct:fail("ASSERT EQUAL~n\tExpected ~p~n\tValue ~p~n", [(E), (V)])
     || (E) =/= (V)])).
-define(SECURE_USER, secure_joe).
-define(CERT_FILE, "priv/ssl/fake_server.pem").
-define(TLS_VERSIONS, ["tlsv1", "tlsv1.1", "tlsv1.2"]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [{group, c2s_noproc}, %% should be first, uses vanilla config
     {group, starttls},
     {group, feature_order},
     {group, tls}
    ].

groups() ->
    [{c2s_noproc, [], [reset_stream_noproc,
                       starttls_noproc,
                       compress_noproc,
                       bad_xml,
                       invalid_host,
                       invalid_stream_namespace,
                       pre_xmpp_1_0_stream]},
     {starttls, test_cases()},
     {tls, [parallel], generate_tls_vsn_tests() ++
                       cipher_test_cases()},
     {feature_order, [parallel], [stream_features_test,
                                  tls_authenticate,
                                  tls_compression_fail,
                                  tls_compression_authenticate_fail,
                                  tls_authenticate_compression,
                                  auth_compression_bind_session,
                                  auth_bind_compression_session]}
    ].

test_cases() ->
    generate_tls_vsn_tests() ++
    [should_fail_with_sslv3,
     should_fail_to_authenticate_without_starttls,
     should_not_send_other_features_with_starttls_required].

cipher_test_cases() ->
    [clients_can_connect_with_advertised_ciphers,
     'clients_can_connect_with_DHE-RSA-AES256-SHA',
     'clients_can_connect_with_DHE-RSA-AES128-SHA',
     %% node2 accepts DHE-RSA-AES256-SHA exclusively (see ejabberd.cfg)
     'clients_can_connect_with_DHE-RSA-AES256-SHA_only'].


suite() ->
    escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config0 = escalus:init_per_suite([{escalus_user_db, {module, escalus_ejabberd, []}} | Config]),
    Config1 = ejabberd_node_utils:init(Config0),
    ejabberd_node_utils:backup_config_file(Config1),
    assert_cert_file_exists(),
    escalus:create_users(Config1, escalus:get_users([?SECURE_USER, alice])).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:delete_users(Config, escalus:get_users([?SECURE_USER, alice])),
    restore_ejabberd_node(Config),
    escalus:end_per_suite(Config).

init_per_group(c2s_noproc, Config) ->
    Config;
init_per_group(starttls, Config) ->
    config_ejabberd_node_tls(Config,
                             fun mk_value_for_starttls_required_config_pattern/0),
    ejabberd_node_utils:restart_application(ejabberd),
    Config;
init_per_group(tls, Config) ->
    config_ejabberd_node_tls(Config, fun mk_value_for_tls_config_pattern/0),
    ejabberd_node_utils:restart_application(ejabberd),
    Users = proplists:get_value(escalus_users, Config, []),
    JoeSpec = lists:keydelete(starttls, 1, proplists:get_value(?SECURE_USER, Users)),
    JoeSpec2 = {?SECURE_USER, lists:keystore(ssl, 1, JoeSpec, {ssl, true})},
    NewUsers = lists:keystore(?SECURE_USER, 1, Users, JoeSpec2),
    Config2 = lists:keystore(escalus_users, 1, Config, {escalus_users, NewUsers}),
    [{c2s_port, 5222} | Config2];
init_per_group(feature_order, Config) ->
    config_ejabberd_node_tls(Config, fun mk_value_for_compression_config_pattern/0),
    ejabberd_node_utils:restart_application(ejabberd),
    Config;
init_per_group(_, Config) ->
    Config.
end_per_group(feature_order, Config) ->
    Config;
end_per_group(_, Config) ->
    Config.

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

generate_tls_vsn_tests() ->
    [list_to_existing_atom("should_pass_with_" ++ VSN)
     || VSN <- ?TLS_VERSIONS].

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

bad_xml(Config) ->
    %% given
    Spec = escalus_users:get_userspec(Config, alice),
    %% when
    [Start, Error, End] = connect_with_bad_xml(Spec),
    %% then
    %% See RFC 6120 4.9.1.3 (http://xmpp.org/rfcs/rfc6120.html#streams-error-rules-host).
    %% Stream start from the server is required in this case.
    escalus:assert(is_stream_start, Start),
    escalus:assert(is_stream_error, [<<"xml-not-well-formed">>, <<>>], Error),
    escalus:assert(is_stream_end, End).

invalid_host(Config) ->
    %% given
    Spec = escalus_users:get_userspec(Config, alice),
    %% when
    [Start, Error, End] = connect_to_invalid_host(Spec),
    %% then
    %% See RFC 6120 4.9.1.3 (http://xmpp.org/rfcs/rfc6120.html#streams-error-rules-host).
    %% Stream start from the server is required in this case.
    escalus:assert(is_stream_start, Start),
    escalus:assert(is_stream_error, [<<"host-unknown">>, <<>>], Error),
    escalus:assert(is_stream_end, End).

invalid_stream_namespace(Config) ->
    %% given
    Spec = escalus_users:get_userspec(Config, alice),
    %% when
    [Start, Error, End] = connect_with_invalid_stream_namespace(Spec),
    %% then
    escalus:assert(is_stream_start, Start),
    escalus:assert(is_stream_error, [<<"invalid-namespace">>, <<>>], Error),
    escalus:assert(is_stream_end, End).

pre_xmpp_1_0_stream(Config) ->
    %% given
    Spec = given_fresh_spec(Config, alice),
    Steps = [
             %% when
             {legacy_stream_helper, start_stream_pre_xmpp_1_0},
             {legacy_stream_helper, failed_legacy_auth}
            ],
    %% ok, now do the plan from above
    {ok, Conn, _, _} = escalus_connection:start(Spec, Steps),
    escalus_connection:stop(Conn).

should_fail_with_sslv3(Config) ->
    %% GIVEN
    UserSpec0 = escalus_users:get_userspec(Config, ?SECURE_USER),
    UserSpec1 = set_secure_connection_protocol(UserSpec0, sslv3),
    %% WHEN
    try escalus_connection:start(UserSpec1) of
    %% THEN
        _ ->
            error(client_connected_using_sslv3)
    catch
        error:closed ->
            ok
    end.

should_pass_with_tlsv1(Config) ->
    should_pass_with_tls(tlsv1, Config).

'should_pass_with_tlsv1.1'(Config) ->
    should_pass_with_tls('tlsv1.1', Config).

'should_pass_with_tlsv1.2'(Config) ->
    should_pass_with_tls('tlsv1.2', Config).

should_pass_with_tls(Version, Config)->
    UserSpec0 = given_fresh_spec(Config, ?SECURE_USER),
    UserSpec1 = set_secure_connection_protocol(UserSpec0, Version),

    %% WHEN
    Result = escalus_connection:start(UserSpec1),

    %% THEN
    ?assertMatch({ok, _, _, _}, Result).

should_fail_to_authenticate_without_starttls(Config) ->
    %% GIVEN
    UserSpec = escalus_users:get_userspec(Config, ?SECURE_USER),
    {Conn, Props, Features} = start_stream_with_compression(UserSpec),

    %% WHEN
    try escalus_session:authenticate(Conn, Props, Features) of
    %% THEN
        _ ->
            error(authentication_without_tls_suceeded)
    catch
        throw:{auth_failed, User, AuthReply} ->
            ?assertEqual(atom_to_binary(?SECURE_USER, utf8), User),
            escalus:assert(is_stream_error, [<<"policy-violation">>,
                                             <<"Use of STARTTLS required">>],
                           AuthReply)
    end.

should_not_send_other_features_with_starttls_required(Config) ->
    UserSpec = escalus_users:get_userspec(Config, ?SECURE_USER),
    {ok, Conn, _, _} = escalus_connection:start(UserSpec, [start_stream]),
    Features = case escalus_connection:get_stanza(Conn, wait_for_features) of
        #xmlel{name = <<"stream:features">>, children = Children} -> Children;
        #xmlel{name = <<"features">>, children = Children} -> Children
    end,
    ?assertMatch([#xmlel{name = <<"starttls">>,
                         children = [#xmlel{name = <<"required">>}]}],
                 Features).

clients_can_connect_with_advertised_ciphers(Config) ->
    ?assert(length(ciphers_working_with_ssl_clients(Config)) > 0).

'clients_can_connect_with_DHE-RSA-AES256-SHA'(Config) ->
    ?assert(lists:member("DHE-RSA-AES256-SHA",
                         ciphers_working_with_ssl_clients(Config))).

'clients_can_connect_with_DHE-RSA-AES256-SHA_only'(Config) ->
    Config1 = [{c2s_port, 5233} | Config],
    CiphersStr = os:cmd("openssl ciphers 'DHE-RSA-AES256-SHA'"),
    ct:pal("Available cipher suites for : ~s", [CiphersStr]),
    ct:pal("Openssl version: ~s", [os:cmd("openssl version")]),
    ?assertEqual(["DHE-RSA-AES256-SHA"],
                 ciphers_working_with_ssl_clients(Config1)).

'clients_can_connect_with_DHE-RSA-AES128-SHA'(Config) ->
    ?assert(lists:member("DHE-RSA-AES128-SHA",
                         ciphers_working_with_ssl_clients(Config))).


reset_stream_noproc(Config) ->
    UserSpec = escalus_users:get_userspec(Config, alice),
    PreAuthF = fun(Conn, Props, Features) ->
                       {Conn, Props, Features}
               end,
    Steps = [start_stream, stream_features],
    {ok, Conn, Props, Features} = escalus_connection:start(UserSpec, Steps),

    [C2sPid] = children_specs_to_pids(escalus_ejabberd:rpc(supervisor, which_children, [ejabberd_c2s_sup])),
    [RcvPid] = children_specs_to_pids(escalus_ejabberd:rpc(supervisor, which_children, [ejabberd_receiver_sup])),
    MonRef = erlang:monitor(process, C2sPid),
    ok = escalus_ejabberd:rpc(sys, suspend, [C2sPid]),
    %% Add auth element into message queue of the c2s process
    %% There is no reply because the process is suspended
    ?assertThrow({timeout, auth_reply}, escalus_session:authenticate(Conn, Props)),
    %% Sim client disconnection
    ok = escalus_ejabberd:rpc(ejabberd_receiver, close, [RcvPid]),
    %% ...c2s process receives close and DOWN messages...
    %% Resume
    ok = escalus_ejabberd:rpc(sys, resume, [C2sPid]),
    receive
        {'DOWN', MonRef, process, C2sPid, normal} ->
            ok;
        {'DOWN', MonRef, process, C2sPid, Reason} ->
            ct:fail("ejabberd_c2s exited with reason ~p", [Reason])
        after 5000 ->
            ct:fail("c2s_monitor_timeout", [])
    end,
    ok.

starttls_noproc(Config) ->
    UserSpec = escalus_users:get_userspec(Config, alice),
    PreAuthF = fun(Conn, Props, Features) ->
                       {Conn, Props, Features}
               end,
    Steps = [start_stream, stream_features],
    {ok, Conn, Props, Features} = escalus_connection:start(UserSpec, Steps),

    [C2sPid] = children_specs_to_pids(escalus_ejabberd:rpc(supervisor, which_children, [ejabberd_c2s_sup])),
    [RcvPid] = children_specs_to_pids(escalus_ejabberd:rpc(supervisor, which_children, [ejabberd_receiver_sup])),
    MonRef = erlang:monitor(process, C2sPid),
    ok = escalus_ejabberd:rpc(sys, suspend, [C2sPid]),
    %% Add starttls element into message queue of the c2s process
    %% There is no reply because the process is suspended
    ?assertThrow({timeout, proceed}, escalus_session:starttls(Conn, Props)),
    %% Sim client disconnection
    ok = escalus_ejabberd:rpc(ejabberd_receiver, close, [RcvPid]),
    %% ...c2s process receives close and DOWN messages...
    %% Resume
    ok = escalus_ejabberd:rpc(sys, resume, [C2sPid]),
    receive
        {'DOWN', MonRef, process, C2sPid, normal} ->
            ok;
        {'DOWN', MonRef, process, C2sPid, Reason} ->
            ct:fail("ejabberd_c2s exited with reason ~p", [Reason])
        after 5000 ->
            ct:fail("c2s_monitor_timeout", [])
    end,
    ok.

compress_noproc(Config) ->
    UserSpec = escalus_users:get_userspec(Config, alice),
    PreAuthF = fun(Conn, Props, Features) ->
                       {Conn, Props, Features}
               end,
    Steps = [start_stream, stream_features],
    {ok, Conn, Props, Features} = escalus_connection:start(UserSpec, Steps),

    [C2sPid] = children_specs_to_pids(escalus_ejabberd:rpc(supervisor, which_children, [ejabberd_c2s_sup])),
    [RcvPid] = children_specs_to_pids(escalus_ejabberd:rpc(supervisor, which_children, [ejabberd_receiver_sup])),
    MonRef = erlang:monitor(process, C2sPid),
    ok = escalus_ejabberd:rpc(sys, suspend, [C2sPid]),
    %% Add compress element into message queue of the c2s process
    %% There is no reply because the process is suspended
    ?assertThrow({timeout, compressed},
                 escalus_session:compress(Conn, [{compression, <<"zlib">>}|Props])),
    %% Sim client disconnection
    ok = escalus_ejabberd:rpc(ejabberd_receiver, close, [RcvPid]),
    %% ...c2s process receives close and DOWN messages...
    %% Resume
    ok = escalus_ejabberd:rpc(sys, resume, [C2sPid]),
    receive
        {'DOWN', MonRef, process, C2sPid, normal} ->
            ok;
        {'DOWN', MonRef, process, C2sPid, Reason} ->
            ct:fail("ejabberd_c2s exited with reason ~p", [Reason])
        after 5000 ->
            ct:fail("c2s_monitor_timeout", [])
    end,
    ok.

%% Tests featuress advertisement
stream_features_test(Config) ->
    UserSpec = given_fresh_spec(Config, ?SECURE_USER),
    List = [start_stream, stream_features, {?MODULE, verify_features}],
    escalus_connection:start(UserSpec, List),
    ok.

given_fresh_spec(Config, User) ->
    Config1 = escalus_fresh:create_users(Config, [{User, 1}]),
    escalus_users:get_userspec(Config1, User).

verify_features(Conn, Props, Features) ->
    %% should not advertise compression before tls
    ?assert_equal(false, has_feature(compression, Features)),
    %% start tls. Starttls should be then removed from list and compression should be added
    {Conn1, Props1} = escalus_session:starttls(Conn, Props),
    {Conn2, Props2, Features2} = escalus_session:stream_features(Conn1, Props1, []),
    ?assert_equal(false, has_feature(starttls, Features2)),
    ?assert(false =/= has_feature(compression, Features2)),
    %% start compression. Compression should be then removed from list
    {Conn3, Props3, Features3} = escalus_session:authenticate(Conn2, Props2, Features2),
    {Conn4, Props4} = escalus_session:compress(Conn3, Props3),
    {Conn5, Props5, Features4} = escalus_session:stream_features(Conn4, Props4, []),
    ?assert_equal(false, has_feature(compression, Features4)),
    ?assert_equal(false, has_feature(starttls, Features4)),
    {Conn5, Props5, Features4}.

has_feature(Feature, FeatureList) ->
    {_, Value} = lists:keyfind(Feature, 1, FeatureList),
    Value.

%% should fail
tls_compression_authenticate_fail(Config) ->
    %% Given
    UserSpec = given_fresh_spec(Config, ?SECURE_USER),
    ConnetctionSteps = [start_stream, stream_features, maybe_use_ssl, maybe_use_compression, authenticate],
    %% when and then
    try escalus_connection:start(UserSpec, ConnetctionSteps) of
        _ ->
            error(compression_without_auth_suceeded)
    catch
        error:{assertion_failed, assert, is_compressed, Stanza, _} ->
            case Stanza of
                #xmlel{name = <<"failure">>} ->
                    ok;
                _ ->
                    error(unknown_compression_response)
            end
    end.

tls_authenticate_compression(Config) ->
    %% Given
    UserSpec = given_fresh_spec(Config, ?SECURE_USER),
    ConnetctionSteps = [start_stream, stream_features, maybe_use_ssl, authenticate, maybe_use_compression],
    %% then
    {ok, Conn, _, _} = escalus_connection:start(UserSpec, ConnetctionSteps),
    % when and then
    Compress = Conn#client.compress,
    SSL = Conn#client.ssl,
    ?assert(false =/= Compress),
    ?assert(false =/= SSL).

tls_authenticate(Config) ->
    %% Given
    UserSpec = given_fresh_spec(Config, ?SECURE_USER),
    ConnetctionSteps = [start_stream, stream_features, maybe_use_ssl, authenticate],
    %% then
    {ok, Conn, _, _} = escalus_connection:start(UserSpec, ConnetctionSteps),
    % when,
    SSL = Conn#client.ssl,
    ?assert(false =/= SSL).

%% should fail
tls_compression_fail(Config) ->
    %% Given
    UserSpec = given_fresh_spec(Config, ?SECURE_USER),
    ConnetctionSteps = [start_stream, stream_features, maybe_use_ssl, maybe_use_compression],
    %% then and when
    try escalus_connection:start(UserSpec, ConnetctionSteps) of
        _ ->
            error(compression_without_auth_suceeded)
    catch
        error:{assertion_failed, assert, is_compressed, Stanza, _} ->
            case Stanza of
                #xmlel{name = <<"failure">>} ->
                    ok;
                _ ->
                    error(unknown_compression_response)
            end
    end.

auth_compression_bind_session(Config) ->
    %% Given
    UserSpec = given_fresh_spec(Config, ?SECURE_USER),
    ConnetctionSteps = [start_stream, stream_features, maybe_use_ssl,
        authenticate, maybe_use_compression, bind, session],
    %% then
    {ok, Conn, _, _} = escalus_connection:start(UserSpec, ConnetctionSteps),
    % when
    Compress = Conn#client.compress,
    ?assert(false =/= Compress).

auth_bind_compression_session(Config) ->
    %% Given
    UserSpec = given_fresh_spec(Config, ?SECURE_USER),
    ConnetctionSteps = [start_stream, stream_features, maybe_use_ssl,
        authenticate, bind, maybe_use_compression, session],
    %% then
    {ok, Conn, _, _} = escalus_connection:start(UserSpec, ConnetctionSteps),
    % when
    Compress = Conn#client.compress,
    ?assert(false =/= Compress).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

c2s_port(Config) ->
    case ?config(c2s_port, Config) of
        undefined -> 5223;
        Value -> Value
    end.

ciphers_available_in_os() ->
    CiphersStr = os:cmd("openssl ciphers 'ALL:eNULL'"),
    [string:strip(C, both, $\n) || C <- string:tokens(CiphersStr, ":")].

ciphers_working_with_ssl_clients(Config) ->
    Port = c2s_port(Config),
    lists:filter(fun(Cipher) ->
                         openssl_client_can_use_cipher(Cipher, Port)
                 end, ciphers_available_in_os()).

openssl_client_can_use_cipher(Cipher, Port) ->
    PortStr = integer_to_list(Port),
    Cmd = "echo '' | openssl s_client -connect localhost:" ++ PortStr ++
          " -cipher " "\"" ++ Cipher ++ "\" 2>&1",
    {done, ReturnCode, _Result} = erlsh:oneliner(Cmd),
    0 == ReturnCode.

restore_ejabberd_node(Config) ->
    ejabberd_node_utils:restore_config_file(Config),
    ejabberd_node_utils:restart_application(ejabberd).

assert_cert_file_exists() ->
    ejabberd_node_utils:file_exists(?CERT_FILE) orelse
        ct:fail("cert file ~s not exists", [?CERT_FILE]).

config_ejabberd_node_tls(Config, Fun) ->
    ejabberd_node_utils:modify_config_file([Fun()], Config).

mk_value_for_tls_config_pattern() ->
    {tls_config, "{certfile, \"" ++ ?CERT_FILE ++ "\"}, tls,"}.

mk_value_for_compression_config_pattern() ->
    {tls_config, "{certfile, \"" ++ ?CERT_FILE ++ "\"}, starttls_required,  {zlib, 10000},"}.

mk_value_for_starttls_required_config_pattern() ->
    {tls_config, "{certfile, \"" ++ ?CERT_FILE ++ "\"}, starttls_required,"}.

set_secure_connection_protocol(UserSpec, Version) ->
    [{ssl_opts, [{versions, [Version]}]} | UserSpec].

start_stream_with_compression(UserSpec) ->
    ConnetctionSteps = [start_stream, stream_features, maybe_use_compression],
    {ok, Conn, Props, Features} = escalus_connection:start(UserSpec,
                                                            ConnetctionSteps),
    {Conn, Props, Features}.

connect_to_invalid_host(Spec) ->
    {ok, Conn, _, _} = escalus_connection:start(Spec, [{?MODULE, connect_to_invalid_host}]),
    escalus:wait_for_stanzas(Conn, 3).

connect_to_invalid_host(Conn, UnusedProps, UnusedFeatures) ->
    escalus:send(Conn, escalus_stanza:stream_start(<<"hopefullynonexistentdomain">>,
                                                   ?NS_JABBER_CLIENT)),
    {Conn, UnusedProps, UnusedFeatures}.

connect_with_bad_xml(Spec) ->
    {ok, Conn, _, _} = escalus_connection:start(Spec, [{?MODULE, connect_with_bad_xml}]),
    escalus:wait_for_stanzas(Conn, 3).

connect_with_bad_xml(Conn, UnusedProps, UnusedFeatures) ->
    escalus_connection:send(Conn, #xmlcdata{content = "asdf\n"}),
    {Conn, UnusedProps, UnusedFeatures}.

connect_with_invalid_stream_namespace(Spec) ->
    F = fun (Conn, UnusedProps, UnusedFeatures) ->
                Start = stream_start_invalid_stream_ns(escalus_users:get_server([], Spec)),
                escalus:send(Conn, Start),
                {Conn, UnusedProps, UnusedFeatures}
        end,
    {ok, Conn, _, _} = escalus_connection:start(Spec, [F]),
    escalus:wait_for_stanzas(Conn, 3).

stream_start_invalid_stream_ns(To) ->
    legacy_stream_helper:stream_start(lists:keystore(stream_ns, 1, default_context(To),
                                {stream_ns, <<"obviously-invalid-namespace">>})).

default_context(To) ->
    [{version, <<"version='1.0'">>},
     {to, To},
     {stream_ns, ?NS_XMPP}].

children_specs_to_pids(Children) ->
    [Pid || {_, Pid, _, _} <- Children].
