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

-compile([export_all, nowarn_export_all]).

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
-define(DH_FILE, "priv/ssl/fake_dh_server.pem").

-import(distributed_helper, [mim/0,
                             require_rpc_nodes/1,
                             rpc/4]).

-import(domain_helper, [domain/0]).

%%--------------------------------------------------------------------
%% Suite configuration
%%--------------------------------------------------------------------

all() ->
    [
     {group, session_replacement},
     {group, security},
     {group, incorrect_behaviors},
     {group, proxy_protocol},
     %% these groups must be last, as they really... complicate configuration
     {group, fast_tls},
     {group, just_tls}
    ].

groups() ->
    G = [ {c2s_noproc, [], [reset_stream_noproc,
                            starttls_noproc,
                            compress_noproc,
                            bad_xml,
                            invalid_host,
                            invalid_stream_namespace,
                            deny_pre_xmpp_1_0_stream]},
          {starttls, [], [should_fail_to_authenticate_without_starttls,
                          should_not_send_other_features_with_starttls_required,
                          auth_bind_pipelined_starttls_skipped_error | protocol_test_cases()]},
          {tls, [parallel], auth_bind_pipelined_cases() ++
                            protocol_test_cases() ++
                            cipher_test_cases()},
          {feature_order, [parallel], [stream_features_test,
                                       tls_authenticate,
                                       tls_compression_fail,
                                       tls_compression_authenticate_fail,
                                       tls_authenticate_compression,
                                       auth_compression_bind_session,
                                       auth_bind_compression_session,
                                       bind_server_generated_resource,
                                       cannot_connect_with_proxy_header]},
          {just_tls, tls_groups()},
          {fast_tls, tls_groups()},
          {session_replacement, [], [
                                     same_resource_replaces_session,
                                     clean_close_of_replaced_session,
                                     replaced_session_cannot_terminate
                                    ]},
          {security, [], [
                          return_proper_stream_error_if_service_is_not_hidden,
                          close_connection_if_service_type_is_hidden
                         ]},
          {incorrect_behaviors, [parallel], [close_connection_if_start_stream_duplicated,
                                             close_connection_if_protocol_violation_after_authentication,
                                             close_connection_if_protocol_violation_after_binding]},
          {proxy_protocol, [parallel], [cannot_connect_without_proxy_header,
                                        connect_with_proxy_header]}
        ],
    ct_helper:repeat_all_until_all_ok(G).

tls_groups()->
    [{group, starttls},
     {group, c2s_noproc},
     {group, feature_order},
     {group, tls}].

auth_bind_pipelined_cases() ->
    [
     auth_bind_pipelined_session,
     auth_bind_pipelined_auth_failure
    ].

protocol_test_cases() ->
    [
     should_fail_with_sslv3,
     should_fail_with_tlsv1,
     should_fail_with_tlsv1_1,
     should_pass_with_tlsv1_2
    ].

cipher_test_cases() ->
    [
     %% Server certificate is signed only with RSA for now, don't try to use ECDSA!
     clients_can_connect_with_advertised_ciphers,
     % String cipher
     'clients_can_connect_with_ECDHE-RSA-AES256-GCM-SHA384',
     %% MIM2 accepts ECDHE-RSA-AES256-GCM-SHA384 exclusively with fast_tls on alternative port
     %% MIM3 accepts #{cipher => aes_256_gcm, key_exchange => ecdhe_rsa, mac => aead, prf => sha384}
     %%      exclusively with just_tls on alternative port
     'clients_can_connect_with_ECDHE-RSA-AES256-GCM-SHA384_only'
    ].

suite() ->
    require_rpc_nodes([mim, mim2, mim3]) ++ escalus:suite().

%%--------------------------------------------------------------------
%% Init & teardown
%%--------------------------------------------------------------------

init_per_suite(Config) ->
    Config0 = escalus:init_per_suite([{escalus_user_db, {module, escalus_ejabberd, []}} | Config]),
    C2SPort = ct:get_config({hosts, mim, c2s_port}),
    [C2SListener] = mongoose_helper:get_listeners(mim(), #{port => C2SPort, module => ejabberd_c2s}),
    Config1 = [{c2s_listener, C2SListener} | Config0],
    assert_cert_file_exists(),
    escalus:create_users(Config1, escalus:get_users([?SECURE_USER, alice])).

end_per_suite(Config) ->
    escalus_fresh:clean(),
    escalus:delete_users(Config, escalus:get_users([?SECURE_USER, alice])),
    restore_c2s_listener(Config),
    escalus:end_per_suite(Config).

init_per_group(c2s_noproc, Config) ->
    configure_c2s_listener(Config, #{tls => [starttls | common_tls_opts(Config)]}),
    Config;
init_per_group(session_replacement, Config) ->
    configure_c2s_listener(Config, #{tls => [starttls | common_tls_opts(Config)]}),
    logger_ct_backend:start(),
    Config;
init_per_group(starttls, Config) ->
    configure_c2s_listener(Config, #{tls => [starttls_required | common_tls_opts(Config)]}),
    Config;
init_per_group(tls, Config) ->
    configure_c2s_listener(Config, #{tls => [tls | common_tls_opts(Config)]}),
    Users = proplists:get_value(escalus_users, Config, []),
    JoeSpec = lists:keydelete(starttls, 1, proplists:get_value(?SECURE_USER, Users)),
    JoeSpec2 = {?SECURE_USER, lists:keystore(ssl, 1, JoeSpec, {ssl, true})},
    NewUsers = lists:keystore(?SECURE_USER, 1, Users, JoeSpec2),
    Config2 = lists:keystore(escalus_users, 1, Config, {escalus_users, NewUsers}),
    [{c2s_port, ct:get_config({hosts, mim, c2s_port})} | Config2];
init_per_group(feature_order, Config) ->
    configure_c2s_listener(Config, #{zlib => 10000,
                                     tls => [starttls_required | common_tls_opts(Config)]}),
    Config;
init_per_group(just_tls, Config)->
    [{tls_module, just_tls} | Config];
init_per_group(fast_tls, Config)->
    [{tls_module, fast_tls} | Config];
init_per_group(proxy_protocol, Config) ->
    configure_c2s_listener(Config, #{proxy_protocol => true}),
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(session_replacement, Config) ->
    logger_ct_backend:stop(),
    Config;
end_per_group(_, Config) ->
    Config.

init_per_testcase(close_connection_if_service_type_is_hidden = CN, Config) ->
    Config1 = mongoose_helper:backup_and_set_config_option(Config, hide_service_name, true),
    escalus:init_per_testcase(CN, Config1);
init_per_testcase(replaced_session_cannot_terminate = CN, Config) ->
    S = escalus_users:get_server(Config, alice),
    OptKey = {replaced_wait_timeout, S},
    Config1 = mongoose_helper:backup_and_set_config_option(Config, OptKey, 1),
    escalus:init_per_testcase(CN, Config1);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    mongoose_helper:restore_config(Config),
    escalus:end_per_testcase(CaseName, Config).

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

deny_pre_xmpp_1_0_stream(Config) ->
    %% given
    Spec = escalus_fresh:freshen_spec(Config, alice),
    Steps = [
             %% when
             {?MODULE, start_stream_pre_xmpp_1_0}
            ],
    {ok, Conn, _} = escalus_connection:start(Spec, Steps),
    StreamError = escalus:wait_for_stanza(Conn),
    escalus:assert(is_stream_error, [<<"unsupported-version">>, <<>>], StreamError),
    escalus_connection:stop(Conn).

should_fail_with_sslv3(Config) ->
    should_fail_with(Config, sslv3).

should_fail_with_tlsv1(Config) ->
    should_fail_with(Config, tlsv1).

should_fail_with_tlsv1_1(Config) ->
    should_fail_with(Config, 'tlsv1.1').

should_fail_with(Config, Protocol) ->
    %% Connection process is spawned with a link so besides the crash itself,
    %%   we will receive an exit signal. We don't want to terminate the test due to this.
    %% TODO: Investigate if this behaviour is not a ticking bomb which may affect other test cases.
    process_flag(trap_exit, true),
    %% GIVEN
    UserSpec0 = escalus_users:get_userspec(Config, ?SECURE_USER),
    UserSpec1 = set_secure_connection_protocol(UserSpec0, Protocol),
    %% WHEN
    try escalus_connection:start(UserSpec1) of
    %% THEN
        _ ->
            error({client_connected, Protocol})
    catch
        _C:_R ->
            ok
    end.

should_pass_with_tlsv1_2(Config) ->
    UserSpec0 = escalus_fresh:create_fresh_user(Config, ?SECURE_USER),
    UserSpec1 = set_secure_connection_protocol(UserSpec0, 'tlsv1.2'),

    %% WHEN
    Result = escalus_connection:start(UserSpec1),

    %% THEN
    ?assertMatch({ok, _, _}, Result).

should_fail_to_authenticate_without_starttls(Config) ->
    %% GIVEN
    UserSpec = escalus_users:get_userspec(Config, ?SECURE_USER),
    {Conn, Features} = start_stream_with_compression(UserSpec),

    %% WHEN
    try escalus_session:authenticate(Conn, Features) of
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
    {ok, Conn, _} = escalus_connection:start(UserSpec, [start_stream]),
    Features = case escalus_connection:get_stanza(Conn, wait_for_features) of
        #xmlel{name = <<"stream:features">>, children = Children} -> Children;
        #xmlel{name = <<"features">>, children = Children} -> Children
    end,
    ?assertMatch([#xmlel{name = <<"starttls">>,
                         children = [#xmlel{name = <<"required">>}]}],
                 Features).

clients_can_connect_with_advertised_ciphers(Config) ->
    ?assert(length(ciphers_working_with_ssl_clients(Config)) > 0).

'clients_can_connect_with_ECDHE-RSA-AES256-GCM-SHA384'(Config) ->
    ?assert(lists:member("ECDHE-RSA-AES256-GCM-SHA384",
                         ciphers_working_with_ssl_clients(Config))).

'clients_can_connect_with_ECDHE-RSA-AES256-GCM-SHA384_only'(Config) ->
    Port = case ?config(tls_module, Config) of
               just_tls -> ct:get_config({hosts, mim3, c2s_tls_port});
               fast_tls -> ct:get_config({hosts, mim2, c2s_tls_port})
           end,
    Config1 = [{c2s_port, Port} | Config],
    CiphersStr = os:cmd("openssl ciphers 'ECDHE-RSA-AES256-GCM-SHA384'"),
    ct:pal("Available cipher suites for : ~s", [CiphersStr]),
    ct:pal("Openssl version: ~s", [os:cmd("openssl version")]),
    ?assertEqual(["ECDHE-RSA-AES256-GCM-SHA384"],
                 ciphers_working_with_ssl_clients(Config1)).

reset_stream_noproc(Config) ->
    UserSpec = escalus_users:get_userspec(Config, alice),
    Steps = [start_stream, stream_features],
    {ok, Conn, _Features} = escalus_connection:start(UserSpec, Steps),

    [C2sPid] = children_specs_to_pids(rpc(mim(), supervisor, which_children, [ejabberd_c2s_sup])),
    [RcvPid] = children_specs_to_pids(rpc(mim(), supervisor, which_children, [ejabberd_receiver_sup])),
    MonRef = erlang:monitor(process, C2sPid),
    ok = rpc(mim(), sys, suspend, [C2sPid]),
    %% Add auth element into message queue of the c2s process
    %% There is no reply because the process is suspended
    ?assertThrow({timeout, auth_reply}, escalus_session:authenticate(Conn)),
    %% Sim client disconnection
    ok = rpc(mim(), ejabberd_receiver, close, [RcvPid]),
    %% ...c2s process receives close and DOWN messages...
    %% Resume
    ok = rpc(mim(), sys, resume, [C2sPid]),
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
    Steps = [start_stream, stream_features],
    {ok, Conn, _Features} = escalus_connection:start(UserSpec, Steps),

    [C2sPid] = children_specs_to_pids(rpc(mim(), supervisor, which_children, [ejabberd_c2s_sup])),
    [RcvPid] = children_specs_to_pids(rpc(mim(), supervisor, which_children, [ejabberd_receiver_sup])),
    MonRef = erlang:monitor(process, C2sPid),
    ok = rpc(mim(), sys, suspend, [C2sPid]),
    %% Add starttls element into message queue of the c2s process
    %% There is no reply because the process is suspended
    ?assertThrow({timeout, proceed}, escalus_session:starttls(Conn)),
    %% Sim client disconnection
    ok = rpc(mim(), ejabberd_receiver, close, [RcvPid]),
    %% ...c2s process receives close and DOWN messages...
    %% Resume
    ok = rpc(mim(), sys, resume, [C2sPid]),
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
    Steps = [start_stream, stream_features],
    {ok, Conn = #client{props = Props}, _Features} = escalus_connection:start(UserSpec, Steps),

    [C2sPid] = children_specs_to_pids(rpc(mim(), supervisor, which_children, [ejabberd_c2s_sup])),
    [RcvPid] = children_specs_to_pids(rpc(mim(), supervisor, which_children, [ejabberd_receiver_sup])),
    MonRef = erlang:monitor(process, C2sPid),
    ok = rpc(mim(), sys, suspend, [C2sPid]),
    %% Add compress element into message queue of the c2s process
    %% There is no reply because the process is suspended
    ?assertThrow({timeout, compressed},
                 escalus_session:compress(Conn#client{props = [{compression, <<"zlib">>}|Props]})),
    %% Sim client disconnection
    ok = rpc(mim(), ejabberd_receiver, close, [RcvPid]),
    %% ...c2s process receives close and DOWN messages...
    %% Resume
    ok = rpc(mim(), sys, resume, [C2sPid]),
    receive
        {'DOWN', MonRef, process, C2sPid, normal} ->
            ok;
        {'DOWN', MonRef, process, C2sPid, Reason} ->
            ct:fail("ejabberd_c2s exited with reason ~p", [Reason])
        after 5000 ->
            ct:fail("c2s_monitor_timeout", [])
    end,
    ok.

%% Tests features advertisement
stream_features_test(Config) ->
    UserSpec = escalus_fresh:freshen_spec(Config, ?SECURE_USER),
    List = [start_stream, stream_features, {?MODULE, verify_features}],
    escalus_connection:start(UserSpec, List),
    ok.

verify_features(Conn, Features) ->
    %% should not advertise compression before tls
    ?assert_equal(false, has_feature(compression, Features)),
    %% start tls. Starttls should be then removed from list and compression should be added
    Conn1 = escalus_session:starttls(Conn),
    {Conn2, Features2} = escalus_session:stream_features(Conn1, []),
    ?assert_equal(false, has_feature(starttls, Features2)),
    ?assert(false =/= has_feature(compression, Features2)),
    %% start compression. Compression should be then removed from list
    {Conn3, _Features3} = escalus_session:authenticate(Conn2, Features2),
    Conn4 = escalus_session:compress(Conn3),
    {Conn5, Features5} = escalus_session:stream_features(Conn4, []),
    ?assert_equal(false, has_feature(compression, Features5)),
    ?assert_equal(false, has_feature(starttls, Features5)),
    {Conn5, Features5}.

has_feature(Feature, FeatureList) ->
    {_, Value} = lists:keyfind(Feature, 1, FeatureList),
    Value.

%% should fail
tls_compression_authenticate_fail(Config) ->
    %% Given
    UserSpec = escalus_fresh:freshen_spec(Config, ?SECURE_USER),
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
    UserSpec = escalus_fresh:create_fresh_user(Config, ?SECURE_USER),
    ConnectionSteps = [start_stream, stream_features, maybe_use_ssl, authenticate, maybe_use_compression],
    %% when
    {ok, Conn, _} = escalus_connection:start(UserSpec, ConnectionSteps),
    % then
    true = escalus_tcp:is_using_compression(Conn#client.rcv_pid),
    true = escalus_tcp:is_using_ssl(Conn#client.rcv_pid).

tls_authenticate(Config) ->
    %% Given
    UserSpec = escalus_fresh:create_fresh_user(Config, ?SECURE_USER),
    ConnetctionSteps = [start_stream, stream_features, maybe_use_ssl, authenticate],
    %% when
    {ok, Conn, _} = escalus_connection:start(UserSpec, ConnetctionSteps),
    % then
    true = escalus_tcp:is_using_ssl(Conn#client.rcv_pid).

%% should fail
tls_compression_fail(Config) ->
    %% Given
    UserSpec = escalus_fresh:freshen_spec(Config, ?SECURE_USER),
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
    UserSpec = escalus_fresh:create_fresh_user(Config, ?SECURE_USER),
    ConnetctionSteps = [start_stream, stream_features, maybe_use_ssl,
        authenticate, maybe_use_compression, bind, session],
    %% when
    {ok, Conn, _} = escalus_connection:start(UserSpec, ConnetctionSteps),
    % then
    true = escalus_tcp:is_using_compression(Conn#client.rcv_pid).

auth_bind_compression_session(Config) ->
    %% Given
    UserSpec = escalus_fresh:create_fresh_user(Config, ?SECURE_USER),
    ConnetctionSteps = [start_stream, stream_features, maybe_use_ssl,
        authenticate, bind, maybe_use_compression, session],
    %% when
    {ok, Conn, _} = escalus_connection:start(UserSpec, ConnetctionSteps),
    % then
    true = escalus_tcp:is_using_compression(Conn#client.rcv_pid).

auth_bind_pipelined_session(Config) ->
    UserSpec = [{ssl, true}, {parser_opts, [{start_tag, <<"stream:stream">>}]}
                | escalus_fresh:create_fresh_user(Config, alice)],

    Username = proplists:get_value(username, UserSpec),
    Conn = pipeline_connect(UserSpec),

    %% Stream start
    StreamResponse = escalus_connection:get_stanza(Conn, stream_response),
    ?assertMatch(#xmlstreamstart{}, StreamResponse),
    escalus_session:stream_features(Conn, []),

    %% Auth response
    escalus_auth:wait_for_success(Username, Conn),
    AuthStreamResponse = escalus_connection:get_stanza(Conn, stream_response),
    ?assertMatch(#xmlstreamstart{}, AuthStreamResponse),
    escalus_session:stream_features(Conn, []),

    %% Bind response
    BindResponse = escalus_connection:get_stanza(Conn, bind_response),
    escalus:assert(is_bind_result, BindResponse),

    %% Session response
    SessionResponse = escalus_connection:get_stanza(Conn, session_response),
    escalus:assert(is_iq_result, SessionResponse).

auth_bind_pipelined_auth_failure(Config) ->
    UserSpec = [{password, <<"badpassword">>}, {ssl, true},
                {parser_opts, [{start_tag, <<"stream:stream">>}]}
                | escalus_fresh:freshen_spec(Config, alice)],

    Conn = pipeline_connect(UserSpec),

    %% Stream start
    StreamResponse = escalus_connection:get_stanza(Conn, stream_response),
    ?assertMatch(#xmlstreamstart{}, StreamResponse),
    escalus_session:stream_features(Conn, []),

    %% Auth response
    AuthResponse = escalus_connection:get_stanza(Conn, auth_response),
    ?assertMatch(#xmlel{name = <<"failure">>, attrs = [{<<"xmlns">>, ?NS_SASL}]}, AuthResponse).

auth_bind_pipelined_starttls_skipped_error(Config) ->
    UserSpec = [{parser_opts, [{start_tag, <<"stream:stream">>}]}
                | escalus_fresh:freshen_spec(Config, ?SECURE_USER)],

    Conn = pipeline_connect(UserSpec),

    %% Stream start
    StreamResponse = escalus_connection:get_stanza(Conn, stream_response),
    ?assertMatch(#xmlstreamstart{}, StreamResponse),
    escalus_session:stream_features(Conn, []),

    %% Auth response
    AuthResponse = escalus_connection:get_stanza(Conn, auth_response),
    escalus:assert(is_stream_error, [<<"policy-violation">>, <<"Use of STARTTLS required">>],
                   AuthResponse).

bind_server_generated_resource(Config) ->
    UserSpec = [{resource, <<>>} | escalus_fresh:create_fresh_user(Config, ?SECURE_USER)],
    ConnectionSteps = [start_stream, stream_features, maybe_use_ssl, authenticate, bind],
    {ok, #client{props = NewSpec}, _} = escalus_connection:start(UserSpec, ConnectionSteps),
    {resource, Resource} = lists:keyfind(resource, 1, NewSpec),
    ?assert(is_binary(Resource)),
    ?assert(byte_size(Resource) > 0).

same_resource_replaces_session(Config) ->
    UserSpec = [{resource, <<"conflict">>} | escalus_users:get_userspec(Config, alice)],
    {ok, Alice1, _} = escalus_connection:start(UserSpec),

    {ok, Alice2, _} = escalus_connection:start(UserSpec),

    ConflictError = escalus:wait_for_stanza(Alice1),
    escalus:assert(is_stream_error, [<<"conflict">>, <<>>], ConflictError),

    mongoose_helper:wait_until(fun() -> escalus_connection:is_connected(Alice1) end, false),

    escalus_connection:stop(Alice2).

clean_close_of_replaced_session(Config) ->
    logger_ct_backend:capture(warning),

    same_resource_replaces_session(Config),

    logger_ct_backend:stop_capture(),
    FilterFun = fun(_, Msg) ->
                        re:run(Msg, "replaced_wait_timeout") /= nomatch
                end,
    [] = logger_ct_backend:recv(FilterFun).

replaced_session_cannot_terminate(Config) ->
    % GIVEN a session that is frozen and cannot terminate
    logger_ct_backend:capture(warning),
    UserSpec = [{resource, <<"conflict">>} | escalus_users:get_userspec(Config, alice)],
    {ok, _Alice1, _} = escalus_connection:start(UserSpec),
    [C2SPid] = children_specs_to_pids(rpc(mim(), supervisor, which_children, [ejabberd_c2s_sup])),
    ok = rpc(mim(), sys, suspend, [C2SPid]),

    % WHEN a session gets replaced ...
    {ok, Alice2, _} = escalus_connection:start(UserSpec),

    % THEN a timeout warning is logged
    FilterFun = fun(_, Msg) ->
                        re:run(Msg, "replaced_wait_timeout") /= nomatch
                end,
    mongoose_helper:wait_until(
      fun() -> length(logger_ct_backend:recv(FilterFun)) end, 1),

    rpc(mim(), sys, resume, [C2SPid]),
    logger_ct_backend:stop_capture(),

    escalus_connection:stop(Alice2).

return_proper_stream_error_if_service_is_not_hidden(_Config) ->
    % GIVEN MongooseIM is running default configuration
    % WHEN we send non-XMPP payload
    % THEN the server replies with stream error xml-not-well-formed and closes the connection
    SendMalformedDataStep = fun(Client, Features) ->
                                    escalus_connection:send_raw(Client, <<"malformed">>),
                                    {Client, Features}
                            end,
    {ok, Connection, _} = escalus_connection:start([], [SendMalformedDataStep]),
    escalus_connection:receive_stanza(Connection, #{ assert => is_stream_start }),
    StreamErrorAssertion = {is_stream_error, [<<"xml-not-well-formed">>, <<>>]},
    escalus_connection:receive_stanza(Connection, #{ assert => StreamErrorAssertion }),
    %% Sometimes escalus needs a moment to report the connection as closed
    escalus_connection:wait_for_close(Connection, 5000).

close_connection_if_service_type_is_hidden(_Config) ->
    % GIVEN the option to hide service name is enabled
    % WHEN we send non-XMPP payload
    % THEN connection is closed without any response from the server
    FailIfAnyDataReturned = fun(Reply) ->
                                    ct:fail({unexpected_data, Reply})
                            end,
    Connection = escalus_tcp:connect(#{ on_reply => FailIfAnyDataReturned }),
    Ref = monitor(process, Connection),
    escalus_tcp:send(Connection, <<"malformed">>),
    receive
        {'DOWN', Ref, _, _, _} -> ok
    after
        5000 ->
            ct:fail(connection_not_closed)
    end.

close_connection_if_start_stream_duplicated(Config) ->
    close_connection_if_protocol_violation(Config, [start_stream, stream_features]).

close_connection_if_protocol_violation_after_authentication(Config) ->
    close_connection_if_protocol_violation(Config, [start_stream, stream_features, authenticate]).

close_connection_if_protocol_violation_after_binding(Config) ->
    close_connection_if_protocol_violation(Config, [start_stream, stream_features, authenticate, bind]).

close_connection_if_protocol_violation(Config, Steps) ->
    AliceSpec = escalus_fresh:create_fresh_user(Config, alice),
    {ok, Alice, _Features} = escalus_connection:start(AliceSpec, Steps),
    escalus:send(Alice, escalus_stanza:stream_start(domain(), ?NS_JABBER_CLIENT)),
    escalus:assert(is_stream_error, [<<"policy-violation">>, <<>>],
                   escalus_connection:get_stanza(Alice, no_stream_error_stanza_received)),
    escalus:assert(is_stream_end,
                   escalus_connection:get_stanza(Alice, no_stream_end_stanza_received)),
    true = escalus_connection:wait_for_close(Alice,timer:seconds(5)).

cannot_connect_with_proxy_header(Config) ->
    %% GIVEN proxy protocol is disabled
    UserSpec = escalus_users:get_userspec(Config, alice),

    %% WHEN
    ConnectionSteps = [{?MODULE, send_proxy_header}, start_stream],
    ConnResult = escalus_connection:start(UserSpec, ConnectionSteps),

    %% THEN
    ?assertMatch({error, {connection_step_failed, _, _}}, ConnResult).

cannot_connect_without_proxy_header(Config) ->
    %% GIVEN proxy protocol is enabled
    UserSpec = escalus_users:get_userspec(Config, alice),

    %% WHEN
    ConnResult = escalus_connection:start(UserSpec, [start_stream]),

    %% THEN
    ?assertMatch({error, {connection_step_failed, _, _}}, ConnResult).

connect_with_proxy_header(Config) ->
    %% GIVEN proxy protocol is enabled
    UserSpec = escalus_users:get_userspec(Config, alice),

    %% WHEN
    ConnectionSteps = [{?MODULE, send_proxy_header}, start_stream, stream_features,
                       authenticate, bind, session],
    {ok, Conn, _Features} = escalus_connection:start(UserSpec, ConnectionSteps),
    % make sure the session is present
    escalus:send(Conn, escalus_stanza:presence(<<"available">>)),
    escalus:assert(is_presence, escalus:wait_for_stanza(Conn)),

    %% THEN
    SessionInfo = mongoose_helper:get_session_info(mim(), Conn),
    #{src_address := IPAddr, src_port := Port} = proxy_info(),
    ?assertMatch({IPAddr, Port}, maps:get(ip, SessionInfo)),
    escalus_connection:stop(Conn).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

c2s_port(Config) ->
    case ?config(c2s_port, Config) of
        undefined -> ct:get_config({hosts, mim, c2s_tls_port});
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
          " -cipher \"" ++ Cipher ++ "\" -tls1_2 2>&1",
    Output = os:cmd(Cmd),
    0 == string:str(Output, ":error:") andalso 0 == string:str(Output, "errno=0").

restore_c2s_listener(Config) ->
    C2SListener = ?config(c2s_listener, Config),
    mongoose_helper:restart_listener(mim(), C2SListener).

assert_cert_file_exists() ->
    ejabberd_node_utils:file_exists(?CERT_FILE) orelse
        ct:fail("cert file ~s not exists", [?CERT_FILE]).

configure_c2s_listener(Config, ExtraC2SOpts) ->
    C2SListener = ?config(c2s_listener, Config),
    NewC2SListener = maps:merge(C2SListener, ExtraC2SOpts),
    mongoose_helper:restart_listener(mim(), NewC2SListener).

common_tls_opts(Config) ->
    tls_module_opts(Config) ++ [{certfile, ?CERT_FILE}, {dhfile, ?DH_FILE}].

tls_module_opts(Config) ->
    case ?config(tls_module, Config) of
        undefined -> [];
        Module -> [{tls_module, Module}]
    end.

set_secure_connection_protocol(UserSpec, Version) ->
    [{ssl_opts, [{versions, [Version]}]} | UserSpec].

start_stream_with_compression(UserSpec) ->
    ConnectionSteps = [start_stream, stream_features, maybe_use_compression],
    {ok, Conn, Features} = escalus_connection:start(UserSpec, ConnectionSteps),
    {Conn, Features}.

connect_to_invalid_host(Spec) ->
    {ok, Conn, _} = escalus_connection:start(Spec, [{?MODULE, connect_to_invalid_host}]),
    escalus:wait_for_stanzas(Conn, 3).

connect_to_invalid_host(Conn, UnusedFeatures) ->
    escalus:send(Conn, escalus_stanza:stream_start(<<"hopefullynonexistentdomain">>,
                                                   ?NS_JABBER_CLIENT)),
    {Conn, UnusedFeatures}.

connect_with_bad_xml(Spec) ->
    {ok, Conn, _} = escalus_connection:start(Spec, [{?MODULE, connect_with_bad_xml}]),
    escalus:wait_for_stanzas(Conn, 3).

connect_with_bad_xml(Conn, UnusedFeatures) ->
    escalus_connection:send(Conn, #xmlcdata{content = "asdf\n"}),
    {Conn, UnusedFeatures}.

connect_with_invalid_stream_namespace(Spec) ->
    F = fun (Conn, UnusedFeatures) ->
                Start = stream_start_invalid_stream_ns(escalus_users:get_server([], Spec)),
                escalus:send(Conn, Start),
                {Conn, UnusedFeatures}
        end,
    {ok, Conn, _} = escalus_connection:start(Spec, [F]),
    escalus:wait_for_stanzas(Conn, 3).

start_stream_pre_xmpp_1_0(Conn = #client{props = Props}, UnusedFeatures) ->
    escalus:send(Conn, stream_start_pre_xmpp_1_0(escalus_users:get_server([], Props))),
    #xmlstreamstart{attrs = StreamAttrs} = StreamStart = escalus:wait_for_stanza(Conn),
    escalus:assert(is_stream_start, StreamStart),
    {<<"id">>, StreamID} = lists:keyfind(<<"id">>, 1, StreamAttrs),
    {Conn#client{props = [{stream_id, StreamID} | Props]}, UnusedFeatures}.

stream_start_pre_xmpp_1_0(To) ->
        stream_start(lists:keystore(version, 1, default_context(To), {version, <<>>})).

stream_start(Context) ->
    %% Be careful! The closing slash here is a hack to enable implementation of from_template/2
    %% to parse the snippet properly. In standard XMPP <stream:stream> is just opening of an XML
    %% element, NOT A SELF CLOSING element.
    T = <<"<stream:stream {{version}} xml:lang='en' xmlns='jabber:client' "
          "               to='{{to}}' "
          "               xmlns:stream='{{stream_ns}}' />">>,
    %% So we rewrap the parsed contents from #xmlel{} to #xmlstreamstart{} here.
    #xmlel{name = Name, attrs = Attrs, children = []} = escalus_stanza:from_template(T, Context),
    #xmlstreamstart{name = Name, attrs = Attrs}.

stream_start_invalid_stream_ns(To) ->
    stream_start(lists:keystore(stream_ns, 1, default_context(To),
                                {stream_ns, <<"obviously-invalid-namespace">>})).

default_context(To) ->
    [{version, <<"version='1.0'">>},
     {to, To},
     {stream_ns, ?NS_XMPP}].

children_specs_to_pids(Children) ->
    [Pid || {_, Pid, _, _} <- Children].

pipeline_connect(UserSpec) ->
    Server = proplists:get_value(server, UserSpec),
    Username = proplists:get_value(username, UserSpec),
    Password = proplists:get_value(password, UserSpec),
    AuthPayload = <<0:8, Username/binary, 0:8, Password/binary>>,

    Conn = escalus_connection:connect(UserSpec),

    Stream = escalus_stanza:stream_start(Server, <<"jabber:client">>),
    Auth = escalus_stanza:auth(<<"PLAIN">>, [#xmlcdata{content = base64:encode(AuthPayload)}]),
    AuthStream = escalus_stanza:stream_start(Server, <<"jabber:client">>),
    Bind = escalus_stanza:bind(<<?MODULE_STRING "_resource">>),
    Session = escalus_stanza:session(),

    escalus_connection:send(Conn, [Stream, Auth, AuthStream, Bind, Session]),
    Conn.

send_proxy_header(Conn, UnusedFeatures) ->
    Header = ranch_proxy_header:header(proxy_info()),
    escalus_connection:send_raw(Conn, iolist_to_binary(Header)),
    {Conn, UnusedFeatures}.

proxy_info() ->
    #{version => 2,
      command => proxy,
      transport_family => ipv4,
      transport_protocol => stream,
      src_address => {1, 2, 3, 4},
      src_port => 444,
      dest_address => {192, 168, 0, 1},
      dest_port => 443
     }.
