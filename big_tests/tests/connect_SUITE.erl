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
-define(CACERT_FILE, "priv/ssl/cacert.pem").
-define(CERT_FILE, "priv/ssl/fake_cert.pem").
-define(KEY_FILE, "priv/ssl/fake_key.pem").
-define(DH_FILE, "priv/ssl/fake_dh_server.pem").

-import(distributed_helper, [mim/0,
                             mim2/0,
                             mim3/0,
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
        {group, disconnect},
        %% these groups must be last, as they really... complicate configuration
        {group, just_tls}
    ].

groups() ->
    [
        {starttls_disabled, [parallel], [correct_features_are_advertised_for_disabled_starttls,
                                         starttls_should_fail_when_disabled]},
        {starttls_optional, [parallel], [bad_xml,
                                         invalid_host,
                                         invalid_stream_namespace,
                                         invalid_stream_version,
                                         deny_pre_xmpp_1_0_stream,
                                         correct_features_are_advertised_for_optional_starttls]},
        {starttls_required, [], [{group, starttls_required_parallel}, metrics_test]},
        {starttls_required_parallel, [parallel], [correct_features_are_advertised_for_required_starttls,
                                                  tls_authenticate,
                                                  bind_server_generated_resource,
                                                  cannot_connect_with_proxy_header,
                                                  should_fail_to_authenticate_without_starttls,
                                                  auth_bind_pipelined_starttls_skipped_error
                                                 | protocol_test_cases()]},
        {tls, [parallel], auth_bind_pipelined_cases() ++
                          protocol_test_cases() ++
                          cipher_test_cases() ++ [should_fail_without_tls]},
        {verify_peer, [], [use_system_certs_when_no_cacertfile,
                           verify_peer_disconnects_when_client_has_no_cert,
                           verify_peer_ignores_when_client_has_no_cert]},
        {just_tls, [{group, verify_peer} | tls_groups()]},
        {session_replacement, [], [same_resource_replaces_session,
                                   same_resource_replaces_session_with_colon,
                                   clean_close_of_replaced_session,
                                   replaced_session_cannot_terminate,
                                   replaced_session_cannot_terminate_different_nodes]},
        {security, [], [return_proper_stream_error_if_service_is_not_hidden,
                        close_connection_if_service_type_is_hidden]},
        {incorrect_behaviors, [parallel], [close_connection_if_start_stream_duplicated,
                                           close_connection_if_protocol_violation_after_authentication,
                                           close_connection_if_protocol_violation_after_binding]},
        {disconnect, [], [disconnect_inactive_tcp_connection_after_timeout]},
        {proxy_protocol, [parallel], [cannot_connect_without_proxy_header,
                                      connect_with_proxy_header]}
    ].

tls_groups()->
    [
        {group, starttls_disabled},
        {group, starttls_optional},
        {group, starttls_required},
        {group, tls}
    ].

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
        %% MIM2 accepts ECDHE-RSA-AES256-GCM-SHA384 exclusively on alternative port
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
    instrument_helper:start(instrumentation_events()),
    logger_ct_backend:start(),
    Config0 = escalus:init_per_suite([{escalus_user_db, {module, escalus_ejabberd, []}} | Config]),
    C2SPort = ct:get_config({hosts, mim, c2s_port}),
    [C2SListener] = mongoose_helper:get_listeners(mim(), #{port => C2SPort, module => mongoose_c2s_listener}),
    Config1 = [{c2s_listener, C2SListener} | Config0],
    assert_cert_file_exists(),
    escalus:create_users(Config1, escalus:get_users([?SECURE_USER, alice])).

end_per_suite(Config) ->
    instrument_helper:stop(),
    logger_ct_backend:stop(),
    escalus_fresh:clean(),
    escalus:delete_users(Config, escalus:get_users([?SECURE_USER, alice])),
    restore_c2s_listener(Config),
    escalus:end_per_suite(Config).

init_per_group(starttls_optional, Config) ->
    configure_c2s_listener(Config, #{tls => tls_opts(starttls, Config)}),
    Config;
init_per_group(session_replacement, Config) ->
    configure_c2s_listener(Config, #{tls => tls_opts(starttls, Config)}),
    Config;
init_per_group(starttls_required, Config) ->
    configure_c2s_listener(Config, #{tls => tls_opts(starttls_required, Config)}),
    Config;
init_per_group(starttls_disabled, Config) ->
    configure_c2s_listener(Config, #{}, [tls]),
    Config;
init_per_group(tls, Config) ->
    configure_c2s_listener(Config, #{tls => tls_opts(tls, Config)}),
    Users = proplists:get_value(escalus_users, Config, []),
    JoeSpec = lists:keydelete(starttls, 1, proplists:get_value(?SECURE_USER, Users)),
    JoeSpec2 = {?SECURE_USER, lists:keystore(ssl, 1, JoeSpec, {ssl, true})},
    NewUsers = lists:keystore(?SECURE_USER, 1, Users, JoeSpec2),
    Config2 = lists:keystore(escalus_users, 1, Config, {escalus_users, NewUsers}),
    [{c2s_port, ct:get_config({hosts, mim, c2s_port})} | Config2];
init_per_group(just_tls, Config)->
    [{tls_module, just_tls} | Config];
init_per_group(proxy_protocol, Config) ->
    configure_c2s_listener(Config, #{proxy_protocol => true}),
    Config;
init_per_group(disconnect, Config) ->
    configure_c2s_listener(Config, #{state_timeout => 100}),
    Config;
init_per_group(verify_peer, Config) ->
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(close_connection_if_service_type_is_hidden = CN, Config) ->
    Config1 = mongoose_helper:backup_and_set_config_option(Config, hide_service_name, true),
    escalus:init_per_testcase(CN, Config1);
init_per_testcase(replaced_session_cannot_terminate = CN, Config) ->
    S = escalus_users:get_server(Config, alice),
    OptKey = {replaced_wait_timeout, S},
    Config1 = mongoose_helper:backup_and_set_config_option(Config, OptKey, 1),
    escalus:init_per_testcase(CN, Config1);
init_per_testcase(replaced_session_cannot_terminate_different_nodes = CN, Config) ->
    S = escalus_users:get_server(Config, alice),
    OptKey = {replaced_wait_timeout, S},
    Config1 = mongoose_helper:backup_and_set_config_option(Config, OptKey, 1),
    Config2 = distributed_helper:add_node_to_cluster(mim2(), Config1),
    logger_ct_backend:start(mim2()),
    escalus:init_per_testcase(CN, Config2);
init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(replaced_session_cannot_terminate_different_nodes = CaseName, Config) ->
    logger_ct_backend:stop(mim2()),
    distributed_helper:remove_node_from_cluster(mim2(), Config),
    mongoose_helper:restore_config(Config),
    escalus:end_per_testcase(CaseName, Config);
end_per_testcase(verify_peer_disconnects_when_client_has_no_cert, Config) ->
    mongoose_helper:restore_config(Config),
    catch escalus_event:stop(Config),
    catch escalus_cleaner:stop(Config);
end_per_testcase(CaseName, Config) ->
    mongoose_helper:restore_config(Config),
    escalus:end_per_testcase(CaseName, Config).

%%--------------------------------------------------------------------
%% Tests
%%--------------------------------------------------------------------

use_system_certs_when_no_cacertfile(Config) ->
    Opts0 = tls_opts(starttls_required, Config),
    Opts = maps:remove(cacertfile, Opts0#{verify_mode => peer}),
    ?assertNot(maps:is_key(cacerts, Opts)),
    ?assertNot(maps:is_key(cacertfile, Opts)),
    ServerOpts = rpc(mim(), just_tls, make_server_opts, [Opts]),
    ?assertMatch({verify, verify_peer}, lists:keyfind(verify, 1, ServerOpts)),
    ?assertMatch({cacerts, _}, lists:keyfind(cacerts, 1, ServerOpts)),
    %% Remove server options
    Opts1 = maps:without([certfile, keyfile, dhfile], Opts),
    ClientOpts = rpc(mim(), just_tls, make_client_opts, [Opts1]),
    ?assertMatch({verify, verify_peer}, lists:keyfind(verify, 1, ClientOpts)),
    ?assertMatch({cacerts, _}, lists:keyfind(cacerts, 1, ClientOpts)),
    ok = ssl:start(),
    ?assertMatch({ok, _}, ssl:connect("google.com", 443, ClientOpts)).

verify_peer_disconnects_when_client_has_no_cert(Config) ->
    logger_ct_backend:capture(error),
    %% Server disconnects only when `disconnect_on_failure` is set to `true`.
    %% It is true by default, so we make sure `disconnect_on_failure` is not in config.
    %% `verify_mode` needs to be set to `peer`.
    ServerTLSOpts0 = tls_opts(starttls_required, Config),
    ServerTLSOpts = maps:remove(disconnect_on_failure, ServerTLSOpts0#{verify_mode => peer}),
    configure_c2s_listener(Config, #{tls => ServerTLSOpts}),
    process_flag(trap_exit, true),
    UserSpec0 = escalus_users:get_userspec(Config, ?SECURE_USER),
    UserSpec = [{ssl_opts, [{verify, verify_none}]}|UserSpec0],
    try
        escalus_connection:start(UserSpec) of
        {error, {connection_step_failed, {{escalus_session, maybe_use_ssl}, _, _}, _}} ->
            ok;
        _Result ->
            error({client_connected, Config})
    catch
        C:E ->
            error({C, E, Config})
    after
        logger_ct_backend:stop_capture()
    end,
    ?assertEqual([], logger_ct_backend:recv(fun(_, _Msg) -> true end)).

verify_peer_ignores_when_client_has_no_cert(Config) ->
    %% Server bypasses TLS client cert verification when `disconnect_on_failure` is set to `false`.
    ServerTLSOpts0 = tls_opts(starttls_required, Config),
    ServerTLSOpts = ServerTLSOpts0#{disconnect_on_failure => false},
    configure_c2s_listener(Config, #{tls => ServerTLSOpts}),
    process_flag(trap_exit, true),
    UserSpec0 = escalus_users:get_userspec(Config, ?SECURE_USER),
    UserSpec = [{ssl_opts, [{verify, verify_none}]}|UserSpec0],
    try
        escalus_connection:start(UserSpec) of
            {ok, _, _} ->
                ok;
            Other ->
                error({client_disconnected, Config, Other})
    catch
        C:E ->
            error({C, E, Config})
    end.

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
    Spec0 = escalus_users:get_userspec(Config, alice),
    Spec = [{stream_attrs, #{<<"xmlns:stream">> => <<"obviously-invalid-namespace">>}} | Spec0],
    %% when
    {ok, Conn, _} = escalus_connection:start(Spec, [{?MODULE, start_stream}]),
    [Start, Error, End] = escalus:wait_for_stanzas(Conn, 3),
    %% then
    escalus:assert(is_stream_start, Start),
    escalus:assert(is_stream_error, [<<"invalid-namespace">>, <<>>], Error),
    escalus:assert(is_stream_end, End).

invalid_stream_version(Config) ->
    %% given
    Spec0 = escalus_users:get_userspec(Config, alice),
    Spec = [{stream_attrs, #{<<"version">> => <<"1.23456">>}} | Spec0],
    %% when
    {ok, Conn, _} = escalus_connection:start(Spec, [{?MODULE, start_stream}]),
    [Start, Error, End] = escalus:wait_for_stanzas(Conn, 3),
    %% then
    escalus:assert(is_stream_start, Start),
    escalus:assert(is_stream_error, [<<"unsupported-version">>, <<>>], Error),
    escalus:assert(is_stream_end, End).

deny_pre_xmpp_1_0_stream(Config) ->
    %% given
    Spec0 = escalus_users:get_userspec(Config, alice),
    Spec = [{stream_attrs, #{<<"version">> => undefined}} | Spec0],
    %% when
    {ok, Conn, _} = escalus_connection:start(Spec, [{?MODULE, start_stream}]),
    [Start, Error, End] = escalus:wait_for_stanzas(Conn, 3),
    %% then
    escalus:assert(is_stream_start, Start),
    escalus:assert(is_stream_error, [<<"unsupported-version">>, <<>>], Error),
    escalus:assert(is_stream_end, End).

should_fail_with_sslv3(Config) ->
    should_fail_with(Config, sslv3).

should_fail_with_tlsv1(Config) ->
    should_fail_with(Config, tlsv1).

should_fail_with_tlsv1_1(Config) ->
    should_fail_with(Config, 'tlsv1.1').

should_fail_with(Config, Protocol) ->
    logger_ct_backend:capture(error),
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
    after
        logger_ct_backend:stop_capture()
    end,
    ?assertEqual([], logger_ct_backend:recv(fun(_, _Msg) -> true end)).

should_fail_without_tls(Config) ->
    logger_ct_backend:capture(error),
    process_flag(trap_exit, true),

    %% WHEN a non-TLS user connects to a TLS listener
    UserSpec0 = escalus_fresh:create_fresh_user(Config, alice),
    Port = ct:get_config({hosts, mim, c2s_tls_port}),
    UserSpec = [{port, Port} | UserSpec0],
    {ok, Client, _} = escalus_connection:start(UserSpec, [{?MODULE, start_stream}]),

    %% THEN the server should close the connection without logging an error
    try
        true = escalus_connection:wait_for_close(Client, timer:seconds(1))
    after
        logger_ct_backend:stop_capture()
    end,
    ?assertEqual([], logger_ct_backend:recv(fun(_, _Msg) -> true end)).

should_pass_with_tlsv1_2(Config) ->
    UserSpec0 = escalus_fresh:create_fresh_user(Config, ?SECURE_USER),
    UserSpec1 = set_secure_connection_protocol(UserSpec0, 'tlsv1.2'),

    %% WHEN
    Result = escalus_connection:start(UserSpec1),

    %% THEN
    ?assertMatch({ok, _, _}, Result).

should_fail_to_authenticate_without_starttls(Config) ->
    %% GIVEN
    UserSpec = escalus_fresh:freshen_spec(Config, ?SECURE_USER),
    ConnectionSteps = [start_stream, stream_features],
    {ok, Conn, Features} = escalus_connection:start(UserSpec, ConnectionSteps),
    UserName = escalus_utils:get_username(Conn),

    %% WHEN
    try escalus_session:authenticate(Conn, Features) of
    %% THEN
        _ ->
            error(authentication_without_tls_suceeded)
    catch
        throw:{auth_failed, User, AuthReply} ->
            ?assertEqual(UserName, User),
            escalus:assert(is_stream_error, [<<"policy-violation">>,
                                             <<"Use of STARTTLS required">>],
                           AuthReply)
    end.

clients_can_connect_with_advertised_ciphers(Config) ->
    ?assert(length(ciphers_working_with_ssl_clients(Config)) > 0).

'clients_can_connect_with_ECDHE-RSA-AES256-GCM-SHA384'(Config) ->
    ?assert(lists:member("ECDHE-RSA-AES256-GCM-SHA384",
                         ciphers_working_with_ssl_clients(Config))).

'clients_can_connect_with_ECDHE-RSA-AES256-GCM-SHA384_only'(Config) ->
    Port = ct:get_config({hosts, mim3, c2s_tls_port}),
    Config1 = [{c2s_port, Port} | Config],
    CiphersStr = os:cmd("openssl ciphers 'ECDHE-RSA-AES256-GCM-SHA384'"),
    ct:pal("Available cipher suites for : ~s", [CiphersStr]),
    ct:pal("Openssl version: ~s", [os:cmd("openssl version")]),
    ?assertEqual(["ECDHE-RSA-AES256-GCM-SHA384"],
                 ciphers_working_with_ssl_clients(Config1)).

correct_features_are_advertised_for_disabled_starttls(Config) ->
    UserSpec = escalus_fresh:freshen_spec(Config, alice),
    Steps = [start_stream,
             stream_features,
             {?MODULE, verify_features_without_starttls},
             authenticate],
    escalus_connection:start(UserSpec, Steps).

correct_features_are_advertised_for_optional_starttls(Config) ->
    UserSpec = escalus_fresh:freshen_spec(Config, ?SECURE_USER),
    Steps = [start_stream,
             stream_features,
             {?MODULE, verify_features_with_optional_starttls},
             maybe_use_ssl,
             {?MODULE, verify_features_without_starttls},
             authenticate],
    escalus_connection:start(UserSpec ++ [{ssl_opts, [{verify, verify_none}]}], Steps).

correct_features_are_advertised_for_required_starttls(Config) ->
    UserSpec = escalus_fresh:freshen_spec(Config, ?SECURE_USER),
    Steps = [start_stream,
             stream_features,
             {?MODULE, verify_features_with_required_starttls},
             maybe_use_ssl,
             {?MODULE, verify_features_without_starttls},
             authenticate],
    escalus_connection:start(UserSpec ++ [{ssl_opts, [{verify, verify_none}]}], Steps).

verify_features_without_starttls(Conn, Features) ->
    ?assertEqual({starttls, false}, get_feature(starttls, Features)),
    ?assertMatch({sasl_mechanisms, [_|_]}, get_feature(sasl_mechanisms, Features)),
    {Conn, Features}.

verify_features_with_optional_starttls(Conn, Features) ->
    ?assertEqual({starttls, true}, get_feature(starttls, Features)),
    ?assertMatch({sasl_mechanisms, [_|_]}, get_feature(sasl_mechanisms, Features)),
    {Conn, Features}.

verify_features_with_required_starttls(Conn, Features) ->
    AdvertisedFeatures = lists:filter(fun is_present/1, Features),
    ?assertEqual([{starttls, true}], AdvertisedFeatures),
    {Conn, Features}.

is_present({_, Value}) ->
    Value =/= false andalso Value =/= [] andalso Value =/= undefined.

get_feature(Feature, FeatureList) ->
    lists:keyfind(Feature, 1, FeatureList).

starttls_should_fail_when_disabled(Config) ->
    UserSpec = escalus_fresh:freshen_spec(Config, alice),
    List = [start_stream, stream_features],
    {ok, Conn, _Features} =
        escalus_connection:start(UserSpec ++ [{ssl_opts, [{verify, verify_none}]}], List),

    %% Client tries to start tls anyway, and fails
    escalus_connection:send(Conn, escalus_stanza:starttls()),
    Result = escalus_connection:get_stanza(Conn, failure),
    %% As defined in https://datatracker.ietf.org/doc/html/rfc6120#section-5.4.2.2, cause 2
    ?assertEqual(<<"failure">>, Result#xmlel.name),
    escalus:assert(has_ns, [?NS_TLS], Result),
    escalus_connection:wait_for_close(Conn, timer:seconds(5)).

metrics_test(Config) ->
    tls_authenticate(Config),

    % Assert that correct events have been executed
    [instrument_helper:assert(Event, Label, fun(#{byte_size := BS}) -> BS > 0;
                                               (#{time := Time}) -> Time > 0 end)
     || {Event, Label} <- instrumentation_events(),
        Event =/= c2s_message_processed].

tls_authenticate(Config) ->
    %% Given
    UserSpec = escalus_fresh:create_fresh_user(Config, ?SECURE_USER),
    ConnectionSteps = [start_stream, stream_features, maybe_use_ssl, authenticate],
    %% when
    {ok, Conn, _} = escalus_connection:start(UserSpec ++ [{ssl_opts, [{verify, verify_none}]}], ConnectionSteps),
    % then
    true = escalus_tcp:is_using_ssl(Conn#client.rcv_pid).

auth_bind_pipelined_session(Config) ->
    UserSpec = [{ssl, true}, {parser_opts, [{start_tag, <<"stream:stream">>}]},
                {ssl_opts, [{verify, verify_none}]}
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
                {ssl_opts, [{verify, verify_none}]},
                {parser_opts, [{start_tag, <<"stream:stream">>}]}
                | escalus_fresh:freshen_spec(Config, alice)],

    Conn = pipeline_connect(UserSpec),

    %% Stream start
    StreamResponse = escalus_connection:get_stanza(Conn, stream_response),
    ?assertMatch(#xmlstreamstart{}, StreamResponse),
    escalus_session:stream_features(Conn, []),

    %% Auth response
    AuthResponse = escalus_connection:get_stanza(Conn, auth_response),
    ?assertMatch(#xmlel{name = <<"failure">>, attrs = #{<<"xmlns">> := ?NS_SASL}}, AuthResponse).

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
    UserSpec = [{resource, <<>>}, {ssl_opts, [{verify, verify_none}]}
                | escalus_fresh:create_fresh_user(Config, ?SECURE_USER)],
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

    wait_helper:wait_until(fun() -> escalus_connection:is_connected(Alice1) end, false),

    escalus_connection:stop(Alice2).

%% Test that resources containing colons work correctly with session management.
%% This is a regression test for https://github.com/esl/MongooseIM/issues/868
same_resource_replaces_session_with_colon(Config) ->
    UserSpec = [{resource, <<"device:with:colons">>} | escalus_users:get_userspec(Config, alice)],
    {ok, Alice1, _} = escalus_connection:start(UserSpec),

    {ok, Alice2, _} = escalus_connection:start(UserSpec),

    ConflictError = escalus:wait_for_stanza(Alice1),
    escalus:assert(is_stream_error, [<<"conflict">>, <<>>], ConflictError),

    wait_helper:wait_until(fun() -> escalus_connection:is_connected(Alice1) end, false),

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
    [C2SPid] = ranch_procs(),
    ok = rpc(mim(), sys, suspend, [C2SPid]),

    % WHEN a session gets replaced ...
    {ok, Alice2, _} = escalus_connection:start(UserSpec),

    % THEN a timeout warning is logged
    FilterFun = fun(_, Msg) ->
                        re:run(Msg, "replaced_wait_timeout") /= nomatch
                end,
    wait_helper:wait_until(
      fun() -> length(logger_ct_backend:recv(FilterFun)) end, 1),

    rpc(mim(), sys, resume, [C2SPid]),
    logger_ct_backend:stop_capture(),

    escalus_connection:stop(Alice2).

replaced_session_cannot_terminate_different_nodes(Config) ->
    % GIVEN a session that is frozen and cannot terminate
    logger_ct_backend:capture(warning, mim2()),
    UserSpec = [{resource, <<"conflict">>} | escalus_users:get_userspec(Config, alice)],
    {ok, _Alice1, _} = escalus_connection:start(UserSpec),
    [C2SPid] = ranch_procs(),
    ok = rpc(mim(), sys, suspend, [C2SPid]),

    % WHEN a session gets replaced on a different node
    UserSpec2 = [{port, 5232} | UserSpec],
    {ok, Alice2, _} = escalus_connection:start(UserSpec2),

    % THEN a timeout warning is logged
    FilterFun = fun(_, Msg) ->
                        re:run(Msg, "replaced_wait_timeout") /= nomatch
                end,
    wait_helper:wait_until(
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
    escalus:send(Alice, escalus_stanza:stream_start(domain())),
    escalus:assert(is_stream_error, [<<"policy-violation">>, <<>>],
                   escalus_connection:get_stanza(Alice, no_stream_error_stanza_received)),
    escalus:assert(is_stream_end,
                   escalus_connection:get_stanza(Alice, no_stream_end_stanza_received)),
    true = escalus_connection:wait_for_close(Alice,timer:seconds(5)).

disconnect_inactive_tcp_connection_after_timeout(Config) ->
    UserSpec = escalus_users:get_userspec(Config, alice),
    % connect w/o sending any XMPP data
    Conn = escalus_connection:connect(UserSpec),
    [Start, Error, _End] = escalus:wait_for_stanzas(Conn, 3),
    escalus:assert(is_stream_error, [<<"connection-timeout">>, <<>>], Error),
    escalus:assert(is_stream_start, Start),
    escalus_connection:stop(Conn).

cannot_connect_with_proxy_header(Config) ->
    %% GIVEN proxy protocol is disabled
    UserSpec = escalus_users:get_userspec(Config, alice),

    %% WHEN
    ConnectionSteps = [{?MODULE, send_proxy_header}, start_stream],
    {ok, ConnResult, _} = escalus_connection:start(UserSpec, ConnectionSteps),

    StreamError = escalus:wait_for_stanza(ConnResult),
    escalus:assert(is_stream_error, [<<"xml-not-well-formed">>, <<>>], StreamError),
    escalus_connection:stop(ConnResult).

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

get_node(Port) ->
    Mim2Port = ct:get_config({hosts, mim2, c2s_tls_port}),
    Mim3Port = ct:get_config({hosts, mim3, c2s_tls_port}),
    case Port of
        Mim2Port ->
            mim2();
        Mim3Port ->
            mim3();
        _ ->
            mim()
    end.

ciphers_available_in_os() ->
    CiphersStr = os:cmd("openssl ciphers 'ALL:eNULL'"),
    [string:strip(C, both, $\n) || C <- string:tokens(CiphersStr, ":")].

ciphers_working_with_ssl_clients(Config) ->
    Port = c2s_port(Config),
    Path = rpc(get_node(Port), os, getenv, ["PWD"]),
    CertPath = Path ++ "/" ++ ?CERT_FILE,
    KeyPath = Path ++ "/" ++ ?KEY_FILE,
    lists:filter(fun(Cipher) ->
                         openssl_client_can_use_cipher(Cipher, Port, CertPath, KeyPath)
                 end, ciphers_available_in_os()).

openssl_client_can_use_cipher(Cipher, Port, CertPath, KeyPath) ->
    PortStr = integer_to_list(Port),
    Cmd = "echo '' | openssl s_client -connect localhost:" ++ PortStr ++
          " -cert \"" ++ CertPath ++ "\""
          " -key \"" ++ KeyPath ++ "\""
          " -cipher \"" ++ Cipher ++ "\" -tls1_2 2>&1",
    Output = os:cmd(Cmd),
    0 == string:str(Output, ":error:") andalso 0 == string:str(Output, "errno=0").

restore_c2s_listener(Config) ->
    C2SListener = ?config(c2s_listener, Config),
    mongoose_helper:restart_listener(mim(), C2SListener).

assert_cert_file_exists() ->
    ejabberd_node_utils:file_exists(?CERT_FILE) orelse
        ct:fail("cert file ~s not exists", [?CERT_FILE]).

configure_c2s_listener(Config, ExtraC2sOpts) ->
    configure_c2s_listener(Config, ExtraC2sOpts, []).

configure_c2s_listener(Config, ExtraC2SOpts, RemovedC2SKeys) ->
    C2SListener = ?config(c2s_listener, Config),
    NewC2SListener = maps:without(RemovedC2SKeys, maps:merge(C2SListener, ExtraC2SOpts)),
    ct:pal("C2S listener: ~p", [NewC2SListener]),
    mongoose_helper:restart_listener(mim(), NewC2SListener).

tls_opts(Mode, Config) ->
    ExtraOpts = #{mode => Mode, verify_mode => none,
                  cacertfile => ?CACERT_FILE, certfile => ?CERT_FILE, keyfile => ?KEY_FILE,
                  dhfile => ?DH_FILE},
    maps:merge(config_parser_helper:default_xmpp_tls(), ExtraOpts).

set_secure_connection_protocol(UserSpec, Version) ->
    [{ssl_opts, [{versions, [Version]}, {verify, verify_none}]} | UserSpec].

connect_to_invalid_host(Spec) ->
    {ok, Conn, _} = escalus_connection:start(Spec, [{?MODULE, connect_to_invalid_host}]),
    escalus:wait_for_stanzas(Conn, 3).

connect_to_invalid_host(Conn, UnusedFeatures) ->
    escalus:send(Conn, escalus_stanza:stream_start(<<"hopefullynonexistentdomain">>)),
    {Conn, UnusedFeatures}.

connect_with_bad_xml(Spec) ->
    {ok, Conn, _} = escalus_connection:start(Spec, [{?MODULE, connect_with_bad_xml}]),
    escalus:wait_for_stanzas(Conn, 3).

connect_with_bad_xml(Conn, UnusedFeatures) ->
    escalus_connection:send(Conn, #xmlcdata{content = "asdf\n"}),
    {Conn, UnusedFeatures}.

start_stream(Conn = #client{props = Props, module = Module}, UnusedFeatures) ->
    escalus:send(Conn, Module:stream_start_req(Props)),
    {Conn, UnusedFeatures}.

children_specs_to_pids(Children) ->
    [Pid || {_, Pid, _, _} <- Children].

ranch_procs() ->
    Listeners = maps:keys(rpc(mim(), ranch, info, [])),
    lists:foldl(
        fun(Listener, Acc) -> rpc(mim(), ranch, procs, [Listener, connections]) ++ Acc end,
        [],
        Listeners).

pipeline_connect(UserSpec) ->
    Server = proplists:get_value(server, UserSpec),
    Username = proplists:get_value(username, UserSpec),
    Password = proplists:get_value(password, UserSpec),
    AuthPayload = <<0:8, Username/binary, 0:8, Password/binary>>,

    Conn = escalus_connection:connect(UserSpec),

    Stream = escalus_stanza:stream_start(Server),
    Auth = escalus_stanza:auth(<<"PLAIN">>, [#xmlcdata{content = base64:encode(AuthPayload)}]),
    Bind = escalus_stanza:bind(<<?MODULE_STRING "_resource">>),
    Session = escalus_stanza:session(),

    escalus_connection:send(Conn, [Stream, Auth, Stream, Bind, Session]),
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

instrumentation_events() ->
    C2sGenericEvents =
        [E || {_, Labels} = E <- instrument_helper:declared_events(mongoose_c2s, []),
              not maps:is_key(host_type, Labels)],
    [{c2s_message_processed, #{host_type => domain_helper:host_type()}} | C2sGenericEvents].
