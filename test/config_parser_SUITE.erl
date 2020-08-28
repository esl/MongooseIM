-module(config_parser_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("ejabberd_config.hrl").

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

-define(err(Expr), ?assertError(_, Expr)).

-define(HOST, <<"myhost">>).

-import(mongoose_config_parser_toml, [parse/1]).

all() ->
    [{group, equivalence},
     {group, general},
     {group, listen}].

groups() ->
    [{equivalence, [parallel], [sample_pgsql,
                                miscellaneous,
                                s2s,
                                modules]},
     {general, [parallel], [loglevel,
                            hosts,
                            registration_timeout,
                            language,
                            all_metrics_are_global,
                            sm_backend,
                            max_fsm_queue,
                            rdbms_server_type,
                            override,
                            pgsql_users_number_estimate,
                            route_subdomain,
                            mongooseimctl_access_commands,
                            routing_modules,
                            replaced_wait_timeout,
                            hide_service_name]},
     {listen, [parallel], [listen_portip,
                           listen_proto,
                           listen_ip_version,
                           listen_backlog,
                           listen_proxy_protocol,
                           listen_access,
                           listen_shaper,
                           listen_xml_socket,
                           listen_zlib,
                           listen_hibernate_after,
                           listen_tls_mode,
                           listen_tls_module,
                           listen_tls_verify,
                           listen_tls_verify_mode,
                           listen_tls_certfile,
                           listen_tls_cacertfile,
                           listen_tls_dhfile,
                           listen_tls_ciphers,
                           listen_tls_versions,
                           listen_max_stanza_size,
                           listen_check_from,
                           listen_hidden_components,
                           listen_conflict_behaviour,
                           listen_password,
                           listen_http_num_acceptors,
                           listen_http_max_connections,
                           listen_http_compress,
                           listen_http_handlers,
                           listen_http_handlers_websockets,
                           listen_http_handlers_lasse,
                           listen_http_handlers_static,
                           listen_http_handlers_api]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    Config.

end_per_suite(_Config) ->
    ok.

sample_pgsql(Config) ->
    test_equivalence_between_files(Config,  "mongooseim-pgsql.cfg",  "mongooseim-pgsql.toml").

miscellaneous(Config) ->
    test_equivalence_between_files(Config,  "miscellaneous.cfg",  "miscellaneous.toml").

s2s(Config) ->
    test_equivalence_between_files(Config,  "s2s_only.cfg",  "s2s_only.toml").

modules(Config) ->
    test_equivalence_between_files(Config,  "modules.cfg",  "modules.toml").

%% tests: general
loglevel(_Config) ->
    ?eq([#local_config{key = loglevel, value = debug}],
        parse(#{<<"general">> => #{<<"loglevel">> => <<"debug">>}})),
    ?err(parse(#{<<"general">> => #{<<"loglevel">> => <<"bebug">>}})).

hosts(_Config) ->
    ?eq([#config{key = hosts, value = [<<"host1">>, <<"host2">>]}],
        parse(#{<<"general">> => #{<<"hosts">> => [<<"host1">>, <<"host2">>]}})),
    ?err(parse(#{<<"general">> => #{<<"hosts">> => [<<"what is this?">>]}})),
    ?err(parse(#{<<"general">> => #{<<"hosts">> => [<<>>]}})),
    ?err(parse(#{<<"general">> => #{<<"hosts">> => []}})),
    ?err(parse(#{<<"general">> => #{<<"hosts">> => [<<"host1">>, <<"host1">>]}})).

registration_timeout(_Config) ->
    ?eq([#local_config{key = registration_timeout, value = infinity}],
        parse(#{<<"general">> => #{<<"registration_timeout">> => <<"infinity">>}})),
    ?eq([#local_config{key = registration_timeout, value = 300}],
        parse(#{<<"general">> => #{<<"registration_timeout">> => 300}})),
    ?err(parse(#{<<"general">> => #{<<"registration_timeout">> => 0}})).

language(_Config) ->
    ?eq([#config{key = language, value = <<"en">>}],
        parse(#{<<"general">> => #{<<"language">> => <<"en">>}})),
    ?err(parse(#{<<"general">> => #{<<"language">> => <<>>}})).

all_metrics_are_global(_Config) ->
    ?eq([#local_config{key = all_metrics_are_global, value = true}],
        parse(#{<<"general">> => #{<<"all_metrics_are_global">> => true}})),
    ?err(parse(#{<<"general">> => #{<<"all_metrics_are_global">> => <<"true">>}})).

sm_backend(_Config) ->
    ?eq([#config{key = sm_backend, value = {mnesia, []}}],
        parse(#{<<"general">> => #{<<"sm_backend">> => <<"mnesia">>}})),
    ?eq([#config{key = sm_backend, value = {redis, []}}],
        parse(#{<<"general">> => #{<<"sm_backend">> => <<"redis">>}})),
    ?err(parse(#{<<"general">> => #{<<"sm_backend">> => <<"amnesia">>}})).

max_fsm_queue(_Config) ->
    ?eq([#local_config{key = max_fsm_queue, value = 100}],
        parse(#{<<"general">> => #{<<"max_fsm_queue">> => 100}})),
    ?err(parse(#{<<"general">> => #{<<"max_fsm_queue">> => -10}})).

http_server_name(_Config) ->
    ?eq([#local_config{key = cowqboy_server_name, value = "myserver"}],
        parse(#{<<"general">> => #{<<"http_server_name">> => <<"my server">>}})),
    ?err(parse(#{<<"general">> => #{<<"http_server_name">> => #{}}})).

rdbms_server_type(_Config) ->
    ?eq([#local_config{key = rdbms_server_type, value = mssql}],
        parse(#{<<"general">> => #{<<"rdbms_server_type">> => <<"mssql">>}})),
    ?eq([#local_config{key = rdbms_server_type, value = pgsql}],
        parse(#{<<"general">> => #{<<"rdbms_server_type">> => <<"pgsql">>}})),
    ?err(parse(#{<<"general">> => #{<<"rdbms_server_type">> => <<"nosql">>}})).

override(_Config) ->
    ?eq([{override, local}, {override, global}, {override, acls}],
        parse(#{<<"general">> => #{<<"override">> => [<<"local">>, <<"global">>, <<"acls">>]}})),
    ?err(parse(#{<<"general">> => #{<<"override">> => [<<"local">>, <<"global">>, <<"local">>]}})),
    ?err(parse(#{<<"general">> => #{<<"override">> => [<<"pingpong">>]}})).

pgsql_users_number_estimate(_Config) ->
    [F] = parse(#{<<"general">> => #{<<"pgsql_users_number_estimate">> => true}}),
    ?eq([#local_config{key = {pgsql_users_number_estimate, ?HOST}, value = true}], F(?HOST)),
    ?err(parse(#{<<"general">> => #{<<"pgsql_users_number_estimate">> => 1200}})).

route_subdomain(_Config) ->
    [F] = parse(#{<<"general">> => #{<<"route_subdomain">> => <<"s2s">>}}),
    ?eq([#local_config{key = {route_subdomain, ?HOST}, value = s2s}], F(?HOST)),
    ?err(parse(#{<<"general">> => #{<<"route_subdomain">> => <<"c2s">>}})).

mongooseimctl_access_commands(_Config) ->
    AccessRule = #{<<"access_rule">> => <<"local">>,
                   <<"commands">> => [<<"join_cluster">>],
                   <<"argument_restrictions">> => #{<<"node">> => <<"mim1@host1">>}},
    ?eq([#local_config{key = mongooseimctl_access_commands,
                       value = [{local, ["join_cluster"], [{node, "mim1@host1"}]}]
                      }],
        parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> => [AccessRule]}})),
    ?err(parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                        [AccessRule#{<<"cat">> => <<"meow">>}]
                                   }})).

routing_modules(_Config) ->
    ?eq([#local_config{key = routing_modules, value = [mongoose_router_global,
                                                       mongoose_router_localdomain]}],
        parse(#{<<"general">> => #{<<"routing_modules">> => [<<"mongoose_router_global">>,
                                                             <<"mongoose_router_localdomain">>]}})),
    ?err(parse(#{<<"general">> => #{<<"routing_modules">> => [<<"moongoose_router_global">>]}})).

replaced_wait_timeout(_Config) ->
    [F] = parse(#{<<"general">> => #{<<"replaced_wait_timeout">> => 1000}}),
    ?eq([#local_config{key = {replaced_wait_timeout, ?HOST}, value = 1000}], F(?HOST)),
    ?err(parse(#{<<"general">> => #{<<"replaced_wait_timeout">> => 0}})).

hide_service_name(_Config) ->
    [F] = parse(#{<<"general">> => #{<<"hide_service_name">> => false}}),
    ?eq([#local_config{key = {hide_service_name, ?HOST}, value = false}], F(?HOST)),
    ?err(parse(#{<<"general">> => #{<<"hide_service_name">> => []}})).

%% tests: listen

listen_portip(_Config) ->
    ?eq(listener_config(ejabberd_c2s, []), parse_listener(<<"c2s">>, #{})),
    ?eq([#local_config{key = listen,
                       value = [{{5222, {192, 168, 1, 16}, tcp}, ejabberd_c2s, []}]}],
        parse_listener(<<"c2s">>, #{<<"ip_address">> => <<"192.168.1.16">>})),
    ?eq([#local_config{key = listen,
                       value = [{{5222, {8193, 3512, 3, 4, 5, 6, 7, 8}, tcp}, ejabberd_c2s, []}]}],
        parse_listener(<<"c2s">>, #{<<"ip_address">> => <<"2001:db8:3:4:5:6:7:8">>})),
    ?err(parse_listener(<<"c2s">>, #{<<"ip_address">> => <<"192.168.1.999">>})),
    ?err(parse(#{<<"listen">> => #{<<"c2s">> => [#{<<"ip_address">> => <<"192.168.1.16">>}]}})),
    ?err(parse(#{<<"listen">> => #{<<"c2s">> => [#{<<"port">> => <<"5222">>}]}})),
    ?err(parse(#{<<"listen">> => #{<<"c2s">> => [#{<<"port">> => 522222}]}})).

listen_proto(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{proto, tcp}]),
        parse_listener(<<"c2s">>, #{<<"proto">> => <<"tcp">>})),
    ?eq([#local_config{key = listen,
                       value = [{{5222, {0, 0, 0, 0}, udp}, ejabberd_c2s, [{proto, udp}]}]}],
        parse_listener(<<"c2s">>, #{<<"proto">> => <<"udp">>})),
    %% 'ejabberd_listener:normalize_proto/1' shows a warning message and falls back to 'tcp'
    ?eq(listener_config(ejabberd_c2s, [{proto, pigeon}]),
        parse_listener(<<"c2s">>, #{<<"proto">> => <<"pigeon">>})).

listen_ip_version(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [inet]),
        parse_listener(<<"c2s">>, #{<<"ip_version">> => 4})),
    ?eq([#local_config{key = listen,
                       value = [{{5222, {0, 0, 0, 0, 0, 0, 0, 0}, tcp}, ejabberd_c2s, []}]}],
        parse_listener(<<"c2s">>, #{<<"ip_version">> => 6})),
    ?err(parse_listener(<<"c2s">>, #{<<"ip_version">> => 7})).

listen_backlog(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{backlog, 10}]),
        parse_listener(<<"c2s">>, #{<<"backlog">> => 10})),
    ?err(parse_listener(<<"c2s">>, #{<<"backlog">> => -10})).

listen_proxy_protocol(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{proxy_protocol, true}]),
        parse_listener(<<"c2s">>, #{<<"proxy_protocol">> => true})),
    ?err(parse_listener(<<"c2s">>, #{<<"proxy_protocol">> => <<"awesome">>})).

listen_access(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{access, rule1}]),
        parse_listener(<<"c2s">>, #{<<"access">> => <<"rule1">>})),
    ?eq(listener_config(ejabberd_s2s_in, [{access, rule1}]),
        parse_listener(<<"s2s">>, #{<<"access">> => <<"rule1">>})),
    ?eq(listener_config(ejabberd_service, [{access, rule1}]),
        parse_listener(<<"service">>, #{<<"access">> => <<"rule1">>})),
    ?err(parse_listener(<<"c2s">>, #{<<"access">> => <<>>})).

listen_shaper(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{shaper, c2s_shaper}]),
        parse_listener(<<"c2s">>, #{<<"shaper">> => <<"c2s_shaper">>})),
    ?eq(listener_config(ejabberd_s2s_in, [{shaper, s2s_shaper}]),
        parse_listener(<<"s2s">>, #{<<"shaper">> => <<"s2s_shaper">>})),
    ?eq(listener_config(ejabberd_service, [{shaper_rule, fast}]),
        parse_listener(<<"service">>, #{<<"shaper_rule">> => <<"fast">>})),
    ?err(parse_listener(<<"s2s">>, #{<<"shaper">> => <<>>})).

listen_xml_socket(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{xml_socket, true}]),
        parse_listener(<<"c2s">>, #{<<"xml_socket">> => true})),
    ?err(parse_listener(<<"c2s">>, #{<<"xml_socket">> => 10})).

listen_zlib(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{zlib, 1024}]),
        parse_listener(<<"c2s">>, #{<<"zlib">> => 1024})),
    ?err(parse_listener(<<"c2s">>, #{<<"zlib">> => 0})).

listen_hibernate_after(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{hibernate_after, 10}]),
        parse_listener(<<"c2s">>, #{<<"hibernate_after">> => 10})),
    ?err(parse_listener(<<"c2s">>, #{<<"hibernate_after">> => -10})).

listen_tls_mode(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [starttls]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"mode">> => <<"starttls">>}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"mode">> => <<"stoptls">>}})).

listen_tls_module(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{tls_module, just_tls}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>}})),
    ?eq(listener_config(ejabberd_c2s, []),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"fast_tls">>}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"slow_tls">>}})).

listen_tls_verify(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [verify_peer]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"verify_peer">> => true}})),
    ?eq(listener_config(ejabberd_c2s, [verify_none]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"verify_peer">> => false}})),
    ?eq(listener_config(ejabberd_c2s, [{tls_module, just_tls}, verify_peer]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"verify_peer">> => true}})),
    ?eq(listener_config(ejabberd_cowboy, [{ssl, [{verify, verify_peer}]}]),
        parse_listener(<<"http">>, #{<<"tls">> => #{<<"verify_peer">> => true}})),
    ?eq(listener_config(ejabberd_c2s, [{tls_module, just_tls}, verify_none]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"verify_peer">> => false}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"verify_peer">> => <<"maybe">>}})).

listen_tls_verify_mode(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{verify_fun, {peer, true}}]}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"verify_mode">> => <<"peer">>}})),
    ?eq(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{verify_fun, {selfsigned_peer, false}}]}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"verify_mode">> => <<"selfsigned_peer">>,
                                                   <<"disconnect_on_failure">> => false}})),
    ?eq(listener_config(ejabberd_cowboy, [{ssl, [{verify_mode, peer}]}]),
        parse_listener(<<"http">>, #{<<"tls">> => #{<<"verify_mode">> => <<"peer">>}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"verify_mode">> => <<"peer">>,
                                                   <<"disconnect_on_failure">> => <<"false">>}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                    <<"verify_mode">> => <<"whatever">>}})),
    ?err(parse_listener(<<"http">>, #{<<"tls">> => #{<<"verify_mode">> => <<"whatever">>}})).

listen_tls_certfile(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{certfile, "mycert.pem"}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"certfile">> => <<"mycert.pem">>}})),
    ?eq(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{certfile, "mycert.pem"}]}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"certfile">> => <<"mycert.pem">>}})),
    ?eq(listener_config(ejabberd_cowboy, [{ssl, [{certfile, "mycert.pem"}]}]),
        parse_listener(<<"http">>, #{<<"tls">> => #{<<"certfile">> => <<"mycert.pem">>}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"certfile">> => <<>>}})).

listen_tls_cacertfile(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{cafile, "cacert.pem"}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"cacertfile">> => <<"cacert.pem">>}})),
    ?eq(listener_config(ejabberd_s2s_in, [{cafile, "cacert.pem"}]),
        parse_listener(<<"s2s">>, #{<<"tls">> => #{<<"cacertfile">> => <<"cacert.pem">>}})),
    ?eq(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{cacertfile, "cacert.pem"}]}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"cacertfile">> => <<"cacert.pem">>}})),
    ?eq(listener_config(ejabberd_cowboy, [{ssl, [{cacertfile, "cacert.pem"}]}]),
        parse_listener(<<"http">>, #{<<"tls">> => #{<<"cacertfile">> => <<"cacert.pem">>}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"cacertfile">> => <<>>}})).

listen_tls_dhfile(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{dhfile, "dh.pem"}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"dhfile">> => <<"dh.pem">>}})),
    ?eq(listener_config(ejabberd_s2s_in, [{dhfile, "dh.pem"}]),
        parse_listener(<<"s2s">>, #{<<"tls">> => #{<<"dhfile">> => <<"dh.pem">>}})),
    ?eq(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{dhfile, "dh.pem"}]}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"dhfile">> => <<"dh.pem">>}})),
    ?eq(listener_config(ejabberd_cowboy, [{ssl, [{dhfile, "dh.pem"}]}]),
        parse_listener(<<"http">>, #{<<"tls">> => #{<<"dhfile">> => <<"dh.pem">>}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"dhfile">> => <<>>}})).


listen_tls_ciphers(_Config) ->
    %% fast_tls ciphers may contain versions as well
    ?eq(listener_config(ejabberd_c2s, [{ciphers, "TLSv1.2:TLSv1.3"}]),
        parse_listener(<<"c2s">>,
                       #{<<"tls">> => #{<<"ciphers">> => <<"TLSv1.2:TLSv1.3">>}})),
    ?eq(listener_config(ejabberd_s2s_in, [{ciphers, "TLSv1.2:TLSv1.3"}]),
        parse_listener(<<"s2s">>,
                       #{<<"tls">> => #{<<"ciphers">> => <<"TLSv1.2:TLSv1.3">>}})),
    ?eq(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{ciphers, ["TLS_AES_256_GCM_SHA384"]}]}]),
        parse_listener(<<"c2s">>,
                       #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                        <<"ciphers">> => [<<"TLS_AES_256_GCM_SHA384">>]}})),
    ?eq(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{ciphers, [#{cipher => aes_256_gcm,
                                                                   key_exchange => any,
                                                                   mac => aead,
                                                                   prf => sha384}]}]}]),
        parse_listener(<<"c2s">>,
                       #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                        <<"ciphers">> => [#{<<"cipher">> => <<"aes_256_gcm">>,
                                                            <<"key_exchange">> => <<"any">>,
                                                            <<"mac">> => <<"aead">>,
                                                            <<"prf">> => <<"sha384">>}]}})),
    ?err(parse_listener(<<"c2s">>,
                        #{<<"tls">> =>
                              #{<<"module">> => <<"just_tls">>,
                                <<"ciphers">> => [#{<<"cipher">> => <<"aes_256_gcm">>}]}})).

listen_tls_versions(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{versions, ['tlsv1.2', 'tlsv1.3']}]}]),
        parse_listener(<<"c2s">>,
                       #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                        <<"versions">> => [<<"tlsv1.2">>, <<"tlsv1.3">>]}})),
    ?err(parse_listener(<<"c2s">>,
                        #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                         <<"versions">> => <<"tlsv1.2">>}})).

listen_max_stanza_size(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{max_stanza_size, 10000}]),
        parse_listener(<<"c2s">>, #{<<"max_stanza_size">> => 10000})),
    ?eq(listener_config(ejabberd_s2s_in, [{max_stanza_size, 10000}]),
        parse_listener(<<"s2s">>, #{<<"max_stanza_size">> => 10000})),
    ?err(parse_listener(<<"c2s">>, #{<<"max_stanza_size">> => <<"infinity">>})).

listen_check_from(_Config) ->
    ?eq(listener_config(ejabberd_service, [{service_check_from, false}]),
        parse_listener(<<"service">>, #{<<"check_from">> => false})),
    ?err(parse_listener(<<"service">>, #{<<"check_from">> => 1})).

listen_hidden_components(_Config) ->
    ?eq(listener_config(ejabberd_service, [{hidden_components, true}]),
        parse_listener(<<"service">>, #{<<"hidden_components">> => true})),
    ?err(parse_listener(<<"service">>, #{<<"hidden_components">> => <<"yes">>})).

listen_conflict_behaviour(_Config) ->
    ?eq(listener_config(ejabberd_service, [{conflict_behaviour, kick_old}]),
        parse_listener(<<"service">>, #{<<"conflict_behaviour">> => <<"kick_old">>})),
    ?err(parse_listener(<<"service">>, #{<<"conflict_behaviour">> => <<"kill_server">>})).

listen_password(_Config) ->
    ?eq(listener_config(ejabberd_service, [{password, "secret"}]),
        parse_listener(<<"service">>, #{<<"password">> => <<"secret">>})),
    ?err(parse_listener(<<"service">>, #{<<"password">> => <<>>})).

listen_http_num_acceptors(_Config) ->
    ?eq(listener_config(ejabberd_cowboy, [{transport_options, [{num_acceptors, 10}]}]),
        parse_listener(<<"http">>, #{<<"transport">> => #{<<"num_acceptors">> => 10}})),
    ?err(parse_listener(<<"http">>, #{<<"transport">> => #{<<"num_acceptors">> => 0}})).

listen_http_max_connections(_Config) ->
    ?eq(listener_config(ejabberd_cowboy, [{transport_options, [{max_connections, 100}]}]),
        parse_listener(<<"http">>, #{<<"transport">> => #{<<"max_connections">> => 100}})),
    ?eq(listener_config(ejabberd_cowboy, [{transport_options, [{max_connections, infinity}]}]),
        parse_listener(<<"http">>, #{<<"transport">> =>
                                         #{<<"max_connections">> => <<"infinity">>}})),
    ?err(parse_listener(<<"http">>, #{<<"transport">> => #{<<"max_connections">> => -1}})).

listen_http_compress(_Config) ->
    ?eq(listener_config(ejabberd_cowboy, [{protocol_options, [{compress, true}]}]),
        parse_listener(<<"http">>, #{<<"protocol">> => #{<<"compress">> => true}})),
    ?err(parse_listener(<<"http">>, #{<<"protocol">> => #{<<"compress">> => 0}})).

listen_http_handlers(_Config) ->
    ?eq(listener_config(ejabberd_cowboy, [{modules, [{"_", "/http-bind", mod_bosh, []}]}]),
        parse_listener(<<"http">>, #{<<"handlers">> =>
                                         #{<<"mod_bosh">> =>
                                               [#{<<"host">> => <<"_">>,
                                                  <<"path">> => <<"/http-bind">>}]}})),
    ?err(parse_listener(<<"http">>, #{<<"handlers">> =>
                                          #{<<"mod_bosch">> =>
                                                [#{<<"host">> => <<"dishwasher">>,
                                                   <<"path">> => <<"/cutlery">>}]}})),
    ?err(parse_listener(<<"http">>, #{<<"handlers">> =>
                                          #{<<"mod_bosh">> =>
                                                [#{<<"host">> => <<"pathless">>}]}})),
    ?err(parse_listener(<<"http">>, #{<<"handlers">> =>
                                          #{<<"mod_bosh">> =>
                                                [#{<<"host">> => <<>>,
                                                   <<"path">> => <<"/">>}]}})),
    ?err(parse_listener(<<"http">>, #{<<"handlers">> =>
                                          #{<<"mod_bosh">> =>
                                                [#{<<"path">> => <<"hostless">>}]}})).

listen_http_handlers_websockets(_Config) ->
    ?eq(listener_config(ejabberd_cowboy, [{modules, [{"localhost", "/api", mod_websockets, []}]}]),
        parse_http_handler(<<"mod_websockets">>, #{})),
    ?eq(listener_config(ejabberd_cowboy, [{modules, [{"localhost", "/api", mod_websockets,
                                                      [{ejabberd_service, [{access, all}]}]
                                                     }]}]),
        parse_http_handler(<<"mod_websockets">>, #{<<"service">> => #{<<"access">> => <<"all">>}})),
    ?err(parse_http_handler(<<"mod_websockets">>, #{<<"service">> => <<"unbelievable">>})).

listen_http_handlers_lasse(_Config) ->
    ?eq(listener_config(ejabberd_cowboy, [{modules, [{"localhost", "/api", lasse_handler,
                                                      [mongoose_client_api_sse]
                                                     }]}]),
        parse_http_handler(<<"lasse_handler">>, #{<<"module">> => <<"mongoose_client_api_sse">>})),
    ?err(parse_http_handler(<<"lasse_handler">>, #{<<"module">> => <<"mooongooose_api_ssie">>})),
    ?err(parse_http_handler(<<"lasse_handler">>, #{})).

listen_http_handlers_static(_Config) ->
    ?eq(listener_config(ejabberd_cowboy, [{modules, [{"localhost", "/api", cowboy_static,
                                                      {priv_dir, cowboy_swagger, "swagger",
                                                       [{mimetypes, cow_mimetypes, all}]}
                                                     }]}]),
        parse_http_handler(<<"cowboy_static">>, #{<<"type">> => <<"priv_dir">>,
                                                  <<"app">> => <<"cowboy_swagger">>,
                                                  <<"content_path">> => <<"swagger">>})),
    ?err(parse_http_handler(<<"cowboy_static">>, #{<<"type">> => <<"priv_dir">>,
                                                   <<"app">> => <<"cowboy_swagger">>})).

listen_http_handlers_api(_Config) ->
    ?eq(listener_config(ejabberd_cowboy, [{modules, [{"localhost", "/api", mongoose_api,
                                                      [{handlers, [mongoose_api_metrics,
                                                                   mongoose_api_users]}]}
                                                    ]}]),
        parse_http_handler(<<"mongoose_api">>, #{<<"handlers">> => [<<"mongoose_api_metrics">>,
                                                                    <<"mongoose_api_users">>]})),
    ?err(parse_http_handler(<<"mongoose_api">>, #{<<"handlers">> => [<<"not_an_api_module">>]})),
    ?err(parse_http_handler(<<"mongoose_api">>, #{})).

%% helpers for 'listen' tests

listener_config(Mod, Opts) ->
    [#local_config{key = listen,
                   value = [{{5222, {0, 0, 0, 0}, tcp}, Mod, Opts}]}].

parse_http_handler(Type, Opts) ->
    parse_listener(<<"http">>, #{<<"handlers">> =>
                                          #{Type =>
                                                [Opts#{<<"host">> => <<"localhost">>,
                                                       <<"path">> => <<"/api">>}]
                                           }}).

parse_listener(Type, Opts) ->
    parse(#{<<"listen">> => #{Type => [Opts#{<<"port">> => 5222}]}}).

%% helpers for 'equivalence' tests

filter_config(#config{key = required_files}) ->
    false; % not supported yet in TOML
filter_config(_) -> true.

handle_config_option(#config{key = K1, value = V1},
                     #config{key = K2, value = V2}) ->
    ?eq(K1, K2),
    compare_values(K1, V1, V2);
handle_config_option(#local_config{key = K1, value = V1},
                     #local_config{key = K2, value = V2}) ->
    ?eq(K1, K2),
    compare_values(K1, V1, V2);
handle_config_option(Opt1, Opt2) ->
    ?eq(Opt1, Opt2).

compare_values(listen, V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_listener/2);
compare_values({auth_opts, _}, V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_auth_opt/2);
compare_values(outgoing_pools, V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_conn_pool/2);
compare_values({modules, _}, V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_modules/2);
compare_values({services, _}, V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_item_with_opts/2);
compare_values({auth_method, _}, V1, V2) when is_atom(V1) ->
    ?eq([V1], V2);
compare_values({s2s_addr, _}, {_, _, _, _} = IP1, IP2) ->
    ?eq(inet:ntoa(IP1), IP2);
compare_values(services, V1, V2) ->
    MetricsOpts1 = proplists:get_value(service_mongoose_system_metrics, V1),
    MetricsOpts2 = proplists:get_value(service_mongoose_system_metrics, V2),
    compare_unordered_lists(MetricsOpts1, MetricsOpts2);
compare_values(K, V1, V2) ->
    ?eq({K, V1}, {K, V2}).

handle_listener({P1, M1, O1}, {P2, M2, O2}) ->
    ?eq(P1, P2),
    ?eq(M1, M2),
    compare_unordered_lists(O1, O2, fun handle_listener_option/2).

handle_listener_option({modules, M1}, {modules, M2}) ->
    compare_unordered_lists(M1, M2, fun handle_listener_module/2);
handle_listener_option({transport_options, O1}, {transport_options, O2}) ->
    compare_unordered_lists(O1, O2);
handle_listener_option(V1, V2) -> ?eq(V1, V2).

handle_listener_module({H1, P1, M1}, M2) ->
    handle_listener_module({H1, P1, M1, []}, M2);
handle_listener_module({H1, P1, M1, O1}, {H2, P2, M2, O2}) ->
    ?eq(H1, H2),
    ?eq(P1, P2),
    ?eq(M1, M2),
    compare_listener_module_options(M1, O1, O2).

compare_listener_module_options(mod_websockets, L1, L2) ->
    E1 = proplists:get_value(ejabberd_service, L1, []),
    E2 = proplists:get_value(ejabberd_service, L2, []),
    T1 = proplists:delete(ejabberd_service, L1),
    T2 = proplists:delete(ejabberd_service, L2),
    compare_unordered_lists(E1, E2),
    compare_unordered_lists(T1, T2);
compare_listener_module_options(_, O1, O2) ->
    ?eq(O1, O2).

handle_auth_opt({cyrsasl_external, M}, {cyrsasl_external, [M]}) -> ok;
handle_auth_opt(V1, V2) -> ?eq(V1, V2).

handle_item_with_opts({M1, O1}, {M2, O2}) ->
    ?eq(M1, M2),
    compare_unordered_lists(O1, O2).

handle_conn_pool({Type1, Scope1, Tag1, POpts1, COpts1},
                 {Type2, Scope2, Tag2, POpts2, COpts2}) ->
    ?eq(Type1, Type2),
    ?eq(Scope1, Scope2),
    ?eq(Tag1, Tag2),
    compare_unordered_lists(POpts1, POpts2),
    compare_unordered_lists(COpts1, COpts2, fun handle_conn_opt/2).

handle_conn_opt({server, {D1, H1, DB1, U1, P1, O1}},
                {server, {D2, H2, DB2, U2, P2, O2}}) ->
    ?eq(D1, D2),
    ?eq(H1, H2),
    ?eq(DB1, DB2),
    ?eq(U1, U2),
    ?eq(P1, P2),
    compare_unordered_lists(O1, O2, fun handle_db_server_opt/2);
handle_conn_opt(V1, V2) -> ?eq(V1, V2).

handle_db_server_opt({ssl_opts, O1}, {ssl_opts, O2}) ->
    compare_unordered_lists(O1, O2);
handle_db_server_opt(V1, V2) -> ?eq(V1, V2).

handle_modules({Name, Opts}, {Name2, Opts2}) ->
    ?eq(Name, Name2),
    compare_unordered_lists(Opts, Opts2, fun handle_module_options/2).

handle_module_options({configs, [Configs1]}, {configs, [Configs2]}) ->
    compare_unordered_lists(Configs1, Configs2, fun handle_module_options/2);
handle_module_options({Name, Opts}, {Name2, Opts2}) ->
    ?eq(Name, Name2),
    compare_unordered_lists(Opts, Opts2, fun handle_module_options/2);
handle_module_options(V1, V2) ->
    ?eq(V1, V2).

%% Generic assertions, use the 'F' handler for any custom cases
compare_unordered_lists(L1, L2) ->
    compare_unordered_lists(L1, L2, fun(V1, V2) -> ?eq(V1, V2) end).

compare_unordered_lists(L1, L2, F) ->
    SL1 = lists:sort(L1),
    SL2 = lists:sort(L2),
    compare_ordered_lists(SL1, SL2, F).

compare_ordered_lists([H1|T1], [H1|T2], F) ->
    compare_ordered_lists(T1, T2, F);
compare_ordered_lists([H1|T1], [H2|T2], F) ->
    try F(H1, H2)
    catch C:R:S ->
            ct:fail({C, R, S})
    end,
    compare_ordered_lists(T1, T2, F);
compare_ordered_lists([], [], _) ->
    ok.

test_equivalence_between_files(Config, File1, File2) ->
    CfgPath = ejabberd_helper:data(Config, File1),
    State1 = mongoose_config_parser_cfg:parse_file(CfgPath),
    Hosts1 = mongoose_config_parser:state_to_host_opts(State1),
    Opts1 = mongoose_config_parser:state_to_opts(State1),

    TOMLPath = ejabberd_helper:data(Config, File2),
    State2 = mongoose_config_parser_toml:parse_file(TOMLPath),
    Hosts2 = mongoose_config_parser:state_to_host_opts(State2),
    Opts2 = mongoose_config_parser:state_to_opts(State2),
    ?eq(Hosts1, Hosts2),
    compare_unordered_lists(lists:filter(fun filter_config/1, Opts1), Opts2,
                            fun handle_config_option/2).
