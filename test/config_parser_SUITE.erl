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
     {group, listen},
     {group, auth},
     {group, pool},
     {group, shaper_acl_access},
     {group, s2s}].

groups() ->
    [{equivalence, [parallel], [sample_pgsql,
                                miscellaneous,
                                s2s,
                                modules,
                                outgoing_pools]},
     {general, [parallel], [loglevel,
                            hosts,
                            registration_timeout,
                            language,
                            all_metrics_are_global,
                            sm_backend,
                            max_fsm_queue,
                            http_server_name,
                            rdbms_server_type,
                            override,
                            pgsql_users_number_estimate,
                            route_subdomains,
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
                           listen_http_handlers_api]},
     {auth, [parallel], [auth_methods,
                         auth_password_format,
                         auth_scram_iterations,
                         auth_cyrsasl_external,
                         auth_allow_multiple_connections,
                         auth_anonymous_protocol,
                         auth_sasl_mechanisms,
                         auth_ldap_pool,
                         auth_ldap_bind_pool,
                         auth_ldap_base,
                         auth_ldap_uids,
                         auth_ldap_filter,
                         auth_ldap_dn_filter,
                         auth_ldap_local_filter,
                         auth_ldap_deref,
                         auth_extauth_instances]},
     {pool, [parallel], [pool_type,
                         pool_tag,
                         pool_scope,
                         pool_workers,
                         pool_strategy,
                         pool_call_timeout,
                         pool_rdbms_settings,
                         pool_rdbms_keepalive_interval,
                         pool_rdbms_server,
                         pool_rdbms_port,
                         pool_rdbms_tls,
                         pool_http_host,
                         pool_http_path_prefix,
                         pool_http_request_timeout,
                         pool_http_opts,
                         pool_redis_host,
                         pool_redis_port,
                         pool_redis_database,
                         pool_redis_password,
                         pool_riak_address,
                         pool_riak_port,
                         pool_riak_credentials,
                         pool_riak_cacertfile,
                         pool_riak_tls,
                         pool_cassandra_servers,
                         pool_cassandra_keyspace,
                         pool_cassandra_tls]},
     {shaper_acl_access, [parallel], [shaper,
                                      acl,
                                      access]},
     {s2s, [parallel], [s2s_dns_timeout,
                        s2s_dns_retries,
                        s2s_outgoing_port,
                        s2s_outgoing_ip_versions,
                        s2s_outgoing_timeout,
                        s2s_use_starttls,
                        s2s_certfile,
                        s2s_default_policy,
                        s2s_address,
                        s2s_ciphers,
                        s2s_domain_certfile,
                        s2s_shared,
                        s2s_max_retry_delay]}
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

outgoing_pools(Config) ->
    test_equivalence_between_files(Config,  "outgoing_pools.cfg",  "outgoing_pools.toml").

%% tests: general
loglevel(_Config) ->
    ?eq([#local_config{key = loglevel, value = debug}],
        parse(#{<<"general">> => #{<<"loglevel">> => <<"debug">>}})),
    ?err(parse(#{<<"general">> => #{<<"loglevel">> => <<"bebug">>}})),
    %% make sure non-host options are not accepted in host_config
    ?err(parse_host_config(#{<<"general">> => #{<<"loglevel">> => <<"debug">>}})).

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
    ?eq([#local_config{key = cowboy_server_name, value = "my server"}],
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
    eq_host_config([#local_config{key = {pgsql_users_number_estimate, ?HOST}, value = true}],
                  #{<<"general">> => #{<<"pgsql_users_number_estimate">> => true}}),
    err_host_config(#{<<"general">> => #{<<"pgsql_users_number_estimate">> => 1200}}).

route_subdomains(_Config) ->
    eq_host_config([#local_config{key = {route_subdomains, ?HOST}, value = s2s}],
                  #{<<"general">> => #{<<"route_subdomains">> => <<"s2s">>}}),
    err_host_config(#{<<"general">> => #{<<"route_subdomains">> => <<"c2s">>}}).

mongooseimctl_access_commands(_Config) ->
    AccessRule = #{<<"commands">> => [<<"join_cluster">>],
                   <<"argument_restrictions">> => #{<<"node">> => <<"mim1@host1">>}},
    ?eq([#local_config{key = mongooseimctl_access_commands,
                       value = [{local, ["join_cluster"], [{node, "mim1@host1"}]}]
                      }],
        parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                       #{<<"local">> => AccessRule}}})),
    ?eq([#local_config{key = mongooseimctl_access_commands,
                       value = [{local, all, []}]
                      }],
        parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                       #{<<"local">> => #{<<"commands">> => <<"all">>}}}})),
    ?err(parse(#{<<"general">> =>
                     #{<<"mongooseimctl_access_commands">> =>
                           #{<<"local">> => #{<<"argument_restrictions">> =>
                                                  #{<<"node">> => <<"mim1@host1">>}}}
                      }})),
    ?err(parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                        #{<<"local">> => #{<<"commands">> => <<"none">>}}
                                   }})).

routing_modules(_Config) ->
    ?eq([#local_config{key = routing_modules, value = [mongoose_router_global,
                                                       mongoose_router_localdomain]}],
        parse(#{<<"general">> => #{<<"routing_modules">> => [<<"mongoose_router_global">>,
                                                             <<"mongoose_router_localdomain">>]}})),
    ?err(parse(#{<<"general">> => #{<<"routing_modules">> => [<<"moongoose_router_global">>]}})).

replaced_wait_timeout(_Config) ->
    eq_host_config([#local_config{key = {replaced_wait_timeout, ?HOST}, value = 1000}],
                  #{<<"general">> => #{<<"replaced_wait_timeout">> => 1000}}),
    err_host_config(#{<<"general">> => #{<<"replaced_wait_timeout">> => 0}}).

hide_service_name(_Config) ->
    eq_host_config([#local_config{key = {hide_service_name, ?HOST}, value = false}],
                  #{<<"general">> => #{<<"hide_service_name">> => false}}),
    err_host_config(#{<<"general">> => #{<<"hide_service_name">> => []}}).

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

%% tests: auth

auth_methods(_Config) ->
    eq_host_config(
      [#local_config{key = {auth_opts, ?HOST}, value = []},
       #local_config{key = {auth_method, ?HOST}, value = [internal, rdbms]}],
      #{<<"auth">> => #{<<"methods">> => [<<"internal">>, <<"rdbms">>]}}),
    err_host_config(#{<<"auth">> => #{<<"methods">> => [<<"supernatural">>]}}).

auth_password_format(_Config) ->
    eq_host_config(
      [#local_config{key = {auth_opts, ?HOST},
                     value = [{password_format, {scram, [sha, sha256]}}]}],
      #{<<"auth">> => #{<<"password">> => #{<<"format">> => <<"scram">>,
                                            <<"hash">> => [<<"sha">>, <<"sha256">>]}}}),
    eq_host_config(
      [#local_config{key = {auth_opts, ?HOST},
                     value = [{password_format, plain}]}],
      #{<<"auth">> => #{<<"password">> => #{<<"format">> => <<"plain">>}}}),

    err_host_config(#{<<"auth">> => #{<<"password">> => #{<<"format">> => <<"no password">>}}}),
    err_host_config(#{<<"auth">> => #{<<"password">> => #{<<"format">> => <<"scram">>,
                                                         <<"hash">> => []}}}),
    err_host_config(#{<<"auth">> => #{<<"password">> => #{<<"format">> => <<"scram">>,
                                                         <<"hash">> => [<<"sha1234">>]}}}).

auth_scram_iterations(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST},
                                 value = [{scram_iterations, 1000}]}],
                  #{<<"auth">> => #{<<"scram_iterations">> => 1000}}),
    err_host_config(#{<<"auth">> => #{<<"scram_iterations">> => false}}).

auth_cyrsasl_external(_Config) ->
    eq_host_config(
      [#local_config{key = {auth_opts, ?HOST},
                     value = [{cyrsasl_external, [standard,
                                                  common_name,
                                                  {mod, cyrsasl_external_verification}]
                              }]}],
      #{<<"auth">> => #{<<"cyrsasl_external">> =>
                            [<<"standard">>,
                             <<"common_name">>,
                             <<"cyrsasl_external_verification">>]}}),
    err_host_config(#{<<"auth">> => #{<<"cyrsasl_external">> => [<<"unknown">>]}}).

auth_allow_multiple_connections(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST}, value = []},
                   #local_config{key = {allow_multiple_connections, ?HOST}, value = true}],
                  #{<<"auth">> => #{<<"allow_multiple_connections">> => true}}),
    err_host_config(#{<<"auth">> => #{<<"allow_multiple_connections">> => <<"yes">>}}).

auth_anonymous_protocol(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST}, value = []},
                   #local_config{key = {anonymous_protocol, ?HOST}, value = login_anon}],
                  #{<<"auth">> => #{<<"anonymous_protocol">> => <<"login_anon">>}}),
    err_host_config(#{<<"auth">> => #{<<"anonymous_protocol">> => <<"none">>}}).

auth_sasl_mechanisms(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST}, value = []},
                   #local_config{key = {sasl_mechanisms, ?HOST},
                                 value = [cyrsasl_external, cyrsasl_scram]}],
                  #{<<"auth">> => #{<<"sasl_mechanisms">> => [<<"external">>, <<"scram">>]}}),
    err_host_config(#{<<"auth">> => #{<<"sasl_mechanisms">> => [<<"none">>]}}).

auth_ldap_pool(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST},
                                 value = [{ldap_pool_tag, ldap_pool}]}],
                  auth_ldap(#{<<"pool_tag">> => <<"ldap_pool">>})),
    err_host_config(auth_ldap(#{<<"pool_tag">> => <<>>})).

auth_ldap_bind_pool(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST},
                                 value = [{ldap_bind_pool_tag, ldap_bind_pool}]}],
                  auth_ldap(#{<<"bind_pool_tag">> => <<"ldap_bind_pool">>})),
    err_host_config(auth_ldap(#{<<"bind_pool_tag">> => true})).

auth_ldap_base(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST},
                                 value = [{ldap_base, "ou=Users,dc=example,dc=com"}]}],
                  auth_ldap(#{<<"base">> => <<"ou=Users,dc=example,dc=com">>})),
    err_host_config(auth_ldap(#{<<"base">> => 10})).

auth_ldap_uids(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST},
                       value = [{ldap_uids, [{"uid1", "user=%u"}]}]}],
                  auth_ldap(#{<<"uids">> => [#{<<"attr">> => <<"uid1">>,
                                               <<"format">> => <<"user=%u">>}]})),
    eq_host_config([#local_config{key = {auth_opts, ?HOST},
                                 value = [{ldap_uids, ["uid1"]}]}],
                  auth_ldap(#{<<"uids">> => [#{<<"attr">> => <<"uid1">>}]})),
    err_host_config(auth_ldap(#{<<"uids">> => [#{<<"format">> => <<"user=%u">>}]})).

auth_ldap_filter(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST},
                                 value = [{ldap_filter, "(objectClass=inetOrgPerson)"}]}],
                  auth_ldap(#{<<"filter">> => <<"(objectClass=inetOrgPerson)">>})),
    err_host_config(auth_ldap(#{<<"filter">> => 10})).

auth_ldap_dn_filter(_Config) ->
    Filter = #{<<"filter">> => <<"(&(name=%s)(owner=%D)(user=%u@%d))">>,
               <<"attributes">> => [<<"sn">>]},
    eq_host_config(
      [#local_config{key = {auth_opts, ?HOST},
                     value = [{ldap_dn_filter, {"(&(name=%s)(owner=%D)(user=%u@%d))", ["sn"]}}]}],
      auth_ldap(#{<<"dn_filter">> => Filter})),
    [err_host_config(auth_ldap(#{<<"dn_filter">> => maps:without([K], Filter)})) ||
        K <- maps:keys(Filter)],
    err_host_config(auth_ldap(#{<<"dn_filter">> => Filter#{<<"filter">> := 12}})),
    err_host_config(auth_ldap(#{<<"dn_filter">> => Filter#{<<"attributes">> := <<"sn">>}})).

auth_ldap_local_filter(_Config) ->
    Filter = #{<<"operation">> => <<"equal">>,
               <<"attribute">> => <<"accountStatus">>,
               <<"values">> => [<<"enabled">>]},
    eq_host_config(
      [#local_config{key = {auth_opts, ?HOST},
                     value = [{ldap_local_filter, {equal, {"accountStatus", ["enabled"]}}}]}],
      auth_ldap(#{<<"local_filter">> => Filter})),
    [err_host_config(auth_ldap(#{<<"local_filter">> => maps:without([K], Filter)})) ||
        K <- maps:keys(Filter)],
    err_host_config(auth_ldap(#{<<"local_filter">> => Filter#{<<"operation">> := <<"lt">>}})),
    err_host_config(auth_ldap(#{<<"local_filter">> => Filter#{<<"attribute">> := <<>>}})),
    err_host_config(auth_ldap(#{<<"local_filter">> => Filter#{<<"values">> := []}})).

auth_ldap_deref(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST},
                                 value = [{ldap_deref, always}]}],
                  auth_ldap(#{<<"deref">> => <<"always">>})),
    err_host_config(auth_ldap(#{<<"deref">> => <<"sometimes">>})).

auth_extauth_instances(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST}, value = []},
                   #local_config{key = {extauth_instances, ?HOST}, value = 2}],
                  #{<<"auth">> => #{<<"extauth_instances">> => 2}}),
    err_host_config(#{<<"auth">> => #{<<"extauth_instances">> => 0}}).

%% tests: outgoing_pools

pool_type(_Config) ->
    ?eq(pool_config({http, global, default, [], []}),
        parse_pool(<<"http">>, <<"default">>, #{})),
    ?err(parse_pool(<<"swimming_pool">>, <<"default">>, #{})).

pool_tag(_Config) ->
    ?eq(pool_config({http, global, my_pool, [], []}),
        parse_pool(<<"http">>, <<"my_pool">>, #{})),
    ?err(parse_pool(<<"http">>, 1000, #{})).

pool_scope(_Config) ->
    ?eq(pool_config({http, global, default, [], []}),
        parse_pool(<<"http">>, <<"default">>, #{})),
    ?eq(pool_config({http, global, default, [], []}),
        parse_pool(<<"http">>, <<"default">>, #{<<"scope">> => <<"global">>})),
    ?eq(pool_config({http, host, default, [], []}),
        parse_pool(<<"http">>, <<"default">>, #{<<"scope">> => <<"host">>})),
    ?eq(pool_config({http, <<"localhost">>, default, [], []}),
        parse_pool(<<"http">>, <<"default">>, #{<<"scope">> => <<"single_host">>,
                                                <<"host">> => <<"localhost">>})),
    ?err(parse_pool(<<"http">>, <<"default">>, #{<<"scope">> => <<"single_host">>})).

pool_workers(_Config) ->
    ?eq(pool_config({http, global, default, [{workers, 10}], []}),
        parse_pool(<<"http">>, <<"default">>, #{<<"workers">> => 10})),
    ?err(parse_pool(<<"http">>, <<"default">>, #{<<"workers">> => 0})).

pool_strategy(_Config) ->
    ?eq(pool_config({http, global, default, [{strategy, best_worker}], []}),
        parse_pool(<<"http">>, <<"default">>, #{<<"strategy">> => <<"best_worker">>})),
    ?err(parse_pool(<<"http">>, <<"default">>, #{<<"strategy">> => <<"worst_worker">>})).

pool_call_timeout(_Config) ->
    ?eq(pool_config({http, global, default, [{call_timeout, 5000}], []}),
        parse_pool(<<"http">>, <<"default">>, #{<<"call_timeout">> => 5000})),
    ?err(parse_pool(<<"http">>, <<"default">>, #{<<"call_timeout">> => 0})).

pool_rdbms_settings(_Config) ->
    ?eq(pool_config({rdbms, global, default, [], [{server, "DSN=mydb"}]}),
        parse_pool_conn(<<"rdbms">>, #{<<"driver">> => <<"odbc">>,
                                       <<"settings">> => <<"DSN=mydb">>})),
    ?err(parse_pool_conn(<<"rdbms">>, #{<<"driver">> => <<"mysql">>,
                                        <<"settings">> => <<"DSN=mydb">>})),
    ?err(parse_pool_conn(<<"rdbms">>, #{<<"driver">> => <<"odbc">>,
                                        <<"settings">> => true})),
    ?err(parse_pool_conn(<<"rdbms">>, #{<<"driver">> => <<"odbc">>})).

pool_rdbms_keepalive_interval(_Config) ->
    ?eq(pool_config({rdbms, global, default, [], [{server, "DSN=mydb"},
                                                  {keepalive_interval, 1000}]}),
        parse_pool_conn(<<"rdbms">>, #{<<"driver">> => <<"odbc">>,
                                       <<"settings">> => <<"DSN=mydb">>,
                                       <<"keepalive_interval">> => 1000})),
    ?err(parse_pool_conn(<<"rdbms">>, #{<<"driver">> => <<"odbc">>,
                                        <<"settings">> => <<"DSN=mydb">>,
                                        <<"keepalive_interval">> => false})).

pool_rdbms_server(_Config) ->
    ServerOpts = rdbms_opts(),
    ?eq(pool_config({rdbms, global, default, [],
                     [{server, {pgsql, "localhost", "db", "dbuser", "secret"}}]}),
        parse_pool_conn(<<"rdbms">>, ServerOpts)),
    ?err(parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"driver">> := <<"odbc">>})),
    [?err(parse_pool_conn(<<"rdbms">>, maps:without([K], ServerOpts))) ||
        K <- maps:keys(ServerOpts)],
    [?err(parse_pool_conn(<<"rdbms">>, ServerOpts#{K := 123})) ||
        K <- maps:keys(ServerOpts)].

pool_rdbms_port(_Config) ->
    ServerOpts = rdbms_opts(),
    ?eq(pool_config({rdbms, global, default, [],
                     [{server, {pgsql, "localhost", 1234, "db", "dbuser", "secret"}}]}),
        parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"port">> => 1234})),
    ?err(parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"port">> => <<"airport">>})).

pool_rdbms_tls(_Config) ->
    ServerOpts = rdbms_opts(),
    ?eq(pool_config({rdbms, global, default, [],
                     [{server, {pgsql, "localhost", "db", "dbuser", "secret",
                                [{ssl, required}]}}]}),
        parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"tls">> => #{<<"required">> => true}})),
    ?eq(pool_config({rdbms, global, default, [],
                     [{server, {pgsql, "localhost", "db", "dbuser", "secret",
                                [{ssl, true}]}}]}),
        parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"tls">> => #{}})),
    ?eq(pool_config({rdbms, global, default, [],
                     [{server, {mysql, "localhost", "db", "dbuser", "secret", []}}]}),
        parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"driver">> => <<"mysql">>,
                                                 <<"tls">> => #{}})),
    ?eq(pool_config({rdbms, global, default, [],
                     [{server, {pgsql, "localhost", 1234, "db", "dbuser", "secret",
                                [{ssl, true}]}}]}),
        parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"tls">> => #{},
                                                 <<"port">> => 1234})),

    %% one option tested here as they are all checked by 'listen_tls_*' tests
    ?eq(pool_config({rdbms, global, default, [],
                     [{server, {pgsql, "localhost", "db", "dbuser", "secret",
                                [{ssl, true}, {ssl_opts, [{certfile, "cert.pem"}]}]}}]}),
        parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"tls">> =>
                                                     #{<<"certfile">> => <<"cert.pem">>}})),
    ?err(parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"tls">> =>
                                                      #{<<"certfile">> => true}})),
    ?err(parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"tls">> => <<"secure">>})).

pool_http_host(_Config) ->
    ?eq(pool_config({http, global, default, [], [{server, "https://localhost:8443"}]}),
        parse_pool_conn(<<"http">>, #{<<"host">> => <<"https://localhost:8443">>})),
    ?err(parse_pool_conn(<<"http">>, #{<<"host">> => 8443})),
    ?err(parse_pool_conn(<<"http">>, #{<<"host">> => ""})).

pool_http_path_prefix(_Config) ->
    ?eq(pool_config({http, global, default, [], [{path_prefix, "/"}]}),
        parse_pool_conn(<<"http">>, #{<<"path_prefix">> => <<"/">>})),
    ?err(parse_pool_conn(<<"http">>, #{<<"path_prefix">> => 8443})),
    ?err(parse_pool_conn(<<"http">>, #{<<"path_prefix">> => ""})).

pool_http_request_timeout(_Config) ->
    ?eq(pool_config({http, global, default, [], [{request_timeout, 2000}]}),
        parse_pool_conn(<<"http">>, #{<<"request_timeout">> => 2000})),
    ?err(parse_pool_conn(<<"http">>, #{<<"request_timeout">> => -1000})),
    ?err(parse_pool_conn(<<"http">>, #{<<"request_timeout">> => <<"infinity">>})).

pool_http_opts(_Config) ->
    HttpOpts = http_opts(),
    ?eq(pool_config({http, global, default, [], 
        [{http_opts, #{retry => 1, retry_timeout => 1000}}]}),
        parse_pool_conn(<<"http">>, #{<<"http_opts">> => HttpOpts})),
    ?err(parse_pool_conn(<<"http">>, #{<<"http_opts">> => HttpOpts#{<<"retry">> => <<"infinity">>}})),
    ?err(parse_pool_conn(<<"http">>, #{<<"http_opts">> => HttpOpts#{<<"server">> => <<"localhost">>}})).

pool_redis_host(_Config) ->
    ?eq(pool_config({redis, global, default, [], [{host, "localhost"}]}),
        parse_pool_conn(<<"redis">>, #{<<"host">> => <<"localhost">>})),
    ?err(parse_pool_conn(<<"redis">>, #{<<"host">> => 8443})),
    ?err(parse_pool_conn(<<"redis">>, #{<<"host">> => ""})).

pool_redis_port(_Config) ->
    ?eq(pool_config({redis, global, default, [], [{port, 6379}]}),
        parse_pool_conn(<<"redis">>, #{<<"port">> => 6379})),
    ?err(parse_pool_conn(<<"redis">>, #{<<"port">> => 666666})),
    ?err(parse_pool_conn(<<"redis">>, #{<<"port">> => <<"airport">>})).

pool_redis_database(_Config) ->
    ?eq(pool_config({redis, global, default, [], [{database, 0}]}),
        parse_pool_conn(<<"redis">>, #{<<"database">> => 0})),
    ?err(parse_pool_conn(<<"redis">>, #{<<"database">> => -1})),
    ?err(parse_pool_conn(<<"redis">>, #{<<"database">> => <<"my_database">>})).

pool_redis_password(_Config) ->
    ?eq(pool_config({redis, global, default, [], [{password, ""}]}),
        parse_pool_conn(<<"redis">>, #{<<"password">> => <<"">>})),
    ?eq(pool_config({redis, global, default, [], [{password, "password1"}]}),
        parse_pool_conn(<<"redis">>, #{<<"password">> => <<"password1">>})),    
    ?err(parse_pool_conn(<<"redis">>, #{<<"password">> => 0})).

pool_riak_address(_Config) ->
    ?eq(pool_config({riak, global, default, [], [{address, "127.0.0.1"}]}),
        parse_pool_conn(<<"riak">>, #{<<"address">> => <<"127.0.0.1">>})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"address">> => 66})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"address">> => <<"">>})).

pool_riak_port(_Config) ->
    ?eq(pool_config({riak, global, default, [], [{port, 8087}]}),
        parse_pool_conn(<<"riak">>, #{<<"port">> => 8087})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"port">> => 666666})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"port">> => <<"airport">>})).

pool_riak_credentials(_Config) ->
    ?eq(pool_config({riak, global, default, [], [{credentials, "user", "pass"}]}),
        parse_pool_conn(<<"riak">>, #{<<"credentials">> =>
            #{<<"user">> => <<"user">>, <<"password">> => <<"pass">>}})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"credentials">> => #{<<"user">> => <<"user">>}})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"credentials">> => #{<<"user">> => <<"">>, <<"password">> => 011001}})).

pool_riak_cacertfile(_Config) ->
    ?eq(pool_config({riak, global, default, [], [{cacertfile, "path/to/cacert.pem"}]}),
        parse_pool_conn(<<"riak">>, #{<<"cacertfile">> => <<"path/to/cacert.pem">>})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"cacertfile">> => <<"">>})).

pool_riak_tls(_Config) ->
    %% one option tested here as they are all checked by 'listen_tls_*' tests
    ?eq(pool_config({riak, global, default, [], [{ssl_opts, [{certfile, "cert.pem"}
        ]}]}),
        parse_pool_conn(<<"riak">>, #{<<"tls">> => #{<<"certfile">> => <<"cert.pem">>}})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"tls">> => #{<<"certfile">> => true}})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"tls">> => <<"secure">>})).

pool_cassandra_servers(_Config) ->
    ?eq(pool_config({cassandra, global, default, [], 
        [{servers, [{"cassandra_server1.example.com", 9042}, {"cassandra_server2.example.com", 9042}]}]}),
        parse_pool_conn(<<"cassandra">>, #{<<"servers">> => [
            #{<<"ip_address">> => <<"cassandra_server1.example.com">>, <<"port">> => 9042},
            #{<<"ip_address">> => <<"cassandra_server2.example.com">>, <<"port">> => 9042}
            ]})),
    ?err(parse_pool_conn(<<"cassandra">>, #{<<"servers">> => 
        #{<<"ip_address">> => <<"cassandra_server1.example.com">>, <<"port">> => 9042}})).

pool_cassandra_keyspace(_Config) ->
    ?eq(pool_config({cassandra, global, default, [], [{keyspace, "big_mongooseim"}]}),
        parse_pool_conn(<<"cassandra">>, #{<<"keyspace">> => <<"big_mongooseim">>})),
    ?err(parse_pool_conn(<<"cassandra">>, #{<<"keyspace">> => <<"">>})).

pool_cassandra_tls(_Config) ->
    %% one option tested here as they are all checked by 'listen_tls_*' tests
    ?eq(pool_config({cassandra, global, default, [], [{ssl, [{verify, verify_none}
        ]}]}),
        parse_pool_conn(<<"cassandra">>, #{<<"tls">> => #{<<"verify_peer">> => false}})),
    ?err(parse_pool_conn(<<"cassandra">>, #{<<"tls">> => #{<<"verify">> => <<"verify_none">>}})).

pool_elastic_host(_Config) ->
    ?eq(pool_config({elastic, global, default, [], [{host, "localhost"}]}),
        parse_pool_conn(<<"elastic">>, #{<<"host">> => <<"localhost">>})),
    ?err(parse_pool_conn(<<"elastic">>, #{<<"host">> => <<"">>})).

pool_elastic_port(_Config) ->
    ?eq(pool_config({elastic, global, default, [], [{port, 9200}]}),
        parse_pool_conn(<<"elastic">>, #{<<"port">> => 9200})),
    ?err(parse_pool_conn(<<"elastic">>, #{<<"port">> => 122333})),
    ?err(parse_pool_conn(<<"elastic">>, #{<<"port">> => <<"airport">>})).

%% tests: shaper, acl, access
shaper(_Config) ->
    eq_host_or_global(
      fun(Host) -> [#config{key = {shaper, normal, Host}, value = {maxrate, 1000}}] end,
      #{<<"shaper">> => #{<<"normal">> => #{<<"max_rate">> => 1000}}}),
    err_host_config(#{<<"shaper">> => #{<<"unlimited">> => #{<<"max_rate">> => <<"infinity">>}}}),
    err_host_config(#{<<"shaper">> => #{<<"fast">> => #{}}}).

acl(_Config) ->
    eq_host_or_global(
      fun(Host) -> [{acl, {local, Host}, all}] end,
      #{<<"acl">> => #{<<"local">> => <<"all">>}}),
    eq_host_or_global(
      fun(Host) -> [{acl, {local, Host}, {user_regexp, <<>>}}] end,
      #{<<"acl">> => #{<<"local">> => #{<<"user_regexp">> => <<>>}}}),
    eq_host_or_global(
      fun(Host) -> [{acl, {alice, Host}, {node_regexp, <<"ali.*">>, <<".*host">>}}] end,
      #{<<"acl">> => #{<<"alice">> => #{<<"user_regexp">> => <<"ali.*">>,
                                        <<"server_regexp">> => <<".*host">>}}}),
    eq_host_or_global(
      fun(Host) -> [{acl, {alice, Host}, {user, <<"alice">>, <<"localhost">>}}] end,
      #{<<"acl">> => #{<<"alice">> => #{<<"user">> => <<"alice">>,
                                        <<"server">> => <<"localhost">>}}}),
    err_host_config(#{<<"acl">> => #{<<"local">> => <<"everybody">>}}),
    err_host_config(#{<<"acl">> => #{<<"alice">> => #{<<"user_glob">> => <<"a*">>,
                                                     <<"server_blog">> => <<"blog.localhost">>}}}).

access(_Config) ->
    eq_host_or_global(
      fun(Host) -> [#config{key = {access, c2s, Host}, value = [{deny, blocked},
                                                                {allow, all}]}]
      end,
      #{<<"access">> => #{<<"c2s">> => [#{<<"acl">> => <<"blocked">>,
                                          <<"value">> => <<"deny">>},
                                        #{<<"acl">> => <<"all">>,
                                          <<"value">> => <<"allow">>}]}}),
    eq_host_or_global(
      fun(Host) -> [#config{key = {access, max_user_sessions, Host}, value = [{10, all}]}] end,
      #{<<"access">> => #{<<"max_user_sessions">> => [#{<<"acl">> => <<"all">>,
                                                        <<"value">> => 10}]}}),
    err_host_config(#{<<"access">> => #{<<"max_user_sessions">> => [#{<<"acl">> => <<"all">>}]}}),
    err_host_config(#{<<"access">> => #{<<"max_user_sessions">> => [#{<<"value">> => 10}]}}),
    err_host_config(#{<<"access">> => #{<<"max_user_sessions">> => [#{<<"acl">> => 10,
                                                                     <<"value">> => 10}]}}).

%% tests: s2s

s2s_dns_timeout(_Config) ->
    ?eq([#local_config{key = s2s_dns_options, value = [{timeout, 5}]}],
        parse(#{<<"s2s">> => #{<<"dns">> => #{<<"timeout">> => 5}}})),
    ?err(parse(#{<<"s2s">> => #{<<"dns">> => #{<<"timeout">> => 0}}})).

s2s_dns_retries(_Config) ->
    ?eq([#local_config{key = s2s_dns_options, value = [{retries, 1}]}],
        parse(#{<<"s2s">> => #{<<"dns">> => #{<<"retries">> => 1}}})),
    ?err(parse(#{<<"s2s">> => #{<<"dns">> => #{<<"retries">> => 0}}})).

s2s_outgoing_port(_Config) ->
    ?eq([#local_config{key = outgoing_s2s_port, value = 5270}],
        parse(#{<<"s2s">> => #{<<"outgoing">> => #{<<"port">> => 5270}}})),
    ?err(parse(#{<<"s2s">> => #{<<"outgoing">> => #{<<"port">> => <<"http">>}}})).

s2s_outgoing_ip_versions(_Config) ->
    ?eq([#local_config{key = outgoing_s2s_families, value = [ipv6, ipv4]}],
        parse(#{<<"s2s">> => #{<<"outgoing">> => #{<<"ip_versions">> => [6, 4]}}})),
    ?err(parse(#{<<"s2s">> => #{<<"outgoing">> => #{<<"ip_versions">> => []}}})),
    ?err(parse(#{<<"s2s">> => #{<<"outgoing">> => #{<<"ip_versions">> => [<<"http">>]}}})).

s2s_outgoing_timeout(_Config) ->
    ?eq([#local_config{key = outgoing_s2s_timeout, value = 5}],
        parse(#{<<"s2s">> => #{<<"outgoing">> => #{<<"connection_timeout">> => 5}}})),
    ?eq([#local_config{key = outgoing_s2s_timeout, value = infinity}],
        parse(#{<<"s2s">> => #{<<"outgoing">> => #{<<"connection_timeout">> => <<"infinity">>}}})),
    ?err(parse(#{<<"s2s">> => #{<<"outgoing">> => #{<<"connection_timeout">> => 0}}})).

s2s_use_starttls(_Config) ->
    ?eq([#local_config{key = s2s_use_starttls, value = required}],
        parse(#{<<"s2s">> => #{<<"use_starttls">> => <<"required">>}})),
    ?err(parse(#{<<"s2s">> => #{<<"use_starttls">> => <<"unnecessary">>}})).

s2s_certfile(_Config) ->
    ?eq([#local_config{key = s2s_certfile, value = "cert.pem"}],
        parse(#{<<"s2s">> => #{<<"certfile">> => <<"cert.pem">>}})),
    ?err(parse(#{<<"s2s">> => #{<<"certfile">> => []}})).

s2s_default_policy(_Config) ->
    eq_host_config([#local_config{key = {s2s_default_policy, ?HOST}, value = deny}],
                  #{<<"s2s">> => #{<<"default_policy">> => <<"deny">>}}),
    err_host_config(#{<<"s2s">> => #{<<"default_policy">> => <<"ask">>}}).

s2s_address(_Config) ->
    Addr = #{<<"host">> => <<"host1">>,
             <<"ip_address">> => <<"192.168.1.2">>,
             <<"port">> => 5321},
    ?eq([#local_config{key = {s2s_addr, <<"host1">>}, value = {"192.168.1.2", 5321}}],
        parse(#{<<"s2s">> => #{<<"address">> => [Addr]}})),
    ?eq([#local_config{key = {s2s_addr, <<"host1">>}, value = "192.168.1.2"}],
        parse(#{<<"s2s">> => #{<<"address">> => [maps:without([<<"port">>], Addr)]}})),
    ?err(parse(#{<<"s2s">> => #{<<"address">> => [maps:without([<<"host">>], Addr)]}})),
    ?err(parse(#{<<"s2s">> => #{<<"address">> => [maps:without([<<"ip_address">>], Addr)]}})),
    ?err(parse(#{<<"s2s">> => #{<<"address">> => [Addr#{<<"host">> => <<>>}]}})),
    ?err(parse(#{<<"s2s">> => #{<<"address">> => [Addr#{<<"ip_address">> => <<"host2">>}]}})),
    ?err(parse(#{<<"s2s">> => #{<<"address">> => [Addr#{<<"port">> => <<"seaport">>}]}})).

s2s_ciphers(_Config) ->
    ?eq([#local_config{key = s2s_ciphers, value = "TLSv1.2:TLSv1.3"}],
        parse(#{<<"s2s">> => #{<<"ciphers">> => <<"TLSv1.2:TLSv1.3">>}})),
    ?err(parse(#{<<"s2s">> => #{<<"ciphers">> => [<<"cipher1">>, <<"cipher2">>]}})).

s2s_domain_certfile(_Config) ->
    DomCert = #{<<"domain">> => <<"myxmpp.com">>,
                <<"certfile">> => <<"mycert.pem">>},
    ?eq([#local_config{key = {domain_certfile, "myxmpp.com"}, value = "mycert.pem"}],
        parse(#{<<"s2s">> => #{<<"domain_certfile">> => [DomCert]}})),
    [?err(parse(#{<<"s2s">> => #{<<"domain_certfile">> => [maps:without([K], DomCert)]}}))
     || K <- maps:keys(DomCert)],
    [?err(parse(#{<<"s2s">> => #{<<"domain_certfile">> => [DomCert#{K := <<>>}]}}))
     || K <- maps:keys(DomCert)].

s2s_shared(_Config) ->
    eq_host_config([#local_config{key = {s2s_shared, ?HOST}, value = <<"secret">>}],
                  #{<<"s2s">> => #{<<"shared">> => <<"secret">>}}),
    err_host_config(#{<<"s2s">> => #{<<"shared">> => 536837}}).

s2s_max_retry_delay(_Config) ->
    eq_host_config([#local_config{key = {s2s_max_retry_delay, ?HOST}, value = 120}],
                  #{<<"s2s">> => #{<<"max_retry_delay">> => 120}}),
    err_host_config(#{<<"s2s">> => #{<<"max_retry_delay">> => 0}}).

%% Helpers for 'listen' tests

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

%% helpers for 'auth' tests

auth_ldap(Opts) ->
    #{<<"auth">> => #{<<"ldap">> => Opts}}.

%% helpers for 'pool' tests

pool_config(Pool) ->
    [#local_config{key = outgoing_pools, value = [Pool]}].

parse_pool(Type, Tag, Opts) ->
   parse(#{<<"outgoing_pools">> => #{Type => #{Tag => Opts}}}).

parse_pool_conn(Type, Opts) ->
   parse(#{<<"outgoing_pools">> => #{Type => #{<<"default">> => #{<<"connection">> => Opts}}}}).

rdbms_opts() ->
    #{<<"driver">> => <<"pgsql">>,
      <<"host">> => <<"localhost">>,
      <<"database">> => <<"db">>,
      <<"username">> => <<"dbuser">>,
      <<"password">> => <<"secret">>}.

http_opts() ->
    #{<<"retry">> => 1, 
      <<"retry_timeout">> => 1000}.

%% helpers for 'host_config' tests

eq_host_config(Result, Config) ->
    [F] = parse(Config), % check for all hosts
    ?eq(Result, F(?HOST)),
    ?eq(Result, parse_host_config(Config)). % check for a single host

eq_host_or_global(ResultF, Config) ->
    ?eq(ResultF(global), parse(Config)), % check for the 'global' host
    ?eq(ResultF(?HOST), parse_host_config(Config)). % check for a single host

err_host_config(Config) ->
    ?err(parse(Config)),
    ?err(parse_host_config(Config)).

parse_host_config(Config) ->
    parse(#{<<"host_config">> => [Config#{<<"host">> => ?HOST}]}).

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
compare_values(s2s_dns_options, V1, V2) ->
    compare_unordered_lists(V1, V2);
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
