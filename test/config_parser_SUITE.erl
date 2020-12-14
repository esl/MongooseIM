-module(config_parser_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("ejabberd_config.hrl").

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

-define(err(Expr), ?assertMatch([#{class := error, what := _}|_],
                                mongoose_config_parser_toml:extract_errors(Expr))).

-define(HOST, <<"myhost">>).

-define(add_loc(X), {X, #{line => ?LINE}}).

-define(eqf(Expected, Actual), eq_host_config(Expected, Actual)).
-define(errf(Config), err_host_config(Config)).

%% Constructs HOF to pass into run_multi/1 function
%% It's a HOF, so it would always pass if not passed into run_multi/1
-define(_eqf(Expected, Actual), ?add_loc(fun() -> ?eqf(Expected, Actual) end)).
-define(_errf(Config), ?add_loc(fun() -> ?errf(Config) end)).

all() ->
    [{group, file},
     {group, general},
     {group, listen},
     {group, auth},
     {group, pool},
     {group, shaper_acl_access},
     {group, s2s},
     {group, modules},
     {group, services}].

groups() ->
    [{file, [parallel], [sample_pgsql,
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
                           listen_num_acceptors,
                           listen_access,
                           listen_shaper,
                           listen_xml_socket,
                           listen_zlib,
                           listen_hibernate_after,
                           listen_max_fsm_queue,
                           listen_max_stanza_size,
                           listen_tls_mode,
                           listen_tls_module,
                           listen_tls_verify,
                           listen_tls_verify_mode,
                           listen_tls_crl_files,
                           listen_tls_certfile,
                           listen_tls_cacertfile,
                           listen_tls_dhfile,
                           listen_tls_ciphers,
                           listen_tls_versions,
                           listen_tls_protocol_options,
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
                         auth_sasl_external,
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
                         auth_external_instances,
                         auth_external_program,
                         auth_http_basic_auth,
                         auth_jwt,
                         auth_riak_bucket_type]},
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
                         pool_http_tls,
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
                         pool_cassandra_auth,
                         pool_cassandra_tls,
                         pool_ldap_host,
                         pool_ldap_port,
                         pool_ldap_servers,
                         pool_ldap_encrypt,
                         pool_ldap_rootdn,
                         pool_ldap_password,
                         pool_ldap_connect_interval,
                         pool_ldap_tls]},
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
                        s2s_host_policy,
                        s2s_address,
                        s2s_ciphers,
                        s2s_domain_certfile,
                        s2s_shared,
                        s2s_max_retry_delay]},
     {modules, [parallel], [mod_adhoc,
                            mod_auth_token,
                            mod_bosh,
                            mod_caps,
                            mod_carboncopy,
                            mod_csi,
                            mod_disco,
                            mod_inbox,
                            mod_global_distrib,
                            mod_event_pusher_sns,
                            mod_event_pusher_push,
                            mod_event_pusher_http,
                            mod_event_pusher_rabbit,
                            mod_extdisco,
                            mod_http_upload,
                            mod_jingle_sip,
                            mod_keystore,
                            mod_last,
                            mod_mam_meta,
                            mod_muc,
                            mod_muc_default_room,
                            mod_muc_default_room_affiliations,
                            mod_muc_log,
                            mod_muc_log_top_link,
                            mod_muc_light,
                            mod_muc_light_config_schema,
                            mod_offline,
                            mod_ping,
                            mod_privacy,
                            mod_private,
                            mod_pubsub,
                            mod_push_service_mongoosepush,
                            mod_register,
                            mod_revproxy,
                            mod_roster,
                            mod_shared_roster_ldap,
                            mod_sic,
                            mod_stream_management,
                            mod_time,
                            mod_vcard,
                            mod_version]},
     {services, [parallel], [service_admin_extra,
                             service_mongoose_system_metrics]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    create_files(Config),
    Config.

end_per_suite(_Config) ->
    ok.

sample_pgsql(Config) ->
    test_config_file(Config,  "mongooseim-pgsql").

miscellaneous(Config) ->
    test_config_file(Config,  "miscellaneous").

s2s(Config) ->
    test_config_file(Config,  "s2s_only").

modules(Config) ->
    test_config_file(Config,  "modules").

outgoing_pools(Config) ->
    test_config_file(Config,  "outgoing_pools").

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
    ?err(parse(#{<<"general">> => #{<<"hosts">> => [<<"host1">>, <<"host1">>]}})),
    ?err(mongoose_config_parser_toml:parse(#{<<"general">> => #{}})). % hosts are mandatory

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
                       value = [{local, all, [{node, "mim1@host1"}]}]
                      }],
        parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                       #{<<"local">> => maps:remove(<<"commands">>,
                                                                    AccessRule)}}})),
    ?eq([#local_config{key = mongooseimctl_access_commands,
                       value = [{local, ["join_cluster"], []}]
                      }],
        parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                       #{<<"local">> => maps:remove(<<"argument_restrictions">>,
                                                                    AccessRule)}}})),
    ?eq([#local_config{key = mongooseimctl_access_commands,
                       value = [{local, all, []}]
                      }],
        parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> => #{<<"local">> => #{}}}})),
    ?err(parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                        #{<<"local">> => #{<<"commands">> => <<"all">>}}}})),
    ?err(parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                        #{<<"local">> => #{<<"argument_restrictions">> =>
                                                               [<<"none">>]}}}})).

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
    ?err(parse_listener(<<"c2s">>, #{<<"proto">> => <<"pigeon">>})).

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
    ?eq(listener_config(ejabberd_s2s_in, [{proxy_protocol, true}]),
        parse_listener(<<"s2s">>, #{<<"proxy_protocol">> => true})),
    ?eq(listener_config(ejabberd_service, [{proxy_protocol, true}]),
        parse_listener(<<"service">>, #{<<"proxy_protocol">> => true})),
    ?err(parse_listener(<<"c2s">>, #{<<"proxy_protocol">> => <<"awesome">>})).

listen_num_acceptors(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{acceptors_num, 100}]),
        parse_listener(<<"c2s">>, #{<<"num_acceptors">> => 100})),
    ?eq(listener_config(ejabberd_s2s_in, [{acceptors_num, 100}]),
        parse_listener(<<"s2s">>, #{<<"num_acceptors">> => 100})),
    ?eq(listener_config(ejabberd_service, [{acceptors_num, 100}]),
        parse_listener(<<"service">>, #{<<"num_acceptors">> => 100})),
    ?err(parse_listener(<<"c2s">>, #{<<"num_acceptors">> => 0})).

listen_access(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{access, rule1}]),
        parse_listener(<<"c2s">>, #{<<"access">> => <<"rule1">>})),
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
    ?eq(listener_config(ejabberd_s2s_in, [{hibernate_after, 10}]),
        parse_listener(<<"s2s">>, #{<<"hibernate_after">> => 10})),
    ?eq(listener_config(ejabberd_service, [{hibernate_after, 10}]),
        parse_listener(<<"service">>, #{<<"hibernate_after">> => 10})),
    ?err(parse_listener(<<"c2s">>, #{<<"hibernate_after">> => -10})).

listen_max_stanza_size(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{max_stanza_size, 10000}]),
        parse_listener(<<"c2s">>, #{<<"max_stanza_size">> => 10000})),
    ?eq(listener_config(ejabberd_s2s_in, [{max_stanza_size, 10000}]),
        parse_listener(<<"s2s">>, #{<<"max_stanza_size">> => 10000})),
    ?eq(listener_config(ejabberd_service, [{max_stanza_size, 10000}]),
        parse_listener(<<"service">>, #{<<"max_stanza_size">> => 10000})),
    ?err(parse_listener(<<"c2s">>, #{<<"max_stanza_size">> => <<"infinity">>})).

listen_max_fsm_queue(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{max_fsm_queue, 1000}]),
        parse_listener(<<"c2s">>, #{<<"max_fsm_queue">> => 1000})),
    ?eq(listener_config(ejabberd_service, [{max_fsm_queue, 1000}]),
        parse_listener(<<"service">>, #{<<"max_fsm_queue">> => 1000})),
    ?err(parse_listener(<<"s2s">>, #{<<"max_fsm_queue">> => 1000})), % only for c2s and service
    ?err(parse_listener(<<"c2s">>, #{<<"max_fsm_queue">> => 0})).

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

listen_tls_crl_files(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {crlfiles, ["file1", "file2"]}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"crl_files">> => [<<"file1">>,
                                                                       <<"file2">>]}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                    <<"crl_files">> => [<<>>]}})),
    %% only for just_tls
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"crl_files">> => [<<"file1">>,
                                                                        <<"file2">>]}})).

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
    ?eq(listener_config(ejabberd_c2s, [{ciphers, "TLS_AES_256_GCM_SHA384"}]),
        parse_listener(<<"c2s">>,
                       #{<<"tls">> => #{<<"ciphers">> => <<"TLS_AES_256_GCM_SHA384">>}})),
    ?eq(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{ciphers, "TLS_AES_256_GCM_SHA384"}]}]),
        parse_listener(<<"c2s">>,
                       #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                        <<"ciphers">> => <<"TLS_AES_256_GCM_SHA384">>}})),
    ?eq(listener_config(ejabberd_s2s_in, [{ciphers, "TLS_AES_256_GCM_SHA384"}]),
        parse_listener(<<"s2s">>,
                       #{<<"tls">> => #{<<"ciphers">> => <<"TLS_AES_256_GCM_SHA384">>}})),
    ?eq(listener_config(ejabberd_cowboy, [{ssl, [{ciphers, "TLS_AES_256_GCM_SHA384"}]}]),
        parse_listener(<<"http">>,
                       #{<<"tls">> => #{<<"ciphers">> => <<"TLS_AES_256_GCM_SHA384">>}})),
    ?err(parse_listener(<<"c2s">>,
                        #{<<"tls">> => #{<<"ciphers">> => [<<"TLS_AES_256_GCM_SHA384">>]}})).

listen_tls_versions(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{versions, ['tlsv1.2', 'tlsv1.3']}]}]),
        parse_listener(<<"c2s">>,
                       #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                        <<"versions">> => [<<"tlsv1.2">>, <<"tlsv1.3">>]}})),
    ?err(parse_listener(<<"c2s">>,
                        #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                         <<"versions">> => <<"tlsv1.2">>}})).

listen_tls_protocol_options(_Config) ->
    ?eq(listener_config(ejabberd_c2s, [{protocol_options, ["nosslv2"]}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"protocol_options">> => [<<"nosslv2">>]}})),
    ?eq(listener_config(ejabberd_s2s_in, [{protocol_options, ["nosslv2"]}]),
        parse_listener(<<"s2s">>, #{<<"tls">> => #{<<"protocol_options">> => [<<"nosslv2">>]}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"protocol_options">> => [<<>>]}})),
    ?err(parse_listener(<<"s2s">>, #{<<"tls">> => #{<<"protocol_options">> => [<<>>]}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                    <<"protocol_options">> => [<<"nosslv2">>]}})).

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
                     value = [{password_format, scram}]}],
      #{<<"auth">> => #{<<"password">> => #{<<"format">> => <<"scram">>}}}),
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

auth_sasl_external(_Config) ->
    eq_host_config(
      [#local_config{key = {auth_opts, ?HOST},
                     value = [{cyrsasl_external, [standard,
                                                  common_name,
                                                  {mod, cyrsasl_external_verification}]
                              }]}],
      #{<<"auth">> => #{<<"sasl_external">> =>
                            [<<"standard">>,
                             <<"common_name">>,
                             <<"cyrsasl_external_verification">>]}}),
    err_host_config(#{<<"auth">> => #{<<"sasl_external">> => [<<"unknown">>]}}).

auth_sasl_mechanisms(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST}, value = []},
                   #local_config{key = {sasl_mechanisms, ?HOST},
                                 value = [cyrsasl_external, cyrsasl_scram]}],
                  #{<<"auth">> => #{<<"sasl_mechanisms">> => [<<"external">>, <<"scram">>]}}),
    err_host_config(#{<<"auth">> => #{<<"sasl_mechanisms">> => [<<"none">>]}}).

auth_allow_multiple_connections(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST}, value = []},
                   #local_config{key = {allow_multiple_connections, ?HOST}, value = true}],
                   auth_config(<<"anonymous">>, #{<<"allow_multiple_connections">> => true})),
    err_host_config(auth_config(<<"anonymous">>, #{<<"allow_multiple_connections">> => <<"yes">>})).

auth_anonymous_protocol(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST}, value = []},
                   #local_config{key = {anonymous_protocol, ?HOST}, value = login_anon}],
                  auth_config(<<"anonymous">>, #{<<"protocol">> => <<"login_anon">>})),
    err_host_config(auth_config(<<"anonymous">>, #{<<"protocol">> => <<"none">>})).

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

auth_external_instances(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST}, value = []},
                   #local_config{key = {extauth_instances, ?HOST}, value = 2}],
                  auth_config(<<"external">>, #{<<"instances">> => 2})),
    err_host_config(auth_config(<<"external">>, #{<<"instances">> => 0})).

auth_external_program(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST},
                                  value = [{extauth_program, "/usr/bin/auth"}]}],
                   auth_config(<<"external">>, #{<<"program">> => <<"/usr/bin/auth">>})),
    err_host_config(auth_config(<<"external">>, #{<<"program">> => <<>>})).

auth_http_basic_auth(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST},
                                  value = [{basic_auth, "admin:admin123"}]}],
                    auth_config(<<"http">>, #{<<"basic_auth">> => <<"admin:admin123">>})),
    err_host_config(auth_config(<<"http">>, #{<<"basic_auth">> => true})).

auth_jwt(_Config) ->
    Opts = #{<<"secret">> => #{<<"value">> => <<"secret123">>},
             <<"algorithm">> => <<"HS512">>,
             <<"username_key">> => <<"user">>}, % tested together as all options are required
    eq_host_config([#local_config{key = {auth_opts, ?HOST},
                                  value = [{jwt_algorithm, "HS512"},
                                           {jwt_secret, "secret123"},
                                           {jwt_username_key, user}]}],
                   auth_config(<<"jwt">>, Opts)),
    FileOpts = Opts#{<<"secret">> := #{<<"file">> => <<"/home/user/jwt_secret">>}},
    eq_host_config([#local_config{key = {auth_opts, ?HOST},
                                  value = [{jwt_algorithm, "HS512"},
                                           {jwt_secret_source, "/home/user/jwt_secret"},
                                           {jwt_username_key, user}]}],
                   auth_config(<<"jwt">>, FileOpts)),
    eq_host_config([#local_config{key = {auth_opts, ?HOST},
                                  value = [{jwt_algorithm, "HS512"},
                                           {jwt_secret_source, {env, "SECRET"}},
                                           {jwt_username_key, user}]}],
                   auth_config(<<"jwt">>, Opts#{<<"secret">> := #{<<"env">> => <<"SECRET">>}})),
    err_host_config(auth_config(<<"jwt">>, Opts#{<<"secret">> := #{<<"value">> => 123}})),
    err_host_config(auth_config(<<"jwt">>, Opts#{<<"secret">> := #{<<"file">> => <<>>}})),
    err_host_config(auth_config(<<"jwt">>, Opts#{<<"secret">> := #{<<"env">> => <<>>}})),
    err_host_config(auth_config(<<"jwt">>, Opts#{<<"secret">> := #{<<"file">> => <<"/jwt_secret">>,
                                                                   <<"env">> => <<"SECRET">>}})),
    err_host_config(auth_config(<<"jwt">>, Opts#{<<"algorithm">> := <<"bruteforce">>})),
    err_host_config(auth_config(<<"jwt">>, Opts#{<<"username_key">> := <<>>})),
    [err_host_config(auth_config(<<"jwt">>, maps:without([K], Opts))) || K <- maps:keys(Opts)].

auth_riak_bucket_type(_Config) ->
    eq_host_config([#local_config{key = {auth_opts, ?HOST},
                                  value = [{bucket_type, <<"buckethead">>}]}],
                   auth_config(<<"riak">>, #{<<"bucket_type">> => <<"buckethead">>})),
    err_host_config(auth_config(<<"riak">>, #{<<"bucket_type">> => <<>>})).

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
    ?err(parse_pool(<<"http">>, <<"default">>, #{<<"scope">> => <<"whatever">>})),
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

pool_http_tls(_Config) ->
    ?eq(pool_config({http, global, default, [], [{http_opts, [{certfile, "cert.pem"} ]}]}),
        parse_pool_conn(<<"http">>, #{<<"tls">> => #{<<"certfile">> => <<"cert.pem">>}})),
    ?err(parse_pool_conn(<<"http">>, #{<<"tls">> => #{<<"certfile">> => true}})),
    ?err(parse_pool_conn(<<"http">>, #{<<"tls">> => <<"secure">>})).

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
    ?err(parse_pool_conn(<<"riak">>, #{<<"credentials">> =>
                                           #{<<"user">> => <<"">>, <<"password">> => 011001}})).

pool_riak_cacertfile(_Config) ->
    ?eq(pool_config({riak, global, default, [], [{cacertfile, "cacert.pem"}]}),
        parse_pool_conn(<<"riak">>, #{<<"tls">> => #{<<"cacertfile">> => <<"cacert.pem">>}})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"cacertfile">> => <<"">>})).

pool_riak_tls(_Config) ->
    %% make sure these options are not extracted out of 'ssl_opts'
    %% all the TLS options are checked by 'listen_tls_*' tests
    ?eq(pool_config({riak, global, default, [], [{ssl_opts, [{certfile, "path/to/cert.pem"},
                                                             {dhfile, "cert.pem"},
                                                             {keyfile, "path/to/key.pem"}]}]}),
        parse_pool_conn(<<"riak">>, #{<<"tls">> => #{<<"certfile">> => <<"path/to/cert.pem">>,
                                                     <<"dhfile">> => <<"cert.pem">>,
                                                     <<"keyfile">> => <<"path/to/key.pem">>}})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"tls">> => #{<<"dhfile">> => true}})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"tls">> => <<"secure">>})).

pool_cassandra_servers(_Config) ->
    ?eq(pool_config({cassandra, global, default, [],
        [{servers, [{"cassandra_server1.example.com", 9042},
                    {"cassandra_server2.example.com", 9042}]}]}),
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

pool_cassandra_auth(_Config) ->
    ?eq(pool_config({cassandra, global, default, [], [{auth, {cqerl_auth_plain_handler,
                                                              [{<<"auser">>, <<"secretpass">>}]
                                                             }}]}),
        parse_pool_conn(<<"cassandra">>,
                        #{<<"auth">> => #{<<"plain">> => #{<<"username">> => <<"auser">>,
                                                           <<"password">> => <<"secretpass">>}}})),
    ?err(parse_pool_conn(<<"cassandra">>, #{<<"tls">> => #{<<"verify">> => <<"verify_none">>}})).

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

pool_rabbit_amqp_host(_Config) ->
    ?eq(pool_config({rabbit, global, default, [], [{amqp_host, "localhost"}]}),
        parse_pool_conn(<<"rabbit">>, #{<<"amqp_host">> => <<"localhost">>})),
    ?err(parse_pool_conn(<<"rabbit">>, #{<<"amqp_host">> => <<"">>})).

pool_rabbit_amqp_port(_Config) ->
    ?eq(pool_config({rabbit, global, default, [], [{amqp_port, 5672}]}),
        parse_pool_conn(<<"rabbit">>, #{<<"amqp_port">> => 5672})),
    ?err(parse_pool_conn(<<"rabbit">>, #{<<"amqp_port">> => <<"airport">>})).

pool_rabbit_amqp_username(_Config) ->
    ?eq(pool_config({rabbit, global, default, [], [{amqp_username, "guest"}]}),
        parse_pool_conn(<<"rabbit">>, #{<<"amqp_username">> => <<"guest">>})),
    ?err(parse_pool_conn(<<"rabbit">>, #{<<"amqp_username">> => <<"">>})).

pool_rabbit_amqp_password(_Config) ->
    ?eq(pool_config({rabbit, global, default, [], [{amqp_password, "guest"}]}),
        parse_pool_conn(<<"rabbit">>, #{<<"amqp_password">> => <<"guest">>})),
    ?err(parse_pool_conn(<<"rabbit">>, #{<<"amqp_password">> => <<"">>})).

pool_rabbit_amqp_confirms_enabled(_Config) ->
    ?eq(pool_config({rabbit, global, default, [], [{confirms_enabled, true}]}),
        parse_pool_conn(<<"rabbit">>, #{<<"confirms_enabled">> => true})),
    ?err(parse_pool_conn(<<"rabbit">>, #{<<"confirms_enabled">> => <<"yes">>})).

pool_rabbit_amqp_max_worker_queue_len(_Config) ->
    ?eq(pool_config({rabbit, global, default, [], [{max_worker_queue_len, 100}]}),
        parse_pool_conn(<<"rabbit">>, #{<<"max_worker_queue_len">> => 100})),
    ?err(parse_pool_conn(<<"rabbit">>, #{<<"max_worker_queue_len">> => 0})).

pool_ldap_host(_Config) ->
    ?eq(pool_config({ldap, global, default, [], [{host, "localhost"}]}),
        parse_pool_conn(<<"ldap">>, #{<<"host">> => <<"localhost">>})),
    ?err(parse_pool_conn(<<"ldap">>, #{<<"host">> => <<"">>})).

pool_ldap_port(_Config) ->
    ?eq(pool_config({ldap, global, default, [], [{port, 389}]}),
        parse_pool_conn(<<"ldap">>, #{<<"port">> => 389})),
    ?err(parse_pool_conn(<<"ldap">>, #{<<"port">> => <<"airport">>})).

pool_ldap_servers(_Config) ->
    ?eq(pool_config({ldap, global, default, [],
        [{servers, ["primary-ldap-server.example.com", "secondary-ldap-server.example.com"]}]}),
        parse_pool_conn(<<"ldap">>, #{<<"servers">> =>
            [<<"primary-ldap-server.example.com">>, <<"secondary-ldap-server.example.com">>]})),
    ?err(parse_pool_conn(<<"ldap">>, #{<<"servers">> => #{<<"server">> => <<"example.com">>}})).

pool_ldap_encrypt(_Config) ->
    ?eq(pool_config({ldap, global, default, [], [{encrypt, none}]}),
        parse_pool_conn(<<"ldap">>, #{<<"encrypt">> => <<"none">>})),
    ?err(parse_pool_conn(<<"ldap">>, #{<<"encrypt">> => true})).

pool_ldap_rootdn(_Config) ->
    ?eq(pool_config({ldap, global, default, [], [{rootdn, ""}]}),
        parse_pool_conn(<<"ldap">>, #{<<"rootdn">> => <<"">>})),
    ?err(parse_pool_conn(<<"ldap">>, #{<<"rootdn">> => false})).

pool_ldap_password(_Config) ->
    ?eq(pool_config({ldap, global, default, [], [{password, "pass"}]}),
        parse_pool_conn(<<"ldap">>, #{<<"password">> => <<"pass">>})),
    ?err(parse_pool_conn(<<"ldap">>, #{<<"password">> => true})).

pool_ldap_connect_interval(_Config) ->
    ?eq(pool_config({ldap, global, default, [], [{connect_interval, 10000}]}),
        parse_pool_conn(<<"ldap">>, #{<<"connect_interval">> => 10000})),
    ?err(parse_pool_conn(<<"ldap">>, #{<<"connect_interval">> => <<"infinity">>})).

pool_ldap_tls(_Config) ->
    %% one option tested here as they are all checked by 'listen_tls_*' tests
    ?eq(pool_config({ldap, global, default, [], [{tls_options, [{verify, verify_peer}
        ]}]}),
        parse_pool_conn(<<"ldap">>, #{<<"tls">> => #{<<"verify_peer">> => true}})),
    ?err(parse_pool_conn(<<"ldap">>, #{<<"tls">> => #{<<"verify">> => <<"verify_none">>}})).

%% tests: shaper, acl, access
shaper(_Config) ->
    eq_host_or_global(
      fun(Host) -> [#config{key = {shaper, normal, Host}, value = {maxrate, 1000}}] end,
      #{<<"shaper">> => #{<<"normal">> => #{<<"max_rate">> => 1000}}}),
    err_host_or_global(#{<<"shaper">> => #{<<"unlimited">> =>
                                               #{<<"max_rate">> => <<"infinity">>}}}),
    err_host_or_global(#{<<"shaper">> => #{<<"fast">> => #{}}}).

acl(_Config) ->
    eq_host_or_global(
      fun(Host) -> [{acl, {local, Host}, all}] end,
      #{<<"acl">> => #{<<"local">> => [#{<<"match">> => <<"all">>}]}}),
    eq_host_or_global(
      fun(Host) -> [{acl, {local, Host}, {user_regexp, <<>>}}] end,
      #{<<"acl">> => #{<<"local">> => [#{<<"user_regexp">> => <<>>}]}}),
    eq_host_or_global(
      fun(Host) -> [{acl, {alice, Host}, {node_regexp, <<"ali.*">>, <<".*host">>}}] end,
      #{<<"acl">> => #{<<"alice">> => [#{<<"user_regexp">> => <<"ali.*">>,
                                         <<"server_regexp">> => <<".*host">>}]}}),
    eq_host_or_global(
      fun(Host) -> [{acl, {alice, Host}, {user, <<"alice">>, <<"localhost">>}}] end,
      #{<<"acl">> => #{<<"alice">> => [#{<<"user">> => <<"alice">>,
                                         <<"server">> => <<"localhost">>}]}}),
    err_host_or_global(#{<<"acl">> => #{<<"local">> => <<"everybody">>}}),
    err_host_or_global(#{<<"acl">> => #{<<"alice">> => [#{<<"user_glob">> => <<"a*">>,
                                                          <<"server_blog">> => <<"bloghost">>}]}}).

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
    err_host_or_global(#{<<"access">> => #{<<"max_user_sessions">> =>
                                               [#{<<"acl">> => <<"all">>}]}}),
    err_host_or_global(#{<<"access">> => #{<<"max_user_sessions">> =>
                                               [#{<<"value">> => 10}]}}),
    err_host_or_global(#{<<"access">> => #{<<"max_user_sessions">> =>
                                               [#{<<"acl">> => 10,
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

s2s_host_policy(_Config) ->
    Policy = #{<<"host">> => <<"host1">>,
               <<"policy">> => <<"allow">>},
    eq_host_config([#local_config{key = {{s2s_host, <<"host1">>}, ?HOST}, value = allow}],
                  #{<<"s2s">> => #{<<"host_policy">> => [Policy]}}),
    eq_host_config([#local_config{key = {{s2s_host, <<"host1">>}, ?HOST}, value = allow},
                    #local_config{key = {{s2s_host, <<"host2">>}, ?HOST}, value = deny}],
                  #{<<"s2s">> => #{<<"host_policy">> => [Policy, #{<<"host">> => <<"host2">>,
                                                                   <<"policy">> => <<"deny">>}]}}),
    err_host_config(#{<<"s2s">> => #{<<"host_policy">> => [maps:without([<<"host">>], Policy)]}}),
    err_host_config(#{<<"s2s">> => #{<<"host_policy">> => [maps:without([<<"policy">>], Policy)]}}),
    err_host_config(#{<<"s2s">> => #{<<"host_policy">> => [Policy#{<<"host">> => <<>>}]}}),
    err_host_config(#{<<"s2s">> => #{<<"host_policy">> => [Policy#{<<"policy">> => <<"huh">>}]}}),
    err_host_config(#{<<"s2s">> => #{<<"host_policy">> => [Policy,
                                                           Policy#{<<"policy">> => <<"deny">>}]}}).

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
    ?err(parse(#{<<"s2s">> => #{<<"address">> => [Addr#{<<"port">> => <<"seaport">>}]}})),
    ?err(parse(#{<<"s2s">> => #{<<"address">> => [Addr, maps:remove(<<"port">>, Addr)]}})).

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
     || K <- maps:keys(DomCert)],
    ?err(parse(#{<<"s2s">> => #{<<"domain_certfile">> => [DomCert, DomCert]}})).

s2s_shared(_Config) ->
    eq_host_config([#local_config{key = {s2s_shared, ?HOST}, value = <<"secret">>}],
                  #{<<"s2s">> => #{<<"shared">> => <<"secret">>}}),
    err_host_config(#{<<"s2s">> => #{<<"shared">> => 536837}}).

s2s_max_retry_delay(_Config) ->
    eq_host_config([#local_config{key = {s2s_max_retry_delay, ?HOST}, value = 120}],
                  #{<<"s2s">> => #{<<"max_retry_delay">> => 120}}),
    err_host_config(#{<<"s2s">> => #{<<"max_retry_delay">> => 0}}).

%% modules

mod_adhoc(_Config) ->
    check_iqdisc(mod_adhoc),
    M = fun(K, V) -> modopts(mod_adhoc, [{K, V}]) end,
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_adhoc">> => #{K => V}}} end,
    %% report_commands_node is boolean
    ?eqf(M(report_commands_node, true), T(<<"report_commands_node">>, true)),
    ?eqf(M(report_commands_node, false), T(<<"report_commands_node">>, false)),
    %% not boolean
    ?errf(T(<<"report_commands_node">>, <<"hello">>)).

mod_auth_token(_Config) ->
    check_iqdisc(mod_auth_token),
    P = fun(X) ->
                Opts = #{<<"validity_period">> => X},
                #{<<"modules">> => #{<<"mod_auth_token">> => Opts}}
        end,
    ?eqf(modopts(mod_auth_token, [{{validity_period, access}, {13, minutes}},
                                  {{validity_period, refresh}, {31, days}}]),
         P([#{<<"token">> => <<"access">>, <<"value">> => 13, <<"unit">> => <<"minutes">>},
            #{<<"token">> => <<"refresh">>, <<"value">> => 31, <<"unit">> => <<"days">>}])),
    ?errf(P([#{<<"token">> => <<"access">>, <<"value">> => <<"13">>,
               <<"unit">> => <<"minutes">>}])),
    ?errf(P([#{<<"token">> => <<"access">>, <<"value">> => 13, <<"unit">> => <<"minute">>}])),
    ?errf(P([#{<<"token">> => <<"Access">>, <<"value">> => 13, <<"unit">> => <<"minutes">>}])),
    ?errf(P([#{<<"value">> => 13, <<"unit">> => <<"minutes">>}])),
    ?errf(P([#{<<"token">> => <<"access">>, <<"unit">> => <<"minutes">>}])),
    ?errf(P([#{<<"token">> => <<"access">>, <<"value">> => 13}])).

mod_bosh(_Config) ->
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_bosh">> => #{K => V}}} end,
    M = fun(K, V) -> modopts(mod_bosh, [{K, V}]) end,
    ?eqf(M(inactivity, 10), T(<<"inactivity">>, 10)),
    ?eqf(M(inactivity, infinity), T(<<"inactivity">>, <<"infinity">>)),
    ?eqf(M(inactivity, 10), T(<<"inactivity">>, 10)),
    ?eqf(M(max_wait, infinity), T(<<"max_wait">>, <<"infinity">>)),
    ?eqf(M(server_acks, true), T(<<"server_acks">>, true)),
    ?eqf(M(server_acks, false), T(<<"server_acks">>, false)),
    ?eqf(M(maxpause, 10), T(<<"max_pause">>, 10)),
    ?errf(T(<<"inactivity">>, -1)),
    ?errf(T(<<"inactivity">>, <<"10">>)),
    ?errf(T(<<"inactivity">>, <<"inactivity">>)),
    ?errf(T(<<"max_wait">>, <<"10">>)),
    ?errf(T(<<"max_wait">>, -1)),
    ?errf(T(<<"server_acks">>, -1)),
    ?errf(T(<<"maxpause">>, 0)).

mod_caps(_Config) ->
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_caps">> => #{K => V}}} end,
    M = fun(K, V) -> modopts(mod_caps, [{K, V}]) end,
    ?eqf(M(cache_size, 10), T(<<"cache_size">>, 10)),
    ?eqf(M(cache_life_time, 10), T(<<"cache_life_time">>, 10)),
    ?errf(T(<<"cache_size">>, 0)),
    ?errf(T(<<"cache_size">>, <<"infinity">>)),
    ?errf(T(<<"cache_life_time">>, 0)),
    ?errf(T(<<"cache_life_time">>, <<"infinity">>)).

mod_carboncopy(_Config) ->
    check_iqdisc(mod_carboncopy).

mod_csi(_Config) ->
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_csi">> => #{K => V}}} end,
    M = fun(K, V) -> modopts(mod_csi, [{K, V}]) end,
    ?eqf(M(buffer_max, 10), T(<<"buffer_max">>, 10)),
    ?eqf(M(buffer_max, infinity), T(<<"buffer_max">>, <<"infinity">>)),
    ?errf(T(<<"buffer_max">>, -1)).

mod_disco(_Config) ->
    check_iqdisc(mod_disco),
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_disco">> => #{K => V}}} end,
    ?eqf(modopts(mod_disco, [{users_can_see_hidden_services, true}]),
         T(<<"users_can_see_hidden_services">>, true)),
    ?eqf(modopts(mod_disco, [{users_can_see_hidden_services, false}]),
         T(<<"users_can_see_hidden_services">>, false)),
    %% extra_domains are binaries
    ?eqf(modopts(mod_disco, [{extra_domains, [<<"localhost">>, <<"erlang-solutions.com">>]}]),
         T(<<"extra_domains">>, [<<"localhost">>, <<"erlang-solutions.com">>])),
    ?eqf(modopts(mod_disco, [{extra_domains, []}]),
         T(<<"extra_domains">>, [])),
    Info = #{<<"name">> => <<"abuse-address">>,
             <<"urls">> => [<<"admin@example.com">>]},
    ?eqf(modopts(mod_disco, [{server_info, [{all, "abuse-address", ["admin@example.com"]},
                                            {[mod_muc, mod_disco], "friendly-spirits",
                                             ["spirit1@localhost", "spirit2@localhost"]}]}
                            ]),
         T(<<"server_info">>, [Info, #{<<"modules">> => [<<"mod_muc">>, <<"mod_disco">>],
                                       <<"name">> => <<"friendly-spirits">>,
                                       <<"urls">> => [<<"spirit1@localhost">>,
                                                      <<"spirit2@localhost">>]}
                              ])),
    ?errf(T(<<"users_can_see_hidden_services">>, 1)),
    ?errf(T(<<"users_can_see_hidden_services">>, <<"true">>)),
    ?errf(T(<<"extra_domains">>, [<<"user@localhost">>])),
    ?errf(T(<<"extra_domains">>, [1])),
    ?errf(T(<<"extra_domains">>, <<"domains domains domains">>)),
    ?errf(T(<<"server_info">>, [Info#{<<"name">> => 1}])),
    ?errf(T(<<"server_info">>, [Info#{<<"name">> => <<"">>}])),
    ?errf(T(<<"server_info">>, [Info#{<<"modules">> => <<"roll">>}])),
    ?errf(T(<<"server_info">>, [Info#{<<"modules">> => [<<"meow_meow_meow">>]}])),
    ?errf(T(<<"server_info">>, [Info#{<<"urls">> => [1]}])),
    ?errf(T(<<"server_info">>, [Info#{<<"urls">> => [<<"">>]}])),
    ?errf(T(<<"server_info">>, [maps:remove(<<"name">>, Info)])),
    ?errf(T(<<"server_info">>, [maps:remove(<<"urls">>, Info)])).

mod_extdisco(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_extdisco">> => Opts}} end,
    Service = #{
        <<"type">> => <<"stun">>,
        <<"host">> => <<"stun1">>,
        <<"port">> => 3478,
        <<"transport">> => <<"udp">>,
        <<"username">> => <<"username">>,
        <<"password">> => <<"password">>},
    Base = #{<<"service">> => [Service]},
    MBase = [{host, "stun1"},
             {password, "password"},
             {port, 3478},
             {transport, "udp"},
             {type, stun},
             {username, "username"}],
    ?eqf(modopts(mod_extdisco, [MBase]), T(Base)),
    %% Invalid service type
    ?errf(T(Base#{<<"service">> => [Base#{<<"type">> => -1}]})),
    ?errf(T(Base#{<<"service">> => [Base#{<<"type">> => ["stun"]}]})),
    %% Invalid host
    ?errf(T(Base#{<<"service">> => [Base#{<<"host">> => [1]}]})),
    ?errf(T(Base#{<<"service">> => [Base#{<<"host">> => true}]})),
    %% Invalid port
    ?errf(T(Base#{<<"service">> => [Base#{<<"port">> => -1}]})),
    ?errf(T(Base#{<<"service">> => [Base#{<<"port">> => 9999999}]})),
    ?errf(T(Base#{<<"service">> => [Base#{<<"port">> => "port"}]})),
    %% Invalid transport
    ?errf(T(Base#{<<"service">> => [Base#{<<"transport">> => -1}]})),
    ?errf(T(Base#{<<"service">> => [Base#{<<"transport">> => ""}]})),
    %% Invalid username
    ?errf(T(Base#{<<"service">> => [Base#{<<"username">> => -2}]})),
    %% Invalid password
    ?errf(T(Base#{<<"service">> => [Base#{<<"password">> => 1}]})),
    ?errf(T(Base#{<<"service">> => [Base#{<<"password">> => [<<"test">>]}]})).

mod_inbox(_Config) ->
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_inbox">> => #{K => V}}} end,
    ?eqf(modopts(mod_inbox, [{reset_markers, [displayed, received, acknowledged]}]),
         T(<<"reset_markers">>, [<<"displayed">>, <<"received">>, <<"acknowledged">>])),
    ?eqf(modopts(mod_inbox, [{reset_markers, []}]),
         T(<<"reset_markers">>, [])),
    ?eqf(modopts(mod_inbox, [{groupchat, [muc, muclight]}]),
         T(<<"groupchat">>, [<<"muc">>, <<"muclight">>])),
    ?eqf(modopts(mod_inbox, [{groupchat, []}]),
         T(<<"groupchat">>, [])),
    ?eqf(modopts(mod_inbox, [{aff_changes, true}]),
         T(<<"aff_changes">>, true)),
    ?eqf(modopts(mod_inbox, [{aff_changes, false}]),
         T(<<"aff_changes">>, false)),
    ?eqf(modopts(mod_inbox, [{remove_on_kicked, true}]),
         T(<<"remove_on_kicked">>, true)),
    ?eqf(modopts(mod_inbox, [{remove_on_kicked, false}]),
         T(<<"remove_on_kicked">>, false)),
    ?errf(T(<<"reset_markers">>, 1)),
    ?errf(T(<<"reset_markers">>, <<"test">>)),
    ?errf(T(<<"reset_markers">>, [<<"test">>])),
    ?errf(T(<<"groupchat">>, [<<"test">>])),
    ?errf(T(<<"groupchat">>, <<"test">>)),
    ?errf(T(<<"groupchat">>, true)),
    ?errf(T(<<"aff_changes">>, 1)),
    ?errf(T(<<"aff_changes">>, <<"true">>)),
    ?errf(T(<<"remove_on_kicked">>, 1)),
    ?errf(T(<<"remove_on_kicked">>, <<"true">>)),
    check_iqdisc(mod_inbox).

mod_global_distrib(_Config) ->
    ConnOpts = [
              {advertised_endpoints, [{"172.16.0.1", 5555}, {"localhost", 80}, {"example.com", 5555}]},
              {connections_per_endpoint, 22},
              {disabled_gc_interval, 60},
              {endpoint_refresh_interval, 120},
              {endpoint_refresh_interval_when_empty, 5},
              {endpoints, [{"172.16.0.2", 5555}, {"localhost", 80}, {"example.com", 5555}]},
              {tls_opts, [
                    {cafile, "/dev/null"},
                    {certfile, "/dev/null"},
                    {ciphers, "TLS_AES_256_GCM_SHA384"},
                    {dhfile, "/dev/null"}
                   ]}
             ],
    CacheOpts = [ {cache_missed, false}, {domain_lifetime_seconds, 60},
                  {jid_lifetime_seconds, 30}, {max_jids, 9999} ],
    BounceOpts = [ {max_retries, 3}, {resend_after_ms, 300} ],
    RedisOpts = [ {expire_after, 120}, {pool, global_distrib}, {refresh_after, 60} ],
    TTOpts = #{
          <<"enabled">> => true,
          <<"certfile">> => <<"/dev/null">>,
          <<"cacertfile">> => <<"/dev/null">>,
          <<"dhfile">> => <<"/dev/null">>,
          <<"ciphers">> => <<"TLS_AES_256_GCM_SHA384">>
         },
    TConnOpts = #{
      <<"endpoints">> => [#{<<"host">> => <<"172.16.0.2">>, <<"port">> => 5555},
                          #{<<"host">> => <<"localhost">>, <<"port">> => 80},
                          #{<<"host">> => <<"example.com">>, <<"port">> => 5555}],
      <<"advertised_endpoints">> =>
                         [#{<<"host">> => <<"172.16.0.1">>, <<"port">> => 5555},
                          #{<<"host">> => <<"localhost">>, <<"port">> => 80},
                          #{<<"host">> => <<"example.com">>, <<"port">> => 5555}],
      <<"connections_per_endpoint">> => 22,
      <<"disabled_gc_interval">> => 60,
      <<"endpoint_refresh_interval">> => 120,
      <<"endpoint_refresh_interval_when_empty">> => 5,
      <<"tls">> => TTOpts
     },
    TCacheOpts = #{ <<"cache_missed">> => false,
                    <<"domain_lifetime_seconds">> => 60,
                    <<"jid_lifetime_seconds">> => 30,
                    <<"max_jids">> => 9999 },
    TBounceOpts = #{ <<"resend_after_ms">> => 300, <<"max_retries">> => 3 },
    TRedisOpts = #{ <<"pool">> => <<"global_distrib">>,
                    <<"expire_after">> => 120,
                    <<"refresh_after">> => 60 },
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_global_distrib">> => Opts}} end,
    Base = #{
           <<"global_host">> => <<"example.com">>,
           <<"local_host">> => <<"datacenter1.example.com">>,
           <<"message_ttl">> => 42,
           <<"hosts_refresh_interval">> => 100,
           <<"connections">> => TConnOpts,
           <<"cache">> => TCacheOpts,
           <<"bounce">> => TBounceOpts,
           <<"redis">> => TRedisOpts
          },
    MBase = [
        {bounce, BounceOpts},
        {cache, CacheOpts},
        {connections, ConnOpts},
        {global_host, "example.com"},
        {hosts_refresh_interval, 100},
        {local_host, "datacenter1.example.com"},
        {message_ttl, 42},
        {redis, RedisOpts}
       ],
    ?eqf(modopts(mod_global_distrib, [
        {bounce, BounceOpts},
        {cache, CacheOpts},
        {connections, ConnOpts},
        {global_host, "example.com"},
        {hosts_refresh_interval, 100},
        {local_host, "datacenter1.example.com"},
        {message_ttl, 42},
        {redis, RedisOpts}
       ]), T(Base)),
    ?eqf(modopts(mod_global_distrib,
                 set_pl(connections,
                        set_pl(advertised_endpoints, false, ConnOpts),
                        MBase)),
         T(Base#{<<"connections">> => TConnOpts#{
        <<"advertised_endpoints">> => false}})),
    ?eqf(modopts(mod_global_distrib,
                 set_pl(connections,
                        set_pl(tls_opts, false, ConnOpts),
                        MBase)),
         T(Base#{<<"connections">> => TConnOpts#{<<"tls">> => #{<<"enabled">> => false}}})),
    ?eqf(modopts(mod_global_distrib,
                 set_pl(connections,
                        set_pl(tls_opts, false, ConnOpts),
                        MBase)),
         T(Base#{<<"connections">> => TConnOpts#{<<"tls">> => #{<<"enabled">> => false}}})),
    %% Connection opts
    ?errf(T(Base#{<<"connections">> => TConnOpts#{
            <<"tls">> =>TTOpts#{<<"certfile">> => <<"/this/does/not/exist">>}}})),
    ?errf(T(Base#{<<"connections">> => TConnOpts#{
            <<"tls">> =>TTOpts#{<<"dhfile">> => <<"/this/does/not/exist">>}}})),
    ?errf(T(Base#{<<"connections">> => TConnOpts#{
            <<"tls">> =>TTOpts#{<<"cacertfile">> => <<"/this/does/not/exist">>}}})),
    ?errf(T(Base#{<<"connections">> => TConnOpts#{
            <<"tls">> =>TTOpts#{<<"ciphers">> => 42}}})),
    ?errf(T(Base#{<<"connections">> => TConnOpts#{
        <<"endpoints">> =>[#{<<"host">> => 234, <<"port">> => 5555}]}})),
    ?errf(T(Base#{<<"connections">> => TConnOpts#{
        <<"advertised_endpoints">> =>[#{<<"host">> => 234, <<"port">> => 5555}]}})),
    ?errf(T(Base#{<<"connections">> => TConnOpts#{
        <<"connections_per_endpoint">> => -1}})),
    ?errf(T(Base#{<<"connections">> => TConnOpts#{
        <<"connections_per_endpoint">> => <<"kek">>}})),
    ?errf(T(Base#{<<"connections">> => TConnOpts#{<<"connections_per_endpoint">> => -1}})),
    ?errf(T(Base#{<<"connections">> => TConnOpts#{<<"disabled_gc_interval">> => -1}})),
    ?errf(T(Base#{<<"connections">> => TConnOpts#{<<"endpoint_refresh_interval">> => -1}})),
    ?errf(T(Base#{<<"connections">> => TConnOpts#{<<"endpoint_refresh_interval_when_empty">> => -1}})),
    %% Redis Opts
    ?errf(T(Base#{<<"redis">> => TRedisOpts#{<<"pool">> => -1}})),
    ?errf(T(Base#{<<"redis">> => TRedisOpts#{<<"expire_after">> => -1}})),
    ?errf(T(Base#{<<"redis">> => TRedisOpts#{<<"refresh_after">> => -1}})),
    %% Cache Opts
    ?errf(T(Base#{<<"cache">> => TCacheOpts#{<<"cache_missed">> => 1}})),
    ?errf(T(Base#{<<"cache">> => TCacheOpts#{<<"domain_lifetime_seconds">> => -1}})),
    ?errf(T(Base#{<<"cache">> => TCacheOpts#{<<"jid_lifetime_seconds">> => -1}})),
    ?errf(T(Base#{<<"cache">> => TCacheOpts#{<<"max_jids">> => -1}})),
    %% Bouncing Opts
    ?errf(T(Base#{<<"bounce">> => TCacheOpts#{<<"resend_after_ms">> => -1}})),
    ?errf(T(Base#{<<"bounce">> => TCacheOpts#{<<"max_retries">> => -1}})),
    %% Global Opts
    ?errf(T(Base#{<<"global_host">> => <<"example omm omm omm">>})),
    ?errf(T(Base#{<<"global_host">> => 1})),
    ?errf(T(Base#{<<"local_host">> => <<"example omm omm omm">>})),
    ?errf(T(Base#{<<"local_host">> => 1})),
    ?errf(T(Base#{<<"message_ttl">> => <<"kek">>})),
    ?errf(T(Base#{<<"message_ttl">> => -1})),
    ?errf(T(Base#{<<"hosts_refresh_interval">> => <<"kek">>})),
    ?errf(T(Base#{<<"hosts_refresh_interval">> => -1})).

mod_event_pusher_sns(_Config) ->
    RequiredOpts = #{<<"access_key_id">> => <<"AKIAIOSFODNN7EXAMPLE">>,
                     <<"secret_access_key">> => <<"KEY">>,
                     <<"region">> => <<"eu-west-1">>,
                     <<"account_id">> => <<"123456789012">>,
                     <<"sns_host">> => <<"sns.eu-west-1.amazonaws.com">>},
    ExpectedCfg = [{access_key_id, "AKIAIOSFODNN7EXAMPLE"},
                   {secret_access_key, "KEY"},
                   {region, "eu-west-1"},
                   {account_id, "123456789012"},
                   {sns_host, "sns.eu-west-1.amazonaws.com"}],
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_event_pusher">> =>
                                 #{<<"backend">> => #{<<"sns">> => Opts}}}}
        end,
    M = fun(Cfg) -> modopts(mod_event_pusher, [{backends, [{sns, Cfg}]}]) end,
    ?eqf(M(ExpectedCfg),
         T(RequiredOpts)),
    ?eqf(M(ExpectedCfg ++ [{presence_updates_topic, "pres"}]),
         T(RequiredOpts#{<<"presence_updates_topic">> => <<"pres">>})),
    ?eqf(M(ExpectedCfg ++ [{pm_messages_topic, "pm"}]),
         T(RequiredOpts#{<<"pm_messages_topic">> => <<"pm">>})),
    ?eqf(M(ExpectedCfg ++ [{muc_messages_topic, "muc"}]),
         T(RequiredOpts#{<<"muc_messages_topic">> => <<"muc">>})),
    ?eqf(M(ExpectedCfg ++ [{plugin_module, mod_event_pusher_sns_defaults}]),
         T(RequiredOpts#{<<"plugin_module">> => <<"mod_event_pusher_sns_defaults">>})),
    ?eqf(M(ExpectedCfg ++ [{pool_size, 10}]),
         T(RequiredOpts#{<<"pool_size">> => 10})),
    ?eqf(M(ExpectedCfg ++ [{publish_retry_count, 1}]),
         T(RequiredOpts#{<<"publish_retry_count">> => 1})),
    ?eqf(M(ExpectedCfg ++ [{publish_retry_time_ms, 100}]),
         T(RequiredOpts#{<<"publish_retry_time_ms">> => 100})),
    [?errf(T(maps:remove(Key, RequiredOpts))) || Key <- maps:keys(RequiredOpts)],
    [?errf(T(RequiredOpts#{Key => 1})) || Key <- maps:keys(RequiredOpts)],
    ?errf(T(RequiredOpts#{<<"presence_updates_topic">> => #{}})),
    ?errf(T(RequiredOpts#{<<"pm_messages_topic">> => true})),
    ?errf(T(RequiredOpts#{<<"muc_messages_topic">> => [1, 2]})),
    ?errf(T(RequiredOpts#{<<"plugin_module">> => <<"plug_and_play">>})),
    ?errf(T(RequiredOpts#{<<"pool_size">> => 0})),
    ?errf(T(RequiredOpts#{<<"publish_retry_count">> => -1})),
    ?errf(T(RequiredOpts#{<<"publish_retry_time_ms">> => -1})).

mod_event_pusher_push(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_event_pusher">> =>
                                 #{<<"backend">> => #{<<"push">> => Opts}}}}
        end,
    M = fun(Cfg) -> modopts(mod_event_pusher, [{backends, [{push, Cfg}]}]) end,
    ?eqf(M([{backend, rdbms}]),
         T(#{<<"backend">> => <<"rdbms">>})),
    ?eqf(M([{wpool, [{workers, 200}]}]),
         T(#{<<"wpool">> => #{<<"workers">> => 200}})),
    ?eqf(M([{plugin_module, mod_event_pusher_push_plugin_defaults}]),
         T(#{<<"plugin_module">> => <<"mod_event_pusher_push_plugin_defaults">>})),
    ?eqf(M([{virtual_pubsub_hosts, ["host1", "host2"]}]),
         T(#{<<"virtual_pubsub_hosts">> => [<<"host1">>, <<"host2">>]})),
    ?errf(T(#{<<"backend">> => <<"redis">>})),
    ?errf(T(#{<<"wpool">> => true})),
    ?errf(T(#{<<"wpool">> => #{<<"workers">> => <<"500">>}})),
    ?errf(T(#{<<"plugin_module">> => <<"wow_cool_but_missing">>})),
    ?errf(T(#{<<"plugin_module">> => 1})),
    ?errf(T(#{<<"virtual_pubsub_hosts">> => [<<"host with whitespace">>]})).

mod_event_pusher_http(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_event_pusher">> =>
                                 #{<<"backend">> => #{<<"http">> => Opts}}}}
        end,
    M = fun(Cfg) -> modopts(mod_event_pusher, [{backends, [{http, Cfg}]}]) end,
    ?eqf(M([{pool_name, http_pool}]),
         T(#{<<"pool_name">> => <<"http_pool">>})),
    ?eqf(M([{path, "/notifications"}]),
         T(#{<<"path">> => <<"/notifications">>})),
    ?eqf(M([{callback_module, mod_event_pusher_http_defaults}]),
         T(#{<<"callback_module">> => <<"mod_event_pusher_http_defaults">>})),
    ?errf(T(#{<<"pool_name">> => <<>>})),
    ?errf(T(#{<<"path">> => true})),
    ?errf(T(#{<<"callback_module">> => <<"wow_cool_but_missing">>})),
    ?errf(T(#{<<"callback_module">> => 1})).

mod_event_pusher_rabbit(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_event_pusher">> =>
                                 #{<<"backend">> => #{<<"rabbit">> => Opts}}}}
        end,
    M = fun(Cfg) -> modopts(mod_event_pusher, [{backends, [{rabbit, Cfg}]}]) end,
    ?eqf(M([{presence_exchange, [{name, <<"pres">>}]}]),
         T(#{<<"presence_exchange">> => #{<<"name">> => <<"pres">>}})),
    ?eqf(M([{presence_exchange, [{type, <<"topic">>}]}]),
         T(#{<<"presence_exchange">> => #{<<"type">> => <<"topic">>}})),

    %% first two keys are the same as before, test them together
    ?eqf(M([{chat_msg_exchange, [{name, <<"pres1">>},
                                 {type, <<"topic1">>}]}]),
         T(#{<<"chat_msg_exchange">> => #{<<"name">> => <<"pres1">>,
                                          <<"type">> => <<"topic1">>}})),
    ?eqf(M([{chat_msg_exchange, [{sent_topic, <<"sent_topic1">>}]}]),
         T(#{<<"chat_msg_exchange">> => #{<<"sent_topic">> => <<"sent_topic1">>}})),
    ?eqf(M([{chat_msg_exchange, [{recv_topic, <<"recv_topic1">>}]}]),
         T(#{<<"chat_msg_exchange">> => #{<<"recv_topic">> => <<"recv_topic1">>}})),

    %% all keys are the same as before, test them together
    ?eqf(M([{groupchat_msg_exchange, [{name, <<"pres2">>},
                                      {type, <<"topic2">>},
                                      {sent_topic, <<"sent_topic2">>},
                                      {recv_topic, <<"recv_topic2">>}]}]),
         T(#{<<"groupchat_msg_exchange">> => #{<<"name">> => <<"pres2">>,
                                               <<"type">> => <<"topic2">>,
                                               <<"sent_topic">> => <<"sent_topic2">>,
                                               <<"recv_topic">> => <<"recv_topic2">>}})),

    Exchanges = [<<"presence_exchange">>, <<"chat_msg_exchange">>, <<"groupchat_msg_exchange">>],
    Keys = [<<"name">>, <<"topic">>, <<"sent_topic">>, <<"recv_topic">>],
    [?errf(T(#{Exch => #{Key => <<>>}})) || Exch <- Exchanges, Key <- Keys],
    [?errf(T(#{Exch => #{<<"badkey">> => <<"goodvalue">>}})) || Exch <- Exchanges],
    ?errf(T(#{<<"money_exchange">> => #{<<"name">> => <<"kantor">>}})).

mod_http_upload(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_http_upload">> => Opts}} end,
    S3 = #{
      <<"bucket_url">> => <<"https://s3-eu-west-1.amazonaws.com/mybucket">>,
      <<"add_acl">> => true,
      <<"region">> => <<"antarctica-1">>,
      <<"access_key_id">> => <<"PLEASE">>,
      <<"secret_access_key">> => <<"ILOVEU">>
     },
    Base = #{
           <<"iqdisc">> => #{<<"type">> => <<"one_queue">>},
           <<"host">> => <<"upload.@HOST@">>,
           <<"backend">> => <<"s3">>,
           <<"expiration_time">> => 666,
           <<"token_bytes">> => 32,
           <<"max_file_size">> => 42,
           <<"s3">> => S3
          },
    MS3 = [{access_key_id, "PLEASE"},
           {add_acl, true},
           {bucket_url, "https://s3-eu-west-1.amazonaws.com/mybucket"},
           {region, "antarctica-1"},
           {secret_access_key, "ILOVEU"}],
    MBase = [{backend, s3},
             {expiration_time, 666},
             {host, "upload.@HOST@"},
             {iqdisc, one_queue},
             {max_file_size, 42},
             {s3, MS3},
             {token_bytes, 32}],
    ?eqf(modopts(mod_http_upload, MBase), T(Base)),
    ?errf(T(Base#{<<"host">> => -1})),
    ?errf(T(Base#{<<"host">> => <<" f g ">>})),
    ?errf(T(Base#{<<"backend">> => <<"dev_null_as_a_service">>})),
    ?errf(T(Base#{<<"expiration_time">> => <<>>})),
    ?errf(T(Base#{<<"expiration_time">> => -1})),
    ?errf(T(Base#{<<"token_bytes">> => -1})),
    ?errf(T(Base#{<<"max_file_size">> => -1})),
    ?errf(T(Base#{<<"s3">> => S3#{<<"access_key_id">> => -1}})),
    ?errf(T(Base#{<<"s3">> => S3#{<<"add_acl">> => -1}})),
    ?errf(T(Base#{<<"s3">> => S3#{<<"bucket_url">> => -1}})),
    ?errf(T(Base#{<<"s3">> => S3#{<<"region">> => -1}})),
    ?errf(T(Base#{<<"s3">> => S3#{<<"secret_access_key">> => -1}})),
    check_iqdisc(mod_http_upload).

mod_jingle_sip(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_jingle_sip">> => Opts}} end,
    Base = #{
      <<"proxy_host">> => <<"proxxxy">>,
      <<"proxy_port">> => 5600,
      <<"listen_port">> => 5601,
      <<"local_host">> => <<"localhost">>,
      <<"sdp_origin">> => <<"127.0.0.1">>
     },
    MBase = [
      {listen_port, 5601},
      {local_host, "localhost"},
      {proxy_host, "proxxxy"},
      {proxy_port, 5600},
      {sdp_origin, "127.0.0.1"}
     ],
    ?eqf(modopts(mod_jingle_sip, MBase), T(Base)),
    ?errf(T(Base#{<<"proxy_host">> => -1})),
    ?errf(T(Base#{<<"proxy_host">> => <<"test test">>})),
    ?errf(T(Base#{<<"listen_port">> => -1})),
    ?errf(T(Base#{<<"listen_port">> => 10000000})),
    ?errf(T(Base#{<<"proxy_port">> => -1})),
    ?errf(T(Base#{<<"proxy_port">> => 10000000})),
    ?errf(T(Base#{<<"local_host">> => 1})),
    ?errf(T(Base#{<<"local_host">> => <<"ok ok">>})),
    ?errf(T(Base#{<<"sdp_origin">> => <<"aaaaaaaaa">>})).

mod_keystore(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_keystore">> => Opts}} end,
    Keys = [#{<<"name">> => <<"access_secret">>,
              <<"type">> => <<"ram">>},
            #{<<"name">> => <<"access_psk">>,
              <<"type">> => <<"file">>,
              <<"path">> => <<"priv/access_psk">>},
            #{<<"name">> => <<"provision_psk">>,
              <<"type">> => <<"file">>,
              <<"path">> => <<"priv/provision_psk">>}],
    NotExistingKey = #{<<"name">> => <<"provision_psk">>,
                       <<"type">> => <<"file">>,
                       <<"path">> => <<"does/not/esit">>},
    InvalidTypeKey = #{<<"name">> => <<"provision_psk">>,
                       <<"type">> => <<"some_cooool_type">>},
    MKeys = [{access_secret, ram},
             {access_psk,    {file, "priv/access_psk"}},
             {provision_psk, {file, "priv/provision_psk"}}],
    Base = #{<<"keys">> => Keys, <<"ram_key_size">> => 10000},
    MBase = [{keys, MKeys}, {ram_key_size, 10000}],
    ?eqf(modopts(mod_keystore, MBase), T(Base)),
    ?errf(T(Base#{<<"keys">> => [NotExistingKey]})),
    ?errf(T(Base#{<<"keys">> => [InvalidTypeKey]})).

mod_last(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_last">> => Opts}} end,
    Base = #{<<"iqdisc">> => #{<<"type">> => <<"one_queue">>},
             <<"backend">> => <<"riak">>,
             <<"riak">> => #{<<"bucket_type">> => <<"test">>}},
    MBase = [{backend, riak},
             {bucket_type, <<"test">>},
             {iqdisc, one_queue}],
    ?eqf(modopts(mod_last, MBase), T(Base)),
    ?errf(T(Base#{<<"backend">> => <<"riak_is_the_best">>})),
    ?errf(T(Base#{<<"riak">> => #{<<"bucket_type">> => 1}})),
    check_iqdisc(mod_last).

mod_mam_meta(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_mam_meta">> => Opts}} end,
    %% You can define options in mod_mam and they would work.
    %%
    %% We _could_ validate that `host' option does not exist in the PM
    %% section, but it would just make everything harder.
    %%
    %% Same with `no_stanzaid_element' for PM.
    Common = #{<<"archive_chat_markers">> => true,
               <<"archive_groupchats">> => true,
               <<"async_writer">> => true,
               <<"async_writer_rdbms_pool">> => <<"poop">>,
               <<"backend">> => <<"riak">>,
               <<"cache_users">> => true,
               <<"db_jid_format">> => <<"mam_jid_rfc">>, % module
               <<"db_message_format">> => <<"mam_message_xml">>, % module
               <<"default_result_limit">> => 50,
               <<"extra_lookup_params">> => <<"mod_mam_utils">>,
               <<"host">> => <<"conf.localhost">>,
               <<"flush_interval">> => 500,
               <<"full_text_search">> => true,
               <<"max_batch_size">> => 50,
               <<"max_result_limit">> => 50,
               <<"message_retraction">> => true,
               <<"rdbms_message_format">> => <<"simple">>,
               <<"simple">> => true,
               <<"no_stanzaid_element">> => true,
               <<"is_archivable_message">> => <<"mod_mam_utils">>,
               <<"user_prefs_store">> => false}, %% or rdbms. but not true
    MCommon = [{archive_chat_markers, true},
               {archive_groupchats, true},
               {async_writer, true},
               {async_writer_rdbms_pool, poop},
               {backend, riak},
               {cache_users, true},
               {db_jid_format, mam_jid_rfc},
               {db_message_format, mam_message_xml},
               {default_result_limit, 50},
               {extra_lookup_params, mod_mam_utils},
               {flush_interval, 500},
               {full_text_search, true},
               %% While applied just for MUC, it could be specified as root option
               %% Still, it probably should've been called muc_host from the
               %% beginning
               {host, "conf.localhost"},
               {is_archivable_message, mod_mam_utils},
               {max_batch_size, 50},
               {max_result_limit, 50},
               {message_retraction, true},
               {no_stanzaid_element, true},
               {rdbms_message_format, simple},
               {simple, true},
               {user_prefs_store, false}],
    ensure_sorted(MCommon),
    Riak = #{<<"bucket_type">> => <<"mam_yz">>, <<"search_index">> => <<"mam">>},
    MRiak = [{bucket_type, <<"mam_yz">>}, {search_index, <<"mam">>}],
    Base = Common#{
             <<"pm">> => Common,
             <<"muc">> => Common,
             %% Separate section for riak. We don't need it in pm or in muc,
             %% because there is no separate riak module for muc.
             <<"riak">> => Riak},
    MBase0 = [{muc, MCommon},
              {pm, MCommon}]
              ++ MRiak, %% This one is flatten into mim opts
    MBase = pl_merge(MCommon, MBase0),
    %% It's not easy to test riak options with check_one_opts function,
    %% so skip it.
    %% We also skip single muc/pm options on this step.
    KeysForOneOpts = binaries_to_atoms(maps:keys(Common)),
    TPM = fun(Map) -> T(#{<<"pm">> => Map}) end,
    TMuc = fun(Map) -> T(#{<<"muc">> => Map}) end,
    TB = fun(Map) -> T(maps:merge(Base, Map)) end,
    %% by default parser adds pm and muc keys set to false
    Hook = fun(Mim, Toml) -> {lists:sort([{pm, false}, {muc, false}|Mim]), Toml} end,
    run_multi(
      %% Test configurations with one option only
      check_one_opts(mod_mam_meta, MBase, Base, T, KeysForOneOpts, Hook) ++ [
        ?_eqf(modopts(mod_mam_meta, [{muc, false}, {pm, false}]), T(#{})),
        ?_eqf(modopts(mod_mam_meta, MBase), T(Base)),
        %% Second format for user_prefs_store
        ?_eqf(modopts(mod_mam_meta, pl_merge(MBase, [{user_prefs_store, rdbms}])),
            T(Base#{<<"user_prefs_store">> => <<"rdbms">>}))
        ]
      ++ mam_failing_cases(T)
      ++ mam_failing_cases(TPM)
      ++ mam_failing_cases(TMuc)
      ++ mam_failing_cases(TB)
      ++ mam_failing_riak_cases(T)
     ).

mam_failing_cases(T) ->
    [?_errf(T(#{<<"pm">> => false})), % should be a section
     ?_errf(T(#{<<"muc">> => false})), % should be a section
     ?_errf(T(#{<<"archive_chat_markers">> => 1})),
     ?_errf(T(#{<<"archive_groupchats">> => 1})),
     ?_errf(T(#{<<"async_writer">> => 1})),
     ?_errf(T(#{<<"async_writer_rdbms_pool">> => 1})),
     ?_errf(T(#{<<"backend">> => 1})),
     ?_errf(T(#{<<"cache_users">> => 1})),
     ?_errf(T(#{<<"db_jid_format">> => 1})),
     ?_errf(T(#{<<"db_jid_format">> => <<"does_not_exist_mod">>})),
     ?_errf(T(#{<<"db_message_format">> => 1})),
     ?_errf(T(#{<<"db_message_format">> => <<"does_not_exist_mod">>})),
     ?_errf(T(#{<<"default_result_limit">> => <<"meow">>})),
     ?_errf(T(#{<<"default_result_limit">> => -20})),
     ?_errf(T(#{<<"extra_lookup_params">> => -1})),
     ?_errf(T(#{<<"extra_lookup_params">> => <<"aaaaaaa_not_exist">>})),
     ?_errf(T(#{<<"host">> => <<"meow meow">>})),
     ?_errf(T(#{<<"flush_interval">> => <<"meow">>})),
     ?_errf(T(#{<<"flush_interval">> => -20})),
     ?_errf(T(#{<<"full_text_search">> => -1})),
     ?_errf(T(#{<<"max_batch_size">> => -1})),
     ?_errf(T(#{<<"max_result_limit">> => -1})),
     ?_errf(T(#{<<"message_retraction">> => -1})),
     ?_errf(T(#{<<"rdbms_message_format">> => <<"verysimple">>})),
     ?_errf(T(#{<<"simple">> => 1})),
     ?_errf(T(#{<<"no_stanzaid_element">> => -1})),
     ?_errf(T(#{<<"is_archivable_message">> => -1})),
     ?_errf(T(#{<<"user_prefs_store">> => 1}))].

mam_failing_riak_cases(T) ->
    [?_errf(T(#{<<"riak">> => #{<<"bucket_type">> => 1}})),
     ?_errf(T(#{<<"riak">> => #{<<"search_index">> => 1}}))].

mod_muc(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_muc">> => Opts}} end,
    M = fun(Cfg) -> modopts(mod_muc, Cfg) end,
    ?eqf(M([{host, "conference.@HOST@"}]),
         T(#{<<"host">> => <<"conference.@HOST@">>})),
    ?eqf(M([{backend, mnesia}]),
         T(#{<<"backend">> => <<"mnesia">>})),
    ?eqf(M([{access, all}]),
         T(#{<<"access">> => <<"all">>})),
    ?eqf(M([{access_create, admin}]),
         T(#{<<"access_create">> => <<"admin">>})),
    ?eqf(M([{access_admin, none}]),
         T(#{<<"access_admin">> => <<"none">>})),
    ?eqf(M([{access_persistent, all}]),
         T(#{<<"access_persistent">> => <<"all">>})),
    ?eqf(M([{history_size, 20}]),
         T(#{<<"history_size">> => 20})),
    ?eqf(M([{room_shaper, muc_room_shaper}]),
         T(#{<<"room_shaper">> => <<"muc_room_shaper">>})),
    ?eqf(M([{max_room_id, infinity}]),
         T(#{<<"max_room_id">> => <<"infinity">>})),
    ?eqf(M([{max_room_name, 30}]),
         T(#{<<"max_room_name">> => 30})),
    ?eqf(M([{max_room_desc, 0}]),
         T(#{<<"max_room_desc">> => 0})),
    ?eqf(M([{min_message_interval, 10}]),
         T(#{<<"min_message_interval">> => 10})),
    ?eqf(M([{min_presence_interval, 0}]),
         T(#{<<"min_presence_interval">> => 0})),
    ?eqf(M([{max_users, 30}]),
         T(#{<<"max_users">> => 30})),
    ?eqf(M([{max_users_admin_threshold, 2}]),
         T(#{<<"max_users_admin_threshold">> => 2})),
    ?eqf(M([{user_message_shaper, muc_msg_shaper}]),
         T(#{<<"user_message_shaper">> => <<"muc_msg_shaper">>})),
    ?eqf(M([{user_presence_shaper, muc_pres_shaper}]),
         T(#{<<"user_presence_shaper">> => <<"muc_pres_shaper">>})),
    ?eqf(M([{max_user_conferences, 10}]),
         T(#{<<"max_user_conferences">> => 10})),
    ?eqf(M([{http_auth_pool, external_auth}]),
         T(#{<<"http_auth_pool">> => <<"external_auth">>})),
    ?eqf(M([{load_permanent_rooms_at_startup, true}]),
         T(#{<<"load_permanent_rooms_at_startup">> => true})),
    ?eqf(M([{hibernate_timeout, infinity}]),
         T(#{<<"hibernate_timeout">> => <<"infinity">>})),
    ?eqf(M([{hibernated_room_check_interval, 5000}]),
         T(#{<<"hibernated_room_check_interval">> => 5000})),
    ?eqf(M([{hibernated_room_timeout, 0}]),
         T(#{<<"hibernated_room_timeout">> => 0})),
    ?errf(T(#{<<"host">> => <<>>})),
    ?errf(T(#{<<"backend">> => <<"amnesia">>})),
    ?errf(T(#{<<"access">> => <<>>})),
    ?errf(T(#{<<"access_create">> => 1})),
    ?errf(T(#{<<"access_admin">> => []})),
    ?errf(T(#{<<"access_persistent">> => true})),
    ?errf(T(#{<<"history_size">> => <<"20">>})),
    ?errf(T(#{<<"room_shaper">> => <<>>})),
    ?errf(T(#{<<"max_room_id">> => #{}})),
    ?errf(T(#{<<"max_room_name">> => <<"infinite!">>})),
    ?errf(T(#{<<"max_room_desc">> => -1})),
    ?errf(T(#{<<"min_message_interval">> => -10})),
    ?errf(T(#{<<"min_presence_interval">> => <<"infinity">>})),
    ?errf(T(#{<<"max_users">> => 0})),
    ?errf(T(#{<<"max_users_admin_threshold">> => 0})),
    ?errf(T(#{<<"user_message_shaper">> => []})),
    ?errf(T(#{<<"user_presence_shaper">> => <<>>})),
    ?errf(T(#{<<"max_user_conferences">> => -1})),
    ?errf(T(#{<<"http_auth_pool">> => <<>>})),
    ?errf(T(#{<<"load_permanent_rooms_at_startup">> => <<"true">>})),
    ?errf(T(#{<<"hibernate_timeout">> => <<"really big">>})),
    ?errf(T(#{<<"hibernated_room_check_interval">> => -1})),
    ?errf(T(#{<<"hibernated_room_timeout">> => false})).

mod_muc_default_room(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_muc">> => #{<<"default_room">> => Opts}}} end,
    M = fun(Cfg) -> modopts(mod_muc, [{default_room_options, Cfg}]) end,
    ?eqf(M([]), T(#{})),
    ?eqf(M([{title, <<"living room">>}]),
         T(#{<<"title">> => <<"living room">>})),
    ?eqf(M([{description, <<"a room that is alive">>}]),
         T(#{<<"description">> => <<"a room that is alive">>})),
    ?eqf(M([{allow_change_subj, true}]),
         T(#{<<"allow_change_subj">> => true})),
    ?eqf(M([{allow_query_users, false}]),
         T(#{<<"allow_query_users">> => false})),
    ?eqf(M([{allow_private_messages, true}]),
         T(#{<<"allow_private_messages">> => true})),
    ?eqf(M([{allow_visitor_status, false}]),
         T(#{<<"allow_visitor_status">> => false})),
    ?eqf(M([{allow_visitor_nickchange, true}]),
         T(#{<<"allow_visitor_nickchange">> => true})),
    ?eqf(M([{public, false}]),
         T(#{<<"public">> => false})),
    ?eqf(M([{public_list, true}]),
         T(#{<<"public_list">> => true})),
    ?eqf(M([{persistent, true}]),
         T(#{<<"persistent">> => true})),
    ?eqf(M([{moderated, false}]),
         T(#{<<"moderated">> => false})),
    ?eqf(M([{members_by_default, true}]),
         T(#{<<"members_by_default">> => true})),
    ?eqf(M([{members_only, false}]),
         T(#{<<"members_only">> => false})),
    ?eqf(M([{allow_user_invites, true}]),
         T(#{<<"allow_user_invites">> => true})),
    ?eqf(M([{allow_multiple_sessions, false}]),
         T(#{<<"allow_multiple_sessions">> => false})),
    ?eqf(M([{password_protected, true}]),
         T(#{<<"password_protected">> => true})),
    ?eqf(M([{password, <<"secret">>}]),
         T(#{<<"password">> => <<"secret">>})),
    ?eqf(M([{anonymous, true}]),
         T(#{<<"anonymous">> => true})),
    ?eqf(M([{max_users, 100}]),
         T(#{<<"max_users">> => 100})),
    ?eqf(M([{logging, false}]),
         T(#{<<"logging">> => false})),
    ?eqf(M([{maygetmemberlist, [moderator]}]),
         T(#{<<"maygetmemberlist">> => [<<"moderator">>]})),
    ?eqf(M([{subject, <<"Lambda days">>}]),
         T(#{<<"subject">> => <<"Lambda days">>})),
    ?eqf(M([{subject_author, <<"Alice">>}]),
         T(#{<<"subject_author">> => <<"Alice">>})),
    ?errf(T(<<"bad value">>)),
    ?errf(T(#{<<"title">> => true})),
    ?errf(T(#{<<"description">> => 1})),
    ?errf(T(#{<<"allow_change_subj">> => <<"true">>})),
    ?errf(T(#{<<"allow_query_users">> => <<>>})),
    ?errf(T(#{<<"allow_private_messages">> => 1})),
    ?errf(T(#{<<"allow_visitor_status">> => []})),
    ?errf(T(#{<<"allow_visitor_nickchange">> => #{}})),
    ?errf(T(#{<<"public">> => 0})),
    ?errf(T(#{<<"public_list">> => [false]})),
    ?errf(T(#{<<"persistent">> => 1})),
    ?errf(T(#{<<"moderated">> => <<"yes">>})),
    ?errf(T(#{<<"members_by_default">> => 0})),
    ?errf(T(#{<<"members_only">> => [true]})),
    ?errf(T(#{<<"allow_user_invites">> => <<>>})),
    ?errf(T(#{<<"allow_multiple_sessions">> => []})),
    ?errf(T(#{<<"password_protected">> => #{}})),
    ?errf(T(#{<<"password">> => false})),
    ?errf(T(#{<<"anonymous">> => <<"maybe">>})),
    ?errf(T(#{<<"max_users">> => 0})),
    ?errf(T(#{<<"logging">> => [true, false]})),
    ?errf(T(#{<<"maygetmemberlist">> => <<"moderator">>})),
    ?errf(T(#{<<"maygetmemberlist">> => [<<>>]})),
    ?errf(T(#{<<"subject">> => [<<"subjective">>]})),
    ?errf(T(#{<<"subject_author">> => 1})).

mod_muc_default_room_affiliations(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_muc">> =>
                                 #{<<"default_room">> => #{<<"affiliations">> => Opts}}}} end,
    M = fun(Cfg) -> modopts(mod_muc, [{default_room_options, [{affiliations, Cfg}]}]) end,
    RequiredOpts = #{<<"user">> => <<"alice">>,
                     <<"server">> => <<"localhost">>,
                     <<"resource">> => <<"phone">>,
                     <<"affiliation">> => <<"moderator">>},
    ExpectedCfg = {{<<"alice">>, <<"localhost">>, <<"phone">>}, moderator},
    ?eqf(M([]), T([])),
    ?eqf(M([ExpectedCfg]), T([RequiredOpts])),
    [?errf(T([maps:remove(Key, RequiredOpts)])) || Key <- maps:keys(RequiredOpts)],
    ?errf(T([RequiredOpts#{<<"user">> := <<>>}])),
    ?errf(T([RequiredOpts#{<<"server">> := <<"domain? not really!">>}])),
    ?errf(T([RequiredOpts#{<<"resource">> := false}])),
    ?errf(T([RequiredOpts#{<<"affiliation">> := <<>>}])).

mod_muc_log(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_muc_log">> => Opts}} end,
    M = fun(Cfg) -> modopts(mod_muc_log, Cfg) end,
    ?eqf(M([{outdir, "www/muc"}]),
         T(#{<<"outdir">> => <<"www/muc">>})),
    ?eqf(M([{access_log, muc_admin}]),
         T(#{<<"access_log">> => <<"muc_admin">>})),
    ?eqf(M([{dirtype, subdirs}]),
         T(#{<<"dirtype">> => <<"subdirs">>})),
    ?eqf(M([{dirname, room_name}]),
         T(#{<<"dirname">> => <<"room_name">>})),
    ?eqf(M([{file_format, html}]),
         T(#{<<"file_format">> => <<"html">>})),
    ?eqf(M([{cssfile, <<"path/to/css_file">>}]),
         T(#{<<"css_file">> => <<"path/to/css_file">>})),
    ?eqf(M([{timezone, local}]),
         T(#{<<"timezone">> => <<"local">>})),
    ?eqf(M([{spam_prevention, false}]),
         T(#{<<"spam_prevention">> => false})),
    ?errf(T(#{<<"outdir">> => <<"does/not/exist">>})),
    ?errf(T(#{<<"access_log">> => 1})),
    ?errf(T(#{<<"dirtype">> => <<"imaginary">>})),
    ?errf(T(#{<<"dirname">> => <<"dyrektory">>})),
    ?errf(T(#{<<"file_format">> => <<"none">>})),
    ?errf(T(#{<<"css_file">> => <<>>})),
    ?errf(T(#{<<"timezone">> => <<"yes">>})),
    ?errf(T(#{<<"spam_prevention">> => <<"spam and eggs and spam">>})).

mod_muc_log_top_link(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_muc_log">> => #{<<"top_link">> => Opts}}} end,
    M = fun(Cfg) -> modopts(mod_muc_log, [{top_link, Cfg}]) end,
    RequiredOpts = #{<<"target">> => <<"https://esl.github.io/MongooseDocs/">>,
                     <<"text">> => <<"Docs">>},
    ExpectedCfg = {"https://esl.github.io/MongooseDocs/", "Docs"},
    ?eqf(M(ExpectedCfg), T(RequiredOpts)),
    [?errf(T(maps:remove(K, RequiredOpts))) || K <- maps:keys(RequiredOpts)],
    ?errf(T(RequiredOpts#{<<"target">> => true})),
    ?errf(T(RequiredOpts#{<<"text">> => <<"">>})).

mod_muc_light(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_muc_light">> => Opts}} end,
    M = fun(Cfg) -> modopts(mod_muc_light, Cfg) end,
    ?eqf(M([{backend, mnesia}]),
         T(#{<<"backend">> => <<"mnesia">>})),
    ?eqf(M([{host, "muclight.@HOST@"}]),
         T(#{<<"host">> => <<"muclight.@HOST@">>})),
    ?eqf(M([{equal_occupants, true}]),
         T(#{<<"equal_occupants">> => true})),
    ?eqf(M([{legacy_mode, false}]),
         T(#{<<"legacy_mode">> => false})),
    ?eqf(M([{rooms_per_user, 100}]),
         T(#{<<"rooms_per_user">> => 100})),
    ?eqf(M([{blocking, false}]),
         T(#{<<"blocking">> => false})),
    ?eqf(M([{all_can_configure, true}]),
         T(#{<<"all_can_configure">> => true})),
    ?eqf(M([{all_can_invite, false}]),
         T(#{<<"all_can_invite">> => false})),
    ?eqf(M([{max_occupants, infinity}]),
         T(#{<<"max_occupants">> => <<"infinity">>})),
    ?eqf(M([{rooms_per_page, 10}]),
         T(#{<<"rooms_per_page">> => 10})),
    ?eqf(M([{rooms_in_rosters, true}]),
         T(#{<<"rooms_in_rosters">> => true})),
    ?errf(T(#{<<"backend">> => <<"frontend">>})),
    ?errf(T(#{<<"host">> => <<"what is a domain?!">>})),
    ?errf(T(#{<<"equal_occupants">> => <<"true">>})),
    ?errf(T(#{<<"legacy_mode">> => 1234})),
    ?errf(T(#{<<"rooms_per_user">> => 0})),
    ?errf(T(#{<<"blocking">> => <<"true">>})),
    ?errf(T(#{<<"all_can_configure">> => []})),
    ?errf(T(#{<<"all_can_invite">> => #{}})),
    ?errf(T(#{<<"max_occupants">> => <<"seven">>})),
    ?errf(T(#{<<"rooms_per_page">> => false})),
    ?errf(T(#{<<"rooms_in_rosters">> => [1, 2, 3]})).

mod_muc_light_config_schema(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_muc_light">> => #{<<"config_schema">> => Opts}}} end,
    M = fun(Cfg) -> modopts(mod_muc_light, [{config_schema, Cfg}]) end,
    Field = #{<<"field">> => <<"my_field">>},
    ?eqf(M([]), T([])),
    ?eqf(M([{"my_field", <<"My Room">>, my_field, binary}]),
         T([Field#{<<"string_value">> => <<"My Room">>}])),
    ?eqf(M([{"my_field", 1, my_field, integer}]),
         T([Field#{<<"integer_value">> => 1}])),
    ?eqf(M([{"my_field", 0.5, my_field, float}]),
         T([Field#{<<"float_value">> => 0.5}])),
    ?eqf(M([{"my_field", 0, your_field, integer}]),
         T([Field#{<<"integer_value">> => 0,
                   <<"internal_key">> => <<"your_field">>}])),
    ?errf(T([#{<<"string_value">> => <<"My Room">>}])),
    ?errf(T([#{<<"field">> => <<>>,
               <<"string_value">> => <<"My Room">>}])),
    ?errf(T([Field#{<<"string_value">> => 0}])),
    ?errf(T([Field#{<<"integer_value">> => 1.5}])),
    ?errf(T([Field#{<<"float_value">> => 1}])),
    ?errf(T([Field#{<<"integer_value">> => 0,
                    <<"string_value">> => <<"My Room">>}])),
    ?errf(T([Field#{<<"integer_value">> => 0,
                    <<"internal_key">> => <<>>}])).

mod_offline(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_offline">> => Opts}} end,
    M = fun(Cfg) -> modopts(mod_offline, Cfg) end,
    ?eqf(M([{access_max_user_messages, max_user_offline_messages}]),
         T(#{<<"access_max_user_messages">> => <<"max_user_offline_messages">>})),
    ?eqf(M([{backend, rdbms}]),
         T(#{<<"backend">> => <<"rdbms">>})),
    ?eqf(M([{bucket_type, <<"test">>}]),
         T(#{<<"riak">> => #{<<"bucket_type">> => <<"test">>}})),
    ?errf(T(#{<<"access_max_user_messages">> => 1})),
    ?errf(T(#{<<"backend">> => <<"riak_is_the_best">>})),
    ?errf(T(#{<<"riak">> => #{<<"bucket_type">> => 1}})),
    ?errf(T(#{<<"riak">> => #{<<"bucket">> => <<"leaky">>}})).

mod_ping(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_ping">> => Opts}} end,
    M = fun(Cfg) -> modopts(mod_ping, Cfg) end,
    ?eqf(M([{send_pings, true}]),
         T(#{<<"send_pings">> => true})),
    ?eqf(M([{ping_interval, 10}]),
         T(#{<<"ping_interval">> => 10})),
    ?eqf(M([{timeout_action, kill}]),
         T(#{<<"timeout_action">> => <<"kill">>})),
    ?eqf(M([{ping_req_timeout, 20}]),
         T(#{<<"ping_req_timeout">> => 20})),
    ?errf(T(#{<<"send_pings">> => 1})),
    ?errf(T(#{<<"ping_interval">> => 0})),
    ?errf(T(#{<<"timeout_action">> => <<"kill_them_all">>})),
    ?errf(T(#{<<"ping_req_timeout">> => 0})),
    check_iqdisc(mod_ping).

mod_privacy(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_privacy">> => Opts}} end,
    Riak = #{<<"defaults_bucket_type">> => <<"privacy_defaults">>,
             <<"names_bucket_type">> => <<"privacy_lists_names">>,
             <<"bucket_type">> => <<"privacy_defaults">>},
    Base = #{<<"backend">> => <<"mnesia">>,
             <<"riak">> => Riak},
    MBase = [{backend, mnesia},
             %% Riak opts
             {defaults_bucket_type, <<"privacy_defaults">>},
             {names_bucket_type, <<"privacy_lists_names">>},
             {bucket_type, <<"privacy_defaults">>}],
    ?eqf(modopts(mod_privacy, lists:sort(MBase)), T(Base)),
    ?errf(T(Base#{<<"backend">> => 1})),
    ?errf(T(Base#{<<"backend">> => <<"mongoddt">>})),
    ?errf(T(Base#{<<"riak">> => #{<<"defaults_bucket_type">> => 1}})),
    ?errf(T(Base#{<<"riak">> => #{<<"names_bucket_type">> => 1}})),
    ?errf(T(Base#{<<"riak">> => #{<<"bucket_type">> => 1}})).

mod_private(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_private">> => Opts}} end,
    Riak = #{<<"bucket_type">> => <<"private_stuff">>},
    Base = #{<<"backend">> => <<"riak">>,
             <<"riak">> => Riak},
    MBase = [{backend, riak},
             %% Riak opts
             {bucket_type, <<"private_stuff">>}],
    ?eqf(modopts(mod_private, lists:sort(MBase)), T(Base)),
    ?errf(T(Base#{<<"backend">> => 1})),
    ?errf(T(Base#{<<"backend">> => <<"mongoddt">>})),
    ?errf(T(Base#{<<"riak">> => #{<<"bucket_type">> => 1}})),
    check_iqdisc(mod_private).

mod_pubsub(_Config) ->
    %% TODO default_node_config
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_pubsub">> => Opts}} end,
    Base = #{<<"backend">> => <<"mnesia">>,
             <<"host">> => <<"pubsub.@HOST@">>,
             <<"access_createnode">> => <<"all">>,
             <<"max_items_node">> => 10,
             <<"max_subscriptions_node">> => 10,
             <<"nodetree">> => <<"tree">>,
             <<"ignore_pep_from_offline">> => true,
             <<"last_item_cache">> => false,
             <<"plugins">> => [<<"flat">>],
             <<"pep_mapping">> => [#{<<"namespace">> => <<"urn:xmpp:microblog:0">>,
                                     <<"node">> => <<"mb">>}],
             <<"item_publisher">> => false,
             <<"sync_broadcast">> => true},
    MBase = [{backend, mnesia},
             {access_createnode, all},
             {host, "pubsub.@HOST@"},
             {max_items_node, 10},
             {max_subscriptions_node, 10},
             {nodetree, <<"tree">>},
             {ignore_pep_from_offline, true},
             {last_item_cache, false},
             {plugins, [<<"flat">>]},
             {pep_mapping, [{"urn:xmpp:microblog:0", "mb"}]},
             {item_publisher, false},
             {sync_broadcast, true}],
    ?eqf(modopts(mod_pubsub, lists:sort(MBase)), T(Base)),
    ?eqf(modopts(mod_pubsub, [{last_item_cache, mnesia}]),
                 T(#{<<"last_item_cache">> => <<"mnesia">>})),
    ?eqf(modopts(mod_pubsub, []), %% The option is undefined, i.e. parser just removes it
                 T(#{<<"max_subscriptions_node">> => <<"infinity">>})),
    run_multi(
        good_default_node_config_opts(T) ++
        bad_default_node_config_opts(T) ++
        generic_bad_opts_cases(T, mod_pubsub_bad_opts())),
    check_iqdisc(mod_pubsub).

good_default_node_config_opts(T) ->
    [good_default_node_config_opt(T, K, Toml, Mim)
     || {K, Toml, Mim} <- default_node_config_opts()].

good_default_node_config_opt(T, K, Toml, Mim) ->
    MBase = [{default_node_config, [{K, Mim}]}],
    Base = #{<<"default_node_config">> => #{a2b(K) => Toml}},
    ?_eqf(modopts(mod_pubsub, MBase), T(Base)).

bad_default_node_config_opts(T) ->
    [bad_default_node_config_opt(T, K, Toml)
     || {K, Toml} <- default_node_config_bad_opts()].

bad_default_node_config_opt(T, K, Toml) ->
    Base = #{<<"default_node_config">> => #{a2b(K) => Toml}},
    ?_errf(T(Base)).

default_node_config_opts() ->
    [{access_model, <<"open">>, open},
     {deliver_notifications, true, true},
     {deliver_payloads, true, true},
     {max_items, 10, 10},
     {max_payload_size, 10000, 10000},
     {node_type, <<"leaf">>, leaf},
     {notification_type, <<"headline">>, headline},
     {notify_config, false, false},
     {notify_delete, false, false},
     {notify_retract, false, false},
     {persist_items, true, true},
     {presence_based_delivery, true, true},
     {publish_model, <<"open">>, open},
     {purge_offline, false, false},
     {roster_groups_allowed, [<<"friends">>], [<<"friends">>]},
     {send_last_published_item, <<"on_sub_and_presence">>, on_sub_and_presence},
     {subscribe, true, true}].

default_node_config_bad_opts() ->
    [{access_model, 1},
     {deliver_notifications, 1},
     {deliver_payloads, 1},
     {max_items, -1},
     {max_payload_size, -1},
     {node_type, 1},
     {notification_type, 1},
     {notify_config, 1},
     {notify_delete, 1},
     {notify_retract, 1},
     {persist_items, 1},
     {presence_based_delivery, 1},
     {publish_model, 1},
     {purge_offline, 1},
     {roster_groups_allowed, [1]},
     {roster_groups_allowed, 1},
     {send_last_published_item, 1},
     {subscribe, 1}].

mod_pubsub_bad_opts() ->
    [{backend, 1},
     {access_createnode, 1},
     {host, 1},
     {host, <<"aaa aaa">>},
     {max_items_node, -1},
     {max_subscriptions_node, -1},
     {nodetree, -1},
     {nodetree, <<"oops">>},
     {ignore_pep_from_offline, 1},
     {last_item_cache, 1},
     {plugins, [<<"fat">>]},
     {pep_mapping, [#{<<"namespace">> => 1, <<"node">> => <<"mb">>}]},
     {pep_mapping, [#{<<"namespace">> => <<"urn:xmpp:microblog:0">>, <<"node">> => 1}]},
     {item_publisher, 1},
     {sync_broadcast, 1}].

mod_push_service_mongoosepush(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_push_service_mongoosepush">> => Opts}} end,
    Base = #{<<"pool_name">> => <<"test_pool">>,
             <<"api_version">> => <<"v3">>,
             <<"max_http_connections">> => 100},
    MBase = [{pool_name, test_pool},
             {api_version, "v3"},
             {max_http_connections, 100}],
    ?eqf(modopts(mod_push_service_mongoosepush, lists:sort(MBase)), T(Base)),
    ?errf(T(Base#{<<"pool_name">> => 1})),
    ?errf(T(Base#{<<"api_version">> => 1})),
    ?errf(T(Base#{<<"max_http_connections">> => -1})),
    ok.

mod_register(_Config) ->
    ?eqf(modopts(mod_register,
                [{access,register},
                 {ip_access, [{allow,"127.0.0.0/8"},
                              {deny,"0.0.0.0"}]}
                ]),
         ip_access_register(<<"0.0.0.0">>)),
    ?eqf(modopts(mod_register,
                [{access,register},
                 {ip_access, [{allow,"127.0.0.0/8"},
                              {deny,"0.0.0.4"}]}
                ]),
         ip_access_register(<<"0.0.0.4">>)),
    ?eqf(modopts(mod_register,
                [{access,register},
                 {ip_access, [{allow,"127.0.0.0/8"},
                              {deny,"::1"}]}
                ]),
         ip_access_register(<<"::1">>)),
    ?eqf(modopts(mod_register,
                [{access,register},
                 {ip_access, [{allow,"127.0.0.0/8"},
                              {deny,"::1/128"}]}
                ]),
         ip_access_register(<<"::1/128">>)),
    ?errf(invalid_ip_access_register()),
    ?errf(invalid_ip_access_register_ipv6()),
    ?errf(ip_access_register(<<"hello">>)),
    ?errf(ip_access_register(<<"0.d">>)),
    ?eqf(modopts(mod_register,
                [{welcome_message, {"Subject", "Body"}}]),
         welcome_message()),
    %% List of jids
    ?eqf(modopts(mod_register,
                [{registration_watchers,
                  [<<"alice@bob">>, <<"ilovemongoose@help">>]}]),
         registration_watchers([<<"alice@bob">>, <<"ilovemongoose@help">>])),
    ?errf(registration_watchers([<<"alice@bob">>, <<"jids@have@no@feelings!">>])),
    %% non-negative integer
    ?eqf(modopts(mod_register, [{password_strength, 42}]),
         password_strength_register(42)),
    ?errf(password_strength_register(<<"42">>)),
    ?errf(password_strength_register(<<"strong">>)),
    ?errf(password_strength_register(-150)),
    ?errf(welcome_message(<<"Subject">>, 1)),
    ?errf(welcome_message(1, <<"Body">>)),
    check_iqdisc(mod_register).

welcome_message() ->
    welcome_message(<<"Subject">>, <<"Body">>).

welcome_message(S, B) ->
    Opts = #{<<"welcome_message">> => #{<<"subject">> => S, <<"body">> => B}},
    #{<<"modules">> => #{<<"mod_register">> => Opts}}.

password_strength_register(Strength) ->
    Opts = #{<<"password_strength">> => Strength},
    #{<<"modules">> => #{<<"mod_register">> => Opts}}.

ip_access_register(Ip) ->
    Opts = #{<<"access">> => <<"register">>,
             <<"ip_access">> =>
                [#{<<"address">> => <<"127.0.0.0/8">>, <<"policy">> => <<"allow">>},
                 #{<<"address">> => Ip, <<"policy">> => <<"deny">>}]},
    #{<<"modules">> => #{<<"mod_register">> => Opts}}.

invalid_ip_access_register() ->
    Opts = #{<<"access">> => <<"register">>,
             <<"ip_access">> =>
                [#{<<"address">> => <<"127.0.0.0/8">>, <<"policy">> => <<"allawww">>},
                 #{<<"address">> => <<"8.8.8.8">>, <<"policy">> => <<"denyh">>}]},
    #{<<"modules">> => #{<<"mod_register">> => Opts}}.

invalid_ip_access_register_ipv6() ->
    Opts = #{<<"access">> => <<"register">>,
             <<"ip_access">> =>
                [#{<<"address">> => <<"::1/129">>, <<"policy">> => <<"allow">>}]},
    #{<<"modules">> => #{<<"mod_register">> => Opts}}.

registration_watchers(JidBins) ->
    Opts = #{<<"registration_watchers">> => JidBins},
    #{<<"modules">> => #{<<"mod_register">> => Opts}}.

mod_revproxy(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_revproxy">> => Opts}} end,
    R = fun(Route) -> T(#{<<"routes">> => [Route]}) end,
    Base = #{<<"routes">> => [R1 = #{
                 <<"host">> => <<"www.erlang-solutions.com">>,
                 <<"path">> => <<"/admin">>,
                 <<"method">> => <<"_">>,
                 <<"upstream">> => <<"https://www.erlang-solutions.com/">>
                }, #{
                 <<"host">> => <<"example.com">>,
                 <<"path">> => <<"/test">>,
                 <<"upstream">> => <<"https://example.com/">>
                }]},
    MBase = [{routes, [{"www.erlang-solutions.com", "/admin", "_",
                        "https://www.erlang-solutions.com/"},
                       {"example.com", "/test", "https://example.com/"}]}],
    run_multi([
            ?_eqf(modopts(mod_revproxy, MBase), T(Base)),
            ?_errf(R(R1#{<<"host">> => 1})),
            ?_errf(R(R1#{<<"path">> => 1})),
            ?_errf(R(R1#{<<"method">> => 1})),
            ?_errf(R(R1#{<<"upstream">> => 1})),
            ?_errf(R(R1#{<<"upstream">> => <<>>})),
            ?_errf(R(R1#{<<"host">> => <<>>}))
          ]).

mod_roster(_Config) ->
    Riak = #{<<"bucket_type">> => <<"rosters">>,
             <<"version_bucket_type">> => <<"roster_versions">>},
    Base = #{<<"iqdisc">> => #{<<"type">> => <<"one_queue">>},
             <<"versioning">> => false,
             <<"store_current_id">> => false,
             <<"backend">> => <<"mnesia">>,
             <<"riak">> => Riak},
    MBase = [{iqdisc, one_queue},
             {versioning, false},
             {store_current_id, false},
             {backend, mnesia},
             {bucket_type, <<"rosters">>},
             {version_bucket_type, <<"roster_versions">>}],
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_roster">> => Opts}} end,
    run_multi([
            ?_eqf(modopts(mod_roster, lists:sort(MBase)), T(Base)),
            ?_errf(T(#{<<"versioning">> => 1})),
            ?_errf(T(#{<<"store_current_id">> => 1})),
            ?_errf(T(#{<<"backend">> => 1})),
            ?_errf(T(#{<<"backend">> => <<"iloveyou">>})),
            ?_errf(T(#{<<"riak">> => #{<<"version_bucket_type">> => 1}})),
            ?_errf(T(#{<<"riak">> => #{<<"bucket_type">> => 1}}))
          ]),
    check_iqdisc(mod_roster).

mod_shared_roster_ldap(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_shared_roster_ldap">> => Opts}} end,
    MBase = [{ldap_pool_tag, default},
             {ldap_base,  "string"},
             {ldap_deref, never},
             %% Options: attributes
             {ldap_groupattr, "cn"},
             {ldap_groupdesc, "default"},
             {ldap_userdesc, "cn"},
             {ldap_useruid, "cn"},
             {ldap_memberattr, "memberUid"},
             {ldap_memberattr_format, "%u"},
             {ldap_memberattr_format_re,""},
             %% Options: parameters
             {ldap_auth_check, true},
             {ldap_user_cache_validity, 300},
             {ldap_group_cache_validity, 300},
             {ldap_user_cache_size, 300},
             {ldap_group_cache_size, 300},
             %% Options: LDAP filters
             {ldap_rfilter, "test"},
             {ldap_gfilter, "test"},
             {ldap_ufilter, "test"},
             {ldap_filter, "test"}
        ],
    Base = #{
            <<"ldap_pool_tag">> => <<"default">>,
            <<"ldap_base">> => <<"string">>,
            <<"ldap_deref">> => <<"never">>,
            %% Options: attributes
            <<"ldap_groupattr">> => <<"cn">>,
            <<"ldap_groupdesc">> => <<"default">>,
            <<"ldap_userdesc">> => <<"cn">>,
            <<"ldap_useruid">> => <<"cn">>,
            <<"ldap_memberattr">> => <<"memberUid">>,
            <<"ldap_memberattr_format">> => <<"%u">>,
            <<"ldap_memberattr_format_re">> => <<"">>,
            %% Options: parameters
            <<"ldap_auth_check">> => true,
            <<"ldap_user_cache_validity">> => 300,
            <<"ldap_group_cache_validity">> => 300,
            <<"ldap_user_cache_size">> => 300,
            <<"ldap_group_cache_size">> => 300,
            %% Options: LDAP filters
            <<"ldap_rfilter">> => <<"test">>,
            <<"ldap_gfilter">> => <<"test">>,
            <<"ldap_ufilter">> => <<"test">>,
            <<"ldap_filter">> => <<"test">>
        },
    run_multi(
      check_one_opts(mod_shared_roster_ldap, MBase, Base, T) ++ [
            ?_eqf(modopts(mod_shared_roster_ldap, lists:sort(MBase)), T(Base)),
            ?_errf(T(#{<<"ldap_pool_tag">> => 1})),
            ?_errf(T(#{<<"ldap_base">> => 1})),
            ?_errf(T(#{<<"ldap_deref">> => 1})),
            %% Options: attributes
            ?_errf(T(#{<<"ldap_groupattr">> => 1})),
            ?_errf(T(#{<<"ldap_groupdesc">> => 1})),
            ?_errf(T(#{<<"ldap_userdesc">> => 1})),
            ?_errf(T(#{<<"ldap_useruid">> => 1})),
            ?_errf(T(#{<<"ldap_memberattr">> => 1})),
            ?_errf(T(#{<<"ldap_memberattr_format">> => 1})),
            ?_errf(T(#{<<"ldap_memberattr_format_re">> => 1})),
            %% Options: parameters
            ?_errf(T(#{<<"ldap_auth_check">> => 1})),
            ?_errf(T(#{<<"ldap_user_cache_validity">> => -1})),
            ?_errf(T(#{<<"ldap_group_cache_validity">> => -1})),
            ?_errf(T(#{<<"ldap_user_cache_size">> => -1})),
            ?_errf(T(#{<<"ldap_group_cache_size">> => -1})),
            %% Options: LDAP filters
            ?_errf(T(#{<<"ldap_rfilter">> => 1})),
            ?_errf(T(#{<<"ldap_gfilter">> => 1})),
            ?_errf(T(#{<<"ldap_ufilter">> => 1})),
            ?_errf(T(#{<<"ldap_filter">> => 1}))
        ]).

mod_sic(_Config) ->
    check_iqdisc(mod_sic).

mod_stream_management(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_stream_management">> => Opts}} end,
    Base = #{
        <<"buffer_max">> => 100,
        <<"ack_freq">> => 1,
        <<"resume_timeout">> => 600,
        <<"stale_h">> => #{<<"enabled">> => true,
                           <<"repeat_after">> => 1800,
                           <<"geriatric">> => 3600}
       },
    MBase = [
             {buffer_max, 100},
             {ack_freq, 1},
             {resume_timeout, 600},
             {stale_h, [{enabled, true},
                        {stale_h_geriatric, 3600},
                        {stale_h_repeat_after, 1800}]}
       ],
    ?eqf(modopts(mod_stream_management, lists:sort(MBase)), T(Base)),
    ?eqf(modopts(mod_stream_management, [{buffer_max, no_buffer}]),
         T(#{<<"buffer_max">> => <<"no_buffer">>})),
    ?errf(T(#{<<"buffer_max">> => -1})),
    ?errf(T(#{<<"ack_freq">> => -1})),
    ?errf(T(#{<<"resume_timeout">> => -1})),
    ?errf(T(#{<<"stale_h">> => #{<<"enabled">> => <<"true">>}})),
    ?errf(T(#{<<"stale_h">> => #{<<"enabled">> => 1}})),
    ?errf(T(#{<<"stale_h">> => #{<<"repeat_after">> => -1}})),
    ?errf(T(#{<<"stale_h">> => #{<<"geriatric">> => -1}})),
    ok.

mod_time(_Config) ->
    check_iqdisc(mod_time).

mod_vcard(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_vcard">> => Opts}} end,
    MBase = [{iqdisc, one_queue},
             {host, "vjud.@HOST@"},
             {search, true},
             {backend, mnesia},
             {matches, infinity},
             %% ldap
             {ldap_pool_tag, default},
             {ldap_base, "ou=Users,dc=ejd,dc=com"},
             {ldap_deref, never},
             {ldap_uids, [{"mail", "%u@mail.example.org"}, "name"]},
             {ldap_filter, "(&(objectClass=shadowAccount)(memberOf=Jabber Users))"},
             %% MIM accepts {"FAMILY", "%s", ["sn", "cn"]} form too
             {ldap_vcard_map, [{<<"FAMILY">>, <<"%s">>, [<<"sn">>]}]}, %% iolists
             {ldap_search_fields, [{<<"Full Name">>, <<"cn">>}]}, %% pair of iolists
             {ldap_search_reported, [{<<"Full Name">>, <<"FN">>}]}, %% iolists
             {ldap_search_operator, 'or'},
             {ldap_binary_search_fields, [<<"PHOTO">>]},
             %% riak
             {bucket_type, <<"vcard">>},
             {search_index, <<"vcard">>}
            ],
    Riak = #{<<"bucket_type">> => <<"vcard">>,
             <<"search_index">> => <<"vcard">>},
    Base = #{
      <<"iqdisc">> => #{<<"type">> => <<"one_queue">>},
      <<"host">> => <<"vjud.@HOST@">>,
      <<"search">> => true,
      <<"backend">> => <<"mnesia">>,
      <<"matches">> => <<"infinity">>,
      %% ldap
      <<"ldap_pool_tag">> => <<"default">>,
      <<"ldap_base">> => <<"ou=Users,dc=ejd,dc=com">>,
      <<"ldap_deref">> => <<"never">>,
      <<"ldap_uids">> => [#{<<"attr">> => <<"mail">>,
                            <<"format">> => <<"%u@mail.example.org">>},
                          #{<<"attr">> => <<"name">>}],
      <<"ldap_filter">> => <<"(&(objectClass=shadowAccount)(memberOf=Jabber Users))">>,
      <<"ldap_vcard_map">> => [#{<<"vcard_field">> => <<"FAMILY">>,
                                 <<"ldap_pattern">> => <<"%s">>,
                                 <<"ldap_field">> => <<"sn">>}],
      <<"ldap_search_fields">> => [#{<<"search_field">> => <<"Full Name">>, <<"ldap_field">> => <<"cn">>}],
      <<"ldap_search_reported">> => [#{<<"search_field">> => <<"Full Name">>, <<"vcard_field">> => <<"FN">>}],
      <<"ldap_search_operator">> => <<"or">>, % atom
      <<"ldap_binary_search_fields">> => [<<"PHOTO">>],
      <<"riak">> => Riak
     },
    run_multi(check_one_opts_with_same_field_name(mod_vcard, MBase, Base, T)
              ++ [ ?_eqf(modopts(mod_vcard, lists:sort(MBase)), T(Base)),
                   ?_eqf(modopts(mod_vcard, [{matches, 1}]), T(#{<<"matches">> => 1})) ]
              ++ generic_bad_opts_cases(T, mod_vcard_bad_opts())),
    check_iqdisc(mod_vcard).

mod_vcard_bad_opts() ->
    M = #{<<"vcard_field">> => <<"FAMILY">>,
          <<"ldap_pattern">> => <<"%s">>,
          <<"ldap_field">> => <<"sn">>},
    [{host, 1},
     {host, <<"test test">>},
     {search, 1},
     {backend, 1},
     {backend, <<"mememesia">>},
     {matches, -1},
     {ldap_pool_tag, -1},
     {ldap_base, -1},
     {ldap_deref, <<"nevernever">>},
     {ldap_deref, -1},
     {ldap_uids, -1},
     {ldap_uids, [#{}]},
     {ldap_uids, [#{<<"attr">> => 1, <<"format">> => <<"ok">>}]},
     {ldap_uids, [#{<<"attr">> => <<"ok">>, <<"format">> => 1}]},
     {ldap_uids, [#{<<"format">> => <<"ok">>}]},
     {ldap_filter, 1},
     {ldap_vcard_map, [M#{<<"vcard_field">> => 1}]},
     {ldap_vcard_map, [M#{<<"ldap_pattern">> => 1}]},
     {ldap_vcard_map, [M#{<<"ldap_pattern">> => 1}]},
     {ldap_search_fields, [#{<<"search_field">> => 1, <<"ldap_field">> => <<"cn">>}]},
     {ldap_search_fields, [#{<<"search_field">> => <<"Full Name">>, <<"ldap_field">> => 1}]},
     {ldap_search_reported, [#{<<"search_field">> => 1, <<"vcard_field">> => <<"FN">>}]},
     {ldap_search_reported, [#{<<"search_field">> => <<"Full Name">>, <<"vcard_field">> => 1}]},
     {ldap_search_operator, <<"more">>},
     {ldap_binary_search_fields, [1]},
     {ldap_binary_search_fields, 1},
     {riak, #{<<"bucket_type">> => 1}},
     {riak, #{<<"search_index">> => 1}}].

mod_version(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_version">> => Opts}} end,
    ?eqf(modopts(mod_version, [{os_info, false}]), T(#{<<"os_info">> => false})),
    ?errf(T(#{<<"os_info">> => 1})),
    check_iqdisc(mod_version).

%% Services

service_admin_extra(_Config) ->
    T = fun(Opts) -> #{<<"services">> => #{<<"service_admin_extra">> => Opts}} end,
    ?eq(servopts(service_admin_extra, [{submods, [node]}]),
        parse(T(#{<<"submods">> => [<<"node">>]}))),
    ?err(parse(T(#{<<"submods">> => 1}))),
    ?err(parse(T(#{<<"submods">> => [1]}))),
    ?err(parse(T(#{<<"submods">> => [<<"nodejshaha">>]}))),
    ok.

service_mongoose_system_metrics(_Config) ->
    M = service_mongoose_system_metrics,
    T = fun(Opts) -> #{<<"services">> => #{<<"service_mongoose_system_metrics">> => Opts}} end,
    ?eq(servopts(M, [{initial_report, 5000}]),
        parse(T(#{<<"initial_report">> => 5000}))),
    ?eq(servopts(M, [{periodic_report, 5000}]),
        parse(T(#{<<"periodic_report">> => 5000}))),
    ?eq(servopts(M, [{tracking_id, "UA-123456789"}]),
        parse(T(#{<<"tracking_id">> => <<"UA-123456789">>}))),
    ?eq(servopts(M, [no_report]),
        parse(T(#{<<"report">> => false}))),
    %% error cases
    ?err(parse(T(#{<<"initial_report">> => <<"forever">>}))),
    ?err(parse(T(#{<<"periodic_report">> => <<"forever">>}))),
    ?err(parse(T(#{<<"initial_report">> => -1}))),
    ?err(parse(T(#{<<"periodic_report">> => -1}))),
    ?err(parse(T(#{<<"tracking_id">> => 666}))),
    ok.

%% Helpers for module tests

iqdisc({queues, Workers}) -> #{<<"type">> => <<"queues">>, <<"workers">> => Workers};
iqdisc(Atom) -> #{<<"type">> => atom_to_binary(Atom, utf8)}.

iq_disc_generic(Module, Value) ->
    Opts = #{<<"iqdisc">> => Value},
    #{<<"modules">> => #{atom_to_binary(Module, utf8) => Opts}}.

check_iqdisc(Module) ->
    ?eqf(modopts(Module, [{iqdisc, {queues, 10}}]),
         iq_disc_generic(Module, iqdisc({queues, 10}))),
    ?eqf(modopts(Module, [{iqdisc, parallel}]),
         iq_disc_generic(Module, iqdisc(parallel))),
    ?errf(iq_disc_generic(Module, iqdisc(bad_haha))).

modopts(Mod, Opts) ->
    [#local_config{key = {modules, ?HOST}, value = [{Mod, Opts}]}].

servopts(Mod, Opts) ->
    [#local_config{key = services, value = [{Mod, Opts}]}].

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

%% helpers for 'auth' tests

auth_ldap(Opts) ->
    auth_config(<<"ldap">>, Opts).

auth_config(Method, Opts) ->
    #{<<"auth">> => #{Method => Opts}}.

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

%% helpers for 'host_config' tests

eq_host_config(Result, Config) ->
    ConfigFunctions = parse(Config), % check for all hosts
    compare_config(Result, lists:flatmap(fun(F) -> F(?HOST) end, ConfigFunctions)),
    compare_config(Result, parse_host_config(Config)). % Check for a single host

eq_host_or_global(ResultF, Config) ->
    compare_config(ResultF(global), parse(Config)), % check for the 'global' host
    compare_config(ResultF(?HOST), parse_host_config(Config)). % check for a single host

err_host_config(Config) ->
    ?err(begin
             Items = parse(Config),
             lists:flatmap(fun(F) when is_function(F) -> F(?HOST);
                              (Item) -> [Item]
                           end, Items)
         end),
    ?err(parse_host_config(Config)).

err_host_or_global(Config) ->
    ?err(parse(Config)),
    ?err(parse_host_config(Config)).

parse_host_config(Config) ->
    parse(#{<<"host_config">> => [Config#{<<"host">> => ?HOST}]}).

%% plug in 'hosts' as this option is mandatory, then parse, then remove the extra 'hosts'
parse(M)
  when not is_map_key(<<"general">>, M) ->
    parse(M#{<<"general">> => #{<<"hosts">> => [?HOST]}});
parse(M = #{<<"general">> := GenM})
  when not is_map_key(<<"hosts">>, GenM) ->
    parse(M#{<<"general">> => GenM#{<<"hosts">> => [?HOST]}});
parse(M) ->
    Config = mongoose_config_parser_toml:parse(M),
    lists:filter(fun(#config{key = hosts, value = [?HOST]}) -> false;
                    (_) -> true
                 end, Config).

%% helpers for file tests

test_config_file(Config, File) ->
    OptionsPath = ejabberd_helper:data(Config, File ++ ".options"),
    {ok, ExpectedOpts} = file:consult(OptionsPath),

    TOMLPath = ejabberd_helper:data(Config, File ++ ".toml"),
    State = mongoose_config_parser_toml:parse_file(TOMLPath),
    TOMLOpts = mongoose_config_parser:state_to_opts(State),

    %% Save the parsed TOML options
    %% - for debugging
    %% - to update tests after a config change - always check the diff!
    save_opts(OptionsPath ++ ".parsed", TOMLOpts),
    compare_config(ExpectedOpts, TOMLOpts).

save_opts(Path, Opts) ->
    FormattedOpts = [io_lib:format("~p.~n", [Opt]) || Opt <- lists:sort(Opts)],
    file:write_file(Path, FormattedOpts).

compare_config(C1, C2) ->
    compare_unordered_lists(C1, C2, fun handle_config_option/2).

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

set_pl(K, V, List) ->
    lists:keyreplace(K, 1, List, {K, V}).

create_files(Config) ->
    %% The files must exist for validation to pass
    Root = small_path_helper:repo_dir(Config),
    file:make_dir("priv"),
    PrivkeyPath = filename:join(Root, "tools/ssl/mongooseim/privkey.pem"),
    CertPath = filename:join(Root, "tools/ssl/mongooseim/cert.pem"),
    CaPath = filename:join(Root, "tools/ssl/ca/cacert.pem"),
    ok = file:write_file("priv/access_psk", ""),
    ok = file:write_file("priv/provision_psk", ""),
    ok = filelib:ensure_dir("www/muc/dummy"),
    ensure_copied(CaPath, "priv/ca.pem"),
    ensure_copied(CertPath, "priv/cert.pem"),
    ensure_copied(PrivkeyPath, "priv/dc1.pem").

ensure_copied(From, To) ->
    case file:copy(From, To) of
        {ok,_} ->
            ok;
        Other ->
            error(#{what => ensure_copied_failed, from => From, to => To,
                    reason => Other})
    end.

pl_merge(L1, L2) ->
    M1 = maps:from_list(L1),
    M2 = maps:from_list(L2),
    maps:to_list(maps:merge(M1, M2)).

%% Runs check_one_opts, but only for fields, that present in both
%% MongooseIM and TOML config formats with the same name.
%% Helps to filter out riak fields automatically.
check_one_opts_with_same_field_name(M, MBase, Base, T) ->
    KeysM = maps:keys(maps:from_list(MBase)),
    KeysT = lists:map(fun b2a/1, maps:keys(Base)),
    Keys = ordsets:intersection(ordsets:from_list(KeysT),
                                ordsets:from_list(KeysM)),
    Hook = fun(A,B) -> {A,B} end,
    check_one_opts(M, MBase, Base, T, Keys, Hook).

check_one_opts(M, MBase, Base, T) ->
    Keys = maps:keys(maps:from_list(MBase)),
    Hook = fun(A,B) -> {A,B} end,
    check_one_opts(M, MBase, Base, T, Keys, Hook).

check_one_opts(M, MBase, Base, T, Keys, Hook) ->
    [check_one_opts_key(M, K, MBase, Base, T, Hook) || K <- Keys].

check_one_opts_key(M, K, MBase, Base, T, Hook) when is_atom(M), is_atom(K) ->
    BK = atom_to_binary(K, utf8),
    MimValue = maps:get(K, maps:from_list(MBase)),
    TomValue = maps:get(BK, Base),
    Mim0 = [{K, MimValue}],
    Toml0 = #{BK => TomValue},
    {Mim, Toml} = Hook(Mim0, Toml0),
    ?_eqf(modopts(M, Mim), T(Toml)).

binaries_to_atoms(Bins) ->
    [binary_to_atom(B, utf8) || B <- Bins].

run_multi(Cases) ->
    Results = [run_case(F) || {F,_} <- Cases],
    case lists:all(fun(X) -> X =:= ok end, Results) of
        true ->
            ok;
        false ->
            Failed = [Zip || {Res,_}=Zip <- lists:zip(Results, Cases), Res =/= ok],
            [ct:pal("Info: ~p~nResult: ~p~n", [Info, Res]) || {Res, Info} <- Failed],
            ct:fail(#{what => run_multi_failed, failed_cases => length(Failed)})
    end.

run_case(F) ->
    try
        F(), ok
    catch Class:Reason:Stacktrace ->
        {Class, Reason, Stacktrace}
    end.

ensure_sorted(List) ->
    [ct:fail("Not sorted list ~p~nSorted order ~p~n", [List, lists:sort(List)])
     || lists:sort(List) =/= List].

a2b(X) -> atom_to_binary(X, utf8).
b2a(X) -> binary_to_atom(X, utf8).


generic_opts_cases(M, T, Opts) ->
    [generic_opts_case(M, T, K, Toml, Mim) || {K, Toml, Mim} <- Opts].

generic_opts_case(M, T, K, Toml, Mim) ->
    Info = #{key => K, toml => Toml, mim => Mim},
    info(Info, ?_eqf(modopts(M, [{K, Mim}]), T(#{a2b(K) => Toml}))).

generic_renamed_opts_cases(M, T, Opts) ->
    [generic_renamed_opts_case(M, T, TomlKey, MimKey, Toml, Mim)
     || {TomlKey, MimKey, Toml, Mim} <- Opts].

generic_renamed_opts_case(M, T, TomlKey, MimKey, Toml, Mim) ->
    ?_eqf(modopts(M, [{MimKey, Mim}]), T(#{a2b(TomlKey) => Toml})).


generic_bad_opts_cases(T, Opts) ->
    [generic_bad_opts_case(T, K, Toml) || {K, Toml} <- Opts].

generic_bad_opts_case(T, K, Toml) ->
    ?_errf(T(#{a2b(K) => Toml})).

info(Info, {F, Extra}) ->
    {F, maps:merge(Extra, Info)}.
