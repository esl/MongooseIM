-module(config_parser_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("ejabberd_config.hrl").

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

-define(err(Expr), ?assertMatch([#{class := error, what := _}|_],
                                mongoose_config_parser_toml:extract_errors(Expr))).

-define(cfg(Opts, Config), assert_options(Opts, Config)).

-define(HOST, <<"myhost">>).

-define(cfgh(Expected, Actual), assert_host_or_global(fun(Host) -> Expected end, Actual)).
-define(errh(Config), err_host_or_global(Config)).

all() ->
    [{group, file},
     {group, general},
     {group, listen},
     {group, auth},
     {group, pool},
     {group, shaper_acl_access},
     {group, s2s},
     {group, modules},
     {group, services},
     {group, host_types_group}].

groups() ->
    [{file, [parallel], [sample_pgsql,
                         miscellaneous,
                         s2s,
                         modules,
                         outgoing_pools]},
     {host_types_group, [], [host_types_file,
                             host_types_missing_auth_methods_and_modules,
                             host_types_unsupported_modules,
                             host_types_unsupported_auth_methods,
                             host_types_unsupported_auth_methods_and_modules]},
     {general, [parallel], [loglevel,
                            hosts,
                            host_types,
                            default_server_domain,
                            registration_timeout,
                            language,
                            all_metrics_are_global,
                            sm_backend,
                            max_fsm_queue,
                            http_server_name,
                            rdbms_server_type,
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
                           listen_http_handlers_api,
                           listen_http_handlers_domain]},
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
                         auth_riak_bucket_type,
                         auth_rdbms_users_number_estimate]},
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
                            mod_cache_users,
                            mod_carboncopy,
                            mod_csi,
                            mod_disco,
                            mod_inbox,
                            mod_global_distrib,
                            mod_global_distrib_connections,
                            mod_global_distrib_connections_endpoints,
                            mod_global_distrib_connections_advertised_endpoints,
                            mod_global_distrib_connections_tls,
                            mod_global_distrib_redis,
                            mod_global_distrib_cache,
                            mod_global_distrib_bounce,
                            mod_event_pusher_sns,
                            mod_event_pusher_push,
                            mod_event_pusher_http,
                            mod_event_pusher_rabbit,
                            mod_extdisco,
                            mod_http_upload,
                            mod_http_upload_s3,
                            mod_jingle_sip,
                            mod_keystore,
                            mod_keystore_keys,
                            mod_last,
                            mod_mam_meta,
                            mod_mam_meta_pm,
                            mod_mam_meta_muc,
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
                            mod_pubsub_pep_mapping,
                            mod_pubsub_default_node_config,
                            mod_push_service_mongoosepush,
                            mod_register,
                            mod_roster,
                            mod_shared_roster_ldap,
                            mod_sic,
                            mod_stream_management,
                            mod_stream_management_stale_h,
                            mod_time,
                            mod_vcard,
                            mod_vcard_ldap_uids,
                            mod_vcard_ldap_vcard_map,
                            mod_vcard_ldap_search_fields,
                            mod_vcard_ldap_search_reported,
                            mod_version,
                            modules_without_config]},
     {services, [parallel], [service_admin_extra,
                             service_mongoose_system_metrics]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    create_files(Config),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(host_types_group, Config) ->
    Modules = [test_mim_module1, test_mim_module2, test_mim_module3],
    AuthModules = [ejabberd_auth_test1, ejabberd_auth_test2, ejabberd_auth_test3],
    [{mocked_modules, Modules ++ AuthModules} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(_, Config) ->
    case proplists:get_value(mocked_modules, Config, no_mocks) of
        no_mocks -> ok;
        Modules -> [meck:new(M, [non_strict, no_link]) || M <- Modules]
    end,
    Config.

end_per_testcase(_, Config) ->
    meck:unload(),
    Config.

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

host_types_file(Config) ->
    Modules = [test_mim_module1, test_mim_module2,
               ejabberd_auth_test1, ejabberd_auth_test2],
    FN = fun() -> [dynamic_domains] end,
    [meck:expect(M, supported_features, FN) || M <- Modules],
    test_config_file(Config, "host_types").

host_types_missing_auth_methods_and_modules(Config) ->
    Modules = [ejabberd_auth_test1, test_mim_module1, test_mim_module2],
    [meck:unload(M) || M <- Modules],
    {'EXIT', {{config_error, "Could not read the TOML configuration file", ErrorList}, _}}
        = (catch test_config_file(Config, "host_types")),
    MissingModules = [M || #{reason := module_not_found, module := M} <- ErrorList],
    ?assertEqual(lists:sort(Modules), lists:usort(MissingModules)).

host_types_unsupported_modules(Config) ->
    Modules = [ejabberd_auth_test1, ejabberd_auth_test2],
    FN = fun() -> [dynamic_domains] end,
    [meck:expect(M, supported_features, FN) || M <- Modules],
    ?assertError({config_error, "Invalid host type configuration",
                  %% please note that the sequence of these errors is not
                  %% guarantied and may change in the future
                  [#{reason := not_supported_module, module := test_mim_module1,
                     host_type := <<"yet another host type">>},
                   #{reason := not_supported_module, module := test_mim_module2,
                     host_type := <<"another host type">>}]},
                 test_config_file(Config, "host_types")).

host_types_unsupported_auth_methods(Config) ->
    Modules = [test_mim_module1, test_mim_module2, ejabberd_auth_test1],
    FN = fun() -> [dynamic_domains] end,
    [meck:expect(M, supported_features, FN) || M <- Modules],
    ?assertError({config_error, "Invalid host type configuration",
                  %% please note that the sequence of these errors is not
                  %% guarantied and may change in the future
                  [#{reason := not_supported_auth_method, auth_method := test2,
                     host_type := <<"yet another host type">>},
                   #{reason := not_supported_auth_method, auth_method := test2,
                     host_type := <<"some host type">>}]},
                 test_config_file(Config, "host_types")).

host_types_unsupported_auth_methods_and_modules(Config) ->
    Modules = [test_mim_module1, ejabberd_auth_test1],
    FN = fun() -> [dynamic_domains] end,
    [meck:expect(M, supported_features, FN) || M <- Modules],
    ?assertError({config_error, "Invalid host type configuration",
                  %% please note that the sequence of these errors is not
                  %% guarantied and may change in the future
                  [#{reason := not_supported_auth_method, auth_method := test2,
                     host_type := <<"yet another host type">>},
                   #{reason := not_supported_module, module := test_mim_module2,
                     host_type := <<"another host type">>},
                   #{reason := not_supported_auth_method, auth_method := test2,
                     host_type := <<"some host type">>}]},
                 test_config_file(Config, "host_types")).

%% tests: general
loglevel(_Config) ->
    ?cfg([#local_config{key = loglevel, value = debug}],
        parse(#{<<"general">> => #{<<"loglevel">> => <<"debug">>}})),
    ?err(parse(#{<<"general">> => #{<<"loglevel">> => <<"bebug">>}})),
    %% make sure non-host options are not accepted in host_config
    ?err(parse_host_config(#{<<"general">> => #{<<"loglevel">> => <<"debug">>}})).

hosts(_Config) ->
    ?cfg([#local_config{key = hosts, value = [<<"host1">>]}],
        parse(#{<<"general">> => #{<<"hosts">> => [<<"host1">>]}})),
    GenM = #{<<"default_server_domain">> => <<"some.host">>},
    M = GenM#{<<"hosts">> => [<<"host1">>, <<"host2">>],
              <<"host_types">> => []},
    ?cfg([#local_config{key = hosts, value = [<<"host1">>, <<"host2">>]},
          #local_config{key = host_types, value = []},
          #local_config{key = default_server_domain, value = <<"some.host">>}],
         mongoose_config_parser_toml:parse(#{<<"general">> => M})),
    ?err(parse(#{<<"general">> => #{<<"hosts">> => [<<"what is this?">>]}})),
    ?err(parse(#{<<"general">> => #{<<"hosts">> => [<<>>]}})),
    ?err(parse(#{<<"general">> => #{<<"hosts">> => [<<"host1">>, <<"host1">>]}})),
    % either hosts or host_types must be provided
    ?err(mongoose_config_parser_toml:parse(#{<<"general">> => #{}})),
    ?err(mongoose_config_parser_toml:parse(#{<<"general">> => GenM})),
    ?err(mongoose_config_parser_toml:parse(#{<<"general">> => GenM#{<<"host">> => [],
                                                                    <<"host_types">> => []}})),
    ?err(mongoose_config_parser_toml:parse(#{<<"general">> => GenM#{<<"host">> => []}})),
    ?err(mongoose_config_parser_toml:parse(#{<<"general">> => GenM#{<<"host_types">> => []}})).

host_types(_Config) ->
    ?cfg([#local_config{key = host_types, value = [<<"type 1">>]}],
        parse(#{<<"general">> => #{<<"host_types">> => [<<"type 1">>]}})),
    ?cfg([#local_config{key = host_types, value = [<<"type 1">>, <<"type 2">>]},
          #local_config{key = hosts, value = []}],
         parse(#{<<"general">> => #{<<"host_types">> => [<<"type 1">>, <<"type 2">>],
                                    <<"hosts">> => []}})),
    ?err(parse(#{<<"general">> => #{<<"host_types">> => [<<>>]}})),
    ?err(parse(#{<<"general">> => #{<<"host_types">> => [<<"type1">>, <<"type1">>]}})),
    % either hosts and host_types cannot have the same values
    ?err(parse(#{<<"general">> => #{<<"host_types">> => [<<"type1">>],
                                    <<"hosts">> => [<<"type1">>]}})).

default_server_domain(_Config) ->
    ?cfg([#local_config{key = default_server_domain, value = <<"host1">>}],
        parse(#{<<"general">> => #{<<"default_server_domain">> => <<"host1">>}})),
    GenM = #{<<"hosts">> => [<<"host1">>, <<"host2">>]},
    M = GenM#{<<"default_server_domain">> => <<"some.host">>},
    ?cfg([#local_config{key = hosts, value = [<<"host1">>, <<"host2">>]},
          #local_config{key = default_server_domain, value = <<"some.host">>}],
         mongoose_config_parser_toml:parse(#{<<"general">> => M})),
    ?err(parse(#{<<"general">> => #{<<"default_server_domain">> => <<"what is this?">>}})),
    ?err(parse(#{<<"general">> => #{<<"default_server_domain">> => <<>>}})),
    % default_server_domain must be provided
    ?err(mongoose_config_parser_toml:parse(#{<<"general">> => GenM})).

registration_timeout(_Config) ->
    ?cfg([#local_config{key = registration_timeout, value = infinity}],
        parse(#{<<"general">> => #{<<"registration_timeout">> => <<"infinity">>}})),
    ?cfg([#local_config{key = registration_timeout, value = 300}],
        parse(#{<<"general">> => #{<<"registration_timeout">> => 300}})),
    ?err(parse(#{<<"general">> => #{<<"registration_timeout">> => 0}})).

language(_Config) ->
    ?cfg([#local_config{key = language, value = <<"en">>}],
        parse(#{<<"general">> => #{<<"language">> => <<"en">>}})),
    ?err(parse(#{<<"general">> => #{<<"language">> => <<>>}})).

all_metrics_are_global(_Config) ->
    ?cfg([#local_config{key = all_metrics_are_global, value = true}],
        parse(#{<<"general">> => #{<<"all_metrics_are_global">> => true}})),
    ?err(parse(#{<<"general">> => #{<<"all_metrics_are_global">> => <<"true">>}})).

sm_backend(_Config) ->
    ?cfg([#local_config{key = sm_backend, value = {mnesia, []}}],
        parse(#{<<"general">> => #{<<"sm_backend">> => <<"mnesia">>}})),
    ?cfg([#local_config{key = sm_backend, value = {redis, []}}],
        parse(#{<<"general">> => #{<<"sm_backend">> => <<"redis">>}})),
    ?err(parse(#{<<"general">> => #{<<"sm_backend">> => <<"amnesia">>}})).

max_fsm_queue(_Config) ->
    ?cfg([#local_config{key = max_fsm_queue, value = 100}],
        parse(#{<<"general">> => #{<<"max_fsm_queue">> => 100}})),
    ?err(parse(#{<<"general">> => #{<<"max_fsm_queue">> => -10}})).

http_server_name(_Config) ->
    ?cfg([#local_config{key = cowboy_server_name, value = "my server"}],
        parse(#{<<"general">> => #{<<"http_server_name">> => <<"my server">>}})),
    ?err(parse(#{<<"general">> => #{<<"http_server_name">> => #{}}})).

rdbms_server_type(_Config) ->
    ?cfg([#local_config{key = rdbms_server_type, value = mssql}],
        parse(#{<<"general">> => #{<<"rdbms_server_type">> => <<"mssql">>}})),
    ?cfg([#local_config{key = rdbms_server_type, value = pgsql}],
        parse(#{<<"general">> => #{<<"rdbms_server_type">> => <<"pgsql">>}})),
    ?err(parse(#{<<"general">> => #{<<"rdbms_server_type">> => <<"nosql">>}})).

route_subdomains(_Config) ->
    ?cfgh([#local_config{key = {route_subdomains, Host}, value = s2s}],
         #{<<"general">> => #{<<"route_subdomains">> => <<"s2s">>}}),
    ?errh(#{<<"general">> => #{<<"route_subdomains">> => <<"c2s">>}}).

mongooseimctl_access_commands(_Config) ->
    AccessRule = #{<<"commands">> => [<<"join_cluster">>],
                   <<"argument_restrictions">> => #{<<"node">> => <<"mim1@host1">>}},
    ?cfg([#local_config{key = mongooseimctl_access_commands,
                       value = [{local, ["join_cluster"], [{node, "mim1@host1"}]}]
                      }],
        parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                       #{<<"local">> => AccessRule}}})),
    ?cfg([#local_config{key = mongooseimctl_access_commands,
                       value = [{local, all, [{node, "mim1@host1"}]}]
                      }],
        parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                       #{<<"local">> => maps:remove(<<"commands">>,
                                                                    AccessRule)}}})),
    ?cfg([#local_config{key = mongooseimctl_access_commands,
                       value = [{local, ["join_cluster"], []}]
                      }],
        parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                       #{<<"local">> => maps:remove(<<"argument_restrictions">>,
                                                                    AccessRule)}}})),
    ?cfg([#local_config{key = mongooseimctl_access_commands,
                       value = [{local, all, []}]
                      }],
        parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> => #{<<"local">> => #{}}}})),
    ?err(parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                        #{<<"local">> => #{<<"commands">> => <<"all">>}}}})),
    ?err(parse(#{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                        #{<<"local">> => #{<<"argument_restrictions">> =>
                                                               [<<"none">>]}}}})).

routing_modules(_Config) ->
    ?cfg([#local_config{key = routing_modules, value = [mongoose_router_global,
                                                       mongoose_router_localdomain]}],
        parse(#{<<"general">> => #{<<"routing_modules">> => [<<"mongoose_router_global">>,
                                                             <<"mongoose_router_localdomain">>]}})),
    ?err(parse(#{<<"general">> => #{<<"routing_modules">> => [<<"moongoose_router_global">>]}})).

replaced_wait_timeout(_Config) ->
    ?cfgh([#local_config{key = {replaced_wait_timeout, Host}, value = 1000}],
         #{<<"general">> => #{<<"replaced_wait_timeout">> => 1000}}),
    ?errh(#{<<"general">> => #{<<"replaced_wait_timeout">> => 0}}).

hide_service_name(_Config) ->
    ?cfg([#local_config{key = hide_service_name, value = false}],
        parse(#{<<"general">> => #{<<"hide_service_name">> => false}})),
    ?err(parse(#{<<"general">> => #{<<"hide_service_name">> => []}})).

%% tests: listen

listen_portip(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, []), parse_listener(<<"c2s">>, #{})),
    ?cfg([#local_config{key = listen,
                       value = [{{5222, {192, 168, 1, 16}, tcp}, ejabberd_c2s, []}]}],
        parse_listener(<<"c2s">>, #{<<"ip_address">> => <<"192.168.1.16">>})),
    ?cfg([#local_config{key = listen,
                       value = [{{5222, {8193, 3512, 3, 4, 5, 6, 7, 8}, tcp}, ejabberd_c2s, []}]}],
        parse_listener(<<"c2s">>, #{<<"ip_address">> => <<"2001:db8:3:4:5:6:7:8">>})),
    ?err(parse_listener(<<"c2s">>, #{<<"ip_address">> => <<"192.168.1.999">>})),
    ?err(parse(#{<<"listen">> => #{<<"c2s">> => [#{<<"ip_address">> => <<"192.168.1.16">>}]}})),
    ?err(parse(#{<<"listen">> => #{<<"c2s">> => [#{<<"port">> => <<"5222">>}]}})),
    ?err(parse(#{<<"listen">> => #{<<"c2s">> => [#{<<"port">> => 522222}]}})).

listen_proto(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{proto, tcp}]),
        parse_listener(<<"c2s">>, #{<<"proto">> => <<"tcp">>})),
    ?cfg([#local_config{key = listen,
                       value = [{{5222, {0, 0, 0, 0}, udp}, ejabberd_c2s, [{proto, udp}]}]}],
        parse_listener(<<"c2s">>, #{<<"proto">> => <<"udp">>})),
    ?err(parse_listener(<<"c2s">>, #{<<"proto">> => <<"pigeon">>})).

listen_ip_version(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [inet]),
        parse_listener(<<"c2s">>, #{<<"ip_version">> => 4})),
    ?cfg([#local_config{key = listen,
                       value = [{{5222, {0, 0, 0, 0, 0, 0, 0, 0}, tcp}, ejabberd_c2s, []}]}],
        parse_listener(<<"c2s">>, #{<<"ip_version">> => 6})),
    ?err(parse_listener(<<"c2s">>, #{<<"ip_version">> => 7})).

listen_backlog(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{backlog, 10}]),
        parse_listener(<<"c2s">>, #{<<"backlog">> => 10})),
    ?err(parse_listener(<<"c2s">>, #{<<"backlog">> => -10})).

listen_proxy_protocol(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{proxy_protocol, true}]),
        parse_listener(<<"c2s">>, #{<<"proxy_protocol">> => true})),
    ?cfg(listener_config(ejabberd_s2s_in, [{proxy_protocol, true}]),
        parse_listener(<<"s2s">>, #{<<"proxy_protocol">> => true})),
    ?cfg(listener_config(ejabberd_service, [{proxy_protocol, true}]),
        parse_listener(<<"service">>, #{<<"proxy_protocol">> => true})),
    ?err(parse_listener(<<"c2s">>, #{<<"proxy_protocol">> => <<"awesome">>})).

listen_num_acceptors(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{acceptors_num, 100}]),
        parse_listener(<<"c2s">>, #{<<"num_acceptors">> => 100})),
    ?cfg(listener_config(ejabberd_s2s_in, [{acceptors_num, 100}]),
        parse_listener(<<"s2s">>, #{<<"num_acceptors">> => 100})),
    ?cfg(listener_config(ejabberd_service, [{acceptors_num, 100}]),
        parse_listener(<<"service">>, #{<<"num_acceptors">> => 100})),
    ?err(parse_listener(<<"c2s">>, #{<<"num_acceptors">> => 0})).

listen_access(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{access, rule1}]),
        parse_listener(<<"c2s">>, #{<<"access">> => <<"rule1">>})),
    ?cfg(listener_config(ejabberd_service, [{access, rule1}]),
        parse_listener(<<"service">>, #{<<"access">> => <<"rule1">>})),
    ?err(parse_listener(<<"c2s">>, #{<<"access">> => <<>>})).

listen_shaper(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{shaper, c2s_shaper}]),
        parse_listener(<<"c2s">>, #{<<"shaper">> => <<"c2s_shaper">>})),
    ?cfg(listener_config(ejabberd_s2s_in, [{shaper, s2s_shaper}]),
        parse_listener(<<"s2s">>, #{<<"shaper">> => <<"s2s_shaper">>})),
    ?cfg(listener_config(ejabberd_service, [{shaper_rule, fast}]),
        parse_listener(<<"service">>, #{<<"shaper_rule">> => <<"fast">>})),
    ?err(parse_listener(<<"s2s">>, #{<<"shaper">> => <<>>})).

listen_xml_socket(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{xml_socket, true}]),
        parse_listener(<<"c2s">>, #{<<"xml_socket">> => true})),
    ?err(parse_listener(<<"c2s">>, #{<<"xml_socket">> => 10})).

listen_zlib(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{zlib, 1024}]),
        parse_listener(<<"c2s">>, #{<<"zlib">> => 1024})),
    ?err(parse_listener(<<"c2s">>, #{<<"zlib">> => 0})).

listen_hibernate_after(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{hibernate_after, 10}]),
        parse_listener(<<"c2s">>, #{<<"hibernate_after">> => 10})),
    ?cfg(listener_config(ejabberd_s2s_in, [{hibernate_after, 10}]),
        parse_listener(<<"s2s">>, #{<<"hibernate_after">> => 10})),
    ?cfg(listener_config(ejabberd_service, [{hibernate_after, 10}]),
        parse_listener(<<"service">>, #{<<"hibernate_after">> => 10})),
    ?err(parse_listener(<<"c2s">>, #{<<"hibernate_after">> => -10})).

listen_max_stanza_size(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{max_stanza_size, 10000}]),
        parse_listener(<<"c2s">>, #{<<"max_stanza_size">> => 10000})),
    ?cfg(listener_config(ejabberd_s2s_in, [{max_stanza_size, 10000}]),
        parse_listener(<<"s2s">>, #{<<"max_stanza_size">> => 10000})),
    ?cfg(listener_config(ejabberd_service, [{max_stanza_size, 10000}]),
        parse_listener(<<"service">>, #{<<"max_stanza_size">> => 10000})),
    ?err(parse_listener(<<"c2s">>, #{<<"max_stanza_size">> => <<"infinity">>})).

listen_max_fsm_queue(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{max_fsm_queue, 1000}]),
        parse_listener(<<"c2s">>, #{<<"max_fsm_queue">> => 1000})),
    ?cfg(listener_config(ejabberd_service, [{max_fsm_queue, 1000}]),
        parse_listener(<<"service">>, #{<<"max_fsm_queue">> => 1000})),
    ?err(parse_listener(<<"s2s">>, #{<<"max_fsm_queue">> => 1000})), % only for c2s and service
    ?err(parse_listener(<<"c2s">>, #{<<"max_fsm_queue">> => 0})).

listen_tls_mode(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [starttls]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"mode">> => <<"starttls">>}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"mode">> => <<"stoptls">>}})).

listen_tls_module(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{tls_module, just_tls}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>}})),
    ?cfg(listener_config(ejabberd_c2s, []),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"fast_tls">>}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"slow_tls">>}})).

listen_tls_verify(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [verify_peer]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"verify_peer">> => true}})),
    ?cfg(listener_config(ejabberd_c2s, [verify_none]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"verify_peer">> => false}})),
    ?cfg(listener_config(ejabberd_c2s, [{tls_module, just_tls}, verify_peer]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"verify_peer">> => true}})),
    ?cfg(listener_config(ejabberd_cowboy, [{ssl, [{verify, verify_peer}]}]),
        parse_listener(<<"http">>, #{<<"tls">> => #{<<"verify_peer">> => true}})),
    ?cfg(listener_config(ejabberd_c2s, [{tls_module, just_tls}, verify_none]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"verify_peer">> => false}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"verify_peer">> => <<"maybe">>}})).

listen_tls_verify_mode(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{verify_fun, {peer, true}}]}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"verify_mode">> => <<"peer">>}})),
    ?cfg(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{verify_fun, {selfsigned_peer, false}}]}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"verify_mode">> => <<"selfsigned_peer">>,
                                                   <<"disconnect_on_failure">> => false}})),
    ?cfg(listener_config(ejabberd_cowboy, [{ssl, [{verify_mode, peer}]}]),
        parse_listener(<<"http">>, #{<<"tls">> => #{<<"verify_mode">> => <<"peer">>}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"verify_mode">> => <<"peer">>,
                                                   <<"disconnect_on_failure">> => <<"false">>}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                    <<"verify_mode">> => <<"whatever">>}})),
    ?err(parse_listener(<<"http">>, #{<<"tls">> => #{<<"verify_mode">> => <<"whatever">>}})).

listen_tls_crl_files(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{tls_module, just_tls},
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
    ?cfg(listener_config(ejabberd_c2s, [{certfile, "mycert.pem"}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"certfile">> => <<"mycert.pem">>}})),
    ?cfg(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{certfile, "mycert.pem"}]}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"certfile">> => <<"mycert.pem">>}})),
    ?cfg(listener_config(ejabberd_cowboy, [{ssl, [{certfile, "mycert.pem"}]}]),
        parse_listener(<<"http">>, #{<<"tls">> => #{<<"certfile">> => <<"mycert.pem">>}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"certfile">> => <<>>}})).

listen_tls_cacertfile(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{cafile, "cacert.pem"}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"cacertfile">> => <<"cacert.pem">>}})),
    ?cfg(listener_config(ejabberd_s2s_in, [{cafile, "cacert.pem"}]),
        parse_listener(<<"s2s">>, #{<<"tls">> => #{<<"cacertfile">> => <<"cacert.pem">>}})),
    ?cfg(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{cacertfile, "cacert.pem"}]}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"cacertfile">> => <<"cacert.pem">>}})),
    ?cfg(listener_config(ejabberd_cowboy, [{ssl, [{cacertfile, "cacert.pem"}]}]),
        parse_listener(<<"http">>, #{<<"tls">> => #{<<"cacertfile">> => <<"cacert.pem">>}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"cacertfile">> => <<>>}})).

listen_tls_dhfile(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{dhfile, "dh.pem"}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"dhfile">> => <<"dh.pem">>}})),
    ?cfg(listener_config(ejabberd_s2s_in, [{dhfile, "dh.pem"}]),
        parse_listener(<<"s2s">>, #{<<"tls">> => #{<<"dhfile">> => <<"dh.pem">>}})),
    ?cfg(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{dhfile, "dh.pem"}]}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                   <<"dhfile">> => <<"dh.pem">>}})),
    ?cfg(listener_config(ejabberd_cowboy, [{ssl, [{dhfile, "dh.pem"}]}]),
        parse_listener(<<"http">>, #{<<"tls">> => #{<<"dhfile">> => <<"dh.pem">>}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"dhfile">> => <<>>}})).

listen_tls_ciphers(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{ciphers, "TLS_AES_256_GCM_SHA384"}]),
        parse_listener(<<"c2s">>,
                       #{<<"tls">> => #{<<"ciphers">> => <<"TLS_AES_256_GCM_SHA384">>}})),
    ?cfg(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{ciphers, "TLS_AES_256_GCM_SHA384"}]}]),
        parse_listener(<<"c2s">>,
                       #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                        <<"ciphers">> => <<"TLS_AES_256_GCM_SHA384">>}})),
    ?cfg(listener_config(ejabberd_s2s_in, [{ciphers, "TLS_AES_256_GCM_SHA384"}]),
        parse_listener(<<"s2s">>,
                       #{<<"tls">> => #{<<"ciphers">> => <<"TLS_AES_256_GCM_SHA384">>}})),
    ?cfg(listener_config(ejabberd_cowboy, [{ssl, [{ciphers, "TLS_AES_256_GCM_SHA384"}]}]),
        parse_listener(<<"http">>,
                       #{<<"tls">> => #{<<"ciphers">> => <<"TLS_AES_256_GCM_SHA384">>}})),
    ?err(parse_listener(<<"c2s">>,
                        #{<<"tls">> => #{<<"ciphers">> => [<<"TLS_AES_256_GCM_SHA384">>]}})).

listen_tls_versions(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{tls_module, just_tls},
                                       {ssl_options, [{versions, ['tlsv1.2', 'tlsv1.3']}]}]),
        parse_listener(<<"c2s">>,
                       #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                        <<"versions">> => [<<"tlsv1.2">>, <<"tlsv1.3">>]}})),
    ?err(parse_listener(<<"c2s">>,
                        #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                         <<"versions">> => <<"tlsv1.2">>}})).

listen_tls_protocol_options(_Config) ->
    ?cfg(listener_config(ejabberd_c2s, [{protocol_options, ["nosslv2"]}]),
        parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"protocol_options">> => [<<"nosslv2">>]}})),
    ?cfg(listener_config(ejabberd_s2s_in, [{protocol_options, ["nosslv2"]}]),
        parse_listener(<<"s2s">>, #{<<"tls">> => #{<<"protocol_options">> => [<<"nosslv2">>]}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"protocol_options">> => [<<>>]}})),
    ?err(parse_listener(<<"s2s">>, #{<<"tls">> => #{<<"protocol_options">> => [<<>>]}})),
    ?err(parse_listener(<<"c2s">>, #{<<"tls">> => #{<<"module">> => <<"just_tls">>,
                                                    <<"protocol_options">> => [<<"nosslv2">>]}})).

listen_check_from(_Config) ->
    ?cfg(listener_config(ejabberd_service, [{service_check_from, false}]),
        parse_listener(<<"service">>, #{<<"check_from">> => false})),
    ?err(parse_listener(<<"service">>, #{<<"check_from">> => 1})).

listen_hidden_components(_Config) ->
    ?cfg(listener_config(ejabberd_service, [{hidden_components, true}]),
        parse_listener(<<"service">>, #{<<"hidden_components">> => true})),
    ?err(parse_listener(<<"service">>, #{<<"hidden_components">> => <<"yes">>})).

listen_conflict_behaviour(_Config) ->
    ?cfg(listener_config(ejabberd_service, [{conflict_behaviour, kick_old}]),
        parse_listener(<<"service">>, #{<<"conflict_behaviour">> => <<"kick_old">>})),
    ?err(parse_listener(<<"service">>, #{<<"conflict_behaviour">> => <<"kill_server">>})).

listen_password(_Config) ->
    ?cfg(listener_config(ejabberd_service, [{password, "secret"}]),
        parse_listener(<<"service">>, #{<<"password">> => <<"secret">>})),
    ?err(parse_listener(<<"service">>, #{<<"password">> => <<>>})).

listen_http_num_acceptors(_Config) ->
    ?cfg(listener_config(ejabberd_cowboy, [{transport_options, [{num_acceptors, 10}]}]),
        parse_listener(<<"http">>, #{<<"transport">> => #{<<"num_acceptors">> => 10}})),
    ?err(parse_listener(<<"http">>, #{<<"transport">> => #{<<"num_acceptors">> => 0}})).

listen_http_max_connections(_Config) ->
    ?cfg(listener_config(ejabberd_cowboy, [{transport_options, [{max_connections, 100}]}]),
        parse_listener(<<"http">>, #{<<"transport">> => #{<<"max_connections">> => 100}})),
    ?cfg(listener_config(ejabberd_cowboy, [{transport_options, [{max_connections, infinity}]}]),
        parse_listener(<<"http">>, #{<<"transport">> =>
                                         #{<<"max_connections">> => <<"infinity">>}})),
    ?err(parse_listener(<<"http">>, #{<<"transport">> => #{<<"max_connections">> => -1}})).

listen_http_compress(_Config) ->
    ?cfg(listener_config(ejabberd_cowboy, [{protocol_options, [{compress, true}]}]),
        parse_listener(<<"http">>, #{<<"protocol">> => #{<<"compress">> => true}})),
    ?err(parse_listener(<<"http">>, #{<<"protocol">> => #{<<"compress">> => 0}})).

listen_http_handlers(_Config) ->
    ?cfg(listener_config(ejabberd_cowboy, [{modules, [{"_", "/http-bind", mod_bosh, []}]}]),
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
    ?cfg(listener_config(ejabberd_cowboy, [{modules, [{"localhost", "/api", mod_websockets, []}]}]),
        parse_http_handler(<<"mod_websockets">>, #{})),
    ?cfg(listener_config(ejabberd_cowboy, [{modules, [{"localhost", "/api", mod_websockets,
                                                      [{ejabberd_service, [{access, all}]}]
                                                     }]}]),
        parse_http_handler(<<"mod_websockets">>, #{<<"service">> => #{<<"access">> => <<"all">>}})),
    ?err(parse_http_handler(<<"mod_websockets">>, #{<<"service">> => <<"unbelievable">>})).

listen_http_handlers_lasse(_Config) ->
    ?cfg(listener_config(ejabberd_cowboy, [{modules, [{"localhost", "/api", lasse_handler,
                                                      [mongoose_client_api_sse]
                                                     }]}]),
        parse_http_handler(<<"lasse_handler">>, #{<<"module">> => <<"mongoose_client_api_sse">>})),
    ?err(parse_http_handler(<<"lasse_handler">>, #{<<"module">> => <<"mooongooose_api_ssie">>})),
    ?err(parse_http_handler(<<"lasse_handler">>, #{})).

listen_http_handlers_static(_Config) ->
    ?cfg(listener_config(ejabberd_cowboy, [{modules, [{"localhost", "/api", cowboy_static,
                                                      {priv_dir, cowboy_swagger, "swagger",
                                                       [{mimetypes, cow_mimetypes, all}]}
                                                     }]}]),
        parse_http_handler(<<"cowboy_static">>, #{<<"type">> => <<"priv_dir">>,
                                                  <<"app">> => <<"cowboy_swagger">>,
                                                  <<"content_path">> => <<"swagger">>})),
    ?err(parse_http_handler(<<"cowboy_static">>, #{<<"type">> => <<"priv_dir">>,
                                                   <<"app">> => <<"cowboy_swagger">>})).

listen_http_handlers_api(_Config) ->
    ?cfg(listener_config(ejabberd_cowboy, [{modules, [{"localhost", "/api", mongoose_api,
                                                      [{handlers, [mongoose_api_metrics,
                                                                   mongoose_api_users]}]}
                                                    ]}]),
        parse_http_handler(<<"mongoose_api">>, #{<<"handlers">> => [<<"mongoose_api_metrics">>,
                                                                    <<"mongoose_api_users">>]})),
    ?err(parse_http_handler(<<"mongoose_api">>, #{<<"handlers">> => [<<"not_an_api_module">>]})),
    ?err(parse_http_handler(<<"mongoose_api">>, #{})).

listen_http_handlers_domain(_Config) ->
    ?cfg(listener_config(ejabberd_cowboy,
                        [{modules, [{"localhost", "/api", mongoose_domain_handler,
                                     [{password, <<"cool">>}, {username, <<"admin">>}]
                                    }]}]),
        parse_http_handler(<<"mongoose_domain_handler">>,
                           #{<<"username">> => <<"admin">>, <<"password">> => <<"cool">>})),
    ?cfg(listener_config(ejabberd_cowboy,
                        [{modules, [{"localhost", "/api", mongoose_domain_handler,
                                     [] }]}]),
        parse_http_handler(<<"mongoose_domain_handler">>, #{})),
    %% Both username and password required. Or none.
    ?err(parse_http_handler(<<"mongoose_domain_handler">>, #{<<"username">> => <<"admin">>})),
    ?err(parse_http_handler(<<"mongoose_domain_handler">>, #{<<"password">> => <<"cool">>})).

%% tests: auth

auth_methods(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host}, value = []},
          #local_config{key = {auth_method, Host}, value = [internal, rdbms]}],
      #{<<"auth">> => #{<<"methods">> => [<<"internal">>, <<"rdbms">>]}}),
    ?errh(#{<<"auth">> => #{<<"methods">> => [<<"supernatural">>]}}).

auth_password_format(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{password_format, {scram, [sha, sha256]}}]}],
         #{<<"auth">> => #{<<"password">> => #{<<"format">> => <<"scram">>,
                                            <<"hash">> => [<<"sha">>, <<"sha256">>]}}}),
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{password_format, scram}]}],
         #{<<"auth">> => #{<<"password">> => #{<<"format">> => <<"scram">>}}}),
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{password_format, plain}]}],
         #{<<"auth">> => #{<<"password">> => #{<<"format">> => <<"plain">>}}}),

    ?errh(#{<<"auth">> => #{<<"password">> => #{<<"format">> => <<"no password">>}}}),
    ?errh(#{<<"auth">> => #{<<"password">> => #{<<"format">> => <<"scram">>,
                                                          <<"hash">> => []}}}),
    ?errh(#{<<"auth">> => #{<<"password">> => #{<<"format">> => <<"scram">>,
                                                          <<"hash">> => [<<"sha1234">>]}}}).

auth_scram_iterations(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{scram_iterations, 1000}]}],
         #{<<"auth">> => #{<<"scram_iterations">> => 1000}}),
    ?errh(#{<<"auth">> => #{<<"scram_iterations">> => false}}).

auth_sasl_external(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{cyrsasl_external, [standard,
                                                     common_name,
                                                     {mod, cyrsasl_external_verification}]
                                 }]}],
         #{<<"auth">> => #{<<"sasl_external">> =>
                               [<<"standard">>,
                             <<"common_name">>,
                                <<"cyrsasl_external_verification">>]}}),
    ?errh(#{<<"auth">> => #{<<"sasl_external">> => [<<"unknown">>]}}).

auth_sasl_mechanisms(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host}, value = []},
          #local_config{key = {sasl_mechanisms, Host},
                        value = [cyrsasl_external, cyrsasl_scram]}],
         #{<<"auth">> => #{<<"sasl_mechanisms">> => [<<"external">>, <<"scram">>]}}),
    ?errh(#{<<"auth">> => #{<<"sasl_mechanisms">> => [<<"none">>]}}).

auth_allow_multiple_connections(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host}, value = []},
          #local_config{key = {allow_multiple_connections, Host}, value = true}],
         auth_config(<<"anonymous">>, #{<<"allow_multiple_connections">> => true})),
    ?errh(auth_config(<<"anonymous">>, #{<<"allow_multiple_connections">> => <<"yes">>})).

auth_anonymous_protocol(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host}, value = []},
          #local_config{key = {anonymous_protocol, Host}, value = login_anon}],
         auth_config(<<"anonymous">>, #{<<"protocol">> => <<"login_anon">>})),
    ?errh(auth_config(<<"anonymous">>, #{<<"protocol">> => <<"none">>})).

auth_ldap_pool(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{ldap_pool_tag, ldap_pool}]}],
         auth_ldap(#{<<"pool_tag">> => <<"ldap_pool">>})),
    ?errh(auth_ldap(#{<<"pool_tag">> => <<>>})).

auth_ldap_bind_pool(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{ldap_bind_pool_tag, ldap_bind_pool}]}],
         auth_ldap(#{<<"bind_pool_tag">> => <<"ldap_bind_pool">>})),
    ?errh(auth_ldap(#{<<"bind_pool_tag">> => true})).

auth_ldap_base(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{ldap_base, "ou=Users,dc=example,dc=com"}]}],
         auth_ldap(#{<<"base">> => <<"ou=Users,dc=example,dc=com">>})),
    ?errh(auth_ldap(#{<<"base">> => 10})).

auth_ldap_uids(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{ldap_uids, [{"uid1", "user=%u"}]}]}],
         auth_ldap(#{<<"uids">> => [#{<<"attr">> => <<"uid1">>,
                                      <<"format">> => <<"user=%u">>}]})),
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{ldap_uids, ["uid1"]}]}],
         auth_ldap(#{<<"uids">> => [#{<<"attr">> => <<"uid1">>}]})),
    ?errh(auth_ldap(#{<<"uids">> => [#{<<"format">> => <<"user=%u">>}]})).

auth_ldap_filter(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{ldap_filter, "(objectClass=inetOrgPerson)"}]}],
         auth_ldap(#{<<"filter">> => <<"(objectClass=inetOrgPerson)">>})),
    ?errh(auth_ldap(#{<<"filter">> => 10})).

auth_ldap_dn_filter(_Config) ->
    Filter = #{<<"filter">> => <<"(&(name=%s)(owner=%D)(user=%u@%d))">>,
               <<"attributes">> => [<<"sn">>]},
    ?cfgh(
       [#local_config{key = {auth_opts, Host},
                      value = [{ldap_dn_filter, {"(&(name=%s)(owner=%D)(user=%u@%d))", ["sn"]}}]}],
       auth_ldap(#{<<"dn_filter">> => Filter})),
    [?errh(auth_ldap(#{<<"dn_filter">> => maps:without([K], Filter)})) ||
        K <- maps:keys(Filter)],
    ?errh(auth_ldap(#{<<"dn_filter">> => Filter#{<<"filter">> := 12}})),
    ?errh(auth_ldap(#{<<"dn_filter">> => Filter#{<<"attributes">> := <<"sn">>}})).

auth_ldap_local_filter(_Config) ->
    Filter = #{<<"operation">> => <<"equal">>,
               <<"attribute">> => <<"accountStatus">>,
               <<"values">> => [<<"enabled">>]},
    ?cfgh(
       [#local_config{key = {auth_opts, Host},
                      value = [{ldap_local_filter, {equal, {"accountStatus", ["enabled"]}}}]}],
       auth_ldap(#{<<"local_filter">> => Filter})),
    [?errh(auth_ldap(#{<<"local_filter">> => maps:without([K], Filter)})) ||
        K <- maps:keys(Filter)],
    ?errh(auth_ldap(#{<<"local_filter">> => Filter#{<<"operation">> := <<"lt">>}})),
    ?errh(auth_ldap(#{<<"local_filter">> => Filter#{<<"attribute">> := <<>>}})),
    ?errh(auth_ldap(#{<<"local_filter">> => Filter#{<<"values">> := []}})).

auth_ldap_deref(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{ldap_deref, always}]}],
         auth_ldap(#{<<"deref">> => <<"always">>})),
    ?errh(auth_ldap(#{<<"deref">> => <<"sometimes">>})).

auth_external_instances(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host}, value = []},
          #local_config{key = {extauth_instances, Host}, value = 2}],
         auth_config(<<"external">>, #{<<"instances">> => 2})),
    ?errh(auth_config(<<"external">>, #{<<"instances">> => 0})).

auth_external_program(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{extauth_program, "/usr/bin/auth"}]}],
         auth_config(<<"external">>, #{<<"program">> => <<"/usr/bin/auth">>})),
    ?errh(auth_config(<<"external">>, #{<<"program">> => <<>>})).

auth_http_basic_auth(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{basic_auth, "admin:admin123"}]}],
         auth_config(<<"http">>, #{<<"basic_auth">> => <<"admin:admin123">>})),
    ?errh(auth_config(<<"http">>, #{<<"basic_auth">> => true})).

auth_jwt(_Config) ->
    Opts = #{<<"secret">> => #{<<"value">> => <<"secret123">>},
             <<"algorithm">> => <<"HS512">>,
             <<"username_key">> => <<"user">>}, % tested together as all options are required
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{jwt_algorithm, <<"HS512">>},
                                 {jwt_secret, "secret123"},
                                 {jwt_username_key, user}]}],
         auth_config(<<"jwt">>, Opts)),
    FileOpts = Opts#{<<"secret">> := #{<<"file">> => <<"/home/user/jwt_secret">>}},
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{jwt_algorithm, <<"HS512">>},
                                 {jwt_secret_source, "/home/user/jwt_secret"},
                                 {jwt_username_key, user}]}],
         auth_config(<<"jwt">>, FileOpts)),
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{jwt_algorithm, <<"HS512">>},
                                 {jwt_secret_source, {env, "SECRET"}},
                                 {jwt_username_key, user}]}],
         auth_config(<<"jwt">>, Opts#{<<"secret">> := #{<<"env">> => <<"SECRET">>}})),
    ?errh(auth_config(<<"jwt">>, Opts#{<<"secret">> := #{<<"value">> => 123}})),
    ?errh(auth_config(<<"jwt">>, Opts#{<<"secret">> := #{<<"file">> => <<>>}})),
    ?errh(auth_config(<<"jwt">>, Opts#{<<"secret">> := #{<<"env">> => <<>>}})),
    ?errh(auth_config(<<"jwt">>, Opts#{<<"secret">> := #{<<"file">> => <<"/jwt_secret">>,
                                                         <<"env">> => <<"SECRET">>}})),
    ?errh(auth_config(<<"jwt">>, Opts#{<<"algorithm">> := <<"bruteforce">>})),
    ?errh(auth_config(<<"jwt">>, Opts#{<<"username_key">> := <<>>})),
    [?errh(auth_config(<<"jwt">>, maps:without([K], Opts))) || K <- maps:keys(Opts)].

auth_riak_bucket_type(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{bucket_type, <<"buckethead">>}]}],
         auth_config(<<"riak">>, #{<<"bucket_type">> => <<"buckethead">>})),
    ?errh(auth_config(<<"riak">>, #{<<"bucket_type">> => <<>>})).

auth_rdbms_users_number_estimate(_Config) ->
    ?cfgh([#local_config{key = {auth_opts, Host},
                        value = [{rdbms_users_number_estimate, true}]}],
         auth_config(<<"rdbms">>, #{<<"users_number_estimate">> => true})),
    ?errh(auth_config(<<"rdbms">>, #{<<"users_number_estimate">> => 1200})).

%% tests: outgoing_pools

pool_type(_Config) ->
    ?cfg(pool_config({http, global, default, [], []}),
        parse_pool(<<"http">>, <<"default">>, #{})),
    ?err(parse_pool(<<"swimming_pool">>, <<"default">>, #{})).

pool_tag(_Config) ->
    ?cfg(pool_config({http, global, my_pool, [], []}),
        parse_pool(<<"http">>, <<"my_pool">>, #{})),
    ?err(parse_pool(<<"http">>, 1000, #{})).

pool_scope(_Config) ->
    ?cfg(pool_config({http, global, default, [], []}),
        parse_pool(<<"http">>, <<"default">>, #{})),
    ?cfg(pool_config({http, global, default, [], []}),
        parse_pool(<<"http">>, <<"default">>, #{<<"scope">> => <<"global">>})),
    ?cfg(pool_config({http, host, default, [], []}),
        parse_pool(<<"http">>, <<"default">>, #{<<"scope">> => <<"host">>})),
    ?cfg(pool_config({http, <<"localhost">>, default, [], []}),
        parse_pool(<<"http">>, <<"default">>, #{<<"scope">> => <<"single_host">>,
                                                <<"host">> => <<"localhost">>})),
    ?err(parse_pool(<<"http">>, <<"default">>, #{<<"scope">> => <<"whatever">>})),
    ?err(parse_pool(<<"http">>, <<"default">>, #{<<"scope">> => <<"single_host">>})).

pool_workers(_Config) ->
    ?cfg(pool_config({http, global, default, [{workers, 10}], []}),
        parse_pool(<<"http">>, <<"default">>, #{<<"workers">> => 10})),
    ?err(parse_pool(<<"http">>, <<"default">>, #{<<"workers">> => 0})).

pool_strategy(_Config) ->
    ?cfg(pool_config({http, global, default, [{strategy, best_worker}], []}),
        parse_pool(<<"http">>, <<"default">>, #{<<"strategy">> => <<"best_worker">>})),
    ?err(parse_pool(<<"http">>, <<"default">>, #{<<"strategy">> => <<"worst_worker">>})).

pool_call_timeout(_Config) ->
    ?cfg(pool_config({http, global, default, [{call_timeout, 5000}], []}),
        parse_pool(<<"http">>, <<"default">>, #{<<"call_timeout">> => 5000})),
    ?err(parse_pool(<<"http">>, <<"default">>, #{<<"call_timeout">> => 0})).

pool_rdbms_settings(_Config) ->
    ?cfg(pool_config({rdbms, global, default, [], [{server, "DSN=mydb"}]}),
        parse_pool_conn(<<"rdbms">>, #{<<"driver">> => <<"odbc">>,
                                       <<"settings">> => <<"DSN=mydb">>})),
    ?err(parse_pool_conn(<<"rdbms">>, #{<<"driver">> => <<"mysql">>,
                                        <<"settings">> => <<"DSN=mydb">>})),
    ?err(parse_pool_conn(<<"rdbms">>, #{<<"driver">> => <<"odbc">>,
                                        <<"settings">> => true})),
    ?err(parse_pool_conn(<<"rdbms">>, #{<<"driver">> => <<"odbc">>})).

pool_rdbms_keepalive_interval(_Config) ->
    ?cfg(pool_config({rdbms, global, default, [], [{server, "DSN=mydb"},
                                                  {keepalive_interval, 1000}]}),
        parse_pool_conn(<<"rdbms">>, #{<<"driver">> => <<"odbc">>,
                                       <<"settings">> => <<"DSN=mydb">>,
                                       <<"keepalive_interval">> => 1000})),
    ?err(parse_pool_conn(<<"rdbms">>, #{<<"driver">> => <<"odbc">>,
                                        <<"settings">> => <<"DSN=mydb">>,
                                        <<"keepalive_interval">> => false})).

pool_rdbms_server(_Config) ->
    ServerOpts = rdbms_opts(),
    ?cfg(pool_config({rdbms, global, default, [],
                     [{server, {pgsql, "localhost", "db", "dbuser", "secret"}}]}),
        parse_pool_conn(<<"rdbms">>, ServerOpts)),
    ?err(parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"driver">> := <<"odbc">>})),
    [?err(parse_pool_conn(<<"rdbms">>, maps:without([K], ServerOpts))) ||
        K <- maps:keys(ServerOpts)],
    [?err(parse_pool_conn(<<"rdbms">>, ServerOpts#{K := 123})) ||
        K <- maps:keys(ServerOpts)].

pool_rdbms_port(_Config) ->
    ServerOpts = rdbms_opts(),
    ?cfg(pool_config({rdbms, global, default, [],
                     [{server, {pgsql, "localhost", 1234, "db", "dbuser", "secret"}}]}),
        parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"port">> => 1234})),
    ?err(parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"port">> => <<"airport">>})).

pool_rdbms_tls(_Config) ->
    ServerOpts = rdbms_opts(),
    ?cfg(pool_config({rdbms, global, default, [],
                     [{server, {pgsql, "localhost", "db", "dbuser", "secret",
                                [{ssl, required}]}}]}),
        parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"tls">> => #{<<"required">> => true}})),
    ?cfg(pool_config({rdbms, global, default, [],
                     [{server, {pgsql, "localhost", "db", "dbuser", "secret",
                                [{ssl, true}]}}]}),
        parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"tls">> => #{}})),
    ?cfg(pool_config({rdbms, global, default, [],
                     [{server, {mysql, "localhost", "db", "dbuser", "secret", []}}]}),
        parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"driver">> => <<"mysql">>,
                                                 <<"tls">> => #{}})),
    ?cfg(pool_config({rdbms, global, default, [],
                     [{server, {pgsql, "localhost", 1234, "db", "dbuser", "secret",
                                [{ssl, true}]}}]}),
        parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"tls">> => #{},
                                                 <<"port">> => 1234})),

    %% one option tested here as they are all checked by 'listen_tls_*' tests
    ?cfg(pool_config({rdbms, global, default, [],
                     [{server, {pgsql, "localhost", "db", "dbuser", "secret",
                                [{ssl, true}, {ssl_opts, [{certfile, "cert.pem"}]}]}}]}),
        parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"tls">> =>
                                                     #{<<"certfile">> => <<"cert.pem">>}})),
    ?err(parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"tls">> =>
                                                      #{<<"certfile">> => true}})),
    ?err(parse_pool_conn(<<"rdbms">>, ServerOpts#{<<"tls">> => <<"secure">>})).

pool_http_host(_Config) ->
    ?cfg(pool_config({http, global, default, [], [{server, "https://localhost:8443"}]}),
        parse_pool_conn(<<"http">>, #{<<"host">> => <<"https://localhost:8443">>})),
    ?err(parse_pool_conn(<<"http">>, #{<<"host">> => 8443})),
    ?err(parse_pool_conn(<<"http">>, #{<<"host">> => ""})).

pool_http_path_prefix(_Config) ->
    ?cfg(pool_config({http, global, default, [], [{path_prefix, "/"}]}),
        parse_pool_conn(<<"http">>, #{<<"path_prefix">> => <<"/">>})),
    ?err(parse_pool_conn(<<"http">>, #{<<"path_prefix">> => 8443})),
    ?err(parse_pool_conn(<<"http">>, #{<<"path_prefix">> => ""})).

pool_http_request_timeout(_Config) ->
    ?cfg(pool_config({http, global, default, [], [{request_timeout, 2000}]}),
        parse_pool_conn(<<"http">>, #{<<"request_timeout">> => 2000})),
    ?err(parse_pool_conn(<<"http">>, #{<<"request_timeout">> => -1000})),
    ?err(parse_pool_conn(<<"http">>, #{<<"request_timeout">> => <<"infinity">>})).

pool_http_tls(_Config) ->
    ?cfg(pool_config({http, global, default, [], [{http_opts, [{certfile, "cert.pem"} ]}]}),
        parse_pool_conn(<<"http">>, #{<<"tls">> => #{<<"certfile">> => <<"cert.pem">>}})),
    ?err(parse_pool_conn(<<"http">>, #{<<"tls">> => #{<<"certfile">> => true}})),
    ?err(parse_pool_conn(<<"http">>, #{<<"tls">> => <<"secure">>})).

pool_redis_host(_Config) ->
    ?cfg(pool_config({redis, global, default, [], [{host, "localhost"}]}),
        parse_pool_conn(<<"redis">>, #{<<"host">> => <<"localhost">>})),
    ?err(parse_pool_conn(<<"redis">>, #{<<"host">> => 8443})),
    ?err(parse_pool_conn(<<"redis">>, #{<<"host">> => ""})).

pool_redis_port(_Config) ->
    ?cfg(pool_config({redis, global, default, [], [{port, 6379}]}),
        parse_pool_conn(<<"redis">>, #{<<"port">> => 6379})),
    ?err(parse_pool_conn(<<"redis">>, #{<<"port">> => 666666})),
    ?err(parse_pool_conn(<<"redis">>, #{<<"port">> => <<"airport">>})).

pool_redis_database(_Config) ->
    ?cfg(pool_config({redis, global, default, [], [{database, 0}]}),
        parse_pool_conn(<<"redis">>, #{<<"database">> => 0})),
    ?err(parse_pool_conn(<<"redis">>, #{<<"database">> => -1})),
    ?err(parse_pool_conn(<<"redis">>, #{<<"database">> => <<"my_database">>})).

pool_redis_password(_Config) ->
    ?cfg(pool_config({redis, global, default, [], [{password, ""}]}),
        parse_pool_conn(<<"redis">>, #{<<"password">> => <<"">>})),
    ?cfg(pool_config({redis, global, default, [], [{password, "password1"}]}),
        parse_pool_conn(<<"redis">>, #{<<"password">> => <<"password1">>})),
    ?err(parse_pool_conn(<<"redis">>, #{<<"password">> => 0})).

pool_riak_address(_Config) ->
    ?cfg(pool_config({riak, global, default, [], [{address, "127.0.0.1"}]}),
        parse_pool_conn(<<"riak">>, #{<<"address">> => <<"127.0.0.1">>})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"address">> => 66})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"address">> => <<"">>})).

pool_riak_port(_Config) ->
    ?cfg(pool_config({riak, global, default, [], [{port, 8087}]}),
        parse_pool_conn(<<"riak">>, #{<<"port">> => 8087})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"port">> => 666666})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"port">> => <<"airport">>})).

pool_riak_credentials(_Config) ->
    ?cfg(pool_config({riak, global, default, [], [{credentials, "user", "pass"}]}),
        parse_pool_conn(<<"riak">>, #{<<"credentials">> =>
            #{<<"user">> => <<"user">>, <<"password">> => <<"pass">>}})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"credentials">> => #{<<"user">> => <<"user">>}})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"credentials">> =>
                                           #{<<"user">> => <<"">>, <<"password">> => 011001}})).

pool_riak_cacertfile(_Config) ->
    ?cfg(pool_config({riak, global, default, [], [{cacertfile, "cacert.pem"}]}),
        parse_pool_conn(<<"riak">>, #{<<"tls">> => #{<<"cacertfile">> => <<"cacert.pem">>}})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"cacertfile">> => <<"">>})).

pool_riak_tls(_Config) ->
    %% make sure these options are not extracted out of 'ssl_opts'
    %% all the TLS options are checked by 'listen_tls_*' tests
    ?cfg(pool_config({riak, global, default, [], [{ssl_opts, [{certfile, "path/to/cert.pem"},
                                                             {dhfile, "cert.pem"},
                                                             {keyfile, "path/to/key.pem"}]}]}),
        parse_pool_conn(<<"riak">>, #{<<"tls">> => #{<<"certfile">> => <<"path/to/cert.pem">>,
                                                     <<"dhfile">> => <<"cert.pem">>,
                                                     <<"keyfile">> => <<"path/to/key.pem">>}})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"tls">> => #{<<"dhfile">> => true}})),
    ?err(parse_pool_conn(<<"riak">>, #{<<"tls">> => <<"secure">>})).

pool_cassandra_servers(_Config) ->
    ?cfg(pool_config({cassandra, global, default, [],
        [{servers, [{"cassandra_server1.example.com", 9042},
                    {"cassandra_server2.example.com", 9042}]}]}),
        parse_pool_conn(<<"cassandra">>, #{<<"servers">> => [
            #{<<"ip_address">> => <<"cassandra_server1.example.com">>, <<"port">> => 9042},
            #{<<"ip_address">> => <<"cassandra_server2.example.com">>, <<"port">> => 9042}
            ]})),
    ?err(parse_pool_conn(<<"cassandra">>, #{<<"servers">> =>
        #{<<"ip_address">> => <<"cassandra_server1.example.com">>, <<"port">> => 9042}})).

pool_cassandra_keyspace(_Config) ->
    ?cfg(pool_config({cassandra, global, default, [], [{keyspace, "big_mongooseim"}]}),
        parse_pool_conn(<<"cassandra">>, #{<<"keyspace">> => <<"big_mongooseim">>})),
    ?err(parse_pool_conn(<<"cassandra">>, #{<<"keyspace">> => <<"">>})).

pool_cassandra_auth(_Config) ->
    ?cfg(pool_config({cassandra, global, default, [], [{auth, {cqerl_auth_plain_handler,
                                                              [{<<"auser">>, <<"secretpass">>}]
                                                             }}]}),
        parse_pool_conn(<<"cassandra">>,
                        #{<<"auth">> => #{<<"plain">> => #{<<"username">> => <<"auser">>,
                                                           <<"password">> => <<"secretpass">>}}})),
    ?err(parse_pool_conn(<<"cassandra">>, #{<<"tls">> => #{<<"verify">> => <<"verify_none">>}})).

pool_cassandra_tls(_Config) ->
    %% one option tested here as they are all checked by 'listen_tls_*' tests
    ?cfg(pool_config({cassandra, global, default, [], [{ssl, [{verify, verify_none}
        ]}]}),
        parse_pool_conn(<<"cassandra">>, #{<<"tls">> => #{<<"verify_peer">> => false}})),
    ?err(parse_pool_conn(<<"cassandra">>, #{<<"tls">> => #{<<"verify">> => <<"verify_none">>}})).

pool_elastic_host(_Config) ->
    ?cfg(pool_config({elastic, global, default, [], [{host, "localhost"}]}),
        parse_pool_conn(<<"elastic">>, #{<<"host">> => <<"localhost">>})),
    ?err(parse_pool_conn(<<"elastic">>, #{<<"host">> => <<"">>})).

pool_elastic_port(_Config) ->
    ?cfg(pool_config({elastic, global, default, [], [{port, 9200}]}),
        parse_pool_conn(<<"elastic">>, #{<<"port">> => 9200})),
    ?err(parse_pool_conn(<<"elastic">>, #{<<"port">> => 122333})),
    ?err(parse_pool_conn(<<"elastic">>, #{<<"port">> => <<"airport">>})).

pool_rabbit_amqp_host(_Config) ->
    ?cfg(pool_config({rabbit, global, default, [], [{amqp_host, "localhost"}]}),
        parse_pool_conn(<<"rabbit">>, #{<<"amqp_host">> => <<"localhost">>})),
    ?err(parse_pool_conn(<<"rabbit">>, #{<<"amqp_host">> => <<"">>})).

pool_rabbit_amqp_port(_Config) ->
    ?cfg(pool_config({rabbit, global, default, [], [{amqp_port, 5672}]}),
        parse_pool_conn(<<"rabbit">>, #{<<"amqp_port">> => 5672})),
    ?err(parse_pool_conn(<<"rabbit">>, #{<<"amqp_port">> => <<"airport">>})).

pool_rabbit_amqp_username(_Config) ->
    ?cfg(pool_config({rabbit, global, default, [], [{amqp_username, "guest"}]}),
        parse_pool_conn(<<"rabbit">>, #{<<"amqp_username">> => <<"guest">>})),
    ?err(parse_pool_conn(<<"rabbit">>, #{<<"amqp_username">> => <<"">>})).

pool_rabbit_amqp_password(_Config) ->
    ?cfg(pool_config({rabbit, global, default, [], [{amqp_password, "guest"}]}),
        parse_pool_conn(<<"rabbit">>, #{<<"amqp_password">> => <<"guest">>})),
    ?err(parse_pool_conn(<<"rabbit">>, #{<<"amqp_password">> => <<"">>})).

pool_rabbit_amqp_confirms_enabled(_Config) ->
    ?cfg(pool_config({rabbit, global, default, [], [{confirms_enabled, true}]}),
        parse_pool_conn(<<"rabbit">>, #{<<"confirms_enabled">> => true})),
    ?err(parse_pool_conn(<<"rabbit">>, #{<<"confirms_enabled">> => <<"yes">>})).

pool_rabbit_amqp_max_worker_queue_len(_Config) ->
    ?cfg(pool_config({rabbit, global, default, [], [{max_worker_queue_len, 100}]}),
        parse_pool_conn(<<"rabbit">>, #{<<"max_worker_queue_len">> => 100})),
    ?err(parse_pool_conn(<<"rabbit">>, #{<<"max_worker_queue_len">> => 0})).

pool_ldap_host(_Config) ->
    ?cfg(pool_config({ldap, global, default, [], [{host, "localhost"}]}),
        parse_pool_conn(<<"ldap">>, #{<<"host">> => <<"localhost">>})),
    ?err(parse_pool_conn(<<"ldap">>, #{<<"host">> => <<"">>})).

pool_ldap_port(_Config) ->
    ?cfg(pool_config({ldap, global, default, [], [{port, 389}]}),
        parse_pool_conn(<<"ldap">>, #{<<"port">> => 389})),
    ?err(parse_pool_conn(<<"ldap">>, #{<<"port">> => <<"airport">>})).

pool_ldap_servers(_Config) ->
    ?cfg(pool_config({ldap, global, default, [],
        [{servers, ["primary-ldap-server.example.com", "secondary-ldap-server.example.com"]}]}),
        parse_pool_conn(<<"ldap">>, #{<<"servers">> =>
            [<<"primary-ldap-server.example.com">>, <<"secondary-ldap-server.example.com">>]})),
    ?err(parse_pool_conn(<<"ldap">>, #{<<"servers">> => #{<<"server">> => <<"example.com">>}})).

pool_ldap_encrypt(_Config) ->
    ?cfg(pool_config({ldap, global, default, [], [{encrypt, none}]}),
        parse_pool_conn(<<"ldap">>, #{<<"encrypt">> => <<"none">>})),
    ?err(parse_pool_conn(<<"ldap">>, #{<<"encrypt">> => true})).

pool_ldap_rootdn(_Config) ->
    ?cfg(pool_config({ldap, global, default, [], [{rootdn, ""}]}),
        parse_pool_conn(<<"ldap">>, #{<<"rootdn">> => <<"">>})),
    ?err(parse_pool_conn(<<"ldap">>, #{<<"rootdn">> => false})).

pool_ldap_password(_Config) ->
    ?cfg(pool_config({ldap, global, default, [], [{password, "pass"}]}),
        parse_pool_conn(<<"ldap">>, #{<<"password">> => <<"pass">>})),
    ?err(parse_pool_conn(<<"ldap">>, #{<<"password">> => true})).

pool_ldap_connect_interval(_Config) ->
    ?cfg(pool_config({ldap, global, default, [], [{connect_interval, 10000}]}),
        parse_pool_conn(<<"ldap">>, #{<<"connect_interval">> => 10000})),
    ?err(parse_pool_conn(<<"ldap">>, #{<<"connect_interval">> => <<"infinity">>})).

pool_ldap_tls(_Config) ->
    %% one option tested here as they are all checked by 'listen_tls_*' tests
    ?cfg(pool_config({ldap, global, default, [], [{tls_options, [{verify, verify_peer}
        ]}]}),
        parse_pool_conn(<<"ldap">>, #{<<"tls">> => #{<<"verify_peer">> => true}})),
    ?err(parse_pool_conn(<<"ldap">>, #{<<"tls">> => #{<<"verify">> => <<"verify_none">>}})).

%% tests: shaper, acl, access
shaper(_Config) ->
    ?cfgh([#local_config{key = {shaper, normal, Host}, value = {maxrate, 1000}}],
         #{<<"shaper">> => #{<<"normal">> => #{<<"max_rate">> => 1000}}}),
    ?errh(#{<<"shaper">> => #{<<"unlimited">> => #{<<"max_rate">> => <<"infinity">>}}}),
    ?errh(#{<<"shaper">> => #{<<"fast">> => #{}}}).

acl(_Config) ->
    ?cfgh([#local_config{key = {acl, local, Host}, value = [all]}],
         #{<<"acl">> => #{<<"local">> => [#{<<"match">> => <<"all">>}]}}),
    ?cfgh([#local_config{key = {acl, local, Host}, value = [{user_regexp, <<>>}]}],
         #{<<"acl">> => #{<<"local">> => [#{<<"user_regexp">> => <<>>}]}}),
    ?cfgh([#local_config{key = {acl, alice, Host},
                        value = [{node_regexp, <<"ali.*">>, <<".*host">>}]}],
         #{<<"acl">> => #{<<"alice">> => [#{<<"user_regexp">> => <<"ali.*">>,
                                            <<"server_regexp">> => <<".*host">>}]}}),
    ?cfgh([#local_config{key = {acl, alice, Host},
                        value = [{user, <<"alice">>, <<"localhost">>}]}],
         #{<<"acl">> => #{<<"alice">> => [#{<<"user">> => <<"alice">>,
                                            <<"server">> => <<"localhost">>}]}}),
    ?errh(#{<<"acl">> => #{<<"local">> => <<"everybody">>}}),
    ?errh(#{<<"acl">> => #{<<"alice">> => [#{<<"user_glob">> => <<"a*">>,
                                             <<"server_blog">> => <<"bloghost">>}]}}).

access(_Config) ->
    ?cfgh([#local_config{key = {access, c2s, Host}, value = [{deny, blocked},
                                                            {allow, all}]}],
         #{<<"access">> => #{<<"c2s">> => [#{<<"acl">> => <<"blocked">>,
                                             <<"value">> => <<"deny">>},
                                           #{<<"acl">> => <<"all">>,
                                             <<"value">> => <<"allow">>}]}}),
    ?cfgh([#local_config{key = {access, max_user_sessions, Host},
                        value = [{10, all}]}],
         #{<<"access">> => #{<<"max_user_sessions">> => [#{<<"acl">> => <<"all">>,
                                                           <<"value">> => 10}]}}),
    ?errh(#{<<"access">> => #{<<"max_user_sessions">> => [#{<<"acl">> => <<"all">>}]}}),
    ?errh(#{<<"access">> => #{<<"max_user_sessions">> => [#{<<"value">> => 10}]}}),
    ?errh(#{<<"access">> => #{<<"max_user_sessions">> => [#{<<"acl">> => 10,
                                                            <<"value">> => 10}]}}).

%% tests: s2s

s2s_dns_timeout(_Config) ->
    ?cfg([#local_config{key = s2s_dns_options, value = [{timeout, 5}]}],
        parse(#{<<"s2s">> => #{<<"dns">> => #{<<"timeout">> => 5}}})),
    ?err(parse(#{<<"s2s">> => #{<<"dns">> => #{<<"timeout">> => 0}}})).

s2s_dns_retries(_Config) ->
    ?cfg([#local_config{key = s2s_dns_options, value = [{retries, 1}]}],
        parse(#{<<"s2s">> => #{<<"dns">> => #{<<"retries">> => 1}}})),
    ?err(parse(#{<<"s2s">> => #{<<"dns">> => #{<<"retries">> => 0}}})).

s2s_outgoing_port(_Config) ->
    ?cfg([#local_config{key = outgoing_s2s_port, value = 5270}],
        parse(#{<<"s2s">> => #{<<"outgoing">> => #{<<"port">> => 5270}}})),
    ?err(parse(#{<<"s2s">> => #{<<"outgoing">> => #{<<"port">> => <<"http">>}}})).

s2s_outgoing_ip_versions(_Config) ->
    ?cfg([#local_config{key = outgoing_s2s_families, value = [ipv6, ipv4]}],
        parse(#{<<"s2s">> => #{<<"outgoing">> => #{<<"ip_versions">> => [6, 4]}}})),
    ?err(parse(#{<<"s2s">> => #{<<"outgoing">> => #{<<"ip_versions">> => []}}})),
    ?err(parse(#{<<"s2s">> => #{<<"outgoing">> => #{<<"ip_versions">> => [<<"http">>]}}})).

s2s_outgoing_timeout(_Config) ->
    ?cfg([#local_config{key = outgoing_s2s_timeout, value = 5}],
        parse(#{<<"s2s">> => #{<<"outgoing">> => #{<<"connection_timeout">> => 5}}})),
    ?cfg([#local_config{key = outgoing_s2s_timeout, value = infinity}],
        parse(#{<<"s2s">> => #{<<"outgoing">> => #{<<"connection_timeout">> => <<"infinity">>}}})),
    ?err(parse(#{<<"s2s">> => #{<<"outgoing">> => #{<<"connection_timeout">> => 0}}})).

s2s_use_starttls(_Config) ->
    ?cfg([#local_config{key = s2s_use_starttls, value = required}],
        parse(#{<<"s2s">> => #{<<"use_starttls">> => <<"required">>}})),
    ?err(parse(#{<<"s2s">> => #{<<"use_starttls">> => <<"unnecessary">>}})).

s2s_certfile(_Config) ->
    ?cfg([#local_config{key = s2s_certfile, value = "cert.pem"}],
        parse(#{<<"s2s">> => #{<<"certfile">> => <<"cert.pem">>}})),
    ?err(parse(#{<<"s2s">> => #{<<"certfile">> => []}})).

s2s_default_policy(_Config) ->
    ?cfgh([#local_config{key = {s2s_default_policy, Host}, value = deny}],
         #{<<"s2s">> => #{<<"default_policy">> => <<"deny">>}}),
    ?errh(#{<<"s2s">> => #{<<"default_policy">> => <<"ask">>}}).

s2s_host_policy(_Config) ->
    Policy = #{<<"host">> => <<"host1">>,
               <<"policy">> => <<"allow">>},
    ?cfgh([#local_config{key = {{s2s_host, <<"host1">>}, Host}, value = allow}],
         #{<<"s2s">> => #{<<"host_policy">> => [Policy]}}),
    ?cfgh([#local_config{key = {{s2s_host, <<"host1">>}, Host}, value = allow},
          #local_config{key = {{s2s_host, <<"host2">>}, Host}, value = deny}],
         #{<<"s2s">> => #{<<"host_policy">> => [Policy, #{<<"host">> => <<"host2">>,
                                                          <<"policy">> => <<"deny">>}]}}),
    ?errh(#{<<"s2s">> => #{<<"host_policy">> => [maps:without([<<"host">>], Policy)]}}),
    ?errh(#{<<"s2s">> => #{<<"host_policy">> => [maps:without([<<"policy">>], Policy)]}}),
    ?errh(#{<<"s2s">> => #{<<"host_policy">> => [Policy#{<<"host">> => <<>>}]}}),
    ?errh(#{<<"s2s">> => #{<<"host_policy">> => [Policy#{<<"policy">> => <<"huh">>}]}}),
    ?errh(#{<<"s2s">> => #{<<"host_policy">> => [Policy,
                                                 Policy#{<<"policy">> => <<"deny">>}]}}).

s2s_address(_Config) ->
    Addr = #{<<"host">> => <<"host1">>,
             <<"ip_address">> => <<"192.168.1.2">>,
             <<"port">> => 5321},
    ?cfg([#local_config{key = {s2s_addr, <<"host1">>}, value = {"192.168.1.2", 5321}}],
        parse(#{<<"s2s">> => #{<<"address">> => [Addr]}})),
    ?cfg([#local_config{key = {s2s_addr, <<"host1">>}, value = "192.168.1.2"}],
        parse(#{<<"s2s">> => #{<<"address">> => [maps:without([<<"port">>], Addr)]}})),
    ?err(parse(#{<<"s2s">> => #{<<"address">> => [maps:without([<<"host">>], Addr)]}})),
    ?err(parse(#{<<"s2s">> => #{<<"address">> => [maps:without([<<"ip_address">>], Addr)]}})),
    ?err(parse(#{<<"s2s">> => #{<<"address">> => [Addr#{<<"host">> => <<>>}]}})),
    ?err(parse(#{<<"s2s">> => #{<<"address">> => [Addr#{<<"ip_address">> => <<"host2">>}]}})),
    ?err(parse(#{<<"s2s">> => #{<<"address">> => [Addr#{<<"port">> => <<"seaport">>}]}})),
    ?err(parse(#{<<"s2s">> => #{<<"address">> => [Addr, maps:remove(<<"port">>, Addr)]}})).

s2s_ciphers(_Config) ->
    ?cfg([#local_config{key = s2s_ciphers, value = "TLSv1.2:TLSv1.3"}],
        parse(#{<<"s2s">> => #{<<"ciphers">> => <<"TLSv1.2:TLSv1.3">>}})),
    ?err(parse(#{<<"s2s">> => #{<<"ciphers">> => [<<"cipher1">>, <<"cipher2">>]}})).

s2s_domain_certfile(_Config) ->
    DomCert = #{<<"domain">> => <<"myxmpp.com">>,
                <<"certfile">> => <<"mycert.pem">>},
    ?cfg([#local_config{key = {domain_certfile, "myxmpp.com"}, value = "mycert.pem"}],
        parse(#{<<"s2s">> => #{<<"domain_certfile">> => [DomCert]}})),
    [?err(parse(#{<<"s2s">> => #{<<"domain_certfile">> => [maps:without([K], DomCert)]}}))
     || K <- maps:keys(DomCert)],
    [?err(parse(#{<<"s2s">> => #{<<"domain_certfile">> => [DomCert#{K := <<>>}]}}))
     || K <- maps:keys(DomCert)],
    ?err(parse(#{<<"s2s">> => #{<<"domain_certfile">> => [DomCert, DomCert]}})).

s2s_shared(_Config) ->
    ?cfgh([#local_config{key = {s2s_shared, Host}, value = <<"secret">>}],
         #{<<"s2s">> => #{<<"shared">> => <<"secret">>}}),
    ?errh(#{<<"s2s">> => #{<<"shared">> => 536837}}).

s2s_max_retry_delay(_Config) ->
    ?cfgh([#local_config{key = {s2s_max_retry_delay, Host}, value = 120}],
         #{<<"s2s">> => #{<<"max_retry_delay">> => 120}}),
    ?errh(#{<<"s2s">> => #{<<"max_retry_delay">> => 0}}).

%% modules

mod_adhoc(_Config) ->
    check_iqdisc(mod_adhoc),
    M = fun(Host, K, V) -> modopts(Host, mod_adhoc, [{K, V}]) end,
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_adhoc">> => #{K => V}}} end,
    %% report_commands_node is boolean
    ?cfgh(M(Host, report_commands_node, true), T(<<"report_commands_node">>, true)),
    ?cfgh(M(Host, report_commands_node, false), T(<<"report_commands_node">>, false)),
    %% not boolean
    ?errh(T(<<"report_commands_node">>, <<"hello">>)).

mod_auth_token(_Config) ->
    check_iqdisc(mod_auth_token),
    P = fun(X) ->
                Opts = #{<<"validity_period">> => X},
                #{<<"modules">> => #{<<"mod_auth_token">> => Opts}}
        end,
    ?cfgh(modopts(Host, mod_auth_token, [{{validity_period, access}, {13, minutes}},
                                        {{validity_period, refresh}, {31, days}}]),
         P([#{<<"token">> => <<"access">>, <<"value">> => 13, <<"unit">> => <<"minutes">>},
            #{<<"token">> => <<"refresh">>, <<"value">> => 31, <<"unit">> => <<"days">>}])),
    ?errh(P([#{<<"token">> => <<"access">>, <<"value">> => <<"13">>,
               <<"unit">> => <<"minutes">>}])),
    ?errh(P([#{<<"token">> => <<"access">>, <<"value">> => 13, <<"unit">> => <<"minute">>}])),
    ?errh(P([#{<<"token">> => <<"Access">>, <<"value">> => 13, <<"unit">> => <<"minutes">>}])),
    ?errh(P([#{<<"value">> => 13, <<"unit">> => <<"minutes">>}])),
    ?errh(P([#{<<"token">> => <<"access">>, <<"unit">> => <<"minutes">>}])),
    ?errh(P([#{<<"token">> => <<"access">>, <<"value">> => 13}])).

mod_bosh(_Config) ->
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_bosh">> => #{K => V}}} end,
    M = fun(Host, K, V) -> modopts(Host, mod_bosh, [{K, V}]) end,
    ?cfgh(M(Host, inactivity, 10), T(<<"inactivity">>, 10)),
    ?cfgh(M(Host, inactivity, infinity), T(<<"inactivity">>, <<"infinity">>)),
    ?cfgh(M(Host, inactivity, 10), T(<<"inactivity">>, 10)),
    ?cfgh(M(Host, max_wait, infinity), T(<<"max_wait">>, <<"infinity">>)),
    ?cfgh(M(Host, server_acks, true), T(<<"server_acks">>, true)),
    ?cfgh(M(Host, server_acks, false), T(<<"server_acks">>, false)),
    ?cfgh(M(Host, maxpause, 10), T(<<"max_pause">>, 10)),
    ?errh(T(<<"inactivity">>, -1)),
    ?errh(T(<<"inactivity">>, <<"10">>)),
    ?errh(T(<<"inactivity">>, <<"inactivity">>)),
    ?errh(T(<<"max_wait">>, <<"10">>)),
    ?errh(T(<<"max_wait">>, -1)),
    ?errh(T(<<"server_acks">>, -1)),
    ?errh(T(<<"maxpause">>, 0)).

mod_caps(_Config) ->
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_caps">> => #{K => V}}} end,
    M = fun(Host, K, V) -> modopts(Host, mod_caps, [{K, V}]) end,
    ?cfgh(M(Host, cache_size, 10), T(<<"cache_size">>, 10)),
    ?cfgh(M(Host, cache_life_time, 10), T(<<"cache_life_time">>, 10)),
    ?errh(T(<<"cache_size">>, 0)),
    ?errh(T(<<"cache_size">>, <<"infinity">>)),
    ?errh(T(<<"cache_life_time">>, 0)),
    ?errh(T(<<"cache_life_time">>, <<"infinity">>)).

mod_cache_users(_Config) ->
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_cache_users">> => #{K => V}}} end,
    M = fun(Host, K, V) -> modopts(Host, mod_cache_users, [{K, V}]) end,
    ?cfgh(M(Host, time_to_live, 8600), T(<<"time_to_live">>, 8600)),
    ?cfgh(M(Host, time_to_live, infinity), T(<<"time_to_live">>, <<"infinity">>)),
    ?cfgh(M(Host, number_of_segments, 10), T(<<"number_of_segments">>, 10)),
    ?cfgh(M(Host, strategy, fifo), T(<<"strategy">>, <<"fifo">>)),
    ?errh(T(<<"time_to_live">>, 0)),
    ?errh(T(<<"strategy">>, <<"lifo">>)),
    ?errh(T(<<"number_of_segments">>, 0)),
    ?errh(T(<<"number_of_segments">>, <<"infinity">>)).

mod_carboncopy(_Config) ->
    check_iqdisc(mod_carboncopy).

mod_csi(_Config) ->
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_csi">> => #{K => V}}} end,
    M = fun(Host, K, V) -> modopts(Host, mod_csi, [{K, V}]) end,
    ?cfgh(M(Host, buffer_max, 10), T(<<"buffer_max">>, 10)),
    ?cfgh(M(Host, buffer_max, infinity), T(<<"buffer_max">>, <<"infinity">>)),
    ?errh(T(<<"buffer_max">>, -1)).

mod_disco(_Config) ->
    check_iqdisc(mod_disco),
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_disco">> => #{K => V}}} end,
    ?cfgh(modopts(Host, mod_disco, [{users_can_see_hidden_services, true}]),
         T(<<"users_can_see_hidden_services">>, true)),
    ?cfgh(modopts(Host, mod_disco, [{users_can_see_hidden_services, false}]),
         T(<<"users_can_see_hidden_services">>, false)),
    %% extra_domains are binaries
    ?cfgh(modopts(Host, mod_disco, [{extra_domains, [<<"localhost">>, <<"erlang-solutions.com">>]}]),
         T(<<"extra_domains">>, [<<"localhost">>, <<"erlang-solutions.com">>])),
    ?cfgh(modopts(Host, mod_disco, [{extra_domains, []}]),
         T(<<"extra_domains">>, [])),
    Info = #{<<"name">> => <<"abuse-address">>,
             <<"urls">> => [<<"admin@example.com">>]},
    SpiritUrls = [<<"spirit1@localhost">>, <<"spirit2@localhost">>],
    ?cfgh(modopts(Host, mod_disco, [{server_info, [[{name, <<"abuse-address">>},
                                                   {urls, [<<"admin@example.com">>]}],
                                                  [{modules, [mod_muc, mod_disco]},
                                                   {name, <<"friendly-spirits">>},
                                                   {urls, SpiritUrls}]
                                                 ]}
                                  ]),
         T(<<"server_info">>, [Info, #{<<"modules">> => [<<"mod_muc">>, <<"mod_disco">>],
                                       <<"name">> => <<"friendly-spirits">>,
                                       <<"urls">> => SpiritUrls}
                              ])),
    ?errh(T(<<"users_can_see_hidden_services">>, 1)),
    ?errh(T(<<"users_can_see_hidden_services">>, <<"true">>)),
    ?errh(T(<<"extra_domains">>, [<<"user@localhost">>])),
    ?errh(T(<<"extra_domains">>, [1])),
    ?errh(T(<<"extra_domains">>, <<"domains domains domains">>)),
    ?errh(T(<<"server_info">>, [Info#{<<"name">> => 1}])),
    ?errh(T(<<"server_info">>, [Info#{<<"name">> => <<"">>}])),
    ?errh(T(<<"server_info">>, [Info#{<<"modules">> => <<"roll">>}])),
    ?errh(T(<<"server_info">>, [Info#{<<"modules">> => [<<"meow_meow_meow">>]}])),
    ?errh(T(<<"server_info">>, [Info#{<<"urls">> => [1]}])),
    ?errh(T(<<"server_info">>, [Info#{<<"urls">> => [<<"">>]}])),
    ?errh(T(<<"server_info">>, [maps:remove(<<"name">>, Info)])),
    ?errh(T(<<"server_info">>, [maps:remove(<<"urls">>, Info)])).

mod_extdisco(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                         #{<<"mod_extdisco">> =>
                             #{<<"service">> => [Opts]}}}
        end,
    M = fun(Host, Opts) -> modopts(Host, mod_extdisco, [Opts]) end,
    RequiredOpts = #{
        <<"type">> => <<"stun">>,
        <<"host">> => <<"stun1">>},
    ExpectedCfg = [{host, "stun1"},
                   {type, stun}],
    ?cfgh(M(Host, ExpectedCfg), T(RequiredOpts)),
    ?cfgh(M(Host, ExpectedCfg ++ [{port, 3478}]),
         T(RequiredOpts#{<<"port">> => 3478})),
    ?cfgh(M(Host, ExpectedCfg ++ [{transport, "udp"}]),
         T(RequiredOpts#{<<"transport">> => <<"udp">>})),
    ?cfgh(M(Host, ExpectedCfg ++ [{username, "username"}]),
         T(RequiredOpts#{<<"username">> => <<"username">>})),
    ?cfgh(M(Host, ExpectedCfg ++ [{password, "password"}]),
         T(RequiredOpts#{<<"password">> => <<"password">>})),
    [?errh(T(maps:remove(Key, RequiredOpts))) || Key <- maps:keys(RequiredOpts)],
    [?errh(T(RequiredOpts#{Key => 1})) || Key <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"type">> => <<"">>})),
    ?errh(T(RequiredOpts#{<<"host">> => <<"">>})),
    ?errh(T(RequiredOpts#{<<"port">> => -1})),
    ?errh(T(RequiredOpts#{<<"transport">> => <<"">>})),
    ?errh(T(RequiredOpts#{<<"username">> => <<"">>})),
    ?errh(T(RequiredOpts#{<<"password">> => <<"">>})).

mod_inbox(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_inbox">> => Opts}} end,
    M = fun(Host, Opts) -> modopts(Host, mod_inbox, Opts) end,
    ChatMarkers = [<<"displayed">>, <<"received">>, <<"acknowledged">>],
    ?cfgh(M(Host, [{reset_markers, ChatMarkers}]),
         T(#{<<"reset_markers">> => ChatMarkers})),
    ?cfgh(M(Host, [{groupchat, [muc, muclight]}]),
         T(#{<<"groupchat">> => [<<"muc">>, <<"muclight">>]})),
    ?cfgh(M(Host, [{aff_changes, true}]),
         T(#{<<"aff_changes">> => true})),
    ?cfgh(M(Host, [{remove_on_kicked, false}]),
         T(#{<<"remove_on_kicked">> => false})),
    ?errh(T(#{<<"reset_markers">> => 1})),
    ?errh(T(#{<<"reset_markers">> => [<<"destroyed">>]})),
    ?errh(T(#{<<"groupchat">> => [<<"test">>]})),
    ?errh(T(#{<<"aff_changes">> => 1})),
    ?errh(T(#{<<"remove_on_kicked">> => 1})),
    check_iqdisc(mod_inbox).

mod_global_distrib(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_global_distrib">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_global_distrib, Cfg) end,
    RequiredOpts = global_distrib_required_opts(),
    ExpectedCfg = global_distrib_expected_config(),
    ?cfgh(M(Host, ExpectedCfg), T(RequiredOpts)),
    ?cfgh(M(Host, ExpectedCfg ++ [{message_ttl, 42}]),
         T(RequiredOpts#{<<"message_ttl">> => 42})),
    ?cfgh(M(Host, ExpectedCfg ++ [{hosts_refresh_interval, 100}]),
         T(RequiredOpts#{<<"hosts_refresh_interval">> => 100})),
    [?errh(T(maps:remove(Key, RequiredOpts))) || Key <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"global_host">> => <<"">>})),
    ?errh(T(RequiredOpts#{<<"local_host">> => <<"">>})),
    ?errh(T(RequiredOpts#{<<"message_ttl">> => -1})),
    ?errh(T(RequiredOpts#{<<"hosts_refresh_interval">> => -1})).

mod_global_distrib_connections(_Config) ->
    RequiredOpts = global_distrib_required_opts(),
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_global_distrib">> =>
                                 RequiredOpts#{<<"connections">> => Opts}}}
        end,
    M = fun(Host, Cfg) -> modopts(Host, mod_global_distrib,
                            global_distrib_expected_config() ++ [{connections, Cfg}])
        end,
    ?cfgh(M(Host, []), T(#{})),
    ?cfgh(M(Host, [{connections_per_endpoint, 22}]),
         T(#{<<"connections_per_endpoint">> => 22})),
    ?cfgh(M(Host, [{endpoint_refresh_interval, 120}]),
         T(#{<<"endpoint_refresh_interval">> => 120})),
    ?cfgh(M(Host, [{endpoint_refresh_interval_when_empty, 5}]),
         T(#{<<"endpoint_refresh_interval_when_empty">> => 5})),
    ?cfgh(M(Host, [{disabled_gc_interval, 60}]),
         T(#{<<"disabled_gc_interval">> => 60})),
    ?errh(T(#{<<"connections_per_endpoint">> => -1})),
    ?errh(T(#{<<"endpoint_refresh_interval">> => 0})),
    ?errh(T(#{<<"endpoint_refresh_interval_when_empty">> => 0})),
    ?errh(T(#{<<"disabled_gc_interval">> => 0})).

mod_global_distrib_connections_endpoints(_Config) ->
    check_mod_global_distrib_endpoints(<<"endpoints">>).

mod_global_distrib_connections_advertised_endpoints(_Config) ->
    check_mod_global_distrib_endpoints(<<"advertised_endpoints">>).

check_mod_global_distrib_endpoints(OptKey) ->
    CfgKey = binary_to_atom(OptKey, utf8),
    RequiredModOpts = global_distrib_required_opts(),
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_global_distrib">> =>
                                 RequiredModOpts#{<<"connections">> => #{OptKey => Opts}}}}
        end,
    M = fun(Host, Cfg) -> modopts(Host, mod_global_distrib,
                                  global_distrib_expected_config() ++
                                      [{connections, [{CfgKey, Cfg}]}])
        end,
    RequiredOpts = #{<<"host">> => <<"172.16.0.2">>,
                     <<"port">> => 5555},
    ?cfgh(M(Host, [{"172.16.0.2", 5555}]), T([RequiredOpts])),
    [?errh(T(maps:remove(Key, RequiredOpts))) || Key <- maps:keys(RequiredOpts)],
    ?errh(T([RequiredOpts#{<<"host">> => <<>>}])),
    ?errh(T([RequiredOpts#{<<"port">> => -1}])).

mod_global_distrib_connections_tls(_Config) ->
    RequiredModOpts = global_distrib_required_opts(),
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_global_distrib">> =>
                                 RequiredModOpts#{<<"connections">> => #{<<"tls">> => Opts}}}}
        end,
    M = fun(Host, Cfg) -> modopts(Host, mod_global_distrib,
                                  global_distrib_expected_config() ++
                                      [{connections, [{tls_opts, Cfg}]}])
        end,
    RequiredOpts = #{<<"certfile">> => <<"priv/cert.pem">>,
                     <<"cacertfile">> => <<"priv/ca.pem">>},
    ExpectedCfg = [{certfile, "priv/cert.pem"},
                   {cafile, "priv/ca.pem"}],
    ?cfgh(M(Host, ExpectedCfg), T(RequiredOpts)),
    ?cfgh(M(Host, ExpectedCfg ++ [{ciphers, "TLS_AES_256_GCM_SHA384"}]),
         T(RequiredOpts#{<<"ciphers">> => <<"TLS_AES_256_GCM_SHA384">>})),
    ?cfgh(M(Host, ExpectedCfg ++ [{dhfile, "priv/cert.pem"}]),
         T(RequiredOpts#{<<"dhfile">> => <<"priv/cert.pem">>})),
    [?errh(T(maps:remove(Key, RequiredOpts))) || Key <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"certfile">> => <<"/this/does/not/exist">>})),
    ?errh(T(RequiredOpts#{<<"cacertfile">> => <<"/this/does/not/exist">>})),
    ?errh(T(RequiredOpts#{<<"dhfile">> => <<"/this/does/not/exist">>})),
    ?errh(T(RequiredOpts#{<<"ciphers">> => 42})).

mod_global_distrib_redis(_Config) ->
    RequiredModOpts = global_distrib_required_opts(),
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_global_distrib">> =>
                                 RequiredModOpts#{<<"redis">> => Opts}}}
        end,
    M = fun(Host, Cfg) -> modopts(Host, mod_global_distrib,
                                  global_distrib_expected_config() ++ [{redis, Cfg}])
        end,
    ?cfgh(M(Host, []), T(#{})),
    ?cfgh(M(Host, [{pool, global_distrib}]),
         T(#{<<"pool">> => <<"global_distrib">>})),
    ?cfgh(M(Host, [{expire_after, 120}]),
         T(#{<<"expire_after">> => 120})),
    ?cfgh(M(Host, [{refresh_after, 60}]),
         T(#{<<"refresh_after">> => 60})),
    ?errh(T(#{<<"pool">> => <<"">>})),
    ?errh(T(#{<<"expire_after">> => 0})),
    ?errh(T(#{<<"refresh_after">> => -1})).

mod_global_distrib_cache(_Config) ->
    RequiredModOpts = global_distrib_required_opts(),
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_global_distrib">> =>
                                 RequiredModOpts#{<<"cache">> => Opts}}}
        end,
    M = fun(Host, Cfg) -> modopts(Host, mod_global_distrib,
                                  global_distrib_expected_config() ++ [{cache, Cfg}])
        end,
    ?cfgh(M(Host, []), T(#{})),
    ?cfgh(M(Host, [{cache_missed, false}]),
         T(#{<<"cache_missed">> => false})),
    ?cfgh(M(Host, [{domain_lifetime_seconds, 60}]),
         T(#{<<"domain_lifetime_seconds">> => 60})),
    ?cfgh(M(Host, [{jid_lifetime_seconds, 30}]),
         T(#{<<"jid_lifetime_seconds">> => 30})),
    ?cfgh(M(Host, [{max_jids, 9999}]),
         T(#{<<"max_jids">> => 9999})),
    ?errh(T(#{<<"cache_missed">> => <<"yes">>})),
    ?errh(T(#{<<"domain_lifetime_seconds">> => -1})),
    ?errh(T(#{<<"jid_lifetime_seconds">> => -1})),
    ?errh(T(#{<<"max_jids">> => -1})).

mod_global_distrib_bounce(_Config) ->
    RequiredModOpts = global_distrib_required_opts(),
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_global_distrib">> =>
                                 RequiredModOpts#{<<"bounce">> => Opts}}}
        end,
    M = fun(Host, Cfg) -> modopts(Host, mod_global_distrib,
                                  global_distrib_expected_config() ++ [{bounce, Cfg}])
        end,
    ?cfgh(M(Host, false),
         T(#{<<"enabled">> => false})),
    ?cfgh(M(Host, []),
         T(#{<<"enabled">> => true})),
    ?cfgh(M(Host, [{resend_after_ms, 300}]),
         T(#{<<"resend_after_ms">> => 300})),
    ?cfgh(M(Host, [{max_retries, 3}]),
         T(#{<<"max_retries">> => 3})),
    ?errh(T(#{<<"enabled">> => <<"">>})),
    ?errh(T(#{<<"resend_after_ms">> => -1})),
    ?errh(T(#{<<"max_retries">> => -1})).

global_distrib_required_opts() ->
    #{<<"global_host">> => <<"example.com">>,
      <<"local_host">> => <<"datacenter1.example.com">>}.

global_distrib_expected_config() ->
    [{global_host, "example.com"},
     {local_host, "datacenter1.example.com"}].

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
    M = fun(Host, Cfg) -> modopts(Host, mod_event_pusher, [{backends, [{sns, Cfg}]}]) end,
    ?cfgh(M(Host, ExpectedCfg),
         T(RequiredOpts)),
    ?cfgh(M(Host, ExpectedCfg ++ [{presence_updates_topic, "pres"}]),
         T(RequiredOpts#{<<"presence_updates_topic">> => <<"pres">>})),
    ?cfgh(M(Host, ExpectedCfg ++ [{pm_messages_topic, "pm"}]),
         T(RequiredOpts#{<<"pm_messages_topic">> => <<"pm">>})),
    ?cfgh(M(Host, ExpectedCfg ++ [{muc_messages_topic, "muc"}]),
         T(RequiredOpts#{<<"muc_messages_topic">> => <<"muc">>})),
    ?cfgh(M(Host, ExpectedCfg ++ [{plugin_module, mod_event_pusher_sns_defaults}]),
         T(RequiredOpts#{<<"plugin_module">> => <<"mod_event_pusher_sns_defaults">>})),
    ?cfgh(M(Host, ExpectedCfg ++ [{pool_size, 10}]),
         T(RequiredOpts#{<<"pool_size">> => 10})),
    ?cfgh(M(Host, ExpectedCfg ++ [{publish_retry_count, 1}]),
         T(RequiredOpts#{<<"publish_retry_count">> => 1})),
    ?cfgh(M(Host, ExpectedCfg ++ [{publish_retry_time_ms, 100}]),
         T(RequiredOpts#{<<"publish_retry_time_ms">> => 100})),
    [?errh(T(maps:remove(Key, RequiredOpts))) || Key <- maps:keys(RequiredOpts)],
    [?errh(T(RequiredOpts#{Key => 1})) || Key <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"presence_updates_topic">> => #{}})),
    ?errh(T(RequiredOpts#{<<"pm_messages_topic">> => true})),
    ?errh(T(RequiredOpts#{<<"muc_messages_topic">> => [1, 2]})),
    ?errh(T(RequiredOpts#{<<"plugin_module">> => <<"plug_and_play">>})),
    ?errh(T(RequiredOpts#{<<"pool_size">> => 0})),
    ?errh(T(RequiredOpts#{<<"publish_retry_count">> => -1})),
    ?errh(T(RequiredOpts#{<<"publish_retry_time_ms">> => -1})).

mod_event_pusher_push(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_event_pusher">> =>
                                 #{<<"backend">> => #{<<"push">> => Opts}}}}
        end,
    M = fun(Host, Cfg) -> modopts(Host, mod_event_pusher, [{backends, [{push, Cfg}]}]) end,
    ?cfgh(M(Host, [{backend, rdbms}]),
         T(#{<<"backend">> => <<"rdbms">>})),
    ?cfgh(M(Host, [{wpool, [{workers, 200}]}]),
         T(#{<<"wpool">> => #{<<"workers">> => 200}})),
    ?cfgh(M(Host, [{plugin_module, mod_event_pusher_push_plugin_defaults}]),
         T(#{<<"plugin_module">> => <<"mod_event_pusher_push_plugin_defaults">>})),
    ?cfgh(M(Host, [{virtual_pubsub_hosts, [{fqdn, <<"host1">>}, {fqdn, <<"host2">>}]}]),
         T(#{<<"virtual_pubsub_hosts">> => [<<"host1">>, <<"host2">>]})),
    ?cfgh(M(Host, [{virtual_pubsub_hosts, [{prefix, <<"pubsub.">>}, {prefix, <<"pub-sub.">>}]}]),
         T(#{<<"virtual_pubsub_hosts">> => [<<"pubsub.@HOST@">>, <<"pub-sub.@HOST@">>]})),
    ?errh(T(#{<<"backend">> => <<"redis">>})),
    ?errh(T(#{<<"wpool">> => true})),
    ?errh(T(#{<<"wpool">> => #{<<"workers">> => <<"500">>}})),
    ?errh(T(#{<<"plugin_module">> => <<"wow_cool_but_missing">>})),
    ?errh(T(#{<<"plugin_module">> => 1})),
    ?errh(T(#{<<"virtual_pubsub_hosts">> => [<<"host with whitespace">>]})),
    ?errh(T(#{<<"virtual_pubsub_hosts">> => [<<"invalid.sub@HOST@">>]})),
    ?errh(T(#{<<"virtual_pubsub_hosts">> => [<<"invalid.sub.@HOST@.as.well">>]})).

mod_event_pusher_http(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_event_pusher">> =>
                                 #{<<"backend">> => #{<<"http">> => Opts}}}}
        end,
    M = fun(Host, Cfg) -> modopts(Host, mod_event_pusher, [{backends, [{http, Cfg}]}]) end,
    ?cfgh(M(Host, [{pool_name, http_pool}]),
         T(#{<<"pool_name">> => <<"http_pool">>})),
    ?cfgh(M(Host, [{path, "/notifications"}]),
         T(#{<<"path">> => <<"/notifications">>})),
    ?cfgh(M(Host, [{callback_module, mod_event_pusher_http_defaults}]),
         T(#{<<"callback_module">> => <<"mod_event_pusher_http_defaults">>})),
    ?errh(T(#{<<"pool_name">> => <<>>})),
    ?errh(T(#{<<"path">> => true})),
    ?errh(T(#{<<"callback_module">> => <<"wow_cool_but_missing">>})),
    ?errh(T(#{<<"callback_module">> => 1})).

mod_event_pusher_rabbit(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_event_pusher">> =>
                                 #{<<"backend">> => #{<<"rabbit">> => Opts}}}}
        end,
    M = fun(Host, Cfg) -> modopts(Host, mod_event_pusher, [{backends, [{rabbit, Cfg}]}]) end,
    ?cfgh(M(Host, [{presence_exchange, [{name, <<"pres">>}]}]),
         T(#{<<"presence_exchange">> => #{<<"name">> => <<"pres">>}})),
    ?cfgh(M(Host, [{presence_exchange, [{type, <<"topic">>}]}]),
         T(#{<<"presence_exchange">> => #{<<"type">> => <<"topic">>}})),

    %% first two keys are the same as before, test them together
    ?cfgh(M(Host, [{chat_msg_exchange, [{name, <<"pres1">>},
                                 {type, <<"topic1">>}]}]),
         T(#{<<"chat_msg_exchange">> => #{<<"name">> => <<"pres1">>,
                                          <<"type">> => <<"topic1">>}})),
    ?cfgh(M(Host, [{chat_msg_exchange, [{sent_topic, <<"sent_topic1">>}]}]),
         T(#{<<"chat_msg_exchange">> => #{<<"sent_topic">> => <<"sent_topic1">>}})),
    ?cfgh(M(Host, [{chat_msg_exchange, [{recv_topic, <<"recv_topic1">>}]}]),
         T(#{<<"chat_msg_exchange">> => #{<<"recv_topic">> => <<"recv_topic1">>}})),

    %% all keys are the same as before, test them together
    ?cfgh(M(Host, [{groupchat_msg_exchange, [{name, <<"pres2">>},
                                      {type, <<"topic2">>},
                                      {sent_topic, <<"sent_topic2">>},
                                      {recv_topic, <<"recv_topic2">>}]}]),
         T(#{<<"groupchat_msg_exchange">> => #{<<"name">> => <<"pres2">>,
                                               <<"type">> => <<"topic2">>,
                                               <<"sent_topic">> => <<"sent_topic2">>,
                                               <<"recv_topic">> => <<"recv_topic2">>}})),

    Exchanges = [<<"presence_exchange">>, <<"chat_msg_exchange">>, <<"groupchat_msg_exchange">>],
    Keys = [<<"name">>, <<"topic">>, <<"sent_topic">>, <<"recv_topic">>],
    [?errh(T(#{Exch => #{Key => <<>>}})) || Exch <- Exchanges, Key <- Keys],
    [?errh(T(#{Exch => #{<<"badkey">> => <<"goodvalue">>}})) || Exch <- Exchanges],
    ?errh(T(#{<<"money_exchange">> => #{<<"name">> => <<"kantor">>}})).

mod_http_upload(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_http_upload">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_http_upload, Cfg) end,
    RequiredOpts = #{<<"s3">> => http_upload_s3_required_opts()},
    ExpectedCfg = [{s3, http_upload_s3_expected_cfg()}],
    ?cfgh(M(Host, ExpectedCfg), T(RequiredOpts)),
    ?cfgh(M(Host, ExpectedCfg ++ [{host, {prefix, <<"upload.">>}}]),
         T(RequiredOpts#{<<"host">> => <<"upload.@HOST@">>})),
    ?cfgh(M(Host, ExpectedCfg ++ [{host, {fqdn, <<"upload.test">>}}]),
         T(RequiredOpts#{<<"host">> => <<"upload.test">>})),
    ?cfgh(M(Host, ExpectedCfg ++ [{backend, s3}]),
         T(RequiredOpts#{<<"backend">> => <<"s3">>})),
    ?cfgh(M(Host, ExpectedCfg ++ [{expiration_time, 666}]),
         T(RequiredOpts#{<<"expiration_time">> => 666})),
    ?cfgh(M(Host, ExpectedCfg ++ [{token_bytes, 32}]),
         T(RequiredOpts#{<<"token_bytes">> => 32})),
    ?cfgh(M(Host, ExpectedCfg ++ [{max_file_size, 42}]),
         T(RequiredOpts#{<<"max_file_size">> => 42})),
    ?errh(T(#{})), %% missing 's3'
    ?errh(T(RequiredOpts#{<<"backend">> => <<"">>})),
    ?errh(T(RequiredOpts#{<<"expiration_time">> => 0})),
    ?errh(T(RequiredOpts#{<<"token_bytes">> => 0})),
    ?errh(T(RequiredOpts#{<<"max_file_size">> => 0})),
    ?errh(T(RequiredOpts#{<<"host">> => <<"is this a host? no.">>})),
    ?errh(T(RequiredOpts#{<<"host">> => [<<"invalid.sub@HOST@">>]})),
    ?errh(T(RequiredOpts#{<<"host">> => [<<"invalid.sub.@HOST@.as.well">>]})),
    ?errh(T(RequiredOpts#{<<"host">> => [<<"not.supported.any.more.@HOSTS@">>]})),
    check_iqdisc(mod_http_upload, ExpectedCfg, RequiredOpts).

mod_http_upload_s3(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_http_upload">> =>
                                              #{<<"s3">> => Opts}}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_http_upload, [{s3, Cfg}]) end,
    RequiredOpts = http_upload_s3_required_opts(),
    ExpectedCfg = http_upload_s3_expected_cfg(),
    ?cfgh(M(Host, ExpectedCfg), T(RequiredOpts)),
    ?cfgh(M(Host, ExpectedCfg ++ [{add_acl, true}]),
         T(RequiredOpts#{<<"add_acl">> => true})),
    [?errh(T(maps:remove(Key, RequiredOpts))) || Key <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"bucket_url">> => <<>>})),
    ?errh(T(RequiredOpts#{<<"region">> => true})),
    ?errh(T(RequiredOpts#{<<"access_key_id">> => []})),
    ?errh(T(RequiredOpts#{<<"secret_access_key">> => 3})),
    ?errh(T(RequiredOpts#{<<"add_acl">> => <<"true">>})).

http_upload_s3_required_opts() ->
    #{<<"bucket_url">> => <<"https://s3-eu-west-1.amazonaws.com/mybucket">>,
      <<"region">> => <<"antarctica-1">>,
      <<"access_key_id">> => <<"PLEASE">>,
      <<"secret_access_key">> => <<"ILOVEU">>}.

http_upload_s3_expected_cfg() ->
    [{access_key_id, "PLEASE"},
     {bucket_url, "https://s3-eu-west-1.amazonaws.com/mybucket"},
     {region, "antarctica-1"},
     {secret_access_key, "ILOVEU"}].

mod_jingle_sip(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_jingle_sip">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_jingle_sip, Cfg) end,
    ?cfgh(M(Host, [{proxy_host, "proxxxy"}]),
         T(#{<<"proxy_host">> => <<"proxxxy">>})),
    ?cfgh(M(Host, [{proxy_port, 5601}]),
         T(#{<<"proxy_port">> => 5601})),
    ?cfgh(M(Host, [{listen_port, 5602}]),
         T(#{<<"listen_port">> => 5602})),
    ?cfgh(M(Host, [{local_host, "localhost"}]),
         T(#{<<"local_host">> => <<"localhost">>})),
    ?cfgh(M(Host, [{sdp_origin, "127.0.0.1"}]),
         T(#{<<"sdp_origin">> => <<"127.0.0.1">>})),
    ?errh(T(#{<<"proxy_host">> => 1})),
    ?errh(T(#{<<"proxy_port">> => 1000000})),
    ?errh(T(#{<<"listen_port">> => -1})),
    ?errh(T(#{<<"local_host">> => <<>>})),
    ?errh(T(#{<<"sdp_origin">> => <<"abc">>})).

mod_keystore(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_keystore">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_keystore, Cfg) end,
    ?cfgh(M(Host, [{ram_key_size, 1024}]),
         T(#{<<"ram_key_size">> => 1024})),
    ?errh(T(#{<<"ram_key_size">> => -1})).

mod_keystore_keys(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_keystore">> =>
                                              #{<<"keys">> => Opts}}}
        end,
    M = fun(Host, Cfg) -> modopts(Host, mod_keystore, [{keys, Cfg}]) end,
    RequiredOpts = #{<<"name">> => <<"access_secret">>,
                     <<"type">> => <<"ram">>},
    ?cfgh(M(Host, [{access_secret, ram}]),
         T([RequiredOpts])),
    ?cfgh(M(Host, [{access_secret, {file, "priv/access_psk"}}]),
         T([RequiredOpts#{<<"type">> => <<"file">>,
                          <<"path">> => <<"priv/access_psk">>}])),
    [?errh(T([maps:remove(Key, RequiredOpts)])) || Key <- maps:keys(RequiredOpts)],
    ?errh(T([RequiredOpts#{<<"name">> => <<>>}])),
    ?errh(T([RequiredOpts#{<<"type">> => <<"rampampam">>}])),
    ?errh(T([RequiredOpts#{<<"type">> => <<"file">>}])),
    ?errh(T([RequiredOpts#{<<"type">> => <<"file">>,
                           <<"path">> => <<"does/not/exists">>}])).

mod_last(_Config) ->
    check_iqdisc(mod_last),
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_last">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_last, Cfg) end,
    ?cfgh(M(Host, [{backend, mnesia}]),
       T(#{<<"backend">> => <<"mnesia">>})),
    ?cfgh(M(Host, [{bucket_type, <<"test">>}]),
       T(#{<<"riak">> => #{<<"bucket_type">> => <<"test">>}})),

    ?errh(T(#{<<"backend">> => <<"frontend">>})),
    ?errh(T(#{<<"riak">> => #{<<"bucket_type">> => 1}})).

mod_mam_meta(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_mam_meta">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_mam_meta, Cfg) end,
    test_mod_mam_meta(T, M),
    ?cfgh(M(Host, [{bucket_type, <<"mam_bucket">>}]),
         T(#{<<"riak">> => #{<<"bucket_type">> => <<"mam_bucket">>}})),
    ?cfgh(M(Host, [{search_index, <<"mam_index">>}]),
         T(#{<<"riak">> => #{<<"search_index">> => <<"mam_index">>}})),
    ?errh(T(#{<<"riak">> => #{<<"bucket_type">> => <<>>}})),
    ?errh(T(#{<<"riak">> => #{<<"search_index">> => <<>>}})).

mod_mam_meta_pm(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_mam_meta">> => #{<<"pm">> => Opts}}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_mam_meta, [{pm, Cfg}]) end,
    test_mod_mam_meta(T, M),
    ?cfgh(M(Host, [{archive_groupchats, true}]),
         T(#{<<"archive_groupchats">> => true})),
    ?cfgh(M(Host, [{same_mam_id_for_peers, true}]),
         T(#{<<"same_mam_id_for_peers">> => true})),
    ?errh(T(#{<<"archive_groupchats">> => <<"not really">>})),
    ?errh(T(#{<<"same_mam_id_for_peers">> => <<"not really">>})).

mod_mam_meta_muc(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_mam_meta">> => #{<<"muc">> => Opts}}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_mam_meta, [{muc, Cfg}]) end,
    test_mod_mam_meta(T, M),
    ?cfgh(M(Host, [{host, {prefix, <<"muc.">>}}]),
         T(#{<<"host">> => <<"muc.@HOST@">>})),
    ?cfgh(M(Host, [{host, {fqdn, <<"muc.test">>}}]),
         T(#{<<"host">> => <<"muc.test">>})),
    ?errh(T(#{<<"host">> => <<"is this a host? no.">>})),
    ?errh(T(#{<<"host">> => [<<"invalid.sub@HOST@">>]})),
    ?errh(T(#{<<"host">> => [<<"invalid.sub.@HOST@.as.well">>]})),
    ?errh(T(#{<<"archive_groupchats">> => true})),
    ?errh(T(#{<<"same_mam_id_for_peers">> => true})).

test_mod_mam_meta(T, M) ->
    ?cfgh(M(Host, [{backend, rdbms}]),
         T(#{<<"backend">> => <<"rdbms">>})),
    ?cfgh(M(Host, [{no_stanzaid_element, true}]),
         T(#{<<"no_stanzaid_element">> => true})),
    ?cfgh(M(Host, [{is_archivable_message, mod_mam_utils}]),
         T(#{<<"is_archivable_message">> => <<"mod_mam_utils">>})),
    ?cfgh(M(Host, [{archive_chat_markers, false}]),
         T(#{<<"archive_chat_markers">> => false})),
    ?cfgh(M(Host, [{message_retraction, true}]),
         T(#{<<"message_retraction">> => true})),
    ?cfgh(M(Host, [{cache_users, false}]),
         T(#{<<"cache_users">> => false})),
    ?cfgh(M(Host, [{rdbms_message_format, simple}]),
         T(#{<<"rdbms_message_format">> => <<"simple">>})),
    ?cfgh(M(Host, [{async_writer, true}]),
         T(#{<<"async_writer">> => true})),
    ?cfgh(M(Host, [{flush_interval, 1500}]),
         T(#{<<"flush_interval">> => 1500})),
    ?cfgh(M(Host, [{max_batch_size, 50}]),
         T(#{<<"max_batch_size">> => 50})),
    ?cfgh(M(Host, [{user_prefs_store, rdbms}]),
         T(#{<<"user_prefs_store">> => <<"rdbms">>})),
    ?cfgh(M(Host, [{full_text_search, false}]),
         T(#{<<"full_text_search">> => false})),
    ?cfgh(M(Host, [{default_result_limit, 100}]),
         T(#{<<"default_result_limit">> => 100})),
    ?cfgh(M(Host, [{max_result_limit, 1000}]),
         T(#{<<"max_result_limit">> => 1000})),
    ?cfgh(M(Host, [{async_writer_rdbms_pool, async_pool}]),
         T(#{<<"async_writer_rdbms_pool">> => <<"async_pool">>})),
    ?cfgh(M(Host, [{db_jid_format, mam_jid_rfc}]),
         T(#{<<"db_jid_format">> => <<"mam_jid_rfc">>})),
    ?cfgh(M(Host, [{db_message_format, mam_message_xml}]),
         T(#{<<"db_message_format">> => <<"mam_message_xml">>})),
    ?cfgh(M(Host, [{simple, false}]),
         T(#{<<"simple">> => false})),
    ?cfgh(M(Host, [{extra_fin_element, mod_mam_utils}]),
         T(#{<<"extra_fin_element">> => <<"mod_mam_utils">>})),
    ?cfgh(M(Host, [{extra_lookup_params, mod_mam_utils}]),
         T(#{<<"extra_lookup_params">> => <<"mod_mam_utils">>})),
    ?cfgh(M(Host, [{cache, [{module, internal}]}]),
         T(#{<<"cache">> => #{<<"module">> => <<"internal">>}})),
    ?cfgh(M(Host, [{cache, [{time_to_live, 8600}]}]),
         T(#{<<"cache">> => #{<<"time_to_live">> => 8600}})),
    ?cfgh(M(Host, [{cache, [{time_to_live, infinity}]}]),
         T(#{<<"cache">> => #{<<"time_to_live">> => <<"infinity">>}})),
    ?cfgh(M(Host, [{cache, [{number_of_segments, 10}]}]),
         T(#{<<"cache">> => #{<<"number_of_segments">> => 10}})),
    ?cfgh(M(Host, [{cache, [{strategy, fifo}]}]),
         T(#{<<"cache">> => #{<<"strategy">> => <<"fifo">>}})),
    ?errh(T(#{<<"backend">> => <<"notepad">>})),
    ?errh(T(#{<<"no_stanzaid_element">> => <<"true">>})),
    ?errh(T(#{<<"is_archivable_message">> => <<"mod_mam_fake">>})),
    ?errh(T(#{<<"archive_chat_markers">> => <<"maybe">>})),
    ?errh(T(#{<<"message_retraction">> => 1})),
    ?errh(T(#{<<"cache_users">> => []})),
    ?errh(T(#{<<"rdbms_message_format">> => <<"complex">>})),
    ?errh(T(#{<<"async_writer">> => #{}})),
    ?errh(T(#{<<"flush_interval">> => -1})),
    ?errh(T(#{<<"max_batch_size">> => -1})),
    ?errh(T(#{<<"user_prefs_store">> => <<"textfile">>})),
    ?errh(T(#{<<"full_text_search">> => <<"disabled">>})),
    ?errh(T(#{<<"default_result_limit">> => -1})),
    ?errh(T(#{<<"max_result_limit">> => -2})),
    ?errh(T(#{<<"async_writer_rdbms_pool">> => <<>>})),
    ?errh(T(#{<<"db_jid_format">> => <<"not_a_module">>})),
    ?errh(T(#{<<"db_message_format">> => <<"not_a_module">>})),
    ?errh(T(#{<<"simple">> => <<"yes">>})),
    ?errh(T(#{<<"extra_fin_element">> => <<"bad_module">>})),
    ?errh(T(#{<<"extra_lookup_params">> => <<"bad_module">>})),
    ?errh(T(#{<<"cache">> => #{<<"module">> => <<"mod_wrong_cache">>}})),
    ?errh(T(#{<<"cache">> => #{<<"module">> => <<"mod_cache_users">>,
                               <<"time_to_live">> => 8600}})),
    ?errh(T(#{<<"cache">> => #{<<"time_to_live">> => 0}})),
    ?errh(T(#{<<"cache">> => #{<<"strategy">> => <<"lifo">>}})),
    ?errh(T(#{<<"cache">> => #{<<"number_of_segments">> => 0}})),
    ?errh(T(#{<<"cache">> => #{<<"number_of_segments">> => <<"infinity">>}})).

mod_muc(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_muc">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_muc, Cfg) end,
    ?cfgh(M(Host, [{host, {prefix, <<"conference.">>}}]),
         T(#{<<"host">> => <<"conference.@HOST@">>})),
    ?cfgh(M(Host, [{host, {fqdn, <<"conference.test">>}}]),
         T(#{<<"host">> => <<"conference.test">>})),
    ?cfgh(M(Host, [{backend, mnesia}]),
         T(#{<<"backend">> => <<"mnesia">>})),
    ?cfgh(M(Host, [{access, all}]),
         T(#{<<"access">> => <<"all">>})),
    ?cfgh(M(Host, [{access_create, admin}]),
         T(#{<<"access_create">> => <<"admin">>})),
    ?cfgh(M(Host, [{access_admin, none}]),
         T(#{<<"access_admin">> => <<"none">>})),
    ?cfgh(M(Host, [{access_persistent, all}]),
         T(#{<<"access_persistent">> => <<"all">>})),
    ?cfgh(M(Host, [{history_size, 20}]),
         T(#{<<"history_size">> => 20})),
    ?cfgh(M(Host, [{room_shaper, muc_room_shaper}]),
         T(#{<<"room_shaper">> => <<"muc_room_shaper">>})),
    ?cfgh(M(Host, [{max_room_id, infinity}]),
         T(#{<<"max_room_id">> => <<"infinity">>})),
    ?cfgh(M(Host, [{max_room_name, 30}]),
         T(#{<<"max_room_name">> => 30})),
    ?cfgh(M(Host, [{max_room_desc, 0}]),
         T(#{<<"max_room_desc">> => 0})),
    ?cfgh(M(Host, [{min_message_interval, 10}]),
         T(#{<<"min_message_interval">> => 10})),
    ?cfgh(M(Host, [{min_presence_interval, 0}]),
         T(#{<<"min_presence_interval">> => 0})),
    ?cfgh(M(Host, [{max_users, 30}]),
         T(#{<<"max_users">> => 30})),
    ?cfgh(M(Host, [{max_users_admin_threshold, 2}]),
         T(#{<<"max_users_admin_threshold">> => 2})),
    ?cfgh(M(Host, [{user_message_shaper, muc_msg_shaper}]),
         T(#{<<"user_message_shaper">> => <<"muc_msg_shaper">>})),
    ?cfgh(M(Host, [{user_presence_shaper, muc_pres_shaper}]),
         T(#{<<"user_presence_shaper">> => <<"muc_pres_shaper">>})),
    ?cfgh(M(Host, [{max_user_conferences, 10}]),
         T(#{<<"max_user_conferences">> => 10})),
    ?cfgh(M(Host, [{http_auth_pool, external_auth}]),
         T(#{<<"http_auth_pool">> => <<"external_auth">>})),
    ?cfgh(M(Host, [{load_permanent_rooms_at_startup, true}]),
         T(#{<<"load_permanent_rooms_at_startup">> => true})),
    ?cfgh(M(Host, [{hibernate_timeout, infinity}]),
         T(#{<<"hibernate_timeout">> => <<"infinity">>})),
    ?cfgh(M(Host, [{hibernated_room_check_interval, 5000}]),
         T(#{<<"hibernated_room_check_interval">> => 5000})),
    ?cfgh(M(Host, [{hibernated_room_timeout, 0}]),
         T(#{<<"hibernated_room_timeout">> => 0})),
    ?errh(T(#{<<"host">> => <<>>})),
    ?errh(T(#{<<"host">> => <<"is this a host? no.">>})),
    ?errh(T(#{<<"host">> => [<<"invalid.sub@HOST@">>]})),
    ?errh(T(#{<<"host">> => [<<"invalid.sub.@HOST@.as.well">>]})),
    ?errh(T(#{<<"backend">> => <<"amnesia">>})),
    ?errh(T(#{<<"access">> => <<>>})),
    ?errh(T(#{<<"access_create">> => 1})),
    ?errh(T(#{<<"access_admin">> => []})),
    ?errh(T(#{<<"access_persistent">> => true})),
    ?errh(T(#{<<"history_size">> => <<"20">>})),
    ?errh(T(#{<<"room_shaper">> => <<>>})),
    ?errh(T(#{<<"max_room_id">> => #{}})),
    ?errh(T(#{<<"max_room_name">> => <<"infinite!">>})),
    ?errh(T(#{<<"max_room_desc">> => -1})),
    ?errh(T(#{<<"min_message_interval">> => -10})),
    ?errh(T(#{<<"min_presence_interval">> => <<"infinity">>})),
    ?errh(T(#{<<"max_users">> => 0})),
    ?errh(T(#{<<"max_users_admin_threshold">> => 0})),
    ?errh(T(#{<<"user_message_shaper">> => []})),
    ?errh(T(#{<<"user_presence_shaper">> => <<>>})),
    ?errh(T(#{<<"max_user_conferences">> => -1})),
    ?errh(T(#{<<"http_auth_pool">> => <<>>})),
    ?errh(T(#{<<"load_permanent_rooms_at_startup">> => <<"true">>})),
    ?errh(T(#{<<"hibernate_timeout">> => <<"really big">>})),
    ?errh(T(#{<<"hibernated_room_check_interval">> => -1})),
    ?errh(T(#{<<"hibernated_room_timeout">> => false})).

mod_muc_default_room(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_muc">> => #{<<"default_room">> => Opts}}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_muc, [{default_room_options, Cfg}]) end,
    ?cfgh(M(Host, []), T(#{})),
    ?cfgh(M(Host, [{title, <<"living room">>}]),
         T(#{<<"title">> => <<"living room">>})),
    ?cfgh(M(Host, [{description, <<"a room that is alive">>}]),
         T(#{<<"description">> => <<"a room that is alive">>})),
    ?cfgh(M(Host, [{allow_change_subj, true}]),
         T(#{<<"allow_change_subj">> => true})),
    ?cfgh(M(Host, [{allow_query_users, false}]),
         T(#{<<"allow_query_users">> => false})),
    ?cfgh(M(Host, [{allow_private_messages, true}]),
         T(#{<<"allow_private_messages">> => true})),
    ?cfgh(M(Host, [{allow_visitor_status, false}]),
         T(#{<<"allow_visitor_status">> => false})),
    ?cfgh(M(Host, [{allow_visitor_nickchange, true}]),
         T(#{<<"allow_visitor_nickchange">> => true})),
    ?cfgh(M(Host, [{public, false}]),
         T(#{<<"public">> => false})),
    ?cfgh(M(Host, [{public_list, true}]),
         T(#{<<"public_list">> => true})),
    ?cfgh(M(Host, [{persistent, true}]),
         T(#{<<"persistent">> => true})),
    ?cfgh(M(Host, [{moderated, false}]),
         T(#{<<"moderated">> => false})),
    ?cfgh(M(Host, [{members_by_default, true}]),
         T(#{<<"members_by_default">> => true})),
    ?cfgh(M(Host, [{members_only, false}]),
         T(#{<<"members_only">> => false})),
    ?cfgh(M(Host, [{allow_user_invites, true}]),
         T(#{<<"allow_user_invites">> => true})),
    ?cfgh(M(Host, [{allow_multiple_sessions, false}]),
         T(#{<<"allow_multiple_sessions">> => false})),
    ?cfgh(M(Host, [{password_protected, true}]),
         T(#{<<"password_protected">> => true})),
    ?cfgh(M(Host, [{password, <<"secret">>}]),
         T(#{<<"password">> => <<"secret">>})),
    ?cfgh(M(Host, [{anonymous, true}]),
         T(#{<<"anonymous">> => true})),
    ?cfgh(M(Host, [{max_users, 100}]),
         T(#{<<"max_users">> => 100})),
    ?cfgh(M(Host, [{logging, false}]),
         T(#{<<"logging">> => false})),
    ?cfgh(M(Host, [{maygetmemberlist, [moderator]}]),
         T(#{<<"maygetmemberlist">> => [<<"moderator">>]})),
    ?cfgh(M(Host, [{subject, <<"Lambda days">>}]),
         T(#{<<"subject">> => <<"Lambda days">>})),
    ?cfgh(M(Host, [{subject_author, <<"Alice">>}]),
         T(#{<<"subject_author">> => <<"Alice">>})),
    ?errh(T(<<"bad value">>)),
    ?errh(T(#{<<"title">> => true})),
    ?errh(T(#{<<"description">> => 1})),
    ?errh(T(#{<<"allow_change_subj">> => <<"true">>})),
    ?errh(T(#{<<"allow_query_users">> => <<>>})),
    ?errh(T(#{<<"allow_private_messages">> => 1})),
    ?errh(T(#{<<"allow_visitor_status">> => []})),
    ?errh(T(#{<<"allow_visitor_nickchange">> => #{}})),
    ?errh(T(#{<<"public">> => 0})),
    ?errh(T(#{<<"public_list">> => [false]})),
    ?errh(T(#{<<"persistent">> => 1})),
    ?errh(T(#{<<"moderated">> => <<"yes">>})),
    ?errh(T(#{<<"members_by_default">> => 0})),
    ?errh(T(#{<<"members_only">> => [true]})),
    ?errh(T(#{<<"allow_user_invites">> => <<>>})),
    ?errh(T(#{<<"allow_multiple_sessions">> => []})),
    ?errh(T(#{<<"password_protected">> => #{}})),
    ?errh(T(#{<<"password">> => false})),
    ?errh(T(#{<<"anonymous">> => <<"maybe">>})),
    ?errh(T(#{<<"max_users">> => 0})),
    ?errh(T(#{<<"logging">> => [true, false]})),
    ?errh(T(#{<<"maygetmemberlist">> => <<"moderator">>})),
    ?errh(T(#{<<"maygetmemberlist">> => [<<>>]})),
    ?errh(T(#{<<"subject">> => [<<"subjective">>]})),
    ?errh(T(#{<<"subject_author">> => 1})).

mod_muc_default_room_affiliations(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_muc">> =>
                                 #{<<"default_room">> => #{<<"affiliations">> => Opts}}}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_muc, [{default_room_options, [{affiliations, Cfg}]}]) end,
    RequiredOpts = #{<<"user">> => <<"alice">>,
                     <<"server">> => <<"localhost">>,
                     <<"resource">> => <<"phone">>,
                     <<"affiliation">> => <<"moderator">>},
    ExpectedCfg = {{<<"alice">>, <<"localhost">>, <<"phone">>}, moderator},
    ?cfgh(M(Host, []), T([])),
    ?cfgh(M(Host, [ExpectedCfg]), T([RequiredOpts])),
    [?errh(T([maps:remove(Key, RequiredOpts)])) || Key <- maps:keys(RequiredOpts)],
    ?errh(T([RequiredOpts#{<<"user">> := <<>>}])),
    ?errh(T([RequiredOpts#{<<"server">> := <<"domain? not really!">>}])),
    ?errh(T([RequiredOpts#{<<"resource">> := false}])),
    ?errh(T([RequiredOpts#{<<"affiliation">> := <<>>}])).

mod_muc_log(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_muc_log">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_muc_log, Cfg) end,
    ?cfgh(M(Host, [{outdir, "www/muc"}]),
         T(#{<<"outdir">> => <<"www/muc">>})),
    ?cfgh(M(Host, [{access_log, muc_admin}]),
         T(#{<<"access_log">> => <<"muc_admin">>})),
    ?cfgh(M(Host, [{dirtype, subdirs}]),
         T(#{<<"dirtype">> => <<"subdirs">>})),
    ?cfgh(M(Host, [{dirname, room_name}]),
         T(#{<<"dirname">> => <<"room_name">>})),
    ?cfgh(M(Host, [{file_format, html}]),
         T(#{<<"file_format">> => <<"html">>})),
    ?cfgh(M(Host, [{cssfile, <<"path/to/css_file">>}]),
         T(#{<<"css_file">> => <<"path/to/css_file">>})),
    ?cfgh(M(Host, [{timezone, local}]),
         T(#{<<"timezone">> => <<"local">>})),
    ?cfgh(M(Host, [{spam_prevention, false}]),
         T(#{<<"spam_prevention">> => false})),
    ?errh(T(#{<<"outdir">> => <<"does/not/exist">>})),
    ?errh(T(#{<<"access_log">> => 1})),
    ?errh(T(#{<<"dirtype">> => <<"imaginary">>})),
    ?errh(T(#{<<"dirname">> => <<"dyrektory">>})),
    ?errh(T(#{<<"file_format">> => <<"none">>})),
    ?errh(T(#{<<"css_file">> => <<>>})),
    ?errh(T(#{<<"timezone">> => <<"yes">>})),
    ?errh(T(#{<<"spam_prevention">> => <<"spam and eggs and spam">>})).

mod_muc_log_top_link(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_muc_log">> => #{<<"top_link">> => Opts}}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_muc_log, [{top_link, Cfg}]) end,
    RequiredOpts = #{<<"target">> => <<"https://esl.github.io/MongooseDocs/">>,
                     <<"text">> => <<"Docs">>},
    ExpectedCfg = {"https://esl.github.io/MongooseDocs/", "Docs"},
    ?cfgh(M(Host, ExpectedCfg), T(RequiredOpts)),
    [?errh(T(maps:remove(K, RequiredOpts))) || K <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"target">> => true})),
    ?errh(T(RequiredOpts#{<<"text">> => <<"">>})).

mod_muc_light(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_muc_light">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_muc_light, Cfg) end,
    ?cfgh(M(Host, [{backend, mnesia}]),
         T(#{<<"backend">> => <<"mnesia">>})),
    ?cfgh(M(Host, [{host, {prefix, <<"muclight.">>}}]),
         T(#{<<"host">> => <<"muclight.@HOST@">>})),
    ?cfgh(M(Host, [{host, {fqdn, <<"muclight.test">>}}]),
         T(#{<<"host">> => <<"muclight.test">>})),
    ?cfgh(M(Host, [{equal_occupants, true}]),
         T(#{<<"equal_occupants">> => true})),
    ?cfgh(M(Host, [{legacy_mode, false}]),
         T(#{<<"legacy_mode">> => false})),
    ?cfgh(M(Host, [{rooms_per_user, 100}]),
         T(#{<<"rooms_per_user">> => 100})),
    ?cfgh(M(Host, [{blocking, false}]),
         T(#{<<"blocking">> => false})),
    ?cfgh(M(Host, [{all_can_configure, true}]),
         T(#{<<"all_can_configure">> => true})),
    ?cfgh(M(Host, [{all_can_invite, false}]),
         T(#{<<"all_can_invite">> => false})),
    ?cfgh(M(Host, [{max_occupants, infinity}]),
         T(#{<<"max_occupants">> => <<"infinity">>})),
    ?cfgh(M(Host, [{rooms_per_page, 10}]),
         T(#{<<"rooms_per_page">> => 10})),
    ?cfgh(M(Host, [{rooms_in_rosters, true}]),
         T(#{<<"rooms_in_rosters">> => true})),
    ?errh(T(#{<<"backend">> => <<"frontend">>})),
    ?errh(T(#{<<"host">> => <<"what is a domain?!">>})),
    ?errh(T(#{<<"host">> => [<<"invalid.sub@HOST@">>]})),
    ?errh(T(#{<<"host">> => [<<"invalid.sub.@HOST@.as.well">>]})),
    ?errh(T(#{<<"equal_occupants">> => <<"true">>})),
    ?errh(T(#{<<"legacy_mode">> => 1234})),
    ?errh(T(#{<<"rooms_per_user">> => 0})),
    ?errh(T(#{<<"blocking">> => <<"true">>})),
    ?errh(T(#{<<"all_can_configure">> => []})),
    ?errh(T(#{<<"all_can_invite">> => #{}})),
    ?errh(T(#{<<"max_occupants">> => <<"seven">>})),
    ?errh(T(#{<<"rooms_per_page">> => false})),
    ?errh(T(#{<<"rooms_in_rosters">> => [1, 2, 3]})).

mod_muc_light_config_schema(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_muc_light">> => #{<<"config_schema">> => Opts}}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_muc_light, [{config_schema, Cfg}]) end,
    Field = #{<<"field">> => <<"my_field">>},
    ?cfgh(M(Host, []), T([])),
    ?cfgh(M(Host, [{<<"my_field">>, <<"My Room">>, my_field, binary}]),
         T([Field#{<<"string_value">> => <<"My Room">>}])),
    ?cfgh(M(Host, [{<<"my_field">>, 1, my_field, integer}]),
         T([Field#{<<"integer_value">> => 1}])),
    ?cfgh(M(Host, [{<<"my_field">>, 0.5, my_field, float}]),
         T([Field#{<<"float_value">> => 0.5}])),
    ?cfgh(M(Host, [{<<"my_field">>, 0, your_field, integer}]),
         T([Field#{<<"integer_value">> => 0,
                   <<"internal_key">> => <<"your_field">>}])),
    ?cfgh(M(Host, [{<<"żółć"/utf8>>, <<"Рентгеноэлектрокардиографический"/utf8>>, 'żółć', binary}]),
         T([#{<<"field">> => <<"żółć"/utf8>>,
              <<"string_value">> => <<"Рентгеноэлектрокардиографический"/utf8>>}])),
    ?cfgh(M(Host, [{<<"first">>, 1, first, integer}, % the config is u-key-sorted
            {<<"second">>, <<"two">>, second, binary}]),
         T([#{<<"field">> => <<"second">>, <<"string_value">> => <<"two">>},
            #{<<"field">> => <<"second">>, <<"float_value">> => 2.0},
            #{<<"field">> => <<"first">>, <<"integer_value">> => 1}])),
    ?errh(T([#{<<"string_value">> => <<"My Room">>}])),
    ?errh(T([#{<<"field">> => <<>>,
               <<"string_value">> => <<"My Room">>}])),
    ?errh(T([Field#{<<"string_value">> => 0}])),
    ?errh(T([Field#{<<"integer_value">> => 1.5}])),
    ?errh(T([Field#{<<"float_value">> => 1}])),
    ?errh(T([Field#{<<"integer_value">> => 0,
                    <<"string_value">> => <<"My Room">>}])),
    ?errh(T([Field#{<<"integer_value">> => 0,
                    <<"internal_key">> => <<>>}])).

mod_offline(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_offline">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_offline, Cfg) end,
    ?cfgh(M(Host, [{access_max_user_messages, max_user_offline_messages}]),
         T(#{<<"access_max_user_messages">> => <<"max_user_offline_messages">>})),
    ?cfgh(M(Host, [{backend, rdbms}]),
         T(#{<<"backend">> => <<"rdbms">>})),
    ?cfgh(M(Host, [{bucket_type, <<"test">>}]),
         T(#{<<"riak">> => #{<<"bucket_type">> => <<"test">>}})),
    ?errh(T(#{<<"access_max_user_messages">> => 1})),
    ?errh(T(#{<<"backend">> => <<"riak_is_the_best">>})),
    ?errh(T(#{<<"riak">> => #{<<"bucket_type">> => 1}})),
    ?errh(T(#{<<"riak">> => #{<<"bucket">> => <<"leaky">>}})).

mod_ping(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_ping">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_ping, Cfg) end,
    ?cfgh(M(Host, [{send_pings, true}]),
         T(#{<<"send_pings">> => true})),
    ?cfgh(M(Host, [{ping_interval, timer:seconds(10)}]),
         T(#{<<"ping_interval">> => 10})),
    ?cfgh(M(Host, [{timeout_action, kill}]),
         T(#{<<"timeout_action">> => <<"kill">>})),
    ?cfgh(M(Host, [{ping_req_timeout, timer:seconds(20)}]),
         T(#{<<"ping_req_timeout">> => 20})),
    ?errh(T(#{<<"send_pings">> => 1})),
    ?errh(T(#{<<"ping_interval">> => 0})),
    ?errh(T(#{<<"timeout_action">> => <<"kill_them_all">>})),
    ?errh(T(#{<<"ping_req_timeout">> => 0})),
    check_iqdisc(mod_ping).

mod_privacy(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_privacy">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_privacy, Cfg) end,
    ?cfgh(M(Host, [{backend, mnesia}]),
         T(#{<<"backend">> => <<"mnesia">>})),
    ?cfgh(M(Host, [{defaults_bucket_type, <<"defaults">>}]),
         T(#{<<"riak">> => #{<<"defaults_bucket_type">> => <<"defaults">>}})),
    ?cfgh(M(Host, [{names_bucket_type, <<"names">>}]),
         T(#{<<"riak">> => #{<<"names_bucket_type">> => <<"names">>}})),
    ?cfgh(M(Host, [{bucket_type, <<"bucket">>}]),
         T(#{<<"riak">> => #{<<"bucket_type">> => <<"bucket">>}})),
    ?errh(T(#{<<"backend">> => <<"mongoddt">>})),
    ?errh(T(#{<<"riak">> => #{<<"defaults_bucket_type">> => <<>>}})),
    ?errh(T(#{<<"riak">> => #{<<"names_bucket_type">> => 1}})),
    ?errh(T(#{<<"riak">> => #{<<"bucket_type">> => 1}})).

mod_private(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_private">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_private, Cfg) end,
    ?cfgh(M(Host, [{backend, riak}]),
         T(#{<<"backend">> => <<"riak">>})),
    ?cfgh(M(Host, [{bucket_type, <<"private_stuff">>}]),
         T(#{<<"riak">> => #{<<"bucket_type">> => <<"private_stuff">>}})),
    ?errh(T(#{<<"backend">> => <<"mssql">>})),
    ?errh(T(#{<<"riak">> => #{<<"bucket_type">> => 1}})),
    check_iqdisc(mod_private).

mod_pubsub(_Config) ->
    check_iqdisc(mod_pubsub),
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_pubsub">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_pubsub, Cfg) end,
    ?cfgh(M(Host, [{host, {prefix, <<"pubsub.">>}}]),
         T(#{<<"host">> => <<"pubsub.@HOST@">>})),
    ?cfgh(M(Host, [{host, {fqdn, <<"pubsub.test">>}}]),
         T(#{<<"host">> => <<"pubsub.test">>})),
    ?cfgh(M(Host, [{backend, rdbms}]),
         T(#{<<"backend">> => <<"rdbms">>})),
    ?cfgh(M(Host, [{access_createnode, all}]),
         T(#{<<"access_createnode">> => <<"all">>})),
    ?cfgh(M(Host, [{max_items_node, 20}]),
         T(#{<<"max_items_node">> => 20})),
    ?cfgh(M(Host, [{max_subscriptions_node, 30}]),
         T(#{<<"max_subscriptions_node">> => 30})),
    ?cfgh(M(Host, [{nodetree, <<"tree">>}]),
         T(#{<<"nodetree">> => <<"tree">>})),
    ?cfgh(M(Host, [{ignore_pep_from_offline, false}]),
         T(#{<<"ignore_pep_from_offline">> => false})),
    ?cfgh(M(Host, [{last_item_cache, rdbms}]),
         T(#{<<"last_item_cache">> => <<"rdbms">>})),
    ?cfgh(M(Host, [{plugins, [<<"flat">>, <<"dag">>]}]),
         T(#{<<"plugins">> => [<<"flat">>, <<"dag">>]})),
    ?cfgh(M(Host, [{item_publisher, true}]),
         T(#{<<"item_publisher">> => true})),
    ?cfgh(M(Host, [{sync_broadcast, false}]),
         T(#{<<"sync_broadcast">> => false})),
    ?errh(T(#{<<"host">> => <<"">>})),
    ?errh(T(#{<<"host">> => <<"is this a host? no.">>})),
    ?errh(T(#{<<"host">> => [<<"invalid.sub@HOST@">>]})),
    ?errh(T(#{<<"host">> => [<<"invalid.sub.@HOST@.as.well">>]})),
    ?errh(T(#{<<"backend">> => <<"amnesia">>})),
    ?errh(T(#{<<"access_createnode">> => <<"">>})),
    ?errh(T(#{<<"max_items_node">> => -1})),
    ?errh(T(#{<<"max_subscriptions_node">> => 3.1415})),
    ?errh(T(#{<<"nodetree">> => <<"christmas_tree">>})),
    ?errh(T(#{<<"ignore_pep_from_offline">> => <<"maybe">>})),
    ?errh(T(#{<<"last_item_cache">> => false})),
    ?errh(T(#{<<"plugins">> => [<<"deep">>]})),
    ?errh(T(#{<<"item_publisher">> => 1})),
    ?errh(T(#{<<"sync_broadcast">> => []})).

mod_pubsub_pep_mapping(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_pubsub">> =>
                                              #{<<"pep_mapping">> => Opts}}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_pubsub, [{pep_mapping, Cfg}]) end,
    RequiredOpts = #{<<"namespace">> => <<"urn:xmpp:microblog:0">>,
                     <<"node">> => <<"mb">>},
    ?cfgh(M(Host, [{<<"urn:xmpp:microblog:0">>, <<"mb">>}]),
         T([RequiredOpts])),
    [?errh(T([maps:remove(Key, RequiredOpts)])) || Key <- maps:keys(RequiredOpts)],
    [?errh(T([RequiredOpts#{Key => <<>>}])) || Key <- maps:keys(RequiredOpts)].

mod_pubsub_default_node_config(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_pubsub">> =>
                                              #{<<"default_node_config">> => Opts}}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_pubsub, [{default_node_config, Cfg}]) end,
    ?cfgh(M(Host, [{access_model, open}]),
         T(#{<<"access_model">> => <<"open">>})),
    ?cfgh(M(Host, [{deliver_notifications, true}]),
         T(#{<<"deliver_notifications">> => true})),
    ?cfgh(M(Host, [{deliver_payloads, false}]),
         T(#{<<"deliver_payloads">> => false})),
    ?cfgh(M(Host, [{max_items, 1000}]),
         T(#{<<"max_items">> => 1000})),
    ?cfgh(M(Host, [{max_payload_size, 1000}]),
         T(#{<<"max_payload_size">> => 1000})),
    ?cfgh(M(Host, [{node_type, dag}]),
         T(#{<<"node_type">> => <<"dag">>})),
    ?cfgh(M(Host, [{notification_type, headline}]),
         T(#{<<"notification_type">> => <<"headline">>})),
    ?cfgh(M(Host, [{notify_config, true}]),
         T(#{<<"notify_config">> => true})),
    ?cfgh(M(Host, [{notify_delete, false}]),
         T(#{<<"notify_delete">> => false})),
    ?cfgh(M(Host, [{notify_retract, true}]),
         T(#{<<"notify_retract">> => true})),
    ?cfgh(M(Host, [{persist_items, false}]),
         T(#{<<"persist_items">> => false})),
    ?cfgh(M(Host, [{presence_based_delivery, true}]),
         T(#{<<"presence_based_delivery">> => true})),
    ?cfgh(M(Host, [{publish_model, open}]),
         T(#{<<"publish_model">> => <<"open">>})),
    ?cfgh(M(Host, [{purge_offline, false}]),
         T(#{<<"purge_offline">> => false})),
    ?cfgh(M(Host, [{roster_groups_allowed, [<<"friends">>]}]),
         T(#{<<"roster_groups_allowed">> => [<<"friends">>]})),
    ?cfgh(M(Host, [{send_last_published_item, on_sub_and_presence}]),
         T(#{<<"send_last_published_item">> => <<"on_sub_and_presence">>})),
    ?cfgh(M(Host, [{subscribe, true}]),
         T(#{<<"subscribe">> => true})),
    ?errh(T(#{<<"access_model">> => <<>>})),
    ?errh(T(#{<<"deliver_notifications">> => <<"yes">>})),
    ?errh(T(#{<<"deliver_payloads">> => 0})),
    ?errh(T(#{<<"max_items">> => -1})),
    ?errh(T(#{<<"max_payload_size">> => -1})),
    ?errh(T(#{<<"node_type">> => [<<"dag">>]})),
    ?errh(T(#{<<"notification_type">> => <<>>})),
    ?errh(T(#{<<"notify_config">> => <<"false">>})),
    ?errh(T(#{<<"notify_delete">> => [true]})),
    ?errh(T(#{<<"notify_retract">> => #{}})),
    ?errh(T(#{<<"persist_items">> => 1})),
    ?errh(T(#{<<"presence_based_delivery">> => []})),
    ?errh(T(#{<<"publish_model">> => <<"">>})),
    ?errh(T(#{<<"purge_offline">> => 1})),
    ?errh(T(#{<<"roster_groups_allowed">> => [<<>>]})),
    ?errh(T(#{<<"send_last_published_item">> => <<>>})),
    ?errh(T(#{<<"subscribe">> => <<"never">>})).

mod_push_service_mongoosepush(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_push_service_mongoosepush">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_push_service_mongoosepush, Cfg) end,
    ?cfgh(M(Host, [{pool_name, test_pool}]),
         T(#{<<"pool_name">> => <<"test_pool">>})),
    ?cfgh(M(Host, [{api_version, "v3"}]),
         T(#{<<"api_version">> => <<"v3">>})),
    ?cfgh(M(Host, [{max_http_connections, 100}]),
         T(#{<<"max_http_connections">> => 100})),
    ?errh(T(#{<<"pool_name">> => 1})),
    ?errh(T(#{<<"api_version">> => <<"v4">>})),
    ?errh(T(#{<<"max_http_connections">> => -1})).

mod_register(_Config) ->
    ?cfgh(modopts(Host, mod_register, [{access,register},
                                      {ip_access, [{allow,"127.0.0.0/8"},
                                                   {deny,"0.0.0.0"}]}
                                     ]),
         ip_access_register(<<"0.0.0.0">>)),
    ?cfgh(modopts(Host, mod_register, [{access,register},
                                      {ip_access, [{allow,"127.0.0.0/8"},
                                                   {deny,"0.0.0.4"}]}
                                     ]),
         ip_access_register(<<"0.0.0.4">>)),
    ?cfgh(modopts(Host, mod_register, [{access,register},
                                      {ip_access, [{allow,"127.0.0.0/8"},
                                                   {deny,"::1"}]}
                                     ]),
         ip_access_register(<<"::1">>)),
    ?cfgh(modopts(Host, mod_register, [{access,register},
                                      {ip_access, [{allow,"127.0.0.0/8"},
                                                   {deny,"::1/128"}]}
                                     ]),
         ip_access_register(<<"::1/128">>)),
    ?errh(invalid_ip_access_register()),
    ?errh(invalid_ip_access_register_ipv6()),
    ?errh(ip_access_register(<<"hello">>)),
    ?errh(ip_access_register(<<"0.d">>)),
    ?cfgh(modopts(Host, mod_register, [{welcome_message, {"Subject", "Body"}}]),
         welcome_message()),
    %% List of jids
    ?cfgh(modopts(Host, mod_register, [{registration_watchers,
                                       [<<"alice@bob">>, <<"ilovemongoose@help">>]}]),
         registration_watchers([<<"alice@bob">>, <<"ilovemongoose@help">>])),
    ?errh(registration_watchers([<<"alice@bob">>, <<"jids@have@no@feelings!">>])),
    %% non-negative integer
    ?cfgh(modopts(Host, mod_register, [{password_strength, 42}]),
         password_strength_register(42)),
    ?errh(password_strength_register(<<"42">>)),
    ?errh(password_strength_register(<<"strong">>)),
    ?errh(password_strength_register(-150)),
    ?errh(welcome_message(<<"Subject">>, 1)),
    ?errh(welcome_message(1, <<"Body">>)),
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

mod_roster(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_roster">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_roster, Cfg) end,

    ?cfgh(M(Host, [{versioning, false}]),
        T(#{<<"versioning">> => false})),
    ?cfgh(M(Host, [{store_current_id, false}]),
       T(#{<<"store_current_id">> => false})),
    ?cfgh(M(Host, [{backend, mnesia}]),
       T(#{<<"backend">> => <<"mnesia">>})),
    ?cfgh(M(Host, [{bucket_type, <<"rosters">>}]),
       T(#{<<"riak">> => #{<<"bucket_type">> => <<"rosters">>}})),
    ?cfgh(M(Host, [{version_bucket_type, <<"roster_versions">>}]),
       T(#{<<"riak">> => #{<<"version_bucket_type">> => <<"roster_versions">>}})),

    ?errh(T(#{<<"versioning">> => 1})),
    ?errh(T(#{<<"store_current_id">> => 1})),
    ?errh(T(#{<<"backend">> => 1})),
    ?errh(T(#{<<"backend">> => <<"iloveyou">>})),
    ?errh(T(#{<<"riak">> => #{<<"version_bucket_type">> => 1}})),
    ?errh(T(#{<<"riak">> => #{<<"bucket_type">> => 1}})),
    check_iqdisc(mod_roster).

mod_shared_roster_ldap(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_shared_roster_ldap">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_shared_roster_ldap, Cfg) end,
    ?cfgh(M(Host, [{ldap_pool_tag, default}]),
       T(#{<<"ldap_pool_tag">> => <<"default">>})),
    ?cfgh(M(Host, [{ldap_base,  "string"}]),
       T(#{<<"ldap_base">> => <<"string">>})),
    ?cfgh(M(Host, [{ldap_deref, never}]),
       T(#{<<"ldap_deref">> => <<"never">>})),
    %% Options: attributes
    ?cfgh(M(Host, [ {ldap_groupattr, "cn"}]),
       T(#{<<"ldap_groupattr">> => <<"cn">>})),
    ?cfgh(M(Host, [{ldap_groupdesc, "default"}]),
       T(#{<<"ldap_groupdesc">> => <<"default">>})),
    ?cfgh(M(Host, [{ldap_userdesc, "cn"}]),
       T(#{<<"ldap_userdesc">> => <<"cn">>})),
    ?cfgh(M(Host, [{ldap_useruid, "cn"}]),
       T(#{<<"ldap_useruid">> => <<"cn">>})),
    ?cfgh(M(Host, [{ldap_memberattr, "memberUid"}]),
       T(#{<<"ldap_memberattr">> => <<"memberUid">>})),
    ?cfgh(M(Host, [{ldap_memberattr_format, "%u"}]),
       T(#{<<"ldap_memberattr_format">> => <<"%u">>})),
    ?cfgh(M(Host, [{ldap_memberattr_format_re,""}]),
       T(#{<<"ldap_memberattr_format_re">> => <<"">>})),
    %% Options: parameters
    ?cfgh(M(Host, [ {ldap_auth_check, true}]),
       T(#{<<"ldap_auth_check">> => true})),
    ?cfgh(M(Host, [{ldap_user_cache_validity, 300}]),
       T(#{<<"ldap_user_cache_validity">> => 300})),
    ?cfgh(M(Host, [{ldap_group_cache_validity, 300}]),
       T(#{<<"ldap_group_cache_validity">> => 300})),
    ?cfgh(M(Host, [{ldap_user_cache_size, 300}]),
       T(#{<<"ldap_user_cache_size">> => 300})),
    ?cfgh(M(Host, [{ldap_group_cache_size, 300}]),
       T(#{<<"ldap_group_cache_size">> => 300})),
    %% Options: LDAP filters
    ?cfgh(M(Host, [{ldap_rfilter, "rfilter_test"}]),
       T(#{<<"ldap_rfilter">> => <<"rfilter_test">>})),
    ?cfgh(M(Host, [{ldap_gfilter, "gfilter_test"}]),
       T(#{<<"ldap_gfilter">> => <<"gfilter_test">>})),
    ?cfgh(M(Host, [{ldap_ufilter, "ufilter_test"}]),
       T(#{<<"ldap_ufilter">> => <<"ufilter_test">>})),
   ?cfgh(M(Host, [{ldap_filter, "filter_test"}]),
       T(#{<<"ldap_filter">> => <<"filter_test">>})),
    ?errh(T(#{<<"ldap_pool_tag">> => 1})),
    ?errh(T(#{<<"ldap_base">> => 1})),
    ?errh(T(#{<<"ldap_deref">> => 1})),
    %% Options: attributes
    ?errh(T(#{<<"ldap_groupattr">> => 1})),
    ?errh(T(#{<<"ldap_groupdesc">> => 1})),
    ?errh(T(#{<<"ldap_userdesc">> => 1})),
    ?errh(T(#{<<"ldap_useruid">> => 1})),
    ?errh(T(#{<<"ldap_memberattr">> => 1})),
    ?errh(T(#{<<"ldap_memberattr_format">> => 1})),
    ?errh(T(#{<<"ldap_memberattr_format_re">> => 1})),
    %% Options: parameters
    ?errh(T(#{<<"ldap_auth_check">> => 1})),
    ?errh(T(#{<<"ldap_user_cache_validity">> => -1})),
    ?errh(T(#{<<"ldap_group_cache_validity">> => -1})),
    ?errh(T(#{<<"ldap_user_cache_size">> => -1})),
    ?errh(T(#{<<"ldap_group_cache_size">> => -1})),
    %% Options: LDAP filters
    ?errh(T(#{<<"ldap_rfilter">> => 1})),
    ?errh(T(#{<<"ldap_gfilter">> => 1})),
    ?errh(T(#{<<"ldap_ufilter">> => 1})),
    ?errh(T(#{<<"ldap_filter">> => 1})).

mod_sic(_Config) ->
    check_iqdisc(mod_sic),
    ?cfgh(modopts(Host, mod_sic, []), #{<<"modules">> => #{<<"mod_sic">> => #{}}}).

mod_stream_management(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_stream_management">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_stream_management, Cfg) end,
    ?cfgh(M(Host, [{buffer_max, no_buffer}]),  T(#{<<"buffer">> => false})),
    ?cfgh(M(Host, [{buffer_max, 10}]),  T(#{<<"buffer_max">> => 10})),
    ?cfgh(M(Host, [{ack_freq, never}]), T(#{<<"ack">> => false})),
    ?cfgh(M(Host, [{ack_freq, 1}]), T(#{<<"ack_freq">> => 1})),
    ?cfgh(M(Host, [{resume_timeout, 600}]), T(#{<<"resume_timeout">> => 600})),

    ?errh(T(#{<<"buffer">> => 0})),
    ?errh(T(#{<<"buffer_max">> => -1})),
    ?errh(T(#{<<"ack">> => <<"false">>})),
    ?errh(T(#{<<"ack_freq">> => 0})),
    ?errh(T(#{<<"resume_timeout">> => true})).

mod_stream_management_stale_h(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
        #{<<"mod_stream_management">> => #{<<"stale_h">> => Opts}}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_stream_management, [{stale_h, Cfg}]) end,
    ?cfgh(M(Host, [{enabled, true}]), T(#{<<"enabled">> => true})),
    ?cfgh(M(Host, [{stale_h_repeat_after, 1800}]), T(#{<<"repeat_after">> => 1800})),
    ?cfgh(M(Host, [{stale_h_geriatric, 3600}]), T(#{<<"geriatric">> => 3600})),

    ?errh(T(#{<<"enabled">> => <<"true">>})),
    ?errh(T(#{<<"repeat_after">> => -1})),
    ?errh(T(#{<<"geriatric">> => <<"one">>})).

mod_time(_Config) ->
    check_iqdisc(mod_time),
    ?cfgh(modopts(Host, mod_time, []), #{<<"modules">> => #{<<"mod_time">> => #{}}}).

mod_vcard(_Config) ->
    check_iqdisc(mod_vcard),
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_vcard">> => Opts}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_vcard, Cfg) end,
    ?cfgh(M(Host, [{iqdisc, one_queue}]),
        T(#{<<"iqdisc">> => #{<<"type">> => <<"one_queue">>}})),
    ?cfgh(M(Host, [{host, {prefix, <<"vjud.">>}}]),
         T(#{<<"host">> => <<"vjud.@HOST@">>})),
    ?cfgh(M(Host, [{host, {fqdn, <<"vjud.test">>}}]),
         T(#{<<"host">> => <<"vjud.test">>})),
    ?cfgh(M(Host, [{search, true}]),
        T(#{<<"search">> => true})),
    ?cfgh(M(Host, [{backend, mnesia}]),
        T(#{<<"backend">> => <<"mnesia">>})),
    ?cfgh(M(Host, [{matches, infinity}]),
        T(#{<<"matches">> => <<"infinity">>})),
    %% ldap
    ?cfgh(M(Host, [{ldap_pool_tag, default}]),
        T(#{<<"ldap_pool_tag">> => <<"default">>})),
    ?cfgh(M(Host, [{ldap_base, "ou=Users,dc=ejd,dc=com"}]),
        T(#{<<"ldap_base">> => <<"ou=Users,dc=ejd,dc=com">>})),
    ?cfgh(M(Host, [{ldap_filter, "(&(objectClass=shadowAccount)(memberOf=Jabber Users))"}]),
        T(#{<<"ldap_filter">> => <<"(&(objectClass=shadowAccount)(memberOf=Jabber Users))">>})),
    ?cfgh(M(Host, [{ldap_deref, never}]),
        T(#{<<"ldap_deref">> => <<"never">>})),
    ?cfgh(M(Host, [{ldap_search_operator, 'or'}]),
        T(#{<<"ldap_search_operator">> => <<"or">>})),
    ?cfgh(M(Host, [{ldap_binary_search_fields, [<<"PHOTO">>]}]),
        T(#{<<"ldap_binary_search_fields">> => [<<"PHOTO">>]})),
    %% riak
    ?cfgh(M(Host, [{bucket_type, <<"vcard">>}]),
        T(#{<<"riak">> =>  #{<<"bucket_type">> => <<"vcard">>}})),
    ?cfgh(M(Host, [{search_index, <<"vcard">>}]),
        T(#{<<"riak">> =>  #{<<"search_index">> => <<"vcard">>}})),

    ?errh(T(#{<<"host">> => 1})),
    ?errh(T(#{<<"host">> => <<"is this a host? no.">>})),
    ?errh(T(#{<<"host">> => [<<"invalid.sub@HOST@">>]})),
    ?errh(T(#{<<"host">> => [<<"invalid.sub.@HOST@.as.well">>]})),
    ?errh(T(#{<<"search">> => 1})),
    ?errh(T(#{<<"backend">> => <<"mememesia">>})),
    ?errh(T(#{<<"matches">> => -1})),
    %% ldap
    ?errh(T(#{<<"ldap_pool_tag">> => -1})),
    ?errh(T(#{<<"ldap_base">> => -1})),
    ?errh(T(#{<<"ldap_field">> => -1})),
    ?errh(T(#{<<"ldap_deref">> => <<"nevernever">>})),
    ?errh(T(#{<<"ldap_search_operator">> => <<"more">>})),
    ?errh(T(#{<<"ldap_binary_search_fields">> => [1]})),
    %% riak
    ?errh(T(#{<<"riak">> =>  #{<<"bucket_type">> => 1}})),
    ?errh(T(#{<<"riak">> =>  #{<<"search_index">> => 1}})).

mod_vcard_ldap_uids(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                        #{<<"mod_vcard">> => #{<<"ldap_uids">> => Opts}}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_vcard, [{ldap_uids, Cfg}]) end,
    RequiredOpts = #{<<"attr">> => <<"name">>},
    ExpectedCfg = "name",
    ?cfgh(M(Host, []), T([])),
    ?cfgh(M(Host, [ExpectedCfg]), T([RequiredOpts])),
    ?cfgh(M(Host, [{"name", "%u@mail.example.org"}]),
        T([RequiredOpts#{<<"format">> => <<"%u@mail.example.org">>}])),
    ?cfgh(M(Host, [{"name", "%u@mail.example.org"}, ExpectedCfg]),
        T([RequiredOpts#{<<"format">> => <<"%u@mail.example.org">>}, RequiredOpts])),
    [?errh(T([maps:remove(Key, RequiredOpts)])) || Key <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"attr">> := 1})),
    ?errh(T(RequiredOpts#{<<"format">> => true})).

mod_vcard_ldap_vcard_map(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                        #{<<"mod_vcard">> => #{<<"ldap_vcard_map">> => Opts}}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_vcard, [{ldap_vcard_map, Cfg}]) end,
    RequiredOpts = #{<<"vcard_field">> => <<"FAMILY">>,
                     <<"ldap_pattern">> => <<"%s">>,
                     <<"ldap_field">> => <<"sn">>},
    ExpectedCfg = {<<"FAMILY">>, <<"%s">>, [<<"sn">>]},
    ?cfgh(M(Host, []), T([])),
    ?cfgh(M(Host, [ExpectedCfg]), T([RequiredOpts])),
    [?errh(T([maps:remove(Key, RequiredOpts)])) || Key <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"vcard_field">> := false})),
    ?errh(T(RequiredOpts#{<<"ldap_pattern">> := false})),
    ?errh(T(RequiredOpts#{<<"ldap_field">> := -1})).

mod_vcard_ldap_search_fields(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                        #{<<"mod_vcard">> => #{<<"ldap_search_fields">> => Opts}}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_vcard, [{ldap_search_fields, Cfg}]) end,
    RequiredOpts = #{<<"search_field">> => <<"Full Name">>,
                     <<"ldap_field">> => <<"cn">>},
    ExpectedCfg = {<<"Full Name">>, <<"cn">>},
    ?cfgh(M(Host, []), T([])),
    ?cfgh(M(Host, [ExpectedCfg]), T([RequiredOpts])),
    [?errh(T([maps:remove(Key, RequiredOpts)])) || Key <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"search_field">> := false})),
    ?errh(T(RequiredOpts#{<<"ldap_field">> := -1})).

mod_vcard_ldap_search_reported(_Config) ->
    T = fun(Opts) -> #{<<"modules">> =>
                        #{<<"mod_vcard">> => #{<<"ldap_search_reported">> => Opts}}} end,
    M = fun(Host, Cfg) -> modopts(Host, mod_vcard, [{ldap_search_reported, Cfg}]) end,
    RequiredOpts = #{<<"search_field">> => <<"Full Name">>,
                     <<"vcard_field">> => <<"FN">>},
    ExpectedCfg = {<<"Full Name">>, <<"FN">>},
    ?cfgh(M(Host, []), T([])),
    ?cfgh(M(Host, [ExpectedCfg]), T([RequiredOpts])),
    [?errh(T([maps:remove(Key, RequiredOpts)])) || Key <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"search_field">> := false})),
    ?errh(T(RequiredOpts#{<<"vcard_field">> := -1})).

mod_version(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_version">> => Opts}} end,
    ?cfgh(modopts(Host, mod_version, [{os_info, false}]), T(#{<<"os_info">> => false})),
    ?errh(T(#{<<"os_info">> => 1})),
    check_iqdisc(mod_version).

modules_without_config(_Config) ->
    ?cfgh(modopts(Host, mod_amp, []), #{<<"modules">> => #{<<"mod_amp">> => #{}}}),
    ?errh(#{<<"modules">> => #{<<"mod_wrong">> => #{}}}).

%% Services

service_admin_extra(_Config) ->
    T = fun(Opts) -> #{<<"services">> => #{<<"service_admin_extra">> => Opts}} end,
    ?cfg(servopts(service_admin_extra, [{submods, [node]}]),
        parse(T(#{<<"submods">> => [<<"node">>]}))),
    ?err(parse(T(#{<<"submods">> => 1}))),
    ?err(parse(T(#{<<"submods">> => [1]}))),
    ?err(parse(T(#{<<"submods">> => [<<"nodejshaha">>]}))),
    ok.

service_mongoose_system_metrics(_Config) ->
    M = service_mongoose_system_metrics,
    T = fun(Opts) -> #{<<"services">> => #{<<"service_mongoose_system_metrics">> => Opts}} end,
    ?cfg(servopts(M, [{initial_report, 5000}]),
        parse(T(#{<<"initial_report">> => 5000}))),
    ?cfg(servopts(M, [{periodic_report, 5000}]),
        parse(T(#{<<"periodic_report">> => 5000}))),
    ?cfg(servopts(M, [{tracking_id, "UA-123456789"}]),
        parse(T(#{<<"tracking_id">> => <<"UA-123456789">>}))),
    ?cfg(servopts(M, [no_report]),
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

iq_disc_generic(Module, RequiredOpts, Value) ->
    Opts = RequiredOpts#{<<"iqdisc">> => Value},
    #{<<"modules">> => #{atom_to_binary(Module, utf8) => Opts}}.

check_iqdisc(Module) ->
    check_iqdisc(Module, [], #{}).

check_iqdisc(Module, ExpectedCfg, RequiredOpts) ->
    ?cfgh(modopts(Host, Module, ExpectedCfg ++ [{iqdisc, {queues, 10}}]),
         iq_disc_generic(Module, RequiredOpts, iqdisc({queues, 10}))),
    ?cfgh(modopts(Host, Module, ExpectedCfg ++ [{iqdisc, parallel}]),
         iq_disc_generic(Module, RequiredOpts, iqdisc(parallel))),
    ?errh(iq_disc_generic(Module, RequiredOpts, iqdisc(bad_haha))).

modopts(Host, Mod, Opts) ->
    [#local_config{key = {modules, Host}, value = [{Mod, Opts}]}].

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

assert_host_or_global(ResultF, Config) ->
    assert_options(ResultF(global), parse(Config)), % check for the 'global' host
    assert_options(ResultF(?HOST), parse_host_config(Config)). % check for a single host

err_host_or_global(Config) ->
    ?err(parse(Config)),
    ?err(parse_host_config(Config)).

parse_host_config(Config) ->
    parse(#{<<"host_config">> => [Config#{<<"host_type">> => ?HOST}]}).

parse(M0) ->
    %% 'hosts' (or 'host_types') and `default_server_domain` options are mandatory.
    %% this function does the following things:
    %%   1) plugs that mandatory options with dummy values (if required).
    %%   2) executes parsing.
    %% DummyDomainName value must be unique to avoid accidental config keys removal.
    DummyDomainName = <<"dummy.domain.name">>,
    M = maybe_insert_dummy_domain(M0, DummyDomainName),
    mongoose_config_parser_toml:parse(M).

maybe_insert_dummy_domain(M, DomainName) ->
    DummyGenM = #{<<"default_server_domain">> => DomainName,
                  <<"hosts">> => [DomainName]},
    OldGenM = maps:get(<<"general">>, M, #{}),
    NewGenM = maps:merge(DummyGenM, OldGenM),
    M#{<<"general">> => NewGenM}.

%% helpers for testing individual options

assert_options(ExpectedOptions, Config) ->
    [assert_option(ExpectedOption, Config) || ExpectedOption <- ExpectedOptions],
    ok.

assert_option(ExpectedOpt = #local_config{key = Key}, Config) ->
    case lists:keyfind(Key, #local_config.key, Config) of
        false -> ct:fail({"option not found", ExpectedOpt, Config});
        ActualOpt -> handle_config_option(ExpectedOpt, ActualOpt)
    end.

%% helpers for file tests

test_config_file(Config, File) ->
    OptionsPath = ejabberd_helper:data(Config, File ++ ".options"),
    {ok, ExpectedOpts} = file:consult(OptionsPath),

    TOMLPath = ejabberd_helper:data(Config, File ++ ".toml"),
    State = mongoose_config_parser:parse_file(TOMLPath),
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
compare_values({modules, _}, [{mod_extdisco, V1}], [{mod_extdisco, V2}]) ->
    compare_ordered_lists(V1, V2, fun compare_unordered_lists/2);
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
