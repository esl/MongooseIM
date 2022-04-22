-module(config_parser_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(HOST, <<"example.com">>).

-define(eq(Expected, Actual), ?assertEqual(Expected, Actual)).

%% Assertions

%% global config options
-define(cfg(Key, Value, RawConfig), ?cfg([{Key, Value}], RawConfig)).
-define(cfg(ExpectedOpts, RawConfig), assert_options(ExpectedOpts, parse(RawConfig))).

%% global config error
-define(err(RawConfig), ?err(_, RawConfig)).
-define(err(Pattern, RawConfig), ?assertError({config_error, _, Pattern}, parse(RawConfig))).

%% host-or-global config options
-define(cfgh(KeyPrefix, Value, RawConfig), ?cfgh([{KeyPrefix, Value}], RawConfig)).
-define(cfgh(ExpectedOpts, RawConfig),
        begin
            ?cfg(host_opts(ExpectedOpts), RawConfig),
            ?cfg(host_opts(ExpectedOpts), host_config(RawConfig))
        end).

%% host-or-global config error
-define(errh(RawConfig), ?errh(_, RawConfig)).
-define(errh(Pattern, RawConfig),
        begin
            ?err(Pattern, RawConfig),
            ?err(Pattern, host_config(RawConfig))
        end).

-import(mongoose_config_parser_toml, [extract_errors/1]).
-import(config_parser_helper, [merge_with_default_pool_config/1, default_s2s/0,
                               extra_service_listener_config/0,
                               mod_event_pusher_http_handler/0,
                               mod_config/2, default_mod_config/1,
                               config/2, default_config/1]).

-type key_prefix() :: top_level_key_prefix() | key_path_prefix().
-type top_level_key_prefix() :: atom().
-type key_path_prefix() :: [atom() | binary()].

all() ->
    [{group, file},
     {group, dynamic_domains},
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
                         outgoing_pools,
                         host_types_file]},
     {dynamic_domains, [parallel], [supported_features,
                                    unsupported_features]},
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
                            hide_service_name,
                            domain_certfile]},
     {listen, [parallel], [listen_duplicate,
                           listen_c2s,
                           listen_c2s_fast_tls,
                           listen_c2s_just_tls,
                           listen_s2s,
                           listen_s2s_tls,
                           listen_service,
                           listen_http,
                           listen_http_tls,
                           listen_http_transport,
                           listen_http_handlers,
                           listen_http_handlers_websockets,
                           listen_http_handlers_lasse,
                           listen_http_handlers_static,
                           listen_http_handlers_api,
                           listen_http_handlers_domain]},
     {auth, [parallel], [auth_methods,
                         auth_password,
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
                         auth_external,
                         auth_http_basic_auth,
                         auth_jwt,
                         auth_riak_bucket_type,
                         auth_rdbms_users_number_estimate,
                         auth_dummy]},
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
                         pool_ldap_port,
                         pool_ldap_servers,
                         pool_ldap_encrypt,
                         pool_ldap_rootdn,
                         pool_ldap_password,
                         pool_ldap_connect_interval,
                         pool_ldap_tls]},
     {shaper_acl_access, [parallel], [shaper,
                                      acl,
                                      acl_merge_host_and_global,
                                      access,
                                      access_merge_host_and_global]},
     {s2s, [parallel], [s2s_host_config,
                        s2s_dns_timeout,
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
                        s2s_shared,
                        s2s_max_retry_delay]},
     {modules, [parallel], [mod_adhoc,
                            mod_auth_token,
                            mod_blocking,
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
                            mod_mam_meta_riak,
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
                            mod_offline_chatmarkers,
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
                            mod_smart_markers,
                            mod_stream_management,
                            mod_stream_management_stale_h,
                            mod_time,
                            mod_vcard,
                            mod_vcard_ldap_uids,
                            mod_vcard_ldap_vcard_map,
                            mod_vcard_ldap_search_fields,
                            mod_vcard_ldap_search_reported,
                            mod_version,
                            modules_without_config,
                            incorrect_module]},
     {services, [parallel], [service_admin_extra,
                             service_domain_db,
                             service_mongoose_system_metrics]}
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(jid),
    create_files(Config),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(dynamic_domains, Config) ->
    meck:new(ejabberd_auth_http, [passthrough, no_link]),
    meck:new(mod_test, [non_strict, no_link]),
    meck:expect(ejabberd_auth_http, supported_features, fun() -> [] end),
    meck:expect(mod_test, supported_features, fun() -> [] end),
    Config;
init_per_group(_, Config) ->
    Config.

end_per_group(dynamic_domains, _Config) ->
    meck:unload();
end_per_group(_, _Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
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

host_types_file(Config) ->
    test_config_file(Config, "host_types").

supported_features(_Config) ->
    Gen = #{<<"general">> => #{<<"host_types">> => [<<"type1">>, <<"type2">>]}},
    Auth = #{<<"auth">> => #{<<"internal">> => #{}}},
    Mod = #{<<"modules">> => #{<<"mod_amp">> => #{}}},
    ?cfg([{auth, <<"type1">>}, methods], [internal], maps:merge(Gen, Auth)),
    ?cfg([{auth, <<"type1">>}, methods], [internal],
         Gen#{<<"host_config">> => [Auth#{<<"host_type">> => <<"type1">>}]}),
    ?cfg([{modules, <<"type1">>}, mod_amp], #{}, maps:merge(Gen, Mod)),
    ?cfg([{modules, <<"type1">>}, mod_amp], #{},
          Gen#{<<"host_config">> => [Mod#{<<"host_type">> => <<"type1">>}]}).

unsupported_features(_Config) ->
    % ejabberd_auth_http and mod_test are mocked and they don't support dynamic domains
    Gen = #{<<"general">> => #{<<"host_types">> => [<<"type1">>, <<"type2">>]}},
    Auth = #{<<"auth">> => #{<<"http">> => #{}}},
    Mod = #{<<"modules">> => #{<<"mod_test">> => #{}}},
    ?err([#{reason := dynamic_domains_not_supported,
            unsupported_auth_methods := [http],
            unsupported_modules := []}],
         maps:merge(Gen, Auth)),
    ?err([#{reason := dynamic_domains_not_supported,
            unsupported_auth_methods := [http],
            unsupported_modules := []}],
         Gen#{<<"host_config">> => [Auth#{<<"host_type">> => <<"type1">>}]}),
    ?err([#{reason := dynamic_domains_not_supported,
            unsupported_auth_methods := [],
            unsupported_modules := [mod_test]}],
         maps:merge(Gen, Mod)),
    ?err([#{reason := dynamic_domains_not_supported,
            unsupported_auth_methods := [],
            unsupported_modules := [mod_test]}],
         Gen#{<<"host_config">> => [Mod#{<<"host_type">> => <<"type1">>}]}).

%% tests: general
loglevel(_Config) ->
    ?cfg(loglevel, warning, #{}), % default
    ?cfg(loglevel, debug, #{<<"general">> => #{<<"loglevel">> => <<"debug">>}}),
    ?err(#{<<"general">> => #{<<"loglevel">> => <<"bebug">>}}),
    %% make sure non-host options are not accepted in host_config
    ?err(host_config(#{<<"general">> => #{<<"loglevel">> => <<"debug">>}})).

hosts(_Config) ->
    ?cfg(hosts, [], % default
         #{<<"general">> => #{<<"host_types">> => [<<"type1">>]}, without => [<<"hosts">>]}),
    ?cfg(hosts, [<<"host1">>],
         #{<<"general">> => #{<<"hosts">> => [<<"host1">>]}}),
    ?cfg(hosts, [<<"host1">>, <<"host2">>],
         #{<<"general">> => #{<<"hosts">> => [<<"host1">>, <<"host2">>]}}),
    ?err(#{<<"general">> => #{<<"hosts">> => [<<"what is this?">>]}}),
    ?err(#{<<"general">> => #{<<"hosts">> => [<<>>]}}),
    ?err(#{<<"general">> => #{<<"hosts">> => [<<"host1">>, <<"host1">>]}}),
    %% at least one host or host_type must be provided
    ?err(#{<<"general">> => #{}, without => [<<"hosts">>]}),
    ?err(#{<<"general">> => #{<<"hosts">> => []}}),
    ?err(#{<<"general">> => #{<<"host_types">> => []}, without => [<<"hosts">>]}),
    ?err(#{<<"general">> => #{<<"hosts">> => [], <<"host_types">> => []}}).

host_types(_Config) ->
    ?cfg(host_types, [], #{}), % default
    ?cfg([{host_types, [<<"type 1">>]},
          {hosts, []}],
         #{<<"general">> => #{<<"host_types">> => [<<"type 1">>]}, without => [<<"hosts">>]}),
    ?cfg([{host_types, [<<"type 1">>, <<"type 2">>]},
          {hosts, []}],
         #{<<"general">> => #{<<"host_types">> => [<<"type 1">>, <<"type 2">>],
                              <<"hosts">> => []}}),
    ?err(#{<<"general">> => #{<<"host_types">> => [<<>>]}}),
    ?err(#{<<"general">> => #{<<"host_types">> => [<<"type1">>, <<"type1">>]}}),
    %% either hosts and host_types cannot have the same values
    ?err(#{<<"general">> => #{<<"host_types">> => [<<"type1">>],
                              <<"hosts">> => [<<"type1">>]}}).

default_server_domain(_Config) ->
    ?cfg(default_server_domain, <<"host1">>,
         #{<<"general">> => #{<<"default_server_domain">> => <<"host1">>}}),
    ?err(#{<<"general">> => #{<<"default_server_domain">> => <<"what is this?">>}}),
    ?err(#{<<"general">> => #{<<"default_server_domain">> => <<>>}}),
    %% default_server_domain must be provided
    ?err(#{without => [<<"default_server_domain">>]}).

registration_timeout(_Config) ->
    ?cfg(registration_timeout, 600, #{}), % default
    ?cfg(registration_timeout, infinity,
         #{<<"general">> => #{<<"registration_timeout">> => <<"infinity">>}}),
    ?cfg(registration_timeout, 300,
         #{<<"general">> => #{<<"registration_timeout">> => 300}}),
    ?err(#{<<"general">> => #{<<"registration_timeout">> => 0}}).

language(_Config) ->
    ?cfg(language, <<"en">>, #{}), % default
    ?cfg(language, <<"pl">>, #{<<"general">> => #{<<"language">> => <<"pl">>}}),
    ?err(#{<<"general">> => #{<<"language">> => <<>>}}).

all_metrics_are_global(_Config) ->
    ?cfg(all_metrics_are_global, false, #{}), % default
    ?cfg(all_metrics_are_global, true, #{<<"general">> => #{<<"all_metrics_are_global">> => true}}),
    ?err(#{<<"general">> => #{<<"all_metrics_are_global">> => <<"true">>}}).

sm_backend(_Config) ->
    ?cfg(sm_backend, mnesia, #{}), % default
    ?cfg(sm_backend, mnesia, #{<<"general">> => #{<<"sm_backend">> => <<"mnesia">>}}),
    ?cfg(sm_backend, redis, #{<<"general">> => #{<<"sm_backend">> => <<"redis">>}}),
    ?err(#{<<"general">> => #{<<"sm_backend">> => <<"amnesia">>}}).

max_fsm_queue(_Config) ->
    ?cfg(max_fsm_queue, 100, #{<<"general">> => #{<<"max_fsm_queue">> => 100}}),
    ?err(#{<<"general">> => #{<<"max_fsm_queue">> => -10}}).

http_server_name(_Config) ->
    ?cfg(cowboy_server_name, "my server",
         #{<<"general">> => #{<<"http_server_name">> => <<"my server">>}}),
    ?err(#{<<"general">> => #{<<"http_server_name">> => #{}}}).

rdbms_server_type(_Config) ->
    ?cfg(rdbms_server_type, generic, #{}), % default
    ?cfg(rdbms_server_type, mssql, #{<<"general">> => #{<<"rdbms_server_type">> => <<"mssql">>}}),
    ?cfg(rdbms_server_type, pgsql, #{<<"general">> => #{<<"rdbms_server_type">> => <<"pgsql">>}}),
    ?err(#{<<"general">> => #{<<"rdbms_server_type">> => <<"nosql">>}}).

route_subdomains(_Config) ->
    ?cfgh(route_subdomains, s2s, #{<<"general">> => #{<<"route_subdomains">> => <<"s2s">>}}),
    ?errh(#{<<"general">> => #{<<"route_subdomains">> => <<"c2s">>}}).

mongooseimctl_access_commands(_Config) ->
    ?cfg(mongooseimctl_access_commands, [], #{}), % default
    AccessRule = #{<<"commands">> => [<<"join_cluster">>],
                   <<"argument_restrictions">> => #{<<"node">> => <<"mim1@host1">>}},
    ?cfg(mongooseimctl_access_commands, [{local, ["join_cluster"], [{node, "mim1@host1"}]}],
         #{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                  #{<<"local">> => AccessRule}}}),
    ?cfg(mongooseimctl_access_commands, [{local, all, [{node, "mim1@host1"}]}],
         #{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                  #{<<"local">> => maps:remove(<<"commands">>, AccessRule)}}}),
    ?cfg(mongooseimctl_access_commands, [{local, ["join_cluster"], []}],
         #{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                  #{<<"local">> => maps:remove(<<"argument_restrictions">>,
                                                               AccessRule)}}}),
    ?cfg(mongooseimctl_access_commands, [{local, all, []}],
         #{<<"general">> => #{<<"mongooseimctl_access_commands">> => #{<<"local">> => #{}}}}),
    ?err(#{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                  #{<<"local">> => #{<<"commands">> => <<"all">>}}}}),
    ?err(#{<<"general">> => #{<<"mongooseimctl_access_commands">> =>
                                  #{<<"local">> => #{<<"argument_restrictions">> =>
                                                         [<<"none">>]}}}}).

routing_modules(_Config) ->
    ?cfg(routing_modules, mongoose_router:default_routing_modules(), #{}), % default
    ?cfg(routing_modules, [mongoose_router_global, mongoose_router_localdomain],
         #{<<"general">> => #{<<"routing_modules">> => [<<"mongoose_router_global">>,
                                                        <<"mongoose_router_localdomain">>]}}),
    ?err(#{<<"general">> => #{<<"routing_modules">> => [<<"moongoose_router_global">>]}}).

replaced_wait_timeout(_Config) ->
    ?cfg({replaced_wait_timeout, ?HOST}, 2000, #{}), % global default
    ?cfgh(replaced_wait_timeout, 1000, #{<<"general">> => #{<<"replaced_wait_timeout">> => 1000}}),
    ?errh(#{<<"general">> => #{<<"replaced_wait_timeout">> => 0}}).

hide_service_name(_Config) ->
    ?cfg(hide_service_name, false, #{}), % default
    ?cfg(hide_service_name, true, #{<<"general">> => #{<<"hide_service_name">> => true}}),
    ?err(#{<<"general">> => #{<<"hide_service_name">> => []}}).

domain_certfile(_Config) ->
    DomCert = #{<<"domain">> => <<"myxmpp.com">>,
                <<"certfile">> => <<"priv/cert.pem">>},
    ?cfg(domain_certfile, #{<<"myxmpp.com">> => "priv/cert.pem"},
         #{<<"general">> => #{<<"domain_certfile">> => [DomCert]}}),
    ?err([#{reason := invalid_filename}],
         #{<<"general">> => #{<<"domain_certfile">> =>
                                  [DomCert#{<<"certfile">> => <<"missing.pem">>}]}}),
    [?err(#{<<"general">> => #{<<"domain_certfile">> => [maps:without([K], DomCert)]}})
     || K <- maps:keys(DomCert)],
    [?err(#{<<"general">> => #{<<"domain_certfile">> => [DomCert#{K := <<>>}]}})
     || K <- maps:keys(DomCert)],
    ?err(#{<<"general">> => #{<<"domain_certfile">> => [DomCert, DomCert]}}).

%% tests: listen

listen_duplicate(_Config) ->
    ?cfg(listen, [listener(c2s, #{port => 5222}),
                  listener(c2s, #{port => 5223})],
         #{<<"listen">> => #{<<"c2s">> => [#{<<"port">> => 5222, <<"ip_address">> => <<"0">>},
                                           #{<<"port">> => 5223}]}}),
    ?err([#{reason := duplicate_listeners,
            duplicates := [{5222, {0, 0, 0, 0}, tcp}]}],
         #{<<"listen">> => #{<<"c2s">> => [#{<<"port">> => 5222, <<"ip_address">> => <<"0">>},
                                           #{<<"port">> => 5222}]}}),
    ?err([#{reason := duplicate_listeners,
            duplicates := [{5222, {0, 0, 0, 0}, tcp}]}],
         #{<<"listen">> => #{<<"c2s">> => [#{<<"port">> => 5222, <<"ip_address">> => <<"0">>}],
                             <<"s2s">> => [#{<<"port">> => 5222}]}}).

listen_c2s(_Config) ->
    T = fun(Opts) -> listen_raw(c2s, maps:merge(#{<<"port">> => 5222}, Opts)) end,
    P = [listen, 1],
    ?cfg(P, config([listen, c2s], #{port => 5222}), T(#{})),
    test_listen(P, T),
    test_listen_xmpp(P, T),
    ?cfg(P ++ [access], rule1, T(#{<<"access">> => <<"rule1">>})),
    ?cfg(P ++ [shaper], c2s_shaper, T(#{<<"shaper">> => <<"c2s_shaper">>})),
    ?cfg(P ++ [zlib], 1024, T(#{<<"zlib">> => 1024})),
    ?cfg(P ++ [max_fsm_queue], 1000, T(#{<<"max_fsm_queue">> => 1000})),
    ?cfg(P ++ [allowed_auth_methods], [rdbms, http],
         T(#{<<"allowed_auth_methods">> => [<<"rdbms">>, <<"http">>]})),
    ?err(T(#{<<"access">> => <<>>})),
    ?err(T(#{<<"shaper">> => <<>>})),
    ?err(T(#{<<"zlib">> => 0})),
    ?err(T(#{<<"max_fsm_queue">> => 0})),
    ?err(T(#{<<"allowed_auth_methods">> => [<<"bad_method">>]})),
    ?err(T(#{<<"allowed_auth_methods">> => [<<"rdbms">>, <<"rdbms">>]})).

listen_c2s_fast_tls(_Config) ->
    T = fun(Opts) -> listen_raw(c2s, #{<<"port">> => 5222,
                                       <<"tls">> => Opts}) end,
    P = [listen, 1, tls],
    ?cfg(P, [], T(#{<<"module">> => <<"fast_tls">>})),
    ?cfg(P, [starttls], T(#{<<"mode">> => <<"starttls">>})),
    ?cfg(P, [verify_peer], T(#{<<"verify_peer">> => true})),
    ?cfg(P, [verify_none], T(#{<<"verify_peer">> => false})),
    ?cfg(P, [{certfile, "priv/cert.pem"}], T(#{<<"certfile">> => <<"priv/cert.pem">>})),
    ?cfg(P, [{cafile, "priv/ca.pem"}], T(#{<<"cacertfile">> => <<"priv/ca.pem">>})),
    ?cfg(P, [{dhfile, "priv/dh.pem"}], T(#{<<"dhfile">> => <<"priv/dh.pem">>})),
    ?cfg(P, [{ciphers, "TLS_AES_256_GCM_SHA384"}],
         T(#{<<"ciphers">> => <<"TLS_AES_256_GCM_SHA384">>})),
    ?cfg(P, [{protocol_options, ["nosslv2"]}], T(#{<<"protocol_options">> => [<<"nosslv2">>]})),
    ?err(T(#{<<"mode">> => <<"stopttls">>})),
    ?err(T(#{<<"module">> => <<"slow_tls">>})),
    ?err(T(#{<<"verify_peer">> => <<"maybe">>})),
    ?err(T(#{<<"crl_files">> => [<<"file1">>, <<"file2">>]})), % only for just_tls
    ?err(T(#{<<"certfile">> => <<"no_such_file.pem">>})),
    ?err(T(#{<<"cacertfile">> => <<"no_such_file.pem">>})),
    ?err(T(#{<<"dhfile">> => <<"no_such_file.pem">>})),
    ?err(T(#{<<"ciphers">> => [<<"TLS_AES_256_GCM_SHA384">>]})),
    ?err(T(#{<<"protocol_options">> => [<<>>]})).

listen_c2s_just_tls(_Config) ->
    T = fun(Opts) -> listen_raw(c2s, #{<<"port">> => 5222,
                                       <<"tls">> => Opts#{<<"module">> => <<"just_tls">>}}) end,
    P = [listen, 1, tls],
    BaseOpts = [{tls_module, just_tls}],
    ?cfg(P, BaseOpts, T(#{})),
    ?cfg(P, BaseOpts ++ [{ssl_options, [{verify_fun, {selfsigned_peer, false}}]}],
         T(#{<<"verify_mode">> => <<"selfsigned_peer">>, <<"disconnect_on_failure">> => false})),
    ?cfg(P,  BaseOpts ++ [{ssl_options, [{verify_fun, {peer, true}}]}],
         T(#{<<"verify_mode">> => <<"peer">>})),
    ?cfg(P,  BaseOpts ++ [{crlfiles, ["file1", "file2"]}],
         T(#{<<"crl_files">> => [<<"file1">>, <<"file2">>]})),
    ?cfg(P,  BaseOpts ++ [{ssl_options, [{certfile, "priv/cert.pem"}]}],
         T(#{<<"certfile">> => <<"priv/cert.pem">>})),
    ?cfg(P,  BaseOpts ++ [{ssl_options, [{cacertfile, "priv/ca.pem"}]}],
         T(#{<<"cacertfile">> => <<"priv/ca.pem">>})),
    ?cfg(P,  BaseOpts ++ [{ssl_options, [{dhfile, "priv/dh.pem"}]}],
         T(#{<<"dhfile">> => <<"priv/dh.pem">>})),
    ?cfg(P,  BaseOpts ++ [{ssl_options, [{ciphers, "TLS_AES_256_GCM_SHA384"}]}],
         T(#{<<"ciphers">> => <<"TLS_AES_256_GCM_SHA384">>})),
    ?cfg(P,  BaseOpts ++ [{ssl_options, [{versions, ['tlsv1.2', 'tlsv1.3']}]}],
         T(#{<<"versions">> => [<<"tlsv1.2">>, <<"tlsv1.3">>]})),
    ?err(T(#{<<"verify_mode">> => <<"whatever">>})),
    ?err(T(#{<<"verify_mode">> => <<"peer">>, <<"disconnect_on_failure">> => <<"sometimes">>})),
    ?err(T(#{<<"crl_files">> => [<<>>]})),
    ?err(T(#{<<"versions">> => <<"tlsv1.2">>})),
    ?err(T(#{<<"protocol_options">> => [<<"nosslv2">>]})). % only for fast_tls

listen_s2s(_Config) ->
    T = fun(Opts) -> listen_raw(s2s, maps:merge(#{<<"port">> => 5269}, Opts)) end,
    P = [listen, 1],
    ?cfg(P, config([listen, s2s], #{port => 5269}), T(#{})),
    test_listen(P, T),
    test_listen_xmpp(P, T),
    ?cfg(P ++ [shaper], s2s_shaper, T(#{<<"shaper">> => <<"s2s_shaper">>})),
    ?err(T(#{<<"shaper">> => <<>>})).

listen_s2s_tls(_Config) ->
    T = fun(Opts) -> listen_raw(s2s, #{<<"port">> => 5269, <<"tls">> => Opts}) end,
    P = [listen, 1, tls],
    ?cfg(P, [{cafile, "priv/ca.pem"}], T(#{<<"cacertfile">> => <<"priv/ca.pem">>})),
    ?cfg(P, [{dhfile, "priv/dh.pem"}], T(#{<<"dhfile">> => <<"priv/dh.pem">>})),
    ?cfg(P, [{ciphers, "TLS_AES_256_GCM_SHA384"}],
         T(#{<<"ciphers">> => <<"TLS_AES_256_GCM_SHA384">>})),
    ?cfg(P, [{protocol_options, ["nosslv2"]}], T(#{<<"protocol_options">> => [<<"nosslv2">>]})),
    ?err(T(#{<<"cacertfile">> => <<>>})),
    ?err(T(#{<<"dhfile">> => 12})),
    ?err(T(#{<<"ciphers">> => [<<"TLS_AES_256_GCM_SHA384">>]})),
    ?err(T(#{<<"protocol_options">> => [<<>>]})).

listen_service(_Config) ->
    T = fun(Opts) -> listen_raw(service, maps:merge(#{<<"port">> => 8888,
                                                      <<"password">> => <<"secret">>}, Opts))
        end,
    P = [listen, 1],
    ?cfg(P, config([listen, service], #{port => 8888, password => "secret"}), T(#{})),
    test_listen(P, T),
    test_listen_xmpp(P, T),
    ?cfg(P ++ [access], rule1, T(#{<<"access">> => <<"rule1">>})),
    ?cfg(P ++ [shaper_rule], fast, T(#{<<"shaper_rule">> => <<"fast">>})),
    ?cfg(P ++ [check_from], false, T(#{<<"check_from">> => false})),
    ?cfg(P ++ [hidden_components], true, T(#{<<"hidden_components">> => true})),
    ?cfg(P ++ [conflict_behaviour], kick_old, T(#{<<"conflict_behaviour">> => <<"kick_old">>})),
    ?cfg(P ++ [max_fsm_queue], 1000, T(#{<<"max_fsm_queue">> => 1000})),
    ?err(T(#{<<"access">> => <<>>})),
    ?err(T(#{<<"shaper_rule">> => <<>>})),
    ?err(T(#{<<"check_from">> => 1})),
    ?err(T(#{<<"hidden_components">> => <<"yes">>})),
    ?err(T(#{<<"conflict_behaviour">> => <<"kill_server">>})),
    ?err(T(#{<<"password">> => <<>>})),
    ?err(T(#{<<"password">> => undefined})),
    ?err(T(#{<<"max_fsm_queue">> => 0})).

listen_http(_Config) ->
    T = fun(Opts) -> listen_raw(http, maps:merge(#{<<"port">> => 5280}, Opts)) end,
    P = [listen, 1],
    ?cfg(P, config([listen, http], #{port => 5280}), T(#{})),
    test_listen(P, T).

listen_http_tls(_Config) ->
    T = fun(Opts) -> listen_raw(http, #{<<"port">> => 5280, <<"tls">> => Opts}) end,
    P = [listen, 1, tls],
    ?cfg(P, [{verify, verify_peer}], T(#{<<"verify_peer">> => true})),
    ?cfg(P, [{verify_mode, peer}], T(#{<<"verify_mode">> => <<"peer">>})),
    ?cfg(P, [{certfile, "priv/cert.pem"}], T(#{<<"certfile">> => <<"priv/cert.pem">>})),
    ?cfg(P, [{cacertfile, "priv/ca.pem"}], T(#{<<"cacertfile">> => <<"priv/ca.pem">>})),
    ?cfg(P, [{dhfile, "priv/dh.pem"}], T(#{<<"dhfile">> => <<"priv/dh.pem">>})),
    ?cfg(P, [{ciphers, "TLS_AES_256_GCM_SHA384"}],
         T(#{<<"ciphers">> => <<"TLS_AES_256_GCM_SHA384">>})),
    ?err(T(#{<<"verify_peer">> => 0})),
    ?err(T(#{<<"verify_mode">> => <<"pear">>})),
    ?err(T(#{<<"certfile">> => <<>>})),
    ?err(T(#{<<"cacertfile">> => <<>>})),
    ?err(T(#{<<"dhfile">> => 12})),
    ?err(T(#{<<"ciphers">> => [<<"TLS_AES_256_GCM_SHA384">>]})).

listen_http_transport(_Config) ->
    T = fun(Opts) -> listen_raw(http, #{<<"port">> => 5280, <<"transport">> => Opts}) end,
    P = [listen, 1, transport],
    ?cfg(P ++ [num_acceptors], 10, T(#{<<"num_acceptors">> => 10})),
    ?cfg(P ++ [max_connections], 1024, T(#{<<"max_connections">> => 1024})),
    ?err(T(#{<<"num_acceptors">> => 0})),
    ?err(T(#{<<"max_connections">> => -1})).

listen_http_protocol(_Config) ->
    T = fun(Opts) -> listen_raw(http, #{<<"port">> => 5280, <<"protocol">> => Opts}) end,
    P = [listen, 1, protocol],
    ?cfg(P ++ [compress], true, T(#{<<"compress">> => true})),
    ?err(T(#{<<"compress">> => 1})).

listen_http_handlers(_Config) ->
    T = fun(Opts) -> listen_raw(http, #{<<"port">> => 5280, <<"handlers">> => Opts}) end,
    P = [listen, 1, handlers],
    ?cfg(P, [{"_", "/http-bind", mod_bosh, []}],
         T(#{<<"mod_bosh">> => [#{<<"host">> => <<"_">>,
                                  <<"path">> => <<"/http-bind">>}]})),
    ?err(T(#{<<"mod_bosch">> => [#{<<"host">> => <<"dishwasher">>,
                                   <<"path">> => <<"/cutlery">>}]})),
    ?err(T(#{<<"mod_bosh">> => [#{<<"host">> => <<"pathless">>}]})),
    ?err(T(#{<<"mod_bosh">> => [#{<<"host">> => <<>>, <<"path">> => <<"/">>}]})),
    ?err(T(#{<<"mod_bosh">> => [#{<<"path">> => <<"hostless">>}]})).

listen_http_handlers_websockets(_Config) ->
    T = fun(Opts) -> http_handler_raw(<<"mod_websockets">>, Opts) end,
    P = [listen, 1, handlers],
    ?cfg(P, [{"localhost", "/api", mod_websockets, []}], T(#{})),
    ?cfg(P, [{"localhost", "/api", mod_websockets,
              [{service, maps:merge(extra_service_listener_config(), #{password => "secret"})}]
             }],
         T(#{<<"service">> => #{<<"password">> => <<"secret">>}})),
    ?err(T(#{<<"service">> => #{}})).

listen_http_handlers_lasse(_Config) ->
    T = fun(Opts) -> http_handler_raw(<<"lasse_handler">>, Opts) end,
    P = [listen, 1, handlers],
    ?cfg(P, [{"localhost", "/api", lasse_handler, [mongoose_client_api_sse]}],
         T(#{<<"module">> => <<"mongoose_client_api_sse">>})),
    ?err(T(#{<<"module">> => <<"mooongooose_api_ssie">>})),
    ?err(T(#{})).

listen_http_handlers_static(_Config) ->
    T = fun(Opts) -> http_handler_raw(<<"cowboy_static">>, Opts) end,
    P = [listen, 1, handlers],
    ?cfg(P, [{"localhost", "/api", cowboy_static,
              {priv_dir, cowboy_swagger, "swagger",
               [{mimetypes, cow_mimetypes, all}]}
             }],
         T(#{<<"type">> => <<"priv_dir">>, <<"app">> => <<"cowboy_swagger">>,
             <<"content_path">> => <<"swagger">>})),
    ?cfg(P, [{"localhost", "/api", cowboy_static,
              {file, "swagger", [{mimetypes, cow_mimetypes, all}]}
             }],
         T(#{<<"type">> => <<"file">>, <<"content_path">> => <<"swagger">>})),
    ?err(T(#{<<"type">> => <<"priv_dir">>, <<"app">> => <<"cowboy_swagger">>})).

listen_http_handlers_api(_Config) ->
    T = fun(Opts) -> http_handler_raw(<<"mongoose_api">>, Opts) end,
    P = [listen, 1, handlers],
    ?cfg(P, [{"localhost", "/api", mongoose_api,
              [{handlers, [mongoose_api_metrics,
                           mongoose_api_users]}]}
            ],
         T(#{<<"handlers">> => [<<"mongoose_api_metrics">>, <<"mongoose_api_users">>]})),
    ?err(T(#{<<"handlers">> => [<<"not_an_api_module">>]})),
    ?err(T(#{})).

listen_http_handlers_domain(_Config) ->
    T = fun(Opts) -> http_handler_raw(<<"mongoose_domain_handler">>, Opts) end,
    P = [listen, 1, handlers],
    ?cfg(P, [{"localhost", "/api", mongoose_domain_handler,
              [{password, <<"cool">>}, {username, <<"admin">>}]
             }],
         T(#{<<"username">> => <<"admin">>, <<"password">> => <<"cool">>})),
    ?cfg(P, [{"localhost", "/api", mongoose_domain_handler, []}], T(#{})),
    %% Both username and password required. Or none.
    ?err(T(#{<<"username">> => <<"admin">>})),
    ?err(T(#{<<"password">> => <<"cool">>})).

test_listen(P, T) ->
    ?cfg(P ++ [ip_address], "192.168.1.16", T(#{<<"ip_address">> => <<"192.168.1.16">>})),
    ?cfg(P ++ [ip_tuple], {192, 168, 1, 16}, T(#{<<"ip_address">> => <<"192.168.1.16">>})),
    ?cfg(P ++ [ip_version], 4, T(#{<<"ip_address">> => <<"192.168.1.16">>})),
    ?cfg(P ++ [ip_address], "2001:db8:3:4:5:6:7:8",
         T(#{<<"ip_address">> => <<"2001:db8:3:4:5:6:7:8">>})),
    ?cfg(P ++ [ip_tuple], {8193, 3512, 3, 4, 5, 6, 7, 8},
         T(#{<<"ip_address">> => <<"2001:db8:3:4:5:6:7:8">>})),
    ?cfg(P ++ [ip_version], 6,
         T(#{<<"ip_address">> => <<"2001:db8:3:4:5:6:7:8">>})),
    ?cfg(P ++ [ip_version], 4, T(#{<<"ip_version">> => 4})),
    ?cfg(P ++ [ip_version], 6, T(#{<<"ip_version">> => 6})),
    ?cfg(P ++ [ip_address], "::", T(#{<<"ip_version">> => 6})),
    ?cfg(P ++ [ip_tuple], {0, 0, 0, 0, 0, 0, 0, 0}, T(#{<<"ip_version">> => 6})),
    ?cfg(P ++ [proto], tcp, T(#{<<"proto">> => <<"tcp">>})),
    ?err(T(#{<<"ip_address">> => <<"192.168.1.999">>})),
    ?err(T(#{<<"port">> => <<"5222">>})),
    ?err(T(#{<<"port">> => 522222})),
    ?err(T(#{<<"port">> => undefined})),
    ?err(T(#{<<"ip_version">> => 7})),
    ?err(T(#{<<"proto">> => <<"udp">>})). % only TCP is accepted

test_listen_xmpp(P, T) ->
    ?cfg(P ++ [backlog], 10, T(#{<<"backlog">> => 10})),
    ?cfg(P ++ [proxy_protocol], true, T(#{<<"proxy_protocol">> => true})),
    ?cfg(P ++ [hibernate_after], 10, T(#{<<"hibernate_after">> => 10})),
    ?cfg(P ++ [max_stanza_size], 10000, T(#{<<"max_stanza_size">> => 10000})),
    ?cfg(P ++ [num_acceptors], 100, T(#{<<"num_acceptors">> => 100})),
    ?err(T(#{<<"backlog">> => -10})),
    ?err(T(#{<<"proxy_protocol">> => <<"awesome">>})),
    ?err(T(#{<<"hibernate_after">> => -10})),
    ?err(T(#{<<"max_stanza_size">> => <<"unlimited">>})),
    ?err(T(#{<<"num_acceptors">> => 0})).

%% tests: auth

auth_methods(_Config) ->
    ?cfg([{auth, ?HOST}, methods], [], #{}), % global default
    ?cfgh([auth, methods], [], #{<<"auth">> => #{}}), % default
    ?cfgh([auth, methods], [internal, rdbms], % default alphabetical order
          #{<<"auth">> => #{<<"internal">> => #{},
                            <<"rdbms">> => #{}}}),
    ?cfgh([auth, methods], [rdbms, internal], % specified order
          #{<<"auth">> => #{<<"internal">> => #{},
                            <<"rdbms">> => #{},
                            <<"methods">> => [<<"rdbms">>, <<"internal">>]}}),
    ?cfgh([auth, methods], [internal], % only one of the defined methods is enabled
          #{<<"auth">> => #{<<"internal">> => #{},
                            <<"rdbms">> => #{},
                            <<"methods">> => [<<"internal">>]}}),
    ?errh(#{<<"auth">> => #{<<"rdbms">> => <<"enabled">>}}),
    ?errh(#{<<"auth">> => #{<<"supernatural">> => #{}}}),
    ?errh(#{<<"auth">> => #{<<"methods">> => [<<"rdbms">>]}}).

auth_password(_Config) ->
    Defaults = #{format => scram, scram_iterations => 10000},
    ?cfg([{auth, ?HOST}, password], Defaults, #{}), % global default
    ?cfgh([auth, password], Defaults, #{<<"auth">> => #{}}), % default
    ?cfgh([auth, password], Defaults, #{<<"auth">> => #{<<"password">> => #{}}}), % default
    ?cfgh([auth, password, format], plain,
          #{<<"auth">> => #{<<"password">> => #{<<"format">> => <<"plain">>}}}),
    ?errh(#{<<"auth">> => #{<<"password">> => #{<<"format">> => <<"plane">>}}}),
    ?cfgh([auth, password, hash], [sha, sha256],
          #{<<"auth">> => #{<<"password">> => #{<<"hash">> => [<<"sha">>, <<"sha256">>]}}}),
    ?errh(#{<<"auth">> => #{<<"password">> => #{<<"hash">> => [<<"sha1234">>]}}}),
    ?errh(#{<<"auth">> => #{<<"password">> => #{<<"harsh">> => [<<"sha">>]}}}),
    ?cfgh([auth, password, scram_iterations], 1000,
          #{<<"auth">> => #{<<"password">> => #{<<"scram_iterations">> => 1000}}}),
    ?errh(#{<<"auth">> => #{<<"password">> => #{<<"scram_iterations">> => false}}}).

auth_sasl_external(_Config) ->
    ?cfg([{auth, ?HOST}, sasl_external], [standard], #{}), % global default
    ?cfgh([auth, sasl_external], [standard], #{<<"auth">> => #{}}), % default
    ?cfgh([auth, sasl_external], [standard,
                                  common_name,
                                  {mod, cyrsasl_external_verification}],
          #{<<"auth">> => #{<<"sasl_external">> =>
                                [<<"standard">>,
                                 <<"common_name">>,
                                 <<"cyrsasl_external_verification">>]}}),
    ?errh(#{<<"auth">> => #{<<"sasl_external">> => [<<"unknown">>]}}).

auth_sasl_mechanisms(_Config) ->
    Default = cyrsasl:default_modules(),
    ?cfg([{auth, ?HOST}, sasl_mechanisms], Default, #{}), % global default
    ?cfg([{auth, ?HOST}, sasl_mechanisms], Default, #{<<"auth">> => #{}}), % default
    ?cfgh([auth, sasl_mechanisms], [cyrsasl_external, cyrsasl_scram],
          #{<<"auth">> => #{<<"sasl_mechanisms">> => [<<"external">>, <<"scram">>]}}),
    ?errh(#{<<"auth">> => #{<<"sasl_mechanisms">> => [<<"none">>]}}).

auth_allow_multiple_connections(_Config) ->
    ?cfgh([auth, anonymous, allow_multiple_connections], true,
          auth_raw(<<"anonymous">>, #{<<"allow_multiple_connections">> => true})),
    ?errh(auth_raw(<<"anonymous">>, #{<<"allow_multiple_connections">> => <<"yes">>})).

auth_anonymous_protocol(_Config) ->
    ?cfgh([auth, anonymous, protocol], login_anon,
          auth_raw(<<"anonymous">>, #{<<"protocol">> => <<"login_anon">>})),
    ?errh(auth_raw(<<"anonymous">>, #{<<"protocol">> => <<"none">>})).

auth_ldap_pool(_Config) ->
    ?cfgh([auth, ldap, pool_tag], default, auth_ldap_raw(#{})), % default
    ?cfgh([auth, ldap, pool_tag], ldap_pool,
          auth_ldap_raw(#{<<"pool_tag">> => <<"ldap_pool">>})),
    ?errh(auth_ldap_raw(#{<<"pool_tag">> => <<>>})).

auth_ldap_bind_pool(_Config) ->
    ?cfgh([auth, ldap, bind_pool_tag], bind, auth_ldap_raw(#{})), % default
    ?cfgh([auth, ldap, bind_pool_tag], ldap_bind_pool,
          auth_ldap_raw(#{<<"bind_pool_tag">> => <<"ldap_bind_pool">>})),
    ?errh(auth_ldap_raw(#{<<"bind_pool_tag">> => true})).

auth_ldap_base(_Config) ->
    ?cfgh([auth, ldap, base], <<>>, auth_ldap_raw(#{})), % default
    ?cfgh([auth, ldap, base], <<"ou=Users,dc=example,dc=com">>,
          auth_ldap_raw(#{<<"base">> => <<"ou=Users,dc=example,dc=com">>})),
    ?errh(auth_ldap_raw(#{<<"base">> => 10})).

auth_ldap_uids(_Config) ->
    ?cfgh([auth, ldap, uids], [{<<"uid">>, <<"%u">>}], auth_ldap_raw(#{})), % default
    ?cfgh([auth, ldap, uids], [{<<"uid1">>, <<"user=%u">>}],
          auth_ldap_raw(#{<<"uids">> => [#{<<"attr">> => <<"uid1">>,
                                           <<"format">> => <<"user=%u">>}]})),
    ?cfgh([auth, ldap, uids], [<<"uid1">>],
          auth_ldap_raw(#{<<"uids">> => [#{<<"attr">> => <<"uid1">>}]})),
    ?errh(auth_ldap_raw(#{<<"uids">> => [#{<<"format">> => <<"user=%u">>}]})).

auth_ldap_filter(_Config) ->
    ?cfgh([auth, ldap, filter], <<>>, auth_ldap_raw(#{})), % default
    ?cfgh([auth, ldap, filter], <<"(objectClass=inetOrgPerson)">>,
          auth_ldap_raw(#{<<"filter">> => <<"(objectClass=inetOrgPerson)">>})),
    ?errh(auth_ldap_raw(#{<<"filter">> => 10})).

auth_ldap_dn_filter(_Config) ->
    ?cfgh([auth, ldap, dn_filter], {undefined, []}, auth_ldap_raw(#{})), % default
    ?cfgh([auth, ldap, dn_filter], {<<"(user=%u@%d)">>, []},
          auth_ldap_raw(#{<<"dn_filter">> => #{<<"filter">> => <<"(user=%u@%d)">>}})),
    Pattern = <<"(&(name=%s)(owner=%D)(user=%u@%d))">>,
    ?cfgh([auth, ldap, dn_filter], {Pattern, [<<"sn">>]},
          auth_ldap_raw(#{<<"dn_filter">> => #{<<"filter">> => Pattern,
                                               <<"attributes">> => [<<"sn">>]}})),
    ?errh(auth_ldap_raw(#{<<"dn_filter">> => #{<<"attributes">> => [<<"sn">>]}})),
    ?errh(auth_ldap_raw(#{<<"dn_filter">> => #{<<"filter">> => 12}})),
    ?errh(auth_ldap_raw(#{<<"dn_filter">> => #{<<"filter">> => Pattern,
                                               <<"attributes">> => <<"sn">>}})).

auth_ldap_local_filter(_Config) ->
    ?cfgh([auth, ldap, local_filter], undefined, auth_ldap_raw(#{})), % default
    Filter = #{<<"operation">> => <<"equal">>,
               <<"attribute">> => <<"accountStatus">>,
               <<"values">> => [<<"enabled">>]},
    ?cfgh([auth, ldap, local_filter], {equal, {"accountStatus", ["enabled"]}},
          auth_ldap_raw(#{<<"local_filter">> => Filter})),
    [?errh(auth_ldap_raw(#{<<"local_filter">> => maps:remove(K, Filter)})) ||
        K <- maps:keys(Filter)],
    ?errh(auth_ldap_raw(#{<<"local_filter">> => Filter#{<<"operation">> := <<"lt">>}})),
    ?errh(auth_ldap_raw(#{<<"local_filter">> => Filter#{<<"attribute">> := <<>>}})),
    ?errh(auth_ldap_raw(#{<<"local_filter">> => Filter#{<<"values">> := []}})).

auth_ldap_deref(_Config) ->
    ?cfgh([auth, ldap, deref], never, auth_ldap_raw(#{})), % default
    ?cfgh([auth, ldap, deref], always, auth_ldap_raw(#{<<"deref">> => <<"always">>})),
    ?errh(auth_ldap_raw(#{<<"deref">> => <<"sometimes">>})).

auth_external(_Config) ->
    RequiredOpts = #{<<"program">> => <<"/usr/bin/auth">>},
    Config = #{program => "/usr/bin/auth",
               instances => 1}, % default
    ?cfgh([auth, external], Config,
          auth_raw(<<"external">>, RequiredOpts)),
    ?cfgh([auth, external, instances], 2,
          auth_raw(<<"external">>, RequiredOpts#{<<"instances">> => 2})),
    ?errh(auth_raw(<<"external">>, #{<<"program">> => <<>>})),
    ?errh(auth_raw(<<"external">>, #{<<"instances">> => 2})),
    ?errh(auth_raw(<<"external">>, RequiredOpts#{<<"instances">> => 0})).

auth_http_basic_auth(_Config) ->
    ?cfgh([auth, http, basic_auth], "admin:admin123",
          auth_raw(<<"http">>, #{<<"basic_auth">> => <<"admin:admin123">>})),
    ?errh(auth_raw(<<"http">>, #{<<"basic_auth">> => true})).

auth_jwt(_Config) ->
    Opts = #{<<"secret">> => #{<<"value">> => <<"secret123">>},
             <<"algorithm">> => <<"HS512">>,
             <<"username_key">> => <<"user">>}, % tested together as all options are required
    Config = #{algorithm => <<"HS512">>,
               secret => {value, "secret123"},
               username_key => user},
    ?cfgh([auth, jwt], Config,
          auth_raw(<<"jwt">>, Opts)),
    ?cfgh([auth, jwt, secret], {file, "/home/user/jwt_secret"},
          auth_raw(<<"jwt">>, Opts#{<<"secret">> := #{<<"file">> => <<"/home/user/jwt_secret">>}})),
    ?cfgh([auth, jwt, secret], {env, "SECRET"},
          auth_raw(<<"jwt">>, Opts#{<<"secret">> := #{<<"env">> => <<"SECRET">>}})),
    ?errh(auth_raw(<<"jwt">>, Opts#{<<"secret">> := #{<<"value">> => 123}})),
    ?errh(auth_raw(<<"jwt">>, Opts#{<<"secret">> := #{<<"file">> => <<>>}})),
    ?errh(auth_raw(<<"jwt">>, Opts#{<<"secret">> := #{<<"env">> => <<>>}})),
    ?errh(auth_raw(<<"jwt">>, Opts#{<<"secret">> := #{<<"file">> => <<"/jwt_secret">>,
                                                      <<"env">> => <<"SECRET">>}})),
    ?errh(auth_raw(<<"jwt">>, Opts#{<<"algorithm">> := <<"bruteforce">>})),
    ?errh(auth_raw(<<"jwt">>, Opts#{<<"username_key">> := <<>>})),
    [?errh(auth_raw(<<"jwt">>, maps:without([K], Opts))) || K <- maps:keys(Opts)].

auth_riak_bucket_type(_Config) ->
    ?cfgh([auth, riak, bucket_type], <<"users">>, auth_raw(<<"riak">>, #{})), % default
    ?cfgh([auth, riak, bucket_type], <<"buckethead">>,
          auth_raw(<<"riak">>, #{<<"bucket_type">> => <<"buckethead">>})),
    ?errh(auth_raw(<<"riak">>, #{<<"bucket_type">> => <<>>})).

auth_rdbms_users_number_estimate(_Config) ->
    ?cfgh([auth, rdbms, users_number_estimate], false, auth_raw(<<"rdbms">>, #{})), % default
    ?cfgh([auth, rdbms, users_number_estimate], true,
          auth_raw(<<"rdbms">>, #{<<"users_number_estimate">> => true})),
    ?errh(auth_raw(<<"rdbms">>, #{<<"users_number_estimate">> => 1200})).

auth_dummy(_Config) ->
    ?cfgh([auth, dummy], #{base_time => 50, variance => 450}, auth_raw(<<"dummy">>, #{})), % default
    ?cfgh([auth, dummy, base_time], 0, auth_raw(<<"dummy">>, #{<<"base_time">> => 0})),
    ?cfgh([auth, dummy, variance], 10, auth_raw(<<"dummy">>, #{<<"variance">> => 10})),
    ?errh(auth_raw(<<"dummy">>, #{<<"base_time">> => -5})),
    ?errh(auth_raw(<<"dummy">>, #{<<"variance">> => 0})).

%% tests: outgoing_pools

pool_type(_Config) ->
    ?cfg(pool_config(#{type => http}),
         pool_raw(<<"http">>, <<"default">>, #{})),
    ?err(pool_raw(<<"swimming_pool">>, <<"default">>, #{})).

pool_tag(_Config) ->
    ?cfg(pool_config(#{type => http, tag => my_pool}),
         pool_raw(<<"http">>, <<"my_pool">>, #{})),
    ?err(pool_raw(<<"http">>, 1000, #{})).

pool_scope(_Config) ->
    ?cfg(pool_config(#{type => http, scope => host}),
         pool_raw(<<"http">>, <<"default">>, #{<<"scope">> => <<"host">>})),
    ?cfg(pool_config(#{type => http, scope => <<"localhost">>}),
         pool_raw(<<"http">>, <<"default">>, #{<<"scope">> => <<"single_host">>,
                                               <<"host">> => <<"localhost">>})),
    ?err(pool_raw(<<"http">>, <<"default">>, #{<<"scope">> => <<"whatever">>})),
    ?err(pool_raw(<<"http">>, <<"default">>, #{<<"scope">> => <<"single_host">>})).

pool_workers(_Config) ->
    ?cfg(pool_config(#{type => http, opts => #{workers => 11}}),
         pool_raw(<<"http">>, <<"default">>, #{<<"workers">> => 11})),
    ?err(pool_raw(<<"http">>, <<"default">>, #{<<"workers">> => 0})).

pool_strategy(_Config) ->
    ?cfg(pool_config(#{type => http, opts => #{strategy => random_worker}}),
         pool_raw(<<"http">>, <<"default">>, #{<<"strategy">> => <<"random_worker">>})),
    ?err(pool_raw(<<"http">>, <<"default">>, #{<<"strategy">> => <<"worst_worker">>})).

pool_call_timeout(_Config) ->
    ?cfg(pool_config(#{type => http, opts => #{call_timeout => 999}}),
         pool_raw(<<"http">>, <<"default">>, #{<<"call_timeout">> => 999})),
    ?err(pool_raw(<<"http">>, <<"default">>, #{<<"call_timeout">> => 0})).

pool_rdbms_settings(_Config) ->
    ?cfg(pool_config(#{type => rdbms, conn_opts => #{server => "DSN=mydb"}}),
         pool_conn_raw(<<"rdbms">>, #{<<"driver">> => <<"odbc">>,
                                      <<"settings">> => <<"DSN=mydb">>})),
    ?err(pool_conn_raw(<<"rdbms">>, #{<<"driver">> => <<"mysql">>,
                                      <<"settings">> => <<"DSN=mydb">>})),
    ?err(pool_conn_raw(<<"rdbms">>, #{<<"driver">> => <<"odbc">>,
                                      <<"settings">> => true})),
    ?err(pool_conn_raw(<<"rdbms">>, #{<<"driver">> => <<"odbc">>})).

pool_rdbms_keepalive_interval(_Config) ->
    ?cfg(pool_config(#{type => rdbms, conn_opts => #{server => "DSN=mydb",
                                                     keepalive_interval => 1000}}),
         pool_conn_raw(<<"rdbms">>, #{<<"driver">> => <<"odbc">>,
                                      <<"settings">> => <<"DSN=mydb">>,
                                      <<"keepalive_interval">> => 1000})),
    ?err(pool_conn_raw(<<"rdbms">>, #{<<"driver">> => <<"odbc">>,
                                      <<"settings">> => <<"DSN=mydb">>,
                                      <<"keepalive_interval">> => false})).

pool_rdbms_server(_Config) ->
    ServerOpts = rdbms_opts(),
    ?cfg(pool_config(#{type => rdbms,
                       conn_opts => #{server => {pgsql, "localhost", "db", "dbuser", "secret"}}}),
         pool_conn_raw(<<"rdbms">>, ServerOpts)),
    ?err(pool_conn_raw(<<"rdbms">>, ServerOpts#{<<"driver">> := <<"odbc">>})),
    [?err(pool_conn_raw(<<"rdbms">>, maps:without([K], ServerOpts))) ||
        K <- maps:keys(ServerOpts)],
    [?err(pool_conn_raw(<<"rdbms">>, ServerOpts#{K := 123})) ||
        K <- maps:keys(ServerOpts)].

pool_rdbms_port(_Config) ->
    ServerOpts = rdbms_opts(),
    ?cfg(pool_config(#{type => rdbms,
                       conn_opts => #{server => {pgsql, "localhost", 1234, "db", "dbuser", "secret"}}}),
         pool_conn_raw(<<"rdbms">>, ServerOpts#{<<"port">> => 1234})),
    ?err(pool_conn_raw(<<"rdbms">>, ServerOpts#{<<"port">> => <<"airport">>})).

pool_rdbms_tls(_Config) ->
    ServerOpts = rdbms_opts(),
    ?cfg(pool_config(#{type => rdbms,
                       conn_opts => #{server => {pgsql, "localhost", "db", "dbuser", "secret",
                                                 [{ssl, required}]}}}),
         pool_conn_raw(<<"rdbms">>, ServerOpts#{<<"tls">> => #{<<"required">> => true}})),
    ?cfg(pool_config(#{type => rdbms,
                       conn_opts => #{server => {pgsql, "localhost", "db", "dbuser", "secret",
                                                 [{ssl, true}]}}}),
         pool_conn_raw(<<"rdbms">>, ServerOpts#{<<"tls">> => #{}})),
    ?cfg(pool_config(#{type => rdbms,
                       conn_opts => #{server => {mysql, "localhost", "db", "dbuser", "secret", []}}}),
         pool_conn_raw(<<"rdbms">>, ServerOpts#{<<"driver">> => <<"mysql">>,
                                                <<"tls">> => #{}})),
    ?cfg(pool_config(#{type => rdbms,
                       conn_opts => #{server => {pgsql, "localhost", 1234, "db", "dbuser", "secret",
                                                 [{ssl, true}]}}}),
         pool_conn_raw(<<"rdbms">>, ServerOpts#{<<"tls">> => #{},
                                                <<"port">> => 1234})),

    %% one option tested here as they are all checked by 'listen_tls_*' tests
    ?cfg(pool_config(#{type => rdbms,
                       conn_opts => #{server => {pgsql, "localhost", "db", "dbuser", "secret",
                                                 [{ssl, true}, {ssl_opts, [{certfile, "cert.pem"}]}]}}}),
         pool_conn_raw(<<"rdbms">>, ServerOpts#{<<"tls">> =>
                                                    #{<<"certfile">> => <<"cert.pem">>}})),
    ?err(pool_conn_raw(<<"rdbms">>, ServerOpts#{<<"tls">> =>
                                                    #{<<"certfile">> => true}})),
    ?err(pool_conn_raw(<<"rdbms">>, ServerOpts#{<<"tls">> => <<"secure">>})).

pool_http_host(_Config) ->
    ?cfg(pool_config(#{type => http, conn_opts => #{server => "https://localhost:8443"}}),
         pool_conn_raw(<<"http">>, #{<<"host">> => <<"https://localhost:8443">>})),
    ?err(pool_conn_raw(<<"http">>, #{<<"host">> => 8443})),
    ?err(pool_conn_raw(<<"http">>, #{<<"host">> => ""})).

pool_http_path_prefix(_Config) ->
    ?cfg(pool_config(#{type => http, conn_opts => #{path_prefix => "/my_path/"}}),
         pool_conn_raw(<<"http">>, #{<<"path_prefix">> => <<"/my_path/">>})),
    ?err(pool_conn_raw(<<"http">>, #{<<"path_prefix">> => 8443})),
    ?err(pool_conn_raw(<<"http">>, #{<<"path_prefix">> => ""})).

pool_http_request_timeout(_Config) ->
    ?cfg(pool_config(#{type => http, conn_opts => #{request_timeout => 999}}),
         pool_conn_raw(<<"http">>, #{<<"request_timeout">> => 999})),
    ?err(pool_conn_raw(<<"http">>, #{<<"request_timeout">> => -1000})),
    ?err(pool_conn_raw(<<"http">>, #{<<"request_timeout">> => <<"infinity">>})).

pool_http_tls(_Config) ->
    ?cfg(pool_config(#{type => http, conn_opts => #{http_opts => [{certfile, "cert.pem"}]}}),
         pool_conn_raw(<<"http">>, #{<<"tls">> => #{<<"certfile">> => <<"cert.pem">>}})),
    ?cfg(pool_config(#{type => http,
                       conn_opts => #{http_opts => [{certfile, "cert.pem"},
                                                    {verify, verify_peer},
                                                    {cacertfile, "priv/ca.pem"},
                                                    {server_name_indication, disable}]}}),
         pool_conn_raw(<<"http">>, #{<<"tls">> => #{<<"certfile">> => <<"cert.pem">>,
                                                    <<"verify_peer">> => true,
                                                    <<"cacertfile">> => <<"priv/ca.pem">>,
                                                    <<"server_name_indication">> => false}})),
    ?cfg(pool_config(#{type => http,
                       conn_opts => #{http_opts => [{certfile, "cert.pem"},
                                                    {verify, verify_peer},
                                                    {cacertfile, "priv/ca.pem"},
                                                    {server_name_indication, "domain.com"}]}}),
         pool_conn_raw(<<"http">>, #{<<"tls">> => #{<<"certfile">> => <<"cert.pem">>,
                                                    <<"verify_peer">> => true,
                                                    <<"cacertfile">> => <<"priv/ca.pem">>,
                                                    <<"server_name_indication">> => true,
                                                    <<"server_name_indication_host">> => <<"domain.com">>}})),
    ?cfg(pool_config(#{type => http,
                       conn_opts => #{http_opts => [{certfile, "cert.pem"},
                                                    {verify, verify_peer},
                                                    {cacertfile, "priv/ca.pem"},
                                                    {server_name_indication, "domain.com"},
                                                    {customize_hostname_check,
                                                     [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}]}}),
         pool_conn_raw(<<"http">>, #{<<"tls">> => #{<<"certfile">> => <<"cert.pem">>,
                                                    <<"verify_peer">> => true,
                                                    <<"cacertfile">> => <<"priv/ca.pem">>,
                                                    <<"server_name_indication">> => true,
                                                    <<"server_name_indication_host">> => <<"domain.com">>,
                                                    <<"server_name_indication_protocol">> => <<"https">>}})),
    ?cfg(pool_config(#{type => http,
                       conn_opts => #{http_opts => [{verify, verify_peer},
                                                    {cacertfile, "priv/ca.pem"}]}}),
         pool_conn_raw(<<"http">>, #{<<"tls">> => #{<<"verify_peer">> => true,
                                                    <<"cacertfile">> => <<"priv/ca.pem">>}})),
    ?err(pool_conn_raw(<<"http">>, #{<<"tls">> => #{<<"verify_peer">> => true,
                                                    <<"cacertfile">> => <<"priv/ca.pem">>,
                                                    <<"server_name_indication">> => <<"domain.com">>,
                                                    <<"server_name_indication_host">> => <<"domain.com">>}})),
    ?err(pool_conn_raw(<<"http">>, #{<<"tls">> => #{<<"verify_peer">> => true,
                                                    <<"cacertfile">> => <<"priv/ca.pem">>,
                                                    <<"server_name_indication">> => <<"true">>,
                                                    <<"server_name_indication_host">> => <<"domain.com">>,
                                                    <<"server_name_indication_protocol">> => <<"non_value">>}})),
    ?err(pool_conn_raw(<<"http">>, #{<<"tls">> => #{<<"certfile">> => true}})),
    ?err(pool_conn_raw(<<"http">>, #{<<"tls">> => <<"secure">>})).

pool_redis_host(_Config) ->
    ?cfg(pool_config(#{type => redis, conn_opts => #{host => "my_host"}}),
         pool_conn_raw(<<"redis">>, #{<<"host">> => <<"my_host">>})),
    ?err(pool_conn_raw(<<"redis">>, #{<<"host">> => 8443})),
    ?err(pool_conn_raw(<<"redis">>, #{<<"host">> => ""})).

pool_redis_port(_Config) ->
    ?cfg(pool_config(#{type => redis, conn_opts => #{port => 9999}}),
         pool_conn_raw(<<"redis">>, #{<<"port">> => 9999})),
    ?err(pool_conn_raw(<<"redis">>, #{<<"port">> => 666666})),
    ?err(pool_conn_raw(<<"redis">>, #{<<"port">> => <<"airport">>})).

pool_redis_database(_Config) ->
    ?cfg(pool_config(#{type => redis, conn_opts => #{database => 1}}),
         pool_conn_raw(<<"redis">>, #{<<"database">> => 1})),
    ?err(pool_conn_raw(<<"redis">>, #{<<"database">> => -1})),
    ?err(pool_conn_raw(<<"redis">>, #{<<"database">> => <<"my_database">>})).

pool_redis_password(_Config) ->
    ?cfg(pool_config(#{type => redis, conn_opts => #{password => "password1"}}),
         pool_conn_raw(<<"redis">>, #{<<"password">> => <<"password1">>})),
    ?err(pool_conn_raw(<<"redis">>, #{<<"password">> => 0})).

pool_riak_address(_Config) ->
    ?cfg(pool_config(#{type => riak, conn_opts => #{address => "127.0.0.1"}}),
         pool_conn_raw(<<"riak">>, #{<<"address">> => <<"127.0.0.1">>})),
    ?err(pool_conn_raw(<<"riak">>, #{<<"address">> => 66})),
    ?err(pool_conn_raw(<<"riak">>, #{<<"address">> => <<"">>})).

pool_riak_port(_Config) ->
    ?cfg(pool_config(#{type => riak, conn_opts => #{port => 8087}}),
         pool_conn_raw(<<"riak">>, #{<<"port">> => 8087})),
    ?err(pool_conn_raw(<<"riak">>, #{<<"port">> => 666666})),
    ?err(pool_conn_raw(<<"riak">>, #{<<"port">> => <<"airport">>})).

pool_riak_credentials(_Config) ->
    ?cfg(pool_config(#{type => riak, conn_opts => #{credentials => {"user", "pass"}}}),
         pool_conn_raw(<<"riak">>, #{<<"credentials">> =>
                                         #{<<"user">> => <<"user">>, <<"password">> => <<"pass">>}})),
    ?err(pool_conn_raw(<<"riak">>, #{<<"credentials">> => #{<<"user">> => <<"user">>}})),
    ?err(pool_conn_raw(<<"riak">>, #{<<"credentials">> =>
                                         #{<<"user">> => <<"">>, <<"password">> => 011001}})).

pool_riak_cacertfile(_Config) ->
    ?cfg(pool_config(#{type => riak, conn_opts => #{cacertfile => "cacert.pem"}}),
         pool_conn_raw(<<"riak">>, #{<<"tls">> => #{<<"cacertfile">> => <<"cacert.pem">>}})),
    ?err(pool_conn_raw(<<"riak">>, #{<<"cacertfile">> => <<"">>})).

pool_riak_tls(_Config) ->
    %% make sure these options are not extracted out of 'ssl_opts'
    %% all the TLS options are checked by 'listen_tls_*' tests
    ?cfg(pool_config(#{type => riak,
                       conn_opts => #{ssl_opts => [{certfile, "path/to/cert.pem"},
                                                   {dhfile, "cert.pem"},
                                                   {keyfile, "path/to/key.pem"}]}}),
         pool_conn_raw(<<"riak">>, #{<<"tls">> => #{<<"certfile">> => <<"path/to/cert.pem">>,
                                                    <<"dhfile">> => <<"cert.pem">>,
                                                    <<"keyfile">> => <<"path/to/key.pem">>}})),
    ?err(pool_conn_raw(<<"riak">>, #{<<"tls">> => #{<<"dhfile">> => true}})),
    ?err(pool_conn_raw(<<"riak">>, #{<<"tls">> => <<"secure">>})).

pool_cassandra_servers(_Config) ->
    ?cfg(pool_config(#{type => cassandra,
                       conn_opts => #{servers => [{"cassandra_server1.example.com", 9042},
                                                  {"cassandra_server2.example.com", 9042}]}}),
         pool_conn_raw(<<"cassandra">>,
                       #{<<"servers">> => [#{<<"ip_address">> => <<"cassandra_server1.example.com">>,
                                             <<"port">> => 9042},
                                           #{<<"ip_address">> => <<"cassandra_server2.example.com">>,
                                             <<"port">> => 9042}]})),
    ?err(pool_conn_raw(<<"cassandra">>,
                       #{<<"servers">> => #{<<"ip_address">> => <<"cassandra_server1.example.com">>,
                                            <<"port">> => 9042}})).

pool_cassandra_keyspace(_Config) ->
    ?cfg(pool_config(#{type => cassandra, conn_opts => #{keyspace => big_mongooseim}}),
         pool_conn_raw(<<"cassandra">>, #{<<"keyspace">> => <<"big_mongooseim">>})),
    ?err(pool_conn_raw(<<"cassandra">>, #{<<"keyspace">> => <<"">>})).

pool_cassandra_auth(_Config) ->
    ?cfg(pool_config(#{type => cassandra,
                       conn_opts => #{auth => {cqerl_auth_plain_handler,
                                               [{<<"auser">>, <<"secretpass">>}]}}}),
         pool_conn_raw(<<"cassandra">>,
                       #{<<"auth">> => #{<<"plain">> => #{<<"username">> => <<"auser">>,
                                                          <<"password">> => <<"secretpass">>}}})),
    ?err(pool_conn_raw(<<"cassandra">>, #{<<"tls">> => #{<<"verify">> => <<"verify_none">>}})).

pool_cassandra_tls(_Config) ->
    %% one option tested here as they are all checked by 'listen_tls_*' tests
    ?cfg(pool_config(#{type => cassandra, conn_opts => #{ssl => [{verify, verify_none}]}}),
         pool_conn_raw(<<"cassandra">>, #{<<"tls">> => #{<<"verify_peer">> => false}})),
    ?err(pool_conn_raw(<<"cassandra">>, #{<<"tls">> => #{<<"verify">> => <<"verify_none">>}})).

pool_elastic_host(_Config) ->
    ?cfg(pool_config(#{type => elastic, conn_opts => #{host => "my_host"}}),
         pool_conn_raw(<<"elastic">>, #{<<"host">> => <<"my_host">>})),
    ?err(pool_conn_raw(<<"elastic">>, #{<<"host">> => <<"">>})).

pool_elastic_port(_Config) ->
    ?cfg(pool_config(#{type => elastic, conn_opts => #{port => 9999}}),
         pool_conn_raw(<<"elastic">>, #{<<"port">> => 9999})),
    ?err(pool_conn_raw(<<"elastic">>, #{<<"port">> => 122333})),
    ?err(pool_conn_raw(<<"elastic">>, #{<<"port">> => <<"airport">>})).

pool_rabbit_amqp_host(_Config) ->
    ?cfg(pool_config(#{type => rabbit, conn_opts => #{amqp_host => "localhost"}}),
         pool_conn_raw(<<"rabbit">>, #{<<"amqp_host">> => <<"localhost">>})),
    ?err(pool_conn_raw(<<"rabbit">>, #{<<"amqp_host">> => <<"">>})).

pool_rabbit_amqp_port(_Config) ->
    ?cfg(pool_config(#{type => rabbit, conn_opts => #{amqp_port => 5672}}),
         pool_conn_raw(<<"rabbit">>, #{<<"amqp_port">> => 5672})),
    ?err(pool_conn_raw(<<"rabbit">>, #{<<"amqp_port">> => <<"airport">>})).

pool_rabbit_amqp_username(_Config) ->
    ?cfg(pool_config(#{type => rabbit, conn_opts => #{amqp_username => "guest"}}),
         pool_conn_raw(<<"rabbit">>, #{<<"amqp_username">> => <<"guest">>})),
    ?err(pool_conn_raw(<<"rabbit">>, #{<<"amqp_username">> => <<"">>})).

pool_rabbit_amqp_password(_Config) ->
    ?cfg(pool_config(#{type => rabbit, conn_opts => #{amqp_password => "guest"}}),
         pool_conn_raw(<<"rabbit">>, #{<<"amqp_password">> => <<"guest">>})),
    ?err(pool_conn_raw(<<"rabbit">>, #{<<"amqp_password">> => <<"">>})).

pool_rabbit_amqp_confirms_enabled(_Config) ->
    ?cfg(pool_config(#{type => rabbit, conn_opts => #{confirms_enabled => true}}),
         pool_conn_raw(<<"rabbit">>, #{<<"confirms_enabled">> => true})),
    ?err(pool_conn_raw(<<"rabbit">>, #{<<"confirms_enabled">> => <<"yes">>})).

pool_rabbit_amqp_max_worker_queue_len(_Config) ->
    ?cfg(pool_config(#{type => rabbit, conn_opts => #{max_worker_queue_len => 100}}),
         pool_conn_raw(<<"rabbit">>, #{<<"max_worker_queue_len">> => 100})),
    ?err(pool_conn_raw(<<"rabbit">>, #{<<"max_worker_queue_len">> => 0})).

pool_ldap_port(_Config) ->
    ?cfg(pool_config(#{type => ldap, conn_opts => #{port => 389}}),
         pool_conn_raw(<<"ldap">>, #{<<"port">> => 389})),
    ?err(pool_conn_raw(<<"ldap">>, #{<<"port">> => <<"airport">>})).

pool_ldap_servers(_Config) ->
    ?cfg(pool_config(#{type => ldap,
                       conn_opts => #{servers => ["primary-ldap-server.example.com",
                                                  "secondary-ldap-server.example.com"]}}),
         pool_conn_raw(<<"ldap">>, #{<<"servers">> => [<<"primary-ldap-server.example.com">>,
                                                       <<"secondary-ldap-server.example.com">>]})),
    ?err(pool_conn_raw(<<"ldap">>, #{<<"servers">> => #{<<"server">> => <<"example.com">>}})).

pool_ldap_encrypt(_Config) ->
    ?cfg(pool_config(#{type => ldap, conn_opts => #{encrypt => tls}}),
         pool_conn_raw(<<"ldap">>, #{<<"encrypt">> => <<"tls">>})),
    ?err(pool_conn_raw(<<"ldap">>, #{<<"encrypt">> => true})).

pool_ldap_rootdn(_Config) ->
    ?cfg(pool_config(#{type => ldap, conn_opts => #{rootdn => <<"my_rootdn">>}}),
         pool_conn_raw(<<"ldap">>, #{<<"rootdn">> => <<"my_rootdn">>})),
    ?err(pool_conn_raw(<<"ldap">>, #{<<"rootdn">> => false})).

pool_ldap_password(_Config) ->
    ?cfg(pool_config(#{type => ldap, conn_opts => #{password => <<"pass">>}}),
         pool_conn_raw(<<"ldap">>, #{<<"password">> => <<"pass">>})),
    ?err(pool_conn_raw(<<"ldap">>, #{<<"password">> => true})).

pool_ldap_connect_interval(_Config) ->
    ?cfg(pool_config(#{type => ldap, conn_opts => #{connect_interval => 9999}}),
         pool_conn_raw(<<"ldap">>, #{<<"connect_interval">> => 9999})),
    ?err(pool_conn_raw(<<"ldap">>, #{<<"connect_interval">> => <<"infinity">>})).

pool_ldap_tls(_Config) ->
    %% one option tested here as they are all checked by 'listen_tls_*' tests
    ?cfg(pool_config(#{type => ldap, conn_opts => #{tls_options => [{verify, verify_peer}]}}),
         pool_conn_raw(<<"ldap">>, #{<<"tls">> => #{<<"verify_peer">> => true}})),
    ?err(pool_conn_raw(<<"ldap">>, #{<<"tls">> => #{<<"verify">> => <<"verify_none">>}})).

%% tests: shaper, acl, access
shaper(_Config) ->
    ?cfg([shaper, normal], #{max_rate => 1000},
         #{<<"shaper">> => #{<<"normal">> => #{<<"max_rate">> => 1000}}}),
    ?err(#{<<"shaper">> => #{<<"unlimited">> => #{<<"max_rate">> => <<"infinity">>}}}),
    ?err(#{<<"shaper">> => #{<<"fast">> => #{}}}).

acl(_Config) ->
    ?cfgh([acl, local], [#{match => all}],
          #{<<"acl">> => #{<<"local">> => [#{<<"match">> => <<"all">>}]}}),
    ?cfgh([acl, local], [#{match => any_hosted_domain}],
          #{<<"acl">> => #{<<"local">> => [#{<<"match">> => <<"any_hosted_domain">>}]}}),
    ?cfgh([acl, local], [#{match => current_domain,
                           user_regexp => <<>>}],
          #{<<"acl">> => #{<<"local">> => [#{<<"user_regexp">> => <<>>}]}}),
    ?cfgh([acl, alice], [#{match => current_domain,
                           user_regexp => <<"ali.*">>,
                           server_regexp => <<".*host">>}],
          #{<<"acl">> => #{<<"alice">> => [#{<<"user_regexp">> => <<"aLi.*">>,
                                             <<"server_regexp">> => <<".*HosT">>}]}}),
    ?cfgh([acl, alice], [#{match => current_domain,
                           user => <<"alice">>,
                           server => <<"localhost">>}],
          #{<<"acl">> => #{<<"alice">> => [#{<<"user">> => <<"alice">>,
                                             <<"server">> => <<"localhost">>}]}}),
    ?errh(#{<<"acl">> => #{<<"local">> => <<"everybody">>}}),
    ?errh(#{<<"acl">> => #{<<"local">> => [#{<<"match">> => <<"lit">>}]}}),
    ?errh(#{<<"acl">> => #{<<"alice">> => [#{<<"user_glob">> => <<"a*">>,
                                             <<"server_blog">> => <<"bloghost">>}]}}),
    ?errh([#{reason := incorrect_acl_condition_value}],
          #{<<"acl">> => #{<<"local">> => [#{<<"user">> => <<"@@@">>}]}}).

acl_merge_host_and_global(_Config) ->
    G = #{<<"acl">> => #{<<"admin">> => [#{<<"user">> => <<"george">>}]}},
    H1 = #{<<"acl">> => #{<<"admin">> => [#{<<"user">> => <<"henry">>}]}},
    H2 = #{<<"acl">> => #{<<"hostile">> => [#{<<"user">> => <<"hacker">>}]}},
    ?cfg([{{acl, global}, #{admin => [#{user => <<"george">>, match => current_domain}]}},
          {{acl, ?HOST}, #{admin => [#{user => <<"george">>, match => current_domain}]}}],
         maps:merge(G, host_config(G))),
    ?cfg([{{acl, global}, #{admin => [#{user => <<"george">>, match => current_domain}]}},
          {{acl, ?HOST}, #{admin => [#{user => <<"george">>, match => current_domain},
                                     #{user => <<"henry">>, match => current_domain}]}}],
         maps:merge(G, host_config(H1))),
    ?cfg([{{acl, global}, #{admin => [#{user => <<"george">>, match => current_domain}]}},
          {{acl, ?HOST}, #{admin => [#{user => <<"george">>, match => current_domain}],
                           hostile => [#{user => <<"hacker">>, match => current_domain}]}}],
         maps:merge(G, host_config(H2))).

access(_Config) ->
    ?cfgh([access, c2s], [#{acl => blocked, value => deny},
                          #{acl => all, value => allow}],
          access_raw(<<"c2s">>, [#{<<"acl">> => <<"blocked">>, <<"value">> => <<"deny">>},
                                 #{<<"acl">> => <<"all">>, <<"value">> => <<"allow">>}])),
    ?cfgh([access, max_user_sessions], [#{acl => all, value => 10}],
          access_raw(<<"max_user_sessions">>, [#{<<"acl">> => <<"all">>, <<"value">> => 10}])),
    ?errh(access_raw(<<"max_user_sessions">>, [#{<<"acl">> => <<"all">>}])),
    ?errh(access_raw(<<"max_user_sessions">>, [#{<<"value">> => 10}])),
    ?errh(access_raw(<<"max_user_sessions">>, [#{<<"acl">> => 10, <<"value">> => 10}])).

access_merge_host_and_global(_Config) ->
    G1 = access_raw(<<"c2s">>, [#{<<"acl">> => <<"good">>, <<"value">> => <<"allow">>}]),
    G2 = access_raw(<<"c2s">>, [#{<<"acl">> => <<"gangsters">>, <<"value">> => <<"deny">>},
                                #{<<"acl">> => <<"all">>, <<"value">> => <<"allow">>}]),
    H1 = access_raw(<<"c2s">>, [#{<<"acl">> => <<"harmless">>, <<"value">> => <<"allow">>}]),
    H2 = access_raw(<<"s2s">>, [#{<<"acl">> => <<"harmless">>, <<"value">> => <<"allow">>}]),
    H3 = access_raw(<<"c2s">>, [#{<<"acl">> => <<"hackers">>, <<"value">> => <<"deny">>}]),
    ?cfg([{{access, global}, #{c2s => [#{acl => good, value => allow}]}},
          {{access, ?HOST}, #{c2s => [#{acl => good, value => allow}]}}],
         maps:merge(G1, host_config(G1))),
    ?cfg([{{access, global}, #{c2s => [#{acl => good, value => allow}]}},
          {{access, ?HOST}, #{c2s => [#{acl => good, value => allow},
                                      #{acl => harmless, value => allow}]}}],
         maps:merge(G1, host_config(H1))),
    ?cfg([{{access, global}, #{c2s => [#{acl => good, value => allow}]}},
          {{access, ?HOST}, #{c2s => [#{acl => good, value => allow}],
                              s2s => [#{acl => harmless, value => allow}]}}],
         maps:merge(G1, host_config(H2))),
    ?cfg([{{access, global}, #{c2s => [#{acl => gangsters, value => deny},
                                       #{acl => all, value => allow}]}},
          {{access, ?HOST}, #{c2s => [#{acl => gangsters, value => deny},
                                      #{acl => hackers, value => deny},
                                      #{acl => all, value => allow}]}}],
         maps:merge(G2, host_config(H3))).

%% tests: s2s

s2s_host_config(_Config) ->
    DefaultS2S = default_s2s(),
    EmptyHostConfig = host_config(#{<<"s2s">> => #{}}),
    ?cfg(host_key(s2s), DefaultS2S,
         EmptyHostConfig#{<<"s2s">> => #{<<"dns">> => #{<<"timeout">> => 5}}}),
    StartTLSHostConfig = host_config(#{<<"s2s">> => #{<<"use_starttls">> => <<"required">>}}),
    ?cfg(host_key(s2s), DefaultS2S#{use_starttls => required},
         StartTLSHostConfig#{<<"s2s">> => #{<<"dns">> => #{<<"timeout">> => 5}}}).

s2s_dns_timeout(_Config) ->
    ?cfgh([s2s, dns, timeout], 10, #{}), % default
    ?cfgh([s2s, dns, timeout], 5, #{<<"s2s">> => #{<<"dns">> => #{<<"timeout">> => 5}}}),
    ?errh(#{<<"s2s">> => #{<<"dns">> => #{<<"timeout">> => 0}}}).

s2s_dns_retries(_Config) ->
    ?cfgh([s2s, dns, retries], 2, #{}), % default
    ?cfgh([s2s, dns, retries], 1, #{<<"s2s">> => #{<<"dns">> => #{<<"retries">> => 1}}}),
    ?errh(#{<<"s2s">> => #{<<"dns">> => #{<<"retries">> => 0}}}).

s2s_outgoing_port(_Config) ->
    ?cfgh([s2s, outgoing, port], 5269, #{}), % default
    ?cfgh([s2s, outgoing, port], 5270, #{<<"s2s">> => #{<<"outgoing">> => #{<<"port">> => 5270}}}),
    ?errh(#{<<"s2s">> => #{<<"outgoing">> => #{<<"port">> => <<"http">>}}}).

s2s_outgoing_ip_versions(_Config) ->
    ?cfgh([s2s, outgoing, ip_versions], [4, 6], #{}), % default
    ?cfgh([s2s, outgoing, ip_versions], [6, 4],
         #{<<"s2s">> => #{<<"outgoing">> => #{<<"ip_versions">> => [6, 4]}}}),
    ?errh(#{<<"s2s">> => #{<<"outgoing">> => #{<<"ip_versions">> => []}}}),
    ?errh(#{<<"s2s">> => #{<<"outgoing">> => #{<<"ip_versions">> => [<<"http">>]}}}).

s2s_outgoing_timeout(_Config) ->
    ?cfgh([s2s, outgoing, connection_timeout], 10000, #{}), % default
    ?cfgh([s2s, outgoing, connection_timeout], 5000,
          #{<<"s2s">> => #{<<"outgoing">> => #{<<"connection_timeout">> => 5000}}}),
    ?cfgh([s2s, outgoing, connection_timeout], infinity,
          #{<<"s2s">> => #{<<"outgoing">> => #{<<"connection_timeout">> => <<"infinity">>}}}),
    ?errh(#{<<"s2s">> => #{<<"outgoing">> => #{<<"connection_timeout">> => 0}}}).

s2s_use_starttls(_Config) ->
    ?cfgh([s2s, use_starttls], false, #{}), % default
    ?cfgh([s2s, use_starttls], required, #{<<"s2s">> => #{<<"use_starttls">> => <<"required">>}}),
    ?errh(#{<<"s2s">> => #{<<"use_starttls">> => <<"unnecessary">>}}).

s2s_certfile(_Config) ->
    ?cfgh([s2s, certfile], "priv/server.pem",  #{<<"s2s">> => #{<<"certfile">> => <<"priv/server.pem">>}}),
    ?errh([#{reason := invalid_filename}], #{<<"s2s">> => #{<<"certfile">> => <<"nofile.pem">>}}),
    ?errh(#{<<"s2s">> => #{<<"certfile">> => []}}).

s2s_default_policy(_Config) ->
    ?cfgh([s2s, default_policy], allow, #{}), % default
    ?cfgh([s2s, default_policy], deny, #{<<"s2s">> => #{<<"default_policy">> => <<"deny">>}}),
    ?errh(#{<<"s2s">> => #{<<"default_policy">> => <<"ask">>}}).

s2s_host_policy(_Config) ->
    Policy = #{<<"host">> => <<"host1">>,
               <<"policy">> => <<"allow">>},
    ?cfgh([s2s, host_policy], #{<<"host1">> => allow},
          #{<<"s2s">> => #{<<"host_policy">> => [Policy]}}),
    ?cfgh([s2s, host_policy], #{<<"host1">> => allow,
                                <<"host2">> => deny},
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
    ?cfgh([s2s, address], #{<<"host1">> => #{ip_address => "192.168.1.2", port => 5321}},
          #{<<"s2s">> => #{<<"address">> => [Addr]}}),
    ?cfgh([s2s, address], #{<<"host1">> => #{ip_address => "192.168.1.2"}},
          #{<<"s2s">> => #{<<"address">> => [maps:without([<<"port">>], Addr)]}}),
    ?errh(#{<<"s2s">> => #{<<"address">> => [maps:without([<<"host">>], Addr)]}}),
    ?errh(#{<<"s2s">> => #{<<"address">> => [maps:without([<<"ip_address">>], Addr)]}}),
    ?errh(#{<<"s2s">> => #{<<"address">> => [Addr#{<<"host">> => <<>>}]}}),
    ?errh(#{<<"s2s">> => #{<<"address">> => [Addr#{<<"ip_address">> => <<"host2">>}]}}),
    ?errh(#{<<"s2s">> => #{<<"address">> => [Addr#{<<"port">> => <<"seaport">>}]}}),
    ?errh(#{<<"s2s">> => #{<<"address">> => [Addr, maps:remove(<<"port">>, Addr)]}}).

s2s_ciphers(_Config) ->
    ?cfgh([s2s, ciphers], ejabberd_tls:default_ciphers(), #{}), % default
    ?cfgh([s2s, ciphers], "TLSv1.2",
          #{<<"s2s">> => #{<<"ciphers">> => <<"TLSv1.2">>}}),
    ?errh(#{<<"s2s">> => #{<<"ciphers">> => [<<"cipher1">>, <<"cipher2">>]}}).

s2s_shared(_Config) ->
    ?cfgh([s2s, shared], <<"secret">>, #{<<"s2s">> => #{<<"shared">> => <<"secret">>}}),
    ?errh(#{<<"s2s">> => #{<<"shared">> => 536837}}).

s2s_max_retry_delay(_Config) ->
    ?cfgh([s2s, max_retry_delay], 120, #{<<"s2s">> => #{<<"max_retry_delay">> => 120}}),
    ?errh(#{<<"s2s">> => #{<<"max_retry_delay">> => 0}}).

%% modules

mod_adhoc(_Config) ->
    check_module_defaults(mod_adhoc),
    check_iqdisc(mod_adhoc),
    P = [modules, mod_adhoc],
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_adhoc">> => #{K => V}}} end,
    %% report_commands_node is boolean
    ?cfgh(P ++ [report_commands_node], true, T(<<"report_commands_node">>, true)),
    ?cfgh(P ++ [report_commands_node], false, T(<<"report_commands_node">>, false)),
    %% not boolean
    ?errh(T(<<"report_commands_node">>, <<"hello">>)).

mod_auth_token(_Config) ->
    check_module_defaults(mod_auth_token),
    check_iqdisc(mod_auth_token),
    P = [modules, mod_auth_token],
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_auth_token">> => #{K => V}}} end,
    ?cfgh(P ++ [backend], rdbms, T(<<"backend">>, <<"rdbms">>)),
    ?cfgh(P ++ [validity_period, access], #{unit => minutes, value => 13},
          T(<<"validity_period">>,
            #{<<"access">> => #{<<"value">> => 13, <<"unit">> => <<"minutes">>}})),
    ?cfgh(P ++ [validity_period, refresh], #{unit => days, value => 31},
          T(<<"validity_period">>,
            #{<<"refresh">> => #{<<"value">> => 31, <<"unit">> => <<"days">>}})),
    ?errh(T(<<"backend">>, <<"nosql">>)),
    ?errh(T(<<"validity_period">>,
            #{<<"access">> => #{<<"value">> => -1, <<"unit">> => <<"minutes">>}})),
    ?errh(T(<<"validity_period">>,
            #{<<"access">> => #{<<"value">> => 10, <<"unit">> => <<"centuries">>}})),
    ?errh(T(<<"validity_period">>, #{<<"access">> => #{<<"value">> => 10}})),
    ?errh(T(<<"validity_period">>, #{<<"access">> => #{<<"unit">> => <<"days">>}})).

mod_blocking(_Config) ->
    test_privacy_opts(mod_blocking).

mod_bosh(_Config) ->
    check_module_defaults(mod_bosh),
    P = [modules, mod_bosh],
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_bosh">> => #{K => V}}} end,
    ?cfgh(P ++ [backend], mnesia, T(<<"backend">>, <<"mnesia">>)),
    ?cfgh(P ++ [inactivity], 10, T(<<"inactivity">>, 10)),
    ?cfgh(P ++ [inactivity], infinity, T(<<"inactivity">>, <<"infinity">>)),
    ?cfgh(P ++ [max_wait], 10, T(<<"max_wait">>, 10)),
    ?cfgh(P ++ [max_wait], infinity, T(<<"max_wait">>, <<"infinity">>)),
    ?cfgh(P ++ [server_acks], true, T(<<"server_acks">>, true)),
    ?cfgh(P ++ [server_acks], false, T(<<"server_acks">>, false)),
    ?cfgh(P ++ [max_pause], 10, T(<<"max_pause">>, 10)),
    ?errh(T(<<"backend">>, <<"nodejs">>)),
    ?errh(T(<<"inactivity">>, 0)),
    ?errh(T(<<"inactivity">>, <<"10">>)),
    ?errh(T(<<"inactivity">>, <<"inactivity">>)),
    ?errh(T(<<"max_wait">>, <<"10">>)),
    ?errh(T(<<"max_wait">>, 0)),
    ?errh(T(<<"server_acks">>, -1)),
    ?errh(T(<<"maxpause">>, 0)).

mod_caps(_Config) ->
    check_module_defaults(mod_caps),
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_caps">> => #{K => V}}} end,
    P = [modules, mod_caps],
    ?cfgh(P ++ [cache_size], 10, T(<<"cache_size">>, 10)),
    ?cfgh(P ++ [cache_life_time], 10, T(<<"cache_life_time">>, 10)),
    ?errh(T(<<"cache_size">>, 0)),
    ?errh(T(<<"cache_size">>, <<"infinity">>)),
    ?errh(T(<<"cache_life_time">>, 0)),
    ?errh(T(<<"cache_life_time">>, <<"infinity">>)).

mod_cache_users(_Config) ->
    check_module_defaults(mod_cache_users),
    P = [modules, mod_cache_users],
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_cache_users">> => #{K => V}}} end,
    ?cfgh(P ++ [time_to_live], 8600, T(<<"time_to_live">>, 8600)),
    ?cfgh(P ++ [time_to_live], infinity, T(<<"time_to_live">>, <<"infinity">>)),
    ?cfgh(P ++ [number_of_segments], 10, T(<<"number_of_segments">>, 10)),
    ?cfgh(P ++ [strategy], fifo, T(<<"strategy">>, <<"fifo">>)),
    ?errh(T(<<"time_to_live">>, 0)),
    ?errh(T(<<"strategy">>, <<"lifo">>)),
    ?errh(T(<<"number_of_segments">>, 0)),
    ?errh(T(<<"number_of_segments">>, <<"infinity">>)).

mod_carboncopy(_Config) ->
    check_iqdisc(mod_carboncopy).

mod_csi(_Config) ->
    check_module_defaults(mod_csi),
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_csi">> => #{K => V}}} end,
    P = [modules, mod_csi],
    ?cfgh(P ++ [buffer_max], 10, T(<<"buffer_max">>, 10)),
    ?cfgh(P ++ [buffer_max], infinity, T(<<"buffer_max">>, <<"infinity">>)),
    ?errh(T(<<"buffer_max">>, -1)).

mod_disco(_Config) ->
    check_module_defaults(mod_disco),
    check_iqdisc(mod_disco),
    P = [modules, mod_disco],
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_disco">> => #{K => V}}} end,
    ?cfgh(P ++ [users_can_see_hidden_services], true,
          T(<<"users_can_see_hidden_services">>, true)),
    ?cfgh(P ++ [users_can_see_hidden_services], false,
          T(<<"users_can_see_hidden_services">>, false)),
    %% extra_domains are binaries
    ?cfgh(P ++ [extra_domains], [<<"localhost">>, <<"erlang-solutions.com">>],
          T(<<"extra_domains">>, [<<"localhost">>, <<"erlang-solutions.com">>])),
    ?cfgh(P ++ [extra_domains], [],
          T(<<"extra_domains">>, [])),
    Info = #{<<"name">> => <<"abuse-address">>,
             <<"urls">> => [<<"admin@example.com">>]},
    SpiritUrls = [<<"spirit1@localhost">>, <<"spirit2@localhost">>],
    ?cfgh(P ++ [server_info],
          [#{name => <<"abuse-address">>, urls => [<<"admin@example.com">>]},
           #{name => <<"friendly-spirits">>, urls => SpiritUrls, modules => [mod_muc, mod_disco]}],
          T(<<"server_info">>, [Info, #{<<"name">> => <<"friendly-spirits">>,
                                        <<"urls">> => SpiritUrls,
                                        <<"modules">> => [<<"mod_muc">>, <<"mod_disco">>]}])),
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
    check_module_defaults(mod_extdisco),
    check_iqdisc(mod_extdisco),
    P = [modules, mod_extdisco, service],
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_extdisco">> =>
                                 #{<<"service">> => [Opts]}}}
        end,
    RequiredOpts = #{<<"type">> => <<"stun">>, <<"host">> => <<"stun1">>},
    Service = #{type => stun, host => <<"stun1">>},
    ?cfgh(P, [Service], T(RequiredOpts)),
    ?cfgh(P, [Service#{port => 3478}], T(RequiredOpts#{<<"port">> => 3478})),
    ?cfgh(P, [Service#{transport => <<"udp">>}], T(RequiredOpts#{<<"transport">> => <<"udp">>})),
    ?cfgh(P, [Service#{username => <<"user">>}], T(RequiredOpts#{<<"username">> => <<"user">>})),
    ?cfgh(P, [Service#{password => <<"pass">>}], T(RequiredOpts#{<<"password">> => <<"pass">>})),
    [?errh(T(maps:remove(Key, RequiredOpts))) || Key <- maps:keys(RequiredOpts)],
    [?errh(T(RequiredOpts#{Key => 1})) || Key <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"type">> => <<>>})),
    ?errh(T(RequiredOpts#{<<"host">> => <<>>})),
    ?errh(T(RequiredOpts#{<<"port">> => -1})),
    ?errh(T(RequiredOpts#{<<"transport">> => <<>>})),
    ?errh(T(RequiredOpts#{<<"username">> => <<>>})),
    ?errh(T(RequiredOpts#{<<"password">> => <<>>})).

mod_inbox(_Config) ->
    check_module_defaults(mod_inbox),
    check_iqdisc(mod_inbox),
    P = [modules, mod_inbox],
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_inbox">> => Opts}} end,
    ChatMarkers = [<<"displayed">>, <<"received">>, <<"acknowledged">>],
    ?cfgh(P ++ [backend], rdbms, T(#{<<"backend">> => <<"rdbms">>})),
    ?cfgh(P ++ [async_writer], #{pool_size => 8}, T(#{<<"async_writer">> => #{<<"pool_size">> => 8}})),
    ?cfgh(P ++ [reset_markers], ChatMarkers, T(#{<<"reset_markers">> => ChatMarkers})),
    ?cfgh(P ++ [groupchat], [muc, muclight], T(#{<<"groupchat">> => [<<"muc">>, <<"muclight">>]})),
    ?cfgh(P ++ [boxes],
          [<<"inbox">>, <<"archive">>, <<"bin">>, <<"favourites">>, <<"spam">>],
          T(#{<<"boxes">> => [<<"favourites">>, <<"spam">>]})),
    ?cfgh(P ++ [bin_ttl], 30, T(#{<<"bin_ttl">> => 30})),
    ?cfgh(P ++ [bin_clean_after], 43200000, T(#{<<"bin_clean_after">> => 12})),
    ?cfgh(P ++ [aff_changes], true, T(#{<<"aff_changes">> => true})),
    ?cfgh(P ++ [remove_on_kicked], false, T(#{<<"remove_on_kicked">> => false})),
    ?errh(T(#{<<"backend">> => <<"nodejs">>})),
    ?errh(T(#{<<"pool_size">> => -1})),
    ?errh(T(#{<<"reset_markers">> => 1})),
    ?errh(T(#{<<"reset_markers">> => [<<"destroyed">>]})),
    ?errh(T(#{<<"groupchat">> => [<<"test">>]})),
    ?errh(T(#{<<"boxes">> => [<<"archive">>]})),
    ?errh(T(#{<<"boxes">> => [<<"duplicate">>, <<"duplicate">>]})),
    ?errh(T(#{<<"boxes">> => <<"test">>})),
    ?errh(T(#{<<"bin_ttl">> => true})),
    ?errh(T(#{<<"bin_clean_after">> => -1})),
    ?errh(T(#{<<"aff_changes">> => 1})),
    ?errh(T(#{<<"remove_on_kicked">> => 1})).

mod_global_distrib(_Config) ->
    P = [modules, mod_global_distrib],
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_global_distrib">> => Opts}} end,
    RequiredOpts = global_distrib_required_opts(),
    ExpectedCfg = mod_config(mod_global_distrib, global_distrib_expected_config()),
    ?cfgh(P, ExpectedCfg, T(RequiredOpts)),
    ?cfgh(P ++ [message_ttl], 42,
          T(RequiredOpts#{<<"message_ttl">> => 42})),
    ?cfgh(P ++ [hosts_refresh_interval], 100,
          T(RequiredOpts#{<<"hosts_refresh_interval">> => 100})),
    [?errh(T(maps:remove(Key, RequiredOpts))) || Key <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"global_host">> => <<>>})),
    ?errh(T(RequiredOpts#{<<"local_host">> => <<>>})),
    ?errh(T(RequiredOpts#{<<"local_host">> => <<"nohost">>})), % passed to 'endpoints', not resolved
    ?errh(T(RequiredOpts#{<<"message_ttl">> => -1})),
    ?errh(T(RequiredOpts#{<<"hosts_refresh_interval">> => -1})).

mod_global_distrib_connections(_Config) ->
    RequiredOpts = global_distrib_required_opts(),
    P = [modules, mod_global_distrib, connections],
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_global_distrib">> =>
                                 RequiredOpts#{<<"connections">> => Opts}}}
        end,
    ?cfgh(P, global_distrib_expected_connections(), T(#{})),
    ?cfgh(P ++ [connections_per_endpoint], 22,
          T(#{<<"connections_per_endpoint">> => 22})),
    ?cfgh(P ++ [endpoint_refresh_interval], 120,
          T(#{<<"endpoint_refresh_interval">> => 120})),
    ?cfgh(P ++ [endpoint_refresh_interval_when_empty], 5,
          T(#{<<"endpoint_refresh_interval_when_empty">> => 5})),
    ?cfgh(P ++ [disabled_gc_interval], 60,
          T(#{<<"disabled_gc_interval">> => 60})),
    ?errh(T(#{<<"connections_per_endpoint">> => -1})),
    ?errh(T(#{<<"endpoint_refresh_interval">> => 0})),
    ?errh(T(#{<<"endpoint_refresh_interval_when_empty">> => 0})),
    ?errh(T(#{<<"disabled_gc_interval">> => 0})).

mod_global_distrib_connections_endpoints(_Config) ->
    check_mod_global_distrib_endpoint_opts(<<"endpoints">>),
    RequiredModOpts = global_distrib_required_opts(),
    P = [modules, mod_global_distrib, connections],
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_global_distrib">> =>
                                 RequiredModOpts#{<<"connections">> => #{<<"endpoints">> => Opts}}}}
        end,

    %% 'enpoints' propagate to 'advertised_endpoints' and 'resolved_endpoints'
    ?cfgh([{P ++ [endpoints], [{"172.16.0.2", 5555}]},
           {P ++ [advertised_endpoints], [{"172.16.0.2", 5555}]},
           {P ++ [resolved_endpoints], [{{172, 16, 0, 2}, 5555}]}],
          T([#{<<"host">> => <<"172.16.0.2">>, <<"port">> => 5555}])),

    ?cfgh([{P ++ [endpoints], [{"localhost", 15555}]},
           {P ++ [advertised_endpoints], [{"localhost", 15555}]},
           {P ++ [resolved_endpoints], [{{0, 0, 0, 0, 0, 0, 0, 1}, 15555},
                                        {{127, 0, 0, 1}, 15555}]
           }],
          T([#{<<"host">> => <<"localhost">>, <<"port">> => 15555}])),
    ?errh(T([#{<<"host">> => <<"172.16.0.299">>, <<"port">> => 5555}])),
    ?errh(T([#{<<"host">> => <<"nohost">>, <<"port">> => 15555}])).

mod_global_distrib_connections_advertised_endpoints(_Config) ->
    check_mod_global_distrib_endpoint_opts(<<"advertised_endpoints">>).

check_mod_global_distrib_endpoint_opts(OptKey) ->
    RequiredModOpts = global_distrib_required_opts(),
    P = [modules, mod_global_distrib, connections, binary_to_atom(OptKey)],
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_global_distrib">> =>
                                 RequiredModOpts#{<<"connections">> => #{OptKey => Opts}}}}
        end,
    RequiredOpts = #{<<"host">> => <<"172.16.0.2">>, <<"port">> => 5678},
    ?cfgh(P, [{"172.16.0.2", 5678}], T([RequiredOpts])),
    [?errh(T(maps:remove(Key, RequiredOpts))) || Key <- maps:keys(RequiredOpts)],
    ?errh(T([RequiredOpts#{<<"host">> => <<>>}])),
    ?errh(T([RequiredOpts#{<<"port">> => -1}])).

mod_global_distrib_connections_tls(_Config) ->
    RequiredModOpts = global_distrib_required_opts(),
    P = [modules, mod_global_distrib, connections, tls],
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_global_distrib">> =>
                                 RequiredModOpts#{<<"connections">> => #{<<"tls">> => Opts}}}}
        end,
    RequiredOpts = #{<<"certfile">> => <<"priv/cert.pem">>,
                     <<"cacertfile">> => <<"priv/ca.pem">>},
    ExpectedCfg = config(P, #{certfile => "priv/cert.pem", cafile => "priv/ca.pem"}),
    ?cfgh(P, ExpectedCfg, T(RequiredOpts)),
    ?cfgh(P ++ [ciphers], "TLS_AES_256_GCM_SHA384",
          T(RequiredOpts#{<<"ciphers">> => <<"TLS_AES_256_GCM_SHA384">>})),
    ?cfgh(P ++ [dhfile], "priv/cert.pem",
          T(RequiredOpts#{<<"dhfile">> => <<"priv/cert.pem">>})),
    [?errh(T(maps:remove(Key, RequiredOpts))) || Key <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"certfile">> => <<"/this/does/not/exist">>})),
    ?errh(T(RequiredOpts#{<<"cacertfile">> => <<"/this/does/not/exist">>})),
    ?errh(T(RequiredOpts#{<<"dhfile">> => <<"/this/does/not/exist">>})),
    ?errh(T(RequiredOpts#{<<"ciphers">> => 42})).

mod_global_distrib_redis(_Config) ->
    RequiredModOpts = global_distrib_required_opts(),
    P = [modules, mod_global_distrib, redis],
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_global_distrib">> =>
                                 RequiredModOpts#{<<"redis">> => Opts}}}
        end,
    ?cfgh(P, default_config(P), T(#{})),
    ?cfgh(P ++ [pool], global_distrib, T(#{<<"pool">> => <<"global_distrib">>})),
    ?cfgh(P ++ [expire_after], 120, T(#{<<"expire_after">> => 120})),
    ?cfgh(P ++ [refresh_after], 60, T(#{<<"refresh_after">> => 60})),
    ?errh(T(#{<<"pool">> => <<"">>})),
    ?errh(T(#{<<"expire_after">> => 0})),
    ?errh(T(#{<<"refresh_after">> => -1})).

mod_global_distrib_cache(_Config) ->
    RequiredModOpts = global_distrib_required_opts(),
    P = [modules, mod_global_distrib, cache],
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_global_distrib">> =>
                                 RequiredModOpts#{<<"cache">> => Opts}}}
        end,
    ?cfgh(P, default_config(P), T(#{})),
    ?cfgh(P ++ [cache_missed], false, T(#{<<"cache_missed">> => false})),
    ?cfgh(P ++ [domain_lifetime_seconds], 60, T(#{<<"domain_lifetime_seconds">> => 60})),
    ?cfgh(P ++ [jid_lifetime_seconds], 30, T(#{<<"jid_lifetime_seconds">> => 30})),
    ?cfgh(P ++ [max_jids], 9999, T(#{<<"max_jids">> => 9999})),
    ?errh(T(#{<<"cache_missed">> => <<"yes">>})),
    ?errh(T(#{<<"domain_lifetime_seconds">> => -1})),
    ?errh(T(#{<<"jid_lifetime_seconds">> => -1})),
    ?errh(T(#{<<"max_jids">> => -1})).

mod_global_distrib_bounce(_Config) ->
    RequiredModOpts = global_distrib_required_opts(),
    P = [modules, mod_global_distrib, bounce],
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_global_distrib">> =>
                                 RequiredModOpts#{<<"bounce">> => Opts}}}
        end,
    ?cfgh(P, default_config(P), T(#{})),
    ?cfgh(P ++ [enabled], false, T(#{<<"enabled">> => false})),
    ?cfgh(P ++ [resend_after_ms], 300, T(#{<<"resend_after_ms">> => 300})),
    ?cfgh(P ++ [max_retries], 3, T(#{<<"max_retries">> => 3})),
    ?errh(T(#{<<"enabled">> => <<"">>})),
    ?errh(T(#{<<"resend_after_ms">> => -1})),
    ?errh(T(#{<<"max_retries">> => -1})).

global_distrib_required_opts() ->
    #{<<"global_host">> => <<"global.example.com">>,
      <<"local_host">> => <<"localhost">>}.

global_distrib_expected_config() ->
    #{global_host => <<"global.example.com">>,
      local_host => <<"localhost">>,
      connections => global_distrib_expected_connections()}.

global_distrib_expected_connections() ->
    config([modules, mod_global_distrib, connections],
           #{endpoints => [{"localhost", 5555}],
             advertised_endpoints => [{"localhost", 5555}],
             resolved_endpoints => [{{0, 0, 0, 0, 0, 0, 0, 1}, 5555},
                                    {{127, 0, 0, 1}, 5555}]}).

mod_event_pusher_sns(_Config) ->
    RequiredOpts = #{<<"sns_host">> => <<"sns.eu-west-1.amazonaws.com">>,
                     <<"region">> => <<"eu-west-1">>,
                     <<"access_key_id">> => <<"AKIAIOSFODNN7EXAMPLE">>,
                     <<"secret_access_key">> => <<"KEY">>,
                     <<"account_id">> => <<"123456789012">>},
    ExpectedCfg = #{sns_host => "sns.eu-west-1.amazonaws.com",
                    region => "eu-west-1",
                    access_key_id => "AKIAIOSFODNN7EXAMPLE",
                    secret_access_key => "KEY",
                    account_id => "123456789012"},
    P = [modules, mod_event_pusher, sns],
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_event_pusher">> => #{<<"sns">> => Opts}}}
        end,
    ?cfgh(P, config(P, ExpectedCfg), T(RequiredOpts)),
    ?cfgh(P ++ [presence_updates_topic], "pres",
          T(RequiredOpts#{<<"presence_updates_topic">> => <<"pres">>})),
    ?cfgh(P ++ [pm_messages_topic], "pm",
          T(RequiredOpts#{<<"pm_messages_topic">> => <<"pm">>})),
    ?cfgh(P ++ [muc_messages_topic], "muc",
          T(RequiredOpts#{<<"muc_messages_topic">> => <<"muc">>})),
    ?cfgh(P ++ [plugin_module], mod_event_pusher_sns_defaults,
          T(RequiredOpts#{<<"plugin_module">> => <<"mod_event_pusher_sns_defaults">>})),
    ?cfgh(P ++ [pool_size], 10,
          T(RequiredOpts#{<<"pool_size">> => 10})),
    ?cfgh(P ++ [publish_retry_count], 1,
          T(RequiredOpts#{<<"publish_retry_count">> => 1})),
    ?cfgh(P ++ [publish_retry_time_ms], 100,
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
    P = [modules, mod_event_pusher, push],
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_event_pusher">> => #{<<"push">> => Opts}}}
        end,
    ?cfgh(P, default_config(P), T(#{})),
    check_iqdisc(P, T),
    test_wpool(P ++ [wpool], fun(Opts) -> T(#{<<"wpool">> => Opts}) end),
    ?cfgh(P ++ [backend], rdbms, T(#{<<"backend">> => <<"rdbms">>})),
    ?cfgh(P ++ [plugin_module], mod_event_pusher_push_plugin_enhanced,
          T(#{<<"plugin_module">> => <<"mod_event_pusher_push_plugin_enhanced">>})),
    ?cfgh(P ++ [virtual_pubsub_hosts], [{fqdn, <<"host1">>}, {fqdn, <<"host2">>}],
          T(#{<<"virtual_pubsub_hosts">> => [<<"host1">>, <<"host2">>]})),
    ?cfgh(P ++ [virtual_pubsub_hosts], [{prefix, <<"pubsub.">>}, {prefix, <<"pub-sub.">>}],
          T(#{<<"virtual_pubsub_hosts">> => [<<"pubsub.@HOST@">>, <<"pub-sub.@HOST@">>]})),
    ?errh(T(#{<<"backend">> => <<"redis">>})),
    ?errh(T(#{<<"wpool">> => true})),
    ?errh(T(#{<<"plugin_module">> => <<"wow_cool_but_missing">>})),
    ?errh(T(#{<<"plugin_module">> => 1})),
    ?errh(T(#{<<"virtual_pubsub_hosts">> => [<<"host with whitespace">>]})),
    ?errh(T(#{<<"virtual_pubsub_hosts">> => [<<"invalid.sub@HOST@">>]})),
    ?errh(T(#{<<"virtual_pubsub_hosts">> => [<<"invalid.sub.@HOST@.as.well">>]})).

test_wpool(P, T) ->
    ?cfgh(P, default_config(P), T(#{})),
    ?cfgh(P ++ [workers], 200, T(#{<<"workers">> => 200})),
    ?cfgh(P ++ [strategy], random_worker, T(#{<<"strategy">> => <<"random_worker">>})),
    ?cfgh(P ++ [call_timeout], 1000, T(#{<<"call_timeout">> => 1000})),
    ?errh(T(#{<<"workers">> => 0})),
    ?errh(T(#{<<"strategy">> => <<"worst_worker">>})),
    ?errh(T(#{<<"workers">> => 0})).

mod_event_pusher_http(_Config) ->
    P = [modules, mod_event_pusher, http, handlers],
    T = fun(Handlers) -> #{<<"modules">> =>
                           #{<<"mod_event_pusher">> =>
                                 #{<<"http">> => #{<<"handlers">> => Handlers}}}}
        end,
    DefaultHandler = mod_event_pusher_http_handler(),
    ?cfgh(P, [DefaultHandler], T([#{}])),
    ?cfgh(P, [DefaultHandler#{pool_name => my_pool}],
          T([#{<<"pool_name">> => <<"my_pool">>}])),
    ?cfgh(P, [DefaultHandler#{path => <<"notifications">>}],
          T([#{<<"path">> => <<"/notifications">>}])),
    ?cfgh(P, [DefaultHandler#{callback_module => mod_event_pusher_http}], % existing module
          T([#{<<"callback_module">> => <<"mod_event_pusher_http">>}])),
    ?cfgh(P, [DefaultHandler, DefaultHandler#{pool_name => my_pool}], % two handlers
          T([#{}, #{<<"pool_name">> => <<"my_pool">>}])),
    ?errh(T([#{<<"pool_name">> => <<>>}])),
    ?errh(T([#{<<"path">> => true}])),
    ?errh(T([#{<<"callback_module">> => <<"wow_cool_but_missing">>}])),
    ?errh(T([#{<<"callback_module">> => 1}])),
    ?errh(T([#{}, #{}])). % handlers have to be unique

mod_event_pusher_rabbit(_Config) ->
    P = [modules, mod_event_pusher, rabbit],
    T = fun(Key, Opts) -> #{<<"modules">> =>
                                #{<<"mod_event_pusher">> =>
                                      #{<<"rabbit">> => #{Key => Opts}}}}
        end,
    test_event_pusher_rabbit_exchange(P ++ [presence_exchange],
                                      fun(Opts) -> T(<<"presence_exchange">>, Opts) end),
    test_event_pusher_rabbit_msg_exchange(P ++ [chat_msg_exchange],
                                          fun(Opts) -> T(<<"chat_msg_exchange">>, Opts) end),
    test_event_pusher_rabbit_msg_exchange(P ++ [groupchat_msg_exchange],
                                          fun(Opts) -> T(<<"groupchat_msg_exchange">>, Opts) end).

test_event_pusher_rabbit_msg_exchange(P, T) ->
    test_event_pusher_rabbit_exchange(P, T),
    ?cfgh(P, default_config(P), T(#{})),
    ?cfgh(P ++ [sent_topic], <<"outgoing">>, T(#{<<"sent_topic">> => <<"outgoing">>})),
    ?cfgh(P ++ [recv_topic], <<"incoming">>, T(#{<<"recv_topic">> => <<"incoming">>})),
    ?errh(T(#{<<"sent_topic">> => <<>>})),
    ?errh(T(#{<<"recv_topic">> => <<>>})).

test_event_pusher_rabbit_exchange(P, T) ->
    ?cfgh(P, default_config(P), T(#{})),
    ?cfgh(P ++ [name], <<"notifications">>, T(#{<<"name">> => <<"notifications">>})),
    ?cfgh(P ++ [type], <<"direct">>, T(#{<<"type">> => <<"direct">>})),
    ?errh(T(#{<<"name">> => <<>>})),
    ?errh(T(#{<<"type">> => <<>>})).

mod_http_upload(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_http_upload">> => Opts}} end,
    P = [modules, mod_http_upload],
    RequiredOpts = #{<<"s3">> => http_upload_s3_required_opts()},
    S3Cfg = http_upload_s3_expected_cfg(),
    ?cfgh(P, mod_config(mod_http_upload,
        #{host => <<"upload.@HOST@">>,
          s3 => config_parser_helper:config([modules, mod_http_upload, s3], S3Cfg)
         }),
        T(RequiredOpts)),
    ?cfgh(P ++ [s3], S3Cfg#{add_acl => false}, T(RequiredOpts)),
    ?cfgh(P ++ [host], {prefix, <<"upload.">>},
          T(RequiredOpts#{<<"host">> => <<"upload.@HOST@">>})),
    ?cfgh(P ++ [host], {fqdn, <<"upload.test">>},
          T(RequiredOpts#{<<"host">> => <<"upload.test">>})),
    ?cfgh(P ++ [backend], s3,
          T(RequiredOpts#{<<"backend">> => <<"s3">>})),
    ?cfgh(P ++ [expiration_time], 666,
          T(RequiredOpts#{<<"expiration_time">> => 666})),
    ?cfgh(P ++ [token_bytes], 32,
          T(RequiredOpts#{<<"token_bytes">> => 32})),
    ?cfgh(P ++ [max_file_size], 42,
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
    check_iqdisc(mod_http_upload, RequiredOpts).

mod_http_upload_s3(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_http_upload">> =>
                                              #{<<"s3">> => Opts}}} end,
    RequiredOpts = http_upload_s3_required_opts(),
    ExpectedCfg = http_upload_s3_expected_cfg(),
    P = [modules, mod_http_upload, s3],
    ?cfgh(P, ExpectedCfg#{add_acl => false}, T(RequiredOpts)),
    ?cfgh(P ++ [add_acl], true, T(RequiredOpts#{<<"add_acl">> => true})),
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
    #{access_key_id => <<"PLEASE">>,
      bucket_url => <<"https://s3-eu-west-1.amazonaws.com/mybucket">>,
      region => <<"antarctica-1">>,
      secret_access_key => <<"ILOVEU">>}.

mod_jingle_sip(_Config) ->
    check_module_defaults(mod_jingle_sip),
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_jingle_sip">> => Opts}} end,
    P = [modules, mod_jingle_sip],
    ?cfgh(P ++ [proxy_host], "proxxxy.com",
          T(#{<<"proxy_host">> => <<"proxxxy.com">>})),
    ?cfgh(P ++ [proxy_port], 5601,
          T(#{<<"proxy_port">> => 5601})),
    ?cfgh(P ++ [listen_port], 5602,
          T(#{<<"listen_port">> => 5602})),
    ?cfgh(P ++ [local_host], "localhost",
          T(#{<<"local_host">> => <<"localhost">>})),
    ?cfgh(P ++ [sdp_origin], "127.0.0.1",
          T(#{<<"sdp_origin">> => <<"127.0.0.1">>})),
    ?cfgh(P ++ [transport], "tcp",
          T(#{<<"transport">> => <<"tcp">>})),
    ?cfgh(P ++ [username_to_phone], [{<<"2000006168">>, <<"+919177074440">>}],
          T(#{<<"username_to_phone">> => [#{<<"username">> => <<"2000006168">>,
                                            <<"phone">> => <<"+919177074440">>}]})),
    ?errh(T(#{<<"proxy_host">> => 1})),
    ?errh(T(#{<<"proxy_port">> => 1000000})),
    ?errh(T(#{<<"listen_port">> => -1})),
    ?errh(T(#{<<"local_host">> => <<>>})),
    ?errh(T(#{<<"sdp_origin">> => <<"abc">>})).

mod_keystore(_Config) ->
    check_module_defaults(mod_keystore),
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_keystore">> => Opts}} end,
    P = [modules, mod_keystore],
    ?cfgh(P ++ [ram_key_size], 1024,
          T(#{<<"ram_key_size">> => 1024})),
    ?errh(T(#{<<"ram_key_size">> => -1})).

mod_keystore_keys(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_keystore">> =>
                                              #{<<"keys">> => Opts}}}
        end,
    P = [modules, mod_keystore, keys],
    RequiredOpts = #{<<"name">> => <<"access_secret">>,
                     <<"type">> => <<"ram">>},
    ?cfgh(P ++ [access_secret], ram,
          T([RequiredOpts])),
    ?cfgh(P ++ [access_secret], {file, "priv/access_psk"},
          T([RequiredOpts#{<<"type">> => <<"file">>,
                           <<"path">> => <<"priv/access_psk">>}])),
    [?errh(T([maps:remove(Key, RequiredOpts)])) || Key <- maps:keys(RequiredOpts)],
    ?errh(T([RequiredOpts#{<<"name">> => <<>>}])),
    ?errh(T([RequiredOpts#{<<"type">> => <<"rampampam">>}])),
    ?errh(T([RequiredOpts#{<<"type">> => <<"file">>}])),
    ?errh(T([RequiredOpts#{<<"type">> => <<"file">>,
                           <<"path">> => <<"does/not/exists">>}])),
    ?errh(T([#{<<"name">> => <<"same_name_twice">>,
               <<"type">> => <<"ram">>},
             #{<<"name">> => <<"same_name_twice">>,
               <<"type">> => <<"file">>,
               <<"path">> => <<"priv/access_psk">>}])).

mod_last(_Config) ->
    check_iqdisc(mod_last),
    check_module_defaults(mod_last),
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_last">> => Opts}} end,
    P = [modules, mod_last],
    ?cfgh(P ++ [backend], mnesia, T(#{<<"backend">> => <<"mnesia">>})),
    ?cfgh(P ++ [backend], rdbms, T(#{<<"backend">> => <<"rdbms">>})),
    ?cfgh(P ++ [riak, bucket_type], <<"last">>, T(#{<<"backend">> => <<"riak">>})),
    ?cfgh(P ++ [riak, bucket_type], <<"test">>,
          T(#{<<"backend">> => <<"riak">>, <<"riak">> => #{<<"bucket_type">> => <<"test">>}})),
    ?errh(T(#{<<"backend">> => <<"frontend">>})),
    ?errh(T(#{<<"riak">> => #{<<"bucket_type">> => 1}})).

mod_mam_meta(_Config) ->
    check_module_defaults(mod_mam_meta),
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_mam_meta">> => Opts}} end,
    P = [modules, mod_mam_meta],
    test_cache_config(P ++ [cache], fun(Opts) -> T(#{<<"cache">> => Opts}) end),
    test_mod_mam_meta(P, T).

mod_mam_meta_riak(_Config) ->
    T = fun(Opts) ->
                #{<<"modules">> => #{<<"mod_mam_meta">> => Opts#{<<"backend">> => <<"riak">>}}}
        end,
    P = [modules, mod_mam_meta, riak],
    ?cfgh(P, default_config([modules, mod_mam_meta, riak]), T(#{})),
    ?cfgh(P ++ [bucket_type], <<"mam_bucket">>,
          T(#{<<"riak">> => #{<<"bucket_type">> => <<"mam_bucket">>}})),
    ?cfgh(P ++ [search_index], <<"mam_index">>,
          T(#{<<"riak">> => #{<<"search_index">> => <<"mam_index">>}})),
    ?errh(T(#{<<"riak">> => #{<<"bucket_type">> => <<>>}})),
    ?errh(T(#{<<"riak">> => #{<<"search_index">> => <<>>}})).

mod_mam_meta_pm(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_mam_meta">> => #{<<"pm">> => Opts}}} end,
    P = [modules, mod_mam_meta, pm],
    test_mod_mam_meta(P, T),
    ?cfgh(P, default_config(P), T(#{})),
    ?cfgh(P ++ [archive_groupchats], true, T(#{<<"archive_groupchats">> => true})),
    ?cfgh(P ++ [same_mam_id_for_peers], true, T(#{<<"same_mam_id_for_peers">> => true})),
    ?errh(T(#{<<"host">> => <<"muc.@HOST@">>})), % muc-only
    ?errh(T(#{<<"archive_groupchats">> => <<"not really">>})),
    ?errh(T(#{<<"same_mam_id_for_peers">> => <<"not really">>})).

mod_mam_meta_muc(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_mam_meta">> => #{<<"muc">> => Opts}}} end,
    P = [modules, mod_mam_meta, muc],
    test_mod_mam_meta(P, T),
    ?cfgh(P, default_config(P), T(#{})),
    ?cfgh(P ++ [host], {prefix, <<"muc.">>}, T(#{<<"host">> => <<"muc.@HOST@">>})),
    ?cfgh(P ++ [host], {fqdn, <<"muc.test">>}, T(#{<<"host">> => <<"muc.test">>})),
    ?errh(T(#{<<"host">> => <<"is this a host? no.">>})),
    ?errh(T(#{<<"host">> => [<<"invalid.sub@HOST@">>]})),
    ?errh(T(#{<<"host">> => [<<"invalid.sub.@HOST@.as.well">>]})),
    ?errh(T(#{<<"archive_groupchats">> => true})), % pm-only
    ?errh(T(#{<<"same_mam_id_for_peers">> => true})). % pm-only

test_mod_mam_meta(P, T) ->
    test_async_writer(P, T),
    ?cfgh(P ++ [backend], rdbms,
          T(#{<<"backend">> => <<"rdbms">>})),
    ?cfgh(P ++ [no_stanzaid_element], true,
          T(#{<<"no_stanzaid_element">> => true})),
    ?cfgh(P ++ [is_archivable_message], mod_mam_utils,
          T(#{<<"is_archivable_message">> => <<"mod_mam_utils">>})),
    ?cfgh(P ++ [archive_chat_markers], true,
          T(#{<<"archive_chat_markers">> => true})),
    ?cfgh(P ++ [message_retraction], false,
          T(#{<<"message_retraction">> => false})),
    ?cfgh(P ++ [user_prefs_store], rdbms,
          T(#{<<"user_prefs_store">> => <<"rdbms">>})),
    ?cfgh(P ++ [full_text_search], false,
          T(#{<<"full_text_search">> => false})),
    ?cfgh(P ++ [cache_users], false,
          T(#{<<"cache_users">> => false})),
    ?cfgh(P ++ [default_result_limit], 100,
          T(#{<<"default_result_limit">> => 100})),
    ?cfgh(P ++ [max_result_limit], 1000,
          T(#{<<"max_result_limit">> => 1000})),
    ?cfgh(P ++ [db_jid_format], mam_jid_rfc,
          T(#{<<"db_jid_format">> => <<"mam_jid_rfc">>})),
    ?cfgh(P ++ [db_message_format], mam_message_xml,
          T(#{<<"db_message_format">> => <<"mam_message_xml">>})),
    ?cfgh(P ++ [extra_fin_element], mod_mam_utils,
          T(#{<<"extra_fin_element">> => <<"mod_mam_utils">>})),
    ?cfgh(P ++ [extra_lookup_params], mod_mam_utils,
          T(#{<<"extra_lookup_params">> => <<"mod_mam_utils">>})),
    ?errh(T(#{<<"backend">> => <<"notepad">>})),
    ?errh(T(#{<<"no_stanzaid_element">> => <<"true">>})),
    ?errh(T(#{<<"is_archivable_message">> => <<"mod_mam_fake">>})),
    ?errh(T(#{<<"archive_chat_markers">> => <<"maybe">>})),
    ?errh(T(#{<<"message_retraction">> => 1})),
    ?errh(T(#{<<"user_prefs_store">> => <<"textfile">>})),
    ?errh(T(#{<<"full_text_search">> => <<"disabled">>})),
    ?errh(T(#{<<"cache_users">> => []})),
    ?errh(T(#{<<"default_result_limit">> => -1})),
    ?errh(T(#{<<"max_result_limit">> => -2})),
    ?errh(T(#{<<"db_jid_format">> => <<"not_a_module">>})),
    ?errh(T(#{<<"db_message_format">> => <<"not_a_module">>})),
    ?errh(T(#{<<"extra_fin_element">> => <<"bad_module">>})),
    ?errh(T(#{<<"extra_lookup_params">> => <<"bad_module">>})).

test_cache_config(P, T) ->
    ?cfgh(P ++ [module], internal, T(#{<<"module">> => <<"internal">>})),
    ?cfgh(P ++ [time_to_live], 8600, T(#{<<"time_to_live">> => 8600})),
    ?cfgh(P ++ [time_to_live], infinity, T(#{<<"time_to_live">> => <<"infinity">>})),
    ?cfgh(P ++ [number_of_segments], 10, T(#{<<"number_of_segments">> => 10})),
    ?cfgh(P ++ [strategy], fifo, T(#{<<"strategy">> => <<"fifo">>})),
    ?errh(T(#{<<"module">> => <<"mod_wrong_cache">>})),
    ?errh(T(#{<<"time_to_live">> => 0})),
    ?errh(T(#{<<"strategy">> => <<"lifo">>})),
    ?errh(T(#{<<"number_of_segments">> => 0})),
    ?errh(T(#{<<"number_of_segments">> => <<"infinity">>})),
    ?errh(T(#{<<"cache">> => []})).

test_async_writer(ParentP, ParentT) ->
    P = ParentP ++ [async_writer],
    T = fun(Opts) -> ParentT(#{<<"async_writer">> => Opts}) end,
    ?cfgh(P ++ [flush_interval], 1500, T(#{<<"flush_interval">> => 1500})),
    ?cfgh(P ++ [batch_size], 1500, T(#{<<"batch_size">> => 1500})),
    ?cfgh(P ++ [pool_size], 1500, T(#{<<"pool_size">> => 1500})),
    ?cfgh(P ++ [enabled], false, T(#{<<"enabled">> => false})),
    ?errh(T(#{<<"flush_interval">> => -1})),
    ?errh(T(#{<<"batch_size">> => -1})),
    ?errh(T(#{<<"pool_size">> => -1})),
    ?errh(T(#{<<"enabled">> => <<"wrong">>})).

mod_muc(_Config) ->
    check_module_defaults(mod_muc),
    P = [modules, mod_muc],
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_muc">> => #{K => V}}} end,
    ?cfgh(P ++ [host], {prefix, <<"conference.">>},
          T(<<"host">>, <<"conference.@HOST@">>)),
    ?cfgh(P ++ [host], {fqdn, <<"conference.test">>},
          T(<<"host">>, <<"conference.test">>)),
    ?cfgh(P ++ [backend], mnesia, T(<<"backend">>, <<"mnesia">>)),
    ?cfgh(P ++ [access], all, T(<<"access">>, <<"all">>)),
    ?cfgh(P ++ [access_create], admin, T(<<"access_create">>, <<"admin">>)),
    ?cfgh(P ++ [access_admin], none, T(<<"access_admin">>, <<"none">>)),
    ?cfgh(P ++ [access_persistent], all, T(<<"access_persistent">>, <<"all">>)),
    ?cfgh(P ++ [history_size], 20, T(<<"history_size">>, 20)),
    ?cfgh(P ++ [room_shaper], muc_room_shaper,
          T(<<"room_shaper">>, <<"muc_room_shaper">>)),
    ?cfgh(P ++ [max_room_id], infinity, T(<<"max_room_id">>, <<"infinity">>)),
    ?cfgh(P ++ [max_room_name], 30, T(<<"max_room_name">>, 30)),
    ?cfgh(P ++ [max_room_desc], 0, T(<<"max_room_desc">>, 0)),
    ?cfgh(P ++ [min_message_interval], 10, T(<<"min_message_interval">>, 10)),
    ?cfgh(P ++ [min_presence_interval], 0, T(<<"min_presence_interval">>, 0)),
    ?cfgh(P ++ [max_users], 30, T(<<"max_users">>, 30)),
    ?cfgh(P ++ [max_users_admin_threshold], 2,
          T(<<"max_users_admin_threshold">>, 2)),
    ?cfgh(P ++ [user_message_shaper], muc_msg_shaper,
          T(<<"user_message_shaper">>, <<"muc_msg_shaper">>)),
    ?cfgh(P ++ [user_presence_shaper], muc_pres_shaper,
          T(<<"user_presence_shaper">>, <<"muc_pres_shaper">>)),
    ?cfgh(P ++ [max_user_conferences], 10, T(<<"max_user_conferences">>, 10)),
    ?cfgh(P ++ [http_auth_pool], external_auth,
          T(<<"http_auth_pool">>, <<"external_auth">>)),
    ?cfgh(P ++ [load_permanent_rooms_at_startup], true,
          T(<<"load_permanent_rooms_at_startup">>, true)),
    ?cfgh(P ++ [hibernate_timeout], infinity,
          T(<<"hibernate_timeout">>, <<"infinity">>)),
    ?cfgh(P ++ [hibernated_room_check_interval], 5000,
          T(<<"hibernated_room_check_interval">>, 5000)),
    ?cfgh(P ++ [hibernated_room_timeout], 0,
          T(<<"hibernated_room_timeout">>, 0)),
    ?errh(T(<<"host">>, <<>>)),
    ?errh(T(<<"host">>, <<"is this a host? no.">>)),
    ?errh(T(<<"host">>, [<<"invalid.sub@HOST@">>])),
    ?errh(T(<<"host">>, [<<"invalid.sub.@HOST@.as.well">>])),
    ?errh(T(<<"backend">>, <<"amnesia">>)),
    ?errh(T(<<"access">>, <<>>)),
    ?errh(T(<<"access_create">>, 1)),
    ?errh(T(<<"access_admin">>, [])),
    ?errh(T(<<"access_persistent">>, true)),
    ?errh(T(<<"history_size">>, <<"20">>)),
    ?errh(T(<<"room_shaper">>, <<>>)),
    ?errh(T(<<"max_room_id">>, #{})),
    ?errh(T(<<"max_room_name">>, <<"infinite!">>)),
    ?errh(T(<<"max_room_desc">>, -1)),
    ?errh(T(<<"min_message_interval">>, -10)),
    ?errh(T(<<"min_presence_interval">>, <<"infinity">>)),
    ?errh(T(<<"max_users">>, 0)),
    ?errh(T(<<"max_users_admin_threshold">>, 0)),
    ?errh(T(<<"user_message_shaper">>, [])),
    ?errh(T(<<"user_presence_shaper">>, <<>>)),
    ?errh(T(<<"max_user_conferences">>, -1)),
    ?errh(T(<<"http_auth_pool">>, <<>>)),
    ?errh(T(<<"load_permanent_rooms_at_startup">>, <<"true">>)),
    ?errh(T(<<"hibernate_timeout">>, <<"really big">>)),
    ?errh(T(<<"hibernated_room_check_interval">>, -1)),
    ?errh(T(<<"hibernated_room_timeout">>, false)).

mod_muc_default_room(_Config) ->
    P = [modules, mod_muc, default_room],
    T = fun(K, V) ->
                M = #{<<"mod_muc">> => #{<<"default_room">> => #{K => V}}},
                #{<<"modules">> => M}
        end,
    ?cfgh(P ++ [title], <<"living room">>, T(<<"title">>, <<"living room">>)),
    ?cfgh(P ++ [description], <<"a room that is alive">>,
          T(<<"description">>, <<"a room that is alive">>)),
    ?cfgh(P ++ [allow_change_subj], true, T(<<"allow_change_subj">>, true)),
    ?cfgh(P ++ [allow_query_users], false, T(<<"allow_query_users">>, false)),
    ?cfgh(P ++ [allow_private_messages], true,
          T(<<"allow_private_messages">>, true)),
    ?cfgh(P ++ [allow_visitor_status], false,
          T(<<"allow_visitor_status">>, false)),
    ?cfgh(P ++ [allow_visitor_nickchange], true,
          T(<<"allow_visitor_nickchange">>, true)),
    ?cfgh(P ++ [public], false, T(<<"public">>, false)),
    ?cfgh(P ++ [public_list], true, T(<<"public_list">>, true)),
    ?cfgh(P ++ [persistent], true, T(<<"persistent">>, true)),
    ?cfgh(P ++ [moderated], false, T(<<"moderated">>, false)),
    ?cfgh(P ++ [members_by_default], true, T(<<"members_by_default">>, true)),
    ?cfgh(P ++ [members_only], false, T(<<"members_only">>, false)),
    ?cfgh(P ++ [allow_user_invites], true, T(<<"allow_user_invites">>, true)),
    ?cfgh(P ++ [allow_multiple_sessions], false,
          T(<<"allow_multiple_sessions">>, false)),
    ?cfgh(P ++ [password_protected], true, T(<<"password_protected">>, true)),
    ?cfgh(P ++ [password], <<"secret">>, T(<<"password">>, <<"secret">>)),
    ?cfgh(P ++ [anonymous], true, T(<<"anonymous">>, true)),
    ?cfgh(P ++ [max_users],  100, T(<<"max_users">>, 100)),
    ?cfgh(P ++ [logging], false, T(<<"logging">>, false)),
    ?cfgh(P ++ [maygetmemberlist], [moderator],
          T(<<"maygetmemberlist">>, [<<"moderator">>])),
    ?cfgh(P ++ [subject], <<"Lambda days">>, T(<<"subject">>, <<"Lambda days">>)),
    ?cfgh(P ++ [subject_author], <<"Alice">>, T(<<"subject_author">>, <<"Alice">>)),
    ?errh(T(<<"title">>, true)),
    ?errh(T(<<"description">>, 1)),
    ?errh(T(<<"allow_change_subj">>, <<"true">>)),
    ?errh(T(<<"allow_query_users">>, <<>>)),
    ?errh(T(<<"allow_private_messages">>, 1)),
    ?errh(T(<<"allow_visitor_status">>, [])),
    ?errh(T(<<"allow_visitor_nickchange">>, #{})),
    ?errh(T(<<"public">>, 0)),
    ?errh(T(<<"public_list">>, [false])),
    ?errh(T(<<"persistent">>, 1)),
    ?errh(T(<<"moderated">>, <<"yes">>)),
    ?errh(T(<<"members_by_default">>, 0)),
    ?errh(T(<<"members_only">>, [true])),
    ?errh(T(<<"allow_user_invites">>, <<>>)),
    ?errh(T(<<"allow_multiple_sessions">>, [])),
    ?errh(T(<<"password_protected">>, #{})),
    ?errh(T(<<"password">>, false)),
    ?errh(T(<<"anonymous">>, <<"maybe">>)),
    ?errh(T(<<"max_users">>, 0)),
    ?errh(T(<<"logging">>, [true, false])),
    ?errh(T(<<"maygetmemberlist">>, <<"moderator">>)),
    ?errh(T(<<"maygetmemberlist">>, [<<>>])),
    ?errh(T(<<"subject">>, [<<"subjective">>])),
    ?errh(T(<<"subject_author">>, 1)).

mod_muc_default_room_affiliations(_Config) ->
    P = [modules, mod_muc, default_room, affiliations],
    T = fun(V) ->
                M = #{<<"mod_muc">> => #{<<"default_room">> => #{<<"affiliations">> => V}}},
                #{<<"modules">> => M}
        end,
    RequiredOpts = #{<<"user">> => <<"alice">>,
                     <<"server">> => <<"localhost">>,
                     <<"resource">> => <<"phone">>,
                     <<"affiliation">> => <<"moderator">>},
    ExpectedCfg = {{<<"alice">>, <<"localhost">>, <<"phone">>}, moderator},
    ?cfgh(P, [], T([])),
    ?cfgh(P, [ExpectedCfg], T([RequiredOpts])),
    [?errh(T([maps:remove(Key, RequiredOpts)])) || Key <- maps:keys(RequiredOpts)],
    ?errh(T([RequiredOpts#{<<"user">> := <<>>}])),
    ?errh(T([RequiredOpts#{<<"server">> := <<"domain? not really!">>}])),
    ?errh(T([RequiredOpts#{<<"resource">> := false}])),
    ?errh(T([RequiredOpts#{<<"affiliation">> := <<>>}])).

mod_muc_log(_Config) ->
    check_module_defaults(mod_muc_log),
    P = [modules, mod_muc_log],
    T = fun(K, V) -> #{<<"modules">> => #{<<"mod_muc_log">> => #{K => V}}} end,
    ?cfgh(P ++ [outdir], "www/muc", T(<<"outdir">>, <<"www/muc">>)),
    ?cfgh(P ++ [access_log], muc_admin, T(<<"access_log">>, <<"muc_admin">>)),
    ?cfgh(P ++ [dirtype], subdirs, T(<<"dirtype">>, <<"subdirs">>)),
    ?cfgh(P ++ [dirname], room_name, T(<<"dirname">>, <<"room_name">>)),
    ?cfgh(P ++ [file_format], html, T(<<"file_format">>, <<"html">>)),
    ?cfgh(P ++ [css_file], <<"path/to/css_file">>,
          T(<<"css_file">>, <<"path/to/css_file">>)),
    ?cfgh(P ++ [timezone], local, T(<<"timezone">>, <<"local">>)),
    ?cfgh(P ++ [spam_prevention], false, T(<<"spam_prevention">>, false)),
    ?errh(T(<<"outdir">>, <<"does/not/exist">>)),
    ?errh(T(<<"access_log">>, 1)),
    ?errh(T(<<"dirtype">>, <<"imaginary">>)),
    ?errh(T(<<"dirname">>, <<"dyrektory">>)),
    ?errh(T(<<"file_format">>, <<"none">>)),
    ?errh(T(<<"css_file">>, <<>>)),
    ?errh(T(<<"timezone">>, <<"yes">>)),
    ?errh(T(<<"spam_prevention">>, <<"spam and eggs and spam">>)).

mod_muc_log_top_link(_Config) ->
    P = [modules, mod_muc_log, top_link],
    T = fun(V) ->
                M = #{<<"mod_muc_log">> => #{<<"top_link">> => V}},
                #{<<"modules">> => M}
        end,
    RequiredOpts = #{<<"target">> => <<"https://esl.github.io/MongooseDocs/">>,
                     <<"text">> => <<"Docs">>},
    ExpectedCfg = {"https://esl.github.io/MongooseDocs/", "Docs"},
    ?cfgh(P, ExpectedCfg, T(RequiredOpts)),
    [?errh(T(maps:remove(K, RequiredOpts))) || K <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"target">> => true})),
    ?errh(T(RequiredOpts#{<<"text">> => <<"">>})).

mod_muc_light(_Config) ->
    check_module_defaults(mod_muc_light),
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_muc_light">> => Opts}} end,
    P = [modules, mod_muc_light],
    test_cache_config(P ++ [cache_affs], fun(Opts) -> T(#{<<"cache_affs">> => Opts}) end),
    ?cfgh(P ++ [backend], mnesia,
          T(#{<<"backend">> => <<"mnesia">>})),
    ?cfgh(P ++ [host], {prefix, <<"muclight.">>},
          T(#{<<"host">> => <<"muclight.@HOST@">>})),
    ?cfgh(P ++ [host], {fqdn, <<"muclight.test">>},
          T(#{<<"host">> => <<"muclight.test">>})),
    ?cfgh(P ++ [equal_occupants], true,
          T(#{<<"equal_occupants">> => true})),
    ?cfgh(P ++ [legacy_mode], false,
          T(#{<<"legacy_mode">> => false})),
    ?cfgh(P ++ [rooms_per_user], 100,
          T(#{<<"rooms_per_user">> => 100})),
    ?cfgh(P ++ [blocking], false,
          T(#{<<"blocking">> => false})),
    ?cfgh(P ++ [all_can_configure], true,
          T(#{<<"all_can_configure">> => true})),
    ?cfgh(P ++ [all_can_invite], false,
          T(#{<<"all_can_invite">> => false})),
    ?cfgh(P ++ [max_occupants], infinity,
          T(#{<<"max_occupants">> => <<"infinity">>})),
    ?cfgh(P ++ [rooms_per_page], 10,
          T(#{<<"rooms_per_page">> => 10})),
    ?cfgh(P ++ [rooms_in_rosters], true,
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
    P = [modules, mod_muc_light, config_schema],
    Field = #{<<"field">> => <<"my_field">>},
    ?cfgh(P, [], T([])),
    ?cfgh(P, [{<<"my_field">>, <<"My Room">>, my_field, binary}],
          T([Field#{<<"string_value">> => <<"My Room">>}])),
    ?cfgh(P, [{<<"my_field">>, 1, my_field, integer}],
          T([Field#{<<"integer_value">> => 1}])),
    ?cfgh(P, [{<<"my_field">>, 0.5, my_field, float}],
          T([Field#{<<"float_value">> => 0.5}])),
    ?cfgh(P, [{<<"my_field">>, 0, your_field, integer}],
          T([Field#{<<"integer_value">> => 0,
                    <<"internal_key">> => <<"your_field">>}])),
    ?cfgh(P, [{<<"żółć"/utf8>>, <<"Рентгеноэлектрокардиографический"/utf8>>, 'żółć', binary}],
          T([#{<<"field">> => <<"żółć"/utf8>>,
               <<"string_value">> => <<"Рентгеноэлектрокардиографический"/utf8>>}])),
    ?cfgh(P, [{<<"first">>, 1, first, integer}, % the config is u-key-sorted
              {<<"second">>, <<"two">>, second, binary}],
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
    check_module_defaults(mod_offline),
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_offline">> => Opts}} end,
    P = [modules, mod_offline],
    ?cfgh(P ++ [access_max_user_messages], custom_max_user_offline_messages,
          T(#{<<"access_max_user_messages">> => <<"custom_max_user_offline_messages">>})),
    ?cfgh(P ++ [backend], rdbms,
          T(#{<<"backend">> => <<"rdbms">>})),
    ?cfgh(P ++ [riak, bucket_type], <<"test">>,
          T(#{<<"backend">> => <<"riak">>, <<"riak">> => #{<<"bucket_type">> => <<"test">>}})),
    ?errh(T(#{<<"access_max_user_messages">> => 1})),
    ?errh(T(#{<<"backend">> => <<"riak_is_the_best">>})),
    ?errh(T(#{<<"riak">> => #{<<"bucket_type">> => 1}})),
    ?errh(T(#{<<"riak">> => #{<<"bucket">> => <<"leaky">>}})).

mod_offline_chatmarkers(_Config) ->
    check_module_defaults(mod_offline_chatmarkers),
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_offline_chatmarkers">> => Opts}} end,
    P = [modules, mod_offline_chatmarkers],
    ?cfgh(P ++ [backend], rdbms,
          T(#{<<"backend">> => <<"rdbms">>})),
    ?cfgh(P ++ [store_groupchat_messages], true,
          T(#{<<"store_groupchat_messages">> => true})),
    ?errh(T(#{<<"store_groupchat_messages">> => 1})),
    ?errh(T(#{<<"backend">> => <<"riak_is_the_best">>})).

mod_ping(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_ping">> => Opts}} end,
    P = [modules, mod_ping],
    check_iqdisc(mod_ping),
    check_module_defaults(mod_ping),
    ?cfgh(P ++ [send_pings], true,
          T(#{<<"send_pings">> => true})),
    ?cfgh(P ++ [ping_interval], timer:seconds(10),
          T(#{<<"ping_interval">> => 10})),
    ?cfgh(P ++ [timeout_action], kill,
          T(#{<<"timeout_action">> => <<"kill">>})),
    ?cfgh(P ++ [ping_req_timeout], timer:seconds(20),
          T(#{<<"ping_req_timeout">> => 20})),
    ?errh(T(#{<<"send_pings">> => 1})),
    ?errh(T(#{<<"ping_interval">> => 0})),
    ?errh(T(#{<<"timeout_action">> => <<"kill_them_all">>})),
    ?errh(T(#{<<"ping_req_timeout">> => 0})).

mod_privacy(_Config) ->
    test_privacy_opts(mod_privacy).

test_privacy_opts(Module) ->
    T = fun(Opts) -> #{<<"modules">> => #{atom_to_binary(Module) => Opts}} end,
    P = [modules, Module],
    check_module_defaults(Module),
    ?cfgh(P ++ [backend], mnesia,
          T(#{<<"backend">> => <<"mnesia">>})),
    ?cfgh(P ++ [riak, defaults_bucket_type], <<"defaults">>,
          T(#{<<"backend">> => <<"riak">>,
              <<"riak">> => #{<<"defaults_bucket_type">> => <<"defaults">>}})),
    ?cfgh(P ++ [riak, names_bucket_type], <<"names">>,
          T(#{<<"backend">> => <<"riak">>,
              <<"riak">> => #{<<"names_bucket_type">> => <<"names">>}})),
    ?cfgh(P ++ [riak, bucket_type], <<"bucket">>,
          T(#{<<"backend">> => <<"riak">>,
              <<"riak">> => #{<<"bucket_type">> => <<"bucket">>}})),
    ?cfgh(P ++ [riak],
          default_config([modules, mod_privacy, riak]),
          T(#{<<"backend">> => <<"riak">>})),
    ?errh(T(#{<<"backend">> => <<"mongoddt">>})),
    ?errh(T(#{<<"riak">> => #{<<"defaults_bucket_type">> => <<>>}})),
    ?errh(T(#{<<"riak">> => #{<<"names_bucket_type">> => 1}})),
    ?errh(T(#{<<"riak">> => #{<<"bucket_type">> => 1}})).

mod_private(_Config) ->
    check_iqdisc(mod_private),
    check_module_defaults(mod_private),
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_private">> => Opts}} end,
    P = [modules, mod_private],
    ?cfgh(P ++ [backend], riak, T(#{<<"backend">> => <<"riak">>})),
    ?cfgh(P ++ [riak, bucket_type], <<"private">>, T(#{<<"backend">> => <<"riak">>})),
    ?cfgh(P ++ [riak, bucket_type], <<"private_stuff">>, T(#{<<"backend">> => <<"riak">>,
        <<"riak">> => #{<<"bucket_type">> => <<"private_stuff">>}})),
    ?errh(T(#{<<"backend">> => <<"mssql">>})),
    ?errh(T(#{<<"riak">> => #{<<"bucket_type">> => 1}})).

mod_pubsub(_Config) ->
    check_iqdisc(mod_pubsub),
    check_module_defaults(mod_pubsub),
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_pubsub">> => Opts}} end,
    P = [modules, mod_pubsub],
    ?cfgh(P ++ [host], {prefix, <<"pubsub.">>},
          T(#{<<"host">> => <<"pubsub.@HOST@">>})),
    ?cfgh(P ++ [host], {fqdn, <<"pubsub.test">>},
          T(#{<<"host">> => <<"pubsub.test">>})),
    ?cfgh(P ++ [backend], rdbms,
          T(#{<<"backend">> => <<"rdbms">>})),
    ?cfgh(P ++ [access], all,
          T(#{<<"access_createnode">> => <<"all">>})),
    ?cfgh(P ++ [max_items_node], 20,
          T(#{<<"max_items_node">> => 20})),
    ?cfgh(P ++ [max_subscriptions_node], 30,
          T(#{<<"max_subscriptions_node">> => 30})),
    ?cfgh(P ++ [nodetree], nodetree_tree,
          T(#{<<"nodetree">> => <<"tree">>})),
    ?cfgh(P ++ [ignore_pep_from_offline], false,
          T(#{<<"ignore_pep_from_offline">> => false})),
    ?cfgh(P ++ [last_item_cache], rdbms,
          T(#{<<"last_item_cache">> => <<"rdbms">>})),
    ?cfgh(P ++ [plugins], [<<"flat">>, <<"dag">>],
          T(#{<<"plugins">> => [<<"flat">>, <<"dag">>]})),
    ?cfgh(P ++ [item_publisher], true,
          T(#{<<"item_publisher">> => true})),
    ?cfgh(P ++ [sync_broadcast], false,
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
    P = [modules, mod_pubsub, pep_mapping],
    RequiredOpts = #{<<"namespace">> => <<"urn:xmpp:microblog:0">>,
                     <<"node">> => <<"mb">>},
    ?cfgh(P ++ [<<"urn:xmpp:microblog:0">>], <<"mb">>,
          T([RequiredOpts])),
    [?errh(T([maps:remove(Key, RequiredOpts)])) || Key <- maps:keys(RequiredOpts)],
    [?errh(T([RequiredOpts#{Key => <<>>}])) || Key <- maps:keys(RequiredOpts)].

mod_pubsub_default_node_config(_Config) ->
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_pubsub">> =>
                                              #{<<"default_node_config">> => Opts}}} end,
    P = [modules, mod_pubsub, default_node_config],
    ?cfgh(P, [{access_model, open}],
          T(#{<<"access_model">> => <<"open">>})),
    ?cfgh(P, [{deliver_notifications, true}],
          T(#{<<"deliver_notifications">> => true})),
    ?cfgh(P, [{deliver_payloads, false}],
          T(#{<<"deliver_payloads">> => false})),
    ?cfgh(P, [{max_items, 1000}],
          T(#{<<"max_items">> => 1000})),
    ?cfgh(P, [{max_payload_size, 1000}],
          T(#{<<"max_payload_size">> => 1000})),
    ?cfgh(P, [{node_type, dag}],
          T(#{<<"node_type">> => <<"dag">>})),
    ?cfgh(P, [{notification_type, headline}],
          T(#{<<"notification_type">> => <<"headline">>})),
    ?cfgh(P, [{notify_config, true}],
          T(#{<<"notify_config">> => true})),
    ?cfgh(P, [{notify_delete, false}],
          T(#{<<"notify_delete">> => false})),
    ?cfgh(P, [{notify_retract, true}],
          T(#{<<"notify_retract">> => true})),
    ?cfgh(P, [{persist_items, false}],
          T(#{<<"persist_items">> => false})),
    ?cfgh(P, [{presence_based_delivery, true}],
          T(#{<<"presence_based_delivery">> => true})),
    ?cfgh(P, [{publish_model, open}],
          T(#{<<"publish_model">> => <<"open">>})),
    ?cfgh(P, [{purge_offline, false}],
          T(#{<<"purge_offline">> => false})),
    ?cfgh(P, [{roster_groups_allowed, [<<"friends">>]}],
          T(#{<<"roster_groups_allowed">> => [<<"friends">>]})),
    ?cfgh(P, [{send_last_published_item, on_sub_and_presence}],
          T(#{<<"send_last_published_item">> => <<"on_sub_and_presence">>})),
    ?cfgh(P, [{subscribe, true}],
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
    check_module_defaults(mod_push_service_mongoosepush),
    P = [modules, mod_push_service_mongoosepush],
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_push_service_mongoosepush">> => Opts}} end,
    ?cfgh(P ++ [pool_name], test_pool,
          T(#{<<"pool_name">> => <<"test_pool">>})),
    ?cfgh(P ++ [api_version], <<"v2">>,
          T(#{<<"api_version">> => <<"v2">>})),
    ?cfgh(P ++ [max_http_connections], 999,
          T(#{<<"max_http_connections">> => 999})),
    ?errh(T(#{<<"pool_name">> => 1})),
    ?errh(T(#{<<"api_version">> => <<"v4">>})),
    ?errh(T(#{<<"max_http_connections">> => -1})).

mod_register(_Config) ->
    check_module_defaults(mod_register),
    check_iqdisc(mod_register),
    P = [modules, mod_register],
    ?cfgh(P ++ [access], register,
          ip_access_register(<<"127.0.0.1">>)),
    ?cfgh(P ++ [ip_access], [{allow, "127.0.0.0/8"},
                             {deny, "0.0.0.0"}],
          ip_access_register(<<"0.0.0.0">>)),
    ?cfgh(P ++ [ip_access], [{allow, "127.0.0.0/8"},
                             {deny, "0.0.0.4"}],
          ip_access_register(<<"0.0.0.4">>)),
    ?cfgh(P ++ [ip_access], [{allow, "127.0.0.0/8"},
                             {deny, "::1"}],
          ip_access_register(<<"::1">>)),
    ?cfgh(P ++ [ip_access], [{allow, "127.0.0.0/8"},
                             {deny, "::1/128"}],
          ip_access_register(<<"::1/128">>)),
    ?errh(invalid_ip_access_register()),
    ?errh(invalid_ip_access_register_ipv6()),
    ?errh(ip_access_register(<<"hello">>)),
    ?errh(ip_access_register(<<"0.d">>)),
    ?cfgh(P ++ [welcome_message], {"Subject", "Body"},
          welcome_message()),
    %% List of jids
    ?cfgh(P ++ [registration_watchers], [<<"alice@bob">>, <<"ilovemongoose@help">>],
          registration_watchers([<<"alice@bob">>, <<"ilovemongoose@help">>])),
    ?errh(registration_watchers([<<"alice@bob">>, <<"jids@have@no@feelings!">>])),
    %% non-negative integer
    ?cfgh(P ++ [password_strength], 42,
          password_strength_register(42)),
    ?errh(password_strength_register(<<"42">>)),
    ?errh(password_strength_register(<<"strong">>)),
    ?errh(password_strength_register(-150)),
    ?errh(welcome_message(<<"Subject">>, 1)),
    ?errh(welcome_message(1, <<"Body">>)).

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
    check_iqdisc(mod_roster),
    check_module_defaults(mod_roster),
    P = [modules, mod_roster],
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_roster">> => Opts}} end,
    ?cfgh(P ++ [versioning],  true,
          T(#{<<"versioning">> => true})),
    ?cfgh(P ++ [store_current_id], true,
          T(#{<<"store_current_id">> => true})),
    ?cfgh(P ++ [backend], rdbms,
          T(#{<<"backend">> => <<"rdbms">>})),
    ?cfgh(P ++ [riak], config_parser_helper:default_config(P ++ [riak]),
          T(#{<<"backend">> => <<"riak">>})),
    ?cfgh(P ++ [riak, bucket_type], <<"my_type">>,
          T(#{<<"backend">> => <<"riak">>, <<"riak">> => #{<<"bucket_type">> => <<"my_type">>}})),
    ?cfgh(P ++ [riak, version_bucket_type], <<"my_versions">>,
          T(#{<<"backend">> => <<"riak">>, <<"riak">> => #{<<"version_bucket_type">> => <<"my_versions">>}})),

    ?errh(T(#{<<"versioning">> => 1})),
    ?errh(T(#{<<"store_current_id">> => 1})),
    ?errh(T(#{<<"backend">> => 1})),
    ?errh(T(#{<<"backend">> => <<"iloveyou">>})),
    ?errh(T(#{<<"riak">> => #{<<"version_bucket_type">> => 1}})),
    ?errh(T(#{<<"riak">> => #{<<"bucket_type">> => 1}})).

mod_shared_roster_ldap(_Config) ->
    check_module_defaults(mod_shared_roster_ldap),
    P = [modules, mod_shared_roster_ldap],
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_shared_roster_ldap">> => Opts}} end,
    ?cfgh(P ++ [pool_tag], my_tag,
          T(#{<<"pool_tag">> => <<"my_tag">>})),
    ?cfgh(P ++ [base], <<"string">>,
          T(#{<<"base">> => <<"string">>})),
    ?cfgh(P ++ [deref], always,
          T(#{<<"deref">> => <<"always">>})),
    %% Options: attributes
    ?cfgh(P ++ [groupattr], <<"cn">>,
          T(#{<<"groupattr">> => <<"cn">>})),
    ?cfgh(P ++ [groupdesc], <<"default">>,
          T(#{<<"groupdesc">> => <<"default">>})),
    ?cfgh(P ++ [userdesc], <<"cn">>,
          T(#{<<"userdesc">> => <<"cn">>})),
    ?cfgh(P ++ [useruid], <<"cn">>,
          T(#{<<"useruid">> => <<"cn">>})),
    ?cfgh(P ++ [memberattr], <<"memberUid">>,
          T(#{<<"memberattr">> => <<"memberUid">>})),
    ?cfgh(P ++ [memberattr_format], <<"%u">>,
          T(#{<<"memberattr_format">> => <<"%u">>})),
    ?cfgh(P ++ [memberattr_format_re], <<"">>,
          T(#{<<"memberattr_format_re">> => <<"">>})),
    %% Options: parameters
    ?cfgh(P ++ [auth_check], true,
          T(#{<<"auth_check">> => true})),
    ?cfgh(P ++ [user_cache_validity], 300,
          T(#{<<"user_cache_validity">> => 300})),
    ?cfgh(P ++ [group_cache_validity], 300,
          T(#{<<"group_cache_validity">> => 300})),
    ?cfgh(P ++ [user_cache_size], 300,
          T(#{<<"user_cache_size">> => 300})),
    ?cfgh(P ++ [group_cache_size], 300,
          T(#{<<"group_cache_size">> => 300})),
    %% Options: LDAP filters
    ?cfgh(P ++ [rfilter], <<"rfilter_test">>,
          T(#{<<"rfilter">> => <<"rfilter_test">>})),
    ?cfgh(P ++ [gfilter], <<"gfilter_test">>,
          T(#{<<"gfilter">> => <<"gfilter_test">>})),
    ?cfgh(P ++ [ufilter], <<"ufilter_test">>,
          T(#{<<"ufilter">> => <<"ufilter_test">>})),
    ?cfgh(P ++ [filter], <<"filter_test">>,
          T(#{<<"filter">> => <<"filter_test">>})),
    ?errh(T(#{<<"pool_tag">> => 1})),
    ?errh(T(#{<<"base">> => 1})),
    ?errh(T(#{<<"deref">> => 1})),
    %% Options: attributes
    ?errh(T(#{<<"groupattr">> => 1})),
    ?errh(T(#{<<"groupdesc">> => 1})),
    ?errh(T(#{<<"userdesc">> => 1})),
    ?errh(T(#{<<"useruid">> => 1})),
    ?errh(T(#{<<"memberattr">> => 1})),
    ?errh(T(#{<<"memberattr_format">> => 1})),
    ?errh(T(#{<<"memberattr_format_re">> => 1})),
    %% Options: parameters
    ?errh(T(#{<<"auth_check">> => 1})),
    ?errh(T(#{<<"user_cache_validity">> => -1})),
    ?errh(T(#{<<"group_cache_validity">> => -1})),
    ?errh(T(#{<<"user_cache_size">> => -1})),
    ?errh(T(#{<<"group_cache_size">> => -1})),
    %% Options: LDAP filters
    ?errh(T(#{<<"rfilter">> => 1})),
    ?errh(T(#{<<"gfilter">> => 1})),
    ?errh(T(#{<<"ufilter">> => 1})),
    ?errh(T(#{<<"filter">> => 1})).

mod_sic(_Config) ->
    check_module_defaults(mod_sic),
    check_iqdisc(mod_sic).

mod_smart_markers(_Config) ->
    check_module_defaults(mod_smart_markers),
    check_iqdisc(mod_smart_markers),
    P = [modules, mod_smart_markers],
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_smart_markers">> => Opts}} end,
    ?cfgh(P ++ [backend], rdbms, T(#{<<"backend">> => <<"rdbms">>})),
    ?cfgh(P ++ [keep_private], true, T(#{<<"keep_private">> => true})),
    ?cfgh(P ++ [async_writer], #{pool_size => 8}, T(#{<<"async_writer">> => #{<<"pool_size">> => 8}})),
    ?errh(T(#{<<"backend">> => <<"nodejs">>})),
    ?errh(T(#{<<"keep_private">> => 1})).

mod_stream_management(_Config) ->
    check_module_defaults(mod_stream_management),
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_stream_management">> => Opts}} end,
    P = [modules, mod_stream_management],
    ?cfgh(P ++ [buffer_max], no_buffer, T(#{<<"buffer">> => false})),
    ?cfgh(P ++ [buffer_max], 10,  T(#{<<"buffer_max">> => 10})),
    ?cfgh(P ++ [ack_freq], never, T(#{<<"ack">> => false})),
    ?cfgh(P ++ [ack_freq], 1, T(#{<<"ack_freq">> => 1})),
    ?cfgh(P ++ [resume_timeout], 999, T(#{<<"resume_timeout">> => 999})),

    ?errh(T(#{<<"buffer">> => 0})),
    ?errh(T(#{<<"buffer_max">> => -1})),
    ?errh(T(#{<<"ack">> => <<"false">>})),
    ?errh(T(#{<<"ack_freq">> => 0})),
    ?errh(T(#{<<"resume_timeout">> => true})),
    ?errh(T(#{<<"backend">> => <<"iloveyou">>})).

mod_stream_management_stale_h(_Config) ->
    P = [modules, mod_stream_management, stale_h],
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_stream_management">> => #{<<"stale_h">> => Opts}}} end,
    ?cfgh(P ++ [enabled], true, T(#{<<"enabled">> => true})),
    ?cfgh(P ++ [repeat_after], 999, T(#{<<"repeat_after">> => 999})),
    ?cfgh(P ++ [geriatric], 999, T(#{<<"geriatric">> => 999})),
    ?cfgh(P, config_parser_helper:default_config(P), T(#{})),

    ?errh(T(#{<<"enabled">> => <<"true">>})),
    ?errh(T(#{<<"repeat_after">> => -1})),
    ?errh(T(#{<<"geriatric">> => <<"one">>})).

mod_time(_Config) ->
    check_iqdisc(mod_time),
    check_module_defaults(mod_time).

mod_vcard(_Config) ->
    check_module_defaults(mod_vcard),
    check_iqdisc(mod_vcard),
    P = [modules, mod_vcard],
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_vcard">> => Opts}} end,
    ?cfgh(P ++ [iqdisc], one_queue,
          T(#{<<"iqdisc">> => #{<<"type">> => <<"one_queue">>}})),
    ?cfgh(P ++ [host], {prefix, <<"vjud.">>},
          T(#{<<"host">> => <<"vjud.@HOST@">>})),
    ?cfgh(P ++ [host], {fqdn, <<"vjud.test">>},
          T(#{<<"host">> => <<"vjud.test">>})),
    ?cfgh(P ++ [search], true,
          T(#{<<"search">> => true})),
    ?cfgh(P ++ [backend], mnesia,
          T(#{<<"backend">> => <<"mnesia">>})),
    ?cfgh(P ++ [matches], infinity,
          T(#{<<"matches">> => <<"infinity">>})),
    %% ldap
    ?cfgh(P ++ [ldap], config_parser_helper:default_config(P ++ [ldap]),
          T(#{<<"backend">> => <<"ldap">>})),
    ?cfgh(P ++ [ldap, pool_tag], my_tag,
          T(#{<<"backend">> => <<"ldap">>, <<"ldap">> => #{<<"pool_tag">> => <<"my_tag">>}})),
    ?cfgh(P ++ [ldap, base], <<"ou=Users,dc=ejd,dc=com">>,
          T(#{<<"backend">> => <<"ldap">>, <<"ldap">> => #{<<"base">> => <<"ou=Users,dc=ejd,dc=com">>}})),
    ?cfgh(P ++ [ldap, filter], <<"(&(objectClass=shadowAccount)(memberOf=Jabber Users))">>,
          T(#{<<"backend">> => <<"ldap">>, <<"ldap">> => #{<<"filter">> => <<"(&(objectClass=shadowAccount)(memberOf=Jabber Users))">>}})),
    ?cfgh(P ++ [ldap, deref], always,
          T(#{<<"backend">> => <<"ldap">>, <<"ldap">> => #{<<"deref">> => <<"always">>}})),
    ?cfgh(P ++ [ldap, search_operator], 'or',
          T(#{<<"backend">> => <<"ldap">>, <<"ldap">> => #{<<"search_operator">> => <<"or">>}})),
    ?cfgh(P ++ [ldap, binary_search_fields], [<<"PHOTO">>],
          T(#{<<"backend">> => <<"ldap">>, <<"ldap">> => #{<<"binary_search_fields">> => [<<"PHOTO">>]}})),
    %% riak
    ?cfgh(P ++ [riak, bucket_type], <<"vcard">>,
          T(#{<<"backend">> => <<"riak">>, <<"riak">> =>  #{<<"bucket_type">> => <<"vcard">>}})),
    ?cfgh(P ++ [riak, search_index], <<"vcard">>,
          T(#{<<"backend">> => <<"riak">>, <<"riak">> =>  #{<<"search_index">> => <<"vcard">>}})),

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
    P = [modules, mod_vcard, ldap, uids],
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_vcard">> => #{<<"backend">> => <<"ldap">>,
                                                  <<"ldap">> => #{<<"uids">> => Opts}}}} end,
    RequiredOpts = #{<<"attr">> => <<"name">>},
    ExpectedCfg = <<"name">>,
    ?cfgh(P, [], T([])),
    ?cfgh(P, [ExpectedCfg], T([RequiredOpts])),
    ?cfgh(P, [{<<"name">>, <<"%u@mail.example.org">>}],
          T([RequiredOpts#{<<"format">> => <<"%u@mail.example.org">>}])),
    ?cfgh(P, [{<<"name">>, <<"%u@mail.example.org">>}, ExpectedCfg],
          T([RequiredOpts#{<<"format">> => <<"%u@mail.example.org">>}, RequiredOpts])),
    [?errh(T([maps:remove(Key, RequiredOpts)])) || Key <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"attr">> := 1})),
    ?errh(T(RequiredOpts#{<<"format">> => true})).

mod_vcard_ldap_vcard_map(_Config) ->
    P = [modules, mod_vcard, ldap, vcard_map],
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_vcard">> => #{<<"backend">> => <<"ldap">>,
                                                  <<"ldap">> => #{<<"vcard_map">> => Opts}}}} end,
    RequiredOpts = #{<<"vcard_field">> => <<"FAMILY">>,
                     <<"ldap_pattern">> => <<"%s">>,
                     <<"ldap_field">> => <<"sn">>},
    ExpectedCfg = {<<"FAMILY">>, <<"%s">>, [<<"sn">>]},
    ?cfgh(P, [], T([])),
    ?cfgh(P, [ExpectedCfg], T([RequiredOpts])),
    [?errh(T([maps:remove(Key, RequiredOpts)])) || Key <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"vcard_field">> := false})),
    ?errh(T(RequiredOpts#{<<"ldap_pattern">> := false})),
    ?errh(T(RequiredOpts#{<<"ldap_field">> := -1})).

mod_vcard_ldap_search_fields(_Config) ->
    P = [modules, mod_vcard, ldap, search_fields],
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_vcard">> => #{<<"backend">> => <<"ldap">>,
                                                  <<"ldap">> => #{<<"search_fields">> => Opts}}}} end,
    RequiredOpts = #{<<"search_field">> => <<"Full Name">>,
                     <<"ldap_field">> => <<"cn">>},
    ExpectedCfg = {<<"Full Name">>, <<"cn">>},
    ?cfgh(P, [], T([])),
    ?cfgh(P, [ExpectedCfg], T([RequiredOpts])),
    [?errh(T([maps:remove(Key, RequiredOpts)])) || Key <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"search_field">> := false})),
    ?errh(T(RequiredOpts#{<<"ldap_field">> := -1})).

mod_vcard_ldap_search_reported(_Config) ->
    P = [modules, mod_vcard, ldap, search_reported],
    T = fun(Opts) -> #{<<"modules">> =>
                           #{<<"mod_vcard">> => #{<<"backend">> => <<"ldap">>,
                                                  <<"ldap">> => #{<<"search_reported">> => Opts}}}} end,
    RequiredOpts = #{<<"search_field">> => <<"Full Name">>,
                     <<"vcard_field">> => <<"FN">>},
    ExpectedCfg = {<<"Full Name">>, <<"FN">>},
    ?cfgh(P, [], T([])),
    ?cfgh(P, [ExpectedCfg], T([RequiredOpts])),
    [?errh(T([maps:remove(Key, RequiredOpts)])) || Key <- maps:keys(RequiredOpts)],
    ?errh(T(RequiredOpts#{<<"search_field">> := false})),
    ?errh(T(RequiredOpts#{<<"vcard_field">> := -1})).

mod_version(_Config) ->
    check_module_defaults(mod_version),
    check_iqdisc(mod_version),
    P = [modules, mod_version],
    T = fun(Opts) -> #{<<"modules">> => #{<<"mod_version">> => Opts}} end,
    ?cfgh(P ++ [os_info], true, T(#{<<"os_info">> => true})),
    ?errh(T(#{<<"os_info">> => 1})).

modules_without_config(_Config) ->
    ?cfgh([modules, mod_amp], #{}, #{<<"modules">> => #{<<"mod_amp">> => #{}}}),
    ?errh(#{<<"modules">> => #{<<"mod_wrong">> => #{}}}).

incorrect_module(_Config) ->
    ?errh(#{<<"modules">> => #{<<"mod_incorrect">> => #{}}}).

%% Services

service_admin_extra(_Config) ->
    P = [services, service_admin_extra],
    T = fun(Opts) -> #{<<"services">> => #{<<"service_admin_extra">> => Opts}} end,
    ?cfg(P, default_config(P), T(#{})),
    ?cfg(P ++ [submods], [node], T(#{<<"submods">> => [<<"node">>]})),
    ?err(T(#{<<"submods">> => 1})),
    ?err(T(#{<<"submods">> => [1]})),
    ?err(T(#{<<"submods">> => [<<"nodejshaha">>]})).

service_domain_db(_Config) ->
    P = [services, service_domain_db],
    T = fun(Opts) -> #{<<"services">> => #{<<"service_domain_db">> => Opts}} end,
    ?cfg(P, default_config(P), T(#{})),
    ?cfg(P ++ [event_cleaning_interval], 1000, T(#{<<"event_cleaning_interval">> => 1000})),
    ?cfg(P ++ [event_max_age], 5000, T(#{<<"event_max_age">> => 5000})),
    ?cfg(P ++ [db_pool], my_pool, T(#{<<"db_pool">> => <<"my_pool">>})),
    ?err(T(#{<<"event_cleaning_interval">> => 0})),
    ?err(T(#{<<"event_max_age">> => 0})),
    ?err(T(#{<<"db_pool">> => 10})).

service_mongoose_system_metrics(_Config) ->
    P = [services, service_mongoose_system_metrics],
    T = fun(Opts) -> #{<<"services">> => #{<<"service_mongoose_system_metrics">> => Opts}} end,
    ?cfg(P, default_config(P), T(#{})),
    ?cfg(P ++ [initial_report], 5000, T(#{<<"initial_report">> => 5000})),
    ?cfg(P ++ [periodic_report], 5000, T(#{<<"periodic_report">> => 5000})),
    ?cfg(P ++ [tracking_id], "UA-123456789", T(#{<<"tracking_id">> => <<"UA-123456789">>})),
    ?cfg(P ++ [report], true, T(#{<<"report">> => true})),
    ?err(T(#{<<"initial_report">> => <<"forever">>})),
    ?err(T(#{<<"periodic_report">> => <<"forever">>})),
    ?err(T(#{<<"initial_report">> => -1})),
    ?err(T(#{<<"periodic_report">> => -1})),
    ?err(T(#{<<"tracking_id">> => 666})),
    ?err(T(#{<<"report">> => <<"maybe">>})).

%% Helpers for module tests

check_iqdisc(Module) ->
    P = [modules, Module],
    T = fun(Opts) -> #{<<"modules">> => #{atom_to_binary(Module) => Opts}} end,
    check_iqdisc(P, T).

check_iqdisc(Module, RequiredOpts) when is_map(RequiredOpts) ->
    P = [modules, Module],
    T = fun(Opts) ->
                #{<<"modules">> => #{atom_to_binary(Module) => maps:merge(RequiredOpts, Opts)}}
        end,
    check_iqdisc(P, T);
check_iqdisc(ParentP, ParentT) when is_function(ParentT, 1) ->
    P = ParentP ++ [iqdisc],
    T = fun(Opts) -> ParentT(#{<<"iqdisc">> => Opts}) end,
    ?cfgh(P, {queues, 10}, T(#{<<"type">> => <<"queues">>, <<"workers">> => 10})),
    ?cfgh(P, parallel, T(#{<<"type">> => <<"parallel">>})),
    ?cfgh(P, one_queue, T(#{<<"type">> => <<"one_queue">>})),
    ?cfgh(P, no_queue, T(#{<<"type">> => <<"no_queue">>})),
    ?errh(T(#{<<"type">> => <<"one_queue_and_a_half">>})),
    ?errh(T(#{<<"type">> => <<"queues">>, <<"workers">> => 0})),
    ?errh(T(#{<<"type">> => <<"no_queue">>, <<"workers">> => 10})),
    ?errh(T(#{<<"workers">> => 10})).

check_module_defaults(Mod) ->
    ExpectedCfg = default_mod_config(Mod),
    ?cfgh([modules, Mod], ExpectedCfg, #{<<"modules">> => #{atom_to_binary(Mod) => #{}}}).

%% helpers for 'listen' tests

listener(Type, Opts) ->
    config([listen, Type], Opts).

http_handler_raw(Type, Opts) ->
    listen_raw(http, #{<<"port">> => 5280,
                       <<"handlers">> => #{Type =>
                                               [Opts#{<<"host">> => <<"localhost">>,
                                                      <<"path">> => <<"/api">>}]
                                          }}).

listen_raw(Type, Opts) ->
    #{<<"listen">> => #{atom_to_binary(Type) => [remove_undefined(Opts)]}}.

remove_undefined(M) ->
    maps:filter(fun(_, V) -> V =/= undefined end, M).

%% helpers for 'auth' tests

auth_ldap_raw(Opts) ->
    auth_raw(<<"ldap">>, Opts).

auth_raw(Method, Opts) ->
    #{<<"auth">> => #{Method => Opts}}.

%% helpers for 'pool' tests

pool_config(PoolIn) ->
    Pool = merge_with_default_pool_config(PoolIn),
    [{outgoing_pools, [Pool]}].

pool_raw(Type, Tag, Opts) ->
    #{<<"outgoing_pools">> => #{Type => #{Tag => Opts}}}.

pool_conn_raw(Type, Opts) ->
    #{<<"outgoing_pools">> => #{Type => #{<<"default">> => #{<<"connection">> => Opts}}}}.

rdbms_opts() ->
    #{<<"driver">> => <<"pgsql">>,
      <<"host">> => <<"localhost">>,
      <<"database">> => <<"db">>,
      <<"username">> => <<"dbuser">>,
      <<"password">> => <<"secret">>}.

%% helpers for 'access' tests

access_raw(RuleName, RuleSpec) ->
    #{<<"access">> => #{RuleName => RuleSpec}}.

%% helpers for 'host_config' tests

host_config(Config) ->
    #{<<"host_config">> => [Config#{<<"host_type">> => ?HOST}]}.

%% helpers for parsing

-spec parse(map()) -> [mongoose_config_parser_toml:config()].
parse(M0) ->
    %% As 'hosts' (or 'host_types') and 'default_server_domain' options are mandatory,
    %% this function inserts them with dummy values if they are missing.
    %% To prevent the insertion, add a 'without' option to the map, e.g. without => [<<"hosts">>]
    %% The resulting map is then passed to the TOML config parser.
    M = maybe_insert_dummy_domain(M0),
    mongoose_config_parser:get_opts(mongoose_config_parser_toml:process(M)).

maybe_insert_dummy_domain(M) ->
    DummyGenM = #{<<"default_server_domain">> => ?HOST,
                  <<"hosts">> => [?HOST]},
    {FilteredGenM, RawConfig} = case maps:take(without, M) of
                                    {Keys, Cfg} -> {maps:without(Keys, DummyGenM), Cfg};
                                    error -> {DummyGenM, M}
                                end,
    OldGenM = maps:get(<<"general">>, RawConfig, #{}),
    NewGenM = maps:merge(FilteredGenM, OldGenM),
    RawConfig#{<<"general">> => NewGenM}.

%% helpers for testing individual options

-spec host_opts([{key_prefix(), mongoose_config:value()}]) ->
          [{mongoose_config:key() | mongoose_config:key_path(), mongoose_config:value()}].
host_opts(ExpectedOptions) ->
    lists:map(fun({Key, Value}) -> {host_key(Key), Value} end, ExpectedOptions).

%% @doc Build full per-host config key for host-or-global options
-spec host_key(top_level_key_prefix()) -> mongoose_config:key();
              (key_path_prefix()) -> mongoose_config:key_path().
host_key([TopKey | Rest]) when is_atom(TopKey) ->
    [{TopKey, ?HOST} | Rest];
host_key(Key) when is_atom(Key) ->
    {Key, ?HOST}.

-spec assert_options([{mongoose_config:key() | mongoose_config:key_path(), mongoose_config:value()}],
                     [mongoose_config_parser_toml:config()]) -> any().
assert_options(ExpectedOptions, Config) ->
    lists:foreach(fun({Key, Value}) -> assert_option(Key, Value, Config) end, ExpectedOptions).

-spec assert_option(mongoose_config:key() | mongoose_config:key_path(), mongoose_config:value(),
                    [mongoose_config_parser_toml:config()]) -> any().
assert_option(KeyPath, Value, Config) when is_list(KeyPath) ->
    compare_nodes(KeyPath, Value, get_config_value(KeyPath, Config));
assert_option(Key, Value, Config) ->
    assert_option([Key], Value, Config).

-spec get_config_value(mongoose_config:key_path(), [mongoose_config_parser_toml:config()]) ->
          mongoose_config:value().
get_config_value([TopKey | Rest], Config) ->
    case lists:keyfind(TopKey, 1, Config) of
        false -> ct:fail({"option not found", TopKey, Config});
        {_, TopValue} -> lists:foldl(fun get_value/2, TopValue, Rest)
    end.

get_value(Index, List) when is_integer(Index), Index > 0, is_list(List) ->
    lists:nth(Index, List);
get_value(Key, Map) when not is_integer(Key), is_map(Map) ->
    maps:get(Key, Map).

%% helpers for file tests

test_config_file(Config, File) ->
    OptionsPath = ejabberd_helper:data(Config, File ++ ".options"),
    ExpectedOpts = config_parser_helper:options(File),

    TOMLPath = ejabberd_helper:data(Config, File ++ ".toml"),
    State = mongoose_config_parser:parse_file(TOMLPath),
    TOMLOpts = mongoose_config_parser:get_opts(State),

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

handle_config_option({K1, V1}, {K2, V2}) ->
    ?eq(K1, K2),
    compare_nodes([K1], V1, V2);
handle_config_option(Opt1, Opt2) ->
    ?eq(Opt1, Opt2).

%% Comparisons for config options that have paths (top-level or nested in maps)

-spec compare_nodes(mongoose_config:key_path(), mongoose_config:value(), mongoose_config:value()) ->
          any().
compare_nodes([listen], V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_listener/2);
compare_nodes([outgoing_pools], V1, V2) ->
    compare_unordered_lists(V1, V2, fun handle_conn_pool/2);
compare_nodes([{auth_method, _}], V1, V2) when is_atom(V1) ->
    ?eq([V1], V2);
compare_nodes([{s2s_addr, _}], {_, _, _, _} = IP1, IP2) ->
    ?eq(inet:ntoa(IP1), IP2);
compare_nodes(Node, V1, V2) when is_map(V1), is_map(V2) ->
    compare_maps(V1, V2, fun({K1, MV1}, {K2, MV2}) ->
                                 ?eq(K1, K2),
                                 compare_nodes(Node ++ [K1], MV1, MV2)
                         end);
compare_nodes(Node, V1, V2) ->
    ?eq({Node, V1}, {Node, V2}).

%% Comparisons of internal config option parts

handle_listener(V1, V2) ->
    compare_maps(V1, V2, fun handle_listener_option/2).

handle_listener_option({tls, O1}, {tls, O2}) ->
    compare_unordered_lists(O1, O2);
handle_listener_option({handlers, M1}, {handlers, M2}) ->
    compare_unordered_lists(M1, M2, fun handle_listener_module/2);
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

handle_item_with_opts({M1, O1}, {M2, O2}) ->
    ?eq(M1, M2),
    compare_unordered_lists(O1, O2).

handle_conn_pool(#{type := Type1, scope := Scope1, tag := Tag1, opts := POpts1, conn_opts := COpts1},
                 #{type := Type2, scope := Scope2, tag := Tag2, opts := POpts2, conn_opts := COpts2}) ->
    ?eq(Type1, Type2),
    ?eq(Scope1, Scope2),
    ?eq(Tag1, Tag2),
    compare_maps(POpts1, POpts2),
    compare_maps(COpts1, COpts2, fun handle_conn_opt/2).

handle_conn_opt({server, {D1, H1, DB1, U1, P1, O1}},
                {server, {D2, H2, DB2, U2, P2, O2}}) ->
    ?eq(D1, D2),
    ?eq(H1, H2),
    ?eq(DB1, DB2),
    ?eq(U1, U2),
    ?eq(P1, P2),
    compare_unordered_lists(O1, O2, fun handle_db_server_opt/2);
handle_conn_opt({http_opts, O1}, {http_opts, O2}) ->
    compare_unordered_lists(O1, O2);
handle_conn_opt(V1, V2) -> ?eq(V1, V2).

handle_db_server_opt({ssl_opts, O1}, {ssl_opts, O2}) ->
    compare_unordered_lists(O1, O2);
handle_db_server_opt(V1, V2) -> ?eq(V1, V2).

%% Generic assertions, use the 'F' handler for any custom cases
compare_unordered_lists(L1, L2) when is_list(L1), is_list(L2) ->
    compare_unordered_lists(L1, L2, fun(V1, V2) -> ?eq(V1, V2) end).

compare_unordered_lists(L1, L2, F) when is_list(L1), is_list(L2) ->
    SL1 = lists:sort(L1),
    SL2 = lists:sort(L2),
    compare_ordered_lists(SL1, SL2, F).

compare_ordered_lists([H1|T1], [H1|T2], F) ->
    compare_ordered_lists(T1, T2, F);
compare_ordered_lists([H1|T1] = L1, [H2|T2] = L2, F) ->
    try F(H1, H2)
    catch error:R:S ->
            ct:fail({"Failed to compare ordered lists", L1, L2, R, S})
    end,
    compare_ordered_lists(T1, T2, F);
compare_ordered_lists([], [], _) ->
    ok.

compare_maps(M1, M2) ->
    compare_maps(M1, M2, fun(V1, V2) -> ?eq(V1, V2) end).

compare_maps(M1, M2, F) ->
    compare_unordered_lists(maps:to_list(M1), maps:to_list(M2), F).

create_files(Config) ->
    %% The files must exist for validation to pass
    Root = small_path_helper:repo_dir(Config),
    file:make_dir("priv"),
    [ensure_copied(filename:join(Root, From), To) || {From, To} <- files_to_copy()],
    ok = file:write_file("priv/access_psk", ""),
    ok = file:write_file("priv/provision_psk", ""),
    ok = filelib:ensure_dir("www/muc/dummy").

ensure_copied(From, To) ->
    case file:copy(From, To) of
        {ok,_} ->
            ok;
        Other ->
            error(#{what => ensure_copied_failed, from => From, to => To,
                    reason => Other})
    end.

files_to_copy() ->
    [{"tools/ssl/mongooseim/privkey.pem", "priv/dc1.pem"},
     {"tools/ssl/mongooseim/cert.pem", "priv/cert.pem"},
     {"tools/ssl/mongooseim/dh_server.pem", "priv/dh.pem"},
     {"tools/ssl/mongooseim/server.pem", "priv/server.pem"},
     {"tools/ssl/ca/cacert.pem", "priv/ca.pem"}].
