%% @doc Expected parsed and processed options for TOML parser tests

-module(config_parser_helper).

-compile([export_all, nowarn_export_all]).

%% Expected configuration options for predefined configurations.
%% For each clause there is a corresponding TOML file in config_parser_SUITE_data.
options("host_types") ->
    [{all_metrics_are_global, false},
     {default_server_domain, <<"localhost">>},
     {hide_service_name, false},
     {host_types,
      [<<"this is host type">>, <<"some host type">>,
       <<"another host type">>, <<"yet another host type">>]},
     {hosts, [<<"localhost">>]},
     {language, <<"en">>},
     {listen, []},
     {loglevel, warning},
     {mongooseimctl_access_commands, []},
     {rdbms_server_type, generic},
     {registration_timeout, 600},
     {routing_modules, mongoose_router:default_routing_modules()},
     {services, #{service_domain_db => config([services, service_domain_db],
                                              #{event_cleaning_interval => 1000,
                                                event_max_age => 5000})}},
     {sm_backend, mnesia},
     {{s2s, <<"another host type">>}, default_s2s()},
     {{s2s, <<"localhost">>}, default_s2s()},
     {{s2s, <<"some host type">>}, default_s2s()},
     {{s2s, <<"this is host type">>}, default_s2s()},
     {{s2s, <<"yet another host type">>}, default_s2s()},
     {{auth, <<"another host type">>}, auth_with_methods(#{})},
     {{auth, <<"localhost">>},
      auth_with_methods(#{rdbms => #{users_number_estimate => false}})},
     {{auth, <<"some host type">>},
      auth_with_methods(#{http => #{}})},
     {{auth, <<"this is host type">>},
      auth_with_methods(#{external => #{instances => 1,
                                        program => "/usr/bin/bash"}})},
     {{auth, <<"yet another host type">>},
      auth_with_methods(#{external => #{instances => 1,
                                        program => "/usr/bin/bash"},
                          http => #{}})},
     {{modules, <<"another host type">>}, #{mod_offline => default_mod_config(mod_offline)}},
     {{modules, <<"localhost">>}, #{mod_vcard => default_mod_config(mod_vcard)}},
     {{modules, <<"some host type">>}, #{}},
     {{modules, <<"this is host type">>}, #{}},
     {{modules, <<"yet another host type">>}, #{mod_amp => #{}}},
     {{replaced_wait_timeout, <<"another host type">>}, 2000},
     {{replaced_wait_timeout, <<"localhost">>}, 2000},
     {{replaced_wait_timeout, <<"some host type">>}, 2000},
     {{replaced_wait_timeout, <<"this is host type">>}, 2000},
     {{replaced_wait_timeout, <<"yet another host type">>}, 2000}];
options("miscellaneous") ->
    [{all_metrics_are_global, false},
     {cowboy_server_name, "Apache"},
     {default_server_domain, <<"localhost">>},
     {domain_certfile, #{<<"example.com">> => "priv/cert.pem",
                         <<"example.org">> => "priv/cert.pem"}},
     {hide_service_name, true},
     {host_types, []},
     {hosts, [<<"localhost">>, <<"anonymous.localhost">>]},
     {language, <<"en">>},
     {listen,
      [config([listen, http],
              #{port => 5280,
                handlers => [{"_", "/ws-xmpp", mod_websockets,
                              [{service, maps:merge(extra_service_listener_config(),
                                                    #{password => "secret",
                                                      shaper_rule => fast,
                                                      max_fsm_queue => 1000})}]
                             }],
                transport => #{num_acceptors => 10, max_connections => 1024}
               })]},
     {loglevel, warning},
     {mongooseimctl_access_commands,
      [{local, ["join_cluster"], [{node, "mongooseim@prime"}]}]},
     {rdbms_server_type, mssql},
     {registration_timeout, 600},
     {routing_modules,
      [mongoose_router_global, mongoose_router_localdomain]},
     {services,
      #{service_mongoose_system_metrics => #{initial_report => 20000,
                                             periodic_report => 300000,
                                             report => true,
                                             tracking_id => "UA-123456789"}}},
     {{s2s, <<"anonymous.localhost">>}, default_s2s()},
     {{s2s, <<"localhost">>}, default_s2s()},
     {sm_backend, mnesia},
     {{auth, <<"anonymous.localhost">>}, custom_auth()},
     {{auth, <<"localhost">>}, custom_auth()},
     {{modules, <<"anonymous.localhost">>}, #{}},
     {{modules, <<"localhost">>}, #{}},
     {{replaced_wait_timeout, <<"anonymous.localhost">>}, 2000},
     {{replaced_wait_timeout, <<"localhost">>}, 2000},
     {{route_subdomains, <<"anonymous.localhost">>}, s2s},
     {{route_subdomains, <<"localhost">>}, s2s}];
options("modules") ->
    [{all_metrics_are_global, false},
     {default_server_domain, <<"localhost">>},
     {hide_service_name, false},
     {host_types, []},
     {hosts, [<<"localhost">>, <<"dummy_host">>]},
     {language, <<"en">>},
     {listen, []},
     {loglevel, warning},
     {mongooseimctl_access_commands, []},
     {rdbms_server_type, generic},
     {registration_timeout, 600},
     {routing_modules, mongoose_router:default_routing_modules()},
     {services, #{}},
     {{s2s, <<"dummy_host">>}, default_s2s()},
     {{s2s, <<"localhost">>}, default_s2s()},
     {sm_backend, mnesia},
     {{auth, <<"dummy_host">>}, default_auth()},
     {{auth, <<"localhost">>}, default_auth()},
     {{modules, <<"dummy_host">>}, all_modules()},
     {{modules, <<"localhost">>}, all_modules()},
     {{replaced_wait_timeout, <<"dummy_host">>}, 2000},
     {{replaced_wait_timeout, <<"localhost">>}, 2000}];
options("mongooseim-pgsql") ->
    [{all_metrics_are_global, false},
     {default_server_domain, <<"localhost">>},
     {hide_service_name, false},
     {host_types, []},
     {hosts,
      [<<"localhost">>, <<"anonymous.localhost">>, <<"localhost.bis">>]},
     {language, <<"en">>},
     {listen,
      [config([listen, c2s],
              #{port => 5222,
                access => c2s,
                shaper => c2s_shaper,
                max_stanza_size => 65536,
                zlib => 10000,
                tls => [{certfile, "priv/dc1.pem"}, {dhfile, "priv/dh.pem"}, starttls]
               }),
       config([listen, c2s],
              #{port => 5223,
                access => c2s,
                shaper => c2s_shaper,
                max_stanza_size => 65536,
                zlib => 4096
               }),
       config([listen, http],
              #{port => 5280,
                handlers => [{"_", "/http-bind", mod_bosh, []},
                             {"_", "/ws-xmpp", mod_websockets,
                              [{service, maps:merge(extra_service_listener_config(),
                                                    #{password => "secret", shaper_rule => fast})}]
                             }],
                transport => #{num_acceptors => 10, max_connections => 1024}
               }),
       config([listen, http],
              #{port => 5285,
                handlers => [{"_", "/http-bind", mod_bosh, []},
                             {"_", "/ws-xmpp", mod_websockets,
                              [{max_stanza_size, 100},
                               {ping_rate, 120000},
                               {timeout, infinity}]},
                             {"localhost", "/api", mongoose_api_admin,
                              [{auth, {<<"ala">>, <<"makotaipsa">>}}]},
                             {"localhost", "/api/contacts/{:jid}", mongoose_api_client, []}],
                transport => #{num_acceptors => 10, max_connections => 1024},
                tls => [{certfile, "priv/cert.pem"}, {keyfile, "priv/dc1.pem"}, {password, []}]
               }),
       config([listen, http],
              #{ip_address => "127.0.0.1",
                ip_tuple => {127, 0, 0, 1},
                port => 8088,
                transport => #{num_acceptors => 10, max_connections => 1024},
                handlers => [{"localhost", "/api", mongoose_api_admin, []}]
               }),
       config([listen, http],
              #{port => 8089,
                handlers => [{"_", "/api-docs/[...]", cowboy_static,
                              {priv_dir, cowboy_swagger, "swagger",
                               [{mimetypes, cow_mimetypes, all}]}},
                             {"_", "/api-docs/swagger.json", cowboy_swagger_json_handler, #{}},
                             {"_", "/api-docs", cowboy_swagger_redirect_handler, #{}},
                             {"_", "/api/sse", lasse_handler, [mongoose_client_api_sse]},
                             {"_", "/api/contacts/[:jid]", mongoose_client_api_contacts, []},
                             {"_", "/api/messages/[:with]", mongoose_client_api_messages, []},
                             {"_", "/api/rooms/[:id]", mongoose_client_api_rooms, []},
                             {"_", "/api/rooms/[:id]/config",
                              mongoose_client_api_rooms_config, []},
                             {"_", "/api/rooms/[:id]/messages",
                              mongoose_client_api_rooms_messages, []},
                             {"_", "/api/rooms/:id/users/[:user]",
                              mongoose_client_api_rooms_users, []}],
                protocol => #{compress => true},
                transport => #{num_acceptors => 10, max_connections => 1024},
                tls => [{certfile, "priv/cert.pem"}, {keyfile, "priv/dc1.pem"}, {password, []}]
               }),
       config([listen, http],
              #{ip_address => "127.0.0.1",
                ip_tuple => {127, 0, 0, 1},
                port => 5288,
                transport => #{num_acceptors => 10, max_connections => 1024},
                handlers =>
                    [{"localhost", "/api", mongoose_api,
                      [{handlers, [mongoose_api_metrics, mongoose_api_users]}]}]
               }),
       config([listen, s2s],
              #{port => 5269,
                shaper => s2s_shaper,
                max_stanza_size => 131072,
                tls => [{dhfile, "priv/dh.pem"}]
               }),
       config([listen, service],
              #{ip_address => "127.0.0.1",
                ip_tuple => {127, 0, 0, 1},
                port => 8888,
                access => all,
                shaper_rule => fast,
                password => "secret"
               }),
       config([listen, service],
              #{ip_address => "127.0.0.1",
                ip_tuple => {127, 0, 0, 1},
                port => 8666,
                access => all,
                shaper_rule => fast,
                password => "secret",
                conflict_behaviour => kick_old
               }),
       config([listen, service],
              #{ip_address => "127.0.0.1",
                ip_tuple => {127, 0, 0, 1},
                port => 8189,
                access => all,
                shaper_rule => fast,
                password => "secret",
                hidden_components => true
               })
      ]},
     {loglevel, warning},
     {max_fsm_queue, 1000},
     {mongooseimctl_access_commands, []},
     {outgoing_pools,
      lists:map(fun merge_with_default_pool_config/1,
      [#{type => rdbms, scope => global, tag => default,
         opts => #{workers => 5},
         conn_opts => #{server =>
                        {pgsql, "localhost", "ejabberd", "ejabberd", "mongooseim_secret",
                         [{ssl, required},
                          {ssl_opts,
                           [{cacertfile, "priv/ca.pem"},
                            {server_name_indication, disable},
                            {verify, verify_peer}]}]}}},
       #{type => redis, scope => <<"localhost">>, tag => global_distrib,
         opts => #{workers => 10}, conn_opts => #{}}])},
     {rdbms_server_type, generic},
     {registration_timeout, infinity},
     {routing_modules, mongoose_router:default_routing_modules()},
     {services,
      #{service_admin_extra =>
            #{submods => [node, accounts, sessions, vcard, gdpr, upload,
                          roster, last, private, stanza, stats]},
        service_mongoose_system_metrics =>
            #{initial_report => 300000,
              periodic_report => 10800000}}},
     {sm_backend, mnesia},
     {{auth, <<"anonymous.localhost">>},
      (default_auth())#{anonymous => #{allow_multiple_connections => true,
                                       protocol => both},
                        methods => [anonymous]}},
     {{auth, <<"localhost">>},
      (default_auth())#{methods => [rdbms],
                        password => #{format => scram,
                                      hash => [sha256],
                                      scram_iterations => 64},
                        rdbms => #{users_number_estimate => false}}},
     {{auth, <<"localhost.bis">>},
      (default_auth())#{methods => [rdbms],
                        password => #{format => scram,
                                      hash => [sha256],
                                      scram_iterations => 64},
                        rdbms => #{users_number_estimate => false}}},
     {{modules, <<"anonymous.localhost">>}, pgsql_modules()},
     {{modules, <<"localhost">>}, pgsql_modules()},
     {{modules, <<"localhost.bis">>}, pgsql_modules()},
     {{replaced_wait_timeout, <<"anonymous.localhost">>}, 2000},
     {{replaced_wait_timeout, <<"localhost">>}, 2000},
     {{replaced_wait_timeout, <<"localhost.bis">>}, 2000},
     {{s2s, <<"anonymous.localhost">>}, pgsql_s2s()},
     {{s2s, <<"localhost">>}, pgsql_s2s()},
     {{s2s, <<"localhost.bis">>}, pgsql_s2s()},
     {{access, global}, pgsql_access()},
     {{access, <<"anonymous.localhost">>}, pgsql_access()},
     {{access, <<"localhost">>}, pgsql_access()},
     {{access, <<"localhost.bis">>}, pgsql_access()},
     {{acl, global}, #{local => [#{match => current_domain,
                                   user_regexp => <<>>}]}},
     {{acl, <<"anonymous.localhost">>}, #{local => [#{match => current_domain,
                                                      user_regexp => <<>>}]}},
     {{acl, <<"localhost">>}, #{local => [#{match => current_domain,
                                            user_regexp => <<>>}]}},
     {{acl, <<"localhost.bis">>}, #{local => [#{match => current_domain,
                                                user_regexp => <<>>}]}},
     {shaper, #{fast => #{max_rate => 50000},
                mam_global_shaper => #{max_rate => 1000},
                mam_shaper => #{max_rate => 1},
                normal => #{max_rate => 1000}}}];
options("outgoing_pools") ->
    [{all_metrics_are_global, false},
     {default_server_domain, <<"localhost">>},
     {hide_service_name, false},
     {host_types, []},
     {hosts,
      [<<"localhost">>, <<"anonymous.localhost">>, <<"localhost.bis">>]},
     {language, <<"en">>},
     {listen, []},
     {loglevel, warning},
     {mongooseimctl_access_commands, []},
     {outgoing_pools,
      lists:map(fun merge_with_default_pool_config/1,
      [#{type => cassandra, scope => global, tag => default, opts => #{},
         conn_opts => #{keyspace => big_mongooseim,
                        servers => [{"cassandra_server1.example.com", 9042},
                                    {"cassandra_server2.example.com", 9042}]}},
       #{type => elastic, scope => global, tag => default, opts => #{},
         conn_opts => #{host => "localhost"}},
       #{type => http, scope => global, tag => mongoose_push_http,
         opts => #{workers => 50},
         conn_opts => #{server => "https://localhost:8443",
                        path_prefix => "/",
                        request_timeout => 2000}},
       #{type => ldap, scope => host, tag => default,
         opts => #{workers => 5},
         conn_opts => #{password => <<"ldap-admin-password">>,
                        rootdn => <<"cn=admin,dc=example,dc=com">>,
                        servers => ["ldap-server.example.com"]}},
       #{type => rabbit, scope => host, tag => event_pusher,
         opts => #{workers => 20},
         conn_opts => #{amqp_host => "localhost",
                        amqp_password => <<"guest">>,
                        amqp_port => 5672,
                        amqp_username => <<"guest">>,
                        confirms_enabled => true,
                        max_worker_queue_len => 100}},
       #{type => rdbms, scope => global, tag => default,
         opts => #{workers => 5},
         conn_opts => #{server =>
                        {pgsql, "localhost", "ejabberd", "ejabberd", "mongooseim_secret",
                         [{ssl, required},
                          {ssl_opts, [{cacertfile, "priv/ca.pem"},
                                      {server_name_indication, disable},
                                      {verify, verify_peer}]}]},
                        keepalive_interval => 30}},
       #{type => redis, scope => <<"localhost">>, tag => global_distrib,
         opts => #{workers => 10}, conn_opts => #{}},
       #{type => riak, scope => global, tag => default,
         opts => #{strategy => next_worker, workers => 20},
         conn_opts => #{address => "127.0.0.1",
                        credentials => {"username", "pass"},
                        port => 8087,
                        ssl_opts => [{certfile, "path/to/cert.pem"},
                                     {keyfile, "path/to/key.pem"},
                                     {verify, verify_peer}],
                        cacertfile => "path/to/cacert.pem"}}])},
     {rdbms_server_type, generic},
     {registration_timeout, 600},
     {routing_modules, mongoose_router:default_routing_modules()},
     {services, #{}},
     {{s2s, <<"anonymous.localhost">>}, default_s2s()},
     {{s2s, <<"localhost">>}, default_s2s()},
     {{s2s, <<"localhost.bis">>}, default_s2s()},
     {sm_backend, mnesia},
     {{auth, <<"anonymous.localhost">>}, default_auth()},
     {{auth, <<"localhost">>}, default_auth()},
     {{auth, <<"localhost.bis">>}, default_auth()},
     {{modules, <<"anonymous.localhost">>}, #{}},
     {{modules, <<"localhost">>}, #{}},
     {{modules, <<"localhost.bis">>}, #{}},
     {{replaced_wait_timeout, <<"anonymous.localhost">>}, 2000},
     {{replaced_wait_timeout, <<"localhost">>}, 2000},
     {{replaced_wait_timeout, <<"localhost.bis">>}, 2000}];
options("s2s_only") ->
    [{all_metrics_are_global, false},
     {default_server_domain, <<"localhost">>},
     {hide_service_name, false},
     {host_types, []},
     {hosts, [<<"localhost">>, <<"dummy_host">>]},
     {language, <<"en">>},
     {listen, []},
     {loglevel, warning},
     {mongooseimctl_access_commands, []},
     {rdbms_server_type, generic},
     {registration_timeout, 600},
     {routing_modules, mongoose_router:default_routing_modules()},
     {services, #{}},
     {sm_backend, mnesia},
     {{auth, <<"dummy_host">>}, default_auth()},
     {{auth, <<"localhost">>}, default_auth()},
     {{modules, <<"dummy_host">>}, #{}},
     {{modules, <<"localhost">>}, #{}},
     {{replaced_wait_timeout, <<"dummy_host">>}, 2000},
     {{replaced_wait_timeout, <<"localhost">>}, 2000},
     {{s2s, <<"dummy_host">>}, custom_s2s()},
     {{s2s, <<"localhost">>}, custom_s2s()}].

all_modules() ->
    #{mod_mam_rdbms_user => #{muc => true, pm => true},
      mod_mam_muc =>
          mod_config(mod_mam_muc,
                     #{archive_chat_markers => true,
                       async_writer => config([modules, mod_mam_meta, async_writer],
                                              #{enabled => false}),
                       host => {fqdn, <<"muc.example.com">>},
                       no_stanzaid_element => true}),
      mod_caps => default_mod_config(mod_caps),
      mod_mam_cache_user => (default_config([modules, mod_mam_meta, cache]))#{muc => true, pm => true},
      mod_offline =>
           mod_config(mod_offline, #{backend => riak,
                                     riak => #{bucket_type => <<"offline">>}}),
      mod_ping =>
          mod_config(mod_ping, #{ping_interval => 60000,
                                 ping_req_timeout => 32000,
                                 send_pings => true,
                                 timeout_action => none}),
      mod_event_pusher =>
          #{http => custom_mod_event_pusher_http(),
            push => custom_mod_event_pusher_push(),
            rabbit => custom_mod_event_pusher_rabbit(),
            sns => custom_mod_event_pusher_sns()},
      mod_event_pusher_http => custom_mod_event_pusher_http(),
      mod_event_pusher_push => custom_mod_event_pusher_push(),
      mod_event_pusher_rabbit => custom_mod_event_pusher_rabbit(),
      mod_event_pusher_sns => custom_mod_event_pusher_sns(),
      mod_adhoc => #{iqdisc => one_queue, report_commands_node => true},
      mod_mam_rdbms_arch_async => default_config([modules, mod_mam_meta, async_writer]),
      mod_keystore =>
          mod_config(mod_keystore, #{keys => #{access_secret => ram,
                                               access_psk => {file, "priv/access_psk"},
                                               provision_psk => {file, "priv/provision_psk"}},
                                     ram_key_size => 1000}),
      mod_global_distrib =>
          mod_config(mod_global_distrib,
                     #{global_host => <<"example.com">>,
                       local_host => <<"datacenter1.example.com">>,
                       bounce =>
                           config([modules, mod_global_distrib, bounce],
                                  #{max_retries => 3, resend_after_ms => 300}),
                       cache =>
                           config([modules, mod_global_distrib, cache],
                                  #{domain_lifetime_seconds => 60}),
                       connections =>
                           config([modules, mod_global_distrib, connections],
                                  #{endpoints => [{"172.16.0.2", 5555}],
                                    advertised_endpoints => [{"172.16.0.2", 5555}],
                                    resolved_endpoints => [{{172, 16, 0, 2}, 5555}],
                                    connections_per_endpoint => 30,
                                    tls => config([modules, mod_global_distrib, connections, tls],
                                                  #{cafile => "priv/ca.pem",
                                                    certfile => "priv/dc1.pem"})
                                   })
                      }),
      mod_pubsub =>
          mod_config(mod_pubsub, #{access => pubsub_createnode,
                                   backend => rdbms,
                                   ignore_pep_from_offline => false,
                                   last_item_cache => mnesia,
                                   max_items_node => 1000,
                                   pep_mapping => #{<<"urn:xmpp:microblog:0">> => <<"mb">>},
                                   plugins => [<<"flat">>, <<"pep">>]}),
      mod_version => mod_config(mod_version, #{os_info => true}),
      mod_auth_token => #{backend => rdbms,
                          validity_period => #{access => #{unit => minutes, value => 13},
                                               refresh => #{unit => days, value => 13}},
                          iqdisc => one_queue},
      mod_carboncopy => #{iqdisc => no_queue},
      mod_mam =>
          mod_config(mod_mam,
                     #{archive_chat_markers => true,
                       archive_groupchats => false,
                       async_writer => default_config([modules, mod_mam_meta, async_writer]),
                       full_text_search => false,
                       same_mam_id_for_peers => false,
                       no_stanzaid_element => true}),
      mod_disco =>
          mod_config(mod_disco,
                     #{extra_domains => [<<"some_domain">>, <<"another_domain">>],
                       server_info =>
                           [#{name => <<"abuse-address">>,
                              urls => [<<"admin@example.com">>]},
                            #{name => <<"friendly-spirits">>,
                              urls => [<<"spirit1@localhost">>, <<"spirit2@localhost">>],
                              modules => [mod_muc, mod_disco]}]}),
      mod_last => #{backend => mnesia, iqdisc => {queues, 10}},
      mod_shared_roster_ldap =>
          mod_config(mod_shared_roster_ldap,
                     #{base => <<"ou=Users,dc=ejd,dc=com">>,
                       filter => <<"(objectClass=inetOrgPerson)">>,
                       group_cache_validity => 1,
                       groupattr => <<"ou">>,
                       groupdesc => <<"ou">>, % would be added with incorrect value by mod_config/2
                       memberattr => <<"cn">>,
                       rfilter => <<"(objectClass=inetOrgPerson)">>,
                       user_cache_validity => 1,
                       userdesc => <<"cn">>}),
      mod_mam_mnesia_prefs => #{muc => true},
      mod_jingle_sip =>
          mod_config(mod_jingle_sip, #{listen_port => 9998,
                                       local_host => "localhost",
                                       proxy_host => "proxy.com",
                                       proxy_port => 9999,
                                       sdp_origin => "127.0.0.1"}),
      mod_mam_rdbms_prefs => #{pm => true},
      mod_extdisco =>
          #{iqdisc => one_queue,
            service => [#{host => <<"stun1">>, password => <<"password">>,
                          port => 3478, transport => <<"udp">>, type => stun,
                          username => <<"username">>},
                        #{host => <<"stun2">>, password => <<"password">>,
                          port => 2222, transport => <<"tcp">>, type => stun,
                          username => <<"username">>},
                        #{host => <<"192.168.0.1">>, type => turn}]},
      mod_csi => mod_config(mod_csi, #{buffer_max => 40}),
      mod_muc_log =>
          mod_config(mod_muc_log,
                     #{access_log => muc,
                       css_file => <<"path/to/css/file">>,
                       outdir => "www/muc",
                       top_link => {"/", "Home"}}),
      mod_http_upload =>
          mod_config(
            mod_http_upload,
            #{backend => s3,
              expiration_time => 120,
              host => {prefix, <<"upload.">>},
              s3 => config([modules, mod_http_upload, s3],
                           #{access_key_id => <<"AKIAIOSFODNN7EXAMPLE">>,
                             add_acl => true,
                             bucket_url => <<"https://s3-eu-west-1.amazonaws.com/mybucket">>,
                             region => <<"eu-west-1">>,
                             secret_access_key => <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>
                            })
             }),
      mod_muc_light =>
          mod_config(mod_muc_light, #{all_can_configure => true,
                                      all_can_invite => true,
                                      blocking => false,
                                      config_schema =>
                                          [{<<"display-lines">>, 30, display_lines, integer},
                                           {<<"roomname">>, <<"The Room">>, roomname, binary}],
                                      equal_occupants => true,
                                      host => {fqdn, <<"muclight.example.com">>},
                                      legacy_mode => true,
                                      max_occupants => 50,
                                      rooms_in_rosters => true,
                                      rooms_per_page => 5,
                                      rooms_per_user => 10}),
      mod_push_service_mongoosepush =>
          #{api_version => <<"v3">>,
            max_http_connections => 100,
            pool_name => mongoose_push_http},
      mod_roster => mod_config(mod_roster, #{store_current_id => true, versioning => true}),
      mod_inbox =>
          #{backend => rdbms,
            async_writer => #{pool_size => 2 * erlang:system_info(schedulers_online)},
            boxes => [<<"inbox">>, <<"archive">>, <<"bin">>],
            bin_ttl => 30,
            bin_clean_after => timer:hours(1),
            iqdisc => no_queue,
            aff_changes => true,
            groupchat => [muclight],
            remove_on_kicked => true,
            reset_markers => [<<"displayed">>]},
      mod_mam_meta =>
          mod_config(mod_mam_meta,
                     #{archive_chat_markers => true,
                       muc =>
                           #{async_writer => config([modules, mod_mam_meta, async_writer],
                                                    #{enabled => false}),
                             db_message_format => mam_message_xml,
                             host => {fqdn, <<"muc.example.com">>},
                             user_prefs_store => mnesia},
                       no_stanzaid_element => true,
                       pm =>
                           #{archive_groupchats => false,
                             full_text_search => false,
                             same_mam_id_for_peers => false,
                             user_prefs_store => rdbms}}),
      mod_register => mod_config(mod_register, #{access => all,
                                                 password_strength => 32,
                                                 registration_watchers => [<<"JID1">>, <<"JID2">>],
                                                 welcome_message => {"Subject", "Body"}}),
      mod_mam_rdbms_arch =>
          mod_config(mod_mam_rdbms_arch, #{no_writer => true}),
      mod_bosh =>
          #{backend => mnesia, inactivity => 20, max_wait => infinity,
            server_acks => true, max_pause => 120},
      mod_muc =>
          mod_config(mod_muc,
                     #{access => muc,
                       access_create => muc_create,
                       default_room => (default_room_opts())
                       #{affiliations =>
                         [{{<<"alice">>, <<"localhost">>, <<"resource1">>}, member},
                          {{<<"bob">>, <<"localhost">>, <<"resource2">>}, owner}],
                         password_protected => true},
                       host => {fqdn, <<"muc.example.com">>},
                       http_auth_pool => my_auth_pool}),
      mod_vcard =>
          mod_config(mod_vcard,
                     #{host => {fqdn, <<"directory.example.com">>},
                       backend => ldap,
                       ldap => #{search_fields =>
                                 [{<<"User">>, <<"%u">>}, {<<"Full Name">>, <<"displayName">>}],
                                 search_reported =>
                                 [{<<"Full Name">>, <<"FN">>}, {<<"Given Name">>, <<"FIRST">>}],
                                 vcard_map =>
                                 [{<<"FAMILY">>, <<"%s">>, [<<"sn">>]},
                                  {<<"FN">>, <<"%s">>, [<<"displayName">>]}],
                                 pool_tag => default, deref => never, filter => <<"">>,
                                 uids => [{<<"uid">>, <<"%u">>}], search_operator => 'and',
                                 binary_search_fields => []},
                       matches => 1,
                       search => true}),
      mod_mam_muc_rdbms_arch =>
          mod_config(mod_mam_muc_rdbms_arch, #{db_message_format => mam_message_xml}),
      mod_stream_management =>
          mod_config(mod_stream_management, #{ack_freq => 2,
                                              buffer_max => 30,
                                              resume_timeout => 600,
                                              stale_h => #{enabled => true,
                                                           geriatric => 3600,
                                                           repeat_after => 1800}})
    }.

custom_mod_event_pusher_http() ->
    #{handlers =>
          [#{callback_module => mod_event_pusher_http_defaults,
             path => <<"notifications">>,
             pool_name => http_pool}]}.

custom_mod_event_pusher_push() ->
    #{iqdisc => one_queue,
      backend => mnesia,
      plugin_module => mod_event_pusher_push_plugin_defaults,
      virtual_pubsub_hosts =>
          [{fqdn,<<"host1">>},{fqdn,<<"host2">>}],
      wpool => #{strategy => available_worker,
                 workers => 200,
                 call_timeout => 5000}}.

custom_mod_event_pusher_rabbit() ->
    #{chat_msg_exchange => #{name => <<"chat_msg">>,
                             recv_topic => <<"chat_msg_recv">>,
                             sent_topic => <<"chat_msg_sent">>,
                             type => <<"topic">>},
      groupchat_msg_exchange => #{name => <<"groupchat_msg">>,
                                  recv_topic => <<"groupchat_msg_recv">>,
                                  sent_topic => <<"groupchat_msg_sent">>,
                                  type => <<"topic">>},
      presence_exchange => #{name => <<"presence">>,
                             type => <<"topic">>}}.

custom_mod_event_pusher_sns() ->
    #{access_key_id => "AKIAIOSFODNN7EXAMPLE",
      account_id => "123456789012",
      muc_messages_topic => "user_messagegroup_sent",
      plugin_module => mod_event_pusher_sns_defaults,
      pm_messages_topic => "user_message_sent",
      pool_size => 100,
      presence_updates_topic => "user_presence_updated",
      publish_retry_count => 2,
      publish_retry_time_ms => 50,
      region => "eu-west-1",
      secret_access_key => "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY",
      sns_host => "eu-west-1.amazonaws.com"}.

pgsql_modules() ->
    #{mod_adhoc => default_mod_config(mod_adhoc),
      mod_amp => #{}, mod_blocking => default_mod_config(mod_blocking),
      mod_bosh => default_mod_config(mod_bosh),
      mod_carboncopy => default_mod_config(mod_carboncopy), mod_commands => #{},
      mod_disco => mod_config(mod_disco, #{users_can_see_hidden_services => false}),
      mod_last => mod_config(mod_last, #{backend => rdbms}),
      mod_muc_commands => #{}, mod_muc_light_commands => #{},
      mod_offline => mod_config(mod_offline, #{backend => rdbms}),
      mod_privacy => mod_config(mod_privacy, #{backend => rdbms}),
      mod_private => default_mod_config(mod_private),
      mod_register =>
          mod_config(mod_register, #{access => register,
                                     ip_access => [{allow, "127.0.0.0/8"}, {deny, "0.0.0.0/0"}],
                                     welcome_message => {"Hello", "I am MongooseIM"}}),
      mod_roster => mod_config(mod_roster, #{backend => rdbms}),
      mod_sic => default_mod_config(mod_sic),
      mod_stream_management => default_mod_config(mod_stream_management),
      mod_vcard => mod_config(mod_vcard, #{backend => rdbms, host => {prefix, <<"vjud.">>}})}.

auth_with_methods(Methods) ->
    maps:merge(default_auth(), Methods#{methods => lists:sort(maps:keys(Methods))}).

custom_auth() ->
    maps:merge(default_auth(), extra_auth()).

extra_auth() ->
    #{anonymous => #{allow_multiple_connections => true,
                     protocol => sasl_anon},
      http => #{basic_auth => "admin:admin"},
      external => #{instances => 1,
                    program => "/usr/bin/authenticator"},
      jwt => #{algorithm => <<"RS256">>,
               secret => {value, "secret123"},
               username_key => user},
      ldap => #{base => <<"ou=Users,dc=esl,dc=com">>,
                bind_pool_tag => bind,
                deref => never,
                dn_filter => {<<"(&(name=%s)(owner=%D)(user=%u@%d))">>, [<<"sn">>]},
                filter => <<"(&(objectClass=shadowAccount)(memberOf=Jabber Users))">>,
                local_filter => {equal, {"accountStatus", ["enabled"]}},
                pool_tag => default,
                uids => [<<"uid">>, {<<"uid2">>, <<"%u">>}]},
      methods => [anonymous, external, http, jwt, ldap, rdbms, riak],
      rdbms => #{users_number_estimate => true},
      riak => #{bucket_type => <<"user_bucket">>}}.

default_auth() ->
    #{methods => [],
      password => #{format => scram,
                    scram_iterations => 10000},
      sasl_external => [standard],
      sasl_mechanisms => cyrsasl:default_modules()}.

pgsql_s2s() ->
    Outgoing = (default_s2s_outgoing())#{port => 5299},
    (default_s2s())#{address => #{<<"fed1">> => #{ip_address => "127.0.0.1"}},
                     certfile => "priv/server.pem",
                     outgoing => Outgoing,
                     use_starttls => optional}.

custom_s2s() ->
    #{address =>
          #{<<"fed1">> => #{ip_address => "127.0.0.1"},
            <<"fed2">> => #{ip_address => "127.0.0.1", port => 8765}},
      certfile => "priv/server.pem",
      ciphers => ejabberd_tls:default_ciphers(),
      default_policy => allow,
      dns => #{retries => 1, timeout => 30},
      host_policy => #{<<"fed1">> => allow, <<"reg1">> => deny},
      max_retry_delay => 30,
      outgoing => #{connection_timeout => 4000, ip_versions => [6, 4], port => 5299},
      shared => <<"shared secret">>,
      use_starttls => optional}.

default_s2s() ->
    #{ciphers => ejabberd_tls:default_ciphers(),
      default_policy => allow,
      dns => #{retries => 2, timeout => 10},
      max_retry_delay => 300,
      outgoing => default_s2s_outgoing(),
      use_starttls => false}.

default_s2s_outgoing() ->
     #{connection_timeout => 10000,
       ip_versions => [4, 6],
       port => 5269}.

pgsql_access() ->
    #{c2s => [#{acl => blocked, value => deny},
              #{acl => all, value => allow}],
      c2s_shaper => [#{acl => admin, value => none},
                     #{acl => all, value => normal}],
      local => [#{acl => local, value => allow}],
      mam_get_prefs => [#{acl => all, value => default}],
      mam_get_prefs_global_shaper => [#{acl => all, value => mam_global_shaper}],
      mam_get_prefs_shaper => [#{acl => all, value => mam_shaper}],
      mam_lookup_messages => [#{acl => all, value => default}],
      mam_lookup_messages_global_shaper => [#{acl => all, value => mam_global_shaper}],
      mam_lookup_messages_shaper => [#{acl => all, value => mam_shaper}],
      mam_set_prefs => [#{acl => all, value => default}],
      mam_set_prefs_global_shaper => [#{acl => all, value => mam_global_shaper}],
      mam_set_prefs_shaper => [#{acl => all, value => mam_shaper}],
      max_user_offline_messages => [#{acl => admin, value => 5000},
                                    #{acl => all, value => 100}],
      max_user_sessions => [#{acl => all, value => 10}],
      muc => [#{acl => all, value => allow}],
      muc_admin => [#{acl => admin, value => allow}],
      muc_create => [#{acl => local, value => allow}],
      register => [#{acl => all, value => allow}],
      s2s_shaper => [#{acl => all, value => fast}]}.

merge_with_default_pool_config(PoolIn = #{type := Type}) ->
    DefaultConfig = #{opts := DefaultOpts, conn_opts := DefaultConnOpts} = default_pool_config(Type),
    WpoolOptsWithDefaults = maps:merge(DefaultOpts, maps:get(opts, PoolIn, #{})),
    ConnOptsWithDefaults = maps:merge(DefaultConnOpts, maps:get(conn_opts, PoolIn, #{})),
    maps:merge(DefaultConfig, PoolIn#{opts => WpoolOptsWithDefaults,
                                      conn_opts => ConnOptsWithDefaults}).

default_pool_config(Type) ->
    #{scope => global,
      tag => default,
      opts => default_pool_wpool_opts(Type),
      conn_opts => default_pool_conn_opts(Type)}.

default_pool_wpool_opts(cassandra) ->
    #{workers => 20,
      strategy => best_worker,
      call_timeout => 5000};
default_pool_wpool_opts(rdbms) ->
    #{workers => 10,
      strategy => best_worker,
      call_timeout => 60000};
default_pool_wpool_opts(_) ->
    default_wpool_opts().

default_wpool_opts() ->
     #{workers => 10,
      strategy => best_worker,
      call_timeout => 5000}.

default_pool_conn_opts(cassandra) ->
    #{servers => [{"localhost", 9042}],
      keyspace => mongooseim};
default_pool_conn_opts(elastic) ->
    #{host => "localhost",
      port => 9200};
default_pool_conn_opts(http) ->
    #{path_prefix => "/",
      request_timeout => 2000};
default_pool_conn_opts(ldap) ->
    #{rootdn => <<"">>,
      password => <<"">>,
      encrypt => none,
      servers => ["localhost"],
      connect_interval => 10000};
default_pool_conn_opts(rabbit) ->
    #{amqp_port => 5672,
      confirms_enabled => false,
      max_worker_queue_len => 1000};
default_pool_conn_opts(redis) ->
    #{host => "127.0.0.1",
      port => 6379,
      database => 0,
      password => ""};
default_pool_conn_opts(_Type) ->
    #{}.

mod_config(Module, ExtraOpts) ->
    maps:merge(default_mod_config(Module), ExtraOpts).

default_mod_config(mod_adhoc) ->
    #{iqdisc => one_queue, report_commands_node => false};
default_mod_config(mod_auth_token) ->
    #{backend => rdbms, iqdisc => no_queue,
      validity_period => #{access => #{unit => hours, value => 1},
                           refresh => #{unit => days, value => 25}}};
default_mod_config(mod_blocking) ->
    #{backend => mnesia};
default_mod_config(mod_bosh) ->
    #{backend => mnesia, inactivity => 30, max_wait => infinity,
      server_acks => false, max_pause => 120};
default_mod_config(mod_cache_users) ->
    #{strategy => fifo, time_to_live => 480, number_of_segments => 3};
default_mod_config(mod_caps) ->
    #{cache_size => 1000,
      cache_life_time => timer:hours(24) div 1000};
default_mod_config(mod_csi) ->
    #{buffer_max => 20};
default_mod_config(mod_carboncopy) ->
    #{iqdisc => no_queue};
default_mod_config(mod_disco) ->
    #{extra_domains => [], server_info => [],
      users_can_see_hidden_services => true, iqdisc => one_queue};
default_mod_config(mod_extdisco) ->
    #{iqdisc => no_queue, service => []};
default_mod_config(mod_global_distrib) ->
    #{message_ttl => 4,
      hosts_refresh_interval => 3000,
      connections => default_config([modules, mod_global_distrib, connections]),
      redis => default_config([modules, mod_global_distrib, redis]),
      cache => default_config([modules, mod_global_distrib, cache]),
      bounce => default_config([modules, mod_global_distrib, bounce])};
default_mod_config(mod_http_upload) ->
    #{iqdisc => one_queue,
      host => {prefix, <<"upload.">>},
      backend => s3,
      expiration_time => 60,
      token_bytes => 32,
      max_file_size => 1024 * 1024 * 10,
      s3 => default_config([modules, mod_http_upload, s3])};
default_mod_config(mod_inbox) ->
    #{backend => rdbms,
      async_writer => #{pool_size => 2 * erlang:system_info(schedulers_online)},
      boxes => [<<"inbox">>, <<"archive">>, <<"bin">>],
      bin_ttl => 30,
      bin_clean_after => timer:hours(1),
      groupchat => [muclight],
      aff_changes => true,
      remove_on_kicked => true,
      reset_markers => [<<"displayed">>],
      iqdisc => no_queue};
default_mod_config(mod_jingle_sip) ->
    #{proxy_host => "localhost", proxy_port => 5060, listen_port => 5600, local_host => "localhost",
      sdp_origin => "127.0.0.1", transport => "udp", username_to_phone => []};
default_mod_config(mod_keystore) ->
    #{ram_key_size => 2048, keys => #{}};
default_mod_config(mod_last) ->
    #{iqdisc => one_queue, backend => mnesia};
default_mod_config(mod_mam) ->
    maps:merge(common_mam_config(), default_config([modules, mod_mam_meta, pm]));
default_mod_config(mod_mam_meta) ->
    (common_mam_config())#{backend => rdbms, cache_users => true,
                           cache => default_config([modules, mod_mam_meta, cache])};
default_mod_config(mod_mam_muc) ->
    maps:merge(common_mam_config(), default_config([modules, mod_mam_meta, muc]));
default_mod_config(mod_mam_rdbms_arch) ->
    #{no_writer => false,
      db_message_format => mam_message_compressed_eterm,
      db_jid_format => mam_jid_mini};
default_mod_config(mod_mam_muc_rdbms_arch) ->
    #{no_writer => false,
      db_message_format => mam_message_compressed_eterm,
      db_jid_format => mam_jid_rfc};
default_mod_config(mod_muc) ->
    #{backend => mnesia,
      host => {prefix,<<"conference.">>},
      access => all,
      access_create => all,
      access_admin => none,
      access_persistent => all,
      history_size => 20,
      room_shaper => none,
      max_room_id => infinity,
      max_room_name => infinity,
      max_room_desc => infinity,
      min_message_interval => 0,
      min_presence_interval => 0,
      max_users => 200,
      max_users_admin_threshold => 5,
      user_message_shaper => none,
      user_presence_shaper => none,
      max_user_conferences => 10,
      http_auth_pool => none,
      load_permanent_rooms_at_startup => false,
      hibernate_timeout => timer:seconds(90),
      hibernated_room_check_interval => infinity,
      hibernated_room_timeout => infinity,
      default_room => default_room_opts()};
default_mod_config(mod_muc_light) ->
    #{backend => mnesia,
      host => {prefix, <<"muclight.">>},
      equal_occupants => false,
      legacy_mode => false,
      rooms_per_user => infinity,
      blocking => true,
      all_can_configure => false,
      all_can_invite => false,
      max_occupants => infinity,
      rooms_per_page => 10,
      rooms_in_rosters => false,
      config_schema => [{<<"roomname">>, <<"Untitled">>, roomname, binary},
                        {<<"subject">>, <<>>, subject, binary}]};
default_mod_config(mod_muc_log) ->
    #{outdir => "www/muc",
      access_log => muc_admin,
      dirtype => subdirs,
      dirname => room_jid,
      file_format => html,
      css_file => false,
      timezone => local,
      top_link => {"/", "Home"},
      spam_prevention => true};
default_mod_config(mod_ping) ->
    #{send_pings => false,
      ping_interval => 60 * 1000,
      timeout_action => none,
      ping_req_timeout => 32 * 1000,
      iqdisc => no_queue};
default_mod_config(mod_offline) ->
    #{backend => mnesia,
      access_max_user_messages => max_user_offline_messages,
      store_groupchat_messages => false};
default_mod_config(mod_offline_chatmarkers) ->
    #{backend => rdbms,
      store_groupchat_messages => false};
default_mod_config(mod_privacy) ->
    #{backend => mnesia};
default_mod_config(mod_private) ->
    #{iqdisc => one_queue, backend => rdbms};
default_mod_config(mod_pubsub) ->
    #{iqdisc => one_queue, host => {prefix, <<"pubsub.">>}, backend => mnesia, access => all,
      max_items_node => 10, nodetree => nodetree_tree, ignore_pep_from_offline => true,
      last_item_cache => false, plugins => [<<"flat">>], pep_mapping => #{},
      default_node_config => [], item_publisher => false, sync_broadcast => false};
default_mod_config(mod_push_service_mongoosepush) ->
    #{pool_name => undefined, api_version => <<"v3">>, max_http_connections => 100};
default_mod_config(mod_register) ->
    #{iqdisc => one_queue, access => all, registration_watchers => [],
      password_strength => 0, ip_access => []};
default_mod_config(mod_roster) ->
    #{iqdisc => one_queue, versioning => false, store_current_id => false, backend => mnesia};
default_mod_config(mod_shared_roster_ldap) ->
    #{pool_tag => default, deref => never, filter => <<"">>,
      groupattr => <<"cn">>, groupdesc => <<"cn">>, userdesc => <<"cn">>, useruid => <<"cn">>,
      memberattr => <<"memberUid">>, memberattr_format => <<"%u">>, memberattr_format_re => <<"">>,
      auth_check => true, user_cache_validity => 300, group_cache_validity => 300, user_cache_size => 1000,
      group_cache_size => 1000, rfilter => <<"">>, gfilter => <<"">>, ufilter => <<"">>};
default_mod_config(mod_sic) ->
    #{iqdisc => one_queue};
default_mod_config(mod_smart_markers) ->
    #{keep_private => false,
      async_writer => #{pool_size => 2 * erlang:system_info(schedulers_online)},
      backend => rdbms, iqdisc => no_queue};
default_mod_config(mod_stream_management) ->
    #{backend => mnesia,
      buffer => true,
      buffer_max => 100,
      ack => true,
      ack_freq => 1,
      resume_timeout => 600,
      stale_h => default_config([modules, mod_stream_management, stale_h])};
default_mod_config(mod_time) ->
    #{iqdisc => one_queue};
default_mod_config(mod_vcard) ->
    #{iqdisc => parallel,
      host => {prefix, <<"vjud.">>},
      search => true,
      backend => mnesia,
      matches => 30};
default_mod_config(mod_version) ->
    #{iqdisc => no_queue, os_info => false};
default_mod_config(_) ->
    #{}.

default_room_opts() ->
    #{title => <<>>,
      description => <<>>,
      allow_change_subj => true,
      allow_query_users => true,
      allow_private_messages => true,
      allow_visitor_status => true,
      allow_visitor_nickchange => true,
      public => true,
      public_list => true,
      persistent => false,
      moderated => true,
      members_by_default => true,
      members_only => false,
      allow_user_invites => false,
      allow_multiple_sessions => false,
      password_protected => false,
      password => <<>>,
      anonymous => true,
      max_users => 200,
      logging => false,
      maygetmemberlist => [],
      affiliations => [],
      subject => <<>>,
      subject_author => <<>>}.

common_xmpp_listener_config() ->
    (common_listener_config())#{backlog => 100,
                                proxy_protocol => false,
                                hibernate_after => 0,
                                max_stanza_size => infinity,
                                num_acceptors => 100}.

common_listener_config() ->
    #{ip_address => "0",
      ip_tuple => {0, 0, 0, 0},
      ip_version => 4,
      proto => tcp}.

extra_service_listener_config() ->
    #{access => all,
      shaper_rule => none,
      check_from => true,
      hidden_components => false,
      conflict_behaviour => disconnect}.

default_config([listen, http]) ->
    (common_listener_config())#{module => ejabberd_cowboy,
                                transport => default_config([listen, http, transport]),
                                protocol => default_config([listen, http, protocol]),
                                handlers => []};
default_config([listen, http, transport]) ->
    #{num_acceptors => 100,
      max_connections => 1024};
default_config([listen, http, protocol]) ->
    #{compress => false};
default_config([listen, c2s]) ->
    (common_xmpp_listener_config())#{module => ejabberd_c2s,
                                     access => all,
                                     shaper => none};
default_config([listen, s2s]) ->
    (common_xmpp_listener_config())#{module => ejabberd_s2s_in,
                                     shaper => none};
default_config([listen, service]) ->
    Extra = maps:merge(common_xmpp_listener_config(), extra_service_listener_config()),
    Extra#{module => ejabberd_service};
default_config([modules, M]) ->
    default_mod_config(M);
default_config([modules, mod_event_pusher, http]) ->
    #{handlers => []};
default_config([modules, mod_event_pusher, push]) ->
    #{iqdisc => one_queue,
      backend => mnesia,
      wpool => default_config([modules, mod_event_pusher, push, wpool]),
      plugin_module => mod_event_pusher_push_plugin_defaults,
      virtual_pubsub_hosts => []};
default_config([modules, mod_event_pusher, push, wpool]) ->
    (default_wpool_opts())#{strategy := available_worker};
default_config([modules, mod_event_pusher, rabbit] = P) ->
    #{presence_exchange => default_config(P ++ [presence_exchange]),
      chat_msg_exchange => default_config(P ++ [chat_msg_exchange]),
      groupchat_msg_exchange => default_config(P ++ [groupchat_msg_exchange])};
default_config([modules, mod_event_pusher, rabbit, presence_exchange]) ->
    #{name => <<"presence">>, type => <<"topic">>};
default_config([modules, mod_event_pusher, rabbit, chat_msg_exchange]) ->
    #{name => <<"chat_msg">>, type => <<"topic">>,
      sent_topic => <<"chat_msg_sent">>, recv_topic => <<"chat_msg_recv">>};
default_config([modules, mod_event_pusher, rabbit, groupchat_msg_exchange]) ->
    #{name => <<"groupchat_msg">>, type => <<"topic">>,
      sent_topic => <<"groupchat_msg_sent">>, recv_topic => <<"groupchat_msg_recv">>};
default_config([modules, mod_event_pusher, sns]) ->
    #{plugin_module => mod_event_pusher_sns_defaults,
      pool_size => 100,
      publish_retry_count => 2,
      publish_retry_time_ms => 50};
default_config([modules, mod_global_distrib, connections]) ->
    #{connections_per_endpoint => 1,
      endpoint_refresh_interval => 60,
      endpoint_refresh_interval_when_empty => 3,
      disabled_gc_interval => 60};
default_config([modules, mod_global_distrib, connections, tls]) ->
    #{ciphers => "TLSv1.2:TLSv1.3"};
default_config([modules, mod_global_distrib, redis]) ->
    #{pool => global_distrib,
      expire_after => 120,
      refresh_after => 60};
default_config([modules, mod_global_distrib, cache]) ->
    #{cache_missed => true,
      domain_lifetime_seconds => 600,
      jid_lifetime_seconds => 5,
      max_jids => 10000};
default_config([modules, mod_global_distrib, bounce]) ->
    #{enabled => true,
      resend_after_ms => 200,
      max_retries => 4};
default_config([modules, mod_http_upload, s3]) ->
    #{add_acl => false};
default_config([modules, mod_privacy, riak]) ->
    #{defaults_bucket_type => <<"privacy_defaults">>,
      names_bucket_type => <<"privacy_lists_names">>,
      bucket_type => <<"privacy_lists">>};
default_config([modules, mod_mam_meta, pm]) ->
    #{archive_groupchats => false, same_mam_id_for_peers => false};
default_config([modules, mod_mam_meta, muc]) ->
    #{host => {prefix, <<"conference.">>}};
default_config([modules, mod_mam_meta, cache]) ->
    #{module => internal, strategy => fifo,
      time_to_live => 480, number_of_segments => 3};
default_config([modules, mod_mam_meta, async_writer]) ->
    #{batch_size => 30, enabled => true, flush_interval => 2000,
      pool_size => 4 * erlang:system_info(schedulers_online)};
default_config([modules, mod_mam_meta, riak]) ->
    #{bucket_type => <<"mam_yz">>, search_index => <<"mam">>};
default_config([modules, mod_muc_light, cache_affs]) ->
    #{module => internal, strategy => fifo,
      time_to_live => 2, number_of_segments => 3};
default_config([modules, mod_roster, riak]) ->
    #{bucket_type => <<"rosters">>, version_bucket_type => <<"roster_versions">>};
default_config([modules, mod_stream_management, stale_h]) ->
    #{enabled => false,
      repeat_after => 1800,
      geriatric => 3600};
default_config([modules, mod_vcard, ldap]) -> % included when backend => ldap
    #{pool_tag => default,
      deref => never,
      filter => <<"">>,
      uids => [{<<"uid">>, <<"%u">>}],
      vcard_map => [{<<"NICKNAME">>, <<"%u">>, []},
                    {<<"FN">>, <<"%s">>, [<<"displayName">>]},
                    {<<"FAMILY">>, <<"%s">>, [<<"sn">>]},
                    {<<"GIVEN">>, <<"%s">>, [<<"givenName">>]},
                    {<<"MIDDLE">>, <<"%s">>, [<<"initials">>]},
                    {<<"ORGNAME">>, <<"%s">>, [<<"o">>]},
                    {<<"ORGUNIT">>, <<"%s">>, [<<"ou">>]},
                    {<<"CTRY">>, <<"%s">>, [<<"c">>]},
                    {<<"LOCALITY">>, <<"%s">>, [<<"l">>]},
                    {<<"STREET">>, <<"%s">>, [<<"street">>]},
                    {<<"REGION">>, <<"%s">>, [<<"st">>]},
                    {<<"PCODE">>, <<"%s">>, [<<"postalCode">>]},
                    {<<"TITLE">>, <<"%s">>, [<<"title">>]},
                    {<<"URL">>, <<"%s">>, [<<"labeleduri">>]},
                    {<<"DESC">>, <<"%s">>, [<<"description">>]},
                    {<<"TEL">>, <<"%s">>, [<<"telephoneNumber">>]},
                    {<<"EMAIL">>, <<"%s">>, [<<"mail">>]},
                    {<<"BDAY">>, <<"%s">>, [<<"birthDay">>]},
                    {<<"ROLE">>, <<"%s">>, [<<"employeeType">>]},
                    {<<"PHOTO">>, <<"%s">>, [<<"jpegPhoto">>]}],
      search_fields => [{<<"User">>, <<"%u">>},
                        {<<"Full Name">>, <<"displayName">>},
                        {<<"Given Name">>, <<"givenName">>},
                        {<<"Middle Name">>, <<"initials">>},
                        {<<"Family Name">>, <<"sn">>},
                        {<<"Nickname">>, <<"%u">>},
                        {<<"Birthday">>, <<"birthDay">>},
                        {<<"Country">>, <<"c">>}, {<<"City">>, <<"l">>},
                        {<<"Email">>, <<"mail">>},
                        {<<"Organization Name">>, <<"o">>},
                        {<<"Organization Unit">>, <<"ou">>}],
      search_reported => [{<<"Full Name">>, <<"FN">>},
                          {<<"Given Name">>, <<"FIRST">>},
                          {<<"Middle Name">>, <<"MIDDLE">>},
                          {<<"Family Name">>, <<"LAST">>},
                          {<<"Nickname">>, <<"NICK">>},
                          {<<"Birthday">>, <<"BDAY">>},
                          {<<"Country">>, <<"CTRY">>},
                          {<<"City">>, <<"LOCALITY">>},
                          {<<"Email">>, <<"EMAIL">>},
                          {<<"Organization Name">>, <<"ORGNAME">>},
                          {<<"Organization Unit">>, <<"ORGUNIT">>}],
      search_operator => 'and',
      binary_search_fields => []};
default_config([services, service_admin_extra]) ->
    #{submods => [node, accounts, sessions, vcard, roster, last,
                  private, stanza, stats, gdpr, upload, domain]};
default_config([services, service_domain_db]) ->
    #{event_cleaning_interval => 1800,
      event_max_age => 7200,
      db_pool => global};
default_config([services, service_mongoose_system_metrics]) ->
    #{initial_report => timer:minutes(5),
      periodic_report => timer:hours(3)}.

common_mam_config() ->
    #{no_stanzaid_element => false,
      is_archivable_message => mod_mam_utils,
      send_message => mod_mam_utils,
      archive_chat_markers => false,
      message_retraction => true,
      full_text_search => true,
      default_result_limit => 50,
      max_result_limit => 50,
      async_writer => default_config([modules, mod_mam_meta, async_writer])}.

mod_event_pusher_http_handler() ->
    #{pool_name => http_pool,
      path => <<>>,
      callback_module => mod_event_pusher_http_defaults}.

config(Path, Opts) ->
    maps:merge(default_config(Path), Opts).
