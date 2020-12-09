-module(mongoose_config_spec).

-compile(export_all).

-include("ejabberd_config.hrl").

-type config_node() :: config_section() | config_list() | config_option().
-type config_section() :: #section{}.
-type config_list() :: #list{}.
-type config_option() :: #option{}.

-export_type([config_node/0, config_section/0, config_list/0, config_option/0]).

handler(Path) ->
    handler(Path, root()).

handler([Node], #section{items = Items}) when is_binary(Node) ->
    case maps:is_key(Node, Items) of
        true -> maps:get(Node, Items);
        false -> maps:get(default, Items)
    end;
handler([item], #list{items = Item}) ->
    Item;
handler([Node|Rest], #section{items = Items}) when is_binary(Node) ->
    Item = case maps:is_key(Node, Items) of
               true -> maps:get(Node, Items);
               false -> maps:get(default, Items)
           end,
    handler(Rest, Item);
handler([item|Rest], #list{items = Items}) ->
    handler(Rest, Items).

root() ->
    #section{
       items = #{<<"general">> => general(),
                 <<"listen">> => listen(),
                 <<"auth">> => auth(),
                 <<"outgoing_pools">> => outgoing_pools(),
                 <<"services">> => services(),
                 <<"modules">> => modules(),
                 <<"shaper">> => shaper(),
                 <<"acl">> => acl(),
                 <<"access">> => access(),
                 <<"s2s">> => s2s()
                },
       required = [<<"general">>],
       format = none
      }.

%% path: general
general() ->
    #section{
       items = #{<<"loglevel">> => #option{type = atom,
                                           validate = loglevel,
                                           format = local_config},
                 <<"hosts">> => #list{items = #option{type = binary,
                                                      validate = non_empty,
                                                      process = fun ?MODULE:prepare_host/1},
                                      validate = unique_non_empty,
                                      format = config},
                 <<"registration_timeout">> => #option{type = int_or_infinity,
                                                       validate = positive,
                                                       format = local_config},
                 <<"language">> => #option{type = binary,
                                           validate = non_empty,
                                           format = config},
                 <<"all_metrics_are_global">> => #option{type = boolean,
                                                         format = local_config},
                 <<"sm_backend">> => #option{type = atom,
                                             validate = {module, ejabberd_sm},
                                             process = fun ?MODULE:process_sm_backend/1,
                                             format = config},
                 <<"max_fsm_queue">> => #option{type = integer,
                                                validate = positive,
                                                format = local_config},
                 <<"http_server_name">> => #option{type = string,
                                                   format = {local_config, cowboy_server_name}},
                 <<"rdbms_server_type">> => #option{type = atom,
                                                    validate = {enum, [mssql, pgsql]},
                                                    format = local_config},
                 <<"override">> => #list{items = #option{type = atom,
                                                         validate = {enum, [local, global, acls]},
                                                         format = override},
                                         validate = unique_non_empty,
                                         format = none},
                 <<"pgsql_users_number_estimate">> => #option{type = boolean,
                                                              format = host_local_config},
                 <<"route_subdomains">> => #option{type = atom,
                                                   validate = {enum, [s2s]},
                                                   format = host_local_config},
                 <<"mongooseimctl_access_commands">> => #section{
                                                           items = #{default => ctl_access_rule()},
                                                           format = local_config},
                 <<"routing_modules">> => #list{items = #option{type = atom,
                                                                validate = module},
                                                format = local_config},
                 <<"replaced_wait_timeout">> => #option{type = integer,
                                                        validate = positive,
                                                        format = host_local_config},
                 <<"hide_service_name">> => #option{type = boolean,
                                                    format = host_local_config}
                },
       required = [<<"hosts">>],
       format = none
      }.

ctl_access_rule() ->
    #section{
       items = #{<<"commands">> => #list{items = #option{type = string}},
                 <<"argument_restrictions">> => #section{
                                                   items = #{default => #option{type = string}}
                                                  }
                },
       process = fun ?MODULE:process_ctl_access_rule/1,
       format = prepend_key
      }.

%% path: listen
listen() ->
    Keys = [<<"http">>, <<"c2s">>, <<"s2s">>, <<"service">>],
    #section{
       items = maps:from_list([{Key, #list{items = listener(Key),
                                           format = none}} || Key <- Keys]),
       format = local_config
      }.

%% path: listen.*[]
listener(Type) ->
    ExtraItems = listener_items(Type),
    #section{
       items = ExtraItems#{<<"port">> => #option{type = integer,
                                                 validate = port},
                           <<"ip_address">> => #option{type = string,
                                                       validate = ip_address},
                           <<"proto">> => #option{type = atom,
                                                  validate = {enum, [tcp, udp, ws, wss]}},
                           <<"ip_version">> => #option{type = integer,
                                                       validate = {enum, [4, 6]},
                                                       process = fun ?MODULE:process_ip_version/1,
                                                       format = item}},
       required = [<<"port">>],
       process = fun ?MODULE:process_listener/2
      }.

listener_items(<<"http">>) ->
    #{<<"tls">> => http_listener_tls(),
      <<"transport">> => http_transport(),
      <<"protocol">> => http_protocol(),
      <<"handlers">> => http_handlers()
     };
listener_items(Type) ->
    ExtraItems = xmpp_listener_items(Type),
    ExtraItems#{<<"hibernate_after">> => #option{type = integer,
                                                 validate = non_negative},
                <<"max_stanza_size">> => #option{type = integer,
                                                 validate = positive},
                <<"backlog">> => #option{type = integer,
                                         validate = non_negative},
                <<"proxy_protocol">> => #option{type = boolean},
                <<"num_acceptors">> => #option{type = integer,
                                               validate = positive,
                                               format = {kv, acceptors_num}}
               }.

xmpp_listener_items(<<"c2s">>) ->
    #{<<"access">> => #option{type = atom,
                              validate = non_empty},
      <<"shaper">> => #option{type = atom,
                              validate = non_empty},
      <<"xml_socket">> => #option{type = boolean},
      <<"zlib">> => #option{type = integer,
                            validate = positive},
      <<"max_fsm_queue">> => #option{type = integer,
                                     validate = positive},
      <<"tls">> => c2s_tls()};
xmpp_listener_items(<<"s2s">>) ->
    #{<<"shaper">> => #option{type = atom,
                              validate = non_empty},
      <<"tls">> => s2s_tls()};
xmpp_listener_items(<<"service">>) ->
    #{<<"access">> => #option{type = atom,
                              validate = non_empty},
      <<"shaper_rule">> => #option{type = atom,
                                   validate = non_empty},
      <<"check_from">> => #option{type = boolean,
                                  format = {kv, service_check_from}},
      <<"hidden_components">> => #option{type = boolean},
      <<"conflict_behaviour">> => #option{type = atom,
                                          validate = {enum, [kick_old, disconnect]}},
      <<"password">> => #option{type = string,
                                validate = non_empty},
      <<"max_fsm_queue">> => #option{type = integer,
                                     validate = positive}}.

%% path: listen.c2s[].tls
c2s_tls() ->
    #section{
       items = #{
                 %% common
                 <<"module">> => #option{type = atom,
                                         validate = {enum, [fast_tls, just_tls]}},
                 <<"mode">> => #option{type = atom,
                                       validate = {enum, [tls, starttls, starttls_required]}},
                 <<"verify_peer">> => #option{type = boolean,
                                              process = fun ?MODULE:process_verify_peer/1},
                 <<"certfile">> => #option{type = string,
                                           validate = non_empty},
                 <<"cacertfile">> => #option{type = string,
                                             validate = non_empty},
                 <<"dhfile">> => #option{type = string,
                                         validate = non_empty},
                 <<"ciphers">> => #option{type = string},

                 %% fast_tls
                 <<"protocol_options">> => #list{items = #option{type = string,
                                                                 validate = non_empty}},

                 %% just_tls
                 <<"verify_mode">> => #option{type = atom,
                                              validate = {enum, [peer, selfsigned_peer, none]}},
                 <<"disconnect_on_failure">> => #option{type = boolean},
                 <<"crl_files">> => #list{items = #option{type = string,
                                                          validate = non_empty},
                                          format = {kv, crlfiles}},
                 <<"password">> => #option{type = string},
                 <<"server_name_indication">> => #option{type = boolean,
                                                         process = fun ?MODULE:process_sni/1},
                 <<"versions">> => #list{items = #option{type = atom}}
                },
       process = fun ?MODULE:process_xmpp_tls/1,
       format = none
      }.

%% path: listen.s2s[].tls
s2s_tls() ->
    #section{
       items = #{<<"cacertfile">> => #option{type = string,
                                             validate = non_empty},
                 <<"dhfile">> => #option{type = string,
                                         validate = non_empty},
                 <<"ciphers">> => #option{type = string},
                 <<"protocol_options">> => #list{items = #option{type = string,
                                                                 validate = non_empty}}
                },
       process = fun ?MODULE:process_fast_tls/1,
       format = none
      }.

%% path: listen.http[].tls
http_listener_tls() ->
    Items = tls_items(),
    #section{
       items = Items#{<<"verify_mode">> => #option{type = atom,
                                                   validate = {enum, [peer, selfsigned_peer, none]}}
                     },
       format = {kv, ssl}
      }.

%% path: listen.http[].transport
http_transport() ->
    #section{
       items = #{<<"num_acceptors">> => #option{type = integer,
                                                validate = positive},
                 <<"max_connections">> => #option{type = int_or_infinity,
                                                  validate = non_negative}
                },
       format = {kv, transport_options}
      }.

%% path: listen.http[].protocol
http_protocol() ->
    #section{
       items = #{<<"compress">> => #option{type = boolean}},
       format = {kv, protocol_options}
      }.

%% path: listen.http[].handlers
http_handlers() ->
    Keys = [<<"mod_websockets">>,
            <<"lasse_handler">>,
            <<"cowboy_static">>,
            <<"mongoose_api">>,
            <<"mongoose_api_admin">>,
            default],
    #section{
       items = maps:from_list([{Key, #list{items = http_handler(Key),
                                           format = none}} || Key <- Keys]),
       validate_keys = module,
       format = {kv, modules}
      }.

%% path: listen.http[].handlers.*[]
http_handler(Key) ->
    ExtraItems = http_handler_items(Key),
    RequiredKeys = case http_handler_required(Key) of
                       all -> all;
                       [] -> [<<"host">>, <<"path">>]
                   end,
    #section{
       items = ExtraItems#{<<"host">> => #option{type = string,
                                                 validate = non_empty},
                           <<"path">> => #option{type = string}
                          },
       required = RequiredKeys,
       process = fun ?MODULE:process_http_handler/2
      }.

http_handler_items(<<"mod_websockets">>) ->
    #{<<"timeout">> => #option{type = int_or_infinity,
                               validate = non_negative},
      <<"ping_rate">> => #option{type = integer,
                                 validate = positive},
      <<"max_stanza_size">> => #option{type = int_or_infinity,
                                       validate = positive},
      <<"service">> => #section{items = xmpp_listener_items(<<"service">>),
                                format = {kv, ejabberd_service}}};
http_handler_items(<<"lasse_handler">>) ->
    #{<<"module">> => #option{type = atom,
                              validate = module}};
http_handler_items(<<"cowboy_static">>) ->
    #{<<"type">> => #option{type = atom},
      <<"app">> => #option{type = atom},
      <<"content_path">> => #option{type = string}};
http_handler_items(<<"mongoose_api">>) ->
    #{<<"handlers">> => #list{items = #option{type = atom,
                                              validate = module}}};
http_handler_items(<<"mongoose_api_admin">>) ->
    #{<<"username">> => #option{type = binary},
      <<"password">> => #option{type = binary}};
http_handler_items(_) ->
    #{}.

http_handler_required(<<"lasse_handler">>) -> all;
http_handler_required(<<"cowboy_static">>) -> all;
http_handler_required(<<"mongoose_api">>) -> all;
http_handler_required(_) -> [].

%% path: (host_config[].)auth
auth() ->
    #section{
       items = #{<<"methods">> => #list{items = #option{type = atom,
                                                        validate = {module, ejabberd_auth}},
                                        format = {kv, auth_method}},
                 <<"password">> => auth_password(),
                 <<"scram_iterations">> => #option{type = integer,
                                                   validate = positive},
                 <<"sasl_external">> =>
                     #list{items = #option{type = atom,
                                           process = fun ?MODULE:process_sasl_external/1},
                           format = {kv, cyrsasl_external}},
                 <<"sasl_mechanisms">> =>
                     #list{items = #option{type = atom,
                                           validate = {module, cyrsasl},
                                           process = fun ?MODULE:process_sasl_mechanism/1}},
                 <<"anonymous">> => auth_anonymous(),
                 <<"external">> => auth_external(),
                 <<"http">> => auth_http(),
                 <<"jwt">> => auth_jwt(),
                 <<"ldap">> => auth_ldap(),
                 <<"riak">> => auth_riak()},
       process = fun ?MODULE:process_auth/1,
       format = {foreach, host_local_config}
      }.

%% path: (host_config[].)auth.password
auth_password() ->
    #section{
       items = #{<<"format">> => #option{type = atom,
                                         validate = {enum, [scram, plain]}},
                 <<"hash">> => #list{items = #option{type = atom,
                                                     validate = {enum, [sha, sha224, sha256,
                                                                        sha384, sha512]}},
                                     validate = unique_non_empty
                                    }
                },
       process = fun ?MODULE:process_auth_password/1,
       format = {kv, password_format}
      }.

%% path: (host_config[].)auth.anonymous
auth_anonymous() ->
    #section{
       items = #{<<"allow_multiple_connections">> => #option{type = boolean},
                 <<"protocol">> => #option{type = atom,
                                           validate = {enum, [sasl_anon, login_anon, both]},
                                           format = {kv, anonymous_protocol}}
                },
       format = none
      }.

%% path: (host_config[].)auth.external
auth_external() ->
    #section{
       items = #{<<"instances">> => #option{type = integer,
                                            validate = positive,
                                            format = {kv, extauth_instances}},
                 <<"program">> => #option{type = string,
                                          validate = non_empty,
                                          format = {kv, extauth_program}}
                },
       format = none
      }.

%% path: (host_config[].)auth.http
auth_http() ->
    #section{
       items = #{<<"basic_auth">> => #option{type = string}},
       format = none
      }.

%% path: (host_config[].)auth.jwt
auth_jwt() ->
    #section{
       items = #{<<"secret">> => auth_jwt_secret(),
                 <<"algorithm">> => #option{type = string,
                                            validate = {enum, ["HS256", "RS256", "ES256",
                                                               "HS386", "RS386", "ES386",
                                                               "HS512", "RS512", "ES512"]},
                                            format = {kv, jwt_algorithm}},
                 <<"username_key">> => #option{type = atom,
                                               validate = non_empty,
                                               format = {kv, jwt_username_key}}
                },
       required = all,
       format = none
      }.

%% path: (host_config[].)auth.jwt.secret
auth_jwt_secret() ->
    #section{
       items = #{<<"file">> => #option{type = string,
                                       validate = non_empty},
                 <<"env">> => #option{type = string,
                                      validate = non_empty},
                 <<"value">> => #option{type = string}},
       process = fun ?MODULE:process_jwt_secret/1,
       format = item
      }.

%% path: (host_config[].)auth.ldap
auth_ldap() ->
    #section{
       items = #{<<"pool_tag">> => #option{type = atom,
                                           validate = non_empty,
                                           format = {kv, ldap_pool_tag}},
                 <<"bind_pool_tag">> => #option{type = atom,
                                                validate = non_empty,
                                                format = {kv, ldap_bind_pool_tag}},
                 <<"base">> => #option{type = string,
                                       format = {kv, ldap_base}},
                 <<"uids">> => #list{items = ldap_uids(),
                                     format = {kv, ldap_uids}},
                 <<"filter">> => #option{type = string,
                                         format = {kv, ldap_filter}},
                 <<"dn_filter">> => ldap_dn_filter(),
                 <<"local_filter">> => ldap_local_filter(),
                 <<"deref">> => #option{type = atom,
                                        validate = {enum, [never, always, finding, searching]},
                                        format = {kv, ldap_deref}}
                },
       format = none
      }.

%% path: (host_config[].)auth.ldap.uids
ldap_uids() ->
    #section{
       items = #{<<"attr">> => #option{type = string},
                 <<"format">> => #option{type = string}},
       process = fun ?MODULE:process_ldap_uids/1,
       required = [<<"attr">>]
      }.

%% path: (host_config[].)auth.ldap.dn_filter
ldap_dn_filter() ->
    #section{
       items = #{<<"filter">> => #option{type = string},
                 <<"attributes">> => #list{items = #option{type = string}}
                },
       required = all,
       process = fun ?MODULE:process_ldap_dn_filter/1,
       format = {kv, ldap_dn_filter}
      }.

%% path: (host_config[].)auth.ldap.local_filter
ldap_local_filter() ->
    #section{
       items = #{<<"operation">> => #option{type = atom,
                                            validate = {enum, [equal, notequal]}},
                 <<"attribute">> => #option{type = string,
                                            validate = non_empty},
                 <<"values">> => #list{items = #option{type = string},
                                       validate = non_empty}
                },
       required = all,
       process = fun ?MODULE:process_ldap_local_filter/1,
       format = {kv, ldap_local_filter}
      }.

%% path: (host_config[].)auth.riak
auth_riak() ->
    #section{
       items = #{<<"bucket_type">> => #option{type = binary,
                                              validate = non_empty}},
       format = none
      }.

%% path: outgoing_pools
outgoing_pools() ->
    PoolTypes = [<<"cassandra">>, <<"elastic">>, <<"http">>, <<"ldap">>,
                 <<"rabbit">>, <<"rdbms">>, <<"redis">>, <<"riak">>],
    Items = [{Type, #section{items = #{default => outgoing_pool(Type)},
                             validate_keys = non_empty,
                             format = none}} || Type <- PoolTypes],
    #section{
       items = maps:from_list(Items),
       format = local_config
      }.

%% path: outgoing_pools.*.*
outgoing_pool(Type) ->
    WPool = wpool_items(),
    #section{
       items = WPool#{<<"scope">> => #option{type = atom,
                                             validate = {enum, [global, host, single_host]}},
                      <<"host">> => #option{type = binary,
                                            validate = non_empty},
                      <<"connection">> => outgoing_pool_connection(Type)
                },
       process = fun ?MODULE:process_pool/2,
       format = item
      }.

wpool_items() ->
    #{<<"workers">> => #option{type = integer,
                               validate = positive},
      <<"strategy">> => #option{type = atom,
                                validate = {enum, wpool_strategy_values()}},
      <<"call_timeout">> => #option{type = integer,
                                    validate = positive}
     }.

%% path: outgoing_pools.*.*.connection
outgoing_pool_connection(<<"cassandra">>) ->
    #section{
       items = #{<<"servers">> => #list{items = cassandra_server()},
                 <<"keyspace">> => #option{type = string,
                                           validate = non_empty},
                 <<"auth">> => #section{items = #{<<"plain">> => cassandra_auth_plain()},
                                        required = all,
                                        process = fun ?MODULE:process_cassandra_auth/1},
                 <<"tls">> => #section{items = tls_items(),
                                       format = {kv, ssl}}
                }
      };
outgoing_pool_connection(<<"elastic">>) ->
    #section{
       items = #{<<"host">> => #option{type = string,
                                       validate = non_empty},
                 <<"port">> => #option{type = integer,
                                       validate = port}
                }
      };
outgoing_pool_connection(<<"http">>) ->
    #section{
       items = #{<<"host">> => #option{type = string,
                                       validate = non_empty,
                                       format = {kv, server}},
                 <<"path_prefix">> => #option{type = string,
                                              validate = non_empty},
                 <<"request_timeout">> => #option{type = integer,
                                                  validate = non_negative},
                 <<"tls">> => #section{items = tls_items(),
                                       format = {kv, http_opts}}
                }
      };
outgoing_pool_connection(<<"ldap">>) ->
    #section{
       items = #{<<"host">> => #option{type = string,
                                       validate = non_empty},
                 <<"port">> => #option{type = integer,
                                       validate = port},
                 <<"rootdn">> => #option{type = string},
                 <<"password">> => #option{type = string},
                 <<"encrypt">> => #option{type = atom,
                                          validate = {enum, [none, tls]}},
                 <<"servers">> => #list{items = #option{type = string}},
                 <<"connect_interval">> => #option{type = integer,
                                                   validate = positive},
                 <<"tls">> => #section{items = tls_items(),
                                       format = {kv, tls_options}}
                }
      };
outgoing_pool_connection(<<"rabbit">>) ->
    #section{
       items = #{<<"amqp_host">> => #option{type = string,
                                            validate = non_empty},
                 <<"amqp_port">> => #option{type = integer,
                                            validate = port},
                 <<"amqp_username">> => #option{type = string,
                                                validate = non_empty},
                 <<"amqp_password">> => #option{type = string,
                                                validate = non_empty},
                 <<"confirms_enabled">> => #option{type = boolean},
                 <<"max_worker_queue_len">> => #option{type = int_or_infinity,
                                                       validate = non_negative}
                }
      };
outgoing_pool_connection(<<"rdbms">>) ->
    #section{
       items = #{<<"driver">> => #option{type = atom,
                                         validate = {enum, [odbc, pgsql, mysql]}},
                 <<"keepalive_interval">> => #option{type = integer,
                                                     validate = positive},

                 % odbc
                 <<"settings">> => #option{type = string},

                 % mysql, pgsql
                 <<"host">> => #option{type = string,
                                       validate = non_empty},
                 <<"database">> => #option{type = string,
                                           validate = non_empty},
                 <<"username">> => #option{type = string,
                                           validate = non_empty},
                 <<"password">> => #option{type = string,
                                           validate = non_empty},
                 <<"port">> => #option{type = integer,
                                       validate = port},
                 <<"tls">> => sql_tls()
                },
       process = fun ?MODULE:process_rdbms_connection/1
      };
outgoing_pool_connection(<<"redis">>) ->
    #section{
       items = #{<<"host">> => #option{type = string,
                                       validate = non_empty},
                 <<"port">> => #option{type = integer,
                                       validate = port},
                 <<"database">> => #option{type = integer,
                                           validate = non_negative},
                 <<"password">> => #option{type = string}
                }
      };
outgoing_pool_connection(<<"riak">>) ->
    #section{
       items = #{<<"address">> => #option{type = string,
                                          validate = non_empty},
                 <<"port">> => #option{type = integer,
                                       validate = port},
                 <<"credentials">> => riak_credentials(),
                 <<"tls">> => #section{items = tls_items(),
                                       process = fun ?MODULE:process_riak_tls/1,
                                       format = none}}
      }.

cassandra_server() ->
    #section{
       items = #{<<"ip_address">> => #option{type = string,
                                             validate = non_empty},
                 <<"port">> => #option{type = integer,
                                       validate = port}},
       required = [<<"ip_address">>],
       process = fun ?MODULE:process_cassandra_server/1
      }.

%% path: outgoing_pools.cassandra.*.connection.auth.plain
cassandra_auth_plain() ->
    #section{
       items = #{<<"username">> => #option{type = binary},
                 <<"password">> => #option{type = binary}},
       required = all
      }.

%% path: outgoing_pools.riak.*.connection.credentials
riak_credentials() ->
    #section{
       items = #{<<"user">> => #option{type = string,
                                       validate = non_empty},
                 <<"password">> => #option{type = string,
                                           validate = non_empty}},
       required = all,
       process = fun ?MODULE:process_riak_credentials/1,
       format = prepend_key
      }.

%% path: outgoing_pools.rdbms.*.connection.tls
sql_tls() ->
    Items = tls_items(),
    #section{
       items = Items#{<<"required">> => #option{type = boolean}}
      }.

tls_items() ->
    #{<<"verify_peer">> => #option{type = boolean,
                                   process = fun ?MODULE:process_verify_peer/1,
                                   format = {kv, verify}},
      <<"certfile">> => #option{type = string,
                                validate = non_empty},
      <<"cacertfile">> => #option{type = string,
                                  validate = non_empty},
      <<"dhfile">> => #option{type = string,
                              validate = non_empty},
      <<"keyfile">> => #option{type = string,
                               validate = non_empty},
      <<"password">> => #option{type = string},
      <<"server_name_indication">> => #option{type = boolean,
                                              process = fun ?MODULE:process_sni/1},
      <<"ciphers">> => #option{type = string},
      <<"versions">> => #list{items = #option{type = atom}}
     }.

%% path: (host_config[].)services
services() ->
    Services = [{a2b(Service), mongoose_service:config_spec(Service)} || Service <- all_services()],
    #section{
       items = maps:from_list(Services),
       format = local_config
      }.

all_services() ->
    [service_admin_extra,
     service_mongoose_system_metrics].

%% path: (host_config[].)modules
modules() ->
    Modules = [{a2b(Module), gen_mod:config_spec(Module)} || Module <- all_modules()],
    #section{
       items = maps:from_list(Modules),
       format = host_local_config
      }.

all_modules() ->
    [mod_adhoc,
     mod_auth_token,
     mod_bosh,
     mod_caps,
     mod_carboncopy,
     mod_csi,
     mod_disco,
     mod_event_pusher].

%% path: (host_config[].)modules.*.iqdisc
iqdisc() ->
    #section{
       items = #{<<"type">> => #option{type = atom,
                                       validate = {enum, [no_queue, one_queue, parallel, queues]}},
                 <<"workers">> => #option{type = integer,
                                          validate = positive}},
       required = [<<"type">>],
       process = fun ?MODULE:format_iqdisc/1
      }.

format_iqdisc(KVs) ->
    {[[{type, Type}]], WorkersOpts} = proplists:split(KVs, [type]),
    iqdisc(Type, WorkersOpts).

iqdisc(queues, [{workers, N}]) -> {queues, N};
iqdisc(Type, []) -> Type.

%% path: (host_config[].)shaper
shaper() ->
    #section{
       items = #{default =>
                     #section{
                        items = #{<<"max_rate">> => #option{type = integer,
                                                            validate = positive,
                                                            format = {kv, maxrate}}},
                        required = all,
                        process = fun ?MODULE:process_shaper/1,
                        format = {host_or_global_config, shaper}
                       }
                },
       validate_keys = non_empty,
       format = none
      }.

%% path: (host_config[].)acl
acl() ->
    #section{
       items = #{default => #list{items = acl_item(),
                                  format = none}
                },
       format = none
      }.

%% path: (host_config[].)acl.*[]
acl_item() ->
    #section{
       items = #{<<"match">> => #option{type = atom,
                                        validate = {enum, [all, none]}},
                 <<"user">> => #option{type = string},
                 <<"server">> => #option{type = string},
                 <<"resource">> => #option{type = string},
                 <<"user_regexp">> => #option{type = string},
                 <<"server_regexp">> => #option{type = string},
                 <<"resource_regexp">> => #option{type = string},
                 <<"user_glob">> => #option{type = string},
                 <<"server_glob">> => #option{type = string},
                 <<"resource_glob">> => #option{type = string}
                },
       validate_keys = non_empty,
       process = fun ?MODULE:process_acl_item/1,
       format = host_or_global_acl
      }.

%% path: (host_config[].)access
access() ->
    #section{
       items = #{default => #list{items = access_rule_item(),
                                  format = {host_or_global_config, access}}
                },
       format = none
      }.

%% path: (host_config[].)access.*[]
access_rule_item() ->
    #section{
       items = #{<<"acl">> => #option{type = atom,
                                      validate = non_empty},
                 <<"value">> => #option{type = int_or_atom}
                },
       required = all,
       process = fun ?MODULE:process_access_rule_item/1
      }.

%% path: (host_config[].)s2s
s2s() ->
    #section{
       items = #{<<"dns">> => s2s_dns(),
                 <<"outgoing">> => s2s_outgoing(),
                 <<"use_starttls">> => #option{type = atom,
                                               validate = {enum, [false, optional, required,
                                                                  required_trusted]},
                                               format = {local_config, s2s_use_starttls}},
                 <<"certfile">> => #option{type = string,
                                           validate = non_empty,
                                           format = {local_config, s2s_certfile}},
                 <<"default_policy">> => #option{type = atom,
                                                 validate = {enum, [allow, deny]},
                                                 format = {host_local_config, s2s_default_policy}},
                 <<"host_policy">> => #list{items = s2s_host_policy(),
                                            format = {foreach, host_local_config}},
                 <<"address">> => #list{items = s2s_address(),
                                        format = {foreach, local_config}},
                 <<"ciphers">> => #option{type = string,
                                          format = {local_config, s2s_ciphers}},
                 <<"domain_certfile">> => #list{items = s2s_domain_cert(),
                                                format = {foreach, local_config}},
                 <<"shared">> => #option{type = binary,
                                         validate = non_empty,
                                         format = {host_local_config, s2s_shared}},
                 <<"max_retry_delay">> => #option{type = integer,
                                                  validate = positive,
                                                  format = {host_local_config, s2s_max_retry_delay}}
                },
       format = none
      }.

%% path: (host_config[].)s2s.dns
s2s_dns() ->
    #section{
       items = #{<<"timeout">> => #option{type = integer,
                                          validate = positive},
                 <<"retries">> => #option{type = integer,
                                          validate = positive}},
       format = {local_config, s2s_dns_options}
      }.

%% path: (host_config[].)s2s.outgoing
s2s_outgoing() ->
    #section{
       items = #{<<"port">> => #option{type = integer,
                                       validate = port,
                                       format = {local_config, outgoing_s2s_port}},
                 <<"ip_versions">> =>
                     #list{items = #option{type = integer,
                                           validate = {enum, [4, 6]},
                                           process = fun ?MODULE:process_s2s_address_family/1},
                           validate = unique_non_empty,
                           format = {local_config, outgoing_s2s_families}},
                 <<"connection_timeout">> => #option{type = int_or_infinity,
                                                     validate = positive,
                                                     format = {local_config, outgoing_s2s_timeout}}
                },
       format = none
      }.

%% path: (host_config[].)s2s.host_policy[]
s2s_host_policy() ->
    #section{
       items = #{<<"host">> => #option{type = binary,
                                       validate = non_empty},
                 <<"policy">> => #option{type = atom,
                                         validate = {enum, [allow, deny]}}
                },
       required = all,
       process = fun ?MODULE:process_s2s_host_policy/1
      }.

%% path: (host_config[].)s2s.address[]
s2s_address() ->
    #section{
       items = #{<<"host">> => #option{type = binary,
                                       validate = non_empty},
                 <<"ip_address">> => #option{type = string,
                                             validate = ip_address},
                 <<"port">> => #option{type = integer,
                                       validate = port}
                },
       required = [<<"host">>, <<"ip_address">>],
       process = fun ?MODULE:process_s2s_address/1
      }.

%% path: (host_config[].)s2s.domain_certfile[]
s2s_domain_cert() ->
    #section{
       items = #{<<"domain">> => #option{type = string,
                                         validate = non_empty},
                 <<"certfile">> => #option{type = string,
                                           validate = non_empty}},
       required = all,
       process = fun ?MODULE:process_s2s_domain_cert/1
      }.

%% Callbacks for 'process'

process_ctl_access_rule(KVs) ->
    Commands = proplists:get_value(commands, KVs, all),
    ArgRestrictions = proplists:get_value(argument_restrictions, KVs, []),
    {Commands, ArgRestrictions}.

process_sm_backend(Backend) ->
    {Backend, []}.

prepare_host(Host) ->
    Node = jid:nodeprep(Host),
    true = Node =/= error,
    Node.

process_sni(false) ->
    disable.

process_lasse_handler([{module, Module}]) ->
    [Module].

process_verify_peer(false) -> verify_none;
process_verify_peer(true) -> verify_peer.

process_xmpp_tls(KVs) ->
    Module = proplists:get_value(module, KVs, fast_tls),
    case proplists:get_keys(KVs) -- (tls_keys(Module) ++ common_tls_keys()) of
        [] -> strip_tls_keys(process_xmpp_tls(Module, proplists:delete(module, KVs)));
        ExcessKeys -> error(#{what => {unexpected_tls_options, Module, ExcessKeys}})
    end.

tls_keys(just_tls) ->
    [verify_mode, disconnect_on_failure, crlfiles, password, server_name_indication, versions];
tls_keys(fast_tls) ->
    [protocol_options].

common_tls_keys() ->
    [module, mode, verify_peer, certfile, cacertfile, dhfile, ciphers].

process_xmpp_tls(just_tls, KVs) ->
    {[VM, DoF], Opts} = proplists:split(KVs, [verify_mode, disconnect_on_failure]),
    {External, Internal} = lists:partition(fun is_external_tls_opt/1, Opts),
    SSLOpts = ssl_opts(verify_fun(VM, DoF) ++ Internal),
    [{tls_module, just_tls}] ++ SSLOpts ++ External;
process_xmpp_tls(fast_tls, KVs) ->
    process_fast_tls(KVs).

process_fast_tls(KVs) ->
    proplists:substitute_aliases([{cacertfile, cafile}], KVs).

strip_tls_keys(Opts) ->
    lists:map(fun strip_tls_key/1, Opts).

strip_tls_key({mode, V}) -> V;
strip_tls_key({verify_peer, V}) -> V;
strip_tls_key(KV) -> KV.

verify_fun([], []) -> [];
verify_fun([{verify_mode, VM}], []) -> [{verify_fun, {VM, true}}];
verify_fun([{verify_mode, VM}], [{disconnect_on_failure, DoF}]) -> [{verify_fun, {VM, DoF}}].

is_external_tls_opt({mode, _}) -> true;
is_external_tls_opt({verify_peer, _}) -> true;
is_external_tls_opt({crlfiles, _}) -> true;
is_external_tls_opt({_, _}) -> false.

ssl_opts([]) -> [];
ssl_opts(Opts) -> [{ssl_options, Opts}].

process_ip_version(4) -> inet;
process_ip_version(6) -> inet6.

process_listener([item, Type | _], KVs) ->
    {[PortOpts, IPOpts], Opts} = proplists:split(KVs, [port, ip_address]),
    PortIP = listener_portip(PortOpts, IPOpts),
    {Port, IPT, _, _, Proto, OptsClean} =
        ejabberd_listener:parse_listener_portip(PortIP, Opts),
    {{Port, IPT, Proto}, listener_module(Type), OptsClean}.

listener_portip([{port, Port}], []) -> Port;
listener_portip([{port, Port}], [{ip_address, Addr}]) -> {Port, Addr}.

listener_module(<<"http">>) -> ejabberd_cowboy;
listener_module(<<"c2s">>) -> ejabberd_c2s;
listener_module(<<"s2s">>) -> ejabberd_s2s_in;
listener_module(<<"service">>) -> ejabberd_service.

process_http_handler([item, Type | _], KVs) ->
    {[[{host, Host}], [{path, Path}]], Opts} = proplists:split(KVs, [host, path]),
    HandlerOpts = process_http_handler_opts(Type, Opts),
    {Host, Path, binary_to_atom(Type, utf8), HandlerOpts}.

process_http_handler_opts(<<"lasse_handler">>, [{module, Module}]) ->
    [Module];
process_http_handler_opts(<<"cowboy_static">>, Opts) ->
    {[[{type, Type}], [{app, App}], [{content_path, Path}]], []} =
        proplists:split(Opts, [type, app, content_path]),
    {Type, App, Path, [{mimetypes, cow_mimetypes, all}]};
process_http_handler_opts(<<"mongoose_api_admin">>, Opts) ->
    {[UserOpts, PassOpts], []} = proplists:split(Opts, [username, password]),
    case {UserOpts, PassOpts} of
        {[], []} -> [];
        {[{username, User}], [{password, Pass}]} -> [{auth, {User, Pass}}]
    end;
process_http_handler_opts(<<"cowboy_swagger_redirect_handler">>, []) -> #{};
process_http_handler_opts(<<"cowboy_swagger_json_handler">>, []) -> #{};
process_http_handler_opts(_, Opts) -> Opts.

process_auth(Opts) ->
    %% some options need to be wrapped in 'auth_opts' - this needs simplifying in the future
    OuterKeys = [auth_method, allow_multiple_connections, anonymous_protocol, sasl_mechanisms,
                 extauth_instances],
    {OuterOpts, InnerOpts} = lists:partition(fun({K, _}) -> lists:member(K, OuterKeys) end, Opts),
    [{auth_opts, InnerOpts} | OuterOpts].

process_sasl_external(V) when V =:= standard;
                              V =:= common_name;
                              V =:= auth_id ->
    V;
process_sasl_external(M) ->
    mongoose_config_validator_toml:validate(M, atom, module),
    {mod, M}.

process_sasl_mechanism(V) ->
    list_to_atom("cyrsasl_" ++ atom_to_list(V)).

process_jwt_secret([{file, V}]) -> {jwt_secret_source, V};
process_jwt_secret([{env, V}]) -> {jwt_secret_source, {env, V}};
process_jwt_secret([{value, V}]) -> {jwt_secret, V}.

process_auth_password(KVs) ->
    {[FormatOpts, HashOpts], []} = proplists:split(KVs, [format, hash]),
    case {FormatOpts, HashOpts} of
        {[{format, Format}], []} -> Format;
        {[{format, scram}], [{hash, Hashes}]} -> {scram, Hashes};
        {[], [{hash, Hashes}]} -> {scram, Hashes}
    end.

process_ldap_dn_filter(KVs) ->
    {_, Filter} = proplists:lookup(filter, KVs),
    {_, Attrs} = proplists:lookup(attributes, KVs),
    {Filter, Attrs}.

process_ldap_local_filter(KVs) ->
    {_, Op} = proplists:lookup(operation, KVs),
    {_, Attribute} = proplists:lookup(attribute, KVs),
    {_, Values} = proplists:lookup(values, KVs),
    {Op, {Attribute, Values}}.

process_ldap_uids(KVs) ->
    {[AttrOpts, FormatOpts], []} = proplists:split(KVs, [attr, format]),
    case {AttrOpts, FormatOpts} of
        {[{attr, Attr}], []} -> Attr;
        {[{attr, Attr}], [{format, Format}]} -> {Attr, Format}
    end.

process_pool([Tag, Type|_], KVs) ->
    {[ScopeOpts, HostOpts, ConnOpts], Opts} = proplists:split(KVs, [scope, host, connection]),
    Scope = pool_scope(ScopeOpts, HostOpts),
    Connection = pool_connection(ConnOpts),
    {b2a(Type), Scope, b2a(Tag), Opts, Connection}.

pool_scope([{scope, single_host}], [{host, Host}]) -> Host;
pool_scope([{scope, host}], []) -> host;
pool_scope([{scope, global}], []) -> global;
pool_scope([], []) -> global.

pool_connection([{connection, Opts}]) -> Opts;
pool_connection([]) -> [].

process_cassandra_server(KVs) ->
    {[[{ip_address, IPAddr}]], Opts} = proplists:split(KVs, [ip_address]),
    case Opts of
        [] -> IPAddr;
        [{port, Port}] -> {IPAddr, Port}
    end.

process_cassandra_auth([{plain, KVs}]) ->
    {[[{username, User}], [{password, Pass}]], []} = proplists:split(KVs, [username, password]),
    {cqerl_auth_plain_handler, [{User, Pass}]}.

process_rdbms_connection(KVs) ->
    {[[{driver, Driver}], KeepaliveIntervalOpts], Opts} =
        proplists:split(KVs, [driver, keepalive_interval]),
    [{server, rdbms_server(Driver, Opts)} | KeepaliveIntervalOpts].

rdbms_server(odbc, Opts) ->
    [{settings, Settings}] = Opts,
    Settings;
rdbms_server(Driver, Opts) ->
    {[[{host, Host}], [{database, DB}], [{username, User}], [{password, Pass}],
      PortOpts, TLSOpts], []} =
        proplists:split(Opts, [host, database, username, password, port, tls]),
    list_to_tuple([Driver, Host] ++ db_port(PortOpts) ++
                      [DB, User, Pass] ++ db_tls(Driver, TLSOpts)).

db_port([{port, Port}]) -> [Port];
db_port([]) -> [].

db_tls(Driver, [{tls, KVs}]) ->
    {[ModeOpts], Opts} = proplists:split(KVs, [required]),
    [ssl_mode(Driver, ModeOpts) ++ ssl_opts(Driver, Opts)];
db_tls(_, []) -> [].

ssl_mode(pgsql, [{required, true}]) -> [{ssl, required}];
ssl_mode(pgsql, [{required, false}]) -> [{ssl, true}];
ssl_mode(pgsql, []) -> [{ssl, true}];
ssl_mode(mysql, []) -> [].

ssl_opts(pgsql, []) -> [];
ssl_opts(pgsql, Opts) -> [{ssl_opts, Opts}];
ssl_opts(mysql, Opts) -> Opts.

process_riak_tls(KVs) ->
    {[CACertFileOpts], SSLOpts} = proplists:split(KVs, [cacertfile]),
    riak_ssl(SSLOpts) ++ CACertFileOpts.

riak_ssl([]) -> [];
riak_ssl(Opts) -> [{ssl_opts, Opts}].

process_riak_credentials(KVs) ->
    {[[{user, User}], [{password, Pass}]], []} = proplists:split(KVs, [user, password]),
    {User, Pass}.

b2a(B) -> binary_to_atom(B, utf8).

a2b(A) -> atom_to_binary(A, utf8).

wpool_strategy_values() ->
    [best_worker, random_worker, next_worker, available_worker, next_available_worker].

process_shaper([MaxRate]) ->
    MaxRate.

process_acl_item([{match, V}]) -> V;
process_acl_item(KVs) ->
    {AclName, AclKeys} = find_acl(KVs, lists:sort(proplists:get_keys(KVs)), acl_keys()),
    list_to_tuple([AclName | lists:map(fun(K) -> proplists:get_value(K, KVs) end, AclKeys)]).

find_acl(KVs, SortedKeys, [{AclName, AclKeys}|Rest]) ->
    case lists:sort(AclKeys) of
        SortedKeys -> {AclName, AclKeys};
        _ -> find_acl(KVs, SortedKeys, Rest)
    end.

acl_keys() ->
    [{user, [user, server]},
     {user, [user]},
     {server, [server]},
     {resource, [resource]},
     {user_regexp, [user_regexp, server]},
     {node_regexp, [user_regexp, server_regexp]},
     {user_regexp, [user_regexp]},
     {server_regexp, [server_regexp]},
     {resource_regexp, [resource_regexp]},
     {user_glob, [user_glob, server]},
     {node_glob, [user_glob, server_glob]},
     {user_glob, [user_glob]},
     {server_glob, [server_glob]},
     {resource_glob, [resource_glob]}
    ].

process_access_rule_item(KVs) ->
    {[[{acl, Acl}], [{value, Value}]], []} = proplists:split(KVs, [acl, value]),
    {Value, Acl}.

process_s2s_address_family(4) -> ipv4;
process_s2s_address_family(6) -> ipv6.

process_s2s_host_policy(KVs) ->
    {[[{host, S2SHost}], [{policy, Policy}]], []} = proplists:split(KVs, [host, policy]),
    {{s2s_host, S2SHost}, Policy}.

process_s2s_address(KVs) ->
    {[[{host, S2SHost}], [{ip_address, IPAddr}]], Opts} = proplists:split(KVs, [host, ip_address]),
    {{s2s_addr, S2SHost}, s2s_address(IPAddr, Opts)}.

s2s_address(IPAddress, []) -> IPAddress;
s2s_address(IPAddress, [{port, Port}]) -> {IPAddress, Port}.

process_s2s_domain_cert(KVs) ->
    {[[{domain, Domain}], [{certfile, Certfile}]], []} = proplists:split(KVs, [domain, certfile]),
    {{domain_certfile, Domain}, Certfile}.
