-module(mongoose_config_spec).

%% entry point - returns the entire spec
-export([root/0]).

%% spec parts used by modules and services
-export([wpool_items/0,
         iqdisc/0]).

%% callbacks for the 'process' step
-export([process_root/1,
         process_host/1,
         process_general/1,
         process_sm_backend/1,
         process_ctl_access_rule/1,
         process_listener/2,
         process_verify_peer/1,
         process_sni/1,
         process_xmpp_tls/1,
         process_fast_tls/1,
         process_http_handler/2,
         process_sasl_external/1,
         process_sasl_mechanism/1,
         process_auth/1,
         process_pool/2,
         process_cassandra_auth/1,
         process_rdbms_connection/1,
         process_riak_tls/1,
         process_cassandra_server/1,
         process_riak_credentials/1,
         process_iqdisc/1,
         process_shaper/1,
         process_acl_item/1,
         process_access_rule_item/1,
         process_s2s_address_family/1,
         process_s2s_host_policy/1,
         process_s2s_address/1,
         process_s2s_domain_cert/1]).

-include("mongoose_config_spec.hrl").

-type config_node() :: config_section() | config_list() | config_option().
-type config_section() :: #section{}.
-type config_list() :: #list{}.
-type config_option() :: #option{}.

-type option_type() :: boolean | binary | string | atom | int_or_infinity
                     | int_or_atom | integer | float.

-type wrapper() :: top_level_config_wrapper() | config_part_wrapper().

%% Wrap the value in a top-level config option
-type top_level_config_wrapper() ::
      % Config option, see the type below for details
        config_option_wrapper()

      % Config option, the key is replaced with NewKey
      | {config_option_wrapper(), NewKey :: atom()}

      % Inside host_config: [{{Tag, Key, Host}, Value}]
      % Otherwise: [{{Tag, Key, global}, Value}]
      | {host_or_global_config, Tag :: atom()}.

-type config_option_wrapper() ::
        global_config % [{Key, Value}]
      | host_config. % Inside host_config: [{{Key, Host}, Value}]
                     % Otherwise: one such option for each configured host

%% Wrap the value in config part - key-value pair or just a value
-type config_part_wrapper() ::
        default      % [{Key, Value}] for section items, [Value] for list items
      | item         % [Value]
      | remove       % [] - the item is ignored
      | none         % just Value - injects elements of Value into the parent section/list
      | {kv, NewKey :: term()} % [{NewKey, Value}] - replaces the key with NewKey
      | prepend_key. % [{Key, V1, ..., Vn}] when Value = {V1, ..., Vn}

%% This option allows to put list/section items in a map
-type format_items() ::
        none         % keep the processed items unchanged
      | map.         % convert the processed items (which have to be a KV list) to a map

-export_type([config_node/0, config_section/0, config_list/0, config_option/0,
              wrapper/0, format_items/0, option_type/0]).

%% Config processing functions are annotated with TOML paths
%% Path syntax: dotted, like TOML keys with the following additions:
%%   - '[]' denotes an element in a list
%%   - '( ... )' encloses an optional prefix
%%   - '*' is a wildcard for names - usually that name is passed as an argument
%% If the path is the same as for the previous function, it is not repeated.
%%
%% Example: (host_config[].)access.*
%% Meaning: either a key in the 'access' section, e.g.
%%            [access]
%%              local = ...
%%          or the same, but prefixed for a specific host, e.g.
%%            [[host_config]]
%%              host = "myhost"
%%              host_config.access
%%                local = ...

root() ->
    General = general(),
    Listen = listen(),
    Auth = auth(),
    Modules = modules(),
    #section{
       items = #{<<"general">> => General#section{required = [<<"default_server_domain">>],
                                                  process = fun ?MODULE:process_general/1,
                                                  defaults = general_defaults()},
                 <<"listen">> => Listen#section{include = always},
                 <<"auth">> => Auth#section{include = always},
                 <<"outgoing_pools">> => outgoing_pools(),
                 <<"services">> => services(),
                 <<"modules">> => Modules#section{include = always},
                 <<"shaper">> => shaper(),
                 <<"acl">> => acl(),
                 <<"access">> => access(),
                 <<"s2s">> => s2s(),
                 <<"host_config">> => #list{items = host_config(),
                                            wrap = none}
                },
       required = [<<"general">>],
       process = fun ?MODULE:process_root/1,
       wrap = none
      }.

%% path: host_config[]
host_config() ->
    #section{
       items = #{%% Host is only validated here - it is stored in the path,
                 %% see mongoose_config_parser_toml:item_key/1
                 %%
                 %% for every configured host the host_type of the same name
                 %% is declared automatically. As host_config section is now
                 %% used for changing configuration of the host_type, we don't
                 %% need host option any more. but to stay compatible with an
                 %% old config format we keep host option as well. now it is
                 %% just a synonym to host_type.
                 <<"host">> => #option{type = binary,
                                       validate = non_empty,
                                       wrap = remove},

                 <<"host_type">> => #option{type = binary,
                                            validate = non_empty,
                                            wrap = remove},

                 %% Sections below are allowed in host_config,
                 %% but only options with these formats are accepted:
                 %%  - host_config
                 %%  - host_or_global_config
                 %% Any other options would be caught by
                 %%   mongoose_config_parser_toml:format/3
                 <<"general">> => general(),
                 <<"auth">> => auth(),
                 <<"modules">> => modules(),
                 <<"shaper">> => shaper(),
                 <<"acl">> => acl(),
                 <<"access">> => access(),
                 <<"s2s">> => s2s()
                },
       wrap = none
      }.

%% path: general
general() ->
    #section{
       items = #{<<"loglevel">> => #option{type = atom,
                                           validate = loglevel,
                                           wrap = global_config},
                 <<"hosts">> => #list{items = #option{type = binary,
                                                      validate = non_empty,
                                                      process = fun ?MODULE:process_host/1},
                                      validate = unique,
                                      wrap = global_config},
                 <<"host_types">> => #list{items = #option{type = binary,
                                                           validate = non_empty},
                                           validate = unique,
                                           wrap = global_config},
                 <<"default_server_domain">> => #option{type = binary,
                                                        validate = non_empty,
                                                        process = fun ?MODULE:process_host/1,
                                                        wrap = global_config},
                 <<"registration_timeout">> => #option{type = int_or_infinity,
                                                       validate = positive,
                                                       wrap = global_config},
                 <<"language">> => #option{type = binary,
                                           validate = non_empty,
                                           wrap = global_config},
                 <<"all_metrics_are_global">> => #option{type = boolean,
                                                         wrap = global_config},
                 <<"sm_backend">> => #option{type = atom,
                                             validate = {module, ejabberd_sm},
                                             process = fun ?MODULE:process_sm_backend/1,
                                             wrap = global_config},
                 <<"max_fsm_queue">> => #option{type = integer,
                                                validate = positive,
                                                wrap = global_config},
                 <<"http_server_name">> => #option{type = string,
                                                   wrap = {global_config, cowboy_server_name}},
                 <<"rdbms_server_type">> => #option{type = atom,
                                                    validate = {enum, [mssql, pgsql]},
                                                    wrap = global_config},
                 <<"route_subdomains">> => #option{type = atom,
                                                   validate = {enum, [s2s]},
                                                   wrap = host_config},
                 <<"mongooseimctl_access_commands">> => #section{
                                                           items = #{default => ctl_access_rule()},
                                                           wrap = global_config},
                 <<"routing_modules">> => #list{items = #option{type = atom,
                                                                validate = module},
                                                wrap = global_config},
                 <<"replaced_wait_timeout">> => #option{type = integer,
                                                        validate = positive,
                                                        wrap = host_config},
                 <<"hide_service_name">> => #option{type = boolean,
                                                    wrap = global_config}
                },
       wrap = none
      }.

general_defaults() ->
    #{<<"loglevel">> => warning,
      <<"hosts">> => [],
      <<"host_types">> => [],
      <<"registration_timeout">> => 600,
      <<"language">> => <<"en">>,
      <<"all_metrics_are_global">> => false,
      <<"sm_backend">> => {mnesia, []},
      <<"rdbms_server_type">> => generic,
      <<"mongooseimctl_access_commands">> => [],
      <<"routing_modules">> => ejabberd_router:default_routing_modules(),
      <<"replaced_wait_timeout">> => 2000,
      <<"hide_service_name">> => false}.

ctl_access_rule() ->
    #section{
       items = #{<<"commands">> => #list{items = #option{type = string}},
                 <<"argument_restrictions">> => #section{
                                                   items = #{default => #option{type = string}}
                                                  }
                },
       process = fun ?MODULE:process_ctl_access_rule/1,
       wrap = prepend_key
      }.

%% path: listen
listen() ->
    Keys = [<<"http">>, <<"c2s">>, <<"s2s">>, <<"service">>],
    #section{
       items = maps:from_list([{Key, #list{items = listener(Key),
                                           wrap = none}} || Key <- Keys]),
       process = fun mongoose_listener_config:verify_unique_listeners/1,
       wrap = global_config
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
                                                  validate = {enum, [tcp, udp]}},
                           <<"ip_version">> => #option{type = integer,
                                                       validate = {enum, [4, 6]}}
                          },
       format_items = map,
       required = [<<"port">>],
       defaults = #{<<"proto">> => tcp},
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
                                               wrap = {kv, acceptors_num}}
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
                                  wrap = {kv, service_check_from}},
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
                                           validate = filename},
                 <<"cacertfile">> => #option{type = string,
                                             validate = filename},
                 <<"dhfile">> => #option{type = string,
                                         validate = filename},
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
                                          wrap = {kv, crlfiles}},
                 <<"password">> => #option{type = string},
                 <<"server_name_indication">> => #option{type = boolean,
                                                         process = fun ?MODULE:process_sni/1},
                 <<"versions">> => #list{items = #option{type = atom}}
                },
       process = fun ?MODULE:process_xmpp_tls/1
      }.

%% path: listen.s2s[].tls
s2s_tls() ->
    #section{
       items = #{<<"cacertfile">> => #option{type = string,
                                             validate = filename},
                 <<"dhfile">> => #option{type = string,
                                         validate = filename},
                 <<"ciphers">> => #option{type = string},
                 <<"protocol_options">> => #list{items = #option{type = string,
                                                                 validate = non_empty}}
                },
       process = fun ?MODULE:process_fast_tls/1
      }.

%% path: listen.http[].tls
http_listener_tls() ->
    Items = tls_items(),
    #section{
       items = Items#{<<"verify_mode">> => #option{type = atom,
                                                   validate = {enum, [peer, selfsigned_peer, none]}}
                     },
       wrap = {kv, ssl}
      }.

%% path: listen.http[].transport
http_transport() ->
    #section{
       items = #{<<"num_acceptors">> => #option{type = integer,
                                                validate = positive},
                 <<"max_connections">> => #option{type = int_or_infinity,
                                                  validate = non_negative}
                },
       wrap = {kv, transport_options}
      }.

%% path: listen.http[].protocol
http_protocol() ->
    #section{
       items = #{<<"compress">> => #option{type = boolean}},
       wrap = {kv, protocol_options}
      }.

%% path: listen.http[].handlers
http_handlers() ->
    Keys = [<<"mod_websockets">>,
            <<"lasse_handler">>,
            <<"cowboy_static">>,
            <<"mongoose_api">>,
            <<"mongoose_api_admin">>,
            <<"mongoose_domain_handler">>,
            default],
    #section{
       items = maps:from_list([{Key, #list{items = http_handler(Key),
                                           wrap = none}} || Key <- Keys]),
       validate_keys = module,
       wrap = {kv, modules}
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
                                wrap = {kv, ejabberd_service}}};
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
http_handler_items(<<"mongoose_domain_handler">>) ->
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
    Items = maps:from_list([{a2b(Method), ejabberd_auth:config_spec(Method)} ||
                               Method <- all_auth_methods()]),
    #section{
       items = Items#{<<"methods">> => #list{items = #option{type = atom,
                                                             validate = {module, ejabberd_auth}}},
                      <<"password">> => auth_password(),
                      <<"sasl_external">> =>
                          #list{items = #option{type = atom,
                                                process = fun ?MODULE:process_sasl_external/1}},
                      <<"sasl_mechanisms">> =>
                          #list{items = #option{type = atom,
                                                validate = {module, cyrsasl},
                                                process = fun ?MODULE:process_sasl_mechanism/1}}
                     },
       format_items = map,
       defaults = #{<<"sasl_external">> => [standard],
                    <<"sasl_mechanisms">> => cyrsasl:default_modules()},
       process = fun ?MODULE:process_auth/1,
       wrap = host_config
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
                                    },
                 <<"scram_iterations">> => #option{type = integer,
                                                   validate = positive}
                },
       format_items = map,
       defaults = #{<<"format">> => scram,
                    <<"scram_iterations">> => mongoose_scram:iterations()},
       include = always
      }.

%% path: outgoing_pools
outgoing_pools() ->
    PoolTypes = [<<"cassandra">>, <<"elastic">>, <<"http">>, <<"ldap">>,
                 <<"rabbit">>, <<"rdbms">>, <<"redis">>, <<"riak">>],
    Items = [{Type, #section{items = #{default => outgoing_pool(Type)},
                             validate_keys = non_empty,
                             wrap = none}} || Type <- PoolTypes],
    #section{
       items = maps:from_list(Items),
       wrap = global_config
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
       wrap = item
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
                                       wrap = {kv, ssl}}
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
                                       wrap = {kv, server}},
                 <<"path_prefix">> => #option{type = string,
                                              validate = non_empty},
                 <<"request_timeout">> => #option{type = integer,
                                                  validate = non_negative},
                 <<"tls">> => #section{items = tls_items(),
                                       wrap = {kv, http_opts}}
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
                                       wrap = {kv, tls_options}}
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
                                       wrap = none}}
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
       wrap = prepend_key
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
                                   wrap = {kv, verify}},
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
    Services = [{a2b(Service), mongoose_service:config_spec(Service)}
                || Service <- configurable_services()],
    #section{
       items = maps:from_list(Services),
       wrap = global_config
      }.

configurable_services() ->
    [service_admin_extra,
     service_mongoose_system_metrics,
     service_domain_db].

%% path: (host_config[].)modules
modules() ->
    Modules = [{a2b(Module), gen_mod:config_spec(Module)}
               || Module <- configurable_modules()],
    Items = maps:from_list(Modules),
    #section{
       items = Items#{default => #section{items = #{}}},
       validate_keys = module,
       format_items = map,
       wrap = host_config
      }.

configurable_modules() ->
    [mod_adhoc,
     mod_auth_token,
     mod_bosh,
     mod_cache_users,
     mod_caps,
     mod_carboncopy,
     mod_csi,
     mod_disco,
     mod_event_pusher,
     mod_extdisco,
     mod_global_distrib,
     mod_http_upload,
     mod_inbox,
     mod_jingle_sip,
     mod_keystore,
     mod_last,
     mod_mam_meta,
     mod_muc,
     mod_muc_light,
     mod_muc_log,
     mod_offline,
     mod_ping,
     mod_privacy,
     mod_private,
     mod_pubsub,
     mod_push_service_mongoosepush,
     mod_register,
     mod_roster,
     mod_shared_roster_ldap,
     mod_sic,
     mod_stream_management,
     mod_time,
     mod_vcard,
     mod_version,
     mod_domain_isolation].

%% path: (host_config[].)modules.*.iqdisc
iqdisc() ->
    #section{
       items = #{<<"type">> => #option{type = atom,
                                       validate = {enum, [no_queue, one_queue, parallel, queues]}},
                 <<"workers">> => #option{type = integer,
                                          validate = positive}},
       required = [<<"type">>],
       process = fun ?MODULE:process_iqdisc/1
      }.

process_iqdisc(KVs) ->
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
                                                            wrap = {kv, maxrate}}},
                        required = all,
                        process = fun ?MODULE:process_shaper/1,
                        wrap = {host_or_global_config, shaper}
                       }
                },
       validate_keys = non_empty,
       wrap = none
      }.

%% path: (host_config[].)acl
acl() ->
    #section{
       items = #{default => #list{items = acl_item(),
                                  wrap = {host_or_global_config, acl}}
                },
       wrap = none
      }.

%% path: (host_config[].)acl.*[]
acl_item() ->
    #section{
       items = #{<<"match">> => #option{type = atom,
                                        validate = {enum, [all, none]}},
                 <<"user">> => #option{type = binary},
                 <<"server">> => #option{type = binary},
                 <<"resource">> => #option{type = binary},
                 <<"user_regexp">> => #option{type = binary},
                 <<"server_regexp">> => #option{type = binary},
                 <<"resource_regexp">> => #option{type = binary},
                 <<"user_glob">> => #option{type = binary},
                 <<"server_glob">> => #option{type = binary},
                 <<"resource_glob">> => #option{type = binary}
                },
       validate_keys = non_empty,
       process = fun ?MODULE:process_acl_item/1
      }.

%% path: (host_config[].)access
access() ->
    #section{
       items = #{default => #list{items = access_rule_item(),
                                  wrap = {host_or_global_config, access}}
                },
       wrap = none
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
                                               wrap = {global_config, s2s_use_starttls}},
                 <<"certfile">> => #option{type = string,
                                           validate = non_empty,
                                           wrap = {global_config, s2s_certfile}},
                 <<"default_policy">> => #option{type = atom,
                                                 validate = {enum, [allow, deny]},
                                                 wrap = {host_config, s2s_default_policy}},
                 <<"host_policy">> => #list{items = s2s_host_policy(),
                                            format_items = map,
                                            wrap = {host_config, s2s_host_policy}},
                 <<"address">> => #list{items = s2s_address(),
                                        format_items = map,
                                        wrap = {global_config, s2s_address}},
                 <<"ciphers">> => #option{type = string,
                                          wrap = {global_config, s2s_ciphers}},
                 <<"domain_certfile">> => #list{items = s2s_domain_cert(),
                                                format_items = map,
                                                wrap = {global_config, domain_certfile}},
                 <<"shared">> => #option{type = binary,
                                         validate = non_empty,
                                         wrap = {host_config, s2s_shared}},
                 <<"max_retry_delay">> => #option{type = integer,
                                                  validate = positive,
                                                  wrap = {host_config, s2s_max_retry_delay}}
                },
       wrap = none
      }.

%% path: (host_config[].)s2s.dns
s2s_dns() ->
    #section{
       items = #{<<"timeout">> => #option{type = integer,
                                          validate = positive},
                 <<"retries">> => #option{type = integer,
                                          validate = positive}},
       wrap = {global_config, s2s_dns_options}
      }.

%% path: (host_config[].)s2s.outgoing
s2s_outgoing() ->
    #section{
       items = #{<<"port">> => #option{type = integer,
                                       validate = port,
                                       wrap = {global_config, outgoing_s2s_port}},
                 <<"ip_versions">> =>
                     #list{items = #option{type = integer,
                                           validate = {enum, [4, 6]},
                                           process = fun ?MODULE:process_s2s_address_family/1},
                           validate = unique_non_empty,
                           wrap = {global_config, outgoing_s2s_families}},
                 <<"connection_timeout">> => #option{type = int_or_infinity,
                                                     validate = positive,
                                                     wrap = {global_config, outgoing_s2s_timeout}}
                },
       wrap = none
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
       format_items = map,
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
       format_items = map,
       process = fun ?MODULE:process_s2s_address/1
      }.

%% path: (host_config[].)s2s.domain_certfile[]
s2s_domain_cert() ->
    #section{
       items = #{<<"domain">> => #option{type = binary,
                                         validate = non_empty},
                 <<"certfile">> => #option{type = string,
                                           validate = non_empty}},
       required = all,
       format_items = map,
       process = fun ?MODULE:process_s2s_domain_cert/1
      }.

%% Callbacks for 'process'

%% Check that all auth methods and modules enabled for any host type support dynamic domains
process_root(Items) ->
    case proplists:lookup(host_types, Items) of
        {_, [_|_] = HostTypes} ->
            HTItems = lists:filter(fun(Item) -> is_host_type_item(Item, HostTypes) end, Items),
            case {unsupported_auth_methods(HTItems), unsupported_modules(HTItems)} of
                {[], []} ->
                    Items;
                {Methods, Modules} ->
                    error(#{what => dynamic_domains_not_supported,
                            text => ("Dynamic modules not supported by the specified authentication "
                                     "methods and/or extension modules"),
                            unsupported_auth_methods => Methods,
                            unsupported_modules => Modules})
            end;
        _ ->
            Items
    end.

unsupported_auth_methods(KVs) ->
    [Method || Method <- extract_auth_methods(KVs),
               not ejabberd_auth:does_method_support(Method, dynamic_domains)].

unsupported_modules(KVs) ->
    [Module || Module <- extract_modules(KVs),
               not gen_mod:does_module_support(Module, dynamic_domains)].

extract_auth_methods(KVs) ->
    lists:usort(lists:flatmap(fun({{auth, _}, Auth}) -> maps:get(methods, Auth);
                                 (_) -> []
                              end, KVs)).

extract_modules(KVs) ->
    lists:usort(lists:flatmap(fun({{modules, _}, Modules}) -> maps:keys(Modules);
                                 (_) -> []
                              end, KVs)).

is_host_type_item({{_, HostType}, _}, HostTypes) ->
    HostType =:= global orelse lists:member(HostType, HostTypes);
is_host_type_item(_, _) ->
    false.

process_ctl_access_rule(KVs) ->
    Commands = proplists:get_value(commands, KVs, all),
    ArgRestrictions = proplists:get_value(argument_restrictions, KVs, []),
    {Commands, ArgRestrictions}.

process_sm_backend(Backend) ->
    {Backend, []}.

process_host(Host) ->
    Node = jid:nodeprep(Host),
    true = Node =/= error,
    Node.

process_general(General) ->
    hosts_and_host_types_are_unique_and_non_empty(General),
    General.

hosts_and_host_types_are_unique_and_non_empty(General) ->
    AllHostTypes = get_all_hosts_and_host_types(General),
    true = lists:sort(AllHostTypes) =:= lists:usort(AllHostTypes),
    true = [] =/= AllHostTypes.

get_all_hosts_and_host_types(General) ->
    lists:flatmap(fun({Key, Value}) when Key =:= hosts;
                                         Key =:= host_types ->
                          Value;
                     (_) ->
                          []
                  end, General).

process_sni(false) ->
    disable.

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

process_listener([item, Type | _], Opts) ->
    mongoose_listener_config:ensure_ip_options(Opts#{module => listener_module(Type)}).

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
process_http_handler_opts(<<"mongoose_domain_handler">>, Opts) ->
    {[UserOpts, PassOpts], []} = proplists:split(Opts, [username, password]),
    case {UserOpts, PassOpts} of
        {[], []} -> ok;
        {[{username, _User}], [{password, _Pass}]} -> ok;
        _ -> error(#{what => both_username_and_password_required,
                     handler => mongoose_domain_handler, opts => Opts})
    end,
    Opts;
process_http_handler_opts(<<"cowboy_swagger_redirect_handler">>, []) -> #{};
process_http_handler_opts(<<"cowboy_swagger_json_handler">>, []) -> #{};
process_http_handler_opts(_, Opts) -> Opts.

process_sasl_external(V) when V =:= standard;
                              V =:= common_name;
                              V =:= auth_id ->
    V;
process_sasl_external(M) ->
    mongoose_config_validator:validate(M, atom, module),
    {mod, M}.

process_sasl_mechanism(V) ->
    list_to_atom("cyrsasl_" ++ atom_to_list(V)).

process_auth(Opts = #{methods := Methods}) ->
    [check_auth_method(Method, Opts) || Method <- Methods],
    Opts;
process_auth(Opts) ->
    MethodsFromSections = lists:filter(fun(K) -> maps:is_key(K, Opts) end, all_auth_methods()),
    Opts#{methods => MethodsFromSections}.

all_auth_methods() ->
    [anonymous, dummy, external, http, internal, jwt, ldap, pki, rdbms, riak].

check_auth_method(Method, Opts) ->
    case maps:is_key(Method, Opts) of
        true -> ok;
        false -> error(#{what => missing_section_for_auth_method, auth_method => Method})
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
    list_to_tuple([AclName | lists:map(fun(K) ->
                                               prepare_acl_value(proplists:get_value(K, KVs))
                                       end, AclKeys)]).

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

prepare_acl_value(Value) ->
    Node = jid:nodeprep(Value),
    true = Node =/= error,
    Node.

process_access_rule_item(KVs) ->
    {[[{acl, Acl}], [{value, Value}]], []} = proplists:split(KVs, [acl, value]),
    {Value, Acl}.

process_s2s_address_family(4) -> ipv4;
process_s2s_address_family(6) -> ipv6.

process_s2s_host_policy(#{host := S2SHost, policy := Policy}) ->
    {S2SHost, Policy}.

process_s2s_address(#{host := S2SHost, ip_address := IPAddr, port := Port}) ->
    {S2SHost, {IPAddr, Port}};
process_s2s_address(#{host := S2SHost, ip_address := IPAddr}) ->
    {S2SHost, IPAddr}.

process_s2s_domain_cert(#{domain := Domain, certfile := Certfile}) ->
    {Domain, Certfile}.
