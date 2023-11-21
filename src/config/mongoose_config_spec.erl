-module(mongoose_config_spec).

%% entry point - returns the entire spec
-export([root/0]).

%% spec parts used by http handlers, modules and services
-export([wpool/1,
         iqdisc/0,
         tls/2]).

%% callbacks for the 'process' step
-export([process_root/1,
         process_host/1,
         process_general/1,
         process_listener/2,
         process_c2s_tls/1,
         process_fast_tls/1,
         process_sasl_external/1,
         process_sasl_mechanism/1,
         process_auth/1,
         process_pool/2,
         process_ldap_connection/1,
         process_iqdisc/1,
         process_acl_condition/1,
         process_s2s_host_policy/1,
         process_s2s_address/1,
         process_domain_cert/1,
         process_infinity_as_zero/1]).

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
        global_config % [{Key, Value}]
      | host_config. % Inside host_config: [{{Key, Host}, Value}]
                     % Otherwise: one such option for each configured host

%% Wrap the value in a nested config part - key-value pair or just a value
-type config_part_wrapper() ::
        default      % [{Key, Value}] for section items, [Value] for list items
      | item         % [Value]
      | remove       % [] - the item is ignored
      | none.        % just Value - injects elements of Value into the parent section/list

%% This option allows to put list/section items in a map
-type format_items() ::
        list         % keep the processed items in a list
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
    S2S = s2s(),
    #section{
       items = #{<<"general">> => General#section{required = [<<"default_server_domain">>],
                                                  process = fun ?MODULE:process_general/1,
                                                  defaults = general_defaults()},
                 <<"listen">> => Listen#section{include = always},
                 <<"auth">> => Auth#section{include = always},
                 <<"outgoing_pools">> => outgoing_pools(),
                 <<"internal_databases">> => internal_databases(),
                 <<"services">> => services(),
                 <<"modules">> => Modules#section{include = always},
                 <<"shaper">> => shaper(),
                 <<"acl">> => acl(),
                 <<"access">> => access(),
                 <<"s2s">> => S2S#section{include = always},
                 <<"host_config">> => #list{items = host_config(),
                                            wrap = none}
                },
       defaults = #{<<"internal_databases">> => default_internal_databases()},
       required = [<<"general">>],
       process = fun ?MODULE:process_root/1,
       wrap = none,
       format_items = list
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
                 %% but only options with 'wrap = host_config' are accepted.
                 %% Options with 'wrap = global_config' would be caught by
                 %%   mongoose_config_parser_toml:wrap/3
                 <<"general">> => general(),
                 <<"auth">> => auth(),
                 <<"modules">> => modules(),
                 <<"acl">> => acl(),
                 <<"access">> => access(),
                 <<"s2s">> => s2s()
                },
       wrap = none,
       format_items = list
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
                                             wrap = global_config},
                 <<"component_backend">> => #option{type = atom,
                                                    validate = {module, mongoose_component},
                                                    wrap = global_config},
                 <<"s2s_backend">> => #option{type = atom,
                                              validate = {module, mongoose_s2s},
                                              wrap = global_config},
                 <<"max_fsm_queue">> => #option{type = integer,
                                                validate = positive,
                                                wrap = global_config},
                 <<"http_server_name">> => #option{type = string,
                                                   wrap = global_config},
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
                                                process = fun xmpp_router:expand_routing_modules/1,
                                                wrap = global_config},
                 <<"replaced_wait_timeout">> => #option{type = integer,
                                                        validate = positive,
                                                        wrap = host_config},
                 <<"hide_service_name">> => #option{type = boolean,
                                                    wrap = global_config},
                 <<"domain_certfile">> => #list{items = domain_cert(),
                                                format_items = map,
                                                wrap = global_config}
                },
       wrap = none,
       format_items = list
      }.

general_defaults() ->
    #{<<"loglevel">> => warning,
      <<"hosts">> => [],
      <<"host_types">> => [],
      <<"registration_timeout">> => 600,
      <<"language">> => <<"en">>,
      <<"all_metrics_are_global">> => false,
      <<"sm_backend">> => mnesia,
      <<"component_backend">> => mnesia,
      <<"s2s_backend">> => mnesia,
      <<"rdbms_server_type">> => generic,
      <<"mongooseimctl_access_commands">> => #{},
      <<"routing_modules">> => mongoose_router:default_routing_modules(),
      <<"replaced_wait_timeout">> => 2000,
      <<"hide_service_name">> => false}.

ctl_access_rule() ->
    #section{
       items = #{<<"commands">> => #list{items = #option{type = atom,
                                                         validate = non_empty}},
                 <<"argument_restrictions">> =>
                     #section{items = #{default => #option{type = string}}}
                },
       defaults = #{<<"commands">> => all,
                    <<"argument_restrictions">> => #{}}
      }.

%% path: general.domain_certfile
domain_cert() ->
    #section{
       items = #{<<"domain">> => #option{type = binary,
                                         validate = non_empty},
                 <<"certfile">> => #option{type = string,
                                           validate = filename}},
       required = all,
       process = fun ?MODULE:process_domain_cert/1
      }.

%% path: listen
listen() ->
    Keys = [c2s, s2s, service, http],
    #section{
       items = maps:from_list([{atom_to_binary(Key), #list{items = listener(Key), wrap = none}}
                               || Key <- Keys]),
       process = fun mongoose_listener_config:verify_unique_listeners/1,
       wrap = global_config,
       format_items = list
      }.

%% path: listen.*[]
listener(Type) ->
    mongoose_config_utils:merge_sections(listener_common(), listener_extra(Type)).

listener_common() ->
    #section{items = #{<<"port">> => #option{type = integer,
                                             validate = port},
                       <<"ip_address">> => #option{type = string,
                                                   validate = ip_address},
                       <<"proto">> => #option{type = atom,
                                              validate = {enum, [tcp]}},
                       <<"ip_version">> => #option{type = integer,
                                                   validate = {enum, [4, 6]}}
                      },
             required = [<<"port">>],
             defaults = #{<<"proto">> => tcp},
             process = fun ?MODULE:process_listener/2
            }.

listener_extra(http) ->
    %% options listed here are passed to ranch_ssl (with verify_mode translated to verify_fun)
    TLSKeys = [verify_mode, certfile, cacertfile, ciphers, keyfile, password, versions, dhfile],
    TLSSection = mongoose_config_utils:section_with_keys(TLSKeys, tls([server], [just_tls])),
    #section{items = #{<<"tls">> => TLSSection,
                       <<"transport">> => http_transport(),
                       <<"protocol">> => http_protocol(),
                       <<"handlers">> => mongoose_http_handler:config_spec()}};
listener_extra(Type) ->
    mongoose_config_utils:merge_sections(xmpp_listener_common(), xmpp_listener_extra(Type)).

xmpp_listener_common() ->
    #section{items = #{<<"backlog">> => #option{type = integer,
                                                validate = non_negative},
                       <<"proxy_protocol">> => #option{type = boolean},
                       <<"hibernate_after">> => #option{type = int_or_infinity,
                                                        validate = non_negative},
                       <<"max_stanza_size">> => #option{type = int_or_infinity,
                                                        validate = positive,
                                                        process = fun ?MODULE:process_infinity_as_zero/1},
                       <<"num_acceptors">> => #option{type = integer,
                                                      validate = positive}
                      },
             defaults = #{<<"backlog">> => 1024,
                          <<"proxy_protocol">> => false,
                          <<"hibernate_after">> => 0,
                          <<"max_stanza_size">> => 0,
                          <<"num_acceptors">> => 100}
            }.

xmpp_listener_extra(c2s) ->
    #section{items = #{<<"access">> => #option{type = atom,
                                               validate = non_empty},
                       <<"shaper">> => #option{type = atom,
                                               validate = non_empty},
                       <<"max_connections">> => #option{type = int_or_infinity,
                                                        validate = positive},
                       <<"c2s_state_timeout">> => #option{type = int_or_infinity,
                                                          validate = non_negative},
                       <<"reuse_port">> => #option{type = boolean},
                       <<"backwards_compatible_session">> => #option{type = boolean},
                       <<"allowed_auth_methods">> =>
                           #list{items = #option{type = atom,
                                                 validate = {module, ejabberd_auth}},
                                 validate = unique},
                       <<"tls">> => c2s_tls()},
             defaults = #{<<"access">> => all,
                          <<"shaper">> => none,
                          <<"max_connections">> => infinity,
                          <<"c2s_state_timeout">> => 5000,
                          <<"reuse_port">> => false,
                          <<"backwards_compatible_session">> => true}
            };
xmpp_listener_extra(s2s) ->
    TLSSection = tls([server], [fast_tls]),
    #section{items = #{<<"shaper">> => #option{type = atom,
                                               validate = non_empty},
                       <<"tls">> => TLSSection#section{include = always,
                                                       process = fun ?MODULE:process_fast_tls/1}},
             defaults = #{<<"shaper">> => none}
            };
xmpp_listener_extra(service) ->
    #section{items = #{<<"access">> => #option{type = atom,
                                               validate = non_empty},
                       <<"shaper_rule">> => #option{type = atom,
                                                    validate = non_empty},
                       <<"check_from">> => #option{type = boolean},
                       <<"hidden_components">> => #option{type = boolean},
                       <<"conflict_behaviour">> => #option{type = atom,
                                                           validate = {enum, [kick_old, disconnect]}},
                       <<"password">> => #option{type = string,
                                                 validate = non_empty},
                       <<"max_fsm_queue">> => #option{type = integer,
                                                      validate = positive}
                      },
             required = [<<"password">>],
             defaults = #{<<"access">> => all,
                          <<"shaper_rule">> => none,
                          <<"check_from">> => true,
                          <<"hidden_components">> => false,
                          <<"conflict_behaviour">> => disconnect}
            }.

%% path: listen.c2s[].tls
c2s_tls() ->
    mongoose_config_utils:merge_sections(tls([server], [fast_tls, just_tls]), c2s_tls_extra()).

c2s_tls_extra() ->
    #section{items = #{<<"module">> => #option{type = atom,
                                               validate = {enum, [fast_tls, just_tls]}},
                       <<"mode">> => #option{type = atom,
                                             validate = {enum, [tls, starttls, starttls_required]}}
                      },
             defaults = #{<<"module">> => fast_tls,
                          <<"mode">> => starttls},
             process = fun ?MODULE:process_c2s_tls/1}.

%% path: listen.http[].transport
http_transport() ->
    #section{
       items = #{<<"num_acceptors">> => #option{type = integer,
                                                validate = positive},
                 <<"max_connections">> => #option{type = int_or_infinity,
                                                  validate = non_negative}
                },
       defaults = #{<<"num_acceptors">> => 100,
                    <<"max_connections">> => 1024},
       include = always
      }.

%% path: listen.http[].protocol
http_protocol() ->
    #section{
       items = #{<<"compress">> => #option{type = boolean}},
       defaults = #{<<"compress">> => false},
       include = always
      }.

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
                                                process = fun ?MODULE:process_sasl_mechanism/1}},
                      <<"max_users_per_domain">> => #option{type = int_or_infinity,
                                                            validate = positive}
                     },
       defaults = #{<<"sasl_external">> => [standard],
                    <<"sasl_mechanisms">> => cyrsasl:default_modules(),
                    <<"max_users_per_domain">> => infinity},
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
       defaults = #{<<"format">> => scram,
                    <<"scram_iterations">> => mongoose_scram:iterations()},
       include = always
      }.

%% path: internal_databases
internal_databases() ->
    Items = #{<<"cets">> => internal_database_cets(),
              <<"mnesia">> => internal_database_mnesia()},
    #section{items = Items,
             format_items = map,
             wrap = global_config}.

default_internal_databases() ->
    #{mnesia => #{}}.

%% path: internal_databases.cets
internal_database_cets() ->
    #section{
       items = #{<<"backend">> => #option{type = atom,
                                          validate = {enum, [file, rdbms]}},
                 <<"cluster_name">> => #option{type = atom, validate = non_empty},
                 %% Relative to the release directory (or an absolute name)
                 <<"node_list_file">> => #option{type = string,
                                                 validate = filename}
                },
       defaults = #{<<"backend">> => rdbms, <<"cluster_name">> => mongooseim}
      }.

%% path: internal_databases.mnesia
internal_database_mnesia() ->
    #section{}.

%% path: outgoing_pools
outgoing_pools() ->
    PoolTypes = [<<"cassandra">>, <<"elastic">>, <<"http">>, <<"ldap">>,
                 <<"rabbit">>, <<"rdbms">>, <<"redis">>],
    Items = [{Type, #section{items = #{default => outgoing_pool(Type)},
                             validate_keys = non_empty,
                             wrap = none,
                             format_items = list}} || Type <- PoolTypes],
    #section{items = maps:from_list(Items),
             format_items = list,
             wrap = global_config,
             include = always}.

%% path: outgoing_pools.*.*
outgoing_pool(Type) ->
    ExtraDefaults = extra_wpool_defaults(Type),
    Pool = mongoose_config_utils:merge_sections(wpool(ExtraDefaults), outgoing_pool_extra(Type)),
    Pool#section{wrap = item}.

extra_wpool_defaults(<<"cassandra">>) ->
    #{<<"workers">> => 20};
extra_wpool_defaults(<<"rdbms">>) ->
    #{<<"call_timeout">> => 60000};
extra_wpool_defaults(_) ->
    #{}.

wpool(ExtraDefaults) ->
    #section{items = #{<<"workers">> => #option{type = integer,
                                                validate = positive},
                       <<"strategy">> => #option{type = atom,
                                                 validate = {enum, wpool_strategy_values()}},
                       <<"call_timeout">> => #option{type = integer,
                                                     validate = positive}
                      },
             defaults = maps:merge(#{<<"workers">> => 10,
                                     <<"strategy">> => best_worker,
                                     <<"call_timeout">> => 5000}, ExtraDefaults)}.

outgoing_pool_extra(Type) ->
    #section{items = #{<<"scope">> => #option{type = atom,
                                              validate = {enum, [global, host, single_host]}},
                       <<"host">> => #option{type = binary,
                                             validate = non_empty},
                       <<"connection">> => outgoing_pool_connection(Type)
                      },
             process = fun ?MODULE:process_pool/2,
             defaults = #{<<"scope">> => global}
            }.

%% path: outgoing_pools.*.*.connection
outgoing_pool_connection(<<"cassandra">>) ->
    #section{
       items = #{<<"servers">> => #list{items = cassandra_server(),
                                        validate = unique_non_empty},
                 <<"keyspace">> => #option{type = atom,
                                           validate = non_empty},
                 <<"auth">> => #section{items = #{<<"plain">> => cassandra_auth_plain()},
                                        required = all},
                 <<"tls">> => tls([client], [just_tls])
                },
       include = always,
       defaults = #{<<"servers">> => [#{host => "localhost", port => 9042}],
                    <<"keyspace">> => mongooseim}
      };
outgoing_pool_connection(<<"elastic">>) ->
    #section{
       items = #{<<"host">> => #option{type = binary,
                                       validate = non_empty},
                 <<"port">> => #option{type = integer,
                                       validate = port}
                },
       include = always,
       defaults = #{<<"host">> => <<"localhost">>,
                    <<"port">> => 9200}
      };
outgoing_pool_connection(<<"http">>) ->
    #section{
       items = #{<<"host">> => #option{type = string,
                                       validate = non_empty},
                 <<"path_prefix">> => #option{type = binary,
                                              validate = non_empty},
                 <<"request_timeout">> => #option{type = integer,
                                                  validate = non_negative},
                 <<"tls">> => tls([client], [just_tls])
                },
       include = always,
       required = [<<"host">>],
       defaults = #{<<"path_prefix">> => <<"/">>,
                    <<"request_timeout">> => 2000}
      };
outgoing_pool_connection(<<"ldap">>) ->
    #section{
       items = #{<<"servers">> => #list{items = #option{type = string},
                                        validate = unique_non_empty},
                 <<"port">> => #option{type = integer,
                                       validate = port},
                 <<"root_dn">> => #option{type = binary},
                 <<"password">> => #option{type = binary},
                 <<"connect_interval">> => #option{type = integer,
                                                   validate = positive},
                 <<"tls">> => tls([client], [just_tls])
                },
       include = always,
       defaults = #{<<"servers">> => ["localhost"],
                    <<"root_dn">> => <<>>,
                    <<"password">> => <<>>,
                    <<"connect_interval">> => 10000},
       process = fun ?MODULE:process_ldap_connection/1
      };
outgoing_pool_connection(<<"rabbit">>) ->
    #section{
       items = #{<<"host">> => #option{type = string,
                                       validate = non_empty},
                 <<"port">> => #option{type = integer,
                                       validate = port},
                 <<"username">> => #option{type = binary,
                                           validate = non_empty},
                 <<"password">> => #option{type = binary,
                                           validate = non_empty},
                 <<"confirms_enabled">> => #option{type = boolean},
                 <<"max_worker_queue_len">> => #option{type = int_or_infinity,
                                                       validate = non_negative}
                },
       include = always,
       defaults = #{<<"host">> => "localhost",
                    <<"port">> => 5672,
                    <<"username">> => <<"guest">>,
                    <<"password">> => <<"guest">>,
                    <<"confirms_enabled">> => false,
                    <<"max_worker_queue_len">> => 1000}
      };
outgoing_pool_connection(<<"rdbms">>) ->
    #section{
       items = #{<<"driver">> => #option{type = atom,
                                         validate = {enum, [odbc, pgsql, mysql]}},
                 <<"keepalive_interval">> => #option{type = integer,
                                                     validate = positive},
                 <<"query_timeout">> => #option{type = integer,
                                                validate = non_negative},
                 <<"max_start_interval">> => #option{type = integer,
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
       required = [<<"driver">>],
       defaults = #{<<"query_timeout">> => 5000,
                    <<"max_start_interval">> => 30},
       process = fun mongoose_rdbms:process_options/1
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
                },
       include = always,
       defaults = #{<<"host">> => "127.0.0.1",
                    <<"port">> => 6379,
                    <<"database">> => 0,
                    <<"password">> => ""}
      }.

cassandra_server() ->
    #section{
       items = #{<<"host">> => #option{type = string,
                                       validate = non_empty},
                 <<"port">> => #option{type = integer,
                                       validate = port}},
       required = [<<"host">>],
       defaults = #{<<"port">> => 9042}
      }.

%% path: outgoing_pools.cassandra.*.connection.auth.plain
cassandra_auth_plain() ->
    #section{
       items = #{<<"username">> => #option{type = binary},
                 <<"password">> => #option{type = binary}},
       required = all
      }.

%% path: outgoing_pools.rdbms.*.connection.tls
sql_tls() ->
    mongoose_config_utils:merge_sections(tls([client], [just_tls]), sql_tls_extra()).

sql_tls_extra() ->
    #section{items = #{<<"required">> => #option{type = boolean}}}.

%% TLS options

tls(Entities, Modules) when is_list(Entities), is_list(Modules) ->
    Sections = [tls(Entity, Module) || Entity <- [common | Entities],
                                       Module <- [common | Modules]],
    lists:foldl(fun mongoose_config_utils:merge_sections/2, hd(Sections), tl(Sections));
tls(common, common) ->
    #section{items = #{<<"verify_mode">> => #option{type = atom,
                                                    validate = {enum, [peer, selfsigned_peer, none]}},
                       <<"certfile">> => #option{type = string,
                                                 validate = filename},
                       <<"cacertfile">> => #option{type = string,
                                                   validate = filename},
                       <<"ciphers">> => #option{type = string}
                      },
             defaults = #{<<"verify_mode">> => peer}};
tls(common, fast_tls) ->
    #section{items = #{<<"protocol_options">> => #list{items = #option{type = string,
                                                                       validate = non_empty}}}};
tls(common, just_tls) ->
    #section{items = #{<<"keyfile">> => #option{type = string,
                                                validate = filename},
                       <<"password">> => #option{type = string},
                       <<"versions">> => #list{items = #option{type = atom}}}};
tls(server, common) ->
    #section{items = #{<<"dhfile">> => #option{type = string,
                                               validate = filename}}};
tls(server, fast_tls) ->
    #section{};
tls(server, just_tls) ->
    #section{items = #{<<"disconnect_on_failure">> => #option{type = boolean},
                       <<"crl_files">> => #list{items = #option{type = string,
                                                                validate = filename}}}};
tls(client, common) ->
    #section{};
tls(client, fast_tls) ->
    #section{};
tls(client, just_tls) ->
    #section{items = #{<<"server_name_indication">> => server_name_indication()}}.

server_name_indication() ->
    #section{items = #{<<"enabled">> => #option{type = boolean},
                       <<"host">> => #option{type = string,
                                             validate = non_empty},
                       <<"protocol">> => #option{type = atom,
                                                 validate = {enum, [default, https]}}
                      },
             defaults = #{<<"enabled">> => true,
                          <<"protocol">> => default},
             include = always}.

%% path: (host_config[].)services
services() ->
    Services = [{a2b(Service), mongoose_service:config_spec(Service)}
                || Service <- configurable_services()],
    #section{
       items = maps:from_list(Services),
       wrap = global_config,
       include = always
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
       items = Items#{default => #section{}},
       validate_keys = module,
       wrap = host_config
      }.

configurable_modules() ->
    [mod_adhoc,
     mod_auth_token,
     mod_blocking,
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
     mod_mam,
     mod_muc,
     mod_muc_light,
     mod_muc_log,
     mod_offline,
     mod_offline_chatmarkers,
     mod_ping,
     mod_privacy,
     mod_private,
     mod_pubsub,
     mod_push_service_mongoosepush,
     mod_register,
     mod_roster,
     mod_shared_roster_ldap,
     mod_smart_markers,
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

process_iqdisc(#{type := Type, workers := N}) -> {queues = Type, N};
process_iqdisc(#{type := Type}) -> Type.

%% path: shaper
shaper() ->
    #section{
       items = #{default =>
                     #section{
                        items = #{<<"max_rate">> => #option{type = integer,
                                                            validate = positive}},
                        required = all
                       }
                },
       validate_keys = non_empty,
       wrap = global_config
      }.

%% path: (host_config[].)acl
acl() ->
    #section{
       items = #{default => #list{items = acl_item()}},
       wrap = host_config
      }.

%% path: (host_config[].)acl.*[]
acl_item() ->
    Match = #option{type = atom,
                    validate = {enum, [all, none, current_domain, any_hosted_domain]}},
    Cond = #option{type = binary,
                   process = fun ?MODULE:process_acl_condition/1},
    #section{
       items = #{<<"match">> => Match,
                 <<"user">> => Cond,
                 <<"server">> => Cond,
                 <<"resource">> => Cond,
                 <<"user_regexp">> => Cond,
                 <<"server_regexp">> => Cond,
                 <<"resource_regexp">> => Cond,
                 <<"user_glob">> => Cond,
                 <<"server_glob">> => Cond,
                 <<"resource_glob">> => Cond
                },
       defaults = #{<<"match">> => current_domain}
      }.

%% path: (host_config[].)access
access() ->
    #section{
       items = #{default => #list{items = access_rule_item()}},
       wrap = host_config
      }.

%% path: (host_config[].)access.*[]
access_rule_item() ->
    #section{
       items = #{<<"acl">> => #option{type = atom,
                                      validate = non_empty},
                 <<"value">> => #option{type = int_or_atom}
                },
       required = all
      }.

%% path: (host_config[].)s2s
s2s() ->
    #section{
       items = #{<<"default_policy">> => #option{type = atom,
                                                 validate = {enum, [allow, deny]}},
                 <<"host_policy">> => #list{items = s2s_host_policy(),
                                            format_items = map},
                 <<"use_starttls">> => #option{type = atom,
                                               validate = {enum, [false, optional, required,
                                                                  required_trusted]}},
                 <<"certfile">> => #option{type = string,
                                           validate = filename},
                 <<"shared">> => #option{type = binary,
                                         validate = non_empty},
                 <<"address">> => #list{items = s2s_address(),
                                        format_items = map},
                 <<"ciphers">> => #option{type = string},
                 <<"max_retry_delay">> => #option{type = integer,
                                                  validate = positive},
                 <<"outgoing">> => s2s_outgoing(),
                 <<"dns">> => s2s_dns()},
       defaults = #{<<"default_policy">> => allow,
                    <<"use_starttls">> => false,
                    <<"ciphers">> => mongoose_tls:default_ciphers(),
                    <<"max_retry_delay">> => 300},
       wrap = host_config
      }.

%% path: (host_config[].)s2s.dns
s2s_dns() ->
    #section{
       items = #{<<"timeout">> => #option{type = integer,
                                          validate = positive},
                 <<"retries">> => #option{type = integer,
                                          validate = positive}},
       include = always,
       defaults = #{<<"timeout">> => 10,
                    <<"retries">> => 2}
      }.

%% path: (host_config[].)s2s.outgoing
s2s_outgoing() ->
    #section{
       items = #{<<"port">> => #option{type = integer,
                                       validate = port},
                 <<"ip_versions">> =>
                     #list{items = #option{type = integer,
                                           validate = {enum, [4, 6]}},
                           validate = unique_non_empty},
                 <<"connection_timeout">> => #option{type = int_or_infinity,
                                                     validate = positive}
                },
       include = always,
       defaults = #{<<"port">> => 5269,
                    <<"ip_versions">> => [4, 6],
                    <<"connection_timeout">> => 10000}
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

process_c2s_tls(M = #{module := Module}) ->
    check_tls_keys(M, [module, mode]),
    process_tls(Module, M).

process_fast_tls(M) ->
    process_tls(fast_tls, M).

process_tls(Module, M) ->
    check_tls_verify_mode(Module, M),
    maps:merge(tls_defaults(Module), M).

%% The user chooses just_tls or fast_tls, and this choice limits the allowed keys
check_tls_keys(M = #{module := Module}, ExtraKeys) ->
    AllowedItems = (tls([server], [Module]))#section.items,
    AllowedKeys = [binary_to_atom(Key) || Key <- maps:keys(AllowedItems)] ++ ExtraKeys,
    case maps:keys(M) -- AllowedKeys of
        [] -> ok;
        UnexpectedKeys -> error(#{what => unexpected_tls_options,
                                  tls_module => Module,
                                  unexpected_keys => UnexpectedKeys})
    end.

tls_defaults(just_tls) ->
    #{crl_files => [],
      disconnect_on_failure => true};
tls_defaults(fast_tls) ->
    #{ciphers => mongoose_tls:default_ciphers(),
      protocol_options => ["no_sslv2", "no_sslv3", "no_tlsv1", "no_tlsv1_1"]}.

check_tls_verify_mode(fast_tls, #{verify_mode := selfsigned_peer}) ->
    error(#{what => invalid_tls_verify_mode,
            text => <<"fast_tls does not support self-signed certificate verification">>});
check_tls_verify_mode(_Module, #{}) ->
    ok.

process_listener([item, Type | _], Opts) ->
    mongoose_listener_config:ensure_ip_options(Opts#{module => listener_module(Type),
                                                     connection_type => connection_type(Type)}).

listener_module(<<"http">>) -> ejabberd_cowboy;
listener_module(<<"c2s">>) -> mongoose_c2s_listener;
listener_module(<<"s2s">>) -> ejabberd_s2s_in;
listener_module(<<"service">>) -> ejabberd_service.

%% required for correct metrics reporting by mongoose_transport module
connection_type(<<"s2s">>) -> s2s;
connection_type(<<"service">>) -> component;
connection_type(_) -> undefined.

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
    [anonymous, dummy, external, http, internal, jwt, ldap, pki, rdbms].

check_auth_method(Method, Opts) ->
    case maps:is_key(Method, Opts) of
        true -> ok;
        false -> error(#{what => missing_section_for_auth_method, auth_method => Method})
    end.

process_pool([Tag, Type|_], AllOpts = #{scope := ScopeIn, connection := Connection}) ->
    Scope = pool_scope(ScopeIn, maps:get(host, AllOpts, none)),
    Opts = maps:without([scope, host, connection], AllOpts),
    #{type => b2a(Type),
      scope => Scope,
      tag => b2a(Tag),
      opts => Opts,
      conn_opts => Connection}.

pool_scope(single_host, none) ->
    error(#{what => pool_single_host_not_specified,
            text => <<"\"host\" option is required if \"single_host\" is used.">>});
pool_scope(single_host, Host) -> Host;
pool_scope(host, none) -> host;
pool_scope(global, none) -> global.

process_ldap_connection(ConnOpts = #{port := _}) -> ConnOpts;
process_ldap_connection(ConnOpts = #{tls := _}) -> ConnOpts#{port => 636};
process_ldap_connection(ConnOpts) -> ConnOpts#{port => 389}.

b2a(B) -> binary_to_atom(B, utf8).

a2b(A) -> atom_to_binary(A, utf8).

wpool_strategy_values() ->
    [best_worker, random_worker, next_worker, available_worker, next_available_worker].

process_acl_condition(Value) ->
    case jid:nodeprep(Value) of
        error -> error(#{what => incorrect_acl_condition_value,
                         text => <<"Value could not be parsed as a JID node part">>});
        Node -> Node
    end.

process_s2s_host_policy(#{host := S2SHost, policy := Policy}) ->
    {S2SHost, Policy}.

process_s2s_address(M) ->
    maps:take(host, M).

process_domain_cert(#{domain := Domain, certfile := Certfile}) ->
    {Domain, Certfile}.

process_infinity_as_zero(infinity) -> 0;
process_infinity_as_zero(Num) -> Num.
