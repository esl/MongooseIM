%% @doc Config parsing and processing for the TOML format
-module(mongoose_config_parser_toml).

-behaviour(mongoose_config_parser).

-export([parse_file/1]).

-include("mongoose.hrl").
-include("ejabberd_config.hrl").

%% Used to create per-host config when the list of hosts is not known yet
-define(HOST_F(Expr), [fun(Host) -> Expr end]).

%% Input: TOML parsed by tomerl
-type toml_key() :: binary().
-type toml_value() :: tomerl:value().
-type toml_section() :: tomerl:section().

%% Output: list of config records, containing key-value pairs
-type option() :: term(). % any part of a config value, which can be a complex term
-type config() :: #config{} | #local_config{} | acl:acl().
-type config_list() :: [config() | fun((ejabberd:server()) -> [config()])]. % see HOST_F

-spec parse_file(FileName :: string()) -> mongoose_config_parser:state().
parse_file(FileName) ->
    {ok, Content} = tomerl:read_file(FileName),
    Config = parse(Content),
    [Hosts] = lists:filtermap(fun(#config{key = hosts, value = Hosts}) ->
                                      {true, Hosts};
                                 (_) -> false
                              end, Config),
    {FOpts, Opts} = lists:partition(fun(Opt) -> is_function(Opt, 1) end, Config),
    HOpts = lists:flatmap(fun(F) -> lists:flatmap(F, Hosts) end, FOpts),
    lists:foldl(fun(F, StateIn) -> F(StateIn) end,
                mongoose_config_parser:new_state(),
                [fun(S) -> mongoose_config_parser:set_hosts(Hosts, S) end,
                 fun(S) -> mongoose_config_parser:set_opts(Opts ++ HOpts, S) end,
                 fun mongoose_config_parser:dedup_state_opts/1,
                 fun mongoose_config_parser:add_dep_modules/1,
                 fun set_overrides/1]).

%% Config processing functions are annotated with paths of the parsed TOML
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

%% root path
-spec parse(toml_section()) -> config_list().
parse(Content) ->
    parse_map(fun process_section/2, Content).

%% path: *
-spec process_section(toml_key(), toml_section() | [toml_section()]) -> config_list().
process_section(<<"general">>, Content) ->
    parse_map(fun process_general/2, Content);
process_section(<<"listen">>, Content) ->
    Listeners = parse_map(fun process_listener_type/2, Content),
    [#local_config{key = listen, value = Listeners}];
process_section(<<"auth">>, Content) ->
    AuthOpts = parse_map(fun auth_option/2, Content),
    ?HOST_F(partition_auth_opts(AuthOpts, Host));
process_section(<<"outgoing_pools">>, Content) ->
    Pools = parse_map(fun process_pool_type/2, Content),
    [#local_config{key = outgoing_pools, value = Pools}];
process_section(<<"services">>, Content) ->
    Services = parse_map(fun process_service/2, Content),
    [#local_config{key = services, value = Services}];
process_section(<<"modules">>, Content) ->
    Mods = parse_map(fun process_module/2, Content),
    ?HOST_F([#local_config{key = {modules, Host}, value = Mods}]);
process_section(<<"shaper">>, Content) ->
    parse_map(fun(Name, C) -> process_shaper(Name, global, C) end, Content);
process_section(<<"acl">>, Content) ->
    parse_map(fun(Name, C) -> process_acl(Name, global, C) end, Content);
process_section(<<"access">>, Content) ->
    parse_map(fun(Name, C) -> process_access_rule(Name, global, C) end, Content);
process_section(<<"s2s">>, Content) ->
    parse_map(fun process_s2s_option/2, Content);
process_section(<<"host_config">>, Content) ->
    parse_list(fun process_host_item/1, Content).

%% path: general.*
-spec process_general(toml_key(), toml_value()) -> [config()].
process_general(<<"loglevel">>, V) ->
    [#local_config{key = loglevel, value = b2a(V)}];
process_general(<<"hosts">>, Hosts) ->
    [#config{key = hosts, value = [jid:nodeprep(H) || H <- Hosts]}];
process_general(<<"registration_timeout">>, <<"infinity">>) ->
    [#local_config{key = registration_timeout, value = infinity}];
process_general(<<"registration_timeout">>, V) ->
    [#local_config{key = registration_timeout, value = V}];
process_general(<<"language">>, V) ->
    [#config{key = language, value = V}];
process_general(<<"all_metrics_are_global">>, V) ->
    [#local_config{key = all_metrics_are_global, value = V}];
process_general(<<"sm_backend">>, V) ->
    [#config{key = sm_backend, value = {b2a(V), []}}];
process_general(<<"max_fsm_queue">>, V) ->
    [#local_config{key = max_fsm_queue, value = V}];
process_general(<<"http_server_name">>, V) ->
    [#local_config{key = cowboy_server_name, value = b2l(V)}];
process_general(<<"rdbms_server_type">>, V) ->
    [#local_config{key = rdbms_server_type, value = b2a(V)}];
process_general(<<"override_local">>, V) ->
    [#local_config{key = override_local, value = V}];
process_general(<<"override_global">>, V) ->
    [#local_config{key = override_global, value = V}];
process_general(<<"override_acls">>, V) ->
    [#local_config{key = override_acls, value = V}];
process_general(<<"pgsql_users_number_estimate">>, V) ->
    ?HOST_F([#local_config{key = {pgsql_users_number_estimate, Host}, value = V}]);
process_general(<<"route_subdomain">>, V) ->
    ?HOST_F([#local_config{key = {route_subdomain, Host}, value = b2a(V)}]);
process_general(<<"mongooseimctl_access_commands">>, Rules) ->
    [#local_config{key = mongooseimctl_access_commands, 
        value = lists:map(fun process_rule/1, Rules)}];
process_general(<<"routing_modules">>, Mods) ->
    [#local_config{key = routing_modules, value = lists:map(fun b2a/1, Mods)}];
process_general(<<"replaced_wait_timeout">>, V) ->
    ?HOST_F([#local_config{key = {replaced_wait_timeout, Host}, value = V}]);
process_general(<<"hide_service_name">>, V) ->
    ?HOST_F([#local_config{key = {hide_service_name, Host}, value = V}]).

process_rule(#{<<"access_rule">> := Rule, 
    <<"argument_restrictions">> := Arg, <<"commands">> := Comms}) ->
        {
            b2a(Rule),
            lists:map(fun b2l/1, Comms),
            [list_to_tuple(lists:map(fun b2l/1, Arg))]
            }.

%% path: listen.*
-spec process_listener_type(toml_key(), [toml_section()]) -> [option()].
process_listener_type(Type, Content) ->
    parse_list(fun(L) -> process_listener(Type, L) end, Content).

%% path: listen.*[]
-spec process_listener(toml_key(), toml_section()) -> [option()].
process_listener(Type, Content) ->
    Options = maps:without([<<"port">>, <<"ip_address">>], Content),
    PortIP = listener_portip(Content),
    Opts = parse_map(fun(K, V) -> listener_opt(Type, K, V) end, Options),
    {Port, IPT, _, _, Proto, OptsClean} =
        ejabberd_listener:parse_listener_portip(PortIP, Opts),
    [{{Port, IPT, Proto}, listener_module(Type), OptsClean}].

-spec listener_portip(toml_section()) -> option().
listener_portip(#{<<"port">> := Port, <<"ip_address">> := Addr}) -> {Port, b2l(Addr)};
listener_portip(#{<<"port">> := Port}) -> Port.

-spec listener_module(toml_key()) -> option().
listener_module(<<"http">>) -> ejabberd_cowboy;
listener_module(<<"c2s">>) -> ejabberd_c2s;
listener_module(<<"s2s">>) -> ejabberd_s2s_in;
listener_module(<<"service">>) -> ejabberd_service.

%% path: listen.*[].*
-spec listener_opt(Type :: toml_key(), Name :: toml_key(), toml_value()) -> [option()].
listener_opt(_, <<"proto">>, Proto) -> [{proto, b2a(Proto)}];
listener_opt(_, <<"ip_version">>, 6) -> [inet6];
listener_opt(_, <<"ip_version">>, 4) -> [inet];
listener_opt(_, <<"backlog">>, N) -> [{backlog, N}];
listener_opt(_, <<"proxy_protocol">>, V) -> [{proxy_protocol, V}];
listener_opt(<<"http">>, <<"tls">>, Opts) -> [{ssl, https_options(Opts)}];
listener_opt(<<"http">>, <<"transport">>, Opts) ->
    [{transport_options, parse_map(fun cowboy_transport_opt/2, Opts)}];
listener_opt(<<"http">>, <<"protocol">>, Opts) ->
    [{protocol_options, parse_map(fun cowboy_protocol_opt/2, Opts)}];
listener_opt(<<"http">>, <<"handlers">>, Handlers) ->
    [{modules, parse_map(fun cowboy_modules/2, Handlers)}];
listener_opt(<<"c2s">>, <<"access">>, V) -> [{access, b2a(V)}];
listener_opt(<<"c2s">>, <<"shaper">>, V) -> [{shaper, b2a(V)}];
listener_opt(<<"c2s">>, <<"xml_socket">>, V) -> [{xml_socket, V}];
listener_opt(<<"c2s">>, <<"zlib">>, V) -> [{zlib, V}];
listener_opt(<<"c2s">>, <<"verify_peer">>, true) -> [verify_peer];
listener_opt(<<"c2s">>, <<"hibernate_after">>, V) -> [{hibernate_after, V}];
listener_opt(<<"c2s">>, <<"tls">>, V) -> listener_tls_opts(V);
listener_opt(<<"c2s">>, <<"certfile">>, V) -> [{certfile, b2l(V)}];
listener_opt(<<"c2s">>, <<"dhfile">>, V) -> [{dhfile, b2l(V)}];
listener_opt(<<"c2s">>, <<"cafile">>, V) -> [{cafile, b2l(V)}];
listener_opt(<<"c2s">>, <<"ciphers">>, V) -> [{ciphers, b2l(V)}];
listener_opt(<<"c2s">>, <<"max_stanza_size">>, V) -> [{max_stanza_size, V}];
listener_opt(<<"c2s">>, <<"password">>, V) -> [{password, b2l(V)}];
listener_opt(<<"s2s">>, <<"access">>, V) -> [{access, b2a(V)}];
listener_opt(<<"s2s">>, <<"shaper">>, V) -> [{shaper, b2a(V)}];
listener_opt(<<"s2s">>, <<"zlib">>, V) -> [{zlib, V}];
listener_opt(<<"s2s">>, <<"dhfile">>, V) -> [{dhfile, b2l(V)}];
listener_opt(<<"s2s">>, <<"cafile">>, V) -> [{cafile, b2l(V)}];
listener_opt(<<"s2s">>, <<"ciphers">>, V) -> [{ciphers, b2l(V)}];
listener_opt(<<"s2s">>, <<"max_stanza_size">>, V) -> [{max_stanza_size, V}];
listener_opt(<<"service">>, <<"access">>, V) -> [{access, b2a(V)}];
listener_opt(<<"service">>, <<"shaper_rule">>, V) -> [{shaper_rule, b2a(V)}];
listener_opt(<<"service">>, <<"check_from">>, V) -> [{service_check_from, V}];
listener_opt(<<"service">>, <<"hidden_components">>, V) -> [{hidden_components, V}];
listener_opt(<<"service">>, <<"conflict_behaviour">>, V) -> [{conflict_behaviour, b2a(V)}];
listener_opt(<<"service">>, <<"password">>, V) -> [{password, b2l(V)}];
listener_opt(<<"service">>, <<"max_fsm_queue">>, V) -> [{max_fsm_queue, V}].


%% path: listen.http[].tls
-spec https_options(toml_section()) -> [option()].
https_options(M) ->
    VM = case M of
             #{<<"verify_mode">> := Mode} -> [{verify_mode, b2a(Mode)}];
             _ -> []
         end,
    Opts = maps:without([<<"verify_mode">>], M),
    VM ++ client_tls_options(Opts).

%% path: listen.http[].transport.*
-spec cowboy_transport_opt(toml_key(), toml_value()) -> [option()].
cowboy_transport_opt(<<"num_acceptors">>, N) -> [{num_acceptors, N}];
cowboy_transport_opt(<<"max_connections">>, N) -> [{max_connections, N}].

%% path: listen.http[].protocol.*
-spec cowboy_protocol_opt(toml_key(), toml_value()) -> [option()].
cowboy_protocol_opt(<<"compress">>, V) -> [{compress, V}].

%% path: listen.http[].handlers.*
-spec cowboy_modules(toml_key(), [toml_section()]) -> [option()].
cowboy_modules(Type, Modules) ->
    parse_list(fun(M) -> cowboy_module(Type, M) end, Modules).

%% path: listen.http[].handlers.*[]
-spec cowboy_module(toml_key(), toml_section()) -> [option()].
cowboy_module(Type, #{<<"host">> := Host, <<"path">> := Path} = Options) ->
    Opts = maps:without([<<"host">>, <<"path">>], Options),
    ModuleOpts = cowboy_module_options(Type, Opts),
    [{b2l(Host), b2l(Path), b2a(Type), ModuleOpts}].

-spec cowboy_module_options(toml_key(), toml_section()) -> [option()].
cowboy_module_options(<<"mod_websockets">>, #{<<"ejabberd_service">> := Opts}) ->
    [{ejabberd_service, parse_map(fun(K, V) -> listener_opt(<<"service">>, K, V) end, Opts)}];
cowboy_module_options(<<"lasse_handler">>, #{<<"modules">> := Modules}) ->
    [b2a(Mod) || Mod <- Modules];
cowboy_module_options(<<"cowboy_static">>, #{<<"type">> := Type,
                                             <<"app">> := App,
                                             <<"content_path">> := Path}) ->
    {b2a(Type), b2a(App), b2l(Path), [{mimetypes, cow_mimetypes, all}]};
cowboy_module_options(<<"cowboy_swagger_redirect_handler">>, _) -> #{};
cowboy_module_options(<<"cowboy_swagger_json_handler">>, _) -> #{};
cowboy_module_options(<<"mongoose_api">>, #{<<"handlers">> := Handlers}) ->
    [{handlers, [b2a(H) || H <- Handlers]}];
cowboy_module_options(_, _) -> [].

%% path: listen.c2s[].tls
-spec listener_tls_opts(toml_section()) -> [option()].
listener_tls_opts(M = #{<<"module">> := <<"just_tls">>}) ->
    VM = case M of
             #{<<"verify_mode">> := VMode, <<"disconnect_on_failure">> := D} ->
                 [{verify_fun, {b2a(VMode), D}}];
             #{<<"verify_mode">> := VMode} ->
                 [{verify_fun, {b2a(VMode), true}}];
             _ -> []
         end,
    Mode = listener_tls_mode(M),
    Opts = maps:without([<<"mode">>, <<"module">>, <<"verify_mode">>, <<"disconnect_on_failure">>],
                        M),
    Mode ++ [{tls_module, just_tls},
             {ssl_options, VM ++ client_tls_options(Opts)}];
listener_tls_opts(M) ->
    Mode = listener_tls_mode(M),
    VM = case M of
             #{<<"verify_mode">> := VMode} -> [{verify_mode, b2a(VMode)}];
             _ -> []
         end,
    Opts = maps:without([<<"mode">>, <<"module">>, <<"verify_mode">>], M),
    Mode ++ VM ++ client_tls_options(Opts).

-spec listener_tls_mode(toml_section()) -> [option()].
listener_tls_mode(#{<<"mode">> := Mode}) -> [b2a(Mode)];
listener_tls_mode(_) -> [].

%% path: (host_config[].)auth.*
-spec auth_option(toml_key(), toml_value()) -> [option()].
auth_option(<<"methods">>, Methods) ->
    [{auth_method, [b2a(Method) || Method <- Methods]}];
auth_option(<<"password">>, #{<<"format">> := <<"scram">>, <<"hash">> := Hashes}) ->
    [{password_format, {scram, [b2a(H) || H <- Hashes]}}];
auth_option(<<"password">>, #{<<"format">> := Format}) ->
    [{password_format, b2a(Format)}];
auth_option(<<"scram_iterations">>, V) ->
    [{scram_iterations, V}];
auth_option(<<"cyrsasl_external">>, V) ->
    [{cyrsasl_external, [cyrsasl_external(M) || M <- V]}];
auth_option(<<"allow_multiple_connections">>, V) ->
    [{allow_multiple_connections, V}];
auth_option(<<"anonymous_protocol">>, V) ->
    [{anonymous_protocol, b2a(V)}];
auth_option(<<"sasl_mechanisms">>, V) ->
    [{sasl_mechanisms, [b2a(M) || M <- V]}];
auth_option(<<"ldap_base">>, V) ->
    [{ldap_base, b2l(V)}];
auth_option(<<"ldap_filter">>, V) ->
    [{ldap_filter, b2l(V)}];
auth_option(<<"extauth_instances">>, V) ->
    [{extauth_instances, V}].

%% path: (host_config[].)auth.cyrsasl_external[]
-spec cyrsasl_external(toml_key()) -> option().
cyrsasl_external(<<"standard">>) -> standard;
cyrsasl_external(<<"common_name">>) -> common_name;
cyrsasl_external(<<"auth_id">>) -> auth_id;
cyrsasl_external(M) -> {mod, b2a(M)}.

-spec partition_auth_opts([{atom(), any()}], ejabberd:server()) -> [config()].
partition_auth_opts(AuthOpts, Host) ->
    {InnerOpts, OuterOpts} = lists:partition(fun({K, _}) -> is_inner_auth_opt(K) end, AuthOpts),
    case InnerOpts of
        [] ->
            [#local_config{key = {K, Host}, value = V} || {K, V} <- OuterOpts];
        _ ->
            [#local_config{key = {auth_opts, Host}, value = InnerOpts} |
                [#local_config{key = {K, Host}, value = V} || {K, V} <- OuterOpts]]
            end.

-spec is_inner_auth_opt(atom()) -> boolean().
is_inner_auth_opt(auth_method) -> false;
is_inner_auth_opt(allow_multiple_connections) -> false;
is_inner_auth_opt(anonymous_protocol) -> false;
is_inner_auth_opt(sasl_mechanisms) -> false;
is_inner_auth_opt(extauth_instances) -> false;
is_inner_auth_opt(_) -> true.

%% path: outgoing_pools.*
-spec process_pool_type(toml_key(), toml_section()) -> [option()].
process_pool_type(Type, Content) ->
    parse_map(fun(Tag, C) -> process_pool(Type, Tag, C) end, Content).

%% path: outgoing_pools.*.*
-spec process_pool(Type :: toml_key(), Tag :: toml_key(), toml_section()) -> [option()].
process_pool(Type, Tag, M) ->
    Scope = pool_scope(M),
    Options = pool_options(maps:without([<<"scope">>, <<"host">>, <<"connection">>], M)),
    ConnectionOptions = connection_options(Type, maps:get(<<"connection">>, M, #{})),
    [{b2a(Type), Scope, b2a(Tag), Options, ConnectionOptions}].

-spec pool_scope(toml_section()) -> option().
pool_scope(#{<<"scope">> := <<"single_host">>, <<"host">> := Host}) -> Host;
pool_scope(#{<<"scope">> := Scope}) -> b2a(Scope).

-spec pool_options(toml_section()) -> [option()].
pool_options(Opts) ->
    parse_map(fun pool_option/2, Opts).

%% path: outgoing_pools.*.*.*
-spec pool_option(toml_key(), toml_value()) -> [option()].
pool_option(<<"workers">>, V) -> [{workers, V}];
pool_option(<<"strategy">>, V) -> [{strategy, b2a(V)}];
pool_option(<<"call_timeout">>, V) -> [{call_timeout, V}].

%% path: outgoing_pools.*.connection
-spec connection_options(toml_key(), toml_section()) -> [option()].
connection_options(<<"rdbms">>, Options) ->
    [{server, rdbms_server(Options)}];
connection_options(<<"redis">>, Options) ->
    parse_map(fun redis_option/2, Options);
connection_options(<<"ldap">>, Options) ->
    parse_map(fun ldap_option/2, Options);
connection_options(<<"riak">>, Options = #{<<"username">> := UserName,
                                           <<"password">> := Password}) ->
    M = maps:without([<<"username">>, <<"password">>], Options),
    [{credentials, b2l(UserName), b2l(Password)} | parse_map(fun riak_option/2, M)];
connection_options(<<"cassandra">>, Options) ->
    parse_map(fun cassandra_option/2, Options);
connection_options(<<"elastic">>, Options) ->
    parse_map(fun elastic_option/2, Options).

%% path: outgoing_pools.rdbms.connection
-spec rdbms_server(toml_section()) -> option().
rdbms_server(#{<<"driver">> := <<"odbc">>,
               <<"settings">> := Settings}) ->
    b2l(Settings);
rdbms_server(#{<<"driver">> := Driver,
               <<"host">> := Host,
               <<"database">> := Database,
               <<"username">> := UserName,
               <<"password">> := Password} = M) ->
    DriverA = b2a(Driver),
    HostS = b2l(Host),
    DatabaseS = b2l(Database),
    UserNameS = b2l(UserName),
    PasswordS = b2l(Password),
    case {maps:get(<<"port">>, M, no_port), db_tls(M)} of
        {no_port, no_tls} -> {DriverA, HostS, DatabaseS, UserNameS, PasswordS};
        {Port, no_tls} -> {DriverA, HostS, Port, DatabaseS, UserNameS, PasswordS};
        {no_port, TLS} -> {DriverA, HostS, DatabaseS, UserNameS, PasswordS, TLS};
        {Port, TLS} -> {DriverA, HostS, Port, DatabaseS, UserNameS, PasswordS, TLS}
    end.

-spec db_tls(toml_section()) -> [option()] | no_tls.
db_tls(#{<<"driver">> := Driver, <<"tls">> := TLS}) -> db_tls_options(Driver, TLS);
db_tls(_) -> no_tls.

%% path: outgoing_pools.rdbms.connection.tls
-spec db_tls_options(toml_key(), toml_section()) -> [option()].
db_tls_options(<<"mysql">>, Opts) ->
    client_tls_options(Opts);
db_tls_options(<<"pgsql">>, Opts) ->
    {SSLMode, Opts1} = case maps:take(<<"required">>, Opts) of
                           {true, M} -> {required, M};
                           {false, M} -> {true, M};
                           error -> {true, Opts}
                       end,
    [{ssl, SSLMode}, {ssl_opts, client_tls_options(Opts1)}].

%% path: outgoing_pools.redis.connection.*
-spec redis_option(toml_key(), toml_value()) -> [option()].
redis_option(<<"host">>, Host) -> [{host, b2l(Host)}];
redis_option(<<"port">>, Port) -> [{port, Port}];
redis_option(<<"database">>, Database) -> [{database, b2l(Database)}];
redis_option(<<"password">>, Password) -> [{password, b2l(Password)}].

%% path: outgoing_pools.ldap.connection.*
-spec ldap_option(toml_key(), toml_value()) -> [option()].
ldap_option(<<"host">>, Host) -> [{host, b2l(Host)}];
ldap_option(<<"port">>, Port) -> [{port, Port}];
ldap_option(<<"rootdn">>, RootDN) -> [{rootdn, b2l(RootDN)}];
ldap_option(<<"password">>, Password) -> [{password, b2l(Password)}];
ldap_option(<<"encrypt">>, <<"tls">>) -> [{encrypt, tls}];
ldap_option(<<"encrypt">>, <<"none">>) -> [{encrypt, none}];
ldap_option(<<"tls">>, Options) -> [{tls_options, client_tls_options(Options)}].

%% path: outgoing_pools.riak.connection.*
-spec riak_option(toml_key(), toml_value()) -> [option()].
riak_option(<<"address">>, Addr) -> [{address, b2l(Addr)}];
riak_option(<<"port">>, Port) -> [{port, Port}];
riak_option(<<"cacertfile">>, Path) -> [{cacertfile, b2l(Path)}];
riak_option(<<"tls">>, Options) -> [{ssl_opts, client_tls_options(Options)}].

%% path: outgoing_pools.cassandra.connnection.*
-spec cassandra_option(toml_key(), toml_value()) -> [option()].
cassandra_option(<<"servers">>, Servers) -> [{servers, [cassandra_server(S) || S <- Servers]}];
cassandra_option(<<"keyspace">>, KeySpace) -> [{keyspace, b2a(KeySpace)}];
cassandra_option(<<"tls">>, Options) -> [{ssl, client_tls_options(Options)}].

%% path: outgoing_pools.cassandra.connection.servers[]
-spec cassandra_server(toml_section()) -> option().
cassandra_server(#{<<"ip_address">> := IPAddr, <<"port">> := Port}) -> {b2l(IPAddr), Port};
cassandra_server(#{<<"ip_address">> := IPAddr}) -> b2l(IPAddr).

%% path: outgoing_pools.elastic.connection.*
-spec elastic_option(toml_key(), toml_value()) -> [option()].
elastic_option(<<"host">>, Host) -> [{host, b2l(Host)}];
elastic_option(<<"port">>, Port) -> [{port, Port}].

%% path: services.*
-spec process_service(toml_key(), toml_section()) -> [option()].
process_service(S, Opts) ->
    [{b2a(S), parse_map(fun(K, V) -> service_opt(S, K, V) end, Opts)}].

%% path: services.*.*
-spec service_opt(toml_key(), toml_key(), toml_value()) -> [option()].
service_opt(<<"service_admin_extra">>, <<"submods">>, V) ->
    [{submods, [b2a(M) || M <- V]}];
service_opt(<<"service_mongoose_system_metrics">>, <<"initial_report">>, V) ->
    [{initial_report, V}];
service_opt(<<"service_mongoose_system_metrics">>, <<"periodic_report">>, V) ->
    [{periodic_report, V}];
service_opt(<<"service_mongoose_system_metrics">>, <<"report">>, true) ->
    [report];
service_opt(<<"service_mongoose_system_metrics">>, <<"report">>, false) ->
    [];
service_opt(<<"service_mongoose_system_metrics">>, <<"no_report">>, true) ->
    [no_report];
service_opt(<<"service_mongoose_system_metrics">>, <<"no_report">>, false) ->
    [];
service_opt(<<"service_mongoose_system_metrics">>, <<"tracking_id">>, V) ->
    [{tracking_id, b2l(V)}].

%% path: modules.*
-spec process_module(toml_key(), toml_section()) -> [option()].
process_module(Mod, Opts) ->
    [{b2a(Mod), parse_map(fun(K, V) -> module_opt(Mod, K, V) end, Opts)}].

%% path: modules.*.*
-spec module_opt(toml_key(), toml_key(), toml_value()) -> [option()].
module_opt(<<"mod_disco">>, <<"users_can_see_hidden_services">>, V) ->
    [{users_can_see_hidden_services, V}];
module_opt(<<"mod_offline">>, <<"access_max_user_messages">>, V) ->
    [{access_max_user_messages, b2a(V)}];
module_opt(<<"mod_register">>, <<"welcome_message">>, V) ->
    [{welcome_message, {b2l(V)}}];
module_opt(<<"mod_register">>, <<"ip_access">>, V) ->
    Rules = parse_list(fun(#{<<"address">> := Addr, <<"policy">> := P}) ->
                               [{b2a(P), b2l(Addr)}]
                       end, V),
    [{ip_access, Rules}];
module_opt(<<"mod_register">>, <<"access">>, V) ->
    [{access, b2a(V)}];
module_opt(<<"mod_vcard">>, <<"host">>, V) ->
    [{host, b2l(V)}];
module_opt(<<"mod_vcard">>, <<"ldap_base">>, V) ->
    [{ldap_base, b2l(V)}];
module_opt(<<"mod_vcard">>, <<"ldap_filter">>, V) ->
    [{ldap_filter, b2l(V)}];
module_opt(_, <<"backend">>, V) ->
    [{backend, b2a(V)}].

%% path: (host_config[].)shaper.*
-spec process_shaper(toml_key(), ejabberd:server() | global, toml_section()) -> [config()].
process_shaper(Name, Host, #{<<"max_rate">> := MaxRate}) ->
    [#config{key = {shaper, b2a(Name), Host}, value = {maxrate, MaxRate}}].

%% path: (host_config[].)acl.*
-spec process_acl(toml_key(), ejabberd:server() | global, toml_value()) -> [config()].
process_acl(ACLName, Host, Content) ->
    [acl:to_record(Host, b2a(ACLName), acl_data(Content))].

-spec acl_data(toml_value()) -> option().
acl_data(Content) when is_map(Content) ->
    case maps:to_list(Content) of
        [{Key, Values}] when is_list(Values) ->
            list_to_tuple([b2a(Key) | Values]);
        [{Key, Value}] when is_binary(Value) ->
            {b2a(Key), Value}
    end;
acl_data(Value) when is_binary(Value) -> b2a(Value).

%% path: (host_config[].)access.*
-spec process_access_rule(toml_key(), ejabberd:server() | global, toml_value()) -> [config()].
process_access_rule(Name, Host, Contents) ->
    Rules = [{access_rule_value(Value), b2a(ACL)} ||
                #{<<"acl">> := ACL, <<"value">> := Value} <- Contents],
    [#config{key = {access, b2a(Name), Host}, value = Rules}].

-spec access_rule_value(toml_value()) -> option().
access_rule_value(B) when is_binary(B) -> b2a(B);
access_rule_value(V) -> V.

%% path: s2s.*
-spec process_s2s_option(toml_key(), toml_value()) -> config_list().
process_s2s_option(<<"use_starttls">>, V) ->
    [#local_config{key = s2s_use_starttls, value = b2a(V)}];
process_s2s_option(<<"certfile">>, V) ->
    [#local_config{key = s2s_certfile, value = b2l(V)}];
process_s2s_option(<<"default_policy">>, V) ->
    ?HOST_F([#local_config{key = {s2s_default_policy, Host}, value = b2a(V)}]);
process_s2s_option(<<"outgoing_port">>, V) ->
    [#local_config{key = outgoing_s2s_port, value = V}];
process_s2s_option(<<"address">>, Addrs) ->
    [#local_config{key = {s2s_addr, Host}, value = s2s_address(Addr)}
     || Addr = #{<<"host">> := Host} <- Addrs].

%% path: s2s.address[]
-spec s2s_address(toml_section()) -> option().
s2s_address(#{<<"ip_address">> := IP, <<"port">> := Port}) ->
    {b2l(IP), Port};
s2s_address(#{<<"ip_address">> := IP}) ->
    b2l(IP).

%% path: host_config[]
-spec process_host_item(toml_section()) -> config_list().
process_host_item(M) ->
    {Host, Sections} = maps:take(<<"host">>, M),
    parse_map(fun(K, V) -> process_host_section(K, Host, V) end, Sections).

%% path: host_config[].*
-spec process_host_section(toml_key(), ejabberd:server(), toml_section()) -> config_list().
process_host_section(<<"auth">>, Host, Content) ->
    ct:pal("In host_config:~n~p~n~p", [Host, Content]),
    AuthOpts = parse_map(fun auth_option/2, Content),
    partition_auth_opts(AuthOpts, Host);
process_host_section(<<"shaper">>, Host, Content) ->
    parse_map(fun(Name, C) -> process_shaper(Name, Host, C) end, Content);
process_host_section(<<"acl">>, Host, Content) ->
    parse_map(fun(Name, C) -> process_acl(Name, Host, C) end, Content);
process_host_section(<<"access">>, Host, Content) ->
    parse_map(fun(Name, C) -> process_access_rule(Name, Host, C) end, Content);
process_host_section(<<"modules">>, Host, Content) ->
    Mods = parse_map(fun process_module/2, Content),
    [#local_config{key = {modules, Host}, value = Mods}].

%% path: listen.http[].tls,
%%       listen.c2s[].tls,
%%       outgoing_pools.rdbms.connection.tls,
%%       outgoing_pools.ldap.connection.tls,
%%       outgoing_pools.riak.connection.tls,
%%       outgoing_pools.cassandra.connection.tls
-spec client_tls_options(toml_section()) -> [option()].
client_tls_options(Opts) ->
    parse_map(fun client_tls_option/2, Opts).

-spec client_tls_option(toml_key(), toml_value()) -> [option()].
client_tls_option(<<"verify_peer">>, V) -> [{verify, verify_peer(V)}];
client_tls_option(<<"certfile">>, V) -> [{certfile, b2l(V)}];
client_tls_option(<<"cacertfile">>, V) -> [{cacertfile, b2l(V)}];
client_tls_option(<<"keyfile">>, V) -> [{keyfile, b2l(V)}];
client_tls_option(<<"password">>, V) -> [{password, b2l(V)}];
client_tls_option(<<"server_name_indication">>, false) -> [{server_name_indication, disable}];
client_tls_option(<<"ciphers">>, L) -> [{ciphers, [tls_cipher(C) || C <- L]}];
client_tls_option(<<"versions">>, L) -> [{versions, [b2a(V) || V <- L]}].

-spec verify_peer(boolean()) -> option().
verify_peer(false) -> verify_none;
verify_peer(true) -> verify_peer.

-spec tls_cipher(toml_value()) -> option().
tls_cipher(#{<<"key_exchange">> := KEx,
             <<"cipher">> := Cipher,
             <<"mac">> := MAC,
             <<"prf">> := PRF}) ->
    #{key_exchange => b2a(KEx), cipher => b2a(Cipher), mac => b2a(MAC), prf => b2a(PRF)};
tls_cipher(Cipher) -> b2l(Cipher).

set_overrides(State) ->
    lists:foldl(fun(F, StateIn) -> F(StateIn) end, State, [
        fun(S) -> maybe_override(S, override_global) end,
        fun(S) -> maybe_override(S, override_local) end,
        fun(S) -> maybe_override(S, override_acls) end
        ]).

maybe_override(S = {state, Config, _, _, _, _}, Name) ->
    case lists:keyfind(Name, 2, Config) of
        {_, _, true} ->
            S1 = mongoose_config_parser:Name(S),
            C1 = lists:keydelete(Name, 2, Config),
            mongoose_config_parser:set_opts(C1, S1);
        {_, _, false} ->
            C1 = lists:keydelete(Name, 2, Config),
            mongoose_config_parser:set_opts(C1, S);
        _ ->
            S
        end.

%% TODO replace with binary_to_existing_atom where possible, prevent atom leak
b2a(B) -> binary_to_atom(B, utf8).

b2l(B) -> binary_to_list(B).

-spec parse_map(fun((toml_key(), toml_value()) -> list()), toml_section()) -> list().
parse_map(F, M) ->
    lists:flatmap(fun({K, V}) -> F(K, V) end, maps:to_list(M)).

-spec parse_list(fun((toml_value()) -> list()), [toml_value()]) -> list().
parse_list(F, L) ->
    lists:flatmap(F, L).
