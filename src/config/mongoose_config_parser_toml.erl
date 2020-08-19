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
-type config() :: #config{} | #local_config{} | acl:acl() | {override, atom()}.
-type config_list() :: [config() | fun((ejabberd:server()) -> [config()])]. % see HOST_F

%% Path from the currently processed config node to the root
%%   - toml_key(): key in a toml_section()
%%   - item: item in a list
%%   - tuple(): item in a list, tagged with data from the item, e.g. host name
-type path() :: [toml_key() | item | tuple()].

-spec parse_file(FileName :: string()) -> mongoose_config_parser:state().
parse_file(FileName) ->
    {ok, Content} = tomerl:read_file(FileName),
    Config = parse(Content),
    [Hosts] = lists:filtermap(fun(#config{key = hosts, value = Hosts}) ->
                                      {true, Hosts};
                                 (_) -> false
                              end, Config),
    {FOpts, Config1} = lists:partition(fun(Opt) -> is_function(Opt, 1) end, Config),
    {Overrides, Opts} = lists:partition(fun({override, _}) -> true;
                                           (_) -> false
                                        end, Config1),
    HOpts = lists:flatmap(fun(F) -> lists:flatmap(F, Hosts) end, FOpts),
    lists:foldl(fun(F, StateIn) -> F(StateIn) end,
                mongoose_config_parser:new_state(),
                [fun(S) -> mongoose_config_parser:set_hosts(Hosts, S) end,
                 fun(S) -> mongoose_config_parser:set_opts(Opts ++ HOpts, S) end,
                 fun mongoose_config_parser:dedup_state_opts/1,
                 fun mongoose_config_parser:add_dep_modules/1,
                 fun(S) -> set_overrides(Overrides, S) end]).

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

%% root path
-spec parse(toml_section()) -> config_list().
parse(Content) ->
    parse_section([], Content).

%% path: *
-spec process_section(path(), toml_section() | [toml_section()]) -> config_list().
process_section([<<"listen">>] = Path, Content) ->
    Listeners = parse_section(Path, Content),
    [#local_config{key = listen, value = Listeners}];
process_section([<<"auth">>] = Path, Content) ->
    AuthOpts = parse_section(Path, Content),
    ?HOST_F(partition_auth_opts(AuthOpts, Host));
process_section([<<"outgoing_pools">>] = Path, Content) ->
    Pools = parse_section(Path, Content),
    [#local_config{key = outgoing_pools, value = Pools}];
process_section([<<"services">>] = Path, Content) ->
    Services = parse_section(Path, Content),
    [#local_config{key = services, value = Services}];
process_section([<<"modules">>] = Path, Content) ->
    Mods = parse_section(Path, Content),
    ?HOST_F([#local_config{key = {modules, Host}, value = Mods}]);
process_section([<<"s2s">>] = Path, Content) ->
    DNSOpts = s2s_dns_opts(Content),
    Out = s2s_outgoing_opts(Content),
    Opts = maps:without(
        [<<"dns_timeout">>, <<"dns_retries">>,
         <<"preferred_ip_version">>, <<"connection_timeout">>], Content),
    S2sOpts = parse_section(Path, Opts),
    Out ++ DNSOpts ++ S2sOpts;
process_section([<<"host_config">>] = Path, Content) ->
    parse_list(Path, Content);
process_section(Path, Content) ->
    parse_section(Path, Content).

%% path: general.*
-spec process_general(path(), toml_value()) -> [config()].
process_general([<<"loglevel">>|_], V) ->
    [#local_config{key = loglevel, value = b2a(V)}];
process_general([<<"hosts">>|_], Hosts) ->
    [#config{key = hosts, value = [jid:nodeprep(H) || H <- Hosts]}];
process_general([<<"registration_timeout">>|_], <<"infinity">>) ->
    [#local_config{key = registration_timeout, value = infinity}];
process_general([<<"registration_timeout">>|_], V) ->
    [#local_config{key = registration_timeout, value = V}];
process_general([<<"language">>|_], V) ->
    [#config{key = language, value = V}];
process_general([<<"all_metrics_are_global">>|_], V) ->
    [#local_config{key = all_metrics_are_global, value = V}];
process_general([<<"sm_backend">>|_], V) ->
    [#config{key = sm_backend, value = {b2a(V), []}}];
process_general([<<"max_fsm_queue">>|_], V) ->
    [#local_config{key = max_fsm_queue, value = V}];
process_general([<<"http_server_name">>|_], V) ->
    [#local_config{key = cowboy_server_name, value = b2l(V)}];
process_general([<<"rdbms_server_type">>|_], V) ->
    [#local_config{key = rdbms_server_type, value = b2a(V)}];
process_general([<<"override">>|_], Overrides) ->
    [{override, b2a(Scope)} || Scope <- Overrides];
process_general([<<"pgsql_users_number_estimate">>|_], V) ->
    ?HOST_F([#local_config{key = {pgsql_users_number_estimate, Host}, value = V}]);
process_general([<<"route_subdomain">>|_], V) ->
    ?HOST_F([#local_config{key = {route_subdomain, Host}, value = b2a(V)}]);
process_general([<<"mongooseimctl_access_commands">>|_], Rules) ->
    [#local_config{key = mongooseimctl_access_commands,
                   value = lists:map(fun process_rule/1, Rules)}];
process_general([<<"routing_modules">>|_], Mods) ->
    [#local_config{key = routing_modules, value = lists:map(fun b2a/1, Mods)}];
process_general([<<"replaced_wait_timeout">>|_], V) ->
    ?HOST_F([#local_config{key = {replaced_wait_timeout, Host}, value = V}]);
process_general([<<"hide_service_name">>|_], V) ->
    ?HOST_F([#local_config{key = {hide_service_name, Host}, value = V}]).

process_rule(#{<<"access_rule">> := Rule,
    <<"argument_restrictions">> := Arg, <<"commands">> := Comms}) ->
        {b2a(Rule),
         lists:map(fun b2l/1, Comms),
         [list_to_tuple(lists:map(fun b2l/1, Arg))]}.

%% path: listen.*[]
-spec process_listener(path(), toml_section()) -> [option()].
process_listener([_, Type|_] = Path, Content) ->
    Options = maps:without([<<"port">>, <<"ip_address">>], Content),
    PortIP = listener_portip(Content),
    Opts = parse_section(Path, Options),
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

%% path: listen.http[].*
-spec http_listener_opt(path(), toml_value()) -> [option()].
http_listener_opt([<<"tls">>|_] = Path, Opts) ->
    [{ssl, https_options(Path, Opts)}];
http_listener_opt([<<"transport">>|_] = Path, Opts) ->
    [{transport_options, parse_section(Path, Opts)}];
http_listener_opt([<<"protocol">>|_] = Path, Opts) ->
    [{protocol_options, parse_section(Path, Opts)}];
http_listener_opt([<<"handlers">>|_] = Path, Handlers) ->
    [{modules, parse_section(Path, Handlers)}];
http_listener_opt(P, V) -> listener_opt(P, V).

%% path: listen.c2s[].*
-spec c2s_listener_opt(path(), toml_value()) -> [option()].
c2s_listener_opt([<<"access">>|_], V) -> [{access, b2a(V)}];
c2s_listener_opt([<<"shaper">>|_], V) -> [{shaper, b2a(V)}];
c2s_listener_opt([<<"xml_socket">>|_], V) -> [{xml_socket, V}];
c2s_listener_opt([<<"zlib">>|_], V) -> [{zlib, V}];
c2s_listener_opt([<<"verify_peer">>|_], true) -> [verify_peer];
c2s_listener_opt([<<"hibernate_after">>|_], V) -> [{hibernate_after, V}];
c2s_listener_opt([<<"tls">>|_] = P, V) -> listener_tls_opts(P, V);
c2s_listener_opt([<<"certfile">>|_], V) -> [{certfile, b2l(V)}];
c2s_listener_opt([<<"dhfile">>|_], V) -> [{dhfile, b2l(V)}];
c2s_listener_opt([<<"cafile">>|_], V) -> [{cafile, b2l(V)}];
c2s_listener_opt([<<"ciphers">>|_], V) -> [{ciphers, b2l(V)}];
c2s_listener_opt([<<"max_stanza_size">>|_], V) -> [{max_stanza_size, V}];
c2s_listener_opt([<<"password">>|_], V) -> [{password, b2l(V)}];
c2s_listener_opt(P, V) -> listener_opt(P, V).

%% path: listen.s2s[].*
-spec s2s_listener_opt(path(), toml_value()) -> [option()].
s2s_listener_opt([<<"access">>|_], V) -> [{access, b2a(V)}];
s2s_listener_opt([<<"shaper">>|_], V) -> [{shaper, b2a(V)}];
s2s_listener_opt([<<"zlib">>|_], V) -> [{zlib, V}];
s2s_listener_opt([<<"dhfile">>|_], V) -> [{dhfile, b2l(V)}];
s2s_listener_opt([<<"cafile">>|_], V) -> [{cafile, b2l(V)}];
s2s_listener_opt([<<"ciphers">>|_], V) -> [{ciphers, b2l(V)}];
s2s_listener_opt([<<"max_stanza_size">>|_], V) -> [{max_stanza_size, V}];
s2s_listener_opt(P, V) -> listener_opt(P, V).

%% path: listen.service[].*,
%%       listen.http[].handlers.mod_websockets[].ejabberd_service.*
-spec service_listener_opt(path(), toml_value()) -> [option()].
service_listener_opt([<<"access">>|_], V) -> [{access, b2a(V)}];
service_listener_opt([<<"shaper_rule">>|_], V) -> [{shaper_rule, b2a(V)}];
service_listener_opt([<<"check_from">>|_], V) -> [{service_check_from, V}];
service_listener_opt([<<"hidden_components">>|_], V) -> [{hidden_components, V}];
service_listener_opt([<<"conflict_behaviour">>|_], V) -> [{conflict_behaviour, b2a(V)}];
service_listener_opt([<<"password">>|_], V) -> [{password, b2l(V)}];
service_listener_opt([<<"max_fsm_queue">>|_], V) -> [{max_fsm_queue, V}];
service_listener_opt(P, V) -> listener_opt(P, V).

%% path: listen.*[].*
-spec listener_opt(path(), toml_value()) -> [option()].
listener_opt([<<"proto">>|_], Proto) -> [{proto, b2a(Proto)}];
listener_opt([<<"ip_version">>|_], 6) -> [inet6];
listener_opt([<<"ip_version">>|_], 4) -> [inet];
listener_opt([<<"backlog">>|_], N) -> [{backlog, N}];
listener_opt([<<"proxy_protocol">>|_], V) -> [{proxy_protocol, V}].

%% path: listen.http[].tls
-spec https_options(path(), toml_section()) -> [option()].
https_options(Path, M) ->
    VM = case M of
             #{<<"verify_mode">> := Mode} -> [{verify_mode, b2a(Mode)}];
             _ -> []
         end,
    Opts = maps:without([<<"verify_mode">>], M),
    VM ++ parse_section(Path, Opts).

%% path: listen.http[].transport.*
-spec cowboy_transport_opt(path(), toml_value()) -> [option()].
cowboy_transport_opt([<<"num_acceptors">>|_], N) -> [{num_acceptors, N}];
cowboy_transport_opt([<<"max_connections">>|_], N) -> [{max_connections, N}].

%% path: listen.http[].protocol.*
-spec cowboy_protocol_opt(path(), toml_value()) -> [option()].
cowboy_protocol_opt([<<"compress">>|_], V) -> [{compress, V}].

%% path: listen.http[].handlers.*[]
-spec cowboy_module(path(), toml_section()) -> [option()].
cowboy_module([_, Type|_] = Path, #{<<"host">> := Host, <<"path">> := ModPath} = Options) ->
    Opts = maps:without([<<"host">>, <<"path">>], Options),
    ModuleOpts = cowboy_module_options(Path, Opts),
    [{b2l(Host), b2l(ModPath), b2a(Type), ModuleOpts}].

-spec cowboy_module_options(path(), toml_section()) -> [option()].
cowboy_module_options([_, <<"mod_websockets">>|_] = Path, #{<<"ejabberd_service">> := Opts}) ->
    [{ejabberd_service, parse_section([<<"ejabberd_service">>|Path], Opts)}];
    %% TODO get rid of ejabberd
cowboy_module_options([_, <<"lasse_handler">>|_], #{<<"modules">> := Modules}) ->
    [b2a(Mod) || Mod <- Modules];
cowboy_module_options([_, <<"cowboy_static">>|_], #{<<"type">> := Type,
                                                    <<"app">> := App,
                                                    <<"content_path">> := Path}) ->
    {b2a(Type), b2a(App), b2l(Path), [{mimetypes, cow_mimetypes, all}]};
cowboy_module_options([_, <<"cowboy_swagger_redirect_handler">>|_], _) -> #{};
cowboy_module_options([_, <<"cowboy_swagger_json_handler">>|_], _) -> #{};
cowboy_module_options([_, <<"mongoose_api">>|_], #{<<"handlers">> := Handlers}) ->
    [{handlers, [b2a(H) || H <- Handlers]}];
cowboy_module_options(_, _) -> [].

%% path: listen.c2s[].tls
-spec listener_tls_opts(path(), toml_section()) -> [option()].
listener_tls_opts(Path, M = #{<<"module">> := <<"just_tls">>}) ->
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
             {ssl_options, VM ++ parse_section(Path, Opts)}];
listener_tls_opts(Path, M) ->
    Mode = listener_tls_mode(M),
    VM = case M of
             #{<<"verify_mode">> := VMode} -> [{verify_mode, b2a(VMode)}];
             _ -> []
         end,
    Opts = maps:without([<<"mode">>, <<"module">>, <<"verify_mode">>], M),
    Mode ++ VM ++ parse_section(Path, Opts).

-spec listener_tls_mode(toml_section()) -> [option()].
listener_tls_mode(#{<<"mode">> := Mode}) -> [b2a(Mode)];
listener_tls_mode(_) -> [].

%% path: (host_config[].)auth.*
-spec auth_option(path(), toml_value()) -> [option()].
auth_option([<<"methods">>|_], Methods) ->
    [{auth_method, [b2a(Method) || Method <- Methods]}];
auth_option([<<"password">>|_], #{<<"format">> := <<"scram">>, <<"hash">> := Hashes}) ->
    [{password_format, {scram, [b2a(H) || H <- Hashes]}}];
auth_option([<<"password">>|_], #{<<"format">> := Format}) ->
    [{password_format, b2a(Format)}];
auth_option([<<"scram_iterations">>|_], V) ->
    [{scram_iterations, V}];
auth_option([<<"cyrsasl_external">>|_], V) ->
    [{cyrsasl_external, [cyrsasl_external(M) || M <- V]}];
auth_option([<<"allow_multiple_connections">>|_], V) ->
    [{allow_multiple_connections, V}];
auth_option([<<"anonymous_protocol">>|_], V) ->
    [{anonymous_protocol, b2a(V)}];
auth_option([<<"sasl_mechanisms">>|_], V) ->
    [{sasl_mechanisms, [b2a(M) || M <- V]}];
auth_option([<<"ldap_base">>|_], V) ->
    [{ldap_base, b2l(V)}];
auth_option([<<"ldap_filter">>|_], V) ->
    [{ldap_filter, b2l(V)}];
auth_option([<<"extauth_instances">>|_], V) ->
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
    [#local_config{key = {auth_opts, Host}, value = InnerOpts} |
     [#local_config{key = {K, Host}, value = V} || {K, V} <- OuterOpts]].

-spec is_inner_auth_opt(atom()) -> boolean().
is_inner_auth_opt(auth_method) -> false;
is_inner_auth_opt(allow_multiple_connections) -> false;
is_inner_auth_opt(anonymous_protocol) -> false;
is_inner_auth_opt(sasl_mechanisms) -> false;
is_inner_auth_opt(extauth_instances) -> false;
is_inner_auth_opt(_) -> true.

%% path: outgoing_pools.*.*
-spec process_pool(path(), toml_section()) -> [option()].
process_pool([Tag, Type|_] = Path, M) ->
    Scope = pool_scope(M),
    Options = parse_section(Path, maps:without([<<"scope">>, <<"host">>, <<"connection">>], M)),
    ConnectionOptions = connection_options([<<"connection">> | Path],
                                           maps:get(<<"connection">>, M, #{})),
    [{b2a(Type), Scope, b2a(Tag), Options, ConnectionOptions}].

-spec pool_scope(toml_section()) -> option().
pool_scope(#{<<"scope">> := <<"single_host">>, <<"host">> := Host}) -> Host;
pool_scope(#{<<"scope">> := Scope}) -> b2a(Scope).

%% path: outgoing_pools.*.*.*
-spec pool_option(path(), toml_value()) -> [option()].
pool_option([<<"workers">>|_], V) -> [{workers, V}];
pool_option([<<"strategy">>|_], V) -> [{strategy, b2a(V)}];
pool_option([<<"call_timeout">>|_], V) -> [{call_timeout, V}].

%% path: outgoing_pools.*.connection
-spec connection_options(path(), toml_section()) -> [option()].
connection_options([_, _, <<"rdbms">>|_] = Path, Options) ->
    [{server, rdbms_server(Path, Options)}];
connection_options([_, _, <<"riak">>|_] = Path, Options = #{<<"username">> := UserName,
                                                            <<"password">> := Password}) ->
    M = maps:without([<<"username">>, <<"password">>], Options),
    [{credentials, b2l(UserName), b2l(Password)} | parse_section(Path, M)];
connection_options(Path, Options) ->
    parse_section(Path, Options).

%% path: outgoing_pools.rdbms.connection
-spec rdbms_server(path(), toml_section()) -> option().
rdbms_server(_Path, #{<<"driver">> := <<"odbc">>,
                      <<"settings">> := Settings}) ->
    b2l(Settings);
rdbms_server(Path, #{<<"driver">> := Driver,
                     <<"host">> := Host,
                     <<"database">> := Database,
                     <<"username">> := UserName,
                     <<"password">> := Password} = M) ->
    DriverA = b2a(Driver),
    HostS = b2l(Host),
    DatabaseS = b2l(Database),
    UserNameS = b2l(UserName),
    PasswordS = b2l(Password),
    case {maps:get(<<"port">>, M, no_port), db_tls(Path, M)} of
        {no_port, no_tls} -> {DriverA, HostS, DatabaseS, UserNameS, PasswordS};
        {Port, no_tls} -> {DriverA, HostS, Port, DatabaseS, UserNameS, PasswordS};
        {no_port, TLS} -> {DriverA, HostS, DatabaseS, UserNameS, PasswordS, TLS};
        {Port, TLS} -> {DriverA, HostS, Port, DatabaseS, UserNameS, PasswordS, TLS}
    end.

-spec db_tls(path(), toml_section()) -> [option()] | no_tls.
db_tls(Path, #{<<"driver">> := <<"mysql">>, <<"tls">> := Opts}) ->
    parse_section([<<"tls">> | Path], Opts);
db_tls(Path, #{<<"driver">> := <<"pgsql">>, <<"tls">> := Opts}) ->
    {SSLMode, Opts1} = case maps:take(<<"required">>, Opts) of
                           {true, M} -> {required, M};
                           {false, M} -> {true, M};
                           error -> {true, Opts}
                       end,
    [{ssl, SSLMode}, {ssl_opts, parse_section([<<"tls">> | Path], Opts1)}];
db_tls(_, _) -> no_tls.

%% path: outgoing_pools.redis.connection.*
-spec redis_option(path(), toml_value()) -> [option()].
redis_option([<<"host">>|_], Host) -> [{host, b2l(Host)}];
redis_option([<<"port">>|_], Port) -> [{port, Port}];
redis_option([<<"database">>|_], Database) -> [{database, b2l(Database)}];
redis_option([<<"password">>|_], Password) -> [{password, b2l(Password)}].

%% path: outgoing_pools.ldap.connection.*
-spec ldap_option(path(), toml_value()) -> [option()].
ldap_option([<<"host">>|_], Host) -> [{host, b2l(Host)}];
ldap_option([<<"port">>|_], Port) -> [{port, Port}];
ldap_option([<<"rootdn">>|_], RootDN) -> [{rootdn, b2l(RootDN)}];
ldap_option([<<"password">>|_], Password) -> [{password, b2l(Password)}];
ldap_option([<<"encrypt">>|_], <<"tls">>) -> [{encrypt, tls}];
ldap_option([<<"encrypt">>|_], <<"none">>) -> [{encrypt, none}];
ldap_option([<<"tls">>|_] = Path, Options) -> [{tls_options, parse_section(Path, Options)}].

%% path: outgoing_pools.riak.connection.*
-spec riak_option(path(), toml_value()) -> [option()].
riak_option([<<"address">>|_], Addr) -> [{address, b2l(Addr)}];
riak_option([<<"port">>|_], Port) -> [{port, Port}];
riak_option([<<"cacertfile">>|_], Path) -> [{cacertfile, b2l(Path)}];
riak_option([<<"tls">>|_] = Path, Options) -> [{ssl_opts, parse_section(Path, Options)}].

%% path: outgoing_pools.cassandra.connnection.*
-spec cassandra_option(path(), toml_value()) -> [option()].
cassandra_option([<<"servers">>|_], Servers) -> [{servers, [cassandra_server(S) || S <- Servers]}];
cassandra_option([<<"keyspace">>|_], KeySpace) -> [{keyspace, b2a(KeySpace)}];
cassandra_option([<<"tls">>|_] = Path, Options) -> [{ssl, parse_section(Path, Options)}].

%% path: outgoing_pools.cassandra.connection.servers[]
-spec cassandra_server(toml_section()) -> option().
cassandra_server(#{<<"ip_address">> := IPAddr, <<"port">> := Port}) -> {b2l(IPAddr), Port};
cassandra_server(#{<<"ip_address">> := IPAddr}) -> b2l(IPAddr).

%% path: outgoing_pools.elastic.connection.*
-spec elastic_option(path(), toml_value()) -> [option()].
elastic_option([<<"host">>|_], Host) -> [{host, b2l(Host)}];
elastic_option([<<"port">>|_], Port) -> [{port, Port}].

%% path: services.*
-spec process_service(path(), toml_section()) -> [option()].
process_service([S|_] = Path, Opts) ->
    [{b2a(S), parse_section(Path, Opts)}].

%% path: services.*.*
-spec service_opt(path(), toml_value()) -> [option()].
service_opt([<<"submods">>, <<"service_admin_extra">>|_], V) ->
    [{submods, [b2a(M) || M <- V]}];
service_opt([<<"initial_report">>, <<"service_mongoose_system_metrics">>|_], V) ->
    [{initial_report, V}];
service_opt([<<"periodic_report">>, <<"service_mongoose_system_metrics">>|_], V) ->
    [{periodic_report, V}];
service_opt([<<"report">>, <<"service_mongoose_system_metrics">>|_], true) ->
    [report];
service_opt([<<"report">>, <<"service_mongoose_system_metrics">>|_], false) ->
    [no_report];
service_opt([<<"tracking_id">>, <<"service_mongoose_system_metrics">>|_],  V) ->
    [{tracking_id, b2l(V)}].

%% path: modules.*
-spec process_module(path(), toml_section()) -> [option()].
process_module([Mod|_] = Path, Opts) ->
    [{b2a(Mod), parse_section(Path, Opts)}].

%% path: modules.*.*
-spec module_opt(path(), toml_value()) -> [option()].
module_opt([<<"users_can_see_hidden_services">>, <<"mod_disco">>|_], V) ->
    [{users_can_see_hidden_services, V}];
module_opt([<<"access_max_user_messages">>, <<"mod_offline">>|_], V) ->
    [{access_max_user_messages, b2a(V)}];
module_opt([<<"welcome_message">>, <<"mod_register">>|_], V) ->
    [{welcome_message, {b2l(V)}}];
module_opt([<<"ip_access">>, <<"mod_register">>|_] = Path, V) ->
    Rules = parse_list(Path, V),
    [{ip_access, Rules}];
module_opt([<<"access">>, <<"mod_register">>|_], V) ->
    [{access, b2a(V)}];
module_opt([<<"host">>, <<"mod_vcard">>|_], V) ->
    [{host, b2l(V)}];
module_opt([<<"ldap_base">>, <<"mod_vcard">>|_], V) ->
    [{ldap_base, b2l(V)}];
module_opt([<<"ldap_filter">>, <<"mod_vcard">>|_], V) ->
    [{ldap_filter, b2l(V)}];
module_opt([<<"backend">>|_], V) ->
    [{backend, b2a(V)}].

-spec mod_register_ip_access_rule(path(), toml_section()) -> [option()].
mod_register_ip_access_rule(_, #{<<"address">> := Addr, <<"policy">> := Policy}) ->
    [{b2a(Policy), b2l(Addr)}].

%% path: (host_config[].)shaper.*
-spec process_shaper(path(), toml_section()) -> [config()].
process_shaper([Name, _|Path], #{<<"max_rate">> := MaxRate}) ->
    [#config{key = {shaper, b2a(Name), host(Path)}, value = {maxrate, MaxRate}}].

%% path: (host_config[].)acl.*
-spec process_acl(path(), toml_value()) -> [config()].
process_acl([ACLName, _|Path], Content) ->
    [acl:to_record(host(Path), b2a(ACLName), acl_data(Content))].

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
-spec process_access_rule(path(), toml_value()) -> [config()].
process_access_rule([Name, _|Path], Contents) ->
    Rules = [{access_rule_value(Value), b2a(ACL)} ||
                #{<<"acl">> := ACL, <<"value">> := Value} <- Contents],
    [#config{key = {access, b2a(Name), host(Path)}, value = Rules}].

host([]) -> global;
host([{host, Host}, _]) -> Host.

-spec access_rule_value(toml_value()) -> option().
access_rule_value(B) when is_binary(B) -> b2a(B);
access_rule_value(V) -> V.

%% path: s2s
-spec s2s_dns_opts(toml_section()) -> config_list().
s2s_dns_opts(#{<<"dns_timeout">> := Timeout, <<"dns_retries">> := Retries}) ->
    [#local_config{key = s2s_dns_options, value = [{timeout, Timeout}, {retries, Retries}]}];
s2s_dns_opts(#{<<"dns_timeout">> := Timeout}) ->
    [#local_config{key = s2s_dns_options, value = [{timeout, Timeout}]}];
s2s_dns_opts(#{<<"dns_retries">> := Retries}) ->
    [#local_config{key = s2s_dns_options, value = [{retries, Retries}]}];
s2s_dns_opts(_) -> [].

-spec s2s_outgoing_opts(toml_section()) -> [option()].
s2s_outgoing_opts(#{<<"connection_timeout">> := Timeout, <<"preferred_ip_version">> := IPV}) ->
    [#local_config{key = outgoing_s2s_options,
        value = {s2s_preferred_address_family(IPV), Timeout}}];
s2s_outgoing_opts(#{<<"connection_timeout">> := Timeout}) ->
    [#local_config{key = outgoing_s2s_options, value = Timeout}];
s2s_outgoing_opts(#{<<"preferred_ip_version">> := IPV}) ->
    [#local_config{key = outgoing_s2s_options, value = s2s_preferred_address_family(IPV)}];
s2s_outgoing_opts(_) -> [].

%% path: s2s.*
-spec process_s2s_option(path(), toml_value()) -> config_list().
process_s2s_option([<<"use_starttls">>|_], V) ->
    [#local_config{key = s2s_use_starttls, value = b2a(V)}];
process_s2s_option([<<"certfile">>|_], V) ->
    [#local_config{key = s2s_certfile, value = b2l(V)}];
process_s2s_option([<<"default_policy">>|_], V) ->
    ?HOST_F([#local_config{key = {s2s_default_policy, Host}, value = b2a(V)}]);
process_s2s_option([<<"outgoing_port">>|_], V) ->
    [#local_config{key = outgoing_s2s_port, value = V}];
process_s2s_option([<<"address">>|_], Addrs) ->
    [#local_config{key = {s2s_addr, Host}, value = s2s_address(Addr)}
     || Addr = #{<<"host">> := Host} <- Addrs];
process_s2s_option([<<"ciphers">>|_], V) ->
    [#local_config{key = s2s_ciphers, value = b2l(V)}];
process_s2s_option([<<"domain_certfile">>|_], DomCerts) ->
    [#local_config{key = {domain_certfile, b2l(Dom)}, value = b2l(Cert)}
        || #{<<"domain">> := Dom, <<"certfile">> := Cert} <- DomCerts];
process_s2s_option([<<"connection_timeout">>|_], <<"infinity">>) ->
    [#local_config{key = s2s_connection_timeout, value = infinity}];
process_s2s_option([<<"connection_timeout">>|_], V) ->
    [#local_config{key = s2s_connection_timeout, value = V}];
process_s2s_option([<<"preferred_ip_version">>|_], V) ->
    [#local_config{key = preferred_address_family,
        value = s2s_preferred_address_family(V)}];
process_s2s_option([<<"shared">>|_], V) ->
    ?HOST_F([#local_config{key = {s2s_shared, Host}, value = V}]);
process_s2s_option([<<"max_retry_delay">>|_], V) ->
    ?HOST_F([#local_config{key = {s2s_max_retry_delay, Host}, value = V}]).

s2s_preferred_address_family(4) -> [ipv4, ipv6];
s2s_preferred_address_family(6) -> [ipv6, ipv4].

%% path: s2s.address[]
-spec s2s_address(toml_section()) -> option().
s2s_address(#{<<"ip_address">> := IP, <<"port">> := Port}) ->
    {b2l(IP), Port};
s2s_address(#{<<"ip_address">> := IP}) ->
    b2l(IP).

%% path: host_config[]
-spec process_host_item(path(), toml_section()) -> config_list().
process_host_item(Path, M) ->
    {_Host, Sections} = maps:take(<<"host">>, M),
    parse_section(Path, Sections).

%% path: host_config[].*
-spec process_host_section(path(), toml_section()) -> config_list().
process_host_section([<<"auth">>, {host, Host}|_] = Path, Content) ->
    AuthOpts = parse_section(Path, Content),
    partition_auth_opts(AuthOpts, Host);
process_host_section([<<"modules">>|Tail] = Path, Content) ->
    Mods = parse_section(Path, Content),
    [#local_config{key = {modules, host(Tail)}, value = Mods}];
process_host_section(Path, Content) ->
    parse_section(Path, Content).

%% path: listen.http[].tls.*,
%%       listen.c2s[].tls.*,
%%       outgoing_pools.rdbms.connection.tls.*,
%%       outgoing_pools.ldap.connection.tls.*,
%%       outgoing_pools.riak.connection.tls.*,
%%       outgoing_pools.cassandra.connection.tls.*
-spec client_tls_option(path(), toml_value()) -> [option()].
client_tls_option([<<"verify_peer">>|_], V) -> [{verify, verify_peer(V)}];
client_tls_option([<<"certfile">>|_], V) -> [{certfile, b2l(V)}];
client_tls_option([<<"cacertfile">>|_], V) -> [{cacertfile, b2l(V)}];
client_tls_option([<<"keyfile">>|_], V) -> [{keyfile, b2l(V)}];
client_tls_option([<<"password">>|_], V) -> [{password, b2l(V)}];
client_tls_option([<<"server_name_indication">>|_], false) -> [{server_name_indication, disable}];
client_tls_option([<<"ciphers">>|_], L) -> [{ciphers, [tls_cipher(C) || C <- L]}];
client_tls_option([<<"versions">>|_], L) -> [{versions, [b2a(V) || V <- L]}].

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

set_overrides(Overrides, State) ->
    lists:foldl(fun({override, Scope}, CurrentState) ->
                        mongoose_config_parser:override(Scope, CurrentState)
                end, State, Overrides).

%% TODO replace with binary_to_existing_atom where possible, prevent atom leak
b2a(B) -> binary_to_atom(B, utf8).

b2l(B) -> binary_to_list(B).

-spec parse_section(path(), toml_section()) -> [option()].
parse_section(Path, M) ->
    lists:flatmap(fun({K, V}) ->
                          Handler = handler([K|Path]),
                          Handler([K|Path], V)
                  end, maps:to_list(M)).

-spec parse_list(path(), [toml_value()]) -> [option()].
parse_list(Path, L) ->
    lists:flatmap(fun(Elem) ->
                          Key = item_key(Path, Elem),
                          Handler = handler([Key|Path]),
                          Handler([Key|Path], Elem)
                  end, L).

-spec handler(path()) -> fun((path(), toml_value()) -> [option()]).
handler([_]) -> fun process_section/2;

%% general
handler([_, <<"general">>]) -> fun process_general/2;

%% listen
handler([_, <<"listen">>]) -> fun parse_list/2;
handler([_, _, <<"listen">>]) -> fun process_listener/2;
handler([_, _, <<"http">>, <<"listen">>]) -> fun http_listener_opt/2;
handler([_, _, <<"c2s">>, <<"listen">>]) -> fun c2s_listener_opt/2;
handler([_, _, <<"s2s">>, <<"listen">>]) -> fun s2s_listener_opt/2;
handler([_, _, <<"service">>, <<"listen">>]) -> fun service_listener_opt/2;
handler([_, <<"tls">>, _, _, <<"listen">>]) -> fun client_tls_option/2;
handler([_, <<"transport">>, _, <<"http">>, <<"listen">>]) -> fun cowboy_transport_opt/2;
handler([_, <<"protocol">>, _, <<"http">>, <<"listen">>]) -> fun cowboy_protocol_opt/2;
handler([_, <<"handlers">>, _, <<"http">>, <<"listen">>]) -> fun parse_list/2;
handler([_, _, <<"handlers">>, _, <<"http">>, <<"listen">>]) -> fun cowboy_module/2;
handler([_, <<"ejabberd_service">>, _, <<"mod_websockets">>, <<"handlers">>, _,
         <<"http">>, <<"listen">>]) -> fun service_listener_opt/2;

%% auth
handler([_, <<"auth">>]) -> fun auth_option/2;

%% outgoing_pools
handler([_, <<"outgoing_pools">>]) -> fun parse_section/2;
handler([_, _, <<"outgoing_pools">>]) -> fun process_pool/2;
handler([_, _, _, <<"outgoing_pools">>]) -> fun pool_option/2;
handler([_, <<"connection">>, _, <<"redis">>, <<"outgoing_pools">>]) -> fun redis_option/2;
handler([_, <<"connection">>, _, <<"ldap">>, <<"outgoing_pools">>]) -> fun ldap_option/2;
handler([_, <<"connection">>, _, <<"riak">>, <<"outgoing_pools">>]) -> fun riak_option/2;
handler([_, <<"connection">>, _, <<"cassandra">>, <<"outgoing_pools">>]) -> fun cassandra_option/2;
handler([_, <<"connection">>, _, <<"elastic">>, <<"outgoing_pools">>]) -> fun elastic_option/2;
handler([_, <<"tls">>, <<"connection">>, _, _, <<"outgoing_pools">>]) -> fun client_tls_option/2;

%% services
handler([_, <<"services">>]) -> fun process_service/2;
handler([_, _, <<"services">>]) -> fun service_opt/2;

%% modules
handler([_, <<"modules">>]) -> fun process_module/2;
handler([_, _, <<"modules">>]) -> fun module_opt/2;
handler([_, <<"ip_access">>, <<"mod_register">>, <<"modules">>]) ->
    fun mod_register_ip_access_rule/2;

%% shaper, acl, access
handler([_, <<"shaper">>]) -> fun process_shaper/2;
handler([_, <<"acl">>]) -> fun process_acl/2;
handler([_, <<"access">>]) -> fun process_access_rule/2;

%% s2s
handler([_, <<"s2s">>]) -> fun process_s2s_option/2;

%% host_config
handler([_, <<"host_config">>]) -> fun process_host_item/2;
handler([_, _, <<"host_config">>]) -> fun process_host_section/2;
handler([_, <<"auth">>, _, <<"host_config">>] = P) -> handler(strip_host(P));
handler([_, <<"modules">>, _, <<"host_config">>] = P) -> handler(strip_host(P));
handler([_, _, <<"modules">>, _, <<"host_config">>] = P) -> handler(strip_host(P));
handler([_, _, _, <<"modules">>, _, <<"host_config">>] = P) -> handler(strip_host(P));
handler([_, <<"shaper">>, _, <<"host_config">>] = P) -> handler(strip_host(P));
handler([_, <<"acl">>, _, <<"host_config">>] = P) -> handler(strip_host(P));
handler([_, <<"access">>, _, <<"host_config">>] = P) -> handler(strip_host(P)).

strip_host(Path) ->
    [<<"host_config">>, {host, _}|Rest] = lists:reverse(Path),
    lists:reverse(Rest).

-spec item_key(path(), toml_section()) -> tuple() | item.
item_key([<<"host_config">>], #{<<"host">> := Host}) -> {host, Host};
item_key(_, _) -> item.
