%% @doc Config parsing and processing for the TOML format
-module(mongoose_config_parser_toml).

-behaviour(mongoose_config_parser).

-export([parse_file/1]).

-ifdef(TEST).
-export([parse/1]).
-endif.

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
process_general([<<"hosts">>|_] = Path, Hosts) ->
    [#config{key = hosts, value = parse_list(Path, Hosts)}];
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
process_general([<<"override">>|_] = Path, Value) ->
    parse_list(Path, Value);
process_general([<<"pgsql_users_number_estimate">>|_], V) ->
    ?HOST_F([#local_config{key = {pgsql_users_number_estimate, Host}, value = V}]);
process_general([<<"route_subdomain">>|_], V) ->
    ?HOST_F([#local_config{key = {route_subdomain, Host}, value = b2a(V)}]);
process_general([<<"mongooseimctl_access_commands">>|_] = Path, Rules) ->
    [#local_config{key = mongooseimctl_access_commands, value = parse_list(Path, Rules)}];
process_general([<<"routing_modules">>|_] = Path, Mods) ->
    [#local_config{key = routing_modules, value = parse_list(Path, Mods)}];
process_general([<<"replaced_wait_timeout">>|_], V) ->
    ?HOST_F([#local_config{key = {replaced_wait_timeout, Host}, value = V}]);
process_general([<<"hide_service_name">>|_], V) ->
    ?HOST_F([#local_config{key = {hide_service_name, Host}, value = V}]).

-spec process_host(path(), toml_value()) -> [option()].
process_host(_Path, Val) ->
    [jid:nodeprep(Val)].

-spec process_override(path(), toml_value()) -> [option()].
process_override(_Path, Override) ->
    [{override, b2a(Override)}].

-spec ctl_access_rule(path(), toml_section()) -> [option()].
ctl_access_rule(Path, Section = #{<<"access_rule">> := Rule}) ->
    limit_keys([<<"access_rule">>, <<"commands">>, <<"argument_restrictions">>], Section),
    [{b2a(Rule),
      parse_kv(Path, <<"commands">>, Section),
      parse_kv(Path, <<"argument_restrictions">>, Section)}].

-spec ctl_access_commands(path(), toml_value()) -> option().
ctl_access_commands(_Path, <<"all">>) -> all;
ctl_access_commands(Path, Commands) -> parse_list(Path, Commands).

-spec ctl_access_arg_restriction(path(), toml_value()) -> [option()].
ctl_access_arg_restriction([Key|_], Value) ->
    [{b2a(Key), b2l(Value)}].

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
    [{ssl, parse_section(Path, Opts)}];
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
c2s_listener_opt([<<"hibernate_after">>|_], V) -> [{hibernate_after, V}];
c2s_listener_opt([{tls, _}|_] = P, V) -> listener_tls_opts(P, V);
c2s_listener_opt([<<"max_stanza_size">>|_], V) -> [{max_stanza_size, V}];
c2s_listener_opt(P, V) -> listener_opt(P, V).

%% path: listen.s2s[].*
-spec s2s_listener_opt(path(), toml_value()) -> [option()].
s2s_listener_opt([<<"access">>|_], V) -> [{access, b2a(V)}];
s2s_listener_opt([<<"shaper">>|_], V) -> [{shaper, b2a(V)}];
s2s_listener_opt([<<"tls">>|_] = P, V) -> parse_section(P, V);
s2s_listener_opt([<<"max_stanza_size">>|_], V) -> [{max_stanza_size, V}];
s2s_listener_opt(P, V) -> listener_opt(P, V).

%% path: listen.service[].*,
%%       listen.http[].handlers.mod_websockets[].service.*
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

%% path: listen.http[].tls.*
-spec https_option(path(), toml_value()) -> [option()].
https_option([<<"verify_mode">>|_], Value) -> [{verify_mode, b2a(Value)}];
https_option(Path, Value) -> tls_option(Path, Value).

%% path: listen.c2s[].tls.*
-spec c2s_tls_option(path(), toml_value()) -> option().
c2s_tls_option([<<"mode">>|_], V) -> [b2a(V)];
c2s_tls_option([<<"verify_peer">>|_], V) -> [verify_peer(V)];
c2s_tls_option([_, {tls, fast_tls}|_] = Path, V) -> fast_tls_option(Path, V);
c2s_tls_option([<<"verify_mode">>, {tls, just_tls}|_], V) -> b2a(V);
c2s_tls_option([<<"disconnect_on_failure">>, {tls, just_tls}|_], V) -> V;
c2s_tls_option([_, {tls, just_tls}|_] = Path, V) -> tls_option(Path, V).

%% path: listen.s2s[].tls.*
-spec s2s_tls_option(path(), toml_value()) -> [option()].
s2s_tls_option([Opt|_] = Path, Val) when Opt =:= <<"cacertfile">>;
                                         Opt =:= <<"dhfile">>;
                                         Opt =:= <<"ciphers">> ->
    fast_tls_option(Path, Val).

%% path: listen.http[].transport.*
-spec cowboy_transport_opt(path(), toml_value()) -> [option()].
cowboy_transport_opt([<<"num_acceptors">>|_], N) -> [{num_acceptors, N}];
cowboy_transport_opt([<<"max_connections">>|_], <<"infinity">>) -> [{max_connections, infinity}];
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
cowboy_module_options([_, <<"mod_websockets">>|_] = Path, Opts) ->
    parse_section(Path, Opts);
cowboy_module_options([_, <<"lasse_handler">>|_], Opts) ->
    limit_keys([<<"module">>], Opts),
    #{<<"module">> := Module} = Opts,
    [b2a(Module)];
cowboy_module_options([_, <<"cowboy_static">>|_], Opts) ->
    limit_keys([<<"type">>, <<"app">>, <<"content_path">>], Opts),
    #{<<"type">> := Type,
      <<"app">> := App,
      <<"content_path">> := Path} = Opts,
    {b2a(Type), b2a(App), b2l(Path), [{mimetypes, cow_mimetypes, all}]};
cowboy_module_options([_, <<"cowboy_swagger_redirect_handler">>|_], Opts) ->
    Opts = #{};
cowboy_module_options([_, <<"cowboy_swagger_json_handler">>|_], Opts) ->
    Opts = #{};
cowboy_module_options([_, <<"mongoose_api">>|_] = Path, Opts) ->
    #{<<"handlers">> := _} = Opts,
    parse_section(Path, Opts);
cowboy_module_options(_, Opts) ->
    limit_keys([], Opts),
    [].

%% path: listen.http[].handlers.mod_websockets[].*
-spec websockets_option(path(), toml_value()) -> [option()].
websockets_option([<<"timeout">>|_], <<"infinity">>) ->
    [{timeout, infinity}];
websockets_option([<<"timeout">>|_], V) ->
    [{timeout, V}];
websockets_option([<<"ping_rate">>|_], <<"none">>) ->
    [{ping_rate, none}];
websockets_option([<<"ping_rate">>|_], V) ->
    [{ping_rate, V}];
websockets_option([<<"max_stanza_size">>|_], <<"infinity">>) ->
    [{max_stanza_size, infinity}];
websockets_option([<<"max_stanza_size">>|_], V) ->
    [{max_stanza_size, V}];
websockets_option([<<"service">>|_] = Path, Value) ->
    [{ejabberd_service, parse_section(Path, Value)}].

%% path: listen.http[].handlers.mongoose_api[].*
-spec mongoose_api_option(path(), toml_value()) -> [option()].
mongoose_api_option([<<"handlers">>|_] = Path, Value) ->
    [{handlers, parse_list(Path, Value)}].

%% path: listen.c2s[].tls
-spec listener_tls_opts(path(), toml_section()) -> [option()].
listener_tls_opts([{tls, just_tls}|_] = Path, M) ->
    VM = just_tls_verify_fun(Path, M),
    Common = maps:with([<<"mode">>, <<"verify_peer">>], M),
    OptsM = maps:without([<<"module">>, <<"mode">>, <<"verify_peer">>,
                          <<"verify_mode">>, <<"disconnect_on_failure">>], M),
    SSLOpts = case VM ++ parse_section(Path, OptsM) of
                  [] -> [];
                  Opts -> [{ssl_options, Opts}]
              end,
    [{tls_module, just_tls}] ++ SSLOpts ++ parse_section(Path, Common);
listener_tls_opts([{tls, fast_tls}|_] = Path, M) ->
    parse_section(Path, maps:without([<<"module">>], M)).

-spec just_tls_verify_fun(path(), toml_section()) -> [option()].
just_tls_verify_fun(Path, #{<<"verify_mode">> := _} = M) ->
    VMode = parse_kv(Path, <<"verify_mode">>, M),
    Disconnect = parse_kv(Path, <<"disconnect_on_failure">>, M, true),
    [{verify_fun, {VMode, Disconnect}}];
just_tls_verify_fun(_, _) -> [].

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
auth_option([<<"ldap">>|_] = Path, V) ->
    parse_section(Path, V);
% auth_option([<<"ldap_base">>|_], V) ->
%     [{ldap_base, b2l(V)}];
% auth_option([<<"ldap_filter">>|_], V) ->
%     [{ldap_filter, b2l(V)}];
auth_option([<<"extauth_instances">>|_], V) ->
    [{extauth_instances, V}].

-spec auth_ldap_option(path(), toml_section()) -> [option()].
auth_ldap_option([<<"pool_tag">>|_], V) ->
    [{ldap_pool_tag, b2a(V)}];
auth_ldap_option([<<"bind_pool_tag">>|_], V) ->
    [{ldap_bind_pool_tag, b2a(V)}];
auth_ldap_option([<<"base">>|_], V) ->
    [{ldap_base, b2l(V)}];
auth_ldap_option([<<"uids">>|_] = Path, V) ->
    [{ldap_uids, parse_list(Path, V)}];
auth_ldap_option([<<"filter">>|_], V) ->
    [{ldap_filter, b2l(V)}];
auth_ldap_option([<<"dn_filter">>|_] = Path, V) ->
    Opts = parse_section(Path, V),
    Filter = proplists:get_value(filter, Opts),
    Attrs = proplists:get_value(attributes, Opts),
    [{ldap_dn_filter, {Filter, Attrs}}];
auth_ldap_option([<<"local_filter">>|_] = Path, V) ->
    Opts = parse_section(Path, V),
    Op = proplists:get_value(operation, Opts),
    Filter = proplists:get_value(filter, Opts),
    Attrs = proplists:get_value(attributes, Opts),
    [{ldap_local_filter, {Op, {Filter, Attrs}}}];
auth_ldap_option([<<"deref">>|_], V) ->
    [{ldap_deref, b2a(V)}].

-spec auth_ldap_uids(path(), toml_section()) -> [option()].
auth_ldap_uids(_, #{<<"attr">> := Attr, <<"format">> := Format}) ->
    [{b2l(Attr), b2l(Format)}];
auth_ldap_uids(_, #{<<"attr">> := Attr}) ->
    [b2l(Attr)].

-spec auth_ldap_dn_filter(path(), toml_section()) -> [option()].
auth_ldap_dn_filter([<<"filter">>|_], V) ->
    [{filter, b2l(V)}];
auth_ldap_dn_filter([<<"attributes">>|_] = Path, V) ->
    Attrs = parse_list(Path, V),
    [{attributes, Attrs}].

-spec auth_ldap_local_filter(path(), toml_section()) -> [option()].
auth_ldap_local_filter([<<"operation">>|_], V) ->
    [{operation, b2a(V)}];
auth_ldap_local_filter([<<"filter">>|_], V) ->
    [{filter, b2l(V)}];
auth_ldap_local_filter([<<"attributes">>|_] = Path, V) ->
    Attrs = parse_list(Path, V),
    [{attributes, Attrs}].

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

%% path: outgoing_pools.*.*.*,
%%       modules.mod_event_pusher.backend.push.wpool.*
-spec pool_option(path(), toml_value()) -> [option()].
pool_option([<<"workers">>|_], V) -> [{workers, V}];
pool_option([<<"strategy">>|_], V) -> [{strategy, b2a(V)}];
pool_option([<<"call_timeout">>|_], V) -> [{call_timeout, V}].

%% path: outgoing_pools.*.connection
-spec connection_options(path(), toml_section()) -> [option()].
connection_options([_, _, <<"rdbms">>|_] = Path, Options) ->
    Interval = parse_kv(Path, <<"keepalive_interval">>, Options, undefined),
    Server = rdbms_server(Path, Options),
    case Interval of
        undefined ->
            [{server, Server}];
        V ->
            [{server, Server}, {keepalive_interval, V}]
        end;
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

%% path: outgoing_pools.http.*.connection.*
-spec http_option(path(), toml_value()) -> [option()].
http_option([<<"host">>|_], V) -> [{server, b2l(V)}];
http_option([<<"path_prefix">>|_], V) -> [{path_prefix, b2l(V)}];
http_option([<<"request_timeout">>|_], V) -> [{request_timeout, V}];
http_option([<<"http_opts">>|_] = Path, V) ->
    Opts = parse_section(Path, V),
    [{http_opts, maps:from_list(Opts)}].

%% path: outgoing_pools.http.*.connection.http_opts.*
-spec http_opts(path(), toml_value()) -> [option()].
http_opts([<<"retry">>|_], V) -> [{retry, V}];
http_opts([<<"retry_timeout">>|_], V) -> [{retry_timeout, V}].

%% path: outgoing_pools.redis.*.connection.*
-spec redis_option(path(), toml_value()) -> [option()].
redis_option([<<"host">>|_], Host) -> [{host, b2l(Host)}];
redis_option([<<"port">>|_], Port) -> [{port, Port}];
redis_option([<<"database">>|_], Database) -> [{database, b2l(Database)}];
redis_option([<<"password">>|_], Password) -> [{password, b2l(Password)}].

%% path: outgoing_pools.ldap.*.connection.*
-spec ldap_option(path(), toml_value()) -> [option()].
ldap_option([<<"host">>|_], Host) -> [{host, b2l(Host)}];
ldap_option([<<"port">>|_], Port) -> [{port, Port}];
ldap_option([<<"rootdn">>|_], RootDN) -> [{rootdn, b2l(RootDN)}];
ldap_option([<<"password">>|_], Password) -> [{password, b2l(Password)}];
ldap_option([<<"encrypt">>|_], <<"tls">>) -> [{encrypt, tls}];
ldap_option([<<"encrypt">>|_], <<"none">>) -> [{encrypt, none}];
ldap_option([<<"servers">>|_] = Path, V) -> [{servers, parse_list(Path, V)}];
ldap_option([<<"connect_interval">>|_], V) -> [{connect_interval, V}];
ldap_option([<<"tls">>|_] = Path, Options) -> [{tls_options, parse_section(Path, Options)}].

%% path: outgoing_pools.riak.*.connection.*
-spec riak_option(path(), toml_value()) -> [option()].
riak_option([<<"address">>|_], Addr) -> [{address, b2l(Addr)}];
riak_option([<<"port">>|_], Port) -> [{port, Port}];
riak_option([<<"credentials">>|_] = Path, V) ->
    Creds = parse_section(Path, V),
    User = proplists:get_value(user, Creds),
    Pass = proplists:get_value(password, Creds),
    [{credentials, User, Pass}];
riak_option([<<"cacertfile">>|_], Path) -> [{cacertfile, b2l(Path)}];
riak_option([<<"tls">>|_] = Path, Options) -> [{ssl_opts, parse_section(Path, Options)}].

%% path: outgoing_pools.riak.*.connection.credentials.*
-spec riak_credentials(path(), toml_value()) -> [option()].
riak_credentials([<<"user">>|_], V) -> [{user, b2l(V)}];
riak_credentials([<<"password">>|_], V) -> [{password, b2l(V)}].

%% path: outgoing_pools.cassandra.*.connnection.*
-spec cassandra_option(path(), toml_value()) -> [option()].
cassandra_option([<<"servers">>|_] = Path, V) -> [{servers, parse_list(Path, V)}];
cassandra_option([<<"keyspace">>|_], KeySpace) -> [{keyspace, b2l(KeySpace)}];
cassandra_option([<<"tls">>|_] = Path, Options) -> [{ssl, parse_section(Path, Options)}].

%% path: outgoing_pools.cassandra.*.connection.servers[]
-spec cassandra_server(path(), toml_section()) -> [option()].
cassandra_server(_, #{<<"ip_address">> := IPAddr, <<"port">> := Port}) -> [{b2l(IPAddr), Port}];
cassandra_server(_, #{<<"ip_address">> := IPAddr}) -> [b2l(IPAddr)].

%% path: outgoing_pools.elastic.*.connection.*
-spec elastic_option(path(), toml_value()) -> [option()].
elastic_option([<<"host">>|_], Host) -> [{host, b2l(Host)}];
elastic_option([<<"port">>|_], Port) -> [{port, Port}].

%% path: outgoing_pools.rabbit.*.connection.*
-spec rabbit_option(path(), toml_value()) -> [option()].
rabbit_option([<<"amqp_host">>|_], V) -> [{amqp_host, b2l(V)}];
rabbit_option([<<"amqp_port">>|_], V) -> [{amqp_port, V}];
rabbit_option([<<"amqp_username">>|_], V) -> [{amqp_username, b2l(V)}];
rabbit_option([<<"amqp_password">>|_], V) -> [{amqp_password, b2l(V)}];
rabbit_option([<<"confirms_enabled">>|_], V) -> [{confirms_enabled, V}];
rabbit_option([<<"max_worker_queue_len">>|_], <<"infinity">>) ->
     [{max_worker_queue_len, infinity}];
rabbit_option([<<"max_worker_queue_len">>|_], V) -> [{max_worker_queue_len, V}].

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
module_opt([<<"report_commands_node">>, <<"mod_adhoc">>|_], V) ->
    [{report_commands_node, V}];
module_opt([<<"validity_period">>, <<"mod_auth_token">>|_] = Path, V) ->
    parse_list(Path, V);
module_opt([<<"inactivity">>, <<"mod_bosh">>|_], <<"infinity">>) ->
    [{inactivity, infinity}];
module_opt([<<"inactivity">>, <<"mod_bosh">>|_], V) ->
    [{inactivity, V}];
module_opt([<<"max_wait">>, <<"mod_bosh">>|_], <<"infinity">>) ->
    [{max_wait, infinity}];
module_opt([<<"max_wait">>, <<"mod_bosh">>|_], V) ->
    [{max_wait, V}];
module_opt([<<"server_acks">>, <<"mod_bosh">>|_], V) ->
    [{server_acks, V}];
module_opt([<<"backend">>, <<"mod_bosh">>|_], V) ->
    [{backend, b2a(V)}];
module_opt([<<"maxpause">>, <<"mod_bosh">>|_], V) ->
    [{maxpause, V}];
module_opt([<<"cache_size">>, <<"mod_caps">>|_], V) ->
    [{cache_size, V}];
module_opt([<<"cache_life_time">>, <<"mod_caps">>|_], V) ->
    [{cache_life_time, V}];
module_opt([<<"buffer_max">>, <<"mod_csi">>|_], V) ->
    [{buffer_max, V}];
module_opt([<<"extra_domains">>, <<"mod_disco">>|_] = Path, V) ->
    Domains = parse_list(Path, V),
    [{extra_domains, Domains}];
module_opt([<<"server_info">>, <<"mod_disco">>|_] = Path, V) ->
    Info = parse_list(Path, V),
    [{server_info, Info}];
module_opt([<<"users_can_see_hidden_services">>, <<"mod_disco">>|_], V) ->
    [{users_can_see_hidden_services, V}];
module_opt([<<"backend">>, <<"mod_event_pusher">>|_] = Path, V) ->
    Backends = parse_section(Path, V),
    [{backends, Backends}];
module_opt([<<"host">>, <<"mod_http_upload">>|_], V) ->
    [{host, b2l(V)}];
module_opt([<<"backend">>, <<"mod_http_upload">>|_], V) ->
    [{backend, b2a(V)}];
module_opt([<<"expiration_time">>, <<"mod_http_upload">>|_], V) ->
    [{expiration_time, V}];
module_opt([<<"token_bytes">>, <<"mod_http_upload">>|_], V) ->
    [{token_bytes, V}];
module_opt([<<"max_file_size">>, <<"mod_http_upload">>|_], V) ->
    [{max_file_size, V}];
module_opt([<<"s3">>, <<"mod_http_upload">>|_] = Path, V) ->
    S3Opts = parse_section(Path, V),
    [{s3, S3Opts}];
module_opt([<<"backend">>, <<"mod_inbox">>|_], V) ->
    [{backend, b2a(V)}];
module_opt([<<"reset_markers">>, <<"mod_inbox">>|_] = Path, V) ->
    Markers = parse_list(Path, V),
    [{reset_markers, Markers}];
module_opt([<<"groupchat">>, <<"mod_inbox">>|_] = Path, V) ->
    GChats = parse_list(Path, V),
    [{groupchat, GChats}];
module_opt([<<"aff_changes">>, <<"mod_inbox">>|_], V) ->
    [{aff_changes, V}];
module_opt([<<"remove_on_kicked">>, <<"mod_inbox">>|_], V) ->
    [{remove_on_kicked, V}];
module_opt([<<"global_host">>, <<"mod_global_distrib">>|_], V) ->
    [{global_host, b2l(V)}];
module_opt([<<"local_host">>, <<"mod_global_distrib">>|_], V) ->
    [{local_host, b2l(V)}];
module_opt([<<"message_ttl">>, <<"mod_global_distrib">>|_], V) ->
    [{message_ttl, V}];
module_opt([<<"connections">>, <<"mod_global_distrib">>|_] = Path, V) ->
    Conns = parse_section(Path, V),
    [{connections, Conns}];
module_opt([<<"cache">>, <<"mod_global_distrib">>|_] = Path, V) ->
    Cache = parse_section(Path, V),
    [{cache, Cache}];
module_opt([<<"bounce">>, <<"mod_global_distrib">>|_], false) ->
    [{bounce, false}];
module_opt([<<"bounce">>, <<"mod_global_distrib">>|_] = Path, V) ->
    Bounce = parse_section(Path, V),
    [{bounce, Bounce}];
module_opt([<<"redis">>, <<"mod_global_distrib">>|_] = Path, V) ->
    Redis = parse_section(Path, V),
    [{redis, Redis}];
module_opt([<<"hosts_refresh_interval">>, <<"mod_global_distrib">>|_], V) ->
    [{hosts_refresh_interval, V}];
module_opt([<<"proxy_host">>, <<"mod_jingle_sip">>|_], V) ->
    [{proxy_host, b2l(V)}];
module_opt([<<"proxy_port">>, <<"mod_jingle_sip">>|_], V) ->
    [{proxy_port, V}];
module_opt([<<"listen_port">>, <<"mod_jingle_sip">>|_], V) ->
    [{listen_port, V}];
module_opt([<<"local_host">>, <<"mod_jingle_sip">>|_], V) ->
    [{local_host, b2l(V)}];
module_opt([<<"sdp_origin">>, <<"mod_jingle_sip">>|_], V) ->
    [{sdp_origin, b2l(V)}];
module_opt([<<"ram_key_size">>, <<"mod_keystore">>|_], V) ->
    [{ram_key_size, V}];
module_opt([<<"keys">>, <<"mod_keystore">>|_] = Path, V) ->
    Keys = parse_list(Path, V),
    [{keys, Keys}];
module_opt([<<"archive_chat_markers">>, <<"mod_mam_meta">>|_], V) ->
    [{archive_chat_markers, V}];
module_opt([<<"pm">>, <<"mod_mam_meta">>|_], false) ->
    [];
module_opt([<<"pm">>, <<"mod_mam_meta">>|_] = Path, V) ->
    PM = parse_section(Path, V),
    [{pm, PM}];
module_opt([<<"muc">>, <<"mod_mam_meta">>|_], false) ->
    [];
module_opt([<<"muc">>, <<"mod_mam_meta">>|_] = Path, V) ->
    Muc = parse_section(Path, V),
    [{muc, Muc}];
module_opt([<<"extra_lookup_params">>, <<"mod_mam_meta">>|_], V) ->
    [{extra_lookup_params, b2a(V)}];
module_opt([_, <<"mod_mam_meta">>|_] = Path, V) ->
    mod_mam_opts(Path, V);
module_opt([<<"host">>, <<"mod_muc">>|_], V) ->
    [{host, b2l(V)}];
module_opt([<<"access">>, <<"mod_muc">>|_], V) ->
    [{access, b2a(V)}];
module_opt([<<"access_create">>, <<"mod_muc">>|_], V) ->
    [{access_create, b2a(V)}];
module_opt([<<"access_admin">>, <<"mod_muc">>|_], V) ->
    [{access_admin, b2a(V)}];
module_opt([<<"access_persistent">>, <<"mod_muc">>|_], V) ->
    [{access_persistent, b2a(V)}];
module_opt([<<"history_size">>, <<"mod_muc">>|_], V) ->
    [{history_size, V}];
module_opt([<<"room_shaper">>, <<"mod_muc">>|_], V) ->
    [{room_shaper, b2a(V)}];
module_opt([<<"max_room_id">>, <<"mod_muc">>|_], <<"infinity">>) ->
    [{max_room_id, infinite}];
module_opt([<<"max_room_id">>, <<"mod_muc">>|_], V) ->
    [{max_room_id, V}];
module_opt([<<"max_room_name">>, <<"mod_muc">>|_], <<"infinity">>) ->
    [{max_room_name, infinite}];
module_opt([<<"max_room_name">>, <<"mod_muc">>|_], V) ->
    [{max_room_name, V}];
module_opt([<<"max_room_desc">>, <<"mod_muc">>|_], <<"infinity">>) ->
    [{max_room_desc, infinite}];
module_opt([<<"max_room_desc">>, <<"mod_muc">>|_], V) ->
    [{max_room_desc, V}];
module_opt([<<"min_message_interval">>, <<"mod_muc">>|_], V) ->
    [{min_message_interval, V}];
module_opt([<<"min_presence_interval">>, <<"mod_muc">>|_], V) ->
    [{min_presence_interval, V}];
module_opt([<<"max_users">>, <<"mod_muc">>|_], V) ->
    [{max_users, V}];
module_opt([<<"max_users_admin_threshold">>, <<"mod_muc">>|_], V) ->
    [{max_users_admin_threshold, V}];
module_opt([<<"user_message_shaper">>, <<"mod_muc">>|_], V) ->
    [{user_message_shaper, V}];
module_opt([<<"user_presence_shaper">>, <<"mod_muc">>|_], V) ->
    [{user_presence_shaper, V}];
module_opt([<<"max_user_conferences">>, <<"mod_muc">>|_], V) ->
    [{max_user_conferences, V}];
module_opt([<<"http_auth_pool">>, <<"mod_muc">>|_], V) ->
    [{http_auth_pool, b2a(V)}];
module_opt([<<"load_permanent_rooms_at_startup">>, <<"mod_muc">>|_], V) ->
    [{load_permanent_rooms_at_startup, V}];
module_opt([<<"hibernate_timeout">>, <<"mod_muc">>|_], V) ->
    [{hibernate_timeout, V}];
module_opt([<<"hibernated_room_check_interval">>, <<"mod_muc">>|_], <<"infinity">>) ->
    [{hibernated_room_check_interval, infinity}];
module_opt([<<"hibernated_room_check_interval">>, <<"mod_muc">>|_], V) ->
    [{hibernated_room_check_interval, V}];
module_opt([<<"hibernated_room_timeout">>, <<"mod_muc">>|_], <<"infinity">>) ->
    [{hibernated_room_timeout, infinity}];
module_opt([<<"hibernated_room_timeout">>, <<"mod_muc">>|_], V) ->
    [{hibernated_room_timeout, V}];
module_opt([<<"default_room">>, <<"mod_muc">>|_] = Path, V) ->
    Defaults = parse_section(Path, V),
    [{default_room_options, Defaults}];
module_opt([<<"outdir">>, <<"mod_muc_log">>|_], V) ->
    [{outdir, b2l(V)}];
module_opt([<<"access_log">>, <<"mod_muc_log">>|_], V) ->
    [{access_log, b2a(V)}];
module_opt([<<"dirtype">>, <<"mod_muc_log">>|_], V) ->
    [{dirtype, b2a(V)}];
module_opt([<<"dirname">>, <<"mod_muc_log">>|_], V) ->
    [{dirname, b2a(V)}];
module_opt([<<"file_format">>, <<"mod_muc_log">>|_], V) ->
    [{file_format, b2a(V)}];
module_opt([<<"css_file">>, <<"mod_muc_log">>|_], <<"false">>) ->
    [{cssfile, false}];
module_opt([<<"css_file">>, <<"mod_muc_log">>|_], V) ->
    [{cssfile, V}];
module_opt([<<"timezone">>, <<"mod_muc_log">>|_], V) ->
    [{timezone, b2a(V)}];
module_opt([<<"top_link">>, <<"mod_muc_log">>|_] = Path, V) ->
    Link = list_to_tuple(parse_section(Path, V)),
    [{top_link, Link}];
module_opt([<<"spam_prevention">>, <<"mod_muc_log">>|_], V) ->
    [{spam_prevention, V}];
module_opt([<<"host">>, <<"mod_muc_light">>|_], V) ->
    [{host, b2l(V)}];
module_opt([<<"equal_occupants">>, <<"mod_muc_light">>|_], V) ->
    [{equal_occupants, V}];
module_opt([<<"legacy_mode">>, <<"mod_muc_light">>|_], V) ->
    [{legacy_mode, V}];
module_opt([<<"rooms_per_user">>, <<"mod_muc_light">>|_], <<"infinity">>) ->
    [{rooms_per_user, infinity}];
module_opt([<<"rooms_per_user">>, <<"mod_muc_light">>|_], V) ->
    [{rooms_per_user, V}];
module_opt([<<"blocking">>, <<"mod_muc_light">>|_], V) ->
    [{blocking, V}];
module_opt([<<"all_can_configure">>, <<"mod_muc_light">>|_], V) ->
    [{all_can_configure, V}];
module_opt([<<"all_can_invite">>, <<"mod_muc_light">>|_], V) ->
    [{all_can_invite, V}];
module_opt([<<"max_occupants">>, <<"mod_muc_light">>|_], <<"infinity">>) ->
    [{max_occupants, infinity}];
module_opt([<<"max_occupants">>, <<"mod_muc_light">>|_], V) ->
    [{max_occupants, V}];
module_opt([<<"rooms_per_page">>, <<"mod_muc_light">>|_], <<"infinity">>) ->
    [{rooms_per_page, infinity}];
module_opt([<<"rooms_per_page">>, <<"mod_muc_light">>|_], V) ->
    [{rooms_per_page, V}];
module_opt([<<"rooms_in_rosters">>, <<"mod_muc_light">>|_], V) ->
    [{rooms_in_rosters, V}];
module_opt([<<"config_schema">>, <<"mod_muc_light">>|_] = Path, V) ->
    Configs = parse_list(Path, V),
    [{config_schema, Configs}];
module_opt([<<"access_max_user_messages">>, <<"mod_offline">>|_], V) ->
    [{access_max_user_messages, b2a(V)}];
module_opt([<<"send_pings">>, <<"mod_ping">>|_], V) ->
    [{send_pings, V}];
module_opt([<<"ping_interval">>, <<"mod_ping">>|_], V) ->
    [{ping_interval, V}];
module_opt([<<"timeout_action">>, <<"mod_ping">>|_], V) ->
    [{timeout_action, b2a(V)}];
module_opt([<<"ping_req_timeout">>, <<"mod_ping">>|_], V) ->
    [{ping_req_timeout, V}];
module_opt([<<"host">>, <<"mod_pubsub">>|_], V) ->
    [{host, b2l(V)}];
module_opt([<<"access_createnode">>, <<"mod_pubsub">>|_], V) ->
    [{access_createnode, b2a(V)}];
module_opt([<<"max_items_node">>, <<"mod_pubsub">>|_], V) ->
    [{max_items_node, V}];
module_opt([<<"max_subscriptions_node">>, <<"mod_pubsub">>|_], V) ->
    [{max_subscriptions_node, V}];
module_opt([<<"nodetree">>, <<"mod_pubsub">>|_], V) ->
    [{nodetree, V}];
module_opt([<<"ignore_pep_from_offline">>, <<"mod_pubsub">>|_], V) ->
    [{ignore_pep_from_offline, V}];
module_opt([<<"last_item_cache">>, <<"mod_pubsub">>|_], V) ->
    [{last_item_cache, b2a(V)}];
module_opt([<<"plugins">>, <<"mod_pubsub">>|_] = Path, V) ->
    Plugs = parse_list(Path, V),
    [{plugins, Plugs}];
module_opt([<<"pep_mapping">>, <<"mod_pubsub">>|_] = Path, V) ->
    Mappings = parse_list(Path, V),
    [{pep_mapping, Mappings}];
module_opt([<<"default_node_config">>, <<"mod_pubsub">>|_] = Path, V) ->
    Config = parse_list(Path, V),
    [{default_node_config, Config}];
module_opt([<<"item_publisher">>, <<"mod_pubsub">>|_], V) ->
    [{item_publisher, V}];
module_opt([<<"sync_broadcast">>, <<"mod_pubsub">>|_], V) ->
    [{sync_broadcast, V}];
module_opt([<<"pool_name">>, <<"mod_push_service_mongoosepush">>|_], V) ->
    [{pool_name, b2a(V)}];
module_opt([<<"api_version">>, <<"mod_push_service_mongoosepush">>|_], V) ->
    [{api_version, b2l(V)}];
module_opt([<<"max_http_connections">>, <<"mod_push_service_mongoosepush">>|_], V) ->
    [{max_http_connections, V}];
module_opt([<<"access">>, <<"mod_register">>|_], V) ->
    [{access, b2a(V)}];
module_opt([<<"welcome_message">>, <<"mod_register">>|_], V) ->
    [{welcome_message, {b2l(V)}}];
module_opt([<<"registration_watchers">>, <<"mod_register">>|_] = Path, V) ->
    [{registration_watchers, parse_list(Path, V)}];
module_opt([<<"password_strength">>, <<"mod_register">>|_], V) ->
    [{password_strength, V}];
module_opt([<<"ip_access">>, <<"mod_register">>|_] = Path, V) ->
    Rules = parse_list(Path, V),
    [{ip_access, Rules}];
module_opt([<<"routes">>, <<"mod_revproxy">>|_] = Path, V) ->
    Routes = parse_list(Path, V),
    [{routes, Routes}];
module_opt([<<"versioning">>, <<"mod_roster">>|_], V) ->
    [{versioning, V}];
module_opt([<<"store_current_id">>, <<"mod_roster">>|_], V) ->
    [{store_current_id, V}];
module_opt([<<"ldap_groupattr">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_groupattr, b2l(V)}];
module_opt([<<"ldap_groupdesc">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_groupdesc, b2l(V)}];
module_opt([<<"ldap_userdesc">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_userdesc, b2l(V)}];
module_opt([<<"ldap_userid">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_userid, b2l(V)}];
module_opt([<<"ldap_memberattr">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_memberattr, b2l(V)}];
module_opt([<<"ldap_memberattr_format">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_memberattr_format, b2l(V)}];
module_opt([<<"ldap_memberattr_format_re">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_memberattr_format_re, b2l(V)}];
module_opt([<<"ldap_auth_check">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_auth_check, V}];
module_opt([<<"ldap_user_cache_validity">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_user_cache_validity, V}];
module_opt([<<"ldap_group_cache_validity">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_group_cache_validity, V}];
module_opt([<<"ldap_user_cache_size">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_user_cache_size, V}];
module_opt([<<"ldap_group_cache_size">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_group_cache_size, V}];
module_opt([<<"ldap_rfilter">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_rfilter, b2l(V)}];
module_opt([<<"ldap_gfilter">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_gfilter, b2l(V)}];
module_opt([<<"ldap_ufilter">>, <<"mod_shared_roster_ldap">>|_], V) ->
    [{ldap_ufilter, b2l(V)}];
module_opt([<<"buffer_max">>, <<"mod_stream_management">>|_], <<"infinity">>) ->
    [{buffer_max, infinity}];
module_opt([<<"buffer_max">>, <<"mod_stream_management">>|_], <<"no_buffer">>) ->
    [{buffer_max, no_buffer}];
module_opt([<<"buffer_max">>, <<"mod_stream_management">>|_], V) ->
    [{buffer_max, V}];
module_opt([<<"ack_freq">>, <<"mod_stream_management">>|_], <<"never">>) ->
    [{ack_freq, never}];
module_opt([<<"ack_freq">>, <<"mod_stream_management">>|_], V) ->
    [{ack_freq, V}];
module_opt([<<"resume_timeout">>, <<"mod_stream_management">>|_], V) ->
    [{resume_timeout, V}];
module_opt([<<"stale_h">>, <<"mod_stream_management">>|_] = Path, V) ->
    Stale = parse_section(Path, V),
    [{stale_h, Stale}];
module_opt([<<"host">>, <<"mod_vcard">>|_], V) ->
    [{host, b2l(V)}];
module_opt([<<"search">>, <<"mod_vcard">>|_], V) ->
    [{search, V}];
module_opt([<<"matches">>, <<"mod_vcard">>|_], <<"infinity">>) ->
    [{matches, infinity}];
module_opt([<<"matches">>, <<"mod_vcard">>|_], V) ->
    [{matches, V}];
module_opt([<<"ldap_vcard_map">>, <<"mod_vcard">>|_] = Path, V) ->
    Maps = parse_list(Path, V),
    [{ldap_vcard_map, Maps}];
module_opt([<<"ldap_search_fields">>, <<"mod_vcard">>|_] = Path, V) ->
    Fields = parse_list(Path, V),
    [{ldap_search_fields, Fields}];
module_opt([<<"ldap_search_reported">>, <<"mod_vcard">>|_] = Path, V) ->
    Reported = parse_list(Path, V),
    [{ldap_search_reported, Reported}];
module_opt([<<"ldap_search_operator">>, <<"mod_vcard">>|_], V) ->
    [{ldap_search_operator, b2a(V)}];
module_opt([<<"ldap_binary_search_fields">>, <<"mod_vcard">>|_], V) ->
    [{ldap_binary_search_fields, V}];
module_opt([<<"os_info">>, <<"mod_version">>|_], V) ->
    [{os_info, V}];
% General options
module_opt([<<"iqdisc">>|_], V) ->
    [{iqdisc, b2a(V)}];
module_opt([<<"backend">>|_], V) ->
    [{backend, b2a(V)}];
%% LDAP-specific options
module_opt([<<"ldap_pool_tag">>|_], V) ->
    [{ldap_pool_tag, b2a(V)}];
module_opt([<<"ldap_base">>|_], V) ->
    [{ldap_base, b2l(V)}];
module_opt([<<"ldap_filter">>|_], V) ->
    [{ldap_filter, b2l(V)}];
module_opt([<<"ldap_deref">>|_], V) ->
    [{ldap_deref, b2a(V)}];
%% Backend-specific options
module_opt([<<"riak">>|_] = Path, V) ->
    parse_section(Path, V).

%% path: modules.*.riak.*
-spec riak_opts(path(), toml_section()) -> [option()].
riak_opts([<<"defaults_bucket_type">>|_], V) ->
    [{defaults_bucket_type, V}];
riak_opts([<<"names_bucket_type">>|_], V) ->
    [{names_bucket_type, V}];
riak_opts([<<"version_bucket_type">>|_], V) ->
    [{version_bucket_type, V}];
riak_opts([<<"bucket_type">>|_], V) ->
    [{bucket_type, V}];
riak_opts([<<"search_index">>|_], V) ->
    [{search_index, V}].

-spec mod_register_ip_access_rule(path(), toml_section()) -> [option()].
mod_register_ip_access_rule(_, #{<<"address">> := Addr, <<"policy">> := Policy}) ->
    [{b2a(Policy), b2l(Addr)}].

-spec mod_auth_token_validity_periods(path(), toml_section()) -> [option()].
mod_auth_token_validity_periods(_, 
    #{<<"token">> := Token, <<"value">> := Value, <<"unit">> := Unit}) ->
        [{{validity_period, b2a(Token)}, {Value, b2a(Unit)}}].

-spec mod_disco_server_info(path(), toml_section()) -> [option()].
mod_disco_server_info(Path, #{<<"module">> := <<"all">>, <<"name">> := Name, <<"urls">> := Urls}) ->
    URLList = parse_list([<<"urls">> | Path], Urls),
    [{all, b2l(Name), URLList}];
mod_disco_server_info(Path, #{<<"module">> := Modules, <<"name">> := Name, <<"urls">> := Urls}) ->
    Mods = parse_list([<<"module">> | Path], Modules),
    URLList = parse_list([<<"urls">> | Path], Urls),
    [{Mods, b2l(Name), URLList}].

-spec mod_event_pusher_backend_sns(path(), toml_section()) -> [option()].
mod_event_pusher_backend_sns(Path, Opts) ->
    SnsOpts = parse_section(Path, Opts),
    [{sns, SnsOpts}].

-spec mod_event_pusher_backend_push(path(), toml_section()) -> [option()].
mod_event_pusher_backend_push(Path, Opts) ->
    PushOpts = parse_section(Path, Opts),
    [{push, PushOpts}].

-spec mod_event_pusher_backend_http(path(), toml_section()) -> [option()].
mod_event_pusher_backend_http(Path, Opts) ->
    HttpOpts = parse_section(Path, Opts),
    [{http, HttpOpts}].

-spec mod_event_pusher_backend_rabbit(path(), toml_section()) -> [option()].
mod_event_pusher_backend_rabbit(Path, Opts) ->
    ROpts = parse_section(Path, Opts),
    [{rabbit, ROpts}].

-spec mod_event_pusher_backend_sns_opts(path(), toml_value()) -> [option()].
mod_event_pusher_backend_sns_opts([<<"presence_updates_topic">>|_], V) ->
    [{presence_updates_topic, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"pm_messages_topic">>|_], V) ->
    [{pm_messages_topic, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"muc_messages_topic">>|_], V) ->
    [{muc_messages_topic, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"plugin_module">>|_], V) ->
    [{plugin_module, b2a(V)}];
mod_event_pusher_backend_sns_opts([<<"muc_host">>|_], V) ->
    [{muc_host, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"sns_host">>|_], V) ->
    [{sns_host, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"region">>|_], V) ->
    [{region, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"access_key_id">>|_], V) ->
    [{access_key_id, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"secret_access_key">>|_], V) ->
    [{secret_access_key, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"account_id">>|_], V) ->
    [{account_id, b2l(V)}];
mod_event_pusher_backend_sns_opts([<<"pool_size">>|_], V) ->
    [{pool_size, V}];
mod_event_pusher_backend_sns_opts([<<"publish_retry_count">>|_], V) ->
    [{publish_retry_count, V}];
mod_event_pusher_backend_sns_opts([<<"publish_retry_time_ms">>|_], V) ->
    [{publish_retry_time_ms, V}].

-spec mod_event_pusher_backend_push_opts(path(), toml_value()) -> [option()].
mod_event_pusher_backend_push_opts([<<"backend">>|_], V) ->
    [{backend, b2a(V)}];
mod_event_pusher_backend_push_opts([<<"wpool">>|_] = Path, V) ->
    WpoolOpts = parse_section(Path, V),
    [{wpool, WpoolOpts}];
mod_event_pusher_backend_push_opts([<<"plugin_module">>|_], V) ->
    [{plugin_module, b2a(V)}];
mod_event_pusher_backend_push_opts([<<"virtual_pubsub_hosts">> |_] = Path, V) ->
    VPH = parse_list(Path, V),
    [{virtual_pubsub_hosts, VPH}].

-spec mod_event_pusher_backend_http_opts(path(), toml_value()) -> [option()].
mod_event_pusher_backend_http_opts([<<"pool_name">>|_], V) ->
    [{pool_name, b2a(V)}];
mod_event_pusher_backend_http_opts([<<"path">>|_], V) ->
    [{path, b2l(V)}];
mod_event_pusher_backend_http_opts([<<"callback_module">>|_], V) ->
    [{callback_module, b2a(V)}].

-spec mod_event_pusher_backend_rabbit_opts(path(), toml_value()) -> [option()].
mod_event_pusher_backend_rabbit_opts([<<"presence_exchange">>|_] = Path, V) ->
    [{presence_exchange, parse_section(Path, V)}];
mod_event_pusher_backend_rabbit_opts([<<"chat_msg_exchange">>|_] = Path, V) ->
    [{chat_msg_exchange, parse_section(Path, V)}];
mod_event_pusher_backend_rabbit_opts([<<"groupchat_msg_exchange">>|_] = Path, V) ->
    [{groupchat_msg_exchange, parse_section(Path, V)}].

-spec mod_event_pusher_rabbit_presence_ex(path(), toml_value()) -> [option()].
mod_event_pusher_rabbit_presence_ex([<<"name">>|_], V) ->
    [{name, V}];
mod_event_pusher_rabbit_presence_ex([<<"type">>|_], V) ->
    [{type, V}].

-spec mod_event_pusher_rabbit_msg_ex(path(), toml_value()) -> [option()].
mod_event_pusher_rabbit_msg_ex([<<"name">>|_], V) ->
    [{name, V}];
mod_event_pusher_rabbit_msg_ex([<<"type">>|_], V) ->
    [{type, V}];
mod_event_pusher_rabbit_msg_ex([<<"sent_topic">>|_], V) ->
    [{sent_topic, V}];
mod_event_pusher_rabbit_msg_ex([<<"recv_topic">>|_], V) ->
    [{recv_topic, V}].

-spec mod_http_upload_s3(path(), toml_value()) -> [option()].
mod_http_upload_s3([<<"bucket_url">>|_], V) ->
    [{bucket_url, b2l(V)}];
mod_http_upload_s3([<<"add_acl">>|_], V) ->
    [{add_acl, V}];
mod_http_upload_s3([<<"region">>|_], V) ->
    [{region, b2l(V)}];
mod_http_upload_s3([<<"access_key_id">>|_], V) ->
    [{access_key_id, b2l(V)}];
mod_http_upload_s3([<<"secret_access_key">>|_], V) ->
    [{secret_access_key, b2l(V)}].

-spec mod_global_distrib_connections(path(), toml_value()) -> [option()].
mod_global_distrib_connections([<<"endpoints">>|_] = Path, V) ->
    Endpoints = parse_list(Path, V),
    [{endpoints, Endpoints}];
mod_global_distrib_connections([<<"advertised_endpoints">>|_], false) ->
    [{advertised_endpoints, false}];
mod_global_distrib_connections([<<"advertised_endpoints">>|_] = Path, V) ->
    Endpoints = parse_list(Path, V),
    [{advertised_endpoints, Endpoints}];
mod_global_distrib_connections([<<"connections_per_endpoint">>|_], V) ->
    [{connections_per_endpoint, V}];
mod_global_distrib_connections([<<"num_of_connections">>|_], V) ->
    [{num_of_connections, V}];
mod_global_distrib_connections([<<"endpoint_refresh_interval">>|_], V) ->
    [{endpoint_refresh_interval, V}];
mod_global_distrib_connections([<<"endpoint_refresh_interval_when_empty">>|_], V) ->
    [{endpoint_refresh_interval_when_empty, V}];
mod_global_distrib_connections([<<"disabled_gc_interval">>|_], V) ->
    [{disabled_gc_interval, V}];
mod_global_distrib_connections([<<"tls">>|_] = Path, V) ->
    TLSOpts = parse_section(Path, V),
    [{tls_opts, TLSOpts}].

-spec mod_global_distrib_cache(path(), toml_value()) -> [option()].
mod_global_distrib_cache([<<"cache_missed">>|_], V) ->
    [{cache_missed, V}];
mod_global_distrib_cache([<<"domain_lifetime_seconds">>|_], V) ->
    [{domain_lifetime_seconds, V}];
mod_global_distrib_cache([<<"jid_lifetime_seconds">>|_], V) ->
    [{jid_lifetime_seconds, V}];
mod_global_distrib_cache([<<"max_jids">>|_], V) ->
    [{max_jids, V}].

-spec mod_global_distrib_redis(path(), toml_value()) -> [option()].
mod_global_distrib_redis([<<"pool">>|_], V) ->
    [{pool, b2a(V)}];
mod_global_distrib_redis([<<"expire_after">>|_], V) ->
    [{expire_after, V}];
mod_global_distrib_redis([<<"refresh_after">>|_], V) ->
    [{refresh_after, V}].

-spec mod_global_distrib_bounce(path(), toml_value()) -> [option()].
mod_global_distrib_bounce([<<"resend_after_ms">>|_], V) ->
    [{resend_after_ms, V}];
mod_global_distrib_bounce([<<"max_retries">>|_], V) ->
    [{max_retries, V}].

-spec mod_global_distrib_connections_endpoints(path(), toml_section()) -> [option()].
mod_global_distrib_connections_endpoints(_, #{<<"host">> := Host, <<"port">> := Port}) ->
    [{b2l(Host), Port}].

-spec mod_keystore_keys(path(), toml_section()) -> [option()].
mod_keystore_keys(_, #{<<"name">> := Name, <<"type">> := <<"ram">>}) ->
    [{b2a(Name), ram}];
mod_keystore_keys(_, #{<<"name">> := Name, <<"type">> := <<"file">>, <<"path">> := Path}) ->
    [{b2a(Name), {file, b2l(Path)}}].

-spec mod_mam_opts(path(), toml_value()) -> [option()].
mod_mam_opts([<<"backend">>|_], V) ->
    [{backend, b2a(V)}];
mod_mam_opts([<<"no_stanzaid_element">>|_], V) ->
    [{no_stanzaid_element, V}];
mod_mam_opts([<<"is_archivable_message">>|_], V) ->
    [{is_archivable_message, b2a(V)}];
mod_mam_opts([<<"message_retraction">>|_], V) ->
    [{message_retraction, V}];
mod_mam_opts([<<"user_prefs_store">>|_], false) ->
    [{user_prefs_store, false}];
mod_mam_opts([<<"user_prefs_store">>|_], V) ->
    [{user_prefs_store, b2a(V)}];
mod_mam_opts([<<"full_text_search">>|_], V) ->
    [{full_text_search, V}];
mod_mam_opts([<<"cache_users">>|_], V) ->
    [{cache_users, V}];
mod_mam_opts([<<"rdbms_message_format">>|_], V) ->
    [{rdbms_message_format, b2a(V)}];
mod_mam_opts([<<"async_writer">>|_], V) ->
    [{async_writer, V}];
mod_mam_opts([<<"flush_interval">>|_], V) ->
    [{flush_interval, V}];
mod_mam_opts([<<"max_batch_size">>|_], V) ->
    [{max_batch_size, V}];
mod_mam_opts([<<"archive_groupchats">>, <<"pm">>|_], V) ->
    [{archive_groupchats, V}];
mod_mam_opts([<<"host">>, <<"muc">>|_], V) ->
    [{host, b2l(V)}].

-spec mod_muc_default_room(path(), toml_value()) -> [option()].
mod_muc_default_room([<<"title">>|_], V) ->
    [{title, V}];
mod_muc_default_room([<<"description">>|_], V) ->
    [{description, V}];
mod_muc_default_room([<<"allow_change_subj">>|_], V) ->
    [{allow_change_subj, V}];
mod_muc_default_room([<<"allow_query_users">>|_], V) ->
    [{allow_query_users, V}];
mod_muc_default_room([<<"allow_private_messages">>|_], V) ->
    [{allow_private_messages, V}];
mod_muc_default_room([<<"allow_visitor_status">>|_], V) ->
    [{allow_visitor_status, V}];
mod_muc_default_room([<<"allow_visitor_nickchange">>|_], V) ->
    [{allow_visitor_nickchange, V}];
mod_muc_default_room([<<"public">>|_], V) ->
    [{public, V}];
mod_muc_default_room([<<"public_list">>|_], V) ->
    [{public_list, V}];
mod_muc_default_room([<<"persistent">>|_], V) ->
    [{persistent, V}];
mod_muc_default_room([<<"moderated">>|_], V) ->
    [{moderated, V}];
mod_muc_default_room([<<"members_by_default">>|_], V) ->
    [{members_by_default, V}];
mod_muc_default_room([<<"members_only">>|_], V) ->
    [{members_only, V}];
mod_muc_default_room([<<"allow_user_invites">>|_], V) ->
    [{allow_user_invites, V}];
mod_muc_default_room([<<"allow_multiple_sessions">>|_], V) ->
    [{allow_multiple_sessions, V}];
mod_muc_default_room([<<"password_protected">>|_], V) ->
    [{password_protected, V}];
mod_muc_default_room([<<"password">>|_], V) ->
    [{password, V}];
mod_muc_default_room([<<"anonymous">>|_], V) ->
    [{anonymous, V}];
mod_muc_default_room([<<"max_users">>|_], V) ->
    [{max_users, V}];
mod_muc_default_room([<<"logging">>|_], V) ->
    [{logging, V}];
mod_muc_default_room([<<"maygetmemberlist">>|_] = Path, V) ->
    List = parse_list(Path, V),
    [{maygetmemberlist, List}];
mod_muc_default_room([<<"affiliations">>|_] = Path, V) ->
    Affs = parse_list(Path, V),
    [{affiliations, Affs}];
mod_muc_default_room([<<"subject">>|_], V) ->
    [{subject, V}];
mod_muc_default_room([<<"subject_author">>|_], V) ->
    [{subject_author, V}].

-spec mod_muc_default_room_affiliations(path(), toml_section()) -> [option()].
mod_muc_default_room_affiliations(_, #{<<"user">> := User, <<"server">> := Server, 
    <<"resource">> := Resource, <<"affiliation">> := Aff}) ->
    [{{User, Server, Resource}, b2a(Aff)}].

-spec mod_muc_log_top_link(path(), toml_value()) -> [option()].
mod_muc_log_top_link([<<"target">>|_], V) ->
    [b2l(V)];
mod_muc_log_top_link([<<"text">>|_], V) ->
    [b2l(V)].

-spec mod_muc_light_config_schema(path(), toml_section()) -> [option()].
mod_muc_light_config_schema(_, #{<<"field">> := Field, <<"value">> := Val,
    <<"internal_key">> := Key, <<"type">> := Type}) ->
        [{b2l(Field), Val, b2a(Key), b2a(Type)}];
    mod_muc_light_config_schema(_, #{<<"field">> := Field, <<"value">> := Val}) ->
        [{b2l(Field), b2l(Val)}].

-spec mod_pubsub_pep_mapping(path(), toml_section()) -> [option()].
mod_pubsub_pep_mapping(_, #{<<"namespace">> := Name, <<"node">> := Node}) ->
    [{b2l(Name), b2l(Node)}].

-spec mod_pubsub_default_node_config(path(), toml_section()) -> [option()].
mod_pubsub_default_node_config(_, #{<<"key">> := Key, <<"value">> := Value}) ->
    [{b2a(Key), b2l(Value)}].

-spec mod_revproxy_routes(path(), toml_section()) -> [option()].
mod_revproxy_routes(_, #{<<"host">> := Host, <<"path">> := Path, <<"method">> := Method,
    <<"upstream">> := Upstream}) ->
        [{b2l(Host), b2l(Path), b2l(Method), b2l(Upstream)}];
mod_revproxy_routes(_, #{<<"host">> := Host, <<"path">> := Path, <<"upstream">> := Upstream}) ->
        [{b2l(Host), b2l(Path), b2l(Upstream)}].

-spec mod_stream_management_stale_h(path(), toml_value()) -> [option()].
mod_stream_management_stale_h([<<"enabled">>|_], V) ->
    [{enabled, V}];
mod_stream_management_stale_h([<<"repeat_after">>|_], V) ->
    [{stale_h_repeat_after, V}];
mod_stream_management_stale_h([<<"geriatric">>|_], V) ->
    [{stale_h_geriatric, V}].

-spec mod_vcard_ldap_uids(path(), toml_section()) -> [option()].
mod_vcard_ldap_uids(_, #{<<"attr">> := Attr, <<"format">> := Format}) ->
    [{b2a(Attr), b2l(Format)}];
mod_vcard_ldap_uids(_, #{<<"attr">> := Attr}) ->
    [b2a(Attr)].

-spec mod_vcard_ldap_vcard_map(path(), toml_section()) -> [option()].
mod_vcard_ldap_vcard_map(_, #{<<"vcard_field">> := VF, <<"ldap_pattern">> := LP, 
    <<"ldap_field">> := LF}) ->
    [{VF, LP, [LF]}].

-spec mod_vcard_ldap_search_fields(path(), toml_section()) -> [option()].
mod_vcard_ldap_search_fields(_, #{<<"search_field">> := SF, <<"ldap_field">> := LF}) ->
    [{SF, LF}].

-spec mod_vcard_ldap_search_reported(path(), toml_section()) -> [option()].
mod_vcard_ldap_search_reported(_, #{<<"search_field">> := SF, <<"vcard_field">> := VF}) ->
    [{SF, VF}].

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
-spec tls_option(path(), toml_value()) -> [option()].
tls_option([<<"verify_peer">>|_], V) -> [{verify, verify_peer(V)}];
tls_option([<<"certfile">>|_], V) -> [{certfile, b2l(V)}];
tls_option([<<"cacertfile">>|_], V) -> [{cacertfile, b2l(V)}];
tls_option([<<"dhfile">>|_], V) -> [{dhfile, b2l(V)}];
tls_option([<<"keyfile">>|_], V) -> [{keyfile, b2l(V)}];
tls_option([<<"password">>|_], V) -> [{password, b2l(V)}];
tls_option([<<"server_name_indication">>|_], false) -> [{server_name_indication, disable}];
tls_option([<<"ciphers">>|_] = Path, L) -> [{ciphers, parse_list(Path, L)}];
tls_option([<<"versions">>|_] = Path, L) -> [{versions, parse_list(Path, L)}].

%% path: listen.http[].tls.*,
%%       listen.c2s[].tls.*,,
%%       modules.mod_global_distrib.connections.tls.*
-spec fast_tls_option(path(), toml_value()) -> [option()].
fast_tls_option([<<"certfile">>|_], V) -> [{certfile, b2l(V)}];
fast_tls_option([<<"cacertfile">>|_], V) -> [{cafile, b2l(V)}];
fast_tls_option([<<"dhfile">>|_], V) -> [{dhfile, b2l(V)}];
fast_tls_option([<<"ciphers">>|_], V) -> [{ciphers, b2l(V)}].

-spec verify_peer(boolean()) -> option().
verify_peer(false) -> verify_none;
verify_peer(true) -> verify_peer.

-spec tls_cipher(path(), toml_value()) -> [option()].
tls_cipher(_, #{<<"key_exchange">> := KEx,
                <<"cipher">> := Cipher,
                <<"mac">> := MAC,
                <<"prf">> := PRF}) ->
    [#{key_exchange => b2a(KEx), cipher => b2a(Cipher), mac => b2a(MAC), prf => b2a(PRF)}];
tls_cipher(_, Cipher) -> [b2l(Cipher)].

set_overrides(Overrides, State) ->
    lists:foldl(fun({override, Scope}, CurrentState) ->
                        mongoose_config_parser:override(Scope, CurrentState)
                end, State, Overrides).

%% TODO replace with binary_to_existing_atom where possible, prevent atom leak
b2a(B) -> binary_to_atom(B, utf8).

b2l(B) -> binary_to_list(B).

-spec limit_keys([toml_key()], toml_section()) -> any().
limit_keys(Keys, Section) ->
    Section = maps:with(Keys, Section).

-spec parse_kv(path(), toml_key(), toml_section(), option()) -> option().
parse_kv(Path, Key, Section, Default) ->
    Value = maps:get(Key, Section, Default),
    handle([Key|Path], Value).

-spec parse_kv(path(), toml_key(), toml_section()) -> option().
parse_kv(Path, Key, Section) ->
    #{Key := Value} = Section,
    handle([Key|Path], Value).

-spec parse_section(path(), toml_section()) -> [option()].
parse_section(Path, M) ->
    lists:flatmap(fun({K, V}) ->
                          Key = key(K, Path, V),
                          handle([Key|Path], V)
                  end, maps:to_list(M)).

-spec parse_list(path(), [toml_value()]) -> [option()].
parse_list(Path, L) ->
    lists:flatmap(fun(Elem) ->
                          Key = item_key(Path, Elem),
                          handle([Key|Path], Elem)
                  end, L).

-spec handle(path(), toml_value()) -> option().
handle(Path, Value) ->
    Handler = handler(Path),
    Option = Handler(Path, Value),
    mongoose_config_validator_toml:validate(Path, Option),
    Option.

-spec handler(path()) -> fun((path(), toml_value()) -> option()).
handler([_]) -> fun process_section/2;

%% general
handler([_, <<"general">>]) -> fun process_general/2;
handler([_, <<"hosts">>, <<"general">>]) -> fun process_host/2;
handler([_, <<"override">>, <<"general">>]) -> fun process_override/2;
handler([_, <<"mongooseimctl_access_commands">>, <<"general">>]) -> fun ctl_access_rule/2;
handler([<<"commands">>, _, <<"mongooseimctl_access_commands">>, <<"general">>]) ->
    fun ctl_access_commands/2;
handler([_, <<"commands">>, _, <<"mongooseimctl_access_commands">>, <<"general">>]) ->
    fun(_, Val) -> [b2l(Val)] end;
handler([<<"argument_restrictions">>, _, <<"mongooseimctl_access_commands">>, <<"general">>]) ->
    fun parse_section/2;
handler([_, <<"argument_restrictions">>, _, <<"mongooseimctl_access_commands">>, <<"general">>]) ->
    fun ctl_access_arg_restriction/2;
handler([_, <<"routing_modules">>, <<"general">>]) ->
    fun(_, Val) -> [b2a(Val)] end;

%% listen
handler([_, <<"listen">>]) -> fun parse_list/2;
handler([_, _, <<"listen">>]) -> fun process_listener/2;
handler([_, _, <<"http">>, <<"listen">>]) -> fun http_listener_opt/2;
handler([_, _, <<"c2s">>, <<"listen">>]) -> fun c2s_listener_opt/2;
handler([_, _, <<"s2s">>, <<"listen">>]) -> fun s2s_listener_opt/2;
handler([_, <<"tls">>, _, <<"s2s">>, <<"listen">>]) -> fun s2s_tls_option/2;
handler([_, _, <<"service">>, <<"listen">>]) -> fun service_listener_opt/2;
handler([_, {tls, _}, _, <<"c2s">>, <<"listen">>]) -> fun c2s_tls_option/2;
handler([_, <<"versions">>, {tls, just_tls}, _, <<"c2s">>, <<"listen">>]) ->
    fun(_, Val) -> [b2a(Val)] end;
handler([_, <<"ciphers">>, {tls, just_tls}, _, <<"c2s">>, <<"listen">>]) -> fun tls_cipher/2;
handler([_, <<"tls">>, _, <<"http">>, <<"listen">>]) -> fun https_option/2;
handler([_, <<"transport">>, _, <<"http">>, <<"listen">>]) -> fun cowboy_transport_opt/2;
handler([_, <<"protocol">>, _, <<"http">>, <<"listen">>]) -> fun cowboy_protocol_opt/2;
handler([_, <<"handlers">>, _, <<"http">>, <<"listen">>]) -> fun parse_list/2;
handler([_, _, <<"handlers">>, _, <<"http">>, <<"listen">>]) -> fun cowboy_module/2;
handler([_, _, <<"mongoose_api">>, <<"handlers">>, _, <<"http">>, <<"listen">>]) ->
    fun mongoose_api_option/2;
handler([_, <<"handlers">>, _, <<"mongoose_api">>, <<"handlers">>, _, <<"http">>, <<"listen">>]) ->
    fun(_, Val) -> [b2a(Val)] end;
handler([_, _, <<"mod_websockets">>, <<"handlers">>, _, <<"http">>, <<"listen">>]) ->
    fun websockets_option/2;
handler([_, <<"service">>, _, <<"mod_websockets">>, <<"handlers">>, _, <<"http">>, <<"listen">>]) ->
    fun service_listener_opt/2;

%% auth
handler([_, <<"auth">>]) -> fun auth_option/2;
handler([_, <<"ldap">>, <<"auth">>]) -> fun auth_ldap_option/2;
handler([_, <<"uids">>, <<"ldap">>, <<"auth">>]) -> fun auth_ldap_uids/2;
handler([_, <<"dn_filter">>, <<"ldap">>, <<"auth">>]) -> fun auth_ldap_dn_filter/2;
handler([_, <<"local_filter">>, <<"ldap">>, <<"auth">>]) -> fun auth_ldap_local_filter/2;
handler([_, <<"attributes">>, _, <<"ldap">>, <<"auth">>]) -> fun(_, V) -> [b2l(V)] end;

%% outgoing_pools
handler([_, <<"outgoing_pools">>]) -> fun parse_section/2;
handler([_, _, <<"outgoing_pools">>]) -> fun process_pool/2;
handler([_, _, _, <<"outgoing_pools">>]) -> fun pool_option/2;
handler([_, <<"connection">>, _, <<"http">>, <<"outgoing_pools">>]) -> fun http_option/2;
handler([_, <<"http_opts">>, <<"connection">>, _, <<"http">>, <<"outgoing_pools">>]) -> fun http_opts/2;
handler([_, <<"connection">>, _, <<"redis">>, <<"outgoing_pools">>]) -> fun redis_option/2;
handler([_, <<"connection">>, _, <<"ldap">>, <<"outgoing_pools">>]) -> fun ldap_option/2;
handler([_, <<"servers">>, <<"connection">>, _, <<"ldap">>, <<"outgoing_pools">>]) -> fun(_, V) -> [b2l(V)] end;
handler([_, <<"connection">>, _, <<"riak">>, <<"outgoing_pools">>]) -> fun riak_option/2;
handler([_, <<"credentials">>, <<"connection">>, _, <<"riak">>, <<"outgoing_pools">>]) -> fun riak_credentials/2;
handler([_, <<"connection">>, _, <<"cassandra">>, <<"outgoing_pools">>]) -> fun cassandra_option/2;
handler([_, <<"servers">>, <<"connection">>, _, <<"cassandra">>, <<"outgoing_pools">>]) -> fun cassandra_server/2;
handler([_, <<"connection">>, _, <<"elastic">>, <<"outgoing_pools">>]) -> fun elastic_option/2;
handler([_, <<"connection">>, _, <<"rabbit">>, <<"outgoing_pools">>]) -> fun rabbit_option/2;
handler([<<"keepalive_interval">>, <<"connection">>, _, <<"rdbms">>, <<"outgoing_pools">>]) -> 
    fun(_, V) -> V end;
handler([_, <<"tls">>, <<"connection">>, _, _, <<"outgoing_pools">>]) -> fun tls_option/2;
handler([_, <<"versions">>, <<"tls">>, <<"connection">>, _, _, <<"outgoing_pools">>]) ->
    fun(_, Val) -> [b2a(Val)] end;
handler([_, <<"ciphers">>, <<"tls">>, <<"connection">>, _, _, <<"outgoing_pools">>]) ->
    fun tls_cipher/2;

%% services
handler([_, <<"services">>]) -> fun process_service/2;
handler([_, _, <<"services">>]) -> fun service_opt/2;

%% modules
handler([_, <<"modules">>]) -> fun process_module/2;
handler([_, _, <<"modules">>]) -> fun module_opt/2;
handler([_, <<"riak">>, _, <<"modules">>]) ->
    fun riak_opts/2;
handler([_, <<"ip_access">>, <<"mod_register">>, <<"modules">>]) ->
    fun mod_register_ip_access_rule/2;
handler([_, <<"registration_watchers">>, <<"mod_register">>, <<"modules">>]) ->
    fun(_, V) -> [V] end;
handler([_, <<"validity_period">>, <<"mod_auth_token">>, <<"modules">>]) ->
    fun mod_auth_token_validity_periods/2;
handler([_, <<"extra_domains">>, <<"mod_disco">>, <<"modules">>]) ->
    fun(_, V) -> [b2l(V)] end;
handler([_, <<"server_info">>, <<"mod_disco">>, <<"modules">>]) ->
    fun mod_disco_server_info/2;
handler([_, <<"urls">>, _, <<"server_info">>, <<"mod_disco">>, <<"modules">>]) ->
    fun(_, V) -> [b2l(V)] end;
handler([_, <<"module">>, _, <<"server_info">>, <<"mod_disco">>, <<"modules">>]) ->
    fun(_, V) -> [b2a(V)] end;
handler([<<"sns">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_backend_sns/2;
handler([<<"push">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_backend_push/2;
handler([<<"http">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_backend_http/2;
handler([<<"rabbit">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_backend_rabbit/2;
handler([_, <<"sns">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_backend_sns_opts/2;
handler([_, <<"push">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_backend_push_opts/2;
handler([_, <<"http">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_backend_http_opts/2;
handler([_, <<"rabbit">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_backend_rabbit_opts/2;
handler([_,<<"wpool">>, <<"push">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun pool_option/2;
handler([_,<<"virtual_pubsub_hosts">>, <<"push">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun (_, V) -> [b2l(V)] end;
handler([_,<<"presence_exchange">>, <<"rabbit">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_rabbit_presence_ex/2;
handler([_,<<"chat_msg_exchange">>, <<"rabbit">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_rabbit_msg_ex/2;
handler([_,<<"groupchat_msg_exchange">>, <<"rabbit">>, <<"backend">>, <<"mod_event_pusher">>, <<"modules">>]) ->
    fun mod_event_pusher_rabbit_msg_ex/2;
handler([_, <<"s3">>, <<"mod_http_upload">>, <<"modules">>]) ->
    fun mod_http_upload_s3/2;
handler([_, <<"reset_markers">>, <<"mod_inbox">>, <<"modules">>]) ->
    fun(_, V) -> [b2a(V)] end;
handler([_, <<"groupchat">>, <<"mod_inbox">>, <<"modules">>]) ->
    fun(_, V) -> [b2a(V)] end;
handler([_, <<"connections">>, <<"mod_global_distrib">>, <<"modules">>]) ->
    fun mod_global_distrib_connections/2;
handler([_, <<"cache">>, <<"mod_global_distrib">>, <<"modules">>]) ->
    fun mod_global_distrib_cache/2;
handler([_, <<"bounce">>, <<"mod_global_distrib">>, <<"modules">>]) ->
    fun mod_global_distrib_bounce/2;
handler([_, <<"redis">>, <<"mod_global_distrib">>, <<"modules">>]) ->
    fun mod_global_distrib_redis/2;
handler([_,<<"endpoints">>, <<"connections">>, <<"mod_global_distrib">>, <<"modules">>]) ->
    fun mod_global_distrib_connections_endpoints/2;
handler([_,<<"tls">>, <<"connections">>, <<"mod_global_distrib">>, <<"modules">>]) ->
    fun fast_tls_option/2;
handler([_, <<"keys">>, <<"mod_keystore">>, <<"modules">>]) ->
    fun mod_keystore_keys/2;
handler([_, _, <<"mod_mam_meta">>, <<"modules">>]) ->
    fun mod_mam_opts/2;
handler([_, <<"default_room">>, <<"mod_muc">>, <<"modules">>]) ->
    fun mod_muc_default_room/2;
handler([_, <<"maygetmemberlist">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>]) ->
    fun (_, V) -> [b2a(V)] end;
handler([_, <<"affiliations">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>]) ->
    fun mod_muc_default_room_affiliations/2;
handler([_, <<"top_link">>, <<"mod_muc_log">>, <<"modules">>]) ->
    fun mod_muc_log_top_link/2;
handler([_, <<"config_schema">>, <<"mod_muc_light">>, <<"modules">>]) ->
    fun mod_muc_light_config_schema/2;
handler([_, <<"plugins">>, <<"mod_pubsub">>, <<"modules">>]) ->
    fun(_, V) -> [V] end;
handler([_, <<"pep_mapping">>, <<"mod_pubsub">>, <<"modules">>]) ->
    fun mod_pubsub_pep_mapping/2;
handler([_, <<"default_node_config">>, <<"mod_pubsub">>, <<"modules">>]) ->
    fun mod_pubsub_default_node_config/2;
handler([_, <<"routes">>, <<"mod_revproxy">>, <<"modules">>]) ->
    fun mod_revproxy_routes/2;
handler([_, <<"stale_h">>, <<"mod_stream_management">>, <<"modules">>]) ->
    fun mod_stream_management_stale_h/2;
handler([_, <<"ldap_uids">>, <<"mod_vcard">>, <<"modules">>]) ->
    fun mod_vcard_ldap_uids/2;
handler([_, <<"ldap_vcard_map">>, <<"mod_vcard">>, <<"modules">>]) ->
    fun mod_vcard_ldap_vcard_map/2;
handler([_, <<"ldap_search_fields">>, <<"mod_vcard">>, <<"modules">>]) ->
    fun mod_vcard_ldap_search_fields/2;
handler([_, <<"ldap_search_reported">>, <<"mod_vcard">>, <<"modules">>]) ->
    fun mod_vcard_ldap_search_reported/2;

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

-spec key(toml_key(), path(), toml_value()) -> tuple() | toml_key().
key(<<"tls">>, [item, <<"c2s">>, <<"listen">>], M) ->
    %% store the tls module in path as both of them need different options
    case maps:get(<<"module">>, M, <<"fast_tls">>) of
        <<"just_tls">> -> {tls, just_tls};
        <<"fast_tls">> -> {tls, fast_tls}
    end;
key(Key, _Path, _) -> Key.

-spec item_key(path(), toml_value()) -> tuple() | item.
item_key([<<"host_config">>], #{<<"host">> := Host}) -> {host, Host};
item_key(_, _) -> item.
