-module(mongoose_config_parser_toml).

-export([parse/1]).

-include("mongoose.hrl").
-include("ejabberd_config.hrl").

-define(HOST_F(Expr), fun(Host) -> Expr end).

parse(Content) ->
    lists:append([process_section(Section, SecContent) ||
                     {Section, SecContent} <- maps:to_list(Content)]).

process_section(<<"acl">>, Content) ->
    [process_acl(Name, global, C) || {Name, C} <- maps:to_list(Content)];
process_section(<<"access">>, Content) ->
    [process_access_rule(Name, global, C) || {Name, C} <- maps:to_list(Content)];
process_section(<<"shaper">>, Content) ->
    [process_shaper(Name, global, C) || {Name, C} <- maps:to_list(Content)];
process_section(<<"outgoing_pools">>, Content) ->
    Pools = lists:append([process_pool_type(Type, C) || {Type, C} <- maps:to_list(Content)]),
    [#local_config{key = outgoing_pools, value = Pools}];
process_section(<<"host_config">>, Content) ->
    lists:append([process_host_item(C) || C <- Content]);
process_section(<<"listen">>, Content) ->
    lists:append([process_listener_type(Type, C) || {Type, C} <- maps:to_list(Content)]);
process_section(<<"general">>, Content) ->
    [process_general(Name, C) || {Name, C} <- maps:to_list(Content)];
process_section(<<"s2s">>, Content) ->
    [process_s2s_option(Name, C) || {Name, C} <- maps:to_list(Content)];
process_section(<<"auth">>, Content = #{<<"methods">> := Methods}) ->
    AuthOpts = auth_opts(maps:without([<<"methods">>], Content)),
    [?HOST_F(#local_config{key = {auth_method, Host},
                           value = [b2a(Method) || Method <- Methods]}),
     ?HOST_F(#local_config{key = {auth_opts, Host},
                           value = [Opt || {inner, Opt} <- AuthOpts]}) |
     [?HOST_F(#local_config{key = {K, Host}, value = V}) || {outer, {K, V}} <- AuthOpts]];
process_section(<<"modules">>, Content) ->
    Mods = [process_module(Mod, Opts) || {Mod, Opts} <- maps:to_list(Content)],
    [?HOST_F(#local_config{key = {modules, Host}, value = Mods})];
process_section(<<"services">>, Content) ->
    Services = [process_service(S, Opts) || {S, Opts} <- maps:to_list(Content)],
    [#local_config{key = services, value = Services}].

process_module(Mod, Opts) ->
    {b2a(Mod), [module_opt(Mod, K, V) || {K, V} <- maps:to_list(Opts)]}.

module_opt(<<"mod_disco">>, <<"users_can_see_hidden_services">>, V) ->
    {users_can_see_hidden_services, V};
module_opt(<<"mod_register">>, <<"welcome_message">>, V) ->
    {welcome_message, binary_to_list(V)};
module_opt(<<"mod_register">>, <<"ip_access">>, V) ->
    Rules = [{Addr, b2a(P)} || #{<<"address">> := Addr, <<"policy">> := P} <- V],
    {ip_access, Rules};
module_opt(<<"mod_register">>, <<"access">>, V) ->
    {access, b2a(V)};
module_opt(<<"mod_vcard">>, <<"host">>, V) ->
    {host, binary_to_list(V)};
module_opt(_, <<"backend">>, V) ->
    {backend, b2a(V)}.

process_service(S, Opts) ->
    {b2a(S), [service_opt(S, K, V) || {K, V} <- maps:to_list(Opts)]}.

service_opt(<<"service_admin_extra">>, <<"submods">>, V) ->
    {submods, [binary_to_atom(M, utf8) || M <- V]};
service_opt(<<"service_mongoose_system_metrics">>, <<"initial_report">>, V) ->
    {initial_report, V};
service_opt(<<"service_mongoose_system_metrics">>, <<"periodic_report">>, V) ->
    {periodic_report, V}.

auth_opts(Content) ->
    [{auth_option_placement(K), auth_option(K, V)} || {K, V} <- maps:to_list(Content)].

auth_option(<<"password">>, #{<<"format">> := <<"scram">>, <<"hash">> := Hashes}) ->
    {password_format, {scram, [b2a(H) || H <- Hashes]}};
auth_option(<<"password">>, #{<<"format">> := Format}) ->
    {password_format, b2a(Format)};
auth_option(<<"scram_iterations">>, V) ->
    {scram_iterations, V};
auth_option(<<"cyrsasl_external">>, V) ->
    {cyrsasl_external, [b2a(M) || M <- V]};
auth_option(<<"allow_multiple_connections">>, V) ->
    {allow_multiple_connections, V};
auth_option(<<"anonymous_protocol">>, V) ->
    {anonymous_protocol, b2a(V)}.

auth_option_placement(<<"allow_multiple_connections">>) -> outer;
auth_option_placement(<<"anonymous_protocol">>) -> outer;
auth_option_placement(_) -> inner.

process_general(<<"loglevel">>, V) ->
    #local_config{key = loglevel, value = V};
process_general(<<"hosts">>, Hosts) ->
    #config{key = hosts, value = [jid:nodeprep(H) || H <- Hosts]};
process_general(<<"registration_timeout">>, V) ->
    #local_config{key = registration_timeout, value = V};
process_general(<<"language">>, V) ->
    #local_config{key = language, value = V};
process_general(<<"all_metrics_are_global">>, V) ->
    #local_config{key = language, value = V};
process_general(<<"sm_backend">>, V) ->
    #config{key = sm_backend, value = b2a(V)};
process_general(<<"max_fsm_queue">>, V) ->
    #local_config{key = max_fsm_queue, value = V}.

process_s2s_option(<<"use_starttls">>, V) ->
    [#local_config{key = s2s_use_starttls, value = b2a(V)}];
process_s2s_option(<<"certfile">>, V) ->
    [#local_config{key = s2s_cerfile, value = binary_to_list(V)}];
process_s2s_option(<<"default_policy">>, V) ->
    [?HOST_F(#local_config{key = {s2s_default_policy, Host}, value = b2a(V)})];
process_s2s_option(<<"outgoing_port">>, V) ->
    [#local_config{key = outgoing_s2s_port, value = V}];
process_s2s_option(<<"address">>, Addrs) ->
    [#local_config{key = {s2s_addr, Host}, value = s2s_address(Addr)}
     || Addr = #{<<"host">> := Host} <- Addrs].

s2s_address(#{<<"ip_address">> := IP, <<"port">> := Port}) ->
    {binary_to_list(IP), Port};
s2s_address(#{<<"ip_address">> := IP}) ->
    binary_to_list(IP).

process_listener_type(Type, Content) ->
    [process_listener(Type, C) || C <- Content].

process_listener(Type, Content) ->
    Options = maps:without([<<"port">>, <<"ip_address">>], Content),
    PortIP = listener_portip(Content),
    Opts = lists:flatmap(fun({K, V}) -> listener_opt(Type, K, V) end, maps:to_list(Options)),
    {Port, IPT, _, _, Proto, OptsClean} =
        ejabberd_listener:parse_listener_portip(PortIP, Opts),
    {{Port, IPT, Proto}, listener_module(Type), OptsClean}.

listener_portip(#{<<"port">> := Port}) -> Port;
listener_portip(#{<<"port">> := Port, <<"ip_address">> := Addr}) -> {Port, Addr}.

listener_opt(_, <<"proto">>, Proto) -> [{proto, b2a(Proto)}];
listener_opt(_, <<"ip_version">>, 6) -> [inet6];
listener_opt(_, <<"ip_version">>, 4) -> [inet];
listener_opt(_, <<"backlog">>, N) -> [{backlog, N}];
listener_opt(<<"cowboy">>, <<"tls">>, Opts) -> [{ssl, client_tls_options(Opts)}];
listener_opt(<<"cowboy">>, <<"transport">>, Opts) ->
    [{transport_options,
      [{b2a(K), cowboy_transport_opt(K, V)} || {K, V} <- maps:to_list(Opts)]
     }];
listener_opt(<<"cowboy">>, <<"protocol">>, Opts) ->
    [{protocol_options,
      [{b2a(K), cowboy_protocol_opt(K, V)} || {K, V} <- maps:to_list(Opts)]
     }];
listener_opt(<<"cowboy">>, <<"modules">>, Modules) ->
    [{modules,
      [cowboy_module(Type, M) || {Type, Mods} <- maps:to_list(Modules), M <- Mods]
     }];
listener_opt(<<"c2s">>, <<"access">>, V) -> [{access, b2a(V)}];
listener_opt(<<"c2s">>, <<"shaper">>, V) -> [{shaper, b2a(V)}];
listener_opt(<<"c2s">>, <<"xml_socket">>, V) -> [{xml_socket, V}];
listener_opt(<<"c2s">>, <<"zlib">>, V) -> [{zlib, V}];
listener_opt(<<"c2s">>, <<"verify_peer">>, V) -> [{verify_peer, V}];
listener_opt(<<"c2s">>, <<"hibernate_after">>, V) -> [{hibernate_after, V}];
listener_opt(<<"c2s">>, <<"starttls">>, true) -> [starttls];
listener_opt(<<"c2s">>, <<"starttls">>, false) -> [];
listener_opt(<<"c2s">>, <<"starttls_required">>, true) -> [starttls_required];
listener_opt(<<"c2s">>, <<"starttls_required">>, false) -> [];
listener_opt(<<"c2s">>, <<"tls">>, true) -> [tls];
listener_opt(<<"c2s">>, <<"tls">>, false) -> [];
listener_opt(<<"c2s">>, <<"certfile">>, V) -> [{certfile, binary_to_list(V)}];
listener_opt(<<"c2s">>, <<"dhfile">>, V) -> [{dhfile, binary_to_list(V)}];
listener_opt(<<"c2s">>, <<"cafile">>, V) -> [{cafile, binary_to_list(V)}];
listener_opt(<<"c2s">>, <<"ciphers">>, V) -> [{ciphers, binary_to_list(V)}];
listener_opt(<<"c2s">>, <<"max_stanza_size">>, V) -> [{max_stanza_size, V}];
listener_opt(<<"s2s">>, <<"access">>, V) -> [{access, b2a(V)}];
listener_opt(<<"s2s">>, <<"shaper">>, V) -> [{shaper, b2a(V)}];
listener_opt(<<"s2s">>, <<"zlib">>, V) -> [{zlib, V}];
listener_opt(<<"s2s">>, <<"dhfile">>, V) -> [{dhfile, binary_to_list(V)}];
listener_opt(<<"s2s">>, <<"cafile">>, V) -> [{cafile, binary_to_list(V)}];
listener_opt(<<"s2s">>, <<"ciphers">>, V) -> [{ciphers, binary_to_list(V)}];
listener_opt(<<"s2s">>, <<"max_stanza_size">>, V) -> [{max_stanza_size, V}];
listener_opt(<<"service">>, <<"access">>, V) -> [{access, b2a(V)}];
listener_opt(<<"service">>, <<"shaper_rule">>, V) -> [{shaper, b2a(V)}];
listener_opt(<<"service">>, <<"check_from">>, V) -> [{service_check_from, V}];
listener_opt(<<"service">>, <<"hidden_components">>, V) -> [{hidden_components, V}];
listener_opt(<<"service">>, <<"conflict_behaviour">>, V) -> [{conflict_behaviour, b2a(V)}];
listener_opt(<<"service">>, <<"password">>, V) -> [{password, binary_to_list(V)}].

cowboy_module(Type, #{<<"host">> := Host, <<"path">> := Path} = Options) ->
    Opts = maps:without([<<"host">>, <<"path">>], Options),
    ModuleOpts = cowboy_module_options(Type, Opts),
    {binary_to_list(Host), binary_to_list(Path), Type, ModuleOpts}.

cowboy_module_options(<<"mod_websockets">>, #{<<"ejabberd_service">> := Opts}) ->
    [{ejaberd_service, [listener_opt(<<"service">>, K, V) || {K, V} <- maps:to_list(Opts)]}];
cowboy_module_options(<<"lasse_handler">>, #{<<"modules">> := Modules}) ->
    [b2a(Mod) || Mod <- Modules];
cowboy_module_options(<<"cowboy_static">>, #{<<"type">> := Type,
                                             <<"app">> := App,
                                             <<"content_path">> := Path}) ->
    {b2a(Type), b2a(App), Path, [{mimetypes, cow_mimetypes, all}]};
cowboy_module_options(<<"cowboy_swagger_redirect_handler">>, _) -> #{};
cowboy_module_options(<<"cowboy_swagger_json_handler">>, _) -> #{};
cowboy_module_options(<<"mongoose_api">>, #{<<"handlers">> := Handlers}) ->
    [{handlers, [b2a(H) || H <- Handlers]}];
cowboy_module_options(_, _) -> [].

cowboy_transport_opt(<<"num_acceptors">>, N) -> N;
cowboy_transport_opt(<<"max_connections">>, N) -> N.

cowboy_protocol_opt(<<"compress">>, V) -> V.

listener_module(<<"cowboy">>) -> ejabberd_cowboy;
listener_module(<<"c2s">>) -> ejabberd_c2s;
listener_module(<<"s2s">>) -> ejabberd_s2s_in;
listener_module(<<"service">>) -> ejabberd_service.

process_host_item(M) ->
    {Host, Sections} = maps:take(<<"host">>, M),
    lists:append([process_host_section(K, Host, V) || {K, V} <- maps:to_list(Sections)]).

process_host_section(<<"acl">>, Host, Content) ->
    [process_acl(Name, Host, C) || {Name, C} <- maps:to_list(Content)];
process_host_section(<<"access">>, Host, Content) ->
    [process_access_rule(Name, Host, C) || {Name, C} <- maps:to_list(Content)];
process_host_section(<<"shaper">>, Host, Content) ->
    [process_shaper(Name, Host, C) || {Name, C} <- maps:to_list(Content)];
process_host_section(<<"modules">>, Host, Content) ->
    Mods = [process_module(Mod, Opts) || {Mod, Opts} <- maps:to_list(Content)],
    [#local_config{key = {modules, Host}, value = Mods}];
process_host_section(<<"auth">>, Host, Content = #{<<"methods">> := Methods}) ->
    AuthOpts = auth_opts(maps:without([<<"methods">>], Content)),
    [#local_config{key = {auth_method, Host},
                   value = [b2a(Method) || Method <- Methods]},
     #local_config{key = {auth_opts, Host},
                   value = [Opt || {inner, Opt} <- AuthOpts]} |
     [#local_config{key = {K, Host}, value = V} || {outer, {K, V}} <- AuthOpts]].

process_acl(ACLName, Host, Content) ->
    acl:to_record(Host, ACLName, acl_data(Content)).

acl_data(Content) when is_map(Content) ->
    case maps:to_list(Content) of
        [{Key, Values}] when is_list(Values) ->
            list_to_tuple([b2a(Key) | Values]);
        [{Key, Value}] when is_binary(Value) ->
            {b2a(Key), Value}
    end;
acl_data(Value) when is_binary(Value) -> b2a(Value).

process_access_rule(Name, Host, Contents) ->
    Rules = [{Value, b2a(ACL)} || #{<<"acl">> := ACL, <<"value">> := Value} <- Contents],
    #config{key = {access, b2a(Name), Host}, value = Rules}.

process_shaper(Name, Host, #{<<"max_rate">> := MaxRate}) ->
    #config{key = {shaper, b2a(Name), Host}, value = {maxrate, MaxRate}}.

process_pool_type(Type, Content) ->
    [process_pool(b2a(Type), Name, C) || {Name, C} <- maps:to_list(Content)].

process_pool(Type, Tag, M) ->
    ConnectionOptions = maps:get(<<"connection">>, M, #{}),
    Scope = pool_scope(M),
    Options = maps:without([<<"scope">>, <<"host">>, <<"connection">>], M),
    {Type, Scope, Tag, pool_options(Options), connection_options(Type, ConnectionOptions)}.

connection_options(rdbms, Options) ->
    [{server, rdbms_server(Options)}];
connection_options(redis, Options) ->
    [{b2a(K), redis_option(K, V)} || {K, V} <- maps:to_list(Options)].

rdbms_server(#{<<"driver">> := <<"odbc">>,
               <<"settings">> := Settings}) ->
    binary_to_list(Settings);
rdbms_server(#{<<"driver">> := Driver,
               <<"host">> := Host,
               <<"database">> := Database,
               <<"username">> := UserName,
               <<"password">> := Password} = M) ->
    case {maps:get(<<"port">>, M, no_port), db_tls(M)} of
        {no_port, no_tls} -> {b2a(Driver), Host, Database, UserName, Password};
        {Port, no_tls} -> {b2a(Driver), Host, Port, Database, UserName, Password};
        {no_port, TLS} -> {b2a(Driver), Host, Database, UserName, Password, TLS};
        {Port, TLS} -> {b2a(Driver), Host, Port, Database, UserName, Password, TLS}
    end.

redis_option(<<"host">>, Host) -> binary_to_list(Host);
redis_option(<<"port">>, Port) -> Port;
redis_option(<<"database">>, Database) -> binary_to_list(Database);
redis_option(<<"password">>, Password) -> binary_to_list(Password).

db_tls(#{<<"driver">> := Driver, <<"tls">> := TLS}) -> db_tls_options(Driver, TLS);
db_tls(_) -> no_tls.

db_tls_options(<<"mysql">>, Opts) ->
    client_tls_options(Opts);
db_tls_options(<<"pgsql">>, Opts) ->
    {SSLMode, Opts1} = case maps:take(<<"required">>, Opts) of
                           {true, M} -> {required, M};
                           {false, M} -> {true, M};
                           error -> {true, Opts}
                       end,
    [{ssl, SSLMode}, {ssl_opts, client_tls_options(Opts1)}].

pool_options(Opts) ->
    [{b2a(K), pool_option(K, V)} || {K, V} <- maps:to_list(Opts)].

pool_option(<<"workers">>, V) -> V;
pool_option(<<"strategy">>, V) -> b2a(V);
pool_option(<<"call_timeout">>, V) -> V.

client_tls_options(Opts) ->
    [client_tls_option(K, V) || {K, V} <- maps:to_list(Opts)].

client_tls_option(<<"verify_peer">>, V) -> {verify, verify_peer(V)};
client_tls_option(<<"certfile">>, V) -> {certfile, binary_to_list(V)};
client_tls_option(<<"cacertfile">>, V) -> {cacertfile, binary_to_list(V)};
client_tls_option(<<"keyfile">>, V) -> {keyfile, binary_to_list(V)};
client_tls_option(<<"password">>, V) -> {password, binary_to_list(V)};
client_tls_option(<<"server_name_indication">>, false) -> {server_name_indication, disable}.

verify_peer(false) -> verify_none;
verify_peer(true) -> verify_peer.

pool_scope(#{<<"scope">> := <<"single_host">>, <<"host">> := Host}) -> Host;
pool_scope(#{<<"scope">> := Scope}) -> b2a(Scope).

b2a(B) -> binary_to_existing_atom(B, utf8).
