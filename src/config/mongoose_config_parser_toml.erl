-module(mongoose_config_parser_toml).

-export([read_file/1, parse/1]).

-include("mongoose.hrl").
-include("ejabberd_config.hrl").

-define(HOST_F(Expr), fun(Host) -> Expr end).

read_file(File) ->
    {ok, Content} = tomerl:read_file(File),
    Config = parse(Content),
    [Hosts] = lists:filtermap(fun(#config{key = hosts, value = Hosts}) ->
                                      {true, Hosts};
                                 (_) -> false
                              end, Config),
    {FOpts, Opts} = lists:partition(fun(Opt) -> is_function(Opt, 1) end, Config),
    HOpts = lists:flatmap(fun(F) -> [F(Host) || Host <- Hosts] end, FOpts),
    State = mongoose_config_parser:opts_to_state(Opts ++ HOpts, Hosts),
    State1 = mongoose_config_parser:dedup_state_opts(State),
    mongoose_config_parser:add_dep_modules(State1).

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
    Listeners = lists:append([process_listener_type(Type, C)
                              || {Type, C} <- maps:to_list(Content)]),
    [#local_config{key = listen, value = Listeners}];
process_section(<<"general">>, Content) ->
    [process_general(Name, C) || {Name, C} <- maps:to_list(Content)];
process_section(<<"s2s">>, Content) ->
    lists:append([process_s2s_option(Name, C) || {Name, C} <- maps:to_list(Content)]);
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
module_opt(<<"mod_offline">>, <<"access_max_user_messages">>, V) ->
    {access_max_user_messages, b2a(V)};
module_opt(<<"mod_register">>, <<"welcome_message">>, V) ->
    {welcome_message, {b2l(V)}};
module_opt(<<"mod_register">>, <<"ip_access">>, V) ->
    Rules = [{b2a(P), b2l(Addr)} || #{<<"address">> := Addr, <<"policy">> := P} <- V],
    {ip_access, Rules};
module_opt(<<"mod_register">>, <<"access">>, V) ->
    {access, b2a(V)};
module_opt(<<"mod_vcard">>, <<"host">>, V) ->
    {host, b2l(V)};
module_opt(<<"mod_vcard">>, <<"ldap_base">>, V) ->
    {ldap_base, b2l(V)};
module_opt(<<"mod_vcard">>, <<"ldap_filter">>, V) ->
    {ldap_filter, b2l(V)};
module_opt(_, <<"backend">>, V) ->
    {backend, b2a(V)}.

process_service(S, Opts) ->
    {b2a(S), [service_opt(S, K, V) || {K, V} <- maps:to_list(Opts)]}.

service_opt(<<"service_admin_extra">>, <<"submods">>, V) ->
    {submods, [b2a(M) || M <- V]};
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
    {cyrsasl_external, [cyrsasl_external(M) || M <- V]};
auth_option(<<"allow_multiple_connections">>, V) ->
    {allow_multiple_connections, V};
auth_option(<<"anonymous_protocol">>, V) ->
    {anonymous_protocol, b2a(V)};
auth_option(<<"sasl_mechanisms">>, V) ->
    {sasl_mechanisms, [b2a(M) || M <- V]};
auth_option(<<"ldap_base">>, V) ->
    {ldap_base, b2l(V)};
auth_option(<<"ldap_filter">>, V) ->
    {ldap_filter, b2l(V)}.

cyrsasl_external(<<"standard">>) -> standard;
cyrsasl_external(<<"common_name">>) -> common_name;
cyrsasl_external(<<"auth_id">>) -> auth_id;
cyrsasl_external(M) -> {mod, b2a(M)}.

auth_option_placement(<<"allow_multiple_connections">>) -> outer;
auth_option_placement(<<"anonymous_protocol">>) -> outer;
auth_option_placement(<<"sasl_mechanisms">>) -> outer;
auth_option_placement(_) -> inner.

process_general(<<"loglevel">>, V) ->
    #local_config{key = loglevel, value = V};
process_general(<<"hosts">>, Hosts) ->
    #config{key = hosts, value = [jid:nodeprep(H) || H <- Hosts]};
process_general(<<"registration_timeout">>, <<"infinity">>) ->
    #local_config{key = registration_timeout, value = infinity};
process_general(<<"registration_timeout">>, V) ->
    #local_config{key = registration_timeout, value = V};
process_general(<<"language">>, V) ->
    #config{key = language, value = V};
process_general(<<"all_metrics_are_global">>, V) ->
    #local_config{key = all_metrics_are_global, value = V};
process_general(<<"sm_backend">>, V) ->
    #config{key = sm_backend, value = {b2a(V), []}};
process_general(<<"max_fsm_queue">>, V) ->
    #local_config{key = max_fsm_queue, value = V};
process_general(<<"http_server_name">>, V) ->
    #local_config{key = cowboy_server_name, value = b2l(V)};
process_general(<<"rdbms_server_type">>, V) ->
    #local_config{key = rdbms_server_type, value = b2a(V)}.

process_s2s_option(<<"use_starttls">>, V) ->
    [#local_config{key = s2s_use_starttls, value = b2a(V)}];
process_s2s_option(<<"certfile">>, V) ->
    [#local_config{key = s2s_certfile, value = b2l(V)}];
process_s2s_option(<<"default_policy">>, V) ->
    [?HOST_F(#local_config{key = {s2s_default_policy, Host}, value = b2a(V)})];
process_s2s_option(<<"outgoing_port">>, V) ->
    [#local_config{key = outgoing_s2s_port, value = V}];
process_s2s_option(<<"address">>, Addrs) ->
    [#local_config{key = {s2s_addr, Host}, value = s2s_address(Addr)}
     || Addr = #{<<"host">> := Host} <- Addrs].

s2s_address(#{<<"ip_address">> := IP, <<"port">> := Port}) ->
    {b2l(IP), Port};
s2s_address(#{<<"ip_address">> := IP}) ->
    b2l(IP).

process_listener_type(Type, Content) ->
    [process_listener(Type, C) || C <- Content].

process_listener(Type, Content) ->
    Options = maps:without([<<"port">>, <<"ip_address">>], Content),
    PortIP = listener_portip(Content),
    Opts = lists:flatmap(fun({K, V}) -> listener_opt(Type, K, V) end, maps:to_list(Options)),
    {Port, IPT, _, _, Proto, OptsClean} =
        ejabberd_listener:parse_listener_portip(PortIP, Opts),
    {{Port, IPT, Proto}, listener_module(Type), OptsClean}.

listener_portip(#{<<"port">> := Port, <<"ip_address">> := Addr}) -> {Port, b2l(Addr)};
listener_portip(#{<<"port">> := Port}) -> Port.

listener_opt(_, <<"proto">>, Proto) -> [{proto, b2a(Proto)}];
listener_opt(_, <<"ip_version">>, 6) -> [inet6];
listener_opt(_, <<"ip_version">>, 4) -> [inet];
listener_opt(_, <<"backlog">>, N) -> [{backlog, N}];
listener_opt(_, <<"proxy_protocol">>, V) -> [{proxy_protocol, V}];
listener_opt(<<"http">>, <<"tls">>, Opts) -> [{ssl, https_options(Opts)}];
listener_opt(<<"http">>, <<"transport">>, Opts) ->
    [{transport_options,
      [{b2a(K), cowboy_transport_opt(K, V)} || {K, V} <- maps:to_list(Opts)]
     }];
listener_opt(<<"http">>, <<"protocol">>, Opts) ->
    [{protocol_options,
      [{b2a(K), cowboy_protocol_opt(K, V)} || {K, V} <- maps:to_list(Opts)]
     }];
listener_opt(<<"http">>, <<"handlers">>, Handlers) ->
    [{modules,
      [cowboy_module(Type, M) || {Type, Mods} <- maps:to_list(Handlers), M <- Mods]
     }];
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
listener_opt(<<"service">>, <<"password">>, V) -> [{password, b2l(V)}].

https_options(M) ->
    VM = case M of
             #{<<"verify_mode">> := Mode} -> [{verify_mode, b2a(Mode)}];
             _ -> []
         end,
    Opts = maps:without([<<"verify_mode">>], M),
    VM ++ client_tls_options(Opts).

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

listener_tls_mode(#{<<"mode">> := Mode}) -> [b2a(Mode)];
listener_tls_mode(_) -> [].

cowboy_module(Type, #{<<"host">> := Host, <<"path">> := Path} = Options) ->
    Opts = maps:without([<<"host">>, <<"path">>], Options),
    ModuleOpts = cowboy_module_options(Type, Opts),
    {b2l(Host), b2l(Path), b2a(Type), ModuleOpts}.

cowboy_module_options(<<"mod_websockets">>, #{<<"ejabberd_service">> := Opts}) ->
    [{ejabberd_service,
      lists:append([listener_opt(<<"service">>, K, V) || {K, V} <- maps:to_list(Opts)])}];
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

cowboy_transport_opt(<<"num_acceptors">>, N) -> N;
cowboy_transport_opt(<<"max_connections">>, N) -> N.

cowboy_protocol_opt(<<"compress">>, V) -> V.

listener_module(<<"http">>) -> ejabberd_cowboy;
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
    acl:to_record(Host, b2a(ACLName), acl_data(Content)).

acl_data(Content) when is_map(Content) ->
    case maps:to_list(Content) of
        [{Key, Values}] when is_list(Values) ->
            list_to_tuple([b2a(Key) | Values]);
        [{Key, Value}] when is_binary(Value) ->
            {b2a(Key), Value}
    end;
acl_data(Value) when is_binary(Value) -> b2a(Value).

process_access_rule(Name, Host, Contents) ->
    Rules = [{access_rule_value(Value), b2a(ACL)} ||
                #{<<"acl">> := ACL, <<"value">> := Value} <- Contents],
    #config{key = {access, b2a(Name), Host}, value = Rules}.

access_rule_value(B) when is_binary(B) -> b2a(B);
access_rule_value(V) -> V.

process_shaper(Name, Host, #{<<"max_rate">> := MaxRate}) ->
    #config{key = {shaper, b2a(Name), Host}, value = {maxrate, MaxRate}}.

process_pool_type(Type, Content) ->
    [process_pool(b2a(Type), b2a(Name), C) || {Name, C} <- maps:to_list(Content)].

process_pool(Type, Tag, M) ->
    ConnectionOptions = maps:get(<<"connection">>, M, #{}),
    Scope = pool_scope(M),
    Options = maps:without([<<"scope">>, <<"host">>, <<"connection">>], M),
    {Type, Scope, Tag, pool_options(Options), connection_options(Type, ConnectionOptions)}.

connection_options(rdbms, Options) ->
    [{server, rdbms_server(Options)}];
connection_options(redis, Options) ->
    [{b2a(K), redis_option(K, V)} || {K, V} <- maps:to_list(Options)];
connection_options(ldap, Options) ->
    [ldap_option(K, V) || {K, V} <- maps:to_list(Options)];
connection_options(riak, Options = #{<<"username">> := UserName,
                                     <<"password">> := Password}) ->
    [{credentials, b2l(UserName), b2l(Password)} |
     [riak_option(K, V) || {K, V} <- maps:to_list(Options)]];
connection_options(cassandra, Options) ->
    [cassandra_option(K, V) || {K, V} <- maps:to_list(Options)].

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

redis_option(<<"host">>, Host) -> b2l(Host);
redis_option(<<"port">>, Port) -> Port;
redis_option(<<"database">>, Database) -> b2l(Database);
redis_option(<<"password">>, Password) -> b2l(Password).

ldap_option(<<"host">>, Host) -> {host, b2l(Host)};
ldap_option(<<"port">>, Port) -> {port, Port};
ldap_option(<<"rootdn">>, RootDN) -> {rootdn, b2l(RootDN)};
ldap_option(<<"password">>, Password) -> {password, b2l(Password)};
ldap_option(<<"encrypt">>, <<"tls">>) -> {encrypt, tls};
ldap_option(<<"encrypt">>, <<"none">>) -> {encrypt, none};
ldap_option(<<"tls">>, Options) -> {tls_options, client_tls_options(Options)}.

riak_option(<<"address">>, Addr) -> {address, b2l(Addr)};
riak_option(<<"port">>, Port) -> {port, Port};
riak_option(<<"username">>, UserName) -> {username, b2l(UserName)};
riak_option(<<"password">>, Password) -> {password, b2l(Password)};
riak_option(<<"cacertfile">>, Path) -> {cacertfile, b2l(Path)};
riak_option(<<"tls">>, Options) -> {ssl_opts, client_tls_options(Options)}.

cassandra_option(<<"tls">>, Options) -> {ssl, client_tls_options(Options)}.

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
client_tls_option(<<"certfile">>, V) -> {certfile, b2l(V)};
client_tls_option(<<"cacertfile">>, V) -> {cacertfile, b2l(V)};
client_tls_option(<<"keyfile">>, V) -> {keyfile, b2l(V)};
client_tls_option(<<"password">>, V) -> {password, b2l(V)};
client_tls_option(<<"server_name_indication">>, false) -> {server_name_indication, disable};
client_tls_option(<<"ciphers">>, L) -> {ciphers, [tls_cipher(C) || C <- L]};
client_tls_option(<<"versions">>, L) -> {versions, [b2a(V) || V <- L]}.

tls_cipher(#{<<"key_exchange">> := KEx,
             <<"cipher">> := Cipher,
             <<"mac">> := MAC,
             <<"prf">> := PRF}) ->
    #{key_exchange => b2a(KEx), cipher => b2a(Cipher), mac => b2a(MAC), prf => b2a(PRF)};
tls_cipher(Cipher) -> b2l(Cipher).

verify_peer(false) -> verify_none;
verify_peer(true) -> verify_peer.

pool_scope(#{<<"scope">> := <<"single_host">>, <<"host">> := Host}) -> Host;
pool_scope(#{<<"scope">> := Scope}) -> b2a(Scope).

%% TODO replace with binary_to_existing_atom where possible, prevent atom leak
b2a(B) -> binary_to_atom(B, utf8).

b2l(B) -> binary_to_list(B).
