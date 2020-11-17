-module(mongoose_config_spec).

-compile(export_all).

-include("ejabberd_config.hrl").

-type config_node() :: #section{} | #option{} | #list{}.

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
                 <<"listen">> => listen()
                },
       required = [<<"general">>]
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
                                             validate = {module, ejabberd_sm_},
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
       required = [<<"hosts">>]
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
    #section{
       items = #{<<"verify_peer">> => #option{type = boolean,
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
                 <<"versions">> => #list{items = #option{type = atom}},
                 <<"verify_mode">> => #option{type = atom,
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
