-module(mongoose_config_validator_toml).

-export([validate/2]).
-export([module_option_paths/0]).

-include("mongoose.hrl").
-include("ejabberd_config.hrl").
-include_lib("jid/include/jid.hrl").

-define(HOST, 'HOST').

-spec validate(mongoose_config_parser_toml:path(),
               mongoose_config_parser_toml:option() | mongoose_config_parser_toml:config_list()) ->
          any().
validate(Path, [F]) when is_function(F, 1) ->
    validate(Path, F(?HOST));

%% general
validate([<<"loglevel">>, <<"general">>],
         [#local_config{value = Val}]) -> validate_loglevel(Val);
validate([item, <<"hosts">>, <<"general">>],
         [Value]) ->
    validate_non_empty_binary(Value);
validate([<<"hosts">>, <<"general">>],
         [#config{value = Val}]) ->
    validate_hosts(Val);
validate([<<"registration_timeout">>, <<"general">>],
         [#local_config{value = Val}]) ->
    validate_timeout(Val);
validate([<<"language">>, <<"general">>],
         [#config{value = Value}]) ->
    validate_non_empty_binary(Value);
validate([<<"all_metrics_are_global">>, <<"general">>],
         [#local_config{value = Val}]) ->
    validate_boolean(Val);
validate([<<"sm_backend">>, <<"general">>],
         [#config{value = {Backend, []}}]) ->
    validate_module(list_to_atom("ejabberd_sm_" ++ atom_to_list(Backend)));
validate([<<"max_fsm_queue">>, <<"general">>],
         [#local_config{value = Value}]) ->
    validate_positive_integer(Value);
validate([<<"rdbms_server_type">>, <<"general">>],
         [#local_config{value = Value}]) ->
    validate_enum(Value, [mssql, pgsql]);
validate([item, <<"override">>, <<"general">>],
         [{override, Value}]) ->
    validate_enum(Value, [local, global, acls]);
validate([<<"override">>, <<"general">>],
         Items) ->
    validate_unique_items(Items);
validate([<<"pgsql_users_number_estimate">>, <<"general">>|Path],
         [#local_config{value = Value}]) ->
    validate_root_or_host_config(Path),
    validate_boolean(Value);
validate([<<"route_subdomains">>, <<"general">>|Path],
         [#local_config{value = Value}]) ->
    validate_root_or_host_config(Path),
    validate_enum(Value, [s2s]);
validate([item, <<"routing_modules">>, <<"general">>],
         [Value]) ->
    validate_module(Value);
validate([<<"replaced_wait_timeout">>, <<"general">>|Path],
         [#local_config{value = Value}]) ->
    validate_root_or_host_config(Path),
    validate_positive_integer(Value);
validate([<<"hide_service_name">>, <<"general">>|Path],
         [#local_config{value = Value}]) ->
    validate_root_or_host_config(Path),
    validate_boolean(Value);

%% listen
validate([item, _Type, <<"listen">>],
         [{{Port, _IPT, _Proto}, _Module, _Opts}]) ->
    validate_port(Port);
validate([<<"backlog">>, item, _Type, <<"listen">>],
         [{backlog, Value}]) ->
    validate_non_negative_integer(Value);
validate([<<"proxy_protocol">>, item, _Type, <<"listen">>],
         [{proxy_protocol, Value}]) ->
    validate_boolean(Value);
validate([<<"num_acceptors">>, item, _Type, <<"listen">>],
         [{acceptors_num, Value}]) ->
    validate_positive_integer(Value);
validate([<<"access">>, item, _Type, <<"listen">>],
         [{access, Value}]) ->
    validate_non_empty_atom(Value);
validate([<<"shaper">>, item, _Type, <<"listen">>],
         [{shaper, Value}]) ->
    validate_non_empty_atom(Value);
validate([<<"shaper_rule">>, item, <<"service">>, <<"listen">>],
         [{shaper_rule, Value}]) ->
    validate_non_empty_atom(Value);
validate([<<"xml_socket">>, item, <<"c2s">>, <<"listen">>],
         [{xml_socket, Value}]) ->
    validate_boolean(Value);
validate([<<"zlib">>, item, <<"c2s">>, <<"listen">>],
         [{zlib, Value}]) ->
    validate_positive_integer(Value);
validate([<<"hibernate_after">>, item, _, <<"listen">>],
         [{hibernate_after, Value}]) ->
    validate_non_negative_integer(Value);
validate([<<"mode">>, {tls, _}, item, <<"c2s">>, <<"listen">>],
         [Value]) ->
    validate_enum(Value, [tls, starttls, starttls_required]);
validate([<<"verify_mode">>, {tls, just_tls}, item, <<"c2s">>, <<"listen">>],
         Value) ->
    validate_enum(Value, [peer, selfsigned_peer, none]);
validate([<<"disconnect_on_failure">>, {tls, just_tls}, item, <<"c2s">>, <<"listen">>],
         Value) ->
    validate_boolean(Value);
validate([item, <<"crl_files">>, {tls, just_tls}, item, <<"c2s">>, <<"listen">>],
         [Value]) ->
    validate_non_empty_string(Value);
validate([item, <<"protocol_options">>, _TLS, item, _Type, <<"listen">>],
         [Value]) ->
    validate_non_empty_string(Value);
validate([FileType, _TLS, item, _Type, <<"listen">>],
         [{_, Value}]) when FileType =:= <<"certfile">>;
                            FileType =:= <<"cacertfile">>;
                            FileType =:= <<"dhfile">> ->
    validate_non_empty_string(Value);
validate([<<"max_stanza_size">>, item, _Type, <<"listen">>],
         [{max_stanza_size, Value}]) ->
    validate_positive_integer(Value);
validate([<<"max_fsm_queue">>, item, _Type, <<"listen">>],
         [{max_fsm_queue, Value}]) ->
    validate_positive_integer(Value);
validate([<<"check_from">>, item, <<"service">>, <<"listen">>],
         [{service_check_from, Value}]) ->
    validate_boolean(Value);
validate([<<"hidden_components">>, item, <<"service">>, <<"listen">>],
         [{hidden_components, Value}]) ->
    validate_boolean(Value);
validate([<<"conflict_behaviour">>, item, <<"service">>, <<"listen">>],
         [{conflict_behaviour, Value}]) ->
    validate_enum(Value, [kick_old, disconnect]);
validate([<<"password">>, item, <<"service">>, <<"listen">>],
         [{password, Value}]) ->
    validate_non_empty_string(Value);
validate([<<"verify_mode">>, <<"tls">>, item, <<"http">>, <<"listen">>],
         [{verify_mode, Value}]) ->
    validate_enum(Value, [peer, selfsigned_peer, none]);
validate([<<"num_acceptors">>, <<"transport">>, item, <<"http">>, <<"listen">>],
         [{num_acceptors, Value}]) ->
    validate_positive_integer(Value);
validate([<<"max_connections">>, <<"transport">>, item, <<"http">>, <<"listen">>],
         [{max_connections, Value}]) ->
    validate_non_negative_integer_or_infinity(Value);
validate([<<"compress">>, <<"protocol">>, item, <<"http">>, <<"listen">>],
         [{compress, Value}]) ->
    validate_boolean(Value);
validate([item, <<"lasse_handler">>, <<"handlers">>, item, <<"http">>, <<"listen">>],
         [{Host, _Path, lasse_handler, Opts}]) ->
    validate_non_empty_string(Host),
    [Module] = Opts,
    validate_module(Module);
validate([item, <<"handlers">>,
          item, <<"mongoose_api">>, <<"handlers">>, item, <<"http">>, <<"listen">>],
         [Value]) ->
    validate_module(Value);
validate([item, _TypeBin, <<"handlers">>, item, <<"http">>, <<"listen">>],
         [{Host, _Path, Type, _Opts}]) ->
    validate_non_empty_string(Host),
    validate_module(Type);

%% auth
validate([item, <<"methods">>, <<"auth">>|Path],
         [Value]) ->
    validate_root_or_host_config(Path),
    validate_module(list_to_atom("ejabberd_auth_" ++ atom_to_list(Value)));
validate([<<"password">>, <<"auth">>|Path],
         [{password_format, Value}]) ->
    validate_root_or_host_config(Path),
    validate_password_format(Value);
validate([item, <<"hash">>, <<"password">>, <<"auth">>|Path],
         [Value]) ->
    validate_root_or_host_config(Path),
    validate_enum(Value, [sha, sha224, sha256, sha384, sha512]);
validate([<<"scram_iterations">>, <<"auth">>|Path],
         [{scram_iterations, Value}]) ->
    validate_root_or_host_config(Path),
    validate_positive_integer(Value);
validate([item, <<"cyrsasl_external">>, <<"auth">>|Path],
         [{mod, Module}]) ->
    validate_root_or_host_config(Path),
    validate_module(Module);
validate([<<"allow_multiple_connections">>, <<"auth">>|Path],
         [{allow_multiple_connections, Value}]) ->
    validate_root_or_host_config(Path),
    validate_boolean(Value);
validate([<<"anonymous_protocol">>, <<"auth">>|Path],
         [{anonymous_protocol, Value}]) ->
    validate_root_or_host_config(Path),
    validate_enum(Value, [sasl_anon, login_anon, both]);
validate([Pool, <<"ldap">>, <<"auth">>|Path],
         [{_, Value}]) when Pool =:= <<"pool_tag">>;
                            Pool =:= <<"bind_pool_tag">> ->
    validate_root_or_host_config(Path),
    validate_non_empty_atom(Value);
validate([<<"operation">>, <<"local_filter">>, <<"ldap">>, <<"auth">>|Path],
         [{operation, Value}]) ->
    validate_root_or_host_config(Path),
    validate_enum(Value, [equal, not_equal]);
validate([<<"attribute">>, <<"local_filter">>, <<"ldap">>, <<"auth">>|Path],
         [{attribute, Value}]) ->
    validate_root_or_host_config(Path),
    validate_non_empty_string(Value);
validate([<<"values">>, <<"local_filter">>, <<"ldap">>, <<"auth">>|Path],
         [{values, Value}]) ->
    validate_root_or_host_config(Path),
    validate_non_empty_list(Value);
validate([<<"deref">>, <<"ldap">>, <<"auth">>|Path],
         [{ldap_deref, Value}]) ->
    validate_root_or_host_config(Path),
    validate_enum(Value, [never, always, finding, searching]);
validate([item, <<"sasl_mechanisms">>, <<"auth">>|Path],
         [Value]) ->
    validate_root_or_host_config(Path),
    validate_module(Value);
validate([<<"extauth_instances">>, <<"auth">>|Path],
         [{extauth_instances, Value}]) ->
    validate_root_or_host_config(Path),
    validate_positive_integer(Value);

%% outgoing_pools
validate([_Tag, _Type, <<"outgoing_pools">>],
         [{TypeAtom, Scope, TagAtom, _Options, _ConnectionOptions}]) ->
    validate_enum(TypeAtom, [redis, riak, http, rdbms, cassandra, elastic, generic, rabbit, ldap]),
    validate_pool_scope(Scope),
    validate_non_empty_atom(TagAtom);
validate([<<"workers">>, _Tag, _Type, <<"outgoing_pools">>],
         [{workers, Value}]) ->
    validate_positive_integer(Value);
validate([<<"strategy">>, _Tag, _Type, <<"outgoing_pools">>],
         [{strategy, Value}]) ->
    validate_wpool_strategy(Value);
validate([<<"call_timeout">>, _Tag, _Type, <<"outgoing_pools">>],
         [{call_timeout, Value}]) ->
    validate_positive_integer(Value);
validate([<<"keepalive_interval">>, _Conn, _Tag, <<"rdbms">>, <<"outgoing_pools">>],
         [{keepalive_interval, Value}]) ->
    validate_positive_integer(Value);
validate([{connection, Driver}, _Tag, <<"rdbms">>, <<"outgoing_pools">>],
         [_Value]) ->
    validate_enum(Driver, [odbc, pgsql, mysql]);
validate([Key, {connection, _}, _Tag, <<"rdbms">>, <<"outgoing_pools">>],
         [{_, Value}]) when Key =:= <<"host">>;
                            Key =:= <<"database">>;
                            Key =:= <<"username">>;
                            Key =:= <<"password">> ->
    validate_non_empty_string(Value);
validate([<<"port">>, {connection, _}, _Tag, <<"rdbms">>, <<"outgoing_pools">>],
         [{port, Value}]) ->
    validate_port(Value);
validate([<<"host">>, _Conn, _Tag, <<"http">>, <<"outgoing_pools">>],
         [{server, Value}]) ->
    validate_non_empty_string(Value);
validate([<<"path_prefix">>, _Conn, _Tag, <<"http">>, <<"outgoing_pools">>],
         [{path_prefix, Value}]) ->
    validate_non_empty_string(Value);
validate([<<"request_timeout">>, _Conn, _Tag, <<"http">>, <<"outgoing_pools">>],
         [{request_timeout, Value}]) ->
    validate_non_negative_integer(Value);
validate([<<"retry">>, _Conn, _Tag, <<"http">>, <<"outgoing_pools">>],
         [{retry, Value}]) ->
    validate_non_negative_integer(Value);
validate([<<"retry_timeout">>, _Conn, _Tag, <<"http">>, <<"outgoing_pools">>],
         [{retry_timeout, Value}]) ->
    validate_positive_integer(Value);
validate([<<"host">>, _Conn, _Tag, <<"redis">>, <<"outgoing_pools">>],
         [{host, Value}]) ->
    validate_non_empty_string(Value);
validate([<<"port">>, _Conn, _Tag, <<"redis">>, <<"outgoing_pools">>],
         [{port, Value}]) ->
    validate_port(Value);
validate([<<"database">>, _Conn, _Tag, <<"redis">>, <<"outgoing_pools">>],
         [{database, Value}]) ->
    validate_non_negative_integer(Value);
validate([<<"password">>, _Conn, _Tag, <<"redis">>, <<"outgoing_pools">>],
         [{host, Value}]) ->
    validate_string(Value);
validate([<<"address">>, _Conn, _Tag, <<"riak">>, <<"outgoing_pools">>],
         [{address, Value}]) ->
    validate_non_empty_string(Value);
validate([<<"port">>, _Conn, _Tag, <<"riak">>, <<"outgoing_pools">>],
         [{port, Value}]) ->
    validate_port(Value);
validate([<<"credentials">>, _Conn, _Tag, <<"riak">>, <<"outgoing_pools">>],
         [{credentials, User, Password}]) ->
    validate_non_empty_string(User),
    validate_non_empty_string(Password);
validate([<<"cacertfile">>, _Conn, _Tag, <<"riak">>, <<"outgoing_pools">>],
         [{cacertfile, Value}]) ->
    validate_non_empty_string(Value);
validate([<<"servers">>, _Conn, _Tag, <<"cassandra">>, <<"outgoing_pools">>],
         [{servers, Value}]) ->
    [{validate_non_empty_string(Host), validate_port(Port)} || {Host, Port} <- Value];
validate([<<"keyspace">>, _Conn, _Tag, <<"cassandra">>, <<"outgoing_pools">>],
         [{keyspace, Value}]) ->
    validate_non_empty_string(Value);
validate([<<"host">>, _Conn, _Tag, <<"elastic">>, <<"outgoing_pools">>],
         [{host, Value}]) ->
    validate_non_empty_string(Value);
validate([<<"port">>, _Conn, _Tag, <<"elastic">>, <<"outgoing_pools">>],
         [{host, Value}]) ->
    validate_port(Value);
validate([<<"amqp_host">>, _Conn, _Tag, <<"rabbit">>, <<"outgoing_pools">>],
         [{amqp_host, Value}]) ->
    validate_non_empty_string(Value);
validate([<<"amqp_port">>, _Conn, _Tag, <<"rabbit">>, <<"outgoing_pools">>],
         [{amqp_port, Value}]) ->
    validate_port(Value);
validate([<<"amqp_username">>, _Conn, _Tag, <<"rabbit">>, <<"outgoing_pools">>],
         [{amqp_username, Value}]) ->
    validate_non_empty_string(Value);
validate([<<"amqp_password">>, _Conn, _Tag, <<"rabbit">>, <<"outgoing_pools">>],
         [{amqp_password, Value}]) ->
    validate_non_empty_string(Value);
validate([<<"confirms_enabled">>, _Conn, _Tag, <<"rabbit">>, <<"outgoing_pools">>],
         [{confirms_enabled, Value}]) ->
    validate_boolean(Value);
validate([<<"max_worker_queue_len">>, _Conn, _Tag, <<"rabbit">>, <<"outgoing_pools">>],
         [{max_worker_queue_len, Value}]) ->
    validate_non_negative_integer_or_infinity(Value);
validate([<<"host">>, _Conn, _Tag, <<"ldap">>, <<"outgoing_pools">>],
         [{host, Value}]) ->
    validate_non_empty_string(Value);
validate([<<"port">>, _Conn, _Tag, <<"ldap">>, <<"outgoing_pools">>],
         [{port, Value}]) ->
    validate_port(Value);
validate([<<"servers">>, _Conn, _Tag, <<"ldap">>, <<"outgoing_pools">>],
         [{servers, Value}]) ->
    [validate_non_empty_string(Server) || Server <- Value];
validate([<<"encrypt">>, _Conn, _Tag, <<"ldap">>, <<"outgoing_pools">>],
         [{encrypt, Value}]) ->
    validate_enum(Value, [tls, none]);
validate([<<"rootdn">>, _Conn, _Tag, <<"ldap">>, <<"outgoing_pools">>],
         [{rootdn, Value}]) ->
    validate_string(Value);
validate([<<"password">>, _Conn, _Tag, <<"ldap">>, <<"outgoing_pools">>],
         [{password, Value}]) ->
    validate_string(Value);
validate([<<"connect_interval">>, _Conn, _Tag, <<"ldap">>, <<"outgoing_pools">>],
         [{connect_interval, Value}]) ->
    validate_positive_integer(Value);

%% shaper
validate([_, <<"shaper">>|Path],
         [#config{value = {maxrate, Value}}]) ->
    validate_root_or_host_config(Path),
    validate_positive_integer(Value);

%% s2s
validate([<<"timeout">>, <<"dns">>, <<"s2s">>],
         [{timeout, Value}]) ->
    validate_positive_integer(Value);
validate([<<"retries">>, <<"dns">>, <<"s2s">>],
         [{retries, Value}]) ->
    validate_positive_integer(Value);
validate([<<"port">>, <<"outgoing">>, <<"s2s">>],
         [#local_config{value = Value}]) ->
    validate_port(Value);
validate([<<"ip_versions">>, <<"outgoing">>, <<"s2s">>],
         [#local_config{value = Value}]) ->
    validate_non_empty_list(Value);
validate([<<"connection_timeout">>, <<"outgoing">>, <<"s2s">>],
         [#local_config{value = Value}]) ->
    validate_timeout(Value);
validate([<<"use_starttls">>, <<"s2s">>],
         [#local_config{value = Value}]) ->
    validate_enum(Value, [false, optional, required, required_trusted]);
validate([<<"certfile">>, <<"s2s">>],
         [#local_config{value = Value}]) ->
    validate_non_empty_string(Value);
validate([<<"default_policy">>, <<"s2s">>|Path],
         [#local_config{value = Value}]) ->
    validate_root_or_host_config(Path),
    validate_enum(Value, [allow, deny]);
validate([<<"host">>, item, <<"address">>, <<"s2s">>],
         [{host, Value}]) ->
    validate_non_empty_binary(Value);
validate([<<"ip_address">>, item, <<"address">>, <<"s2s">>],
         [{ip_address, Value}]) ->
    validate_ip_address(Value);
validate([<<"port">>, item, <<"address">>, <<"s2s">>],
         [{port, Value}]) ->
    validate_port(Value);
validate([item, <<"domain_certfile">>, <<"s2s">>],
         [#local_config{key = {domain_certfile, Domain}, value = Certfile}]) ->
    validate_non_empty_string(Domain),
    validate_non_empty_string(Certfile);
validate([<<"shared">>, <<"s2s">>|Path],
         [#local_config{value = Value}]) ->
    validate_root_or_host_config(Path),
    validate_non_empty_binary(Value);
validate([<<"max_retry_delay">>, <<"s2s">>|Path],
         [#local_config{value = Value}]) ->
    validate_root_or_host_config(Path),
    validate_positive_integer(Value);

validate(Path, Value) ->
    PathR = lists:reverse(Path),
    validate_r(Path, PathR, Value).

validate_r(Path, [<<"modules">>|_], Value) ->
    validate_modules(Path, Value);
validate_r(Path, [<<"host_config">>, {host, _}, <<"modules">>|_], Value) ->
    validate_modules(path_without_host_config(Path), Value);
validate_r(Path, _PathR, Value) ->
    ?LOG_DEBUG(#{ what => validate_unknown, path => Path, value => Value}).

validate_modules([_,<<"modules">>], [{Mod,_}]) ->
    validate_module(Mod);
validate_modules(Path, Value) ->
    Types = [Type || {Path1, Type} <- module_option_paths(), Path =:= Path1],
    case Types of
        [] ->
            ?LOG_DEBUG(#{ what => validate_unknown_module_path, path => Path, value => Value});
        _ ->
            ?LOG_DEBUG(#{ what => validate_module_with, path => Path, value => Value, types => Types})
    end,
    [try
         validate_type(Type, Path, Value)
     catch Class:Reason:Stacktrace ->
               erlang:raise(Class, {Type, Reason}, Stacktrace)
     end || Type <- Types],
    ok.

tls_opts_spec() ->
    #{cacertfile => {wrapped, cafile, filename}, %% renamed option
      certfile => filename,
      dhfile => filename,
      ciphers => string}.

module_option_types() ->
    [{mod_adhoc, iqdisc, iqdisc},
     {mod_adhoc, report_commands_node, boolean},
     {mod_auth_token, iqdisc, iqdisc},
     %% Technically, we can use this,
     %% if parser was calling handle for each suboption
%    {mod_auth_token, validity_period, {list, #{token => auth_token_domain,
%                                               value => non_neg_integer,
%                                               unit => period_unit}}},
     {mod_auth_token, validity_period, {unwrapped, {multi, validity_period}}},
     {mod_bosh, inactivity, non_neg_integer_or_inf},
     {mod_bosh, max_wait, non_neg_integer_or_inf},
     {mod_bosh, server_acks, boolean},
     {mod_bosh, backend, backend},
     {mod_carboncopy, iqdisc, iqdisc},
     {mod_caps, cache_size, non_neg_integer_or_inf},
     {mod_caps, cache_life_time, non_neg_integer_or_inf},
     {mod_csi, buffer_max, non_neg_integer_or_inf},
     {mod_disco, users_can_see_hidden_services, boolean},
     {mod_disco, extra_domains, {list, binary_domain}},
     {mod_disco, urls, {list, url}},
     {mod_disco, server_info, {list, #{name => non_empty_binary,
                                       module => {list, module},
                                       urls => {list, url}}}},
     {mod_inbox, iqdisc, iqdisc},
     {mod_inbox, backend, backend},
     {mod_inbox, aff_changes, boolean},
     {mod_inbox, remove_on_kicked, boolean},
     {mod_inbox, reset_markers, {list, chat_marker_type}},
     {mod_inbox, groupchat, {list, groupchat_type}},
     {mod_global_distrib, global_host, domain},
     {mod_global_distrib, local_host, domain},
     {mod_global_distrib, message_ttl, non_neg_integer},
     {mod_global_distrib, hosts_refresh_interval, non_neg_integer},
     {mod_global_distrib, connections, {wrapped_section, #{
        endpoints => {list, #{host => network_address, port => network_port}},
        advertised_endpoints => {optional_section, advertised_endpoints,
                                 {list, #{host => network_address, port => network_port}}},
        connections_per_endpoint => non_neg_integer,
        endpoint_refresh_interval => pos_integer,
        endpoint_refresh_interval_when_empty => pos_integer,
        disabled_gc_interval => non_neg_integer,
        tls => {optional_section, tls_opts,
                {wrapped_section, tls_opts_spec()}}}}},
     {mod_global_distrib, redis, {wrapped_section, #{
            pool => non_empty_atom,
            expire_after => non_neg_integer,
            refresh_after => non_neg_integer}}},
     {mod_global_distrib, cache, {wrapped_section, #{
                                    cache_missed => boolean,
                                    domain_lifetime_seconds => non_neg_integer,
                                    jid_lifetime_seconds => non_neg_integer,
                                    max_jids => non_neg_integer}}},
     {mod_global_distrib, bounce, {wrapped_section, #{
                                    resend_after_ms => non_neg_integer,
                                    max_retries => non_neg_integer}}},
     {mod_event_pusher, backend, #{
         sns => {wrapped_section,
                 #{access_key_id => string,
                   secret_access_key => string,
                   region => string,
                   account_id => string,
                   sns_host => string,
                   muc_host => domain_template,
                   presence_updates_topic => string,
                   pm_messages_topic => string,
                   muc_messages_topic => string,
                   plugin_module => module,
                   pool_size => non_neg_integer,
                   publish_retry_count => non_neg_integer,
                   publish_retry_time_ms => non_neg_integer}},
          push => {wrapped_section,
                   #{backend => {backend, mod_event_pusher_push},
                     wpool => {wrapped_section, #{strategy => wpool_strategy, workers => pos_integer}},
                     plugin_module => module,
                     virtual_pubsub_hosts => {list, domain_template}}},
          http => {wrapped_section,
                   #{pool_name => non_empty_atom,
                     path => string,
                     callback_module => module}},
          rabbit => {wrapped_section,
                     #{presence_exchange => {wrapped_section,
                                             #{name => non_empty_binary,
                                               type => non_empty_binary}},
                       chat_msg_exchange => {wrapped_section,
                                             #{name => non_empty_binary,
                                               sent_topic => non_empty_binary,
                                               recv_topic => non_empty_binary}},
                       groupchat_msg_exchange => {wrapped_section,
                                                  #{name => non_empty_binary,
                                                    sent_topic => non_empty_binary,
                                                    recv_topic => non_empty_binary}}}}

         }},
     {mod_http_upload, iqdisc, iqdisc},
     {mod_http_upload, backend, backend},
     {mod_http_upload, host, domain_template},
     {mod_http_upload, expiration_time, non_neg_integer},
     {mod_http_upload, token_bytes, non_neg_integer},
     {mod_http_upload, token_bytes, pos_integer},
     {mod_http_upload, max_file_size, non_neg_integer},
     {mod_http_upload, s3, {wrapped_section, #{
                              bucket_url => url,
                              add_acl => boolean,
                              region => string,
                              access_key_id => string,
                              secret_access_key => string
                             }}},
     {mod_register, iqdisc, iqdisc},
     %% Validator is not called for each leaf, so we need a separate validator below
%    {mod_register, ip_access, {list, #{address => ip_mask, policy => {enum, [allow, deny]}}}},
     {mod_register, ip_access, {list, ip_access}},
     {mod_register, welcome_message, #{subject => string, body => string}},
     {mod_register, access, non_empty_atom},
     {mod_register, registration_watchers, {list, jid}},
     {mod_register, password_strength, non_neg_integer}
    ].

type_to_validator() ->
    #{string => fun validate_string/1,
      boolean => fun validate_boolean/1,
      atom => fun validate_non_empty_atom/1,
      non_empty_atom => fun validate_non_empty_atom/1,
      non_empty_binary => fun validate_non_empty_binary/1,
      non_neg_integer => fun validate_non_negative_integer/1,
      non_neg_integer_or_inf => fun validate_non_negative_integer_or_infinity/1,
      pos_integer => fun validate_positive_integer/1,
      wpool_strategy => fun validate_wpool_strategy/1,
      iqdisc => fun validate_iqdisc/1,
      url => fun validate_url/1,
      module => fun validate_module/1,
      groupchat_type => fun validate_groupchat_type/1,
      chat_marker_type => fun validate_chat_marker_type/1,
      ip_mask => fun validate_ip_mask/1,
      jid => fun validate_jid/1,
      auth_token_domain => fun validate_auth_token_domain/1,
      domain => fun validate_domain/1,
      binary_domain => fun validate_binary_domain/1,
      domain_template => fun validate_domain_template/1,
      binary_domain_template => fun validate_binary_domain_template/1,
      period_unit => fun validate_period_unit/1,
      validity_period => fun validate_validity_period/1,
      ip_access => fun validate_ip_access/1,
      ip_address => fun validate_ip_address/1,
      network_address  => fun validate_network_address/1,
      network_port => fun validate_network_port/1,
      filename => fun validate_filename/1
%     wpool_options => fun validate_wpool_options/1
     }.

validate_type({unwrapped, Type}, Path, Value) ->
    validate_type(Type, Path, Value);
validate_type({multi, Type}, Path, Value) ->
    %% Validate multiple values in a list
    [validate_type(Type, Path, Val) || Val <- Value];
validate_type({listed, Type}, Path, Value) ->
    case Value of
        [Unwrapped] ->
            validate_type(Type, Path, Unwrapped);
        _ ->
            error(#{what => list_value_expected,
                    text => <<"We expect Value argument to be a list of one">>,
                    value => Value, path => Path, type => Type})
    end;
validate_type({wrapped, Wrapper, Type}, Path, Value) ->
    case Value of
        {Wrapper, Unwrapped} ->
            validate_type(Type, Path, Unwrapped);
        _ ->
            error(#{what => expected_to_be_wrapped,
                    wrapper => Wrapper, value => Value, path => Path, type => Type})
    end;
validate_type(backend, Path, Value) ->
    Module = path_to_module(Path),
    validate_backend(Module, Value);
validate_type({backend, Module}, _Path, Value) ->
    validate_backend(Module, Value);
validate_type({enum, Types}, _Path, Value) ->
    validate_enum(Value, Types);
validate_type({optional_section, Name}, _Path, Value) ->
    validate_optional_section(Name, Value);
validate_type(Type, Path, Value) when is_atom(Type) ->
    case maps:find(Type, type_to_validator()) of
        {ok, F} ->
            F(Value);
        _ ->
            error(#{what => unknown_validator_type, type => Type,
                    path => Path, value => Value})
    end;
validate_type(Type, Path, Value) ->
    error(#{what => unknown_validator_type, type => Type,
            path => Path, value => Value}).

module_option_paths() ->
    lists:append([module_option_paths(M, O, T) || {M,O,T} <- module_option_types()]).

module_option_paths(Mod, Opt, Type) ->
    Path = [atom_to_binary(Opt, utf8), atom_to_binary(Mod, utf8), <<"modules">>],
    PathType = type_to_paths(add_wrapped(Opt, Type), Path),
    [{P, add_unlistify(T)} || {P,T} <- PathType].

%% Majority of values are inside a list.
%% We need to pass only head into a validator.
add_unlistify({unwrapped, Type}) ->
    Type;
add_unlistify({multi, Type}) ->
    {multi, Type};
add_unlistify(Type) ->
    {listed, Type}.

%% Values for basic module options are wrapped into an option name.
%% Tell to remove this wrapper and pass only option value into validators
%% for most non-nested values.
%% Example: {iqdisc, one_queue}
add_wrapped(_Opt, Type = {list, _}) ->
    Type;
add_wrapped(_Opt, Type = {unwrapped, _}) ->
    Type;
add_wrapped(_Opt, Type = {wrapped, _, _}) -> %% Already wrapped
    Type;
add_wrapped(_Opt, Type = {wrapped_section, _}) ->
    Type;
add_wrapped(_Opt, Type = {optional_section, _Type}) ->
    Type;
add_wrapped(_Opt, Type = {optional_section, _Name, _Type}) ->
    Type;
add_wrapped(_Opt, Type = #{}) ->
    Type;
add_wrapped(Opt, Type) ->
    {wrapped, Opt, Type}.

type_to_paths({list, Type}, Path) ->
    type_to_paths(Type, [item|Path]);
type_to_paths({optional_section, Name, Dict}, Path) ->
    [{Path, {optional_section, Name}}] ++ type_to_paths(Dict, Path);
type_to_paths({wrapped_section, Dict}, Path) when is_map(Dict) ->
    lists:append([type_to_paths(add_wrapped(Key, Type),
                                [atom_to_binary(Key, utf8)|Path])
                  || {Key, Type} <- maps:to_list(Dict)]);
type_to_paths(Dict, Path) when is_map(Dict) ->
    lists:append([type_to_paths(Type, [atom_to_binary(Key, utf8)|Path])
                  || {Key, Type} <- maps:to_list(Dict)]);
type_to_paths(Type, Path) ->
    [{Path, Type}].

path_to_module(Path) ->
    PathR = lists:reverse(Path),
    case PathR of
        [<<"host_config">>, _, <<"modules">>, Mod|_] ->
            binary_to_atom(Mod, utf8);
        [<<"modules">>, Mod|_] ->
            binary_to_atom(Mod, utf8)
    end.

path_without_host_config(Path) ->
    PathR = lists:reverse(Path),
    [<<"host_config">>, _|Rest] = PathR,
    lists:reverse(Rest).


%% validators

validate_loglevel(Level) ->
    mongoose_logs:loglevel_number_keyword(Level).

validate_non_empty_binary(Value) when is_binary(Value), Value =/= <<>> -> ok.

validate_hosts(Hosts = [_|_]) ->
    validate_unique_items(Hosts).

validate_unique_items(Items) ->
    L = sets:size(sets:from_list(Items)),
    L = length(Items).

validate_timeout(infinity) -> ok;
validate_timeout(Timeout) when is_integer(Timeout), Timeout > 0 -> ok.

validate_boolean(Value) when is_boolean(Value) -> ok.

validate_module(Mod) ->
    case code:ensure_loaded(Mod) of
        {module, _} ->
            ok;
        Other ->
            error(#{what => module_not_found, module => Mod, reason => Other})
    end.

validate_positive_integer(Value) when is_integer(Value), Value > 0 -> ok.

validate_non_negative_integer(Value) when is_integer(Value), Value >= 0 -> ok.

validate_non_negative_integer_or_infinity(Value) when is_integer(Value), Value >= 0 -> ok;
validate_non_negative_integer_or_infinity(infinity) -> ok.

validate_enum(Value, Values) ->
    case lists:member(Value, Values) of
        true ->
            ok;
        false ->
            error(#{what => validate_enum_failed,
                    value => Value,
                    allowed_values => Values})
    end.

validate_ip_address(Value) ->
    {ok, _} = inet:parse_address(Value).

validate_port(Value) when is_integer(Value), Value >= 0, Value =< 65535 -> ok.

validate_non_empty_atom(Value) when is_atom(Value), Value =/= '' -> ok.

validate_non_empty_string(Value) when is_list(Value), Value =/= "" -> ok.

validate_non_empty_list(Value) when is_list(Value), Value =/= [] -> ok.

validate_password_format({scram, [_|_]}) -> ok;
validate_password_format(Value) -> validate_enum(Value, [scram, plain]).

validate_pool_scope(Value) when is_binary(Value) -> validate_non_empty_binary(Value);
validate_pool_scope(Value) -> validate_enum(Value, [host, global]).

validate_root_or_host_config([]) -> ok;
validate_root_or_host_config([{host, _}, <<"host_config">>]) -> ok.

validate_list_of_jids(Jids) ->
    [validate_jid(Jid) || Jid <- Jids].

validate_jid(Jid) ->
    case jid:from_binary(Jid) of
        #jid{} ->
            ok;
        _ ->
            error(#{what => validate_jid_failed, value => Jid})
    end.

validate_iqdisc(no_queue) -> ok;
validate_iqdisc(one_queue) -> ok;
validate_iqdisc(parallel) -> ok;
validate_iqdisc({queues, N}) when is_integer(N), N > 0 -> ok.

-spec validate_auth_token_domain(mod_auth_token:token_type()) -> ok.
validate_auth_token_domain(Type) ->
    validate_enum(Type, [access, refresh, provision]).

validate_validity_period({{validity_period, Token}, {Value, Unit}}) ->
    validate_auth_token_domain(Token),
    validate_non_negative_integer(Value),
    validate_period_unit(Unit).

validate_period_unit(Unit) ->
    validate_enum(Unit, [days, hours, minutes, seconds]).

validate_ip_access({Access,_}) ->
    %% TODO validate second arg
    validate_enum(Access, [allow, deny]).

validate_backend(Mod, Backend) ->
    validate_module(backend_module:backend_module(Mod, Backend)).

validate_chat_marker_type(Type) ->
    validate_enum(Type, [displayed, received, acknowledged]).

validate_groupchat_type(Type) ->
    validate_enum(Type, [muc, muclight]).

validate_domain(Domain) ->
    #jid{luser = <<>>, lresource = <<>>} = jid:from_binary(list_to_binary(Domain)),
    validate_domain_res(Domain).

validate_domain_res(Domain) ->
    case inet_res:gethostbyname(Domain) of
        {ok, _} ->
            ok;
        {error,nxdomain} ->
            ?LOG_WARNING(#{what => cfg_validate_domain,
                           reason => nxdomain, domain => Domain,
                           text => <<"Couldn't resolve domain. "
                  "It could cause issues with production installations">>}),
            ok;
        {error,formerr} ->
            error(#{what => cfg_validate_domain_failed,
                    reason => formerr, text => <<"Invalid domain name">>,
                    domain => Domain})
    end.

validate_binary_domain(Domain) when is_binary(Domain) ->
    #jid{luser = <<>>, lresource = <<>>} = jid:from_binary(Domain),
    validate_domain_res(binary_to_list(Domain)).

validate_domain_template(Domain) ->
    validate_binary_domain(gen_mod:make_subhost(Domain, <<"example.com">>)).

validate_binary_domain_template(Domain) ->
    validate_domain_template(binary_to_list(Domain)).

validate_url(Url) ->
    validate_non_empty_string(Url).

validate_string(Value) ->
    is_binary(unicode:characters_to_binary(Value)).

validate_ip_mask({IP, Mask}) ->
    validate_string(inet:ntoa(IP)),
    validate_range(Mask, 0, 32).

validate_network_address(Value) ->
    validate_oneof(Value, [domain, ip_address]).

validate_oneof(Value, Validators) ->
    Map = type_to_validator(),
    Funs = [maps:get(Type, Map) || Type <- Validators],
    lists:any(fun(F) -> is_valid(F, Value) end, Funs).

is_valid(F, Value) ->
    try
        F(Value),
        true
    catch _:_ ->
              false
    end.

validate_network_port(Value) ->
    validate_range(Value, 0, 65535).

validate_range(Value, Min, Max) when Value >= Min; Value =< Max ->
    ok.

validate_wpool_strategy(Value) ->
    validate_enum(Value, [best_worker, random_worker, next_worker,
                          available_worker, next_available_worker]).

validate_filename(Filename) ->
    case file:read_file_info(Filename) of
        {ok, _} ->
            ok;
        Reason ->
            error(#{what => invalid_filename, filename => Filename, reason => Reason})
    end.

validate_optional_section(Name, {Name, false}) -> %% set to false to disable the feature
    ok;
validate_optional_section(Name, {Name, List}) when is_list(List) -> %% proplist
    ok.
