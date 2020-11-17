-module(mongoose_config_validator_toml).

-export([validate/2,
         validate/3]).
-compile(export_all).

-include("mongoose.hrl").
-include("ejabberd_config.hrl").
-include_lib("jid/include/jid.hrl").

-define(HOST, 'HOST').

-spec validate(mongoose_config_parser_toml:path(),
               mongoose_config_parser_toml:option() | mongoose_config_parser_toml:config_list()) ->
          any().
validate(Path, [F]) when is_function(F, 1) ->
    validate(Path, F(?HOST));

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
validate([item, <<"sasl_external">>, <<"auth">>|Path],
         [{mod, Module}]) ->
    validate_root_or_host_config(Path),
    validate_module(Module);
validate([<<"allow_multiple_connections">>, <<"anonymous">>, <<"auth">>|Path],
         [{allow_multiple_connections, Value}]) ->
    validate_root_or_host_config(Path),
    validate_boolean(Value);
validate([<<"protocol">>, <<"anonymous">>, <<"auth">>|Path],
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
validate([<<"instances">>, <<"external">>, <<"auth">>|Path],
         [{extauth_instances, Value}]) ->
    validate_root_or_host_config(Path),
    validate_positive_integer(Value);
validate([<<"program">>, <<"external">>, <<"auth">>|Path],
         [{extauth_program, Value}]) ->
    validate_root_or_host_config(Path),
    validate_non_empty_string(Value);
validate([<<"file">>, <<"secret">>, <<"jwt">>, <<"auth">>|Path],
         [{jwt_secret_source, Value}]) ->
    validate_root_or_host_config(Path),
    validate_non_empty_string(Value);
validate([<<"env">>, <<"secret">>, <<"jwt">>, <<"auth">>|Path],
         [{jwt_secret_source, {env, Value}}]) ->
    validate_root_or_host_config(Path),
    validate_non_empty_string(Value);
validate([<<"algorithm">>, <<"jwt">>, <<"auth">>|Path],
         [{jwt_algorithm, Value}]) ->
    validate_root_or_host_config(Path),
    validate_enum(Value, ["HS256", "RS256", "ES256", "HS386", "RS386", "ES386",
                          "HS512", "RS512", "ES512"]);
validate([<<"username_key">>, <<"jwt">>, <<"auth">>|Path],
         [{jwt_username_key, Value}]) ->
    validate_root_or_host_config(Path),
    validate_non_empty_atom(Value);
validate([<<"bucket_type">>, <<"riak">>, <<"auth">>|Path],
         [{bucket_type, Value}]) ->
    validate_root_or_host_config(Path),
    validate_non_empty_binary(Value);

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
validate([<<"certfile">>, _Conn, _Tag, <<"riak">>, <<"outgoing_pools">>],
         [{certfile, Value}]) ->
    validate_non_empty_string(Value);
validate([<<"keyfile">>, _Conn, _Tag, <<"riak">>, <<"outgoing_pools">>],
         [{keyfile, Value}]) ->
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
    validate_positive_integer_or_infinity(Value);
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
validate([<<"host">>, item, <<"host_policy">>, <<"s2s">>|Path],
         [{host, Value}]) ->
    validate_root_or_host_config(Path),
    validate_non_empty_binary(Value);
validate([<<"policy">>, item, <<"host_policy">>, <<"s2s">>|Path],
         [{policy, Value}]) ->
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

%% Services
validate([item, <<"submods">>, <<"service_admin_extra">>, <<"services">>],
         [Value]) ->
    validate_backend(service_admin_extra, Value);
validate([<<"initial_report">>,
          <<"service_mongoose_system_metrics">>, <<"services">>],
         [{initial_report, Value}]) ->
    validate_non_negative_integer(Value);
validate([<<"periodic_report">>,
          <<"service_mongoose_system_metrics">>, <<"services">>],
         [{periodic_report, Value}]) ->
    validate_non_negative_integer(Value);
validate([<<"tracking_id">>,
          <<"service_mongoose_system_metrics">>, <<"services">>],
         [{tracking_id, Value}]) ->
    validate_non_empty_string(Value);

%% Modules
validate([<<"callback_module">>, <<"http">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{callback_module, V}]) ->
    validate_module(V);
validate([<<"path">>, <<"http">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{path, V}]) ->
    validate_string(V);
validate([<<"pool_name">>, <<"http">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{pool_name, V}]) ->
    validate_pool_name(V);
validate([<<"backend">>, <<"push">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{backend, V}]) ->
    validate_backend(mod_event_pusher_push, V);
validate([<<"plugin_module">>, <<"push">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{plugin_module, V}]) ->
    validate_module(V);
validate([item, <<"virtual_pubsub_hosts">>, <<"push">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [V]) ->
    validate_domain_template(V);
validate([<<"strategy">>, <<"wpool">>, <<"push">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{strategy, V}]) ->
    validate_wpool_strategy(V);
validate([<<"workers">>, <<"wpool">>, <<"push">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{workers, V}]) ->
    validate_positive_integer(V);
validate([<<"name">>, <<"chat_msg_exchange">>, <<"rabbit">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{name, V}]) ->
    validate_non_empty_binary(V);
validate([<<"type">>, <<"chat_msg_exchange">>, <<"rabbit">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{type, V}]) ->
    validate_non_empty_binary(V);
validate([<<"recv_topic">>, <<"chat_msg_exchange">>, <<"rabbit">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{recv_topic, V}]) ->
    validate_non_empty_binary(V);
validate([<<"sent_topic">>, <<"chat_msg_exchange">>, <<"rabbit">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{sent_topic, V}]) ->
    validate_non_empty_binary(V);
validate([<<"name">>, <<"groupchat_msg_exchange">>, <<"rabbit">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{name, V}]) ->
    validate_non_empty_binary(V);
validate([<<"type">>, <<"groupchat_msg_exchange">>, <<"rabbit">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{type, V}]) ->
    validate_non_empty_binary(V);
validate([<<"recv_topic">>, <<"groupchat_msg_exchange">>, <<"rabbit">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{recv_topic, V}]) ->
    validate_non_empty_binary(V);
validate([<<"sent_topic">>, <<"groupchat_msg_exchange">>, <<"rabbit">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{sent_topic, V}]) ->
    validate_non_empty_binary(V);
validate([<<"name">>, <<"presence_exchange">>, <<"rabbit">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{name, V}]) ->
    validate_non_empty_binary(V);
validate([<<"type">>, <<"presence_exchange">>, <<"rabbit">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{type, V}]) ->
    validate_non_empty_binary(V);
validate([<<"access_key_id">>, <<"sns">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{access_key_id, V}]) ->
    validate_string(V);
validate([<<"account_id">>, <<"sns">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{account_id, V}]) ->
    validate_string(V);
validate([<<"muc_host">>, <<"sns">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{muc_host, V}]) ->
    validate_domain_template(V);
validate([<<"muc_messages_topic">>, <<"sns">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{muc_messages_topic, V}]) ->
    validate_string(V);
validate([<<"plugin_module">>, <<"sns">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{plugin_module, V}]) ->
    validate_module(V);
validate([<<"pm_messages_topic">>, <<"sns">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{pm_messages_topic, V}]) ->
    validate_string(V);
validate([<<"pool_size">>, <<"sns">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{pool_size, V}]) ->
    validate_non_negative_integer(V);
validate([<<"presence_updates_topic">>, <<"sns">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{presence_updates_topic, V}]) ->
    validate_string(V);
validate([<<"publish_retry_count">>, <<"sns">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{publish_retry_count, V}]) ->
    validate_non_negative_integer(V);
validate([<<"publish_retry_time_ms">>, <<"sns">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{publish_retry_time_ms, V}]) ->
    validate_non_negative_integer(V);
validate([<<"region">>, <<"sns">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{region, V}]) ->
    validate_string(V);
validate([<<"secret_access_key">>, <<"sns">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{secret_access_key, V}]) ->
    validate_string(V);
validate([<<"sns_host">>, <<"sns">>, <<"backend">>,
          <<"mod_event_pusher">>, <<"modules">>|_],
         [{sns_host, V}]) ->
    validate_string(V);
validate([<<"enabled">>, <<"bounce">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{enabled, true}]) ->
    ok;
validate([<<"bounce">>, <<"mod_global_distrib">>, <<"modules">>|_],
         [{bounce, false}]) ->
    ok;
validate([<<"max_retries">>, <<"bounce">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{max_retries, V}]) ->
    validate_non_negative_integer(V);
validate([<<"resend_after_ms">>, <<"bounce">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{resend_after_ms, V}]) ->
    validate_non_negative_integer(V);
validate([<<"cache_missed">>, <<"cache">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{cache_missed, V}]) ->
    validate_boolean(V);
validate([<<"domain_lifetime_seconds">>, <<"cache">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{domain_lifetime_seconds, V}]) ->
    validate_non_negative_integer(V);
validate([<<"jid_lifetime_seconds">>, <<"cache">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{jid_lifetime_seconds, V}]) ->
    validate_non_negative_integer(V);
validate([<<"max_jids">>, <<"cache">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{max_jids, V}]) ->
    validate_non_negative_integer(V);
validate([<<"advertised_endpoints">>, <<"connections">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [false]) ->
   ok;
validate([<<"host">>, item, <<"advertised_endpoints">>, <<"connections">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [V]) ->
    validate_network_address(V);
validate([<<"port">>, item, <<"advertised_endpoints">>, <<"connections">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [V]) ->
    validate_network_port(V);
validate([<<"connections_per_endpoint">>, <<"connections">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{connections_per_endpoint, V}]) ->
    validate_non_negative_integer(V);
validate([<<"disabled_gc_interval">>, <<"connections">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{disabled_gc_interval, V}]) ->
    validate_non_negative_integer(V);
validate([<<"endpoint_refresh_interval">>, <<"connections">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{endpoint_refresh_interval, V}]) ->
    validate_positive_integer(V);
validate([<<"endpoint_refresh_interval_when_empty">>, <<"connections">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{endpoint_refresh_interval_when_empty, V}]) ->
    validate_positive_integer(V);
validate([<<"host">>, item, <<"endpoints">>, <<"connections">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [V]) ->
    validate_network_address(V);
validate([<<"port">>, item, <<"endpoints">>, <<"connections">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [V]) ->
    validate_network_port(V);
validate([<<"tls">>, <<"connections">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{tls_opts, false}]) ->
    ok;
validate([<<"enabled">>, <<"tls">>, <<"connections">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{enabled, true}]) ->
    ok;
validate([<<"cacertfile">>, <<"tls">>, <<"connections">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{cafile, V}]) ->
    validate_filename(V);
validate([<<"certfile">>, <<"tls">>, <<"connections">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{certfile, V}]) ->
    validate_filename(V);
validate([<<"ciphers">>, <<"tls">>, <<"connections">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{ciphers, V}]) ->
    validate_string(V);
validate([<<"dhfile">>, <<"tls">>, <<"connections">>,
          <<"mod_global_distrib">>, <<"modules">>|_],
         [{dhfile, V}]) ->
    validate_filename(V);
validate([<<"global_host">>, <<"mod_global_distrib">>, <<"modules">>|_],
         [{global_host, V}]) ->
    validate_domain(V);
validate([<<"hosts_refresh_interval">>, <<"mod_global_distrib">>, <<"modules">>|_],
         [{hosts_refresh_interval, V}]) ->
    validate_non_negative_integer(V);
validate([<<"local_host">>, <<"mod_global_distrib">>, <<"modules">>|_],
         [{local_host, V}]) ->
    validate_domain(V);
validate([<<"message_ttl">>, <<"mod_global_distrib">>, <<"modules">>|_],
         [{message_ttl, V}]) ->
    validate_non_negative_integer(V);
validate([<<"expire_after">>, <<"redis">>, <<"mod_global_distrib">>, <<"modules">>|_],
         [{expire_after, V}]) ->
    validate_non_negative_integer(V);
validate([<<"pool">>, <<"redis">>, <<"mod_global_distrib">>, <<"modules">>|_],
         [{pool, V}]) ->
    validate_pool_name(V);
validate([<<"refresh_after">>, <<"redis">>, <<"mod_global_distrib">>, <<"modules">>|_],
         [{refresh_after, V}]) ->
    validate_non_negative_integer(V);
validate([<<"ack_freq">>, <<"mod_stream_management">>, <<"modules">>|_],
         [{ack_freq, V}]) ->
    validate_positive_integer_or_atom(V, never);
validate([<<"buffer_max">>, <<"mod_stream_management">>, <<"modules">>|_],
         [{buffer_max, V}]) ->
    validate_positive_integer_or_infinity_or_atom(V, no_buffer);
validate([<<"resume_timeout">>, <<"mod_stream_management">>, <<"modules">>|_],
         [{resume_timeout, V}]) ->
    validate_positive_integer(V);
validate([<<"enabled">>, <<"stale_h">>, <<"mod_stream_management">>, <<"modules">>|_],
         [{enabled, V}]) ->
    validate_boolean(V);
validate([<<"geriatric">>, <<"stale_h">>, <<"mod_stream_management">>, <<"modules">>|_],
         [{stale_h_geriatric, V}]) ->
    validate_positive_integer(V);
validate([<<"repeat_after">>, <<"stale_h">>, <<"mod_stream_management">>, <<"modules">>|_],
         [{stale_h_repeat_after, V}]) ->
    validate_positive_integer(V);
validate([<<"ldap_auth_check">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_auth_check, V}]) ->
    validate_boolean(V);
validate([<<"ldap_base">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_base, V}]) ->
    validate_string(V);
validate([<<"ldap_deref">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_deref, V}]) ->
    validate_enum(V, [never,always,finding,searching]);
validate([<<"ldap_filter">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_filter, V}]) ->
    validate_string(V);
validate([<<"ldap_gfilter">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_gfilter, V}]) ->
    validate_string(V);
validate([<<"ldap_group_cache_size">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_group_cache_size, V}]) ->
    validate_non_negative_integer(V);
validate([<<"ldap_group_cache_validity">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_group_cache_validity, V}]) ->
    validate_non_negative_integer(V);
validate([<<"ldap_groupattr">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_groupattr, V}]) ->
    validate_string(V);
validate([<<"ldap_groupdesc">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_groupdesc, V}]) ->
    validate_string(V);
validate([<<"ldap_memberattr">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_memberattr, V}]) ->
    validate_string(V);
validate([<<"ldap_memberattr_format">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_memberattr_format, V}]) ->
    validate_string(V);
validate([<<"ldap_memberattr_format_re">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_memberattr_format_re, V}]) ->
    validate_string(V);
validate([<<"ldap_pool_tag">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_pool_tag, V}]) ->
    validate_pool_name(V);
validate([<<"ldap_rfilter">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_rfilter, V}]) ->
    validate_string(V);
validate([<<"ldap_ufilter">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_ufilter, V}]) ->
    validate_string(V);
validate([<<"ldap_user_cache_size">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_user_cache_size, V}]) ->
    validate_non_negative_integer(V);
validate([<<"ldap_user_cache_validity">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_user_cache_validity, V}]) ->
    validate_non_negative_integer(V);
validate([<<"ldap_userdesc">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_userdesc, V}]) ->
    validate_string(V);
validate([<<"ldap_useruid">>, <<"mod_shared_roster_ldap">>, <<"modules">>|_],
         [{ldap_useruid, V}]) ->
    validate_string(V);
validate([<<"iqdisc">>, <<"mod_version">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
validate([<<"os_info">>, <<"mod_version">>, <<"modules">>|_],
         [{os_info, V}]) ->
    validate_boolean(V);
validate([<<"access">>, <<"mod_register">>, <<"modules">>|_],
         [{access, V}]) ->
    validate_non_empty_atom(V);
validate([item, <<"ip_access">>, <<"mod_register">>, <<"modules">>|_],
         [V]) ->
    validate_ip_access(V);
validate([<<"iqdisc">>, <<"mod_register">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
validate([<<"password_strength">>, <<"mod_register">>, <<"modules">>|_],
         [{password_strength, V}]) ->
    validate_non_negative_integer(V);
validate([item, <<"registration_watchers">>, <<"mod_register">>, <<"modules">>|_],
         [V]) ->
    validate_jid(V);
validate([<<"body">>, <<"welcome_message">>, <<"mod_register">>, <<"modules">>|_],
         [{body, V}]) ->
    validate_string(V);
validate([<<"subject">>, <<"welcome_message">>, <<"mod_register">>, <<"modules">>|_],
         [{subject, V}]) ->
    validate_string(V);
validate([<<"iqdisc">>, <<"mod_adhoc">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
validate([<<"report_commands_node">>, <<"mod_adhoc">>, <<"modules">>|_],
         [{report_commands_node, V}]) ->
    validate_boolean(V);
validate([<<"cache_life_time">>, <<"mod_caps">>, <<"modules">>|_],
         [{cache_life_time, V}]) ->
    validate_non_negative_integer_or_infinity(V);
validate([<<"cache_size">>, <<"mod_caps">>, <<"modules">>|_],
         [{cache_size, V}]) ->
    validate_non_negative_integer(V);
validate([<<"type">>, _, <<"service">>, <<"mod_extdisco">>, <<"modules">>|_],
         [{type, V}]) ->
    validate_non_empty_atom(V);
validate([<<"host">>, _,<<"service">>, <<"mod_extdisco">>, <<"modules">>|_],
         [{host, V}]) ->
    validate_non_empty_list(V);
validate([<<"port">>, _,<<"service">>, <<"mod_extdisco">>, <<"modules">>|_],
         [{port, V}]) ->
    validate_port(V);
validate([<<"transport">>,_, <<"service">>, <<"mod_extdisco">>, <<"modules">>|_],
         [{transport, V}]) ->
    validate_non_empty_list(V);
validate([<<"username">>, _,<<"service">>, <<"mod_extdisco">>, <<"modules">>|_],
         [{username, V}]) ->
    validate_non_empty_list(V);
validate([<<"password">>, _,<<"service">>, <<"mod_extdisco">>, <<"modules">>|_],
         [{password, V}]) ->
    validate_non_empty_list(V);
validate([<<"backend">>, <<"mod_http_upload">>, <<"modules">>|_],
         [{backend, V}]) ->
    validate_backend(mod_http_upload, V);
validate([<<"expiration_time">>, <<"mod_http_upload">>, <<"modules">>|_],
         [{expiration_time, V}]) ->
    validate_non_negative_integer(V);
validate([<<"host">>, <<"mod_http_upload">>, <<"modules">>|_],
         [{host, V}]) ->
    validate_domain_template(V);
validate([<<"iqdisc">>, <<"mod_http_upload">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
validate([<<"max_file_size">>, <<"mod_http_upload">>, <<"modules">>|_],
         [{max_file_size, V}]) ->
    validate_non_negative_integer(V);
validate([<<"access_key_id">>, <<"s3">>, <<"mod_http_upload">>, <<"modules">>|_],
         [{access_key_id, V}]) ->
    validate_string(V);
validate([<<"add_acl">>, <<"s3">>, <<"mod_http_upload">>, <<"modules">>|_],
         [{add_acl, V}]) ->
    validate_boolean(V);
validate([<<"bucket_url">>, <<"s3">>, <<"mod_http_upload">>, <<"modules">>|_],
         [{bucket_url, V}]) ->
    validate_url(V);
validate([<<"region">>, <<"s3">>, <<"mod_http_upload">>, <<"modules">>|_],
         [{region, V}]) ->
    validate_string(V);
validate([<<"secret_access_key">>, <<"s3">>, <<"mod_http_upload">>, <<"modules">>|_],
         [{secret_access_key, V}]) ->
    validate_string(V);
validate([<<"token_bytes">>, <<"mod_http_upload">>, <<"modules">>|_],
         [{token_bytes, V}]) ->
    validate_positive_integer(V);
validate([<<"api_version">>, <<"mod_push_service_mongoosepush">>, <<"modules">>|_],
         [{api_version, V}]) ->
    validate_string(V);
validate([<<"max_http_connections">>, <<"mod_push_service_mongoosepush">>, <<"modules">>|_],
         [{max_http_connections, V}]) ->
    validate_non_negative_integer(V);
validate([<<"pool_name">>, <<"mod_push_service_mongoosepush">>, <<"modules">>|_],
         [{pool_name, V}]) ->
    validate_pool_name(V);
validate([<<"backend">>, <<"mod_last">>, <<"modules">>|_],
         [{backend, V}]) ->
    validate_backend(mod_last, V);
validate([<<"iqdisc">>, <<"mod_last">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
validate([<<"bucket_type">>, <<"riak">>, <<"mod_last">>, <<"modules">>|_],
         [{bucket_type, V}]) ->
    validate_non_empty_binary(V);
validate([<<"iqdisc">>, <<"mod_time">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
validate([item, <<"routes">>, <<"mod_revproxy">>, <<"modules">>|_],
         [V]) ->
    validate_revproxy_route(V);
validate([<<"backend">>, <<"mod_privacy">>, <<"modules">>|_],
         [{backend, V}]) ->
    validate_backend(mod_privacy, V);
validate([<<"bucket_type">>, <<"riak">>, <<"mod_privacy">>, <<"modules">>|_],
         [{bucket_type, V}]) ->
    validate_non_empty_binary(V);
validate([<<"defaults_bucket_type">>, <<"riak">>, <<"mod_privacy">>, <<"modules">>|_],
         [{defaults_bucket_type, V}]) ->
    validate_non_empty_binary(V);
validate([<<"names_bucket_type">>, <<"riak">>, <<"mod_privacy">>, <<"modules">>|_],
         [{names_bucket_type, V}]) ->
    validate_non_empty_binary(V);
validate([<<"archive_chat_markers">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{archive_chat_markers, V}]) ->
    validate_boolean(V);
validate([<<"archive_groupchats">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{archive_groupchats, V}]) ->
    validate_boolean(V);
validate([<<"async_writer">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{async_writer, V}]) ->
    validate_boolean(V);
validate([<<"async_writer_rdbms_pool">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{async_writer_rdbms_pool, V}]) ->
    validate_non_empty_atom(V);
validate([<<"backend">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{backend, V}]) ->
    validate_enum(V, [rdbms,riak,cassandra,elasticsearch]);
validate([<<"cache_users">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{cache_users, V}]) ->
    validate_boolean(V);
validate([<<"db_jid_format">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{db_jid_format, V}]) ->
    validate_module(V);
validate([<<"db_message_format">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{db_message_format, V}]) ->
    validate_module(V);
validate([<<"default_result_limit">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{default_result_limit, V}]) ->
    validate_non_negative_integer(V);
validate([<<"extra_lookup_params">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{extra_lookup_params, V}]) ->
    validate_module(V);
validate([<<"flush_interval">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{flush_interval, V}]) ->
    validate_non_negative_integer(V);
validate([<<"full_text_search">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{full_text_search, V}]) ->
    validate_boolean(V);
validate([<<"host">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{host, V}]) ->
    validate_domain_template(V);
validate([<<"is_archivable_message">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{is_archivable_message, V}]) ->
    validate_module(V);
validate([<<"max_batch_size">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{max_batch_size, V}]) ->
    validate_non_negative_integer(V);
validate([<<"max_result_limit">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{max_result_limit, V}]) ->
    validate_non_negative_integer(V);
validate([<<"message_retraction">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{message_retraction, V}]) ->
    validate_boolean(V);
validate([<<"archive_chat_markers">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{archive_chat_markers, V}]) ->
    validate_boolean(V);
validate([<<"archive_groupchats">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{archive_groupchats, V}]) ->
    validate_boolean(V);
validate([<<"async_writer">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{async_writer, V}]) ->
    validate_boolean(V);
validate([<<"async_writer_rdbms_pool">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{async_writer_rdbms_pool, V}]) ->
    validate_non_empty_atom(V);
validate([<<"backend">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{backend, V}]) ->
    validate_enum(V, [rdbms,riak,cassandra,elasticsearch]);
validate([<<"cache_users">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{cache_users, V}]) ->
    validate_boolean(V);
validate([<<"db_jid_format">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{db_jid_format, V}]) ->
    validate_module(V);
validate([<<"db_message_format">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{db_message_format, V}]) ->
    validate_module(V);
validate([<<"default_result_limit">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{default_result_limit, V}]) ->
    validate_non_negative_integer(V);
validate([<<"extra_lookup_params">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{extra_lookup_params, V}]) ->
    validate_module(V);
validate([<<"flush_interval">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{flush_interval, V}]) ->
    validate_non_negative_integer(V);
validate([<<"full_text_search">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{full_text_search, V}]) ->
    validate_boolean(V);
validate([<<"host">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{host, V}]) ->
    validate_domain_template(V);
validate([<<"is_archivable_message">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{is_archivable_message, V}]) ->
    validate_module(V);
validate([<<"max_batch_size">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{max_batch_size, V}]) ->
    validate_non_negative_integer(V);
validate([<<"max_result_limit">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{max_result_limit, V}]) ->
    validate_non_negative_integer(V);
validate([<<"message_retraction">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{message_retraction, V}]) ->
    validate_boolean(V);
validate([<<"no_stanzaid_element">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{no_stanzaid_element, V}]) ->
    validate_boolean(V);
validate([<<"rdbms_message_format">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{rdbms_message_format, V}]) ->
    validate_enum(V, [simple,internal]);
validate([<<"simple">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{simple, V}]) ->
    validate_boolean(V);
validate([<<"user_prefs_store">>, <<"muc">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{user_prefs_store, V}]) ->
    validate_enum(V, [false,rdbms,cassandra,mnesia]);
validate([<<"no_stanzaid_element">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{no_stanzaid_element, V}]) ->
    validate_boolean(V);
validate([<<"archive_chat_markers">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{archive_chat_markers, V}]) ->
    validate_boolean(V);
validate([<<"archive_groupchats">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{archive_groupchats, V}]) ->
    validate_boolean(V);
validate([<<"async_writer">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{async_writer, V}]) ->
    validate_boolean(V);
validate([<<"async_writer_rdbms_pool">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{async_writer_rdbms_pool, V}]) ->
    validate_non_empty_atom(V);
validate([<<"backend">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{backend, V}]) ->
    validate_enum(V, [rdbms,riak,cassandra,elasticsearch]);
validate([<<"cache_users">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{cache_users, V}]) ->
    validate_boolean(V);
validate([<<"db_jid_format">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{db_jid_format, V}]) ->
    validate_module(V);
validate([<<"db_message_format">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{db_message_format, V}]) ->
    validate_module(V);
validate([<<"default_result_limit">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{default_result_limit, V}]) ->
    validate_non_negative_integer(V);
validate([<<"extra_lookup_params">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{extra_lookup_params, V}]) ->
    validate_module(V);
validate([<<"flush_interval">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{flush_interval, V}]) ->
    validate_non_negative_integer(V);
validate([<<"full_text_search">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{full_text_search, V}]) ->
    validate_boolean(V);
validate([<<"host">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{host, V}]) ->
    validate_domain_template(V);
validate([<<"is_archivable_message">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{is_archivable_message, V}]) ->
    validate_module(V);
validate([<<"max_batch_size">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{max_batch_size, V}]) ->
    validate_non_negative_integer(V);
validate([<<"max_result_limit">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{max_result_limit, V}]) ->
    validate_non_negative_integer(V);
validate([<<"message_retraction">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{message_retraction, V}]) ->
    validate_boolean(V);
validate([<<"no_stanzaid_element">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{no_stanzaid_element, V}]) ->
    validate_boolean(V);
validate([<<"rdbms_message_format">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{rdbms_message_format, V}]) ->
    validate_enum(V, [simple,internal]);
validate([<<"simple">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{simple, V}]) ->
    validate_boolean(V);
validate([<<"user_prefs_store">>, <<"pm">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{user_prefs_store, V}]) ->
    validate_enum(V, [false,rdbms,cassandra,mnesia]);
validate([<<"rdbms_message_format">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{rdbms_message_format, V}]) ->
    validate_enum(V, [simple,internal]);
validate([<<"bucket_type">>, <<"riak">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{bucket_type, V}]) ->
    validate_non_empty_binary(V);
validate([<<"search_index">>, <<"riak">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{search_index, V}]) ->
    validate_non_empty_binary(V);
validate([<<"simple">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{simple, V}]) ->
    validate_boolean(V);
validate([<<"user_prefs_store">>, <<"mod_mam_meta">>, <<"modules">>|_],
         [{user_prefs_store, V}]) ->
    validate_enum(V, [false,rdbms,cassandra,mnesia]);
validate([<<"iqdisc">>, <<"mod_auth_token">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
validate([<<"validity_period">>,<<"mod_auth_token">>,<<"modules">>|_], Vs) ->
    lists:foreach(fun validate_validity_period/1, Vs);
validate([<<"listen_port">>, <<"mod_jingle_sip">>, <<"modules">>|_],
         [{listen_port, V}]) ->
    validate_network_port(V);
validate([<<"local_host">>, <<"mod_jingle_sip">>, <<"modules">>|_],
         [{local_host, V}]) ->
    validate_network_address(V);
validate([<<"proxy_host">>, <<"mod_jingle_sip">>, <<"modules">>|_],
         [{proxy_host, V}]) ->
    validate_network_address(V);
validate([<<"proxy_port">>, <<"mod_jingle_sip">>, <<"modules">>|_],
         [{proxy_port, V}]) ->
    validate_network_port(V);
validate([<<"sdp_origin">>, <<"mod_jingle_sip">>, <<"modules">>|_],
         [{sdp_origin, V}]) ->
    validate_ip_address(V);
validate([<<"buffer_max">>, <<"mod_csi">>, <<"modules">>|_],
         [{buffer_max, V}]) ->
    validate_non_negative_integer_or_infinity(V);
validate([<<"iqdisc">>, <<"mod_sic">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
validate([<<"backend">>, <<"mod_roster">>, <<"modules">>|_],
         [{backend, V}]) ->
    validate_backend(mod_roster, V);
validate([<<"iqdisc">>, <<"mod_roster">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
validate([<<"bucket_type">>, <<"riak">>, <<"mod_roster">>, <<"modules">>|_],
         [{bucket_type, V}]) ->
    validate_non_empty_binary(V);
validate([<<"version_bucket_type">>, <<"riak">>, <<"mod_roster">>, <<"modules">>|_],
         [{version_bucket_type, V}]) ->
    validate_non_empty_binary(V);
validate([<<"store_current_id">>, <<"mod_roster">>, <<"modules">>|_],
         [{store_current_id, V}]) ->
    validate_boolean(V);
validate([<<"versioning">>, <<"mod_roster">>, <<"modules">>|_],
         [{versioning, V}]) ->
    validate_boolean(V);
validate([item, <<"keys">>, <<"mod_keystore">>, <<"modules">>|_],
         [V]) ->
    validate_keystore_key(V);
validate([<<"ram_key_size">>, <<"mod_keystore">>, <<"modules">>|_],
         [{ram_key_size, V}]) ->
    validate_non_negative_integer(V);
validate([<<"backend">>, <<"mod_vcard">>, <<"modules">>|_],
         [{backend, V}]) ->
    validate_backend(mod_vcard, V);
validate([<<"host">>, <<"mod_vcard">>, <<"modules">>|_],
         [{host, V}]) ->
    validate_domain_template(V);
validate([<<"iqdisc">>, <<"mod_vcard">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
validate([<<"ldap_base">>, <<"mod_vcard">>, <<"modules">>|_],
         [{ldap_base, V}]) ->
    validate_string(V);
validate([item, <<"ldap_binary_search_fields">>, <<"mod_vcard">>, <<"modules">>|_],
         [V]) ->
    validate_non_empty_binary(V);
validate([<<"ldap_deref">>, <<"mod_vcard">>, <<"modules">>|_],
         [{ldap_deref, V}]) ->
    validate_enum(V, [never,always,finding,searching]);
validate([<<"ldap_filter">>, <<"mod_vcard">>, <<"modules">>|_],
         [{ldap_filter, V}]) ->
    validate_string(V);
validate([<<"ldap_pool_tag">>, <<"mod_vcard">>, <<"modules">>|_],
         [{ldap_pool_tag, V}]) ->
    validate_pool_name(V);
validate([item, <<"ldap_search_fields">>, <<"mod_vcard">>, <<"modules">>|_],
         [V]) ->
    validate_ldap_search_field(V);
validate([<<"ldap_search_operator">>, <<"mod_vcard">>, <<"modules">>|_],
         [{ldap_search_operator, V}]) ->
    validate_enum(V, ['or','and']);
validate([item, <<"ldap_search_reported">>, <<"mod_vcard">>, <<"modules">>|_],
         [V]) ->
    validate_ldap_search_reported(V);
validate([item, <<"ldap_uids">>, <<"mod_vcard">>, <<"modules">>|_],
         [V]) ->
    validate_ldap_uids(V);
validate([item, <<"ldap_vcard_map">>, <<"mod_vcard">>, <<"modules">>|_],
         [V]) ->
    validate_ldap_vcard_map(V);
validate([<<"matches">>, <<"mod_vcard">>, <<"modules">>|_],
         [{matches, V}]) ->
    validate_non_negative_integer_or_infinity(V);
validate([<<"bucket_type">>, <<"riak">>, <<"mod_vcard">>, <<"modules">>|_],
         [{bucket_type, V}]) ->
    validate_non_empty_binary(V);
validate([<<"search_index">>, <<"riak">>, <<"mod_vcard">>, <<"modules">>|_],
         [{search_index, V}]) ->
    validate_non_empty_binary(V);
validate([<<"search">>, <<"mod_vcard">>, <<"modules">>|_],
         [{search, V}]) ->
    validate_boolean(V);
validate([<<"backend">>, <<"mod_private">>, <<"modules">>|_],
         [{backend, V}]) ->
    validate_backend(mod_private, V);
validate([<<"iqdisc">>, <<"mod_private">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
validate([<<"bucket_type">>, <<"riak">>, <<"mod_private">>, <<"modules">>|_],
         [{bucket_type, V}]) ->
    validate_non_empty_binary(V);
validate([<<"inactivity">>, <<"mod_bosh">>, <<"modules">>|_],
         [{inactivity, V}]) ->
    validate_non_negative_integer_or_infinity(V);
validate([<<"max_wait">>, <<"mod_bosh">>, <<"modules">>|_],
         [{max_wait, V}]) ->
    validate_non_negative_integer_or_infinity(V);
validate([<<"server_acks">>, <<"mod_bosh">>, <<"modules">>|_],
         [{server_acks, V}]) ->
    validate_boolean(V);
validate([<<"aff_changes">>, <<"mod_inbox">>, <<"modules">>|_],
         [{aff_changes, V}]) ->
    validate_boolean(V);
validate([item, <<"groupchat">>, <<"mod_inbox">>, <<"modules">>|_],
         [V]) ->
    validate_groupchat_type(V);
validate([<<"iqdisc">>, <<"mod_inbox">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
validate([<<"remove_on_kicked">>, <<"mod_inbox">>, <<"modules">>|_],
         [{remove_on_kicked, V}]) ->
    validate_boolean(V);
validate([item, <<"reset_markers">>, <<"mod_inbox">>, <<"modules">>|_],
         [V]) ->
    validate_chat_marker_type(V);
validate([item, <<"extra_domains">>, <<"mod_disco">>, <<"modules">>|_],
         [V]) ->
    validate_binary_domain(V);
validate([item, <<"module">>, item, <<"server_info">>, <<"mod_disco">>, <<"modules">>|_],
         [V]) ->
    validate_module(V);
validate([<<"name">>, item, <<"server_info">>, <<"mod_disco">>, <<"modules">>|_],
         [V]) ->
    validate_non_empty_binary(V);
validate([item, <<"urls">>, item, <<"server_info">>, <<"mod_disco">>, <<"modules">>|_],
         [V]) ->
    validate_url(V);
validate([item, <<"urls">>, <<"mod_disco">>, <<"modules">>|_],
         [V]) ->
    validate_url(V);
validate([<<"users_can_see_hidden_services">>, <<"mod_disco">>, <<"modules">>|_],
         [{users_can_see_hidden_services, V}]) ->
    validate_boolean(V);
validate([<<"all_can_configure">>, <<"mod_muc_light">>, <<"modules">>|_],
         [{all_can_configure, V}]) ->
    validate_boolean(V);
validate([<<"all_can_invite">>, <<"mod_muc_light">>, <<"modules">>|_],
         [{all_can_invite, V}]) ->
    validate_boolean(V);
validate([<<"backend">>, <<"mod_muc_light">>, <<"modules">>|_],
         [{backend, V}]) ->
    validate_backend(mod_muc_light_db, V);
validate([<<"blocking">>, <<"mod_muc_light">>, <<"modules">>|_],
         [{blocking, V}]) ->
    validate_boolean(V);
validate([item, <<"config_schema">>, <<"mod_muc_light">>, <<"modules">>|_],
         [V]) ->
    validate_muc_config_schema(V);
validate([<<"equal_occupants">>, <<"mod_muc_light">>, <<"modules">>|_],
         [{equal_occupants, V}]) ->
    validate_boolean(V);
validate([<<"host">>, <<"mod_muc_light">>, <<"modules">>|_],
         [{host, V}]) ->
    validate_domain_template(V);
validate([<<"legacy_mode">>, <<"mod_muc_light">>, <<"modules">>|_],
         [{legacy_mode, V}]) ->
    validate_boolean(V);
validate([<<"max_occupants">>, <<"mod_muc_light">>, <<"modules">>|_],
         [{max_occupants, V}]) ->
    validate_positive_integer_or_infinity(V);
validate([<<"rooms_in_rosters">>, <<"mod_muc_light">>, <<"modules">>|_],
         [{rooms_in_rosters, V}]) ->
    validate_boolean(V);
validate([<<"rooms_per_page">>, <<"mod_muc_light">>, <<"modules">>|_],
         [{rooms_per_page, V}]) ->
    validate_positive_integer_or_infinity(V);
validate([<<"iqdisc">>, <<"mod_carboncopy">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
validate([<<"access_max_user_messages">>, <<"mod_offline">>, <<"modules">>|_],
         [{access_max_user_messages, V}]) ->
    validate_access_rule(V);
validate([<<"backend">>, <<"mod_offline">>, <<"modules">>|_],
         [{backend, V}]) ->
    validate_backend(mod_offline, V);
validate([<<"bucket_type">>, <<"riak">>, <<"mod_offline">>, <<"modules">>|_],
         [{bucket_type, V}]) ->
    validate_non_empty_binary(V);
validate([<<"access_log">>, <<"mod_muc_log">>, <<"modules">>|_],
         [{access_log, V}]) ->
    validate_access_rule(V);
validate([<<"css_file">>, <<"mod_muc_log">>, <<"modules">>|_],
         [{cssfile, V}]) ->
    validate_maybe_css_file(V);
validate([<<"dirname">>, <<"mod_muc_log">>, <<"modules">>|_],
         [{dirname, V}]) ->
    validate_enum(V, [room_jid,room_name]);
validate([<<"dirtype">>, <<"mod_muc_log">>, <<"modules">>|_],
         [{dirtype, V}]) ->
    validate_enum(V, [subdirs,plain]);
validate([<<"file_format">>, <<"mod_muc_log">>, <<"modules">>|_],
         [{file_format, V}]) ->
    validate_enum(V, [html,plaintext]);
validate([<<"outdir">>, <<"mod_muc_log">>, <<"modules">>|_],
         [{outdir, V}]) ->
    validate_dirname(V);
validate([<<"spam_prevention">>, <<"mod_muc_log">>, <<"modules">>|_],
         [{spam_prevention, V}]) ->
    validate_boolean(V);
validate([<<"timezone">>, <<"mod_muc_log">>, <<"modules">>|_],
         [{timezone, V}]) ->
    validate_enum(V, [local,universal]);
validate([<<"top_link">>, <<"mod_muc_log">>, <<"modules">>|_],
         [{top_link, V}]) ->
    validate_top_link(V);
validate([<<"access_createnode">>, <<"mod_pubsub">>, <<"modules">>|_],
         [{access_createnode, V}]) ->
    validate_access_rule(V);
validate([<<"backend">>, <<"mod_pubsub">>, <<"modules">>|_],
         [{backend, V}]) ->
    validate_backend(mod_pubsub_db, V);
validate([<<"access_model">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [{access_model, V}]) ->
    validate_non_empty_atom(V);
validate([<<"deliver_notifications">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [{deliver_notifications, V}]) ->
    validate_boolean(V);
validate([<<"deliver_payloads">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [{deliver_payloads, V}]) ->
    validate_boolean(V);
validate([<<"max_items">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [{max_items, V}]) ->
    validate_non_negative_integer(V);
validate([<<"max_payload_size">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [{max_payload_size, V}]) ->
    validate_non_negative_integer(V);
validate([<<"node_type">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [{node_type, V}]) ->
    validate_non_empty_atom(V);
validate([<<"notification_type">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [{notification_type, V}]) ->
    validate_non_empty_atom(V);
validate([<<"notify_config">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [{notify_config, V}]) ->
    validate_boolean(V);
validate([<<"notify_delete">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [{notify_delete, V}]) ->
    validate_boolean(V);
validate([<<"notify_retract">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [{notify_retract, V}]) ->
    validate_boolean(V);
validate([<<"persist_items">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [{persist_items, V}]) ->
    validate_boolean(V);
validate([<<"presence_based_delivery">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [{presence_based_delivery, V}]) ->
    validate_boolean(V);
validate([<<"publish_model">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [{publish_model, V}]) ->
    validate_non_empty_atom(V);
validate([<<"purge_offline">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [{purge_offline, V}]) ->
    validate_boolean(V);
validate([item, <<"roster_groups_allowed">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [V]) ->
    validate_non_empty_binary(V);
validate([<<"send_last_published_item">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [{send_last_published_item, V}]) ->
    validate_non_empty_atom(V);
validate([<<"subscribe">>, <<"default_node_config">>,
          <<"mod_pubsub">>, <<"modules">>|_],
         [{subscribe, V}]) ->
    validate_boolean(V);
validate([<<"host">>, <<"mod_pubsub">>, <<"modules">>|_],
         [{host, V}]) ->
    validate_domain_template(V);
validate([<<"ignore_pep_from_offline">>, <<"mod_pubsub">>, <<"modules">>|_],
         [{ignore_pep_from_offline, V}]) ->
    validate_boolean(V);
validate([<<"iqdisc">>, <<"mod_pubsub">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
validate([<<"item_publisher">>, <<"mod_pubsub">>, <<"modules">>|_],
         [{item_publisher, V}]) ->
    validate_boolean(V);
validate([<<"last_item_cache">>, <<"mod_pubsub">>, <<"modules">>|_],
         [{last_item_cache, V}]) ->
    validate_enum(V, [mnesia,rdbms,false]);
validate([<<"max_items_node">>, <<"mod_pubsub">>, <<"modules">>|_],
         [{max_items_node, V}]) ->
    validate_non_negative_integer(V);
validate([<<"max_subscriptions_node">>, <<"mod_pubsub">>, <<"modules">>|_],
         [{max_subscriptions_node, V}]) ->
    validate_non_negative_integer(V);
validate([<<"nodetree">>, <<"mod_pubsub">>, <<"modules">>|_],
         [{nodetree, V}]) ->
    validate_pubsub_nodetree(V);
validate([item, <<"pep_mapping">>, <<"mod_pubsub">>, <<"modules">>|_],
         [V]) ->
    validate_pubsub_pep_mapping(V);
validate([item, <<"plugins">>, <<"mod_pubsub">>, <<"modules">>|_],
         [V]) ->
    validate_pubsub_plugin(V);
validate([<<"sync_broadcast">>, <<"mod_pubsub">>, <<"modules">>|_],
         [{sync_broadcast, V}]) ->
    validate_boolean(V);
validate([<<"access">>, <<"mod_muc">>, <<"modules">>|_],
         [{access, V}]) ->
    validate_access_rule(V);
validate([<<"access_admin">>, <<"mod_muc">>, <<"modules">>|_],
         [{access_admin, V}]) ->
    validate_access_rule(V);
validate([<<"access_create">>, <<"mod_muc">>, <<"modules">>|_],
         [{access_create, V}]) ->
    validate_access_rule(V);
validate([<<"access_persistent">>, <<"mod_muc">>, <<"modules">>|_],
         [{access_persistent, V}]) ->
    validate_access_rule(V);
validate([<<"backend">>, <<"mod_muc">>, <<"modules">>|_],
         [{backend, V}]) ->
    validate_backend(mod_muc_db, V);
validate([item, <<"affiliations">>, <<"default_room">>,
          <<"mod_muc">>, <<"modules">>|_],
         [V]) ->
    validate_muc_affiliation_rule(V);
validate([<<"allow_change_subj">>, <<"default_room">>,
          <<"mod_muc">>, <<"modules">>|_],
         [{allow_change_subj, V}]) ->
    validate_boolean(V);
validate([<<"allow_multiple_sessions">>, <<"default_room">>,
          <<"mod_muc">>, <<"modules">>|_],
         [{allow_multiple_sessions, V}]) ->
    validate_boolean(V);
validate([<<"allow_private_messages">>, <<"default_room">>,
          <<"mod_muc">>, <<"modules">>|_],
         [{allow_private_messages, V}]) ->
    validate_boolean(V);
validate([<<"allow_query_users">>, <<"default_room">>,
          <<"mod_muc">>, <<"modules">>|_],
         [{allow_query_users, V}]) ->
    validate_boolean(V);
validate([<<"allow_user_invites">>, <<"default_room">>,
          <<"mod_muc">>, <<"modules">>|_],
         [{allow_user_invites, V}]) ->
    validate_boolean(V);
validate([<<"allow_visitor_nickchange">>, <<"default_room">>,
          <<"mod_muc">>, <<"modules">>|_],
         [{allow_visitor_nickchange, V}]) ->
    validate_boolean(V);
validate([<<"allow_visitor_status">>, <<"default_room">>,
          <<"mod_muc">>, <<"modules">>|_],
         [{allow_visitor_status, V}]) ->
    validate_boolean(V);
validate([<<"anonymous">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>|_],
         [{anonymous, V}]) ->
    validate_boolean(V);
validate([<<"description">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>|_],
         [{description, V}]) ->
    validate_binary(V);
validate([<<"logging">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>|_],
         [{logging, V}]) ->
    validate_boolean(V);
validate([<<"max_users">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>|_],
         [{max_users, V}]) ->
    validate_positive_integer(V);
validate([item, <<"maygetmemberlist">>, <<"default_room">>,
          <<"mod_muc">>, <<"modules">>|_],
         [V]) ->
    validate_non_empty_atom(V);
validate([<<"members_by_default">>, <<"default_room">>,
          <<"mod_muc">>, <<"modules">>|_],
         [{members_by_default, V}]) ->
    validate_boolean(V);
validate([<<"members_only">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>|_],
         [{members_only, V}]) ->
    validate_boolean(V);
validate([<<"moderated">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>|_],
         [{moderated, V}]) ->
    validate_boolean(V);
validate([<<"password">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>|_],
         [{password, V}]) ->
    validate_string(V);
validate([<<"password_protected">>, <<"default_room">>,
          <<"mod_muc">>, <<"modules">>|_],
         [{password_protected, V}]) ->
    validate_boolean(V);
validate([<<"persistent">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>|_],
         [{persistent, V}]) ->
    validate_boolean(V);
validate([<<"public">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>|_],
         [{public, V}]) ->
    validate_boolean(V);
validate([<<"public_list">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>|_],
         [{public_list, V}]) ->
    validate_boolean(V);
validate([<<"subject">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>|_],
         [{subject, V}]) ->
    validate_string(V);
validate([<<"subject_author">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>|_],
         [{subject_author, V}]) ->
    validate_string(V);
validate([<<"title">>, <<"default_room">>, <<"mod_muc">>, <<"modules">>|_],
         [{title, V}]) ->
    validate_string(V);
validate([<<"hibernated_room_check_interval">>, <<"mod_muc">>, <<"modules">>|_],
         [{hibernated_room_check_interval, V}]) ->
    validate_non_negative_integer_or_infinity(V);
validate([<<"hibernated_room_timeout">>, <<"mod_muc">>, <<"modules">>|_],
         [{hibernated_room_timeout, V}]) ->
    validate_non_negative_integer_or_infinity(V);
validate([<<"history_size">>, <<"mod_muc">>, <<"modules">>|_],
         [{history_size, V}]) ->
    validate_non_negative_integer(V);
validate([<<"host">>, <<"mod_muc">>, <<"modules">>|_],
         [{host, V}]) ->
    validate_domain_template(V);
validate([<<"http_auth_pool">>, <<"mod_muc">>, <<"modules">>|_],
         [{http_auth_pool, V}]) ->
    validate_pool_name(V);
validate([<<"load_permanent_rooms_at_startup">>, <<"mod_muc">>, <<"modules">>|_],
         [{load_permanent_rooms_at_startup, V}]) ->
    validate_boolean(V);
validate([<<"max_room_desc">>, <<"mod_muc">>, <<"modules">>|_],
         [{max_room_desc, V}]) ->
    validate_non_negative_integer_or_infinity(V);
validate([<<"max_room_id">>, <<"mod_muc">>, <<"modules">>|_],
         [{max_room_id, V}]) ->
    validate_non_negative_integer_or_infinity(V);
validate([<<"max_room_name">>, <<"mod_muc">>, <<"modules">>|_],
         [{max_room_name, V}]) ->
    validate_non_negative_integer_or_infinity(V);
validate([<<"max_user_conferences">>, <<"mod_muc">>, <<"modules">>|_],
         [{max_user_conferences, V}]) ->
    validate_non_negative_integer(V);
validate([<<"max_users">>, <<"mod_muc">>, <<"modules">>|_],
         [{max_users, V}]) ->
    validate_positive_integer(V);
validate([<<"max_users_admin_threshold">>, <<"mod_muc">>, <<"modules">>|_],
         [{max_users_admin_threshold, V}]) ->
    validate_positive_integer(V);
validate([<<"min_message_interval">>, <<"mod_muc">>, <<"modules">>|_],
         [{min_message_interval, V}]) ->
    validate_non_negative_integer(V);
validate([<<"min_presence_interval">>, <<"mod_muc">>, <<"modules">>|_],
         [{min_presence_interval, V}]) ->
    validate_non_negative_integer(V);
validate([<<"room_shaper">>, <<"mod_muc">>, <<"modules">>|_],
         [{room_shaper, V}]) ->
    validate_shaper_name(V);
validate([<<"user_message_shaper">>, <<"mod_muc">>, <<"modules">>|_],
         [{user_message_shaper, V}]) ->
    validate_shaper_name(V);
validate([<<"user_presence_shaper">>, <<"mod_muc">>, <<"modules">>|_],
         [{user_presence_shaper, V}]) ->
    validate_shaper_name(V);
validate([<<"iqdisc">>, <<"mod_ping">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
validate([<<"ping_interval">>, <<"mod_ping">>, <<"modules">>|_],
         [{ping_interval, V}]) ->
    validate_positive_integer(V);
validate([<<"ping_req_timeout">>, <<"mod_ping">>, <<"modules">>|_],
         [{ping_req_timeout, V}]) ->
    validate_positive_integer(V);
validate([<<"send_pings">>, <<"mod_ping">>, <<"modules">>|_],
         [{send_pings, V}]) ->
    validate_boolean(V);
validate([<<"timeout_action">>, <<"mod_ping">>, <<"modules">>|_],
         [{timeout_action, V}]) ->
    validate_enum(V, [none,kill]);
validate(_Path, _Value) ->
    ok.

validate(V, boolean, any) -> validate_boolean(V);
validate(V, binary, domain) -> validate_binary_domain(V);
validate(V, binary, non_empty) -> validate_non_empty_binary(V);
validate(V, integer, non_negative) -> validate_non_negative_integer(V);
validate(V, integer, positive) -> validate_positive_integer(V);
validate(V, integer, port) -> validate_port(V);
validate(V, int_or_infinity, non_negative) -> validate_non_negative_integer_or_infinity(V);
validate(V, int_or_infinity, positive) -> validate_positive_integer_or_infinity(V);
validate(V, string, url) -> validate_url(V);
validate(V, string, ip_address) -> validate_ip_address(V);
validate(V, string, non_empty) -> validate_non_empty_string(V);
validate(V, atom, module) -> validate_module(V);
validate(V, atom, {module, Prefix}) ->
    validate_module(list_to_atom(atom_to_list(Prefix) ++ atom_to_list(V)));
validate(V, atom, loglevel) -> validate_loglevel(V);
validate(V, atom, non_empty) -> validate_non_empty_atom(V);
validate(V, _, {enum, Values}) -> validate_enum(V, Values);
validate(_V, _, any) -> ok.

validate_list([_|_], non_empty) -> ok;
validate_list(L = [_|_], unique_non_empty) ->
    validate_unique_items(L);
validate_list(L, any) when is_list(L) -> ok.

validate_section([_|_], non_empty) -> ok;
validate_section(L, any) when is_list(L) -> ok.

%% validators

validate_loglevel(Level) ->
    mongoose_logs:loglevel_keyword_to_number(Level).

validate_non_empty_binary(Value) when is_binary(Value), Value =/= <<>> -> ok.

validate_binary(Value) when is_binary(Value) -> ok.

validate_hosts(Hosts = [_|_]) ->
    validate_unique_items(Hosts).

validate_unique_items(Items) ->
    L = sets:size(sets:from_list(Items)),
    L = length(Items).

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

validate_positive_integer_or_infinity(Value) when is_integer(Value), Value > 0 -> ok;
validate_positive_integer_or_infinity(infinity) -> ok.

validate_positive_integer_or_atom(Value, Atom) when is_atom(Value), Value == Atom -> ok;
validate_positive_integer_or_atom(Value, _) when is_integer(Value), Value > 0 -> ok.

validate_positive_integer_or_infinity_or_atom(Value, _) when is_integer(Value), Value > 0 -> ok;
validate_positive_integer_or_infinity_or_atom(infinity, _) -> ok;
validate_positive_integer_or_infinity_or_atom(Value, Atom) when is_atom(Value), Value == Atom -> ok.

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

validate_ip_access({Access, IPMask}) ->
    validate_enum(Access, [allow, deny]),
    validate_ip_mask_string(IPMask).

validate_backend(Mod, Backend) ->
    validate_module(backend_module:backend_module(Mod, Backend)).

validate_chat_marker_type(Type) ->
    validate_enum(Type, [displayed, received, acknowledged]).

validate_groupchat_type(Type) ->
    validate_enum(Type, [muc, muclight]).

validate_domain(Domain) when is_list(Domain) ->
    #jid{luser = <<>>, lresource = <<>>} = jid:from_binary(list_to_binary(Domain)),
    validate_domain_res(Domain).

validate_domain_res(Domain) ->
    case inet_res:gethostbyname(Domain) of
        {ok, _} ->
            ok;
        {error,formerr} ->
            error(#{what => cfg_validate_domain_failed,
                    reason => formerr, text => <<"Invalid domain name">>,
                    domain => Domain});
        {error,Reason} -> %% timeout, nxdomain
            ?LOG_WARNING(#{what => cfg_validate_domain,
                           reason => Reason, domain => Domain,
                           text => <<"Couldn't resolve domain. "
                  "It could cause issues with production installations">>}),
            ignore
    end.

validate_binary_domain(Domain) when is_binary(Domain) ->
    #jid{luser = <<>>, lresource = <<>>} = jid:from_binary(Domain),
    validate_domain_res(binary_to_list(Domain)).

validate_domain_template(Domain) ->
    validate_binary_domain(gen_mod:make_subhost(Domain, <<"example.com">>)).

validate_url(Url) ->
    validate_non_empty_string(Url).

validate_string(Value) ->
    is_binary(unicode:characters_to_binary(Value)).

validate_ip_mask_string(IPMaskString) ->
    validate_non_empty_string(IPMaskString),
    {ok, IPMask} = mongoose_lib:parse_ip_netmask(IPMaskString),
    validate_ip_mask(IPMask).

validate_ip_mask({IP, Mask}) ->
    validate_string(inet:ntoa(IP)),
    case IP of
        {_,_,_,_} ->
            validate_ipv4_mask(Mask);
        _ ->
            validate_ipv6_mask(Mask)
    end.

validate_ipv4_mask(Mask) ->
    validate_range(Mask, 0, 32).

validate_ipv6_mask(Mask) ->
    validate_range(Mask, 0, 128).

validate_network_address(Value) ->
    ?LOG_DEBUG(#{what => validate_network_address,
                 value => Value}),
    validate_oneof(Value, [fun validate_domain/1, fun validate_ip_address/1]).

validate_oneof(Value, Funs) ->
    Results = [safe_call_validator(F, Value) || F <- Funs],
    case lists:any(fun(R) -> R =:= ok end, Results) of
        true ->
            ok;
        false ->
            error(#{what => validate_oneof_failed,
                    validation_results => Results})
    end.

safe_call_validator(F, Value) ->
    try
        F(Value),
        ok
    catch error:Reason:Stacktrace ->
              #{reason => Reason, stacktrace => Stacktrace}
    end.

validate_network_port(Value) ->
    validate_range(Value, 0, 65535).

validate_range(Value, Min, Max) when Value >= Min, Value =< Max ->
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

validate_dirname(Dirname) ->
    case file:list_dir(Dirname) of
        {ok, _} ->
            ok;
        Reason ->
            error(#{what => invalid_dirname, dirname => Dirname, reason => Reason})
    end.

validate_keystore_key({Name, ram}) ->
    validate_non_empty_atom(Name);
validate_keystore_key({Name, {file, Path}}) ->
    validate_non_empty_atom(Name),
    validate_filename(Path).

validate_muc_affiliation_rule({{User, Server, Resource}, Affiliation}) ->
    validate_non_empty_binary(User),
    validate_binary_domain(Server),
    validate_binary(Resource),
    validate_non_empty_atom(Affiliation).

validate_maybe_css_file(false) ->
    ok;
validate_maybe_css_file(Bin) ->
    validate_non_empty_binary(Bin). %% Could be more precise type

validate_top_link({Url, Text}) ->
    validate_url(Url),
    validate_non_empty_string(Text).

validate_muc_config_schema({Field, Value}) ->
    validate_non_empty_string(Field),
    validate_string(Value);
validate_muc_config_schema({Field, Value, InternalField, FieldType})
    when is_list(Value); is_float(Value); is_integer(Value) ->
    validate_non_empty_string(Field),
    validate_enum(FieldType, [binary, integer, float]),
    validate_non_empty_atom(InternalField).

validate_pubsub_nodetree(Value) ->
    validate_non_empty_binary(Value),
    validate_backend(nodetree, b2a(Value)).

validate_pubsub_plugin(Value) ->
    validate_non_empty_binary(Value),
    validate_backend(node, b2a(Value)).

validate_pubsub_pep_mapping({Namespace, Id}) ->
    validate_non_empty_string(Namespace),
    validate_non_empty_string(Id).

b2a(Bin) ->
    binary_to_atom(Bin, utf8).

validate_revproxy_route({Host, Path, Method, Upstream}) ->
    validate_non_empty_string(Host),
    validate_string(Path),
    validate_string(Method),
    validate_non_empty_string(Upstream);
validate_revproxy_route({Host, Path, Upstream}) ->
    validate_non_empty_string(Host),
    validate_string(Path),
    validate_non_empty_string(Upstream).

validate_ldap_vcard_map({VCardField, LDAPPattern, LDAPFields}) ->
    validate_non_empty_binary(VCardField),
    validate_non_empty_binary(LDAPPattern),
    lists:foreach(fun validate_non_empty_binary/1, LDAPFields).

validate_ldap_search_field({SearchField, LDAPField}) ->
    validate_non_empty_binary(SearchField),
    validate_non_empty_binary(LDAPField).

validate_ldap_search_reported({SearchField, VCardField}) ->
    validate_non_empty_binary(SearchField),
    validate_non_empty_binary(VCardField).

validate_ldap_uids({Attribute, Format}) ->
    validate_non_empty_string(Attribute),
    validate_non_empty_string(Format);
validate_ldap_uids(Attribute) ->
    validate_non_empty_string(Attribute).

validate_pool_name(V) ->
    validate_non_empty_atom(V).

validate_access_rule(V) ->
    validate_non_empty_atom(V).

validate_shaper_name(V) ->
    validate_non_empty_atom(V).
