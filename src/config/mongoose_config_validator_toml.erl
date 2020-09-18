-module(mongoose_config_validator_toml).

-export([validate/2]).

%% To explore module option paths
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
               erlang:raise(Class, #{type => Type, reason => Reason}, Stacktrace)
     end || Type <- Types],
    ok.

%% Module type specs

%% Module validators follow the rules:
%% - Each module has it's own Validator Type Spec Function.
%% - Validator Type Spec Function has the same name as the module it validates.
%% - Validator Type Spec Function should be present in module_spec_functions/0.
%% - Validator Type Spec Function returns Options Spec Map.
%% - Keys in the Options Spec Map are TOML keys. But atoms.
%% - Values in the Options Spec Map are types.
%% - Types are defined in type_to_validator/0 function.
%% - Types have validators (consult with type_to_validator/0 to find out the validator function).
%% - If Type is a map, it means that the TOML path contains a map. 

mod_adhoc() ->
    #{iqdisc => iqdisc,
      report_commands_node => boolean}.

mod_auth_token() ->
    #{iqdisc => iqdisc,
      %% Pass the whole list into validator
      validity_period => {multi, validity_period}}.

mod_bosh() ->
    #{inactivity => non_neg_integer_or_inf,
      max_wait => non_neg_integer_or_inf,
      server_acks => boolean,
      backend => backend}.

mod_carboncopy() ->
    #{iqdisc => iqdisc}.

mod_caps() ->
    #{cache_size => non_neg_integer_or_inf,
      cache_life_time => non_neg_integer_or_inf}.

mod_csi() ->
    #{buffer_max => non_neg_integer_or_inf}.

mod_disco() ->
    #{users_can_see_hidden_services => boolean,
      extra_domains => {list, binary_domain},
      urls => {list, url},
      server_info => {list, #{name => non_empty_binary,
                              module => {list, module},
                              urls => {list, url}}}}.

mod_inbox() ->
    #{iqdisc => iqdisc,
      backend => backend,
      aff_changes => boolean,
      remove_on_kicked => boolean,
      reset_markers => {list, chat_marker_type},
      groupchat => {list, groupchat_type}}.

mod_global_distrib() ->
    #{global_host => domain,
      local_host => domain,
      message_ttl => non_neg_integer,
      hosts_refresh_interval => non_neg_integer,
      connections => global_distrib_connections(),
      redis => #{pool => non_empty_atom,
                 expire_after => non_neg_integer,
                 refresh_after => non_neg_integer},
      cache => #{cache_missed => boolean,
                 domain_lifetime_seconds => non_neg_integer,
                 jid_lifetime_seconds => non_neg_integer,
                 max_jids => non_neg_integer},
      bounce => #{resend_after_ms => non_neg_integer,
                  max_retries => non_neg_integer}}.

%% This block is too long for inlining into mod_global_distrib
global_distrib_connections() ->
    #{endpoints => {list, #{host => network_address, port => network_port}},
      advertised_endpoints => {optional_section, advertised_endpoints,
                               {list, #{host => network_address, port => network_port}}},
      connections_per_endpoint => non_neg_integer,
      endpoint_refresh_interval => pos_integer,
      endpoint_refresh_interval_when_empty => pos_integer,
      disabled_gc_interval => non_neg_integer,
      tls => {optional_section, tls_opts, tls_opts_spec()}}.

tls_opts_spec() ->
    #{cacertfile => {renamed, cafile, filename},
      certfile => filename,
      dhfile => filename,
      ciphers => string}.

mod_event_pusher() ->
    #{backend => event_pusher_backend()}.

event_pusher_backend() ->
    #{sns => #{access_key_id => string,
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
               publish_retry_time_ms => non_neg_integer},
       push => #{backend => {backend, mod_event_pusher_push},
                 wpool => #{strategy => wpool_strategy, workers => pos_integer},
                 plugin_module => module,
                 virtual_pubsub_hosts => {list, domain_template}},
       http => #{pool_name => non_empty_atom,
                 path => string,
                 callback_module => module},
       rabbit => #{presence_exchange => #{name => non_empty_binary,
                                          type => non_empty_binary},
                   chat_msg_exchange => #{name => non_empty_binary,
                                          sent_topic => non_empty_binary,
                                          recv_topic => non_empty_binary},
                   groupchat_msg_exchange => #{name => non_empty_binary,
                                               sent_topic => non_empty_binary,
                                               recv_topic => non_empty_binary}}
      }.

mod_http_upload() ->
    #{iqdisc => iqdisc,
      backend => backend,
      host => domain_template,
      expiration_time => non_neg_integer,
      token_bytes => pos_integer,
      max_file_size => non_neg_integer,
      s3 => #{bucket_url => url,
              add_acl => boolean,
              region => string,
              access_key_id => string,
              secret_access_key => string}}.

mod_jingle_sip() ->
    #{proxy_host => network_address,
      proxy_port => network_port,
      listen_port => network_port,
      local_host => network_address,
      sdp_origin => ip_address}.

mod_keystore() ->
    #{ram_key_size => non_neg_integer,
      keys => {list, keystore_key}}.

mod_last() ->
    #{iqdisc => iqdisc,
      backend => backend,
      riak => #{bucket_type => non_empty_binary}}.

mod_mam_meta() ->
    Base = mam_meta_opts(),
    Base#{
      pm => Base,
      muc => Base,
      riak => #{bucket_type => non_empty_binary, search_index => non_empty_binary}
     }.

%% We need some spec duplication,
%% because options could be defined as root module options
%% or PM (or MUC) specific
mam_meta_opts() ->
    #{backend => {enum, [rdbms, riak, cassandra, elasticsearch]},
      archive_chat_markers => boolean,
      archive_groupchats => boolean,
      async_writer => boolean,
      async_writer_rdbms_pool => non_empty_atom,
      cache_users => boolean,
      db_jid_format => module,
      db_message_format => module,
      default_result_limit => non_neg_integer,
      extra_lookup_params => module,
      host => domain_template,
      flush_interval => non_neg_integer,
      full_text_search => boolean,
      max_batch_size => non_neg_integer,
      max_result_limit => non_neg_integer,
      message_retraction => boolean,
      rdbms_message_format => {enum, [simple]},
      simple => boolean,
      no_stanzaid_element => boolean,
      is_archivable_message => module,
      user_prefs_store => {enum, [false, rdbms, cassandra, mnesia]}}.

mod_muc() ->
    #{host => domain_template,
      backend => {backend, mod_muc_db},
      access => access_rule,
      access_create => access_rule,
      access_admin => access_rule,
      access_persistent => access_rule,
      history_size => non_neg_integer,
      room_shaper => shaper_name,
      max_room_id => non_neg_integer_or_inf,
      max_room_name => non_neg_integer_or_inf,
      max_room_desc => non_neg_integer_or_inf,
      min_message_interval => non_neg_integer,
      min_presence_interval => non_neg_integer,
      max_users => pos_integer,
      max_users_admin_threshold => pos_integer,
      user_message_shaper => shaper_name,
      user_presence_shaper => shaper_name,
      max_user_conferences => non_neg_integer,
      http_auth_pool => pool_name,
      load_permanent_rooms_at_startup => boolean,
      hibernated_room_check_interval => non_neg_integer_or_inf,
      hibernated_room_timeout => non_neg_integer_or_inf,
      default_room => muc_default_room()}.

muc_default_room() ->
    #{title => string,
      description => binary,
      allow_change_subj => boolean,
      allow_query_users => boolean,
      allow_private_messages => boolean,
      allow_visitor_status => boolean,
      allow_visitor_nickchange => boolean,
      public => boolean,
      public_list => boolean,
      persistent => boolean,
      moderated => boolean,
      members_by_default => boolean,
      members_only => boolean,
      allow_user_invites => boolean,
      allow_multiple_sessions => boolean,
      password_protected => boolean,
      password => string,
      anonymous => boolean,
      max_users => pos_integer,
      logging => boolean,
      subject => string,
      subject_author => string,
      maygetmemberlist => {list, non_empty_atom},
      affiliations => {list, muc_affiliation_rule}}.

mod_muc_log() ->
    #{outdir => dirname,
      access_log => access_rule,
      dirtype => {enum, [subdirs, plain]},
      dirname => {enum, [room_jid, room_name]},
      file_format => {enum, [html, plaintext]},
      timezone => {enum, [local, universal]},
      spam_prevention => boolean,
      css_file => {renamed, cssfile, maybe_css_file},
      top_link => top_link}.

mod_muc_light() ->
    #{host => domain_template,
      backend => {backend, mod_muc_light_db},
      equal_occupants => boolean,
      legacy_mode => boolean,
      rooms_per_page => pos_integer_or_inf,
      blocking => boolean,
      all_can_configure => boolean,
      all_can_invite => boolean,
      max_occupants => pos_integer_or_inf,
      rooms_in_rosters => boolean,
      config_schema => {list, muc_config_schema}}.

mod_offline() ->
    #{access_max_user_messages => access_rule,
      backend => backend,
      riak => #{bucket_type => non_empty_binary}}.

mod_ping() ->
    #{iqdisc => iqdisc,
      send_pings => boolean,
      timeout_action => {enum, [none, kill]},
      ping_interval => pos_integer,
      ping_req_timeout => pos_integer}.

mod_privacy() ->
     #{backend => backend,
       riak => #{defaults_bucket_type => non_empty_binary,
                 names_bucket_type => non_empty_binary,
                 bucket_type => non_empty_binary}}.

mod_private() ->
    #{iqdisc => iqdisc,
      backend => backend,
      riak => #{bucket_type => non_empty_binary}}.

mod_pubsub() ->
    #{iqdisc => iqdisc,
      host => domain_template,
      backend => {backend, mod_pubsub_db},
      access_createnode => access_rule,
      max_items_node => non_neg_integer,
      max_subscriptions_node => non_neg_integer,
      ignore_pep_from_offline => boolean,
      item_publisher => boolean,
      sync_broadcast => boolean,
      nodetree => pubsub_nodetree,
      last_item_cache => {enum, [mnesia, rdbms, false]},
      plugins => {list, pubsub_plugin},
      pep_mapping => {list, pubsub_pep_mapping},
      default_node_config => pubsub_default_node_config()}.

pubsub_default_node_config() ->
    #{access_model => non_empty_atom,
      deliver_notifications => boolean,
      deliver_payloads => boolean,
      max_items => non_neg_integer,
      max_payload_size => non_neg_integer,
      node_type => non_empty_atom,
      notification_type => non_empty_atom,
      notify_config => boolean,
      notify_delete => boolean,
      notify_retract => boolean,
      persist_items => boolean,
      presence_based_delivery => boolean,
      publish_model => non_empty_atom,
      purge_offline => boolean,
      send_last_published_item => non_empty_atom,
      subscribe => boolean,
      roster_groups_allowed => {list, non_empty_binary}
    }.

mod_push_service_mongoosepush() ->
    #{pool_name => pool_name,
      api_version => string,
      max_http_connections => non_neg_integer}.

mod_register() ->
    #{iqdisc => iqdisc,
      %% Pass the whole thing into validator
      ip_access => {list, ip_access},
      welcome_message => #{subject => string, body => string},
      access => non_empty_atom,
      registration_watchers => {list, jid},
      password_strength => non_neg_integer}.

mod_revproxy() ->
    #{routes => {list, revproxy_route}}.

mod_roster() ->
    #{iqdisc => iqdisc,
      versioning => boolean,
      store_current_id => boolean,
      backend => backend,
      riak => #{bucket_type => non_empty_binary,
                version_bucket_type => non_empty_binary}}.

mod_shared_roster_ldap() ->
    #{ldap_pool_tag => pool_name,
      ldap_base => string,
      ldap_deref => {enum, [never, always, finding, searching]},
      %% - attributes
      ldap_groupattr => string,
      ldap_groupdesc => string,
      ldap_userdesc => string,
      ldap_useruid => string,
      ldap_memberattr => string,
      ldap_memberattr_format => string,
      ldap_memberattr_format_re => string,
      %% - parameters
      ldap_auth_check => boolean,
      ldap_user_cache_validity => non_neg_integer,
      ldap_group_cache_validity => non_neg_integer,
      ldap_user_cache_size => non_neg_integer,
      ldap_group_cache_size => non_neg_integer,
      %% - LDAP filters
      ldap_rfilter => string,
      ldap_gfilter => string,
      ldap_ufilter => string,
      ldap_filter => string}.

mod_sic() ->
    #{iqdisc => iqdisc}.

mod_stream_management() ->
    #{buffer_max => non_neg_integer,
      ack_freq => non_neg_integer,
      resume_timeout => non_neg_integer,
      stale_h => #{enabled => boolean,
                   repeat_after => {renamed, stale_h_repeat_after,
                                    non_neg_integer},
                   geriatric => {renamed, stale_h_geriatric,
                                 non_neg_integer}}}.

mod_version() ->
    #{iqdisc => iqdisc,
      os_info => boolean}.

mod_vcard() ->
    #{iqdisc => iqdisc,
      host => domain_template,
      search => boolean,
      backend => backend,
      matches => non_neg_integer_or_inf,
      %% - ldap
      ldap_pool_tag => pool_name,
      ldap_base => string,
      ldap_deref => {enum, [never, always, finding, searching]},
      ldap_uids => {list, ldap_uids},
      ldap_filter => string,
      ldap_vcard_map => {list, ldap_vcard_map},
      ldap_search_fields => {list, ldap_search_field},
      ldap_search_reported => {list, ldap_search_reported},
      ldap_search_operator => {enum, ['or', 'and']},
      ldap_binary_search_fields => {list, non_empty_binary},
      riak => #{bucket_type => non_empty_binary, search_index => non_empty_binary}}.

mod_time() ->
    #{iqdisc => iqdisc}.


module_spec_functions() ->
    %% Module name and function name should be the same in this map
    #{mod_adhoc => fun mod_adhoc/0,
      mod_auth_token => fun mod_auth_token/0,
      mod_bosh => fun mod_bosh/0,
      mod_carboncopy => fun mod_carboncopy/0,
      mod_caps => fun mod_caps/0,
      mod_csi => fun mod_csi/0,
      mod_disco => fun mod_disco/0,
      mod_inbox => fun mod_inbox/0,
      mod_global_distrib => fun mod_global_distrib/0,
      mod_event_pusher => fun mod_event_pusher/0,
      mod_http_upload => fun mod_http_upload/0,
      mod_jingle_sip => fun mod_jingle_sip/0,
      mod_keystore => fun mod_keystore/0,
      mod_last => fun mod_last/0,
      mod_mam_meta => fun mod_mam_meta/0,
      mod_muc => fun mod_muc/0,
      mod_muc_log => fun mod_muc_log/0,
      mod_muc_light => fun mod_muc_light/0,
      mod_offline => fun mod_offline/0,
      mod_ping => fun mod_ping/0,
      mod_privacy => fun mod_privacy/0,
      mod_private => fun mod_private/0,
      mod_pubsub => fun mod_pubsub/0,
      mod_push_service_mongoosepush => fun mod_push_service_mongoosepush/0,
      mod_register => fun mod_register/0,
      mod_revproxy => fun mod_revproxy/0,
      mod_roster => fun mod_roster/0,
      mod_shared_roster_ldap => fun mod_shared_roster_ldap/0,
      mod_sic => fun mod_sic/0,
      mod_stream_management => fun mod_stream_management/0,
      mod_version => fun mod_version/0,
      mod_vcard => fun mod_vcard/0,
      mod_time => fun mod_time/0}.

type_to_validator() ->
    #{%% Basic validators
      string => fun validate_string/1,
      boolean => fun validate_boolean/1,
      binary => fun validate_binary/1,
      non_empty_atom => fun validate_non_empty_atom/1,
      non_empty_binary => fun validate_non_empty_binary/1,
      non_neg_integer => fun validate_non_negative_integer/1,
      non_neg_integer_or_inf => fun validate_non_negative_integer_or_infinity/1,
      pos_integer => fun validate_positive_integer/1,
      pos_integer_or_inf => fun validate_positive_integer_or_infinity/1,
      filename => fun validate_filename/1,
      dirname => fun validate_dirname/1,
      module => fun validate_module/1,
      %% Networking and addresation
      url => fun validate_url/1,
      jid => fun validate_jid/1,
      domain => fun validate_domain/1,
      domain_template => fun validate_domain_template/1,
      binary_domain => fun validate_binary_domain/1,
      binary_domain_template => fun validate_binary_domain_template/1,
      ip_access => fun validate_ip_access/1,
      ip_address => fun validate_ip_address/1,
      network_address  => fun validate_network_address/1,
      network_port => fun validate_network_port/1,
      %% Other
      iqdisc => fun validate_iqdisc/1,
      pool_name => fun validate_non_empty_atom/1,
      groupchat_type => fun validate_groupchat_type/1,
      chat_marker_type => fun validate_chat_marker_type/1,
      period_unit => fun validate_period_unit/1,
      keystore_key => fun validate_keystore_key/1,
      access_rule => fun validate_non_empty_atom/1,
      shaper_name => fun validate_non_empty_atom/1,
      wpool_strategy => fun validate_wpool_strategy/1,
      maybe_css_file => fun validate_maybe_css_file/1,
      pubsub_nodetree => fun validate_pubsub_nodetree/1,
      pubsub_plugin => fun validate_pubsub_plugin/1,
      %% Sections (the whole section term is passed into the validators)
      top_link => fun validate_top_link/1,
      auth_token_domain => fun validate_auth_token_domain/1,
      validity_period => fun validate_validity_period/1,
      muc_affiliation_rule => fun validate_muc_affiliation_rule/1,
      muc_config_schema => fun validate_muc_config_schema/1,
      pubsub_pep_mapping => fun validate_pubsub_pep_mapping/1,
      revproxy_route => fun validate_revproxy_route/1,
      ldap_uids => fun validate_ldap_uids/1,
      ldap_vcard_map => fun validate_ldap_vcard_map/1,
      ldap_search_field => fun validate_ldap_search_field/1,
      ldap_search_reported => fun validate_ldap_search_reported/1
      %% Called from validate_type function: 
      %% backend => fun validate_backend/2,
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
    lists:append([module_option_paths(M, O, T) || {M,O,T} <- module_option_types_spec()]).

module_option_types_spec() ->
    ModuleSpecs = [{Module, Fun()}
                   || {Module, Fun} <- maps:to_list(module_spec_functions())],
    [{Module, OptName, OptSpec} ||
     {Module, SpecMap} <- ModuleSpecs,
     {OptName, OptSpec} <- maps:to_list(SpecMap)].

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
add_unlistify(Type = #{}) ->
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
add_wrapped(_Opt, {renamed, NewName, Type}) ->
    {wrapped, NewName, Type}; %% Reuse wrapped logic
add_wrapped(_Opt, Type = {optional_section, _Type}) ->
    Type;
add_wrapped(_Opt, Type = {optional_section, _Name, _Type}) ->
    Type;
add_wrapped(_Opt, Type = #{}) ->
    Type;
add_wrapped(_Opt, {multi, Type}) ->
    {multi, Type};
add_wrapped(Opt, Type) ->
    {wrapped, Opt, Type}.

%% maps inside lists are unwrapped
%% other maps are usually wrapped_section
%% maps inside optional_section are wrapped_section
type_to_paths({list, Type}, Path) ->
    type_to_paths(Type, [item|Path]);
type_to_paths({optional_section, Name, Dict}, Path) ->
    [{Path, {optional_section, Name}}] ++ type_to_paths(Dict, Path);
type_to_paths(Dict, [item|_] = Path) when is_map(Dict) ->
    lists:append([type_to_paths(Type, [atom_to_binary(Key, utf8)|Path])
                  || {Key, Type} <- maps:to_list(Dict)]);
type_to_paths(Dict, Path) when is_map(Dict) ->
    lists:append([type_to_paths(add_wrapped(Key, Type),
                                [atom_to_binary(Key, utf8)|Path])
                  || {Key, Type} <- maps:to_list(Dict)]);
type_to_paths(Type, Path) ->
    [{Path, Type}].

path_to_module(Path) ->
    PathR = lists:reverse(Path),
    case PathR of
        [<<"host_config">>, _, <<"modules">>, Mod|_] ->
            b2a(Mod);
        [<<"modules">>, Mod|_] ->
            b2a(Mod)
    end.

path_without_host_config(Path) ->
    PathR = lists:reverse(Path),
    [<<"host_config">>, _|Rest] = PathR,
    lists:reverse(Rest).


%% validators

validate_loglevel(Level) ->
    mongoose_logs:loglevel_number_keyword(Level).

validate_non_empty_binary(Value) when is_binary(Value), Value =/= <<>> -> ok.

validate_binary(Value) when is_binary(Value) -> ok.

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

validate_positive_integer_or_infinity(Value) when is_integer(Value), Value > 0 -> ok;
validate_positive_integer_or_infinity(infinity) -> ok.

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

validate_binary_domain_template(Domain) ->
    validate_domain_template(binary_to_list(Domain)).

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
    validate_range(Mask, 0, 32).

validate_network_address(Value) ->
    ?LOG_DEBUG(#{what => validate_network_address,
                 value => Value}),
    validate_oneof(Value, [domain, ip_address]).

validate_oneof(Value, Validators) ->
    Map = type_to_validator(),
    Funs = [maps:get(Type, Map) || Type <- Validators],
    Results = [safe_call_validator(F, Value) || F <- Funs],
    case lists:any(fun(R) -> R =:= ok end, Results) of
        true ->
            ok;
        false ->
            error(#{what => validate_oneof_failed,
                    validation_results => lists:zip(Validators, Results)})
    end.

safe_call_validator(F, Value) ->
    try
        F(Value),
        ok
    catch Class:Reason:Stacktrace ->
              #{class => Class, reason => Reason, stacktrace => Stacktrace}
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

validate_optional_section(Name, {Name, false}) -> %% set to false to disable the feature
    ok;
validate_optional_section(Name, {Name, List}) when is_list(List) -> %% proplist
    ok.

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
