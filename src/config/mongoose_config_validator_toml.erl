-module(mongoose_config_validator_toml).

-export([validate/2]).

-include("mongoose.hrl").
-include("ejabberd_config.hrl").

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
validate([<<"pgsql_users_number_estimate">>, <<"general">>],
         [#local_config{value = Value}]) ->
    validate_boolean(Value);
validate([<<"route_subdomain">>, <<"general">>],
         [#local_config{value = Value}]) ->
    validate_enum(Value, [s2s]);
validate([item, <<"routing_modules">>, <<"general">>],
         [Value]) ->
    validate_module(Value);
validate([<<"replaced_wait_timeout">>, <<"general">>],
         [#local_config{value = Value}]) ->
    validate_positive_integer(Value);
validate([<<"hide_service_name">>, <<"general">>],
         [#local_config{value = Value}]) ->
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
validate([<<"hibernate_after">>, item, <<"c2s">>, <<"listen">>],
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
validate([FileType, _TLS, item, _Type, <<"listen">>],
         [{_, Value}]) when FileType =:= <<"certfile">>;
                            FileType =:= <<"cacertfile">>;
                            FileType =:= <<"dhfile">> ->
    validate_non_empty_string(Value);
validate([<<"max_stanza_size">>, item, _Type, <<"listen">>],
         [{max_stanza_size, Value}]) ->
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

validate(_, _) ->
    ok.

%% helpers

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
    {module, _} = code:ensure_loaded(Mod).

validate_positive_integer(Value) when is_integer(Value), Value > 0 -> ok.

validate_non_negative_integer(Value) when is_integer(Value), Value >= 0 -> ok.

validate_non_negative_integer_or_infinity(Value) when is_integer(Value), Value >= 0 -> ok;
validate_non_negative_integer_or_infinity(infinity) -> ok.

validate_enum(Value, Values) ->
    true = lists:member(Value, Values).

validate_port(Value) when is_integer(Value), Value >= 0, Value =< 65535 -> ok.

validate_non_empty_atom(Value) when is_atom(Value), Value =/= '' -> ok.

validate_non_empty_string(Value) when is_list(Value), Value =/= "" -> ok.
