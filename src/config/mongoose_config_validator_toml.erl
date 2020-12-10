-module(mongoose_config_validator_toml).

-export([validate/2,
         validate/3,
         validate_section/2,
         validate_list/2]).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").
-include_lib("jid/include/jid.hrl").

-define(HOST, 'HOST').

-spec validate(mongoose_config_parser_toml:path(),
               mongoose_config_parser_toml:option() | mongoose_config_parser_toml:config_list()) ->
          any().
validate(Path, [F]) when is_function(F, 1) ->
    validate(Path, F(?HOST));

%% Modules
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
validate([<<"iqdisc">>, <<"mod_time">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
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
validate([<<"iqdisc">>, <<"mod_sic">>, <<"modules">>|_],
         [{iqdisc, V}]) ->
    validate_iqdisc(V);
validate([item, <<"keys">>, <<"mod_keystore">>, <<"modules">>|_],
         [V]) ->
    validate_keystore_key(V);
validate([<<"ram_key_size">>, <<"mod_keystore">>, <<"modules">>|_],
         [{ram_key_size, V}]) ->
    validate_non_negative_integer(V);

validate(_Path, _Value) ->
    ok.

validate(V, binary, domain) -> validate_binary_domain(V);
validate(V, binary, non_empty) -> validate_non_empty_binary(V);
validate(V, binary, {module, Prefix}) ->
    validate_module(list_to_atom(atom_to_list(Prefix) ++ "_" ++ binary_to_list(V)));
validate(V, binary, jid) -> validate_jid(V);
validate(V, integer, non_negative) -> validate_non_negative_integer(V);
validate(V, integer, positive) -> validate_positive_integer(V);
validate(V, integer, port) -> validate_port(V);
validate(V, int_or_infinity, non_negative) -> validate_non_negative_integer_or_infinity(V);
validate(V, int_or_infinity, positive) -> validate_positive_integer_or_infinity(V);
validate(V, int_or_infinity_or_atom, positive) ->
    validate_positive_integer_or_infinity_or_atom(V, no_buffer);
validate(V, int_or_atom, positive) ->
    validate_positive_integer_or_atom(V, never);
validate(V, string, url) -> validate_url(V);
validate(V, string, domain_template) -> validate_domain_template(V);
validate(V, string, ip_address) -> validate_ip_address(V);
validate(V, string, ip_mask) -> validate_ip_mask_string(V);
validate(V, string, non_empty) -> validate_non_empty_string(V);
validate(V, string, dirname) -> validate_dirname(V);
validate(V, atom, module) -> validate_module(V);
validate(V, atom, {module, Prefix}) ->
    validate_module(list_to_atom(atom_to_list(Prefix) ++ "_" ++ atom_to_list(V)));
validate(V, atom, loglevel) -> validate_loglevel(V);
validate(V, atom, pool_name) -> validate_non_empty_atom(V);
validate(V, atom, shaper) -> validate_non_empty_atom(V);
validate(V, atom, access_rule) -> validate_non_empty_atom(V);
validate(V, atom, non_empty) -> validate_non_empty_atom(V);
validate(V, _, {enum, Values}) -> validate_enum(V, Values);
validate(_V, _, any) -> ok.

validate_list([_|_], non_empty) -> ok;
validate_list(L = [_|_], unique_non_empty) -> validate_unique_items(L);
validate_list(L, unique) -> validate_unique_items(L);
validate_list(L, any) when is_list(L) -> ok.

validate_section([_|_], non_empty) -> ok;
validate_section(L, any) when is_list(L) -> ok.

%% validators

validate_loglevel(Level) ->
    mongoose_logs:loglevel_keyword_to_number(Level).

validate_non_empty_binary(Value) when is_binary(Value), Value =/= <<>> -> ok.

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

validate_backend(Mod, Backend) ->
    validate_module(backend_module:backend_module(Mod, Backend)).

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

validate_pool_name(V) ->
    validate_non_empty_atom(V).
