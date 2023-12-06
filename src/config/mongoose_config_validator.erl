-module(mongoose_config_validator).

-export([validate/3,
         validate_section/2,
         validate_list/2]).

-include("mongoose.hrl").
-include_lib("jid/include/jid.hrl").

-type validator() ::
        any | non_empty | non_negative | positive | module | {module, Prefix :: atom()}
      | jid | domain | subdomain_template | url | ip_address | ip_mask | network_address | port
      | filename | dirname | loglevel | pool_name | shaper | access_rule | {enum, list()}.

-type section_validator() :: any | non_empty.

-type list_validator() :: any | non_empty | unique | unique_non_empty.

-export_type([validator/0, section_validator/0, list_validator/0]).

-spec validate(mongoose_config_parser_toml:option_value(),
               mongoose_config_spec:option_type(), validator()) -> any().
validate(V, binary, domain) -> validate_domain(V);
validate(V, binary, url) -> validate_non_empty_binary(V);
validate(V, binary, non_empty) -> validate_non_empty_binary(V);
validate(V, binary, subdomain_template) -> validate_subdomain_template(V);
validate(V, binary, {module, Prefix}) ->
    validate_module(list_to_atom(atom_to_list(Prefix) ++ "_" ++ binary_to_list(V)));
validate(V, binary, jid) -> validate_jid(V);
validate(V, binary, ldap_filter) -> validate_ldap_filter(V);
validate(V, integer, non_negative) -> validate_non_negative_integer(V);
validate(V, integer, positive) -> validate_positive_integer(V);
validate(V, integer, port) -> validate_port(V);
validate(V, int_or_infinity, non_negative) -> validate_non_negative_integer_or_infinity(V);
validate(V, int_or_infinity, positive) -> validate_positive_integer_or_infinity(V);
validate(V, string, url) -> validate_url(V);
validate(V, string, domain) -> validate_domain(V);
validate(V, string, subdomain_template) -> validate_subdomain_template(V);
validate(V, string, ip_address) -> validate_ip_address(V);
validate(V, string, ip_mask) -> validate_ip_mask_string(V);
validate(V, string, network_address) -> validate_network_address(V);
validate(V, string, filename) -> validate_filename(V);
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

-spec validate_list([mongoose_config_parser_toml:config_part()], list_validator()) -> any().
validate_list([_|_], non_empty) -> ok;
validate_list(L = [_|_], unique_non_empty) -> validate_unique_items(L);
validate_list(L, unique) -> validate_unique_items(L);
validate_list(L, any) when is_list(L) -> ok.

-spec validate_section([mongoose_config_parser_toml:config_part()], section_validator()) -> any().
validate_section([_|_], non_empty) -> ok;
validate_section(L, any) when is_list(L) -> ok.

%% validators

validate_loglevel(Level) ->
    mongoose_logs:loglevel_keyword_to_number(Level).

validate_non_empty_binary(Value) when is_binary(Value), Value =/= <<>> -> ok.

validate_unique_items(Items) ->
    L = sets:size(sets:from_list(Items)),
    L = length(Items).

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

validate_jid(Jid) ->
    case jid:from_binary(Jid) of
        #jid{} ->
            ok;
        _ ->
            error(#{what => validate_jid_failed, value => Jid})
    end.

validate_ldap_filter(Value) ->
    {ok, _} = eldap_filter:parse(Value).

validate_subdomain_template(SubdomainTemplate) ->
    case mongoose_subdomain_utils:make_subdomain_pattern(SubdomainTemplate) of
        {fqdn, Domain} ->
            validate_domain(Domain);
        Pattern ->
            Domain = binary_to_list(mongoose_subdomain_utils:get_fqdn(Pattern, <<"example.com">>)),
            case inet_parse:domain(Domain) of
                true ->
                    ok;
                false ->
                    error(#{what => validate_subdomain_template_failed,
                            text => <<"Invalid subdomain template">>,
                            subdomain_template => SubdomainTemplate})
            end
    end.

validate_domain(Domain) when is_binary(Domain) ->
    validate_domain(binary_to_list(Domain));
validate_domain(Domain) ->
    validate_domain_name(Domain),
    resolve_domain(Domain).

validate_domain_name(Domain) ->
    case inet_parse:domain(Domain) of
        true ->
            ok;
        false ->
            error(#{what => validate_domain_failed,
                    text => <<"Invalid domain name">>,
                    domain => Domain})
    end.

resolve_domain(Domain) ->
    case inet_res:gethostbyname(Domain) of
        {ok, _} ->
            ok;
        {error, Reason} -> %% timeout, nxdomain
            ?LOG_WARNING(#{what => cfg_validate_domain,
                           reason => Reason, domain => Domain,
                           text => <<"Couldn't resolve domain. "
                  "It could cause issues with production installations">>}),
            ignore
    end.


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
