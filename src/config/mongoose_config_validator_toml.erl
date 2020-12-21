-module(mongoose_config_validator_toml).

-export([validate/3,
         validate_section/2,
         validate_list/2]).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").
-include_lib("jid/include/jid.hrl").

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
validate(V, string, domain) -> validate_domain(V);
validate(V, string, domain_template) -> validate_domain_template(V);
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
