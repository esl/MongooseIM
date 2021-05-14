-module(mongoose_subdomain_utils).

%% API
-export([make_subdomain_pattern/1,
         get_fqdn/2,
         subdomain_type/1,
         is_subdomain/3]).

-type subdomain_pattern() :: {fqdn | prefix, binary()}.
-export_type([subdomain_pattern/0]).

%% this function preprocesses configuration subdomain templates like:
%%  "subdomain.@HOST@"
%% it is compatible with mongoose_config_parser_toml:processor() type,
%% so it can be used as #option.process value (for more information see
%% mongoose_config_spec.hrl)
-spec make_subdomain_pattern(SubdomainPatternConfigOpt :: binary() | string()) ->
    subdomain_pattern().
make_subdomain_pattern(ConfigOpt) when is_list(ConfigOpt) ->
    make_subdomain_pattern(list_to_binary(ConfigOpt));
make_subdomain_pattern(ConfigOpt) when is_binary(ConfigOpt) ->
    case re:replace(ConfigOpt, "\\.@HOST@$", ".", [{return, binary}]) of
        ConfigOpt -> {fqdn, ConfigOpt};
        Prefix -> {prefix, Prefix}
    end.

-spec get_fqdn(subdomain_pattern(), Domain :: mongooseim:domain_name()) ->
    Subdomain :: mongooseim:domain_name().
get_fqdn({fqdn, Subdomain}, _Domain) -> Subdomain;
get_fqdn({prefix, Prefix}, Domain)   -> <<Prefix/binary, Domain/binary>>.

-spec subdomain_type(subdomain_pattern()) -> fqdn | subdomain.
subdomain_type({fqdn, _}) -> fqdn;
subdomain_type({prefix, _}) -> subdomain.

-spec is_subdomain(subdomain_pattern(),
                   Domain :: mongooseim:domain_name(),
                   Subdomain :: mongooseim:domain_name()) -> boolean().
is_subdomain({fqdn, FQDN}, _, Subdomain) ->
    FQDN =:= Subdomain;
is_subdomain({prefix, Prefix}, Domain, Subdomain) ->
    Subdomain =:= <<Prefix/binary, Domain/binary>>.
