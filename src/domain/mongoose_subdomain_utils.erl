-module(mongoose_subdomain_utils).

-include("mongoose_logger.hrl").

%% API
-export([make_subdomain_pattern/1,
         get_fqdn/2,
         subdomain_type/1,
         check_domain_name/2,
         check_subdomain_name/1,
         report_subdomains_collision/2,
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

-spec check_domain_name(mongooseim:host_type(), mongooseim:domain_name()) ->
    boolean().
check_domain_name(_HostType, Domain) ->
    case mongoose_subdomain_core:get_subdomain_info(Domain) of
        {error, not_found} -> true;
        {ok, _Info} ->
            %% TODO: this is critical collision, and it must be reported properly
            %% think about adding some metric, so devops can set some alarm for it
            ?LOG_ERROR(#{what => check_domain_name_failed, domain => Domain}),
            false
    end.

-spec check_subdomain_name(mongoose_subdomain_core:subdomain_info()) -> boolean().
check_subdomain_name(#{subdomain := Subdomain} = _SubdomainInfo) ->
    case mongoose_domain_core:get_host_type(Subdomain) of
        {error, not_found} -> true;
        {ok, _HostType} ->
            %% TODO: this is critical collision, and it must be reported properly
            %% think about adding some metric, so devops can set some alarm for it
            ?LOG_ERROR(#{what => check_subdomain_name_failed, subdomain => Subdomain}),
            false
    end.

-spec report_subdomains_collision(mongoose_subdomain_core:subdomain_info(),
                                  mongoose_subdomain_core:subdomain_info()) -> ok.
report_subdomains_collision(#{subdomain := Subdomain} = _ExistingSubdomainInfo,
                            _NewSubdomainInfo) ->
    %% TODO: this is critical collision, and it must be reported properly
    %% think about adding some metric, so devops can set some alarm for it
    ?LOG_ERROR(#{what => subdomains_collision, subdomain => Subdomain}),
    ok.

-spec is_subdomain(subdomain_pattern(),
                   Domain :: mongooseim:domain_name(),
                   Subdomain :: mongooseim:domain_name()) -> boolean().
is_subdomain({fqdn, FQDN}, _, Subdomain) ->
    FQDN =:= Subdomain;
is_subdomain({prefix, Prefix}, Domain, Subdomain) ->
    Subdomain =:= <<Prefix/binary, Domain/binary>>.
