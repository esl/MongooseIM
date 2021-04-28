%% Main module other parts of MongooseIM should use to access the domain
%% management.
-module(mongoose_domain_api).

-export([init/0,
         insert_domain/2,
         delete_domain/2,
         disable_domain/1,
         enable_domain/1,
         get_host_type/1,
         get_all_static/0,
         get_domains_by_host_type/1]).

-type domain() :: jid:lserver().
-type host_type() :: mongooseim:host_type().
-type pair() :: {domain(), host_type()}.


-spec init() -> ok | {error, term()}.
init() ->
    Pairs = get_static_pairs(),
    AllowedHostTypes = ejabberd_config:get_global_option_or_default(host_types, []),
    mongoose_domain_core:start(Pairs, AllowedHostTypes).

%% Domain should be nameprepped using `jid:nameprep'.
-spec insert_domain(domain(), host_type()) ->
    ok  | {error, duplicate} | {error, static} | {error, {db_error, term()}}
    | {error, service_disabled} | {error, unknown_host_type}.
insert_domain(Domain, HostType) ->
    case check_domain(Domain, HostType) of
        ok ->
            check_db(mongoose_domain_sql:insert_domain(Domain, HostType));
        Other ->
            Other
    end.

%% Returns ok, if domain not found.
%% Domain should be nameprepped using `jid:nameprep'.
-spec delete_domain(domain(), host_type()) ->
    ok | {error, static} | {error, {db_error, term()}}
    | {error, service_disabled} | {error, wrong_host_type} | {error, unknown_host_type}.
delete_domain(Domain, HostType) ->
    case check_domain(Domain, HostType) of
        ok ->
            check_db(mongoose_domain_sql:delete_domain(Domain, HostType));
        Other ->
            Other
    end.

-spec disable_domain(domain()) ->
    ok | {error, not_found} | {error, static} | {error, service_disabled}
    | {error, {db_error, term()}}.
disable_domain(Domain) ->
    case mongoose_domain_core:is_static(Domain) of
        true ->
            {error, static};
        false ->
            case service_domain_db:enabled() of
                true ->
                    check_db(mongoose_domain_sql:disable_domain(Domain));
                false ->
                    {error, service_disabled}
            end
    end.

-spec enable_domain(domain()) ->
    ok | {error, not_found} | {error, static} | {error, service_disabled}
    | {error, {db_error, term()}}.
enable_domain(Domain) ->
    case mongoose_domain_core:is_static(Domain) of
        true ->
            {error, static};
        false ->
            case service_domain_db:enabled() of
                true ->
                    check_db(mongoose_domain_sql:enable_domain(Domain));
                false ->
                    {error, service_disabled}
            end
    end.

check_db(ok) ->
    %% Speedup the next check.  %% It's async.
    service_domain_db:force_check_for_updates(),
    ok;
check_db(Result) ->
    Result.

%% Domain should be nameprepped using `jid:nameprep'
-spec get_host_type(domain()) ->
    {ok, host_type()} | {error, not_found}.
get_host_type(Domain) ->
    mongoose_domain_core:get_host_type(Domain).

%% Get the list of the host_types provided during initialisation
%% This has complexity N, where N is the number of online domains.
-spec get_all_static() -> [{domain(), host_type()}].
get_all_static() ->
    mongoose_domain_core:get_all_static().

%% Get the list of the host_types provided during initialisation
%% This has complexity N, where N is the number of online domains.
-spec get_domains_by_host_type(host_type()) -> [domain()].
get_domains_by_host_type(HostType) ->
    mongoose_domain_core:get_domains_by_host_type(HostType).

check_domain(Domain, HostType) ->
    Static = mongoose_domain_core:is_static(Domain),
    Allowed = mongoose_domain_core:is_host_type_allowed(HostType),
    HasDb = service_domain_db:enabled(),
    if
        Static ->
            {error, static};
        not Allowed ->
            {error, unknown_host_type};
        not HasDb ->
            {error, service_disabled};
        true ->
            ok
    end.

%% Domains should be nameprepped using `jid:nameprep'
-spec get_static_pairs() -> [{domain(), host_type()}].
get_static_pairs() ->
    [{H, H} || H <- ejabberd_config:get_global_option_or_default(hosts, [])].
