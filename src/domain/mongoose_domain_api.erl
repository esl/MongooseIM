%% Main module other parts of MongooseIM should use to access the domain
%% management.
-module(mongoose_domain_api).

-export([init/1,
         insert_domain/2,
         remove_domain/1,
         disable_domain/1,
         enable_domain/1,
         get_host_type/1,
         get_all_locked/0,
         get_domains_by_host_type/1]).

-type domain() :: jid:lserver().
-type host_type() :: jid:lserver().
-type pair() :: {domain(), host_type()}.

%% Init - on init all the “enabled” domain names from the persistent storage
%% must be added to the core MIM component described above.
%% Domains should be nameprepped using `jid:nameprep'
-spec init([pair()]) -> ok | {error, term()}.
init(Pairs) ->
    mongoose_domain_core:start(Pairs).

%% Add domain name (w/ host type) - This function must be idempotent.
%% Added domain is always “enabled” by default it must be added in the core
%% MIM component described in the previous section.
%% If it’s successfully enabled than Information about the domain name
%% must be added into persistent storage and distributed across all the nodes
%% in the cluster.
%% Domain and HostType should be nameprepped using `jid:nameprep'
-spec insert_domain(domain(), host_type()) ->
    ok  | {error, duplicate} | {error, {db_error, term()}}
    | {error, service_disabled}.
insert_domain(Domain, HostType) ->
    case mongoose_domain_core:is_locked(Domain) of
        true ->
            {error, locked};
        false ->
            case service_domain_db:enabled() of
                true ->
                    check_db(mongoose_domain_sql:insert_domain(Domain, HostType));
                false ->
                    {error, service_disabled}
            end
    end.

%% Removal the domain name - This function must be idempotent.
%% domain name must be removed from the core MIM component (if required)
%% and from the DB. this action must be distributed across
%% all the nodes in the cluster.
%% Returns ok, if domain not found.
%% Domain should be nameprepped using `jid:nameprep'.
-spec remove_domain(domain()) ->
    ok | {error, locked} | {error, {db_error, term()}}
    | {error, service_disabled}.
remove_domain(Domain) ->
    case mongoose_domain_core:is_locked(Domain) of
        true ->
            {error, locked};
        false ->
            case service_domain_db:enabled() of
                true ->
                    check_db(mongoose_domain_sql:remove_domain(Domain));
                false ->
                    {error, service_disabled}
            end
    end.

%% Disabling/Enabling domain name - This function must be idempotent.
%% the status of the existing domain must be changed.
%% If domain name is enabled, then it must be added in the core MIM component.
%% On disabling domain name must be removed from the core MIM component.
%% Change of the status must be distributed across all the nodes in the cluster.
-spec disable_domain(domain()) ->
    ok | {error, not_found} | {error, locked} | {error, duplicate}
    | {error, service_disabled}.
disable_domain(Domain) ->
    case mongoose_domain_core:is_locked(Domain) of
        true ->
            {error, locked};
        false ->
            case service_domain_db:enabled() of
                true ->
                    check_db(mongoose_domain_sql:disable_domain(Domain));
                false ->
                    {error, service_disabled}
            end
    end.

-spec enable_domain(domain()) ->
    ok | {error, not_found} | {error, locked} | {error, duplicate}
    | {error, service_disabled}.
enable_domain(Domain) ->
    case mongoose_domain_core:is_locked(Domain) of
        true ->
            {error, locked};
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
-spec get_all_locked() -> [domain()].
get_all_locked() ->
    mongoose_domain_core:get_all_locked().

%% Get the list of the host\_types provided during initialisation
%% This has complexity N, where N is the number of online domains.
-spec get_domains_by_host_type(host_type()) -> [domain()].
get_domains_by_host_type(HostType) ->
    mongoose_domain_core:get_domains_by_host_type(HostType).
