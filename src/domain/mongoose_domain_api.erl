%% Main module other parts of MongooseIM should use to access the domain
%% management.
-module(mongoose_domain_api).

-export([init/0,
         stop/0,
         get_host_type/1]).

%% domain API
-export([insert_domain/2,
         delete_domain/2,
         disable_domain/1,
         enable_domain/1,
         get_domain_host_type/1,
         get_all_static/0,
         get_domains_by_host_type/1]).

%% domain admin API
-export([check_domain_password/2,
         set_domain_password/2,
         delete_domain_password/1]).

%% subdomain API
-export([register_subdomain/3,
         unregister_subdomain/2,
         get_subdomain_host_type/1,
         get_subdomain_info/1,
         get_all_subdomains_for_domain/1]).

%% For testing
-export([get_all_dynamic/0]).

-ignore_xref([get_all_static/0]).
-ignore_xref([get_all_dynamic/0]).
-ignore_xref([stop/0]).

-type status() :: enabled | disabled | deleting.
-type domain() :: jid:lserver().
-type host_type() :: mongooseim:host_type().
-type subdomain_pattern() :: mongoose_subdomain_utils:subdomain_pattern().
-export_type([status/0]).

-spec init() -> ok | {error, term()}.
init() ->
    mongoose_domain_core:start(),
    mongoose_subdomain_core:start(),
    mongoose_lazy_routing:start().

%% Stops gen_servers, that are started from init/0
%% Does not fail, even if servers are already stopped
-spec stop() -> ok.
stop() ->
    catch mongoose_domain_core:stop(),
    catch mongoose_subdomain_core:stop(),
    catch mongoose_lazy_routing:stop(),
    ok.

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

-type delete_domain_return() ::
    ok | {error, static} | {error, unknown_host_type} | {error, service_disabled}
    | {error, {db_error, term()}} | {error, wrong_host_type} | {error, {modules_failed, [module()]}}.

%% Returns ok, if domain not found.
%% Domain should be nameprepped using `jid:nameprep'.
-spec delete_domain(domain(), host_type()) -> delete_domain_return().
delete_domain(Domain, HostType) ->
    case check_domain(Domain, HostType) of
        ok ->
            Res0 = check_db(mongoose_domain_sql:set_domain_for_deletion(Domain, HostType)),
            case Res0 of
                ok ->
                    delete_domain_password(Domain),
                    do_delete_domain_in_progress(Domain, HostType);
                Other ->
                    Other
            end;
        Other ->
            Other
    end.

%% This is ran only in the context of `do_delete_domain',
%% so it can already skip some checks
-spec do_delete_domain_in_progress(domain(), host_type()) -> delete_domain_return().
do_delete_domain_in_progress(Domain, HostType) ->
    case mongoose_hooks:remove_domain(HostType, Domain) of
        #{failed := []} ->
            check_db(mongoose_domain_sql:delete_domain(Domain, HostType));
        #{failed := Failed} ->
            {error, {modules_failed, Failed}}
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
    case get_domain_host_type(Domain) of
        {ok, HostType} -> {ok, HostType};
        {error, not_found} ->
            get_subdomain_host_type(Domain)
    end.

%% Domain should be nameprepped using `jid:nameprep'
-spec get_domain_host_type(domain()) ->
    {ok, host_type()} | {error, not_found}.
get_domain_host_type(Domain) ->
    mongoose_domain_core:get_host_type(Domain).

%% Subdomain should be nameprepped using `jid:nameprep'
-spec get_subdomain_host_type(domain()) ->
    {ok, host_type()} | {error, not_found}.
get_subdomain_host_type(Subdomain) ->
    mongoose_subdomain_core:get_host_type(Subdomain).

%% Subdomain should be nameprepped using `jid:nameprep'
-spec get_subdomain_info(domain()) ->
    {ok, mongoose_subdomain_core:subdomain_info()} | {error, not_found}.
get_subdomain_info(Subdomain) ->
    mongoose_subdomain_core:get_subdomain_info(Subdomain).

%% Get the list of the host_types provided during initialisation
%% This has complexity N, where N is the number of online domains.
-spec get_all_static() -> [{domain(), host_type()}].
get_all_static() ->
    mongoose_domain_core:get_all_static().

%% Get domains, loaded from DB to this node
-spec get_all_dynamic() -> [{domain(), host_type()}].
get_all_dynamic() ->
    mongoose_domain_core:get_all_dynamic().

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

-type password() :: binary().

-spec check_domain_password(domain(), password()) -> ok | {error, wrong_password | not_found}.
check_domain_password(Domain, Password) ->
    case mongoose_domain_sql:select_domain_admin(Domain) of
        {ok, {Domain, PassDetails}} ->
            case do_check_domain_password(Password, PassDetails) of
                true ->
                    ok;
                false ->
                    {error, wrong_password}
            end;
        {error, not_found} ->
            {error, not_found}
    end.

do_check_domain_password(Password, PassDetails) ->
    case mongoose_scram:deserialize(PassDetails) of
        {ok, Scram} ->
            mongoose_scram:check_password(Password, Scram);
        {error, _Reason} ->
            false
    end.

-spec set_domain_password(domain(), password()) -> ok | {error, not_found}.
set_domain_password(Domain, Password) ->
    case get_host_type(Domain) of
        {ok, _} ->
            mongoose_domain_sql:set_domain_admin(Domain, Password);
        {error, not_found} ->
            {error, not_found}
    end.

-spec delete_domain_password(domain()) -> ok.
delete_domain_password(Domain) ->
    mongoose_domain_sql:delete_domain_admin(Domain).

-spec register_subdomain(host_type(), subdomain_pattern(),
                         mongoose_packet_handler:t()) ->
                            ok | {error, already_registered | subdomain_already_exists}.
register_subdomain(HostType, SubdomainPattern, PacketHandler) ->
    mongoose_subdomain_core:register_subdomain(HostType, SubdomainPattern,
                                               PacketHandler).

-spec unregister_subdomain(host_type(), subdomain_pattern()) -> ok.
unregister_subdomain(HostType, SubdomainPattern) ->
    mongoose_subdomain_core:unregister_subdomain(HostType, SubdomainPattern).

-spec get_all_subdomains_for_domain(domain()) ->
          [mongoose_subdomain_core:subdomain_info()].
get_all_subdomains_for_domain(Domain) ->
    mongoose_subdomain_core:get_all_subdomains_for_domain(Domain).
