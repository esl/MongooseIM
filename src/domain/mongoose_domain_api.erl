%% Main module other parts of MongooseIM should use to access the domain
%% management.
-module(mongoose_domain_api).

-include("mongoose_logger.hrl").

-export([get_host_type/1]).

%% external domain API for GraphQL or REST handlers
-export([insert_domain/2,
         delete_domain/2,
         request_delete_domain/2,
         disable_domain/1,
         enable_domain/1,
         get_domain_details/1,
         check_host_type_and_get_domains/1]).

%% external domain admin API for GraphQL or REST handlers
-export([set_domain_password/2,
         delete_domain_password/1]).

%% domain API
-export([get_domain_host_type/1,
         get_all_static/0,
         get_domains_by_host_type/1,
         get_all_domains/0]).

%% domain admin API
-export([check_domain_password/2]).

%% subdomain API
-export([register_subdomain/3,
         unregister_subdomain/2,
         get_subdomain_host_type/1,
         get_subdomain_info/1,
         get_all_subdomains_for_domain/1]).

%% Helper for remove_domain
-export([remove_domain_wrapper/3,
         do_delete_domain_in_progress/2]).

%% For testing
-export([get_all_dynamic/0]).
-ignore_xref([get_all_static/0, get_all_dynamic/0]).

-type status() :: enabled | disabled | deleting.
-type domain() :: jid:lserver().
-type host_type() :: mongooseim:host_type().
-type subdomain_pattern() :: mongoose_subdomain_utils:subdomain_pattern().
-type remove_domain_acc() :: #{failed := [module()]}.

-type domain_info() :: #{domain := domain(), host_type => host_type(), status => status()}.

-type insert_result() :: {ok, domain_info()} |
                         {static | unknown_host_type | duplicate, iodata()}.
-type delete_result() :: {ok, domain_info()} |
                         {static | unknown_host_type | not_found | wrong_host_type, iodata()}.
-type set_status_result() :: {ok, domain_info()} |
                             {static | unknown_host_type | not_found | deleted, iodata()}.
-type get_domains_result() :: {ok, [domain()]} | {unknown_host_type, iodata()}.
-type get_domain_details_result() :: {ok, domain_info()} | {static | not_found, iodata()}.

-export_type([status/0, remove_domain_acc/0]).

-spec insert_domain(domain(), host_type()) -> insert_result().
insert_domain(Domain, HostType) ->
    M = #{domain => Domain, host_type => HostType},
    fold(M, [fun check_domain/1, fun check_host_type/1,
             fun do_insert_domain/1, fun force_check/1, fun return_domain/1]).

-spec delete_domain(domain(), host_type()) -> delete_result().
delete_domain(Domain, HostType) ->
    delete_domain(Domain, HostType, sync).

-spec request_delete_domain(domain(), host_type()) -> delete_result().
request_delete_domain(Domain, HostType) ->
    delete_domain(Domain, HostType, async).

-spec delete_domain(domain(), host_type(), sync | async) -> delete_result().
delete_domain(Domain, HostType, RequestType) ->
    M = #{domain => Domain, host_type => HostType, request_type => RequestType},
    fold(M, [fun check_domain/1, fun check_host_type/1, fun set_domain_for_deletion/1,
             fun force_check/1, fun do_delete_domain/1, fun return_domain/1]).

-spec disable_domain(domain()) -> set_status_result().
disable_domain(Domain) ->
    M = #{domain => Domain, status => disabled},
    fold(M, [fun check_domain/1, fun set_status/1, fun force_check/1, fun return_domain/1]).

-spec enable_domain(domain()) -> set_status_result().
enable_domain(Domain) ->
    M = #{domain => Domain, status => enabled},
    fold(M, [fun check_domain/1, fun set_status/1, fun force_check/1, fun return_domain/1]).

-spec check_host_type_and_get_domains(host_type()) -> get_domains_result().
check_host_type_and_get_domains(HostType) ->
    M = #{host_type => HostType},
    fold(M, [fun check_host_type/1, fun get_domains/1]).

-spec get_domain_details(domain()) -> get_domain_details_result().
get_domain_details(Domain) ->
    M = #{domain => Domain},
    fold(M, [fun check_domain/1, fun select_domain/1, fun return_domain/1]).

check_domain(M = #{domain := Domain}) ->
    case mongoose_domain_core:is_static(Domain) of
        true ->
            {static, <<"Domain is static">>};
        false ->
            M
    end.

check_host_type(M = #{host_type := HostType}) ->
    case mongoose_domain_core:is_host_type_allowed(HostType) of
        true ->
            M;
        false ->
            {unknown_host_type, <<"Unknown host type">>}
    end.

select_domain(M = #{domain := Domain}) ->
    case mongoose_domain_sql:select_domain(Domain) of
        {ok, DomainDetails} ->
            maps:merge(M, DomainDetails);
        {error, not_found} ->
            {not_found, <<"Given domain does not exist">>}
    end.

do_insert_domain(M = #{domain := Domain, host_type := HostType}) ->
    case mongoose_domain_sql:insert_domain(Domain, HostType) of
        ok ->
            M;
        {error, duplicate} ->
            {duplicate, <<"Domain already exists">>}
    end.

set_domain_for_deletion(M = #{domain := Domain, host_type := HostType}) ->
    case mongoose_domain_sql:set_domain_for_deletion(Domain, HostType) of
        ok ->
            M;
        {error, wrong_host_type} ->
            {wrong_host_type, <<"Wrong host type was provided">>};
        {error, not_found} ->
            {not_found, <<"Given domain does not exist">>}
    end.

force_check(M) ->
    service_domain_db:force_check_for_updates(),
    M.

do_delete_domain(M = #{domain := Domain, host_type := HostType, request_type := RequestType}) ->
    mongoose_domain_sql:delete_domain_admin(Domain),
    case RequestType of
        sync ->
            do_delete_domain_in_progress(Domain, HostType),
            M#{status => deleted};
        async ->
            mongoose_domain_db_cleaner:request_delete_domain(Domain, HostType),
            M#{status => deleting}
    end.

set_status(M = #{domain := Domain, status := Status}) ->
    case mongoose_domain_sql:set_status(Domain, Status) of
        {error, unknown_host_type} ->
            {unknown_host_type, <<"Unknown host type">>};
        {error, domain_deleted} ->
            {deleted, <<"Domain has been deleted">>};
        {error, not_found} ->
            {not_found, <<"Given domain does not exist">>};
        ok ->
            M
    end.

return_domain(M) ->
    {ok, maps:with([domain, host_type, status], M)}.

get_domains(#{host_type := HostType}) ->
    {ok, get_domains_by_host_type(HostType)}.

-spec do_delete_domain_in_progress(domain(), host_type()) -> ok.
do_delete_domain_in_progress(Domain, HostType) ->
    #{failed := []} = mongoose_hooks:remove_domain(HostType, Domain),
    ok = mongoose_domain_sql:delete_domain(Domain, HostType).

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

-spec get_all_domains() -> [domain_info()].
get_all_domains() ->
    mongoose_domain_sql:select_all_domains().

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

-spec set_domain_password(domain(), password()) ->  {ok | not_found, iodata()}.
set_domain_password(Domain, Password) ->
    case get_host_type(Domain) of
        {ok, _} ->
            ok = mongoose_domain_sql:set_domain_admin(Domain, Password),
            {ok, <<"Domain password set successfully">>};
        {error, not_found} ->
            {not_found, <<"Given domain does not exist or is disabled">>}
    end.

-spec delete_domain_password(domain()) -> {ok, iodata()}.
delete_domain_password(Domain) ->
    case mongoose_domain_sql:delete_domain_admin(Domain) of
        ok ->
            {ok, <<"Domain password deleted successfully">>};
        {error, not_found} ->
            {not_found, <<"Domain password does not exist">>}
    end.

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

-spec remove_domain_wrapper(remove_domain_acc(), fun(() -> remove_domain_acc()), module()) ->
    {ok | stop, remove_domain_acc()}.
remove_domain_wrapper(Acc, F, Module) ->
    try F() of
        Acc -> {ok, Acc}
    catch C:R:S ->
        ?LOG_ERROR(#{what => hook_failed,
                     text => <<"Error running hook">>,
                     module => Module,
                     class => C, reason => R, stacktrace => S}),
        {stop, Acc#{failed := [Module | maps:get(failed, Acc)]}}
    end.

fold({_, _} = Result, _) ->
    Result;
fold(M, [Step | Rest]) when is_map(M) ->
    fold(Step(M), Rest).
