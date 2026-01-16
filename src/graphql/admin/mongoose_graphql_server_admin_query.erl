-module(mongoose_graphql_server_admin_query).
-behaviour(mongoose_graphql).

-export([execute/4]).

-ignore_xref([execute/4]).

-include("../mongoose_graphql_types.hrl").


execute(_Ctx, server, <<"status">>, _) ->
    {ok, {Status, Message, Version, CommitHash}} = mongoose_server_api:status(),
    {ok, #{<<"statusCode">> => status_code(Status), <<"message">> => Message,
           <<"version">> => Version, <<"commitHash">> => CommitHash}};
execute(_Ctx, server, <<"hostTypes">>, _) ->
    HostTypes = lists:sort(all_host_types()),
    {ok, [{ok, host_type_info(HostType)} || HostType <- HostTypes]};
execute(_Ctx, server, <<"globalInfo">>, _) ->
    Services = get_loaded_services(),
    InternalDatabases = get_internal_databases(),
    {ok, #{<<"services">> => [{ok, #{<<"name">> => atom_to_binary(S, utf8)}} || S <- Services],
           <<"internalDatabases">> => [{ok, atom_to_binary(DB, utf8)} || DB <- InternalDatabases]}};
execute(_Ctx, server, <<"getLoglevel">>, _) ->
    mongoose_server_api:get_loglevel();
execute(_Ctx, server, <<"getCookie">>, _) ->
    mongoose_server_api:get_cookie().

status_code(true) -> <<"RUNNING">>;
status_code(false) -> <<"NOT_RUNNING">>.

all_host_types() ->
    mongoose_config:get_opt(hosts) ++ mongoose_config:get_opt(host_types).

host_type_info(HostType) ->
    Domains = lists:sort(mongoose_domain_api:get_domains_by_host_type(HostType)),
    ModulesWithOpts = gen_mod:loaded_modules_with_opts(HostType),
    Modules = lists:keysort(1, maps:to_list(ModulesWithOpts)),
    AuthMethods = get_auth_methods(HostType),
    #{<<"name">> => HostType,
    <<"domains">> => [{ok, D} || D <- Domains],
    <<"modules">> => [{ok, module_info(HostType, Module, Opts)} || {Module, Opts} <- Modules],
    <<"authMethods">> => [{ok, M} || M <- AuthMethods]}.

get_auth_methods(HostType) ->
    try mongoose_config:get_opt([{auth, HostType}, methods]) of
        Methods when is_list(Methods) ->
            [atom_to_binary(M, utf8) || M <- Methods]
    catch
        _:_ -> []
    end.

get_loaded_services() ->
    ServicesWithOpts = mongoose_service:loaded_services_with_opts(),
    lists:sort(maps:keys(ServicesWithOpts)).

get_internal_databases() ->
    InternalDatabasesWithOpts = mongoose_config:get_opt(internal_databases),
    lists:sort(maps:keys(InternalDatabasesWithOpts)).

module_info(_, Module, Opts) ->
    #{<<"name">> => atom_to_binary(Module, utf8),
      <<"backend">> => configured_backend(Opts)}.

configured_backend(Opts) ->
    case maps:get(backend, Opts, undefined) of
        undefined -> null;
        Backend -> atom_to_binary(Backend, utf8)
    end.

