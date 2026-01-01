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
    {ok, [host_type_info(HostType) || HostType <- HostTypes]};
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
    #{<<"name">> => HostType,
      <<"domains">> => Domains,
      <<"modules">> => [module_info(HostType, Module, Opts) || {Module, Opts} <- Modules]}.

module_info(HostType, Module, Opts) ->
    #{<<"name">> => atom_to_binary(Module, utf8),
      <<"backend">> => backend_info(HostType, Module, Opts)}.

backend_info(HostType, Module, Opts) ->
    Configured = configured_backend(Opts),
    Runtime = runtime_backend(HostType, Module),
    case {Configured, Runtime} of
        {null, null} ->
            null;
        _ ->
            #{<<"configured">> => Configured,
              <<"runtime">> => Runtime}
    end.

configured_backend(Opts) ->
    case maps:get(backend, Opts, undefined) of
        undefined -> null;
        Backend -> atom_to_binary(Backend, utf8)
    end.

runtime_backend(HostType, Module) ->
    try mongoose_backend:get_backend_name(HostType, Module) of
        Backend -> atom_to_binary(Backend, utf8)
    catch
        error:badarg -> null
    end.
