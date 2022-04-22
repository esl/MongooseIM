%% @doc Service dependency management

-module(mongoose_service_deps).

-export([resolve_deps/1, sort_deps/1]).

-type service_list() :: mongoose_service:service_list().
-type service_map() :: mongoose_service:service_map().

%% @doc Add dependencies to the service map
-spec resolve_deps(service_map()) -> service_map().
resolve_deps(Services) when is_map(Services) ->
    resolve_deps(maps:to_list(Services), #{}).

-spec resolve_deps(service_list(), service_map()) -> service_map().
resolve_deps([], Acc) ->
    Acc;
resolve_deps([{Service, Opts} | Rest], Acc) ->
    {DepsToResolve, NewAcc} =
        case Acc of
            #{Service := PreviousOpts} ->
                {[], Acc#{Service => merge_opts(PreviousOpts, Opts)}};
            #{} ->
                Deps = [{Dep, #{}} || Dep <- mongoose_service:get_deps(Service)],
                {Deps, Acc#{Service => Opts}}
        end,
    resolve_deps(DepsToResolve ++ Rest, NewAcc).

%% Deps always have no options, so one argument has to be an empty map
merge_opts(#{}, Opts) -> Opts;
merge_opts(Opts, #{}) -> Opts.

%% @doc Sort services (which already include their dependencies) according to the dependency graph
-spec sort_deps(service_map()) -> service_list().
sort_deps(Services) when is_map(Services) ->
    DepsGraph = digraph:new([acyclic, private]),
    try
        [add_service_with_deps(Service, DepsGraph) || Service <- maps:keys(Services)],
        [{Service, maps:get(Service, Services)} || Service <- digraph_utils:topsort(DepsGraph)]
    after
        digraph:delete(DepsGraph)
    end.

add_service_with_deps(Service, DepsGraph) ->
    digraph:add_vertex(DepsGraph, Service),
    lists:foreach(fun(DepService) -> add_service_dep(Service, DepService, DepsGraph) end,
                  mongoose_service:get_deps(Service)).

add_service_dep(Service, DepService, DepsGraph) ->
    digraph:add_vertex(DepsGraph, DepService),
    case digraph:add_edge(DepsGraph, DepService, Service) of
        {error, Error} ->
            {bad_edge, CyclePath} = Error,
            error(#{what => resolving_dependencies_aborted,
                    text => <<"Service dependency cycle found">>,
                    service => Service,
                    dep_service => DepService,
                    cycle_path => CyclePath});
        _Edge ->
            ok
    end.
