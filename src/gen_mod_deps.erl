%%==============================================================================
%% Copyright 2016 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(gen_mod_deps).

-include("mongoose.hrl").

-type hardness() :: soft | hard | optional.
-type module_opts() :: gen_mod:module_opts().
-type module_dep() :: {module(), module_opts(), hardness()}.
-type module_deps() :: [module_dep()].
-type deps() :: [module_dep() | {service, mongoose_service:service()}].
-type module_list() :: [{module(), module_opts()}].
-type module_map() :: #{module() => module_opts()}.

-export([add_deps/2, resolve_deps/2, sort_deps/2]).

-ignore_xref([add_deps/2]).

-export_type([hardness/0, module_list/0, module_map/0, module_deps/0, deps/0]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc Adds deps into module list.
%% Side-effect free.
-spec add_deps(mongooseim:host_type(), module_map() | module_list()) -> module_list().
add_deps(HostType, Modules) ->
    sort_deps(HostType, resolve_deps(HostType, Modules)).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

%% Resolving dependencies

%% @doc
%% Determines all modules to start, along with their options.
%%
%% NOTE: A dependency will not be discarded during resolving, e.g.
%% if the resolver processes dependencies in order:
%%
%% deps(mod_a, #{}) -> [{mod_b, #{}}, {mod_c, #{}}]
%% deps(mod_parent, #{}) -> [{mod_a, #{opt => val}}]
%%
%% then the dependency for mod_a will be reevaluated with new opts and might return:
%%
%% deps(mod_a, #{opt := val}) -> [{mod_c, #{}}]
%%
%% In this case, mod_b will still be started.
%% @end
-spec resolve_deps(mongooseim:host_type(), module_map() | module_list()) -> module_map().
resolve_deps(HostType, Modules) when is_map(Modules) ->
    resolve_deps(HostType, lists:sort(maps:to_list(Modules)));
resolve_deps(HostType, ModuleQueue) ->
    resolve_deps(HostType, ModuleQueue, #{}, #{}).

-spec resolve_deps(mongooseim:host_type(),
                   ModuleQueue :: [{module(), module_opts()} | module_dep()],
                   OptionalMods :: module_map(),
                   Acc :: module_map()) -> module_map().
resolve_deps(HostType, [], OptionalMods, KnownModules) ->
    KnownModNames = maps:keys(KnownModules),
    case maps:with(KnownModNames, OptionalMods) of
        NewQueueMap when map_size(NewQueueMap) > 0 ->
            resolve_deps(HostType, maps:to_list(NewQueueMap),
                         maps:without(KnownModNames, OptionalMods), KnownModules);
        _Nothing ->
            KnownModules
    end;
resolve_deps(HostType, [{Module, Opts, optional} | ModuleQueue], OptionalMods, KnownModules) ->
    resolve_deps(HostType, ModuleQueue, maps:put(Module, Opts, OptionalMods), KnownModules);
resolve_deps(HostType, [{Module, Opts, _Hardness} | ModuleQueue], OptionalMods, KnownModules) ->
    resolve_deps(HostType, [{Module, Opts} | ModuleQueue], OptionalMods, KnownModules);
resolve_deps(HostType, [{Module, Opts} | ModuleQueue], OptionalMods, KnownModules)
  when is_list(Opts); is_map(Opts) ->
    NewOpts =
        case maps:find(Module, KnownModules) of
            {ok, PreviousOpts} ->
                case merge_opts(Module, PreviousOpts, Opts) of
                    PreviousOpts -> undefined;
                    MergedOpts -> MergedOpts
                end;
            error ->
                Opts
        end,
    case NewOpts of
        undefined -> resolve_deps(HostType, ModuleQueue, OptionalMods, KnownModules);
        _ ->
            Deps = gen_mod:get_deps(HostType, Module, NewOpts),
            UpdatedQueue = Deps ++ ModuleQueue,
            UpdatedKnownModules = maps:put(Module, NewOpts, KnownModules),
            resolve_deps(HostType, UpdatedQueue, OptionalMods, UpdatedKnownModules)
    end.

%% @doc
%% Merges module opts prioritizing the new ones, and warns on overrides.
%% @end
-spec merge_opts(module(), module_opts(), module_opts()) -> module_opts().
merge_opts(Module, PreviousOpts, Opts) when is_map(PreviousOpts), is_map(Opts) ->
    case changed_opts(PreviousOpts, Opts) of
        [] ->
            ok;
        Changed ->
            ?LOG_WARNING(#{what => overriding_options, module => Module, options => Changed})
    end,
    maps:merge(PreviousOpts, Opts).

-spec changed_opts(module_opts(), module_opts()) -> [map()].
changed_opts(PreviousOpts, Opts) ->
    lists:flatmap(
      fun({Key, OldValue}) ->
              case maps:find(Key, Opts) of
                  error -> [];
                  {ok, OldValue} -> [];
                  {ok, NewValue} -> [#{key => Key, old_value => OldValue, new_value => NewValue}]
              end
      end, maps:to_list(PreviousOpts)).

%% Sorting resolved dependencies

-spec sort_deps(mongooseim:host_type(), module_map()) -> module_list().
sort_deps(HostType, ModuleMap) ->
    DepsGraph = digraph:new([acyclic, private]),

    try
        maps:fold(
          fun(Module, Opts, _) ->
                  process_module_dep(HostType, Module, Opts, DepsGraph)
          end,
          undefined, ModuleMap),

        lists:filtermap(
          fun(Module) ->
                  case maps:find(Module, ModuleMap) of
                      error -> false;
                      {ok, Opts} -> {true, {Module, Opts}}
                  end
          end,
          digraph_utils:topsort(DepsGraph))
    after
        digraph:delete(DepsGraph)
    end.

-spec process_module_dep(mongooseim:host_type(), module(), module_opts(), digraph:graph()) -> ok.
process_module_dep(HostType, Module, Opts, DepsGraph) ->
    digraph:add_vertex(DepsGraph, Module),
    lists:foreach(
      fun({DepModule, _, DepHardness}) -> process_dep(Module, DepModule, DepHardness, DepsGraph) end,
      gen_mod:get_deps(HostType, Module, Opts)).

-spec process_dep(Module :: module(), DepModule :: module(),
                  DepHardness :: hardness(), Graph :: digraph:graph()) -> ok.
process_dep(Module, DepModule, DepHardness, Graph) ->
    digraph:add_vertex(Graph, DepModule),
    case {digraph:add_edge(Graph, DepModule, Module, DepHardness), DepHardness} of
        {['$e' | _], _} ->
            ok;

        {{error, {bad_edge, CyclePath}}, hard} ->
            case find_soft_edge(Graph, CyclePath) of
                false ->
                    ?LOG_CRITICAL(#{what => resolving_dependencies_aborted,
                                    text => <<"Module dependency cycle found">>,
                                    cycle_path => CyclePath}),
                    error({dependency_cycle, CyclePath});

                {EdgeId, B, A, _} ->
                    ?LOG_INFO(#{what => soft_module_dependency_cycle_detected,
                                text => <<"Soft module dependency cycle detected. "
                                          "Dropping edge">>, edge => {A, B},
                                cyclepath => CyclePath}),

                    digraph:del_edge(Graph, EdgeId),
                    ['$e' | _] = digraph:add_edge(Graph, DepModule, Module, hard),
                    ok
            end;

        {{error, {bad_edge, CyclePath}}, _Soft} ->
            ?LOG_INFO(#{what => soft_module_dependency_cycle_detected,
                        text => <<"Soft module dependency cycle detected. "
                                  "Dropping edge">>, edge => {Module, DepModule},
                        cyclepath => CyclePath}),
            ok
    end.

-spec find_soft_edge(digraph:graph(), [digraph:vertex()]) ->
                            {digraph:edge(), digraph:vertex(),
                             digraph:vertex(), digraph:label()} | false.
find_soft_edge(Graph, CyclePath) ->
    VerticePairs = lists:zip(CyclePath, tl(CyclePath) ++ [hd(CyclePath)]),
    Edges = lists:filtermap(
              fun({A, B}) ->
                      case find_edge(Graph, A, B) of
                          false -> false;
                          Edge -> {true, digraph:edge(Graph, Edge)}
                      end
              end,
              VerticePairs),

    case lists:keyfind(optional, 4, Edges) of
        false -> lists:keyfind(soft, 4, Edges);
        Edge -> Edge
    end.

-spec find_edge(digraph:graph(), digraph:vertex(), digraph:vertex()) -> digraph:edge() | false.
find_edge(Graph, A, B) ->
    OutEdges = ordsets:from_list(digraph:out_edges(Graph, A)),
    InEdges = ordsets:from_list(digraph:in_edges(Graph, B)),

    case ordsets:intersection(OutEdges, InEdges) of
        [Edge] -> Edge;
        [] -> false
    end.
