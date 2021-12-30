-module(gen_mod_deps_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(MODS, [mod_a, mod_b, mod_c, mod_d]).

all() ->
    [
     no_dependencies,
     dependency,
     dependency_chain,
     dependency_dag,
     fails_on_dependency_cycle,
     succeeds_on_cycle_with_soft_dep_in_path,
     succeeds_on_adding_soft_dependency_cycle_edge,
     forces_dependency_args,
     appends_dependencies_args,
     optional_dependencies_are_not_started_by_themselves,
     optional_dependencies_add_arguments,
     overrides_dependency_args,
     merges_dependency_args
    ].

%% Fixtures

init_per_testcase(_, C) ->
    meck:new(?MODS, [non_strict]),
    C.

end_per_testcase(_, _C) ->
    meck:unload(?MODS).

%% Tests

no_dependencies(_Config) ->
    Modules = #{mod_a => [], mod_b => []},
    ?assertEqual(Modules, maps:from_list(add_deps(Modules))).

dependency(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}]}),
    ?assertMatch([{mod_b, _}, {mod_a, _}], add_deps(#{mod_a => []})).

dependency_chain(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}], mod_b => [{mod_c, hard}]}),
    ?assertMatch([{mod_c, _}, {mod_b, _}, {mod_a, _}], add_deps(#{mod_a => []})).

dependency_dag(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}, {mod_c, hard}],
               mod_b => [{mod_c, hard}]}),
    ?assertMatch([{mod_c, _}, {mod_b, _}, {mod_a, _}], add_deps(#{mod_a => []})).

fails_on_dependency_cycle(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}],
               mod_b => [{mod_c, hard}],
               mod_c => [{mod_a, hard}]}),
    ?assertError(_, add_deps(#{mod_a => []})).

succeeds_on_cycle_with_soft_dep_in_path(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}],
               mod_b => [{mod_c, soft}],
               mod_c => [{mod_a, hard}]}),
    ?assertMatch([{mod_b, _}, {mod_a, _}, {mod_c, _}], add_deps(#{mod_a => []})).

succeeds_on_adding_soft_dependency_cycle_edge(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}], mod_b => [{mod_a, soft}]}),
    ?assertMatch([{mod_b, _}, {mod_a, _}], add_deps(#{mod_a => []})).

forces_dependency_args(_Config) ->
    set_deps(#{mod_a => [{mod_b, [arg1, arg2], hard}]}),
    ?assertEqual([{mod_b, [arg1, arg2]}, {mod_a, []}], add_deps(#{mod_a => []})).

appends_dependencies_args(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}, {mod_c, [arg1], hard}],
               mod_b => [{mod_c, [arg2], hard}]}),
    ?assertEqual([{mod_c, [arg1, arg2]}, {mod_b, []}, {mod_a, []}], add_deps(#{mod_a => []})).

optional_dependencies_are_not_started_by_themselves(_Config) ->
    set_deps(#{mod_a => [{mod_b, optional}]}),
    ?assertMatch([{mod_a, _}], add_deps(#{mod_a => []})).

optional_dependencies_add_arguments(_Config) ->
    set_deps(#{mod_a => [{mod_b, [arg1], optional}], mod_c => [{mod_b, hard}]}),
    ?assertMatch([{mod_b, [arg1]}, {mod_a, _}, {mod_c, _}], add_deps(#{mod_a => [], mod_c => []})).

overrides_dependency_args(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}, {mod_c, [{arg, a}], hard}],
               mod_b => [{mod_c, [{arg, b}], hard}]}),
    ?assertMatch([{mod_c, [{arg, a}]}, {mod_b, _}, {mod_a, _}], add_deps(#{mod_a => []})).

merges_dependency_args(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}, {mod_c, [{arg, a}], hard}],
               mod_b => [{mod_c, [{arg, a}, arg2], hard}]}),
    ?assertMatch([{mod_c, [{arg, a}, arg2]}, {mod_b, _}, {mod_a, _}], add_deps(#{mod_a => []})).

%% Helpers

set_deps(DepsMap) ->
    maps:fold(fun(Mod, Deps, _) -> meck:expect(Mod, deps, fun(_, _) -> Deps end) end,
              undefined, DepsMap).

add_deps(Modules) ->
    gen_mod_deps:add_deps(<<"host">>, Modules).
