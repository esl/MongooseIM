-module(gen_mod_deps_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(MODS, [mod_a, mod_b, mod_c, mod_d]).

all() ->
    [
     starts_modules,
     starts_dependencies,
     starts_dependency_chain,
     starts_dependency_dag,
     fails_on_dependency_cycle,
     succeeds_on_cycle_with_soft_dep_in_path,
     succeeds_on_adding_soft_dependency_cycle_edge,
     forces_dependency_args,
     appends_dependencies_args,
     optional_dependencies_are_not_started_by_themselves,
     optional_dependencies_add_arguments,
     overrides_dependency_args,
     merges_dependency_args,
     replaces_modules,
     reloads_modules_with_changed_args
    ].

%% Fixtures

init_per_testcase(_, C) ->
    meck:new(gen_mod, [passthrough]),
    meck:new(?MODS, [non_strict]),
    meck:expect(gen_mod, start_module,  fun(_, _, _) -> ok end),
    meck:expect(gen_mod, stop_module,   fun(_, _)    -> ok end),
    meck:expect(gen_mod, reload_module, fun(_, _, _) -> ok end),
    meck:expect(gen_mod, get_deps, fun(Host, Mod, Opts) -> meck:passthrough([Host, Mod, Opts]) end),
    C.

end_per_testcase(_, C) ->
    meck:unload(gen_mod),
    meck:unload(?MODS),
    C.

%% Tests

starts_modules(_Config) ->
    gen_mod_deps:start_modules(<<"host">>, [{mod_a, []}, {mod_b, []}]),
    check_started([mod_a, mod_b]).


starts_dependencies(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}, {mod_c, hard}]}),
    gen_mod_deps:start_modules(<<"host">>, [{mod_a, []}]),
    check_started([mod_a, mod_b, mod_c]).


starts_dependency_chain(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}], mod_b => [{mod_c, hard}]}),
    gen_mod_deps:start_modules(<<"host">>, [{mod_a, []}]),

    check_started([mod_a, mod_b, mod_c]),

    StartOrder = [Mod || {_, {_, start_module, [_, Mod, _]}, _} <- meck:history(gen_mod)],
    ?assertEqual([mod_c, mod_b, mod_a], StartOrder).


starts_dependency_dag(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}, {mod_c, hard}],
               mod_b => [{mod_c, hard}]}),
    gen_mod_deps:start_modules(<<"host">>, [{mod_a, []}]),
    check_started([mod_a, mod_b, mod_c]).


fails_on_dependency_cycle(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}],
               mod_b => [{mod_c, hard}],
               mod_c => [{mod_a, hard}]}),
    ?assertError(_, gen_mod_deps:start_modules(<<"host">>, [{mod_a, []}])).


succeeds_on_cycle_with_soft_dep_in_path(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}],
               mod_b => [{mod_c, soft}],
               mod_c => [{mod_a, hard}]}),
    gen_mod_deps:start_modules(<<"host">>, [{mod_a, []}]),
    check_started([mod_a, mod_b, mod_c]).


succeeds_on_adding_soft_dependency_cycle_edge(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}], mod_b => [{mod_a, soft}]}),
    gen_mod_deps:start_modules(<<"host">>, [{mod_a, []}]),
    check_started([mod_a, mod_b]).


forces_dependency_args(_Config) ->
    set_deps(#{mod_a => [{mod_b, [arg1, arg2], hard}]}),
    gen_mod_deps:start_modules(<<"host">>, [{mod_a, []}]),
    check_started({mod_b, [arg1, arg2]}).


appends_dependencies_args(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}, {mod_c, [arg1], hard}],
               mod_b => [{mod_c, [arg2], hard}]}),
    gen_mod_deps:start_modules(<<"host">>, [{mod_a, []}]),
    check_started({mod_c, [arg1, arg2]}).


optional_dependencies_are_not_started_by_themselves(_Config) ->
    set_deps(#{mod_a => [{mod_b, optional}]}),
    gen_mod_deps:start_modules(<<"host">>, [{mod_a, []}]),
    ?assertNot(started(mod_b)).


optional_dependencies_add_arguments(_Config) ->
    set_deps(#{mod_a => [{mod_b, [arg1], optional}], mod_c => [{mod_b, hard}]}),
    gen_mod_deps:start_modules(<<"host">>, [{mod_a, []}, {mod_c, []}]),
    check_started({mod_b, [arg1]}).


overrides_dependency_args(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}, {mod_c, [{arg, a}], hard}],
               mod_b => [{mod_c, [{arg, b}], hard}]}),
    gen_mod_deps:start_modules(<<"host">>, [{mod_a, []}]),

    ?assert(meck:called(gen_mod, start_module, ['_', mod_c, [{arg, a}]]) orelse
            meck:called(gen_mod, start_module, ['_', mod_c, [{arg, b}]])).


merges_dependency_args(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}, {mod_c, [{arg, a}], hard}],
               mod_b => [{mod_c, [{arg, a}, arg2], hard}]}),
    gen_mod_deps:start_modules(<<"host">>, [{mod_a, []}]),

    check_started({mod_c, [{arg, a}, arg2]}).


replaces_modules(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}, {mod_c, hard}]}),
    gen_mod_deps:start_modules(<<"host">>, [{mod_a, []}]),

    meck:reset(gen_mod),

    gen_mod_deps:replace_modules(<<"host">>, [{mod_a, []}], [{mod_d, []}, {mod_b, []}]),

    check_stopped(mod_a),
    ?assertNot(meck:called(gen_mod, replace_module, '_')),
    check_started(mod_d).


reloads_modules_with_changed_args(_Config) ->
    set_deps(#{mod_a => [{mod_b, hard}]}),
    gen_mod_deps:start_modules(<<"host">>, [{mod_a, []}]),
    gen_mod_deps:replace_modules(<<"host">>, [{mod_a, []}], [{mod_b, [arg]}]),
    check_reloaded({mod_b, [arg]}).

%% Helpers

set_deps(DepsMap) ->
    maps:fold(fun(Mod, Deps, _) -> meck:expect(Mod, deps, fun(_, _) -> Deps end) end,
              undefined, DepsMap).


check_started([]) -> ok;
check_started([ModNArgs | Mods]) ->
    ?assert(started(ModNArgs)),
    check_started(Mods);
check_started(Mod) ->
    check_started([Mod]).

started({Mod, Args}) ->
    meck:called(gen_mod, start_module, ['_', Mod, Args]);
started(Mod) ->
    meck:called(gen_mod, start_module, ['_', Mod, '_']).

check_stopped([]) -> ok;
check_stopped([Mod | Mods]) ->
    ?assert(meck:called(gen_mod, stop_module, ['_', Mod])),
    check_stopped(Mods);
check_stopped(Mod) ->
    check_stopped([Mod]).


check_reloaded([]) -> ok;
check_reloaded([{Mod, Args} | Mods]) ->
    ?assert(meck:called(gen_mod, reload_module, ['_', Mod, Args])),
    check_reloaded(Mods);
check_reloaded([Mod | Mods]) ->
    ?assert(meck:called(gen_mod, reload_module, ['_', Mod, '_'])),
    check_reloaded(Mods);
check_reloaded(Mod) ->
    check_reloaded([Mod]).
