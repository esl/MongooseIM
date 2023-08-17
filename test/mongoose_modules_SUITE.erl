-module(mongoose_modules_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(MODS, [mod_a, mod_b, mod_c, mod_d]).

-define(HOST, <<"localhost">>).

all() ->
    [starts_and_stops_modules,
     ensures_module,
     reverts_config_when_module_fails_to_start,
     does_not_change_config_when_module_fails_to_stop,
     replaces_modules,
     replaces_modules_with_new_deps,
     replaces_modules_with_old_deps,
     replaces_modules_with_same_deps].

init_per_suite(C) ->
    mongoose_config:set_opts(opts()),
    C.

end_per_suite(_C) ->
    mongoose_config:erase_opts().

init_per_testcase(_TC, C) ->
    meck:new(gen_mod, [passthrough]),
    meck:expect(gen_mod, start_module, fun(_, _, _) -> {ok, start_result} end),
    meck:expect(gen_mod, stop_module, fun(_, _) -> ok end),
    meck:new(?MODS, [non_strict]),
    C.

end_per_testcase(_, _C) ->
    mongoose_config:unset_opt({modules, ?HOST}),
    meck:unload(gen_mod),
    meck:unload(?MODS).

starts_and_stops_modules(_Config) ->
    set_modules(Modules = #{mod_a => #{}, mod_b => [{opt, val}]}),
    ok = mongoose_modules:start(),
    check_started(maps:to_list(Modules)),

    ok = mongoose_modules:stop(),
    check_stopped([mod_a, mod_b]).

ensures_module(_Config) ->
    set_modules(#{}),
    ?assertEqual({started, start_result}, mongoose_modules:ensure_started(?HOST, mod_a, #{})),
    ?assertEqual(#{mod_a => #{}}, get_modules()),

    ?assertEqual(already_started, mongoose_modules:ensure_started(?HOST, mod_a, #{})),
    ?assertEqual(#{mod_a => #{}}, get_modules()),

    ?assertEqual({restarted, #{}, start_result},
                 mongoose_modules:ensure_started(?HOST, mod_a, #{opt => val})),
    ?assertEqual(#{mod_a => #{opt => val}}, get_modules()),

    ?assertEqual({stopped, #{opt => val}}, mongoose_modules:ensure_stopped(?HOST, mod_a)),
    ?assertEqual(#{}, get_modules()),

    ?assertEqual(already_stopped, mongoose_modules:ensure_stopped(?HOST, mod_a)),
    ?assertEqual(#{}, get_modules()).

reverts_config_when_module_fails_to_start(_Config) ->
    set_modules(#{}),
    meck:expect(gen_mod, start_module, fun(_, _, _) -> error(something_awful) end),
    ?assertError(something_awful, mongoose_modules:ensure_started(?HOST, mod_a, #{})),
    ?assertEqual(#{}, get_modules()).

does_not_change_config_when_module_fails_to_stop(_Config) ->
    set_modules(#{mod_a => #{}}),
    meck:expect(gen_mod, stop_module, fun(_, _) -> error(something_awful) end),
    ?assertError(something_awful, mongoose_modules:ensure_stopped(?HOST, mod_a)),
    ?assertEqual(#{mod_a => #{}}, get_modules()).

replaces_modules(_Config) ->
    set_modules(Modules = #{mod_a => #{}, mod_b => #{opt => val}, mod_c => #{}}),
    ok = mongoose_modules:start(),
    check_started(maps:to_list(Modules)),

    %% Stop mod_a, change opts for mod_b, do not change mod_c, start mod_d
    NewModules = #{mod_b => #{new_opt => new_val}, mod_c => #{}, mod_d => #{}},
    ok = mongoose_modules:replace_modules(?HOST, [mod_a], NewModules),
    check_stopped([mod_a, mod_b]),
    check_not_stopped([mod_c]),
    check_started([{mod_b, #{new_opt => new_val}}, {mod_d, #{}}]),
    ?assertEqual(NewModules, get_modules()),

    ok = mongoose_modules:stop(),
    check_stopped([mod_b, mod_c, mod_d]).

replaces_modules_with_new_deps(_Config) ->
    set_deps(#{mod_b => [{mod_c, #{}, hard}]}),
    set_modules(Modules = #{mod_a => #{}}),
    ok = mongoose_modules:start(),
    check_started(maps:to_list(Modules)),

    %% Start mod_b, which depends on mod_c
    ok = mongoose_modules:replace_modules(?HOST, [], #{mod_b => #{}}),
    check_not_stopped([mod_a]),
    check_started([{mod_b, #{}}, {mod_c, #{}}]),
    ?assertEqual(Modules#{mod_b => #{}, mod_c => #{}}, get_modules()),

    ok = mongoose_modules:stop(),
    check_stopped([mod_a, mod_b, mod_c]).

replaces_modules_with_old_deps(_Config) ->
    set_deps(#{mod_a => [{mod_c, #{}, hard}]}),
    set_modules(Modules = #{mod_a => #{}, mod_c => #{}}),
    ok = mongoose_modules:start(),
    check_started(maps:to_list(Modules)),

    %% Stop mod_a, which depends on mod_c, and start mod_b
    ok = mongoose_modules:replace_modules(?HOST, [mod_a], #{mod_b => #{}}),
    check_stopped([mod_a, mod_c]),
    check_started([{mod_b, #{}}]),
    ?assertEqual(#{mod_b => #{}}, get_modules()),

    ok = mongoose_modules:stop(),
    check_stopped([mod_b]).

replaces_modules_with_same_deps(_Config) ->
    set_deps(#{mod_a => [{mod_c, #{}, hard}], mod_b => [{mod_c, #{}, hard}]}),
    set_modules(Modules = #{mod_a => #{}, mod_c => #{}}),
    ok = mongoose_modules:start(),
    check_started(maps:to_list(Modules)),

    %% Stop mod_a, and start mod_b, both depending on mod_c
    ok = mongoose_modules:replace_modules(?HOST, [mod_a], #{mod_b => #{}}),
    check_stopped([mod_a]),
    check_not_stopped([mod_c]),
    check_started([{mod_b, #{}}]),
    ?assertEqual(#{mod_b => #{}, mod_c => #{}}, get_modules()),

    ok = mongoose_modules:stop(),
    check_stopped([mod_b]).

check_started(ModulesWithOpts) ->
    lists:foreach(fun({Mod, Opts}) ->
                          ?assert(meck:called(gen_mod, start_module, [?HOST, Mod, Opts]))
                  end, ModulesWithOpts).

check_stopped(Modules) ->
    lists:foreach(fun(Mod) ->
                          ?assert(meck:called(gen_mod, stop_module, [?HOST, Mod]))
                  end, Modules).

check_not_stopped(Modules) ->
    lists:foreach(fun(Mod) ->
                          ?assertNot(meck:called(gen_mod, stop_module, [?HOST, Mod]))
                  end, Modules).

check_modules(ExpectedModules) ->
    ?assertEqual(ExpectedModules, gen_mod:loaded_modules_with_opts(?HOST)).

opts() ->
    #{hosts => [?HOST], host_types => []}.

set_modules(Modules) ->
    mongoose_config:set_opt({modules, ?HOST}, Modules).

get_modules() ->
    mongoose_config:get_opt({modules, ?HOST}).

set_deps(DepsMap) ->
    maps:fold(fun(Mod, Deps, _) -> meck:expect(Mod, deps, fun(_, _) -> Deps end) end,
              undefined, DepsMap).
