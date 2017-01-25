-module(ejabberd_hooks_SUITE).
-compile([export_all]).

-define(HOST, <<"localhost">>).

all() ->
    [ a_fun_can_be_added,
      a_module_fun_can_be_added,
      a_fun_can_be_removed,
      a_module_fun_can_be_removed,

      hooks_run_launches_nullary_fun,
      hooks_run_launches_unary_fun,
      hooks_run_ignores_different_arity_funs,
      hooks_run_stops_when_fun_returns_stop,

      hooks_run_fold_folds_with_unary_fun,
      hooks_run_fold_folds_with_binary_fun,
      hooks_run_fold_passes_acc_along,
      hooks_run_fold_stops_when_fun_returns_stop,
      hooks_run_fold_preserves_order,

      error_in_run_fold_is_ignored,
      throw_in_run_fold_is_ignored,
      exit_in_run_fold_is_ignored
    ].

init_per_suite(C) ->
    application:ensure_all_started(exometer),
    C.

end_per_suite(_C) ->
    application:stop(exometer),
    application:stop(exometer_core).

a_fun_can_be_added(_) ->
    given_hooks_started(),

    % when
    ejabberd_hooks:add(test_run_hook, ?HOST, fun(_) -> ok end, 1),

    % then
    [{{test_run_hook,<<"localhost">>}, [{1,undefined,_}]}] = get_hooks().


a_module_fun_can_be_added(_) ->
    given_hooks_started(),
    given_module(hook_mod, fun_a, fun(_) -> ok end),

    % when
    ejabberd_hooks:add(test_run_hook, ?HOST, hook_mod, fun_a, 1),

    % then
    [{{test_run_hook,<<"localhost">>}, [{1,hook_mod,fun_a}]}] = get_hooks().


a_fun_can_be_removed(_) ->
    given_hooks_started(),
    GivenFun = fun(_) -> ok end,
    ejabberd_hooks:add(test_run_hook, ?HOST, GivenFun, 1),

    % when
    ejabberd_hooks:delete(test_run_hook, ?HOST, GivenFun, 1),

    % then
    [{{test_run_hook,<<"localhost">>}, []}] = get_hooks().

a_module_fun_can_be_removed(_) ->
    given_hooks_started(),
    given_module(hook_mod, fun_nullary, fun() -> success0 end),
    given_hook_added(test_run_hook, hook_mod, fun_nullary, 1),

    % when
    ejabberd_hooks:delete(test_run_hook, ?HOST, hook_mod, fun_nullary, 1),

    % then
    [{{test_run_hook,<<"localhost">>}, []}] = get_hooks().


hooks_run_launches_nullary_fun(_) ->
    given_hooks_started(),
    given_module(hook_mod, fun_nullary, fun() -> success0 end),
    given_hook_added(test_run_hook, hook_mod, fun_nullary, 1),

    %% when
    ejabberd_hooks:run(test_run_hook, ?HOST, []),

    %% then
    [{_,{hook_mod,fun_nullary,[]}, success0}] = meck:history(hook_mod).

hooks_run_launches_unary_fun(_) ->
    given_hooks_started(),
    given_module(hook_mod, fun_onearg, fun(oneval) -> success1 end),
    given_hook_added(test_run_hook, hook_mod, fun_onearg, 1),

    %% when
    ejabberd_hooks:run(test_run_hook, ?HOST, [oneval]),

    %% then
    [{_,{hook_mod,fun_onearg,[oneval]}, success1}] = meck:history(hook_mod).

hooks_run_ignores_different_arity_funs(_) ->
    given_hooks_started(),
    given_module(hook_mod, fun_onearg, fun(unused) -> never_return end),
    given_fun(hook_mod, fun_twoarg, fun(one,two)-> success2 end),

    given_hook_added(test_run_hook, hook_mod, fun_onearg, 1),
    given_hook_added(test_run_hook, hook_mod, fun_twoarg, 1),

    %% when
    ejabberd_hooks:run(test_run_hook, ?HOST, [one, two]),

    %% then
    [{_,{hook_mod,fun_twoarg,[one,two]}, success2}] = meck:history(hook_mod).

hooks_run_stops_when_fun_returns_stop(_) ->
    given_hooks_started(),
    given_module(hook_mod, a_fun, const(stop)),
    given_fun(hook_mod, another_fun, const(success)),

    given_hook_added(test_run_hook, hook_mod, a_fun, 1),
    given_hook_added(test_run_hook, hook_mod, another_fun, 2),

    %% when
    ejabberd_hooks:run(test_run_hook, ?HOST, [x]),

    %% then
    [{_,{hook_mod,a_fun,[x]}, stop}] = meck:history(hook_mod).


hooks_run_fold_folds_with_unary_fun(_) ->
    given_hooks_started(),
    given_module(hook_mod, unary_folder, fun(initial) -> done end),
    given_hook_added(test_fold_hook, hook_mod, unary_folder, 1),

    %% when
    ejabberd_hooks:run_fold(test_fold_hook, ?HOST, initial, []),

    %% then
    [{_,{hook_mod,unary_folder,[initial]}, done}] = meck:history(hook_mod).

hooks_run_fold_folds_with_binary_fun(_) ->
    given_hooks_started(),
    given_module(hook_mod, binary_folder, fun(initial, arg1) -> done end),
    given_hook_added(test_fold_hook, hook_mod, binary_folder, 1),

    %% when
    ejabberd_hooks:run_fold(test_fold_hook, ?HOST, initial, [arg1]),

    %% then
    [{_,{hook_mod,binary_folder,[initial, arg1]}, done}] = meck:history(hook_mod).

hooks_run_fold_passes_acc_along(_) ->
    given_hooks_started(),
    given_module(hook_mod1, first_folder, fun(N, Const) -> N+Const end),
    given_module(hook_mod2, second_folder, fun(N, Const) -> N-Const*2 end),

    given_hook_added(test_fold_hook, hook_mod1, first_folder, 1),
    given_hook_added(test_fold_hook, hook_mod2, second_folder, 2),

    %% when
    R = ejabberd_hooks:run_fold(test_fold_hook, ?HOST, 0, [10]),

    %% then
    -10 = R.

hooks_run_fold_stops_when_fun_returns_stop(_) ->
    given_hooks_started(),
    given_module(hook_mod1, stopper, const(stop)),
    given_module(hook_mod2, folder, const(continue)),

    given_hook_added(test_fold_hook, hook_mod1, stopper, 1),
    given_hook_added(test_fold_hook, hook_mod2, folder, 2),

    %% when
    R = ejabberd_hooks:run_fold(test_fold_hook, ?HOST, continue, []),

    %% then
    [{_,{hook_mod1,stopper,[continue]}, stop}] = meck:history(hook_mod1),
    [] = meck:history(hook_mod2),
    stopped = R.


hooks_run_fold_preserves_order(_) ->
    given_hooks_started(),
    given_module(hook_mod1, first_folder, const(1)),
    given_module(hook_mod2, second_folder, const(2)),

    given_hook_added(test_fold_hook, hook_mod1, first_folder, 1),
    given_hook_added(test_fold_hook, hook_mod2, second_folder, 2),

    %% when
    R = ejabberd_hooks:run_fold(test_fold_hook, ?HOST, 0, []),

    %% then
    2 = R.


error_in_run_fold_is_ignored(_) ->
    given_hooks_started(),

    given_module(failing_mod, broken, fun(_) -> error(broken) end),
    given_hook_added(test_fold_hook, failing_mod, broken, 1),

    given_module(working_mod, good, const(i_was_run)),
    given_hook_added(test_fold_hook, working_mod, good, 2),

    %% when
    R = ejabberd_hooks:run_fold(test_fold_hook, ?HOST, initial, []),

    %% then
    i_was_run = R,
    [{_Pid, {failing_mod,broken,[initial]}, error,broken, _Stacktrace}] =
        meck:history(failing_mod).


throw_in_run_fold_is_ignored(_) ->
    given_hooks_started(),

    given_module(throwing_mod, throwing_fun, fun(_) -> throw(ball) end),
    given_hook_added(test_fold_hook, throwing_mod, throwing_fun, 1),

    given_module(working_mod, good, fun(X) -> X end),
    given_hook_added(test_fold_hook, working_mod, good, 2),

    %% when
    R = ejabberd_hooks:run_fold(test_fold_hook, ?HOST, initial, []),

    %% then
    initial = R,
    [{_Pid, {throwing_mod,throwing_fun,[initial]}, throw, ball, _ST}] =
        meck:history(throwing_mod).


exit_in_run_fold_is_ignored(_) ->
    given_hooks_started(),

    given_module(exiting_mod, exiting_fun,
                 fun(_) -> meck:exception(exit,oops) end),
    given_hook_added(test_fold_hook, exiting_mod, exiting_fun, 1),

    given_module(working_mod, good, fun(X) -> X end),
    given_hook_added(test_fold_hook, working_mod, good, 2),

    %% when
    R = ejabberd_hooks:run_fold(test_fold_hook, ?HOST, initial, []),

    %% then
    initial = R,
    [{_Pid, {exiting_mod,exiting_fun,[initial]}, exit, oops, _ST}] =
        meck:history(exiting_mod).




%% Givens
const(N) -> fun(_) -> N end.

given_hooks_started() ->
    error_logger:tty(false),
    Fun = fun(all_metrics_are_global) -> false end,
    given_module(ejabberd_config, get_local_option, Fun),
    ejabberd_hooks:start_link().

given_hook_added(HookName, ModName, FunName, Prio) ->
    ejabberd_hooks:add(HookName, ?HOST, ModName, FunName, Prio).

given_module(ModName, FunName, Fun) ->
    catch meck:unload(ModName),
    meck:new(ModName, [non_strict]),
    meck:expect(ModName, FunName, Fun).

given_fun(ModName, FunName, Fun) ->
    meck:expect(ModName, FunName, Fun).


get_hooks() ->
    ets:tab2list(hooks).
