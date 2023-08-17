-module(gen_hook_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(HOOK_TAG1, global).
-define(HOOK_TAG2, <<"some tag">>).

-define(assertEqualLists(L1, L2), ?assertEqual(lists:sort(L1), lists:sort(L2))).

all() ->
    [single_handler_can_be_added_and_removed,
     multiple_handlers_can_be_added_and_removed,

     local_fun_references_causes_error,
     anonymous_fun_references_causes_error,
     not_exported_external_fun_references_causes_error,
     invalid_hook_handler_parameters_causes_error,

     run_fold_executes_handlers_in_the_right_order,
     run_fold_stops_when_handler_returns_stop,

     errors_in_handlers_are_reported_but_ignored].

init_per_suite(Config) ->
    application:ensure_all_started(exometer_core),
    mongoose_config:set_opts(#{all_metrics_are_global => false}),
    Config.

end_per_suite(Config) ->
    mongoose_config:erase_opts(),
    application:stop(exometer_core),
    Config.

init_per_testcase(_, Config) ->
    mongooseim_helper:start_link_loaded_hooks(),
    Config.

end_per_testcase(_, Config) ->
    meck:unload(),
    Config.


%%----------------------------------------------------------------
%% test cases
%%----------------------------------------------------------------
single_handler_can_be_added_and_removed(_) ->
    meck:new([mod1, mod2], [non_strict]),
    PlusHandlerFn = get_hook_handler(mod1, plus, fun hook_handler_plus/3),
    MultiplyHandlerFn = get_hook_handler(mod2, multiply, fun hook_handler_multiply/3),
    %% check that there are no hook handlers added yet
    ?assertEqual([], get_handlers_for_all_hooks()),
    %% add various hook handlers
    ?assertEqual(ok, gen_hook:add_handler(calculate, ?HOOK_TAG1, MultiplyHandlerFn,
                                          #{id => 2}, 2)),
    ?assertEqual(ok, gen_hook:add_handler(calculate, ?HOOK_TAG1, PlusHandlerFn,
                                          #{id => 1}, 1)),
    ?assertEqual(ok, gen_hook:add_handler(calculate, ?HOOK_TAG2, PlusHandlerFn,
                                          #{id => 1}, 1)),
    %% check that hook handlers are added
    Tag1Handlers = [%% this list must be sorted by priority
                    {hook_handler, 1, PlusHandlerFn,
                     #{hook_name => calculate, hook_tag => ?HOOK_TAG1, id => 1}},
                    {hook_handler, 2, MultiplyHandlerFn,
                     #{hook_name => calculate, hook_tag => ?HOOK_TAG1, id => 2}}],
    AllHandlers = [{{calculate, ?HOOK_TAG1}, Tag1Handlers},
                   {{calculate, ?HOOK_TAG2},
                    [{hook_handler, 1, PlusHandlerFn,
                      #{hook_name => calculate, hook_tag => ?HOOK_TAG2,
                        host_type =>?HOOK_TAG2, id => 1}}]}],
    ?assertEqualLists(AllHandlers, get_handlers_for_all_hooks()),
    %% try to add some hook handler second time and check that nothing has changed
    ?assertEqual(ok, gen_hook:add_handler(calculate, ?HOOK_TAG1, MultiplyHandlerFn,
                                          #{id => 2}, 2)),
    ?assertEqualLists(AllHandlers, get_handlers_for_all_hooks()),
    %% try to remove hook handler for ?HOOK_TAG2 and check that it's removed
    ?assertEqual(ok, gen_hook:delete_handler(calculate, ?HOOK_TAG2, PlusHandlerFn,
                                             #{id => 1}, 1)),
    ?assertEqualLists([{{calculate, ?HOOK_TAG1}, Tag1Handlers},
                       {{calculate, ?HOOK_TAG2}, []}],
                      get_handlers_for_all_hooks()),
    %% try to remove hook handler for ?HOOK_TAG2 second time
    %% and check that nothing has changed
    ?assertEqual(ok, gen_hook:delete_handler(calculate, ?HOOK_TAG2, PlusHandlerFn,
                                             #{id => 1}, 1)),
    ?assertEqualLists([{{calculate, ?HOOK_TAG1}, Tag1Handlers},
                       {{calculate, ?HOOK_TAG2}, []}],
                      get_handlers_for_all_hooks()),
    %% try to remove hook handlers for ?HOOK_TAG1 and check that they are removed
    ?assertEqual(ok, gen_hook:delete_handler(calculate, ?HOOK_TAG1, MultiplyHandlerFn,
                                             #{id => 2}, 2)),
    ?assertEqual(ok, gen_hook:delete_handler(calculate, ?HOOK_TAG1, PlusHandlerFn,
                                             #{id => 1}, 1)),
    ?assertEqualLists([{{calculate, ?HOOK_TAG1}, []}, {{calculate, ?HOOK_TAG2}, []}],
                      get_handlers_for_all_hooks()).

multiple_handlers_can_be_added_and_removed(_) ->
    meck:new([mod1, mod2], [non_strict]),
    PlusHandlerFn = get_hook_handler(mod1, plus, fun hook_handler_plus/3),
    MultiplyHandlerFn = get_hook_handler(mod2, multiply, fun hook_handler_multiply/3),
    %% check that there are no hook handlers added yet
    ?assertEqual([], get_handlers_for_all_hooks()),
    %% add various hook handlers
    HookHandlers = [{calculate, ?HOOK_TAG1, MultiplyHandlerFn, #{id => 2}, 2},
                    {calculate, ?HOOK_TAG2, PlusHandlerFn, #{id => 1}, 1},
                    {calculate, ?HOOK_TAG1, PlusHandlerFn, #{id => 1}, 1}],
    ?assertEqual(ok, gen_hook:add_handlers(HookHandlers)),
    %% check that hook handlers are added
    Tag1Handlers = [%% this list must be sorted by priority
                    {hook_handler, 1, PlusHandlerFn,
                     #{hook_name => calculate, hook_tag => ?HOOK_TAG1, id => 1}},
                    {hook_handler, 2, MultiplyHandlerFn,
                     #{hook_name => calculate, hook_tag => ?HOOK_TAG1, id => 2}}],
    AllHandlers = [{{calculate, ?HOOK_TAG1}, Tag1Handlers},
                   {{calculate, ?HOOK_TAG2},
                    [{hook_handler, 1, PlusHandlerFn,
                      #{hook_name => calculate, hook_tag => ?HOOK_TAG2,
                        host_type =>?HOOK_TAG2, id => 1}}]}],
    ?assertEqualLists(AllHandlers, get_handlers_for_all_hooks()),
    %% try to add hook handlers second time and check that nothing has changed
    ?assertEqual(ok, gen_hook:add_handlers(HookHandlers)),
    ?assertEqualLists(AllHandlers, get_handlers_for_all_hooks()),
    %% try to remove hook handlers and check that they are removed
    ?assertEqual(ok, gen_hook:delete_handlers(HookHandlers)),
    ?assertEqualLists([{{calculate, ?HOOK_TAG1}, []}, {{calculate, ?HOOK_TAG2}, []}],
                      get_handlers_for_all_hooks()),
    %% try to remove hook handlers second time and check that nothing has changed
    ?assertEqual(ok, gen_hook:delete_handlers(HookHandlers)),
    ?assertEqualLists([{{calculate, ?HOOK_TAG1}, []}, {{calculate, ?HOOK_TAG2}, []}],
                      get_handlers_for_all_hooks()).

local_fun_references_causes_error(_) ->
    meck:new([mod1, mod2], [non_strict]),
    PlusHandlerFn = get_hook_handler(mod1, plus, fun hook_handler_plus/3),
    MultiplyHandlerFn = get_hook_handler(mod2, multiply, fun hook_handler_multiply/3),
    %% check that there are no hook handlers added yet
    ?assertEqual([], get_handlers_for_all_hooks()),
    %% try to add multiple hook handlers, when one of them uses local function reference
    LocalFunctionReference = fun hook_handler_plus/3,
    HookHandlers = [{calculate, ?HOOK_TAG1, MultiplyHandlerFn, #{id => 2}, 2},
                    {calculate, ?HOOK_TAG2, LocalFunctionReference, #{id => 1}, 1},
                    {calculate, ?HOOK_TAG1, PlusHandlerFn, #{id => 1}, 1}],
    ?assertError(#{what := only_external_function_references_allowed,
                   function := LocalFunctionReference},
                 gen_hook:add_handlers(HookHandlers)),
    %% check that handlers in the list are partially added (till error occurs)
    ?assertEqual([{{calculate, ?HOOK_TAG1},
                   [{hook_handler, 2, MultiplyHandlerFn,
                     #{hook_name => calculate, hook_tag => ?HOOK_TAG1, id => 2}}]}],
                 get_handlers_for_all_hooks()),
    %% try to remove the same list of handlers
    ?assertError(#{what := only_external_function_references_allowed,
                   function := LocalFunctionReference},
                 gen_hook:delete_handlers(HookHandlers)),
    %% check that partially added handlers are removed
    ?assertEqual([{{calculate, ?HOOK_TAG1}, []}], get_handlers_for_all_hooks()).

anonymous_fun_references_causes_error(_) ->
    %% check that there are no hook handlers added yet
    ?assertEqual([], get_handlers_for_all_hooks()),
    %% try to add hook handler using anonymous function reference
    AnonymousFunctionReference = fun(Acc, _, _) -> {ok, Acc} end,
    ?assertError(#{what := only_external_function_references_allowed,
                   function := AnonymousFunctionReference},
                 gen_hook:add_handler(calculate, ?HOOK_TAG1, AnonymousFunctionReference,
                                      #{id => 2}, 2)),
    %% check that nothing is added
    ?assertEqual([], get_handlers_for_all_hooks()).

not_exported_external_fun_references_causes_error(_) ->
    %% check that there are no hook handlers added yet
    ?assertEqual([], get_handlers_for_all_hooks()),
    %% try to add hook handler using function reference for a missing module
    NotExportedExternalFunctionReference1 = fun missing_module:missing_function/3,
    ?assertError(#{what := module_is_not_loaded, module := missing_module},
                 gen_hook:add_handler(calculate, ?HOOK_TAG1,
                                      NotExportedExternalFunctionReference1,
                                      #{id => 2}, 2)),
    %% try to add hook handler using function reference for a missing module
    NotExportedExternalFunctionReference2 = fun ?MODULE:missing_function/3,
    ?assertError(#{what := function_is_not_exported,
                   function := NotExportedExternalFunctionReference2},
                 gen_hook:add_handler(calculate, ?HOOK_TAG1,
                                      NotExportedExternalFunctionReference2,
                                      #{id => 2}, 2)),
    %% check that nothing is added
    ?assertEqual([], get_handlers_for_all_hooks()).

invalid_hook_handler_parameters_causes_error(_) ->
    %% check that there are no hook handlers added yet
    ?assertEqual([], get_handlers_for_all_hooks()),
    HandlerFn = fun ?MODULE:hook_handler_stop/3,
    InvalidHookHandlers = [{calculate, ?HOOK_TAG1, HandlerFn, invalid_extra_param, 2},
                           {<<"invalid hook name">>, ?HOOK_TAG1, HandlerFn, #{}, 2},
                           {calculate, ?HOOK_TAG1, HandlerFn, #{}, invalid_priority},
                           {calculate, invalid_hook_tag, HandlerFn, #{}, 2}],
    [?assertError(function_clause, gen_hook:add_handlers([HookHandler]))
     || HookHandler <- InvalidHookHandlers],
    ?assertEqual([], get_handlers_for_all_hooks()).

run_fold_executes_handlers_in_the_right_order(_) ->
    meck:new(mod1, [non_strict]),
    PlusHandlerFn = get_hook_handler(mod1, plus, fun hook_handler_plus/3),
    MultiplyHandlerFn = get_hook_handler(mod1, multiply, fun hook_handler_multiply/3),
    %% check that there are no hook handlers added yet
    ?assertEqual([], get_handlers_for_all_hooks()),
    %% add various hook handlers
    HookHandlers = [{calculate, ?HOOK_TAG1, MultiplyHandlerFn, #{n => 5}, 5},
                    {calculate, ?HOOK_TAG1, MultiplyHandlerFn, #{}, 2},
                    {calculate, ?HOOK_TAG1, PlusHandlerFn, #{n => 3}, 1},
                    {calculate, ?HOOK_TAG1, PlusHandlerFn, #{}, 4}],
    ?assertEqual(ok, gen_hook:add_handlers(HookHandlers)),
    %% run the hook
    N = (((0 + 3) * 2) + 2) * 5, %% 40
    ?assertEqual({ok, N}, gen_hook:run_fold(calculate, ?HOOK_TAG1, 0, #{n => 2})),
    %% check hook handlers execution sequence
    Self = self(),
    ?assertEqual([{Self,
                   {mod1, plus, [0, #{n => 2}, #{hook_name => calculate, n => 3,
                                                 hook_tag => ?HOOK_TAG1}]},
                   {ok, 3}},
                  {Self,
                   {mod1, multiply, [3, #{n => 2}, #{hook_name => calculate,
                                                     hook_tag => ?HOOK_TAG1}]},
                   {ok, 6}},
                  {Self,
                   {mod1, plus, [6, #{n => 2}, #{hook_name => calculate,
                                                 hook_tag => ?HOOK_TAG1}]},
                   {ok, 8}},
                  {Self,
                   {mod1, multiply, [8, #{n => 2}, #{hook_name => calculate, n => 5,
                                                     hook_tag => ?HOOK_TAG1}]},
                   {ok, 40}}],
                 meck:history(mod1)).

run_fold_stops_when_handler_returns_stop(_) ->
    meck:new(mod1, [non_strict]),
    PlusHandlerFn = get_hook_handler(mod1, plus, fun hook_handler_plus/3),
    StopHandlerFn = get_hook_handler(mod1, stop, fun hook_handler_stop/3),
    MultiplyHandlerFn = get_hook_handler(mod1, multiply, fun hook_handler_multiply/3),
    %% check that there are no hook handlers added yet
    ?assertEqual([], get_handlers_for_all_hooks()),
    %% add various hook handlers
    HookHandlers = [{calculate, ?HOOK_TAG1, MultiplyHandlerFn, #{n => 5}, 5},
                    {calculate, ?HOOK_TAG1, MultiplyHandlerFn, #{}, 2},
                    {calculate, ?HOOK_TAG1, PlusHandlerFn, #{n => 3}, 1},
                    {calculate, ?HOOK_TAG1, PlusHandlerFn, #{}, 4},
                    {calculate, ?HOOK_TAG1, StopHandlerFn, #{}, 3}],
    ?assertEqual(ok, gen_hook:add_handlers(HookHandlers)),
    %% run the hook
    N = ((0 + 3) * 2), %% 6
    ?assertEqual({stop, N}, gen_hook:run_fold(calculate, ?HOOK_TAG1, 0, #{n => 2})),
    %% check hook handlers execution sequence
    Self = self(),
    ?assertEqual([{Self,
                   {mod1, plus, [0, #{n => 2}, #{hook_name => calculate, n => 3,
                                                 hook_tag => ?HOOK_TAG1}]},
                   {ok, 3}},
                  {Self,
                   {mod1, multiply, [3, #{n => 2}, #{hook_name => calculate,
                                                     hook_tag => ?HOOK_TAG1}]},
                   {ok, 6}},
                  {Self,
                   {mod1, stop, [6, #{n => 2}, #{hook_name => calculate,
                                                 hook_tag => ?HOOK_TAG1}]},
                   {stop, 6}}],
                 meck:history(mod1)).

errors_in_handlers_are_reported_but_ignored(_) ->
    meck:new(mod1, [non_strict]),
    meck:new(gen_hook, [passthrough]),
    PlusHandlerFn = get_hook_handler(mod1, plus, fun hook_handler_plus/3),
    ErrorHandlerFn = get_hook_handler(mod1, error, fun hook_handler_error/3),
    MultiplyHandlerFn = get_hook_handler(mod1, multiply, fun hook_handler_multiply/3),
    %% check that there are no hook handlers added yet
    ?assertEqual([], get_handlers_for_all_hooks()),
    %% add various hook handlers
    HookHandlers = [{calculate, ?HOOK_TAG1, MultiplyHandlerFn, #{n => 5}, 5},
                    {calculate, ?HOOK_TAG1, MultiplyHandlerFn, #{}, 2},
                    {calculate, ?HOOK_TAG1, PlusHandlerFn, #{n => 3}, 1},
                    {calculate, ?HOOK_TAG1, PlusHandlerFn, #{}, 4},
                    {calculate, ?HOOK_TAG1, ErrorHandlerFn, #{}, 3}],
    ?assertEqual(ok, gen_hook:add_handlers(HookHandlers)),
    %% run the hook
    N = (((0 + 3) * 2) + 2) * 5, %% 40
    ?assertEqual({ok, N}, gen_hook:run_fold(calculate, ?HOOK_TAG1, 0, #{n => 2})),
    %% check that error is reported
    ?assertEqual(true, meck:called(gen_hook, error_running_hook,
                                   [#{class => error, reason => some_error, stacktrace => '_'},
                                    {hook_handler, 3, ErrorHandlerFn,
                                     #{hook_name => calculate, hook_tag => ?HOOK_TAG1}},
                                    6, #{n => 2}, {calculate, ?HOOK_TAG1}])),
    %% check hook handlers execution sequence
    Self = self(),
    ?assertMatch([{Self,
                   {mod1, plus, [0, #{n := 2}, #{hook_name := calculate, n := 3,
                                                 hook_tag := ?HOOK_TAG1}]},
                   {ok, 3}},
                  {Self,
                   {mod1, multiply, [3, #{n := 2}, #{hook_name := calculate,
                                                     hook_tag := ?HOOK_TAG1}]},
                   {ok, 6}},
                  {Self,
                   {mod1, error, [6, #{n := 2}, #{hook_name := calculate,
                                                  hook_tag := ?HOOK_TAG1}]},
                   error, some_error, _},
                  {Self,
                   {mod1, plus, [6, #{n := 2}, #{hook_name := calculate,
                                                 hook_tag := ?HOOK_TAG1}]},
                   {ok, 8}},
                  {Self,
                   {mod1, multiply, [8, #{n := 2}, #{hook_name := calculate, n := 5,
                                                     hook_tag := ?HOOK_TAG1}]},
                   {ok, 40}}],
                 meck:history(mod1)).

%%----------------------------------------------------------------
%% helper functions
%%----------------------------------------------------------------
hook_handler_plus(Acc, _, #{n := N}) ->
    {ok, Acc + N};
hook_handler_plus(Acc, #{n := N}, _) ->
    {ok, Acc + N}.

hook_handler_multiply(Acc, _, #{n := N}) ->
    {ok, Acc * N};
hook_handler_multiply(Acc, #{n := N}, _) ->
    {ok, Acc * N}.

hook_handler_stop(Acc, _, _) ->
    {stop, Acc}.

hook_handler_error(_, _, _) ->
    error(some_error).

get_hook_handler(ModName, FunName, Fun) when is_function(Fun, 3) ->
    meck:expect(ModName, FunName, Fun),
    fun ModName:FunName/3.

get_handlers_for_all_hooks() ->
    maps:to_list(persistent_term:get(gen_hook, #{})).
