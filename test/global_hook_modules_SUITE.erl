-module(global_hook_modules_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-define(HOST, <<"localhost">>).
-define(HOST2, <<"localhost2">>).

opts() ->
    [{hosts, [?HOST, ?HOST2]},
     {host_types, []},
     {all_metrics_are_global, true},
     {{modules, ?HOST}, #{mod_test_global_hook => #{}}},
     {{modules, ?HOST2}, #{mod_test_global_hook => #{}}}].

all() ->
    [global_hook_handler_remains_when_module_stops_for_one_host,
     global_hook_handler_removed_when_module_stops_for_all_hosts].

init_per_suite(C) ->
    {ok, _} = application:ensure_all_started(exometer_core),
    [mongoose_config:set_opt(Opt, Val) || {Opt, Val} <- opts()],
    C.

end_per_suite(_C) ->
    [mongoose_config:unset_opt(Opt) || {Opt, _} <- opts()],
    application:stop(exometer_core),
    ok.

init_per_testcase(_TC, C) ->
    C.

end_per_testcase(_, _C) ->
    ok.

global_hook_handler_remains_when_module_stops_for_one_host(_Config) ->
    ResOK = #{mod_test_global_hook => 1},
    gen_hook:start_link(),
    gen_mod:start_module(?HOST, mod_test_global_hook, #{}),
    gen_mod:start_module(?HOST2, mod_test_global_hook, #{}),
    ResOK = mongoose_hooks:node_cleanup(node()),
    %% Still works if we stop the module for one host type
    gen_mod:stop_module(?HOST, mod_test_global_hook),
    ResOK = mongoose_hooks:node_cleanup(node()).

global_hook_handler_removed_when_module_stops_for_all_hosts(_Config) ->
    ResSkip = #{},
    gen_hook:start_link(),
    gen_mod:start_module(?HOST, mod_test_global_hook, #{}),
    gen_mod:start_module(?HOST2, mod_test_global_hook, #{}),
    gen_mod:stop_module(?HOST, mod_test_global_hook),
    gen_mod:stop_module(?HOST2, mod_test_global_hook),
    ResSkip = mongoose_hooks:node_cleanup(node()).
