%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(mongoose_service_SUITE).
-compile([export_all]).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
    [
        starts_service,
        ensure,
        verify_deps,
        start_deps,
        module_deps,
        misconfigured
    ].

services() -> [service_a, service_b, service_c, service_d, service_e, service_f, service_g,
               service_h, service_i, service_j, service_k, service_l].

dep_services() -> [service_c, service_d, service_e, service_f, service_g, service_h].

%%      c
%%     / \
%%    d   e
%%   / \ / \
%%  f   g   h
%%  ^       |
%%  |       |
%%   -------

%%    i
%%   / \
%%  j   k <--
%%       \   |
%%        l--

service_deps() ->
    [
        {service_c, [service_d, service_e]},
        {service_d, [service_f, service_g]},
        {service_e, [service_g, service_h]},
        {service_h, [service_f]},
        % and now for something completely cicrular
        {service_i, [service_j, service_k]},
        {service_k, [service_l]},
        {service_l, [service_k]}
    ].

init_per_testcase(misconfigured, C) ->
    mongoose_service:start(),
    meck:new(ejabberd_config, [passthrough]),
    meck:expect(ejabberd_config, get_local_option_or_default,
        fun(services, _) -> [{service_a, []}] end),
    meck:new(service_a, [non_strict]),
    meck:expect(service_a, deps, fun() -> [service_b] end),
    C;

init_per_testcase(module_deps, C) ->
    init_per_testcase(generic, C),
    meck:expect(ejabberd_config, get_local_option, fun(_) -> undefined end),
    meck:expect(ejabberd_config, add_local_option, fun(_, _) -> ok end),
    gen_mod:start(),
    meck:new(module_a, [non_strict]),
    meck:expect(module_a, deps, fun(_, _) -> [{service, service_d}, {service, service_h}] end),
    meck:expect(module_a, start, fun(_, _) -> ok end),
    meck:expect(module_a, stop, fun(_) -> ok end),
    C;
init_per_testcase(_, C) ->
    mongoose_service:start(),
    ets:new(testservice, [named_table]),
    meck:new(services(), [non_strict]),
    lists:map(fun(S) ->
                  meck:expect(S, start, fun(_Opts) -> increment(S), done end)
              end, services()),
    lists:map(fun(S) ->
                  meck:expect(S, stop, fun() -> decrement(S) end)
              end, services()),
    meck:new(ejabberd_config, [passthrough]),
    meck:expect(ejabberd_config, get_local_option_or_default,
                fun(services, _) -> [{Serv, []} || Serv <- services()] end),
    lists:map(fun(Serv) ->
                  meck:expect(Serv, deps, fun() -> proplists:get_value(Serv, service_deps()) end)
              end,
              proplists:get_keys(service_deps())),
    C.

end_per_testcase(misconfigured, C) ->
    meck:unload(service_a),
    meck:unload(ejabberd_config),
    C;

end_per_testcase(module_deps, C) ->
    meck:unload(module_a),
    end_per_testcase(generic, C);
end_per_testcase(_, C) ->
    meck:unload(services()),
    meck:unload(ejabberd_config),
    lists:foreach(fun mongoose_service:stop_service/1, services()),
    mongoose_service:stop(),
    C.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% tests
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

starts_service(_C) ->
    {ok, done} = mongoose_service:start_service(service_a, []),
    {error, already_started} = mongoose_service:start_service(service_a, []),
    ?assertEqual(1, read(service_a)),
    {ok, done} = mongoose_service:start_service(service_b, []),
    {error, already_started} = mongoose_service:start_service(service_b, []),
    ?assertEqual(1, read(service_b)),
    ok = mongoose_service:stop_service(service_a),
    ?assertEqual(0, read(service_a)),
    {error, not_running} = mongoose_service:stop_service(service_a),
    ?assertEqual(0, read(service_a)),
    ok = mongoose_service:stop_service(service_b),
    ?assertEqual(0, read(service_b)),
    {error, not_running} = mongoose_service:stop_service(service_b),
    ?assertEqual(0, read(service_b)),
    ok.

ensure(_C) ->
    ok = mongoose_service:ensure_loaded(service_a),
    ok = mongoose_service:ensure_loaded(service_a),
    ok = mongoose_service:ensure_loaded(service_a),
    ?assertEqual(1, read(service_a)),
    ok = mongoose_service:stop_service(service_a),
    ?assertEqual(0, read(service_a)),
    ok.

verify_deps(_) ->
    mongoose_service:check_deps(service_c),
    mongoose_service:check_deps(service_d),
    mongoose_service:check_deps(service_e),
    mongoose_service:check_deps(service_f),
    mongoose_service:check_deps(service_g),
    mongoose_service:check_deps(service_g),
    ?assertException(error, {circular_deps_detected, _}, mongoose_service:check_deps(service_i)),
    mongoose_service:check_deps(service_j),
    ?assertException(error, {circular_deps_detected, _}, mongoose_service:check_deps(service_k)),
    ?assertException(error, {circular_deps_detected, _}, mongoose_service:check_deps(service_l)),
    ok.

start_deps(_) ->
    assert_loaded([]),
    mongoose_service:ensure_loaded(service_f),
    assert_loaded([service_f]),
    mongoose_service:ensure_loaded(service_d),
    assert_loaded([service_d, service_f, service_g]),
    mongoose_service:ensure_loaded(service_e),
    assert_loaded([service_d, service_e, service_f, service_g, service_h]),
    mongoose_service:ensure_loaded(service_c),
    assert_loaded(dep_services()),
    lists:foreach(fun mongoose_service:stop_service/1, services()),
    assert_loaded([]),
    mongoose_service:ensure_loaded(service_c),
    assert_loaded(dep_services()),
    ok.

module_deps(_) ->
    assert_loaded([]),
    meck:new(gen_mod, [passthrough]),
    meck:expect(gen_mod, is_app_running, fun(_Mooo) -> true end),
    ?assertError({service_not_loaded, _}, gen_mod:start_module(<<"localhost">>, module_a, [])),
    meck:unload(gen_mod),
    mongoose_service:ensure_loaded(service_c),
    gen_mod_deps:start_modules(<<"localhost">>, [{module_a, []}]),
    ?assert(gen_mod:is_loaded(<<"localhost">>, module_a)),
    ok.

misconfigured(_) ->
    ?assertError({service_not_configured, service_b}, mongoose_service:ensure_loaded(service_a)),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% helpers
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

increment(S) ->
    Ni = case ets:lookup(testservice, S) of
             [{S, I}] -> I + 1;
             [] -> 1
         end,
    ets:insert(testservice, {S, Ni}).

decrement(S) ->
    Ni = case ets:lookup(testservice, S) of
             [{S, I}] -> I - 1;
             [] -> -1
         end,
    ets:insert(testservice, {S, Ni}).

read(S) ->
    case ets:lookup(testservice, S) of
        [{S, I}] -> I;
        [] -> 0
    end.

assert_loaded(Loaded) ->
    _ = [{S, mongoose_service:is_loaded(S)} || S <- services()],
    NotLoaded = sets:to_list(
                    sets:subtract(
                        sets:from_list(dep_services()),
                        sets:from_list(Loaded))),
    ?assert(lists:all(fun mongoose_service:is_loaded/1, Loaded)),
    ?assert(lists:all(fun(S) -> not mongoose_service:is_loaded(S) end, NotLoaded)),
    ok.
