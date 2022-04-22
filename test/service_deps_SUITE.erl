-module(service_deps_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

-import(mongoose_service_deps, [resolve_deps/1, sort_deps/1]).

all() ->
    [
     no_dependencies,
     dependency,
     dependency_chain,
     dependency_dag,
     fails_on_dependency_cycle,
     preserves_dependency_opts
    ].

init_per_testcase(_, C) ->
    meck:new(test_services(), [non_strict]),
    C.

end_per_testcase(_, _C) ->
    meck:unload(test_services()).

%% Test cases

no_dependencies(_Config) ->
    Services = #{service_a => #{}, service_b => #{}},
    ?assertEqual(Services, resolve_deps(Services)).

dependency(_Config) ->
    set_deps(#{service_a => [service_b]}),
    ?assertMatch([{service_b, _}, {service_a, _}], add_deps(#{service_a => #{}})).

dependency_chain(_Config) ->
    set_deps(#{service_a => [service_b], service_b => [service_c]}),
    ?assertMatch([{service_c, _}, {service_b, _}, {service_a, _}], add_deps(#{service_a => #{}})).

dependency_dag(_Config) ->
    set_deps(#{service_a => [service_b, service_c], service_b => [service_c]}),
    ?assertMatch([{service_c, _}, {service_b, _}, {service_a, _}], add_deps(#{service_a => #{}})).

fails_on_dependency_cycle(_Config) ->
    set_deps(#{service_a => [service_b], service_b => [service_c], service_c => [service_a]}),
    ?assertError(#{what := resolving_dependencies_aborted}, add_deps(#{service_a => #{}})).

preserves_dependency_opts(_Config) ->
    set_deps(#{service_a => [service_b]}),
    ?assertMatch([{service_b, #{opt1 := 1}}, {service_a, _}],
                 add_deps(#{service_a => #{}, service_b => #{opt1 => 1}})).

%% Helpers

add_deps(ServiceMap) ->
    sort_deps(resolve_deps(ServiceMap)).

set_deps(DepsMap) ->
    maps:map(fun(Service, Deps) -> meck:expect(Service, deps, fun() -> Deps end) end, DepsMap).

test_services() ->
    [service_a, service_b, service_c].
