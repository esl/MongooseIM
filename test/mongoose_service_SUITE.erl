-module(mongoose_service_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [starts_and_stops_services,
     ensures_service,
     reverts_config_when_service_fails_to_start,
     does_not_change_config_when_service_fails_to_stop,
     replaces_services,
     replaces_services_with_new_deps,
     replaces_services_with_old_deps,
     replaces_services_with_same_deps].

init_per_suite(C) ->
    C.

end_per_suite(_C) ->
    ok.

init_per_testcase(_TC, C) ->
    [mock_service(Service) || Service <- test_services()],
    C.

end_per_testcase(_, _C) ->
    mongoose_config:erase_opts(),
    meck:unload(test_services()).

%% Test cases

starts_and_stops_services(_Config) ->
    set_services(Services = #{service_a => #{}, service_b => [{opt, val}]}),
    ok = mongoose_service:start(),
    check_started(maps:to_list(Services)),

    ok = mongoose_service:stop(),
    check_stopped([service_a, service_b]).

ensures_service(_Config) ->
    set_services(#{}),
    ?assertEqual({started, start_result}, mongoose_service:ensure_started(service_a, #{})),
    ?assertEqual(#{service_a => #{}}, get_services()),

    ?assertEqual(already_started, mongoose_service:ensure_started(service_a, #{})),
    ?assertEqual(#{service_a => #{}}, get_services()),

    ?assertEqual({restarted, #{}, start_result},
                 mongoose_service:ensure_started(service_a, #{opt => val})),
    ?assertEqual(#{service_a => #{opt => val}}, get_services()),

    ?assertEqual({stopped, #{opt => val}}, mongoose_service:ensure_stopped(service_a)),
    ?assertEqual(#{}, get_services()),

    ?assertEqual(already_stopped, mongoose_service:ensure_stopped(service_a)),
    ?assertEqual(#{}, get_services()).

reverts_config_when_service_fails_to_start(_Config) ->
    set_services(#{}),
    meck:expect(service_a, start, fun(_) -> error(something_awful) end),
    ?assertError(something_awful, mongoose_service:ensure_started(service_a, #{})),
    ?assertEqual(#{}, get_services()).

does_not_change_config_when_service_fails_to_stop(_Config) ->
    set_services(#{service_a => #{}}),
    meck:expect(service_a, stop, fun() -> error(something_awful) end),
    ?assertError(something_awful, mongoose_service:ensure_stopped(service_a)),
    ?assertEqual(#{service_a => #{}}, get_services()).

replaces_services(_Config) ->
    set_services(Services = #{service_a => #{}, service_b => #{opt => val}, service_c => #{}}),
    ok = mongoose_service:start(),
    check_started(maps:to_list(Services)),

    %% Stop service_a, change opts for service_b, do not change service_c, start service_d
    NewServices = #{service_b => #{new_opt => new_val}, service_c => #{}, service_d => #{}},
    ok = mongoose_service:replace_services([service_a], NewServices),
    check_stopped([service_a, service_b]),
    check_not_stopped([service_c]),
    check_started([{service_b, #{new_opt => new_val}}, {service_d, #{}}]),
    ?assertEqual(NewServices, get_services()),

    ok = mongoose_service:stop(),
    check_stopped([service_b, service_c, service_d]).

replaces_services_with_new_deps(_Config) ->
    set_deps(#{service_b => [service_c]}),
    set_services(Services = #{service_a => #{}}),
    ok = mongoose_service:start(),
    check_started(maps:to_list(Services)),

    %% Start service_b, which depends on service_c
    ok = mongoose_service:replace_services([], #{service_b => #{}}),
    check_not_stopped([service_a]),
    check_started([{service_b, #{}}, {service_c, #{}}]),
    ?assertEqual(Services#{service_b => #{}, service_c => #{}}, get_services()),

    ok = mongoose_service:stop(),
    check_stopped([service_a, service_b, service_c]).

replaces_services_with_old_deps(_Config) ->
    set_deps(#{service_a => [service_c]}),
    set_services(Services = #{service_a => #{}, service_c => #{}}),
    ok = mongoose_service:start(),
    check_started(maps:to_list(Services)),

    %% Stop service_a, which depends on service_c, and start service_b
    ok = mongoose_service:replace_services([service_a], #{service_b => #{}}),
    check_stopped([service_a, service_c]),
    check_started([{service_b, #{}}]),
    ?assertEqual(#{service_b => #{}}, get_services()),

    ok = mongoose_service:stop(),
    check_stopped([service_b]).

replaces_services_with_same_deps(_Config) ->
    set_deps(#{service_a => [service_c], service_b => [service_c]}),
    set_services(Services = #{service_a => #{}, service_c => #{}}),
    ok = mongoose_service:start(),
    check_started(maps:to_list(Services)),

    %% Stop service_a, and start service_b, both depending on service_c
    ok = mongoose_service:replace_services([service_a], #{service_b => #{}}),
    check_stopped([service_a]),
    check_not_stopped([service_c]),
    check_started([{service_b, #{}}]),
    ?assertEqual(#{service_b => #{}, service_c => #{}}, get_services()),

    ok = mongoose_service:stop(),
    check_stopped([service_b]).

%% Helpers

set_services(Services) ->
    mongoose_config:set_opts(#{services => Services}).

get_services() ->
    mongoose_config:get_opt(services).

check_started(ServicesWithOpts) ->
    lists:foreach(fun({Service, Opts}) ->
                          ?assert(meck:called(Service, start, [Opts]))
                  end, ServicesWithOpts).

check_stopped(Services) ->
    lists:foreach(fun(Service) ->
                          ?assert(meck:called(Service, stop, []))
                  end, Services).

check_not_stopped(Services) ->
    lists:foreach(fun(Service) ->
                          ?assertNot(meck:called(Service, stop, []))
                  end, Services).

mock_service(Service) ->
    meck:new(Service, [non_strict]),
    meck:expect(Service, start, fun(_) -> start_result end),
    meck:expect(Service, stop, fun() -> ok end).

set_deps(DepsMap) ->
    maps:fold(fun(Service, Deps, _) -> meck:expect(Service, deps, fun() -> Deps end) end,
              undefined, DepsMap).

test_services() ->
    [service_a, service_b, service_c, service_d].
