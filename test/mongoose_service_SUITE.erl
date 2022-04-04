-module(mongoose_service_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [starts_and_stops_services,
     ensures_service,
     reverts_config_when_service_fails_to_start,
     does_not_change_config_when_service_fails_to_stop].

init_per_suite(C) ->
    C.

end_per_suite(_C) ->
    ok.

init_per_testcase(_TC, C) ->
    [mock_service(Service) || Service <- test_services()],
    C.

end_per_testcase(_, _C) ->
    mongoose_config:unset_opt(services),
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

%% Helpers

set_services(Services) ->
    mongoose_config:set_opt(services, Services).

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

mock_service(Service) ->
    meck:new(Service, [non_strict]),
    meck:expect(Service, start, fun(_) -> start_result end),
    meck:expect(Service, stop, fun() -> ok end).

test_services() ->
    [service_a, service_b, service_c, service_d].
