-module(mongoose_instrument_log_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").
-include("log_helper.hrl").

%% Setup and teardown

all() ->
    [{group, default_level},
     {group, configured_level}].

groups() ->
    [{default_level, [parallel], cases()},
     {configured_level, [parallel], cases()}].

cases() ->
    [log_with_default_level,
     log_level_in_event_config,
     log_level_in_measurement,
     log_level_in_measurement_overrides_event_config].

init_per_suite(Config) ->
    mongoose_logs:set_module_loglevel(mongoose_instrument_log, debug),
    log_helper:set_up(),
    Config.

end_per_suite(_Config) ->
    log_helper:tear_down(),
    mongoose_logs:clear_module_loglevel(mongoose_instrument_log).

init_per_group(Group, Config) ->
    Opts = #{log := #{level := LogLevel}} = opts(Group),
    mongoose_config:set_opts(#{instrumentation => Opts}),
    Config1 = async_helper:start(Config, mongoose_instrument, start_link, []),
    mongoose_instrument:persist(),
    [{loglevel, LogLevel} | Config1].

end_per_group(_Group, Config) ->
    async_helper:stop_all(Config),
    mongoose_config:erase_opts().

init_per_testcase(Case, Config) ->
    log_helper:subscribe(),
    [{event, concat(Case, event)} | Config].

end_per_testcase(_Case, _Config) ->
    log_helper:unsubscribe().

opts(default_level) ->
    #{log => config_parser_helper:default_config([instrumentation, log])};
opts(configured_level) ->
    #{log => #{level => info}}.

%% Test cases

log_with_default_level(Config) ->
    Event = ?config(event, Config),
    Level = ?config(loglevel, Config),
    ok = mongoose_instrument:set_up(Event, #{host_type => <<"host1">>}, #{}),
    ok = mongoose_instrument:execute(Event, #{host_type => <<"host1">>}, #{count => 1}),
    ?assertLog(Level, #{what := Event, labels := #{host_type := <<"host1">>},
                        measurements := #{count := 1}}).

log_level_in_event_config(Config) ->
    Event = ?config(event, Config),
    ok = mongoose_instrument:set_up(Event, #{host_type => <<"host1">>}, #{loglevel => error}),
    ok = mongoose_instrument:execute(Event, #{host_type => <<"host1">>}, #{count => 1}),
    ?assertLog(error, #{what := Event, labels := #{host_type := <<"host1">>},
                        measurements := #{count := 1}}).

log_level_in_measurement(Config) ->
    Event = ?config(event, Config),
    ok = mongoose_instrument:set_up(Event, #{host_type => <<"host1">>}, #{}),
    ok = mongoose_instrument:execute(Event, #{host_type => <<"host1">>}, #{count => 1,
                                                                           loglevel => warning}),
    ?assertLog(warning, #{what := Event, labels := #{host_type := <<"host1">>},
                          measurements := #{count := 1}}).

log_level_in_measurement_overrides_event_config(Config) ->
    Event = ?config(event, Config),
    ok = mongoose_instrument:set_up(Event, #{host_type => <<"host1">>}, #{loglevel => error}),
    ok = mongoose_instrument:execute(Event, #{host_type => <<"host1">>}, #{count => 1,
                                                                           loglevel => warning}),
    ?assertLog(warning, #{what := Event, labels := #{host_type := <<"host1">>},
                          measurements := #{count := 1}}).

concat(A1, A2) ->
    list_to_atom(atom_to_list(A1) ++ "_" ++ atom_to_list(A2)).
