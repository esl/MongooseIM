-module(exometer_report_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-compile([export_all, nowarn_export_all]).

-define(LABELS, #{host_type => <<"localhost">>}).
-define(HOST_TYPE, <<"localhost">>).

all() ->
    [{group, no_prefix},
     {group, prefix},
     {group, env_prefix},
     {group, all_metrics_are_global}].

groups() ->
    [{no_prefix, [parallel], metrics_tests()},
     {prefix, [parallel], metrics_tests()},
     {env_prefix, [parallel], metrics_tests()},
     {all_metrics_are_global, [parallel], metrics_tests()}].

metrics_tests() ->
    [global_metrics_are_reported,
     host_type_metrics_are_reported].

init_per_suite(Config) ->
    {ok, _Apps} = application:ensure_all_started(exometer_core),
    carbon_cache_server:start(Config).

end_per_suite(Config) ->
    carbon_cache_server:stop(Config),
    application:stop(exometer_core).

init_per_group(Group, Config) ->
    Config1 = [{host_type_prefix, host_type_prefix(Group)},
               {extra_opts, extra_opts(Group)} | Config],
    mongoose_config:set_opts(opts(Group, Config1)),
    async_helper:start(Config1, mongoose_instrument, start_link, []).

end_per_group(_Group, Config) ->
    async_helper:stop_all(Config),
    mongoose_config:erase_opts().

init_per_testcase(TestCase, Config) ->
    carbon_cache_server:subscribe(Config),
    [{event, join_atoms(TestCase, event)} | Config].

host_type_prefix(all_metrics_are_global) -> global;
host_type_prefix(_) -> ?HOST_TYPE.

extra_opts(prefix) -> #{prefix => "mongooseim"};
extra_opts(env_prefix) -> #{env_prefix => "PROGNAME"};
extra_opts(_) -> #{}.

global_metrics_are_reported(Config) ->
    Event = ?config(event, Config),
    Prefix = prefix(Config) ++ [global, Event],
    ok = mongoose_instrument:set_up(Event, #{}, #{metrics => #{requests => spiral,
                                                               sessions => counter,
                                                               time => histogram}}),
    ok = mongoose_instrument:execute(Event, #{}, #{requests => 1, sessions => 5, time => 2}),
    wait_for_metric_value(Prefix ++ [requests, count], 1),
    wait_for_metric_value(Prefix ++ [sessions, value], 5),
    wait_for_metric_value(Prefix ++ [time, mean], 2).

host_type_metrics_are_reported(Config) ->
    Event = ?config(event, Config),
    HostType = ?config(host_type_prefix, Config),
    Prefix = prefix(Config) ++ [HostType, Event],
    ok = mongoose_instrument:set_up(Event, ?LABELS, #{metrics => #{requests => spiral,
                                                                   sessions => counter,
                                                                   time => histogram}}),
    ok = mongoose_instrument:execute(Event, ?LABELS, #{requests => 1, sessions => 5, time => 2}),
    wait_for_metric_value(Prefix ++ [requests, count], 1),
    wait_for_metric_value(Prefix ++ [sessions, value], 5),
    wait_for_metric_value(Prefix ++ [time, mean], 2).

wait_for_metric_value(Metric, ExpectedValue) ->
    ReportedName = reported_metric_name(Metric),
    F = fun() -> carbon_cache_server:get_metric(ReportedName) end,
    Check = fun({Value, TS}) when is_integer(TS), TS > 0 ->
                     Value =:= ExpectedValue;
                (_) -> false
            end,
    wait_helper:wait_until(F, ok, #{validator => Check, name => ReportedName}).

reported_metric_name(Metric) ->
    lists:flatten(string:join([io_lib:format("~s", [Part]) || Part <- Metric], ".")).

prefix(Config) ->
    case ?config(extra_opts, Config) of
        #{prefix := Prefix} ->
            [Prefix];
        #{env_prefix := EnvPrefix} ->
            case os:getenv(EnvPrefix) of
                false -> ct:fail("Environment variable ~s is undefined", EnvPrefix);
                Prefix -> [Prefix]
            end;
        #{} ->
            []
    end.

opts(Group, Config) ->
    AllGlobal = Group =:= all_metrics_are_global,
    InstrConfig = #{exometer => #{all_metrics_are_global => AllGlobal,
                                  report => get_reporters_cfg(Config)}},
    #{hosts => [?HOST_TYPE],
      host_types => [],
      internal_databases => #{},
      instrumentation => config_parser_helper:config([instrumentation], InstrConfig)}.

get_reporters_cfg(Config) ->
    Port = ?config(carbon_port, Config),
    Name = list_to_atom("graphite:127.0.0.1:" ++ integer_to_list(Port)),
    ExtraOpts = ?config(extra_opts, Config),
    #{Name => ExtraOpts#{connect_timeout => 10000,
                         host => "127.0.0.1",
                         port => Port,
                         interval => 1000}}.

join_atoms(A1, A2) ->
    list_to_atom(join_atoms_to_list(A1, A2)).

join_atoms_to_list(A1, A2) ->
    atom_to_list(A1) ++ "_" ++ atom_to_list(A2).
