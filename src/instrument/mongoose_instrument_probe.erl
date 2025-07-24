-module(mongoose_instrument_probe).

-export([start_probe_timer/3, call/3]).

-callback probe(mongoose_instrument:event_name(), mongoose_instrument:labels()) ->
    mongoose_instrument:measurements().

-ignore_xref([call/3]).

-spec start_probe_timer(mongoose_instrument:event_name(),
                        mongoose_instrument:labels(),
                        mongoose_instrument:probe_config()) -> timer:tref().
start_probe_timer(EventName, Labels, #{module := Module} = ProbeConfig) ->
    Interval = timer:seconds(get_probe_interval(ProbeConfig)),
    {ok, TRef} = timer:apply_repeatedly(Interval, ?MODULE, call, [Module, EventName, Labels]),
    TRef.

call(ProbeMod, EventName, Labels) ->
    case safely:apply_and_log(ProbeMod, probe, [EventName, Labels],
                              #{what => probe_failed, probe_mod => ProbeMod,
                                event_name => EventName, labels => Labels}) of
        {exception, _} ->
            ok; % Already logged
        Measurements = #{} ->
            mongoose_instrument:execute(EventName, Labels, Measurements)
    end.

-spec get_probe_interval(mongoose_instrument:probe_config()) -> pos_integer().
get_probe_interval(#{interval := Interval}) when is_integer(Interval), Interval > 0 ->
    Interval;
get_probe_interval(#{}) ->
    mongoose_config:get_opt([instrumentation, probe_interval]).
