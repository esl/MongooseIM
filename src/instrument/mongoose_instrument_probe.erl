-module(mongoose_instrument_probe).

-export([start_probe_timer/3, call/4]).

-callback probe(mongoose_instrument:event_name(), mongoose_instrument:labels(), mongoose_instrument:extra()) ->
    mongoose_instrument:measurements().

-ignore_xref([call/4]).

-spec start_probe_timer(mongoose_instrument:event_name(),
                        mongoose_instrument:labels(),
                        mongoose_instrument:probe_config()) -> timer:tref().
start_probe_timer(EventName, Labels, #{module := Module} = ProbeConfig) ->
    Interval = get_probe_interval(ProbeConfig),
    Extra = maps:get(extra, ProbeConfig, #{}),
    Args = [Module, EventName, Labels, Extra],
    % Execute the first probe asynchronously to avoid calling `mongoose_instrument:execute/3` from
    % the `mongoose_instrument` process.
    {ok, _} = timer:apply_after(0, ?MODULE, call, Args),
    {ok, TRef} = timer:apply_repeatedly(Interval, ?MODULE, call, Args),
    TRef.

call(ProbeMod, EventName, Labels, Extra) ->
    case safely:apply_and_log(ProbeMod, probe, [EventName, Labels, Extra],
                              #{what => probe_failed, probe_mod => ProbeMod,
                                event_name => EventName, labels => Labels, extra => Extra}) of
        {exception, _} ->
            ok; % Already logged
        Measurements = #{} ->
            mongoose_instrument:execute(EventName, Labels, Measurements)
    end.

-spec get_probe_interval(mongoose_instrument:probe_config()) -> pos_integer().
get_probe_interval(#{interval := Interval}) when is_integer(Interval), Interval > 0 ->
    Interval;
get_probe_interval(#{}) ->
    timer:seconds(mongoose_config:get_opt([instrumentation, probe_interval])).
