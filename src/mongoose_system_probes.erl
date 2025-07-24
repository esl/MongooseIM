-module(mongoose_system_probes).

-behaviour(mongoose_instrument_probe).

%% API
-export([start/0, stop/0]).

%% Behaviour callbacks
-export([probe/2]).

%% API for tests
-export([instrumentation/0]).

-ignore_xref([instrumentation/0]).

-include("mongoose.hrl").

-spec start() -> ok.
start() ->
    mongoose_instrument:set_up(instrumentation()).

-spec stop() -> ok.
stop() ->
    mongoose_instrument:tear_down(instrumentation()).

-spec instrumentation() -> [mongoose_instrument:spec()].
instrumentation() ->
    [{system_up_time, #{},
      #{metrics => gauges([seconds]), probe => #{module => ?MODULE}}},
     {system_tcp_ports, #{},
      #{metrics => gauges([count]), probe => #{module => ?MODULE}}},
     {system_process_queue_lengths, #{},
      #{metrics => gauges([total]), probe => #{module => ?MODULE}}},
     {system_info, #{},
      #{metrics => gauges(system_info_metrics()), probe => #{module => ?MODULE}}},
     {system_memory, #{},
      #{metrics => gauges(proplists:get_keys(erlang:memory())), probe => #{module => ?MODULE}}},
     {system_dist_data, #{},
      #{metrics => gauges([connections | inet_stats()]), probe => #{module => ?MODULE}}} |
     mongoose_internal_databases:instrumentation()].

-spec probe(mongoose_instrument:event_name(), mongoose_instrument:labels()) ->
          mongoose_instrument:measurements().
probe(system_up_time, #{}) ->
    #{seconds => up_time()};
probe(system_tcp_ports, #{}) ->
    #{count => count_tcp_ports()};
probe(system_process_queue_lengths, #{}) ->
    #{total => total_process_queue_length()};
probe(system_info, #{}) ->
    maps:from_list([{Metric, erlang:system_info(Metric)} || Metric <- system_info_metrics()]);
probe(system_memory, #{}) ->
    maps:from_list(erlang:memory());
probe(system_dist_data, #{}) ->
    dist_data_stats().

-spec up_time() -> non_neg_integer().
up_time() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    UpTime div timer:seconds(1).

-spec count_tcp_ports() -> non_neg_integer().
count_tcp_ports() ->
    length([Port || Port <- erlang:ports(), erlang:port_info(Port, name) =:= {name, "tcp_inet"}]).

-spec total_process_queue_length() -> non_neg_integer().
total_process_queue_length() ->
    lists:foldl(fun(Pid, TotalLen) ->
                        case erlang:process_info(Pid, message_queue_len) of
                            {message_queue_len, Len} -> TotalLen + Len;
                            _ -> TotalLen
                        end
                end, 0, erlang:processes()).

-spec system_info_metrics() -> [mongoose_instrument:metric_name()].
system_info_metrics() ->
    [port_count, port_limit, process_count, process_limit, ets_count, ets_limit].

-spec dist_data_stats() -> mongoose_instrument:measurements().
dist_data_stats() ->
    DistCtrl = erlang:system_info(dist_ctrl),
    Stats = lists:foldl(fun try_get_dist_inet_stats/2, #{}, DistCtrl),
    Stats#{connections => length(DistCtrl)}.

-spec try_get_dist_inet_stats({node(), pid() | port()}, mongoose_instrument:measurements()) ->
          mongoose_instrument:measurements().
try_get_dist_inet_stats({_Node, PortOrPid}, StatsIn) ->
    try dist_inet_stats(PortOrPid) of
        PidStats ->
            maps:merge_with(fun merge_stat/3, StatsIn, maps:from_list(PidStats))
    catch C:R:S ->
            ?LOG_INFO(#{what => dist_inet_stats_failed, class => C, reason => R, stacktrace => S}),
            StatsIn
    end.

-spec dist_inet_stats(pid() | port()) -> [{mongoose_instrument:metric_name(), non_neg_integer()}].
dist_inet_stats(Pid) when is_pid(Pid) ->
    {status, _Pid, _Mod, RawInfo} = sys:get_status(Pid),
    case find_port(RawInfo) of
        {ok, Port} ->
            inet_stats(Port);
        not_found ->
            []
    end;
dist_inet_stats(Port) ->
    inet_stats(Port).

-spec inet_stats(port()) -> [{mongoose_instrument:metric_name(), non_neg_integer()}].
inet_stats(Port) ->
    {ok, Stats} = inet:getstat(Port, inet_stats()),
    Stats.

-spec inet_stats() -> [mongoose_instrument:metric_name()].
inet_stats() ->
    [recv_oct, recv_cnt, recv_max, send_oct, send_cnt, send_max, send_pend].

-spec find_port(term()) -> {ok, port()} | not_found.
find_port([Tag, _, _, Port | _]) when (Tag == env orelse Tag == static),
                                      is_port(Port) ->
    {ok, Port};
find_port(Tuple) when is_tuple(Tuple) ->
    find_port(tuple_to_list(Tuple));
find_port([]) ->
    not_found;
find_port([H | _] = List) when is_list(List), is_integer(H) ->
    not_found;
find_port(List) when is_list(List) ->
    lists:foldl(fun fold_find_port/2, not_found, List);
find_port(_) ->
    not_found.

-spec fold_find_port(term(), {ok, port()} | not_found) -> {ok, port()} | not_found.
fold_find_port(_, {ok, _} = Found) ->
    Found;
fold_find_port(Elem, not_found) ->
    find_port(Elem).

-spec merge_stat(mongoose_instrument:metric_name(), non_neg_integer(), non_neg_integer()) ->
          non_neg_integer().
merge_stat(recv_max, V1, V2) ->
    max(V1, V2);
merge_stat(send_max, V1, V2) ->
    max(V1, V2);
merge_stat(_, V1, V2) ->
    V1 + V2.

-spec gauges([mongoose_instrument:metric_name()]) -> mongoose_instrument:metrics().
gauges(Keys) ->
    maps:from_keys(Keys, gauge).
