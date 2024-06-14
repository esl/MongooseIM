-module(mongoose_instrument_probe_cets).
-behaviour(mongoose_instrument_probe).
-include("mongoose_logger.hrl").

-export([start/0, stop/0]).

%% Probe callbacks
-export([probe/2, instrumentation/0]).
-ignore_xref([stop/0, instrumentation/0]).

start() ->
    mongoose_instrument:set_up(instrumentation()).

stop() ->
    mongoose_instrument:tear_down(instrumentation()).

all_zeros() ->
    #{available_nodes => 0,
      unavailable_nodes => 0,
      joined_nodes => 0,
      discovered_nodes => 0,
      discovery_works => 0,
      remote_nodes_without_disco => 0,
      remote_nodes_with_unknown_tables => 0,
      remote_unknown_tables => 0,
      remote_nodes_with_missing_tables => 0,
      remote_missing_tables => 0,
      conflict_nodes => 0,
      conflict_tables => 0}.

-spec instrumentation() -> [mongoose_instrument:spec()].
instrumentation() ->
    [{cets_info, #{}, #{probe => #{module => ?MODULE}, metrics => instrumentation_metrics()}}].

instrumentation_metrics() ->
    maps:from_list([{Name, gauge} || Name <- instrumentation_metrics_names()]).

instrumentation_metrics_names() ->
    [available_nodes,
     unavailable_nodes,
     joined_nodes,
     discovered_nodes,
     discovery_works,
     remote_nodes_without_disco,
     remote_nodes_with_unknown_tables,
     remote_unknown_tables,
     remote_nodes_with_missing_tables,
     remote_missing_tables,
     conflict_nodes,
     conflict_tables].

probe(cets_info, _labels) ->
    try cets_status:status(mongoose_cets_discovery) of
        #{available_nodes := AvNodes,
          unavailable_nodes := UnNodes,
          joined_nodes := Joined,
          discovered_nodes := DiscoNodes,
          discovery_works := DiscoveryWorks,
          remote_nodes_without_disco := NoDisco,
          remote_nodes_with_unknown_tables := UnkNodes,
          remote_unknown_tables := UnkTabs,
          remote_nodes_with_missing_tables := MissNodes,
          remote_missing_tables := MissTabs,
          conflict_nodes := ConNodes,
          conflict_tables := ConTabs} ->
                #{available_nodes => length(AvNodes),
                  unavailable_nodes => length(UnNodes),
                  joined_nodes => length(Joined),
                  discovered_nodes => length(DiscoNodes),
                  discovery_works => boolean_to_integer(DiscoveryWorks),
                  remote_nodes_without_disco => length(NoDisco),
                  remote_nodes_with_unknown_tables => length(UnkNodes),
                  remote_unknown_tables => length(UnkTabs),
                  remote_nodes_with_missing_tables => length(MissNodes),
                  remote_missing_tables => length(MissTabs),
                  conflict_nodes => length(ConNodes),
                  conflict_tables => length(ConTabs)}
    catch Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => cets_system_info_failed, class => Class,
                         reason => Reason, stacktrace => Stacktrace}),
                all_zeros()
    end.

boolean_to_integer(true) -> 1;
boolean_to_integer(false) -> 0.
