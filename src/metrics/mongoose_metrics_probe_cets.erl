-module(mongoose_metrics_probe_cets).
-behaviour(mongoose_metrics_probe).
-include("mongoose_logger.hrl").

%% Init the metrics
-export([start/0]).

%% Probe callbacks
-export([sample/0, datapoints/0]).

%% GraphQL helper
-export([format_probe_cets/1]).

start() ->
    Interval = mongoose_metrics:get_report_interval(),
    M = [{callback_module, ?MODULE},
         {sample_interval, Interval}],
    mongoose_metrics:ensure_metric(global, [cets, system], {probe, M}),
    ok.

format_probe_cets(#{available_nodes := AvNodes,
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
                    conflict_tables := ConTabs}) ->
    #{<<"type">> => <<"cets_system">>,
      <<"available_nodes">> => AvNodes,
      <<"unavailable_nodes">> => UnNodes,
      <<"joined_nodes">> => Joined,
      <<"discovered_nodes">> => DiscoNodes,
      <<"discovery_works">> => DiscoveryWorks,
      <<"remote_nodes_without_disco">> => NoDisco,
      <<"remote_nodes_with_unknown_tables">> => UnkNodes,
      <<"remote_unknown_tables">> => UnkTabs,
      <<"remote_nodes_with_missing_tables">> => MissNodes,
      <<"remote_missing_tables">> => MissTabs,
      <<"conflict_nodes">> => ConNodes,
      <<"conflict_tables">> => ConTabs}.

all_zeros() ->
    #{<<"available_nodes">> => 0,
      <<"unavailable_nodes">> => 0,
      <<"joined_nodes">> => 0,
      <<"discovered_nodes">> => 0,
      <<"discovery_works">> => 0,
      <<"remote_nodes_without_disco">> => 0,
      <<"remote_nodes_with_unknown_tables">> => 0,
      <<"remote_unknown_tables">> => 0,
      <<"remote_nodes_with_missing_tables">> => 0,
      <<"remote_missing_tables">> => 0,
      <<"conflict_nodes">> => 0,
      <<"conflict_tables">> => 0}.

datapoints() ->
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

sample() ->
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
