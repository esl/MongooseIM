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

format_probe_cets(#{unavailable_nodes := UnNodes,
                    available_nodes := AvailNodes,
                    joined_nodes := JoinedNodes,
                    partially_joined_nodes := PartNodes,
                    partially_joined_tables := PartTables,
                    discovered_nodes := NodesSorted,
                    discovery_works := DiscoveryWorks}) ->
    #{<<"type">> => <<"cets_system">>,
      <<"unavailable_nodes">> => UnNodes,
      <<"available_nodes">> => AvailNodes,
      <<"joined_nodes">> => JoinedNodes,
      <<"partially_joined_nodes">> => PartNodes,
      <<"partially_joined_tables">> => PartTables,
      <<"discovered_nodes">> => NodesSorted,
      <<"discovery_works">> => DiscoveryWorks}.

datapoints() ->
    [unavailable_nodes,
     available_nodes,
     joined_nodes,
     partially_joined_nodes,
     partially_joined_tables,
     discovered_nodes,
     discovery_works].

sample() ->
    try mongoose_cets_api:take() of
        #{unavailable_nodes := UnNodes,
          available_nodes := AvailNodes,
          joined_nodes := JoinedNodes,
          partially_joined_nodes := PartNodes,
          partially_joined_tables := PartTables,
          discovered_nodes := NodesSorted,
          discovery_works := DiscoveryWorks} ->
                #{unavailable_nodes => length(UnNodes),
                  available_nodes => length(AvailNodes),
                  joined_nodes => length(JoinedNodes),
                  partially_joined_nodes => length(PartNodes),
                  partially_joined_tables => length(PartTables),
                  discovered_nodes => length(NodesSorted),
                  discovery_works => boolean_to_integer(DiscoveryWorks)}
    catch Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => cets_system_info_failed, class => Class,
                         reason => Reason, stacktrace => Stacktrace}),
                #{unavailable_nodes => 0,
                  available_nodes => 0,
                  joined_nodes => 0,
                  partially_joined_nodes => 0,
                  partially_joined_tables => 0,
                  discovered_nodes => 0,
                  discovery_works => 0}
    end.

boolean_to_integer(true) -> 1;
boolean_to_integer(false) -> 0.
