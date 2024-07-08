-define(REPORT_INTERVAL, mongoose_metrics:get_report_interval()).

-define(PROBE(Name, Module),
        {Name,
          {probe,
           [{callback_module, Module},
            {sample_interval, ?REPORT_INTERVAL}]}}).

-define(GLOBAL_COUNTERS,
        [{nodeUpTime,
          {function, mongoose_metrics, get_up_time, [],
           tagged, [value]}},
         ?PROBE(tcpPortsUsed, mongoose_metrics_probe_tcp),
         ?PROBE(processQueueLengths, mongoose_metrics_probe_queues)
        ]
).

-define(MNESIA_COUNTERS,
        [{clusterSize,
          {function, mongoose_metrics, get_mnesia_running_db_nodes_count, [],
           tagged, [value]}}
        ]
).

-define(VM_STATS, [{[erlang, system_info], [function, erlang, system_info, ['$dp'], value],
                    [port_count, port_limit, process_count, process_limit, ets_limit]},
                   {[erlang, memory], [function, erlang, memory, ['$dp'], value],
                    [total, processes_used, atom_used, binary, ets, system]}]).

-define(DATA_FUN_METRICS,
        [{[data, dist],
          {function, mongoose_metrics, get_dist_data_stats, [], proplist, [connections | ?INET_STATS]}}]).


-define(INET_STATS, [recv_oct,
                     recv_cnt,
                     recv_max,
                     send_oct,
                     send_max,
                     send_cnt,
                     send_pend
                    ]).

-define(EMPTY_INET_STATS, [{recv_oct,0},
                           {recv_cnt,0},
                           {recv_max,0},
                           {send_oct,0},
                           {send_max,0},
                           {send_cnt,0},
                           {send_pend,0}
                          ]).

-define(INET_STATS_METRICS, #{recv_oct => spiral,
                              recv_cnt => spiral,
                              recv_max => gauge,
                              send_oct => spiral,
                              send_max => gauge,
                              send_cnt => spiral,
                              send_pend => spiral}).
