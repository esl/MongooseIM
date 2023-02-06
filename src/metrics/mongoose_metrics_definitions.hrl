-define(GENERAL_SPIRALS, [
    sessionSuccessfulLogins,
    sessionAuthFails,
    sessionLogouts,
    xmppMessageSent,
    xmppMessageReceived,
    xmppMessageBounced,
    xmppPresenceSent,
    xmppPresenceReceived,
    xmppIqSent,
    xmppIqReceived,
    xmppStanzaSent,
    xmppStanzaReceived,
    xmppStanzaDropped,
    xmppStanzaCount,
    xmppErrorTotal,
    xmppErrorIq,
    xmppErrorMessage,
    xmppErrorPresence,
    modRosterSets,
    modRosterGets,
    modPresenceSubscriptions,
    modPresenceUnsubscriptions,
    modRosterPush,
    modRegisterCount,
    modUnregisterCount,
    modPrivacySets,
    modPrivacySetsActive,
    modPrivacySetsDefault,
    modPrivacyPush,
    modPrivacyGets,
    modPrivacyStanzaDenied,
    modPrivacyStanzaBlocked,
    modPrivacyStanzaAll
]).

-define(TOTAL_COUNTERS, [
    sessionCount
]).

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
         {clusterSize,
          {function, mongoose_metrics, get_mnesia_running_db_nodes_count, [],
           tagged, [value]}},
         ?PROBE(totalSessionCount, mongoose_metrics_probe_total_sessions),
         ?PROBE(uniqueSessionCount, mongoose_metrics_probe_unique_sessions),
         ?PROBE(nodeSessionCount, mongoose_metrics_probe_node_sessions),
         ?PROBE(tcpPortsUsed, mongoose_metrics_probe_tcp),
         ?PROBE(processQueueLengths, mongoose_metrics_probe_queues)
        ]
).

-define(VM_STATS, [{[erlang, system_info], [function, erlang, system_info, ['$dp'], value],
                    [port_count, port_limit, process_count, process_limit, ets_limit]},
                   {[erlang, memory], [function, erlang, memory, ['$dp'], value],
                    [total, processes_used, atom_used, binary, ets, system]}]).

-define(GLOBAL_HISTOGRAMS, [[data, xmpp, received, encrypted_size],
                            [data, xmpp, received, compressed_size],
                            [data, xmpp, received, xml_stanza_size],
                            [data, xmpp, sent, encrypted_size],
                            [data, xmpp, sent, compressed_size],
                            [data, xmpp, sent, xml_stanza_size],
                            [data, xmpp, sent, message, processing_time]
                           ]).

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

