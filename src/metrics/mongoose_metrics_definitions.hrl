-define(GENERAL_SPIRALS, [
    sessionSuccessfulLogins,
    sessionAuthAnonymous,
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
    modGlobalDistribMessagesSent,
    modGlobalDistribMessagesReceived,
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
    modPrivacyStanzaAll,
    modMamPrefsSets,
    modMamPrefsGets,
    modMamArchiveRemoved,
    modMamLookups,
    modMamForwarded,
    modMamArchived,
    modMamFlushed,
    modMamDropped,
    modMamDropped2,
    modMamDroppedIQ,
    modMamSinglePurges,
    modMamMultiplePurges,
    modMucMamPrefsSets,
    modMucMamPrefsGets,
    modMucMamArchiveRemoved,
    modMucMamLookups,
    modMucMamForwarded,
    modMucMamArchived,
    modMucMamSinglePurges,
    modMucMamMultiplePurges,
    modCSIInactive,
    modCSIActive
]).

-define(TOTAL_COUNTERS, [
    sessionCount
]).

-define(EX_EVAL_SINGLE_VALUE, {[{l, [{t, [value, {v, 'Value'}]}]}],[value]}).

-define(GLOBAL_COUNTERS,
        [{totalSessionCount,
          {function, ejabberd_sm, get_total_sessions_number, [],
           eval, ?EX_EVAL_SINGLE_VALUE}},
         {uniqueSessionCount,
          {function, ejabberd_sm, get_unique_sessions_number, [],
           eval, ?EX_EVAL_SINGLE_VALUE}},
         {nodeSessionCount,
          {function, ejabberd_sm, get_node_sessions_number, [],
           eval, ?EX_EVAL_SINGLE_VALUE}},
         {nodeUpTime,
          {function, mongoose_metrics, get_up_time, [],
           tagged, [value]}},
         {clusterSize,
          {function, mongoose_metrics, get_mnesia_running_db_nodes_count, [],
           tagged, [value]}},
         {tcpPortsUsed,
          {probe,
           [{callback_module, mongoose_metrics_probe_tcp},
            {sample_interval, timer:seconds(30)}]}},
         {processQueueLengths,
          {probe,
           [{callback_module, mongoose_metrics_probe_queues},
            {sample_interval, timer:seconds(30)}]}}
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
                            [data, xmpp, sent, xml_stanza_size]]).

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

