-define(GLOBAL_SPIRALS, [
    [data, xmpp, received, s2s],
    [data, xmpp, received, component],
    [data, xmpp, sent, s2s],
    [data, xmpp, sent, component]
]).

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
