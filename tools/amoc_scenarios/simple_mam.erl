%%==============================================================================
%% Copyright 2015 Erlang Solutions Ltd.
%% Licensed under the Apache License, Version 2.0 (see LICENSE file)
%%
%% In this scenarion users are sending message to its neighbours
%% (users wiht lower and grater idea defined by NUMBER_OF_*_NEIGHBOURS values)
%% Messages will be send NUMBER_OF_SEND_MESSAGE_REPEATS to every selected neighbour
%% after every message given the script will wait SLEEP_TIME_AFTER_EVERY_MESSAGE ms
%% Every CHECKER_SESSIONS_INDICATOR is a checker session which just measures message TTD
%%
%%==============================================================================
-module(one2one_mam).
-compile([export_all]).

-include_lib("exml/include/exml.hrl").

-define(HOST, <<"localhost">>). %% The virtual host served by the server
-define(CHECKER_SESSIONS_INDICATOR, 10). %% How often a checker session should be generated
-define(MAM_READER_SESSIONS_INDICATOR, 101). %% How often a MAM reader session should be generated
-define(SLEEP_TIME_AFTER_SCENARIO, 10000). %% wait 10s after scenario before disconnecting
-define(NUMBER_OF_PREV_NEIGHBOURS, 1).
-define(NUMBER_OF_NEXT_NEIGHBOURS, 1).
-define(NUMBER_OF_SEND_MESSAGE_REPEATS, 73).
-define(SLEEP_TIME_AFTER_EVERY_MESSAGE, 3*60*1000).

%%% MAM configuration
%% Read MAM every 10 receiver scenario repeats
-define(MAM_READ_ARCHIVE_INTERVAL, (60+rand:uniform(20))*1000).
%% Read no more than 10 messages from MAM at once; should be larger than read MAM
%% interval so that there's enough messages in the archive (>= 10 in this case)
-define(MAM_RSM_LIMIT, 10).
%% Wait at most 5s for MAM responses (IQ or message)
-define(MAM_STANZAS_TIMEOUT, 5000).

-behaviour(amoc_scenario).

-export([start/1]).
-export([init/0]).

-define(NS_MAM, <<"urn:xmpp:mam:1">>).

-define(MESSAGES_CT, [amoc, counters, messages_sent]).
-define(MESSAGE_TTD_CT, [amoc, times, message_ttd]).
-define(MAM_LOOKUPS_CT, [amoc, counters, mam_lookups]).
-define(MAM_FAILED_LOOKUPS_CT, [amoc, counters, mam_failed_lookups]).
-define(MAM_LOOKUP_RESP_TIME, [amoc, times, mam_lookup_response_time]).

-type binjid() :: binary().
-type session_type() ::
        checker |
        mam_reader |
        sender.

-spec init() -> ok.
init() ->
    lager:info("init some metrics"),
    exometer:new(?MESSAGES_CT, spiral),
    exometer_report:subscribe(exometer_report_graphite, ?MESSAGES_CT, [one, count], 10000),
    exometer:new(?MAM_LOOKUPS_CT, spiral),
    exometer_report:subscribe(exometer_report_graphite, ?MAM_LOOKUPS_CT, [one, count], 10000),
    exometer:new(?MAM_FAILED_LOOKUPS_CT, spiral),
    exometer_report:subscribe(exometer_report_graphite, ?MAM_FAILED_LOOKUPS_CT, [one, count], 10000),
    exometer:new(?MESSAGE_TTD_CT, histogram),
    exometer_report:subscribe(exometer_report_graphite, ?MESSAGE_TTD_CT, [mean, min, max, median, 95, 99, 999], 10000),
    exometer:new(?MAM_LOOKUP_RESP_TIME, histogram),
    exometer_report:subscribe(exometer_report_graphite, ?MAM_LOOKUP_RESP_TIME, [mean, min, max, median, 95, 99, 999], 10000),
    ok.



-spec start(amoc_scenario:user_id()) -> any().
start(MyId) ->
    Cfg1 = make_user(MyId, <<"res1">>),
    Cfg2 = [{socket_opts, socket_opts()} | Cfg1],

    SessionIndicator = session_indicator(MyId),

    {ConnectionTime, ConnectionResult} = timer:tc(escalus_connection, start, [Cfg2]),
    Client = case ConnectionResult of
        {ok, ConnectedClient, _, _} ->
            exometer:update([amoc, counters, connections], 1),
            exometer:update([amoc, times, connection], ConnectionTime),
            ConnectedClient;
        Error ->
            exometer:update([amoc, counters, connection_failures], 1),
            lager:error("Could not connect user=~p, reason=~p", [Cfg2, Error]),
            exit(connection_failed)
    end,

    do(SessionIndicator, MyId, Client),

    timer:sleep(?SLEEP_TIME_AFTER_SCENARIO),
    send_presence_unavailable(Client),
    escalus_connection:stop(Client).

-spec session_indicator(amoc_scenario:user_id()) -> session_type().
session_indicator(MyId) when MyId rem ?CHECKER_SESSIONS_INDICATOR == 0 ->
    checker;
session_indicator(MyId) when MyId rem ?MAM_READER_SESSIONS_INDICATOR == 0 ->
    mam_reader;
session_indicator(_) ->
    sender.

-spec do(boolean(), amoc_scenario:user_id(), escalus:client()) -> any().
do(sender, MyId, Client) ->
    escalus_connection:set_filter_predicate(Client, none),

    send_presence_available(Client),
    timer:sleep(5000),

    NeighbourIds = lists:delete(MyId, lists:seq(max(1,MyId-?NUMBER_OF_PREV_NEIGHBOURS),
                                                MyId+?NUMBER_OF_NEXT_NEIGHBOURS)),
    send_messages_many_times(Client, ?SLEEP_TIME_AFTER_EVERY_MESSAGE, NeighbourIds);
do(checker, _MyId, Client) ->
    lager:info("checker"),
    send_presence_available(Client),
    receive_forever(Client);
do(mam_reader, _MyId, Client) ->
    lager:info("mam_reader"),
    send_presence_available(Client),
    read_archive_forever(Client, erlang:timestamp()).

%%%%%
%% Scenario helpers
%%%%%

-spec receive_forever(escalus:client()) -> no_return().
receive_forever(Client) ->
    Stanza = escalus_connection:get_stanza(Client, message, infinity),
    Now = usec:from_now(os:timestamp()),
    case is_message_with_timestamp(Stanza) of
        true ->
            Timestamp = get_message_timestamp(Stanza),
            TTD = (Now - binary_to_integer(Timestamp)),
            exometer:update(?MESSAGE_TTD_CT, TTD);
        _ ->
            ok
    end,
    receive_forever(Client).

-spec read_archive_forever(escalus:client(), erlang:timestamp()) -> no_return().
read_archive_forever(Client, Timestamp) ->
    CurrentTimestamp = erlang:timestamp(),
    read_messages_from_archive_since_timestamp(Client, Timestamp, ?MAM_STANZAS_TIMEOUT),
    timer:sleep(?MAM_READ_ARCHIVE_INTERVAL),
    read_archive_forever(Client, CurrentTimestamp).

-spec send_messages_many_times(escalus:client(), timeout(), [binjid()]) -> ok.
send_messages_many_times(Client, MessageInterval, NeighbourIds) ->
    S = fun(_) ->
                send_messages_to_neighbors(Client, NeighbourIds, MessageInterval)
        end,
    lists:foreach(S, lists:seq(1, ?NUMBER_OF_SEND_MESSAGE_REPEATS)).

-spec send_messages_to_neighbors(escalus:client(), [binjid()], timeout()) -> list().
send_messages_to_neighbors(Client, TargetIds, SleepTime) ->
    [
     send_message(Client, make_jid(TargetId), SleepTime) ||
     TargetId <- TargetIds
    ].

-spec send_message(escalus:client(), binjid(), timeout()) -> ok.
send_message(Client, ToId, SleepTime) ->
    MsgIn = make_message(ToId),
    TimeStamp = integer_to_binary(usec:from_now(os:timestamp())),
    Stanza = set_message_timestamp(MsgIn, TimeStamp),
    escalus_connection:send(Client, Stanza),
    exometer:update(?MESSAGES_CT, 1),
    timer:sleep(SleepTime).

%%%%%
%% MAM helpers
%%%%%

-spec read_messages_from_archive_since_timestamp(
        Client :: escalus:client(),
        Timestamp :: erlang:timestamp(),
        Timeout :: timer:time()
       ) -> any().
read_messages_from_archive_since_timestamp(Client, Timestamp, Timeout) ->
    case catch do_read_messages_from_archive_since_timestamp(Client,
                                                             Timestamp,
                                                             Timeout) of
        {timeout, What} ->
            lager:warning("Failed to read archive timeout=~p", [What]),
            exometer:update(?MAM_FAILED_LOOKUPS_CT, 1);
        {'EXIT', What} ->
            lager:warning("Failed to read archive error=~p", [What]),
            exometer:update(?MAM_FAILED_LOOKUPS_CT, 1);
        ResponseTimeMicros when is_integer(ResponseTimeMicros) ->
            exometer:update(?MAM_LOOKUP_RESP_TIME, ResponseTimeMicros),
            exometer:update(?MAM_LOOKUPS_CT, 1)
    end.

-spec do_read_messages_from_archive_since_timestamp(
        Client :: escalus:client(),
        Timestamp :: erlang:timestamp(),
        Timeout :: timer:time()
       ) -> ResponseTimeMicroseconds :: integer() |
            no_return(). % escalus throws an exception after Timeout
do_read_messages_from_archive_since_timestamp(Client, Timestamp, Timeout) ->
    filter_out_all_but_mam_archived_messages_and_iqs(Client),
    receive_all_pending_stanzas(Client, Timeout),
    IQSet = mam_v4_archive_query_since_timestamp(<<"query1">>, Timestamp),
    escalus_connection:send(Client, IQSet),
    lager:debug("Sent MAM query stanza=~p", [IQSet]),
    {Micros, _} = timer:tc(
                    fun() ->
                            receive_mam_messages_until_end(Client, Timeout)
                    end),
    filter_out_presence_stanzas(Client),
    Micros.

-spec receive_mam_messages_until_end(
        Client :: escalus_connection:client(),
        Timeout :: timer:time()) -> ok | no_return().
receive_mam_messages_until_end(Client, Timeout) ->
    Stanza = escalus_connection:get_stanza(Client, mam_message_timeout, Timeout),
    lager:debug("Stanza = ~p", [Stanza]),
    case is_mam_archived_message(Stanza) of
        false ->
            maybe_mam_fin_message(Stanza, Client, Timeout);
        true ->
            lager:debug("Received MAM archived message=~p", [Stanza]),
            receive_mam_messages_until_end(Client, Timeout)
    end.

-spec maybe_mam_fin_message(
        Stanza :: exml:element(),
        Client :: escalus_connection:client(),
        Timeout :: timer:time()) -> ok | no_return().
maybe_mam_fin_message(Stanza, Client, Timeout) ->
    case is_mam_fin_complete_message(Stanza) of
        true ->
            lager:debug("Received MAM result stanza=~p", [Stanza]),
            ok;
        false ->
            lager:debug("Received stanza=~p when waiting for MAM archived message ~n", [Stanza]),
            receive_mam_messages_until_end(Client, Timeout)
    end.

%% `micro_seconds` is deprecated as per
%% http://erlang.org/doc/man/erlang.html#type-deprecated_time_unit
%% but `microsecond` was not available in Erlang 18.3 that Amoc uses.
timestamp_diff(T2, T1) -> erlang:convert_time_unit(T2 - T1, native, micro_seconds).

timestamp_to_isotime({_, _, _} = Timestamp) ->
    FmtStr = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
    {{Y, Mo, D}, {H, Mn, S}} = calendar:now_to_datetime(Timestamp),
    IsoStr = io_lib:format(FmtStr, [Y, Mo, D, H, Mn, S]),
    iolist_to_binary(IsoStr).

%%%%%%
%% Escalus helpers
%%%%%

-spec send_presence_available(escalus:client()) -> ok.
send_presence_available(Client) ->
    Pres = escalus_stanza:presence(<<"available">>),
    escalus_connection:send(Client, Pres).

-spec send_presence_unavailable(escalus:client()) -> ok.
send_presence_unavailable(Client) ->
    Pres = escalus_stanza:presence(<<"unavailable">>),
    escalus_connection:send(Client, Pres).

-spec receive_all_pending_stanzas(escalus:client(), timeout()) -> ok.
receive_all_pending_stanzas(Client, Timeout) ->
    receive_all_pending_stanzas(Client, Timeout, 0).

-spec receive_all_pending_stanzas(escalus:client(), timeout(), integer()) -> ok.
receive_all_pending_stanzas(Client, Timeout, Cnt) ->
    case catch escalus_connection:get_stanza(Client, no_stanzas, Timeout) of
        {timeout, no_stanzas} ->
            lager:debug("Received pending stanzas count=~p", [Cnt]);
        #xmlel{} = Stanza ->
            lager:debug("Received pending stanza=~p", [Stanza]),
            receive_all_pending_stanzas(Client, Timeout, Cnt+1)
    end.

-spec filter_out_presence_stanzas(escalus:client()) -> ok.
filter_out_presence_stanzas(Client) ->
    escalus_connection:set_filter_predicate(
      Client,
      fun(Stanza) ->
              not is_presence_message(Stanza)
      end).

-spec filter_out_all_but_mam_archived_messages_and_iqs(escalus:client()) -> ok.
filter_out_all_but_mam_archived_messages_and_iqs(Client) ->
    escalus_connection:set_filter_predicate(
      Client,
      fun(Stanza) ->
              is_mam_archived_message(Stanza) orelse
              is_iq_message(Stanza)
      end).

%%%%%%
%% User helpers
%%%%%

-spec user_spec(binary(), binary(), binary()) -> escalus_users:user_spec().
user_spec(ProfileId, Password, Res) ->
    Server = pick_server(),
    [ {username, ProfileId},
      {server, ?HOST},
      {password, Password},
      {carbons, false},
      {stream_management, false},
      {resource, Res}
    ] ++ Server.

-spec make_user(amoc_scenario:user_id(), binary()) -> escalus_users:user_spec().
make_user(Id, R) ->
    BinId = integer_to_binary(Id),
    ProfileId = <<"user_", BinId/binary>>,
    Password = <<"password_", BinId/binary>>,
    user_spec(ProfileId, Password, R).

-spec socket_opts() -> [gen_tcp:option()].
socket_opts() ->
    [binary,
     {reuseaddr, false},
     {nodelay, true}].

-spec make_message(binjid()) -> exml:element().
make_message(ToId) ->
    Body = body(),
    Id = escalus_stanza:id(),
    escalus_stanza:set_id(escalus_stanza:chat_to(ToId, Body), Id).

-spec make_jid(amoc_scenario:user_id()) -> binjid().
make_jid(Id) ->
    BinInt = integer_to_binary(Id),
    ProfileId = <<"user_", BinInt/binary>>,
    Host = ?HOST,
    << ProfileId/binary, "@", Host/binary >>.

-spec pick_server() -> [proplists:property()].
pick_server() ->
    Servers = amoc_config:get(xmpp_servers),
    verify(Servers),
    S = length(Servers),
    N = erlang:phash2(self(), S) + 1,
    lists:nth(N, Servers).

verify(Servers) ->
    lists:foreach(
      fun(Proplist) ->
              true = proplists:is_defined(host, Proplist)
      end,
      Servers
     ).

body() -> <<"hello sir, you are a gentelman and a scholar.">>.

%%%%
%% XMPP helpers
%%%%%

mam_v4_archive_query_since_timestamp(QueryId, Timestamp) when is_binary(QueryId) ->
    IQ = #xmlel{children = [MAMQuery]} = escalus_stanza:mam_archive_query(QueryId),
    MAMQueryAttrs = lists:keyreplace(
                      <<"xmlns">>,
                      1,
                      MAMQuery#xmlel.attrs,
                      {<<"xmlns">>, ?NS_MAM}),
    IQ#xmlel{children = [
                         MAMQuery#xmlel{attrs = MAMQueryAttrs,
                                        children = [mam_lookup_after_date_xml(Timestamp)]}
                        ]}.

mam_lookup_after_date_xml(Timestamp) ->
    IsoTime = timestamp_to_isotime(Timestamp),
    TimeValueEl = value_xml(IsoTime),
    MamVsnValueEl = value_xml(<<"urn:xmpp:mam:1">>),
    QueryFields =
      [
       field_xml(
         [{<<"var">>, <<"FORM_TYPE">>},
          {<<"type">>, <<"hidden">>}],
         [MamVsnValueEl]),
       field_xml(
         [{<<"var">>, <<"start">>}],
         [TimeValueEl])
      ],
    #xmlel{name = <<"x">>,
           attrs = [
                    {<<"xmlns">>, <<"jabber:x:data">>},
                    {<<"type">>, <<"submit">>}
                   ],
           children = QueryFields
          }.

field_xml(Attrs, Children) ->
    #xmlel{name = <<"field">>,
           attrs = Attrs,
           children = Children}.

value_xml(Data) ->
    #xmlel{name = <<"value">>,
           children = [
                       #xmlcdata{
                          content = Data
                         }
                      ]}.

-spec get_message_timestamp(exml:element()) -> Timestamp | undefined when
      Timestamp :: binary().
get_message_timestamp(#xmlel{} = S) ->
    exml_query:path(S, [{attr, <<"timestamp">>}]).

-spec set_message_timestamp(exml:element(), Timestamp :: binary()) ->
    StanzaWithTimestamp :: exml:element().
set_message_timestamp(Stanza, Timestamp) ->
    escalus_stanza:setattr(Stanza, <<"timestamp">>, Timestamp).

-spec is_mam_archived_message(exml:element()) -> boolean().
is_mam_archived_message(#xmlel{name = <<"message">>} = Stanza) ->
    NS = exml_query:path(Stanza, [{element, <<"result">>}, {attr, <<"xmlns">>}]),
    NS == ?NS_MAM;

is_mam_archived_message(_) ->
    false.

-spec is_mam_fin_complete_message(exml:element()) -> boolean().
is_mam_fin_complete_message(#xmlel{} = Stanza) ->
    case exml_query:path(Stanza, [{element, <<"fin">>}]) of
        undefined  ->
            false;
        FinEl ->
            exml_query:attr(FinEl, <<"xmlns">>) == ?NS_MAM andalso
                exml_query:attr(FinEl, <<"complete">>) == <<"true">>
    end.

-spec is_iq_message(exml:element()) -> boolean().
is_iq_message(#xmlel{name = <<"iq">>}) ->
    true;
is_iq_message(_) ->
    false.

-spec is_presence_message(exml:element()) -> boolean().
is_presence_message(#xmlel{name = <<"presence">>}) ->
    true;
is_presence_message(_) ->
    false.

-spec is_message_with_timestamp(exml:element()) -> boolean().
is_message_with_timestamp(#xmlel{name = <<"message">>, attrs = A}) ->
    lists:keymember(<<"timestamp">>, 1, A);
is_message_with_timestamp(_) ->
    false.
