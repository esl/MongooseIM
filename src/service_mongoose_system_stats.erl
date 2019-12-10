-module(service_mongoose_system_stats).

-behaviour(gen_server).

-define(BASE_URL, "https://www.google-analytics.com/batch").
-define(TRACKING_ID, "UA-151671255-1").
-define(DEFAULT_REPORT_AFTER, 1000). % Every hour
-define(STAT_TYPE, [mongoose_system_stats]).

-export([report/2]).
-export([start_link/0, handle_event/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    url_base = ?BASE_URL,
    client_id,
    reports = [],
    report_after = ?DEFAULT_REPORT_AFTER,
    loop_timer_ref
}).
-record(service_mongoose_system_stats, {key, value}).

-spec report(#{}, #{}) -> any().
report(Metrics, Metadata) -> 
    telemetry:execute(?STAT_TYPE, Metrics, Metadata).

handle_event(?STAT_TYPE, Metrics, Metadata , _Config) ->
    lager:error("Hello Jan from handle event"),
    ReportLine = parse_telemetry_report(Metrics, Metadata),
    gen_server:cast(?MODULE, {add_report, ReportLine});
handle_event(StatType, Map1, Map2, _Config) ->
  lager:info("Unknown Stat Type: ~p, Map1:~p Map2:~p sent in wololo", [StatType, Map1, Map2]).

parse_telemetry_report(_Metrics, _Metadata) -> 
    %TODO: add event label and make a report
    "v=1&tid=" ?TRACKING_ID "&t=event&ec=wololo&ea=convert". 

start_link() ->
    lager:error("Hello Jan from start_link"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    lager:error("Hello Jan from init"),
    % TODO fix getting config
    IsAllowed = ejabberd_config:get_local_option(service_mongoose_system_stats_is_allowed),
    case IsAllowed of
        true ->
            Result = telemetry:attach(
                <<"mongoose_system_stats">>,
                ?STAT_TYPE,
                fun service_mongoose_system_stats:handle_event/4,
                [] ),
            case Result of
                ok -> ok;
                {error, already_exists} ->
                    lager:warning("Reattaching telemetry");
                _ ->
                    % TODO: adjust log level, consider crashing 
                    lager:error("Telemetry result: ~p", [Result])
            end,
            Url = ejabberd_config:get_local_option(google_analytics_url),
            ReportAfter = ?DEFAULT_REPORT_AFTER,
            TimerRef = erlang:send_after(ReportAfter, self(), flush_reports),
            State = #state{
                url_base = Url,
                client_id = get_client_id(),
                report_after = ReportAfter,
                reports = [],
                loop_timer_ref = TimerRef
            },
            {ok, State};
        _ -> {stop, is_not_allowed}
    end.

handle_info(flush_reports, State = #state{url_base = UrlBase, reports = Reports, loop_timer_ref = TimerRef, report_after = ReportAfter}) ->
    erlang:cancel_timer(TimerRef),
    % TODO maybe send report
    flush_reports(UrlBase, Reports),
    TimerRef = erlang:send_after(ReportAfter, self(), flush_reports),
    {noreply, State#state{reports = [], loop_timer_ref = TimerRef}}.

handle_cast({add_report, NewReport}, State = #state{reports = Reports, client_id = ClientID}) ->
    FullReport = [NewReport ++ ClientID | Reports],
    maybe_flush_report(length(FullReport)),
    {noreply, State#state{reports = FullReport}}.

maybe_flush_report(ReportLength) when ReportLength >= 20 -> 
    self() ! flush_reports;
maybe_flush_report(_) ->
    ok.

% % https://developers.google.com/analytics/devguides/collection/protocol/v1/devguide#batch-limitations
% % A maximum of 20 hits can be specified per request.
flush_reports(_, []) ->
    ok;
flush_reports(ReportUrl, Lines) when length(Lines) =< 20 ->
    Headers = [],
    ContentType = "",
    Body = string:join(Lines, "\n"),
    Request = {ReportUrl, Headers, ContentType, Body},
    httpc:request(post, Request, [], []);
flush_reports(ReportUrl, Lines) ->
    {NewBatch, RemainingLines} = lists:split(20, Lines),
    flush_reports(ReportUrl, NewBatch),
    flush_reports(ReportUrl, RemainingLines),
    ok.

get_client_id() ->
    T = fun() ->
        mnesia:read(service_mongoose_system_stats, client_id)
    end,
    case mnesia:transaction(T) of 
        {aborted, {no_exists, service_mongoose_system_stats}} ->
            maybe_create_table(),
            maybe_make_and_save_new_client_id();
        {atomic, [Record]} ->
            #service_mongoose_system_stats{value = ClientId} = Record,
            ClientId
    end.

maybe_create_table() ->
    mnesia:create_table(service_mongoose_system_stats,
        [
            {type, set},
            {record_name, service_mongoose_system_stats},
            {attributes, record_info(fields, service_mongoose_system_stats)},
            {disc_copies, [node() | nodes()]}
        ]),
    mnesia:wait_for_tables([service_mongoose_system_stats], 5000).

maybe_make_and_save_new_client_id() ->
    T = fun() ->
        case mnesia:read(service_mongoose_system_stats, client_id) of
            [] ->
                ClientId = rand:uniform(1000 * 1000 * 1000 * 1000 * 1000),
                mnesia:write(#service_mongoose_system_stats{key = client_id, value = ClientId}),
                ClientId;
            [#service_mongoose_system_stats{value = ClientId}] ->
                ClientId
        end
    end,
    mnesia:transaction(T).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UNUSED
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
terminate(_Reason, _State) ->
   ok.