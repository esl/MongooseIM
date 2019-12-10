-module(service_mongoose_system_stats).
-author('jan.ciesla@erlang-solutions.com').

-behaviour(gen_server).

-define(BASE_URL, "https://www.google-analytics.com/batch").
-define(TRACKING_ID, "UA-151671255-1").
-define(DEFAULT_REPORT_AFTER, 60 * 60 * 1000).
-define(STAT_TYPE, [mongoose_system_stats]).

-export([start_link/0, handle_event/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    client_id = '',
    reports = [],
    report_after = ?DEFAULT_REPORT_AFTER,
    loop_timer_ref
    }).
-record(service_mongoose_system_stats, {key, value}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    IsAllowed = ejabberd_config:get_local_option(service_mongoose_system_stats_is_allowed),
    case {IsAllowed, init_telemetry_reporter()} of
        {true, ok} ->
            ReportAfter = ?DEFAULT_REPORT_AFTER,
            TimerRef = erlang:send_after(ReportAfter, self(), flush_reports),
            State = #state{
                client_id = get_client_id(),
                report_after = ReportAfter,
                reports = [],
                loop_timer_ref = TimerRef
                },
            report_hosts_count(),
            {ok, State};
        {false, _} -> {stop, is_not_allowed};
        {_ , Error} -> {stop, Error}
    end.

init_telemetry_reporter() ->
    Result = telemetry:attach(
                <<"mongoose_system_stats">>,
                ?STAT_TYPE,
                fun service_mongoose_system_stats:handle_event/4,
                [] ),
    case Result of
        ok -> ok;
        {error, already_exists} -> ok;
        Reason -> {error, Reason}
    end.

handle_event(?STAT_TYPE, Metrics, Metadata , _Config) ->
    ReportLine = parse_telemetry_report(Metrics, Metadata),
    gen_server:cast(?MODULE, {add_report, ReportLine});
handle_event(_StatType, _Map1, _Map2, _Config) ->
    ok.

parse_telemetry_report(#{module := Module}, #{host := _Host, opts := Opts}) ->
    Backend = proplists:get_value(backend, Opts, none),
    build_report(modules, Module, Backend);
parse_telemetry_report(#{hosts_count := HostsCount}, _Metadata) ->
    build_report(hosts_count, HostsCount);
parse_telemetry_report(#{event_category := EventCategory}, #{event_action := EventAction}) ->
    build_report(EventCategory, EventAction);
parse_telemetry_report(#{event_category := EventCategory}, #{event_action := EventAction, event_label := EventLabel}) ->
    build_report(EventCategory, EventAction, EventLabel);
parse_telemetry_report(_Metrics, _Metadata) ->
    %incorrect reports are ignored
    ok.

handle_info(flush_reports, State = #state{reports = Reports, loop_timer_ref = TimerRef, report_after = ReportAfter}) ->
    UrlBase = maybe_get_url(),
    erlang:cancel_timer(TimerRef),
    flush_reports(UrlBase, Reports),
    NewTimerRef = erlang:send_after(ReportAfter, self(), flush_reports),
    {noreply, State#state{reports = [], loop_timer_ref = NewTimerRef}};
handle_info(_Message, _State) ->
    ok.

maybe_get_url() ->
    MaybeUrl = ejabberd_config:get_local_option(google_analytics_url),
    get_url(MaybeUrl).
get_url(undefined)->
    ?BASE_URL;
get_url(Url) ->
    Url.

handle_cast({add_report, NewReport}, State = #state{reports = Reports}) ->
    FullReport = [NewReport | Reports],
    maybe_flush_report(length(FullReport)),
    {noreply, State#state{reports = FullReport}}.

% %%-----------------------------------------
% %% Helpers
% %%-----------------------------------------

report_hosts_count() ->
    Hosts = ejabberd_config:get_global_option(hosts),
    NumberOfHosts = length(Hosts),
    telemetry:execute(?STAT_TYPE, #{hosts_count => NumberOfHosts}, #{}).

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
                ClientId = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
                ListClientId = erlang:binary_to_list(ClientId),
                mnesia:write(#service_mongoose_system_stats{key = client_id, value = ListClientId}),
                ListClientId;
            [#service_mongoose_system_stats{value = ListClientId}] ->
                ListClientId
        end
    end,
    mnesia:transaction(T).

build_report(EventCategory, EventAction) ->
    build_report(EventCategory, EventAction, none).

build_report(EventCategory, EventAction, EventLabel) ->
    MaybeLabel = maybe_event_label(EventLabel),
    LstClientId = term_to_string(get_client_id()),
    LstEventCategory = term_to_string(EventCategory),
    LstEventAction = term_to_string(EventAction),
    LstLine = [
        "v=1",
        "&tid=", ?TRACKING_ID,
        "&t=event",
        "&cid=", LstClientId,
        "&ec=", LstEventCategory,
        "&ea=", LstEventAction
        ] ++ MaybeLabel,
    string:join(LstLine, "").

maybe_event_label(none) -> [];
maybe_event_label(EventLabel) ->
    LstEventLabel = term_to_string(EventLabel),
    ["&el=", LstEventLabel].

term_to_string(Term) ->
    R = io_lib:format("~p",[Term]),
    lists:flatten(R).

% %%-----------------------------------------
% %% Unused
% %%-----------------------------------------

handle_call(_Request, _From, State) ->
    {reply, ok, State}.
terminate(_Reason, _State) ->
   ok.