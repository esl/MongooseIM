-module(mongoose_system_metrics_sender).

-define(BASE_URL, "https://www.google-analytics.com/batch").
-define(TRACKING_ID, "UA-151671255-2").

-export([send/2]).

-type google_analytics_report() :: string().
-type url() :: string().
-type report_struct() :: mongoose_system_metrics_collector:report_struct().

-spec send(string(), [report_struct()]) -> ok.
send(ClientId, ReportStructs) ->
    Reports = build_reports(ClientId, ReportStructs),
    send_reports(Reports),
    ok.

-spec build_reports(string(), [report_struct()]) -> [google_analytics_report()].
build_reports(ClientId, ReportStructs) ->
    TrackingId = get_tracking_id(),
    lists:map(
        fun(Report) -> 
            build_report(ClientId, TrackingId, Report)
        end, ReportStructs).

send_reports(Reports) ->
    Url = get_url(),
    flush_reports(Url, Reports).

get_url() ->
    ejabberd_config:get_local_option_or_default(google_analytics_url, ?BASE_URL).

get_tracking_id() ->
    ejabberd_config:get_local_option_or_default(google_analytics_tracking_id, ?TRACKING_ID).

% % https://developers.google.com/analytics/devguides/collection/protocol/v1/devguide#batch-limitations
% % A maximum of 20 hits can be specified per request.
-spec flush_reports(url(), [google_analytics_report()]) -> {ok, term()} | {error, term()}.
flush_reports(_, []) ->
    {ok, nothing_to_do};
flush_reports(ReportUrl, Lines) when length(Lines) =< 20 ->
    Headers = [],
    ContentType = "",
    Body = string:join(Lines, "\n"),
    Request = {ReportUrl, Headers, ContentType, Body},
    httpc:request(post, Request, [], []);
flush_reports(ReportUrl, Lines) ->
    {NewBatch, RemainingLines} = lists:split(20, Lines),
    flush_reports(ReportUrl, NewBatch),
    flush_reports(ReportUrl, RemainingLines).

build_report(ClientId, TrackingId, #{report_name := EventCategory, key := EventAction, value := EventLabel})  ->
    LstClientId = term_to_string(ClientId),
    LstEventCategory = term_to_string(EventCategory),
    LstEventAction = term_to_string(EventAction),
    LstEventLabel = term_to_string(EventLabel),
    LstLine = [
        "v=1",
        "&tid=", TrackingId,
        "&t=event",
        "&cid=", LstClientId,
        "&ec=", LstEventCategory,
        "&ea=", LstEventAction,
        "&el=", LstEventLabel],
    string:join(LstLine, "").

term_to_string(Term) ->
    R = io_lib:format("~p",[Term]),
    lists:flatten(R).
