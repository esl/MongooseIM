-module(mongoose_system_metrics_sender).

-define(BASE_URL, "https://www.google-analytics.com/batch").
-define(TRACKING_ID, "UA-151671255-2").

-export([send/2]).

-type google_analytics_report() :: string().
-type url() :: string().
-type report_struct() :: mongoose_system_metrics_collector:report_struct().

-spec send(string(), [report_struct()]) -> ok.
send(ClientId, ReportStructs) ->
    TrackingIds = get_tracking_ids(),
    Reports = build_reports_for_each_tracking_id(ClientId, TrackingIds, ReportStructs),
    send_reports(Reports),
    ok.

-spec build_reports_for_each_tracking_id(string(), string(), [report_struct()]) -> [google_analytics_report()].
build_reports_for_each_tracking_id(ClientId, TrackingIds, ReportStructs) ->
    lists:map(
        fun(Tid) ->
            build_reports(ClientId, Tid, ReportStructs)
        end, TrackingIds).

-spec build_reports(string(), string(), [report_struct()]) -> [google_analytics_report()].
build_reports(ClientId, TrackingId, ReportStructs) ->
    lists:map(
        fun(Report) ->
            build_report(ClientId, TrackingId, Report)
        end, ReportStructs).

send_reports(ReportsList) ->
    Url = get_url(),
    lists:map(
        fun(Reports) ->
            flush_reports(Url, Reports)
        end, ReportsList).

get_url() ->
    ejabberd_config:get_local_option_or_default(google_analytics_url, ?BASE_URL).

get_tracking_ids() ->
    DevTrackingId = ejabberd_config:get_local_option_or_default(google_analytics_tracking_id, ?TRACKING_ID),
    ExtraTrackingId = ejabberd_config:get_local_option(extra_google_analytics_tracking_id),
    case ExtraTrackingId of
        undefined -> [DevTrackingId];
        ExtraTrackingId -> [DevTrackingId, ExtraTrackingId]
    end.
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

term_to_string(Term) when is_binary(Term) ->
    term_to_string(binary_to_list(Term));
term_to_string(Term) ->
    R = io_lib:format("~p",[Term]),
    lists:flatten(R).
