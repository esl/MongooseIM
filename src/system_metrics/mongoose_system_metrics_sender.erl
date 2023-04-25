-module(mongoose_system_metrics_sender).

-define(BASE_URL, "https://www.google-analytics.com/mp/collect").

-export([send/3]).

-type google_analytics_report() :: string().
-type url() :: string().
-type report_struct() :: mongoose_system_metrics_collector:report_struct().

-spec send(service_mongoose_system_metrics:client_id(),
           [report_struct()],
           [service_mongoose_system_metrics:tracking_id()]) -> ok.
send(ClientId, Reports, TrackingIds) ->
    send_reports_for_each_tracking_id(ClientId, TrackingIds, Reports),
    ok.

%-spec build_reports_for_each_tracking_id(service_mongoose_system_metrics:client_id(),
%                                         [service_mongoose_system_metrics:tracking_id()],
%                                         [report_struct()]) -> [google_analytics_report()].
% 
send_reports_for_each_tracking_id(ClientId, TrackingIds, Reports) ->
    Url = get_url(),
    lists:map(
        fun(TrackingId) ->
            flush_reports(Url, Reports, ClientId, TrackingId)
        end, TrackingIds).

get_url() ->
    mongoose_config:get_opt(google_analytics_url, ?BASE_URL).

% % https://developers.google.com/analytics/devguides/collection/protocol/v1/devguide#batch-limitations
% % A maximum of 20 hits can be specified per request.
flush_reports(_, [], _, _) ->
    {ok, nothing_to_do};
flush_reports(ReportUrl, Reports, ClientId,
              #{id := TrackingId, secret := TrackingSecret}) when length(Reports) =< 20 ->
    Headers = [],
    ContentType = "application/json",
    Body = jiffy:encode(#{client_id => list_to_binary(ClientId), events => Reports}),
    ReportUrl2 = uri_string:normalize(
        ReportUrl ++ "?api_secret=" ++ TrackingSecret ++ "&measurement_id=" ++ TrackingId),
    Request = {ReportUrl2, Headers, ContentType, Body},
    httpc:request(post, Request, [{ssl, [{verify, verify_none}]}], []);
flush_reports(ReportUrl, Reports, ClientId, TrackingId) ->
    {NewBatch, RemainingLines} = lists:split(20, Reports),
    flush_reports(ReportUrl, NewBatch, ClientId, TrackingId),
    flush_reports(ReportUrl, RemainingLines, ClientId, TrackingId).
