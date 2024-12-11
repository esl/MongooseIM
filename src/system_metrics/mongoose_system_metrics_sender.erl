-module(mongoose_system_metrics_sender).

-define(BASE_URL, "https://www.google-analytics.com/mp/collect").

-export([send/3]).

-type report_struct() :: mongoose_system_metrics_collector:report_struct().

-spec send(service_mongoose_system_metrics:client_id(),
           [report_struct()],
           [service_mongoose_system_metrics:tracking_id()]) -> ok.
send(ClientId, Reports, TrackingIds) ->
    Url = get_url(),
    lists:map(
        fun(TrackingId) ->
            flush_reports(Url, Reports, ClientId, TrackingId)
        end, TrackingIds),
    ok.

get_url() ->
    mongoose_config:get_opt(google_analytics_url, ?BASE_URL).

% https://developers.google.com/analytics/devguides/collection/protocol/ga4/sending-events?client_type=gtag#limitations
% % A maximum of 25 hits can be specified per request.
flush_reports(_, [], _, _) ->
    {ok, nothing_to_do};
flush_reports(ReportUrl, Reports, ClientId,
              #{id := TrackingId, secret := TrackingSecret}) when length(Reports) =< 25 ->
    Headers = [],
    ContentType = "application/json",
    Body = jiffy:encode(#{client_id => list_to_binary(ClientId), events => Reports}),
    ReportUrl2 = uri_string:normalize(
        ReportUrl ++ "?api_secret=" ++ TrackingSecret ++ "&measurement_id=" ++ TrackingId),
    Request = {ReportUrl2, Headers, ContentType, Body},
    httpc:request(post, Request, [{ssl, [{verify, verify_none}]}], []);
flush_reports(ReportUrl, Reports, ClientId, TrackingId) ->
    {NewBatch, RemainingLines} = lists:split(25, Reports),
    flush_reports(ReportUrl, NewBatch, ClientId, TrackingId),
    flush_reports(ReportUrl, RemainingLines, ClientId, TrackingId).
