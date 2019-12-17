-module(mongoose_system_metrics_sender).

-define(BASE_URL, "https://www.google-analytics.com/batch").
-define(TRACKING_ID, "UA-151671255-2").

-export([send/2]).

-type google_analytics_report() :: string().
-type url() :: string().
-type report_struct() :: mongoose_system_metrics_gatherer:report_struct().

-spec send(string(), [report_struct()]) -> ok.
send(ClientId, ReportStructs) ->
    Reports = build_reports(ClientId, ReportStructs),
    send_reports(Reports),
    ok.

-spec build_reports(string(), [report_struct()]) -> [google_analytics_report()].
build_reports(ClientId, ReportStructs) ->
    lists:map(
        fun(Report) -> 
            build_report(ClientId, Report)
        end, ReportStructs).

send_reports(Reports) ->
    Url = get_url(),
    flush_reports(Url, Reports).

get_url() ->
    ejabberd_config:get_local_option_or_default(google_analytics_url, ?BASE_URL).

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

build_report(ClientId, #{report_name := EventCategory, key := EventAction, value := AnyTerm})  ->
    LabelOrValue = label_or_value(AnyTerm),
    LstAnyTerm = term_to_string(AnyTerm),
    LstClientId = term_to_string(ClientId),
    LstEventCategory = term_to_string(EventCategory),
    LstEventAction = term_to_string(EventAction),
    LstLine = [
        "v=1",
        "&tid=", ?TRACKING_ID,
        "&t=event",
        "&cid=", LstClientId,
        "&ec=", LstEventCategory,
        "&ea=", LstEventAction,
        LabelOrValue, LstAnyTerm],
    string:join(LstLine, "").

label_or_value(Value) when is_integer(Value) ->
    "&ev=";
label_or_value(_Label) ->
    "&el=".

term_to_string(Term) ->
    R = io_lib:format("~p",[Term]),
    lists:flatten(R).
