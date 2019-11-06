-module(mongoose_user_stats).

-author('aleksander.lisiecki@erlang-solutions.com').

-export([report/0]).

-define(BASE_URL, "https://www.google-analytics.com/batch").
% TODO when finished replace tid to official ESL one
-define(TRACKING_ID, "UA-151110014-1").

report() ->
    ReportUrl = ejabberd_config:get_local_option_or_default(google_analytics_url, ?BASE_URL),
    report_user_stats(ReportUrl).

% Functions are spawned and not linked, as MongooseIM should not care if they fail or not.
% Moreover the MongooseIM's start should not be blocked.
report_user_stats(disable) -> ok;
report_user_stats(ReportUrl) ->
    % Data used for more then one report
    Hosts = ejabberd_config:get_global_option(hosts),
    Reports = [
        fun() -> report_number_of_hosts(Hosts, ReportUrl) end,
        fun() -> report_used_modules(Hosts, ReportUrl) end
    ],
    [spawn(Fun) || Fun <- Reports].

report_number_of_hosts(Hosts, ReportUrl) ->
    Len = length(Hosts),
    ReportLine = build_report(hosts_count, Len),
    send_reports(ReportUrl, [ReportLine]).

report_used_modules(Hosts, ReportUrl) ->
    ModulesWithOpts = lists:flatten(
        lists:map(fun gen_mod:loaded_modules_with_opts/1, Hosts)),
    Lines = lists:map(
        fun({Module, Opts}) ->
            Backend = proplists:get_value(backend, Opts, none),
            build_report(modules, Module, Backend)
        end, ModulesWithOpts),
    send_reports(ReportUrl, Lines   ).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

build_report(EventCategory, EventAction) ->
    build_report(EventCategory, EventAction, empty).

build_report(EventCategory, EventAction, EventLabel) ->
    MaybeLabel = maybe_event_label(EventLabel),
    LstClientId = term_to_string(client_id()),
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
    Line = string:join(LstLine, ""),
    lager:error("~p reported = ~p", [?MODULE, Line]),
    Line.

% https://developers.google.com/analytics/devguides/collection/protocol/v1/devguide#batch-limitations
% A maximum of 20 hits can be specified per request.
send_reports(ReportUrl, Lines) when length(Lines) =< 20 ->
    Headers = [],
    ContentType = "",
    Body = string:join(Lines, "\n"),
    Request = {ReportUrl, Headers, ContentType, Body},
    Res = httpc:request(post, Request, [], []),
    lager:error("Res = ~p", [Res]);
send_reports(ReportUrl, Lines) ->
    {NewBatch, RemainigLines} = lists:split(20, Lines),
    send_reports(ReportUrl, NewBatch),
    send_reports(ReportUrl, RemainigLines),
    ok.


maybe_event_label(empty) -> [];
maybe_event_label(EventLabel) ->
    LstEventLabel = term_to_string(EventLabel),
    ["&el=", LstEventLabel].

term_to_string(Term) ->
    R= io_lib:format("~p",[Term]),
    lists:flatten(R).

client_id() ->
    % TODO in the later implementation store client's ID in eg mnesia table and report stats with the same ID for the same client
    rand:uniform(1000 * 1000 * 1000 * 1000 * 1000).
