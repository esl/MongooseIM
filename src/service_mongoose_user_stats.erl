-module(service_mongoose_user_stats).

-author('aleksander.lisiecki@erlang-solutions.com').

-behaviour(mongoose_service).

-export([start/1, stop/0]).

-record(service_mongoose_user_stats, {key, value}).

-define(BASE_URL, "https://www.google-analytics.com/batch").
-define(TRACKING_ID, "UA-151671255-1").

start(_) ->
    init(),
    report().

stop() ->
    ok.

init() ->
    mnesia:create_table(service_mongoose_user_stats,
        [
            {type, set},
            {record_name, service_mongoose_user_stats},
            {attributes, record_info(fields, service_mongoose_user_stats)},
            {disc_copies, [node() | nodes()]}
        ]),
    mnesia:wait_for_tables([service_mongoose_user_stats], 5000),
    maybe_make_and_save_new_client_id().

report() ->
    IsAllowed = ejabberd_config:get_local_option(service_mongoose_user_stats_is_allowed),
    case IsAllowed of
        true ->
            ReportUrl = ejabberd_config:get_local_option(google_analytics_url),
            report_user_stats(ReportUrl);
        _ -> ok
    end.

% Functions are spawned and not linked, as MongooseIM should not care if they fail or not.
% Moreover the MongooseIM's start should not be blocked.
report_user_stats(undefined) ->
    report_user_stats(?BASE_URL);
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
    send_reports(ReportUrl, Lines).

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

build_report(EventCategory, EventAction) ->
    build_report(EventCategory, EventAction, empty).

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
    Line = string:join(LstLine, ""),
    lager:debug("~p reported = ~p", [?MODULE, Line]),
    Line.

% https://developers.google.com/analytics/devguides/collection/protocol/v1/devguide#batch-limitations
% A maximum of 20 hits can be specified per request.
send_reports(ReportUrl, Lines) when length(Lines) =< 20 ->
    Headers = [],
    ContentType = "",
    Body = string:join(Lines, "\n"),
    Request = {ReportUrl, Headers, ContentType, Body},
    httpc:request(post, Request, [], []);
send_reports(ReportUrl, Lines) ->
    {NewBatch, RemainingLines} = lists:split(20, Lines),
    send_reports(ReportUrl, NewBatch),
    send_reports(ReportUrl, RemainingLines),
    ok.


maybe_event_label(empty) -> [];
maybe_event_label(EventLabel) ->
    LstEventLabel = term_to_string(EventLabel),
    ["&el=", LstEventLabel].

term_to_string(Term) ->
    R= io_lib:format("~p",[Term]),
    lists:flatten(R).

maybe_make_and_save_new_client_id() ->
    T = fun() ->
        case mnesia:read(service_mongoose_user_stats, client_id) of
            [] ->
                ClientId = rand:uniform(1000 * 1000 * 1000 * 1000 * 1000),
                mnesia:write(#service_mongoose_user_stats{key = client_id, value = ClientId}),
                ClientId;
            [#service_mongoose_user_stats{value = ClientId}] ->
                ClientId
        end
    end,
    {atomic, _} = mnesia:transaction(T).

get_client_id() ->
    T = fun() ->
        mnesia:read(service_mongoose_user_stats, client_id)
    end,
    {atomic, [Record]} = mnesia:transaction(T),
    #service_mongoose_user_stats{value = ClientId} = Record,
    ClientId.
