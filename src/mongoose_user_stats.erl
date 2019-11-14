-module(mongoose_user_stats).

-author('aleksander.lisiecki@erlang-solutions.com').

-define(TAB_NAME, persistent_user_info).

% dummy is needed as it is not possible to create a mnesia table from single filed record.
-record(?TAB_NAME, {client_id, dummy}).

-export([report/0]).

-define(BASE_URL, "https://www.google-analytics.com/batch").
-define(TRACKING_ID, "UA-151671255-1").

report() ->
    IsAllowed = ejabberd_config:get_local_option(mongoose_user_stats_is_allowed),
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

client_id() ->
    CreateTableResult = mnesia:create_table(?TAB_NAME,
        [
            {type, set},
            {record_name, ?TAB_NAME},
            {attributes, record_info(fields, ?TAB_NAME)},
            {disc_copies, [node() | nodes()]}
        ]),
    get_client_id(CreateTableResult).

get_client_id({aborted, {already_exists, persistent_user_info}}) ->
    case ets:tab2list(?TAB_NAME) of
                [] ->
                    new_client_id();
                [#?TAB_NAME{client_id = ClientId}] ->
                    ClientId
            end;
get_client_id({atomic, ok}) ->
    mnesia:wait_for_tables([?TAB_NAME], 5000),
    new_client_id().

new_client_id() ->
    ClientId = rand:uniform(1000 * 1000 * 1000 * 1000 * 1000),
    T = fun() ->
        mnesia:write(#?TAB_NAME{client_id = ClientId})
    end,
    mnesia:transaction(T),
    ClientId.
