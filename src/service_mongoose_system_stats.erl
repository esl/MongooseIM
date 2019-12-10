-module(service_mongoose_system_stats).
-author('jan.ciesla@erlang-solutions.com').

-behaviour(gen_server).

-define(BASE_URL, "https://www.google-analytics.com/batch").
-define(TRACKING_ID, "UA-151671255-1").
-define(DEFAULT_REPORT_AFTER, 60 * 60 * 1000).
-define(STAT_TYPE, [mongoose_system_stats]).

-include("mongoose.hrl").

-export([start/1, stop/0]).
-export([start_link/0,
         init/1,
         handle_event/4,
         handle_continue/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-record(system_stats_state, {
    client_id = '',
    reports = [],
    report_after = ?DEFAULT_REPORT_AFTER,
    loop_timer_ref
    }).
-type system_stats_state() :: #system_stats_state{}.

-record(service_mongoose_system_stats, {key, value}).

-spec start(proplists:proplist()) -> {ok, pid()}.
start(_Args) ->
    Spec = {?MODULE, {?MODULE, start_link, []}, temporary, brutal_kill, worker, [?MODULE]},
    {ok, _} = ejabberd_sup:start_child(Spec).

-spec stop() -> ok.
stop() ->
    ejabberd_sup:stop_child(?MODULE).

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec init([]) -> {ok, system_stats_state(), {continue, do_init}}.
init(_Args) ->
    {ok, #system_stats_state{}, {continue, do_init}}.

-spec handle_continue(do_init, system_stats_state()) ->
    {noreply, system_stats_state()} | {stop, no_client_id, system_stats_state()}.
handle_continue(do_init, State) ->
    case get_client_id() of
        no_client_id -> {stop, no_client_id, State};
        Value ->
            telemetry:attach(
                <<"mongoose_system_stats">>,
                ?STAT_TYPE,
                fun service_mongoose_system_stats:handle_event/4,
                [] ),
            TimerRef = erlang:send_after(?DEFAULT_REPORT_AFTER, self(), flush_reports),
            NewState = State#system_stats_state{
                client_id = Value,
                loop_timer_ref = TimerRef
                },
            report_hosts_count(),
            {noreply, NewState}
    end.

handle_event(?STAT_TYPE, Metrics, Metadata , _Config) ->
    gen_server:cast(?MODULE, {add_report, {Metrics, Metadata}});
handle_event(_StatType, _Map1, _Map2, _Config) ->
    ok.

parse_telemetry_report(ClientId, #{module := Module}, #{host := _Host, opts := Opts}) ->
    Backend = proplists:get_value(backend, Opts, none),
    build_report(ClientId, modules, Module, Backend);
parse_telemetry_report(ClientId, #{hosts_count := HostsCount}, _Metadata) ->
    build_report(ClientId, hosts_count, HostsCount);
parse_telemetry_report(ClientId,
                       #{event_category := EventCategory},
                       #{event_action := EventAction}) ->
    build_report(ClientId, EventCategory, EventAction);
parse_telemetry_report(ClientId,
                       #{event_category := EventCategory},
                       #{event_action := EventAction, event_label := EventLabel}) ->
    build_report(ClientId, EventCategory, EventAction, EventLabel);
parse_telemetry_report(_ClientId, _Metrics, _Metadata) ->
    %incorrect reports are ignored
    ok.

handle_info(flush_reports, #system_stats_state{reports = Reports,
                                               loop_timer_ref = TimerRef,
                                               report_after = ReportAfter} = State ) ->
    UrlBase = get_url(),
    erlang:cancel_timer(TimerRef),
    flush_reports(UrlBase, Reports),
    NewTimerRef = erlang:send_after(ReportAfter, self(), flush_reports),
    {noreply, State#system_stats_state{reports = [], loop_timer_ref = NewTimerRef}};
handle_info(_Message, _State) ->
    ok.

get_url() ->
    ejabberd_config:get_local_option_or_default(google_analytics_url, ?BASE_URL).

handle_cast({add_report, {Metrics, Metadata}},
            #system_stats_state{reports = Reports, client_id = ClientId} = State) ->
    NewReport = parse_telemetry_report(ClientId, Metrics, Metadata),
    FullReport = [NewReport | Reports],
    maybe_flush_report(length(FullReport)),
    {noreply, State#system_stats_state{reports = FullReport}}.

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
-spec flush_reports(string(), list(string())) -> {ok, term()} | {error, term()}.
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

get_client_id() ->
    get_client_id(20). % trying to get client ID 20 times, because it seems fine

get_client_id(0) ->
    no_client_id;
get_client_id(Counter) when Counter > 0 ->
    T = fun() ->
        mnesia:read(service_mongoose_system_stats, client_id)
    end,
    case mnesia:transaction(T) of
        {aborted, {no_exists, service_mongoose_system_stats}} ->
            maybe_create_table(),
            get_client_id(Counter - 1);
        {atomic, []} ->
            maybe_make_and_save_new_client_id(),
            get_client_id(Counter - 1);
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
            {ram_copies, [node()]}
        ]),
    mnesia:add_table_copy(service_mongoose_system_stats, node(), ram_copies).

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

build_report(ClientId, EventCategory, EventAction) ->
    build_report(ClientId, EventCategory, EventAction, none).

build_report(ClientId, EventCategory, EventAction, EventLabel) ->
    MaybeLabel = maybe_event_label(EventLabel),
    LstClientId = term_to_string(ClientId),
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