-module(service_mongoose_system_metrics).
-author('jan.ciesla@erlang-solutions.com').

-behaviour(mongoose_service).
-behaviour(gen_server).

-define(DEFAULT_INITIAL_REPORT, timer:minutes(5)).
-define(DEFAULT_REPORT_AFTER, timer:hours(3)).
-define(TRACKING_ID, "UA-151671255-2").
-define(TRACKING_ID_CI, "UA-151671255-1").
-define(MSG_REMOVED_FROM_CONFIG,
        "Are you sure you don't want to report anything?"
        "TODO: Explain this better"
       ).
-define(MSG_ACCEPT_TERMS_AND_CONDITIONS,
        "Do you agree with reporting?"
        "TODO: Explain this better"
       ).

-include("mongoose.hrl").

-export([start/1, stop/0]).
-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-record(system_metrics_state, {report_after, reporter_monitor = none, reporter_pid = none}).

-type system_metrics_state() :: #system_metrics_state{}.
-type client_id() :: string().

-spec start(proplists:proplist()) -> {ok, pid()}.
start(Args) ->
    Spec = {?MODULE, {?MODULE, start_link, [Args]}, temporary, brutal_kill, worker, [?MODULE]},
    {ok, _} = ejabberd_sup:start_child(Spec).

-spec stop() -> ok.
stop() ->
    ejabberd_sup:stop_child(?MODULE).

-spec start_link(proplists:proplist()) -> {ok, pid()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-spec init(proplists:proplist()) -> {ok, system_metrics_state()}.
init(Args) ->
    case report_transparency(Args) of
        skip -> ignore;
        continue ->
            {InitialReport, ReportAfter} = metrics_module_config(Args),
            erlang:send_after(InitialReport, self(), spawn_reporter),
            {ok, #system_metrics_state{report_after = ReportAfter}}
    end.

handle_info(spawn_reporter, #system_metrics_state{report_after = ReportAfter,
                                                  reporter_monitor = none,
                                                  reporter_pid = none} = State) ->
    case get_client_id() of
        {ok, ClientId} ->
            {Pid, Monitor} = spawn_monitor(
                fun() ->
                    Reports = mongoose_system_metrics_collector:collect(),
                    mongoose_system_metrics_sender:send(ClientId, Reports)
                end),
            erlang:send_after(ReportAfter, self(), spawn_reporter),
            {noreply, State#system_metrics_state{reporter_monitor = Monitor,
                                                 reporter_pid = Pid}};
        {error, _} -> {stop, no_client_id, State}
    end;
handle_info(spawn_reporter, #system_metrics_state{reporter_pid = Pid} = State) ->
    exit(Pid, kill),
    self() ! spawn_reporter,
    {noreply, State#system_metrics_state{reporter_monitor = none, reporter_pid = none}};
handle_info({'DOWN', CollectorMonitor, _, _, _},
                #system_metrics_state{reporter_monitor = CollectorMonitor} = State) ->
    {noreply, State#system_metrics_state{reporter_monitor = none, reporter_pid = none}};
handle_info(_Message, State) ->
    {noreply, State}.


% %%-----------------------------------------
% %% Helpers
% %%-----------------------------------------

-spec get_client_id() -> {ok, client_id()} | {error, any()}.
get_client_id() ->
    case mongoose_cluster_id:get_cached_cluster_id() of
        {error, _} = Err -> Err;
        {ok, ID} when is_binary(ID) -> {ok, binary_to_list(ID)}
    end.

-spec metrics_module_config(list()) -> {non_neg_integer(), non_neg_integer()}.
metrics_module_config(Args) ->
    {InitialReport, ReportAfter} = get_timeouts(Args, os:getenv("CI")),
    ExtraTrackingID = proplists:get_value(tracking_id, Args, undefined),
    ejabberd_config:add_local_option(extra_google_analytics_tracking_id, ExtraTrackingID),
    {InitialReport, ReportAfter}.

get_timeouts(Args, "true") ->
    ejabberd_config:add_local_option(google_analytics_tracking_id, ?TRACKING_ID_CI),
    I = proplists:get_value(initial_report, Args, timer:seconds(20)),
    R = proplists:get_value(report_after, Args, timer:minutes(5)),
    {I, R};
get_timeouts(Args, _) ->
    ejabberd_config:add_local_option(google_analytics_tracking_id, ?TRACKING_ID),
    I = proplists:get_value(initial_report, Args, ?DEFAULT_INITIAL_REPORT),
    R = proplists:get_value(report_after, Args, ?DEFAULT_REPORT_AFTER),
    {I, R}.

-spec report_transparency(proplists:proplist()) -> skip | continue.
report_transparency(Args) ->
    case {manually_removed_from_config(Args),
              explicit_no_report(Args),
                  will_printout(Args)} of
        {true, ____, ____} -> ?WARNING_MSG(?MSG_REMOVED_FROM_CONFIG, []), skip;
        {____, true, ____} -> skip;
        {____, ____, true} -> continue;
        {____, ____, ____} -> ?WARNING_MSG(?MSG_ACCEPT_TERMS_AND_CONDITIONS, []), continue
    end.

manually_removed_from_config(Args) ->
    proplists:get_value(removed_from_config, Args, false).
explicit_no_report(Args) ->
    proplists:get_value(no_report, Args, false).
will_printout(Args) ->
    proplists:get_value(report, Args, false).

% %%-----------------------------------------
% %% Unused
% %%-----------------------------------------

handle_cast(_Request, State) ->
    {noreply, State}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
terminate(_Reason, _State) ->
   ok.
