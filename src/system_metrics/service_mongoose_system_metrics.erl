-module(service_mongoose_system_metrics).
-author('jan.ciesla@erlang-solutions.com').

-behaviour(mongoose_service).
-behaviour(gen_server).

-define(DEFAULT_INITIAL_REPORT, timer:minutes(5)).
-define(DEFAULT_REPORT_AFTER, timer:hours(3)).
-ifdef(PROD_NODE).
-define(TRACKING_ID, "UA-151671255-3").
-else.
-define(TRACKING_ID, "UA-151671255-2").
-endif.
-define(TRACKING_ID_CI, "UA-151671255-1").

-include("mongoose.hrl").

-export([start/1, stop/0]).
-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-export([verify_if_configured/0]).

-record(system_metrics_state, {report_after, reporter_monitor = none, reporter_pid = none}).

-type system_metrics_state() :: #system_metrics_state{}.
-type client_id() :: string().

-spec verify_if_configured() -> ok | ignore.
verify_if_configured() ->
    Services = ejabberd_config:get_local_option_or_default(services, []),
    case proplists:is_defined(?MODULE, Services) of
        false ->
            ?WARNING_MSG(msg_removed_from_config(), []),
            ignore;
        true ->
            ok
    end.

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
                    mongoose_system_metrics_sender:send(ClientId, Reports),
                    mongoose_system_metrics_file:save(Reports)
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
    R = proplists:get_value(periodic_report, Args, timer:minutes(5)),
    {I, R};
get_timeouts(Args, _) ->
    ejabberd_config:add_local_option(google_analytics_tracking_id, ?TRACKING_ID),
    I = proplists:get_value(initial_report, Args, ?DEFAULT_INITIAL_REPORT),
    R = proplists:get_value(periodic_report, Args, ?DEFAULT_REPORT_AFTER),
    {I, R}.

-spec report_transparency(proplists:proplist()) -> skip | continue.
report_transparency(Args) ->
    case {explicit_no_report(Args), explicit_gathering_agreement(Args)} of
        {true, ____} -> skip;
        {____, true} -> continue;
        {____, ____} ->
            ?WARNING_MSG(msg_accept_terms_and_conditions(), [mongoose_system_metrics_file:location()]),
            continue
    end.

explicit_no_report(Args) ->
    proplists:get_value(no_report, Args, false).
explicit_gathering_agreement(Args) ->
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

%%-----------------------------------------
%% Internal
%%-----------------------------------------
msg_removed_from_config() ->
    <<"We're sorry to hear you don't want to share the system's metrics with us. "
      "These metrics would enable us to improve MongooseIM and know where to focus our efforts. "
      "To stop being notified, you can add this to the services section of your config file: \n"
      "    '{services_mongoose_system_metrics, [no_report]}' \n"
      "For more info on how to customise, read, enable, and disable the metrics visit: \n"
      "- MongooseIM docs - \n"
      "     https://mongooseim.readthedocs.io/en/latest/operation-and-maintenance/System-Metrics-Privacy-Policy/ \n"
      "- MongooseIM GitHub page - https://github.com/esl/MongooseIM">>.

msg_accept_terms_and_conditions() ->
    <<"We are gathering the MongooseIM system's metrics to analyse the trends and needs of our users, "
      "improve MongooseIM, and know where to focus our efforts. "
      "For more info on how to customise, read, enable, and disable these metrics visit: \n"
      "- MongooseIM docs - \n"
      "      https://mongooseim.readthedocs.io/en/latest/operation-and-maintenance/System-Metrics-Privacy-Policy/ \n"
      "- MongooseIM GitHub page - https://github.com/esl/MongooseIM \n"
      "The last sent report is also written to a file ~s">>.
