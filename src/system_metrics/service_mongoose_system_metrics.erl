-module(service_mongoose_system_metrics).
-author('jan.ciesla@erlang-solutions.com').

-behaviour(mongoose_service).
-behaviour(gen_server).

-include("mongoose_config_spec.hrl").

-define(DEFAULT_INITIAL_REPORT, timer:minutes(5)).
-define(DEFAULT_REPORT_AFTER, timer:hours(3)).
-ifdef(PROD_NODE).
-define(TRACKING_ID, "UA-151671255-3").
-else.
-define(TRACKING_ID, "UA-151671255-2").
-endif.
-define(TRACKING_ID_CI, "UA-151671255-1").

-include("mongoose.hrl").

-export([start/1, stop/0, config_spec/0]).
-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

%% config spec callbacks
-export([process_report_option/1]).

-export([verify_if_configured/0]).

-ignore_xref([start_link/1]).

-record(system_metrics_state,
        {report_after :: non_neg_integer(),
         reporter_monitor = none :: none | reference(),
         reporter_pid = none :: none | pid(),
         prev_report = [] :: [mongoose_system_metrics_collector:report_struct()],
         tracking_ids :: [tracking_id()]}).

-type system_metrics_state() :: #system_metrics_state{}.
-type client_id() :: string().
-type tracking_id() :: string().

-spec verify_if_configured() -> ok | ignore.
verify_if_configured() ->
    Services = mongoose_config:get_opt(services, []),
    case proplists:is_defined(?MODULE, Services) of
        false ->
            %% Technically, notice level.
            %% Though make it louder, in case people set minimum level as warning.
            ?LOG_WARNING(#{what => system_metrics_disabled,
                           text => msg_removed_from_config()}),
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

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"initial_report">> => #option{type = integer,
                                                 validate = non_negative},
                 <<"periodic_report">> => #option{type = integer,
                                                  validate = non_negative},
                 <<"report">> => #option{type = boolean,
                                         process = fun ?MODULE:process_report_option/1,
                                         wrap = item},
                 <<"tracking_id">> => #option{type = string,
                                              validate = non_empty}
                }
      }.

process_report_option(true) -> report;
process_report_option(false) -> no_report.

-spec start_link(proplists:proplist()) -> {ok, pid()}.
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

-spec init(proplists:proplist()) -> {ok, system_metrics_state()}.
init(Args) ->
    case report_transparency(Args) of
        skip -> ignore;
        continue ->
            {InitialReport, ReportAfter, TrackingIds} = metrics_module_config(Args),
            erlang:send_after(InitialReport, self(), spawn_reporter),
            {ok, #system_metrics_state{report_after = ReportAfter,
                                       tracking_ids = TrackingIds}}
    end.

handle_info(spawn_reporter, #system_metrics_state{report_after = ReportAfter,
                                                  reporter_monitor = none,
                                                  reporter_pid = none,
                                                  prev_report = PrevReport,
                                                  tracking_ids = TrackingIds} = State) ->
    ServicePid = self(),
    case get_client_id() of
        {ok, ClientId} ->
            {Pid, Monitor} = spawn_monitor(
                fun() ->
                    Reports = mongoose_system_metrics_collector:collect(PrevReport),
                    mongoose_system_metrics_sender:send(ClientId, Reports, TrackingIds),
                    mongoose_system_metrics_file:save(Reports),
                    ServicePid ! {prev_report, Reports}
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
handle_info({prev_report, Report}, State) ->
    {noreply, State#system_metrics_state{prev_report = Report}};
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

-spec metrics_module_config(list()) -> {non_neg_integer(), non_neg_integer(), [tracking_id()]}.
metrics_module_config(Args) ->
    {InitialReport, ReportAfter, TrackingId} = get_config(Args, os:getenv("CI")),
    TrackingIds = case proplists:lookup(tracking_id, Args) of
        none -> [TrackingId];
        {_, ExtraTrackingId} -> [TrackingId, ExtraTrackingId]
    end,
    {InitialReport, ReportAfter, TrackingIds}.

get_config(Args, "true") ->
    I = proplists:get_value(initial_report, Args, timer:seconds(20)),
    R = proplists:get_value(periodic_report, Args, timer:minutes(5)),
    {I, R, ?TRACKING_ID_CI};
get_config(Args, _) ->
    I = proplists:get_value(initial_report, Args, ?DEFAULT_INITIAL_REPORT),
    R = proplists:get_value(periodic_report, Args, ?DEFAULT_REPORT_AFTER),
    {I, R, ?TRACKING_ID}.

-spec report_transparency(proplists:proplist()) -> skip | continue.
report_transparency(Args) ->
    case {explicit_no_report(Args), explicit_gathering_agreement(Args)} of
        {true, ____} -> skip;
        {____, true} -> continue;
        {____, ____} ->
            File = mongoose_system_metrics_file:location(),
            Text = iolist_to_binary(io_lib:format(msg_accept_terms_and_conditions(), [File])),
            ?LOG_WARNING(#{what => report_transparency,
                           text => Text, report_filename => File}),
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
    "  [services.service_mongoose_system_metrics]\n"
    "    report = false\n"
    "For more info on how to customise, read, enable, and disable the metrics visit: \n"
    "- MongooseIM docs - \n"
    "     https://esl.github.io/MongooseDocs/latest/operation-and-maintenance/System-Metrics-Privacy-Policy/ \n"
    "- MongooseIM GitHub page - https://github.com/esl/MongooseIM">>.

msg_accept_terms_and_conditions() ->
    <<"We are gathering the MongooseIM system's metrics to analyse the trends and needs of our users, "
    "improve MongooseIM, and know where to focus our efforts. "
    "For more info on how to customise, read, enable, and disable these metrics visit: \n"
    "- MongooseIM docs - \n"
    "      https://esl.github.io/MongooseDocs/latest/operation-and-maintenance/System-Metrics-Privacy-Policy/ \n"
    "- MongooseIM GitHub page - https://github.com/esl/MongooseIM \n"
    "The last sent report is also written to a file ~s">>.
