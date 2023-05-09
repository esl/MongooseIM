-module(service_mongoose_system_metrics).
-author('jan.ciesla@erlang-solutions.com').

-behaviour(mongoose_service).
-behaviour(gen_server).

-include("mongoose_config_spec.hrl").

-ifdef(PROD_NODE).
% The value "Secret" here is an app id from Google Analytics.
% We refer to it as "Secret" because it is named there that way.
-define(TRACKING_ID, #{id => "G-B4S18X6KY5",
                       secret => "Rj6lopOLQZ2HPLj50QtYeQ"}).
-else.
-define(TRACKING_ID, #{id => "G-7KQE4W9SVJ",
                       secret => "8P4wQIkwSV6zay22uKsnLg"}).
-endif.
-define(TRACKING_ID_CI, #{id => "G-VB91V60SKT",
                          secret => "LFxdCgDsSa-OAPtBnLDqpQ"}).

-include("mongoose.hrl").

-export([start/1, stop/0, config_spec/0]).
-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-export([verify_if_configured/0]).

-ignore_xref([start_link/1]).

-record(system_metrics_state,
        {report_after :: non_neg_integer(),
         reporter_monitor = none :: none | reference(),
         reporter_pid = none :: none | pid(),
         prev_report = [] :: [mongoose_system_metrics_collector:report_struct()],
         tracking_ids :: [tracking_id()]}).

-export_type([client_id/0, tracking_id/0]).

-type system_metrics_state() :: #system_metrics_state{}.
-type client_id() :: string().
-type tracking_id() :: #{id => string(), secret => string()}.

-spec verify_if_configured() -> ok | ignore.
verify_if_configured() ->
    case mongoose_service:is_loaded(?MODULE) of
        false ->
            %% Technically, notice level.
            %% Though make it louder, in case people set minimum level as warning.
            ?LOG_WARNING(#{what => system_metrics_disabled,
                           text => msg_removed_from_config()}),
            ignore;
        true ->
            ok
    end.

-spec start(mongoose_service:options()) -> {ok, pid()}.
start(Opts) ->
    Spec = {?MODULE, {?MODULE, start_link, [Opts]}, temporary, brutal_kill, worker, [?MODULE]},
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
                 <<"report">> => #option{type = boolean},
                 <<"tracking_id">> => tracking_id_section()
                },
       defaults = #{<<"initial_report">> => timer:minutes(5),
                    <<"periodic_report">> => timer:hours(3)}
      }.

tracking_id_section() ->
    #section{
       items = #{<<"id">> => #option{type = string,
                                     validate = non_empty},
                 <<"secret">> => #option{type = string,
                                         validate = non_empty}
                },
       required = all
       }.

-spec start_link(mongoose_service:options()) -> {ok, pid()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-spec init(mongoose_service:options()) -> {ok, system_metrics_state()}.
init(Opts) ->
    case report_transparency(Opts) of
        skip -> ignore;
        continue ->
            #{initial_report := InitialReport, periodic_report := PeriodicReport} = Opts,
            TrackingIds = tracking_ids(Opts),
            erlang:send_after(InitialReport, self(), spawn_reporter),
            {ok, #system_metrics_state{report_after = PeriodicReport,
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

-spec tracking_ids(mongoose_service:options()) -> [tracking_id()].
tracking_ids(#{tracking_id := ExtraTrackingId}) ->
    [predefined_tracking_id(), ExtraTrackingId];
tracking_ids(#{}) ->
    [predefined_tracking_id()].

-spec predefined_tracking_id() -> tracking_id().
predefined_tracking_id() ->
    case os:getenv("CI") of
        "true" -> ?TRACKING_ID_CI;
        _ -> ?TRACKING_ID
    end.

-spec report_transparency(mongoose_service:options()) -> skip | continue.
report_transparency(#{report := false}) ->
    skip;
report_transparency(#{report := true}) ->
    continue;
report_transparency(#{}) ->
    File = mongoose_system_metrics_file:location(),
    Text = iolist_to_binary(io_lib:format(msg_accept_terms_and_conditions(), [File])),
    ?LOG_WARNING(#{what => report_transparency, text => Text, report_filename => File}),
    continue.

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
