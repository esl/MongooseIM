-module(service_mongoose_system_metrics).
-author('jan.ciesla@erlang-solutions.com').

-behaviour(mongoose_service).
-behaviour(gen_server).

-define(DEFAULT_INITIAL_REPORT, timer:minutes(5)).
-define(DEFAULT_REPORT_AFTER, timer:hours(3)).
-define(TRACKING_ID, "UA-151671255-2").
-define(TRACKING_ID_CI, "UA-151671255-1").

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
    {InitialReport, ReportAfter} = metrics_module_config(Args),
    erlang:send_after(InitialReport, self(), spawn_reporter),
    {ok, #system_metrics_state{report_after = ReportAfter}}.
    
handle_info(spawn_reporter, #system_metrics_state{report_after = ReportAfter,
                                                  reporter_monitor = none,
                                                  reporter_pid = none} = State) ->
    case get_client_id() of
        {error, no_client_id} -> {stop, no_client_id, State};
        {ok, ClientId} ->
            {Pid, Monitor} = spawn_monitor(
                fun() ->
                    Reports = mongoose_system_metrics_collector:collect(),
                    mongoose_system_metrics_sender:send(ClientId, Reports)
                end),
            erlang:send_after(ReportAfter, self(), spawn_reporter),
            {noreply, State#system_metrics_state{reporter_monitor = Monitor,
                                                 reporter_pid = Pid}}
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

% trying to get client ID 20 times, because it seems fine
-spec get_client_id() -> {ok, client_id()} | {error, no_client_id}.
get_client_id() ->
    get_client_id(20).

get_client_id(0) ->
    {error, no_client_id};
get_client_id(Counter) when Counter > 0 ->
    T = fun() ->
        case mnesia:read(service_mongoose_system_metrics, client_id) of
            [] ->
                ClientId = uuid:uuid_to_string(uuid:get_v4()),
                mnesia:write(#service_mongoose_system_metrics{key = client_id, value = ClientId}),
                ClientId;
            [#service_mongoose_system_metrics{value = ClientId}] ->
                ClientId
        end
    end,
    case mnesia:transaction(T) of
        {aborted, {no_exists, service_mongoose_system_metrics}} ->
            maybe_create_table(),
            get_client_id(Counter - 1);
        {atomic, ClientId} ->
            {ok, ClientId}
    end.

maybe_create_table() ->
    mnesia:create_table(service_mongoose_system_metrics,
        [
            {type, set},
            {record_name, service_mongoose_system_metrics},
            {attributes, record_info(fields, service_mongoose_system_metrics)},
            {ram_copies, [node()]}
        ]),
    mnesia:add_table_copy(service_mongoose_system_metrics, node(), ram_copies).

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

% %%-----------------------------------------
% %% Unused
% %%-----------------------------------------

handle_cast(_Request, State) ->
    {noreply, State}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
terminate(_Reason, _State) ->
   ok.
