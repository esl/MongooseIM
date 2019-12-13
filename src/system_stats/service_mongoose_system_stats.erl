-module(service_mongoose_system_stats).
-author('jan.ciesla@erlang-solutions.com').

-behaviour(gen_server).

-define(DEFAULT_INITIAL_REPORT, 100). %TODO: restore values
-define(DEFAULT_REPORT_AFTER, 1000).

-include("mongoose.hrl").

-export([start/1, stop/0]).
-export([start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-type system_stats_state() :: pos_integer().

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

-spec init(proplists:proplist()) -> {ok, system_stats_state()}.
init(Args) ->
    InitialReport = proplists:get_value(initial_report, Args, ?DEFAULT_INITIAL_REPORT),
    ReportAfter = proplists:get_value(report_after, Args, ?DEFAULT_REPORT_AFTER),
    load_client_id(),
    erlang:send_after(InitialReport, self(), spawn_gatherer),
    {ok, ReportAfter}.
    
handle_info(spawn_gatherer, ReportAfter) ->
    spawn(mongoose_system_stats_gatherer, gather, []),
    erlang:send_after(ReportAfter, self(), spawn_gatherer),
    {noreply, ReportAfter};
handle_info(_Message, _State) ->
    ok.

% %%-----------------------------------------
% %% Helpers
% %%-----------------------------------------

load_client_id() ->
    ClientId = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    persistent_term:put(ga_client_id, ClientId).
%TODO: save client id to file and if file exists, read from file

% %%-----------------------------------------
% %% Unused
% %%-----------------------------------------

handle_cast(_Request, State) ->
    {noreply, State}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.
terminate(_Reason, _State) ->
   ok.
