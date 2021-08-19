%% Cleaning is triggered from all MongooseIM nodes.
%% Though, it's quick, if there is nothing to remove.
-module(mongoose_domain_db_cleaner).
-include("mongoose_logger.hrl").

-export([start/1, stop/0]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
              start_link/1, terminate/2]).

%% ---------------------------------------------------------------------------
%% Config

default_cleaning_interval() ->
    1800. %% 30 minutes

default_max_age() ->
    7200. %% 2 hours

%% ---------------------------------------------------------------------------
%% Client code

start(Opts) ->
    ChildSpec =
        {?MODULE,
         {?MODULE, start_link, [Opts]},
         permanent, infinity, worker, [?MODULE]},
    supervisor:start_child(mongoose_domain_sup, ChildSpec),
    ok.

stop() ->
    supervisor:terminate_child(mongoose_domain_sup, ?MODULE),
    supervisor:delete_child(mongoose_domain_sup, ?MODULE),
    ok.

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% ---------------------------------------------------------------------------
%% Server callbacks

init(Opts) ->
    Interval = proplists:get_value(event_cleaning_interval, Opts, default_cleaning_interval()),
    MaxAge = proplists:get_value(event_max_age, Opts, default_max_age()),
    ?LOG_INFO(#{what => domain_cleaner_start, cleaning_interval => Interval, max_age => MaxAge}),
    State = #{max_age => MaxAge},
    self() ! schedule_removal,
    timer:send_interval(timer:seconds(Interval), schedule_removal),
    {ok, State}.

handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

handle_info(schedule_removal, State) ->
    {noreply, schedule_removal(State)};
handle_info({timeout, TimerRef, Msg}, State) ->
    {noreply, handle_timeout(TimerRef, Msg, State)};
handle_info(Info, State) ->
    ?UNEXPECTED_INFO(Info),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Server helpers

schedule_removal(State = #{max_age := MaxAge}) ->
    LastEventId = mongoose_domain_sql:get_max_event_id_or_set_dummy(),
    Msg = {do_removal, LastEventId},
    erlang:start_timer(timer:seconds(MaxAge), self(), Msg),
    State.

handle_timeout(_TimerRef, {do_removal, LastEventId}, State) ->
    mongoose_domain_sql:delete_events_older_than(LastEventId),
    State.
