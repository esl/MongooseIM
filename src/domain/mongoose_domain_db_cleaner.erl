%% Cleaning is triggered from all MongooseIM nodes.
%% Though, it's quick, if there is nothing to remove.
-module(mongoose_domain_db_cleaner).
-include("mongoose_logger.hrl").

-export([start/1, stop/0]).
-export([start_link/1, request_delete_domain/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-ignore_xref([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
              start_link/1, terminate/2]).

%% ---------------------------------------------------------------------------
%% Config

%% ---------------------------------------------------------------------------
%% Client code

-spec start(mongoose_service:options()) -> ok.
start(Opts) ->
    ChildSpec =
        {?MODULE,
         {?MODULE, start_link, [Opts]},
         permanent, infinity, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec),
    ok.

-spec stop() -> ok.
stop() ->
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    ok.

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-spec request_delete_domain(jid:lserver(), mongooseim:host_type()) -> ok.
request_delete_domain(Domain, HostType) ->
    gen_server:cast(?MODULE, {delete_domain, Domain, HostType}).

%% ---------------------------------------------------------------------------
%% Server callbacks

init(#{event_cleaning_interval := Interval, event_max_age := MaxAge}) ->
    ?LOG_INFO(#{what => domain_cleaner_start, cleaning_interval => Interval, max_age => MaxAge}),
    State = #{max_age => MaxAge},
    self() ! schedule_removal,
    timer:send_interval(timer:seconds(Interval), schedule_removal),
    {ok, State}.

handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, ok, State}.

handle_cast({delete_domain, Domain, HostType}, State) ->
    {noreply, handle_delete_domain(Domain, HostType, State)};
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

%% We are ensuring that to remove events, they have to be in the database
%% for some amount of time
schedule_removal(State = #{max_age := MaxAge}) ->
    try mongoose_domain_sql:get_minmax_event_id() of
        {_Min, LastEventId} ->
            Msg = {do_removal, LastEventId},
            erlang:start_timer(timer:seconds(MaxAge), self(), Msg)
    catch Class:Reason:Stacktrace ->
        %% It's safe to skip scheduling
        ?LOG_ERROR(#{what => domain_cleaning_schedule_failed,
                     text => <<"Failed to get LastEventId">>,
                     class => Class, reason => Reason, stacktrace => Stacktrace})
    end,
    State.

handle_timeout(_TimerRef, {do_removal, LastEventId}, State) ->
    mongoose_domain_sql:delete_events_older_than(LastEventId),
    State.

handle_delete_domain(Domain, HostType, State) ->
    try
        mongoose_domain_api:do_delete_domain_in_progress(Domain, HostType)
    catch Class:Reason:Stacktrace ->
              ?LOG_ERROR(#{what => domain_deletion_failed,
                           domain => Domain, host_type => HostType,
                           class => Class, reason => Reason, stacktrace => Stacktrace})
    end,
    State.
