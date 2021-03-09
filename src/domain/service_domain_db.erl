-module(service_domain_db).

-behaviour(mongoose_service).

-include("mongoose_config_spec.hrl").
-include("mongoose_logger.hrl").

-define(GROUP, service_domain_db_group).

-export([start/1, stop/0, config_spec/0]).
-export([start_link/0]).
-export([enabled/0]).
-export([force_check_for_updates/0]).
-export([sync/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ---------------------------------------------------------------------------
%% Client code

start(Opts) ->
    mongoose_domain_sql:start(Opts),
    mongoose_domain_db_cleaner:start(Opts),
    ChildSpec =
        {?MODULE,
         {?MODULE, start_link, []},
         permanent, infinity, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec),
    ok.

stop() ->
    mongoose_domain_db_cleaner:stop(),
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{
               <<"event_cleaning_interval">> => #option{type = integer,
                                                        validate = positive},
               <<"event_max_age">> => #option{type = integer,
                                              validate = positive}
              }}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enabled() ->
    is_pid(whereis(?MODULE)).

force_check_for_updates() ->
    %% Send a broadcast message.
    case pg2:get_members(?GROUP) of
        Pids when is_list(Pids) ->
            [Pid ! check_for_updates || Pid <- Pids],
            ok;
        {error, _Reason} -> ok
    end.

%% Does nothing but blocks until every member processes its queue.
sync() ->
    case pg2:get_members(?GROUP) of
        Pids when is_list(Pids) ->
            [gen_server:call(Pid, ping) || Pid <- Pids],
            ok;
        {error, _Reason} -> ok
    end.

%% ---------------------------------------------------------------------------
%% Server callbacks

init([]) ->
    pg2:create(?GROUP),
    pg2:join(?GROUP, self()),
    LastEventId = mongoose_domain_sql:get_max_event_id(),
    PageSize = 10000,
    mongoose_domain_core:remove_all_unlocked(),
    mongoose_domain_loader:load_data_from_base(0, PageSize),
    ?LOG_INFO(#{what => domains_loaded, last_event_id => LastEventId}),
    State = #{last_event_id => LastEventId,
              check_for_updates_interval => 30000},
    {ok, handle_check_for_updates(State)}.

handle_call(ping, _From, State) ->
    {reply, pong, State};
handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?UNEXPECTED_CAST(Msg),
    {noreply, State}.

handle_info(check_for_updates, State) ->
    {noreply, handle_check_for_updates(State)};
handle_info(Info, State) ->
    ?UNEXPECTED_INFO(Info),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ---------------------------------------------------------------------------
%% Server helpers

handle_check_for_updates(State = #{last_event_id := LastEventId,
                                   check_for_updates_interval := Interval}) ->
    maybe_cancel_timer(State),
    receive_all_check_for_updates(),
    PageSize = 1000,
    LastEventId2 = mongoose_domain_loader:check_for_updates(LastEventId, PageSize),
    TRef = erlang:send_after(Interval, self(), check_for_updates),
    State#{last_event_id => LastEventId2, check_for_updates => TRef}.

maybe_cancel_timer(#{check_for_updates_tref := TRef}) ->
    erlang:cancel_timer(TRef);
maybe_cancel_timer(_) ->
    ok.

receive_all_check_for_updates() ->
    receive check_for_updates -> ok after 0 -> ok end.
