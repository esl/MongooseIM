-module(service_domain_db).

-behaviour(mongoose_service).

-include("mongoose_config_spec.hrl").
-include("mongoose_logger.hrl").
-include("backward_compatible.hrl").

-define(GROUP, service_domain_db_group).
-define(LAST_EVENT_ID_KEY, {?MODULE, last_event_id}).

-export([start/1, stop/0, restart/0, config_spec/0]).
-export([start_link/0]).
-export([enabled/0]).
-export([force_check_for_updates/0]).
-export([sync/0, sync_local/0]).

%% exported for integration tests only!
-export([reset_last_event_id/0]).

-ignore_xref([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
              init/1, start_link/0, sync/0, sync_local/0, terminate/2, reset_last_event_id/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ---------------------------------------------------------------------------
%% Client code

start(Opts) ->
    mongoose_domain_sql:start(Opts),
    ChildSpec =
        {?MODULE,
         {?MODULE, start_link, []},
         permanent, infinity, worker, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec),
    mongoose_domain_db_cleaner:start(Opts),
    ok.

stop() ->
    mongoose_domain_db_cleaner:stop(),
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE),
    ok.

restart() ->
    %% if service goes out of sync with DB this interface
    %% can be used to restart the service.
    %% it's enough to just shut down gen_server, supervisor
    %% will restart it.
    gen_server:cast(?MODULE, reset_and_shutdown).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{
               <<"event_cleaning_interval">> => #option{type = integer,
                                                        validate = positive},
               <<"event_max_age">> => #option{type = integer,
                                              validate = positive},
               <<"db_pool">> => #option{type = atom,
                                        validate = pool_name}
              }}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enabled() ->
    mongoose_service:is_loaded(?MODULE).

force_check_for_updates() ->
    %% Send a broadcast message.
    case ?PG_GET_MEMBERS(?GROUP) of
        [_|_] = Pids ->
            [Pid ! check_for_updates || Pid <- Pids],
            ok;
        _ ->
            ok
    end.

%% Does nothing but blocks until every member processes its queue.
sync() ->
    case ?PG_GET_MEMBERS(?GROUP) of
        [_|_] = Pids ->
            [gen_server:call(Pid, ping) || Pid <- Pids],
            ok;
        _ ->
            ok
    end.

sync_local() ->
    gen_server:call(?MODULE, ping).

%% ---------------------------------------------------------------------------
%% Server callbacks

init([]) ->
    ?PG_JOIN(?GROUP, self()),
    gen_server:cast(self(), initial_loading),
    %% initial state will be set on initial_loading processing
    {ok, #{}}.

handle_call(ping, _From, State) ->
    {reply, pong, State};
handle_call(Request, From, State) ->
    ?UNEXPECTED_CALL(Request, From),
    {reply, ok, State}.

handle_cast(initial_loading, State) ->
    LastEventId = initial_load(get_last_event_id()),
    ?LOG_INFO(#{what => domains_loaded, last_event_id => LastEventId}),
    NewState = State#{last_event_id => LastEventId,
                      check_for_updates_interval => 30000},
    {noreply, handle_check_for_updates(NewState)};
handle_cast(reset_and_shutdown, State) ->
    %% to ensure that domains table is re-read from
    %% scratch, we must reset the last event id.
    reset_last_event_id(),
    {stop, shutdown, State};
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

initial_load(undefined) ->
    LastEventId = mongoose_domain_sql:get_max_event_id_or_set_dummy(),
    PageSize = 10000,
    mongoose_domain_loader:load_data_from_base(0, PageSize),
    mongoose_domain_loader:remove_outdated_domains_from_core(),
    set_last_event_id(LastEventId),
    LastEventId;
initial_load(LastEventId) when is_integer(LastEventId) ->
    LastEventId. %% Skip initial init

handle_check_for_updates(State = #{last_event_id := LastEventId,
                                   check_for_updates_interval := Interval}) ->
    maybe_cancel_timer(State),
    receive_all_check_for_updates(),
    PageSize = 1000,
    LastEventId2 = mongoose_domain_loader:check_for_updates(LastEventId, PageSize),
    maybe_set_last_event_id(LastEventId, LastEventId2),
    TRef = erlang:send_after(Interval, self(), check_for_updates),
    State#{last_event_id => LastEventId2, check_for_updates => TRef}.

maybe_cancel_timer(#{check_for_updates_tref := TRef}) ->
    erlang:cancel_timer(TRef);
maybe_cancel_timer(_) ->
    ok.

receive_all_check_for_updates() ->
    receive check_for_updates -> receive_all_check_for_updates() after 0 -> ok end.

maybe_set_last_event_id(LastEventId, LastEventId) ->
    ok;
maybe_set_last_event_id(_LastEventId, LastEventId2) ->
    set_last_event_id(LastEventId2).

set_last_event_id(LastEventId) ->
    persistent_term:put(?LAST_EVENT_ID_KEY, LastEventId).

get_last_event_id() ->
    persistent_term:get(?LAST_EVENT_ID_KEY, undefined).

reset_last_event_id() ->
    set_last_event_id(undefined).
