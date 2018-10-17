%%%-------------------------------------------------------------------
%%% @author denys
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. Oct 2018 16:08
%%%-------------------------------------------------------------------
-module(mongoose_watchdog).
-author("DenysGonchar").

-behaviour(gen_event).
-behaviour(mongoose_service).


%% gen_event callbacks
-export([init/1,
         handle_event/2,
         handle_call/2,
         terminate/2,
         code_change/3]).

%% mongoose_service callbacks
-export([start/1,
         stop/0]).

%% API
-export([add_handler/2,
         call/2,
         get_alarms/0]).

-define(SERVER, ?MODULE).

-record(state, {default_handlers = [],
                active_alarms = []}).

%%%===================================================================
%%% API
%%%===================================================================
-spec add_handler(Handler :: atom() | {atom(), term()},
                  Args :: term()) ->
                     term().
add_handler(Handler, Args) ->
    safe_add_handler(?SERVER, Handler, Args).

-spec call(Handler :: atom() | {atom(), term()},
           Args :: term()) ->
              term().
call(Handler, Args) ->
    gen_event:call(?SERVER, Handler, Args).

get_alarms() ->
    gen_event:call(alarm_handler, ?MODULE, get_alarms).


%%%===================================================================
%%% mongoose_service callbacks
%%%===================================================================
-spec start(Opts :: list()) -> any().
start(Opts) ->
    OsMonEnv = proplists:get_value(os_mon_env, Opts, []),
    start_os_mon(OsMonEnv),
    MemsupSettings = proplists:get_value(memsup_settings, Opts, []),
    configure_memsup(MemsupSettings),
    DefaultHandlers = proplists:get_value(default_handlers, Opts,
                                          [{mongoose_watchdog_default, []}]),
    safe_add_handler(alarm_handler, ?MODULE, DefaultHandlers),
    start_event_manager(DefaultHandlers),
    ok.


-spec stop() -> any().
stop() ->
    gen_event:delete_handler(alarm_handler, ?MODULE, {?MODULE, stop, 0}).


%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(InitArgs :: term()) ->
    {ok, State :: #state{}} |
    {ok, State :: #state{}, hibernate} |
    {error, Reason :: term()}.
init(DefaultHandlers) ->
    {ok, #state{default_handlers = DefaultHandlers}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_event(Event :: term(), State :: #state{}) ->
    {ok, NewState :: #state{}} |
    {ok, NewState :: #state{}, hibernate} |
    {swap_handler, Args1 :: term(), NewState :: #state{},
     Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    remove_handler).
handle_event(Event, State) ->
    {ok, safe_handle_event(Event, State)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), State :: #state{}) ->
    {ok, Reply :: term(), NewState :: #state{}} |
    {ok, Reply :: term(), NewState :: #state{}, hibernate} |
    {swap_handler, Reply :: term(), Args1 :: term(), NewState :: #state{},
     Handler2 :: (atom() | {atom(), Id :: term()}), Args2 :: term()} |
    {remove_handler, Reply :: term()}).
handle_call(get_alarms, State) ->
    {ok, State#state.active_alarms, State};
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec terminate(Args :: (term() | {stop, Reason :: term()} | stop |
                         {error, {'EXIT', Reason :: term()}} |
                         remove_handler | {error, term()}),
                State :: term()) -> term().
terminate(_Arg, _State) ->
    catch gen_event:stop(?SERVER),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_os_mon(OsMonEnv) ->
    OS = proplists:get_value(start_os_sup, OsMonEnv, false),
    CPU = proplists:get_value(start_cpu_sup, OsMonEnv, false),
    Disk = proplists:get_value(start_disksup, OsMonEnv, false),
    case application:get_env(os_mon,start_memsup) of
        undefined -> %application is not loaded
            application:load(os_mon),
            application:set_env(os_mon, start_cpu_sup, CPU),
            application:set_env(os_mon, start_os_sup, OS),
            application:set_env(os_mon, start_memsup, true),
            application:set_env(os_mon, start_disksup, Disk);
        {ok, true} -> ok
        %let it crash if start_memsup is set to false
    end,
    application:ensure_all_started(os_mon).

configure_memsup(MemsupSettings) ->
    case proplists:get_value(helper_timeout, MemsupSettings) of
        undefined -> ok;
        Timeout -> memsup:set_helper_timeout(Timeout)
    end,
    case proplists:get_value(procmem_high_watermark, MemsupSettings) of
        undefined -> ok;
        Watermark when is_integer(Watermark), Watermark > 0, Watermark < 100->
            memsup:set_procmem_high_watermark(Watermark/100)
        %% let it crash if watermark has invalid format
    end.

start_event_manager(DefaultHandlers) ->
    case erlang:whereis(?SERVER) of
        undefined ->
            gen_event:start({local, ?SERVER});
        _ -> ok
    end,
    [safe_add_handler(?SERVER, H, A) || {H, A} <- DefaultHandlers].

safe_add_handler(Manager, Handler, Agrs) ->
    AnyFun = fun(H) when H =:= Handler -> true;
                (_) -> false
             end,
    case lists:any(AnyFun, gen_event:which_handlers(Manager)) of
        false -> gen_event:add_handler(Manager, Handler, Agrs);
        _ -> ok
    end.

get_alarm_id({set_alarm, {AlarmId, Description}}) -> {set, AlarmId, Description};
get_alarm_id({clear_alarm, AlarmId})              -> {clear, AlarmId, no_description};
get_alarm_id(_)                                   -> invalid_alarm.

track_alarms({set, AlarmId, Descr}, #state{active_alarms = Alarms} = S) ->
    S#state{active_alarms = lists:keystore(AlarmId, 1, Alarms, {AlarmId, Descr})};
track_alarms({clear, AlarmId, _}, #state{active_alarms = Alarms} = S) ->
    S#state{active_alarms = lists:keydelete(AlarmId, 1, Alarms)}.

safe_handle_event(Event, #state{default_handlers = DefaultHandlers} = State) ->
    try
        case get_alarm_id(Event) of
            {_, process_memory_high_watermark, _} = Alarm ->
                start_event_manager(DefaultHandlers),
                gen_event:notify(?SERVER, Event),
                track_alarms(Alarm, State);
            _ -> State
        end
    catch
        _:_ -> State
    end.
