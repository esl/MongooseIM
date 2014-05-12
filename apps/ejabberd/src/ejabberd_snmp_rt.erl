%%%------------------------------------------------------------------------
%%% File:   ejabberd_snmp_rt.erl
%%% Author: Aleksandra Lipiec <aleksandra.lipiec@erlang-solutions.com>
%%%         Radoslaw Szymczyszyn <radoslaw.szymczyszyn@erlang-solutions.com>
%%% Description: Module for snmp counters that require periodical calculations
%%%
%%% Created: 5 Aug 2011 by <aleksandra.lipiec@erlang-solutions.com>
%%%-----------------------------------------------------------------------
-module(ejabberd_snmp_rt).

-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

%%--------------------
%% API
%%--------------------

%% will potentially include function for updating temporary window counters
-export([start_link/3,
         stop/0]).

-define(DEFAULT_INTERVAL, 60).
-define(MIN_INTERVAL, 10).
-define(COUNTERS, [globalSessionCount,       %% counters computed by this module
                   globalUniqueSessionCount,
                   modPrivacyListLength,
                   modRosterSize,
                   modRosterGroups,
                   modRegisterUserCount
                  ]).

-define(BACKEND, ejabberd_snmp_backend).

%% Internal exports (gen_server)
-export([init/1,
         start_link2/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {timer_ref_rt,      % ref to timer (ticks for rt counters)
                timer_ref_w,       % ref to timer (ticks for window counters)
                computing_num}).   % number of counters in computation
-type state() :: #state{}.

%%--------------------
%% Implementation
%%--------------------

-spec start_link(true | false, integer(), integer()) ->
  {'error',_} | {'ok','undefined' | pid()} | {'ok','undefined' | pid(),_}.
start_link(RtEnabled, RtInterval, WInterval) ->
    NewRtInterval = case {RtEnabled, RtInterval} of
                      {true, I} when I < ?MIN_INTERVAL ->
                          ?MIN_INTERVAL;
                      {true, I} when is_integer(I) ->
                          I;
                      {true, _} ->
                          ?DEFAULT_INTERVAL;
                      {false, _} ->
                            undefined
                  end,
    NewWInterval = case WInterval of
                      W when W < ?MIN_INTERVAL ->
                          ?MIN_INTERVAL;
                      W when is_integer(W) ->
                          W;
                      _ ->
                          ?DEFAULT_INTERVAL
                  end,
    Spec = {?MODULE, {?MODULE, start_link2, [NewRtInterval, NewWInterval]},
            transient, 2000, worker, [?MODULE]},
    case supervisor:start_child(ejabberd_sup, Spec) of
        {error, Error} ->
            {error, Error};
        Ok ->
            Ok
    end.


-spec start_link2(integer(), integer()) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link2(RtInterval, WInterval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [RtInterval, WInterval], []).


-spec stop() -> term().
stop() ->
    supervisor:terminate_child(ejabberd_sup, ?MODULE),
    supervisor:delete_child(ejabberd_sup, ?MODULE).

%%----------------------
%% Callbacks
%%----------------------
-spec init(['undefined' | non_neg_integer(),...]) -> {'ok',state()}.
init([RtInterval, WInterval]) ->
    TickRefRt = case RtInterval of
                    undefined ->
                        none;
                    I ->
                        {ok, TickRef} = start_timer(I, tick_rt),
                        TickRef
                end,
    {ok, TickRefW} = start_timer(WInterval, tick_w),
    {ok, #state{timer_ref_rt = TickRefRt,
                timer_ref_w = TickRefW,
                computing_num = 0}}.

%%-------------------

handle_call({change_interval_rt, _NewInterval}, _From,
            #state{timer_ref_rt = none} = State) ->
    {reply, {error, disabled}, State};
handle_call({change_interval_rt, NewInterval}, _From,
            #state{timer_ref_rt = TickRef} = State) ->
    timer:cancel(TickRef),
    {ok, NewTickRef} = start_timer(NewInterval, tick_rt),
    {reply, ok, State#state{timer_ref_rt = NewTickRef}};
handle_call({change_interval_w, NewInterval}, _From,
            #state{timer_ref_w = TickRef} = State) ->
    timer:cancel(TickRef),
    {ok, NewTickRef} = start_timer(NewInterval, tick_w),
    {reply, ok, State#state{timer_ref_w = NewTickRef}};
handle_call(Request, _From, State) ->
    ?WARNING_MSG("~p received unknown call ~p ", [?MODULE, Request]),
    {noreply, State}.

%%-------------------

handle_cast({compute, globalSessionCount},
            #state{computing_num = Num} = State) ->
    Count = ejabberd_sm:get_total_sessions_number(),
    ejabberd_snmp_core:set_counter(globalSessionCount, Count),
    {noreply, State#state{computing_num = Num - 1}};
handle_cast({compute, globalUniqueSessionCount},
            #state{computing_num = Num} = State) ->
    Count = ejabberd_sm:get_unique_sessions_number(),
    ejabberd_snmp_core:set_counter(globalUniqueSessionCount, Count),
    {noreply, State#state{computing_num = Num - 1}};
handle_cast({compute, modPrivacyListLength},
            #state{computing_num = Num} = State) ->
    case ?BACKEND:privacy_list_length() of
        {error, Error} ->
            ?ERROR_MSG("Error while computing privacy list length in ~p:~n~p",
                       [?BACKEND, Error]);
        Value -> ejabberd_snmp_core:set_counter(modPrivacyListLength, Value)
    end,
    {noreply, State#state{computing_num = Num - 1}};
handle_cast({compute, modRosterSize},
            #state{computing_num = Num} = State) ->
    case ?BACKEND:roster_size() of
        {error, Error} ->
            ?ERROR_MSG("Error while computing roster size in ~p:~n~p",
                       [?BACKEND, Error]);
        Value -> ejabberd_snmp_core:set_counter(modRosterSize, Value)
    end,
    {noreply, State#state{computing_num = Num - 1}};
handle_cast({compute, modRosterGroups},
            #state{computing_num = Num} = State) ->
    case ?BACKEND:roster_groups() of
        {error, Error} ->
            ?ERROR_MSG("Error while computing roster groups in ~p:~n~p",
                       [?BACKEND, Error]);
        Value -> ejabberd_snmp_core:set_counter(modRosterGroups, Value)
    end,
    {noreply, State#state{computing_num = Num - 1}};
handle_cast({compute, modRegisterUserCount},
            #state{computing_num = Num} = State) ->
    case ?BACKEND:registered_count() of
        {error, Error} ->
            ?ERROR_MSG("Error while computing registered count in ~p:~n~p",
                       [?BACKEND, Error]);
        Value -> ejabberd_snmp_core:set_counter(modRegisterUserCount, Value)
    end,
    {noreply, State#state{computing_num = Num - 1}};
%% Similar for all rt counters
handle_cast(Request, State) ->
    ?WARNING_MSG("~p received unknown cast ~p ", [?MODULE, Request]),
    {noreply, State}.

%%-------------------

handle_info(tick_rt, #state{computing_num = 0} = State) ->
    Num = lists:foldl(fun(Counter, Res) ->
                              gen_server:cast(?MODULE, {compute, Counter}),
                              Res + 1
                      end, 0, ?COUNTERS),
    {noreply, State#state{computing_num = Num}};
handle_info(tick_rt, State) ->
    {noreply, State};
handle_info(tick_w, State) ->
    ejabberd_snmp_core:window_change(),
    {noreply, State};
handle_info(Info, State) ->
    ?WARNING_MSG("~p received unknown info ~p (~p)", [?MODULE, Info, State]),
    {noreply, State}.

%%-------------------

terminate(_Reason, _State) ->
    ok.

%%-------------------

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------
%% Helpers
%%--------------------
-spec start_timer(Interval :: non_neg_integer(), Type :: 'tick_rt' | 'tick_w'
                 ) -> {'error',_} | {'ok',timer:tref()}.
start_timer(Interval, Type) ->
    timer:send_interval(Interval*1000, self(), Type).
