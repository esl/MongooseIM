%%%-------------------------------------------------------------------
%%% @author bartek
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Oct 2016 11:19
%%%-------------------------------------------------------------------
-module(mod_spamctl).
-author("bartek").
-behaviour(gen_mod).

%% API
-export([start/2, stop/1, initialise/1, control/3]).
-export([cutoff/5, notify_offender/5, notify_core/5]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(DEFAULT_POOL_NAME, http_pool).
-define(DEFAULT_PATH, "").

start(Host, _Opts) ->
    ejabberd_hooks:add(spamctl_initialise, Host,
                       ?MODULE, initialise, 50),
    ejabberd_hooks:add(spamctl_control, Host,
                       ?MODULE, control, 50),
    ejabberd_hooks:add(spamctl_react, Host,
                       ?MODULE, notify_offender, 30),
    ejabberd_hooks:add(spamctl_react, Host,
                       ?MODULE, notify_core, 40),
    ejabberd_hooks:add(spamctl_react, Host,
                       ?MODULE, cutoff, 50),
    ok.

stop(Host) ->
    ejabberd_hooks:delete(spamctl_initialise, Host,
                          ?MODULE, initialise, 50),
    ejabberd_hooks:delete(spamctl_control, Host,
                          ?MODULE, control, 50),
    ejabberd_hooks:delete(spamctl_react, Host,
                          ?MODULE, notify_offender, 30),
    ejabberd_hooks:delete(spamctl_react, Host,
                          ?MODULE, notify_core, 40),
    ejabberd_hooks:delete(spamctl_react, Host,
                          ?MODULE, cutoff, 50),
    ok.

initialise(#{host := Host} = State) ->
    MaxRate = gen_mod:get_module_opt(Host, ?MODULE, maxrate, 10),
    Span = gen_mod:get_module_opt(Host, ?MODULE, span, 2),
    ModOpt = #{maxrate => MaxRate, span => Span},
    % merge config options into accumulator
    NState = maps:merge(State, ModOpt),
    % add working values used by this module
    ModState = #{rate => 0,
                 decision => ok,
                 lasttime => usec:from_now(os:timestamp())},
    maps:merge(NState, ModState).

control(State, Name, M) ->
    NState = check_msg(Name, M, State),
    case NState of
        #{decision := excess} ->
            {stop, NState};
        _ ->
            NState
    end.

check_msg(<<"message">>, M, State) ->
    Now = usec:from_now(os:timestamp()),
    Span = maps:get(span, State) * 1000000,
    Lasttime = maps:get(lasttime, State),
    Cycled = Now - Lasttime > Span,
    check_msg(M, State, Now, Cycled);
check_msg(_, _, State) ->
    State.

check_msg(_M, State, Now, true) ->
    set_decision(ok, Now, State);
check_msg(M, State, Now, false) ->
    NRate = maps:get(rate, State) + 1,
    check_msg_rate(M, Now, State#{rate => NRate}).

check_msg_rate(_M, Now, #{maxrate := Max, rate := Rate} = State) when Rate > Max ->
    set_decision(excess, Now, State);
check_msg_rate(_M, _Now, State) ->
    State.

set_decision(Dec, Now, State) ->
    State#{decision => Dec, lasttime => Now, rate => 0}.

cutoff(State, excess, _From, _To, _Msg) ->
    self() ! {stop, killed_by_spamctl},
    State;
cutoff(State, _, _, _, _) ->
    State.

notify_offender(State, excess, From, To, Msg) ->
    send_back_error(?ERR_NOT_ACCEPTABLE, From, To, Msg),
    State;
notify_offender(State, _, _, _, _) ->
    State.

send_back_error(Etype, From, To, Packet) ->
    Err = jlib:make_error_reply(Packet, Etype),
    ejabberd_router:route(To, From, Err).

notify_core(State, excess, From, _To, _Msg) ->
    C = jid:to_binary({From#jid.user, From#jid.server}),
    send_http_notification(maps:get(host, State), C, <<"exceeded message limit">>),
    State.

send_http_notification(Host, Culprit, Body) ->
    Path = fix_path(list_to_binary(gen_mod:get_module_opt(Host, ?MODULE, path, ?DEFAULT_PATH))),
    PoolName = gen_mod:get_module_opt(Host, ?MODULE, pool_name, ?DEFAULT_POOL_NAME),
    Pool = mongoose_http_client:get_pool(PoolName),
    Query = <<"culprit=", Culprit/binary, "&message=", Body/binary>>,
    ?INFO_MSG("Making request '~p' for user ~s@~s...", [Path, Culprit, Host]),
    Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    case mongoose_http_client:post(Pool, Path, Headers, Query) of
        {ok, _} ->
            ok;
        {error, E} ->
            ?ERROR_MSG("Failed to record spam policy violation (~p):~nOffender: ~p, message: ~p~n",
                [E, Culprit, Body])
    end.


fix_path(<<"/", R/binary>>) ->
    R;
fix_path(R) ->
    R.
