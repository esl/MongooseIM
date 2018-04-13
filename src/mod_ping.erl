%%%----------------------------------------------------------------------
%%% File    : mod_ping.erl
%%% Author  : Brian Cully <bjc@kublai.com>
%%% Purpose : Support XEP-0199 XMPP Ping and periodic keepalives
%%% Created : 11 Jul 2009 by Brian Cully <bjc@kublai.com>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_ping).
-author('bjc@kublai.com').

-behavior(gen_mod).
-behavior(gen_server).
-xep([{xep, 199}, {version, "2.0"}]).
-include("mongoose.hrl").
-include("jlib.hrl").

-define(SUPERVISOR, ejabberd_sup).
-define(DEFAULT_SEND_PINGS, false). % bool()
-define(DEFAULT_PING_INTERVAL, 60). % seconds
-define(DEFAULT_PING_REQ_TIMEOUT, 32).

%% API
-export([start_link/2, start_ping/2, stop_ping/2]).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3]).

%% Hook callbacks
-export([iq_ping/4,
         user_online/4,
         user_offline/5,
         user_send/4,
         user_keep_alive/2]).

-record(state, {host = <<"">>,
                send_pings = ?DEFAULT_SEND_PINGS,
                ping_interval = ?DEFAULT_PING_INTERVAL,
                timeout_action = none,
                ping_req_timeout = ?DEFAULT_PING_REQ_TIMEOUT,
                timers = dict:new()}).

-type state() :: #state{}.

%%====================================================================
%% API
%%====================================================================
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).

start_ping(Host, JID) when JID#jid.lresource =/= <<>> ->
    %% Guard check above ensures we are not adding a user without a
    %% resource by accident. If this happens the server ends up in a
    %% hot loop of iq-error replies.
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {start_ping, JID});
start_ping(_Host, _JID) -> ok.

stop_ping(Host, JID) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    gen_server:cast(Proc, {stop_ping, JID}).

%%====================================================================
%% gen_mod callbacks
%%====================================================================
start(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    PingSpec = {Proc, {?MODULE, start_link, [Host, Opts]},
                transient, 2000, worker, [?MODULE]},
    supervisor:start_child(?SUPERVISOR, PingSpec).

stop(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?MODULE),
    Pid = erlang:whereis(Proc),
    gen_server:call(Proc, stop),
    wait_for_process_to_stop(Pid),
    supervisor:delete_child(?SUPERVISOR, Proc).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(Args :: list()) -> {ok, state()}.
init([Host, Opts]) ->
    SendPings = gen_mod:get_opt(send_pings, Opts, ?DEFAULT_SEND_PINGS),
    PingInterval = gen_mod:get_opt(ping_interval, Opts, ?DEFAULT_PING_INTERVAL),
    PingReqTimeout = gen_mod:get_opt(ping_req_timeout, Opts, ?DEFAULT_PING_REQ_TIMEOUT),
    TimeoutAction = gen_mod:get_opt(timeout_action, Opts, none),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, no_queue),
    mod_disco:register_feature(Host, ?NS_PING),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PING,
                                  ?MODULE, iq_ping, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_PING,
                                  ?MODULE, iq_ping, IQDisc),

    maybe_add_hooks_handlers(Host, SendPings),

    {ok, #state{host = Host,
                send_pings = SendPings,
                ping_interval = timer:seconds(PingInterval),
                timeout_action = TimeoutAction,
                ping_req_timeout = timer:seconds(PingReqTimeout),
                timers = dict:new()}}.

maybe_add_hooks_handlers(Host, true) ->
    ejabberd_hooks:add(sm_register_connection_hook, Host,
                       ?MODULE, user_online, 100),
    ejabberd_hooks:add(sm_remove_connection_hook, Host,
                       ?MODULE, user_offline, 100),
    ejabberd_hooks:add(user_send_packet, Host,
                       ?MODULE, user_send, 100),
    ejabberd_hooks:add(user_sent_keep_alive, Host,
                       ?MODULE, user_keep_alive, 100);
maybe_add_hooks_handlers(_, _) ->
    ok.

terminate(_Reason, #state{host = Host}) ->
    ejabberd_hooks:delete(sm_remove_connection_hook, Host,
                          ?MODULE, user_offline, 100),
    ejabberd_hooks:delete(sm_register_connection_hook, Host,
                          ?MODULE, user_online, 100),
    ejabberd_hooks:delete(user_send_packet, Host,
                          ?MODULE, user_send, 100),
    ejabberd_hooks:delete(user_sent_keep_alive, Host,
                          ?MODULE, user_keep_alive, 100),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_PING),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PING),
    mod_disco:unregister_feature(Host, ?NS_PING).

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badarg}, State}.

handle_cast({start_ping, JID}, State) ->
    Timers = add_timer(JID, State#state.ping_interval, State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_cast({stop_ping, JID}, State) ->
    Timers = del_timer(JID, State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_cast({iq_pong, JID, timeout}, State) ->
    Timers = del_timer(JID, State#state.timers),
    ejabberd_hooks:run(user_ping_timeout, State#state.host, [JID]),
    case State#state.timeout_action of
        kill ->
            #jid{user = User, server = Server, resource = Resource} = JID,
            case ejabberd_sm:get_session_pid(User, Server, Resource) of
                Pid when is_pid(Pid) ->
                    ejabberd_c2s:stop(Pid);
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    {noreply, State#state{timers = Timers}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _TRef, {ping, JID}},
            #state{ping_req_timeout = PingReqTimeout} = State) ->
    IQ = #iq{type = get,
             sub_el = [#xmlel{name = <<"ping">>,
                              attrs = [{<<"xmlns">>, ?NS_PING}]}]},
    Pid = self(),
    F = fun(Response) ->
                gen_server:cast(Pid, {iq_pong, JID, Response})
        end,
    From = jid:make(<<"">>, State#state.host, <<"">>),
    ejabberd_local:route_iq(From, JID, undefined, IQ, F, PingReqTimeout),
    Timers = add_timer(JID, State#state.ping_interval, State#state.timers),
    {noreply, State#state{timers = Timers}};
handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Hook callbacks
%%====================================================================
iq_ping(_From, _To, Acc, #iq{type = Type, sub_el = SubEl} = IQ) ->
    case {Type, SubEl} of
        {get, #xmlel{name = <<"ping">>}} ->
            {Acc, IQ#iq{type = result, sub_el = []}};
        _ ->
            {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:feature_not_implemented()]}}
    end.

user_online(Acc, _SID, JID, _Info) ->
    start_ping(JID#jid.lserver, JID),
    Acc.

user_offline(Acc, _SID, JID, _Info, _Reason) ->
    stop_ping(JID#jid.lserver, JID),
    Acc.

user_send(Acc, JID, _From, _Packet) ->
    start_ping(JID#jid.lserver, JID),
    Acc.

user_keep_alive(Acc, JID) ->
    start_ping(JID#jid.lserver, JID),
    Acc.

%%====================================================================
%% Internal functions
%%====================================================================
add_timer(JID, Interval, Timers) ->
    LJID = jid:to_lower(JID),
    NewTimers = case dict:find(LJID, Timers) of
                    {ok, OldTRef} ->
                        cancel_timer(OldTRef),
                        dict:erase(LJID, Timers);
                    _ ->
                        Timers
                end,
    TRef = erlang:start_timer(Interval, self(), {ping, JID}),
    dict:store(LJID, TRef, NewTimers).

del_timer(JID, Timers) ->
    LJID = jid:to_lower(JID),
    case dict:find(LJID, Timers) of
        {ok, TRef} ->
            cancel_timer(TRef),
            dict:erase(LJID, Timers);
        _ ->
            Timers
    end.

cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
        false ->
            receive
                {timeout, TRef, _} ->
                    ok
            after 0 ->
                      ok
            end;
        _ ->
            ok
    end.

wait_for_process_to_stop(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _} ->
            ok
    after
        1000 ->
            {error, still_running}
    end.
