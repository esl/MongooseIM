%%%----------------------------------------------------------------------
%%% File    : mod_ping.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : XEP-0199 XMPP Ping implementation
%%% Created : 14 Nov 2019 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_ping).
-author('piotr.nosek@erlang-solutions.com').

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).
-xep([{xep, 199}, {version, "2.0"}]).
-include("mongoose.hrl").
-include("jlib.hrl").

-define(DEFAULT_SEND_PINGS, false). % bool()
-define(DEFAULT_PING_INTERVAL, 60). % seconds
-define(DEFAULT_PING_REQ_TIMEOUT, 32).

%% C2S custom info handler
-export([handle_info/3]).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% Hook callbacks
-export([iq_ping/4,
         user_online/4,
         user_offline/5,
         user_send/4,
         user_keep_alive/2]).

%%====================================================================
%% Info Handler
%%====================================================================

handle_info(init, HandlerState, C2SState) ->
    {add_ping_timer(HandlerState, C2SState), C2SState};
handle_info(send_ping, HandlerState, C2SState) ->
    JID = ejabberd_c2s_state:jid(C2SState),
    Server = ejabberd_c2s_state:server(C2SState),
    route_ping_iq(JID, Server),
    {add_ping_timer(HandlerState, C2SState), C2SState};
handle_info(timeout, HandlerState, C2SState) ->
    Server = ejabberd_c2s_state:server(C2SState),
    case gen_mod:get_module_opt(Server, ?MODULE, timeout_action, none) of
        kill -> ejabberd_c2s:stop(self());
        _ -> ok
    end,
    {HandlerState, C2SState}.

add_ping_timer(HandlerState, C2SState) ->
    cancel_timer(HandlerState),
    Server = ejabberd_c2s_state:server(C2SState),
    PingInterval = gen_mod:get_module_opt(Server, ?MODULE, ping_interval, ?DEFAULT_PING_INTERVAL),
    erlang:send_after(PingInterval * 1000, self(), {mod_ping, send_ping}).

route_ping_iq(JID, Server) ->
    PingReqTimeout = gen_mod:get_module_opt(Server, ?MODULE, ping_req_timeout,
                                            ?DEFAULT_PING_REQ_TIMEOUT),
    IQ = #iq{type = get,
             sub_el = [#xmlel{name = <<"ping">>,
                              attrs = [{<<"xmlns">>, ?NS_PING}]}]},
    Pid = self(),
    T0 = erlang:monotonic_time(millisecond),
    F = fun(_, _, Acc, timeout) ->
               mongoose_metrics:update(Server, [mod_ping, ping_response_timeout], 1),
               mongoose_hooks:user_ping_timeout(Server, Acc, JID),
               Pid ! {mod_ping, timeout};
           (_From, _To, Acc, Response) ->
                TDelta = erlang:monotonic_time(millisecond) - T0,
                mongoose_metrics:update(Server, [mod_ping, ping_response_time], TDelta),
                mongoose_metrics:update(Server, [mod_ping, ping_response], 1),
                mongoose_hooks:user_ping_response(Server, Acc, JID, Response, TDelta),
                Pid ! {mod_ping, init}
                % NewAcc
        end,
    From = jid:make_noprep(<<"">>, Server, <<"">>),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => Server,
                              from_jid => From,
                              to_jid => JID,
                              element => jlib:iq_to_xml(IQ) }),
    ejabberd_local:route_iq(From, JID, Acc, IQ, F, PingReqTimeout).

cancel_timer(undefined) ->
    do_nothing;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

start(Host, Opts) ->
    ensure_metrics(Host),
    SendPings = gen_mod:get_opt(send_pings, Opts, ?DEFAULT_SEND_PINGS),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, no_queue),
    mod_disco:register_feature(Host, ?NS_PING),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_PING,
                                  ?MODULE, iq_ping, IQDisc),
    gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_PING,
                                  ?MODULE, iq_ping, IQDisc),
    maybe_add_hooks_handlers(Host, SendPings).
    
maybe_add_hooks_handlers(Host, true) ->
    ejabberd_hooks:add(hooks(Host));
maybe_add_hooks_handlers(_, _) ->
    ok.

stop(Host) ->
    ejabberd_hooks:delete(hooks(Host)),
    gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_PING),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_PING),
    mod_disco:unregister_feature(Host, ?NS_PING).

%%====================================================================
%% Hook callbacks
%%====================================================================

iq_ping(_From, _To, Acc, #iq{type = get, sub_el = #xmlel{ name = <<"ping">> }} = IQ) ->
    {Acc, IQ#iq{type = result, sub_el = []}};
iq_ping(_From, _To, Acc, #iq{sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:feature_not_implemented()]}}.

-spec user_online(Acc, _SID, JID, _Info) -> Acc when
                  Acc :: any(),
                  JID :: jid:jid().
user_online(Acc, {_, Pid} = _SID, _JID, _Info) ->
    ejabberd_c2s_info_handler:add(Pid, mod_ping, ?MODULE, handle_info, undefined),
    Acc.

user_offline(Acc, {_, Pid} = _SID, _JID, _Info, _Reason) ->
    ejabberd_c2s_info_handler:remove(Pid, mod_ping),
    Acc.

user_send(Acc, _JID, _From, _Packet) ->
    self() ! {mod_ping, init},
    Acc.

%%====================================================================
%% Internal functions
%%====================================================================
hooks(Host) ->
    [{sm_register_connection_hook, Host, ?MODULE, user_online, 100},
     {sm_remove_connection_hook, Host, ?MODULE, user_offline, 100},
     {user_send_packet, Host, ?MODULE, user_send, 100},
     {user_sent_keep_alive, Host, ?MODULE, user_keep_alive, 100}].

ensure_metrics(Host) ->
    mongoose_metrics:ensure_metric(Host, [mod_ping, ping_response], spiral),
    mongoose_metrics:ensure_metric(Host, [mod_ping, ping_response_timeout], spiral),
    mongoose_metrics:ensure_metric(Host, [mod_ping, ping_response_time], histogram).

user_keep_alive(Acc, _JID) ->
    self() ! {mod_ping, init},
    Acc.

