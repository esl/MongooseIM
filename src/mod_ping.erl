%%%----------------------------------------------------------------------
%%% File    : mod_ping.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : XEP-0199 XMPP Ping implementation
%%% Created : 14 Nov 2019 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_ping).
-author('piotr.nosek@erlang-solutions.com').

-behavior(gen_mod).
-behavior(ejabberd_c2s_info_handler).
-xep([{xep, 199}, {version, "2.0"}]).
-include("mongoose.hrl").
-include("jlib.hrl").

-define(DEFAULT_SEND_PINGS, false). % bool()
-define(DEFAULT_PING_INTERVAL, 60). % seconds
-define(DEFAULT_PING_REQ_TIMEOUT, 32).

%% C2S custom info handler
-export([handle_c2s_info/3]).

%% gen_mod callbacks
-export([start/2, stop/1]).

%% Hook callbacks
-export([iq_ping/4,
         user_online/4,
         user_offline/5,
         user_send/4,
         user_ping_response/4,
         user_keep_alive/2]).

%%====================================================================
%% Info Handler
%%====================================================================

handle_c2s_info(init, HandlerState, #{server := Server}) ->
    start_ping_timer(HandlerState, Server);
handle_c2s_info(send_ping, HandlerState, #{jid := JID, server := Server}) ->
    route_ping_iq(JID, Server),
    start_ping_timer(HandlerState, Server);
handle_c2s_info(timeout, HandlerState, #{jid := JID, server := Server}) ->
    mongoose_hooks:user_ping_timeout(Server, ok, JID),
    case gen_mod:get_module_opt(Server, ?MODULE, timeout_action, none) of
        kill -> ejabberd_c2s:stop(self());
        _ -> ok
    end,
    HandlerState.

-spec start_ping_timer(ejabberd_c2s_info_handler:handler_state(), jid:server()) -> reference().
start_ping_timer(HandlerState, Server) ->
    cancel_timer(HandlerState),
    PingInterval = gen_mod:get_module_opt(Server, ?MODULE, ping_interval, ?DEFAULT_PING_INTERVAL),
    ejabberd_c2s_info_handler:call_after(timer:seconds(PingInterval), self(), mod_ping, send_ping).

route_ping_iq(JID, Server) ->
    PingReqTimeout = timer:seconds(gen_mod:get_module_opt(Server, ?MODULE, ping_req_timeout,
                                            ?DEFAULT_PING_REQ_TIMEOUT)),
    IQ = #iq{type = get,
             sub_el = [#xmlel{name = <<"ping">>,
                              attrs = [{<<"xmlns">>, ?NS_PING}]}]},
    Pid = self(),
    T0 = erlang:monotonic_time(millisecond),
    F = fun(_From, _To, Acc, timeout) ->
               ejabberd_c2s_info_handler:call(Pid, mod_ping, timeout),
               NewAcc = mongoose_hooks:user_ping_response(Server,
                                                          Acc, JID, timeout, 0),
               NewAcc;
           (_From, _To, Acc, Response) ->
               % received a pong from client
               TDelta = erlang:monotonic_time(millisecond) - T0,
               NewAcc = mongoose_hooks:user_ping_response(Server,
                                                          Acc, JID, Response, TDelta),
               NewAcc
        end,
    From = jid:make_noprep(<<"">>, Server, <<"">>),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => Server,
                              from_jid => From,
                              to_jid => JID,
                              element => jlib:iq_to_xml(IQ) }),
    ejabberd_local:route_iq(From, JID, Acc, IQ, F, PingReqTimeout).

cancel_timer(empty_state) ->
    do_nothing;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).

%%====================================================================
%% utility
%%====================================================================

hooks(Host) ->
    [{sm_register_connection_hook, Host, ?MODULE, user_online, 100},
     {sm_remove_connection_hook, Host, ?MODULE, user_offline, 100},
     {user_send_packet, Host, ?MODULE, user_send, 100},
     {user_sent_keep_alive, Host, ?MODULE, user_keep_alive, 100},
     {user_ping_response, Host, ?MODULE, user_ping_response, 100}].


ensure_metrics(Host) ->
    mongoose_metrics:ensure_metric(Host, [mod_ping, ping_response], spiral),
    mongoose_metrics:ensure_metric(Host, [mod_ping, ping_response_timeout], spiral),
    mongoose_metrics:ensure_metric(Host, [mod_ping, ping_response_time], histogram).

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
%% IQ handlers
%%====================================================================
iq_ping(_From, _To, Acc, #iq{type = get, sub_el = #xmlel{name = <<"ping">>}} = IQ) ->
    {Acc, IQ#iq{type = result, sub_el = []}};
iq_ping(_From, _To, Acc, #iq{sub_el = SubEl} = IQ) ->
    {Acc, IQ#iq{type = error, sub_el = [SubEl, mongoose_xmpp_errors:feature_not_implemented()]}}.

%%====================================================================
%% Hook callbacks
%%====================================================================

user_online(Acc, {_, Pid} = _SID, #jid{lserver = Server}, _Info) ->
    Timer = start_ping_timer(empty_state, Server),
    ejabberd_c2s_info_handler:add(Pid, mod_ping, Timer),
    Acc.

user_offline(Acc, {_, Pid} = _SID, _JID, _Info, _Reason) ->
    ejabberd_c2s_info_handler:remove(Pid, mod_ping),
    Acc.

user_send(Acc, _JID, _From, _Packet) ->
    ejabberd_c2s_info_handler:call(self(), mod_ping, init),
    Acc.

user_keep_alive(Acc, _JID) ->
    ejabberd_c2s_info_handler:call(self(), mod_ping, init),
    Acc.

-spec user_ping_response(Acc :: mongoose_acc:t(),
                         JID :: jid:jid(),
                         Response :: timeout | jlib:iq(),
                         TDelta :: pos_integer()) -> mongoose_acc:t().
user_ping_response(Acc, #jid{server = Server}, timeout, _TDelta) ->
    mongoose_metrics:update(Server, [mod_ping, ping_response_timeout], 1),
    Acc;
user_ping_response(Acc, #jid{server = Server}, _Response, TDelta) ->
    mongoose_metrics:update(Server, [mod_ping, ping_response_time], TDelta),
    mongoose_metrics:update(Server, [mod_ping, ping_response], 1),
    Acc.
