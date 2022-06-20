%%%----------------------------------------------------------------------
%%% File    : mod_ping.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : XEP-0199 XMPP Ping implementation
%%% Created : 14 Nov 2019 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_ping).
-author('piotr.nosek@erlang-solutions.com').

-behavior(gen_mod).
-xep([{xep, 199}, {version, "2.0"}]).
-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").

-define(DEFAULT_SEND_PINGS, false). % bool()
-define(DEFAULT_PING_INTERVAL, (60*1000)). % 60 seconds
-define(DEFAULT_PING_REQ_TIMEOUT, (32*1000)).% 32 seconds

%% gen_mod callbacks
-export([start/2,
         stop/1,
         config_spec/0,
         supported_features/0]).

%% Hook callbacks
-export([iq_ping/5,
         user_online/5,
         user_offline/5,
         user_send/4,
         user_ping_response/5,
         user_keep_alive/2]).

%% Remote hook callback
-export([handle_remote_hook/4]).

-ignore_xref([handle_remote_hook/4, user_keep_alive/2, user_offline/5, user_online/5,
              user_ping_response/5, user_ping_response/5, user_send/4]).

%%====================================================================
%% Info Handler
%%====================================================================

route_ping_iq(JID, Server, HostType) ->
    PingReqTimeout = gen_mod:get_module_opt(HostType, ?MODULE, ping_req_timeout),
    IQ = #iq{type = get,
             sub_el = [#xmlel{name = <<"ping">>,
                              attrs = [{<<"xmlns">>, ?NS_PING}]}]},
    Pid = self(),
    T0 = erlang:monotonic_time(millisecond),
    F = fun(_From, _To, Acc, timeout) ->
               ejabberd_c2s:run_remote_hook(Pid, mod_ping, timeout),
               NewAcc = mongoose_hooks:user_ping_response(HostType,
                                                          Acc, JID, timeout, 0),
               NewAcc;
           (_From, _To, Acc, Response) ->
               % received a pong from client
               TDelta = erlang:monotonic_time(millisecond) - T0,
               NewAcc = mongoose_hooks:user_ping_response(HostType,
                                                          Acc, JID, Response, TDelta),
               NewAcc
        end,
    From = jid:make_noprep(<<"">>, Server, <<"">>),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => Server,
                              host_type => HostType,
                              from_jid => From,
                              to_jid => JID,
                              element => jlib:iq_to_xml(IQ) }),
    ejabberd_local:route_iq(From, JID, Acc, IQ, F, PingReqTimeout).

%%====================================================================
%% utility
%%====================================================================

hooks(HostType) ->
    [{sm_register_connection_hook, HostType, ?MODULE, user_online, 100},
     {sm_remove_connection_hook, HostType, ?MODULE, user_offline, 100},
     {user_send_packet, HostType, ?MODULE, user_send, 100},
     {user_sent_keep_alive, HostType, ?MODULE, user_keep_alive, 100},
     {user_ping_response, HostType, ?MODULE, user_ping_response, 100},
     {c2s_remote_hook, HostType, ?MODULE, handle_remote_hook, 100}].

ensure_metrics(HostType) ->
    mongoose_metrics:ensure_metric(HostType, [mod_ping, ping_response], spiral),
    mongoose_metrics:ensure_metric(HostType, [mod_ping, ping_response_timeout], spiral),
    mongoose_metrics:ensure_metric(HostType, [mod_ping, ping_response_time], histogram).

%%====================================================================
%% gen_mod callbacks
%%====================================================================

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, #{send_pings := SendPings, iqdisc := IQDisc}) ->
    ensure_metrics(HostType),
    gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_PING, ejabberd_sm,
                                             fun ?MODULE:iq_ping/5, #{}, IQDisc),
    gen_iq_handler:add_iq_handler_for_domain(HostType, ?NS_PING, ejabberd_local,
                                             fun ?MODULE:iq_ping/5, #{}, IQDisc),
    maybe_add_hooks_handlers(HostType, SendPings).

-spec maybe_add_hooks_handlers(mongooseim:host_type(), boolean()) -> ok.
maybe_add_hooks_handlers(Host, true) ->
    ejabberd_hooks:add(hooks(Host));
maybe_add_hooks_handlers(_, _) ->
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
%%    a word of warning: timers are installed in c2s processes, so stopping mod_ping
%%    won't stop currently running timers. They'll run one more time, and then stop.
    ejabberd_hooks:delete(hooks(HostType)),
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_PING, ejabberd_local),
    gen_iq_handler:remove_iq_handler_for_domain(HostType, ?NS_PING, ejabberd_sm),
    ok.

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"send_pings">> => #option{type = boolean},
                 <<"ping_interval">> => #option{type = integer,
                                                validate = positive,
                                                process = fun timer:seconds/1},
                 <<"timeout_action">> => #option{type = atom,
                                                 validate = {enum, [none, kill]}},
                 <<"ping_req_timeout">> => #option{type = integer,
                                                   validate = positive,
                                                   process = fun timer:seconds/1},
                 <<"iqdisc">> => mongoose_config_spec:iqdisc()
                },
       defaults = #{<<"send_pings">> => ?DEFAULT_SEND_PINGS,
                    <<"ping_interval">> => ?DEFAULT_PING_INTERVAL,
                    <<"timeout_action">> => none,
                    <<"ping_req_timeout">> => ?DEFAULT_PING_REQ_TIMEOUT,
                    <<"iqdisc">> => no_queue
                   }
      }.

supported_features() -> [dynamic_domains].

%%====================================================================
%% IQ handlers
%%====================================================================
iq_ping(Acc, _From, _To, #iq{type = get, sub_el = #xmlel{name = <<"ping">>}} = IQ, _) ->
    {Acc, IQ#iq{type = result, sub_el = []}};
iq_ping(Acc, _From, _To, #iq{sub_el = SubEl} = IQ, _) ->
    NewSubEl = [SubEl, mongoose_xmpp_errors:feature_not_implemented()],
    {Acc, IQ#iq{type = error, sub_el = NewSubEl}}.

%%====================================================================
%% Hook callbacks
%%====================================================================

handle_remote_hook(HandlerState, mod_ping, Args, C2SState) ->
    handle_remote_call(Args,
                       ejabberd_c2s_state:jid(C2SState),
                       ejabberd_c2s_state:server(C2SState),
                       ejabberd_c2s_state:host_type(C2SState),
                       HandlerState);
handle_remote_hook(HandlerState, _, _, _) ->
    HandlerState.

user_online(Acc, _HostType, {_, Pid} = _SID, _Jid, _Info) ->
    ejabberd_c2s:run_remote_hook(Pid, mod_ping, init),
    Acc.

user_offline(Acc, {_, Pid} = _SID, _JID, _Info, _Reason) ->
    ejabberd_c2s:run_remote_hook(Pid, mod_ping, remove_timer),
    Acc.

user_send(Acc, _JID, _From, _Packet) ->
    ejabberd_c2s:run_remote_hook(self(), mod_ping, init),
    Acc.

user_keep_alive(Acc, _JID) ->
    ejabberd_c2s:run_remote_hook(self(), mod_ping, init),
    Acc.

-spec user_ping_response(Acc :: mongoose_acc:t(),
                         HostType :: mongooseim:host_type(),
                         JID :: jid:jid(),
                         Response :: timeout | jlib:iq(),
                         TDelta :: pos_integer()) -> mongoose_acc:t().
user_ping_response(Acc, HostType, _JID, timeout, _TDelta) ->
    mongoose_metrics:update(HostType, [mod_ping, ping_response_timeout], 1),
    Acc;
user_ping_response(Acc, HostType, _JID, _Response, TDelta) ->
    mongoose_metrics:update(HostType, [mod_ping, ping_response_time], TDelta),
    mongoose_metrics:update(HostType, [mod_ping, ping_response], 1),
    Acc.

%%====================================================================
%% Implementation
%%====================================================================

handle_remote_call(init, _JID, _Server, HostType, HandlerState) ->
    start_ping_timer(HandlerState, HostType);
handle_remote_call(send_ping, JID, Server, HostType, HandlerState) ->
    route_ping_iq(JID, Server, HostType),
    start_ping_timer(HandlerState, HostType);
handle_remote_call(timeout, JID, _Server, HostType, HandlerState) ->
    mongoose_hooks:user_ping_timeout(HostType, JID),
    case gen_mod:get_module_opt(HostType, ?MODULE, timeout_action) of
        kill -> ejabberd_c2s:stop(self());
        _ -> ok
    end,
    HandlerState;
handle_remote_call(remove_timer, _JID, _Server, _HostType, HandlerState) ->
    cancel_timer(HandlerState),
    empty_state.

-spec start_ping_timer(term(), mongooseim:host_type()) -> reference().
start_ping_timer(HandlerState, HostType) ->
    cancel_timer(HandlerState),
    PingInterval = gen_mod:get_module_opt(HostType, ?MODULE, ping_interval),
    ejabberd_c2s:run_remote_hook_after(PingInterval, self(), mod_ping, send_ping).

cancel_timer(empty_state) ->
    do_nothing;
cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).

