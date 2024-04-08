%%%----------------------------------------------------------------------
%%% File    : mod_ping.erl
%%% Author  : Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%% Purpose : XEP-0199 XMPP Ping implementation
%%% Created : 14 Nov 2019 by Piotr Nosek <piotr.nosek@erlang-solutions.com>
%%%----------------------------------------------------------------------

-module(mod_ping).
-author('piotr.nosek@erlang-solutions.com').

-behavior(gen_mod).
-xep([{xep, 199}, {version, "2.0.1"}]).

-include("jlib.hrl").
-include("mongoose_logger.hrl").
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
-export([user_send_packet/3,
         user_send_iq/3,
         user_ping_response/3,
         filter_local_packet/3,
         iq_ping/5]).

%% Record that will be stored in the c2s state when the server pings the client,
%% in order to indentify the possible client's answer.
-record(ping_handler, {id :: binary(), time :: integer()}).

%%====================================================================
%% Info Handler
%%====================================================================

hooks(HostType) ->
    [{user_ping_response, HostType, fun ?MODULE:user_ping_response/3, #{}, 100},
     {filter_local_packet, HostType, fun ?MODULE:filter_local_packet/3, #{}, 100}
     | c2s_hooks(HostType)].

-spec c2s_hooks(mongooseim:host_type()) -> gen_hook:hook_list(mongoose_c2s_hooks:fn()).
c2s_hooks(HostType) ->
    [
     {user_send_packet, HostType, fun ?MODULE:user_send_packet/3, #{}, 100},
     {user_send_iq, HostType, fun ?MODULE:user_send_iq/3, #{}, 100}
    ].

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
    gen_iq_handler:add_iq_handler_for_domain(
      HostType, ?NS_PING, ejabberd_sm, fun ?MODULE:iq_ping/5, #{}, IQDisc),
    gen_iq_handler:add_iq_handler_for_domain(
      HostType, ?NS_PING, ejabberd_local, fun ?MODULE:iq_ping/5, #{}, IQDisc),
    maybe_add_hooks_handlers(HostType, SendPings).

-spec maybe_add_hooks_handlers(mongooseim:host_type(), boolean()) -> ok.
maybe_add_hooks_handlers(Host, true) ->
    gen_hook:add_handlers(hooks(Host));
maybe_add_hooks_handlers(_, _) ->
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
%%    a word of warning: timers are installed in c2s processes, so stopping mod_ping
%%    won't stop currently running timers. They'll run one more time, and then stop.
    gen_hook:delete_handlers(hooks(HostType)),
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

-spec filter_local_packet(Acc, Params, Extra) -> {ok, Acc} | {stop, drop} when
      Acc :: mongoose_hooks:filter_packet_acc(),
      Params :: map(),
      Extra :: gen_hook:extra().
filter_local_packet({_, _, _, Stanza} = Acc, _Params, _Extra) ->
    case is_ping_error(Stanza) of
        true ->
            ?LOG_DEBUG(#{what => ping_error_received, acc => Acc}),
            {stop, drop};
        false ->
            {ok, Acc}
    end.

-spec user_send_iq(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_send_iq(Acc, #{c2s_data := StateData}, #{host_type := HostType}) ->
    StanzaType = mongoose_acc:stanza_type(Acc),
    ModState = mongoose_c2s:get_mod_state(StateData, ?MODULE),
    handle_stanza(StanzaType, ModState, Acc, StateData, HostType).

handle_stanza(Type, {ok, PingHandler}, Acc, StateData, HostType) when Type == <<"result">>;
                                                                      Type == <<"error">> ->
    handle_ping_response(Type, PingHandler, Acc, StateData, HostType);
handle_stanza(_, _, Acc, _, _) ->
    {ok, Acc}.

handle_ping_response(Type, #ping_handler{id = PingId, time = T0}, Acc, StateData, HostType) ->
    IqResponse = mongoose_acc:element(Acc),
    IqId = exml_query:attr(IqResponse, <<"id">>),
    case IqId of
        PingId ->
            Jid = mongoose_c2s:get_jid(StateData),
            TDelta = erlang:monotonic_time(millisecond) - T0,
            mongoose_hooks:user_ping_response(HostType, #{}, Jid, IqResponse, TDelta),
            Action = determine_action(Type),
            {stop, mongoose_c2s_acc:to_acc(Acc, actions, Action)};
        _ ->
            {ok, Acc}
    end.

determine_action(<<"result">>) ->
    {{timeout, ping_timeout}, cancel};
determine_action(<<"error">>) ->
    [{{timeout, ping_timeout}, cancel}, {{timeout, ping_error}, 0, fun ping_c2s_handler/2}].

-spec user_send_packet(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_send_packet(Acc, _Params, #{host_type := HostType}) ->
    Interval = gen_mod:get_module_opt(HostType, ?MODULE, ping_interval),
    Action = {{timeout, ping}, Interval, fun ping_c2s_handler/2},
    {ok, mongoose_c2s_acc:to_acc(Acc, actions, Action)}.

-spec ping_c2s_handler(atom(), mongoose_c2s:data()) -> mongoose_c2s_acc:t().
ping_c2s_handler(ping, StateData) ->
    HostType = mongoose_c2s:get_host_type(StateData),
    Interval = gen_mod:get_module_opt(HostType, ?MODULE, ping_req_timeout),
    Actions = [{{timeout, send_ping}, Interval, fun ping_c2s_handler/2}],
    mongoose_c2s_acc:new(#{actions => Actions});
ping_c2s_handler(send_ping, StateData) ->
    PingId = mongoose_bin:gen_from_crypto(),
    IQ = ping_get(PingId),
    HostType = mongoose_c2s:get_host_type(StateData),
    LServer = mongoose_c2s:get_lserver(StateData),
    Jid = mongoose_c2s:get_jid(StateData),
    FromServer = jid:make_noprep(<<>>, LServer, <<>>),
    Interval = gen_mod:get_module_opt(HostType, ?MODULE, ping_req_timeout),
    Actions = [{{timeout, ping_timeout}, Interval, fun ping_c2s_handler/2}],
    T0 = erlang:monotonic_time(millisecond),
    Params = #{host_type => HostType, lserver => LServer, location => ?LOCATION,
               from_jid => FromServer, to_jid => Jid, element => IQ},
    Acc = mongoose_acc:new(Params),
    mongoose_c2s_acc:new(#{state_mod => #{?MODULE => #ping_handler{id = PingId, time = T0}},
                           actions => Actions, route => [Acc]});
ping_c2s_handler(ping_timeout, StateData) ->
    Jid = mongoose_c2s:get_jid(StateData),
    HostType = mongoose_c2s:get_host_type(StateData),
    mongoose_hooks:user_ping_response(HostType, #{}, Jid, timeout, 0),
    handle_ping_action(HostType, ping_timeout);
ping_c2s_handler(ping_error, StateData) ->
    HostType = mongoose_c2s:get_host_type(StateData),
    handle_ping_action(HostType, ping_error).

handle_ping_action(HostType, Reason) ->
    TimeoutAction = gen_mod:get_module_opt(HostType, ?MODULE, timeout_action),
    case TimeoutAction of
        kill -> mongoose_c2s_acc:new(#{stop => {shutdown, Reason}});
        _ -> mongoose_c2s_acc:new()
    end.

-spec user_ping_response(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{response := timeout | jlib:iq(), time_delta := non_neg_integer()},
    Extra :: #{host_type := mongooseim:host_type()}.
user_ping_response(Acc, #{response := timeout}, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, [mod_ping, ping_response_timeout], 1),
    {ok, Acc};
user_ping_response(Acc, #{time_delta := TDelta}, #{host_type := HostType}) ->
    mongoose_metrics:update(HostType, [mod_ping, ping_response_time], TDelta),
    mongoose_metrics:update(HostType, [mod_ping, ping_response], 1),
    {ok, Acc}.

%%====================================================================
%% Stanzas
%%====================================================================

-spec ping_get(binary()) -> exml:element().
ping_get(Id) ->
    #xmlel{name = <<"iq">>,
           attrs = [{<<"type">>, <<"get">>}, {<<"id">>, Id}],
           children = [#xmlel{name = <<"ping">>, attrs = [{<<"xmlns">>, ?NS_PING}]}]}.

-spec is_ping_error(exml:element()) -> boolean().
is_ping_error(Stanza) ->
    case exml_query:attr(Stanza, <<"type">>) of
        <<"error">> ->
            undefined =/= exml_query:subelement_with_name_and_ns(Stanza, <<"ping">>, ?NS_PING)
            andalso
            undefined =/= exml_query:subelement(Stanza, <<"error">>);
        _ ->
            false
    end.
