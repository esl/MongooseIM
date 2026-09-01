-module(mod_csi).

-xep([{xep, 352}, {version, "1.0.0"}]).

-include("mongoose_config_spec.hrl").
-include("jlib.hrl").

-behaviour(gen_mod).
-behaviour(mongoose_module_metrics).

%% gen_mod callbacks
-export([start/2, stop/1, hooks/1, config_spec/0, supported_features/0, instrumentation/1]).

%% Hook handlers
-export([c2s_stream_features/3,
         bind2_stream_features/3,
         bind2_enable_features/3,
         get_user_status/3,
         xmpp_presend_element/3,
         user_send_xmlel/3,
         handle_user_stopping/3,
         reroute_unacked_messages/3
        ]).

-record(csi_buffer_state, {buffer = [] :: [mongoose_acc:t()],
                           buffer_max :: non_neg_integer() | infinity}).

-type params() :: #{c2s_data := mongoose_c2s:data(), _ := _}.
-type state() :: active | inactive.
-type csi_buffer_state() :: #csi_buffer_state{}.

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(_HostType, _Opts) ->
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    Opts = gen_mod:get_module_opts(HostType, ?MODULE),
    basic_hooks(HostType) ++ buffer_hooks(HostType, Opts).

-spec instrumentation(mongooseim:host_type()) -> [mongoose_instrument:spec()].
instrumentation(HostType) ->
    [{mod_csi_active, #{host_type => HostType},
      #{metrics => #{count => spiral}}},
     {mod_csi_inactive, #{host_type => HostType},
      #{metrics => #{count => spiral}}}].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
       items = #{<<"buffer">> => buffer_config_spec()}
    }.

-spec buffer_config_spec() -> mongoose_config_spec:config_section().
buffer_config_spec() ->
    #section{
       items = #{<<"max_size">> => #option{type = int_or_infinity,
                                           validate = non_negative}},
       defaults = #{<<"max_size">> => 20}
    }.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec basic_hooks(mongooseim:host_type()) -> gen_hook:hook_list().
basic_hooks(HostType) ->
    [{c2s_stream_features, HostType, fun ?MODULE:c2s_stream_features/3, #{}, 60},
     {bind2_stream_features, HostType, fun ?MODULE:bind2_stream_features/3, #{}, 50},
     {bind2_enable_features, HostType, fun ?MODULE:bind2_enable_features/3, #{}, 50},
     {get_user_status, HostType, fun ?MODULE:get_user_status/3, #{}, 50},
     {user_send_xmlel, HostType, fun ?MODULE:user_send_xmlel/3, #{}, 60}].

% Note: xmpp_presend_element handler has to run before stream management
-spec buffer_hooks(mongooseim:host_type(), gen_mod:module_opts()) -> gen_hook:hook_list().
buffer_hooks(HostType, #{buffer := _}) ->
    [{xmpp_presend_element, HostType, fun ?MODULE:xmpp_presend_element/3, #{}, 45},
     {user_stop_request, HostType, fun ?MODULE:handle_user_stopping/3, #{}, 95},
     {user_socket_closed, HostType, fun ?MODULE:handle_user_stopping/3, #{}, 95},
     {user_socket_error, HostType, fun ?MODULE:handle_user_stopping/3, #{}, 95},
     {reroute_unacked_messages, HostType, fun ?MODULE:reroute_unacked_messages/3, #{}, 70}];
buffer_hooks(_, _) ->
    [].

%%% Hook handlers
-spec c2s_stream_features(Acc, map(), gen_hook:extra()) -> {ok, Acc} when Acc :: [exml:element()].
c2s_stream_features(Acc, _, _) ->
    {ok, lists:keystore(<<"csi">>, #xmlel.name, Acc, csi())}.

-spec bind2_stream_features(Acc, #{c2s_data := mongoose_c2s:data()}, gen_hook:extra()) ->
    {ok, Acc} when Acc :: [exml:element()].
bind2_stream_features(Acc, _, _) ->
    SmFeature = #xmlel{name = <<"feature">>, attrs = #{<<"var">> => ?NS_CSI}},
    {ok, [SmFeature | Acc]}.

-spec bind2_enable_features(SaslAcc, mod_sasl2:c2s_state_data(), gen_hook:extra()) ->
    {ok, SaslAcc} when SaslAcc :: mongoose_acc:t().
bind2_enable_features(SaslAcc, Params, _) ->
    #{request := BindRequest} = mod_bind2:get_bind_request(SaslAcc),
    case exml_query:subelement_with_ns(BindRequest, ?NS_CSI) of
        undefined ->
            {ok, SaslAcc};
        #xmlel{name = <<"active">>} ->
            SaslAcc1 = handle_active_request(SaslAcc, Params),
            {ok, SaslAcc1};
        #xmlel{name = <<"inactive">>} ->
            SaslAcc1 = handle_inactive_request(SaslAcc, Params),
            {ok, SaslAcc1}
    end.

-spec get_user_status(map(), #{sessions := [ejabberd_sm:session()]}, gen_hook:extra()) ->
          {ok, #{client_state := state(), _ => _}}.
get_user_status(Acc, #{sessions := Sessions}, _) ->
    {ok, Acc#{client_state => aggregate_client_state(Sessions)}}.

%% The XEP doesn't require any specific server behaviour in response to CSI stanzas,
%% there are only some suggestions. The implementation in MongooseIM will simply buffer
%% all packets (up to a configured limit) when the session is "inactive" and will flush
%% the buffer when it becomes "active" again.
-spec xmpp_presend_element(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
xmpp_presend_element(Acc, #{c2s_data := C2SData}, _Extra) ->
    case mongoose_c2s:get_mod_state(C2SData, ?MODULE) of
        {ok, Csi = #csi_buffer_state{}} ->
            handle_presend_element(Acc, Csi, get_client_state(C2SData));
        {error, not_found} ->
            {ok, Acc}
    end.

-spec handle_presend_element(mongoose_acc:t(), csi_buffer_state(), state()) ->
    mongoose_c2s_hooks:result().
handle_presend_element(Acc, Csi = #csi_buffer_state{buffer = Buffer, buffer_max = BMax}, inactive) ->
    case length(Buffer) + 1 > BMax of
        true ->
            NewBuffer = [mark_acc_as_buffered(Acc) | Buffer],
            NewCsi = Csi#csi_buffer_state{buffer = []},
            ToAcc = [{state_mod, {?MODULE, NewCsi}}, {flush, lists:reverse(NewBuffer)}],
            {stop, mongoose_c2s_acc:to_acc_many(Acc, ToAcc)};
        _ ->
            buffer_acc(Acc, Csi, is_acc_buffered(Acc))
    end;
handle_presend_element(Acc, _, _) ->
    {ok, Acc}.

-spec buffer_acc(mongoose_acc:t(), csi_buffer_state(), boolean()) -> mongoose_c2s_hooks:result().
buffer_acc(Acc, Csi = #csi_buffer_state{buffer = Buffer}, false) ->
    AccToBuffer = mark_acc_as_buffered(Acc),
    NewCsi = Csi#csi_buffer_state{buffer = [AccToBuffer | Buffer]},
    {stop, mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewCsi})};
buffer_acc(Acc, _, true) ->
    {ok, Acc}.

-spec user_send_xmlel(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_send_xmlel(Acc, Params, _Extra) ->
    El = mongoose_acc:element(Acc),
    case exml_query:attr(El, <<"xmlns">>) of
        ?NS_CSI ->
            {stop, handle_csi_request(Acc, Params, El)};
        _ ->
            {ok, Acc}
    end.

%% here we ensure CSI is active and won't buffer anymore, and the current buffer is given to sm
-spec handle_user_stopping(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
     mongoose_c2s_hooks:result().
handle_user_stopping(Acc, #{c2s_data := C2SData}, _) ->
    case mongoose_c2s:get_mod_state(C2SData, ?MODULE) of
        {ok, Csi = #csi_buffer_state{buffer = Buffer}} ->
            NewCsi = Csi#csi_buffer_state{buffer = []},
            Acc1 = mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewCsi}),
            Acc2 = set_client_state(Acc1, C2SData, active),
            {ok, mongoose_c2s_acc:to_acc(Acc2, flush, lists:reverse(Buffer))};
        _ ->
            {ok, Acc}
    end.

-spec reroute_unacked_messages(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
reroute_unacked_messages(Acc, #{c2s_data := C2SData}, _) ->
    case mongoose_c2s:get_mod_state(C2SData, ?MODULE) of
        {ok, Csi = #csi_buffer_state{buffer = Buffer}} ->
            mongoose_c2s:reroute_buffer(C2SData, Buffer),
            NewCsi = Csi#csi_buffer_state{buffer = []},
            Acc1 = mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewCsi}),
            {ok, set_client_state(Acc1, C2SData, active)};
        _ ->
            {ok, Acc}
    end.

-spec handle_csi_request(mongoose_acc:t(), mongoose_c2s_hooks:params(), exml:element()) ->
    mongoose_acc:t().
handle_csi_request(Acc, Params, #xmlel{name = <<"inactive">>}) ->
    handle_inactive_request(Acc, Params);
handle_csi_request(Acc, Params, #xmlel{name = <<"active">>}) ->
    handle_active_request(Acc, Params);
handle_csi_request(Acc, _, _) ->
    {From, To, El} = mongoose_acc:packet(Acc),
    Error = jlib:make_error_reply(El, mongoose_xmpp_errors:bad_request()),
    ErrorAcc = mongoose_acc:update_stanza(#{from_jid => From, to_jid => To, element => Error }, Acc),
    mongoose_c2s_acc:to_acc(Acc, route, ErrorAcc).

-spec handle_inactive_request(mongoose_acc:t(), params()) -> mongoose_acc:t().
handle_inactive_request(Acc, #{c2s_data := C2SData} = _Params) ->
    HostType = mongoose_c2s:get_host_type(C2SData),
    JID = mongoose_c2s:get_jid(C2SData),
    mongoose_instrument:execute(mod_csi_inactive, #{host_type => HostType}, #{count => 1, jid => JID}),
    case get_client_state(C2SData) of
        active ->
            set_client_state(maybe_init_buffer(Acc, C2SData, HostType), C2SData, inactive);
        inactive ->
            Acc
    end.

-spec handle_active_request(mongoose_acc:t(), params()) -> mongoose_acc:t().
handle_active_request(Acc, #{c2s_data := C2SData}) ->
    HostType = mongoose_c2s:get_host_type(C2SData),
    JID = mongoose_c2s:get_jid(C2SData),
    mongoose_instrument:execute(mod_csi_active, #{host_type => HostType}, #{count => 1, jid => JID}),
    case get_client_state(C2SData) of
        active ->
            Acc;
        inactive ->
            set_client_state(maybe_flush_buffer(Acc, C2SData), C2SData, active)
    end.

-spec maybe_init_buffer(mongoose_acc:t(), mongoose_c2s:data(), mongooseim:host_type()) ->
          mongoose_acc:t().
maybe_init_buffer(Acc, C2SData, HostType) ->
    maybe
        {error, not_found} ?= mongoose_c2s:get_mod_state(C2SData, ?MODULE),
        #{buffer := #{max_size := BMax}} ?= gen_mod:get_module_opts(HostType, ?MODULE),
        Csi = #csi_buffer_state{buffer_max = BMax},
        mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, Csi})
    else
        _ -> Acc
    end.

-spec maybe_flush_buffer(mongoose_acc:t(), mongoose_c2s:data()) -> mongoose_acc:t().
maybe_flush_buffer(Acc, C2SData) ->
    case mongoose_c2s:get_mod_state(C2SData, ?MODULE) of
        {ok, Csi = #csi_buffer_state{buffer = Buffer}} ->
            NewCsi = Csi#csi_buffer_state{buffer = []},
            ToAcc = [{state_mod, {?MODULE, NewCsi}}, {flush, lists:reverse(Buffer)}],
            mongoose_c2s_acc:to_acc_many(Acc, ToAcc);
        _ ->
            Acc
    end.

-spec get_client_state(mongoose_c2s:data()) -> state().
get_client_state(C2SData) ->
    case mongoose_c2s:get_info(C2SData) of
        #{client_state := State} -> State;
        _ -> active
    end.

-spec set_client_state(mongoose_acc:t(), mongoose_c2s:data(), state()) -> mongoose_acc:t().
set_client_state(Acc, C2SData, State) ->
    JID = mongoose_c2s:get_jid(C2SData),
    SID = mongoose_c2s:get_sid(C2SData),
    Info = mongoose_c2s:get_info(C2SData),
    ejabberd_sm:store_info(JID, SID, client_state, State),
    C2SData1 = mongoose_c2s:set_info(C2SData, Info#{client_state => State}),
    mongoose_c2s_acc:to_acc(Acc, c2s_data, C2SData1).

-spec aggregate_client_state([ejabberd_sm:session()]) -> state().
aggregate_client_state([Session | Sessions]) ->
    case mongoose_session:get_info(Session) of
        #{client_state := inactive} -> aggregate_client_state(Sessions);
        #{} -> active
    end;
aggregate_client_state([]) ->
    inactive.

-spec csi() -> exml:element().
csi() ->
    #xmlel{name = <<"csi">>, attrs = #{<<"xmlns">> => ?NS_CSI}}.

-spec mark_acc_as_buffered(mongoose_acc:t()) -> mongoose_acc:t().
mark_acc_as_buffered(Acc) ->
    mongoose_acc:set(?MODULE, buffered, true, Acc).

-spec is_acc_buffered(mongoose_acc:t()) -> boolean().
is_acc_buffered(Acc) ->
    mongoose_acc:get(?MODULE, buffered, false, Acc).
