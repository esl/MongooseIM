-module(mod_stream_management).
-xep([{xep, 198}, {version, "1.6"}]).
-behaviour(gen_mod).
-behaviour(gen_statem).
-behaviour(mongoose_module_metrics).

%% `gen_mod' callbacks
-export([start/2,
         stop/1,
         config_spec/0,
         supported_features/0,
         process_buffer_and_ack/1]).

%% hooks handlers
-export([c2s_stream_features/3,
         remove_smid/5,
         session_cleanup/5]).

%% API for `mongoose_c2s'
-export([make_smid/0,
         get_session_from_smid/2,
         get_buffer_max/1,
         get_ack_freq/1,
         get_resume_timeout/1,
         register_smid/3]).

-export([user_send_packet/3,
         user_receive_packet/3,
         user_send_xmlel/3,
         foreign_event/3,
         handle_user_stop/3,
         user_terminate/3
        ]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3]).

%% API for inspection and tests
-export([get_sid/2,
         get_stale_h/2,
         register_stale_smid_h/3,
         remove_stale_smid_h/2]).

-ignore_xref([c2s_stream_features/3, get_sid/2, get_stale_h/2, remove_smid/5,
              register_stale_smid_h/3, remove_stale_smid_h/2, session_cleanup/5]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").
-define(STREAM_MGMT_H_MAX, (1 bsl 32 - 1)).
-define(STREAM_MGMT_SHUTDOWN, {shutdown, stream_management}).

-record(sm_handler, {
          buffer = [] :: [mongoose_acc:t()],
          buffer_size = 0 :: non_neg_integer(),
          counter_in = 0 :: short(),
          counter_out = 0 :: short(),
          buffer_max = 100 :: buffer_max(),
          ack_freq = 1 :: ack_freq()
         }).

-type sm_handler() :: #sm_handler{}.

-type buffer_max() :: pos_integer() | infinity | no_buffer.
-type ack_freq() :: pos_integer() | never.

-type smid() :: base64:ascii_binary().
-type short() :: 0..?STREAM_MGMT_H_MAX.

-type maybe_smid() :: {sid, ejabberd_sm:sid()}
                    | {stale_h, non_neg_integer()}
                    | {error, smid_not_found}.

-export_type([smid/0]).

%%
%% `gen_mod' callbacks
%%

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    mod_stream_management_backend:init(HostType, Opts),
    ?LOG_INFO(#{what => stream_management_starting}),
    ejabberd_hooks:add(hooks(HostType)),
    gen_hook:add_handlers(c2s_hooks(HostType)),
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ?LOG_INFO(#{what => stream_management_stopping}),
    gen_hook:delete_handlers(c2s_hooks(HostType)),
    ejabberd_hooks:delete(hooks(HostType)),
    ok.

hooks(HostType) ->
    [{sm_remove_connection_hook, HostType, ?MODULE, remove_smid, 50},
     {c2s_stream_features, HostType, ?MODULE, c2s_stream_features, 50},
     {session_cleanup, HostType, ?MODULE, session_cleanup, 50}].

-spec c2s_hooks(mongooseim:host_type()) -> gen_hook:hook_list(mongoose_c2s_hooks:hook_fn()).
c2s_hooks(HostType) ->
    [
     {user_send_packet, HostType, fun ?MODULE:user_send_packet/3, #{}, 100},
     {user_receive_packet, HostType, fun ?MODULE:user_receive_packet/3, #{}, 100},
     {user_send_xmlel, HostType, fun ?MODULE:user_send_xmlel/3, #{}, 100},
     {foreign_event, HostType, fun ?MODULE:foreign_event/3, #{}, 100},
     {user_stop_request, HostType, fun ?MODULE:handle_user_stop/3, #{}, 100},
     {user_socket_closed, HostType, fun ?MODULE:handle_user_stop/3, #{}, 100},
     {user_socket_error, HostType, fun ?MODULE:handle_user_stop/3, #{}, 100},
     {user_terminate, HostType, fun ?MODULE:user_terminate/3, #{}, 80}
    ].

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{
        items = #{<<"backend">> => #option{type = atom, validate = {module, ?MODULE}},
                  <<"buffer">> => #option{type = boolean},
                  <<"buffer_max">> => #option{type = int_or_infinity,
                                              validate = positive},
                  <<"ack">> => #option{type = boolean},
                  <<"ack_freq">> => #option{type = integer,
                                            validate = positive},
                  <<"resume_timeout">> => #option{type = integer,
                                                  validate = positive},
                  <<"stale_h">> => stale_h_config_spec()
                 },
        process = fun ?MODULE:process_buffer_and_ack/1,
        defaults = #{<<"backend">> => mnesia,
                     <<"buffer">> => true,
                     <<"buffer_max">> => 100,
                     <<"ack">> => true,
                     <<"ack_freq">> => 1,
                     <<"resume_timeout">> => 600 % seconds
        }
      }.

supported_features() -> [dynamic_domains].

process_buffer_and_ack(Opts = #{buffer := Buffer, ack := Ack}) ->
    OptsWithBuffer = check_buffer(Opts, Buffer),
    check_ack(OptsWithBuffer, Ack).

check_buffer(Opts, false) ->
    Opts#{buffer_max => no_buffer};
check_buffer(Opts, _) ->
    Opts.

check_ack(Opts, false) ->
    Opts#{ack_freq => never};
check_ack(Opts, _) ->
    Opts.

stale_h_config_spec() ->
    #section{
        items = #{<<"enabled">> => #option{type = boolean},
                  <<"repeat_after">> => #option{type = integer,
                                                validate = positive},
                  <<"geriatric">> => #option{type = integer,
                                             validate = positive}},
        include = always,
        defaults = #{<<"enabled">> => false,
                     <<"repeat_after">> => 1800, % seconds
                     <<"geriatric">> => 3600 % seconds
        }
    }.

%%
%% hooks handlers
%%

-spec user_send_packet(mongoose_acc:t(), mongoose_c2s:hook_params(), gen_hook:extra()) ->
    gen_hook:hook_fn_ret(mongoose_acc:t()).
user_send_packet(Acc, #{c2s_data := StateData}, _Extra) ->
    case get_handler(StateData) of
        {error, not_found} ->
            {ok, Acc};
        #sm_handler{counter_in = Counter} = SmHandler ->
            NewSmHandler = SmHandler#sm_handler{counter_in = incr_counter(Counter)},
            {ok, mongoose_c2s_acc:to_acc(Acc, handlers, {?MODULE, NewSmHandler})}
    end.

-spec user_receive_packet(mongoose_acc:t(), mongoose_c2s:hook_params(), gen_hook:extra()) ->
    gen_hook:hook_fn_ret(mongoose_acc:t()).
user_receive_packet(Acc, #{c2s_data := StateData}, _Extra) ->
    case get_handler(StateData) of
        {error, not_found} ->
            {ok, Acc};
        #sm_handler{buffer_max = no_buffer} = SmHandler ->
            maybe_send_ack_request(Acc, SmHandler);
        SmHandler ->
            handle_buffer_and_ack(Acc, StateData, SmHandler)
    end.

handle_buffer_and_ack(Acc, StateData,
        #sm_handler{buffer = Buffer, buffer_max = BufferMax,
                    buffer_size = BufferSize} = SmHandler) ->
    case is_buffer_full(BufferSize, BufferMax) of
        true ->
            Lang = mongoose_c2s:get_lang(StateData),
            Err = mongoose_xmpp_errors:stream_resource_constraint(
                    Lang, <<"too many unacked stanzas">>),
            mongoose_c2s:c2s_stream_error(StateData, Err),
            {stop, mongoose_c2s_acc:to_acc(Acc, stop, {shutdown, too_many_unacked_stanzas})};
        false ->
            NewSmHandler = SmHandler#sm_handler{buffer = [Acc | Buffer],
                                                buffer_size = BufferSize + 1},
            maybe_send_ack_request(Acc, NewSmHandler)
    end.

-spec is_buffer_full(non_neg_integer(), buffer_max()) -> boolean().
is_buffer_full(_BufferSize, infinity) ->
    false;
is_buffer_full(BufferSize, BufferMax) when BufferSize =< BufferMax ->
    false;
is_buffer_full(_, _) ->
    true.

-spec maybe_send_ack_request(mongoose_acc:t(), sm_handler()) ->
    gen_hook:hook_fn_ret(mongoose_acc:t()).
maybe_send_ack_request(Acc, #sm_handler{ack_freq = never} = SmHandler) ->
    {ok, mongoose_c2s_acc:to_acc(Acc, handlers, {?MODULE, SmHandler})};
maybe_send_ack_request(Acc, #sm_handler{buffer_size = BufferSize,
                                        counter_out = Out,
                                        ack_freq = AckFreq} = SmHandler)
  when 0 =:= (Out + BufferSize) rem AckFreq ->
    Stanza = stream_mgmt_request(),
    ToAcc = [{socket_send, Stanza}, {handlers, {?MODULE, SmHandler}}],
    {ok, mongoose_c2s_acc:to_acc_many(Acc, ToAcc)};
maybe_send_ack_request(Acc, _) ->
    {ok, Acc}.

-spec user_terminate(mongoose_acc:t(), mongoose_c2s:hook_params(), gen_hook:extra()) ->
    gen_hook:hook_fn_ret(mongoose_acc:t()).
user_terminate(Acc, #{c2s_data := StateData}, #{host_type := HostType}) ->
    case {get_handler(StateData), mongoose_acc:get(c2s, terminate, normal, Acc)} of
        {{error, not_found}, _} ->
            {ok, Acc};
        {_, ?STREAM_MGMT_SHUTDOWN} ->
            {ok, Acc};
        {SmHandler, _} ->
            NewSmHandler = handle_user_terminate(SmHandler, StateData, HostType),
            {ok, mongoose_c2s_acc:to_acc(Acc, handlers, {?MODULE, NewSmHandler})}
    end.

-spec user_send_xmlel(Acc, Params, Extra) -> Result when
      Acc :: mongoose_acc:t(),
      Params :: mongoose_c2s:hook_params(),
      Extra :: gen_hook:extra(),
      Result :: gen_hook:hook_fn_ret(mongoose_acc:t()).
user_send_xmlel(Acc, Params, _Extra) ->
    El = mongoose_acc:element(Acc),
    case exml_query:attr(El, <<"xmlns">>) of
        ?NS_STREAM_MGNT_3 ->
            handle_stream_mgnt(Acc, Params, El);
        _ -> {ok, Acc}
    end.

-spec foreign_event(Acc, Params, Extra) -> Result when
      Acc :: mongoose_acc:t(),
      Params :: mongoose_c2s:hook_params(),
      Extra :: gen_hook:extra(),
      Result :: gen_hook:hook_fn_ret(mongoose_acc:t()).
foreign_event(Acc, #{c2s_data := StateData, event_type := {call, From}, event_content := resume}, _Extra) ->
    case handle_resume_call(StateData, Acc, From) of
        {stop, bad_stream_management_request} ->
            {stop, mongoose_c2s_acc:to_acc(Acc, stop, bad_stream_management_request)};
        {stop_and_reply, Reason, ReplyAction, NewStateData} ->
            gen_statem:reply(ReplyAction),
            ToAcc = [{c2s_data, NewStateData}, {hard_stop, Reason}],
            {stop, mongoose_c2s_acc:to_acc_many(Acc, ToAcc)}
    end;
foreign_event(Acc, _Params, _Extra) ->
    {ok, Acc}.

-spec handle_user_terminate(sm_handler(), mongoose_c2s:c2s_data(), mongooseim:host_type()) ->
    sm_handler().
handle_user_terminate(SmHandler = #sm_handler{buffer = Buffer}, StateData, HostType) ->
    Jid = mongoose_c2s:get_jid(StateData),
    OrderedBuffer = lists:reverse(Buffer),
    FilteredBuffer = mongoose_hooks:filter_unacknowledged_messages(HostType, Jid, OrderedBuffer),
    [mongoose_c2s:reroute(StateData, BufferedAcc) || BufferedAcc <- FilteredBuffer],
    SmHandler#sm_handler{buffer = [], buffer_size = 0}.

-spec handle_user_stop(Acc, Params, Extra) -> Result when
      Acc :: mongoose_acc:t(),
      Params :: mongoose_c2s:hook_params(),
      Extra :: gen_hook:extra(),
      Result :: gen_hook:hook_fn_ret(mongoose_acc:t()).
handle_user_stop(Acc, #{c2s_data := StateData}, #{host_type := HostType}) ->
    case get_handler(StateData) of
        {error, not_found} ->
            {ok, Acc};
        _ ->
            Timeout = get_resume_timeout(HostType),
            Actions = [{push_callback_module, ?MODULE}, {timeout, Timeout, resume_timeout}, hibernate],
            ToAcc = [{c2s_state, resume_session}, {actions, Actions}],
            {stop, mongoose_c2s_acc:to_acc_many(Acc, ToAcc)}
    end.

-spec handle_stream_mgnt(mongoose_acc:t(), mongoose_c2s:hook_params(), exml:element()) ->
    mongoose_c2s_hooks:hook_result().
handle_stream_mgnt(Acc, Params = #{c2s_state := session_established}, El = #xmlel{name = <<"a">>}) ->
    handle_a(Acc, Params, El);
handle_stream_mgnt(Acc, Params = #{c2s_state := session_established}, #xmlel{name = <<"r">>}) ->
    handle_r(Acc, Params);
handle_stream_mgnt(Acc, Params = #{c2s_state := {wait_for_feature_after_auth, _}}, El = #xmlel{name = <<"resume">>}) ->
    handle_resume(Acc, Params, El);
handle_stream_mgnt(Acc, Params = #{c2s_state := session_established}, El = #xmlel{name = <<"enable">>}) ->
    handle_enable(Acc, Params, El);
handle_stream_mgnt(Acc, #{c2s_data := StateData, c2s_state := C2SState}, _El) ->
    unexpected_sm_request(Acc, StateData, C2SState);
handle_stream_mgnt(Acc, _Params, _El) ->
    {ok, Acc}.

-spec handle_r(mongoose_acc:t(), mongoose_c2s:hook_params()) ->
    mongoose_c2s_hooks:hook_result().
handle_r(Acc, #{c2s_data := StateData}) ->
    case get_handler(StateData) of
        {error, not_found} ->
            ?LOG_WARNING(#{what => unexpected_r, c2s_state => StateData,
                           text => <<"received <r/> but stream management is off!">>}),
            {ok, Acc};
        #sm_handler{counter_in = In} ->
            Stanza = stream_mgmt_ack(In),
            {ok, mongoose_c2s_acc:to_acc(Acc, socket_send, Stanza)}
    end.

-spec handle_a(mongoose_acc:t(), mongoose_c2s:hook_params(), exml:element()) ->
    mongoose_c2s_hooks:hook_result().
handle_a(Acc, #{c2s_data := StateData}, El) ->
    case {get_handler(StateData), stream_mgmt_parse_h(El)} of
        {{error, not_found}, _} ->
            {ok, Acc};
        {_, invalid_h_attribute} ->
            Stanza = mongoose_xmpp_errors:policy_violation(?MYLANG, <<"Invalid h attribute">>),
            mongoose_c2s:c2s_stream_error(StateData, Stanza),
            {stop, mongoose_c2s_acc:to_acc(Acc, stop, {shutdown, invalid_h_attribute})};
        {Handler, H} ->
            do_handle_ack(Acc, StateData, Handler, H)
    end.

-spec do_handle_ack(mongoose_acc:t(), mongoose_c2s:c2s_data(), sm_handler(), non_neg_integer()) ->
    mongoose_c2s_hooks:hook_result().
do_handle_ack(Acc, StateData, SmHandler = #sm_handler{counter_out = OldAcked,
                                                      buffer_size = BufferSize,
                                                      buffer = Buffer}, Acked) ->
    ToDrop = calc_to_drop(Acked, OldAcked),
    case BufferSize < ToDrop of
        true ->
            ErrorStanza0 = #xmlel{children = Children}
                = mongoose_xmpp_errors:undefined_condition(
                    ?MYLANG, <<"You acknowledged more stanzas that what has been sent">>),
            HandledCountField = sm_handled_count_too_high_stanza(Acked, OldAcked),
            ErrorStanza = ErrorStanza0#xmlel{children = [HandledCountField | Children]},
            mongoose_c2s:c2s_stream_error(StateData, ErrorStanza),
            {stop, mongoose_c2s_acc:to_acc(Acc, stop, {shutdown, sm_handled_count_too_high_stanza})};
        false ->
            NewSize = BufferSize - ToDrop,
            {NewBuffer, _Dropped} = lists:split(NewSize, Buffer),
            NewSmHandler = SmHandler#sm_handler{counter_out = Acked, buffer_size = NewSize, buffer = NewBuffer},
            {ok, mongoose_c2s_acc:to_acc(Acc, handlers, {?MODULE, NewSmHandler})}
    end.

-spec incr_counter(short()) -> short().
incr_counter(Incoming) when Incoming < ?STREAM_MGMT_H_MAX - 1 ->
    Incoming + 1;
incr_counter(_Incoming) ->
    0.

-spec calc_to_drop(short(), short()) -> short().
calc_to_drop(Acked, OldAcked) when Acked >= OldAcked ->
    Acked - OldAcked;
calc_to_drop(Acked, OldAcked) ->
    Acked + ?STREAM_MGMT_H_MAX - OldAcked + 1.

-spec handle_enable(mongoose_acc:t(), mongoose_c2s:hook_params(), exml:element()) ->
    mongoose_c2s_hooks:hook_result().
handle_enable(Acc, #{c2s_data := StateData}, El) ->
    case {get_handler(StateData), exml_query:attr(El, <<"resume">>, false)} of
        {{error, not_found}, <<"true">>} ->
            do_handle_enable(Acc, StateData, true);
        {{error, not_found}, <<"1">>} ->
            do_handle_enable(Acc, StateData, true);
        {{error, not_found}, false} ->
            do_handle_enable(Acc, StateData, false);
        {{error, not_found}, _InvalidResumeAttr} ->
            stream_error(Acc, StateData);
        {#sm_handler{}, _} ->
            stream_error(Acc, StateData)
    end.

-spec do_handle_enable(mongoose_acc:t(), mongoose_c2s:c2s_data(), boolean()) ->
    mongoose_c2s_hooks:hook_result().
do_handle_enable(Acc, StateData, false) ->
    Stanza = stream_mgmt_enabled(),
    HostType = mongoose_c2s:get_host_type(StateData),
    SmHandler = build_sm_handler(HostType),
    ToAcc = [{handlers, {?MODULE, SmHandler}}, {socket_send, Stanza}],
    {ok, mongoose_c2s_acc:to_acc_many(Acc, ToAcc)};

do_handle_enable(Acc, StateData, true) ->
    SMID = make_smid(),
    Sid = mongoose_c2s:get_sid(StateData),
    HostType = mongoose_c2s:get_host_type(StateData),
    ok = register_smid(HostType, SMID, Sid),
    Stanza = stream_mgmt_enabled([{<<"id">>, SMID}, {<<"resume">>, <<"true">>}]),
    SmHandler = build_sm_handler(HostType),
    ToAcc = [{handlers, {?MODULE, SmHandler}}, {socket_send, Stanza}],
    {ok, mongoose_c2s_acc:to_acc_many(Acc, ToAcc)}.

-spec handle_resume(mongoose_acc:t(), mongoose_c2s:hook_params(), exml:element()) ->
    mongoose_c2s_hooks:hook_result().
handle_resume(Acc, #{c2s_state := C2SState, c2s_data := StateData}, El) ->
    case {exml_query:attr(El, <<"previd">>, undefined),
          stream_mgmt_parse_h(El),
          get_handler(StateData)} of
        {undefined, _, _} ->
            bad_request(Acc);
        {_, invalid_h_attribute, _} ->
            bad_request(Acc);
        {_, _, #sm_handler{}} ->
            bad_request(Acc);
        {SMID, H, {error, not_found}} ->
            HostType = mongoose_c2s:get_host_type(StateData),
            FromSMID = get_session_from_smid(HostType, SMID),
            do_handle_resume(Acc, StateData, C2SState, SMID, H, FromSMID)
    end.

-spec do_handle_resume(Acc, StateData, C2SState, SMID, H, FromSMID) -> HookResult when
      Acc :: mongoose_acc:t(),
      StateData :: mongoose_c2s:c2s_data(),
      C2SState :: mongoose_c2s:c2s_state(),
      SMID :: smid(),
      H :: non_neg_integer(),
      FromSMID :: maybe_smid(),
      HookResult :: mongoose_c2s_hooks:hook_result().
do_handle_resume(Acc, StateData, _FsmState, SMID, H, {sid, {_TS, Pid}}) ->
    case get_peer_state(Pid) of
        {ok, OldStateData} ->
            NewState = mongoose_c2s:merge_states(OldStateData, StateData),
            do_resume(Acc, NewState, SMID, H);
        {C, R, S} ->
            ?LOG_WARNING(#{what => resumption_error,
                           text => <<"Resumption error because of invalid response">>,
                           class => C, reason => R, stacktrace => S, c2s_state => StateData}),
            Err = stream_mgmt_failed(<<"item-not-found">>),
            {stop, mongoose_c2s_acc:to_acc(Acc, socket_send, Err)}

    end;
do_handle_resume(Acc, StateData, C2SState, SMID, _H, {stale_h, StaleH}) ->
    ?LOG_WARNING(#{what => resumption_error, reason => session_resumption_timed_out,
                 smid => SMID, stale_h => StaleH, c2s_state => StateData}),
    Err = stream_mgmt_failed(<<"item-not-found">>, [{<<"h">>, integer_to_binary(StaleH)}]),
    stream_mgmt_error(Acc, StateData, C2SState, Err);
do_handle_resume(Acc, StateData, C2SState, SMID, _H, {error, smid_not_found}) ->
    ?LOG_WARNING(#{what => resumption_error, reason => no_previous_session_for_smid,
                 smid => SMID, c2s_state => StateData}),
    Err = stream_mgmt_failed(<<"item-not-found">>),
    stream_mgmt_error(Acc, StateData, C2SState, Err).

-spec do_resume(Acc, StateData, SMID, H) -> HookResult when
      Acc :: mongoose_acc:t(),
      StateData :: mongoose_c2s:c2s_data(),
      SMID :: smid(),
      H :: non_neg_integer(),
      HookResult :: mongoose_c2s_hooks:hook_result().
do_resume(Acc0, StateData, SMID, H) ->
    NewSid = ejabberd_sm:make_new_sid(),
    OldSmHandler = get_handler(StateData),
    case do_handle_ack(Acc0, StateData, OldSmHandler, H) of
        {ok, Acc1} ->
            Jid = mongoose_c2s:get_jid(StateData),
            LServer = mongoose_c2s:get_lserver(StateData),
            HostType = mongoose_c2s:get_host_type(StateData),
            ejabberd_sm:open_session(HostType, NewSid, Jid, 0, #{}),
            ok = register_smid(HostType, SMID, NewSid),
            #{?MODULE := NewSmHandler} = mongoose_c2s_acc:from_mongoose_acc(Acc1, handlers),
            Stanzas = get_all_stanzas_to_forward(NewSmHandler, SMID, LServer),
            ToAcc = [{c2s_state, session_established}, {c2s_data, StateData}, {socket_send, Stanzas}],
            {ok, mongoose_c2s_acc:to_acc_many(Acc1, ToAcc)};
        {stop, Acc} ->
            {stop, Acc}
    end.

-spec get_all_stanzas_to_forward(sm_handler(), smid(), jid:lserver()) -> [exml:element()].
get_all_stanzas_to_forward(#sm_handler{counter_in = Counter, buffer = Buffer}, SMID, LServer) ->
    Resumed = stream_mgmt_resumed(SMID, Counter),
    FromServer = jid:make_noprep(<<>>, LServer, <<>>),
    ToForward = [ begin
                      TS = mongoose_acc:timestamp(Acc),
                      Packet = mongoose_acc:element(Acc),
                      StanzaName = mongoose_acc:stanza_name(Acc),
                      StanzaType = mongoose_acc:stanza_type(Acc),
                      maybe_add_timestamp(Packet, StanzaName, StanzaType, TS, FromServer)
                  end || Acc <- lists:reverse(Buffer)],
    [Resumed | ToForward].

maybe_add_timestamp(Packet, <<"message">>, <<"error">>, _, _) ->
    Packet;
maybe_add_timestamp(Packet, <<"message">>, <<"headline">>, _, _) ->
    Packet;
maybe_add_timestamp(Packet, <<"message">>, _, TS, FromServer) ->
    jlib:maybe_append_delay(Packet, FromServer, TS, <<"SM Storage">>);
maybe_add_timestamp(Packet, _StanzaName, _StanzaType, _TS, _FromServer) ->
    Packet.

-spec bad_request(mongoose_acc:t()) ->
    {stop, mongoose_acc:t()}.
bad_request(Acc) ->
    Err = stream_mgmt_failed(<<"bad-request">>),
    {stop, mongoose_c2s_acc:to_acc(Acc, socket_send, Err)}.

-spec stream_error(mongoose_acc:t(), mongoose_c2s:c2s_data()) ->
    {stop, mongoose_acc:t()}.
stream_error(Acc, StateData) ->
    Err = stream_mgmt_failed(<<"unexpected-request">>),
    mongoose_c2s:c2s_stream_error(StateData, Err),
    {stop, mongoose_c2s_acc:to_acc(Acc, stop, {shutdown, stream_error})}.

-spec unexpected_sm_request(mongoose_acc:t(), mongoose_c2s:c2s_data(), mongoose_c2s:c2s_state()) ->
    {stop, mongoose_acc:t()}.
unexpected_sm_request(Acc, StateData, C2SState) ->
    Err = stream_mgmt_failed(<<"unexpected-request">>),
    stream_mgmt_error(Acc, StateData, C2SState, Err).

-spec stream_mgmt_error(
        mongoose_acc:t(), mongoose_c2s:c2s_data(), mongoose_c2s:c2s_state(), exml:element()) ->
    {stop, mongoose_acc:t()}.
stream_mgmt_error(Acc, _StateData, C2SState, Err) ->
    case mongoose_c2s:maybe_retry_state(C2SState) of
        {stop, {shutdown, retries}} ->
            {stop, mongoose_c2s_acc:to_acc(Acc, stop, {shutdown, retries})};
        NewFsmState ->
            ToAcc = [{c2s_state, NewFsmState}, {socket_send, Err}],
            {stop, mongoose_c2s_acc:to_acc_many(Acc, ToAcc)}
    end.

-spec build_sm_handler(mongooseim:host_type()) -> sm_handler().
build_sm_handler(HostType) ->
    BufferMax = get_buffer_max(HostType),
    AckFreq = get_ack_freq(HostType),
    #sm_handler{buffer_max = BufferMax, ack_freq = AckFreq}.

-spec get_handler(mongoose_c2s:c2s_data()) -> sm_handler() | {error, not_found}.
get_handler(StateData) ->
    mongoose_c2s:get_handler(StateData, ?MODULE).

-spec get_peer_state(pid()) -> {ok, mongoose_c2s:c2s_data()} | {_, _, _}.
get_peer_state(Pid) ->
    try gen_statem:call(Pid, resume, 5000) of
        {ok, StateData} -> {ok, StateData}
    catch
        C:R:S -> {C, R, S}
    end.

-spec stream_mgmt_parse_h(exml:element()) -> invalid_h_attribute | non_neg_integer().
stream_mgmt_parse_h(El) ->
    case catch binary_to_integer(exml_query:attr(El, <<"h">>)) of
        H when is_integer(H), H >= 0 -> H;
        _ -> invalid_h_attribute
    end.

-spec c2s_stream_features([exml:element()], mongooseim:host_type(), jid:lserver()) ->
          [exml:element()].
c2s_stream_features(Acc, _HostType, _Lserver) ->
    lists:keystore(<<"sm">>, #xmlel.name, Acc, sm()).

-spec sm() -> exml:element().
sm() ->
    #xmlel{name = <<"sm">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3}]}.

-spec remove_smid(Acc, SID, JID, Info, Reason) -> Acc1 when
      Acc :: mongoose_acc:t(),
      SID :: ejabberd_sm:sid(),
      JID :: undefined | jid:jid(),
      Info :: undefined | [any()],
      Reason :: undefined | ejabberd_sm:close_reason(),
      Acc1 :: mongoose_acc:t().
remove_smid(Acc, SID, _JID, _Info, _Reason) ->
    HostType = mongoose_acc:host_type(Acc),
    do_remove_smid(Acc, HostType, SID).

-spec session_cleanup(Acc :: map(), LUser :: jid:luser(), LServer :: jid:lserver(),
                      LResource :: jid:lresource(), SID :: ejabberd_sm:sid()) -> any().
session_cleanup(Acc, _LUser, _LServer, _LResource, SID) ->
    HostType = mongoose_acc:host_type(Acc),
    do_remove_smid(Acc, HostType, SID).

-spec do_remove_smid(mongoose_acc:t(), mongooseim:host_type(), ejabberd_sm:sid()) ->
    mongoose_acc:t().
do_remove_smid(Acc, HostType, SID) ->
    H = mongoose_acc:get(stream_mgmt, h, undefined, Acc),
    MaybeSMID = unregister_smid(HostType, SID),
    case MaybeSMID of
        {ok, SMID} when H =/= undefined ->
            register_stale_smid_h(HostType, SMID, H);
        _ ->
            ok
    end,
    mongoose_acc:set(stream_mgmt, smid, MaybeSMID, Acc).

%%
%% `gen_statem' callbacks
%%

-type c2s_state() :: resume_session.

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    handle_event_function.

-spec init(term()) -> gen_statem:init_result(mongoose_c2s:c2s_state(), mongoose_c2s:c2s_data()).
init(_) ->
    {stop, this_should_have_never_been_called}.

-spec handle_event(gen_statem:event_type(), term(), c2s_state(), mongoose_c2s:c2s_data()) ->
    gen_statem:event_handler_result(c2s_state()).
handle_event({call, From}, resume, resume_session, StateData) ->
    LServer = mongoose_c2s:get_lserver(StateData),
    HostType = mongoose_c2s:get_host_type(StateData),
    Acc = mongoose_acc:new(#{host_type => HostType, lserver => LServer, location => ?LOCATION}),
    handle_resume_call(StateData, Acc, From);
handle_event(timeout, resume_timeout, resume_session, StateData) ->
    {stop, {shutdown, resume_timeout}, StateData};
handle_event(EventType, EventContent, C2SState, StateData) ->
    mongoose_c2s:handle_event(EventType, EventContent, C2SState, StateData).

-spec handle_resume_call(mongoose_c2s:c2s_data(), mongoose_acc:t(), gen_statem:from()) ->
    mongoose_c2s:fsm_res().
handle_resume_call(StateData, Acc, From) ->
    case get_handler(StateData) of
        {error, not_found} ->
            {stop, bad_stream_management_request};
        #sm_handler{} ->
            Sid = mongoose_c2s:get_sid(StateData),
            Jid = mongoose_c2s:get_jid(StateData),
            ejabberd_sm:close_session(Acc, Sid, Jid, resumed),
            mongoose_c2s:c2s_stream_error(StateData, mongoose_xmpp_errors:stream_conflict()),
            ReplyAction = {reply, From, {ok, StateData}},
            NewStateData = mongoose_c2s:remove_handler(StateData, ?MODULE),
            {stop_and_reply, {shutdown, resumed}, ReplyAction, NewStateData}
    end.

terminate(Reason, C2SState, StateData) ->
    ?LOG_DEBUG(#{what => c2s_statem_terminate, reason => Reason, c2s_state => C2SState, c2s_data => StateData}),
    mongoose_c2s:terminate(?STREAM_MGMT_SHUTDOWN, C2SState, StateData).

%%
%% API for `mongoose_c2s'
%%

-spec make_smid() -> smid().
make_smid() ->
    base64:encode(crypto:strong_rand_bytes(21)).

-spec stream_mgmt_enabled() -> exml:element().
stream_mgmt_enabled() ->
    stream_mgmt_enabled([]).

-spec stream_mgmt_enabled([exml:attr()]) -> exml:element().
stream_mgmt_enabled(ExtraAttrs) ->
    #xmlel{name = <<"enabled">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3}] ++ ExtraAttrs}.

-spec stream_mgmt_resumed(smid(), short()) -> exml:element().
stream_mgmt_resumed(SMID, Handled) ->
    #xmlel{name = <<"resumed">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3},
                    {<<"previd">>, SMID},
                    {<<"h">>, integer_to_binary(Handled)}]}.

-spec stream_mgmt_failed(binary()) -> exml:element().
stream_mgmt_failed(Reason) ->
    stream_mgmt_failed(Reason, []).

-spec stream_mgmt_failed(binary(), [exml:attr()]) -> exml:element().
stream_mgmt_failed(Reason, Attrs) ->
    ReasonEl = #xmlel{name = Reason,
                      attrs = [{<<"xmlns">>, ?NS_STANZAS}]},
    #xmlel{name = <<"failed">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3} | Attrs],
           children = [ReasonEl]}.

-spec stream_mgmt_ack(non_neg_integer()) -> exml:element().
stream_mgmt_ack(NIncoming) ->
    #xmlel{name = <<"a">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3},
                    {<<"h">>, integer_to_binary(NIncoming)}]}.

-spec stream_mgmt_request() -> exml:element().
stream_mgmt_request() ->
    #xmlel{name = <<"r">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3}]}.

-spec sm_handled_count_too_high_stanza(non_neg_integer(), non_neg_integer()) -> exml:element().
sm_handled_count_too_high_stanza(Handled, OldAcked) ->
    #xmlel{name = <<"handled-count-too-high">>,
           attrs = [{<<"xmlns">>, ?NS_STREAM_MGNT_3},
                    {<<"h">>, integer_to_binary(Handled)},
                    {<<"send-count">>, integer_to_binary(OldAcked)}]}.

%% Getters
-spec get_session_from_smid(mongooseim:host_type(), smid()) -> maybe_smid().
get_session_from_smid(HostType, SMID) ->
    case get_sid(HostType, SMID) of
        {sid, SID} -> {sid, SID};
        {error, smid_not_found} ->
            get_stale_h(HostType, SMID)
    end.

-spec get_stale_h(mongooseim:host_type(), smid()) ->
    {stale_h, non_neg_integer()} | {error, smid_not_found}.
get_stale_h(HostType, SMID) ->
    case is_stale_h_enabled(HostType) of
        false -> {error, smid_not_found};
        true -> read_stale_h(HostType, SMID)
    end.

-spec get_buffer_max(mongooseim:host_type()) -> buffer_max().
get_buffer_max(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, buffer_max).

-spec get_ack_freq(mongooseim:host_type()) -> ack_freq().
get_ack_freq(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, ack_freq).

-spec get_resume_timeout(mongooseim:host_type()) -> pos_integer().
get_resume_timeout(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, resume_timeout).

-spec register_stale_smid_h(mongooseim:host_type(), smid(), short()) -> ok | {error, any()}.
register_stale_smid_h(HostType, SMID, H) ->
    case is_stale_h_enabled(HostType) of
        false -> ok;
        true -> write_stale_h(HostType, SMID, H)
    end.

-spec remove_stale_smid_h(mongooseim:host_type(), smid()) -> ok | {error, any()}.
remove_stale_smid_h(HostType, SMID) ->
    case is_stale_h_enabled(HostType) of
        false -> ok;
        true -> delete_stale_h(HostType, SMID)
    end.

-spec is_stale_h_enabled(mongooseim:host_type()) -> boolean().
is_stale_h_enabled(HostType) ->
    gen_mod:get_module_opt(HostType, ?MODULE, [stale_h, enabled]).

%% Backend operations

-spec register_smid(HostType, SMID, SID) ->
    ok | {error, term()} when
    HostType :: mongooseim:host_type(),
    SMID :: smid(),
    SID :: ejabberd_sm:sid().
register_smid(HostType, SMID, SID) ->
    mod_stream_management_backend:register_smid(HostType, SMID, SID).

-spec unregister_smid(mongooseim:host_type(), ejabberd_sm:sid()) ->
    {ok, SMID :: smid()} | {error, smid_not_found}.
unregister_smid(HostType, SID) ->
    mod_stream_management_backend:unregister_smid(HostType, SID).

-spec get_sid(mongooseim:host_type(), smid()) ->
    {sid, ejabberd_sm:sid()} | {error, smid_not_found}.
get_sid(HostType, SMID) ->
    mod_stream_management_backend:get_sid(HostType, SMID).

%% stale_h

-spec write_stale_h(HostType, SMID, H) -> ok | {error, any()} when
    HostType :: mongooseim:host_type(),
    SMID :: smid(),
    H :: non_neg_integer().
write_stale_h(HostType, SMID, H) ->
    mod_stream_management_backend:write_stale_h(HostType, SMID, H).

-spec delete_stale_h(HostType, SMID) -> ok | {error, any()} when
    HostType :: mongooseim:host_type(),
    SMID :: smid().
delete_stale_h(HostType, SMID) ->
    mod_stream_management_backend:delete_stale_h(HostType, SMID).

-spec read_stale_h(HostType, SMID) ->
    {stale_h, non_neg_integer()} | {error, smid_not_found} when
    HostType :: mongooseim:host_type(),
    SMID :: smid().
read_stale_h(HostType, SMID) ->
    mod_stream_management_backend:read_stale_h(HostType, SMID).
