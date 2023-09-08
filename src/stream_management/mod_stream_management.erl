-module(mod_stream_management).
-xep([{xep, 198}, {version, "1.6.1"}]).
-behaviour(gen_mod).
-behaviour(gen_statem).
-behaviour(mongoose_module_metrics).

%% `gen_mod' callbacks
-export([start/2,
         stop/1,
         hooks/1,
         config_spec/0,
         supported_features/0,
         process_buffer_and_ack/1]).

%% hooks handlers
-export([c2s_stream_features/3,
         session_cleanup/3,
         user_send_packet/3,
         user_receive_packet/3,
         xmpp_presend_element/3,
         user_send_xmlel/3,
         foreign_event/3,
         handle_user_stopping/3,
         user_terminate/3,
         reroute_unacked_messages/3
        ]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3]).

%% helpers
-export([handle_resume/3, if_not_already_enabled_create_sm_state/1]).
-export([stream_error/2, register_smid_return_enabled_stanza/1]).

%% API for inspection and tests
-export([get_sid/2, get_stale_h/2, get_session_from_smid/2,
         register_smid/3, register_stale_smid_h/3, remove_stale_smid_h/2]).
-ignore_xref([get_sid/2, get_stale_h/2, get_session_from_smid/2,
              register_smid/3, register_stale_smid_h/3, remove_stale_smid_h/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_config_spec.hrl").
-define(STREAM_MGMT_H_MAX, (1 bsl 32 - 1)).
-define(CONSTRAINT_CHECK_TIMEOUT, 5000).  %% 5 seconds
-define(IS_STREAM_MGMT_STOP(R), R =:= {shutdown, ?MODULE}; R =:= {shutdown, resumed}).
-define(IS_ALLOWED_STATE(S), S =:= wait_for_session_establishment; S =:= session_established).

-record(sm_state, {
          buffer = [] :: [mongoose_acc:t()],
          buffer_size = 0 :: non_neg_integer(),
          counter_in = 0 :: short(),
          counter_out = 0 :: short(),
          buffer_max = 100 :: buffer_max(),
          ack_freq = 1 :: ack_freq(),
          peer :: undefined | {gen_statem, gen_statem:from()} | {sid, ejabberd_sm:sid()}
         }).

-type sm_state() :: #sm_state{}.
-type maybe_sm_state() :: {error, not_found} | #sm_state{}.
-type c2s_state() :: mongoose_c2s:state(resume_session).

-type buffer_max() :: pos_integer() | infinity | no_buffer.
-type ack_freq() :: pos_integer() | never.

-type resume_return() :: {ok, #{resumed := exml:element(),
                                forward := [exml:element()],
                                c2s_state := mongoose_c2s:state(),
                                c2s_data := mongoose_c2s:data()}}
                       | {stream_mgmt_error, exml:element()}
                       | {error, exml:element()}
                       | {error, exml:element(), term()}.

%% Type base64:ascii_binary() is not exported
-type smid() :: binary().
-type short() :: 0..?STREAM_MGMT_H_MAX.

-type maybe_smid() :: {sid, ejabberd_sm:sid()}
                    | {stale_h, non_neg_integer()}
                    | {error, smid_not_found}.

-export_type([smid/0, short/0, sm_state/0, c2s_state/0]).

%%
%% `gen_mod' callbacks
%%

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, Opts) ->
    mod_stream_management_backend:init(HostType, Opts),
    ?LOG_INFO(#{what => stream_management_starting}).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    ?LOG_INFO(#{what => stream_management_stopping}),
    mod_stream_management_backend:stop(HostType),
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list().
hooks(HostType) ->
    [{c2s_stream_features, HostType, fun ?MODULE:c2s_stream_features/3, #{}, 50},
     {session_cleanup, HostType, fun ?MODULE:session_cleanup/3, #{}, 50}]
    ++ mod_stream_management_sasl2:hooks(HostType) ++ c2s_hooks(HostType).

-spec c2s_hooks(mongooseim:host_type()) -> gen_hook:hook_list(mongoose_c2s_hooks:fn()).
c2s_hooks(HostType) ->
    [
     {user_send_packet, HostType, fun ?MODULE:user_send_packet/3, #{}, 20},
     {user_receive_packet, HostType, fun ?MODULE:user_receive_packet/3, #{}, 10},
     {xmpp_presend_element, HostType, fun ?MODULE:xmpp_presend_element/3, #{}, 50},
     {user_send_xmlel, HostType, fun ?MODULE:user_send_xmlel/3, #{}, 50},
     {foreign_event, HostType, fun ?MODULE:foreign_event/3, #{}, 50},
     {user_stop_request, HostType, fun ?MODULE:handle_user_stopping/3, #{}, 100},
     {user_socket_closed, HostType, fun ?MODULE:handle_user_stopping/3, #{}, 100},
     {user_socket_error, HostType, fun ?MODULE:handle_user_stopping/3, #{}, 100},
     {user_terminate, HostType, fun ?MODULE:user_terminate/3, #{}, 50},
     {reroute_unacked_messages, HostType, fun ?MODULE:reroute_unacked_messages/3, #{}, 80}
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

-spec user_send_packet(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_send_packet(Acc, #{c2s_data := StateData}, _Extra) ->
    case {get_mod_state(StateData), is_sm_element(Acc)} of
        {#sm_state{counter_in = Counter} = SmState, false} ->
            NewSmState = SmState#sm_state{counter_in = incr_counter(Counter)},
            {ok, mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewSmState})};
        {_, _} ->
            {ok, Acc}
    end;
user_send_packet(Acc, _Params, _Extra) ->
    {ok, Acc}.

-spec user_receive_packet(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_receive_packet(Acc, #{c2s_data := StateData, c2s_state := C2SState}, _) ->
    Check1 = is_conflict_incoming_acc(Acc, StateData),
    Check2 = is_conflict_receiver_sid(Acc, StateData),
    case {Check1, Check2} of
        {true, _} -> %% A race condition detected: same jid, but different sids
            C2SSid = mongoose_c2s:get_sid(StateData),
            OriginSid = mongoose_acc:get(c2s, origin_sid, undefined, Acc),
            ?LOG_WARNING(#{what => conflict_check_failed,
                           text => <<"Drop Acc that is addressed to another connection "
                                     "(origin SID check failed)">>,
                           c2s_sid => C2SSid, origin_sid => OriginSid,
                           acc => Acc, state_name => C2SState, c2s_state => StateData}),
            {stop, Acc};
        {_, true} ->
            C2SSid = mongoose_c2s:get_sid(StateData),
            ReceiverSID = mongoose_acc:get(c2s, receiver_sid, undefined, Acc),
            ?LOG_WARNING(#{what => conflict_check_failed,
                           text => <<"Drop Acc that is addressed to another connection "
                                     "(receiver SID check failed)">>,
                           c2s_sid => C2SSid, receiver_sid => ReceiverSID,
                           acc => Acc, state_name => C2SState, c2s_state => StateData}),
            {stop, Acc};
        _ ->
            {ok, Acc}
    end;
user_receive_packet(Acc, _Params, _Extra) ->
    {ok, Acc}.

-spec xmpp_presend_element(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
xmpp_presend_element(Acc, #{c2s_data := StateData, c2s_state := C2SState}, _Extra) ->
    case {get_mod_state(StateData), mongoose_acc:stanza_type(Acc)} of
        {{error, not_found}, _} ->
            {ok, Acc};
        {_, <<"probe">>} ->
            {ok, Acc};
        {#sm_state{buffer_max = no_buffer} = SmState, _} ->
            maybe_send_ack_request(Acc, SmState);
        {SmState, _} ->
            Jid = mongoose_c2s:get_jid(StateData),
            handle_buffer_and_ack(Acc, C2SState, Jid, SmState)
    end.

-spec handle_buffer_and_ack(mongoose_acc:t(), c2s_state(), jid:jid(), sm_state()) ->
    mongoose_c2s_hooks:result().
handle_buffer_and_ack(Acc, C2SState, Jid, #sm_state{buffer = Buffer, buffer_max = BufferMax,
                                                    buffer_size = BufferSize} = SmState) ->
    NewBufferSize = BufferSize + 1,
    MaybeActions = case is_buffer_full(NewBufferSize, BufferMax) of
                       true ->
                           {{timeout, ?MODULE}, ?CONSTRAINT_CHECK_TIMEOUT, check_buffer_full};
                       false ->
                           []
                   end,
    Acc1 = notify_unacknowledged_msg_if_in_resume_state(Acc, Jid, C2SState),
    NewSmState = SmState#sm_state{buffer = [Acc1 | Buffer], buffer_size = NewBufferSize},
    Acc2 = mongoose_c2s_acc:to_acc(Acc, actions, MaybeActions),
    maybe_send_ack_request(Acc2, NewSmState).

notify_unacknowledged_msg_if_in_resume_state(Acc, Jid, ?EXT_C2S_STATE(resume_session)) ->
    maybe_notify_unacknowledged_msg(Acc, Jid);
notify_unacknowledged_msg_if_in_resume_state(Acc, _, _) ->
    Acc.

-spec is_buffer_full(non_neg_integer(), buffer_max()) -> boolean().
is_buffer_full(_BufferSize, infinity) ->
    false;
is_buffer_full(BufferSize, BufferMax) when BufferSize =< BufferMax ->
    false;
is_buffer_full(_, _) ->
    true.

-spec maybe_send_ack_request(mongoose_acc:t(), sm_state()) ->
    mongoose_c2s_hooks:result().
maybe_send_ack_request(Acc, #sm_state{buffer_size = BufferSize,
                                      counter_out = Out,
                                      ack_freq = AckFreq} = SmState)
  when 0 =:= (Out + BufferSize) rem AckFreq, ack_freq =/= never ->
    Stanza = mod_stream_management_stanzas:stream_mgmt_request(),
    ToAcc = [{socket_send, Stanza}, {state_mod, {?MODULE, SmState}}],
    {ok, mongoose_c2s_acc:to_acc_many(Acc, ToAcc)};
maybe_send_ack_request(Acc, SmState) ->
    {ok, mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, SmState})}.

-spec user_send_xmlel(Acc, Params, Extra) -> Result when
      Acc :: mongoose_acc:t(),
      Params :: mongoose_c2s_hooks:params(),
      Extra :: gen_hook:extra(),
      Result :: mongoose_c2s_hooks:result().
user_send_xmlel(Acc, Params, Extra) ->
    El = mongoose_acc:element(Acc),
    case exml_query:attr(El, <<"xmlns">>) of
        ?NS_STREAM_MGNT_3 ->
            handle_stream_mgmt(Acc, Params, El);
        _ ->
            user_send_packet(Acc, Params, Extra)
    end.

-spec foreign_event(Acc, Params, Extra) -> Result when
      Acc :: mongoose_acc:t(),
      Params :: mongoose_c2s_hooks:params(),
      Extra :: gen_hook:extra(),
      Result :: mongoose_c2s_hooks:result().
foreign_event(Acc, #{c2s_data := StateData,
                     event_type := {call, From},
                     event_tag := ?MODULE,
                     event_content := {resume, H}}, _Extra) ->
    case handle_resume_call(StateData, From, H) of
        {ok, SmState} ->
            FlushedStateData = mongoose_c2s:merge_mod_state(StateData, #{?MODULE => SmState}),
            ToAcc = [{c2s_data, FlushedStateData}, {hard_stop, {shutdown, resumed}}],
            {stop, mongoose_c2s_acc:to_acc_many(Acc, ToAcc)};
        error ->
            {stop, mongoose_c2s_acc:to_acc(Acc, hard_stop, bad_stream_management_request)}
    end;
foreign_event(Acc, #{c2s_data := StateData, event_type := {timeout, ?MODULE}, event_content := check_buffer_full}, _Extra) ->
    #sm_state{buffer_size = BufferSize, buffer_max = BufferMax} = get_mod_state(StateData),
    case is_buffer_full(BufferSize, BufferMax) of
        true ->
            Lang = mongoose_c2s:get_lang(StateData),
            Err = mongoose_xmpp_errors:stream_resource_constraint(Lang, <<"too many unacked stanzas">>),
            mongoose_c2s:c2s_stream_error(StateData, Err),
            {stop, mongoose_c2s_acc:to_acc(Acc, hard_stop, too_many_unacked_stanzas)};
        false ->
            {ok, Acc}
    end;
foreign_event(Acc, _Params, _Extra) ->
    {ok, Acc}.

-spec handle_user_stopping(Acc, Params, Extra) -> Result when
      Acc :: mongoose_acc:t(),
      Params :: mongoose_c2s_hooks:params(),
      Extra :: gen_hook:extra(),
      Result :: mongoose_c2s_hooks:result().
handle_user_stopping(Acc, #{c2s_data := StateData}, #{host_type := HostType}) ->
    case get_mod_state(StateData) of
        {error, not_found} ->
            {ok, Acc};
        SmState ->
            Timeout = get_resume_timeout(HostType),
            NewSmState = notify_unacknowledged_messages(Acc, StateData, SmState),
            Actions = [{push_callback_module, ?MODULE}, {{timeout, ?MODULE}, Timeout, resume_timeout}, hibernate],
            ToAcc = [{c2s_state, ?EXT_C2S_STATE(resume_session)}, {actions, Actions}, {state_mod, {?MODULE, NewSmState}}],
            {stop, mongoose_c2s_acc:to_acc_many(Acc, ToAcc)}
    end.

-spec notify_unacknowledged_messages(mongoose_acc:t(), mongoose_c2s:data(), sm_state()) ->
    sm_state().
notify_unacknowledged_messages(_, StateData, #sm_state{buffer = Buffer} = SmState) ->
    Jid = mongoose_c2s:get_jid(StateData),
    NewBuffer = [maybe_notify_unacknowledged_msg(Acc, Jid) || Acc <- lists:reverse(Buffer)],
    SmState#sm_state{buffer = lists:reverse(NewBuffer)}.

-spec maybe_notify_unacknowledged_msg(mongoose_acc:t(), jid:jid()) -> mongoose_acc:t().
maybe_notify_unacknowledged_msg(Acc, Jid) ->
    case mongoose_acc:stanza_name(Acc) of
        <<"message">> -> notify_unacknowledged_msg(Acc, Jid);
        _ -> Acc
    end.

-spec notify_unacknowledged_msg(mongoose_acc:t(), jid:jid()) -> mongoose_acc:t().
notify_unacknowledged_msg(Acc, Jid) ->
    NewAcc = mongoose_hooks:unacknowledged_message(Acc, Jid),
    mongoose_acc:strip(NewAcc).

-spec reroute_unacked_messages(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
reroute_unacked_messages(Acc, #{c2s_data := StateData, reason := Reason}, #{host_type := HostType}) ->
    MaybeSmState = get_mod_state(StateData),
    maybe_handle_stream_mgmt_reroute(Acc, StateData, HostType, Reason, MaybeSmState).

-spec user_terminate(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_terminate(Acc, #{reason := Reason}, _Extra) when ?IS_STREAM_MGMT_STOP(Reason) ->
    {stop, Acc}; %% We stop here because this termination was triggered internally
user_terminate(Acc, _Params, _Extra) ->
    {ok, Acc}.

-spec maybe_handle_stream_mgmt_reroute(
        mongoose_acc:t(), mongoose_c2s:data(), mongooseim:host_type(), term(), maybe_sm_state()) ->
    mongoose_c2s_hooks:result().
maybe_handle_stream_mgmt_reroute(Acc, StateData, HostType, Reason, #sm_state{counter_in = H} = SmState)
  when ?IS_STREAM_MGMT_STOP(Reason) ->
    Sid = mongoose_c2s:get_sid(StateData),
    do_remove_smid(HostType, Sid, H),
    NewSmState = handle_user_terminate(SmState, StateData, HostType),
    {ok, mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewSmState})};
maybe_handle_stream_mgmt_reroute(Acc, StateData, HostType, _Reason, #sm_state{} = SmState) ->
    NewSmState = handle_user_terminate(SmState, StateData, HostType),
    {ok, mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewSmState})};
maybe_handle_stream_mgmt_reroute(Acc, _StateData, _HostType, _Reason, {error, not_found}) ->
    {ok, Acc}.

-spec handle_user_terminate(sm_state(), mongoose_c2s:data(), mongooseim:host_type()) -> sm_state().
handle_user_terminate(#sm_state{counter_in = H} = SmState, StateData, HostType) ->
    Sid = mongoose_c2s:get_sid(StateData),
    do_remove_smid(HostType, Sid, H),
    reroute_buffer(StateData, SmState),
    SmState#sm_state{buffer = [], buffer_size = 0}.

reroute_buffer(StateData, #sm_state{buffer = Buffer, peer = {gen_statem, {Pid, _}}}) ->
    mongoose_c2s:reroute_buffer_to_pid(StateData, Pid, Buffer);
reroute_buffer(StateData, #sm_state{buffer = Buffer}) ->
    mongoose_c2s:reroute_buffer(StateData, Buffer).

-spec terminate(term(), c2s_state(), mongoose_c2s:data()) -> term().
terminate(Reason, C2SState, StateData) ->
    ?LOG_DEBUG(#{what => stream_mgmt_statem_terminate, reason => Reason,
                 c2s_state => C2SState, c2s_data => StateData}),
    mongoose_c2s:terminate({shutdown, ?MODULE}, C2SState, StateData).

-spec handle_stream_mgmt(mongoose_acc:t(), mongoose_c2s_hooks:params(), exml:element()) ->
    mongoose_c2s_hooks:result().
handle_stream_mgmt(Acc, Params = #{c2s_state := C2SState}, El = #xmlel{name = <<"a">>})
  when ?IS_ALLOWED_STATE(C2SState) ->
    handle_a(Acc, Params, El);
handle_stream_mgmt(Acc, Params = #{c2s_state := C2SState}, #xmlel{name = <<"r">>})
  when ?IS_ALLOWED_STATE(C2SState) ->
    handle_r(Acc, Params);
handle_stream_mgmt(Acc, Params = #{c2s_state := C2SState}, El = #xmlel{name = <<"enable">>})
  when ?IS_ALLOWED_STATE(C2SState) ->
    handle_enable(Acc, Params, El);
handle_stream_mgmt(Acc, Params = #{c2s_state := {wait_for_feature_after_auth, _}}, El = #xmlel{name = <<"resume">>}) ->
    handle_resume_request(Acc, Params, El);
handle_stream_mgmt(Acc, #{c2s_data := StateData, c2s_state := C2SState}, _El) ->
    unexpected_sm_request(Acc, StateData, C2SState);
handle_stream_mgmt(Acc, _Params, _El) ->
    {ok, Acc}.

-spec handle_r(mongoose_acc:t(), mongoose_c2s_hooks:params()) ->
    mongoose_c2s_hooks:result().
handle_r(Acc, #{c2s_data := StateData}) ->
    case get_mod_state(StateData) of
        {error, not_found} ->
            ?LOG_WARNING(#{what => unexpected_r, c2s_state => StateData,
                           text => <<"received <r/> but stream management is off!">>}),
            {ok, Acc};
        #sm_state{counter_in = In} ->
            Stanza = mod_stream_management_stanzas:stream_mgmt_ack(In),
            {ok, mongoose_c2s_acc:to_acc(Acc, socket_send, Stanza)}
    end.

-spec handle_a(mongoose_acc:t(), mongoose_c2s_hooks:params(), exml:element()) ->
    mongoose_c2s_hooks:result().
handle_a(Acc, #{c2s_data := StateData}, El) ->
    case {get_mod_state(StateData), stream_mgmt_parse_h(El)} of
        {{error, not_found}, _} ->
            {ok, Acc};
        {_, invalid_h_attribute} ->
            Stanza = mongoose_xmpp_errors:policy_violation(?MYLANG, <<"Invalid h attribute">>),
            mongoose_c2s:c2s_stream_error(StateData, Stanza),
            {stop, mongoose_c2s_acc:to_acc(Acc, hard_stop, invalid_h_attribute)};
        {Handler, H} ->
            HandledAck = do_handle_ack(Handler, H),
            finish_handle_a(StateData, Acc, HandledAck)
    end.

-spec finish_handle_a(mongoose_c2s:data(), mongoose_acc:t(), sm_state() | {error, term()}) ->
    mongoose_c2s_hooks:result().
finish_handle_a(StateData, Acc, {error, ErrorStanza, Reason}) ->
    mongoose_c2s:c2s_stream_error(StateData, ErrorStanza),
    {stop, mongoose_c2s_acc:to_acc(Acc, stop, Reason)};
finish_handle_a(_StateData, Acc, #sm_state{} = NewSmState) ->
    {ok, mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewSmState})}.

-spec do_handle_ack(sm_state(), non_neg_integer()) -> sm_state() | {error, exml:element(), term()}.
do_handle_ack(#sm_state{counter_out = OldAcked,
                        buffer_size = BufferSize,
                        buffer = Buffer} = SmState, Acked) ->
    ToDrop = calc_to_drop(Acked, OldAcked),
    case BufferSize < ToDrop of
        true ->
            ErrorStanza0 = #xmlel{children = Children}
                = mongoose_xmpp_errors:undefined_condition(
                    ?MYLANG, <<"You acknowledged more stanzas that what has been sent">>),
            HandledCountField = mod_stream_management_stanzas:sm_handled_count_too_high_stanza(Acked, OldAcked),
            ErrorStanza = ErrorStanza0#xmlel{children = [HandledCountField | Children]},
            {error, ErrorStanza, {shutdown, sm_handled_count_too_high_stanza}};
        false ->
            NewSize = BufferSize - ToDrop,
            {NewBuffer, _Dropped} = lists:split(NewSize, Buffer),
            SmState#sm_state{counter_out = Acked, buffer_size = NewSize, buffer = NewBuffer}
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

-spec handle_enable(mongoose_acc:t(), mongoose_c2s_hooks:params(), exml:element()) ->
    mongoose_c2s_hooks:result().
handle_enable(Acc, #{c2s_data := StateData}, El) ->
    case if_not_already_enabled_create_sm_state(StateData) of
        error ->
            stream_error(Acc, StateData);
        SmState ->
            do_handle_enable(Acc, StateData, SmState, El)
    end.

-spec do_handle_enable(mongoose_acc:t(), mongoose_c2s:data(), sm_state(), exml:element()) ->
    mongoose_c2s_hooks:result().
do_handle_enable(Acc, StateData, SmState, El) ->
    case exml_query:attr(El, <<"resume">>, false) of
        false ->
            Stanza = mod_stream_management_stanzas:stream_mgmt_enabled(),
            ToAcc = [{state_mod, {?MODULE, SmState}}, {socket_send, Stanza}],
            {stop, mongoose_c2s_acc:to_acc_many(Acc, ToAcc)};
        Attr when Attr =:= <<"true">>; Attr =:= <<"1">> ->
            Stanza = register_smid_return_enabled_stanza(StateData),
            ToAcc = [{state_mod, {?MODULE, SmState}}, {socket_send, Stanza}],
            {stop, mongoose_c2s_acc:to_acc_many(Acc, ToAcc)};
        _ ->
            stream_error(Acc, StateData)
    end.

-spec register_smid_return_enabled_stanza(mongoose_c2s:data()) -> exml:element().
register_smid_return_enabled_stanza(StateData) ->
    SMID = make_smid(),
    Sid = mongoose_c2s:get_sid(StateData),
    HostType = mongoose_c2s:get_host_type(StateData),
    ok = register_smid(HostType, SMID, Sid),
    mod_stream_management_stanzas:stream_mgmt_enabled([{<<"id">>, SMID}, {<<"resume">>, <<"true">>}]).

-spec if_not_already_enabled_create_sm_state(mongoose_c2s:data()) -> sm_state() | error.
if_not_already_enabled_create_sm_state(StateData) ->
    case get_mod_state(StateData) of
        #sm_state{} -> error;
        {error, not_found} ->
            HostType = mongoose_c2s:get_host_type(StateData),
            build_sm_handler(HostType)
    end.

-spec handle_resume_request(mongoose_acc:t(), mongoose_c2s_hooks:params(), exml:element()) ->
    mongoose_c2s_hooks:result().
handle_resume_request(Acc, #{c2s_state := C2SState, c2s_data := C2SData}, El) ->
    case handle_resume(C2SData, C2SState, El) of
        {stream_mgmt_error, ErrorStanza} ->
            stream_mgmt_error(Acc, C2SData, C2SState, ErrorStanza);
        {error, ErrorStanza, Reason} ->
            mongoose_c2s:c2s_stream_error(C2SData, ErrorStanza),
            {stop, mongoose_c2s_acc:to_acc(Acc, stop, Reason)};
        {error, ErrorStanza} ->
            {stop, mongoose_c2s_acc:to_acc(Acc, socket_send, ErrorStanza)};
        {ok, #{resumed := Resumed, forward := ToForward,
               c2s_state := C2SState1, c2s_data := C2SData1}} ->
            ToAcc = [{c2s_state, C2SState1}, {c2s_data, C2SData1}, {socket_send, [Resumed | ToForward]}],
            {ok, mongoose_c2s_acc:to_acc_many(Acc, ToAcc)}
    end.

%% This runs on the new process
-spec handle_resume(mongoose_c2s:data(), mongoose_c2s:state(), exml:element()) ->
    resume_return().
handle_resume(C2SData, C2SState, El) ->
    case {get_previd(El), stream_mgmt_parse_h(El), get_mod_state(C2SData)} of
        {undefined, _, _} ->
            {error, mod_stream_management_stanzas:stream_mgmt_failed(<<"bad-request">>)};
        {_, invalid_h_attribute, _} ->
            {error, mod_stream_management_stanzas:stream_mgmt_failed(<<"bad-request">>)};
        {_, _, #sm_state{}} ->
            {error, mod_stream_management_stanzas:stream_mgmt_failed(<<"bad-request">>)};
        {SMID, H, {error, not_found}} ->
            HostType = mongoose_c2s:get_host_type(C2SData),
            FromSMID = get_session_from_smid(HostType, SMID),
            do_handle_resume(C2SData, C2SState, SMID, H, FromSMID)
    end.

-spec do_handle_resume(StateData, C2SState, SMID, H, FromSMID) -> HookResult when
      StateData :: mongoose_c2s:data(),
      C2SState :: c2s_state(),
      SMID :: smid(),
      H :: non_neg_integer(),
      FromSMID :: maybe_smid(),
      HookResult :: resume_return().
do_handle_resume(StateData, _C2SState, SMID, H, {sid, {_TS, Pid}}) ->
    case get_peer_state(Pid, H) of
        {ok, OldStateData} ->
            NewState = mongoose_c2s:merge_states(OldStateData, StateData),
            do_resume(NewState, SMID);
        {error, ErrorStanza, Reason} ->
            {error, ErrorStanza, Reason};
        {exception, {C, R, S}} ->
            ?LOG_WARNING(#{what => resumption_error,
                           text => <<"Resumption error because of invalid response">>,
                           class => C, reason => R, stacktrace => S, pid => Pid, c2s_state => StateData}),
            {stream_mgmt_error, mod_stream_management_stanzas:stream_mgmt_failed(<<"item-not-found">>)}
    end;
do_handle_resume(StateData, _C2SState, SMID, _H, {stale_h, StaleH}) ->
    ?LOG_WARNING(#{what => resumption_error, reason => session_resumption_timed_out,
                   smid => SMID, stale_h => StaleH, c2s_state => StateData}),
    {stream_mgmt_error, mod_stream_management_stanzas:stream_mgmt_failed(<<"item-not-found">>, [{<<"h">>, integer_to_binary(StaleH)}])};
do_handle_resume(StateData, _C2SState, SMID, _H, {error, smid_not_found}) ->
    ?LOG_WARNING(#{what => resumption_error, reason => no_previous_session_for_smid,
                   smid => SMID, c2s_state => StateData}),
    {stream_mgmt_error, mod_stream_management_stanzas:stream_mgmt_failed(<<"item-not-found">>)}.

%% This runs on the new process
-spec do_resume(StateData, SMID) -> HookResult when
      StateData :: mongoose_c2s:data(),
      SMID :: smid(),
      HookResult :: resume_return().
do_resume(StateData, SMID) ->
    {_ReplacedPids, StateData2} = mongoose_c2s:open_session(StateData),
    ok = register_smid(StateData2, SMID),
    {Resumed, ToForward} = get_all_stanzas_to_forward(StateData2, SMID),
    {ok, #{resumed => Resumed, forward => ToForward,
           c2s_state => session_established, c2s_data => StateData2}}.

register_smid(StateData, SMID) ->
    Sid = mongoose_c2s:get_sid(StateData),
    HostType = mongoose_c2s:get_host_type(StateData),
    ok = register_smid(HostType, SMID, Sid).

-spec get_all_stanzas_to_forward(mongoose_c2s:data(), smid()) -> {exml:element(), [exml:element()]}.
get_all_stanzas_to_forward(StateData, SMID) ->
    #sm_state{counter_in = Counter, buffer = Buffer} = get_mod_state(StateData),
    Resumed = mod_stream_management_stanzas:stream_mgmt_resumed(SMID, Counter),
    LServer = mongoose_c2s:get_lserver(StateData),
    FromServer = jid:make_noprep(<<>>, LServer, <<>>),
    ToForward = [ begin
                      TS = mongoose_acc:timestamp(Acc),
                      Packet = mongoose_acc:element(Acc),
                      StanzaName = mongoose_acc:stanza_name(Acc),
                      StanzaType = mongoose_acc:stanza_type(Acc),
                      maybe_add_timestamp(Packet, StanzaName, StanzaType, TS, FromServer)
                  end || Acc <- lists:reverse(Buffer)],
    {Resumed, ToForward}.

maybe_add_timestamp(Packet, <<"message">>, <<"error">>, _, _) ->
    Packet;
maybe_add_timestamp(Packet, <<"message">>, <<"headline">>, _, _) ->
    Packet;
maybe_add_timestamp(Packet, <<"message">>, _, TS, FromServer) ->
    jlib:maybe_append_delay(Packet, FromServer, TS, <<"SM Storage">>);
maybe_add_timestamp(Packet, _StanzaName, _StanzaType, _TS, _FromServer) ->
    Packet.

%% If jid is the same, but sid is not, then we have a conflict.
%% jid example is alice@localhost/res1.
%% sid example is `{now(), pid()}'.
%% The conflict can happen, when actions with an accumulator were initiated by
%% one process but the resulting stanzas were routed to another process with
%% the same JID but different SID.
%% The conflict usually happens when a user is reconnecting.
%% Both origin_sid and origin_jid props should be defined.
%% But we don't force developers to set both of them, so we should correctly
%% process stanzas, that have only one properly set.
%% "Incoming" means that stanza is coming from ejabberd_router.
-spec is_conflict_incoming_acc(mongoose_acc:t(), mongoose_c2s:data()) -> boolean().
is_conflict_incoming_acc(Acc, StateData) ->
    OriginJid = mongoose_acc:get(c2s, origin_jid, undefined, Acc),
    OriginSid = mongoose_acc:get(c2s, origin_sid, undefined, Acc),
    AreOriginsDefined = OriginJid =/= undefined andalso OriginSid =/= undefined,
    case AreOriginsDefined of
        false ->
            false;
        true ->
            StateJid = mongoose_c2s:get_jid(StateData),
            SameJid = jid:are_equal(OriginJid, StateJid),
            StateSid = mongoose_c2s:get_sid(StateData),
            SameSid = OriginSid =:= StateSid,
            % possible to receive response addressed to process which we resumed from - still valid!
            OldSid = maybe_get_resumed_from_sid(get_mod_state(StateData)),
            SameOldSession = OriginSid =:= OldSid,
            SameJid andalso not (SameSid or SameOldSession)
    end.

-spec maybe_get_resumed_from_sid(maybe_sm_state()) -> undefined | ejabberd_sm:sid().
maybe_get_resumed_from_sid(#sm_state{peer = {sid, ResumedFrom}}) ->
    ResumedFrom;
maybe_get_resumed_from_sid(_) ->
    undefined.

-spec is_conflict_receiver_sid(mongoose_acc:t(), mongoose_c2s:data()) -> boolean().
is_conflict_receiver_sid(Acc, StateData) ->
    StateSid = mongoose_c2s:get_sid(StateData),
    AccSid = mongoose_acc:get(c2s, receiver_sid, StateSid, Acc),
    StateSid =/= AccSid.

-spec stream_error(mongoose_acc:t(), mongoose_c2s:data()) ->
    {stop, mongoose_acc:t()}.
stream_error(Acc, StateData) ->
    Err = mod_stream_management_stanzas:stream_mgmt_failed(<<"unexpected-request">>),
    mongoose_c2s:c2s_stream_error(StateData, Err),
    {stop, mongoose_c2s_acc:to_acc(Acc, stop, {shutdown, stream_error})}.

-spec unexpected_sm_request(mongoose_acc:t(), mongoose_c2s:data(), c2s_state()) ->
    {stop, mongoose_acc:t()}.
unexpected_sm_request(Acc, StateData, C2SState) ->
    Err = mod_stream_management_stanzas:stream_mgmt_failed(<<"unexpected-request">>),
    stream_mgmt_error(Acc, StateData, C2SState, Err).

-spec stream_mgmt_error(
        mongoose_acc:t(), mongoose_c2s:data(), c2s_state(), exml:element()) ->
    {stop, mongoose_acc:t()}.
stream_mgmt_error(Acc, _StateData, C2SState, Err) ->
    case mongoose_c2s:maybe_retry_state(C2SState) of
        {stop, {shutdown, retries}} ->
            {stop, mongoose_c2s_acc:to_acc(Acc, stop, {shutdown, retries})};
        NewC2SState ->
            ToAcc = [{c2s_state, NewC2SState}, {socket_send, Err}],
            {stop, mongoose_c2s_acc:to_acc_many(Acc, ToAcc)}
    end.

-spec build_sm_handler(mongooseim:host_type()) -> sm_state().
build_sm_handler(HostType) ->
    BufferMax = get_buffer_max(HostType),
    AckFreq = get_ack_freq(HostType),
    #sm_state{buffer_max = BufferMax, ack_freq = AckFreq}.

-spec get_mod_state(mongoose_c2s:data()) -> maybe_sm_state().
get_mod_state(StateData) ->
    case mongoose_c2s:get_mod_state(StateData, ?MODULE) of
        {ok, State} -> State;
        Error -> Error
    end.

-spec get_peer_state(pid(), non_neg_integer()) ->
    {ok, mongoose_c2s:data()} | {error, exml:element(), term()} | {exception, tuple()}.
get_peer_state(Pid, H) ->
    try
        mongoose_c2s:call(Pid, ?MODULE, {resume, H})
    catch
        C:R:S -> {exception, {C, R, S}}
    end.

-spec stream_mgmt_parse_h(exml:element()) -> invalid_h_attribute | non_neg_integer().
stream_mgmt_parse_h(El) ->
    case catch binary_to_integer(exml_query:attr(El, <<"h">>)) of
        H when is_integer(H), H >= 0 -> H;
        _ -> invalid_h_attribute
    end.

-spec get_previd(exml:element()) -> undefined | binary().
get_previd(El) ->
    exml_query:attr(El, <<"previd">>, undefined).

-spec c2s_stream_features(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: [exml:element()],
    Params :: map(),
    Extra :: gen_hook:extra().
c2s_stream_features(Acc, _, _) ->
    {ok, lists:keystore(<<"sm">>, #xmlel.name, Acc, mod_stream_management_stanzas:sm())}.

-spec session_cleanup(Acc, Params, Extra) -> {ok, Acc} when
    Acc :: mongoose_acc:t(),
    Params :: #{sid := ejabberd_sm:sid()},
    Extra :: gen_hook:extra().
session_cleanup(Acc, #{sid := SID}, #{host_type := HostType}) ->
    {ok, remove_smid(Acc, HostType, SID)}.


-spec remove_smid(mongoose_acc:t(), mongooseim:host_type(), ejabberd_sm:sid()) ->
    mongoose_acc:t().
remove_smid(Acc, HostType, Sid) ->
    H = mongoose_acc:get(stream_mgmt, h, undefined, Acc),
    MaybeSMID = do_remove_smid(HostType, Sid, H),
    mongoose_acc:set(stream_mgmt, smid, MaybeSMID, Acc).

do_remove_smid(HostType, Sid, H) ->
    MaybeSMID = unregister_smid(HostType, Sid),
    case MaybeSMID of
        {ok, SMID} when H =/= undefined ->
            register_stale_smid_h(HostType, SMID, H);
        _ ->
            ok
    end,
    MaybeSMID.

%%
%% `gen_statem' callbacks
%%

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    handle_event_function.

-spec init(term()) -> gen_statem:init_result(c2s_state(), mongoose_c2s:data()).
init(_) ->
    {stop, this_should_have_never_been_called}.

-spec handle_event(gen_statem:event_type(), term(), c2s_state(), mongoose_c2s:data()) ->
    gen_statem:event_handler_result(c2s_state()).
handle_event({call, From}, #{event_tag := ?MODULE, event_content := {resume, H}},
             ?EXT_C2S_STATE(resume_session), StateData) ->
    case handle_resume_call(StateData, From, H) of
        {ok, SmState} ->
            NewStateData = mongoose_c2s:merge_mod_state(StateData, #{?MODULE => SmState}),
            {stop, {shutdown, resumed}, NewStateData};
        error ->
            {stop, {shutdown, resumed}}
    end;
handle_event({timeout, ?MODULE}, resume_timeout, ?EXT_C2S_STATE(resume_session), StateData) ->
    {stop, {shutdown, ?MODULE}, StateData};
handle_event(EventType, EventContent, C2SState, StateData) ->
    mongoose_c2s:handle_event(EventType, EventContent, C2SState, StateData).

%% This runs on the old process
-spec handle_resume_call(
        mongoose_c2s:data(), gen_statem:from(), non_neg_integer()) ->
    {ok, sm_state()} | error.
handle_resume_call(StateData, From, H) ->
    case do_handle_ack(get_mod_state(StateData), H) of
        #sm_state{} = SmState1 ->
            KeepSmState = sm_state_to_keep(SmState1, From),
            mongoose_c2s:c2s_stream_error(StateData, mongoose_xmpp_errors:stream_conflict()),
            PassSmState = pipeline_future_sm_state(StateData, SmState1),
            pass_c2s_data_to_new_session(StateData, PassSmState, From),
            {ok, KeepSmState};
        HandledAck ->
            ReplyAction = {reply, From, HandledAck},
            gen_statem:reply(ReplyAction),
            error
    end.

-spec pipeline_future_sm_state(mongoose_c2s:data(), sm_state()) -> sm_state().
pipeline_future_sm_state(StateData, SmState) ->
    Sid = mongoose_c2s:get_sid(StateData),
    WithResumedFrom = SmState#sm_state{peer = {sid, Sid}},
    recover_messages(WithResumedFrom).

-spec pass_c2s_data_to_new_session(mongoose_c2s:data(), sm_state(), gen_statem:from()) -> ok.
pass_c2s_data_to_new_session(StateData, PassSmState, From) ->
    FutureStateData = mongoose_c2s:merge_mod_state(StateData, #{?MODULE => PassSmState}),
    ReplyAction = {reply, From, {ok, FutureStateData}},
    gen_statem:reply(ReplyAction).

%% The dying c2s will keep no buffer and will only know of the session that resumed him off,
%% to reroute him messages later
-spec sm_state_to_keep(sm_state(), gen_statem:from()) -> sm_state().
sm_state_to_keep(SmState, From) ->
    SmState#sm_state{buffer = [], buffer_size = 0, peer = {gen_statem, From}}.

%%
%% API for `mongoose_c2s'
%%

-spec recover_messages(sm_state()) -> sm_state().
recover_messages(SmState) ->
    receive
        {route, Acc} ->
            recover_messages(maybe_buffer_acc(SmState, Acc, is_message(mongoose_acc:stanza_name(Acc))))
    after 0 ->
              SmState
    end.

-spec maybe_buffer_acc(sm_state(), mongoose_acc:t(), boolean()) -> sm_state().
maybe_buffer_acc(#sm_state{buffer = Buffer, buffer_size = BufferSize} = SmState, Acc, true) ->
    SmState#sm_state{buffer = [Acc | Buffer], buffer_size = BufferSize + 1};
maybe_buffer_acc(SmState, _Acc, false) ->
    SmState.

%% IQs and presences are allowed to come to the same SID only
-spec is_message(binary()) -> boolean().
is_message(<<"message">>) -> true;
is_message(_) -> false.

-spec is_sm_element(mongoose_acc:t()) -> boolean().
is_sm_element(Acc) ->
    El = mongoose_acc:element(Acc),
    ?NS_STREAM_MGNT_3 =:= exml_query:attr(El, <<"xmlns">>).

-spec make_smid() -> smid().
make_smid() ->
    base64:encode(crypto:strong_rand_bytes(21)).

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
    timer:seconds(gen_mod:get_module_opt(HostType, ?MODULE, resume_timeout)).

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
