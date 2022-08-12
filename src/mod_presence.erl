-module(mod_presence).

-include_lib("exml/include/exml.hrl").
-include("mongoose_logger.hrl").
-include("mongoose_config_spec.hrl").

-behavior(gen_mod).

-type jid_set() :: gb_sets:set(jid:jid()).
-type priority() :: -128..128.
-record(presences, {
          %% We have _subscription to_ these users' presence status;
          %% i.e. they send us presence updates.
          %% This comes from the roster.
          pres_t = gb_sets:new() :: jid_set(),
          %% We have _subscription from_ these users,
          %% i.e. they have subscription to us.
          %% We send them presence updates.
          %% This comes from the roster, and also to ourself
          pres_f = gb_sets:new() :: jid_set(),
          %% We're _available_ to these users,
          %% i.e. we broadcast presence updates to them.
          %% This may change throughout the session.
          pres_a = gb_sets:new() :: jid_set(),
          %% We are _invisible_ to these users.
          %% This may change throughout the session.
          pres_i = gb_sets:new() :: jid_set(),
          pres_pri = 0 :: priority(),
          pres_last :: undefined | exml:element(),
          pres_timestamp :: undefined | integer(), % unix time in microseconds
          pres_invis = false :: boolean() %% Are we invisible?
         }).
-type presences() :: #presences{}.

-export([start/2, stop/1, config_spec/0, supported_features/0]).
-export([
         user_send_presence/3,
         user_received_presence/3,
         user_terminate/3,
         foreign_event/3
        ]).
-export([get/2]).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(HostType, _Opts) ->
    gen_hook:add_handlers(c2s_hooks(HostType)).

-spec stop(mongooseim:host_type()) -> ok.
stop(HostType) ->
    gen_hook:delete_handlers(c2s_hooks(HostType)).

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{}.

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec c2s_hooks(mongooseim:host_type()) -> gen_hook:hook_list(mongoose_c2s_hooks:hook_fn()).
c2s_hooks(HostType) ->
    [
     {user_send_presence, HostType, fun ?MODULE:user_send_presence/3, #{}, 50},
     {user_received_presence, HostType, fun ?MODULE:user_received_presence/3, #{}, 50},
     {user_terminate, HostType, fun ?MODULE:user_terminate/3, #{}, 90},
     {foreign_event, HostType, fun ?MODULE:foreign_event/3, #{}, 50}
    ].

-spec user_send_presence(mongoose_acc:t(), mongoose_c2s_hooks:hook_params(), map()) ->
    mongoose_c2s_hooks:hook_result().
user_send_presence(Acc, #{c2s_data := StateData}, _Extra) ->
    {FromJid, ToJid, Packet} = mongoose_acc:packet(Acc),
    StanzaType = mongoose_acc:stanza_type(Acc),
    case jid:are_bare_equal(FromJid, ToJid) of
        true ->
            Acc1 = presence_update(Acc, FromJid, ToJid, Packet, StateData, StanzaType),
            {ok, Acc1};
        _ ->
            Acc1 = presence_track(Acc, FromJid, ToJid, Packet, StateData, StanzaType),
            {ok, Acc1}
    end.

-spec user_received_presence(mongoose_acc:t(), mongoose_c2s_hooks:hook_params(), map()) ->
    mongoose_c2s_hooks:hook_result().
user_received_presence(Acc, #{c2s_data := StateData}, _Extra) ->
    case get_mod_state(StateData) of
        {error, not_found} ->
            {ok, Acc};
        Handler ->
            handle_user_received_presence(Acc, Handler, mongoose_acc:stanza_type(Acc))
    end.

-spec user_terminate(mongoose_acc:t(), mongoose_c2s:hook_params(), gen_hook:extra()) ->
    gen_hook:hook_fn_ret(mongoose_acc:t()).
user_terminate(Acc, #{c2s_data := StateData}, _Extra) ->
    case get_mod_state(StateData) of
        {error, not_found} -> {ok, Acc};
        #presences{pres_last = undefined} -> {ok, Acc};
        Handler -> handle_user_terminate(Acc, StateData, Handler)
    end.

-spec handle_user_terminate(mongoose_acc:t(), mongoose_c2s:c2s_data(), presences()) ->
    gen_hook:hook_fn_ret(mongoose_acc:t()).
handle_user_terminate(Acc, StateData, Handler) ->
    Jid = mongoose_c2s:get_jid(StateData),
    Reason = mongoose_acc:get(c2s, terminate, normal, Acc),
    Status = close_session_status(Reason),
    PresenceUnavailable = presence_unavailable_stanza(Status),
    ParamsAcc = #{from_jid => Jid, to_jid => jid:to_bare(Jid), element => PresenceUnavailable},
    Acc1 = mongoose_acc:update_stanza(ParamsAcc, Acc),
    presence_broadcast(Acc1, Handler#presences.pres_a),
    presence_broadcast(Acc1, Handler#presences.pres_i),
    mongoose_hooks:unset_presence_hook(Acc1, Jid, Status),
    {ok, Acc}.

-spec foreign_event(Acc, Params, Extra) -> Result when
      Acc :: mongoose_acc:t(),
      Params :: mongoose_c2s:hook_params(),
      Extra :: gen_hook:extra(),
      Result :: gen_hook:hook_fn_ret(mongoose_acc:t()).
foreign_event(Acc, #{c2s_data := StateData,
                     event_type := info,
                     event_content := {broadcast, {item, IJID, ISubscription}}}, _Extra) ->
    case get_mod_state(StateData) of
        {error, not_found} ->
            {ok, Acc};
        Handler ->
            {stop, handle_subscription_change(Acc, StateData, IJID, ISubscription, Handler)}
    end;
foreign_event(Acc, _Params, _Extra) ->
    {ok, Acc}.

-spec handle_subscription_change(mongoose_acc:t(), mongoose_c2s:c2s_data(), term(), term(), presences()) ->
    mongoose_acc:t().
handle_subscription_change(Acc, StateData, IJID, ISubscription, Handler) ->
    To = jid:make(IJID),
    IsSubscribedToMe = (ISubscription == both) or (ISubscription == from),
    AmISubscribedTo = (ISubscription == both) or (ISubscription == to),
    WasSubscribedToMe = gb_sets:is_element(To, Handler#presences.pres_f),
    FSet = case IsSubscribedToMe of
               true ->
                   gb_sets:add_element(To, Handler#presences.pres_f);
               false ->
                   gb_sets:del_element(To, Handler#presences.pres_f)
           end,
    TSet = case AmISubscribedTo of
               true ->
                   gb_sets:add_element(To, Handler#presences.pres_t);
               false ->
                   gb_sets:del_element(To, Handler#presences.pres_t)
           end,
    case Handler#presences.pres_last of
        undefined ->
            NewHandler = Handler#presences{pres_f = FSet, pres_t = TSet},
            mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewHandler});
        P ->
            ?LOG_DEBUG(#{what => roster_changed, roster_jid => To,
                         acc => Acc, c2s_state => StateData}),
            From = mongoose_c2s:get_jid(StateData),
            IsntInvisible = not Handler#presences.pres_invis,
            ImAvailableTo = gb_sets:is_element(To, Handler#presences.pres_a),
            ImInvisibleTo = gb_sets:is_element(To, Handler#presences.pres_i),
            BecomeAvailable = IsntInvisible and IsSubscribedToMe and not WasSubscribedToMe,
            BecomeUnavailable = not IsSubscribedToMe and WasSubscribedToMe
                                and (ImAvailableTo or ImInvisibleTo),
            case {BecomeAvailable, BecomeUnavailable} of
                {true, _} ->
                    ?LOG_DEBUG(#{what => become_available_to, roster_jid => To,
                                 acc => Acc, c2s_state => StateData}),
                    ejabberd_router:route(From, To, Acc, P),
                    A = gb_sets:add_element(To, Handler#presences.pres_a),
                    NewHandler = Handler#presences{pres_a = A, pres_f = FSet, pres_t = TSet},
                    mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewHandler});
                {_, true} ->
                    ?LOG_DEBUG(#{what => become_unavailable_to, roster_jid => To,
                                 acc => Acc, c2s_state => StateData}),
                    PU = presence_unavailable_stanza(),
                    ejabberd_router:route(From, To, Acc, PU),
                    I = gb_sets:del_element(To, Handler#presences.pres_i),
                    A = gb_sets:del_element(To, Handler#presences.pres_a),
                    NewHandler = Handler#presences{pres_i = I, pres_a = A, pres_f = FSet, pres_t = TSet},
                    mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewHandler});
                _ ->
                    NewHandler = Handler#presences{pres_f = FSet, pres_t = TSet},
                    mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewHandler})
            end
    end.

handle_user_received_presence(Acc, Handler, <<"probe">>) ->
    {stop, handle_received_probe(Acc, Handler)};
handle_user_received_presence(Acc, Handler, <<"error">>) ->
    {ok, handle_received_error(Acc, Handler)};
handle_user_received_presence(Acc, _, <<"invisible">>) ->
    {ok, handle_received_invisible(Acc)};
handle_user_received_presence(Acc, _, <<"subscribe">>) ->
    {ok, Acc};
handle_user_received_presence(Acc, _, <<"subscribed">>) ->
    {ok, Acc};
handle_user_received_presence(Acc, _, <<"unsubscribe">>) ->
    {ok, Acc};
handle_user_received_presence(Acc, _, <<"unsubscribed">>) ->
    {ok, Acc};
handle_user_received_presence(Acc, Handler, _) ->
    {ok, handled_received_available(Acc, Handler)}.

handle_received_probe(Acc, Handler) ->
    {FromJid, ToJid, _Packet} = mongoose_acc:packet(Acc),
    BareFromJid = jid:to_bare(FromJid),
    NewHandler = case am_i_available_to(FromJid, BareFromJid, Handler) of
                   true -> Handler;
                   false -> make_available_to(FromJid, BareFromJid, Handler)
               end,
    Acc1 = mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewHandler}),
    process_presence_probe(Acc1, NewHandler, FromJid, BareFromJid, ToJid).

process_presence_probe(Acc, #presences{pres_last = undefined}, _, _, _) ->
    Acc;
process_presence_probe(Acc, Handler, FromJid, BareFromJid, ToJid) ->
    case {should_retransmit_last_presence(FromJid, BareFromJid, Handler),
          specifically_visible_to(FromJid, Handler)} of
        {true, _} ->
            route_probe(Acc, Handler, FromJid, ToJid);
        {false, true} ->
            ejabberd_router:route(ToJid, FromJid, Acc, #xmlel{name = <<"presence">>}),
            Acc;
        _ ->
            Acc
    end.

route_probe(Acc, Handler, FromJid, ToJid) ->
    Packet0 = Handler#presences.pres_last,
    TS = Handler#presences.pres_timestamp,
    %% To is the one sending the presence (the target of the probe)
    Packet1 = jlib:maybe_append_delay(Packet0, ToJid, TS, <<>>),
    HostType = mongoose_acc:host_type(Acc),
    Acc2 = mongoose_hooks:presence_probe_hook(HostType, Acc, FromJid, ToJid, self()),
    %% Don't route a presence probe to oneself
    case jid:are_equal(FromJid, ToJid) of
        false ->
            ejabberd_router:route(ToJid, FromJid, Acc2, Packet1),
            Acc;
        true ->
            Acc2
    end.

handle_received_error(Acc, Handler) ->
    FromJid = mongoose_acc:from_jid(Acc),
    NewA = gb_sets:del_element(FromJid, Handler#presences.pres_a),
    NewHandler = Handler#presences{pres_a = NewA},
    Acc1 = mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewHandler}),
    Acc1.

handle_received_invisible(Acc) ->
    {FromJid, ToJid, Packet} = mongoose_acc:packet(Acc),
    #xmlel{attrs = Attrs} = Packet,
    Attrs1 = lists:keystore(<<"type">>, 1, Attrs, {<<"type">>, <<"unavailable">>}),
    NewElement = Packet#xmlel{attrs = Attrs1},
    Acc1 = mongoose_acc:update_stanza(
             #{element => NewElement, from_jid => FromJid, to_jid => ToJid}, Acc),
    Acc1.

handled_received_available(Acc, Handler) ->
    FromJid = mongoose_acc:from_jid(Acc),
    BareJid = jid:to_bare(FromJid),
    case am_i_available_to(FromJid, BareJid, Handler) of
        true ->
            Acc;
        false ->
            NewHandler = make_available_to(FromJid, BareJid, Handler),
            mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewHandler})
    end.

%% @doc User updates his presence (non-directed presence packet)
-spec presence_update(
        mongoose_acc:t(), jid:jid(), jid:jid(), exml:element(), mongoose_c2s:c2s_data(), undefined | binary()) ->
    mongoose_acc:t().
presence_update(Acc, FromJid, ToJid, Packet, StateData, undefined) ->
    Handler = maybe_get_handler(StateData),
    presence_update_to_available(Acc, FromJid, ToJid, Packet, StateData, Handler);
presence_update(Acc, FromJid, ToJid, Packet, StateData, <<"unavailable">>) ->
    Handler = maybe_get_handler(StateData),
    presence_update_to_unavailable(Acc, FromJid, ToJid, Packet, StateData, Handler);
presence_update(Acc, FromJid, ToJid, Packet, StateData, <<"invisible">>) ->
    Handler = maybe_get_handler(StateData),
    presence_update_to_invisible(Acc, FromJid, ToJid, Packet, StateData, Handler);
presence_update(Acc, _, _, _, _, <<"error">>) -> Acc;
presence_update(Acc, _, _, _, _, <<"probe">>) -> Acc;
presence_update(Acc, _, _, _, _, <<"subscribe">>) -> Acc;
presence_update(Acc, _, _, _, _, <<"subscribed">>) -> Acc;
presence_update(Acc, _, _, _, _, <<"unsubscribe">>) -> Acc;
presence_update(Acc, _, _, _, _, <<"unsubscribed">>) -> Acc.

-spec presence_update_to_available(
        mongoose_acc:t(), jid:jid(), jid:jid(), exml:element(), mongoose_c2s:c2s_data(), presences()) ->
    mongoose_acc:t().
presence_update_to_available(Acc0, FromJid, ToJid, Packet, StateData, Handler) ->
    Jid = mongoose_c2s:get_jid(StateData),
    HostType = mongoose_c2s:get_host_type(StateData),
    Acc1 = mongoose_hooks:roster_get_subscription_lists(HostType, Acc0, Jid),
    {Fs0, Ts0, Pending} = mongoose_acc:get(roster, subscription_lists, {[], [], []}, Acc1),
    Fs = [jid:make(BJ) || BJ <- Fs0],
    Ts = [jid:make(BJ) || BJ <- Ts0],
    OldPriority = get_old_priority(Handler),
    NewPriority = get_priority_from_presence(Packet),
    Timestamp = erlang:system_time(microsecond),
    Acc2 = update_priority(Acc1, NewPriority, Packet, StateData),
    FromUnavail = (Handler#presences.pres_last == undefined) or Handler#presences.pres_invis,
    ?LOG_DEBUG(#{what => presence_update_to_available,
                 text => <<"Presence changes from unavailable to available">>,
                 from_unavail => FromUnavail, acc => Acc2, c2s_state => StateData}),
    BareJid = jid:to_bare(Jid),
    NewHandler = Handler#presences{pres_f = gb_sets:from_list([BareJid | Fs]),
                                   pres_t = gb_sets:from_list([BareJid | Ts]),
                                   pres_pri = NewPriority,
                                   pres_last = Packet,
                                   pres_timestamp = Timestamp,
                                   pres_invis = false},
    presence_update_to_available(
      Acc2, FromJid, ToJid, Packet, StateData, NewHandler, Pending,
      OldPriority, NewPriority, FromUnavail).

presence_update_to_unavailable(Acc, _FromJid, _ToJid, Packet, StateData, Handler) ->
    Status = exml_query:path(Packet, [{element, <<"status">>}, cdata], <<>>),
    Sid = mongoose_c2s:get_sid(StateData),
    Jid = mongoose_c2s:get_jid(StateData),
    Acc1 = ejabberd_sm:unset_presence(Acc, Sid, Jid, Status, #{}),
    presence_broadcast(Acc1, Handler#presences.pres_a),
    presence_broadcast(Acc1, Handler#presences.pres_i),
    NewHandler = Handler#presences{pres_last = undefined,
                                   pres_timestamp = undefined,
                                   pres_a = gb_sets:new(),
                                   pres_i = gb_sets:new(),
                                   pres_invis = false},
    mongoose_c2s_acc:to_acc(Acc1, state_mod, {?MODULE, NewHandler}).

presence_update_to_invisible(Acc, FromJid, _ToJid, Packet, StateData, Handler) ->
    NewPriority = get_priority_from_presence(Packet),
    Acc1 = update_priority(Acc, NewPriority, Packet, StateData),
    case Handler#presences.pres_invis of
        false ->
            presence_broadcast(Acc1, Handler#presences.pres_a),
            presence_broadcast(Acc1, Handler#presences.pres_i),
            NewHandler = Handler#presences{pres_last = undefined,
                                           pres_timestamp = undefined,
                                           pres_a = gb_sets:new(),
                                           pres_i = gb_sets:new(),
                                           pres_invis = true},
            presence_broadcast_first(Acc1, FromJid, Packet, NewHandler, []);
        true ->
            Acc1
    end.

%% @doc User sends a directed presence packet
-spec presence_track(
        mongoose_acc:t(), jid:jid(), jid:jid(), exml:element(), mongoose_c2s:c2s_data(), undefined | binary()) ->
    mongoose_acc:t().
presence_track(Acc, FromJid, ToJid, Packet, StateData, undefined) ->
    Handler = maybe_get_handler(StateData),
    process_presence_track_available(Acc, FromJid, ToJid, Packet, Handler);
presence_track(Acc, FromJid, ToJid, Packet, StateData, <<"unavailable">>) ->
    Handler = maybe_get_handler(StateData),
    process_presence_track_unavailable(Acc, FromJid, ToJid, Packet, Handler);
presence_track(Acc, FromJid, ToJid, Packet, StateData, <<"invisible">>) ->
    Handler = maybe_get_handler(StateData),
    process_presence_track_invisible(Acc, FromJid, ToJid, Packet, Handler);
presence_track(Acc, FromJid, ToJid, Packet, _, <<"subscribe">>) ->
    process_presence_track_subscription_and_route(Acc, FromJid, ToJid, Packet, subscribe);
presence_track(Acc, FromJid, ToJid, Packet, _, <<"subscribed">>) ->
    process_presence_track_subscription_and_route(Acc, FromJid, ToJid, Packet, subscribed);
presence_track(Acc, FromJid, ToJid, Packet, _, <<"unsubscribe">>) ->
    process_presence_track_subscription_and_route(Acc, FromJid, ToJid, Packet, unsubscribe);
presence_track(Acc, FromJid, ToJid, Packet, _, <<"unsubscribed">>) ->
    process_presence_track_subscription_and_route(Acc, FromJid, ToJid, Packet, unsubscribed);
presence_track(Acc, FromJid, ToJid, Packet, _, <<"error">>) ->
    ejabberd_router:route(ToJid, FromJid, Acc, Packet),
    Acc;
presence_track(Acc, FromJid, ToJid, Packet, _, <<"probe">>) ->
    ejabberd_router:route(ToJid, FromJid, Acc, Packet),
    Acc.

process_presence_track_available(Acc, FromJid, ToJid, Packet, Handler) ->
    ejabberd_router:route(FromJid, ToJid, Acc, Packet),
    I = gb_sets:del_element(ToJid, Handler#presences.pres_i),
    A = gb_sets:add_element(ToJid, Handler#presences.pres_a),
    NewHandler = Handler#presences{pres_i = I, pres_a = A},
    mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewHandler}).

process_presence_track_unavailable(Acc, FromJid, ToJid, Packet, Handler) ->
    ejabberd_router:route(FromJid, ToJid, Acc, Packet),
    I = gb_sets:del_element(ToJid, Handler#presences.pres_i),
    A = gb_sets:del_element(ToJid, Handler#presences.pres_a),
    NewHandler = Handler#presences{pres_i = I, pres_a = A},
    mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewHandler}).

process_presence_track_invisible(Acc, FromJid, ToJid, Packet, Handler) ->
    ejabberd_router:route(FromJid, ToJid, Acc, Packet),
    I = gb_sets:add_element(ToJid, Handler#presences.pres_i),
    A = gb_sets:del_element(ToJid, Handler#presences.pres_a),
    NewHandler = Handler#presences{pres_i = I, pres_a = A},
    mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewHandler}).

process_presence_track_subscription_and_route(Acc, FromJid, ToJid, Packet, Type) ->
    Acc1 = mongoose_hooks:roster_out_subscription(Acc, FromJid, ToJid, Type),
    ejabberd_router:route(jid:to_bare(FromJid), ToJid, Acc1, Packet),
    Acc1.

-spec presence_broadcast(mongoose_acc:t(), jid_set()) -> mongoose_acc:t().
presence_broadcast(Acc, JIDSet) ->
    From = mongoose_acc:from_jid(Acc),
    gb_sets:fold(fun(JID, A) ->
                         ejabberd_router:route(From, JID, A)
                 end, Acc, JIDSet).

-spec presence_update_to_available(
        mongoose_acc:t(), jid:jid(), jid:jid(), exml:element(), mongoose_c2s:c2s_data(),
        presences(), list(), priority(), priority(), boolean()) ->
    mongoose_acc:t().
presence_update_to_available(Acc, FromJid, _ToJid, Packet, StateData, Handler, SocketSend,
                             _OldPriority, NewPriority, true) ->
    Acc1 = mongoose_hooks:user_available_hook(Acc, FromJid),
    Acc2 = case NewPriority >= 0 of
              true ->
                  resend_offline_messages(Acc1, StateData);
              false ->
                  Acc1
              end,
    presence_broadcast_first(Acc2, FromJid, Packet, Handler, SocketSend);
presence_update_to_available(Acc, FromJid, _ToJid, Packet, StateData, Handler, Pending,
                             OldPriority, NewPriority, false) ->
    presence_broadcast_to_trusted(
             Acc, FromJid, Handler#presences.pres_f, Handler#presences.pres_a, Packet),
    Acc1 = case OldPriority < 0 andalso NewPriority >= 0 of
               true ->
                   resend_offline_messages(Acc, StateData);
               false ->
                   Acc
           end,
    ToAcc = [{socket_send, Pending}, {state_mod, {?MODULE, Handler}}],
    mongoose_c2s_acc:to_acc_many(Acc1, ToAcc).

-spec presence_broadcast_to_trusted(
        mongoose_acc:t(), jid:jid(), jid_set(), jid_set(), exml:element()) ->
    mongoose_acc:t().
presence_broadcast_to_trusted(Acc, FromJid, T, A, Packet) ->
    gb_sets:fold(
      fun(JID, Ac) ->
              case gb_sets:is_element(JID, T) of
                  true ->
                      ejabberd_router:route(FromJid, JID, Ac, Packet);
                  _ ->
                      Ac
              end
      end, Acc, A).

-spec resend_offline_messages(mongoose_acc:t(), mongoose_c2s:c2s_data()) -> mongoose_acc:t().
resend_offline_messages(Acc, StateData) ->
    ?LOG_DEBUG(#{what => resend_offline_messages, acc => Acc, c2s_state => StateData}),
    Jid = mongoose_c2s:get_jid(StateData),
    Acc1 = mongoose_hooks:resend_offline_messages_hook(Acc, Jid),
    Rs = mongoose_acc:get(offline, messages, [], Acc1),
    Acc2 = lists:foldl(fun({route, FromJid, ToJid, MsgAcc}, A) ->
                           resend_offline_message(A, FromJid, ToJid, MsgAcc, in);
                          ({route, MsgAcc}, A) ->
                               {FromJid, ToJid, _} = mongoose_acc:packet(Acc),
                               resend_offline_message(A, FromJid, ToJid, MsgAcc, in)
                       end, Acc1, Rs),
    mongoose_acc:delete(offline, messages, Acc2). % they are gone from db backend and sent

resend_offline_message(Acc0, FromJid, To, Acc, in) ->
    Packet = mongoose_acc:element(Acc),
    NewAcc = strip_c2s_fields(Acc),
    ejabberd_router:route(FromJid, To, NewAcc, Packet),
    Acc0.

-spec strip_c2s_fields(mongoose_acc:t()) -> mongoose_acc:t().
strip_c2s_fields(Acc) ->
    %% TODO: verify if we really need to strip down these 2 fields
    mongoose_acc:delete_many(c2s, [origin_jid, origin_sid], Acc).

-spec presence_broadcast_first(
        mongoose_acc:t(), jid:jid(), exml:element(), presences(), [exml:element()]) ->
    mongoose_acc:t().
presence_broadcast_first(Acc0, FromJid, Packet, Handler, SocketSend) ->
    Probe = presence_probe(),
    _Acc1 = gb_sets:fold(fun(JID, Accum) ->
                                ejabberd_router:route(FromJid, JID, Accum, Probe)
                         end, Acc0, Handler#presences.pres_t),
    case Handler#presences.pres_invis of
        true ->
            mongoose_c2s_acc:to_acc(Acc0, state_mod, {?MODULE, Handler});
        false ->
            {As, _Acc2} = gb_sets:fold(
                               fun(JID, {A, Accum}) ->
                                       Accum1 = ejabberd_router:route(FromJid, JID, Accum, Packet),
                                       {gb_sets:add_element(JID, A), Accum1}
                               end,
                               {Handler#presences.pres_a, Acc0}, Handler#presences.pres_f),
            NewHandler = Handler#presences{pres_a = As},
            ToAcc = [{socket_send, SocketSend}, {state_mod, {?MODULE, NewHandler}}],
            mongoose_c2s_acc:to_acc_many(Acc0, ToAcc)
    end.

-spec presence_probe() -> exml:element().
presence_probe() ->
    #xmlel{name = <<"presence">>,
           attrs = [{<<"type">>, <<"probe">>}]}.

-spec presence_unavailable_stanza() -> exml:element().
presence_unavailable_stanza() ->
    presence_unavailable_stanza(<<>>).

-spec presence_unavailable_stanza(binary()) -> exml:element().
presence_unavailable_stanza(<<>>) ->
    #xmlel{name = <<"presence">>,
           attrs = [{<<"type">>, <<"unavailable">>}]};
presence_unavailable_stanza(Status) ->
    StatusEl = #xmlel{name = <<"status">>,
                      children = [#xmlcdata{content = Status}]},
    #xmlel{name = <<"presence">>,
           attrs = [{<<"type">>, <<"unavailable">>}],
           children = [StatusEl]}.

close_session_status(normal) ->
    <<>>;
close_session_status({shutdown, retries}) ->
    <<"Too many attempts">>;
close_session_status({shutdown, replaced}) ->
    <<"Replaced by new connection">>;
close_session_status(_) ->
    <<"Unknown condition">>.

-spec maybe_get_handler(mongoose_c2s:c2s_data()) -> presences().
maybe_get_handler(StateData) ->
    case mongoose_c2s:get_mod_state(StateData, ?MODULE) of
        #presences{} = Handler -> Handler;
        {error, not_found} -> #presences{}
    end.

-spec get_mod_state(mongoose_c2s:c2s_data()) -> presences() | {error, not_found}.
get_mod_state(StateData) ->
    mongoose_c2s:get_mod_state(StateData, ?MODULE).

-spec get_priority_from_presence(exml:element()) -> priority().
get_priority_from_presence(PresencePacket) ->
    MaybePriority = exml_query:path(PresencePacket, [{element, <<"priority">>}, cdata], undefined),
    case catch binary_to_integer(MaybePriority) of
        P when is_integer(P), -128 =< P, P =< 128 -> P;
        _ -> 0
    end.

-spec get_old_priority(presences()) -> priority().
get_old_priority(Handler) ->
    case Handler#presences.pres_last of
        undefined -> 0;
        OldPresence -> get_priority_from_presence(OldPresence)
    end.

-spec update_priority(Acc :: mongoose_acc:t(),
                      Priority :: integer(),
                      Packet :: exml:element(),
                      StateData :: mongoose_c2s:c2s_data()) -> mongoose_acc:t().
update_priority(Acc, Priority, Packet, StateData) ->
    Sid = mongoose_c2s:get_sid(StateData),
    Jid = mongoose_c2s:get_jid(StateData),
    ejabberd_sm:set_presence(Acc, Sid, Jid, Priority, Packet, #{}).

-spec am_i_available_to(jid:jid(), jid:jid(), presences()) -> boolean().
am_i_available_to(FromJid, BareJid, Handler) ->
    gb_sets:is_element(FromJid, Handler#presences.pres_a)
    orelse (FromJid /= BareJid)
    andalso gb_sets:is_element(BareJid, Handler#presences.pres_a).

-spec make_available_to(jid:jid(), jid:jid(), presences()) -> presences().
make_available_to(FromJid, BareJid, Handler) ->
    case gb_sets:is_element(FromJid, Handler#presences.pres_f) of
        true ->
            A = gb_sets:add_element(FromJid, Handler#presences.pres_a),
            Handler#presences{pres_a = A};
        false ->
            case gb_sets:is_element(BareJid, Handler#presences.pres_f) of
                true ->
                    A = gb_sets:add_element(BareJid, Handler#presences.pres_a),
                    Handler#presences{pres_a = A};
                false ->
                    Handler
            end
    end.

-spec should_retransmit_last_presence(jid:jid(), jid:jid(), presences()) -> boolean().
should_retransmit_last_presence(FromJid, BareFromJid,
                                #presences{pres_invis = Invisible} = Handler) ->
    not Invisible
    andalso is_subscribed_to_my_presence(FromJid, BareFromJid, Handler)
    andalso not invisible_to(FromJid, BareFromJid, Handler).

-spec is_subscribed_to_my_presence(jid:jid(), jid:jid(), presences()) -> boolean().
is_subscribed_to_my_presence(FromJid, BareFromJid, Handler) ->
    gb_sets:is_element(FromJid, Handler#presences.pres_f)
    orelse (FromJid /= BareFromJid)
    andalso gb_sets:is_element(BareFromJid, Handler#presences.pres_f).

-spec invisible_to(jid:jid(), jid:jid(), presences()) -> boolean().
invisible_to(FromJid, BareFromJid, Handler) ->
    gb_sets:is_element(FromJid, Handler#presences.pres_i)
    orelse (FromJid /= BareFromJid)
    andalso gb_sets:is_element(BareFromJid, Handler#presences.pres_i).

-spec specifically_visible_to(jid:jid(), presences()) -> boolean().
specifically_visible_to(FromJid, #presences{pres_invis = Invisible} = Handler) ->
    Invisible
    andalso gb_sets:is_element(FromJid, Handler#presences.pres_f)
    andalso gb_sets:is_element(FromJid, Handler#presences.pres_a).

-spec get(presences(), s_to) -> jid_set();
         (presences(), s_from) -> jid_set();
         (presences(), s_available) -> jid_set();
         (presences(), s_invisible) -> jid_set();
         (presences(), priority) -> priority();
         (presences(), last) -> undefined | exml:element();
         (presences(), timestamp) -> undefined | integer();
         (presences(), invisible) -> boolean().
get(#presences{pres_t = Value}, s_to) -> Value;
get(#presences{pres_f = Value}, s_from) -> Value;
get(#presences{pres_a = Value}, s_available) -> Value;
get(#presences{pres_i = Value}, s_invisible) -> Value;
get(#presences{pres_pri = Value}, priority) -> Value;
get(#presences{pres_last = Value}, last) -> Value;
get(#presences{pres_timestamp = Value}, timestamp) -> Value;
get(#presences{pres_invis = Value}, invisible) -> Value.
