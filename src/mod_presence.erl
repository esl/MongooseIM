-module(mod_presence).

-include("jlib.hrl").
-include("mongoose_logger.hrl").

-behavior(gen_mod).

-type priority() :: -128..127.
-type maybe_priority() :: priority() | undefined.

-type subscription() :: from | to | both.
-type subscriptions() :: #{jid:jid() := subscription()}.

-type status() :: available | invisible.
-type statuses() :: #{jid:jid() := status()}.

-record(presences_state, {
          subscriptions = #{} :: subscriptions(),
          statuses = #{} :: statuses(),
          pres_pri = 0 :: priority(),
          pres_last :: undefined | exml:element(),
          pres_timestamp :: undefined | integer(), % unix time in microseconds
          pres_invis = false :: boolean() %% Are we invisible?
         }).
-type state() :: #presences_state{}.

-export([start/2, stop/1, hooks/1, supported_features/0]).
-export([
         user_send_presence/3,
         user_receive_presence/3,
         user_terminate/3,
         foreign_event/3
        ]).
-export([
         get/2,
         is_subscribed_to_my_presence/3,
         am_i_subscribed_to_presence/3,
         presence_unavailable_stanza/0,
         get_presence/1,
         get_subscribed/1,
         set_presence/2,
         maybe_get_handler/1,
         get_old_priority/1
        ]).

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> ok.
start(_HostType, _Opts) ->
    ok.

-spec stop(mongooseim:host_type()) -> ok.
stop(_HostType) ->
    ok.

-spec hooks(mongooseim:host_type()) -> gen_hook:hook_list(mongoose_c2s_hooks:fn()).
hooks(HostType) ->
    [
     {user_send_presence, HostType, fun ?MODULE:user_send_presence/3, #{}, 50},
     {user_receive_presence, HostType, fun ?MODULE:user_receive_presence/3, #{}, 50},
     {user_terminate, HostType, fun ?MODULE:user_terminate/3, #{}, 90},
     {foreign_event, HostType, fun ?MODULE:foreign_event/3, #{}, 50}
    ].

-spec supported_features() -> [atom()].
supported_features() ->
    [dynamic_domains].

-spec get_presence(pid()) -> {jid:luser(), jid:lresource(), binary(), binary()}.
get_presence(Pid) ->
    mongoose_c2s:call(Pid, ?MODULE, get_presence).

-spec get_subscribed(pid()) -> [jid:jid()].
get_subscribed(Pid) ->
    mongoose_c2s:call(Pid, ?MODULE, get_subscribed).

-spec set_presence(pid(), exml:element()) -> ok.
set_presence(Pid, Message) ->
    mongoose_c2s:cast(Pid, ?MODULE, {set_presence, Message}).

-spec user_send_presence(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
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

-spec user_receive_presence(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    mongoose_c2s_hooks:result().
user_receive_presence(Acc, #{c2s_data := StateData}, _Extra) ->
    case get_mod_state(StateData) of
        {error, not_found} ->
            {stop, Acc};
        Presences ->
            handle_user_received_presence(Acc, Presences, mongoose_acc:stanza_type(Acc))
    end.

-spec user_terminate(mongoose_acc:t(), mongoose_c2s_hooks:params(), gen_hook:extra()) ->
    gen_hook:hook_fn_ret(mongoose_acc:t()).
user_terminate(Acc, #{c2s_data := StateData, reason := Reason}, _Extra) ->
    case get_mod_state(StateData) of
        {error, not_found} -> {ok, Acc};
        #presences_state{pres_last = undefined} -> {ok, Acc};
        Presences -> handle_user_terminate(Acc, StateData, Presences, Reason)
    end.

-spec handle_user_terminate(mongoose_acc:t(), mongoose_c2s:data(), state(), term()) ->
    gen_hook:hook_fn_ret(mongoose_acc:t()).
handle_user_terminate(Acc, StateData, Presences, Reason) ->
    Jid = mongoose_c2s:get_jid(StateData),
    Status = close_session_status(Reason),
    PresenceUnavailable = presence_unavailable_stanza(Status),
    ParamsAcc = #{from_jid => Jid, to_jid => jid:to_bare(Jid), element => PresenceUnavailable},
    Acc1 = mongoose_acc:update_stanza(ParamsAcc, Acc),
    presence_broadcast(Acc1, Presences#presences_state.statuses),
    mongoose_hooks:unset_presence_hook(Acc1, Jid, Status),
    {ok, Acc}.

-spec foreign_event(Acc, Params, Extra) -> Result when
      Acc :: mongoose_acc:t(),
      Params :: mongoose_c2s_hooks:params(),
      Extra :: gen_hook:extra(),
      Result :: mongoose_c2s_hooks:result().
foreign_event(Acc, #{c2s_data := StateData,
                     event_type := cast,
                     event_tag := mod_roster,
                     event_content := {item, IJID, ISubscription}}, _Extra) ->
    case get_mod_state(StateData) of
        {error, not_found} ->
            {ok, Acc};
        Presences ->
            {stop, handle_subscription_change(Acc, StateData, IJID, ISubscription, Presences)}
    end;
foreign_event(Acc, #{c2s_data := StateData,
                     event_type := {call, From},
                     event_tag := ?MODULE,
                     event_content := get_presence}, _Extra) ->
    PresLast = case get_mod_state(StateData) of
                 {error, not_found} ->
                     undefined;
                 #presences_state{pres_last = Value} ->
                     Value
               end,
    #jid{luser = User, lresource = Resource} = mongoose_c2s:get_jid(StateData),
    Reply = {User, Resource, get_showtag(PresLast), get_statustag(PresLast)},
    Acc1 = mongoose_c2s_acc:to_acc(Acc, actions, [{reply, From, Reply}]),
    {stop, Acc1};
foreign_event(Acc, #{c2s_data := StateData,
                     event_type := {call, From},
                     event_tag := ?MODULE,
                     event_content := get_subscribed}, _Extra) ->
    Subscribed = case get_mod_state(StateData) of
                     {error, not_found} ->
                         [];
                     Presences ->
                         PresF = get_by_sub(Presences, from),
                         maps:keys(PresF)
                 end,
    Acc1 = mongoose_c2s_acc:to_acc(Acc, actions, [{reply, From, Subscribed}]),
    {stop, Acc1};
foreign_event(Acc, #{c2s_data := StateData,
                     event_type := cast,
                     event_tag := ?MODULE,
                     event_content := {set_presence, Message}}, _Extra) ->
    Acc1 = mongoose_acc:update_stanza(#{element => Message}, Acc),
    {FromJid, ToJid, Packet} = mongoose_acc:packet(Acc1),
    StanzaType = mongoose_acc:stanza_type(Acc1),
    {stop, presence_update(Acc1, FromJid, ToJid, Packet, StateData, StanzaType)};
foreign_event(Acc, _Params, _Extra) ->
    {ok, Acc}.

-spec get_showtag(undefined | exml:element()) -> binary().
get_showtag(undefined) ->
    <<"unavailable">>;
get_showtag(Presence) ->
    case xml:get_path_s(Presence, [{elem, <<"show">>}, cdata]) of
        <<>> -> <<"available">>;
        ShowTag -> ShowTag
    end.

-spec get_statustag(undefined | exml:element()) -> binary().

get_statustag(undefined) ->
    <<>>;
get_statustag(Presence) ->
    xml:get_path_s(Presence, [{elem, <<"status">>}, cdata]).

-spec handle_subscription_change(mongoose_acc:t(), mongoose_c2s:data(), term(), term(), state()) ->
    mongoose_acc:t().
handle_subscription_change(Acc, StateData, IJID, ISubscription, Presences) ->
    To = jid:make(IJID),
    IsSubscribedToMe = (ISubscription =:= both) or (ISubscription =:= from),
    AmISubscribedTo = (ISubscription =:= both) or (ISubscription =:= to),
    WasSubscribedToMe = is_subscribed(Presences, To, from),
    Subs = case {IsSubscribedToMe, AmISubscribedTo} of
               {true, true} ->
                   maps:put(To, both, Presences#presences_state.subscriptions);
               {true, _} ->
                   maps:put(To, from, Presences#presences_state.subscriptions);
               {_, true} ->
                   maps:put(To, to, Presences#presences_state.subscriptions);
               {false, false} ->
                   maps:remove(To, Presences#presences_state.subscriptions)
           end,
    case Presences#presences_state.pres_last of
        undefined ->
            NewPresences = Presences#presences_state{subscriptions = Subs},
            mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewPresences});
        P ->
            ?LOG_DEBUG(#{what => roster_changed, roster_jid => To,
                         acc => Acc, c2s_state => StateData}),
            From = mongoose_c2s:get_jid(StateData),
            IsntInvisible = not Presences#presences_state.pres_invis,
            ImAvailableTo = is_status(Presences, To, available),
            ImInvisibleTo = is_status(Presences, To, invisible),
            BecomeAvailable = IsntInvisible and IsSubscribedToMe and not WasSubscribedToMe,
            BecomeUnavailable = not IsSubscribedToMe and WasSubscribedToMe
                                and (ImAvailableTo or ImInvisibleTo),
            case {BecomeAvailable, BecomeUnavailable} of
                {true, _} ->
                    ?LOG_DEBUG(#{what => become_available_to, roster_jid => To,
                                 acc => Acc, c2s_state => StateData}),
                    ejabberd_router:route(From, To, Acc, P),
                    Statuses = maps:put(To, available, Presences#presences_state.statuses),
                    NewPresences = Presences#presences_state{subscriptions = Subs,
                                                             statuses = Statuses},
                    mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewPresences});
                {_, true} ->
                    ?LOG_DEBUG(#{what => become_unavailable_to, roster_jid => To,
                                 acc => Acc, c2s_state => StateData}),
                    PU = presence_unavailable_stanza(),
                    ejabberd_router:route(From, To, Acc, PU),
                    Statuses = maps:remove(To, Presences#presences_state.statuses),
                    NewPresences = Presences#presences_state{subscriptions = Subs,
                                                             statuses = Statuses},
                    mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewPresences});
                _ ->
                    NewPresences = Presences#presences_state{subscriptions = Subs},
                    mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewPresences})
            end
    end.

-spec handle_user_received_presence(mongoose_acc:t(), state(), binary()) ->
    mongoose_c2s_hooks:result().
handle_user_received_presence(Acc, Presences, <<"probe">>) ->
    {stop, handle_received_probe(Acc, Presences)};
handle_user_received_presence(Acc, Presences, <<"error">>) ->
    {ok, handle_received_error(Acc, Presences)};
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
handle_user_received_presence(Acc, Presences, _) ->
    {ok, handle_received_available(Acc, Presences)}.

-spec handle_received_probe(mongoose_acc:t(), state()) -> mongoose_acc:t().
handle_received_probe(Acc, Presences) ->
    {FromJid, ToJid, _Packet} = mongoose_acc:packet(Acc),
    BareFromJid = jid:to_bare(FromJid),
    NewPresences = case am_i_available_to(FromJid, BareFromJid, Presences) of
                   true -> Presences;
                   false -> make_available_to(FromJid, BareFromJid, Presences)
               end,
    Acc1 = mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewPresences}),
    process_presence_probe(Acc1, NewPresences, FromJid, BareFromJid, ToJid).

-spec process_presence_probe(mongoose_acc:t(), state(), jid:jid(), jid:jid(), jid:jid()) -> mongoose_acc:t().
process_presence_probe(Acc, #presences_state{pres_last = undefined}, _, _, _) ->
    Acc;
process_presence_probe(Acc, Presences, FromJid, BareFromJid, ToJid) ->
    case {should_retransmit_last_presence(FromJid, BareFromJid, Presences),
          specifically_visible_to(FromJid, Presences)} of
        {true, _} ->
            route_probe(Acc, Presences, FromJid, ToJid);
        {false, true} ->
            ejabberd_router:route(ToJid, FromJid, Acc, #xmlel{name = <<"presence">>}),
            Acc;
        _ ->
            Acc
    end.

-spec route_probe(mongoose_acc:t(), state(), jid:jid(), jid:jid()) -> mongoose_acc:t().
route_probe(Acc, Presences, FromJid, ToJid) ->
    Packet0 = Presences#presences_state.pres_last,
    TS = Presences#presences_state.pres_timestamp,
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

-spec handle_received_error(mongoose_acc:t(), state()) -> mongoose_acc:t().
handle_received_error(Acc, Presences) ->
    FromJid = mongoose_acc:from_jid(Acc),
    NewS = maps:remove(FromJid, Presences#presences_state.statuses),
    NewPresences = Presences#presences_state{statuses = NewS},
    mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewPresences}).

-spec handle_received_invisible(mongoose_acc:t()) -> mongoose_acc:t().
handle_received_invisible(Acc) ->
    {FromJid, ToJid, Packet} = mongoose_acc:packet(Acc),
    #xmlel{attrs = Attrs} = Packet,
    Attrs1 = lists:keystore(<<"type">>, 1, Attrs, {<<"type">>, <<"unavailable">>}),
    NewElement = Packet#xmlel{attrs = Attrs1},
    mongoose_acc:update_stanza(
      #{element => NewElement, from_jid => FromJid, to_jid => ToJid}, Acc).

-spec handle_received_available(mongoose_acc:t(), state()) -> mongoose_acc:t().
handle_received_available(Acc, Presences) ->
    FromJid = mongoose_acc:from_jid(Acc),
    BareJid = jid:to_bare(FromJid),
    case am_i_available_to(FromJid, BareJid, Presences) of
        true ->
            Acc;
        false ->
            NewPresences = make_available_to(FromJid, BareJid, Presences),
            mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewPresences})
    end.

%% @doc User updates his presence (non-directed presence packet)
-spec presence_update(
        mongoose_acc:t(), jid:jid(), jid:jid(), exml:element(), mongoose_c2s:data(), undefined | binary()) ->
    mongoose_acc:t().
presence_update(Acc, FromJid, ToJid, Packet, StateData, Available)
  when Available =:= undefined; Available =:= <<"available">> ->
    Presences = maybe_get_handler(StateData),
    presence_update_to_available(Acc, FromJid, ToJid, Packet, StateData, Presences);
presence_update(Acc, FromJid, ToJid, Packet, StateData, <<"unavailable">>) ->
    Presences = maybe_get_handler(StateData),
    presence_update_to_unavailable(Acc, FromJid, ToJid, Packet, StateData, Presences);
presence_update(Acc, FromJid, ToJid, Packet, StateData, <<"invisible">>) ->
    Presences = maybe_get_handler(StateData),
    presence_update_to_invisible(Acc, FromJid, ToJid, Packet, StateData, Presences);
presence_update(Acc, _, _, _, _, <<"error">>) -> Acc;
presence_update(Acc, _, _, _, _, <<"probe">>) -> Acc;
presence_update(Acc, _, _, _, _, <<"subscribe">>) -> Acc;
presence_update(Acc, _, _, _, _, <<"subscribed">>) -> Acc;
presence_update(Acc, _, _, _, _, <<"unsubscribe">>) -> Acc;
presence_update(Acc, _, _, _, _, <<"unsubscribed">>) -> Acc.

-spec presence_update_to_available(
        mongoose_acc:t(), jid:jid(), jid:jid(), exml:element(), mongoose_c2s:data(), state()) ->
    mongoose_acc:t().
presence_update_to_available(Acc0, FromJid, ToJid, Packet, StateData, Presences) ->
    Jid = mongoose_c2s:get_jid(StateData),
    HostType = mongoose_c2s:get_host_type(StateData),
    Acc1 = mongoose_hooks:roster_get_subscription_lists(HostType, Acc0, Jid),
    {Subs, Pending} = build_subs_and_pendings(Acc1, Jid),
    OldPriority = get_old_priority(Presences),
    NewPriority = get_priority_from_presence(Packet),
    Acc2 = update_priority(Acc1, NewPriority, Packet, StateData),
    FromUnavail = (Presences#presences_state.pres_last == undefined) orelse (Presences#presences_state.pres_invis),
    ?LOG_DEBUG(#{what => presence_update_to_available,
                 text => <<"Presence changes from unavailable to available">>,
                 from_unavail => FromUnavail, acc => Acc2, c2s_state => StateData}),
    NewPresences = Presences#presences_state{subscriptions = Subs,
                                             pres_pri = NewPriority,
                                             pres_last = Packet,
                                             pres_timestamp = erlang:system_time(microsecond),
                                             pres_invis = false},
    presence_update_to_available(
      Acc2, FromJid, ToJid, Packet, StateData, NewPresences, Pending,
      OldPriority, NewPriority, FromUnavail).

-spec presence_update_to_unavailable(
        mongoose_acc:t(), jid:jid(), jid:jid(), exml:element(), mongoose_c2s:data(), state()) ->
    mongoose_acc:t().
presence_update_to_unavailable(Acc, _FromJid, _ToJid, Packet, StateData, Presences) ->
    Status = exml_query:path(Packet, [{element, <<"status">>}, cdata], <<>>),
    Sid = mongoose_c2s:get_sid(StateData),
    Jid = mongoose_c2s:get_jid(StateData),
    Info = mongoose_c2s:get_info(StateData),
    Acc1 = ejabberd_sm:unset_presence(Acc, Sid, Jid, Status, Info),
    presence_broadcast(Acc1, Presences#presences_state.statuses),
    NewPresences = Presences#presences_state{pres_last = undefined,
                                   pres_timestamp = undefined,
                                   pres_invis = false},
    mongoose_c2s_acc:to_acc(Acc1, state_mod, {?MODULE, NewPresences}).

-spec presence_update_to_invisible(
        mongoose_acc:t(), jid:jid(), jid:jid(), exml:element(), mongoose_c2s:data(), state()) ->
    mongoose_acc:t().
presence_update_to_invisible(Acc, FromJid, _ToJid, Packet, StateData, Presences) ->
    NewPriority = get_priority_from_presence(Packet),
    Acc1 = update_priority(Acc, NewPriority, Packet, StateData),
    case Presences#presences_state.pres_invis of
        false ->
            presence_broadcast(Acc1, Presences#presences_state.statuses),
            NewPresences = Presences#presences_state{pres_last = undefined,
                                           pres_timestamp = undefined,
                                           pres_invis = true},
            presence_broadcast_first(Acc1, FromJid, Packet, NewPresences, []);
        true ->
            Acc1
    end.

%% @doc User sends a directed presence packet
-spec presence_track(
        mongoose_acc:t(), jid:jid(), jid:jid(), exml:element(), mongoose_c2s:data(), undefined | binary()) ->
    mongoose_acc:t().
presence_track(Acc, FromJid, ToJid, Packet, StateData, undefined) ->
    Presences = maybe_get_handler(StateData),
    process_presence_track_available(Acc, FromJid, ToJid, Packet, Presences);
presence_track(Acc, FromJid, ToJid, Packet, StateData, <<"unavailable">>) ->
    Presences = maybe_get_handler(StateData),
    process_presence_track_unavailable(Acc, FromJid, ToJid, Packet, Presences);
presence_track(Acc, FromJid, ToJid, Packet, StateData, <<"invisible">>) ->
    Presences = maybe_get_handler(StateData),
    process_presence_track_invisible(Acc, FromJid, ToJid, Packet, Presences);
presence_track(Acc, FromJid, ToJid, Packet, _, <<"subscribe">>) ->
    process_presence_track_subscription_and_route(Acc, FromJid, ToJid, Packet, subscribe);
presence_track(Acc, FromJid, ToJid, Packet, _, <<"subscribed">>) ->
    process_presence_track_subscription_and_route(Acc, FromJid, ToJid, Packet, subscribed);
presence_track(Acc, FromJid, ToJid, Packet, _, <<"unsubscribe">>) ->
    process_presence_track_subscription_and_route(Acc, FromJid, ToJid, Packet, unsubscribe);
presence_track(Acc, FromJid, ToJid, Packet, _, <<"unsubscribed">>) ->
    process_presence_track_subscription_and_route(Acc, FromJid, ToJid, Packet, unsubscribed);
presence_track(Acc, FromJid, ToJid, Packet, _, <<"error">>) ->
    ejabberd_router:route(FromJid, ToJid, Acc, Packet),
    Acc;
presence_track(Acc, FromJid, ToJid, Packet, _, <<"probe">>) ->
    ejabberd_router:route(FromJid, ToJid, Acc, Packet),
    Acc.

process_presence_track_available(Acc, FromJid, ToJid, Packet, Presences) ->
    ejabberd_router:route(FromJid, ToJid, Acc, Packet),
    Statuses = maps:put(ToJid, available, Presences#presences_state.statuses),
    NewPresences = Presences#presences_state{statuses = Statuses},
    mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewPresences}).

process_presence_track_unavailable(Acc, FromJid, ToJid, Packet, Presences) ->
    ejabberd_router:route(FromJid, ToJid, Acc, Packet),
    Statuses = maps:remove(ToJid, Presences#presences_state.statuses),
    NewPresences = Presences#presences_state{statuses = Statuses},
    mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewPresences}).

process_presence_track_invisible(Acc, FromJid, ToJid, Packet, Presences) ->
    ejabberd_router:route(FromJid, ToJid, Acc, Packet),
    Statuses = maps:put(ToJid, invisible, Presences#presences_state.statuses),
    NewPresences = Presences#presences_state{statuses = Statuses},
    mongoose_c2s_acc:to_acc(Acc, state_mod, {?MODULE, NewPresences}).

process_presence_track_subscription_and_route(Acc, FromJid, ToJid, Packet, Type) ->
    Acc1 = mongoose_hooks:roster_out_subscription(Acc, FromJid, ToJid, Type),
    ejabberd_router:route(jid:to_bare(FromJid), ToJid, Acc1, Packet),
    Acc1.

-spec presence_broadcast(mongoose_acc:t(), statuses()) -> ok.
presence_broadcast(Acc, Statuses) ->
    From = mongoose_acc:from_jid(Acc),
    Route = fun(JID, _) -> ejabberd_router:route(From, JID, Acc) end,
    maps:foreach(Route, Statuses).

-spec presence_update_to_available(
        mongoose_acc:t(), jid:jid(), jid:jid(), exml:element(), mongoose_c2s:data(),
        state(), [exml:element()], maybe_priority(), priority(), boolean()) ->
    mongoose_acc:t().
presence_update_to_available(Acc, FromJid, _ToJid, Packet, StateData, Presences, Pending,
                             _OldPriority, NewPriority, true) ->
    Acc1 = mongoose_hooks:user_available_hook(Acc, FromJid),
    Acc2 = case NewPriority >= 0 of
              true ->
                  resend_offline_messages(Acc1, StateData);
              false ->
                  Acc1
              end,
    presence_broadcast_first(Acc2, FromJid, Packet, Presences, Pending);
presence_update_to_available(Acc, FromJid, _ToJid, Packet, StateData, Presences, Pending,
                             OldPriority, NewPriority, false) ->
    presence_broadcast_to_trusted(
             Acc, FromJid, Presences, Packet),
    Acc1 = case (OldPriority < 0 orelse OldPriority =:= undefined) andalso NewPriority >= 0 of
               true ->
                   resend_offline_messages(Acc, StateData);
               false ->
                   Acc
           end,
    Accs = create_route_accs(Acc1, FromJid, Pending),
    ToAcc = [{route, Accs}, {state_mod, {?MODULE, Presences}}],
    mongoose_c2s_acc:to_acc_many(Acc1, ToAcc).

-spec presence_broadcast_to_trusted(mongoose_acc:t(), jid:jid(), state(), exml:element()) -> ok.
presence_broadcast_to_trusted(Acc, FromJid, Presences, Packet) ->
    maps:foreach(
      fun(JID, Status) ->
              case available =:= Status andalso is_subscribed(Presences, JID, from) of
                  true ->
                      ejabberd_router:route(FromJid, JID, Acc, Packet);
                  _ ->
                      Acc
              end
      end, Presences#presences_state.statuses).

-spec resend_offline_messages(mongoose_acc:t(), mongoose_c2s:data()) -> mongoose_acc:t().
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
        mongoose_acc:t(), jid:jid(), exml:element(), state(), [exml:element()]) ->
    mongoose_acc:t().
presence_broadcast_first(Acc0, FromJid, Packet, Presences, Pending) ->
    broadcast_probe(Acc0, FromJid, Presences),
    case Presences#presences_state.pres_invis of
        true ->
            mongoose_c2s_acc:to_acc(Acc0, state_mod, {?MODULE, Presences});
        false ->
            Ss = maps:fold(
                   fun(JID, Sub, S) ->
                           case Sub of
                               _ when both =:= Sub; from =:= Sub ->
                                   ejabberd_router:route(FromJid, JID, Acc0, Packet),
                                   maps:put(JID, available, S);
                               _ ->
                                   S
                           end
                   end,
                   Presences#presences_state.statuses,
                   Presences#presences_state.subscriptions),
            NewPresences = Presences#presences_state{statuses = Ss},
            Accs = create_route_accs(Acc0, FromJid, Pending),
            ToAcc = [{route, Accs}, {state_mod, {?MODULE, NewPresences}}],
            mongoose_c2s_acc:to_acc_many(Acc0, ToAcc)
    end.

-spec broadcast_probe(mongoose_acc:t(), jid:jid(), state()) -> ok.
broadcast_probe(Acc, FromJid, Presences) ->
    Probe = presence_probe(),
    Fun = fun(ToJid, Sub) when both =:= Sub; to =:= Sub ->
                  ejabberd_router:route(FromJid, ToJid, Acc, Probe);
             (_, _) ->
                  ok
          end,
    maps:foreach(Fun, Presences#presences_state.subscriptions).

-spec create_route_accs(mongoose_acc:t(), jid:jid(), [exml:element()]) -> [mongoose_acc:t()].
create_route_accs(Acc0, To, List) when is_list(List) ->
    [ mongoose_acc:update_stanza(#{to_jid => To, element => P}, Acc0) || P <- List ].

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
close_session_status({shutdown, Reason}) when is_atom(Reason) ->
    <<"Shutdown by reason: ", (atom_to_binary(Reason))/binary>>;
close_session_status({shutdown, Reason}) when is_binary(Reason) ->
    <<"Shutdown by reason: ", Reason/binary>>;
close_session_status(_) ->
    <<"Unknown condition">>.

-spec maybe_get_handler(mongoose_c2s:data()) -> state().
maybe_get_handler(StateData) ->
    case mongoose_c2s:get_mod_state(StateData, ?MODULE) of
        {ok, #presences_state{} = Presences} -> Presences;
        {error, not_found} -> #presences_state{}
    end.

-spec get_mod_state(mongoose_c2s:data()) -> state() | {error, not_found}.
get_mod_state(StateData) ->
    case mongoose_c2s:get_mod_state(StateData, ?MODULE) of
        {ok, Presence} -> Presence;
        Error -> Error
    end.

-spec get_priority_from_presence(exml:element()) -> priority().
get_priority_from_presence(PresencePacket) ->
    MaybePriority = exml_query:path(PresencePacket, [{element, <<"priority">>}, cdata], undefined),
    case catch binary_to_integer(MaybePriority) of
        P when is_integer(P), -128 =< P, P =< 127 -> P;
        _ -> 0
    end.

-spec get_old_priority(state()) -> maybe_priority().
get_old_priority(Presences) ->
    case Presences#presences_state.pres_last of
        undefined -> undefined;
        OldPresence -> get_priority_from_presence(OldPresence)
    end.

-spec build_subs_and_pendings(mongoose_acc:t(), jid:jid()) -> {subscriptions(), [exml:element()]}.
build_subs_and_pendings(Acc1, Jid) ->
    {Fs0, Ts0, Pending} = mongoose_acc:get(roster, subscription_lists, {[], [], []}, Acc1),
    BareJid = jid:to_bare(Jid),
    Fs = maps:from_list([{jid:make(BJ), from} || BJ <- Fs0]),
    Ts = maps:from_list([{jid:make(BJ), to} || BJ <- Ts0]),
    Subs = maps:merge_with(fun(_, _, _) -> both end, Fs, Ts),
    {Subs#{BareJid => both}, Pending}.

-spec update_priority(Acc :: mongoose_acc:t(),
                      Priority :: integer(),
                      Packet :: exml:element(),
                      StateData :: mongoose_c2s:data()) -> mongoose_acc:t().
update_priority(Acc, Priority, Packet, StateData) ->
    Sid = mongoose_c2s:get_sid(StateData),
    Jid = mongoose_c2s:get_jid(StateData),
    Info = mongoose_c2s:get_info(StateData),
    ejabberd_sm:set_presence(Acc, Sid, Jid, Priority, Packet, Info).

-spec am_i_subscribed_to_presence(jid:jid(), jid:jid(), state()) -> boolean().
am_i_subscribed_to_presence(LJID, LBareJID, S) ->
    is_subscribed(S, LJID, to)
    orelse (LJID /= LBareJID)
    andalso is_subscribed(S, LBareJID, to).

-spec am_i_available_to(jid:jid(), jid:jid(), state()) -> boolean().
am_i_available_to(FromJid, BareJid, Presences) ->
    is_status(Presences, FromJid, available)
    orelse (FromJid /= BareJid)
    andalso is_status(Presences, BareJid, available).

-spec make_available_to(jid:jid(), jid:jid(), state()) -> state().
make_available_to(FromJid, BareJid, Presences) ->
    case is_subscribed(Presences, FromJid, from) of
        true ->
            S = maps:put(FromJid, available, Presences#presences_state.statuses),
            Presences#presences_state{statuses = S};
        false ->
            case is_subscribed(Presences, BareJid, from) of
                true ->
                    S = maps:put(FromJid, available, Presences#presences_state.statuses),
                    Presences#presences_state{statuses = S};
                false ->
                    Presences
            end
    end.

-spec should_retransmit_last_presence(jid:jid(), jid:jid(), state()) -> boolean().
should_retransmit_last_presence(FromJid, BareFromJid,
                                #presences_state{pres_invis = Invisible} = Presences) ->
    not Invisible
    andalso is_subscribed_to_my_presence(FromJid, BareFromJid, Presences)
    andalso not invisible_to(FromJid, BareFromJid, Presences).

-spec is_subscribed_to_my_presence(jid:jid(), jid:jid(), state()) -> boolean().
is_subscribed_to_my_presence(FromJid, BareFromJid, Presences) ->
    is_subscribed(Presences, FromJid, from)
    orelse (FromJid /= BareFromJid)
    andalso is_subscribed(Presences, BareFromJid, from).

-spec invisible_to(jid:jid(), jid:jid(), state()) -> boolean().
invisible_to(FromJid, BareFromJid, Presences) ->
    is_status(Presences, FromJid, invisible)
    orelse (FromJid /= BareFromJid)
    andalso is_status(Presences, BareFromJid, invisible).

-spec specifically_visible_to(jid:jid(), state()) -> boolean().
specifically_visible_to(FromJid, #presences_state{pres_invis = Invisible} = Presences) ->
    Invisible
    andalso is_subscribed(Presences, FromJid, from)
    andalso is_status(Presences, FromJid, available).

-spec is_status(state(), jid:jid(), status()) -> boolean().
is_status(#presences_state{statuses = Statuses}, Jid, DesiredStatus) ->
    DesiredStatus =:= maps:get(Jid, Statuses, '$imposible_status').

-spec is_subscribed(state(), jid:jid(), subscription()) -> boolean().
is_subscribed(#presences_state{subscriptions = Subs}, Jid, DesiredSub) ->
    Sub = maps:get(Jid, Subs, '$imposible_status'),
    both =:= Sub orelse DesiredSub =:= Sub.

-spec get_by_status(state(), status()) -> statuses().
get_by_status(#presences_state{statuses = Statuses}, DesiredStatus) ->
    Filter = fun(_, Status) -> DesiredStatus =:= Status end,
    maps:filter(Filter, Statuses).

-spec get_by_sub(state(), subscription()) -> subscriptions().
get_by_sub(#presences_state{subscriptions = Subs}, DesiredStatus) ->
    Filter = fun(_, Status) -> both =:= Status orelse DesiredStatus =:= Status end,
    maps:filter(Filter, Subs).

-spec get(state(), s_to) -> subscriptions();
         (state(), s_from) -> subscriptions();
         (state(), s_available) -> statuses();
         (state(), s_invisible) -> statuses();
         (state(), priority) -> priority();
         (state(), last) -> undefined | exml:element();
         (state(), timestamp) -> undefined | integer();
         (state(), invisible) -> boolean().
get(P, s_to) -> get_by_sub(P, to);
get(P, s_from) -> get_by_sub(P, from);
get(P, s_available) -> get_by_status(P, available);
get(P, s_invisible) -> get_by_status(P, invisible);
get(#presences_state{pres_pri = Value}, priority) -> Value;
get(#presences_state{pres_last = Value}, last) -> Value;
get(#presences_state{pres_timestamp = Value}, timestamp) -> Value;
get(#presences_state{pres_invis = Value}, invisible) -> Value.
