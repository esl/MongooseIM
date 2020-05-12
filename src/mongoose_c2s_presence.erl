%% @doc
%% This module handles part of c2s state, namely related to online presence.
%% It keeps track of which of our contacts is online, away etc, our own online status, and
%% provides API for ejabberd_c2s to retrieve or modify these information.
%% It also takes care of distributing roster iq push and presence updates upon roster change -
%% when roster is changed, mod_roster calls this module in every active session of the
%% owner of the roster.
%% @end

-module(mongoose_c2s_presence).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_roster.hrl").

%% API
-record(roster_state, {
    %% We have _subscription to_ these users' presence status;
    %% i.e. they send us presence updates.
    %% This comes from the roster.
    pres_t = gb_sets:new() :: jid_set() | debug_presences(),
    %% We have _subscription from_ these users,
    %% i.e. they have subscription to us.
    %% We send them presence updates.
    %% This comes from the roster.
    pres_f = gb_sets:new() :: jid_set() | debug_presences(),
    %% We're _available_ to these users,
    %% i.e. we broadcast presence updates to them.
    %% This may change throughout the session.
    pres_a = gb_sets:new() :: jid_set() | debug_presences(),
    %% We are _invisible_ to these users.
    %% This may change throughout the session.
    pres_i = gb_sets:new() :: jid_set() | debug_presences(),
    %% Are we invisible?
    pres_invis = false :: boolean(),
    pres_timestamp :: calendar:datetime() | undefined,
    pres_last
}).

-type roster_state() :: #roster_state{}.
-type roster() :: #roster{}.

-type jid_set() :: gb_sets:set(jid:simple_jid()).

-type debug_presences() :: {atom(), non_neg_integer()}.

-export([initialise_state/2,
         call/3,
         roster_change/3,
         am_i_subscribed_to_presence/3,
         is_subscribed_to_my_presence/2,
         is_subscribed_to_my_presence/3,
         am_i_available_to/3,
         make_available_to/3,
         invisible_to/3,
         is_invisible/1,
         specifically_visible_to/2,
         set_presence_state/3,
         has_active_presence/1,
         is_present_or_invisible/1,
         del_active_presence/3,
         empty_presence/3,
         pack_state/3,
         get_priority/1,
         get_last_presence/1,
         presence_update_to_available/3,
         is_unavailable/1,
         get_presence_timestamp/1,
         get_subscriptions/2]).


-export([handle_remote_hook/4]).

handle_remote_hook(HandlerState, mod_roster, {roster_change, Item}, C2SState) ->
    mongoose_c2s_presence:roster_change(Item, HandlerState, C2SState);
handle_remote_hook(HandlerState, _, _, _) ->
    HandlerState.

%%--------------------------------------------------------------------
%% for c2s
%%--------------------------------------------------------------------

%% this should be used to call a handler module func which modifies handler state
%% it is just a convenient wrapper
call(FunctionName, Args, StateData) ->
    HandlerState = ejabberd_c2s_state:get_handler_state(mod_roster, StateData),
    case ?MODULE:FunctionName(Args, HandlerState, StateData) of
        {new_state, NewHandlerState} ->
            ejabberd_c2s_state:set_handler_state(mod_roster, NewHandlerState, StateData);
        Result ->
            Result
    end.

initialise_state(FromSubs, ToSubs) ->
    #roster_state{pres_f = gb_sets:from_list(FromSubs),
                  pres_t = gb_sets:from_list(ToSubs)
    }.

am_i_subscribed_to_presence(LJID, LBareJID, State) ->
    S = ejabberd_c2s_state:get_handler_state(mod_roster, State),
    gb_sets:is_element(LJID, S#roster_state.pres_t)
        orelse (LJID /= LBareJID)
        andalso gb_sets:is_element(LBareJID, S#roster_state.pres_t).

is_subscribed_to_my_presence(JID, State) ->
    {Lowcase, Bare} = lowcase_and_bare(JID),
    is_subscribed_to_my_presence(Lowcase, Bare, State).

is_subscribed_to_my_presence(LFrom, LBareFrom, State) ->
    S = ejabberd_c2s_state:get_handler_state(mod_roster, State),
    gb_sets:is_element(LFrom, S#roster_state.pres_f)
        orelse (LFrom /= LBareFrom)
        andalso gb_sets:is_element(LBareFrom, S#roster_state.pres_f).

am_i_available_to(LFrom, LBFrom, State) ->
    S = ejabberd_c2s_state:get_handler_state(mod_roster, State),
    gb_sets:is_element(LFrom, S#roster_state.pres_a)
        orelse (LFrom /= LBFrom)
        andalso gb_sets:is_element(LBFrom, S#roster_state.pres_a).

make_available_to({LFrom, LBFrom}, State, _C2SState) ->
    NewState = case gb_sets:is_element(LFrom, State#roster_state.pres_f) of
                   true ->
                       A = gb_sets:add_element(LFrom, State#roster_state.pres_a),
                       State#roster_state{pres_a = A};
                   false ->
                       case gb_sets:is_element(LBFrom, State#roster_state.pres_f) of
                           true ->
                               A = gb_sets:add_element(LBFrom, State#roster_state.pres_a),
                               State#roster_state{pres_a = A};
                           false ->
                               State
                       end
               end,
    {new_state, NewState}.

del_active_presence(From, State, _C2SState) ->
    NewA = gb_sets:del_element(jid:to_lower(From), State#roster_state.pres_a),
    {new_state, State#roster_state{pres_a = NewA}}.

empty_presence(Invisible, State, _C2SState) ->
    {new_state, State#roster_state{pres_last = undefined,
                                   pres_timestamp = undefined,
                                   pres_a = gb_sets:new(),
                                   pres_i = gb_sets:new(),
                                   pres_invis = Invisible}}.

has_active_presence(State) ->
    EmptySet = gb_sets:new(),
    StateData = ejabberd_c2s_state:get_handler_state(mod_roster, State),
    case StateData of
        #roster_state{pres_last = undefined,
                      pres_a = EmptySet,
                      pres_i = EmptySet,
                      pres_invis = false} -> false;
        _ -> true
    end.

invisible_to(LFrom, LBareFrom, State) ->
    S = ejabberd_c2s_state:get_handler_state(mod_roster, State),
    gb_sets:is_element(LFrom, S#roster_state.pres_i)
        orelse (LFrom /= LBareFrom)
        andalso gb_sets:is_element(LBareFrom, S#roster_state.pres_i).

is_invisible(State) ->
    #roster_state{pres_invis = Invisible} = ejabberd_c2s_state:get_handler_state(mod_roster, State),
    Invisible.

%% @doc Is generally invisible, but visible to a particular resource?
specifically_visible_to(LFrom,  State) ->
    #roster_state{pres_invis = Invisible} = S = ejabberd_c2s_state:get_handler_state(mod_roster, State),
    Invisible
        andalso gb_sets:is_element(LFrom, S#roster_state.pres_f)
        andalso gb_sets:is_element(LFrom, S#roster_state.pres_a).

set_presence_state({unavailable, LTo}, StateData, _C2SState) ->
    I = gb_sets:del_element(LTo, StateData#roster_state.pres_i),
    A = gb_sets:del_element(LTo, StateData#roster_state.pres_a),
    {new_state, StateData#roster_state{pres_i = I, pres_a = A}};
set_presence_state({invisible, LTo}, StateData, _C2SState) ->
    I = gb_sets:add_element(LTo, StateData#roster_state.pres_i),
    A = gb_sets:del_element(LTo, StateData#roster_state.pres_a),
    {new_state, StateData#roster_state{pres_i = I, pres_a = A}};
set_presence_state({available, LTo}, StateData, _C2SState) ->
    I = gb_sets:del_element(LTo, StateData#roster_state.pres_i),
    A = gb_sets:add_element(LTo, StateData#roster_state.pres_a),
    {new_state, StateData#roster_state{pres_i = I, pres_a = A}}.

is_present_or_invisible(StateData) ->
    S = ejabberd_c2s_state:get_handler_state(mod_roster, StateData),
    case S#roster_state.pres_last of
        undefined ->
            S#roster_state.pres_invis;
        _ ->
            true
    end.

lowcase_and_bare(JID) ->
    LJID = jid:to_lower(JID),
    {LJID, jid:to_bare(LJID)}.

get_subscriptions(to, #roster_state{pres_t = T}) -> T;
get_subscriptions(from, #roster_state{pres_f = T}) -> T;
get_subscriptions(available, #roster_state{pres_a = T}) -> T;
get_subscriptions(invisible, #roster_state{pres_i = T}) -> T;
get_subscriptions(last, #roster_state{pres_last = T}) -> T;
get_subscriptions(trusted, S) ->
    #roster_state{pres_f = F, pres_a = A} = ejabberd_c2s_state:get_handler_state(mod_roster, S),
    lists:filter(fun(JID) -> gb_sets:is_element(JID, F) end,
                 gb_sets:to_list(A));
get_subscriptions(from_except_self, S) ->
    JID = ejabberd_c2s_state:jid(S),
    SelfJID = jid:to_lower(jid:to_bare(JID)),
    Froms = get_subscriptions(from, ejabberd_c2s_state:get_handler_state(mod_roster, S)),
    gb_sets:to_list(gb_sets:del_element(SelfJID, Froms));
get_subscriptions(Which, S) ->
    gb_sets:to_list(
        get_subscriptions(Which,
                          ejabberd_c2s_state:get_handler_state(mod_roster, S))).

get_last_presence(State) ->
    S = ejabberd_c2s_state:get_handler_state(mod_roster, State),
    S#roster_state.pres_last.

get_presence_timestamp(State) ->
    S = ejabberd_c2s_state:get_handler_state(mod_roster, State),
    S#roster_state.pres_timestamp.

get_priority(State) ->
    case get_last_presence(State) of
        undefined ->
            0;
        OldPresence ->
            get_priority_from_presence(OldPresence)
    end.

presence_update_to_available(Packet, StateData, _C2SState) ->
    {new_state, StateData#roster_state{pres_last = Packet,
                                       pres_invis = false,
                                       pres_timestamp = calendar:universal_time()}}.

is_unavailable(C2SState) ->
    S = ejabberd_c2s_state:get_handler_state(mod_roster, C2SState),
    (S#roster_state.pres_last == undefined) orelse S#roster_state.pres_invis.

-type pack_tree() :: gb_trees:tree(binary() | jid:simple_jid(),
binary() | jid:simple_jid()).


%% @doc Try to reduce the heap footprint of the four presence sets
%% by ensuring that we re-use strings and Jids wherever possible.
-spec pack_state(term(), roster_state(), ejabberd_c2s:state()) -> {new_state, roster_state()}.
pack_state(_, State, _Cstate) ->
    #roster_state{pres_a=A,
                  pres_i=I,
                  pres_f=F,
                  pres_t=T} = State,
    {NewA, Pack1} = pack_jid_set(A, gb_trees:empty()),
    {NewI, Pack2} = pack_jid_set(I, Pack1),
    {NewF, Pack3} = pack_jid_set(F, Pack2),
    {NewT, _Pack4} = pack_jid_set(T, Pack3),
    %% Throw away Pack4 so that if we delete references to
    %% Strings or Jids in any of the sets there will be
    %% no live references for the GC to find.
    {new_state, State#roster_state{pres_a=NewA,
                                   pres_i=NewI,
                                   pres_f=NewF,
                                   pres_t=NewT}}.


-spec pack_jid_set(Set :: jid_set(),
                   Pack :: pack_tree()) -> {jid_set(), pack_tree()}.
pack_jid_set(Set, Pack) ->
    Jids = gb_sets:to_list(Set),
    {PackedJids, NewPack} = pack_jids(Jids, Pack, []),
    {gb_sets:from_list(PackedJids), NewPack}.


-spec pack_jids([{_, _, _}], Pack :: pack_tree(), Acc :: [jid:simple_jid()]) ->
    {[jid:simple_jid()], pack_tree()}.
pack_jids([], Pack, Acc) -> {Acc, Pack};
pack_jids([{U, S, R}=Jid | Jids], Pack, Acc) ->
    case gb_trees:lookup(Jid, Pack) of
        {value, PackedJid} ->
            pack_jids(Jids, Pack, [PackedJid | Acc]);
        none ->
            {NewU, Pack1} = pack_string(U, Pack),
            {NewS, Pack2} = pack_string(S, Pack1),
            {NewR, Pack3} = pack_string(R, Pack2),
            NewJid = {NewU, NewS, NewR},
            NewPack = gb_trees:insert(NewJid, NewJid, Pack3),
            pack_jids(Jids, NewPack, [NewJid | Acc])
    end.


-spec pack_string(String :: binary(), Pack :: pack_tree()) -> {binary(), pack_tree()}.
pack_string(String, Pack) ->
    case gb_trees:lookup(String, Pack) of
        {value, PackedString} ->
            {PackedString, Pack};
        none ->
            {String, gb_trees:insert(String, String, Pack)}
    end.

%%--------------------------------------------------------------------
%% This is called in every active session
%%--------------------------------------------------------------------

-spec roster_change(Item :: roster(),
                    StateData :: roster_state(),
                    State :: ejabberd_c2s:state()) -> roster_state().
roster_change(Item, StateData, C2SState) ->
    From = ejabberd_c2s_state:jid(C2SState),
    To = jid:make(Item#roster.jid),
    #roster{jid = IJID, subscription = ISubscription} = Item,
    {BecomeAvailable, BecomeUnavailable, NState} = do_roster_change(IJID, ISubscription, From, StateData),
    send_updated_presence(BecomeAvailable, BecomeUnavailable, From, To, C2SState),
    send_roster_iq(From, Item, C2SState),
    NState.

send_updated_presence(true, _, From, To, C2SState) ->
    P = get_last_presence(C2SState),
    Acc = mongoose_acc:new(#{location => ?LOCATION,
                             lserver => ejabberd_c2s_state:server(C2SState),
                             from_jid => From,
                             to_jid => To,
                             element => P}),
    send_with_privacy_check(Acc, C2SState, From, To, P, out);
send_updated_presence(_, true, From, To, C2SState) ->
    PU = #xmlel{name = <<"presence">>,
                attrs = [{<<"type">>, <<"unavailable">>}]},
    Acc = mongoose_acc:new(#{location => ?LOCATION,
                             lserver => ejabberd_c2s_state:server(C2SState),
                             from_jid => From,
                             to_jid => To,
                             element => PU}),
    send_with_privacy_check(Acc, C2SState, From, To, PU, out);
send_updated_presence(_, _, _From, _To, _C2SState) ->
    ok.

send_roster_iq(From, Item, C2SState) ->
    Server = From#jid.lserver,
    LUser = From#jid.luser,
    RosterVersion = case mod_roster:roster_versioning_enabled(Server) of
                        true ->
                            mod_roster:roster_version(Server, LUser);
                        false ->
                            not_found
                    end,
    mongoose_hooks:roster_push(Server, ok, From, Item),
    ExtraAttrs = case RosterVersion of
                     not_found -> [];
                     _ -> [{<<"ver">>, RosterVersion}]
                 end,
    ResIQ = #iq{type = set, xmlns = ?NS_ROSTER,
                %% @doc Roster push, calculate and include the version attribute.
                %% TODO: don't push to those who didn't load roster
                id = <<"push", (mongoose_bin:gen_from_crypto())/binary>>,
                sub_el =
                [#xmlel{name = <<"query">>,
                        attrs = [{<<"xmlns">>, ?NS_ROSTER} | ExtraAttrs],
                        children = [mod_roster:item_to_xml(Item)]}]},
    ejabberd_c2s:send_to_local_user(jid:to_bare(From), From, jlib:iq_to_xml(ResIQ), C2SState).

do_roster_change(IJID, ISubscription, OwnerJid, StateData) ->
    LIJID = jid:to_lower(IJID),
    IsSubscribedToMe = (ISubscription == both) or (ISubscription == from),
    AmISubscribedTo = (ISubscription == both) or (ISubscription == to),
    WasSubscribedToMe = gb_sets:is_element(LIJID, StateData#roster_state.pres_f),
    FSet = case IsSubscribedToMe of
               true ->
                   gb_sets:add_element(LIJID, StateData#roster_state.pres_f);
               false ->
                   gb_sets:del_element(LIJID, StateData#roster_state.pres_f)
           end,
    TSet = case AmISubscribedTo of
               true ->
                   gb_sets:add_element(LIJID, StateData#roster_state.pres_t);
               false ->
                   gb_sets:del_element(LIJID, StateData#roster_state.pres_t)
           end,
    case StateData#roster_state.pres_last of
        undefined ->
            {false, false, StateData#roster_state{pres_f = FSet, pres_t = TSet}};
        _ ->
            ?DEBUG("roster changed for ~p~n", [OwnerJid]),
            IsntInvisible = not StateData#roster_state.pres_invis,
            ImAvailableTo = gb_sets:is_element(LIJID, StateData#roster_state.pres_a),
            ImInvisibleTo = gb_sets:is_element(LIJID, StateData#roster_state.pres_i),
            BecomeAvailable = IsntInvisible and IsSubscribedToMe and not WasSubscribedToMe,
            BecomeUnavailable = not IsSubscribedToMe and WasSubscribedToMe
                                and (ImAvailableTo or ImInvisibleTo),
            case {BecomeAvailable, BecomeUnavailable} of
                {true, _} ->
                    ?DEBUG("become_available_to: ~p~n", [LIJID]),
                    A = gb_sets:add_element(LIJID,
                                            StateData#roster_state.pres_a),
                    NState = StateData#roster_state{pres_a = A,
                                                    pres_f = FSet,
                                                    pres_t = TSet},
                    {BecomeAvailable, BecomeUnavailable, NState};
                {_, true} ->
                    ?DEBUG("become_unavailable_to: ~p~n", [LIJID]),
                    I = gb_sets:del_element(LIJID,
                                            StateData#roster_state.pres_i),
                    A = gb_sets:del_element(LIJID,
                                            StateData#roster_state.pres_a),
                    NState = StateData#roster_state{pres_i = I,
                                                    pres_a = A,
                                                    pres_f = FSet,
                                                    pres_t = TSet},
                    {BecomeAvailable, BecomeUnavailable, NState};
                _ ->
                    NState = StateData#roster_state{pres_f = FSet, pres_t = TSet},
                    {BecomeAvailable, BecomeUnavailable, NState}
            end
    end.

send_with_privacy_check(Acc, StateData, From, To, Packet, Dir) ->
    Jid = ejabberd_c2s_state:jid(StateData),
    {Acc2, Res} = mongoose_privacy:privacy_check_packet(Acc,
                                                        ejabberd_c2s_state:server(StateData),
                                                        Jid#jid.luser,
                                                        ejabberd_c2s_state:privacy_list(StateData),
                                                        To,
                                                        Dir),
    case Res of
        allow -> ejabberd_router:route(From, To, Acc2, Packet);
        _ -> Acc2
    end.

%%--------------------------------------------------------------------
%% end of what is called in every active session
%%--------------------------------------------------------------------

-spec get_priority_from_presence(Packet :: exml:element()) -> integer().
get_priority_from_presence(undefined) ->
    0;
get_priority_from_presence(PresencePacket) ->
    case xml:get_subtag(PresencePacket, <<"priority">>) of
        false ->
            0;
        SubEl ->
            case catch list_to_integer(binary_to_list(xml:get_tag_cdata(SubEl))) of
                P when is_integer(P) ->
                    P;
                _ ->
                    0
            end
    end.

