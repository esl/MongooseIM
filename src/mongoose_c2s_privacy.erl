-module(mongoose_c2s_privacy).
-author("bartlomiejgorny").
%% API
-include("mongoose.hrl").
-include("mod_privacy.hrl").
-include("jlib.hrl").


-export([initialise_state/1, check_packet/4, check_packet/6]).
-export([process_privacy_iq/3]).
-export([update_privacy_list/4]).

%% temporary
-export([get_privacy_list/1, set_privacy_list/2]).


-spec initialise_state(jid:jid()) -> privacy_state().
initialise_state(JID) ->
    UserList = mongoose_hooks:privacy_get_user_list(JID#jid.server, #userlist{}, JID#jid.user),
    #privacy_state{userlist = UserList}.

check_packet(Acc, To, Dir, StateData) ->
    Jid = ejabberd_c2s_state:jid(StateData),
    UserList = get_userlist(StateData),
    mongoose_privacy:privacy_check_packet(Acc,
                                          ejabberd_c2s_state:server(StateData),
                                          Jid#jid.user,
                                          UserList,
                                          To,
                                          Dir).

check_packet(Acc, Packet, From, To, Dir, StateData) ->
    UserList = get_userlist(StateData),
    Jid = ejabberd_c2s_state:jid(StateData),
    mongoose_privacy:privacy_check_packet({Acc, Packet},
                                          ejabberd_c2s_state:server(StateData),
                                          Jid#jid.user,
                                          UserList,
                                          From,
                                          To,
                                          Dir).

%% this is called remotely by the process that received an iq
-spec update_privacy_list(privacy_state(), atom(), term(), ejabberd_c2s:state()) -> privacy_state().
update_privacy_list(_HandlerState, mod_privacy, {privacy_change, ListName, Userlist}, C2SState) ->
    JID = ejabberd_c2s_state:jid(C2SState),
    Server = ejabberd_c2s_state:server(C2SState),
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => Server,
                              element => undefined }),
    NewPL = mongoose_hooks:privacy_updated_list(Server,
                                                false,
                                                get_privacy_list(C2SState),
                                                Userlist),
    PrivPushIQ = privacy_list_push_iq(ListName),
    F = jid:to_bare(JID),
    T = JID,
    PrivPushEl = jlib:replace_from_to(F, T, jlib:iq_to_xml(PrivPushIQ)),
    Acc1 = maybe_update_presence(Acc, C2SState, NewPL),
    _Acc2 = ejabberd_c2s:preprocess_and_ship(Acc1, F, T, PrivPushEl, C2SState),
    #privacy_state{userlist = NewPL};
update_privacy_list(HandlerState, _, _, _) ->
    HandlerState.

get_userlist(StateData) ->
    #privacy_state{userlist = UserList} = ejabberd_c2s_state:get_handler_state(mod_privacy, StateData),
    UserList.

%% temporary

get_privacy_list(StateData) ->
    case ejabberd_c2s_state:get_handler_state(mod_privacy, StateData) of
        empty_state -> [];
        #privacy_state{userlist = UserList} -> UserList
    end.

set_privacy_list(UserList, StateData) ->
    ejabberd_c2s_state:set_handler_state(mod_privacy, #privacy_state{userlist = UserList}, StateData).

%% it is not feasible to make this a proper IQ handler, because it requires access to
%% the c2s state and it has to modify state to make it an active list
-spec process_privacy_iq(Acc :: mongoose_acc:t(),
                         To :: jid:jid(),
                         StateData :: ejabberd_c2s:state()) -> {mongoose_acc:t(), ejabberd_c2s:state()}.
process_privacy_iq(Acc1, To, StateData) ->
    case mongoose_iq:info(Acc1) of
        {#iq{type = Type, sub_el = SubEl} = IQ, Acc2} when Type == get; Type == set ->
            From = mongoose_acc:from_jid(Acc2),
            {Acc3, NewStateData} = process_privacy_iq(Acc2, Type, To, StateData),
            Res = mongoose_acc:get(hook, result,
                                   {error, mongoose_xmpp_errors:feature_not_implemented()}, Acc3),
            IQRes = case Res of
                        {result, Result} ->
                            IQ#iq{type = result, sub_el = Result};
                        {result, Result, _} ->
                            IQ#iq{type = result, sub_el = Result};
                        {error, Error} ->
                            IQ#iq{type = error, sub_el = [SubEl, Error]}
                    end,
            Acc4 = ejabberd_c2s:preprocess_and_ship(Acc3, To, From, jlib:iq_to_xml(IQRes), StateData),
            {Acc4, NewStateData};
        _ ->
            {Acc1, StateData}
    end.

-spec process_privacy_iq(Acc :: mongoose_acc:t(),
                         Type :: get | set,
                         To :: jid:jid(),
                         StateData :: ejabberd_c2s:state()) -> {mongoose_acc:t(), ejabberd_c2s:state()}.
process_privacy_iq(Acc, get, To, StateData) ->
    From = mongoose_acc:from_jid(Acc),
    {IQ, Acc1} = mongoose_iq:info(Acc),
    Acc2 = mongoose_hooks:privacy_iq_get(ejabberd_c2s_state:server(StateData),
                                         Acc1,
                                         From, To, IQ,
                                         get_privacy_list(StateData)),
    {Acc2, StateData};
process_privacy_iq(Acc, set, To, StateData) ->
    From = mongoose_acc:from_jid(Acc),
    {IQ, Acc1} = mongoose_iq:info(Acc),
    Acc2 = mongoose_hooks:privacy_iq_set(ejabberd_c2s_state:server(StateData),
                                         Acc1,
                                         From, To, IQ),
    case mongoose_acc:get(hook, result, undefined, Acc2) of
        {result, _, NewPrivList} ->
            % we have a new active list which we need to set in c2s state
            % and send updated presence info if needed
            Acc3 = maybe_update_presence(Acc2, StateData, NewPrivList),
            NState = set_privacy_list(NewPrivList, StateData),
            {Acc3, NState};
        _ ->
            % no change in active list; changes to privacy settings have been propagated
            % by mod_roster hook handlers, and we will receive them presently
            % as so-called 'routed broadcast'
            {Acc2, StateData}
    end.

maybe_update_presence(Acc, StateData, NewList) ->
    % Our own jid is added to pres_f, even though we're not a "contact", so for
    % the purposes of this check we don't want it:
    FromsExceptSelf = mongoose_c2s_presence:get_subscriptions(from_except_self, StateData),

    lists:foldl(
        fun(T, Ac) ->
            send_unavail_if_newly_blocked(Ac, StateData, jid:make(T), NewList)
        end, Acc, FromsExceptSelf).

send_unavail_if_newly_blocked(Acc, StateData,
                              ContactJID, NewList) ->
    JID = ejabberd_c2s_state:jid(StateData),
    Packet = #xmlel{name = <<"presence">>,
                    attrs = [{<<"type">>, <<"unavailable">>}]},
    %% WARNING: we can not use accumulator to cache privacy check result - this is
    %% the only place where the list to check against changes
    EmptyAcc = mongoose_acc:new(#{ location => ?LOCATION,
                                   from_jid => JID,
                                   to_jid => ContactJID,
                                   lserver => ejabberd_c2s_state:server(StateData),
                                   element => Packet }),
    {_, OldResult} = check_packet(EmptyAcc, Packet, JID, ContactJID, out, StateData),
    {_, NewResult} = check_packet(EmptyAcc, Packet, JID, ContactJID, out,
                                     set_privacy_list(NewList, StateData)),
    send_unavail_if_newly_blocked(Acc, OldResult, NewResult, JID,
                                  ContactJID, Packet).

send_unavail_if_newly_blocked(Acc, allow, deny, From, To, Packet) ->
    ejabberd_router:route(From, To, Acc, Packet);
send_unavail_if_newly_blocked(Acc, _, _, _, _, _) ->
    Acc.

privacy_list_push_iq(PrivListName) ->
    #iq{type = set, xmlns = ?NS_PRIVACY,
        id = <<"push", (mongoose_bin:gen_from_crypto())/binary>>,
        sub_el = [#xmlel{name = <<"query">>,
                         attrs = [{<<"xmlns">>, ?NS_PRIVACY}],
                         children = [#xmlel{name = <<"list">>,
                                            attrs = [{<<"name">>, PrivListName}]}]}]}.

