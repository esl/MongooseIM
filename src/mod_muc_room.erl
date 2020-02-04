%%%----------------------------------------------------------------------
%%% File    : mod_muc_room.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC room stuff
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_room).
-author('alexey@process-one.net').
-behaviour(gen_fsm_compat).


%% External exports
-export([start_link/10,
         start_link/8,
         start/10,
         start/8,
         route/5,
         stop/1]).

%% API exports
-export([get_room_users/1,
         is_room_owner/2,
         can_access_room/2,
         can_access_identity/2]).

%% gen_fsm callbacks
-export([init/1,
         normal_state/2,
         locked_state/2,
         initial_state/2,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-import(mongoose_lib, [maps_append/3,
                       maps_foreach/2,
                       pairs_foreach/2,
                       maps_or_pairs_foreach/2]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mod_muc_room.hrl").

-record(routed_message, {allowed,
                         type,
                         from,
                         packet,
                         lang
                        }).
-type routed_message() :: #routed_message{}.

-record(routed_nick_message, {allow_pm,
                              online,
                              type,
                              from,
                              nick,
                              lang,
                              packet,
                              decide,
                              jid
                            }).
-type routed_nick_message() :: #routed_nick_message{}.

-record(routed_iq, {iq,
                    from,
                    packet
                   }).
-type routed_iq() :: #routed_iq{}.

-record(routed_nick_iq, {allow_query,
                         online,
                         iq,
                         packet,
                         lang,
                         nick,
                         jid,
                         from,
                         stanza
                       }).
-type routed_nick_iq() :: #routed_nick_iq{}.

%%%----------------------------------------------------------------------
%%% Types
%%%----------------------------------------------------------------------
-export_type([config/0, user/0, activity/0]).

-type statename() :: 'locked_state' | 'normal_state'.
-type fsm_return() :: {'next_state', statename(), state()}
                    | {'next_state', statename(), state(), timeout() | hibernate}
                    | {'stop', any(), state()}.

-type lqueue() :: #lqueue{}.
-type state() :: #state{}.
-type config() :: #config{}.
-type user() :: #user{}.
-type activity() :: #activity{}.
-type stanzaid() :: {binary(), jid:resource()}.
-type new_user_strategy() :: 'allowed'
                           | 'conflict_registered'
                           | 'conflict_use'
                           | 'invalid_password'
                           | 'limit_reached'
                           | 'require_membership'
                           | 'require_password'
                           | 'user_banned'
                           | 'http_auth'.
-type users_map() :: #{jid:simple_jid() => user()}.
-type users_pairs() :: [{jid:simple_jid(), user()}].
-type sessions_map() :: #{mod_muc:nick() => jid:jid()}.
-type affiliations_map() :: #{jid:simple_jid() => mod_muc:affiliation()}.


-type update_inbox_for_muc_payload() :: #{
        room_jid := jid:jid(),
        from_jid := jid:jid(),
        from_room_jid := jid:jid(),
        packet := exml:element(),
        affiliations_map := affiliations_map()
       }.
-export_type([update_inbox_for_muc_payload/0]).

-define(MAX_USERS_DEFAULT_LIST,
        [5, 10, 20, 30, 50, 100, 200, 500, 1000, 2000, 5000]).

%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

%% Module start with or without supervisor:
-ifdef(NO_TRANSIENT_SUPERVISORS).
-define(SUPERVISOR_START,
        gen_fsm_compat:start(?MODULE,
                      [Host, ServerHost, Access, Room, HistorySize,
                       RoomShaper, HttpAuthPool, Creator, Nick, DefRoomOpts],
                      ?FSMOPTS)).
-else.
-define(SUPERVISOR_START,
        Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_muc_sup),
        supervisor:start_child(Supervisor,
                               [Host, ServerHost, Access, Room, HistorySize,
                                RoomShaper, HttpAuthPool, Creator, Nick, DefRoomOpts])).
-endif.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-spec start(Host :: jid:server(), ServerHost :: jid:server(),
            Access :: _, Room :: mod_muc:room(), HistorySize :: integer(),
            RoomShaper :: shaper:shaper(), HttpAuthPool :: none | mongoose_http_client:pool(),
            Creator :: jid:jid(), Nick :: mod_muc:nick(),
            DefRoomOpts :: list()) -> {'error', _}
                                          | {'ok', 'undefined' | pid()}
                                          | {'ok', 'undefined' | pid(), _}.
start(Host, ServerHost, Access, Room, HistorySize, RoomShaper, HttpAuthPool,
      Creator, Nick, DefRoomOpts) ->
    ?SUPERVISOR_START.


-spec start(Host :: jid:server(), ServerHost :: jid:server(),
            Access :: _, Room :: mod_muc:room(), HistorySize :: integer(),
            RoomShaper :: shaper:shaper(), HttpAuthPool :: none | mongoose_http_client:pool(),
            Opts :: list()) -> {'error', _}
                                   | {'ok', 'undefined' | pid()}
                                   | {'ok', 'undefined' | pid(), _}.
start(Host, ServerHost, Access, Room, HistorySize, RoomShaper, HttpAuthPool, Opts)
    when is_list(Opts) ->
    Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_muc_sup),
    supervisor:start_child(Supervisor, [Host, ServerHost, Access, Room,
                                        HistorySize, RoomShaper, HttpAuthPool, Opts]).

start_link(Host, ServerHost, Access, Room, HistorySize, RoomShaper, HttpAuthPool,
       Creator, Nick, DefRoomOpts)
    when is_list(DefRoomOpts) ->
    gen_fsm_compat:start_link(?MODULE,
                       [Host, ServerHost, Access, Room, HistorySize,
                        RoomShaper, HttpAuthPool, Creator, Nick, DefRoomOpts],
                       ?FSMOPTS).

start_link(Host, ServerHost, Access, Room, HistorySize, RoomShaper, HttpAuthPool, Opts)
    when is_list(Opts) ->
    gen_fsm_compat:start_link(?MODULE,
                       [Host, ServerHost, Access, Room, HistorySize,
                        RoomShaper, HttpAuthPool, Opts],
                       ?FSMOPTS).

stop(Pid) ->
    gen_fsm_compat:stop(Pid).

-spec get_room_users(RoomJID :: jid:jid()) -> {ok, [user()]}
                                             | {error, not_found}.
get_room_users(RoomJID) ->
    case mod_muc:room_jid_to_pid(RoomJID) of
        {ok, Pid} ->
            gen_fsm_compat:sync_send_all_state_event(Pid, get_room_users);
        {error, Reason} ->
            {error, Reason}
    end.

-spec is_room_owner(RoomJID :: jid:jid(), UserJID :: jid:jid()) ->
    {ok, boolean()} | {error, not_found}.
is_room_owner(RoomJID, UserJID) ->
    case mod_muc:room_jid_to_pid(RoomJID) of
        {ok, Pid} ->
            gen_fsm_compat:sync_send_all_state_event(Pid, {is_room_owner, UserJID});
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Return true if UserJID can read room messages
-spec can_access_room(RoomJID :: jid:jid(), UserJID :: jid:jid()) ->
            {ok, boolean()} | {error, not_found}.
can_access_room(RoomJID, UserJID) ->
    case mod_muc:room_jid_to_pid(RoomJID) of
        {ok, Pid} ->
            gen_fsm_compat:sync_send_all_state_event(Pid, {can_access_room, UserJID});
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Return true if UserJID can read real user JIDs
-spec can_access_identity(RoomJID :: jid:jid(), UserJID :: jid:jid()) ->
    {ok, boolean()} | {error, not_found}.
can_access_identity(RoomJID, UserJID) ->
    case mod_muc:room_jid_to_pid(RoomJID) of
        {ok, Pid} ->
            gen_fsm_compat:sync_send_all_state_event(Pid, {can_access_identity, UserJID});
        {error, Reason} ->
            {error, Reason}
    end.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%%-----------------------------------------------------------------------

%% @doc A room is created. Depending on request type (MUC/groupchat 1.0) the
%% next state is determined accordingly (a locked room for MUC or an instant
%% one for groupchat).
-spec init([any(), ...]) ->
    {ok, statename(), state()} | {ok, statename(), state(), timeout()}.
init([Host, ServerHost, Access, Room, HistorySize, RoomShaper, HttpAuthPool,
      Creator, _Nick, DefRoomOpts]) when is_list(DefRoomOpts) ->
    process_flag(trap_exit, true),
    Shaper = shaper:new(RoomShaper),
    State = set_affiliation(Creator, owner,
                            #state{host = Host,
                                   server_host = ServerHost,
                                   access = Access,
                                   room = Room,
                                   history = lqueue_new(HistorySize),
                                   jid = jid:make(Room, Host, <<>>),
                                   just_created = true,
                                   room_shaper = Shaper,
                                   http_auth_pool = HttpAuthPool,
                                   hibernate_timeout = read_hibernate_timeout(ServerHost)
                                  }),
    State1 = set_opts(DefRoomOpts, State),
    ?INFO_MSG("Created MUC room ~s@~s by ~s",
              [Room, Host, jid:to_binary(Creator)]),
    add_to_log(room_existence, created, State1),
    case proplists:get_value(instant, DefRoomOpts, false) of
        true ->
            %% Instant room -- groupchat 1.0 request
            add_to_log(room_existence, started, State1),
            save_persistent_room_state(State1),
            {ok, normal_state, State1, State1#state.hibernate_timeout};
        false ->
            %% Locked room waiting for configuration -- MUC request
            {ok, initial_state, State1}
    end;
%% @doc A room is restored
init([Host, ServerHost, Access, Room, HistorySize, RoomShaper, HttpAuthPool, Opts]) ->
    process_flag(trap_exit, true),
    Shaper = shaper:new(RoomShaper),
    State = set_opts(Opts, #state{host = Host,
                                  server_host = ServerHost,
                                  access = Access,
                                  room = Room,
                                  history = lqueue_new(HistorySize),
                                  jid = jid:make(Room, Host, <<>>),
                                  room_shaper = Shaper,
                                  http_auth_pool = HttpAuthPool,
                                  hibernate_timeout = read_hibernate_timeout(ServerHost)
                                 }),
    add_to_log(room_existence, started, State),
    mongoose_metrics:update(global, [mod_muc, process_recreations], 1),
    {ok, normal_state, State, State#state.hibernate_timeout}.

read_hibernate_timeout(Host) ->
    gen_mod:get_module_opt(Host, mod_muc, hibernate_timeout, timer:seconds(90)).

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------

%% @doc In the locked state StateData contains the same settings it previously
%% held for the normal_state. The fsm awaits either a confirmation or a
%% configuration form from the creator. Responds with error to any other queries.
-spec locked_error({'route', jid:jid(), _, mongoose_acc:t(), exml:element()},
                   statename(), state()) -> fsm_return().
locked_error({route, From, ToNick, Acc, #xmlel{attrs = Attrs} = Packet},
             NextState, StateData) ->
    ?INFO_MSG("Wrong stanza: ~p", [Packet]),
    ErrText = <<"This room is locked">>,
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    {Acc1, Err} = jlib:make_error_reply(Acc, Packet, mongoose_xmpp_errors:item_not_found(Lang, ErrText)),
    ejabberd_router:route(jid:replace_resource(StateData#state.jid,
                                               ToNick),
                          From, Acc1, Err),
    {next_state, NextState, StateData}.

%% @doc  Receive the room-creating Stanza. Will crash if any other stanza is
%% received in this state.
-spec initial_state({'route', From :: jid:jid(), To :: mod_muc:nick(),
                    Acc :: mongoose_acc:t(), Presence :: exml:element()}, state()) -> fsm_return().
initial_state({route, From, ToNick, _Acc, % TOODOO
              #xmlel{name = <<"presence">>} = Presence}, StateData) ->
    %% this should never happen so crash if it does
    <<>> = exml_query:attr(Presence, <<"type">>, <<>>),
    owner = get_affiliation(From, StateData), %% prevent race condition (2 users create same room)
    XNamespaces = exml_query:paths(Presence, [{element, <<"x">>}, {attr, <<"xmlns">>}]),
    case lists:member(?NS_MUC, XNamespaces) of
        true ->
            %% FIXME
            add_to_log(room_existence, started, StateData),
            process_presence(From, ToNick, Presence, StateData, locked_state);
            %% The fragment of normal_state with Activity that used to do this - how does that work?
            %% Seems to work without it
        false ->
            %% groupchat 1.0 user, straight to normal_state
            process_presence(From, ToNick, Presence, StateData)
    end.


-spec is_query_allowed(exml:element()) -> boolean().
is_query_allowed(Query) ->
    X = xml:get_subtag(Query, <<"x">>),
    xml:get_subtag(Query, <<"destroy">>) =/= false orelse
        (X =/= false andalso xml:get_tag_attr_s(<<"xmlns">>, X)== ?NS_XDATA andalso
        (xml:get_tag_attr_s(<<"type">>, X) == <<"submit">> orelse
        xml:get_tag_attr_s(<<"type">>, X)== <<"cancel">>)).


-spec locked_state_process_owner_iq(jid:jid(), exml:element(),
        ejabberd:lang(), 'error' | 'get' | 'invalid' | 'result', _)
            -> {{'error', exml:element()}, statename()}
               | {{result, [exml:element() | jlib:xmlcdata()], state() | stop}, statename()}.
locked_state_process_owner_iq(From, Query, Lang, set, StateData) ->
    Result = case is_query_allowed(Query) of
                 true ->
                     process_iq_owner(From, set, Lang, Query, StateData, locked_state);
                 false ->
                     {error, mongoose_xmpp_errors:item_not_found(Lang, <<"Query not allowed">>)}
             end,
    {Result, normal_state};
locked_state_process_owner_iq(From, Query, Lang, get, StateData) ->
    {process_iq_owner(From, get, Lang, Query, StateData, locked_state), locked_state};
locked_state_process_owner_iq(_From, _Query, Lang, _Type, _StateData) ->
    {{error, mongoose_xmpp_errors:item_not_found(Lang, <<"Wrong type">>)}, locked_state}.


%% @doc Destroy room / confirm instant room / configure room
-spec locked_state({'route', From :: jid:jid(), To :: mod_muc:nick(),
                    Acc :: mongoose_acc:t(), Packet :: exml:element()}, state()) -> fsm_return().
locked_state({route, From, _ToNick, Acc,
              #xmlel{name = <<"iq">>} = Packet}, StateData) ->
    #iq{lang = Lang, sub_el = Query} = IQ = jlib:iq_query_info(Packet),
    {Result, NextState1} =
        case IQ#iq.xmlns == ?NS_MUC_OWNER andalso get_affiliation(From, StateData)  =:= owner of
            true ->
                locked_state_process_owner_iq(From, Query, Lang, IQ#iq.type, StateData);
            false ->
                ErrText = <<"This room is locked">>,
                {{error, mongoose_xmpp_errors:item_not_found(Lang, ErrText)}, locked_state}
        end,
    MkQueryResult = fun(Res) ->
                        IQ#iq{type = result,
                            sub_el = [#xmlel{name = <<"query">>,
                                             attrs = [{<<"xmlns">>, ?NS_MUC_OWNER}],
                                             children = Res}]}
                    end,
    {IQRes, StateData3, NextState2} =
        case Result of
            {result, InnerRes, stop} -> {MkQueryResult(InnerRes), StateData, stop};
            {result, InnerRes, StateData2} -> {MkQueryResult(InnerRes), StateData2, NextState1};
            {error, Error} -> {IQ#iq{type = error, sub_el = [Query, Error]}, StateData, NextState1}
        end,
    ejabberd_router:route(StateData3#state.jid, From, Acc, jlib:iq_to_xml(IQRes)),
    case NextState2 of
        stop ->
            {stop, normal, StateData3};
        locked_state ->
            {next_state, NextState2, StateData3};
        normal_state ->
            next_normal_state(StateData3#state{just_created = false})
    end;
%% Let owner leave. Destroy the room.
locked_state({route, From, ToNick, _Acc,
              #xmlel{name = <<"presence">>, attrs = Attrs} = Presence} = Call,
             StateData) ->
    case xml:get_attr_s(<<"type">>, Attrs) =:= <<"unavailable">>
        andalso get_affiliation(From, StateData)  =:= owner of
        true ->
            %% Will let the owner leave and destroy the room if it's not persistant
            %% The rooms are not persistent by default, but just to be safe...
            NewConfig = (StateData#state.config)#config{persistent = false},
            StateData1 = StateData#state{config = NewConfig},
            process_presence(From, ToNick, Presence, StateData1, locked_state);
        _ ->
            locked_error(Call, locked_state, StateData)
    end;
locked_state(timeout, StateData) ->
    {next_state, locked_state, StateData};
locked_state(Call, StateData) ->
    locked_error(Call, locked_state, StateData).


-spec normal_state({route, From :: jid:jid(), To :: mod_muc:nick(), Acc :: mongoose_acc:t(),
                   Packet :: exml:element()}, state()) -> fsm_return().
normal_state({route, From, <<>>, _Acc,
              #xmlel{name = <<"message">>, attrs = Attrs} = Packet},
             StateData) ->
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    Type = xml:get_attr_s(<<"type">>, Attrs),

    NewStateData = route_message(#routed_message{
        allowed = can_send_to_conference(From, StateData),
        type = Type,
        from = From,
        packet = Packet,
        lang = Lang}, StateData),
    next_normal_state(NewStateData);
normal_state({route, From, <<>>, Acc0,
          #xmlel{name = <<"iq">>} = Packet},
         StateData) ->
    {IQ, Acc} = mongoose_iq:info(Acc0),
    {RoutingEffect, NewStateData} = route_iq(Acc, #routed_iq{
        iq = IQ,
        from = From,
        packet = Packet}, StateData),
    case RoutingEffect of
        ok -> next_normal_state(NewStateData);
        stop -> {stop, normal, NewStateData}
    end;
normal_state({route, From, Nick, _Acc,
              #xmlel{name = <<"presence">>} = Packet},
             StateData) ->
    % FIXME sessions do we need to route presences to all sessions
    Activity = get_user_activity(From, StateData),
    Now = os:system_time(microsecond),
    MinPresenceInterval =
    trunc(gen_mod:get_module_opt(StateData#state.server_host,
                                 mod_muc, min_presence_interval, 0) * 1000000),
    case (Now >= Activity#activity.presence_time + MinPresenceInterval) and
         (Activity#activity.presence == undefined) of
        true ->
            NewActivity = Activity#activity{presence_time = Now},
            StateData1 = store_user_activity(From, NewActivity, StateData),
            process_presence(From, Nick, Packet, StateData1);
        false ->
            case Activity#activity.presence == undefined of
                true ->
                    Interval = (Activity#activity.presence_time +
                                MinPresenceInterval - Now) div 1000,
                    erlang:send_after(Interval, self(), {process_user_presence, From});
                false ->
                    ok
            end,
            NewActivity = Activity#activity{presence = {Nick, Packet}},
            StateData1 = store_user_activity(From, NewActivity, StateData),
            next_normal_state(StateData1)
    end;
normal_state({route, From, ToNick, _Acc,
              #xmlel{name = <<"message">>, attrs = Attrs} = Packet},
             StateData) ->
    Type = xml:get_attr_s(<<"type">>, Attrs),
    FunRouteNickMessage = fun(JID, StateDataAcc) ->
        route_nick_message(#routed_nick_message{
        allow_pm = (StateDataAcc#state.config)#config.allow_private_messages,
        online = is_user_online(From, StateDataAcc),
        type = Type,
        from = From,
        nick = ToNick,
        lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
        decide = decide_fate_message(Type, Packet, From, StateDataAcc),
        packet = Packet,
        jid = JID}, StateDataAcc)
    end,
    NewStateData = case find_jids_by_nick(ToNick, StateData) of
        [] -> FunRouteNickMessage(false, StateData);
        JIDs -> lists:foldl(FunRouteNickMessage, StateData, JIDs)
    end,
    next_normal_state(NewStateData);
normal_state({route, From, ToNick, _Acc,
          #xmlel{name = <<"iq">>, attrs = Attrs} = Packet},
         StateData) ->
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    StanzaId = xml:get_attr_s(<<"id">>, Attrs),
    FunRouteNickIq = fun(JID) ->
        route_nick_iq(#routed_nick_iq{
            allow_query = (StateData#state.config)#config.allow_query_users,
            online = is_user_online_iq(StanzaId, From, StateData),
            jid = JID,
            iq = jlib:iq_query_info(Packet),
            packet = Packet,
            lang = Lang,
            from = From,
            stanza = StanzaId,
            nick = ToNick}, StateData)
    end,
    case find_jids_by_nick(ToNick, StateData) of
        [] -> FunRouteNickIq(false);
        JIDs -> lists:foreach(FunRouteNickIq, JIDs)
    end,
    next_normal_state(StateData);
normal_state({http_auth, AuthPid, Result, From, Nick, Packet, Role}, StateData) ->
    AuthPids = StateData#state.http_auth_pids,
    StateDataWithoutPid = StateData#state{http_auth_pids = lists:delete(AuthPid, AuthPids)},
    NewStateData = handle_http_auth_result(Result, From, Nick, Packet, Role, StateDataWithoutPid),
    destroy_temporary_room_if_empty(NewStateData, normal_state);
normal_state(timeout, StateData) ->
    erlang:put(hibernated, os:timestamp()),
    mongoose_metrics:update(global, [mod_muc, hibernations], 1),
    {next_state, normal_state, StateData, hibernate};
normal_state(_Event, StateData) ->
    next_normal_state(StateData).

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_event({service_message, Msg}, _StateName, StateData) ->
    MessagePkt = #xmlel{name = <<"message">>,
                        attrs = [{<<"type">>, <<"groupchat">>}],
                        children = [#xmlel{name = <<"body">>,
                                           children = [#xmlcdata{content = Msg}]}]},
    send_to_all_users(MessagePkt, StateData),
    NSD = add_message_to_history(<<>>,
                 StateData#state.jid,
                 MessagePkt,
                 StateData),
    next_normal_state(NSD);

handle_event({destroy, Reason}, _StateName, StateData) ->
    {result, [], stop} =
        destroy_room(
          #xmlel{name = <<"destroy">>, attrs = [{<<"xmlns">>, ?NS_MUC_OWNER}],
                 children = case Reason of
                                none -> [];
                                _Else ->
                                    [#xmlel{name = <<"reason">>,
                                            children = [#xmlcdata{content = Reason}]}]
                            end}, StateData),
    ?INFO_MSG("Destroyed MUC room ~s with reason: ~p",
          [jid:to_binary(StateData#state.jid), Reason]),
    add_to_log(room_existence, destroyed, StateData),
    {stop, shutdown, StateData};
handle_event(destroy, StateName, StateData) ->
    ?INFO_MSG("Destroyed MUC room ~s",
          [jid:to_binary(StateData#state.jid)]),
    handle_event({destroy, none}, StateName, StateData);

handle_event({set_affiliations, Affiliations},
             #state{hibernate_timeout = Timeout} = StateName, StateData) ->
    {next_state, StateName, StateData#state{affiliations = Affiliations}, Timeout};

handle_event(_Event, StateName, #state{hibernate_timeout = Timeout} = StateData) ->
    {next_state, StateName, StateData, Timeout}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
handle_sync_event({get_disco_item, JID, Lang}, _From, StateName, StateData) ->
    Reply = get_roomdesc_reply(JID, StateData,
                   get_roomdesc_tail(StateData, Lang)),
    reply_with_timeout(Reply, StateName, StateData);
handle_sync_event(get_config, _From, StateName, StateData) ->
    reply_with_timeout({ok, StateData#state.config}, StateName, StateData);
handle_sync_event(get_state, _From, StateName, StateData) ->
    reply_with_timeout({ok, StateData}, StateName, StateData);
handle_sync_event(get_room_users, _From, StateName, StateData) ->
    reply_with_timeout({ok, maps:values(StateData#state.users)}, StateName, StateData);
handle_sync_event({is_room_owner, UserJID}, _From, StateName, StateData) ->
    reply_with_timeout({ok, get_affiliation(UserJID, StateData) =:= owner}, StateName, StateData);
handle_sync_event({can_access_room, UserJID}, _From, StateName, StateData) ->
    reply_with_timeout({ok,  can_read_conference(UserJID, StateData)}, StateName, StateData);
handle_sync_event({can_access_identity, UserJID}, _From, StateName, StateData) ->
    reply_with_timeout({ok,  can_user_access_identity(UserJID, StateData)}, StateName, StateData);
handle_sync_event({change_config, Config}, _From, StateName, StateData) ->
    {result, [], NSD} = change_config(Config, StateData),
    reply_with_timeout({ok, NSD#state.config}, StateName, NSD);
handle_sync_event({change_state, NewStateData}, _From, StateName, _StateData) ->
    reply_with_timeout({ok, NewStateData}, StateName, NewStateData);
handle_sync_event(_Event, _From, StateName, StateData) ->
    reply_with_timeout(ok, StateName, StateData).

reply_with_timeout(Reply, StateName, #state{hibernate_timeout = Timeout} = State) ->
    {reply, Reply, StateName, State, Timeout}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
maybe_prepare_room_queue(RoomQueue, StateData) ->
    StateData1 = StateData#state{room_queue = RoomQueue},
    case queue:is_empty(StateData#state.room_queue) of
        true ->
            StateData2 = prepare_room_queue(StateData1),
            next_normal_state(StateData2);
        _ ->
            next_normal_state(StateData1)
    end.

-type info_msg() :: {process_user_presence | process_user_message, jid:jid()}
                    | process_room_queue.
-spec handle_info(info_msg(), statename(), state()) -> fsm_return().
handle_info({process_user_presence, From}, normal_state = _StateName, StateData) ->
    RoomQueue = queue:in({presence, From}, StateData#state.room_queue),
    maybe_prepare_room_queue(RoomQueue, StateData);
handle_info({process_user_message, From}, normal_state = _StateName, StateData) ->
    RoomQueue = queue:in({message, From}, StateData#state.room_queue),
    maybe_prepare_room_queue(RoomQueue, StateData);
handle_info(process_room_queue, normal_state, StateData) ->
    case queue:out(StateData#state.room_queue) of
    {{value, {message, From}}, RoomQueue} ->
        Activity = get_user_activity(From, StateData),
        Packet = Activity#activity.message,
        NewActivity = Activity#activity{message = undefined},
        StateData1 =
        store_user_activity(
          From, NewActivity, StateData),
        StateData2 =
        StateData1#state{
          room_queue = RoomQueue},
        StateData3 = prepare_room_queue(StateData2),
        process_groupchat_message(From, Packet, StateData3);
    {{value, {presence, From}}, RoomQueue} ->
        Activity = get_user_activity(From, StateData),
        {Nick, Packet} = Activity#activity.presence,
        NewActivity = Activity#activity{presence = undefined},
        StateData1 =
        store_user_activity(
          From, NewActivity, StateData),
        StateData2 =
        StateData1#state{
          room_queue = RoomQueue},
        StateData3 = prepare_room_queue(StateData2),
        process_presence(From, Nick, Packet, StateData3);
    {empty, _} ->
            next_normal_state(StateData)
    end;
handle_info({'EXIT', FromPid, _Reason}, StateName, StateData) ->
    AuthPids = StateData#state.http_auth_pids,
    StateWithoutPid = StateData#state{http_auth_pids = lists:delete(FromPid, AuthPids)},
    destroy_temporary_room_if_empty(StateWithoutPid, StateName);
handle_info(stop_persistent_room_process, normal_state,
            #state{room = RoomName,
                   config = #config{persistent = true}} = StateData) ->
    maybe_stop_persistent_room(RoomName, is_empty_room(StateData), StateData);
handle_info(_Info, StateName, #state{hibernate_timeout = Timeout} = StateData) ->
    {next_state, StateName, StateData, Timeout}.

maybe_stop_persistent_room(RoomName, true, State) ->
    do_stop_persistent_room(RoomName, State);
maybe_stop_persistent_room(RoomName, _, State) ->
    stop_if_only_owner_is_online(RoomName, count_users(State), State).

stop_if_only_owner_is_online(RoomName, 1, #state{users = Users, jid = RoomJID} = State) ->
    [{LJID, #user{jid = LastUser, nick = Nick}}] = maps:to_list(Users),

    case get_affiliation(LastUser, State) of
        owner ->
            ItemAttrs = [{<<"affiliation">>, <<"owner">>}, {<<"role">>, <<"none">>}],
            Packet = unavailable_presence(ItemAttrs, <<"Room hibernation">>),
            FromRoom = jid:replace_resource(RoomJID, Nick),
            ejabberd_router:route(FromRoom, LastUser, Packet),
            tab_remove_online_user(LJID, State),
            do_stop_persistent_room(RoomName, State);
        _ ->
            next_normal_state(State)
    end;
stop_if_only_owner_is_online(_, _, State) ->
    next_normal_state(State).

do_stop_persistent_room(RoomName, State) ->
    ?INFO_MSG("Stopping persistent room's process, ~p ~p", [self(), RoomName]),
    mongoose_metrics:update(global, [mod_muc, deep_hibernations], 1),
    {stop, normal, State}.

%% @doc Purpose: Shutdown the fsm
-spec terminate(any(), statename(), state()) -> 'ok'.
terminate(Reason, _StateName, StateData) ->
    ?INFO_MSG("Stopping MUC room ~s@~s",
          [StateData#state.room, StateData#state.host]),
    ReasonT = case Reason of
          shutdown -> <<"You are being removed from the room because of a system shutdown">>;
          _ -> <<"Room terminates">>
          end,
    ItemAttrs = [{<<"affiliation">>, <<"none">>}, {<<"role">>, <<"none">>}],
    Packet = unavailable_presence(ItemAttrs, ReasonT),
    maps_foreach(
      fun(LJID, Info) ->
              Nick = Info#user.nick,
              case Reason of
                  shutdown ->
                      ejabberd_router:route(
                        jid:replace_resource(StateData#state.jid, Nick),
                        Info#user.jid,
                        Packet);
                  _ -> ok
              end,
              tab_remove_online_user(LJID, StateData)
      end, StateData#state.users),
    add_to_log(room_existence, stopped, StateData),
    mod_muc:room_destroyed(StateData#state.host, StateData#state.room, self()),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

unavailable_presence(ItemAttrs, ReasonT) ->
    ReasonEl = #xmlel{name = <<"reason">>,
                      children = [#xmlcdata{content = ReasonT}]},
    #xmlel{name = <<"presence">>,
           attrs = [{<<"type">>, <<"unavailable">>}],
           children = [#xmlel{name = <<"x">>,
                              attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
                              children = [#xmlel{name = <<"item">>,
                                                 attrs = ItemAttrs,
                                                 children = [ReasonEl]},
                                          #xmlel{name = <<"status">>,
                                                 attrs = [{<<"code">>, <<"332">>}]}
                                         ]}]}.

-spec occupant_jid(user(), 'undefined' | jid:jid()) -> 'error' | jid:jid().
occupant_jid(#user{nick=Nick}, RoomJID) ->
    jid:replace_resource(RoomJID, Nick).


-spec route(atom() | pid() | port() | {atom(), _} | {'via', _, _},
    From :: jid:jid(), To :: mod_muc:nick(), Acc :: mongoose_acc:t(),
    Pkt :: exml:element()) -> 'ok'.
route(Pid, From, ToNick, Acc, Packet) ->
    gen_fsm_compat:send_event(Pid, {route, From, ToNick, Acc, Packet}).


-spec process_groupchat_message(jid:simple_jid() | jid:jid(),
                                exml:element(), state()) -> fsm_return().
process_groupchat_message(From, #xmlel{name = <<"message">>,
                                       attrs = Attrs} = Packet,
                          StateData) ->
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    case can_send_to_conference(From, StateData) of
        true ->
            process_message_from_allowed_user(From, Packet, StateData);
        false ->
            send_error_only_occupants(<<"messages">>, Packet, Lang,
                                      StateData#state.jid, From),
            next_normal_state(StateData)
    end.

can_send_to_conference(From, StateData) ->
    is_user_online(From, StateData)
    orelse
    is_allowed_nonparticipant(From, StateData).

can_read_conference(UserJID,
                    StateData=#state{config = #config{members_only = MembersOnly,
                                                      password_protected = Protected}}) ->
    Affiliation = get_affiliation(UserJID, StateData),
    %% In a members-only chat room, only owners, admins or members can query a room archive.
    case {MembersOnly, Protected} of
        {_, true} ->
            %% For querying password-protected room user should be a member
            %% or inside the room
            is_user_online(UserJID, StateData)
            orelse
            lists:member(Affiliation, [owner, admin, member]);
        {true, false} ->
            lists:member(Affiliation, [owner, admin, member]);
        {false, false} ->
            %% Outcast (banned) cannot read
            Affiliation =/= outcast
    end.

can_user_access_identity(UserJID, StateData) ->
    is_room_non_anonymous(StateData)
    orelse
    is_user_moderator(UserJID, StateData).

is_room_non_anonymous(StateData) ->
    not is_room_anonymous(StateData).

is_room_anonymous(#state{config = #config{anonymous = IsAnon}}) ->
    IsAnon.

is_user_moderator(UserJID, StateData) ->
    get_role(UserJID, StateData) =:= moderator.

process_message_from_allowed_user(From, #xmlel{attrs = Attrs} = Packet,
                                  StateData) ->
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    {FromNick, Role} = get_participant_data(From, StateData),
    CanSendBroadcasts = can_send_broadcasts(Role, StateData),
    case CanSendBroadcasts of
        true ->
            {NewState, Changed} = change_subject_if_allowed(FromNick, Role,
                                                            Packet, StateData),
            case Changed of
                true ->
                    broadcast_room_packet(From, FromNick, Role, Packet, NewState);
                false ->
                    change_subject_error(From, FromNick, Packet, Lang, NewState),
                    next_normal_state(NewState)
            end;
        false ->
            ErrText = <<"Visitors are not allowed to send messages to all occupants">>,
            Err = jlib:make_error_reply(Packet, mongoose_xmpp_errors:forbidden(Lang, ErrText)),
            ejabberd_router:route(StateData#state.jid, From, Err),
            next_normal_state(StateData)
    end.

can_send_broadcasts(Role, StateData) ->
    (Role == moderator)
    or (Role == participant)
    or ((StateData#state.config)#config.moderated == false).

broadcast_room_packet(From, FromNick, Role, Packet, StateData) ->
    Affiliation = get_affiliation(From, StateData),
    EventData = [{from_nick, FromNick},
                 {from_jid, From},
                 {room_jid, StateData#state.jid},
                 {role, Role},
                 {affiliation, Affiliation}],
    case ejabberd_hooks:run_fold(filter_room_packet,
                                 StateData#state.host, Packet, [EventData])
    of
        drop ->
            next_normal_state(StateData);
        FilteredPacket ->
            ejabberd_hooks:run(room_send_packet, StateData#state.host, [FilteredPacket, EventData]),
            RouteFrom = jid:replace_resource(StateData#state.jid,
                                             FromNick),
            RoomJid = StateData#state.jid,
            HookInfo = #{room_jid => RoomJid,
                         from_jid => From,
                         from_room_jid => RouteFrom,
                         packet => FilteredPacket,
                         affiliations_map => StateData#state.affiliations},
            run_update_inbox_for_muc_hook(StateData#state.server_host, HookInfo),
            maps_foreach(fun(_LJID, Info) ->
                                  ejabberd_router:route(RouteFrom,
                                                        Info#user.jid,
                                                        FilteredPacket)
                          end, StateData#state.users),
            NewStateData2 = add_message_to_history(FromNick,
                                                   From,
                                                   FilteredPacket,
                                                   StateData),
            next_normal_state(NewStateData2)
    end.

-spec run_update_inbox_for_muc_hook(jid:server(),
                                    update_inbox_for_muc_payload()) -> ok.
run_update_inbox_for_muc_hook(ServerHost, HookInfo) ->
    ejabberd_hooks:run_fold(update_inbox_for_muc,
                            ServerHost, HookInfo, []),
    ok.

change_subject_error(From, FromNick, Packet, Lang, StateData) ->
    Err = case (StateData#state.config)#config.allow_change_subj of
              true -> mongoose_xmpp_errors:forbidden(Lang, <<"Only moderators and participants are allowed"
                                              " to change the subject in this room">>);
              _ -> mongoose_xmpp_errors:forbidden(Lang, <<"Only moderators are allowed"
                                           " to change the subject in this room">>)
          end,
    ejabberd_router:route(jid:replace_resource(StateData#state.jid,
                                               FromNick),
                          From,
                          jlib:make_error_reply(Packet, Err)).

change_subject_if_allowed(FromNick, Role, Packet, StateData) ->
    case check_subject(Packet) of
        false ->
            {StateData, true};
        Subject ->
            case can_change_subject(Role, StateData) of
                true ->
                    NSD = StateData#state{subject = Subject,
                                          subject_author = FromNick},
                    save_persistent_room_state(NSD),
                    {NSD, true};
                _ ->
                    {StateData, false}
            end
    end.

save_persistent_room_state(StateData) ->
    case (StateData#state.config)#config.persistent of
        true ->
            mod_muc:store_room(StateData#state.server_host,
                               StateData#state.host,
                               StateData#state.room,
                               make_opts(StateData));
        _ ->
            ok
    end.

%% @doc Check if this non participant can send message to room.
%%
%% XEP-0045 v1.23:
%% 7.9 Sending a Message to All Occupants
%% an implementation MAY allow users with certain privileges
%% (e.g., a room owner, room admin, or service-level admin)
%% to send messages to the room even if those users are not occupants.
-spec is_allowed_nonparticipant(jid:jid(), state()) -> boolean().
is_allowed_nonparticipant(JID, StateData) ->
    get_service_affiliation(JID, StateData) =:= owner.

%% @doc Get information of this participant, or default values.
%% If the JID is not a participant, return values for a service message.
-spec get_participant_data(jid:simple_jid() | jid:jid(), state()) -> {_, _}.
get_participant_data(From, StateData) ->
    case maps:find(jid:to_lower(From), StateData#state.users) of
        {ok, #user{nick = FromNick, role = Role}} ->
            {FromNick, Role};
        error ->
            {<<>>, moderator}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Presence processing

%% @doc Process presence stanza and destroy the room, if it is empty.
-spec process_presence(From :: jid:jid(), Nick :: mod_muc:nick(),
                       Packet :: exml:element(), state()) -> fsm_return().
process_presence(From, ToNick, Presence, StateData) ->
    StateData1 = process_presence1(From, ToNick, Presence, StateData),
    destroy_temporary_room_if_empty(StateData1, normal_state).


-spec process_presence(From :: jid:jid(), Nick :: mod_muc:nick(),
        Presence :: exml:element(), state(), statename()) -> fsm_return().
process_presence(From, ToNick, Presence, StateData, NextState) ->
    StateData1 = process_presence(From, ToNick, Presence, StateData),
    rewrite_next_state(NextState, StateData1).


-spec rewrite_next_state(statename(), fsm_return()) -> fsm_return().
rewrite_next_state(NewState, {next_state, _, StateData, Timeout}) ->
    {next_state, NewState, StateData, Timeout};
rewrite_next_state(NewState, {next_state, _, StateData}) ->
    {next_state, NewState, StateData};
rewrite_next_state(_, {stop, normal, StateData}) ->
    {stop, normal, StateData}.


-spec destroy_temporary_room_if_empty(state(), atom()) -> fsm_return().
destroy_temporary_room_if_empty(StateData=#state{config=C=#config{}}, NextState) ->
    case (not C#config.persistent) andalso is_empty_room(StateData)
        andalso StateData#state.http_auth_pids =:= [] of
        true ->
            ?INFO_MSG("Destroyed MUC room ~s because it's temporary and empty",
                  [jid:to_binary(StateData#state.jid)]),
            add_to_log(room_existence, destroyed, StateData),
            {stop, normal, StateData};
        _ ->
            case NextState of
                normal_state ->
                    next_normal_state(StateData);
                _ ->
                    {next_state, NextState, StateData}
            end
    end.

next_normal_state(#state{hibernate_timeout = Timeout} = StateData) ->
    {next_state, normal_state, StateData, Timeout}.

-spec process_presence1(From, Nick, Packet, state()) -> state() when
      From :: jid:jid(),
      Nick :: mod_muc:nick(),
      Packet :: exml:element().
process_presence1(From, Nick, #xmlel{name = <<"presence">>, attrs = Attrs} = Packet,
                  StateData = #state{}) ->
    Type = xml:get_attr_s(<<"type">>, Attrs),
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    case Type of
        <<"unavailable">> ->
            process_presence_unavailable(From, Packet, StateData);
        <<"error">> ->
            process_presence_error(From, Packet, Lang, StateData);
        <<>> ->
            case is_new_nick_of_online_user(From, Nick, StateData) of
                true ->
                    process_presence_nick_change(From, Nick, Packet, Lang, StateData);
                false ->
                    process_simple_presence(From, Packet, StateData);
                user_is_offline ->
                    %% at this point we know that the presence has no type
                    %% (user wants to enter the room)
                    %% and that the user is not alredy online
                    handle_new_user(From, Nick, Packet, StateData, Attrs)
            end;
        _NotOnline ->
            StateData
    end.


-spec process_simple_presence(jid:jid(), exml:element(), state()) -> state().
process_simple_presence(From, Packet, StateData) ->
    NewPacket = check_and_strip_visitor_status(From, Packet, StateData),
    NewState = add_user_presence(From, NewPacket, StateData),
    send_new_presence(From, NewState),
    NewState.


-spec process_presence_error(jid:simple_jid() | jid:jid(),
                             exml:element(), ejabberd:lang(), state()) -> state().
process_presence_error(From, Packet, Lang, StateData) ->
    case is_user_online(From, StateData) of
        true ->
            ErrorText
            = <<"This participant is kicked from the room because he sent an error presence">>,
            expulse_participant(Packet, From, StateData, translate:translate(Lang, ErrorText));
        _ ->
            StateData
    end.


-spec process_presence_unavailable(jid:jid(), exml:element(), state())
                                    -> state().
process_presence_unavailable(From, Packet, StateData) ->
    case is_user_online(From, StateData) of
        true ->
            NewPacket = check_and_strip_visitor_status(From, Packet, StateData),
            NewState = add_user_presence_un(From, NewPacket, StateData),
            send_new_presence_un(From, NewState),
            Reason = case xml:get_subtag(NewPacket, <<"status">>) of
                false -> <<>>;
                StatusEl -> xml:get_tag_cdata(StatusEl)
            end,
            remove_online_user(From, NewState, Reason);
        _ ->
            StateData
    end.


-spec choose_nick_change_strategy(jid:jid(), binary(), state())
    -> 'allowed' | 'conflict_registered' | 'conflict_use' | 'not_allowed_visitor'.
choose_nick_change_strategy(From, Nick, StateData) ->
    case {is_nick_exists(Nick, StateData),
          mod_muc:can_use_nick(StateData#state.server_host, StateData#state.host, From, Nick),
          (StateData#state.config)#config.allow_visitor_nickchange,
          is_visitor(From, StateData)} of
        {_, _, false, true} ->
            not_allowed_visitor;
        {true, _, _, _} ->
            conflict_use;
        {_, false, _, _} ->
            conflict_registered;
        _ ->
            allowed
    end.


-spec process_presence_nick_change(jid:jid(), mod_muc:nick(), exml:element(),
        ejabberd:lang(), state()) -> state().
process_presence_nick_change(From, Nick, Packet, Lang, StateData) ->
    case choose_nick_change_strategy(From, Nick, StateData) of
        not_allowed_visitor ->
            ErrText = <<"Visitors are not allowed to change their nicknames in this room">>,
            Err = jlib:make_error_reply(Packet, mongoose_xmpp_errors:not_allowed(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        conflict_use ->
            ErrText = <<"That nickname is already in use by another occupant">>,
            Err = jlib:make_error_reply(Packet, mongoose_xmpp_errors:conflict(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        conflict_registered ->
            ErrText = <<"That nickname is registered by another person">>,
            Err = jlib:make_error_reply(Packet, mongoose_xmpp_errors:conflict(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        allowed ->
            change_nick(From, Nick, StateData)
    end.


-spec check_and_strip_visitor_status(jid:jid(), exml:element(), state())
                                        -> exml:element().
check_and_strip_visitor_status(From, Packet, StateData) ->
    case {(StateData#state.config)#config.allow_visitor_status,
          is_visitor(From, StateData)} of
        {false, true} ->
            strip_status(Packet);
        _ ->
            Packet
    end.


-spec handle_new_user(jid:jid(), mod_muc:nick(), exml:element(), state(),
                      [{binary(), binary()}]) -> state().
handle_new_user(From, Nick = <<>>, _Packet, StateData, Attrs) ->
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    ErrText = <<"No nickname">>,
    Error =jlib:make_error_reply(
                #xmlel{name = <<"presence">>},
                mongoose_xmpp_errors:jid_malformed(Lang, ErrText)),
    %ejabberd_route(From, To, Packet),
    ejabberd_router:route(jid:replace_resource(StateData#state.jid, Nick), From, Error),
    StateData;
handle_new_user(From, Nick, Packet, StateData, _Attrs) ->
    add_new_user(From, Nick, Packet, StateData).


-spec is_user_online(jid:simple_jid() | jid:jid(), state()) -> boolean().
is_user_online(JID, StateData) ->
    LJID = jid:to_lower(JID),
    maps:is_key(LJID, StateData#state.users).


%% @doc Check if the user is occupant of the room, or at least is an admin
%% or owner.
-spec is_occupant_or_admin(jid:jid(), state()) -> boolean().
is_occupant_or_admin(JID, StateData) ->
    FAffiliation = get_affiliation(JID, StateData),
    FRole = get_role(JID, StateData),
    (FRole /= none) orelse
    (FAffiliation == admin) orelse
    (FAffiliation == owner).

%%%
%%% Handle IQ queries of vCard
%%%

-spec is_user_online_iq(_, jid:jid(), state())
            -> {'false', _, jid:jid()} | {'true', _, jid:jid()}.
is_user_online_iq(StanzaId, JID, StateData) when JID#jid.lresource /= <<>> ->
    {is_user_online(JID, StateData), StanzaId, JID};
is_user_online_iq(StanzaId, JID, StateData) when JID#jid.lresource == <<>> ->
    try stanzaid_unpack(StanzaId) of
        {OriginalId, Resource} ->
            JIDWithResource = jid:replace_resource(JID, Resource),
            {is_user_online(JIDWithResource, StateData),
             OriginalId, JIDWithResource}
    catch
        _:_ ->
            {is_user_online(JID, StateData), StanzaId, JID}
    end.


-spec handle_iq_vcard(jid:jid(), jid:simple_jid() | jid:jid(),
                      binary(), any(), exml:element()) ->
                {jid:simple_jid() | jid:jid(), exml:element()}.
handle_iq_vcard(FromFull, ToJID, StanzaId, NewId, Packet) ->
    ToBareJID = jid:to_bare(ToJID),
    IQ = jlib:iq_query_info(Packet),
    handle_iq_vcard2(FromFull, ToJID, ToBareJID, StanzaId, NewId, IQ, Packet).


-spec handle_iq_vcard2(FromFull :: jid:jid(),
        ToJID :: jid:simple_jid() | jid:jid(),
        ToBareJID :: jid:simple_jid() | jid:jid(),
        binary(), _NewID, 'invalid' | 'not_iq' | 'reply' | jlib:iq(),
        exml:element()) -> {jid:simple_jid() | jid:jid(), exml:element()}.
handle_iq_vcard2(_FromFull, ToJID, ToBareJID, StanzaId, _NewId,
         #iq{type = get, xmlns = ?NS_VCARD}, Packet)
  when ToBareJID /= ToJID ->
    {ToBareJID, change_stanzaid(StanzaId, ToJID, Packet)};
handle_iq_vcard2(_FromFull, ToJID, _ToBareJID, _StanzaId, NewId, _IQ, Packet) ->
    {ToJID, change_stanzaid(NewId, Packet)}.


-spec stanzaid_pack(binary(), jid:resource()) -> binary().
stanzaid_pack(OriginalId, Resource) ->
    Data64 = base64:encode(<<"ejab\0", OriginalId/binary, 0, Resource/binary>>),
    <<"berd", Data64/binary>>.


-spec stanzaid_unpack(binary()) -> stanzaid().
stanzaid_unpack(<<"berd", StanzaIdBase64/binary>>) ->
    StanzaId = base64:decode(StanzaIdBase64),
    [<<"ejab">>, OriginalId, Resource] = binary:split(StanzaId, <<"\0">>),
    {OriginalId, Resource}.


-spec change_stanzaid(binary(), exml:element()) -> exml:element().
change_stanzaid(NewId, Packet) ->
    XE = #xmlel{attrs = Attrs} = jlib:remove_attr(<<"id">>, Packet),
    XE#xmlel{attrs = [{<<"id">>, NewId} | Attrs]}.
change_stanzaid(PreviousId, ToJID, Packet) ->
    NewId = stanzaid_pack(PreviousId, ToJID#jid.lresource),
    change_stanzaid(NewId, Packet).

%%%
%%%

-spec role_to_binary(mod_muc:role()) -> binary().
role_to_binary(Role) ->
    case Role of
        moderator   -> <<"moderator">>;
        participant -> <<"participant">>;
        visitor     -> <<"visitor">>;
        none        -> <<"none">>
    end.

-spec affiliation_to_binary(mod_muc:affiliation()) -> binary().
affiliation_to_binary(Affiliation) ->
    case Affiliation of
        owner   -> <<"owner">>;
        admin   -> <<"admin">>;
        member  -> <<"member">>;
        outcast -> <<"outcast">>;
        none    -> <<"none">>
    end.

-spec binary_to_role(binary()) -> mod_muc:role().
binary_to_role(Role) ->
    case Role of
        <<"moderator">>     -> moderator;
        <<"participant">>   -> participant;
        <<"visitor">>       -> visitor;
        <<"none">>          -> none
    end.

-spec binary_to_affiliation(binary()) -> mod_muc:affiliation().
binary_to_affiliation(Affiliation) ->
    case Affiliation of
        <<"owner">>     -> owner;
        <<"admin">>     -> admin;
        <<"member">>    -> member;
        <<"outcast">>   -> outcast;
        <<"none">>      -> none
    end.


%% @doc Decide the fate of the message and its sender
%% Returns: continue_delivery | forget_message | {expulse_sender, Reason}
-spec decide_fate_message(binary(), exml:element(), jid:simple_jid() | jid:jid(),
        state()) -> 'continue_delivery'
                  | 'forget_message'
                  | {'expulse_sender', string()}.
decide_fate_message(<<"error">>, Packet, From, StateData) ->
    %% Make a preliminary decision
    PD = case check_error_kick(Packet) of
         %% If this is an error stanza and its condition matches a criteria
         true ->
         Reason = "This participant is considered a ghost and is expulsed: " ++
            binary_to_list(jid:to_binary(From)),
         {expulse_sender, Reason};
         false ->
         continue_delivery
     end,
    case PD of
    {expulse_sender, R} ->
        case is_user_online(From, StateData) of
        true ->
            {expulse_sender, R};
        false ->
            forget_message
        end;
    Other ->
        Other
    end;
decide_fate_message(_, _, _, _) ->
    continue_delivery.


%% @doc Check if the elements of this error stanza indicate
%% that the sender is a dead participant.
%% If so, return true to kick the participant.
-spec check_error_kick(exml:element()) -> boolean().
check_error_kick(Packet) ->
    case get_error_condition(Packet) of
        <<"gone">>                      -> true;
        <<"internal-server-error">>     -> true;
        <<"item-not-found">>            -> true;
        <<"jid-malformed">>             -> true;
        <<"recipient-unavailable">>     -> true;
        <<"redirect">>                  -> true;
        <<"remote-server-not-found">>   -> true;
        <<"remote-server-timeout">>     -> true;
        <<"service-unavailable">>       -> true;
        _                               -> false
    end.


-spec get_error_condition(exml:element()) -> binary().
get_error_condition(Packet) ->
    case catch get_error_condition2(Packet) of
        {condition, ErrorCondition} ->
            ErrorCondition;
        {'EXIT', _} ->
            <<"badformed error stanza">>
    end.


-spec get_error_condition2(exml:element()) -> {condition, binary()}.
get_error_condition2(Packet) ->
    #xmlel{children = EEls} = xml:get_subtag(Packet, <<"error">>),
    [Condition] = [Name || #xmlel{name = Name,
                                  attrs = [{<<"xmlns">>, ?NS_STANZAS}],
                                  children = []} <- EEls],
    {condition, Condition}.


-spec expulse_participant(exml:element(), jid:jid(), state(), binary()) -> state().
expulse_participant(Packet, From, StateData, Reason1) ->
    ErrorCondition = get_error_condition(Packet),
    Reason2 = <<Reason1/binary, ": ", ErrorCondition/binary>>,
    NewState = add_user_presence_un(
        From,
        #xmlel{name = <<"presence">>, attrs = [{<<"type">>, <<"unavailable">>}],
               children = [#xmlel{name = <<"status">>,
                                  children = [#xmlcdata{content = Reason2}]}]},
    StateData),
    send_new_presence_un(From, NewState),
    remove_online_user(From, NewState).


-spec access_admin(state()) -> any().
access_admin(#state{access=Access}) ->
    {_AccessRoute, _AccessCreate, AccessAdmin, _AccessPersistent} = Access,
    AccessAdmin.


-spec access_persistent(state()) -> any().
access_persistent(#state{access=Access}) ->
    {_AccessRoute, _AccessCreate, _AccessAdmin, AccessPersistent} = Access,
    AccessPersistent.


-spec set_affiliation(jid:jid(), mod_muc:affiliation(), state()) -> state().
set_affiliation(JID, Affiliation, StateData)
        when is_atom(Affiliation) ->
    LJID = jid:to_bare(jid:to_lower(JID)),
    Affiliations = case Affiliation of
               none -> maps:remove(LJID, StateData#state.affiliations);
               _ -> maps:put(LJID, Affiliation, StateData#state.affiliations)
           end,
    StateData#state{affiliations = Affiliations}.


-spec set_affiliation_and_reason(jid:jid(), mod_muc:affiliation(), term(),
                                 state()) -> state().
set_affiliation_and_reason(JID, Affiliation, Reason, StateData)
        when is_atom(Affiliation) ->
    LJID = jid:to_bare(jid:to_lower(JID)),
    Affiliations = case Affiliation of
               none -> maps:remove(LJID, StateData#state.affiliations);
               _ -> maps:put(LJID, {Affiliation, Reason}, StateData#state.affiliations)
           end,
    StateData#state{affiliations = Affiliations}.


-spec get_affiliation(jid:jid(), state()) -> mod_muc:affiliation().
get_affiliation(JID, StateData) ->
    AccessAdmin = access_admin(StateData),
    case acl:match_rule(StateData#state.server_host, AccessAdmin, JID) of
        allow ->
            owner;
        _ ->
            LJID = jid:to_lower(JID),
            LJID1 = jid:to_bare(LJID),
            LJID2 = setelement(1, LJID, <<>>),
            LJID3 = jid:to_bare(LJID2),
            lookup_affiliation([ LJID, LJID1, LJID2, LJID3 ], StateData#state.affiliations)
    end.

-spec lookup_affiliation(JIDs :: [jid:simple_jid()],
                         Affiliations :: affiliations_map()) ->
    mod_muc:affiliation().
lookup_affiliation([ JID | RJIDs ], Affiliations) ->
    case maps:find(JID, Affiliations) of
        {ok, {Affiliation, _Reason}} -> Affiliation;
        {ok, Affiliation} -> Affiliation;
        _ -> lookup_affiliation(RJIDs, Affiliations)
    end;
lookup_affiliation([], _Affiliations) ->
    none.

-spec get_service_affiliation(jid:jid(), state()) -> mod_muc:affiliation().
get_service_affiliation(JID, StateData) ->
    AccessAdmin = access_admin(StateData),
    case acl:match_rule(StateData#state.server_host, AccessAdmin, JID) of
    allow ->
        owner;
    _ ->
        none
    end.


-spec set_role(JID :: jid:jid(), Role :: mod_muc:role(), state()) -> state().
set_role(JID, none, StateData) ->
    erase_matched_users(JID, StateData);
set_role(JID, Role, StateData) ->
    update_matched_users(fun(User) -> User#user{role = Role} end,
                         JID, StateData).


-spec get_role( jid:jid(), state()) -> mod_muc:role().
get_role(JID, StateData) ->
    LJID = jid:to_lower(JID),
    case maps:find(LJID, StateData#state.users) of
        {ok, #user{role = Role}} -> Role;
        _ -> none
    end.


-spec get_default_role(mod_muc:affiliation(), state()) -> mod_muc:role().
get_default_role(owner, _StateData) -> moderator;
get_default_role(admin, _StateData) -> moderator;
get_default_role(member, _StateData) -> participant;
get_default_role(outcast, _StateData) -> none;
get_default_role(none, StateData) ->
    case (StateData#state.config)#config.members_only of
        true -> none;
        _ ->
            case (StateData#state.config)#config.members_by_default of
                true -> participant;
                _ -> visitor
            end
    end.


-spec is_visitor(jid:jid(), state()) -> boolean().
is_visitor(Jid, StateData) ->
    get_role(Jid, StateData) =:= visitor.


-spec is_empty_room(state()) -> boolean().
is_empty_room(#state{users=Users}) ->
    is_empty_map(Users).


-spec is_empty_map(map()) -> boolean().
is_empty_map(Map) ->
    maps:size(Map) =:= 0.


-spec map_foreach_value(fun((_) -> ok), users_map()) -> any().
map_foreach_value(F, Map) ->
    maps:fold(fun(_Key, Value, _) -> F(Value) end, ok, Map).


-spec count_users(state()) -> non_neg_integer().
count_users(#state{users=Users}) ->
    maps:size(Users).


-spec get_max_users(state()) -> integer() | none.
get_max_users(StateData) ->
    MaxUsers = (StateData#state.config)#config.max_users,
    ServiceMaxUsers = get_service_max_users(StateData),
    case MaxUsers =< ServiceMaxUsers of
        true -> MaxUsers;
        false -> ServiceMaxUsers
    end.


-spec get_service_max_users(state()) -> integer() | none.
get_service_max_users(StateData) ->
    gen_mod:get_module_opt(StateData#state.server_host,
               mod_muc, max_users, ?MAX_USERS_DEFAULT).


-spec get_max_users_admin_threshold(state()) -> integer().
get_max_users_admin_threshold(StateData) ->
    gen_mod:get_module_opt(StateData#state.server_host,
               mod_muc, max_users_admin_threshold, 5).


-spec get_user_activity(jid:simple_jid() | jid:jid(), state())
                        -> activity().
get_user_activity(JID, StateData) ->
    case treap:lookup(jid:to_lower(JID),
              StateData#state.activity) of
    {ok, _P, A} -> A;
    error ->
        MessageShaper =
        shaper:new(gen_mod:get_module_opt(
                 StateData#state.server_host,
                 mod_muc, user_message_shaper, none)),
        PresenceShaper =
        shaper:new(gen_mod:get_module_opt(
                 StateData#state.server_host,
                 mod_muc, user_presence_shaper, none)),
        #activity{message_shaper = MessageShaper,
              presence_shaper = PresenceShaper}
    end.


-spec store_user_activity(jid:simple_jid() | jid:jid(), activity(),
                         state()) -> state().
store_user_activity(JID, UserActivity, StateData) ->
    MinMessageInterval =
    gen_mod:get_module_opt(
      StateData#state.server_host,
      mod_muc, min_message_interval, 0),
    MinPresenceInterval =
    gen_mod:get_module_opt(
      StateData#state.server_host,
      mod_muc, min_presence_interval, 0),
    Key = jid:to_lower(JID),
    Now = os:system_time(microsecond),
    Activity1 = clean_treap(StateData#state.activity, {1, -Now}),
    Activity =
    case treap:lookup(Key, Activity1) of
        {ok, _P, _A} ->
        treap:delete(Key, Activity1);
        error ->
        Activity1
    end,
    StateData1 =
    case (MinMessageInterval == 0) andalso
        (MinPresenceInterval == 0) andalso
        (UserActivity#activity.message_shaper == none) andalso
        (UserActivity#activity.presence_shaper == none) andalso
        (UserActivity#activity.message == undefined) andalso
        (UserActivity#activity.presence == undefined) of
        true ->
        StateData#state{activity = Activity};
        false ->
        case (UserActivity#activity.message == undefined) andalso
            (UserActivity#activity.presence == undefined) of
            true ->
            {_, MessageShaperInterval} =
                shaper:update(UserActivity#activity.message_shaper,
                      100000),
            {_, PresenceShaperInterval} =
                shaper:update(UserActivity#activity.presence_shaper,
                      100000),
            Delay = lists:max([MessageShaperInterval,
                       PresenceShaperInterval,
                       MinMessageInterval * 1000,
                       MinPresenceInterval * 1000]) * 1000,
            Priority = {1, -(Now + Delay)},
            StateData#state{
              activity = treap:insert(
                       Key,
                       Priority,
                       UserActivity,
                       Activity)};
            false ->
            Priority = {0, 0},
            StateData#state{
              activity = treap:insert(
                       Key,
                       Priority,
                       UserActivity,
                       Activity)}
        end
    end,
    StateData1.


-spec clean_treap(treap:treap(), {1, integer()}) -> treap:treap().
clean_treap(Treap, CleanPriority) ->
    case treap:is_empty(Treap) of
        true ->
            Treap;
        false ->
            {_Key, Priority, _Value} = treap:get_root(Treap),
            case Priority > CleanPriority of
                true -> clean_treap(treap:delete_root(Treap), CleanPriority);
                false -> Treap
            end
    end.


-spec prepare_room_queue(state()) -> state().
prepare_room_queue(StateData) ->
    case queue:out(StateData#state.room_queue) of
    {{value, {message, From}}, _RoomQueue} ->
        Activity = get_user_activity(From, StateData),
        Packet = Activity#activity.message,
        Size = element_size(Packet),
        {RoomShaper, RoomShaperInterval} =
        shaper:update(StateData#state.room_shaper, Size),
        erlang:send_after(
          RoomShaperInterval, self(),
          process_room_queue),
        StateData#state{
          room_shaper = RoomShaper};
    {{value, {presence, From}}, _RoomQueue} ->
        Activity = get_user_activity(From, StateData),
        {_Nick, Packet} = Activity#activity.presence,
        Size = element_size(Packet),
        {RoomShaper, RoomShaperInterval} =
        shaper:update(StateData#state.room_shaper, Size),
        erlang:send_after(
          RoomShaperInterval, self(),
          process_room_queue),
        StateData#state{
          room_shaper = RoomShaper};
    {empty, _} ->
        StateData
    end.

-spec is_first_session(mod_muc:nick(), state()) -> boolean().
is_first_session(Nick, StateData) ->
    case maps:find(Nick, StateData#state.sessions) of
        {ok, _Val} -> false;
        error -> true
    end.

-spec is_last_session(mod_muc:nick(), state()) -> boolean().
is_last_session(Nick, StateData) ->
    case maps:find(Nick, StateData#state.sessions) of
        {ok, [_Val]} -> true;
        _ -> false
    end.

-spec add_online_user(jid:jid(), mod_muc:nick(), mod_muc:role(), state())
                        -> state().
add_online_user(JID, Nick, Role, StateData) ->
    LJID = jid:to_lower(JID),
    Sessions = maps_append(Nick, JID, StateData#state.sessions),
    Info = #user{jid = JID,
                 nick = Nick,
                 role = Role},
    Users = maps:put(LJID, Info, StateData#state.users),
    case is_first_session(Nick, StateData) of
        true ->
            add_to_log(join, Nick, StateData),
            tab_add_online_user(JID, StateData),
            run_join_room_hook(JID, StateData);
        _ ->
            ok
    end,
    notify_users_modified(StateData#state{users = Users, sessions = Sessions}).

-spec run_join_room_hook(jid:jid(), state()) -> ok.
run_join_room_hook(JID, #state{room = Room, host = Host, jid = MucJID, server_host = ServerHost}) ->
  ejabberd_hooks:run(join_room, ServerHost, [ServerHost, Room, Host, JID, MucJID]),
  ok.

-spec remove_online_user(jid:jid(), state()) -> state().
remove_online_user(JID, StateData) ->
    remove_online_user(JID, StateData, <<>>).


-spec remove_online_user(jid:jid(), state(), Reason :: binary()) -> state().
remove_online_user(JID, StateData, Reason) ->

    LJID = jid:to_lower(JID),
    {ok, #user{nick = Nick}} =
        maps:find(LJID, StateData#state.users),
    Sessions = case is_last_session(Nick, StateData) of
        true ->
            add_to_log(leave, {Nick, Reason}, StateData),
            tab_remove_online_user(JID, StateData),
            run_leave_room_hook(JID, StateData),
            maps:remove(Nick, StateData#state.sessions);
        false ->
            IsOtherLJID = fun(J) -> jid:to_lower(J) /= LJID end,
            F = fun (JIDs) -> lists:filter(IsOtherLJID, JIDs) end,
            maps:update_with(Nick, F, StateData#state.sessions)
    end,
    Users = maps:remove(LJID, StateData#state.users),

    notify_users_modified(StateData#state{users = Users, sessions = Sessions}).

-spec run_leave_room_hook(jid:jid(), state()) -> ok.
run_leave_room_hook(JID, #state{room = Room, host = Host, jid = MucJID, server_host = ServerHost}) ->
  ejabberd_hooks:run(leave_room, ServerHost, [ServerHost, Room, Host, JID, MucJID]),
  ok.

-spec filter_presence(exml:element()) -> exml:element().
filter_presence(#xmlel{name = <<"presence">>, attrs = Attrs, children = Els}) ->
    FEls = lists:filter(
             fun(#xmlcdata{}) ->
                     false;
                (#xmlel{attrs = Attrs1}) ->
                     XMLNS = xml:get_attr_s(<<"xmlns">>, Attrs1),
                     case XMLNS of
                         <<?NS_MUC_S, _/binary>> -> false;
                         _ -> true
                     end
             end, Els),
    #xmlel{name = <<"presence">>, attrs = Attrs, children = FEls}.


-spec strip_status(exml:element()) -> exml:element().
strip_status(#xmlel{name = <<"presence">>, attrs = Attrs,
                    children = Els}) ->
    FEls = lists:filter(
         fun(#xmlel{name = <<"status">>}) ->
                     false;
                (_) -> true
         end, Els),
    #xmlel{name = <<"presence">>, attrs = Attrs, children = FEls}.


-spec add_user_presence(jid:jid(), exml:element(), state()) -> state().
add_user_presence(JID, Presence, StateData) ->
    LJID = jid:to_lower(JID),
    FPresence = filter_presence(Presence),
    Users =
    maps:update_with(
      LJID,
      fun(#user{} = User) ->
              User#user{last_presence = FPresence}
      end, StateData#state.users),
    notify_users_modified(StateData#state{users = Users}).


-spec add_user_presence_un(jid:simple_jid() | jid:jid(), exml:element(),
                        state()) -> state().
add_user_presence_un(JID, Presence, StateData) ->
    LJID = jid:to_lower(JID),
    FPresence = filter_presence(Presence),
    Users =
    maps:update_with(
      LJID,
      fun(#user{} = User) ->
              User#user{last_presence = FPresence, role = none}
      end, StateData#state.users),
    notify_users_modified(StateData#state{users = Users}).


-spec is_nick_exists(mod_muc:nick(), state()) -> boolean().
is_nick_exists(Nick, StateData) ->
    maps:is_key(Nick, StateData#state.sessions).


-spec find_jids_by_nick(mod_muc:nick(), state()) -> [jid:jid()].
find_jids_by_nick(Nick, StateData) ->
    case maps:find(Nick, StateData#state.sessions) of
        error -> [];
        {ok, JIDs} -> JIDs
    end.

-spec is_new_nick_of_online_user(jid:simple_jid() | jid:jid(), mod_muc:nick(),
                                 state()) -> boolean() | user_is_offline.
is_new_nick_of_online_user(JID, Nick, StateData) ->
    LJID = jid:to_lower(JID),
    case maps:find(LJID, StateData#state.users) of
        {ok, #user{nick = OldNick}} -> Nick /= <<>> andalso Nick /= OldNick;
        error -> user_is_offline
    end.

-spec is_user_limit_reached(jid:jid(), mod_muc:affiliation(), state()) -> boolean().
is_user_limit_reached(From, Affiliation, StateData) ->
    MaxUsers = get_max_users(StateData),
    MaxAdminUsers = case MaxUsers of
                        none -> none;
                        _ -> MaxUsers + get_max_users_admin_threshold(StateData)
                    end,
    NUsers = count_users(StateData),
    ServiceAffiliation = get_service_affiliation(From, StateData),
    NConferences = tab_count_user(From),
    MaxConferences = gen_mod:get_module_opt(
               StateData#state.server_host,
               mod_muc, max_user_conferences, 10),
    (ServiceAffiliation == owner orelse
       MaxUsers == none orelse
       ((Affiliation == admin orelse Affiliation == owner) andalso
        NUsers < MaxAdminUsers) orelse
       NUsers < MaxUsers) andalso
      NConferences < MaxConferences.

is_next_session_of_occupant(From, Nick, StateData) ->
  IsAllowed = (StateData#state.config)#config.allow_multiple_sessions,
  case {IsAllowed, find_jids_by_nick(Nick, StateData)} of
    {false, _} ->
        false;
    {_, []} ->
        false;
    {true, Jids} ->
        lists:any(fun(Jid) ->
          From#jid.lserver == Jid#jid.lserver
          andalso From#jid.luser == Jid#jid.luser
        end, Jids)
  end.

-spec choose_new_user_strategy(jid:jid(), mod_muc:nick(),
        mod_muc:affiliation(), mod_muc:role(), [jlib:xmlcdata() | exml:element()],
        state()) -> new_user_strategy().
choose_new_user_strategy(From, Nick, Affiliation, Role, Els, StateData) ->
    case {is_user_limit_reached(From, Affiliation, StateData),
          is_nick_exists(Nick, StateData),
          is_next_session_of_occupant(From, Nick, StateData),
          mod_muc:can_use_nick(StateData#state.server_host, StateData#state.host, From, Nick),
          Role,
          Affiliation} of
        {false, _, _, _, _, _} ->
            limit_reached;
        {_, _, _, _, none, outcast} ->
            user_banned;
        {_, _, _, _, none, _} ->
            require_membership;
        {_, true, false, _, _, _} ->
            conflict_use;
        {_, _, _, false, _, _} ->
            conflict_registered;
        _ ->
            choose_new_user_password_strategy(From, Els, StateData)
    end.

-spec choose_new_user_password_strategy(jid:jid(), [jlib:xmlcdata() | exml:element()],
                                        state()) -> new_user_strategy().
choose_new_user_password_strategy(From, Els, StateData) ->
    ServiceAffiliation = get_service_affiliation(From, StateData),
    Config = StateData#state.config,
    case is_password_required(ServiceAffiliation, Config) of
        false -> allowed;
        true -> case extract_password(Els) of
                    false -> require_password;
                    Password -> check_password(StateData, Password)
                end
    end.

-spec add_new_user(jid:jid(), mod_muc:nick(), exml:element(), state()
                   ) -> state().
add_new_user(From, Nick, #xmlel{attrs = Attrs, children = Els} = Packet, StateData) ->
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    Affiliation = get_affiliation(From, StateData),
    Role = get_default_role(Affiliation, StateData),
    case choose_new_user_strategy(From, Nick, Affiliation, Role, Els, StateData) of
        limit_reached ->
            % max user reached and user is not admin or owner
            Err = jlib:make_error_reply(Packet, mongoose_xmpp_errors:service_unavailable_wait()),
            route_error(Nick, From, Err, StateData);
        user_banned ->
            ErrText = <<"You have been banned from this room">>,
            Err = jlib:make_error_reply(Packet, mongoose_xmpp_errors:forbidden(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        require_membership ->
            ErrText = <<"Membership is required to enter this room">>,
            Err = jlib:make_error_reply(
                Packet, mongoose_xmpp_errors:registration_required(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        conflict_use ->
            ErrText = <<"That nickname is already in use by another occupant">>,
            Err = jlib:make_error_reply(Packet, mongoose_xmpp_errors:conflict(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        conflict_registered ->
            ErrText = <<"That nickname is registered by another person">>,
            Err = jlib:make_error_reply(Packet, mongoose_xmpp_errors:conflict(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        require_password ->
            ErrText = <<"A password is required to enter this room">>,
            Err = jlib:make_error_reply(
                Packet, mongoose_xmpp_errors:not_authorized(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        invalid_password ->
            ErrText = <<"Incorrect password">>,
            Err = jlib:make_error_reply(
                Packet, mongoose_xmpp_errors:not_authorized(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        http_auth ->
            Password = extract_password(Els),
            perform_http_auth(From, Nick, Packet, Role, Password, StateData);
        allowed ->
            do_add_new_user(From, Nick, Packet, Role, StateData)
    end.

perform_http_auth(From, Nick, Packet, Role, Password, StateData) ->
    RoomPid = self(),
    RoomJid = StateData#state.jid,
    Pool = StateData#state.http_auth_pool,
    case is_empty_room(StateData) of
        true ->
            Result = make_http_auth_request(From, RoomJid, Password, Pool),
            handle_http_auth_result(Result, From, Nick, Packet, Role, StateData);
        false ->
            %% Perform the request in a separate process to prevent room freeze
            Pid = spawn_link(
                    fun() ->
                            Result = make_http_auth_request(From, RoomJid, Password, Pool),
                            gen_fsm_compat:send_event(RoomPid, {http_auth, self(), Result,
                                                         From, Nick, Packet, Role})
                    end),
            AuthPids = StateData#state.http_auth_pids,
            StateData#state{http_auth_pids = [Pid | AuthPids]}
    end.

make_http_auth_request(From, RoomJid, Password, Pool) ->
    FromVal = uri_encode(jid:to_binary(From)),
    RoomJidVal = uri_encode(jid:to_binary(RoomJid)),
    PassVal = uri_encode(Password),
    Path = <<"check_password?from=", FromVal/binary,
             "&to=", RoomJidVal/binary,
             "&pass=", PassVal/binary>>,
    case mongoose_http_client:get(global, Pool, Path, []) of
        {ok, {<<"200">>, Body}} -> decode_http_auth_response(Body);
        _ -> error
    end.

handle_http_auth_result(allowed, From, Nick, Packet, Role, StateData) ->
    do_add_new_user(From, Nick, Packet, Role, StateData);
handle_http_auth_result({invalid_password, ErrorMsg}, From, Nick, Packet, _Role, StateData) ->
    reply_not_authorized(From, Nick, Packet, StateData, ErrorMsg);
handle_http_auth_result(error, From, Nick, Packet, _Role, StateData) ->
    reply_service_unavailable(From, Nick, Packet, StateData, <<"Internal server error">>).

uri_encode(Bin) ->
    list_to_binary(http_uri:encode(binary_to_list(Bin))).

decode_http_auth_response(Body) ->
    try decode_json_auth_response(Body) of
        {0, _} ->
            allowed;
        {AuthCode, Msg} ->
            {invalid_password, iolist_to_binary([integer_to_list(AuthCode), $ , Msg])}
    catch
        error:_ -> error
    end.

decode_json_auth_response(Body) ->
    Elements = jiffy:decode(Body, [return_maps]),
    Code = maps:get(<<"code">>, Elements, undefined),
    Msg = maps:get(<<"msg">>, Elements, undefined),
    {Code, Msg}.

reply_not_authorized(From, Nick, Packet, StateData, ErrText) ->
    Lang = xml:get_attr_s(<<"xml:lang">>, Packet#xmlel.attrs),
    Err = jlib:make_error_reply(Packet, mongoose_xmpp_errors:not_authorized(Lang, ErrText)),
    route_error(Nick, From, Err, StateData).

reply_service_unavailable(From, Nick, Packet, StateData, ErrText) ->
    Lang = xml:get_attr_s(<<"xml:lang">>, Packet#xmlel.attrs),
    Err = jlib:make_error_reply(Packet, mongoose_xmpp_errors:service_unavailable(Lang, ErrText)),
    route_error(Nick, From, Err, StateData).

do_add_new_user(From, Nick, #xmlel{attrs = Attrs, children = Els} = Packet,
                Role, StateData) ->
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    NewState =
        add_user_presence(
          From, Packet,
          add_online_user(From, Nick, Role, StateData)),
    send_existing_presences(From, NewState),
    send_new_presence(From, NewState),
    Shift = count_stanza_shift(Nick, Els, NewState),
    case send_history(From, Shift, NewState) of
        true ->
            ok;
        _ ->
            send_subject(From, Lang, StateData)
    end,
    case NewState#state.just_created of
        true ->
            NewState#state{just_created = false};
        false ->
            Robots = maps:remove(From, StateData#state.robots),
            NewState#state{robots = Robots}
    end.

is_password_required(owner, _Config) ->
    %% Don't check pass if user is owner in MUC service (access_admin option)
    false;
is_password_required(_, Config) ->
    Config#config.password_protected.

check_password(#state{http_auth_pool = none,
                      config = #config{password = Password}}, Password) ->
    allowed;
check_password(#state{http_auth_pool = none}, _Password) ->
    invalid_password;
check_password(#state{http_auth_pool = _Pool}, _Password) ->
    http_auth.

-spec extract_password([jlib:xmlcdata() | exml:element()]) -> 'false' | binary().
extract_password([]) ->
    false;
extract_password([#xmlel{attrs = Attrs} = El | Els]) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
    ?NS_MUC ->
        case xml:get_subtag(El, <<"password">>) of
        false ->
            false;
        SubEl ->
            xml:get_tag_cdata(SubEl)
        end;
    _ ->
        extract_password(Els)
    end;
extract_password([_ | Els]) ->
    extract_password(Els).


-spec count_stanza_shift(mod_muc:nick(), [jlib:xmlcdata() | exml:element()],
                        state()) -> any().
count_stanza_shift(Nick, Els, StateData) ->
    HL = lqueue_to_list(StateData#state.history),
    Since = extract_history(Els, <<"since">>),
    Shift0 = case Since of
         false ->
             0;
         _ ->
             Sin = calendar:datetime_to_gregorian_seconds(Since),
             count_seconds_shift(Sin, HL)
         end,
    Seconds = extract_history(Els, <<"seconds">>),
    Shift1 = case Seconds of
         false ->
             0;
         _ ->
             Sec = calendar:datetime_to_gregorian_seconds(
                 calendar:now_to_universal_time(os:timestamp())) - Seconds,
             count_seconds_shift(Sec, HL)
         end,
    MaxStanzas = extract_history(Els, <<"maxstanzas">>),
    Shift2 = case MaxStanzas of
         false ->
             0;
         _ ->
             count_maxstanzas_shift(MaxStanzas, HL)
         end,
    MaxChars = extract_history(Els, <<"maxchars">>),
    Shift3 = case MaxChars of
         false ->
             0;
         _ ->
             count_maxchars_shift(Nick, MaxChars, HL)
         end,
    lists:max([Shift0, Shift1, Shift2, Shift3]).


-spec count_seconds_shift(integer(), [any()]) -> number().
count_seconds_shift(Seconds, HistoryList) ->
    lists:sum(
      lists:map(
        fun({_Nick, _Packet, _HaveSubject, TimeStamp, _Size}) ->
                T = calendar:datetime_to_gregorian_seconds(TimeStamp),
                case T < Seconds of
                    true -> 1;
                    false -> 0
                end
        end, HistoryList)).


-spec count_maxstanzas_shift(non_neg_integer(), [any()]) -> integer().
count_maxstanzas_shift(MaxStanzas, HistoryList) ->
    S = length(HistoryList) - MaxStanzas,
    max(0, S).


-spec count_maxchars_shift(mod_muc:nick(), non_neg_integer() | calendar:datetime(),
                          [any()]) -> non_neg_integer().
count_maxchars_shift(Nick, MaxSize, HistoryList) ->
    NLen = string:len(binary_to_list(Nick)) + 1,
    Sizes = lists:map(
          fun({_Nick, _Packet, _HaveSubject, _TimeStamp, Size}) ->
          Size + NLen
          end, HistoryList),
    calc_shift(MaxSize, Sizes).


-spec calc_shift(non_neg_integer() | calendar:datetime(), [number()]) -> non_neg_integer().
calc_shift(MaxSize, Sizes) ->
    Total = lists:sum(Sizes),
    calc_shift(MaxSize, Total, 0, Sizes).


-spec calc_shift(_MaxSize :: non_neg_integer() | calendar:datetime(),
        _Size :: number(), Shift :: non_neg_integer(), TSizes :: [number()]
        ) -> non_neg_integer().
calc_shift(_MaxSize, _Size, Shift, []) ->
    Shift;
calc_shift(MaxSize, Size, Shift, _Sizes) when MaxSize >= Size ->
    Shift;
calc_shift(MaxSize, Size, Shift, [S | TSizes]) ->
    calc_shift(MaxSize, Size - S, Shift + 1, TSizes).


-spec extract_history([jlib:xmlcdata() | exml:element()], Type :: binary()) ->
    false | non_neg_integer() | calendar:datetime1970().
extract_history([], _Type) ->
    false;
extract_history([#xmlel{attrs = Attrs} = El | Els], Type) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
        ?NS_MUC ->
            parse_history_val(xml:get_path_s(El, [{elem, <<"history">>}, {attr, Type}]), Type);
        _ ->
            extract_history(Els, Type)
    end;
extract_history([_ | Els], Type) ->
    extract_history(Els, Type).

-spec parse_history_val(binary(), binary()) -> false | non_neg_integer() | calendar:datetime1970().
parse_history_val(AttrVal, <<"since">>) ->
    case jlib:datetime_binary_to_timestamp(AttrVal) of
        undefined ->
            false;
        TS ->
            calendar:now_to_universal_time(TS)
    end;
parse_history_val(AttrVal, _) ->
    case catch binary_to_integer(AttrVal) of
        IntVal when is_integer(IntVal) and (IntVal >= 0) ->
            IntVal;
        _ ->
            false
    end.

-spec send_update_presence(jid:jid(), Reason :: binary(), state()) -> any().
send_update_presence(JID, Reason, StateData) ->
    foreach_matched_jid(fun(J) ->
                          send_new_presence(J, Reason, StateData)
                        end, JID, StateData).


-spec foreach_matched_jid(fun((_) -> 'ok'), jid:jid(), state()) -> ok.
foreach_matched_jid(F, JID, #state{users=Users}) ->
    LJID = jid:to_lower(JID),
    case LJID of
        %% Match by bare JID
        {U, S, <<>>} ->
            FF = fun({U0, S0, _}, #user{jid = MatchedJID})
                       when U =:= U0, S =:= S0 ->
                         F(MatchedJID);
                    (_, _) -> ok
                 end,
            maps_foreach(FF, Users);
        %% Match by full JID
        _ ->
            case maps:is_key(LJID, Users) of
                true ->
                    F(JID),
                    ok;
                false ->
                    ok
            end
    end.


-spec foreach_matched_user(fun((_) -> 'ok'), jid:simple_jid() | jid:jid(),
                           state()) -> ok.
foreach_matched_user(F, JID, #state{users=Users}) ->
    LJID = jid:to_lower(JID),
    case LJID of
        %% Match by bare JID
        {U, S, <<>>} ->
            FF = fun({U0, S0, _}, User) when U =:= U0, S =:= S0 ->
                         F(User);
                    (_, _) -> ok
                 end,
            maps_foreach(FF, Users);
        %% Match by full JID
        _ ->
            case maps:find(LJID, Users) of
                {ok, User} -> F(User);
                error -> ok
            end
    end.


-spec foreach_user(fun((_) -> 'ok'), state()) -> any().
foreach_user(F, #state{users=Users}) ->
    map_foreach_value(F, Users).


-spec erase_matched_users(jid:simple_jid() | jid:jid(), state()) -> state().
erase_matched_users(JID, StateData=#state{users=Users, sessions=Sessions}) ->
    LJID = jid:to_lower(JID),
    {NewUsers, NewSessions} = erase_matched_users_map(LJID, Users, Sessions),
    notify_users_modified(StateData#state{users=NewUsers, sessions=NewSessions}).


-spec erase_matched_users_map(error | jid:simple_jid(),
                              users_map(), sessions_map()) -> any().
erase_matched_users_map({U, S, <<>>}, Users, Sessions) ->
    FF = fun({U0, S0, _} = J, #user{nick=Nick}, {Us, Ss}) when U =:= U0 andalso S =:= S0->
                 {maps:remove(J, Us), maps:remove(Nick, Ss)};
            (_, _, Acc) ->
                 Acc
         end,
    maps:fold(FF, {Users, Sessions}, Users);
erase_matched_users_map(LJID, Users, Sessions) ->
    {ok, #user{nick=Nick}} = maps:find(LJID, Users),
    {maps:remove(LJID, Users), maps:remove(Nick, Sessions)}.


-spec update_matched_users(F :: fun((user()) -> user()), JID :: jid:jid(),
                           state()) -> state().
update_matched_users(F, JID, StateData=#state{users=Users}) ->
    LJID = jid:to_lower(JID),
    NewUsers = update_matched_users_map(F, LJID, Users),
    notify_users_modified(StateData#state{users=NewUsers}).


-spec update_matched_users_map(fun((user()) -> user()),
                               error | jid:simple_jid(), users_map()) -> any().
update_matched_users_map(F, {U, S, <<>>}, Users) ->
    FF = fun({U0, S0, _} = J, User, Us) when U =:= U0 andalso S =:= S0->
                 maps:put(J, F(User), Us);
            (_, _, Us) ->
                 Us
         end,
    maps:fold(FF, Users, Users);
update_matched_users_map(F, LJID, Users) ->
    case maps:find(LJID, Users) of
        {ok, User} -> maps:put(LJID, F(User), Users);
        error -> Users
    end.

-spec send_new_presence_un(jid:jid(), state()) -> 'ok'.
send_new_presence_un(NJID, StateData) ->
    send_new_presence_un(NJID, <<>>, StateData).


-spec send_new_presence_un(jid:jid(), binary(), state()) -> 'ok'.
send_new_presence_un(NJID, Reason, StateData) ->
    {ok, #user{nick = Nick}} = maps:find(jid:to_lower(NJID), StateData#state.users),
    case is_last_session(Nick, StateData) of
        true ->
            send_new_presence(NJID, Reason, StateData);
        false ->
            UserJIDs = maps:get(Nick, StateData#state.sessions),
            GetUserTupleByJID = fun(JID) ->
                LJID = jid:to_lower(JID),
                {LJID, maps:get(LJID, StateData#state.users)}
            end,
            CurrentSessionUsers = lists:map(GetUserTupleByJID, UserJIDs),
            send_new_presence_to(NJID, Reason, CurrentSessionUsers, StateData)
    end.


-spec send_new_presence(jid:jid(), state()) -> 'ok'.
send_new_presence(NJID, StateData) ->
    send_new_presence(NJID, <<>>, StateData).


-spec send_new_presence(jid:jid(), binary(), state()) -> 'ok'.
send_new_presence(NJID, Reason, StateData) ->
    send_new_presence_to(NJID, Reason, StateData#state.users, StateData).


%% Receivers can be a list or a map
-spec send_new_presence_to(jid:jid(), binary(), users_map() | users_pairs(), state()) -> ok.
send_new_presence_to(NJID, Reason, Receivers, StateData) ->
    {ok, #user{ role = Role } = User} = maps:find(jid:to_lower(NJID), StateData#state.users),
    Affiliation = get_affiliation(NJID, StateData),
    BAffiliation = affiliation_to_binary(Affiliation),
    BRole = role_to_binary(Role),
    F = fun(_LJID, Info) ->
        send_new_presence_to_single(NJID, User, BAffiliation, BRole, Reason, Info, StateData)
      end,
    maps_or_pairs_foreach(F, Receivers).

send_new_presence_to_single(NJID, #user{jid = RealJID, nick = Nick, last_presence = Presence},
                            BAffiliation, BRole, Reason, ReceiverInfo, StateData) ->
    ItemAttrs =
    case (ReceiverInfo#user.role == moderator) orelse
         ((StateData#state.config)#config.anonymous == false) of
        true ->
            [{<<"jid">>, jid:to_binary(RealJID)},
             {<<"affiliation">>, BAffiliation},
             {<<"role">>, BRole}];
        _ ->
            [{<<"affiliation">>, BAffiliation},
             {<<"role">>, BRole}]
    end,
    ItemEls = case Reason of
                  <<>> ->
                      [];
                  _ ->
                      [#xmlel{name = <<"reason">>, children = [#xmlcdata{content = Reason}]}]
              end,
    Status = case StateData#state.just_created of
                 true ->
                     [status_code(201)];
                 false ->
                     []
             end,
    Status2 = case (NJID == ReceiverInfo#user.jid) of
                  true ->
                      Status0 = case (StateData#state.config)#config.logging of
                                    true ->
                                        [status_code(170) | Status];
                                    false ->
                                        Status
                                end,
                      Status1 = case ((StateData#state.config)#config.anonymous==false) of
                                    true ->
                                        [status_code(100) | Status0];
                                    false ->
                                        Status0
                                end,
                      case ((NJID == ReceiverInfo#user.jid)==true) of
                          true ->
                              [status_code(110) | Status1];
                          false ->
                              Status1
                      end;
                  false ->
                      Status
              end,
    Packet = xml:append_subtags(
               Presence,
               [#xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
                       children = [#xmlel{name = <<"item">>, attrs = ItemAttrs,
                                          children = ItemEls} | Status2]}]),
    ejabberd_router:route(jid:replace_resource(StateData#state.jid, Nick),
                          ReceiverInfo#user.jid, Packet).

-spec send_existing_presences(jid:jid(), state()) -> 'ok'.
send_existing_presences(ToJID, StateData) ->
    LToJID = jid:to_lower(ToJID),
    {ok, #user{jid = RealToJID, role = Role, nick = _Nick}} =
    maps:find(LToJID, StateData#state.users),
    % if you don't want to send presences of other sessions of occupant with ToJID
    % switch following lines
    % JIDsToSkip = [RealToJID | find_jids_by_nick(_Nick, StateData)],
    JIDsToSkip = [RealToJID],
    maps_foreach(
        fun({_, #user{jid = FromJID}} = User) ->
                case lists:member(FromJID, JIDsToSkip) of
                    true -> ok;
                    _ -> send_existing_presence(User, Role, RealToJID, StateData)
                end
        end, StateData#state.users).

-spec send_existing_presence({jid:simple_jid(), mod_muc_room_user()}, mod_muc:role(),
                             jid:jid(), state()) -> mongoose_acc:t().
send_existing_presence({_LJID, #user{jid = FromJID, nick = FromNick,
                                    role = FromRole, last_presence = Presence}},
                       Role, RealToJID, StateData) ->
    FromAffiliation = get_affiliation(FromJID, StateData),
    ItemAttrs =
    case (Role == moderator) orelse ((StateData#state.config)#config.anonymous == false) of
        true ->
            [{<<"jid">>, jid:to_binary(FromJID)},
             {<<"affiliation">>,
              affiliation_to_binary(FromAffiliation)},
             {<<"role">>, role_to_binary(FromRole)}];
        _ ->
            [{<<"affiliation">>,
              affiliation_to_binary(FromAffiliation)},
             {<<"role">>, role_to_binary(FromRole)}]
    end,
    Packet = xml:append_subtags(
               Presence,
               [#xmlel{name = <<"x">>,
                       attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
                       children = [#xmlel{name = <<"item">>,
                                          attrs = ItemAttrs}]}]),
    ejabberd_router:route(jid:replace_resource(StateData#state.jid, FromNick), RealToJID, Packet).

-spec send_config_update(atom(), state()) -> 'ok'.
send_config_update(Type, StateData) ->
    Status = case Type of
            logging_enabled     -> <<"170">>;
            logging_disabled    -> <<"171">>;
            nonanonymous        -> <<"172">>;
            semianonymous       -> <<"173">>
        end,
    Message = jlib:make_config_change_message(Status),
    send_to_all_users(Message, StateData).


-spec send_invitation(jid:jid(), jid:jid(), binary(), state()) -> mongoose_acc:t().
send_invitation(From, To, Reason, StateData=#state{host=Host,
                                                   server_host=ServerHost,
                                                   jid=RoomJID}) ->
    ejabberd_hooks:run(invitation_sent, Host, [Host, ServerHost, RoomJID, From, To, Reason]),
    Config = StateData#state.config,
    Password = case Config#config.password_protected of
        false -> <<>>;
        true -> Config#config.password
    end,
    Packet = jlib:make_invitation(jid:to_bare(From), Password, Reason),
    ejabberd_router:route(RoomJID, To, Packet).


-spec change_nick(jid:jid(), binary(), state()) -> state().
change_nick(JID, Nick, StateData) ->
    LJID = jid:to_lower(JID),
    {ok, #user{nick = OldNick}} =
    maps:find(LJID, StateData#state.users),
    Users =
    maps:update_with(
      LJID,
      fun(#user{} = User) ->
              User#user{nick = Nick}
      end, StateData#state.users),
    {ok, JIDs} = maps:find(OldNick, StateData#state.sessions),
    Sessions = maps:remove(OldNick, maps:put(Nick, JIDs, StateData#state.sessions)),
    NewStateData = notify_users_modified(StateData#state{users = Users, sessions = Sessions}),
    send_nick_changing(JID, OldNick, NewStateData),
    add_to_log(nickchange, {OldNick, Nick}, StateData),
    NewStateData.


-spec send_nick_changing(jid:jid(), mod_muc:nick(), state()) -> 'ok'.
send_nick_changing(JID, OldNick, StateData) ->
    User = maps:find(jid:to_lower(JID), StateData#state.users),
    {ok, #user{jid = RealJID,
               nick = Nick,
               role = Role,
               last_presence = Presence}} = User,
    Affiliation = get_affiliation(JID, StateData),
    maps_foreach(mk_send_nick_change(Presence, OldNick, JID, RealJID,
                                      Affiliation, Role, Nick, StateData),
                  StateData#state.users).

mk_send_nick_change(Presence, OldNick, JID, RealJID,  Affiliation,
                    Role, Nick, StateData) ->
    fun({LJID, Info}) ->
            send_nick_change(Presence, OldNick, JID, RealJID, Affiliation,
                             Role, Nick, LJID, Info, StateData)
    end.

send_nick_change(Presence, OldNick, JID, RealJID, Affiliation, Role,
                 Nick, _LJID, Info, #state{} = S) ->
    MaybePublicJID = case is_nick_change_public(Info, S#state.config) of
                         true -> RealJID;
                         false -> undefined
                     end,
    MaybeSelfPresenceCode = case JID == Info#user.jid of
                                true -> status_code(110);
                                false -> undefined
                            end,
    Unavailable = nick_unavailable_presence(MaybePublicJID, Nick, Affiliation,
                                            Role, MaybeSelfPresenceCode),
    ejabberd_router:route(jid:replace_resource(S#state.jid, OldNick),
                          Info#user.jid, Unavailable),
    Available = nick_available_presence(Presence, MaybePublicJID, Affiliation,
                                        Role, MaybeSelfPresenceCode),
    ejabberd_router:route(jid:replace_resource(S#state.jid, Nick),
                          Info#user.jid, Available).

-spec is_nick_change_public(user(), config()) -> boolean().
is_nick_change_public(UserInfo, RoomConfig) ->
    UserInfo#user.role == moderator
    orelse
    RoomConfig#config.anonymous == false.

-spec status_code(integer()) -> exml:element().
status_code(Code) ->
    #xmlel{name = <<"status">>,
           attrs = [{<<"code">>, integer_to_binary(Code)}]}.

-spec nick_unavailable_presence(MaybeJID, Nick, Affiliation, Role, MaybeCode) ->
    exml:element() when
      MaybeJID :: 'undefined' | jid:jid(),
      Nick :: mod_muc:nick(),
      Affiliation :: mod_muc:affiliation(),
      Role :: mod_muc:role(),
      MaybeCode :: 'undefined' | exml:element().
nick_unavailable_presence(MaybeJID, Nick, Affiliation, Role, MaybeCode) ->
    presence(<<"unavailable">>,
             [muc_user_x([muc_user_item(MaybeJID, Nick, Affiliation, Role),
                          status_code(303)]
                         ++ [MaybeCode || MaybeCode /= undefined])]).

-spec nick_available_presence(LastPresence, MaybeJID, Affiliation,
                              Role, MaybeCode) -> exml:element() when
      LastPresence :: exml:element(),
      MaybeJID :: 'undefined' | jid:jid(),
      Affiliation :: mod_muc:affiliation(),
      Role :: mod_muc:role(),
      MaybeCode :: 'undefined' | exml:element().
nick_available_presence(LastPresence, MaybeJID, Affiliation, Role, MaybeCode) ->
    Item = muc_user_item(MaybeJID, undefined, Affiliation, Role),
    xml:append_subtags(LastPresence,
                       [muc_user_x([Item] ++ [MaybeCode
                                              || MaybeCode /= undefined])]).

-spec muc_user_item(MaybeJID, MaybeNick, Affiliation, Role) -> exml:element() when
      MaybeJID :: 'undefined' | jid:jid(),
      MaybeNick :: 'undefined' | mod_muc:nick(),
      Affiliation :: mod_muc:affiliation(),
      Role :: mod_muc:role().
muc_user_item(MaybeJID, MaybeNick, Affiliation, Role) ->
    #xmlel{name = <<"item">>,
           attrs = [{<<"jid">>, jid:to_binary(MaybeJID)}
                    || MaybeJID /= undefined] ++
                   [{<<"nick">>, MaybeNick} || MaybeNick /= undefined] ++
                   [{<<"affiliation">>, affiliation_to_binary(Affiliation)},
                    {<<"role">>, role_to_binary(Role)}]}.

-spec muc_user_x([exml:element()]) -> exml:element().
muc_user_x(Children) ->
    #xmlel{name = <<"x">>,
           attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
           children = Children}.

-spec presence(binary(), [exml:element()]) -> exml:element().
%% Add and validate other types if need be.
presence(<<"unavailable">> = Type, Children) ->
    #xmlel{name = <<"presence">>,
           attrs = [{<<"type">>, Type} || Type /= <<"available">>],
           children = Children}.


-spec lqueue_new(integer()) -> lqueue().
lqueue_new(Max) ->
    #lqueue{queue = queue:new(),
        len = 0,
        max = Max}.


%% @doc If the message queue limit is set to 0, do not store messages.
%% Otherwise, rotate messages in the queue store.
-spec lqueue_in(any(), lqueue()) -> lqueue().
lqueue_in(_Item, LQ = #lqueue{max = 0}) ->
    LQ;
lqueue_in(Item, #lqueue{queue = Q1, len = Len, max = Max}) ->
    Q2 = queue:in(Item, Q1),
    case Len >= Max of
        true ->
            Q3 = lqueue_cut(Q2, Len - Max + 1),
            #lqueue{queue = Q3, len = Max, max = Max};
        false ->
            #lqueue{queue = Q2, len = Len + 1, max = Max}
    end.


-spec lqueue_cut(queue:queue(), non_neg_integer()) -> queue:queue().
lqueue_cut(Q, 0) ->
    Q;
lqueue_cut(Q, N) ->
    {_, Q1} = queue:out(Q),
    lqueue_cut(Q1, N - 1).


-spec lqueue_to_list(lqueue()) -> [any()].
lqueue_to_list(#lqueue{queue = Q1}) ->
    queue:to_list(Q1).


-spec add_message_to_history(mod_muc:nick(), jid:jid(), exml:element(),
                            state()) -> state().
add_message_to_history(FromNick, FromJID, Packet, StateData) ->
    HaveSubject = case xml:get_subtag(Packet, <<"subject">>) of
              false ->
              false;
              _ ->
              true
          end,
    TimeStamp = calendar:now_to_universal_time(os:timestamp()),
    %% Chatroom history is stored as XMPP packets, so
    %% the decision to include the original sender's JID or not is based on the
    %% chatroom configuration when the message was originally sent.
    %% Also, if the chatroom is anonymous, even moderators will not get the real JID
    SenderJid = case   (StateData#state.config)#config.anonymous of
    true -> StateData#state.jid;
    false -> FromJID
    end,
    TSPacket = xml:append_subtags(Packet,
                  [jlib:timestamp_to_xml(TimeStamp, utc, SenderJid, <<>>)]),
    SPacket = jlib:replace_from_to(
        jid:replace_resource(StateData#state.jid, FromNick),
        StateData#state.jid,
        TSPacket),
    Size = element_size(SPacket),
    Q1 = lqueue_in({FromNick, TSPacket, HaveSubject, TimeStamp, Size},
           StateData#state.history),
    add_to_log(text, {FromNick, Packet}, StateData),
    ejabberd_hooks:run(room_packet, StateData#state.host,
                       [FromNick, FromJID, StateData#state.jid, Packet]),
    StateData#state{history = Q1}.


-spec send_history(jid:jid(), Shift :: non_neg_integer(), state()) -> boolean().
send_history(JID, Shift, StateData) ->
    lists:foldl(
      fun({Nick, Packet, HaveSubject, _TimeStamp, _Size}, B) ->
          ejabberd_router:route(
        jid:replace_resource(StateData#state.jid, Nick),
        JID,
        Packet),
          B or HaveSubject
      end, false, lists:nthtail(Shift, lqueue_to_list(StateData#state.history))).


-spec send_subject(jid:jid(), ejabberd:lang(), state()) -> mongoose_acc:t().
send_subject(JID, _Lang, StateData = #state{subject = <<>>, subject_author = <<>>}) ->
    Packet = #xmlel{name = <<"message">>,
                    attrs = [{<<"type">>, <<"groupchat">>}],
                    children = [#xmlel{name = <<"subject">>},
                               #xmlel{name = <<"body">>}]},
    ejabberd_router:route(
        StateData#state.jid,
        JID,
        Packet);
send_subject(JID, _Lang, StateData) ->
    Subject = StateData#state.subject,
    Packet = #xmlel{name = <<"message">>,
                    attrs = [{<<"type">>, <<"groupchat">>}],
                    children = [#xmlel{name = <<"subject">>,
                                       children = [#xmlcdata{content = Subject}]},
                               #xmlel{name = <<"body">>}]},
    ejabberd_router:route(
        StateData#state.jid,
        JID,
        Packet).


-spec check_subject(exml:element()) -> 'false' | binary().
check_subject(Packet) ->
    case xml:get_subtag(Packet, <<"subject">>) of
        false ->
            false;
        SubjEl ->
            xml:get_tag_cdata(SubjEl)
    end.


-spec can_change_subject(mod_muc:role(), state()) -> boolean().
can_change_subject(Role, StateData) ->
    case (StateData#state.config)#config.allow_change_subj of
        true ->
            (Role == moderator) orelse (Role == participant);
        _ ->
            Role == moderator
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Admin stuff

-spec process_iq_admin(jid:jid(), get | set, ejabberd:lang(), exml:element(), state()) ->
    state() | {error, exml:element()}.
process_iq_admin(From, set, Lang, SubEl, StateData) ->
    #xmlel{children = Items} = SubEl,
    process_admin_items_set(From, Items, Lang, StateData);
process_iq_admin(From, get, Lang, SubEl, StateData) ->
    case xml:get_subtag(SubEl, <<"item">>) of
        false ->
            {error, mongoose_xmpp_errors:bad_request()};
        Item ->
            FAffiliation = get_affiliation(From, StateData),
            FRole = get_role(From, StateData),
            {RoleOrAff, _} = ExtractResult = extract_role_or_affiliation(Item),
            IsAllowed = iq_admin_allowed(get, RoleOrAff, FAffiliation, FRole, StateData),
            case {IsAllowed, ExtractResult} of
                {true, {role, Role}} ->
                    Items = items_with_role(Role, StateData),
                    {result, Items, StateData};
                {true, {affiliation, Affiliation}} ->
                    Items = items_with_affiliation(Affiliation, StateData),
                    {result, Items, StateData};
                {_, {role, _}} ->
                    ErrText = <<"Moderator privileges required">>,
                    {error, mongoose_xmpp_errors:forbidden(Lang, ErrText)};
                {_, {affiliation, _}} ->
                    ErrText = <<"Administrator privileges required">>,
                    {error, mongoose_xmpp_errors:forbidden(Lang, ErrText)};
                {_, Error} ->
                    Error
            end
    end.

-spec extract_role_or_affiliation(Item :: exml:element()) ->
    {role, mod_muc:role()} | {affiliation, mod_muc:affiliation()} | {error, exml:element()}.
extract_role_or_affiliation(Item) ->
    case {xml:get_tag_attr(<<"role">>, Item), xml:get_tag_attr(<<"affiliation">>, Item)} of
        {false, false} ->
            {error, mongoose_xmpp_errors:bad_request()};
        {false, {value, BAffiliation}} ->
            case catch binary_to_affiliation(BAffiliation) of
                {'EXIT', _} -> {error, mongoose_xmpp_errors:bad_request()};
                Affiliation -> {affiliation, Affiliation}
            end;
        {{value, BRole}, _} ->
            case catch binary_to_role(BRole) of
                {'EXIT', _} -> {error, mongoose_xmpp_errors:bad_request()};
                Role -> {role, Role}
            end
    end.

-spec iq_admin_allowed(atom(), atom(), atom(), atom(), state()) -> boolean().
iq_admin_allowed(get, What, FAff, none, State) ->
    %% no role is translated to 'visitor'
    iq_admin_allowed(get, What, FAff, visitor, State);
iq_admin_allowed(get, role, _, moderator, _) ->
    %% moderator is allowed by definition, needs it to do his duty
    true;
iq_admin_allowed(get, role, _, Role, State) ->
    Cfg = State#state.config,
    lists:member(Role, Cfg#config.maygetmemberlist);
iq_admin_allowed(get, affiliation, owner, _, _) ->
    true;
iq_admin_allowed(get, affiliation, admin, _, _) ->
    true;
iq_admin_allowed(get, affiliation, _, Role, State) ->
    Cfg = State#state.config,
    lists:member(Role, Cfg#config.maygetmemberlist).


-spec items_with_role(mod_muc:role(), state()) -> [exml:element()].
items_with_role(BRole, StateData) ->
    lists:map(
      fun({_, U}) ->
          user_to_item(U, StateData)
      end, search_role(BRole, StateData)).


-spec items_with_affiliation(mod_muc:affiliation(), state()) -> [exml:element()].
items_with_affiliation(BAffiliation, StateData) ->
    lists:map(
      fun({JID, {Affiliation, Reason}}) ->
          #xmlel{name = <<"item">>,
                 attrs = [{<<"affiliation">>, affiliation_to_binary(Affiliation)},
                      {<<"jid">>, jid:to_binary(JID)}],
                 children = [#xmlel{name = <<"reason">>,
                                    children = [#xmlcdata{content = Reason}]}]};
         ({JID, Affiliation}) ->
              #xmlel{name = <<"item">>,
                     attrs = [{<<"affiliation">>, affiliation_to_binary(Affiliation)},
                          {<<"jid">>, jid:to_binary(JID)}]}
      end, search_affiliation(BAffiliation, StateData)).


-spec user_to_item(user(), state()) -> exml:element().
user_to_item(#user{role = Role,
           nick = Nick,
           jid = JID
          }, StateData) ->
    Affiliation = get_affiliation(JID, StateData),
    #xmlel{name = <<"item">>,
           attrs = [{<<"role">>, role_to_binary(Role)},
                    {<<"affiliation">>, affiliation_to_binary(Affiliation)},
                    {<<"nick">>, Nick},
                    {<<"jid">>, jid:to_binary(JID)}]}.


-spec search_role(mod_muc:role(), state()) -> users_pairs().
search_role(Role, StateData) ->
    F = fun(_, #user{role = R}) -> Role == R end,
    maps:to_list(maps:filter(F, StateData#state.users)).


-spec search_affiliation(mod_muc:affiliation(), state()) -> [{_, _}].
search_affiliation(Affiliation, StateData) when is_atom(Affiliation) ->
    F = fun(_, A) ->
          case A of
          {A1, _Reason} ->
              Affiliation == A1;
          _ ->
              Affiliation == A
          end
      end,
    maps:to_list(maps:filter(F, StateData#state.affiliations)).


-spec process_admin_items_set(jid:jid(), [exml:element(), ...], ejabberd:lang(), state()) ->
    {'error', exml:element()} | {'result', [], state()}.
process_admin_items_set(UJID, Items, Lang, StateData) ->
    UAffiliation = get_affiliation(UJID, StateData),
    URole = get_role(UJID, StateData),
    case find_changed_items(UJID, UAffiliation, URole, Items, Lang, StateData, []) of
        {result, Res} ->
            ?INFO_MSG("Processing MUC admin query from ~s in room ~s:~n ~p",
                      [jid:to_binary(UJID), jid:to_binary(StateData#state.jid), Res]),
            NSD = lists:foldl(
                    fun(ChangedItem, SD) ->
                            process_admin_item_set(ChangedItem, UJID, SD)
                    end, StateData, Res),
            case (NSD#state.config)#config.persistent of
                true ->
                    mod_muc:store_room(NSD#state.server_host,
                                       NSD#state.host, NSD#state.room, make_opts(NSD));
                _ -> ok
            end,
            {result, [], NSD};
        Err ->
            Err
    end.

process_admin_item_set(ChangedItem, UJID, SD) ->
    try
        process_admin_item_set_unsafe(ChangedItem, UJID, SD)
    catch
        _:Error ->
            ?ERROR_MSG("MUC ITEMS SET ERR: ~p~n", [Error]),
            SD
    end.

process_admin_item_set_unsafe({JID, affiliation, owner, _}, _UJID, SD)
  when (JID#jid.luser == <<>>) ->
    %% If the provided JID does not have username,
    %% ignore the affiliation completely
    SD;
process_admin_item_set_unsafe({JID, role, none, Reason}, _UJID, SD) ->
    safe_send_kickban_presence(JID, Reason, <<"307">>, SD),
    set_role(JID, none, SD);
process_admin_item_set_unsafe({JID, affiliation, none, Reason}, _UJID, SD) ->
    case  (SD#state.config)#config.members_only of
        true ->
            safe_send_kickban_presence(JID, Reason, <<"321">>, none, SD),
            SD1 = set_affiliation_and_reason(JID, none, Reason, SD),
            set_role(JID, none, SD1);
        _ ->
            SD1 = set_affiliation_and_reason(JID, none, Reason, SD),
            send_update_presence(JID, Reason, SD1),
            SD1
    end;
process_admin_item_set_unsafe({JID, affiliation, outcast, Reason}, _UJID, SD) ->
    safe_send_kickban_presence(JID, Reason, <<"301">>, outcast, SD),
    set_affiliation_and_reason(JID, outcast, Reason, set_role(JID, none, SD));
process_admin_item_set_unsafe({JID, affiliation, A, Reason}, _UJID, SD)
  when (A == admin) or (A == owner) ->
    SD1 = set_affiliation_and_reason(JID, A, Reason, SD),
    SD2 = set_role(JID, moderator, SD1),
    send_update_presence(JID, Reason, SD2),
    SD2;
process_admin_item_set_unsafe({JID, affiliation, member, Reason}, UJID, SD) ->
    case (SD#state.config)#config.members_only of
        true -> send_invitation(UJID, JID, Reason, SD);
        _ -> ok
    end,
    SD1 = set_affiliation_and_reason(JID, member, Reason, SD),
    SD2 = set_role(JID, participant, SD1),
    send_update_presence(JID, Reason, SD2),
    SD2;
process_admin_item_set_unsafe({JID, role, Role, Reason}, _UJID, SD) ->
    SD1 = set_role(JID, Role, SD),
    catch send_new_presence(JID, Reason, SD1),
    SD1;
process_admin_item_set_unsafe({JID, affiliation, A, Reason}, _UJID, SD) ->
    SD1 = set_affiliation(JID, A, SD),
    send_update_presence(JID, Reason, SD1),
    SD1.

-type res_row() :: {jid:simple_jid() | jid:jid(),
                    'affiliation' | 'role', any(), any()}.
-type find_changed_items_res() :: {'error', exml:element()} | {'result', [res_row()]}.
-spec find_changed_items(jid:jid(), mod_muc:affiliation(), mod_muc:role(),
                         [exml:element()], ejabberd:lang(), state(), [res_row()]) ->
    find_changed_items_res().
find_changed_items(_UJID, _UAffiliation, _URole, [], _Lang, _StateData, Res) ->
    {result, Res};
find_changed_items(UJID, UAffiliation, URole, [#xmlcdata{} | Items],
                   Lang, StateData, Res) ->
    find_changed_items(UJID, UAffiliation, URole, Items, Lang, StateData, Res);
find_changed_items(UJID, UAffiliation, URole,
                   [#xmlel{name = <<"item">>, attrs = Attrs} = Item | Items],
                   Lang, StateData, Res) ->
    case get_affected_jid(Attrs, Lang, StateData) of
        {value, JID} ->
            check_changed_item(UJID, UAffiliation, URole, JID, Item, Items, Lang, StateData, Res);
        Err ->
            Err
    end;
find_changed_items(_UJID, _UAffiliation, _URole, _Items, _Lang, _StateData, _Res) ->
    {error, mongoose_xmpp_errors:bad_request()}.

-spec get_affected_jid(Attrs :: [{binary(), binary()}],
                       Lang :: ejabberd:lang(),
                       StateData :: state()) ->
    {value,jid:jid()} | {error, exml:element()}.
get_affected_jid(Attrs, Lang, StateData) ->
    case {xml:get_attr(<<"jid">>, Attrs), xml:get_attr(<<"nick">>, Attrs)} of
        {{value, S}, _} ->
            case jid:from_binary(S) of
                error ->
                    ErrText = <<(translate:translate(Lang, <<"Jabber ID ">>))/binary,
                                S/binary, (translate:translate(Lang, <<" is invalid">>))/binary>>,
                    {error, mongoose_xmpp_errors:not_acceptable(Lang, ErrText)};
                J ->
                    {value, J}
            end;
        {_, {value, N}} ->
            case find_jids_by_nick(N, StateData) of
                [] ->
                    ErrText
                    = <<(translate:translate(Lang, <<"Nickname ">>))/binary, N/binary,
                        (translate:translate(Lang, <<" does not exist in the room">>))/binary>>,
                    {error, mongoose_xmpp_errors:not_acceptable(Lang, ErrText)};
                [FirstSessionJid | _RestOfSessions] ->
                    {value, FirstSessionJid}
            end;
        _ ->
            {error, mongoose_xmpp_errors:bad_request()}
    end.

-spec check_changed_item(jid:jid(), mod_muc:affiliation(), mod_muc:role(),jid:jid(), exml:element(),
                         [exml:element()], ejabberd:lang(), state(), [res_row()]) ->
    find_changed_items_res().
check_changed_item(UJID, UAffiliation, URole, JID, #xmlel{ attrs = Attrs } = Item, Items,
                   Lang, StateData, Res) ->
    TAffiliation = get_affiliation(JID, StateData),
    TRole = get_role(JID, StateData),
    case which_property_changed(Attrs, Lang) of
        {role, Role} ->
            ServiceAf = get_service_affiliation(JID, StateData),
            CanChangeRA =
            case can_change_ra(UAffiliation, URole, TAffiliation, TRole, role, Role, ServiceAf) of
                nothing -> nothing;
                true -> true;
                check_owner -> is_owner(UJID, StateData);
                _ -> false
            end,
            case CanChangeRA of
                nothing -> find_changed_items(UJID, UAffiliation, URole,
                                              Items, Lang, StateData, Res);
                true -> find_changed_items(UJID, UAffiliation, URole, Items, Lang, StateData,
                                           [{JID, role, Role, decode_reason(Item)} | Res]);
                _ -> {error, mongoose_xmpp_errors:not_allowed()}
            end;
        {affiliation, Affiliation} ->
            ServiceAf = get_service_affiliation(JID, StateData),
            CanChangeRA =
            case can_change_ra(UAffiliation, URole, TAffiliation, TRole,
                               affiliation, Affiliation, ServiceAf) of
                nothing -> nothing;
                true -> true;
                cancel -> cancel;
                check_owner -> is_owner(UJID, StateData);
                _ ->
                    false
            end,
            case CanChangeRA of
                nothing -> find_changed_items(UJID, UAffiliation, URole, Items,
                                              Lang, StateData, Res);
                true -> find_changed_items(UJID, UAffiliation, URole, Items, Lang, StateData,
                                           [{jid:to_bare(JID), affiliation,
                                             Affiliation, decode_reason(Item)} | Res]);
                cancel -> {error, mongoose_xmpp_errors:not_allowed()};
                false -> {error, mongoose_xmpp_errors:forbidden()}
            end;
        Err -> Err
    end.

-spec is_owner(UJID ::jid:jid(), StateData :: state()) -> boolean().
is_owner(UJID, StateData) ->
    case search_affiliation(owner, StateData) of
        [{OJID, _}] -> jid:to_bare(OJID) /= jid:to_lower(jid:to_bare(UJID));
        _ -> true
    end.

-spec which_property_changed(Attrs :: [{binary(), binary()}], Lang :: ejabberd:lang()) ->
    {affiliation, mod_muc:affiliation()} | {role, mod_muc:role()} | {error, exml:element()}.
which_property_changed(Attrs, Lang) ->
    case {xml:get_attr(<<"role">>, Attrs), xml:get_attr(<<"affiliation">>, Attrs)} of
        {false, false} ->
            {error, mongoose_xmpp_errors:bad_request()};
        {false, {value, BAffiliation}} ->
            case catch binary_to_affiliation(BAffiliation) of
                {'EXIT', _} ->
                    ErrText1 = <<(translate:translate(Lang, <<"Invalid affiliation ">>))/binary,
                                 BAffiliation/binary>>,
                    {error, mongoose_xmpp_errors:not_acceptable(Lang, ErrText1)};
                Affiliation ->
                    {affiliation, Affiliation}
            end;
        {{value, BRole}, _} ->
            case catch binary_to_role(BRole) of
                {'EXIT', _} ->
                    ErrText1 = <<(translate:translate(Lang, <<"Invalid role ">>))/binary,
                                 BRole/binary>>,
                    {error, mongoose_xmpp_errors:bad_request(Lang, ErrText1)};
                Role ->
                    {role, Role}
            end
    end.

-spec can_change_ra(FAff :: mod_muc:affiliation(), FRole :: mod_muc:role(),
        TAff :: mod_muc:affiliation(), TRole :: mod_muc:role(),
        RoleOrAff :: 'affiliation' | 'role', Value :: any(),
        ServiceAff :: mod_muc:affiliation())
            -> 'cancel' | 'check_owner' | 'false' | 'nothing' | 'true'.
can_change_ra(_FAffiliation, _FRole,
          owner, _TRole,
          affiliation, owner, owner) ->
    %% A room owner tries to add as persistent owner a
    %% participant that is already owner because he is MUC admin
    true;
can_change_ra(_FAffiliation, _FRole,
              _TAffiliation, _TRole,
              _RoleorAffiliation, _Value, owner) ->
    %% Nobody can decrease MUC admin's role/affiliation
    false;
can_change_ra(_FAffiliation, _FRole,
          TAffiliation, _TRole,
          affiliation, Value, _ServiceAf)
  when (TAffiliation == Value) ->
    nothing;
can_change_ra(_FAffiliation, _FRole,
          _TAffiliation, TRole,
          role, Value, _ServiceAf)
  when (TRole == Value) ->
    nothing;
can_change_ra(FAffiliation, _FRole,
          outcast, _TRole,
          affiliation, none, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole,
          outcast, _TRole,
          affiliation, member, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole,
          outcast, _TRole,
          affiliation, admin, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole,
          outcast, _TRole,
          affiliation, owner, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, _FRole,
          none, _TRole,
          affiliation, outcast, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole,
          none, _TRole,
          affiliation, member, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole,
          none, _TRole,
          affiliation, admin, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole,
          none, _TRole,
          affiliation, owner, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, _FRole,
          member, _TRole,
          affiliation, outcast, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole,
          member, _TRole,
          affiliation, none, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole,
          member, _TRole,
          affiliation, admin, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole,
          member, _TRole,
          affiliation, owner, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole,
          admin, _TRole,
          affiliation, _Affiliation, _ServiceAf) ->
    true;
can_change_ra(owner, _FRole,
          owner, _TRole,
          affiliation, _Affiliation, _ServiceAf) ->
    check_owner;
can_change_ra(none, _FRole,
              TAffiliation, _TRole,
              affiliation, _Affiliation, _ServiceAf)
    when (TAffiliation == admin orelse TAffiliation == owner) ->
    cancel;
can_change_ra(admin, _FRole,
              owner, _TRole,
              affiliation, _Value, _ServiceAf) ->
    cancel;
can_change_ra(_FAffiliation, _FRole,
          _TAffiliation, _TRole,
          affiliation, _Value, _ServiceAf) ->
    false;
can_change_ra(_FAffiliation, moderator,
          _TAffiliation, visitor,
          role, none, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, moderator,
          _TAffiliation, visitor,
          role, participant, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, _FRole,
          _TAffiliation, visitor,
          role, moderator, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(_FAffiliation, moderator,
          _TAffiliation, participant,
          role, none, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, moderator,
          _TAffiliation, participant,
          role, visitor, _ServiceAf) ->
    true;
can_change_ra(FAffiliation, _FRole,
          _TAffiliation, participant,
          role, moderator, _ServiceAf)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(_FAffiliation, _FRole,
          owner, moderator,
          role, visitor, _ServiceAf) ->
    false;
can_change_ra(owner, _FRole,
          _TAffiliation, moderator,
          role, visitor, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, _FRole,
          admin, moderator,
          role, visitor, _ServiceAf) ->
    false;
can_change_ra(admin, _FRole,
          _TAffiliation, moderator,
          role, visitor, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, _FRole,
          owner, moderator,
          role, participant, _ServiceAf) ->
    false;
can_change_ra(owner, _FRole,
          _TAffiliation, moderator,
          role, participant, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, _FRole,
          admin, moderator,
          role, participant, _ServiceAf) ->
    false;
can_change_ra(admin, _FRole,
          _TAffiliation, moderator,
          role, participant, _ServiceAf) ->
    true;
can_change_ra(_FAffiliation, _FRole,
          _TAffiliation, _TRole,
          role, _Value, _ServiceAf) ->
    false.

safe_send_kickban_presence(JID, Reason, Code, StateData) ->
    try
        send_kickban_presence(JID, Reason, Code, StateData)
    catch Error:Reason ->
        ?ERROR_MSG("issue=send_kickban_presence_failed reason=~p:~p",
                   [Error, Reason])
    end.

-spec send_kickban_presence(jid:jid(), binary(), Code :: binary(),
                            state()) -> any().
send_kickban_presence(JID, Reason, Code, StateData) ->
    NewAffiliation = get_affiliation(JID, StateData),
    send_kickban_presence(JID, Reason, Code, NewAffiliation, StateData).


safe_send_kickban_presence(JID, Reason, Code, NewAffiliation, StateData) ->
    try
        send_kickban_presence(JID, Reason, Code, NewAffiliation, StateData)
    catch Error:Reason ->
        ?ERROR_MSG("issue=send_kickban_presence_failed reason=~p:~p",
                   [Error, Reason])
    end.

-spec send_kickban_presence(jid:simple_jid() | jid:jid(),
                            Reason :: binary(), Code :: binary(),
                            mod_muc:affiliation(), state()) -> any().
send_kickban_presence(JID, Reason, Code, NewAffiliation, StateData) ->
    foreach_matched_user(fun(#user{nick = Nick, jid = J}) ->
      add_to_log(kickban, {Nick, Reason, Code}, StateData),
      tab_remove_online_user(J, StateData),
      send_kickban_presence1(J, Reason, Code, NewAffiliation, StateData)
    end, JID, StateData).


-spec send_kickban_presence1(jid:jid(), Reason :: binary(), Code :: binary(),
                             mod_muc:affiliation(), state()) -> 'ok'.
send_kickban_presence1(UJID, Reason, Code, Affiliation, StateData) ->
    {ok, #user{jid = RealJID,
           nick = Nick}} =
    maps:find(jid:to_lower(UJID), StateData#state.users),
    BAffiliation = affiliation_to_binary(Affiliation),
    BannedJIDString = jid:to_binary(RealJID),
    F = fun(Info) ->
          JidAttrList = case (Info#user.role == moderator) orelse
                ((StateData#state.config)#config.anonymous
                 == false) of
                true -> [{<<"jid">>, BannedJIDString}];
                false -> []
                end,
          ItemAttrs = [{<<"affiliation">>, BAffiliation},
               {<<"role">>, <<"none">>}] ++ JidAttrList,
          ItemEls = case Reason of
                <<>> ->
                [];
                _ ->
                [#xmlel{name = <<"reason">>, children = [#xmlcdata{content = Reason}]}]
                    end,
          Packet = #xmlel{name = <<"presence">>,
                          attrs = [{<<"type">>, <<"unavailable">>}],
                          children = [#xmlel{name = <<"x">>,
                                             attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
                                             children = [#xmlel{name = <<"item">>,
                                                                attrs = ItemAttrs,
                                                                children = ItemEls},
                                                         #xmlel{name = <<"status">>,
                                                                attrs = [{<<"code">>, Code}]}]}]},
          ejabberd_router:route(
        jid:replace_resource(StateData#state.jid, Nick),
        Info#user.jid,
        Packet)
      end,
    foreach_user(F, StateData).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Owner stuff

-spec process_iq_owner(jid:jid(), get | set, ejabberd:lang(), exml:element(),
                       state(), statename()) ->
    {error, exml:element()} | {result, [exml:element() | jlib:xmlcdata()], state() | stop}.
process_iq_owner(From, Type, Lang, SubEl, StateData, StateName) ->
    case get_affiliation(From, StateData) of
        owner ->
            process_authorized_iq_owner(From, Type, Lang, SubEl, StateData, StateName);
        _ ->
            ErrText = <<"Owner privileges required">>,
            {error, mongoose_xmpp_errors:forbidden(Lang, ErrText)}
    end.

-spec process_authorized_iq_owner(jid:jid(), get | set, ejabberd:lang(), exml:element(),
                                  state(), statename()) ->
    {error, exml:element()} | {result, [exml:element() | jlib:xmlcdata()], state() | stop}.
process_authorized_iq_owner(From, set, Lang, SubEl, StateData, StateName) ->
    #xmlel{children = Els} = SubEl,
    case xml:remove_cdata(Els) of
        [#xmlel{name = <<"x">>} = XEl] ->
            case {xml:get_tag_attr_s(<<"xmlns">>, XEl),
                  xml:get_tag_attr_s(<<"type">>, XEl),
                  StateName} of
                {?NS_XDATA, <<"cancel">>, locked_state} ->
                    %% received cancel before the room was configured - destroy room
                    ?INFO_MSG("Destroyed MUC room ~s by the owner ~s : cancelled",
                              [jid:to_binary(StateData#state.jid), jid:to_binary(From)]),
                    add_to_log(room_existence, destroyed, StateData),
                    destroy_room(XEl, StateData);
                {?NS_XDATA, <<"cancel">>, normal_state} ->
                    %% received cancel when room was configured - continue without changes
                    {result, [], StateData};
                {?NS_XDATA, <<"submit">>, _} ->
                    process_authorized_submit_owner(From, XEl, StateData);
                _ ->
                    {error, mongoose_xmpp_errors:bad_request()}
            end;
        [#xmlel{name = <<"destroy">>} = SubEl1] ->
            ?INFO_MSG("Destroyed MUC room ~s by the owner ~s",
                      [jid:to_binary(StateData#state.jid), jid:to_binary(From)]),
            add_to_log(room_existence, destroyed, StateData),
            destroy_room(SubEl1, StateData);
        Items ->
            process_admin_items_set(From, Items, Lang, StateData)
    end;
process_authorized_iq_owner(From, get, Lang, SubEl, StateData, _StateName) ->
    case exml_query:path(SubEl, [{element, <<"item">>}, {attr, <<"affiliation">>}]) of
        undefined ->
            get_config(Lang, StateData, From);
        BAffiliation ->
            case catch binary_to_affiliation(BAffiliation) of
                {'EXIT', _} ->
                    InvAffT = translate:translate(Lang, <<"Invalid affiliation ">>),
                    ErrText = <<InvAffT/binary, BAffiliation/binary>>,
                    {error, mongoose_xmpp_errors:not_acceptable(Lang, ErrText)};
                Affiliation ->
                    Items = items_with_affiliation(Affiliation, StateData),
                    {result, Items, StateData}
            end
    end.

-spec process_authorized_submit_owner(From ::jid:jid(), XEl :: exml:element(), StateData :: state()) ->
    {error, exml:element()} | {result, [exml:element() | jlib:xmlcdata()], state() | stop}.
process_authorized_submit_owner(_From, #xmlel{ children = [] } = _XEl, StateData) ->
    %confrm an instant room
    {result, [], StateData};
process_authorized_submit_owner(From, XEl, StateData) ->
    %attepmt to configure
    case is_allowed_log_change(XEl, StateData, From)
         andalso is_allowed_persistent_change(XEl, StateData, From)
         andalso is_allowed_room_name_desc_limits(XEl, StateData)
         andalso is_password_settings_correct(XEl, StateData) of
        true -> set_config(XEl, StateData);
        false -> {error, mongoose_xmpp_errors:not_acceptable()}
    end.

-spec is_allowed_log_change(exml:element(), state(), jid:jid()) -> boolean().
is_allowed_log_change(XEl, StateData, From) ->
    case lists:keymember(<<"muc#roomconfig_enablelogging">>, 1,
             jlib:parse_xdata_submit(XEl)) of
    false ->
        true;
    true ->
        (allow == mod_muc_log:check_access_log(
          StateData#state.server_host, From))
    end.


-spec is_allowed_persistent_change(exml:element(), state(), jid:jid()) -> boolean().
is_allowed_persistent_change(XEl, StateData, From) ->
    case lists:keymember(<<"muc#roomconfig_persistentroom">>, 1,
             jlib:parse_xdata_submit(XEl)) of
    false ->
        true;
    true ->
        AccessPersistent = access_persistent(StateData),
        (allow == acl:match_rule(StateData#state.server_host, AccessPersistent, From))
    end.


%% @doc Check if the Room Name and Room Description defined in the Data Form
%% are conformant to the configured limits
-spec is_allowed_room_name_desc_limits(exml:element(), state()) -> boolean().
is_allowed_room_name_desc_limits(XEl, StateData) ->
    IsNameAccepted =
    case lists:keysearch(<<"muc#roomconfig_roomname">>, 1,
                 jlib:parse_xdata_submit(XEl)) of
        {value, {_, [N]}} ->
        byte_size(N) =< gen_mod:get_module_opt(StateData#state.server_host,
                            mod_muc, max_room_name,
                            infinite);
        _ ->
        true
    end,
    IsDescAccepted =
    case lists:keysearch(<<"muc#roomconfig_roomdesc">>, 1,
                 jlib:parse_xdata_submit(XEl)) of
        {value, {_, [D]}} ->
        byte_size(D) =< gen_mod:get_module_opt(StateData#state.server_host,
                            mod_muc, max_room_desc,
                            infinite);
        _ ->
        true
    end,
    IsNameAccepted and IsDescAccepted.

%% @doc Return false if:
%% `<<"the password for a password-protected room is blank">>'
-spec is_password_settings_correct(exml:element(), state()) -> boolean().
is_password_settings_correct(XEl, StateData) ->
    Config = StateData#state.config,
    OldProtected = Config#config.password_protected,
    OldPassword = Config#config.password,
    NewProtected =
    case lists:keysearch(<<"muc#roomconfig_passwordprotectedroom">>, 1,
                 jlib:parse_xdata_submit(XEl)) of
        {value, {_, [<<"1">>]}} ->
        true;
        {value, {_, [<<"0">>]}} ->
        false;
        _ ->
        undefined
    end,
    NewPassword =
    case lists:keysearch(<<"muc#roomconfig_roomsecret">>, 1,
                 jlib:parse_xdata_submit(XEl)) of
        {value, {_, [P]}} ->
        P;
        _ ->
        undefined
    end,
    case {OldProtected, NewProtected, OldPassword, NewPassword} of
    {true, undefined, <<>>, undefined} ->
        false;
    {true, undefined, _, <<>>} ->
        false;
    {_, true, <<>>, undefined} ->
        false;
    {_, true, _, <<>>} ->
        false;
    _ ->
        true
    end.


-spec get_default_room_maxusers(state()) -> any().
get_default_room_maxusers(RoomState) ->
    DefRoomOpts = gen_mod:get_module_opt(
                    RoomState#state.server_host, mod_muc, default_room_options, []),
    RoomState2 = set_opts(DefRoomOpts, RoomState),
    (RoomState2#state.config)#config.max_users.


-spec get_config(ejabberd:lang(), state(), jid:jid())
            -> {'result', [exml:element(), ...], state()}.
get_config(Lang, StateData, From) ->
    AccessPersistent = access_persistent(StateData),
    Config = StateData#state.config,

    TitleTxt = translate:translate(Lang, <<"Configuration of room ">>),
    Res =
    [#xmlel{name = <<"title">>,
            children = [#xmlcdata{content = <<TitleTxt/binary,
                                              (jid:to_binary(StateData#state.jid))/binary>>}]},
     #xmlel{name = <<"field">>,
            attrs = [{<<"type">>, <<"hidden">>},
          {<<"var">>, <<"FORM_TYPE">>}],
            children = [#xmlel{name = <<"value">>,
                               children = [#xmlcdata{content = ?NS_MUC_CONFIG}]}]},
     stringxfield(<<"Room title">>,
               <<"muc#roomconfig_roomname">>,
                Config#config.title, Lang),
     stringxfield(<<"Room description">>,
               <<"muc#roomconfig_roomdesc">>,
                Config#config.description, Lang)
    ] ++
     case acl:match_rule(StateData#state.server_host, AccessPersistent, From) of
        allow ->
            [boolxfield(<<"Make room persistent">>,
             <<"muc#roomconfig_persistentroom">>,
              Config#config.persistent, Lang)];
        _ -> []
     end ++ [
     boolxfield(<<"Make room public searchable">>,
             <<"muc#roomconfig_publicroom">>,
              Config#config.public, Lang),
     boolxfield(<<"Make participants list public">>,
             <<"public_list">>,
              Config#config.public_list, Lang),
     boolxfield(<<"Make room password protected">>,
             <<"muc#roomconfig_passwordprotectedroom">>,
              Config#config.password_protected, Lang),
     privatexfield(<<"Password">>,
            <<"muc#roomconfig_roomsecret">>,
            case Config#config.password_protected of
                true -> Config#config.password;
                false -> <<>>
            end, Lang),
     getmemberlist_field(Lang),
     maxusers_field(Lang, StateData),
     whois_field(Lang, Config),
     boolxfield(<<"Make room members-only">>,
             <<"muc#roomconfig_membersonly">>,
              Config#config.members_only, Lang),
     boolxfield(<<"Make room moderated">>,
             <<"muc#roomconfig_moderatedroom">>,
              Config#config.moderated, Lang),
     boolxfield(<<"Default users as participants">>,
             <<"members_by_default">>,
              Config#config.members_by_default, Lang),
     boolxfield(<<"Allow users to change the subject">>,
             <<"muc#roomconfig_changesubject">>,
              Config#config.allow_change_subj, Lang),
     boolxfield(<<"Allow users to send private messages">>,
             <<"allow_private_messages">>,
              Config#config.allow_private_messages, Lang),
     boolxfield(<<"Allow users to query other users">>,
             <<"allow_query_users">>,
              Config#config.allow_query_users, Lang),
     boolxfield(<<"Allow users to send invites">>,
             <<"muc#roomconfig_allowinvites">>,
              Config#config.allow_user_invites, Lang),
     boolxfield(<<"Allow users to enter room with multiple sessions">>,
             <<"muc#roomconfig_allowmultisessions">>,
              Config#config.allow_multiple_sessions, Lang),
     boolxfield(<<"Allow visitors to send status text in presence updates">>,
             <<"muc#roomconfig_allowvisitorstatus">>,
              Config#config.allow_visitor_status, Lang),
     boolxfield(<<"Allow visitors to change nickname">>,
             <<"muc#roomconfig_allowvisitornickchange">>,
              Config#config.allow_visitor_nickchange, Lang)
    ] ++
     case mod_muc_log:check_access_log(StateData#state.server_host, From) of
         allow ->
             [boolxfield(
                <<"Enable logging">>,
                <<"muc#roomconfig_enablelogging">>,
                Config#config.logging, Lang)];
         _ -> []
     end,
    InstructionsTxt = translate:translate(
                        Lang, <<"You need an x:data capable client to configure room">>),
    {result, [#xmlel{name = <<"instructions">>, children = [#xmlcdata{content = InstructionsTxt}]},
              #xmlel{name = <<"x">>,
                     attrs = [{<<"xmlns">>, ?NS_XDATA},
                              {<<"type">>, <<"form">>}],
                     children = Res}],
     StateData}.

-spec getmemberlist_field(Lang :: ejabberd:lang()) -> exml:element().
getmemberlist_field(Lang) ->
    LabelTxt = translate:translate(
                 Lang, <<"Roles and affiliations that may retrieve member list">>),
    OptModerator = #xmlel{name = <<"option">>,
                          attrs = [{<<"label">>, translate:translate(Lang, <<"moderator">>)}],
                          children = [
                                      #xmlel{name = <<"value">>,
                                             children = [#xmlcdata{content = <<"moderator">>}]}
                                     ]},
    OptParticipant = #xmlel{name = <<"option">>,
                            attrs = [{<<"label">>, translate:translate(Lang, <<"participant">>)}],
                            children = [
                                        #xmlel{name = <<"value">>,
                                               children = [#xmlcdata{content = <<"participant">>}]}
                                       ]},
    OptVisitor = #xmlel{name = <<"option">>,
                              attrs = [{<<"label">>, translate:translate(Lang, <<"visitor">>)}],
                              children = [
                                          #xmlel{name = <<"value">>,
                                                 children = [#xmlcdata{content = <<"visitor">>}]}
                                         ]},
    #xmlel{name = <<"field">>,
           attrs = [{<<"type">>, <<"list-multi">>},
                    {<<"label">>, LabelTxt},
                    {<<"var">>, <<"muc#roomconfig_getmemberlist">>}],
           children = [
                       #xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = <<"moderator">>}]},
                       #xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = <<"participant">>}]},
                       #xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = <<"visitor">>}]},
                       OptModerator,
                       OptParticipant,
                       OptVisitor
                      ]
          }.

maxusers_field(Lang, StateData) ->
    ServiceMaxUsers = get_service_max_users(StateData),
    DefaultRoomMaxUsers = get_default_room_maxusers(StateData),
    {MaxUsersRoomInteger, MaxUsersRoomString} =
    case get_max_users(StateData) of
        N when is_integer(N) ->
            {N, integer_to_binary(N)};
        _ -> {0, <<"none">>}
    end,

    #xmlel{name = <<"field">>,
           attrs = [{<<"type">>, <<"list-single">>},
                    {<<"label">>, translate:translate(Lang, <<"Maximum Number of Occupants">>)},
                    {<<"var">>, <<"muc#roomconfig_maxusers">>}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = MaxUsersRoomString}]}] ++
           if
               is_integer(ServiceMaxUsers) -> [];
               true ->
                   [#xmlel{name = <<"option">>,
                           attrs = [{<<"label">>, translate:translate(Lang, <<"No limit">>)}],
                           children = [#xmlel{name = <<"value">>,
                                              children = [#xmlcdata{content = <<"none">>}]}]}]
           end ++
           [#xmlel{name = <<"option">>,
                   attrs = [{<<"label">>, list_to_binary(integer_to_list(N))}],
                   children = [#xmlel{name = <<"value">>,
                                      children = [#xmlcdata{content = integer_to_binary(N)}]}]} ||
            N <- lists:usort([ServiceMaxUsers, DefaultRoomMaxUsers, MaxUsersRoomInteger |
                              ?MAX_USERS_DEFAULT_LIST]), N =< ServiceMaxUsers]}.

-spec whois_field(Lang :: ejabberd:lang(), Config :: config()) -> exml:element().
whois_field(Lang, Config) ->
    OptModerators = #xmlel{name = <<"option">>,
                           attrs = [{<<"label">>,
                                     translate:translate(Lang, <<"moderators only">>)}],
                           children = [#xmlel{name = <<"value">>,
                                              children = [#xmlcdata{content = <<"moderators">>}]}]},
    OptAnyone = #xmlel{name = <<"option">>,
                       attrs = [{<<"label">>, translate:translate(Lang, <<"anyone">>)}],
                       children = [#xmlel{name = <<"value">>,
                                          children = [#xmlcdata{content = <<"anyone">>}]}]},
    #xmlel{name = <<"field">>,
           attrs = [{<<"type">>, <<"list-single">>},
                    {<<"label">>, translate:translate(Lang, <<"Present real Jabber IDs to">>)},
                    {<<"var">>, <<"muc#roomconfig_whois">>}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = if Config#config.anonymous ->
                                                                     <<"moderators">>;
                                                                 true ->
                                                                     <<"anyone">>
                                                              end}]},
                       OptModerators,
                       OptAnyone]}.

-spec set_config(exml:element(), state()) -> any().
set_config(XEl, StateData) ->
    XData = jlib:parse_xdata_submit(XEl),
    case XData of
        invalid ->
            {error, mongoose_xmpp_errors:bad_request()};
        _ ->
            case set_xoption(XData, StateData#state.config) of
                #config{} = Config ->
                    Res = change_config(Config, StateData),
                    {result, _, NSD} = Res,
                    PrevLogging = (StateData#state.config)#config.logging,
                    NewLogging = Config#config.logging,
                    PrevAnon = (StateData#state.config)#config.anonymous,
                    NewAnon = Config#config.anonymous,
                    Type = notify_config_change_and_get_type(PrevLogging, NewLogging,
                                                             PrevAnon, NewAnon, StateData),
                    Users = [{U#user.jid, U#user.nick, U#user.role} ||
                             {_, U} <- maps:to_list(StateData#state.users)],
                    add_to_log(Type, Users, NSD),
                    Res;
                Err ->
                    Err
            end
    end.

-spec notify_config_change_and_get_type(PrevLogging :: boolean(), NewLogging :: boolean(),
                                        PrevAnon :: boolean(), NewAnon :: boolean(),
                                        StateData :: state()) ->
    roomconfig_change_disabledlogging | roomconfig_change_enabledlogging
    | roomconfig_change_nonanonymous | roomconfig_change_anonymous | roomconfig_change.
notify_config_change_and_get_type(true, false, _, _, StateData) ->
    send_config_update(logging_disabled, StateData),
    roomconfig_change_disabledlogging;
notify_config_change_and_get_type(false, true, _, _, StateData) ->
    send_config_update(logging_enabled, StateData),
    roomconfig_change_enabledlogging;
notify_config_change_and_get_type(_, _, true, false, StateData) ->
    send_config_update(nonanonymous, StateData),
    roomconfig_change_nonanonymous;
notify_config_change_and_get_type(_, _, false, true, StateData) ->
    send_config_update(semianonymous, StateData),
    roomconfig_change_anonymous;
notify_config_change_and_get_type(_, _, _, _, _StateData) ->
    roomconfig_change.

-define(SET_BOOL_XOPT(Opt, Val),
    case Val of
        <<"0">> -> set_xoption(Opts, Config#config{Opt = false});
        <<"false">> -> set_xoption(Opts, Config#config{Opt = false});
        <<"1">> -> set_xoption(Opts, Config#config{Opt = true});
        <<"true">> -> set_xoption(Opts, Config#config{Opt = true});
        _ -> {error, mongoose_xmpp_errors:bad_request()}
    end).

-define(SET_NAT_XOPT(Opt, Val),
    case catch binary_to_integer(Val) of
        I when is_integer(I),
               I > 0 ->
        set_xoption(Opts, Config#config{Opt = I});
        _ ->
        {error, mongoose_xmpp_errors:bad_request()}
    end).

-define(SET_XOPT(Opt, Val),
    set_xoption(Opts, Config#config{Opt = Val})).

-spec set_xoption([{binary(), [binary()]}], config()) -> config() | {error, exml:element()}.
set_xoption([], Config) ->
    Config;
set_xoption([{<<"muc#roomconfig_roomname">>, [Val]} | Opts], Config) ->
    ?SET_XOPT(title, Val);
set_xoption([{<<"muc#roomconfig_roomdesc">>, [Val]} | Opts], Config) ->
    ?SET_XOPT(description, Val);
set_xoption([{<<"muc#roomconfig_changesubject">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_change_subj, Val);
set_xoption([{<<"allow_query_users">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_query_users, Val);
set_xoption([{<<"allow_private_messages">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_private_messages, Val);
set_xoption([{<<"muc#roomconfig_allowvisitorstatus">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_visitor_status, Val);
set_xoption([{<<"muc#roomconfig_allowvisitornickchange">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_visitor_nickchange, Val);
set_xoption([{<<"muc#roomconfig_publicroom">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(public, Val);
set_xoption([{<<"public_list">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(public_list, Val);
set_xoption([{<<"muc#roomconfig_persistentroom">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(persistent, Val);
set_xoption([{<<"muc#roomconfig_moderatedroom">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(moderated, Val);
set_xoption([{<<"members_by_default">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(members_by_default, Val);
set_xoption([{<<"muc#roomconfig_membersonly">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(members_only, Val);
set_xoption([{<<"muc#roomconfig_allowinvites">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_user_invites, Val);
set_xoption([{<<"muc#roomconfig_allowmultisessions">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_multiple_sessions, Val);
set_xoption([{<<"muc#roomconfig_passwordprotectedroom">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(password_protected, Val);
set_xoption([{<<"muc#roomconfig_roomsecret">>, [Val]} | Opts], Config) ->
    ?SET_XOPT(password, Val);
set_xoption([{<<"anonymous">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(anonymous, Val);
set_xoption([{<<"muc#roomconfig_whois">>, [Val]} | Opts], Config) ->
    case Val of
    <<"moderators">> ->
        ?SET_XOPT(anonymous, true);
    <<"anyone">> ->
        ?SET_XOPT(anonymous, false);
    _ ->
        {error, mongoose_xmpp_errors:bad_request()}
    end;
set_xoption([{<<"muc#roomconfig_maxusers">>, [Val]} | Opts], Config) ->
    case Val of
    <<"none">> ->
        ?SET_XOPT(max_users, none);
    _ ->
        ?SET_NAT_XOPT(max_users, Val)
    end;
set_xoption([{<<"muc#roomconfig_getmemberlist">>, Val} | Opts], Config) ->
    case Val of
        [<<"none">>] ->
            ?SET_XOPT(maygetmemberlist, []);
        _ ->
            ?SET_XOPT(maygetmemberlist, [binary_to_role(V) || V <- Val])
    end;
set_xoption([{<<"muc#roomconfig_enablelogging">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(logging, Val);
set_xoption([{<<"FORM_TYPE">>, _} | Opts], Config) ->
    %% Ignore our FORM_TYPE
    set_xoption(Opts, Config);
set_xoption([_ | _Opts], _Config) ->
    {error, mongoose_xmpp_errors:bad_request()}.


-spec change_config(config(), state()) -> {'result', [], state()}.
change_config(Config, StateData) ->
    NSD = StateData#state{config = Config},
    case {(StateData#state.config)#config.persistent,
      Config#config.persistent} of
    {_, true} ->
        mod_muc:store_room(NSD#state.server_host, NSD#state.host, NSD#state.room, make_opts(NSD));
    {true, false} ->
        mod_muc:forget_room(NSD#state.server_host, NSD#state.host, NSD#state.room);
    {false, false} ->
        ok
    end,
    case {(StateData#state.config)#config.members_only,
          Config#config.members_only} of
    {false, true} ->
        NSD1 = remove_nonmembers(NSD),
        {result, [], NSD1};
    _ ->
        {result, [], NSD}
    end.


-spec remove_nonmembers(state()) -> state().
remove_nonmembers(StateData) ->
    F = fun(_LJID, #user{jid = JID}, SD) ->
        Affiliation = get_affiliation(JID, SD),
        case Affiliation of
        none ->
            safe_send_kickban_presence(JID, <<>>, <<"322">>, SD),
            set_role(JID, none, SD);
        _ ->
            SD
        end
      end,
    maps:fold(F, StateData, StateData#state.users).

-spec set_opts(Opts :: [{atom(), term()}], state()) -> state().
set_opts([], SD) ->
    SD;
set_opts([{Opt, Val} | Opts], SD=#state{config = C = #config{}}) ->
    NSD = case Opt of
        title ->
            SD#state{config = C#config{title = Val}};
        description ->
            SD#state{config = C#config{description = Val}};
        allow_change_subj ->
            SD#state{config = C#config{allow_change_subj = Val}};
        allow_query_users ->
            SD#state{config = C#config{allow_query_users = Val}};
        allow_private_messages ->
            SD#state{config = C#config{allow_private_messages = Val}};
        allow_visitor_nickchange ->
            SD#state{config = C#config{allow_visitor_nickchange = Val}};
        allow_visitor_status ->
            SD#state{config = C#config{allow_visitor_status = Val}};
        public ->
            SD#state{config = C#config{public = Val}};
        public_list ->
            SD#state{config = C#config{public_list = Val}};
        persistent ->
            SD#state{config = C#config{persistent = Val}};
        moderated ->
            SD#state{config = C#config{moderated = Val}};
        members_by_default ->
            SD#state{config = C#config{members_by_default = Val}};
        members_only ->
            SD#state{config = C#config{members_only = Val}};
        allow_user_invites ->
            SD#state{config = C#config{allow_user_invites = Val}};
        allow_multiple_sessions ->
            SD#state{config = C#config{allow_multiple_sessions = Val}};
        password_protected ->
            SD#state{config = C#config{password_protected = Val}};
        password ->
            SD#state{config = C#config{password = Val}};
        anonymous ->
            SD#state{config = C#config{anonymous = Val}};
        logging ->
            SD#state{config = C#config{logging = Val}};
        max_users ->
            MaxUsers = min(Val, get_service_max_users(SD)),
            SD#state{config = C#config{max_users = MaxUsers}};
        maygetmemberlist ->
            SD#state{config = C#config{maygetmemberlist = Val}};
        affiliations ->
            SD#state{affiliations = maps:from_list(Val)};
        subject ->
            SD#state{subject = Val};
        subject_author ->
            SD#state{subject_author = Val};
        _ ->
            SD
       end,
    set_opts(Opts, NSD).


-define(MAKE_CONFIG_OPT(Opt), {Opt, Config#config.Opt}).

-spec make_opts(state()) -> [{atom(), _}, ...].
make_opts(StateData) ->
    Config = StateData#state.config,
    [
     ?MAKE_CONFIG_OPT(title),
     ?MAKE_CONFIG_OPT(description),
     ?MAKE_CONFIG_OPT(allow_change_subj),
     ?MAKE_CONFIG_OPT(allow_query_users),
     ?MAKE_CONFIG_OPT(allow_private_messages),
     ?MAKE_CONFIG_OPT(allow_visitor_status),
     ?MAKE_CONFIG_OPT(allow_visitor_nickchange),
     ?MAKE_CONFIG_OPT(public),
     ?MAKE_CONFIG_OPT(public_list),
     ?MAKE_CONFIG_OPT(persistent),
     ?MAKE_CONFIG_OPT(moderated),
     ?MAKE_CONFIG_OPT(members_by_default),
     ?MAKE_CONFIG_OPT(members_only),
     ?MAKE_CONFIG_OPT(allow_user_invites),
     ?MAKE_CONFIG_OPT(allow_multiple_sessions),
     ?MAKE_CONFIG_OPT(password_protected),
     ?MAKE_CONFIG_OPT(password),
     ?MAKE_CONFIG_OPT(anonymous),
     ?MAKE_CONFIG_OPT(logging),
     ?MAKE_CONFIG_OPT(max_users),
     ?MAKE_CONFIG_OPT(maygetmemberlist),
     {affiliations, maps:to_list(StateData#state.affiliations)},
     {subject, StateData#state.subject},
     {subject_author, StateData#state.subject_author}
    ].


-spec destroy_room(exml:element(), state()) -> {result, [], stop}.
destroy_room(DestroyEl, StateData) ->
    remove_each_occupant_from_room(DestroyEl, StateData),
    case (StateData#state.config)#config.persistent of
        true ->
            mod_muc:forget_room(StateData#state.server_host,
                                StateData#state.host,
                                StateData#state.room);
        false ->
            ok
    end,
    {result, [], stop}.


%% @doc Service Removes Each Occupant
%%
%% Send only one presence stanza of type "unavailable" to each occupant
%% so that the user knows he or she has been removed from the room.
%%
%% If extended presence information specifying the JID of an alternate
%% location and the reason for the room destruction was provided by the
%% room owner, the presence stanza MUST include that information.
%% @end
-spec remove_each_occupant_from_room(exml:element(), state()) -> any().
remove_each_occupant_from_room(DestroyEl, StateData) ->
    Packet = presence_stanza_of_type_unavailable(DestroyEl),
    send_to_occupants(Packet, StateData).


-spec send_to_occupants(exml:element(), state()) -> any().
send_to_occupants(Packet, StateData=#state{jid=RoomJID}) ->
    F = fun(User=#user{jid=UserJID}) ->
        ejabberd_router:route(occupant_jid(User, RoomJID), UserJID, Packet)
        end,
    foreach_user(F, StateData).

-spec send_to_all_users(exml:element(), state()) -> any().
send_to_all_users(Packet, StateData=#state{jid=RoomJID}) ->
    F = fun(#user{jid = UserJID}) ->
          ejabberd_router:route(RoomJID, UserJID, Packet)
      end,
    foreach_user(F, StateData).


-spec presence_stanza_of_type_unavailable(exml:element()) -> exml:element().
presence_stanza_of_type_unavailable(DestroyEl) ->
    ItemEl = #xmlel{
        name = <<"item">>,
        attrs = [{<<"affiliation">>, <<"none">>}, {<<"role">>, <<"none">>}]},
    XEl = #xmlel{
        name = <<"x">>,
        attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
        children = [ItemEl, DestroyEl]},
    #xmlel{
        name = <<"presence">>,
        attrs = [{<<"type">>, <<"unavailable">>}],
        children = [XEl]}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Disco

-spec feature(binary()) -> exml:element().
feature(Var) ->
    #xmlel{name = <<"feature">>,
           attrs = [{<<"var">>, Var}]}.


-spec config_opt_to_feature(boolean(), Fiftrue :: binary(), Fiffalse :: binary())
                            -> exml:element().
config_opt_to_feature(Opt, Fiftrue, Fiffalse) ->
    case Opt of
        true -> feature(Fiftrue);
        false -> feature(Fiffalse)
    end.


-spec process_iq_disco_info(jid:jid(), 'get' | 'set', ejabberd:lang(),
                            state()) -> {'error', exml:element()}
                                      | {'result', [exml:element(), ...], state()}.
process_iq_disco_info(_From, set, _Lang, _StateData) ->
    {error, mongoose_xmpp_errors:not_allowed()};
process_iq_disco_info(From, get, Lang, StateData) ->
    Config = StateData#state.config,
    RoomJID = StateData#state.jid,
    {result, RegisteredFeatures} = mod_disco:get_local_features(empty, From, RoomJID, <<>>, <<>>),
    RegisteredFeaturesXML = [#xmlel{name = <<"feature">>, attrs = [{<<"var">>, URN}]} ||
                             {{URN, _Host}} <- RegisteredFeatures],
    {result, [#xmlel{name = <<"identity">>,
                     attrs = [{<<"category">>, <<"conference">>},
                          {<<"type">>, <<"text">>},
                          {<<"name">>, get_title(StateData)}]},
              #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_MUC}]},
              config_opt_to_feature((Config#config.public),
                         <<"muc_public">>, <<"muc_hidden">>),
              config_opt_to_feature((Config#config.persistent),
                         <<"muc_persistent">>, <<"muc_temporary">>),
              config_opt_to_feature((Config#config.members_only),
                         <<"muc_membersonly">>, <<"muc_open">>),
              config_opt_to_feature((Config#config.anonymous),
                         <<"muc_semianonymous">>, <<"muc_nonanonymous">>),
              config_opt_to_feature((Config#config.moderated),
                         <<"muc_moderated">>, <<"muc_unmoderated">>),
              config_opt_to_feature((Config#config.password_protected),
                         <<"muc_passwordprotected">>, <<"muc_unsecured">>)
             ] ++ iq_disco_info_extras(Lang, StateData) ++ RegisteredFeaturesXML, StateData}.


-spec rfieldt(binary(), binary(), binary()) -> exml:element().
rfieldt(Type, Var, Val) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"type">>, Type}, {<<"var">>, Var}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = Val}]}]}.


-spec rfield(binary(), binary(), binary() | iolist(), ejabberd:lang()) -> exml:element().
rfield(Label, Var, Val, Lang) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"label">>, translate:translate(Lang, Label)},
             {<<"var">>, Var}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = Val}]}]}.


-spec iq_disco_info_extras(ejabberd:lang(), state()) -> [exml:element(), ...].
iq_disco_info_extras(Lang, StateData) ->
    Len = maps:size(StateData#state.users),
    RoomDescription = (StateData#state.config)#config.description,
    [#xmlel{name = <<"x">>,
            attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"result">>}],
            children = [rfieldt(<<"hidden">>, <<"FORM_TYPE">>,
                         <<"http://jabber.org/protocol/muc#roominfo">>),
                        rfield(<<"Room description">>, <<"muc#roominfo_description">>,
                            RoomDescription, Lang),
                        rfield(<<"Number of occupants">>, <<"muc#roominfo_occupants">>,
                            (list_to_binary(integer_to_list(Len))), Lang)
                       ]}].


-spec process_iq_disco_items(jid:jid(), 'get' | 'set', ejabberd:lang(),
                            state()) -> {'error', exml:element()}
                                      | {'result', [exml:element()], state()}.
process_iq_disco_items(_From, set, _Lang, _StateData) ->
    {error, mongoose_xmpp_errors:not_allowed()};
process_iq_disco_items(From, get, _Lang, StateData) ->
    case (StateData#state.config)#config.public_list of
    true ->
        {result, get_mucroom_disco_items(StateData), StateData};
    _ ->
        case is_occupant_or_admin(From, StateData) of
        true ->
            {result, get_mucroom_disco_items(StateData), StateData};
        _ ->
            {error, mongoose_xmpp_errors:forbidden()}
        end
    end.


-spec get_title(state()) -> binary() | mod_muc:room().
get_title(StateData) ->
    case (StateData#state.config)#config.title of
    <<>> ->
        StateData#state.room;
    Name ->
        Name
    end.


-spec get_roomdesc_reply(jid:jid(), state(), Tail :: binary()
                        ) -> 'false' | {'item', _}.
get_roomdesc_reply(JID, StateData, Tail) ->
    IsOccupantOrAdmin = is_occupant_or_admin(JID, StateData),
    case {(StateData#state.config)#config.public or IsOccupantOrAdmin,
          (StateData#state.config)#config.public_list or IsOccupantOrAdmin} of
        {true, true} ->
            Title = get_title(StateData),
            {item, <<Title/binary, Tail/binary>>};
        {true, false} ->
            {item, get_title(StateData)};
        _ ->
            false
    end.


-spec get_roomdesc_tail(state(), ejabberd:lang()) -> binary().
get_roomdesc_tail(StateData, Lang) ->
    Desc = case (StateData#state.config)#config.public of
               true ->
                   <<>>;
               _ ->
                   translate:translate(Lang, <<"private, ">>)
           end,
    Count = count_users(StateData),
    CountBin = list_to_binary(integer_to_list(Count)),
    <<" (", Desc/binary, CountBin/binary, ")">>.


-spec get_mucroom_disco_items(state()) -> [exml:element()].
get_mucroom_disco_items(StateData=#state{jid=RoomJID}) ->
    maps:fold(fun(_LJID, User, Acc) ->
                      Item = disco_item(User, RoomJID),
                      [Item|Acc]
              end, [], StateData#state.users).

-spec disco_item(user(), 'undefined' | jid:jid()) -> exml:element().
disco_item(User=#user{nick=Nick}, RoomJID) ->
    #xmlel{
        name = <<"item">>,
        attrs = [{<<"jid">>, jid:to_binary(occupant_jid(User, RoomJID))},
                 {<<"name">>, Nick}]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle voice request or approval (XEP-0045 7.13, 8.6)
-spec check_voice_approval(From :: jid:jid(), Els :: [exml:element()],
        Lang :: ejabberd:lang(), StateData :: state()
        ) -> {form, BRole :: binary()}
           | {role, BRole :: binary(), RoomNick :: mod_muc:nick()}
           | {error, any()}
           | ok.
check_voice_approval(From, [#xmlel{name = <<"x">>,
                                   children = Items}], _Lang, StateData) ->
    BRole = get_field(<<"muc#role">>, Items),
    case Items of
        [_Form, _Role] ->
            case catch binary_to_role(BRole) of
                {'EXIT', _} -> {error, mongoose_xmpp_errors:bad_request()};
                _ -> {form, BRole}
            end;
        _ ->
            case {get_role(From, StateData),
                  get_field(<<"muc#request_allow">>, Items),
                  get_field(<<"muc#roomnick">>, Items)} of
                {moderator, <<"true">>, false} -> {error, mongoose_xmpp_errors:bad_request()};
                {moderator, <<"true">>, RoomNick} -> {role, BRole, RoomNick};
                {moderator, _, _} -> ok;
                _ -> {error, mongoose_xmpp_errors:not_allowed()}
            end
    end.


-spec get_field(binary(), [jlib:xmlcdata() | exml:element()]) -> any().
get_field(Var, [#xmlel{name = <<"field">>, attrs = Attrs} = Item|Items])
    when is_binary(Var) ->
    case xml:get_attr(<<"var">>, Attrs) of
    {value, Var} ->
        case xml:get_path_s(Item, [{elem, <<"value">>}, cdata]) of
        <<>> -> get_field(Var, Items);
        Value -> Value
        end;
    _ ->
        get_field(Var, Items)
    end;
get_field(_Var, []) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Invitation support

-spec check_invitation(jid:simple_jid() | jid:jid(),
        [jlib:xmlcdata() | exml:element()], ejabberd:lang(), state())
            -> {'error', _} | {'ok', [jid:jid()]}.
check_invitation(FromJID, Els, Lang, StateData) ->
    try
        unsafe_check_invitation(FromJID, Els, Lang, StateData)
    catch throw:{error, Reason} -> {error, Reason}
    end.


-spec unsafe_check_invitation(jid:jid(), [jlib:xmlcdata() | exml:element()],
                              ejabberd:lang(), state()) -> {ok, [jid:jid()]}.
unsafe_check_invitation(FromJID, Els, Lang,
                        StateData=#state{host=Host, server_host=ServerHost, jid=RoomJID}) ->
    FAffiliation = get_affiliation(FromJID, StateData),
    CanInvite = (StateData#state.config)#config.allow_user_invites
                orelse (FAffiliation == admin)
                orelse (FAffiliation == owner),
    case CanInvite of
        false ->
            throw({error, mongoose_xmpp_errors:forbidden()});
        true ->
            InviteEls = find_invite_elems(Els),
            %% Decode all JIDs first, so we fail early if any JID is invalid.
            JIDs = lists:map(fun decode_destination_jid/1, InviteEls),
            lists:foreach(
              fun(InviteEl) ->
                      {JID, Reason, Msg} = create_invite(FromJID, InviteEl, Lang, StateData),
                      ejabberd_hooks:run(invitation_sent, Host,
                                         [Host, ServerHost, RoomJID, FromJID, JID, Reason]),
                      ejabberd_router:route(StateData#state.jid, JID, Msg)
              end, InviteEls),
            {ok, JIDs}
    end.

-spec create_invite(FromJID ::jid:jid(), InviteEl :: exml:element(),
                    Lang :: ejabberd:lang(), StateData :: state()) ->
    {JID ::jid:jid(), Reason :: binary(), Msg :: exml:element()}.
create_invite(FromJID, InviteEl, Lang, StateData) ->
    JID = decode_destination_jid(InviteEl),
    %% Create an invitation message and send it to the user.
    Reason = decode_reason(InviteEl),
    ContinueEl =
    case xml:get_path_s(InviteEl, [{elem, <<"continue">>}]) of
        <<>> -> [];
        Continue1 -> [Continue1]
    end,
    ReasonEl = #xmlel{
                  name = <<"reason">>,
                  children = [#xmlcdata{content = Reason}]},
    OutInviteEl = #xmlel{
                     name = <<"invite">>,
                     attrs = [{<<"from">>, jid:to_binary(FromJID)}],
                     children = [ReasonEl] ++ ContinueEl},
    PasswdEl = create_password_elem(StateData),
    BodyEl = invite_body_elem(FromJID, Reason, Lang, StateData),
    Msg = create_invite_message_elem(
            OutInviteEl, BodyEl, PasswdEl, Reason),
    {JID, Reason, Msg}.

-spec decode_destination_jid(exml:element()) -> jid:jid().
decode_destination_jid(InviteEl) ->
    case jid:from_binary(xml:get_tag_attr_s(<<"to">>, InviteEl)) of
      error -> throw({error, mongoose_xmpp_errors:jid_malformed()});
      JID   -> JID
    end.


-spec find_invite_elems([jlib:xmlcdata() | exml:element()]) -> [exml:element()].
find_invite_elems(Els) ->
    case xml:remove_cdata(Els) of
    [#xmlel{name = <<"x">>, children = Els1} = XEl] ->
        case xml:get_tag_attr_s(<<"xmlns">>, XEl) of
        ?NS_MUC_USER ->
            ok;
        _ ->
            throw({error, mongoose_xmpp_errors:bad_request()})
        end,

        InviteEls =
            [InviteEl || #xmlel{name = <<"invite">>} = InviteEl <- Els1],
        case InviteEls of
            [_|_] ->
                InviteEls;
            _ ->
                throw({error, mongoose_xmpp_errors:bad_request()})
        end;
    _ ->
        throw({error, mongoose_xmpp_errors:bad_request()})
    end.


-spec create_password_elem(state()) -> [exml:element()].
create_password_elem(#state{config=#config{password_protected=IsProtected,
                                           password=Password}}) ->
    case IsProtected of
        true ->
        [#xmlel{
            name = <<"password">>,
            children = [#xmlcdata{content = Password}]}];
        _ ->
        []
    end.


-spec invite_body_elem(jid:jid(), binary(), ejabberd:lang(), state()
                      ) -> exml:element().
invite_body_elem(FromJID, Reason, Lang, StateData) ->
    Text = invite_body_text(FromJID, Reason, Lang, StateData),
    #xmlel{
        name = <<"body">>,
        children = [#xmlcdata{content = Text}]}.


-spec invite_body_text(jid:jid(), binary(), ejabberd:lang(), state()) -> binary().
invite_body_text(FromJID, Reason, Lang,
        #state{
            jid=RoomJID,
            config=#config{
                password_protected=IsProtected,
                password=Password}}) ->
    BFromJID = jid:to_binary(FromJID),
    BRoomJID = jid:to_binary(RoomJID),
    ITranslate = translate:translate(Lang, <<" invites you to the room ">>),
    IMessage = <<BFromJID/binary, ITranslate/binary, BRoomJID/binary>>,
    BPassword = case IsProtected of
        true ->
            PTranslate = translate:translate(Lang, <<"the password is">>),
            <<", ", PTranslate/binary, " '", Password/binary, "'">>;
        _ ->
            <<>>
        end,
    BReason = case Reason of
        <<>> -> <<>>;
        _    -> <<" (", Reason/binary, ") ">>
        end,
    <<IMessage/binary, BPassword/binary, BReason/binary>>.


-spec create_invite_message_elem(Inv :: exml:element(), Body :: exml:element(),
        Passwd :: [exml:element()], Reason :: binary()
        ) -> exml:element().
create_invite_message_elem(InviteEl, BodyEl, PasswdEl, Reason)
    when is_list(PasswdEl), is_binary(Reason) ->
    UserXEl = #xmlel{
        name = <<"x">>,
        attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
        children = [InviteEl|PasswdEl]},
    #xmlel{
        name = <<"message">>,
        attrs = [{<<"type">>, <<"normal">>}],
        children = [UserXEl, BodyEl]}.


%% @doc Handle a message sent to the room by a non-participant.
%% If it is a decline, send to the inviter.
%% Otherwise, an error message is sent to the sender.
-spec handle_roommessage_from_nonparticipant(exml:element(), ejabberd:lang(),
                    state(), jid:simple_jid() | jid:jid()) -> mongoose_acc:t().
handle_roommessage_from_nonparticipant(Packet, Lang, StateData, From) ->
    case catch check_decline_invitation(Packet) of
        {true, DeclineData} ->
            send_decline_invitation(DeclineData, StateData#state.jid, From);
        _ ->
            send_error_only_occupants(<<"messages">>, Packet, Lang, StateData#state.jid, From)
    end.


%% @doc Check in the packet is a decline. If so, also returns the splitted
%% packet. This function must be catched, because it crashes when the packet
%% is not a decline message.
-spec check_decline_invitation(exml:element()) ->
    {'true', {exml:element(), exml:element(), exml:element(), 'error' | jid:jid()}}.
check_decline_invitation(Packet) ->
    #xmlel{name = <<"message">>} = Packet,
    XEl = xml:get_subtag(Packet, <<"x">>),
    ?NS_MUC_USER = xml:get_tag_attr_s(<<"xmlns">>, XEl),
    DEl = xml:get_subtag(XEl, <<"decline">>),
    ToString = xml:get_tag_attr_s(<<"to">>, DEl),
    ToJID = jid:from_binary(ToString),
    {true, {Packet, XEl, DEl, ToJID}}.


%% @doc Send the decline to the inviter user.
%% The original stanza must be slightly modified.
-spec send_decline_invitation({exml:element(), exml:element(), exml:element(), jid:jid()},
        jid:jid(), jid:simple_jid() | jid:jid()) -> mongoose_acc:t().
send_decline_invitation({Packet, XEl, DEl, ToJID}, RoomJID, FromJID) ->
    FromString = jid:to_binary(FromJID),
    #xmlel{name = <<"decline">>, attrs = DAttrs, children = DEls} = DEl,
    DAttrs2 = lists:keydelete(<<"to">>, 1, DAttrs),
    DAttrs3 = [{<<"from">>, FromString} | DAttrs2],
    DEl2 = #xmlel{name = <<"decline">>, attrs = DAttrs3, children = DEls},
    XEl2 = replace_subelement(XEl, DEl2),
    Packet2 = replace_subelement(Packet, XEl2),
    ejabberd_router:route(RoomJID, ToJID, Packet2).

%% @doc Given an element and a new subelement,
%% replace the instance of the subelement in element with the new subelement.
-spec replace_subelement(exml:element(), exml:element()) -> exml:element().
replace_subelement(XE = #xmlel{children = SubEls}, NewSubEl) ->
    {_, NameNewSubEl, _, _} = NewSubEl,
    SubEls2 = lists:keyreplace(NameNewSubEl, 2, SubEls, NewSubEl),
    XE#xmlel{children = SubEls2}.

-spec send_error_only_occupants(binary(), exml:element(),
                                binary() | nonempty_string(),
                                jid:jid(), jid:jid()) -> mongoose_acc:t().
send_error_only_occupants(What, Packet, Lang, RoomJID, From)
  when is_binary(What) ->
    ErrText = <<"Only occupants are allowed to send ",
                What/bytes, " to the conference">>,
    Err = jlib:make_error_reply(Packet, mongoose_xmpp_errors:not_acceptable(Lang, ErrText)),
    ejabberd_router:route(RoomJID, From, Err).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logging

-spec add_to_log(atom(), any(), state()) -> 'ok'.
add_to_log(Type, Data, StateData)
  when Type == roomconfig_change_disabledlogging ->
    %% When logging is disabled, the config change message must be logged:
    mod_muc_log:add_to_log(
      StateData#state.server_host, roomconfig_change, Data,
      jid:to_binary(StateData#state.jid), make_opts(StateData));
add_to_log(Type, Data, StateData) ->
    case (StateData#state.config)#config.logging of
    true ->
        mod_muc_log:add_to_log(
          StateData#state.server_host, Type, Data,
          jid:to_binary(StateData#state.jid), make_opts(StateData));
    false ->
        ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Users number checking

-spec tab_add_online_user(jid:jid(), state()) -> any().
tab_add_online_user(JID, StateData) ->
    {LUser, LServer, _} = jid:to_lower(JID),
    US = {LUser, LServer},
    Room = StateData#state.room,
    Host = StateData#state.host,
    catch ets:insert(
        muc_online_users,
        #muc_online_users{us = US, room = Room, host = Host}).


-spec tab_remove_online_user(jid:simple_jid() | jid:jid(), state()) -> any().
tab_remove_online_user(JID, StateData) ->
    {LUser, LServer, _} = jid:to_lower(JID),
    US = {LUser, LServer},
    Room = StateData#state.room,
    Host = StateData#state.host,
    catch ets:delete_object(
        muc_online_users,
        #muc_online_users{us = US, room = Room, host = Host}).


-spec tab_count_user(jid:jid()) -> non_neg_integer().
tab_count_user(JID) ->
    {LUser, LServer, _} = jid:to_lower(JID),
    US = {LUser, LServer},
    case catch ets:select(
         muc_online_users,
         [{#muc_online_users{us = US, _ = '_'}, [], [[]]}]) of
    Res when is_list(Res) ->
        length(Res);
    _ ->
        0
    end.

element_size(El) ->
    exml:xml_size(El).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Routing functions

-spec route_message(routed_message(), state()) -> state().
route_message(#routed_message{allowed = true, type = <<"groupchat">>,
                              from = From, packet = Packet, lang = Lang}, StateData) ->
    Activity = get_user_activity(From, StateData),
    Now = os:system_time(microsecond),
    MinMessageInterval = trunc(gen_mod:get_module_opt(StateData#state.server_host,
                                                      mod_muc, min_message_interval, 0) * 1000000),
    Size = element_size(Packet),
    {MessageShaper, MessageShaperInterval} = shaper:update(Activity#activity.message_shaper, Size),
    case {Activity#activity.message /= undefined,
          Now >= Activity#activity.message_time + MinMessageInterval,
          MessageShaperInterval} of
        {true, _, _} ->
            ErrText = <<"Traffic rate limit is exceeded">>,
            Err = jlib:make_error_reply(Packet, mongoose_xmpp_errors:resource_constraint(Lang, ErrText)),
            ejabberd_router:route(StateData#state.jid, From, Err),
            StateData;
        {false, true, 0} ->
            {RoomShaper, RoomShaperInterval} = shaper:update(StateData#state.room_shaper, Size),
            RoomQueueEmpty = queue:is_empty(StateData#state.room_queue),
            case {RoomShaperInterval, RoomQueueEmpty} of
                {0, true} ->
                    NewActivity = Activity#activity{
                                    message_time = Now,
                                    message_shaper = MessageShaper},
                    StateData1 = store_user_activity(From, NewActivity, StateData),
                    StateData2 = StateData1#state{room_shaper = RoomShaper},
                    {next_state, normal_state, StateData3, _} =
                    process_groupchat_message(From, Packet, StateData2),
                    StateData3;
                _ ->
                    StateData1 = schedule_queue_processing_when_empty(
                                   RoomQueueEmpty, RoomShaper, RoomShaperInterval, StateData),
                    NewActivity = Activity#activity{
                                    message_time = Now,
                                    message_shaper = MessageShaper,
                                    message = Packet},
                    RoomQueue = queue:in({message, From}, StateData#state.room_queue),
                    StateData2 = store_user_activity(From, NewActivity, StateData1),
                    StateData2#state{room_queue = RoomQueue}
            end;
        _ ->
            MessageInterval = (Activity#activity.message_time + MinMessageInterval - Now) div 1000,
            Interval = lists:max([MessageInterval, MessageShaperInterval]),
            erlang:send_after(Interval, self(), {process_user_message, From}),
            NewActivity = Activity#activity{
                            message = Packet,
                            message_shaper = MessageShaper},
            store_user_activity(From, NewActivity, StateData)
    end;
route_message(#routed_message{allowed = true, type = <<"error">>, from = From,
    packet = Packet, lang = Lang}, StateData) ->
    case is_user_online(From, StateData) of
        true ->
            ErrorText
            = <<"This participant is kicked from the room because he sent an error message">>,
            expulse_participant(Packet, From, StateData, translate:translate(Lang, ErrorText));
        _ ->
            StateData
    end;
route_message(#routed_message{allowed = true, type = <<"chat">>, from = From, packet = Packet,
    lang = Lang}, StateData) ->
    ErrText = <<"It is not allowed to send private messages to the conference">>,
    Err = jlib:make_error_reply(
        Packet, mongoose_xmpp_errors:not_acceptable(Lang, ErrText)),
    ejabberd_router:route(
        StateData#state.jid,
        From, Err),
    StateData;
route_message(#routed_message{allowed = true, type = Type, from = From,
                              packet = #xmlel{name = <<"message">>,
                                              children = Els} = Packet, lang = Lang},
              StateData) when (Type == <<>> orelse Type == <<"normal">>) ->

    Invite = xml:get_path_s(Packet, [{elem, <<"x">>}, {elem, <<"invite">>}]),
    case Invite of
        <<>> ->
            AppType = check_voice_approval(From, Els, Lang, StateData),
            route_voice_approval(AppType, From, Packet, Lang, StateData);
        _ ->
            InType = check_invitation(From, Els, Lang, StateData),
            route_invitation(InType, From, Packet, Lang, StateData)
    end;
route_message(#routed_message{allowed = true, from = From, packet = Packet,
                              lang = Lang}, StateData) ->
    ErrText = <<"Improper message type">>,
    Err = jlib:make_error_reply(Packet, mongoose_xmpp_errors:not_acceptable(Lang, ErrText)),
    ejabberd_router:route(StateData#state.jid,
                          From, Err),
    StateData;
route_message(#routed_message{type = <<"error">>}, StateData) ->
    StateData;
route_message(#routed_message{from = From, packet = Packet, lang = Lang},
              StateData) ->
    handle_roommessage_from_nonparticipant(Packet, Lang, StateData, From),
    StateData.

-spec schedule_queue_processing_when_empty(RoomQueueEmpty :: boolean(),
                                           RoomShaper :: shaper:shaper(),
                                           RoomShaperInterval :: non_neg_integer(),
                                           StateData :: state()) -> state().
schedule_queue_processing_when_empty(true, RoomShaper, RoomShaperInterval, StateData) ->
    erlang:send_after(RoomShaperInterval, self(), process_room_queue),
    StateData#state{room_shaper = RoomShaper};
schedule_queue_processing_when_empty(_RoomQueueEmpty, _RoomShaper,
                                     _RoomShaperInterval, StateData) ->
    StateData.

-spec route_error(mod_muc:nick(), jid:jid(), exml:element(), state()) -> state().
route_error(Nick, From, Error, StateData) ->
    %% TODO: s/Nick/<<>>/
    ejabberd_router:route(jid:replace_resource(StateData#state.jid, Nick),
                          From, Error),
    StateData.


-spec route_voice_approval('ok' | {'error', exml:element()} | {'form', binary()}
        | {'role', binary(), binary()}, jid:jid(), exml:element(),
        ejabberd:lang(), state()) -> state().
route_voice_approval({error, ErrType}, From, Packet, _Lang, StateData) ->
    ejabberd_router:route(StateData#state.jid, From,
                          jlib:make_error_reply(Packet, ErrType)),
    StateData;
route_voice_approval({form, RoleName}, From, _Packet, _Lang, StateData) ->
    {Nick, _} = get_participant_data(From, StateData),
    ApprovalForm = jlib:make_voice_approval_form(From, Nick, RoleName),
    F = fun({_, Info}) ->
                ejabberd_router:route(StateData#state.jid, Info#user.jid,
                                      ApprovalForm)
        end,
    lists:foreach(F, search_role(moderator, StateData)),
    StateData;
route_voice_approval({role, BRole, Nick}, From, Packet, Lang, StateData) ->
    Items = [#xmlel{name = <<"item">>,
                    attrs = [{<<"role">>, BRole},
                             {<<"nick">>, Nick}]}],
    case process_admin_items_set(From, Items, Lang, StateData) of
        {result, _Res, SD1} -> SD1;
        {error, Error} ->
            ejabberd_router:route(StateData#state.jid, From,
                                  jlib:make_error_reply(Packet, Error)),
            StateData
    end;
route_voice_approval(_Type, From, Packet, _Lang, StateData) ->
    ejabberd_router:route(StateData#state.jid, From,
                          jlib:make_error_reply(Packet, mongoose_xmpp_errors:bad_request())),
    StateData.


-spec route_invitation(InvitationsOrError,
                       From, Packet, Lang, state()) -> state() when
      InvitationsOrError :: {'error', jlib:xmlcdata() | exml:element()}
                          | {'ok', [jid:jid()]},
      From :: jid:simple_jid() | jid:jid(),
      Packet :: exml:element(),
      Lang :: ejabberd:lang().
route_invitation({error, Error}, From, Packet, _Lang, StateData) ->
    Err = jlib:make_error_reply(Packet, Error),
    ejabberd_router:route(StateData#state.jid, From, Err),
    StateData;
route_invitation({ok, IJIDs}, _From, _Packet, _Lang,
                 #state{ config = #config{ members_only = true } } = StateData0) ->
    lists:foldl(
      fun(IJID, StateData) ->
              case get_affiliation(IJID, StateData) of
                  none ->
                      NSD = set_affiliation(IJID, member, StateData),
                      store_room_if_persistent(NSD),
                      NSD;
                  _ ->
                      StateData
              end
      end, StateData0, IJIDs);
route_invitation({ok, _IJIDs}, _From, _Packet, _Lang, StateData0) ->
    StateData0.

-spec store_room_if_persistent(state()) -> any().
store_room_if_persistent(#state{ host = Host, room = Room, server_host = ServerHost,
                                 config = #config{ persistent = true } } = StateData) ->
    mod_muc:store_room(ServerHost, Host, Room, make_opts(StateData));
store_room_if_persistent(_SD) ->
    ok.

-spec route_iq(mongoose_acc:t(), routed_iq(), state()) -> {ok | stop, state()}.
route_iq(_Acc, #routed_iq{iq = #iq{type = Type}}, StateData)
  when Type == error; Type == result ->
    {ok, StateData};
route_iq(Acc, #routed_iq{iq = #iq{type = Type, xmlns = ?NS_MUC_ADMIN, lang = Lang,
    sub_el = SubEl}, from = From} = Routed, StateData) ->
    Res = process_iq_admin(From, Type, Lang, SubEl, StateData),
    do_route_iq(Acc, Res, Routed, StateData);
route_iq(Acc, #routed_iq{iq = #iq{type = Type, xmlns = ?NS_MUC_OWNER, lang = Lang,
    sub_el = SubEl}, from = From} = Routed, StateData) ->
    Res = process_iq_owner(From, Type, Lang, SubEl, StateData, normal_state),
    do_route_iq(Acc, Res, Routed, StateData);
route_iq(Acc, #routed_iq{iq = #iq{type = Type, xmlns = ?NS_DISCO_INFO, lang = Lang},
    from = From} = Routed, StateData) ->
    Res = process_iq_disco_info(From, Type, Lang, StateData),
    do_route_iq(Acc, Res, Routed, StateData);
route_iq(Acc, #routed_iq{iq = #iq{type = Type, xmlns = ?NS_DISCO_ITEMS, lang = Lang},
    from = From} = Routed, StateData) ->
    Res = process_iq_disco_items(From, Type, Lang, StateData),
    do_route_iq(Acc, Res, Routed, StateData);
route_iq(Acc, #routed_iq{iq = IQ = #iq{}, packet = Packet, from = From},
         #state{host = Host, jid = RoomJID} = StateData) ->
    %% Custom IQ, addressed to this room's JID.
    case mod_muc_iq:process_iq(Host, From, RoomJID, Acc, IQ) of
        {Acc1, error} ->
            {Acc2, Err} = jlib:make_error_reply(Acc1, Packet, mongoose_xmpp_errors:feature_not_implemented()),
            ejabberd_router:route(RoomJID, From, Acc2, Err);
        _ -> ok
    end,
    {ok, StateData};
route_iq(Acc, #routed_iq{packet = Packet, from = From}, StateData) ->
    {Acc1, Err} = jlib:make_error_reply(
        Acc, Packet, mongoose_xmpp_errors:feature_not_implemented()),
    ejabberd_router:route(StateData#state.jid, From, Acc1, Err),
    {ok, StateData}.


-spec do_route_iq(mongoose_acc:t(), {result, [exml:element()], state()} | {error, exml:element()},
                  routed_iq(), state()) -> {ok | stop, state()}.
do_route_iq(Acc, Res1, #routed_iq{iq = #iq{xmlns = XMLNS, sub_el = SubEl} = IQ,
    from = From}, StateData) ->
    {IQRes, RoutingResult} = case Res1 of
        {result, Res, SD} ->
            {
             IQ#iq{type = result,
                sub_el = [#xmlel{name = <<"query">>,
                                 attrs = [{<<"xmlns">>, XMLNS}],
                                 children = Res}]},
             case SD of
                 stop -> {stop, StateData};
                 _ -> {ok, SD}
             end
            };
        {error, Error} ->
            {
             IQ#iq{type = error, sub_el = [SubEl, Error]},
             {ok, StateData}
            }
    end,
    ejabberd_router:route(StateData#state.jid, From, Acc,
        jlib:iq_to_xml(IQRes)),
    RoutingResult.


-spec route_nick_message(routed_nick_message(), state()) -> state().
route_nick_message(#routed_nick_message{decide = {expulse_sender, Reason},
    packet = Packet, lang = Lang, from = From}, StateData) ->
    ?DEBUG(Reason, []),
    ErrorText = <<"This participant is kicked from the room because he",
                  "sent an error message to another participant">>,
    expulse_participant(Packet, From, StateData, translate:translate(Lang, ErrorText));
route_nick_message(#routed_nick_message{decide = forget_message}, StateData) ->
    StateData;
route_nick_message(#routed_nick_message{decide = continue_delivery, allow_pm = true,
    online = true, packet = Packet, from = From, type = <<"groupchat">>,
    lang = Lang, nick = ToNick}, StateData) ->
    ErrText = <<"It is not allowed to send private messages of type groupchat">>,
    Err = jlib:make_error_reply(
        Packet, mongoose_xmpp_errors:bad_request(Lang, ErrText)),
    ejabberd_router:route(
        jid:replace_resource(
       StateData#state.jid,
       ToNick),
        From, Err),
    StateData;
route_nick_message(#routed_nick_message{decide = continue_delivery, allow_pm = true,
    online = true, packet = Packet, from = From,
    lang = Lang, nick = ToNick, jid = false}, StateData) ->
    ErrText = <<"Recipient is not in the conference room">>,
    Err = jlib:make_error_reply(
        Packet, mongoose_xmpp_errors:item_not_found(Lang, ErrText)),
    ejabberd_router:route(
        jid:replace_resource(
       StateData#state.jid,
       ToNick),
        From, Err),
    StateData;
route_nick_message(#routed_nick_message{decide = continue_delivery, allow_pm = true,
    online = true, packet = Packet, from = From, jid = ToJID}, StateData) ->
    {ok, #user{nick = FromNick}} = maps:find(jid:to_lower(From),
        StateData#state.users),
    ejabberd_router:route(
        jid:replace_resource(StateData#state.jid, FromNick), ToJID, Packet),
    StateData;
route_nick_message(#routed_nick_message{decide = continue_delivery,
                                        allow_pm = true,
                                        online = false} = Routed, StateData) ->
    #routed_nick_message{packet = Packet, from = From,
                         lang = Lang, nick = ToNick} = Routed,
    RoomJID = jid:replace_resource(StateData#state.jid, ToNick),
    send_error_only_occupants(<<"messages">>, Packet, Lang, RoomJID, From),
    StateData;
route_nick_message(#routed_nick_message{decide = continue_delivery, allow_pm = false,
    packet = Packet, from = From,
    lang = Lang, nick = ToNick}, StateData) ->
    ErrText = <<"It is not allowed to send private messages">>,
    Err = jlib:make_error_reply(
        Packet, mongoose_xmpp_errors:forbidden(Lang, ErrText)),
    ejabberd_router:route(
        jid:replace_resource(StateData#state.jid, ToNick), From, Err),
    StateData.


-spec route_nick_iq(routed_nick_iq(), state()) -> 'ok'.
route_nick_iq(#routed_nick_iq{allow_query = true, online = {true, _, _}, jid = false,
    iq = reply}, _StateData) ->
    ok;
route_nick_iq(#routed_nick_iq{allow_query = true, online = {true, _, _}, jid = false,
    packet = Packet, lang = Lang, from = From, nick = ToNick}, StateData) ->
    ErrText = <<"Recipient is not in the conference room">>,
    Err = jlib:make_error_reply(
        Packet, mongoose_xmpp_errors:item_not_found(Lang, ErrText)),
    ejabberd_router:route(
        jid:replace_resource(
       StateData#state.jid, ToNick),
        From, Err);
route_nick_iq(#routed_nick_iq{allow_query = true, online = {true, NewId, FromFull},
    jid = ToJID, packet = Packet, stanza = StanzaId}, StateData) ->
    {ok, #user{nick = FromNick}} = maps:find(jid:to_lower(FromFull),
        StateData#state.users),
    {ToJID2, Packet2} = handle_iq_vcard(FromFull, ToJID, StanzaId, NewId, Packet),
    ejabberd_router:route(
        jid:replace_resource(StateData#state.jid, FromNick),
        ToJID2, Packet2);
route_nick_iq(#routed_nick_iq{online = {false, _, _}, iq = reply}, _StateData) ->
    ok;
route_nick_iq(#routed_nick_iq{online = {false, _, _}, from = From, nick = ToNick,
                              packet = Packet, lang = Lang}, StateData) ->
    RoomJID = jid:replace_resource(StateData#state.jid, ToNick),
    send_error_only_occupants(<<"queries">>, Packet, Lang, RoomJID, From);
route_nick_iq(#routed_nick_iq{iq = reply}, _StateData) ->
    ok;
route_nick_iq(#routed_nick_iq{packet = Packet, lang = Lang, nick = ToNick,
                              from = From}, StateData) ->
    ErrText = <<"Queries to the conference members are "
                "not allowed in this room">>,
    Err = jlib:make_error_reply(Packet, mongoose_xmpp_errors:not_allowed(Lang, ErrText)),
    RouteFrom = jid:replace_resource(StateData#state.jid, ToNick),
    ejabberd_router:route(RouteFrom, From, Err).


-spec decode_reason(exml:element()) -> binary().
decode_reason(Elem) ->
    xml:get_path_s(Elem, [{elem, <<"reason">>}, cdata]).


-spec xfield(binary(), any(), binary(), binary(), ejabberd:lang()) -> exml:element().
xfield(Type, Label, Var, Val, Lang) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"type">>, Type},
                         {<<"label">>, translate:translate(Lang, Label)},
                         {<<"var">>, Var}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = Val}]}]}.


-spec boolxfield(any(), binary(), any(), ejabberd:lang()) -> exml:element().
boolxfield(Label, Var, Val, Lang) ->
    xfield(<<"boolean">>, Label, Var,
        case Val of
            true -> <<"1">>;
            _ -> <<"0">>
        end, Lang).

stringxfield(Label, Var, Val, Lang) ->
    xfield(<<"text-single">>, Label, Var, Val, Lang).

privatexfield(Label, Var, Val, Lang) ->
    xfield(<<"text-private">>, Label, Var, Val, Lang).

notify_users_modified(#state{server_host = Host, jid = JID, users = Users} = State) ->
    mod_muc_log:set_room_occupants(Host, self(), JID, maps:values(Users)),
    State.

