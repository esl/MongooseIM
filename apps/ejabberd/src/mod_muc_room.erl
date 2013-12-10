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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_muc_room).
-author('alexey@process-one.net').

-behaviour(gen_fsm).


%% External exports
-export([start_link/9,
         start_link/7,
         start/9,
         start/7,
         route/4]).

%% API exports
-export([get_room_users/1,
         is_room_owner/2]).

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

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_muc_room.hrl").

-record(routed_message, {allowed, type, from, packet, lang}).
-record(routed_nick_message, {allow_pm, online, type, from, nick, lang, packet, decide, jid}).
-record(routed_iq, {iq, from, packet}).
-record(routed_nick_iq, {allow_query, online, iq, packet, lang, nick, jid, from, stanza}).

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
        gen_fsm:start(?MODULE,
                      [Host, ServerHost, Access, Room, HistorySize,
                       RoomShaper, Creator, Nick, DefRoomOpts],
                      ?FSMOPTS)).
-else.
-define(SUPERVISOR_START,
        Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_muc_sup),
        supervisor:start_child(Supervisor,
                               [Host, ServerHost, Access, Room, HistorySize,
                                RoomShaper, Creator, Nick, DefRoomOpts])).
-endif.

-define(XFIELD(Type, Label, Var, Val),
        #xmlel{name = <<"field">>,
               attrs = [{<<"type">>, Type},
                             {<<"label">>, translate:translate(Lang, Label)},
                             {<<"var">>, Var}],
               children = [#xmlel{name = <<"value">>,
                                  children = [#xmlcdata{content = Val}]}]}).

-define(BOOLXFIELD(Label, Var, Val),
        ?XFIELD(<<"boolean">>, Label, Var,
                case Val of
                    true -> <<"1">>;
                    _ -> <<"0">>
                end)).

-define(STRINGXFIELD(Label, Var, Val),
        ?XFIELD(<<"text-single">>, Label, Var, Val)).

-define(PRIVATEXFIELD(Label, Var, Val),
        ?XFIELD(<<"text-private">>, Label, Var, Val)).

-define(JIDXFIELD(Label, Var, Val),
        ?XFIELD(<<"jid-single">>, Label, Var, Val)).

-define(JIDMULTIXFIELD(Label, Var, JIDList),
        #xmlel{name = <<"field">>,
               attrs = [{<<"type">>, <<"jid-multi">>},
                        {<<"label">>, translate:translate(Lang, Label)},
                        {<<"var">>, Var}],
               children = [#xmlel{name = <<"value">>,
                                  children = [#xmlcdata{content = jlib:jid_to_binary(JID)}]}
                           || JID <- JIDList]}).

%%%----------------------------------------------------------------------
%%% Types
%%%----------------------------------------------------------------------
-type affiliation() :: admin | owner | member | outcast | none.
-type role() :: moderator | participant | visitor | none.
-type state_data() :: #state{}.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host, ServerHost, Access, Room, HistorySize, RoomShaper,
      Creator, Nick, DefRoomOpts) ->
    ?SUPERVISOR_START.

start(Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts) ->
    Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_muc_sup),
    supervisor:start_child(Supervisor, [Host, ServerHost, Access, Room,
                                        HistorySize, RoomShaper, Opts]).

start_link(Host, ServerHost, Access, Room, HistorySize, RoomShaper,
       Creator, Nick, DefRoomOpts) ->
    gen_fsm:start_link(?MODULE,
                       [Host, ServerHost, Access, Room, HistorySize,
                        RoomShaper, Creator, Nick, DefRoomOpts],
                       ?FSMOPTS).

start_link(Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts) ->
    gen_fsm:start_link(?MODULE,
                       [Host, ServerHost, Access, Room, HistorySize,
                        RoomShaper, Opts],
                       ?FSMOPTS).

-spec get_room_users(RoomJID :: jid()) -> {ok, [#user{}]} | {error, not_found}.
get_room_users(RoomJID) ->
    case mod_muc:room_jid_to_pid(RoomJID) of
        {ok, Pid} ->
            gen_fsm:sync_send_all_state_event(Pid, get_room_users);
        {error, Reason} ->
            {error, Reason}
    end.

-spec is_room_owner(RoomJID :: jid(), UserJID :: jid()) ->
    {ok, boolean()} | {error, not_found}.
is_room_owner(RoomJID, UserJID) ->
    case mod_muc:room_jid_to_pid(RoomJID) of
        {ok, Pid} ->
            gen_fsm:sync_send_all_state_event(Pid, {is_room_owner, UserJID});
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

%% A room is created. Depending on request type (MUC/groupchat 1.0) the next
%% state is determined accordingly (a locked room for MUC or an instant
%% one for groupchat).
init([Host, ServerHost, Access, Room, HistorySize, RoomShaper, Creator, _Nick,
      DefRoomOpts]) ->
    process_flag(trap_exit, true),
    Shaper = shaper:new(RoomShaper),
    State = set_affiliation(Creator, owner,
                            #state{host = Host,
                                   server_host = ServerHost,
                                   access = Access,
                                   room = Room,
                                   history = lqueue_new(HistorySize),
                                   jid = jlib:make_jid(Room, Host, <<>>),
                                   just_created = true,
                                   room_shaper = Shaper}),
    State1 = set_opts(DefRoomOpts, State),
    ?INFO_MSG("Created MUC room ~s@~s by ~s",
              [Room, Host, jlib:jid_to_binary(Creator)]),
    add_to_log(room_existence, created, State1),
    NextState = case proplists:get_value(instant, DefRoomOpts, false) of
                    true ->
                        %% Instant room -- groupchat 1.0 request
                        add_to_log(room_existence, started, State1),
                        normal_state;
                    false ->
                        %% Locked room waiting for configuration -- MUC request
                        initial_state
                end,
    {ok, NextState, State1};

%% A room is restored
init([Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts]) ->
    process_flag(trap_exit, true),
    Shaper = shaper:new(RoomShaper),
    State = set_opts(Opts, #state{host = Host,
                                  server_host = ServerHost,
                                  access = Access,
                                  room = Room,
                                  history = lqueue_new(HistorySize),
                                  jid = jlib:make_jid(Room, Host, <<>>),
                                  room_shaper = Shaper}),
    add_to_log(room_existence, started, State),
    {ok, normal_state, State}.


%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------

%% In the locked state StateData contains the same settings it previously held
%% for the normal_state. The fsm awaits either a confirmation
%% or a configuration form from the creator.
%% Responds with error to the any other queries.
locked_error({route, From, ToNick, #xmlel{attrs = Attrs} = Packet},
             NextState, StateData) ->
    ?INFO_MSG("Wrong stanza: ~p", [Packet]),
    ErrText = <<"This room is locked">>,
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    Err = jlib:make_error_reply(Packet, ?ERRT_ITEM_NOT_FOUND(Lang, ErrText)),
    ejabberd_router:route(jlib:jid_replace_resource(StateData#state.jid,
                                                    ToNick),
                          From, Err),
    {next_state, NextState, StateData}.

%% Receive the room-creating Stanza.
%% Will crash if any other stanza is received in this state.
initial_state({route, From, ToNick,
              #xmlel{name = <<"presence">>,
                     attrs = Attrs} = Presence}, StateData) ->
    %% this should never happen so crash if it does
    <<>> = xml:get_attr_s(<<"type">>, Attrs),
    case  xml:get_path_s(Presence,[{elem, <<"x">>}, {attr, <<"xmlns">>}]) of
        ?NS_MUC ->
            %% FIXME
            add_to_log(room_existence, started, StateData),
            process_presence(From, ToNick, Presence, StateData, locked_state);
            %% The fragment of normal_state with Activity that used to do this - how does that work?
            %% Seems to work without it
        <<>> ->
            %% groupchat 1.0 user, straight to normal_state
            process_presence(From, ToNick, Presence, StateData)
        end.

is_query_allowed(Query) ->
    X = xml:get_subtag(Query, <<"x">>),
    xml:get_subtag(Query, <<"destroy">>) =/= false orelse
        (X =/= false andalso xml:get_tag_attr_s(<<"xmlns">>, X)== ?NS_XDATA andalso
        (xml:get_tag_attr_s(<<"type">>, X) == <<"submit">> orelse
        xml:get_tag_attr_s(<<"type">>, X)== <<"cancel">>)).

locked_state_process_owner_iq(From, Query, Lang, set, StateData) ->
    Result = case is_query_allowed(Query) of
                 true ->
                     process_iq_owner(From, set, Lang, Query, StateData);
                 false ->
                     {error, ?ERRT_ITEM_NOT_FOUND(Lang, <<"Query not allowed">>)}
             end,
    {Result, normal_state};

locked_state_process_owner_iq(From, Query, Lang, get, StateData) ->
    {process_iq_owner(From, get, Lang, Query, StateData), locked_state};

locked_state_process_owner_iq(_From, _Query, Lang, _Type, _StateData) ->
    {{error, ?ERRT_ITEM_NOT_FOUND(Lang, <<"Wrong type">>)}, locked_state}.

%% Destroy room / confirm instant room / configure room
locked_state({route, From, _ToNick,
              #xmlel{name = <<"iq">>} = Packet}, StateData) ->
    #iq{lang = Lang, sub_el = Query} = IQ = jlib:iq_query_info(Packet),
    {Result, NextState} =
        case IQ#iq.xmlns == ?NS_MUC_OWNER andalso
            get_affiliation(From, StateData)  =:= owner
        of
            true ->
                locked_state_process_owner_iq(From, Query, Lang,
                                              IQ#iq.type, StateData);
            false ->
                ErrText = <<"This room is locked">>,
                {{error, ?ERRT_ITEM_NOT_FOUND(Lang, ErrText)}, locked_state}
        end,
    MkQueryResult = fun(Res) ->
                        IQ#iq{type = result,
                            sub_el = [#xmlel{name = <<"query">>,
                                             attrs = [{<<"xmlns">>, ?NS_MUC_OWNER}],
                                             children = Res}]}
                    end,
    {IQRes, StateData3, NextState1} =
        case Result of
            {result, Res, stop} ->
                {MkQueryResult(Res), StateData, stop};
            {result, Res, StateData2} ->
                {MkQueryResult(Res), StateData2, NextState};
            {error, Error} ->
                {IQ#iq{type = error, sub_el = [Query, Error]},
                 StateData, NextState}
        end,
    ejabberd_router:route(StateData3#state.jid, From, jlib:iq_to_xml(IQRes)),
    case NextState1 of
        stop->
            {stop, normal, StateData3};
        locked_state ->
            {next_state, NextState1, StateData3};
        normal_state ->
            {next_state, NextState1, StateData3#state{just_created = false}}
    end;

%% Let owner leave. Destroy the room.
locked_state({route, From, ToNick,
              #xmlel{name = <<"presence">>, attrs = Attrs} = Presence} = Call,
             StateData) ->
    case xml:get_attr_s(<<"type">>, Attrs) =:= <<"unavailable">>
        andalso get_affiliation(From, StateData)  =:= owner of
        true ->
            %% Will let the owner leave and destroy the room if it's not persistant
            %% The rooms are not persistent by default, but just to be safe...
            StateData1 = StateData#state{config = (StateData#state.config)#config{persistent = false}},
            process_presence(From, ToNick, Presence, StateData1, locked_state);
        _ ->
            locked_error(Call, locked_state, StateData)
    end;

locked_state(Call, StateData) ->
    locked_error(Call, locked_state, StateData).

normal_state({route, From, <<>>,
              #xmlel{name = <<"message">>, attrs = Attrs} = Packet},
             StateData) ->
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    Type = xml:get_attr_s(<<"type">>, Attrs),

    NewStateData = route_message(#routed_message{
        allowed = is_user_online(From, StateData) orelse
            is_user_allowed_message_nonparticipant(From, StateData),
        type = Type,
        from = From,
        packet = Packet,
        lang = Lang}, StateData),
    {next_state, normal_state, NewStateData};

normal_state({route, From, <<>>,
          #xmlel{name = <<"iq">>} = Packet},
         StateData) ->
    NewStateData = route_iq(#routed_iq{
        iq = jlib:iq_query_info(Packet),
        from = From,
        packet = Packet}, StateData),
    case NewStateData of
        stop ->
            {stop, normal, StateData};
        _ ->
            {next_state, normal_state, NewStateData}
    end;

normal_state({route, From, Nick,
              #xmlel{name = <<"presence">>} = Packet},
             StateData) ->
    Activity = get_user_activity(From, StateData),
    Now = now_to_usec(now()),
    MinPresenceInterval =
        trunc(gen_mod:get_module_opt(StateData#state.server_host,
                                     mod_muc, min_presence_interval, 0)
              * 1000000),
    if
    (Now >= Activity#activity.presence_time + MinPresenceInterval) and
    (Activity#activity.presence == undefined) ->
        NewActivity = Activity#activity{presence_time = Now},
        StateData1 = store_user_activity(From, NewActivity, StateData),
        process_presence(From, Nick, Packet, StateData1);
    true ->
        if
        Activity#activity.presence == undefined ->
            Interval = (Activity#activity.presence_time +
                MinPresenceInterval - Now) div 1000,
            erlang:send_after(
              Interval, self(), {process_user_presence, From});
        true ->
            ok
        end,
        NewActivity = Activity#activity{presence = {Nick, Packet}},
        StateData1 = store_user_activity(From, NewActivity, StateData),
        {next_state, normal_state, StateData1}
    end;

normal_state({route, From, ToNick,
              #xmlel{name = <<"message">>, attrs = Attrs} = Packet},
             StateData) ->
    Type = xml:get_attr_s(<<"type">>, Attrs),
    NewStateData = route_nick_message(#routed_nick_message{
        allow_pm = (StateData#state.config)#config.allow_private_messages,
        online = is_user_online(From, StateData),
        type = Type, 
        from = From,
        nick = ToNick,
        lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
        decide = decide_fate_message(Type, Packet, From, StateData),
        packet = Packet,
        jid = find_jid_by_nick(ToNick, StateData)}, StateData),
    {next_state, normal_state, NewStateData};

normal_state({route, From, ToNick,
          #xmlel{name = <<"iq">>, attrs = Attrs} = Packet},
         StateData) ->
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    StanzaId = xml:get_attr_s(<<"id">>, Attrs),
    route_nick_iq(#routed_nick_iq{
        allow_query = (StateData#state.config)#config.allow_query_users,
        online = is_user_online_iq(StanzaId, From, StateData),
        jid = find_jid_by_nick(ToNick, StateData),
        iq = jlib:iq_query_info(Packet),
        packet = Packet,
        lang = Lang,
        from = From,
        stanza = StanzaId,
        nick = ToNick}, StateData),
    {next_state, normal_state, StateData};

normal_state(_Event, StateData) ->
    {next_state, normal_state, StateData}.



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
    lists:foreach(
      fun({_LJID, Info}) ->
          ejabberd_router:route(
        StateData#state.jid,
        Info#user.jid,
        MessagePkt)
      end,
      ?DICT:to_list(StateData#state.users)),
    NSD = add_message_to_history(<<>>,
                 StateData#state.jid,
                 MessagePkt,
                 StateData),
    {next_state, normal_state, NSD};

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
          [jlib:jid_to_binary(StateData#state.jid), Reason]),
    add_to_log(room_existence, destroyed, StateData),
    {stop, shutdown, StateData};
handle_event(destroy, StateName, StateData) ->
    ?INFO_MSG("Destroyed MUC room ~s", 
          [jlib:jid_to_binary(StateData#state.jid)]),
    handle_event({destroy, none}, StateName, StateData);

handle_event({set_affiliations, Affiliations}, StateName, StateData) ->
    {next_state, StateName, StateData#state{affiliations = Affiliations}};

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

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
    {reply, Reply, StateName, StateData};
handle_sync_event(get_config, _From, StateName, StateData) ->
    {reply, {ok, StateData#state.config}, StateName, StateData};
handle_sync_event(get_state, _From, StateName, StateData) ->
    {reply, {ok, StateData}, StateName, StateData};
handle_sync_event(get_room_users, _From, StateName, StateData) ->
    {reply, {ok, dict_to_values(StateData#state.users)}, StateName, StateData};
handle_sync_event({is_room_owner, UserJID}, _From, StateName, StateData) ->
    {reply, {ok, get_affiliation(UserJID, StateData) =:= owner}, StateName, StateData};
handle_sync_event({change_config, Config}, _From, StateName, StateData) ->
    {result, [], NSD} = change_config(Config, StateData),
    {reply, {ok, NSD#state.config}, StateName, NSD};
handle_sync_event({change_state, NewStateData}, _From, StateName, _StateData) ->
    {reply, {ok, NewStateData}, StateName, NewStateData};
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info({process_user_presence, From}, normal_state = _StateName, StateData) ->
    RoomQueueEmpty = queue:is_empty(StateData#state.room_queue),
    RoomQueue = queue:in({presence, From}, StateData#state.room_queue),
    StateData1 = StateData#state{room_queue = RoomQueue},
    if
    RoomQueueEmpty ->
        StateData2 = prepare_room_queue(StateData1),
        {next_state, normal_state, StateData2};
    true ->
        {next_state, normal_state, StateData1}
    end;
handle_info({process_user_message, From}, normal_state = _StateName, StateData) ->
    RoomQueueEmpty = queue:is_empty(StateData#state.room_queue),
    RoomQueue = queue:in({message, From}, StateData#state.room_queue),
    StateData1 = StateData#state{room_queue = RoomQueue},
    if
    RoomQueueEmpty ->
        StateData2 = prepare_room_queue(StateData1),
        {next_state, normal_state, StateData2};
    true ->
        {next_state, normal_state, StateData1}
    end;
handle_info(process_room_queue, normal_state = StateName, StateData) ->
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
        {next_state, StateName, StateData}
    end;
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, _StateName, StateData) ->
    ?INFO_MSG("Stopping MUC room ~s@~s",
          [StateData#state.room, StateData#state.host]),
    ReasonT = case Reason of
          shutdown -> <<"You are being removed from the room because of a system shutdown">>;
          _ -> <<"Room terminates">>
          end,
    ItemAttrs = [{<<"affiliation">>, <<"none">>}, {<<"role">>, <<"none">>}],
    ReasonEl = #xmlel{name = <<"reason">>,
                      children = [#xmlcdata{content = ReasonT}]},
    Packet = #xmlel{name = <<"presence">>,
                    attrs = [{<<"type">>, <<"unavailable">>}],
                    children = [#xmlel{name = <<"x">>,
                                       attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
                                       children = [#xmlel{name = <<"item">>,
                                                          attrs = ItemAttrs,
                                                          children = [ReasonEl]},
                                                   #xmlel{name = <<"status">>,
                                                          attrs = [{<<"code">>, <<"332">>}]}
                                                  ]}]},
    ?DICT:fold(
       fun(LJID, Info, _) ->
           Nick = Info#user.nick,
           case Reason of
           shutdown ->
               ejabberd_router:route(
             jlib:jid_replace_resource(StateData#state.jid, Nick),
             Info#user.jid,
             Packet);
           _ -> ok
           end,
           tab_remove_online_user(LJID, StateData)
       end, [], StateData#state.users),
    add_to_log(room_existence, stopped, StateData),
    mod_muc:room_destroyed(StateData#state.host, StateData#state.room, self(),
               StateData#state.server_host),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

occupant_jid(#user{nick=Nick}, RoomJID) ->
    jlib:jid_replace_resource(RoomJID, Nick).

route(Pid, From, ToNick, Packet) ->
    gen_fsm:send_event(Pid, {route, From, ToNick, Packet}).

process_groupchat_message(From, #xmlel{name = <<"message">>,
                                       attrs = Attrs} = Packet,
              StateData) ->
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    case is_user_online(From, StateData) orelse
    is_user_allowed_message_nonparticipant(From, StateData) of
    true ->
        {FromNick, Role} = get_participant_data(From, StateData),
        if
        (Role == moderator) or (Role == participant) 
        or ((StateData#state.config)#config.moderated == false) ->
            {NewStateData1, IsAllowed} =
            case check_subject(Packet) of
                false ->
                {StateData, true};
                Subject ->
                case can_change_subject(Role, StateData) of
                    true ->
                    NSD =
                        StateData#state{
                          subject = Subject,
                          subject_author = FromNick},
                    case (NSD#state.config)#config.persistent of
                        true ->
                        mod_muc:store_room(
                          NSD#state.host,
                          NSD#state.room,
                          make_opts(NSD));
                        _ ->
                        ok
                    end,
                    {NSD, true};
                    _ ->
                    {StateData, false}
                end
            end,
            case IsAllowed of
            true ->
                case ejabberd_hooks:run_fold(filter_room_packet,
                       StateData#state.host, Packet,
                       [FromNick, From, StateData#state.jid]) of
                drop -> 
                {next_state, normal_state, NewStateData1};
                Packet1 ->
                lists:foreach(
                  fun({_LJID, Info}) ->
                      ejabberd_router:route(
                    jlib:jid_replace_resource(
                      StateData#state.jid,
                      FromNick),
                    Info#user.jid,
                    Packet1)
                  end,
                  ?DICT:to_list(StateData#state.users)),
                NewStateData2 =
                add_message_to_history(FromNick,
                               From,
                               Packet1,
                               NewStateData1),
                {next_state, normal_state, NewStateData2}
                end;
            _ ->
                Err =
                case (StateData#state.config)#config.allow_change_subj of
                    true ->
                    ?ERRT_FORBIDDEN(
                       Lang,
                       <<"Only moderators and participants are allowed to change the subject in this room">>);
                    _ ->
                    ?ERRT_FORBIDDEN(
                       Lang,
                       <<"Only moderators are allowed to change the subject in this room">>)
                end,
                ejabberd_router:route(
                                jlib:jid_replace_resource(
                                    StateData#state.jid,
                                    FromNick),
                    From,
                    jlib:make_error_reply(Packet, Err)),
                {next_state, normal_state, StateData}
            end;
        true ->
            ErrText = <<"Visitors are not allowed to send messages to all occupants">>,
            Err = jlib:make_error_reply(
                Packet, ?ERRT_FORBIDDEN(Lang, ErrText)),
            ejabberd_router:route(
              StateData#state.jid,
              From, Err),
            {next_state, normal_state, StateData}
        end;
    false ->
        ErrText = <<"Only occupants are allowed to send messages to the conference">>,
        Err = jlib:make_error_reply(
            Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
        ejabberd_router:route(StateData#state.jid, From, Err),
        {next_state, normal_state, StateData}
    end.

%% @doc Check if this non participant can send message to room.
%%
%% XEP-0045 v1.23:
%% 7.9 Sending a Message to All Occupants
%% an implementation MAY allow users with certain privileges
%% (e.g., a room owner, room admin, or service-level admin)
%% to send messages to the room even if those users are not occupants.
is_user_allowed_message_nonparticipant(JID, StateData) ->
    get_service_affiliation(JID, StateData) =:= owner.

%% @doc Get information of this participant, or default values.
%% If the JID is not a participant, return values for a service message.
get_participant_data(From, StateData) ->
    case ?DICT:find(jlib:jid_tolower(From), StateData#state.users) of
        {ok, #user{nick = FromNick, role = Role}} ->
            {FromNick, Role};
        error ->
            {<<>>, moderator}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Presence processing

%% @doc Process presence stanza and destroy the room, if it is empty.
-spec process_presence(From, Nick, Packet, StateData) -> Res when
    From :: #jid{},
    Nick :: binary(),
    Packet :: #xmlel{},
    StateData :: #state{},
    Res :: {next_state, normal_state, StateData}
         | {stop, normal, StateData}.
process_presence(From, ToNick, Presence, StateData) ->
    StateData1 = process_presence1(From, ToNick, Presence, StateData),
    destroy_temporary_room_if_empty(StateData1).

process_presence(From, ToNick, Presence, StateData, NextState) ->
    StateData1 = process_presence(From, ToNick, Presence, StateData),
    rewrite_next_state(NextState, StateData1).

rewrite_next_state(NewState, {next_state, _, StateData}) ->
    {next_state, NewState, StateData};
rewrite_next_state(_, {stop, normal, StateData}) ->
    {stop, normal, StateData}.

destroy_temporary_room_if_empty(StateData=#state{config=C=#config{}}) ->
    case (not C#config.persistent) andalso is_empty_room(StateData) of
        true ->
            ?INFO_MSG("Destroyed MUC room ~s because it's temporary and empty", 
                  [jlib:jid_to_binary(StateData#state.jid)]),
            add_to_log(room_existence, destroyed, StateData),
            {stop, normal, StateData};
        _ ->
            {next_state, normal_state, StateData}
    end.

-spec process_presence1(From, Nick, Packet, StateData) -> StateData when
    From :: #jid{},
    Nick :: binary(),
    Packet :: #xmlel{},
    StateData :: #state{}.
process_presence1(From, Nick, #xmlel{name = <<"presence">>,
                                     attrs = Attrs} = Packet,
         StateData=#state{}) ->
    Type = xml:get_attr_s(<<"type">>, Attrs),
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    case Type of
        <<"unavailable">> ->
            process_presence_unavailable(From, Packet, StateData);
        <<"error">> ->
            process_presence_error(From, Packet, Lang, StateData);
        <<>> ->
            case is_user_online(From, StateData) of
                true ->
                    case is_nick_change(From, Nick, StateData) of
                        true ->
                            process_presence_nick_change(
                                From, Nick, Packet, Lang, StateData);
                        _NotNickChange ->
                            process_simple_presence(
                                From, Packet, StateData)
                    end;
                false ->
                    %% at this point we know that the presence has no type
                    %% (user wants to enter the room)
                    %% and that the user is not alredy online
                    handle_new_user(From, Nick, Packet, StateData, Attrs)
            end;
            _NotOnline ->
                StateData
    end.

process_simple_presence(From, Packet, StateData) ->
    NewPacket = check_and_strip_visitor_status(From, Packet, StateData),
    NewState = add_user_presence(From, NewPacket, StateData),
    send_new_presence(From, NewState),
    NewState.

process_presence_error(From, Packet, Lang, StateData) ->
    case is_user_online(From, StateData) of
        true ->
            ErrorText = <<"This participant is kicked from the room because he sent an error presence">>,
            expulse_participant(Packet, From, StateData,
                translate:translate(Lang, ErrorText)),
            StateData;
        _ ->
            StateData
    end.

process_presence_unavailable(From, Packet, StateData) ->
    case is_user_online(From, StateData) of
        true ->
            NewPacket = check_and_strip_visitor_status(From, Packet, StateData),
            NewState = add_user_presence_un(From, NewPacket, StateData),
            send_new_presence(From, NewState),
            Reason = case xml:get_subtag(NewPacket, <<"status">>) of
                false -> <<>>;
                Status_el -> xml:get_tag_cdata(Status_el)
            end,
            remove_online_user(From, NewState, Reason);
        _ ->
            StateData
    end.

choose_nick_change_strategy(From, Nick, StateData) ->
    case {is_nick_exists(Nick, StateData),
          mod_muc:can_use_nick(StateData#state.host, From, Nick),
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

process_presence_nick_change(From, Nick, Packet, Lang, StateData) ->
    case choose_nick_change_strategy(From, Nick, StateData) of
        not_allowed_visitor ->
            ErrText = <<"Visitors are not allowed to change their nicknames in this room">>,
            Err = jlib:make_error_reply(Packet, ?ERRT_NOT_ALLOWED(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        conflict_use ->
            ErrText = <<"That nickname is already in use by another occupant">>,
            Err = jlib:make_error_reply(Packet, ?ERRT_CONFLICT(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        conflict_registered ->
            ErrText = <<"That nickname is registered by another person">>,
            Err = jlib:make_error_reply(Packet, ?ERRT_CONFLICT(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        allowed ->
            change_nick(From, Nick, StateData)
    end.

check_and_strip_visitor_status(From, Packet, StateData) ->
    case {(StateData#state.config)#config.allow_visitor_status,
          is_visitor(From, StateData)} of
        {false, true} ->
            strip_status(Packet);
        _ ->
            Packet
    end.


handle_new_user(From, Nick = <<>>, _Packet, StateData, Attrs) ->
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    ErrText = <<"No nickname">>,
    Error =jlib:make_error_reply(
                #xmlel{name = <<"presence">>},
                ?ERRT_JID_MALFORMED(Lang, ErrText)),
    %ejabberd_route(From, To, Packet),
    ejabberd_router:route(jlib:jid_replace_resource(StateData#state.jid, Nick), From, Error),
    StateData;

handle_new_user(From, Nick, Packet, StateData, _Attrs) ->
    add_new_user(From, Nick, Packet, StateData).

is_user_online(JID, StateData) ->
    LJID = jlib:jid_tolower(JID),
    ?DICT:is_key(LJID, StateData#state.users).

%% Check if the user is occupant of the room, or at least is an admin or owner.
is_occupant_or_admin(JID, StateData) ->
    FAffiliation = get_affiliation(JID, StateData),
    FRole = get_role(JID, StateData),
    (FRole /= none) orelse
    (FAffiliation == admin) orelse
    (FAffiliation == owner).

%%%
%%% Handle IQ queries of vCard
%%%
is_user_online_iq(StanzaId, JID, StateData) when JID#jid.lresource /= <<>> ->
    {is_user_online(JID, StateData), StanzaId, JID};
is_user_online_iq(StanzaId, JID, StateData) when JID#jid.lresource == <<>> ->
    try stanzaid_unpack(StanzaId) of
        {OriginalId, Resource} ->
            JIDWithResource = jlib:jid_replace_resource(JID, Resource),
            {is_user_online(JIDWithResource, StateData),
             OriginalId, JIDWithResource}
    catch
        _:_ ->
            {is_user_online(JID, StateData), StanzaId, JID}
    end.

handle_iq_vcard(FromFull, ToJID, StanzaId, NewId, Packet) ->
    ToBareJID = jlib:jid_remove_resource(ToJID),
    IQ = jlib:iq_query_info(Packet),
    handle_iq_vcard2(FromFull, ToJID, ToBareJID, StanzaId, NewId, IQ, Packet).
handle_iq_vcard2(_FromFull, ToJID, ToBareJID, StanzaId, _NewId,
         #iq{type = get, xmlns = ?NS_VCARD}, Packet)
  when ToBareJID /= ToJID ->
    {ToBareJID, change_stanzaid(StanzaId, ToJID, Packet)};
handle_iq_vcard2(_FromFull, ToJID, _ToBareJID, _StanzaId, NewId, _IQ, Packet) ->
    {ToJID, change_stanzaid(NewId, Packet)}.

stanzaid_pack(OriginalId, Resource) ->
    Data64 = base64:encode(<<"ejab\0", OriginalId/binary, 0, Resource/binary>>),
    <<"berd", Data64/binary>>.
stanzaid_unpack(<<"berd", StanzaIdBase64/binary>>) ->
    StanzaId = base64:decode(StanzaIdBase64),
    [<<"ejab">>, OriginalId, Resource] = binary:split(StanzaId, <<"\0">>),
    {OriginalId, Resource}.

change_stanzaid(NewId, Packet) ->
    XE = #xmlel{attrs = Attrs} = jlib:remove_attr(<<"id">>, Packet),
    XE#xmlel{attrs = [{<<"id">>, NewId} | Attrs]}.
change_stanzaid(PreviousId, ToJID, Packet) ->
    NewId = stanzaid_pack(PreviousId, ToJID#jid.lresource),
    change_stanzaid(NewId, Packet).
%%%
%%%

-spec role_to_binary(role()) -> binary().
role_to_binary(Role) ->
    case Role of
        moderator   -> <<"moderator">>;
        participant -> <<"participant">>;
        visitor     -> <<"visitor">>;
        none        -> <<"none">>
    end.

-spec affiliation_to_binary(affiliation()) -> binary().
affiliation_to_binary(Affiliation) ->
    case Affiliation of
        owner   -> <<"owner">>;
        admin   -> <<"admin">>;
        member  -> <<"member">>;
        outcast -> <<"outcast">>;
        none    -> <<"none">>
    end.

-spec binary_to_role(binary()) -> role().
binary_to_role(Role) ->
    case Role of
        <<"moderator">>     -> moderator;
        <<"participant">>   -> participant;
        <<"visitor">>       -> visitor;
        <<"none">>          -> none
    end.

-spec binary_to_affiliation(binary()) -> affiliation().
binary_to_affiliation(Affiliation) ->
    case Affiliation of
        <<"owner">>     -> owner;
        <<"admin">>     -> admin;
        <<"member">>    -> member;
        <<"outcast">>   -> outcast;
        <<"none">>      -> none
    end.

%% Decide the fate of the message and its sender
%% Returns: continue_delivery | forget_message | {expulse_sender, Reason}
decide_fate_message(<<"error">>, Packet, From, StateData) ->
    %% Make a preliminary decision
    PD = case check_error_kick(Packet) of
         %% If this is an error stanza and its condition matches a criteria
         true ->
         Reason = "This participant is considered a ghost and is expulsed: " ++
            binary_to_list(jlib:jid_to_binary(From)),
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

%% Check if the elements of this error stanza indicate
%% that the sender is a dead participant.
%% If so, return true to kick the participant.
check_error_kick(Packet) ->
    case get_error_condition(Packet) of
    <<"gone">> -> true;
    <<"internal-server-error">> -> true;
    <<"item-not-found">> -> true;
    <<"jid-malformed">> -> true;
    <<"recipient-unavailable">> -> true;
    <<"redirect">> -> true;
    <<"remote-server-not-found">> -> true;
    <<"remote-server-timeout">> -> true;
    <<"service-unavailable">> -> true;
    _ -> false
    end.

get_error_condition(Packet) ->
    case catch get_error_condition2(Packet) of
         {condition, ErrorCondition} ->
        ErrorCondition;
         {'EXIT', _} ->
        <<"badformed error stanza">>
    end.
get_error_condition2(Packet) ->
    #xmlel{children = EEls} = xml:get_subtag(Packet, <<"error">>),
    [Condition] = [Name || #xmlel{name = Name,
                                  attrs = [{<<"xmlns">>, ?NS_STANZAS}],
                                  children = []} <- EEls],
    {condition, Condition}.

expulse_participant(Packet, From, StateData, Reason1) ->
    ErrorCondition = get_error_condition(Packet),
    Reason2 = <<Reason1/binary, ": ", ErrorCondition/binary>>,
    NewState = add_user_presence_un(
        From,
        #xmlel{name = <<"presence">>, attrs = [{<<"type">>, <<"unavailable">>}],
               children = [#xmlel{name = <<"status">>,
                                  children = [#xmlcdata{content = Reason2}]}]},
    StateData),
    send_new_presence(From, NewState),
    remove_online_user(From, NewState).

access_admin(#state{access=Access}) ->
    {_AccessRoute, _AccessCreate, AccessAdmin, _AccessPersistent} = Access,
    AccessAdmin.

access_persistent(#state{access=Access}) ->
    {_AccessRoute, _AccessCreate, _AccessAdmin, AccessPersistent} = Access,
    AccessPersistent.

-spec set_affiliation(jid(), affiliation(), state_data()) -> state_data().
set_affiliation(JID, Affiliation, StateData)
        when is_atom(Affiliation) ->
    LJID = jlib:jid_remove_resource(jlib:jid_tolower(JID)),
    Affiliations = case Affiliation of
               none ->
               ?DICT:erase(LJID,
                       StateData#state.affiliations);
               _ ->
               ?DICT:store(LJID,
                       Affiliation,
                       StateData#state.affiliations)
           end,
    StateData#state{affiliations = Affiliations}.

-spec set_affiliation_and_reason(
        jid(), affiliation(), term(), state_data()) -> state_data().
set_affiliation_and_reason(JID, Affiliation, Reason, StateData)
        when is_atom(Affiliation) ->
    LJID = jlib:jid_remove_resource(jlib:jid_tolower(JID)),
    Affiliations = case Affiliation of
               none ->
               ?DICT:erase(LJID,
                       StateData#state.affiliations);
               _ ->
               ?DICT:store(LJID,
                       {Affiliation, Reason},
                       StateData#state.affiliations)
           end,
    StateData#state{affiliations = Affiliations}.

-spec get_affiliation(jid(), state_data()) -> affiliation().
get_affiliation(JID, StateData) ->
    AccessAdmin = access_admin(StateData),
    Res =
    case acl:match_rule(StateData#state.server_host, AccessAdmin, JID) of
        allow ->
        owner;
        _ ->
        LJID = jlib:jid_tolower(JID),
        case ?DICT:find(LJID, StateData#state.affiliations) of
            {ok, Affiliation} ->
            Affiliation;
            _ ->
            LJID1 = jlib:jid_remove_resource(LJID),
            case ?DICT:find(LJID1, StateData#state.affiliations) of
                {ok, Affiliation} ->
                Affiliation;
                _ ->
                LJID2 = setelement(1, LJID, <<>>),
                case ?DICT:find(LJID2, StateData#state.affiliations) of
                    {ok, Affiliation} ->
                    Affiliation;
                    _ ->
                    LJID3 = jlib:jid_remove_resource(LJID2),
                    case ?DICT:find(LJID3, StateData#state.affiliations) of
                        {ok, Affiliation} ->
                        Affiliation;
                        _ ->
                        none
                    end
                end
            end
        end
    end,
    case Res of
    {A, _Reason} ->
        A;
    _ ->
        Res
    end.

get_service_affiliation(JID, StateData) ->
    AccessAdmin = access_admin(StateData),
    case acl:match_rule(StateData#state.server_host, AccessAdmin, JID) of
    allow ->
        owner;
    _ ->
        none
    end.

-spec set_role(JID :: jid(), Role :: role(), StateData) -> StateData when
    StateData :: state_data().
set_role(JID, none, StateData) ->
    erase_matched_users(JID, StateData);
set_role(JID, Role, StateData) ->
    update_matched_users(fun(User) -> User#user{role = Role} end,
                         JID, StateData).

-spec get_role(jid(), state_data()) -> role().
get_role(JID, StateData) ->
    LJID = jlib:jid_tolower(JID),
    case ?DICT:find(LJID, StateData#state.users) of
    {ok, #user{role = Role}} ->
        Role;
    _ ->
        none
    end.

-spec get_default_role(affiliation(), state_data()) -> role().
get_default_role(Affiliation, StateData) ->
    case Affiliation of
        owner   -> moderator;
        admin   -> moderator;
        member  -> participant;
        outcast -> none;
        none    ->
            case (StateData#state.config)#config.members_only of
                true ->
                    none;
                _ ->
                    case (StateData#state.config)#config.members_by_default of
                        true ->
                            participant;
                        _ ->
                            visitor
                    end
            end
    end.

-spec is_visitor(jid(), state_data()) -> boolean().
is_visitor(Jid, StateData) ->
    get_role(Jid, StateData) =:= visitor.

is_empty_room(#state{users=Users}) ->
    is_empty_dict(Users).

is_empty_dict(Dict) ->
    dict:size(Dict) =:= 0.

dict_foreach_value(F, Users) ->
    ?DICT:fold(fun(_LJID, User, _) -> F(User) end, undefined, Users).

dict_to_values(Dict) ->
    [V || {_, V} <- ?DICT:to_list(Dict)].

count_users(#state{users=Users}) ->
    ?DICT:size(Users).

get_max_users(StateData) ->
    MaxUsers = (StateData#state.config)#config.max_users,
    ServiceMaxUsers = get_service_max_users(StateData),
    if
    MaxUsers =< ServiceMaxUsers -> MaxUsers;
    true -> ServiceMaxUsers
    end.

get_service_max_users(StateData) ->
    gen_mod:get_module_opt(StateData#state.server_host,
               mod_muc, max_users, ?MAX_USERS_DEFAULT).

get_max_users_admin_threshold(StateData) ->
    gen_mod:get_module_opt(StateData#state.server_host,
               mod_muc, max_users_admin_threshold, 5).

get_user_activity(JID, StateData) ->
    case treap:lookup(jlib:jid_tolower(JID),
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

store_user_activity(JID, UserActivity, StateData) ->
    MinMessageInterval =
    gen_mod:get_module_opt(
      StateData#state.server_host,
      mod_muc, min_message_interval, 0),
    MinPresenceInterval =
    gen_mod:get_module_opt(
      StateData#state.server_host,
      mod_muc, min_presence_interval, 0),
    Key = jlib:jid_tolower(JID),
    Now = now_to_usec(now()),
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

clean_treap(Treap, CleanPriority) ->
    case treap:is_empty(Treap) of
    true ->
        Treap;
    false ->
        {_Key, Priority, _Value} = treap:get_root(Treap),
        if
        Priority > CleanPriority ->
            clean_treap(treap:delete_root(Treap), CleanPriority);
        true ->
            Treap
        end
    end.


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


add_online_user(JID, Nick, Role, StateData) ->
    LJID = jlib:jid_tolower(JID),
    Users = ?DICT:store(LJID,
            #user{jid = JID,
                  nick = Nick,
                  role = Role},
            StateData#state.users),
    add_to_log(join, Nick, StateData),
    tab_add_online_user(JID, StateData),
    StateData#state{users = Users}.

remove_online_user(JID, StateData) ->
    remove_online_user(JID, StateData, <<>>).

remove_online_user(JID, StateData, Reason) ->
    LJID = jlib:jid_tolower(JID),
    {ok, #user{nick = Nick}} =
        ?DICT:find(LJID, StateData#state.users),
    add_to_log(leave, {Nick, Reason}, StateData),
    tab_remove_online_user(JID, StateData),
    Users = ?DICT:erase(LJID, StateData#state.users),
    StateData#state{users = Users}.


filter_presence(#xmlel{name = <<"presence">>, attrs = Attrs,
                       children = Els}) ->
    FEls = lists:filter(
         fun(El) ->
             case El of
             #xmlcdata{} ->
                 false;
             #xmlel{attrs = Attrs1} ->
                 XMLNS = xml:get_attr_s(<<"xmlns">>, Attrs1),
                 case XMLNS of
                 <<?NS_MUC_S, _/binary>> -> false;
                 _ ->
                     true
                 end
             end
         end, Els),
    #xmlel{name = <<"presence">>, attrs = Attrs, children = FEls}.

strip_status(#xmlel{name = <<"presence">>, attrs = Attrs,
                    children = Els}) ->
    FEls = lists:filter(
         fun(#xmlel{name = <<"status">>}) ->
                     false;
                (_) -> true
         end, Els),
    #xmlel{name = <<"presence">>, attrs = Attrs, children = FEls}.

add_user_presence(JID, Presence, StateData) ->
    LJID = jlib:jid_tolower(JID),
    FPresence = filter_presence(Presence),
    Users =
    ?DICT:update(
       LJID,
       fun(#user{} = User) ->
           User#user{last_presence = FPresence}
       end, StateData#state.users),
    StateData#state{users = Users}.

add_user_presence_un(JID, Presence, StateData) ->
    LJID = jlib:jid_tolower(JID),
    FPresence = filter_presence(Presence),
    Users =
    ?DICT:update(
       LJID,
       fun(#user{} = User) ->
           User#user{last_presence = FPresence, role = none}
       end, StateData#state.users),
    StateData#state{users = Users}.


is_nick_exists(Nick, StateData) ->
    ?DICT:fold(fun(_, #user{nick = N}, B) ->
               B orelse (N == Nick)
           end, false, StateData#state.users).

find_jid_by_nick(Nick, StateData) ->
    ?DICT:fold(fun(_, #user{jid = JID, nick = N}, R) ->
               case Nick of
               N -> JID;
               _ -> R
               end
           end, false, StateData#state.users).

is_nick_change(JID, Nick, StateData) ->
    LJID = jlib:jid_tolower(JID),
    case Nick of
    <<>> ->
        false;
    _ ->
        {ok, #user{nick = OldNick}} =
        ?DICT:find(LJID, StateData#state.users),
        Nick /= OldNick
    end.

is_user_limit_reached(From, Affiliation, StateData) ->
    MaxUsers = get_max_users(StateData),
    MaxAdminUsers = MaxUsers + get_max_users_admin_threshold(StateData),
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

choose_new_user_strategy(From, Nick, Affiliation, Role, Els, StateData) ->
    case {is_user_limit_reached(From, Affiliation, StateData),
          is_nick_exists(Nick, StateData),
          mod_muc:can_use_nick(StateData#state.host, From, Nick),
          Role,
          Affiliation} of
        {false, _, _, _, _} ->
            limit_reached;
        {_, _, _, none, outcast} ->
            user_banned;
        {_, _, _, none, _} ->
            require_membership;
        {_, true, _, _, _} ->
            conflict_use;
        {_, _, false, _, _} ->
            conflict_registered;
        _ ->
            ServiceAffiliation = get_service_affiliation(From, StateData),
            case check_password(
                ServiceAffiliation, Affiliation, Els, From, StateData) of
                true    -> allowed;
                nopass  -> require_password;
                _       -> invalid_password
            end
    end.

add_new_user(From, Nick,
             #xmlel{attrs = Attrs, children = Els} = Packet,
             #state{} = StateData) ->
    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
    Affiliation = get_affiliation(From, StateData),
    Role = get_default_role(Affiliation, StateData),
    case choose_new_user_strategy(From, Nick, Affiliation, Role, Els, StateData) of
        limit_reached ->
            % max user reached and user is not admin or owner
            Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE_WAIT),
            route_error(Nick, From, Err, StateData);
        user_banned ->
            ErrText = <<"You have been banned from this room">>,
            Err = jlib:make_error_reply(Packet, ?ERRT_FORBIDDEN(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        require_membership ->
            ErrText = <<"Membership is required to enter this room">>,
            Err = jlib:make_error_reply(
                Packet, ?ERRT_REGISTRATION_REQUIRED(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        conflict_use ->
            ErrText = <<"That nickname is already in use by another occupant">>,
            Err = jlib:make_error_reply(Packet, ?ERRT_CONFLICT(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        conflict_registered ->
            ErrText = <<"That nickname is registered by another person">>,
            Err = jlib:make_error_reply(Packet, ?ERRT_CONFLICT(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        require_password ->
            ErrText = <<"A password is required to enter this room">>,
            Err = jlib:make_error_reply(
                Packet, ?ERRT_NOT_AUTHORIZED(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        invalid_password ->
            ErrText = <<"Incorrect password">>,
            Err = jlib:make_error_reply(
                Packet, ?ERRT_NOT_AUTHORIZED(Lang, ErrText)),
            route_error(Nick, From, Err, StateData);
        allowed ->
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
                    Robots = ?DICT:erase(From, StateData#state.robots),
                    NewState#state{robots = Robots}
            end
    end.

-spec check_password(
    ServiceAffiliation, Affiliation, Els, _From, StateData) ->
    boolean() | nopass when
    ServiceAffiliation :: affiliation(),
    Affiliation :: affiliation(),
    Els :: [#xmlel{}],
    StateData :: state_data().
check_password(owner, _Affiliation, _Els, _From, _StateData) ->
    %% Don't check pass if user is owner in MUC service (access_admin option)
    true;
check_password(_ServiceAffiliation, _Affiliation, Els, _From, StateData) ->
    case (StateData#state.config)#config.password_protected of
        false ->
            %% Don't check password
            true;
        true ->
            Pass = extract_password(Els),
            case Pass of
                false ->
                    nopass;
                _ ->
                    (StateData#state.config)#config.password =:= Pass
            end
    end.

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
                 calendar:now_to_universal_time(now())) - Seconds,
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

count_seconds_shift(Seconds, HistoryList) ->
    lists:sum(
      lists:map(
    fun({_Nick, _Packet, _HaveSubject, TimeStamp, _Size}) ->
        T = calendar:datetime_to_gregorian_seconds(TimeStamp),
        if
        T < Seconds ->
            1;
        true ->
            0
        end
    end, HistoryList)).

count_maxstanzas_shift(MaxStanzas, HistoryList) ->
    S = length(HistoryList) - MaxStanzas,
    max(0, S).

count_maxchars_shift(Nick, MaxSize, HistoryList) ->
    NLen = string:len(binary_to_list(Nick)) + 1,
    Sizes = lists:map(
          fun({_Nick, _Packet, _HaveSubject, _TimeStamp, Size}) ->
          Size + NLen
          end, HistoryList),
    calc_shift(MaxSize, Sizes).

calc_shift(MaxSize, Sizes) ->
    Total = lists:sum(Sizes),
    calc_shift(MaxSize, Total, 0, Sizes).

calc_shift(_MaxSize, _Size, Shift, []) ->
    Shift;
calc_shift(MaxSize, Size, Shift, [S | TSizes]) ->
    if
    MaxSize >= Size ->
        Shift;
    true ->
        calc_shift(MaxSize, Size - S, Shift + 1, TSizes)
    end.

extract_history([], _Type) ->
    false;
extract_history([#xmlel{attrs = Attrs} = El | Els], Type) ->
    case xml:get_attr_s(<<"xmlns">>, Attrs) of
    ?NS_MUC ->
        AttrVal = xml:get_path_s(El,
               [{elem, <<"history">>}, {attr, Type}]),
        case Type of
        <<"since">> ->
            case jlib:datetime_string_to_timestamp(AttrVal) of
            undefined ->
                false;
            TS ->
                calendar:now_to_universal_time(TS)
            end;
        _ ->
            case catch binary_to_integer(AttrVal) of
            IntVal when is_integer(IntVal) and (IntVal >= 0) ->
                IntVal;
            _ ->
                false
            end
        end;
    _ ->
        extract_history(Els, Type)
    end;
extract_history([_ | Els], Type) ->
    extract_history(Els, Type).

send_update_presence(JID, Reason, StateData) ->
    foreach_matched_jid(fun(J) ->
      send_new_presence(J, Reason, StateData)
    end, JID, StateData).

foreach_matched_jid(F, JID, #state{users=Users}) ->
    LJID = jlib:jid_tolower(JID),
    case LJID of
        %% Match by bare JID
        {U, S, <<>>} ->
            FF = fun(J, #user{jid=MatchedJID}, _) ->
                     case J of
                         {U, S, _} -> F(MatchedJID);
                         _         -> ok
                     end
                 end,
            ?DICT:fold(FF, ok, Users);
        %% Match by full JID
        _ ->
            case ?DICT:is_key(LJID, Users) of
                true ->
                    F(JID),
                    ok;
                false ->
                    ok
            end
    end.

foreach_matched_user(F, JID, #state{users=Users}) ->
    LJID = jlib:jid_tolower(JID),
    case LJID of
        %% Match by bare JID
        {U, S, <<>>} ->
            FF = fun(J, User, _) ->
                     case J of
                         {U, S, _} -> F(User);
                         _         -> ok
                     end
                 end,
            ?DICT:fold(FF, ok, Users);
        %% Match by full JID
        _ ->
            case ?DICT:find(LJID, Users) of
                {ok, User} -> F(User);
                error -> ok
            end
    end.

foreach_user(F, #state{users=Users}) ->
    dict_foreach_value(F, Users).

erase_matched_users(JID, StateData=#state{users=Users}) ->
    LJID = jlib:jid_tolower(JID),
    NewUsers = erase_matched_users_dict(LJID, Users),
    StateData#state{users=NewUsers}.

erase_matched_users_dict(LJID, Users) ->
    case LJID of
        %% Match by bare JID
        {U, S, <<>>} ->
            FF = fun(J, _, Us) ->
                     case J of
                         {U, S, _} -> ?DICT:erase(J, Us);
                         _         -> Us
                     end
                 end,
            ?DICT:fold(FF, Users, Users);
        %% Match by full JID
        _ ->
            ?DICT:erase(LJID, Users)
    end.

-spec update_matched_users(F, JID, StateData) -> StateData when
    F :: fun((User) -> User),
    JID :: jid(),
    StateData :: state_data().
update_matched_users(F, JID, StateData=#state{users=Users}) ->
    LJID = jlib:jid_tolower(JID),
    NewUsers = update_matched_users_dict(F, LJID, Users),
    StateData#state{users=NewUsers}.

update_matched_users_dict(F, LJID, Users) ->
    case LJID of
        %% Match by bare JID
        {U, S, <<>>} ->
            FF = fun(J, User, Us) ->
                     case J of
                         {U, S, _} -> ?DICT:store(J, F(User), Us);
                         _         -> Us
                     end
                 end,
            ?DICT:fold(FF, Users, Users);
        %% Match by full JID
        _ ->
            case ?DICT:find(LJID, Users) of
                {ok, User} -> ?DICT:store(LJID, F(User), Users);
                error -> Users
            end
    end.

send_new_presence(NJID, StateData) ->
    send_new_presence(NJID, <<>>, StateData).

send_new_presence(NJID, Reason, StateData) ->
    {ok, #user{jid = RealJID,
           nick = Nick,
           role = Role,
           last_presence = Presence}} =
    ?DICT:find(jlib:jid_tolower(NJID), StateData#state.users),
    Affiliation = get_affiliation(NJID, StateData),
    BAffiliation = affiliation_to_binary(Affiliation),
    BRole = role_to_binary(Role),
    lists:foreach(
      fun({_LJID, Info}) ->
          ItemAttrs =
          case (Info#user.role == moderator) orelse
              ((StateData#state.config)#config.anonymous == false) of
              true ->
              [{<<"jid">>, jlib:jid_to_binary(RealJID)},
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
                   [#xmlel{name = <<"status">>,
                           attrs = [{<<"code">>, <<"201">>}]}];
               false ->
                   []
               end,
          Status2 = case (NJID == Info#user.jid) of
                 true ->
                    Status0 = case   (StateData#state.config)#config.logging of
                            true ->
                            [#xmlel{name = <<"status">>,
                                    attrs = [{<<"code">>, <<"170">>}]}|Status];
                            false ->
                            Status
                        end,
                    Status1 = case ((StateData#state.config)#config.anonymous==false) of
                            true ->
                            [#xmlel{name = <<"status">>,
                                    attrs = [{<<"code">>, <<"100">>}]}
                            | Status0];
                            false ->
                            Status0
                        end,
                    case ((NJID == Info#user.jid)==true) of
                            true ->
                            [#xmlel{name = <<"status">>,
                                    attrs = [{<<"code">>, <<"110">>}]}
                            | Status1];
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
          ejabberd_router:route(
        jlib:jid_replace_resource(StateData#state.jid, Nick),
        Info#user.jid,
        Packet)
      end, ?DICT:to_list(StateData#state.users)).


send_existing_presences(ToJID, StateData) ->
    LToJID = jlib:jid_tolower(ToJID),
    {ok, #user{jid = RealToJID, role = Role}} =
    ?DICT:find(LToJID, StateData#state.users),
    lists:foreach(
      fun({LJID, #user{jid = FromJID,
               nick = FromNick,
               role = FromRole,
               last_presence = Presence
              }}) ->
          case RealToJID of
          FromJID ->
              ok;
          _ ->
              FromAffiliation = get_affiliation(LJID, StateData),
              ItemAttrs =
              case (Role == moderator) orelse
                  ((StateData#state.config)#config.anonymous ==
                   false) of
                  true ->
                  [{<<"jid">>, jlib:jid_to_binary(FromJID)},
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
                 [#xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
                         children = [#xmlel{name = <<"item">>,
                                            attrs = ItemAttrs}]}]),
              ejabberd_router:route(
            jlib:jid_replace_resource(
              StateData#state.jid, FromNick),
            RealToJID,
            Packet)
          end
      end, ?DICT:to_list(StateData#state.users)).

send_config_update(Type, StateData) ->
    Status = case Type of
            logging_enabled     -> <<"170">>;
            logging_disabled    -> <<"171">>;
            nonanonymous        -> <<"172">>;
            semianonymous       -> <<"173">>;
            _                   -> <<"104">>
        end,
    Message = jlib:make_config_change_message(Status),
    lists:foreach(fun({_LJID, Info}) ->
        ejabberd_router:route(
            StateData#state.jid,
            Info#user.jid,
            Message)
        end, ?DICT:to_list(StateData#state.users)).

send_invitation(From, To, Reason, StateData) ->
    Config = StateData#state.config,
    Password = case Config#config.password_protected of
        false -> <<>>;
        true -> Config#config.password
    end,
    ejabberd_router:route(
        StateData#state.jid,
        To,
        jlib:make_invitation(
            jlib:jid_replace_resource(From, <<>>), Password, Reason)).

now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.


change_nick(JID, Nick, StateData) ->
    LJID = jlib:jid_tolower(JID),
    {ok, #user{nick = OldNick}} =
    ?DICT:find(LJID, StateData#state.users),
    Users =
    ?DICT:update(
       LJID,
       fun(#user{} = User) ->
           User#user{nick = Nick}
       end, StateData#state.users),
    NewStateData = StateData#state{users = Users},
    send_nick_changing(JID, OldNick, NewStateData),
    add_to_log(nickchange, {OldNick, Nick}, StateData),
    NewStateData.

send_nick_changing(JID, OldNick, StateData) ->
    {ok, #user{jid = RealJID,
           nick = Nick,
           role = Role,
           last_presence = Presence}} =
    ?DICT:find(jlib:jid_tolower(JID), StateData#state.users),
    Affiliation = get_affiliation(JID, StateData),
    BAffiliation = affiliation_to_binary(Affiliation),
    BRole = role_to_binary(Role),
    lists:foreach(
      fun({_LJID, Info}) ->
          ItemAttrs1 =
          case (Info#user.role == moderator) orelse
              ((StateData#state.config)#config.anonymous == false) of
              true ->
              [{<<"jid">>, jlib:jid_to_binary(RealJID)},
               {<<"affiliation">>, BAffiliation},
               {<<"role">>, BRole},
               {<<"nick">>, Nick}];
              _ ->
              [{<<"affiliation">>, BAffiliation},
               {<<"role">>, BRole},
               {<<"nick">>, Nick}]
          end,
          ItemAttrs2 =
          case (Info#user.role == moderator) orelse
              ((StateData#state.config)#config.anonymous == false) of
              true ->
              [{<<"jid">>, jlib:jid_to_binary(RealJID)},
               {<<"affiliation">>, BAffiliation},
               {<<"role">>, BRole}];
              _ ->
              [{<<"affiliation">>, BAffiliation},
               {<<"role">>, BRole}]
          end,

		  SelfPresenceCode= if 
		  		JID == Info#user.jid -> 
				[#xmlel{name = <<"status">>,
				        attrs = [{<<"code">>, <<"110">>}]}];
			  true ->
				  []
                            end,
          Packet1 =
          #xmlel{name = <<"presence">>,
                 attrs = [{<<"type">>, <<"unavailable">>}],
                 children = [#xmlel{name = <<"x">>,
                                    attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
                                    children = [#xmlel{name = <<"item">>,
                                                       attrs = ItemAttrs1},
                                                #xmlel{name = <<"status">>,
                                                       attrs = [{<<"code">>, <<"303">>}]}| SelfPresenceCode]}]},
          Packet2 = xml:append_subtags(
              Presence,
              [#xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
                      children = [#xmlel{name = <<"item">>,
                                         attrs = ItemAttrs2}| SelfPresenceCode]}]),
          ejabberd_router:route(
        jlib:jid_replace_resource(StateData#state.jid, OldNick),
        Info#user.jid,
        Packet1),
          ejabberd_router:route(
        jlib:jid_replace_resource(StateData#state.jid, Nick),
        Info#user.jid,
        Packet2)
      end, ?DICT:to_list(StateData#state.users)).


lqueue_new(Max) ->
    #lqueue{queue = queue:new(),
        len = 0,
        max = Max}.

%% If the message queue limit is set to 0, do not store messages.
lqueue_in(_Item, LQ = #lqueue{max = 0}) ->
    LQ;
%% Otherwise, rotate messages in the queue store.
lqueue_in(Item, #lqueue{queue = Q1, len = Len, max = Max}) ->
    Q2 = queue:in(Item, Q1),
    if
    Len >= Max ->
        Q3 = lqueue_cut(Q2, Len - Max + 1),
        #lqueue{queue = Q3, len = Max, max = Max};
    true ->
        #lqueue{queue = Q2, len = Len + 1, max = Max}
    end.

lqueue_cut(Q, 0) ->
    Q;
lqueue_cut(Q, N) ->
    {_, Q1} = queue:out(Q),
    lqueue_cut(Q1, N - 1).

lqueue_to_list(#lqueue{queue = Q1}) ->
    queue:to_list(Q1).


add_message_to_history(FromNick, FromJID, Packet, StateData) ->
    HaveSubject = case xml:get_subtag(Packet, <<"subject">>) of
              false ->
              false;
              _ ->
              true
          end,
    TimeStamp = calendar:now_to_universal_time(now()),
    %% Chatroom history is stored as XMPP packets, so
    %% the decision to include the original sender's JID or not is based on the
    %% chatroom configuration when the message was originally sent.
    %% Also, if the chatroom is anonymous, even moderators will not get the real JID
    SenderJid = case ((StateData#state.config)#config.anonymous) of
    true -> StateData#state.jid;
    false -> FromJID
    end,
    TSPacket = xml:append_subtags(Packet,
                  [jlib:timestamp_to_xml(TimeStamp, utc, SenderJid, <<>>),
                   %% TODO: Delete the next line once XEP-0091 is Obsolete
                   jlib:timestamp_to_xml(TimeStamp)]),
    SPacket = jlib:replace_from_to(
        jlib:jid_replace_resource(StateData#state.jid, FromNick),
        StateData#state.jid,
        TSPacket),
    Size = element_size(SPacket),
    Q1 = lqueue_in({FromNick, TSPacket, HaveSubject, TimeStamp, Size},
           StateData#state.history),
    add_to_log(text, {FromNick, Packet}, StateData),
    ejabberd_hooks:run(room_packet, StateData#state.host,
                       [FromNick, FromJID, StateData#state.jid, Packet]),
    StateData#state{history = Q1}.

send_history(JID, Shift, StateData) ->
    lists:foldl(
      fun({Nick, Packet, HaveSubject, _TimeStamp, _Size}, B) ->
          ejabberd_router:route(
        jlib:jid_replace_resource(StateData#state.jid, Nick),
        JID,
        Packet),
          B or HaveSubject
      end, false, lists:nthtail(Shift, lqueue_to_list(StateData#state.history))).


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

check_subject(Packet) ->
    case xml:get_subtag(Packet, <<"subject">>) of
    false ->
        false;
    SubjEl ->
        xml:get_tag_cdata(SubjEl)
    end.

can_change_subject(Role, StateData) ->
    case (StateData#state.config)#config.allow_change_subj of
    true ->
        (Role == moderator) orelse (Role == participant);
    _ ->
        Role == moderator
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Admin stuff

process_iq_admin(From, set, Lang, SubEl, StateData) ->
    #xmlel{children = Items} = SubEl,
    process_admin_items_set(From, Items, Lang, StateData);

process_iq_admin(From, get, Lang, SubEl, StateData) ->
    case xml:get_subtag(SubEl, <<"item">>) of
    false ->
        {error, ?ERR_BAD_REQUEST};
    Item ->
        FAffiliation = get_affiliation(From, StateData),
        FRole = get_role(From, StateData),
        case xml:get_tag_attr(<<"role">>, Item) of
        false ->
            case xml:get_tag_attr(<<"affiliation">>, Item) of
            false ->
                {error, ?ERR_BAD_REQUEST};
            {value, BAffiliation} ->
                case catch binary_to_affiliation(BAffiliation) of
                {'EXIT', _} ->
                    {error, ?ERR_BAD_REQUEST};
                Affiliation ->
                    if
                    (FAffiliation == owner) or
                    (FAffiliation == admin) ->
                        Items = items_with_affiliation(
                              Affiliation, StateData),
                        {result, Items, StateData};
                    true ->
                        ErrText = <<"Administrator privileges required">>,
                        {error, ?ERRT_FORBIDDEN(Lang, ErrText)}
                    end
                end
            end;
        {value, BRole} ->
            case catch binary_to_role(BRole) of
            {'EXIT', _} ->
                {error, ?ERR_BAD_REQUEST};
            Role ->
                if
                FRole == moderator ->
                    Items = items_with_role(Role, StateData),
                    {result, Items, StateData};
                true ->
                    ErrText = <<"Moderator privileges required">>,
                    {error, ?ERRT_FORBIDDEN(Lang, ErrText)}
                end
            end
        end
    end.


items_with_role(BRole, StateData) ->
    lists:map(
      fun({_, U}) ->
          user_to_item(U, StateData)
      end, search_role(BRole, StateData)).

items_with_affiliation(BAffiliation, StateData) ->
    lists:map(
      fun({JID, {Affiliation, Reason}}) ->
          #xmlel{name = <<"item">>,
                 attrs = [{<<"affiliation">>, affiliation_to_binary(Affiliation)},
                      {<<"jid">>, jlib:jid_to_binary(JID)}],
                 children = [#xmlel{name = <<"reason">>,
                                    children = [#xmlcdata{content = Reason}]}]};
         ({JID, Affiliation}) ->
              #xmlel{name = <<"item">>,
                     attrs = [{<<"affiliation">>, affiliation_to_binary(Affiliation)},
                          {<<"jid">>, jlib:jid_to_binary(JID)}]}
      end, search_affiliation(BAffiliation, StateData)).

user_to_item(#user{role = Role,
           nick = Nick,
           jid = JID
          }, StateData) ->
    Affiliation = get_affiliation(JID, StateData),
    #xmlel{name = <<"item">>,
           attrs = [{<<"role">>, role_to_binary(Role)},
                    {<<"affiliation">>, affiliation_to_binary(Affiliation)},
                    {<<"nick">>, Nick},
                    {<<"jid">>, jlib:jid_to_binary(JID)}]}.

search_role(Role, StateData) ->
    lists:filter(
      fun({_, #user{role = R}}) ->
          Role == R
      end, ?DICT:to_list(StateData#state.users)).

search_affiliation(Affiliation, StateData) when is_atom(Affiliation) ->
    lists:filter(
      fun({_, A}) ->
          case A of
          {A1, _Reason} ->
              Affiliation == A1;
          _ ->
              Affiliation == A
          end
      end, ?DICT:to_list(StateData#state.affiliations)).


process_admin_items_set(UJID, Items, Lang, StateData) ->
    UAffiliation = get_affiliation(UJID, StateData),
    URole = get_role(UJID, StateData),
    case find_changed_items(UJID, UAffiliation, URole, Items, Lang, StateData, []) of
    {result, Res} ->
        ?INFO_MSG("Processing MUC admin query from ~s in room ~s:~n ~p",
              [jlib:jid_to_binary(UJID), jlib:jid_to_binary(StateData#state.jid), Res]),
        NSD =
        lists:foldl(
          fun(E, SD) ->
              case catch (
                 case E of
                     {JID, affiliation, owner, _} 
                     when (JID#jid.luser == <<>>) ->
                     %% If the provided JID does not have username,
                     %% forget the affiliation completely
                     SD;
                     {JID, role, none, Reason} ->
                     catch send_kickban_presence(
                         JID, Reason, <<"307">>, SD),
                     set_role(JID, none, SD);
                     {JID, affiliation, none, Reason} ->
                     case (SD#state.config)#config.members_only of
                         true ->
                         catch send_kickban_presence(
                             JID, Reason, <<"321">>, none, SD),
                         SD1 = set_affiliation(JID, none, SD),
                         set_role(JID, none, SD1);
                         _ ->
                         SD1 = set_affiliation(JID, none, SD),
                         send_update_presence(JID, Reason, SD1),
                         SD1
                     end;
                     {JID, affiliation, outcast, Reason} ->
                     catch send_kickban_presence(
                         JID, Reason, <<"301">>, outcast, SD),
                     set_affiliation_and_reason(
                       JID, outcast, Reason,
                       set_role(JID, none, SD));
                     {JID, affiliation, A, Reason} when
                       (A == admin) or (A == owner) ->
                     SD1 = set_affiliation_and_reason(JID, A, Reason, SD),
                     SD2 = set_role(JID, moderator, SD1),
                     send_update_presence(JID, Reason, SD2),
                     SD2;
                     {JID, affiliation, member, Reason} ->
                        case (SD#state.config)#config.members_only of
                            true -> send_invitation(UJID, JID, Reason, SD);
                            _ -> ok
                         end,
                         SD1 = set_affiliation_and_reason(
                             JID, member, Reason, SD),
                         SD2 = set_role(JID, participant, SD1),
                         send_update_presence(JID, Reason, SD2),
                         SD2;
                     {JID, role, Role, Reason} ->
                     SD1 = set_role(JID, Role, SD),
                     catch send_new_presence(JID, Reason, SD1),
                     SD1;
                     {JID, affiliation, A, Reason} ->
                     SD1 = set_affiliation(JID, A, SD),
                     send_update_presence(JID, Reason, SD1),
                     SD1
                       end
                ) of
                  {'EXIT', ErrReason} ->
                  ?ERROR_MSG("MUC ITEMS SET ERR: ~p~n",
                         [ErrReason]),
                  SD;
                  NSD ->
                  NSD
              end
          end, StateData, Res),
        case (NSD#state.config)#config.persistent of
        true ->
            mod_muc:store_room(NSD#state.host, NSD#state.room,
                       make_opts(NSD));
        _ ->
            ok
        end,
        {result, [], NSD};
    Err ->
        Err
    end.


find_changed_items(_UJID, _UAffiliation, _URole, [], _Lang, _StateData, Res) ->
    {result, Res};
find_changed_items(UJID, UAffiliation, URole, [#xmlcdata{} | Items],
           Lang, StateData, Res) ->
    find_changed_items(UJID, UAffiliation, URole, Items, Lang, StateData, Res);
find_changed_items(UJID, UAffiliation, URole,
           [#xmlel{name = <<"item">>, attrs = Attrs} = Item | Items],
           Lang, StateData, Res) ->
    TJID = case xml:get_attr(<<"jid">>, Attrs) of
           {value, S} ->
           case jlib:binary_to_jid(S) of
               error ->
               ErrText = <<(translate:translate(Lang, <<"Jabber ID ">>))/binary,
                  S/binary, (translate:translate(Lang, <<" is invalid">>))/binary>>,
               {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)};
               J ->
               {value, J}
           end;
           _ ->
           case xml:get_attr(<<"nick">>, Attrs) of
               {value, N} ->
               case find_jid_by_nick(N, StateData) of
                   false ->
                   ErrText = <<(translate:translate(Lang, <<"Nickname ">>))/binary,
                      N/binary, (translate:translate(Lang, <<" does not exist in the room">>))/binary>>,
                   {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)};
                   J ->
                   {value, J}
               end;
               _ ->
               {error, ?ERR_BAD_REQUEST}
           end
       end,
    case TJID of
    {value, JID} ->
        TAffiliation = get_affiliation(JID, StateData),
        TRole = get_role(JID, StateData),
        case xml:get_attr(<<"role">>, Attrs) of
        false ->
            case xml:get_attr(<<"affiliation">>, Attrs) of
            false ->
                {error, ?ERR_BAD_REQUEST};
            {value, BAffiliation} ->
                case catch binary_to_affiliation(BAffiliation) of
                {'EXIT', _} ->
                    ErrText1 = <<(translate:translate(Lang, <<"Invalid affiliation ">>))/binary,
                        BAffiliation/binary>>,
                    {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText1)};
                Affiliation ->
                    ServiceAf = get_service_affiliation(JID, StateData),
                    CanChangeRA =
                    case can_change_ra(
                           UAffiliation, URole,
                           TAffiliation, TRole,
                           affiliation, Affiliation,
                           ServiceAf) of
                        nothing ->
                        nothing;
                        true ->
                        true;
                        cancel ->
                        cancel;
                        check_owner ->
                        case search_affiliation(owner, StateData) of
                            [{OJID, _}] ->
                            jlib:jid_remove_resource(OJID) /=
                                jlib:jid_tolower(jlib:jid_remove_resource(UJID));
                            _ ->
                            true
                        end;
                        _ ->
                        false
                    end,
                    case CanChangeRA of
                    nothing ->
                        find_changed_items(
                          UJID,
                          UAffiliation, URole,
                          Items, Lang, StateData,
                          Res);
                    true ->
                        find_changed_items(
                          UJID,
                          UAffiliation, URole,
                          Items, Lang, StateData,
                          [{jlib:jid_remove_resource(JID),
                        affiliation, Affiliation, decode_reason(Item)} | Res]);
                    cancel ->
                        {error, ?ERR_NOT_ALLOWED};
                    false ->
                        {error, ?ERR_FORBIDDEN}
                    end
                end
            end;
        {value, BRole} ->
            case catch binary_to_role(BRole) of
            {'EXIT', _} ->
                ErrText1 = <<(translate:translate(Lang, <<"Invalid role ">>))/binary,
                    BRole/binary>>,
                {error, ?ERRT_BAD_REQUEST(Lang, ErrText1)};
            Role ->
                ServiceAf = get_service_affiliation(JID, StateData),
                CanChangeRA =
                case can_change_ra(
                       UAffiliation, URole,
                       TAffiliation, TRole,
                       role, Role,
                       ServiceAf) of
                    nothing ->
                    nothing;
                    true ->
                    true;
                    check_owner ->
                    case search_affiliation(owner, StateData) of
                        [{OJID, _}] ->
                        jlib:jid_remove_resource(OJID) /=
                            jlib:jid_tolower(jlib:jid_remove_resource(UJID));
                        _ ->
                        true
                    end;
                    _ ->
                    false
                end,
                case CanChangeRA of
                nothing ->
                    find_changed_items(
                      UJID,
                      UAffiliation, URole,
                      Items, Lang, StateData,
                      Res);
                true ->
                    find_changed_items(
                      UJID,
                      UAffiliation, URole,
                      Items, Lang, StateData,
                      [{JID, role, Role, decode_reason(Item)} | Res]);
                _ ->
                    {error, ?ERR_NOT_ALLOWED}
                end
            end
        end;
    Err ->
        Err
    end;
find_changed_items(_UJID, _UAffiliation, _URole, _Items,
           _Lang, _StateData, _Res) ->
    {error, ?ERR_BAD_REQUEST}.


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


send_kickban_presence(JID, Reason, Code, StateData) ->
    NewAffiliation = get_affiliation(JID, StateData),
    send_kickban_presence(JID, Reason, Code, NewAffiliation, StateData).

send_kickban_presence(JID, Reason, Code, NewAffiliation, StateData) ->
    foreach_matched_user(fun(#user{nick = Nick, jid = J}) ->
      add_to_log(kickban, {Nick, Reason, Code}, StateData),
      tab_remove_online_user(J, StateData),
      send_kickban_presence1(J, Reason, Code, NewAffiliation, StateData)
    end, JID, StateData).

send_kickban_presence1(UJID, Reason, Code, Affiliation, StateData) ->
    {ok, #user{jid = RealJID,
           nick = Nick}} =
    ?DICT:find(jlib:jid_tolower(UJID), StateData#state.users),
    BAffiliation = affiliation_to_binary(Affiliation),
    BannedJIDString = jlib:jid_to_binary(RealJID),
    lists:foreach(
      fun({_LJID, Info}) ->
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
        jlib:jid_replace_resource(StateData#state.jid, Nick),
        Info#user.jid,
        Packet)
      end, ?DICT:to_list(StateData#state.users)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Owner stuff

process_iq_owner(From, set, Lang, SubEl, StateData) ->
    
    FAffiliation = get_affiliation(From, StateData),
    case FAffiliation of
    owner ->
        #xmlel{children = Els} = SubEl,
        case xml:remove_cdata(Els) of
        [#xmlel{name = <<"x">>, children = Els1} = XEl] ->
            case {xml:get_tag_attr_s(<<"xmlns">>, XEl),
              xml:get_tag_attr_s(<<"type">>, XEl)} of
            {?NS_XDATA, <<"cancel">>} ->
                ?INFO_MSG("Destroyed MUC room ~s by the owner ~s : cancelled", 
                    [jlib:jid_to_binary(StateData#state.jid), jlib:jid_to_binary(From)]),
                add_to_log(room_existence, destroyed, StateData),
                destroy_room(XEl, StateData);
            {?NS_XDATA, <<"submit">>} ->
                case Els1 of
                [] ->
                    %confrm an instant room
                    {result, [], StateData};
                _ ->
                    %attepmt to configure
                    case is_allowed_log_change(XEl, StateData, From)
                    andalso
                    is_allowed_persistent_change(XEl, StateData, From)
                    andalso
                    is_allowed_room_name_desc_limits(XEl, StateData)
                    andalso
                    is_password_settings_correct(XEl, StateData) of
                    true -> set_config(XEl, StateData);
                    false -> {error, ?ERR_NOT_ACCEPTABLE}
                    end
                end;
            _ ->
                {error, ?ERR_BAD_REQUEST}
            end;
        [#xmlel{name = <<"destroy">>} = SubEl1] ->
            ?INFO_MSG("Destroyed MUC room ~s by the owner ~s", 
                  [jlib:jid_to_binary(StateData#state.jid), jlib:jid_to_binary(From)]),
            add_to_log(room_existence, destroyed, StateData),
            destroy_room(SubEl1, StateData);
        Items ->
            process_admin_items_set(From, Items, Lang, StateData)
        end;
    _ ->
        ErrText = <<"Owner privileges required">>,
        {error, ?ERRT_FORBIDDEN(Lang, ErrText)}
    end;

process_iq_owner(From, get, Lang, SubEl, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    case FAffiliation of
    owner ->
        #xmlel{children = Els} = SubEl,
        case xml:remove_cdata(Els) of
        [] ->
            get_config(Lang, StateData, From);
        [Item] ->
            case xml:get_tag_attr(<<"affiliation">>, Item) of
            false ->
                {error, ?ERR_BAD_REQUEST};
            {value, BAffiliation} ->
                case catch binary_to_affiliation(BAffiliation) of
                {'EXIT', _} ->
                    ErrText = <<(translate:translate(Lang, <<"Invalid affiliation ">>))/binary,
                        BAffiliation/binary>>,
                    {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)};
                Affiliation ->
                    Items = items_with_affiliation(Affiliation, StateData),
                    {result, Items, StateData}
                end
            end;
        _ ->
            {error, ?ERR_FEATURE_NOT_IMPLEMENTED}
        end;
    _ ->
        ErrText = <<"Owner privileges required">>,
        {error, ?ERRT_FORBIDDEN(Lang, ErrText)}
    end.

is_allowed_log_change(XEl, StateData, From) ->
    case lists:keymember(<<"muc#roomconfig_enablelogging">>, 1,
             jlib:parse_xdata_submit(XEl)) of
    false ->
        true;
    true ->
        (allow == mod_muc_log:check_access_log(
          StateData#state.server_host, From))
    end.

is_allowed_persistent_change(XEl, StateData, From) ->
    case lists:keymember(<<"muc#roomconfig_persistentroom">>, 1,
             jlib:parse_xdata_submit(XEl)) of
    false ->
        true;
    true ->
        AccessPersistent = access_persistent(StateData),
        (allow == acl:match_rule(StateData#state.server_host, AccessPersistent, From))
    end.

%% Check if the Room Name and Room Description defined in the Data Form
%% are conformant to the configured limits
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

%% Return false if:
%% <<"the password for a password-protected room is blank">>
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
    {_, true , <<>>, undefined} ->
        false;
    {_, true, _, <<>>} ->
        false;
    _ ->
        true
    end.


get_default_room_maxusers(RoomState) ->
    DefRoomOpts = gen_mod:get_module_opt(RoomState#state.server_host, mod_muc, default_room_options, []),
    RoomState2 = set_opts(DefRoomOpts, RoomState),
    (RoomState2#state.config)#config.max_users.

get_config(Lang, StateData, From) ->
    AccessPersistent = access_persistent(StateData),
    ServiceMaxUsers = get_service_max_users(StateData),
    DefaultRoomMaxUsers = get_default_room_maxusers(StateData),
    Config = StateData#state.config,
    {MaxUsersRoomInteger, MaxUsersRoomString} =
    case get_max_users(StateData) of
        N when is_integer(N) ->
        {N, erlang:integer_to_list(N)};
        _ -> {0, <<"none">>}
    end,
    Res =
    [#xmlel{name = <<"title">>,
            children = [#xmlcdata{content = <<(translate:translate(Lang, <<"Configuration of room ">>))/binary,
                                    (jlib:jid_to_binary(StateData#state.jid))/binary>>}]},
     #xmlel{name = <<"field">>,
            attrs = [{<<"type">>, <<"hidden">>},
          {<<"var">>, <<"FORM_TYPE">>}],
            children = [#xmlel{name = <<"value">>,
                               children = [#xmlcdata{content = <<"http://jabber.org/protocol/muc#roomconfig">>}]}]},
     ?STRINGXFIELD(<<"Room title">>,
               <<"muc#roomconfig_roomname">>,
               (Config#config.title)),
     ?STRINGXFIELD(<<"Room description">>,
               <<"muc#roomconfig_roomdesc">>,
               (Config#config.description))
    ] ++
     case acl:match_rule(StateData#state.server_host, AccessPersistent, From) of
        allow ->
            [?BOOLXFIELD(
             <<"Make room persistent">>,
             <<"muc#roomconfig_persistentroom">>,
             (Config#config.persistent))];
        _ -> []
     end ++ [
     ?BOOLXFIELD(<<"Make room public searchable">>,
             <<"muc#roomconfig_publicroom">>,
             (Config#config.public)),
     ?BOOLXFIELD(<<"Make participants list public">>,
             <<"public_list">>,
             (Config#config.public_list)),
     ?BOOLXFIELD(<<"Make room password protected">>,
             <<"muc#roomconfig_passwordprotectedroom">>,
             (Config#config.password_protected)),
     ?PRIVATEXFIELD(<<"Password">>,
            <<"muc#roomconfig_roomsecret">>,
            case Config#config.password_protected of
                true -> Config#config.password;
                false -> <<>>
            end),
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
                               attrs = [{<<"label">>, erlang:integer_to_list(N)}],
                               children = [#xmlel{name = <<"value">>,
                                                  children = [#xmlcdata{content = erlang:integer_to_list(N)}]}]} ||
                                                                      N <- lists:usort([ServiceMaxUsers, DefaultRoomMaxUsers, MaxUsersRoomInteger |
                                                                               ?MAX_USERS_DEFAULT_LIST]), N =< ServiceMaxUsers]},
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
                        #xmlel{name = <<"option">>,
                               attrs = [{<<"label">>, translate:translate(Lang, <<"moderators only">>)}],
                               children = [#xmlel{name = <<"value">>,
                                                  children = [#xmlcdata{content = <<"moderators">>}]}]},
                        #xmlel{name = <<"option">>,
                               attrs = [{<<"label">>, translate:translate(Lang, <<"anyone">>)}],
                               children = [#xmlel{name = <<"value">>,
                                                  children = [#xmlcdata{content = <<"anyone">>}]}]}]},
     ?BOOLXFIELD(<<"Make room members-only">>,
             <<"muc#roomconfig_membersonly">>,
             (Config#config.members_only)),
     ?BOOLXFIELD(<<"Make room moderated">>,
             <<"muc#roomconfig_moderatedroom">>,
             (Config#config.moderated)),
     ?BOOLXFIELD(<<"Default users as participants">>,
             <<"members_by_default">>,
             (Config#config.members_by_default)),
     ?BOOLXFIELD(<<"Allow users to change the subject">>,
             <<"muc#roomconfig_changesubject">>,
             (Config#config.allow_change_subj)),
     ?BOOLXFIELD(<<"Allow users to send private messages">>,
             <<"allow_private_messages">>,
             (Config#config.allow_private_messages)),
     ?BOOLXFIELD(<<"Allow users to query other users">>,
             <<"allow_query_users">>,
             (Config#config.allow_query_users)),
     ?BOOLXFIELD(<<"Allow users to send invites">>,
             <<"muc#roomconfig_allowinvites">>,
             (Config#config.allow_user_invites)),
     ?BOOLXFIELD(<<"Allow visitors to send status text in presence updates">>,
             <<"muc#roomconfig_allowvisitorstatus">>,
             (Config#config.allow_visitor_status)),
     ?BOOLXFIELD(<<"Allow visitors to change nickname">>,
             <<"muc#roomconfig_allowvisitornickchange">>,
             (Config#config.allow_visitor_nickchange))
    ] ++
    case mod_muc_log:check_access_log(
           StateData#state.server_host, From) of
        allow ->
        [?BOOLXFIELD(
            <<"Enable logging">>,
            <<"muc#roomconfig_enablelogging">>,
            (Config#config.logging))];
        _ -> []
    end,
    {result, [#xmlel{name = <<"instructions">>,
                     children = [#xmlcdata{content = translate:translate(
                                                       Lang, <<"You need an x:data capable client to configure room">>)}]},
          #xmlel{name = <<"x">>,
                 attrs = [{<<"xmlns">>, ?NS_XDATA},
               {<<"type">>, <<"form">>}],
                 children = Res}],
     StateData}.



set_config(XEl, StateData) ->
    XData = jlib:parse_xdata_submit(XEl),
    case XData of
    invalid ->
        {error, ?ERR_BAD_REQUEST};
    _ ->
        case set_xoption(XData, StateData#state.config) of
        #config{} = Config ->
            Res = change_config(Config, StateData),
            {result, _, NSD} = Res,
            Type = case {(StateData#state.config)#config.logging,
                 Config#config.logging} of
                   {true, false} ->
                                  send_config_update(logging_disabled, StateData),
                      roomconfig_change_disabledlogging;
                   {false, true} ->
                                  send_config_update(logging_enabled, StateData),
                      roomconfig_change_enabledlogging;
                               {_, _} -> case {(StateData#state.config)#config.anonymous,
                                    Config#config.anonymous} of
                      {true, false} ->
                                      send_config_update(nonanonymous, StateData),
                                      roomconfig_change_nonanonymous;
                                  {false, true} ->
                                      send_config_update(semianonymous, StateData),
                                      roomconfig_change_anonymous;
                                  {_, _} ->
                      roomconfig_change
                                end
               end,
            Users = [{U#user.jid, U#user.nick, U#user.role} ||
                {_, U} <- ?DICT:to_list(StateData#state.users)],
            add_to_log(Type, Users, NSD),
            Res;
        Err ->
            Err
        end
    end.

-define(SET_BOOL_XOPT(Opt, Val),
    case Val of
        <<"0">> -> set_xoption(Opts, Config#config{Opt = false});
        <<"false">> -> set_xoption(Opts, Config#config{Opt = false});
        <<"1">> -> set_xoption(Opts, Config#config{Opt = true});
        <<"true">> -> set_xoption(Opts, Config#config{Opt = true});
        _ -> {error, ?ERR_BAD_REQUEST}
    end).

-define(SET_NAT_XOPT(Opt, Val),
    case catch binary_to_integer(Val) of
        I when is_integer(I),
               I > 0 ->
        set_xoption(Opts, Config#config{Opt = I});
        _ ->
        {error, ?ERR_BAD_REQUEST}
    end).

-define(SET_STRING_XOPT(Opt, Val),
    set_xoption(Opts, Config#config{Opt = Val})).

-define(SET_JIDMULTI_XOPT(Opt, Vals),
        begin
            Set = lists:foldl(
                    fun({U, S, R}, Set1) ->
                            ?SETS:add_element({U, S, R}, Set1);
                       (#jid{luser = U, lserver = S, lresource = R}, Set1) ->
                            ?SETS:add_element({U, S, R}, Set1);
                       (_, Set1) ->
                            Set1
                    end, ?SETS:empty(), Vals),
            set_xoption(Opts, Config#config{Opt = Set})
        end).

set_xoption([], Config) ->
    Config;
set_xoption([{<<"muc#roomconfig_roomname">>, [Val]} | Opts], Config) ->
    ?SET_STRING_XOPT(title, Val);
set_xoption([{<<"muc#roomconfig_roomdesc">>, [Val]} | Opts], Config) ->
    ?SET_STRING_XOPT(description, Val);
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
set_xoption([{<<"muc#roomconfig_passwordprotectedroom">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(password_protected, Val);
set_xoption([{<<"muc#roomconfig_roomsecret">>, [Val]} | Opts], Config) ->
    ?SET_STRING_XOPT(password, Val);
set_xoption([{<<"anonymous">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(anonymous, Val);
set_xoption([{<<"muc#roomconfig_whois">>, [Val]} | Opts], Config) ->
    case Val of
    <<"moderators">> ->
        ?SET_BOOL_XOPT(anonymous, <<"1">>);
    <<"anyone">> ->
        ?SET_BOOL_XOPT(anonymous, <<"0">>);
    _ ->
        {error, ?ERR_BAD_REQUEST}
    end;
set_xoption([{<<"muc#roomconfig_maxusers">>, [Val]} | Opts], Config) ->
    case Val of
    <<"none">> ->
        ?SET_STRING_XOPT(max_users, none);
    _ ->
        ?SET_NAT_XOPT(max_users, Val)
    end;
set_xoption([{<<"muc#roomconfig_enablelogging">>, [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(logging, Val);
set_xoption([{<<"FORM_TYPE">>, _} | Opts], Config) ->
    %% Ignore our FORM_TYPE
    set_xoption(Opts, Config);
set_xoption([_ | _Opts], _Config) ->
    {error, ?ERR_BAD_REQUEST}.


change_config(Config, StateData) ->
    NSD = StateData#state{config = Config},
    case {(StateData#state.config)#config.persistent,
      Config#config.persistent} of
    {_, true} ->
        mod_muc:store_room(NSD#state.host, NSD#state.room, make_opts(NSD));
    {true, false} ->
        mod_muc:forget_room(NSD#state.host, NSD#state.room);
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

remove_nonmembers(StateData) ->
    lists:foldl(
      fun({_LJID, #user{jid = JID}}, SD) ->
        Affiliation = get_affiliation(JID, SD),
        case Affiliation of
        none ->
            catch send_kickban_presence(
                JID, <<>>, <<"322">>, SD),
            set_role(JID, none, SD);
        _ ->
            SD
        end
      end, StateData, ?DICT:to_list(StateData#state.users)).


-spec set_opts(Opts, StateData) -> StateData when
    Opts :: [{atom(), term()}],
    StateData :: #state{}.
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
        affiliations ->
            SD#state{affiliations = ?DICT:from_list(Val)};
        subject ->
            SD#state{subject = Val};
        subject_author ->
            SD#state{subject_author = Val};
        _ ->
            SD
       end,
    set_opts(Opts, NSD).

-define(MAKE_CONFIG_OPT(Opt), {Opt, Config#config.Opt}).

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
     ?MAKE_CONFIG_OPT(password_protected),
     ?MAKE_CONFIG_OPT(password),
     ?MAKE_CONFIG_OPT(anonymous),
     ?MAKE_CONFIG_OPT(logging),
     ?MAKE_CONFIG_OPT(max_users),
     {affiliations, ?DICT:to_list(StateData#state.affiliations)},
     {subject, StateData#state.subject},
     {subject_author, StateData#state.subject_author}
    ].



destroy_room(DestroyEl, StateData) ->
    remove_each_occupant_from_room(DestroyEl, StateData),
    case (StateData#state.config)#config.persistent of
    true ->
        mod_muc:forget_room(StateData#state.host, StateData#state.room);
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
remove_each_occupant_from_room(DestroyEl, StateData) ->
    Packet = presence_stanza_of_type_unavailable(DestroyEl),
    send_to_occupants(Packet, StateData).

send_to_occupants(Packet, StateData=#state{jid=RoomJID}) ->
    F = fun(User=#user{jid=UserJID}) ->
        ejabberd_router:route(occupant_jid(User, RoomJID), UserJID, Packet)
        end,
    foreach_user(F, StateData).

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

-define(FEATURE(Var), #xmlel{name = <<"feature">>,
                             attrs = [{<<"var">>, Var}]}).

-define(CONFIG_OPT_TO_FEATURE(Opt, Fiftrue, Fiffalse),
    case Opt of
    true ->
        ?FEATURE(Fiftrue);
    false ->
        ?FEATURE(Fiffalse)
    end).

process_iq_disco_info(_From, set, _Lang, _StateData) ->
    {error, ?ERR_NOT_ALLOWED};

process_iq_disco_info(_From, get, Lang, StateData) ->
    Config = StateData#state.config,
    {result, [#xmlel{name = <<"identity">>,
                     attrs = [{<<"category">>, <<"conference">>},
                          {<<"type">>, <<"text">>},
                          {<<"name">>, get_title(StateData)}]},
          #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_MUC}]},
          ?CONFIG_OPT_TO_FEATURE((Config#config.public),
                     <<"muc_public">>, <<"muc_hidden">>),
          ?CONFIG_OPT_TO_FEATURE((Config#config.persistent),
                     <<"muc_persistent">>, <<"muc_temporary">>),
          ?CONFIG_OPT_TO_FEATURE((Config#config.members_only),
                     <<"muc_membersonly">>, <<"muc_open">>),
          ?CONFIG_OPT_TO_FEATURE((Config#config.anonymous),
                     <<"muc_semianonymous">>, <<"muc_nonanonymous">>),
          ?CONFIG_OPT_TO_FEATURE((Config#config.moderated),
                     <<"muc_moderated">>, <<"muc_unmoderated">>),
          ?CONFIG_OPT_TO_FEATURE((Config#config.password_protected),
                     <<"muc_passwordprotected">>, <<"muc_unsecured">>)
         ] ++ iq_disco_info_extras(Lang, StateData), StateData}.

-define(RFIELDT(Type, Var, Val),
    #xmlel{name = <<"field">>,
           attrs = [{<<"type">>, Type}, {<<"var">>, Var}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = Val}]}]}).

-define(RFIELD(Label, Var, Val),
    #xmlel{name = <<"field">>,
           attrs = [{<<"label">>, translate:translate(Lang, Label)},
             {<<"var">>, Var}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = Val}]}]}).

iq_disco_info_extras(Lang, StateData) ->
    Len = length(?DICT:to_list(StateData#state.users)),
    RoomDescription = (StateData#state.config)#config.description,
    [#xmlel{name = <<"x">>,
            attrs = [{<<"xmlns">>, ?NS_XDATA}, {<<"type">>, <<"result">>}],
            children = [?RFIELDT(<<"hidden">>, <<"FORM_TYPE">>,
                         <<"http://jabber.org/protocol/muc#roominfo">>),
                        ?RFIELD(<<"Room description">>, <<"muc#roominfo_description">>,
                            RoomDescription),
                        ?RFIELD(<<"Number of occupants">>, <<"muc#roominfo_occupants">>,
                            (integer_to_list(Len)))
                       ]}].

process_iq_disco_items(_From, set, _Lang, _StateData) ->
    {error, ?ERR_NOT_ALLOWED};

process_iq_disco_items(From, get, _Lang, StateData) ->
    case (StateData#state.config)#config.public_list of
    true ->
        {result, get_mucroom_disco_items(StateData), StateData};
    _ ->
        case is_occupant_or_admin(From, StateData) of
        true ->
            {result, get_mucroom_disco_items(StateData), StateData};
        _ ->
            {error, ?ERR_FORBIDDEN}
        end
    end.

get_title(StateData) ->
    case (StateData#state.config)#config.title of
    <<>> ->
        StateData#state.room;
    Name ->
        Name
    end.

get_roomdesc_reply(JID, StateData, Tail) ->
    IsOccupantOrAdmin = is_occupant_or_admin(JID, StateData),
    if (StateData#state.config)#config.public or IsOccupantOrAdmin ->
        if (StateData#state.config)#config.public_list or IsOccupantOrAdmin ->
                    Title = get_title(StateData),
            {item, <<Title/binary, Tail/binary>>};
           true ->
            {item, get_title(StateData)}
        end;
       true ->
        false
    end.

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


get_mucroom_disco_items(StateData=#state{jid=RoomJID}) ->
    [disco_item(User, RoomJID)
     || {_LJID, User} <- ?DICT:to_list(StateData#state.users)].

disco_item(User=#user{nick=Nick}, RoomJID) ->
    #xmlel{
        name = <<"item">>,
        attrs = [{<<"jid">>, jlib:jid_to_binary(occupant_jid(User, RoomJID))},
                 {<<"name">>, Nick}]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle voice request or approval (XEP-0045 7.13, 8.6)
-spec check_voice_approval(From, Els, Lang, StateData) -> Res when
    From :: jid(),
    Els :: [#xmlel{}],
    Lang :: binary(),
    StateData :: #state{},
    Res :: {form, BRole} | {role, BRole, RoomNick} | {error, Reason} | ok,
    BRole :: binary(),
    RoomNick :: binary(),
    Reason :: term().
check_voice_approval(From, [#xmlel{name = <<"x">>,
                                   children = Items}], _Lang, StateData) ->
    BRole = get_field(<<"muc#role">>, Items),
    case Items of
    [_Form, _Role] ->
        case catch binary_to_role(BRole) of
        {'EXIT', _} -> {error, ?ERR_BAD_REQUEST};
        _ -> {form, BRole}
        end;
    _ ->
        case get_role(From, StateData) of
        moderator ->
            case get_field(<<"muc#request_allow">>, Items) of
            <<"true">> ->
                case get_field(<<"muc#roomnick">>, Items) of
                false -> {error, ?ERR_BAD_REQUEST};
                RoomNick -> {role, BRole, RoomNick}
                end;
            _ -> ok
            end;
        _ -> {error, ?ERR_NOT_ALLOWED}
        end
    end.

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

check_invitation(FromJID, Els, Lang, StateData) ->
    try
        unsave_check_invitation(FromJID, Els, Lang, StateData)
    catch throw:{error, Reason} -> {error, Reason}
    end.

unsave_check_invitation(FromJID, Els, Lang, StateData=#state{jid=RoomJID}) ->
    FAffiliation = get_affiliation(FromJID, StateData),
    CanInvite = (StateData#state.config)#config.allow_user_invites
         orelse (FAffiliation == admin)
         orelse (FAffiliation == owner),
    case CanInvite of
    false ->
        throw({error, ?ERR_FORBIDDEN});
    true ->
        InviteEl = find_invite_elem(Els),
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
            attrs = [{<<"from">>, jlib:jid_to_binary(FromJID)}],
            children = [ReasonEl] ++ ContinueEl},
        PasswdEl = create_password_elem(StateData),
        BodyEl = invite_body_elem(FromJID, Reason, Lang, StateData),
        Msg = create_invite_message_elem(
            OutInviteEl, BodyEl, PasswdEl, RoomJID, Reason),
        ejabberd_router:route(StateData#state.jid, JID, Msg),
        {ok, JID}
    end.

-spec decode_destination_jid(InviteEl) -> JID when
    InviteEl :: #xmlel{},
    JID :: #jid{}.
decode_destination_jid(InviteEl) ->
    case jlib:binary_to_jid(xml:get_tag_attr_s(<<"to">>, InviteEl)) of
      error -> throw({error, ?ERR_JID_MALFORMED});
      JID   -> JID
    end.

-spec find_invite_elem([#xmlel{}]) -> ok | #xmlel{}.
find_invite_elem(Els) ->
    case xml:remove_cdata(Els) of
    [#xmlel{name = <<"x">>, children = Els1} = XEl] ->
        case xml:get_tag_attr_s(<<"xmlns">>, XEl) of
        ?NS_MUC_USER ->
            ok;
        _ ->
            throw({error, ?ERR_BAD_REQUEST})
        end,

        case xml:remove_cdata(Els1) of
        [#xmlel{name = <<"invite">>} = InviteEl1] ->
            InviteEl1;
        _ ->
            throw({error, ?ERR_BAD_REQUEST})
        end;
    _ ->
        throw({error, ?ERR_BAD_REQUEST})
    end.

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

invite_body_elem(FromJID, Reason, Lang, StateData) ->
    Text = invite_body_text(FromJID, Reason, Lang, StateData),
    #xmlel{
        name = <<"body">>,
        children = [#xmlcdata{content = Text}]}.

invite_body_text(FromJID, Reason, Lang,
        #state{
            jid=RoomJID,
            config=#config{
                password_protected=IsProtected,
                password=Password}}) ->
    BFromJID = jlib:jid_to_binary(FromJID),
    BRoomJID = jlib:jid_to_binary(RoomJID),
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

create_invite_message_elem(InviteEl, BodyEl, PasswdEl, RoomJID, Reason)
    when is_list(PasswdEl), is_binary(Reason) ->
    BRoomJID = jlib:jid_to_binary(RoomJID),
    UserXEl = #xmlel{
        name = <<"x">>,
        attrs = [{<<"xmlns">>, ?NS_MUC_USER}],
        children = [InviteEl|PasswdEl]},
    ConfXEl = #xmlel{
        name = <<"x">>,
        attrs = [{<<"xmlns">>, ?NS_XCONFERENCE}, {<<"jid">>, BRoomJID}],
        children = [#xmlcdata{content = Reason}]},
    #xmlel{
        name = <<"message">>,
        attrs = [{<<"type">>, <<"normal">>}],
        children = [UserXEl, ConfXEl, BodyEl]}.


%% Handle a message sent to the room by a non-participant.
%% If it is a decline, send to the inviter.
%% Otherwise, an error message is sent to the sender.
handle_roommessage_from_nonparticipant(Packet, Lang, StateData, From) ->
    case catch check_decline_invitation(Packet) of
    {true, Decline_data} ->
        send_decline_invitation(Decline_data, StateData#state.jid, From);
    _ ->
        send_error_only_occupants(Packet, Lang, StateData#state.jid, From)
    end.

%% Check in the packet is a decline.
%% If so, also returns the splitted packet.
%% This function must be catched, 
%% because it crashes when the packet is not a decline message.
check_decline_invitation(Packet) ->
    #xmlel{name = <<"message">>} = Packet,
    XEl = xml:get_subtag(Packet, <<"x">>),
    ?NS_MUC_USER = xml:get_tag_attr_s(<<"xmlns">>, XEl),
    DEl = xml:get_subtag(XEl, <<"decline">>),
    ToString = xml:get_tag_attr_s(<<"to">>, DEl),
    ToJID = jlib:binary_to_jid(ToString),
    {true, {Packet, XEl, DEl, ToJID}}.

%% Send the decline to the inviter user.
%% The original stanza must be slightly modified.
send_decline_invitation({Packet, XEl, DEl, ToJID}, RoomJID, FromJID) ->
    FromString = jlib:jid_to_binary(FromJID),
    #xmlel{name = <<"decline">>, attrs = DAttrs, children = DEls} = DEl,
    DAttrs2 = lists:keydelete(<<"to">>, 1, DAttrs),
    DAttrs3 = [{<<"from">>, FromString} | DAttrs2],
    DEl2 = #xmlel{name = <<"decline">>, attrs = DAttrs3, children = DEls},
    XEl2 = replace_subelement(XEl, DEl2),
    Packet2 = replace_subelement(Packet, XEl2),
    ejabberd_router:route(RoomJID, ToJID, Packet2).

%% Given an element and a new subelement, 
%% replace the instance of the subelement in element with the new subelement.
replace_subelement(XE = #xmlel{children = SubEls}, NewSubEl) ->
    {_, NameNewSubEl, _, _} = NewSubEl,
    SubEls2 = lists:keyreplace(NameNewSubEl, 2, SubEls, NewSubEl),
    XE#xmlel{children = SubEls2}.

send_error_only_occupants(Packet, Lang, RoomJID, From) ->
    ErrText = <<"Only occupants are allowed to send messages to the conference">>,
    Err = jlib:make_error_reply(Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
    ejabberd_router:route(RoomJID, From, Err).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logging

add_to_log(Type, Data, StateData)
  when Type == roomconfig_change_disabledlogging ->
    %% When logging is disabled, the config change message must be logged:
    mod_muc_log:add_to_log(
      StateData#state.server_host, roomconfig_change, Data,
      StateData#state.jid, make_opts(StateData));
add_to_log(Type, Data, StateData) ->
    case (StateData#state.config)#config.logging of
    true ->
        mod_muc_log:add_to_log(
          StateData#state.server_host, Type, Data,
          StateData#state.jid, make_opts(StateData));
    false ->
        ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Users number checking

tab_add_online_user(JID, StateData) ->
    {LUser, LServer, _} = jlib:jid_tolower(JID),
    US = {LUser, LServer},
    Room = StateData#state.room,
    Host = StateData#state.host,
    catch ets:insert(
        muc_online_users,
        #muc_online_users{us = US, room = Room, host = Host}).


tab_remove_online_user(JID, StateData) ->
    {LUser, LServer, _} = jlib:jid_tolower(JID),
    US = {LUser, LServer},
    Room = StateData#state.room,
    Host = StateData#state.host,
    catch ets:delete_object(
        muc_online_users,
        #muc_online_users{us = US, room = Room, host = Host}).

tab_count_user(JID) ->
    {LUser, LServer, _} = jlib:jid_tolower(JID),
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
    size(xml:element_to_binary(El)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Routing functions

route_message(#routed_message{allowed = true, type = <<"groupchat">>,
    from = From, packet = Packet, lang = Lang}, StateData) ->
    Activity = get_user_activity(From, StateData),
    Now = now_to_usec(now()),
    MinMessageInterval = trunc(gen_mod:get_module_opt(
        StateData#state.server_host,
        mod_muc, min_message_interval, 0) * 1000000),
    Size = element_size(Packet),
    {MessageShaper, MessageShaperInterval} =
        shaper:update(Activity#activity.message_shaper, Size),
    if
        Activity#activity.message /= undefined ->
            ErrText = <<"Traffic rate limit is exceeded">>,
            Err = jlib:make_error_reply(
                Packet, ?ERRT_RESOURCE_CONSTRAINT(Lang, ErrText)),
            ejabberd_router:route(
                StateData#state.jid,
                From, Err),
            StateData;
        Now >= Activity#activity.message_time + MinMessageInterval,
            MessageShaperInterval == 0 ->
            {RoomShaper, RoomShaperInterval} =
                shaper:update(StateData#state.room_shaper, Size),
            RoomQueueEmpty = queue:is_empty(
                StateData#state.room_queue),
            if
                RoomShaperInterval == 0, RoomQueueEmpty ->
                    NewActivity = Activity#activity{
                            message_time = Now,
                            message_shaper = MessageShaper},
                    StateData1 = store_user_activity(
                        From, NewActivity, StateData),
                    StateData2 =
                        StateData1#state{room_shaper = RoomShaper},
                    {next_state, normal_state, StateData3} =
                        process_groupchat_message(From, Packet, StateData2),
                    StateData3;
                true ->
                    StateData1 =
                    if
                        RoomQueueEmpty ->
                            erlang:send_after(RoomShaperInterval, self(),
                                process_room_queue),
                        StateData#state{room_shaper = RoomShaper};
                        true ->
                            StateData
                    end,
                    NewActivity = Activity#activity{
                        message_time = Now,
                        message_shaper = MessageShaper,
                        message = Packet},
                    RoomQueue = queue:in(
                        {message, From},
                        StateData#state.room_queue),
                    StateData2 = store_user_activity(
                        From, NewActivity, StateData1),
                    StateData3 = StateData2#state{room_queue = RoomQueue},
                    StateData3
            end;
        true ->
            MessageInterval =
                (Activity#activity.message_time +
                MinMessageInterval - Now) div 1000,
                Interval = lists:max([MessageInterval,MessageShaperInterval]),
                erlang:send_after(
                    Interval, self(), {process_user_message, From}),
                NewActivity = Activity#activity{
                        message = Packet,
                        message_shaper = MessageShaper},
                StateData1 = store_user_activity(From, NewActivity, StateData),
                StateData1
    end;

route_message(#routed_message{allowed = true, type = <<"error">>, from = From,
    packet = Packet, lang = Lang}, StateData) ->
    case is_user_online(From, StateData) of
        true ->
            ErrorText = <<"This participant is kicked from the room because he sent an error message">>,
            NewState = expulse_participant(Packet, From, StateData,
                translate:translate(Lang, ErrorText)),
            NewState;
        _ ->
            StateData
    end;

route_message(#routed_message{allowed = true, type = <<"chat">>, from = From, packet = Packet,
    lang = Lang}, StateData) ->
    ErrText = <<"It is not allowed to send private messages to the conference">>,
    Err = jlib:make_error_reply(
        Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
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
    Err = jlib:make_error_reply(
        Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
    ejabberd_router:route(
              StateData#state.jid,
              From, Err),
    StateData;

route_message(#routed_message{type = <<"error">>}, StateData) ->
    StateData;

route_message(#routed_message{from = From, packet = Packet, lang = Lang},
    StateData) ->
    handle_roommessage_from_nonparticipant(Packet, Lang, StateData, From),
    StateData.

route_error(Nick, From, Error, StateData) ->
    ejabberd_router:route(
        % TODO: s/Nick/<<>>/
        jlib:jid_replace_resource(StateData#state.jid, Nick),
        From, Error),
    StateData.

route_voice_approval({error, ErrType}, From, Packet, _Lang, StateData) ->
    ejabberd_router:route(StateData#state.jid, From,
        jlib:make_error_reply(Packet, ErrType)),
    StateData;

route_voice_approval({form, RoleName}, From, _Packet, _Lang, StateData) ->
    {Nick, _} = get_participant_data(From, StateData),
    lists:foreach(fun({_, Info}) ->
        ejabberd_router:route(StateData#state.jid, Info#user.jid,
            jlib:make_voice_approval_form(From, Nick, RoleName))
    end, search_role(moderator, StateData)),
    StateData;

route_voice_approval({role, BRole, Nick}, From, Packet, Lang, StateData) ->
    case process_admin_items_set(From,
        [#xmlel{name = <<"item">>,
                attrs = [{<<"role">>, BRole}, {<<"nick">>, Nick}]}],
              Lang, StateData) of
        {result, _Res, SD1} -> SD1;
        {error, Error} ->
            ejabberd_router:route(StateData#state.jid, From,
                jlib:make_error_reply(Packet, Error)),
            StateData
    end;

route_voice_approval(_Type, From, Packet, _Lang, StateData) ->
    ejabberd_router:route(StateData#state.jid, From,
        jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST)),
    StateData.

route_invitation({error, Error}, From, Packet, _Lang, StateData) ->
    Err = jlib:make_error_reply(Packet, Error),
    ejabberd_router:route(StateData#state.jid, From, Err),
    StateData;

route_invitation({ok, IJID}, _From, _Packet, _Lang, StateData) ->
    Config = StateData#state.config,
    case Config#config.members_only of
        true ->
             case get_affiliation(IJID, StateData) of
                none ->
                    NSD = set_affiliation(
                        IJID,
                        member,
                        StateData),
                     case (NSD#state.config)#config.persistent of
                        true ->
                            mod_muc:store_room(
                                NSD#state.host,
                                NSD#state.room,
                                make_opts(NSD));
                        _ ->
                             ok
                    end,
                    NSD;
                _ ->
                    StateData
            end;
        false ->
            StateData
    end.

route_iq(#routed_iq{iq = #iq{type = Type, xmlns = ?NS_MUC_ADMIN, lang = Lang,
    sub_el = SubEl}, from = From} = Routed, StateData) ->
    Res = process_iq_admin(From, Type, Lang, SubEl, StateData),
    do_route_iq(Res, Routed, StateData);

route_iq(#routed_iq{iq = #iq{type = Type, xmlns = ?NS_MUC_OWNER, lang = Lang,
    sub_el = SubEl}, from = From} = Routed, StateData) ->
    Res = process_iq_owner(From, Type, Lang, SubEl, StateData),
    do_route_iq(Res, Routed, StateData);

route_iq(#routed_iq{iq = #iq{type = Type, xmlns = ?NS_DISCO_INFO, lang = Lang},
    from = From} = Routed, StateData) ->
    Res = process_iq_disco_info(From, Type, Lang, StateData),
    do_route_iq(Res, Routed, StateData);

route_iq(#routed_iq{iq = #iq{type = Type, xmlns = ?NS_DISCO_ITEMS, lang = Lang},
    from = From} = Routed, StateData) ->
    Res = process_iq_disco_items(From, Type, Lang, StateData),
    do_route_iq(Res, Routed, StateData);

route_iq(#routed_iq{iq = IQ = #iq{}, packet = Packet, from = From},
         #state{host = Host, jid = RoomJID} = StateData) ->
    %% Custom IQ, addressed to this room's JID.
    case mod_muc_iq:process_iq(Host, From, RoomJID, IQ) of
        ignore -> ok;
        error ->
            Err = jlib:make_error_reply(Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
            ejabberd_router:route(RoomJID, From, Err);
        ResIQ ->
            ejabberd_router:route(RoomJID, From, jlib:iq_to_xml(ResIQ))
    end,
    StateData;

route_iq(#routed_iq{iq = reply}, StateData) ->
    StateData;

route_iq(#routed_iq{packet = Packet, from = From}, StateData) ->
    Err = jlib:make_error_reply(
        Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
    ejabberd_router:route(StateData#state.jid, From, Err),
    StateData.

do_route_iq(Res1, #routed_iq{iq = #iq{xmlns = XMLNS, sub_el = SubEl} = IQ,
    from = From}, StateData) ->
    {IQRes, NewStateData} = case Res1 of
        {result, Res, SD} ->
            {IQ#iq{type = result,
                sub_el = [#xmlel{name = <<"query">>,
                                 attrs = [{<<"xmlns">>, XMLNS}],
                                 children = Res}]},
            SD};
        {error, Error} ->
            {IQ#iq{type = error,
                sub_el = [SubEl, Error]},
            StateData}
    end,
    ejabberd_router:route(StateData#state.jid, From,
        jlib:iq_to_xml(IQRes)),
    NewStateData.

route_nick_message(#routed_nick_message{decide = {expulse_sender, Reason},
    packet = Packet, lang = Lang, from = From}, StateData) ->
    ?DEBUG(Reason, []),
    ErrorText = <<"This participant is kicked from the room because he",
        "sent an error message to another participant">>,
    expulse_participant(Packet, From, StateData,
        translate:translate(Lang, ErrorText));

route_nick_message(#routed_nick_message{decide = forget_message}, StateData) ->
    StateData;

route_nick_message(#routed_nick_message{decide = continue_delivery, allow_pm = true,
    online = true, packet = Packet, from = From, type = <<"groupchat">>,
    lang = Lang, nick = ToNick}, StateData) ->
    ErrText = <<"It is not allowed to send private messages of type groupchat">>,
    Err = jlib:make_error_reply(
        Packet, ?ERRT_BAD_REQUEST(Lang, ErrText)),
    ejabberd_router:route(
        jlib:jid_replace_resource(
            StateData#state.jid,
            ToNick),
        From, Err),
    StateData;

route_nick_message(#routed_nick_message{decide = continue_delivery, allow_pm = true,
    online = true, packet = Packet, from = From,
    lang = Lang, nick = ToNick, jid = false}, StateData) ->
    ErrText = <<"Recipient is not in the conference room">>,
    Err = jlib:make_error_reply(
        Packet, ?ERRT_ITEM_NOT_FOUND(Lang, ErrText)),
    ejabberd_router:route(
        jlib:jid_replace_resource(
            StateData#state.jid,
            ToNick),
        From, Err),
    StateData;

route_nick_message(#routed_nick_message{decide = continue_delivery, allow_pm = true,
    online = true, packet = Packet, from = From, jid = ToJID}, StateData) ->
    {ok, #user{nick = FromNick}} = ?DICT:find(jlib:jid_tolower(From),
        StateData#state.users),
    ejabberd_router:route(
        jlib:jid_replace_resource(StateData#state.jid, FromNick), ToJID, Packet),
    StateData;

route_nick_message(#routed_nick_message{decide = continue_delivery, allow_pm = true,
    online = false, packet = Packet, from = From,
    lang = Lang, nick = ToNick}, StateData) ->
    ErrText = <<"Only occupants are allowed to send messages to the conference">>,
    Err = jlib:make_error_reply(
        Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
    ejabberd_router:route(
        jlib:jid_replace_resource(StateData#state.jid, ToNick),
        From, Err),
    StateData;

route_nick_message(#routed_nick_message{decide = continue_delivery, allow_pm = false,
    packet = Packet, from = From,
    lang = Lang, nick = ToNick}, StateData) ->
    ErrText = <<"It is not allowed to send private messages">>,
    Err = jlib:make_error_reply(
        Packet, ?ERRT_FORBIDDEN(Lang, ErrText)),
    ejabberd_router:route(
        jlib:jid_replace_resource(StateData#state.jid, ToNick), From, Err),
    StateData.

route_nick_iq(#routed_nick_iq{allow_query = true, online = {true, _, _}, jid = false,
    iq = reply}, _StateData) ->
    ok;

route_nick_iq(#routed_nick_iq{allow_query = true, online = {true, _, _}, jid = false,
    packet = Packet, lang = Lang, from = From, nick = ToNick}, StateData) ->
    ErrText = <<"Recipient is not in the conference room">>,
    Err = jlib:make_error_reply(
        Packet, ?ERRT_ITEM_NOT_FOUND(Lang, ErrText)),
    ejabberd_router:route(
        jlib:jid_replace_resource(
            StateData#state.jid, ToNick),
        From, Err);

route_nick_iq(#routed_nick_iq{allow_query = true, online = {true, NewId, FromFull},
    jid = ToJID, packet = Packet, stanza = StanzaId}, StateData) ->
    {ok, #user{nick = FromNick}} = ?DICT:find(jlib:jid_tolower(FromFull),
        StateData#state.users),
    {ToJID2, Packet2} = handle_iq_vcard(FromFull, ToJID,
        StanzaId, NewId,Packet),
    ejabberd_router:route(
        jlib:jid_replace_resource(StateData#state.jid, FromNick),
        ToJID2, Packet2);

route_nick_iq(#routed_nick_iq{online = {false, _, _}, iq = reply}, _StateData) ->
    ok;

route_nick_iq(#routed_nick_iq{online = {false, _, _}, from = From, nick = ToNick,
    packet = Packet, lang = Lang}, StateData) ->
    ErrText = <<"Only occupants are allowed to send queries to the conference">>,
    Err = jlib:make_error_reply(
        Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
    ejabberd_router:route(
      jlib:jid_replace_resource(StateData#state.jid, ToNick),
    From, Err);

route_nick_iq(#routed_nick_iq{iq = reply}, _StateData) ->
    ok;

route_nick_iq(#routed_nick_iq{packet = Packet, lang = Lang, nick = ToNick,
    from = From}, StateData) ->
    ErrText = <<"Queries to the conference members are not allowed in this room">>,
    Err = jlib:make_error_reply(
        Packet, ?ERRT_NOT_ALLOWED(Lang, ErrText)),
    ejabberd_router:route(
        jlib:jid_replace_resource(StateData#state.jid, ToNick),
    From, Err).


decode_reason(Elem) ->
    xml:get_path_s(Elem, [{elem, <<"reason">>}, cdata]).
