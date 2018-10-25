%%%----------------------------------------------------------------------
%%% File    : mod_muc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC support (XEP-0045)
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

-module(mod_muc).
-author('alexey@process-one.net').
-xep([{xep, 45}, {version, "1.25"}]).
-behaviour(gen_server).
-behaviour(gen_mod).
-behaviour(mongoose_packet_handler).

%% API
-export([start_link/2,
         start/2,
         stop/1,
         room_destroyed/3,
         store_room/4,
         restore_room/3,
         forget_room/3,
         create_instant_room/5,
         process_iq_disco_items/4,
         broadcast_service_message/2,
         can_use_nick/4,
         room_jid_to_pid/1,
         default_host/0]).

%% For testing purposes only
-export([load_permanent_rooms/6, register_room/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% packet handler callback
-export([process_packet/5]).

%% Hooks handlers
-export([is_room_owner/3,
         muc_room_pid/2,
         can_access_room/3,
         can_access_identity/3]).

%% Stats
-export([online_rooms_number/0]).
-export([hibernated_rooms_number/0]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("mongoose_rsm.hrl").

-export_type([access/0,
             room/0,
             nick/0,
             packet/0,
             role/0,
             affiliation/0
            ]).

-type role() :: moderator | participant | visitor | none.
-type affiliation() :: admin | owner | member | outcast | none.
-type room() :: binary().
-type nick() :: binary().
-type room_host() :: jid:simple_bare_jid().
-type packet() :: exml:element().
-type from_to_packet() ::
        {From :: jid:jid(), To :: jid:jid(), Acc :: mongoose_acc:t(),
         Packet :: packet()}.
-type access() :: {_AccessRoute, _AccessCreate, _AccessAdmin, _AccessPersistent}.

-include("mod_muc.hrl").

-type muc_room() :: #muc_room{
    name_host    :: room_host(),
    opts         :: list()
}.

-type muc_online_room() :: #muc_online_room{
    name_host :: room_host(),
    pid       :: pid()
}.

-type muc_registered() :: #muc_registered{
                             us_host    :: jid:literal_jid(),
                             nick       :: nick()
                            }.

-record(state, {host                :: jid:server(),
                server_host         :: jid:literal_jid(),
                access,
                history_size        :: integer(),
                default_room_opts   :: list(),
                room_shaper         :: shaper:shaper(),
                http_auth_pool      :: mongoose_http_client:pool(),
                hibernated_room_check_interval :: timeout(),
                hibernated_room_timeout :: timeout()
              }).

-type state() :: #state{}.

-export_type([muc_room/0, muc_registered/0]).

-define(PROCNAME, ejabberd_mod_muc).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok, Pid} | ignore | {error, Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec start_link(jid:server(), list())
            -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(Host, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:start_link({local, Proc}, ?MODULE, [Host, Opts], []).


-spec start(jid:server(), _) ->
    {'error', _} | {'ok', 'undefined' | pid()} | {'ok', 'undefined' | pid(), _}.
start(Host, Opts) ->
    ensure_metrics(Host),
    TrackedDBFuns = [store_room, restore_room, forget_room, get_rooms,
                     can_use_nick, get_nick, set_nick, unset_nick],
    gen_mod:start_backend_module(mod_muc_db, Opts, TrackedDBFuns),
    start_supervisor(Host),
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    ChildSpec =
        {Proc,
         {?MODULE, start_link, [Host, Opts]},
         temporary,
         1000,
         worker,
         [?MODULE]},
    ejabberd_sup:start_child(ChildSpec).

-spec stop(jid:server()) -> 'ok'
    | {'error', 'not_found' | 'restarting' | 'running' | 'simple_one_for_one'}.
stop(Host) ->
    stop_supervisor(Host),
    stop_gen_server(Host).

stop_gen_server(Host) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, stop),
    %% Proc can still be alive because of a race condition
    ejabberd_sup:stop_child(Proc).


%% @doc This function is called by a room in three situations:
%% A) The owner of the room destroyed it
%% B) The only participant of a temporary room leaves it
%% C) mod_muc:stop was called, and each room is being terminated
%%    In this case, the mod_muc process died before the room processes
%%    So the message sending must be catched
-spec room_destroyed(jid:server(), room(), pid()) -> 'ok'.
room_destroyed(Host, Room, Pid) ->
    F = fun() -> mnesia:delete_object(#muc_online_room{name_host = {Room, Host}, pid = Pid}) end,
    {atomic, ok} = mnesia:transaction(F),
    ok.


%% @doc Create a room.
%% If Opts = default, the default room options are used.
%% Else use the passed options as defined in mod_muc_room.
-spec create_instant_room(jid:server(), Name :: room(),
    From :: jid:jid(), Nick :: nick(), Opts :: list()) -> any().
create_instant_room(Host, Name, From, Nick, Opts) ->
    Proc = gen_mod:get_module_proc(Host, ?PROCNAME),
    gen_server:call(Proc, {create_instant, Name, From, Nick, Opts}).


-spec store_room(jid:server(), jid:server(), room(), list()) ->
    {error, _} | ok.
store_room(ServerHost, Host, Name, Opts) ->
    mod_muc_db_backend:store_room(ServerHost, Host, Name, Opts).


-spec restore_room(jid:server(), jid:server(), room()) ->
        {error, _} | {ok, _}.
restore_room(ServerHost, Host, Name) ->
    mod_muc_db_backend:restore_room(ServerHost, Host, Name).

-spec forget_room(jid:server(), jid:server(), room()) -> ok | {error, term()}.
forget_room(ServerHost, Host, Name) ->
    %% Removes room from DB, even if it's already removed.
    Result = mod_muc_db_backend:forget_room(ServerHost, Host, Name),
    case Result of
        ok ->
            %% TODO this hook should be refactored to be executed on ServerHost, not Host.
            %% It also should be renamed to forget_room_hook.
            %% We also need to think how to remove stopped rooms
            %% (i.e. in case we want to expose room removal over REST or SQS).
            %%
            %% In some _rare_ cases this hook can be called more than once for the same room.
            ejabberd_hooks:run(forget_room, Host, [Host, Name]);
        _ ->
            %% Room is not removed or we don't know.
            %% XXX Handle this case better.
            ok
    end,
    Result.

-spec process_iq_disco_items(Host :: jid:server(), From :: jid:jid(),
        To :: jid:jid(), jlib:iq()) -> mongoose_acc:t().
process_iq_disco_items(Host, From, To, #iq{lang = Lang} = IQ) ->
    Rsm = jlib:rsm_decode(IQ),
    Res = IQ#iq{type = result,
                sub_el = [#xmlel{name = <<"query">>,
                                 attrs = [{<<"xmlns">>, ?NS_DISCO_ITEMS}],
                                 children = iq_disco_items(Host, From, Lang, Rsm)}]},
    ejabberd_router:route(To,
                          From,
                          jlib:iq_to_xml(Res)).


-spec can_use_nick(jid:server(), jid:server(), jid:jid(), nick()) -> boolean().
can_use_nick(_ServerHost, _Host, _JID, <<>>) ->
    false;
can_use_nick(ServerHost, Host, JID, Nick) ->
    mod_muc_db_backend:can_use_nick(ServerHost, Host, JID, Nick).

set_nick(LServer, Host, From, <<>>) ->
    {error, should_not_be_empty};
set_nick(LServer, Host, From, Nick) ->
    mod_muc_db_backend:set_nick(LServer, Host, From, Nick).

unset_nick(LServer, Host, From) ->
    mod_muc_db_backend:unset_nick(LServer, Host, From).

get_nick(LServer, Host, From) ->
    mod_muc_db_backend:get_nick(LServer, Host, From).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
-spec init([jid:server() | list(), ...]) -> {'ok', state()}.
init([Host, Opts]) ->
    mod_muc_db_backend:init(Host, Opts),
    mnesia:create_table(muc_online_room,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, muc_online_room)}]),
    mnesia:add_table_copy(muc_online_room, node(), ram_copies),
    catch ets:new(muc_online_users, [bag, named_table, public, {keypos, 2}]),
    MyHost = gen_mod:get_opt_subhost(Host, Opts, default_host()),
    clean_table_from_bad_node(node(), MyHost),
    mnesia:subscribe(system),
    Access = gen_mod:get_opt(access, Opts, all),
    AccessCreate = gen_mod:get_opt(access_create, Opts, all),
    AccessAdmin = gen_mod:get_opt(access_admin, Opts, none),
    AccessPersistent = gen_mod:get_opt(access_persistent, Opts, all),
    HttpAuthPool = gen_mod:get_opt(http_auth_pool, Opts, none),
    HistorySize = gen_mod:get_opt(history_size, Opts, 20),
    DefRoomOpts = gen_mod:get_opt(default_room_options, Opts, []),
    RoomShaper = gen_mod:get_opt(room_shaper, Opts, none),
    CheckInterval = gen_mod:get_opt(hibernated_room_check_interval, Opts, infinity),
    HibernatedTimeout = gen_mod:get_opt(hibernated_room_timeout, Opts, infinity),
    State = #state{host = MyHost,
                   server_host = Host,
                   access = {Access, AccessCreate, AccessAdmin, AccessPersistent},
                   default_room_opts = DefRoomOpts,
                   history_size = HistorySize,
                   room_shaper = RoomShaper,
                   http_auth_pool = HttpAuthPool,
                   hibernated_room_check_interval = CheckInterval,
                   hibernated_room_timeout = HibernatedTimeout},

    ejabberd_hooks:add(is_muc_room_owner, MyHost, ?MODULE, is_room_owner, 50),
    ejabberd_hooks:add(muc_room_pid, MyHost, ?MODULE, muc_room_pid, 50),
    ejabberd_hooks:add(can_access_room, MyHost, ?MODULE, can_access_room, 50),
    ejabberd_hooks:add(can_access_identity, MyHost, ?MODULE, can_access_identity, 50),

    ejabberd_router:register_route(MyHost, mongoose_packet_handler:new(?MODULE, State)),
    mongoose_subhosts:register(Host, MyHost),

    case gen_mod:get_module_opt(Host, mod_muc, load_permanent_rooms_at_startup, false) of
        false ->
            ?INFO_MSG("event=load_permanent_rooms_at_startup, skip=true, "
                      "details=\"each room is loaded when someone access the room\"", []);
        true ->
            ?INFO_MSG("event=load_permanent_rooms_at_startup, skip=false, "
                      "details=\"it can take some time\"", []),
            load_permanent_rooms(MyHost, Host,
                                 {Access, AccessCreate, AccessAdmin, AccessPersistent},
                                 HistorySize, RoomShaper, HttpAuthPool)
    end,
    set_persistent_rooms_timer(State),
    {ok, State}.

set_persistent_rooms_timer(#state{hibernated_room_check_interval = infinity}) ->
    ok;
set_persistent_rooms_timer(#state{hibernated_room_check_interval = Timeout}) ->
    timer:send_after(Timeout, stop_hibernated_persistent_rooms).

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    ejabberd_hooks:delete(is_muc_room_owner, State#state.host, ?MODULE, is_room_owner, 50),
    ejabberd_hooks:delete(muc_room_pid, State#state.host, ?MODULE, muc_room_pid, 50),
    ejabberd_hooks:delete(can_access_room, State#state.host, ?MODULE, can_access_room, 50),
    ejabberd_hooks:delete(can_access_identity, State#state.host, ?MODULE, can_access_identity, 50),

    {stop, normal, ok, State};

handle_call({create_instant, Room, From, Nick, Opts},
            _From,
            #state{host = Host,
                   server_host = ServerHost,
                   access = Access,
                   default_room_opts = DefOpts,
                   history_size = HistorySize,
                   room_shaper = RoomShaper,
                   http_auth_pool = HttpAuthPool} = State) ->
    ?DEBUG("MUC: create new room '~s'~n", [Room]),
    NewOpts = case Opts of
                  default -> DefOpts;
                  _ -> Opts
              end,
    {ok, Pid} = mod_muc_room:start(
                  Host, ServerHost, Access,
                  Room, HistorySize,
                  RoomShaper, HttpAuthPool, From,
          Nick, [{instant, true}|NewOpts]),
    register_room_or_stop_if_duplicate(Host, Room, Pid),
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info({mnesia_system_event, {mnesia_down, Node}}, State) ->
    clean_table_from_bad_node(Node),
    {noreply, State};
handle_info(stop_hibernated_persistent_rooms,
            #state{server_host = ServerHost,
                   hibernated_room_timeout = Timeout} = State) when is_integer(Timeout) ->
    ?INFO_MSG("Closing hibernated persistent rooms", []),
    Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_muc_sup),
    Now = os:timestamp(),
    [stop_if_hibernated(Pid, Now, Timeout * 1000) ||
     {undefined, Pid, worker, _} <- supervisor:which_children(Supervisor)],

    set_persistent_rooms_timer(State),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

stop_if_hibernated(Pid, Now, Timeout) ->
    stop_if_hibernated(Pid, Now, Timeout, erlang:process_info(Pid, current_function)).

stop_if_hibernated(Pid, Now, Timeout, {current_function, {erlang, hibernate, 3}}) ->
    {dictionary, Dictionary} = erlang:process_info(Pid, dictionary),
    LastHibernated = lists:keyfind(hibernated, 1, Dictionary),
    stop_if_hibernated_for_specified_time(Pid, Now, Timeout, LastHibernated),
    ok;
stop_if_hibernated(_, _, _, _) ->
    ok.

stop_if_hibernated_for_specified_time(_Pid, _, _, false) ->
    ok;
stop_if_hibernated_for_specified_time(Pid, Now, Timeout, {hibernated, LastHibernated}) ->
    TimeDiff = timer:now_diff(Now, LastHibernated),
    case TimeDiff >= Timeout of
        true ->
            Pid ! stop_persistent_room_process;
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    mongoose_subhosts:unregister(State#state.host),
    ejabberd_router:unregister_route(State#state.host),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
-spec start_supervisor(jid:server()) -> {'error', _}
                                           | {'ok', 'undefined' | pid()}
                                           | {'ok', 'undefined' | pid(), _}.
start_supervisor(Host) ->
    Proc = gen_mod:get_module_proc(Host, ejabberd_mod_muc_sup),
    ChildSpec =
        {Proc,
         {ejabberd_tmp_sup, start_link,
          [Proc, mod_muc_room]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    ejabberd_sup:start_child(ChildSpec).


-spec stop_supervisor(jid:server()) -> 'ok'
    | {'error', 'not_found' | 'restarting' | 'running' | 'simple_one_for_one'}.
stop_supervisor(Host) ->
    Proc = gen_mod:get_module_proc(Host, ejabberd_mod_muc_sup),
    ejabberd_sup:stop_child(Proc).


-spec process_packet(Acc :: mongoose_acc:t(),
                     From :: jid:jid(),
                     To :: jid:simple_jid() | jid:jid(),
                     El :: exml:element(),
                     State :: state()) -> ok | mongoose_acc:t().
process_packet(Acc, From, To, El, #state{
                                    access = {AccessRoute, _, _, _},
                                    server_host = ServerHost} = State) ->
    case acl:match_rule(ServerHost, AccessRoute, From) of
        allow ->
            {Room, _, _} = jid:to_lower(To),
            route_to_room(Room, {From, To, Acc, El}, State);
        _ ->
            #xmlel{attrs = Attrs} = El,
            Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
            ErrText = <<"Access denied by service policy">>,
            ejabberd_router:route_error_reply(To, From, Acc,
                                              mongoose_xmpp_errors:forbidden(Lang, ErrText))
    end.


-spec route_to_room(room(), from_to_packet(), state()) -> 'ok' | pid().
route_to_room(<<>>, {_, To, _Acc, _} = Routed, State) ->
    {_, _, Nick} = jid:to_lower(To),
    route_by_nick(Nick, Routed, State);
route_to_room(Room, Routed, #state{host=Host} = State) ->
    case mnesia:dirty_read(muc_online_room, {Room, Host}) of
        [] ->
            case get_registered_room_or_route_error(Room, Routed, State) of
                {ok, Pid} ->
                    route_to_online_room(Pid, Routed);
                {route_error, _ErrText} ->
                    ok
            end;
        [R] ->
            Pid = R#muc_online_room.pid,
            route_to_online_room(Pid, Routed)
    end.

route_to_online_room(Pid, {From, To, Acc, Packet}) ->
    ?DEBUG("MUC: send to process ~p~n", [Pid]),
    {_, _, Nick} = jid:to_lower(To),
    ok = mod_muc_room:route(Pid, From, Nick, Acc, Packet).

-spec get_registered_room_or_route_error(room(), from_to_packet(), state()) -> 'ok' | {_, pid()}.
get_registered_room_or_route_error(Room, {From, To, Acc, Packet}, State) ->
    #xmlel{name = Name, attrs = Attrs} = Packet,
    Type = xml:get_attr_s(<<"type">>, Attrs),
    case {Name, Type} of
        {<<"presence">>, <<>>} ->
            get_registered_room_or_route_error_from_presence(Room, From, To, Acc, Packet, State);
        _ ->
            get_registered_room_or_route_error_from_packet(Room, From, To, Acc, Packet, State)
    end.

get_registered_room_or_route_error_from_presence(Room, From, To, Acc, Packet,
                                   #state{server_host = ServerHost,
                                          host = Host,
                                          access = Access} = State) ->
    {_, AccessCreate, _, _} = Access,
    case check_user_can_create_room(ServerHost, AccessCreate,
                                    From, Room) of
        true ->
            #state{history_size = HistorySize,
                   room_shaper = RoomShaper,
                   http_auth_pool = HttpAuthPool,
                   default_room_opts = DefRoomOpts} = State,
            {_, _, Nick} = jid:to_lower(To),
            Result = start_new_room(Host, ServerHost, Access, Room,
                                       HistorySize, RoomShaper, HttpAuthPool,
                                       From, Nick, DefRoomOpts),
            case Result of
                {ok, Pid} ->
                    register_room_or_stop_if_duplicate(Host, Room, Pid);
                {error, {failed_to_restore, Reason}} ->
                    %% Notify user about our backend module error
                    ?WARNING_MSG("event=send_service_unavailable room=~ts reason=~p",
                                 [Room, Reason]),
                    Lang = exml_query:attr(Packet, <<"xml:lang">>, <<>>),
                    ErrText = <<"Service is temporary unavailable">>,
                    {Acc1, Err} = jlib:make_error_reply(
                            Acc, Packet, mongoose_xmpp_errors:service_unavailable(Lang, ErrText)),
                    ejabberd_router:route(To, From, Acc1, Err),
                    {route_error, ErrText};
                _ ->
                    %% Unknown error, most likely a room process failed to start.
                    %% Do not notify user (we can send "internal server error").
                    erlang:error({start_new_room_failed, Room, Result})
            end;
        false ->
            Lang = exml_query:attr(Packet, <<"xml:lang">>, <<>>),
            ErrText = <<"Room creation is denied by service policy">>,
            {Acc1, Err} = jlib:make_error_reply(
                    Acc, Packet, mongoose_xmpp_errors:not_allowed(Lang, ErrText)),
            ejabberd_router:route(To, From, Acc1, Err),
            {route_error, ErrText}
    end.

get_registered_room_or_route_error_from_packet(Room, From, To, Acc, Packet,
                                 #state{server_host = ServerHost,
                                        host = Host,
                                        access = Access} = State) ->

    case restore_room(ServerHost, Host, Room) of
        {error, room_not_found} ->
            Lang = exml_query:attr(Packet, <<"xml:lang">>, <<>>),
            ErrText = <<"Conference room does not exist">>,
            {Acc1, Err} = jlib:make_error_reply(
                    Acc, Packet, mongoose_xmpp_errors:item_not_found(Lang, ErrText)),
            ejabberd_router:route(To, From, Acc1, Err),
            {route_error, ErrText};
        {error, Reason} ->
            ?WARNING_MSG("event=send_service_unavailable room=~ts reason=~p",
                         [Room, Reason]),
            Lang = exml_query:attr(Packet, <<"xml:lang">>, <<>>),
            ErrText = <<"Service is temporary unavailable">>,
            {Acc1, Err} = jlib:make_error_reply(
                    Acc, Packet, mongoose_xmpp_errors:service_unavailable(Lang, ErrText)),
            ejabberd_router:route(To, From, Acc1, Err),
            {route_error, ErrText};
        {ok, Opts} ->
            ?DEBUG("MUC: restore room '~s'~n", [Room]),
            #state{history_size = HistorySize,
                   room_shaper = RoomShaper,
                   http_auth_pool = HttpAuthPool} = State,
            {ok, Pid} = mod_muc_room:start(Host, ServerHost, Access,
                                           Room, HistorySize,
                                           RoomShaper, HttpAuthPool, Opts),
            register_room_or_stop_if_duplicate(Host, Room, Pid)
    end.

-spec route_by_nick(room(), from_to_packet(), state()) -> 'ok' | pid().
route_by_nick(<<>>, {_, _, _, Packet} = Routed, State) ->
    #xmlel{name = Name} = Packet,
    route_by_type(Name, Routed, State);
route_by_nick(_Nick, {From, To, Acc, Packet}, _State) ->
    #xmlel{attrs = Attrs} = Packet,
    case xml:get_attr_s(<<"type">>, Attrs) of
        <<"error">> ->
            ok;
        <<"result">> ->
            ok;
        _ ->
            {Acc1, Err} = jlib:make_error_reply(Acc, Packet, mongoose_xmpp_errors:item_not_found()),
            ejabberd_router:route(To, From, Acc1, Err)
    end.


-spec route_by_type(binary(), from_to_packet(), state()) -> 'ok' | pid().
route_by_type(<<"iq">>, {From, To, Acc, Packet}, #state{host = Host} = State) ->
    ServerHost = State#state.server_host,
    case jlib:iq_query_info(Packet) of
        #iq{type = get, xmlns = ?NS_DISCO_INFO = XMLNS, lang = Lang} = IQ ->
            Info = ejabberd_hooks:run_fold(disco_info, ServerHost, [],
                                           [ServerHost, ?MODULE, <<"">>, Lang]),
            Res = IQ#iq{type = result,
                        sub_el = [#xmlel{name = <<"query">>,
                                         attrs = [{<<"xmlns">>, XMLNS}],
                                         children = iq_disco_info(Lang) ++ Info}]},
            ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
        #iq{type = get, xmlns = ?NS_DISCO_ITEMS} = IQ ->
            spawn(?MODULE, process_iq_disco_items, [Host, From, To, IQ]);
        #iq{type = get, xmlns = ?NS_REGISTER = XMLNS, lang = Lang} = IQ ->
            Result = iq_get_register_info(ServerHost, Host, From, Lang),
            Res = IQ#iq{type = result,
                        sub_el = [#xmlel{name = <<"query">>,
                                         attrs = [{<<"xmlns">>, XMLNS}],
                                         children = Result}]},
            ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
        #iq{type = set,
            xmlns = ?NS_REGISTER = XMLNS,
            lang = Lang,
            sub_el = SubEl} = IQ ->
            case process_iq_register_set(ServerHost, Host, From, SubEl, Lang) of
                {result, IQRes} ->
                    Res = IQ#iq{type = result,
                                sub_el = [#xmlel{name = <<"query">>,
                                                 attrs = [{<<"xmlns">>, XMLNS}],
                                                 children = IQRes}]},
                    ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
                {error, Error} ->
                    {Acc1, Err} = jlib:make_error_reply(Acc, Packet, Error),
                    ejabberd_router:route(To, From, Acc1, Err)
            end;
        #iq{type = get, xmlns = ?NS_VCARD = XMLNS, lang = Lang} = IQ ->
            Res = IQ#iq{type = result,
                        sub_el = [#xmlel{name = <<"vCard">>,
                                         attrs = [{<<"xmlns">>, XMLNS}],
                                         children = iq_get_vcard(Lang)}]},
            ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
        #iq{type = get, xmlns = ?NS_MUC_UNIQUE} = IQ ->
           Res = IQ#iq{type = result,
                       sub_el = [#xmlel{name = <<"unique">>,
                                        attrs = [{<<"xmlns">>, ?NS_MUC_UNIQUE}],
                                        children = [iq_get_unique(From)]}]},
           ejabberd_router:route(To, From, jlib:iq_to_xml(Res));
        #iq{} ->
            ?INFO_MSG("event=ignore_unknown_iq from=~ts to=~ts packet=~1000p",
                      [jid:to_binary(From), jid:to_binary(To), exml:to_binary(Packet)]),
            {Acc1, Err} = jlib:make_error_reply(Acc, Packet, mongoose_xmpp_errors:feature_not_implemented()),
            ejabberd_router:route(To, From, Acc1, Err);
        _ ->
            ?INFO_MSG("event=failed_to_parse_iq from=~ts to=~ts packet=~1000p",
                      [jid:to_binary(From), jid:to_binary(To), exml:to_binary(Packet)]),
            ok
    end;
route_by_type(<<"message">>, {From, To, Acc, Packet},
              #state{host = Host, server_host = ServerHost,
                     access = {_, _, AccessAdmin, _}}) ->
    #xmlel{attrs = Attrs} = Packet,
    case xml:get_attr_s(<<"type">>, Attrs) of
        <<"error">> ->
            ok;
        _ ->
            case acl:match_rule(ServerHost, AccessAdmin, From) of
                allow ->
                    Msg = xml:get_path_s(Packet, [{elem, <<"body">>}, cdata]),
                    broadcast_service_message(Host, Msg);
                _ ->
                    Lang = xml:get_attr_s(<<"xml:lang">>, Attrs),
                    ErrTxt = <<"Only service administrators are allowed to send service messages">>,
                    Err = mongoose_xmpp_errors:forbidden(Lang, ErrTxt),
                    {Acc1, ErrorReply} = jlib:make_error_reply(Acc, Packet, Err),
                    ejabberd_router:route(To, From, Acc1, ErrorReply)
            end
    end;
route_by_type(<<"presence">>, _Routed, _State) ->
    ok.


-spec check_user_can_create_room('global' | jid:server(),
        'allow' | atom(), jid:jid(), room()) -> boolean().
check_user_can_create_room(ServerHost, AccessCreate, From, RoomID) ->
    case acl:match_rule(ServerHost, AccessCreate, From) of
        allow ->
            (size(RoomID) =< gen_mod:get_module_opt(ServerHost, mod_muc,
                                                    max_room_id, infinite));
        _ ->
            false
    end.

-spec load_permanent_rooms(Host :: jid:server(), Srv :: jid:server(),
        Access :: access(), HistorySize :: 'undefined' | integer(),
        RoomShaper :: shaper:shaper(), HttpAuthPool :: none | mongoose_http_client:pool()) -> 'ok'.
load_permanent_rooms(Host, ServerHost, Access, HistorySize, RoomShaper, HttpAuthPool) ->
    RoomsToLoad =
    case mod_muc_db_backend:get_rooms(ServerHost, Host) of
        {ok, Rs} ->
            Rs;
        {error, Reason} ->
            ?ERROR_MSG("event=get_rooms_failed event=skip_load_permanent_rooms reason=~p",
                       [Reason]),
            []
    end,
    lists:foreach(
      fun(R) ->
              {Room, Host} = R#muc_room.name_host,
              {ok, Pid} = mod_muc_room:start(
                  Host,
                  ServerHost,
                  Access,
                  Room,
                  HistorySize,
                  RoomShaper,
                  HttpAuthPool,
                  R#muc_room.opts),
          register_room_or_stop_if_duplicate(Host, Room, Pid)

      end, RoomsToLoad).

-spec start_new_room(Host :: 'undefined' | jid:server(),
        Srv :: jid:server(), Access :: access(), room(),
        HistorySize :: 'undefined' | integer(), RoomShaper :: shaper:shaper(),
        HttpAuthPool :: none | mongoose_http_client:pool(), From :: jid:jid(), nick(),
        DefRoomOpts :: 'undefined' | [any()])
            -> {'error', _}
             | {'ok', 'undefined' | pid()}
             | {'ok', 'undefined' | pid(), _}.
start_new_room(Host, ServerHost, Access, Room,
               HistorySize, RoomShaper, HttpAuthPool, From,
               Nick, DefRoomOpts) ->
    case mod_muc_db_backend:restore_room(ServerHost, Host, Room) of
        {error, room_not_found} ->
            ?DEBUG("MUC: open new room '~s'~n", [Room]),
            mod_muc_room:start(Host, ServerHost, Access,
                               Room, HistorySize,
                               RoomShaper, HttpAuthPool, From,
                               Nick, DefRoomOpts);
        {error, Reason} ->
            {error, {failed_to_restore, Reason}};
        {ok, Opts} ->
            ?DEBUG("MUC: restore room '~s'~n", [Room]),
            mod_muc_room:start(Host, ServerHost, Access,
                               Room, HistorySize,
                               RoomShaper, HttpAuthPool, Opts)
    end.

register_room_or_stop_if_duplicate(Host, Room, Pid) ->
    case register_room(Host, Room, Pid) of
        {_, ok} ->
            {ok, Pid};
        {_, {exists, OldPid}} ->
            mod_muc_room:stop(Pid),
            {ok, OldPid}
    end.

-spec register_room('undefined' | jid:server(), room(),
                    'undefined' | pid()) -> {'aborted', _} | {'atomic', _}.
register_room(Host, Room, Pid) ->
    F = fun() ->
            case mnesia:read(muc_online_room,  {Room, Host}, write) of
                [] ->
                    mnesia:write(#muc_online_room{name_host = {Room, Host}, pid = Pid});
                [R] ->
                    {exists, R#muc_online_room.pid}
            end
        end,
    mnesia:transaction(F).


-spec room_jid_to_pid(RoomJID :: jid:jid()) -> {ok, pid()} | {error, not_found}.
room_jid_to_pid(#jid{luser=RoomName, lserver=MucService}) ->
    case mnesia:dirty_read(muc_online_room, {RoomName, MucService}) of
        [R] ->
        {ok, R#muc_online_room.pid};
    [] ->
        {error, not_found}
    end.

-spec default_host() -> binary().
default_host() -> <<"conference.@HOST@">>.

-spec iq_disco_info(ejabberd:lang()) -> [exml:element(), ...].
iq_disco_info(Lang) ->
    [#xmlel{name = <<"identity">>,
            attrs = [{<<"category">>, <<"conference">>},
                     {<<"type">>, <<"text">>},
                     {<<"name">>, translate:translate(Lang, <<"Chatrooms">>)}]},
     #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_DISCO_INFO}]},
     #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_DISCO_ITEMS}]},
     #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_MUC}]},
     #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_MUC_UNIQUE}]},
     #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_REGISTER}]},
     #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_RSM}]},
     #xmlel{name = <<"feature">>, attrs = [{<<"var">>, ?NS_VCARD}]}].


-spec iq_disco_items(jid:server(), jid:jid(), ejabberd:lang(),
        Rsm :: none | jlib:rsm_in()) -> any().
iq_disco_items(Host, From, Lang, none) ->
    lists:zf(fun(#muc_online_room{name_host = {Name, _Host}, pid = Pid}) ->
                     case catch gen_fsm_compat:sync_send_all_state_event(
                                  Pid, {get_disco_item, From, Lang}, 100) of
                         {item, Desc} ->
                             flush(),
                             {true,
                              #xmlel{name = <<"item">>,
                                     attrs = [{<<"jid">>, jid:to_binary({Name, Host, <<>>})},
                                              {<<"name">>, Desc}]}};
                         _ ->
                             false
                     end
             end, get_vh_rooms(Host));
iq_disco_items(Host, From, Lang, Rsm) ->
    {Rooms, RsmO} = get_vh_rooms(Host, Rsm),
    RsmOut = jlib:rsm_encode(RsmO),
    lists:zf(fun(#muc_online_room{name_host = {Name, _Host}, pid = Pid}) ->
                     case catch gen_fsm_compat:sync_send_all_state_event(
                                  Pid, {get_disco_item, From, Lang}, 100) of
                         {item, Desc} ->
                             flush(),
                             {true,
                              #xmlel{name = <<"item">>,
                                     attrs = [{<<"jid">>, jid:to_binary({Name, Host, <<>>})},
                                              {<<"name">>, Desc}]}};
                         _ ->
                             false
                     end
             end, Rooms) ++ RsmOut.


-spec get_vh_rooms(jid:server(), jlib:rsm_in()) -> {list(), jlib:rsm_out()}.
get_vh_rooms(Host, #rsm_in{max=M, direction=Direction, id=I, index=Index}) ->
    AllRooms = lists:sort(get_vh_rooms(Host)),
    Count = erlang:length(AllRooms),
    Guard = case Direction of
                _ when Index =/= undefined ->
            [{'=:=', {element, 2, '$1'}, Host}];
                aft ->
            [{'=:=', {element, 2, '$1'}, Host},
             {'>',   {element, 1, '$1'}, I}]; %% not exact here
        before when I =/= <<>> ->
            [{'=:=', {element, 2, '$1'}, Host},
             {'<',   {element, 1, '$1'}, I}]; %% not exact here
                _ ->
            [{'=:=', {element, 2, '$1'}, Host}]
            end,
    L = lists:sort(
          mnesia:dirty_select(muc_online_room,
                              [{#muc_online_room{name_host = '$1', _ = '_'},
                                Guard,
                                ['$_']}])),
    L2 = case {Index, Direction} of
             {undefined, before} ->
                 lists:reverse(lists:sublist(lists:reverse(L), 1, M));
             {undefined, _} ->
                 lists:sublist(L, 1, M);
             {Index, _} when Index > Count orelse Index < 0 ->
                 [];
             _ ->
                 lists:sublist(L, Index+1, M)
         end,
    case L2 of
        [] ->
            {L2, #rsm_out{count=Count}};
        _ ->
            H = hd(L2),
            NewIndex = get_room_pos(H, AllRooms),
            T=lists:last(L2),
            {F, _} = H#muc_online_room.name_host,
            {Last, _} = T#muc_online_room.name_host,
            {L2, #rsm_out{first=F, last=Last, count=Count, index=NewIndex}}
    end.

%% @doc Return the position of desired room in the list of rooms.
%% The room must exist in the list. The count starts in 0.
-spec get_room_pos(muc_online_room(), [muc_online_room()]) -> integer().
get_room_pos(Desired, Rooms) ->
    get_room_pos(Desired, Rooms, 0).
get_room_pos(Desired, [HeadRoom | _], HeadPosition)
  when (Desired#muc_online_room.name_host ==
        HeadRoom#muc_online_room.name_host) ->
    HeadPosition;
get_room_pos(Desired, [_ | Rooms], HeadPosition) ->
    get_room_pos(Desired, Rooms, HeadPosition + 1).


-spec flush() -> 'ok'.
flush() ->
    receive
        _ ->
            flush()
    after 0 ->
            ok
    end.


-spec xfield(Type :: binary(), Label :: binary(), Var :: binary(),
             Val :: binary(), ejabberd:lang()) -> exml:element().
xfield(Type, Label, Var, Val, Lang) ->
    #xmlel{name = <<"field">>,
           attrs = [{<<"type">>, Type},
                     {<<"label">>, translate:translate(Lang, Label)},
                     {<<"var">>, Var}],
           children = [#xmlel{name = <<"value">>,
                              children = [#xmlcdata{content = Val}]}]}.


%% @doc Get a pseudo unique Room Name. The Room Name is generated as a hash of
%%      the requester JID, the local time and a random salt.
%%
%%      <<"pseudo">> because we don't verify that there is not a room
%%       with the returned Name already created, nor mark the generated Name
%%       as <<"already used">>.  But in practice, it is unique enough. See
%%       http://xmpp.org/extensions/xep-0045.html#createroom-unique
-spec iq_get_unique(jid:jid()) -> jlib:xmlcdata().
iq_get_unique(From) ->
        #xmlcdata{content = sha:sha1_hex(term_to_binary([From, p1_time_compat:unique_integer(),
                                                         mongoose_bin:gen_from_crypto()]))}.


-spec iq_get_register_info(jid:server(), jid:server(),
        jid:simple_jid() | jid:jid(), ejabberd:lang())
            -> [jlib:xmlel(), ...].
iq_get_register_info(ServerHost, Host, From, Lang) ->
    {Nick, Registered} =
        case catch get_nick(ServerHost, Host, From) of
            {'EXIT', _Reason} ->
                {<<>>, []};
            {error, _} ->
                {<<>>, []};
            {ok, N} ->
                {N, [#xmlel{name = <<"registered">>}]}
        end,
    ClientReqText = translate:translate(
                      Lang, <<"You need a client that supports x:data to register the nickname">>),
    ClientReqEl = #xmlel{name = <<"instructions">>,
                         children = [#xmlcdata{content = ClientReqText}]},
    EnterNicknameText = translate:translate(Lang, <<"Enter nickname you want to register">>),
    EnterNicknameEl = #xmlel{name = <<"instructions">>,
                             children = [#xmlcdata{content = EnterNicknameText}]},
    TitleText = <<(translate:translate(Lang, <<"Nickname Registration at ">>))/binary,
                  Host/binary>>,
    TitleEl = #xmlel{name = <<"title">>, children = [#xmlcdata{content = TitleText}]},
    Registered ++
    [ClientReqEl,
     #xmlel{name = <<"x">>, attrs = [{<<"xmlns">>, ?NS_XDATA}],
            children = [TitleEl,
                        EnterNicknameEl,
                        xfield(<<"text-single">>, <<"Nickname">>, <<"nick">>, Nick, Lang)]}].


-spec iq_set_register_info(jid:server(), jid:server(),
        jid:simple_jid() | jid:jid(), nick(), ejabberd:lang())
            -> {'error', jlib:xmlel()} | {'result', []}.
iq_set_register_info(ServerHost, Host, From, Nick, Lang) ->
    case set_nick(ServerHost, Host, From, Nick) of
        ok ->
            {result, []};
        {error, conflict} ->
            ErrText = <<"That nickname is registered by another person">>,
            {error, mongoose_xmpp_errors:conflict(Lang, ErrText)};
        {error, should_not_be_empty} ->
            ErrText = <<"You must fill in field \"Nickname\" in the form">>,
            {error, mongoose_xmpp_errors:not_acceptable(Lang, ErrText)};
        {error, ErrorReason} ->
            ?ERROR_MSG("event=iq_set_register_info_failed, "
                        "jid=~ts, nick=~p, reason=~p",
                       [jid:to_binary(From), Nick, ErrorReason]),
            {error, mongoose_xmpp_errors:internal_server_error()}
    end.

-spec iq_set_unregister_info(jid:server(), jid:server(),
        jid:simple_jid() | jid:jid(), ejabberd:lang())
            -> {'error', jlib:xmlel()} | {'result', []}.
iq_set_unregister_info(ServerHost, Host, From, _Lang) ->
    case unset_nick(ServerHost, Host, From) of
        ok ->
            {result, []};
        {error, ErrorReason} ->
            ?ERROR_MSG("event=iq_set_unregister_info_failed, "
                        "jid=~ts, reason=~p",
                       [jid:to_binary(From), ErrorReason]),
            {error, mongoose_xmpp_errors:internal_server_error()}
    end.

-spec process_iq_register_set(jid:server(), jid:server(),
                              jid:jid(), jlib:xmlel(), ejabberd:lang())
            -> {'error', jlib:xmlel()} | {'result', []}.
process_iq_register_set(ServerHost, Host, From, SubEl, Lang) ->
    #xmlel{children = Els} = SubEl,
    case xml:get_subtag(SubEl, <<"remove">>) of
        false ->
            case xml:remove_cdata(Els) of
                [#xmlel{name = <<"x">>} = XEl] ->
                    process_register(xml:get_tag_attr_s(<<"xmlns">>, XEl),
                                     xml:get_tag_attr_s(<<"type">>, XEl),
                                     ServerHost, Host, From, Lang, XEl);
                _ ->
                    {error, mongoose_xmpp_errors:bad_request()}
            end;
        _ ->
            iq_set_unregister_info(ServerHost, Host, From, Lang)
    end.

-spec process_register(XMLNS :: binary(), Type :: binary(),
                       ServerHost :: jid:server(), Host :: jid:server(),
                       From :: jid:jid(), Lang :: ejabberd:lang(), XEl :: exml:element()) ->
    {error, exml:element()} | {result, []}.
process_register(?NS_XDATA, <<"cancel">>, _ServerHost, _Host, _From, _Lang, _XEl) ->
    {result, []};
process_register(?NS_XDATA, <<"submit">>, ServerHost, Host, From, Lang, XEl) ->
    XData = jlib:parse_xdata_submit(XEl),
    case XData of
        invalid ->
            {error, mongoose_xmpp_errors:bad_request()};
        _ ->
            case lists:keysearch(<<"nick">>, 1, XData) of
                {value, {_, [Nick]}} when Nick /= <<>> ->
                    iq_set_register_info(ServerHost, Host, From, Nick, Lang);
                _ ->
                    ErrText = <<"You must fill in field \"Nickname\" in the form">>,
                    {error, mongoose_xmpp_errors:not_acceptable(Lang, ErrText)}
            end
    end;
process_register(_, _, _ServerHost, _Host, _From, _Lang, _XEl) ->
    {error, mongoose_xmpp_errors:bad_request()}.

-spec iq_get_vcard(ejabberd:lang()) -> [exml:element(), ...].
iq_get_vcard(Lang) ->
    [#xmlel{name = <<"FN">>,
            children = [#xmlcdata{content = <<"ejabberd/mod_muc">>}]},
     #xmlel{name = <<"URL">>, children = [#xmlcdata{content = ?MONGOOSE_URI}]},
     #xmlel{name = <<"DESC">>,
            children = [#xmlcdata{content =
                                  <<(translate:translate(Lang, <<"ejabberd MUC module">>))/binary,
                                    "\nCopyright (c) 2003-2011 ProcessOne">>}]}].


-spec broadcast_service_message(jid:server(), binary() | string()) -> ok.
broadcast_service_message(Host, Msg) ->
    lists:foreach(
      fun(#muc_online_room{pid = Pid}) ->
              gen_fsm_compat:send_all_state_event(
                Pid, {service_message, Msg})
      end, get_vh_rooms(Host)).


-spec get_vh_rooms(jid:server()) -> [muc_online_room()].
get_vh_rooms(Host) ->
    mnesia:dirty_select(muc_online_room,
                        [{#muc_online_room{name_host = '$1', _ = '_'},
                          [{'==', {element, 2, '$1'}, Host}],
                          ['$_']}]).


-spec clean_table_from_bad_node(node()) -> any().
clean_table_from_bad_node(Node) ->
    F = fun() ->
                Es = mnesia:select(
                       muc_online_room,
                       [{#muc_online_room{pid = '$1', _ = '_'},
                         [{'==', {node, '$1'}, Node}],
                         ['$_']}]),
                lists:foreach(fun(E) ->
                                      mnesia:delete_object(E)
                              end, Es)
        end,
    mnesia:async_dirty(F).


-spec clean_table_from_bad_node(node(), jid:server()) -> any().
clean_table_from_bad_node(Node, Host) ->
    F = fun() ->
                Es = mnesia:select(
                       muc_online_room,
                       [{#muc_online_room{pid = '$1',
                                          name_host = {'_', Host},
                                          _ = '_'},
                         [{'==', {node, '$1'}, Node}],
                         ['$_']}]),
                lists:foreach(fun(E) ->
                                      mnesia:delete_object(E)
                              end, Es)
        end,
    mnesia:async_dirty(F).

%%====================================================================
%% Hooks handlers
%%====================================================================

-spec is_room_owner(Acc :: boolean(), Room :: jid:jid(), User :: jid:jid()) -> boolean().
is_room_owner(_, Room, User) ->
    mod_muc_room:is_room_owner(Room, User) =:= {ok, true}.

-spec muc_room_pid(Acc :: any(), Room :: jid:jid()) -> {ok, pid()} | {error, not_found}.
muc_room_pid(_, Room) ->
    room_jid_to_pid(Room).

-spec can_access_room(Acc :: boolean(), Room :: jid:jid(), User :: jid:jid()) ->
    boolean().
can_access_room(_, Room, User) ->
    case mod_muc_room:can_access_room(Room, User) of
        {error, _} -> false;
        {ok, CanAccess} -> CanAccess
    end.

-spec can_access_identity(Acc :: boolean(), Room :: jid:jid(), User :: jid:jid()) ->
    boolean().
can_access_identity(_, Room, User) ->
    case mod_muc_room:can_access_identity(Room, User) of
        {error, _} -> false;
        {ok, CanAccess} -> CanAccess
    end.

online_rooms_number() ->
    lists:sum([online_rooms_number(Host) || Host <- ?MYHOSTS]).

online_rooms_number(Host) ->
    try
        Supervisor = gen_mod:get_module_proc(Host, ejabberd_mod_muc_sup),
        Stats = supervisor:count_children(Supervisor),
        proplists:get_value(active, Stats)
    catch _:_ ->
              0
    end.

hibernated_rooms_number() ->
    lists:sum([hibernated_rooms_number(Host) || Host <- ?MYHOSTS]).

hibernated_rooms_number(Host) ->
    try
        count_hibernated_rooms(Host)
    catch _:_ ->
              0
    end.

count_hibernated_rooms(Host) ->
    AllRooms = all_room_pids(Host),
    lists:foldl(fun count_hibernated_rooms/2, 0, AllRooms).

all_room_pids(Host) ->
    Supervisor = gen_mod:get_module_proc(Host, ejabberd_mod_muc_sup),
    [Pid || {undefined, Pid, worker, _} <- supervisor:which_children(Supervisor)].


count_hibernated_rooms(Pid, Count) ->
    case erlang:process_info(Pid, current_function) of
        {current_function, {erlang, hibernate, _}} ->
            Count + 1;
        _ ->
            Count
    end.

-define(EX_EVAL_SINGLE_VALUE, {[{l, [{t, [value, {v, 'Value'}]}]}], [value]}).
ensure_metrics(_Host) ->
    mongoose_metrics:ensure_metric(global, [mod_muc, deep_hibernations], spiral),
    mongoose_metrics:ensure_metric(global, [mod_muc, process_recreations], spiral),
    mongoose_metrics:ensure_metric(global, [mod_muc, hibernations], spiral),
    mongoose_metrics:ensure_metric(global, [mod_muc, hibernated_rooms],
                                   {function, mod_muc, hibernated_rooms_number, [],
                                    eval, ?EX_EVAL_SINGLE_VALUE}),
    mongoose_metrics:ensure_metric(global, [mod_muc, online_rooms],
                                   {function, mod_muc, online_rooms_number, [],
                                    eval, ?EX_EVAL_SINGLE_VALUE}).
