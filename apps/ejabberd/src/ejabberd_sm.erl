%%%----------------------------------------------------------------------
%%% File    : ejabberd_sm.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Session manager
%%% Created : 24 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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
-module(ejabberd_sm).
-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/0,
         route/3,
         open_session/5, open_session/6,
         close_session/5,
         store_info/4,
         check_in_subscription/6,
         bounce_offline_message/3,
         disconnect_removed_user/2,
         get_user_resources/2,
         set_presence/7,
         unset_presence/6,
         close_session_unset_presence/6,
         get_unique_sessions_number/0,
         get_total_sessions_number/0,
         get_node_sessions_number/0,
         get_vh_session_number/1,
         get_vh_session_list/1,
         get_full_session_list/0,
         register_iq_handler/4,
         register_iq_handler/5,
         unregister_iq_handler/2,
         force_update_presence/1,
         user_resources/2,
         get_session_pid/3,
         get_session/3,
         get_session_ip/3,
         get_user_present_resources/2,
         get_raw_sessions/2
        ]).

%% Hook handlers
-export([node_cleanup/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% xmpp_router callback
-export([do_filter/3]).
-export([do_route/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("ejabberd_commands.hrl").
-include("mod_privacy.hrl").

-record(state, {}).
-type state() :: #state{}.

-type sid() :: tuple().
-type priority() :: integer() | undefined.

-type session() :: #session{
                      sid      :: sid(),
                      usr      :: ejabberd:simple_jid(),
                      us       :: ejabberd:simple_bare_jid(),
                      priority :: priority(),
                      info     :: list()
                     }.

%% Session representation as 4-tuple.
-type ses_tuple() :: {USR :: ejabberd:simple_jid(),
                      Sid :: ejabberd_sm:sid(),
                      Prio :: priority(),
                      Info :: list()}.
-type backend() :: ejabberd_sm_mnesia | ejabberd_sm_redis.
-type close_reason() :: resumed | normal | replaced.

-export_type([session/0,
              sid/0,
              ses_tuple/0,
              backend/0,
              close_reason/0
            ]).

%% default value for the maximum number of user connections
-define(MAX_USER_SESSIONS, 100).
-define(SM_BACKEND, (ejabberd_sm_backend:backend())).
-define(UNIQUE_COUNT_CACHE, [cache, unique_sessions_number]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
    mongoose_metrics:ensure_metric(?UNIQUE_COUNT_CACHE, gauge),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec route(From, To, Packet) -> ok when
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Packet :: jlib:xmlel() | ejabberd_c2s:broadcast().
route(From, To, Packet) ->
    case (catch do_route(From, To, Packet)) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("error when routing from=~ts to=~ts in module=~p, reason=~p, packet=~ts, stack_trace=~p",
                [jid:to_binary(From), jid:to_binary(To),
                    ?MODULE, Reason, exml:to_binary(Packet),
                    erlang:get_stacktrace()]);
        _ -> ok
    end.

-spec open_session(SID, User, Server, Resource, Info) -> ok when
      SID :: 'undefined' | sid(),
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Resource :: binary(),
      Info :: 'undefined' | [any()].
open_session(SID, User, Server, Resource, Info) ->
    open_session(SID, User, Server, Resource, undefined, Info).

-spec open_session(SID, User, Server, Resource, Priority, Info) -> ok when
      SID :: 'undefined' | sid(),
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Resource :: binary(),
      Priority :: integer() | undefined,
      Info :: 'undefined' | [any()].
open_session(SID, User, Server, Resource, Priority, Info) ->
    set_session(SID, User, Server, Resource, Priority, Info),
    check_for_sessions_to_replace(User, Server, Resource),
    JID = jid:make(User, Server, Resource),
    ejabberd_hooks:run(sm_register_connection_hook, JID#jid.lserver,
                       [SID, JID, Info]).

-spec close_session(SID, User, Server, Resource, Reason) -> ok when
    SID :: 'undefined' | sid(),
    User :: ejabberd:user(),
    Server :: ejabberd:server(),
    Resource :: ejabberd:resource(),
    Reason :: close_reason().
close_session(SID, User, Server, Resource, Reason) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    Info = case ?SM_BACKEND:get_sessions(LUser, LServer, LResource) of
               [Session] ->
                   Session#session.info;
               _ ->
                   []
           end,
    ?SM_BACKEND:delete_session(SID, LUser, LServer, LResource),
    JID = jid:make(User, Server, Resource),
    ejabberd_hooks:run(sm_remove_connection_hook, JID#jid.lserver,
                       [SID, JID, Info, Reason]).

-spec store_info(ejabberd:user(), ejabberd:server(), ejabberd:resource(),
                 {any(), any()}) -> {ok, {any(), any()}} | {error, offline}.
store_info(User, Server, Resource, {Key, _Value} = KV) ->
    case get_session(User, Server, Resource) of
        offline -> {error, offline};
        {_SUser,SID,SPriority,SInfo} ->
            set_session(SID, User, Server, Resource, SPriority,
                        lists:keystore(Key, 1, SInfo, KV)),
            {ok, KV}
    end.

-spec check_in_subscription(Acc, User, Server, JID, Type, Reason) -> any() | {stop, false} when
      Acc :: any(),
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      JID :: ejabberd:jid(),
      Type :: any(),
      Reason :: any().
check_in_subscription(Acc, User, Server, _JID, _Type, _Reason) ->
    case ejabberd_auth:is_user_exists(User, Server) of
        true ->
            Acc;
        false ->
            {stop, false}
    end.


-spec bounce_offline_message(From, To, Packet) -> stop when
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Packet :: jlib:xmlel().
bounce_offline_message(#jid{server = Server} = From, To, Packet) ->
    ejabberd_hooks:run(xmpp_bounce_message,
                       Server,
                       [Server, Packet]),
    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
    ejabberd_router:route(To, From, Err),
    stop.

-spec disconnect_removed_user(User :: ejabberd:user(), Server :: ejabberd:server()) -> ok.
disconnect_removed_user(User, Server) ->
    ejabberd_sm:route(jid:make(<<>>, <<>>, <<>>),
                      jid:make(User, Server, <<>>),
                      {broadcast, {exit, <<"User removed">>}}).


-spec get_user_resources(User :: ejabberd:user(), Server :: ejabberd:server()) -> [binary()].
get_user_resources(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Ss = ?SM_BACKEND:get_sessions(LUser, LServer),
    [element(3, S#session.usr) || S <- clean_session_list(Ss)].


-spec get_session_ip(User, Server, Resource) -> undefined | {inet:ip_address(), integer()} when
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Resource :: ejabberd:resource().
get_session_ip(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    case ?SM_BACKEND:get_sessions(LUser, LServer, LResource) of
        [] ->
            undefined;
        Ss ->
            Session = lists:max(Ss),
            proplists:get_value(ip, Session#session.info)
    end.


-spec get_session(User, Server, Resource) -> offline | ses_tuple() when
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Resource :: ejabberd:resource().
get_session(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    case ?SM_BACKEND:get_sessions(LUser, LServer, LResource) of
        [] ->
            offline;
        Ss ->
            Session = lists:max(Ss),
            {Session#session.usr,
             Session#session.sid,
             Session#session.priority,
             Session#session.info}
    end.
-spec get_raw_sessions(ejabberd:user(), ejabberd:server()) -> [#session{}].
get_raw_sessions(User, Server) ->
    clean_session_list(
      ?SM_BACKEND:get_sessions(jid:nodeprep(User), jid:nameprep(Server))).

-spec set_presence(SID, User, Server, Resource, Prio, Presence, Info) -> ok when
      SID :: 'undefined' | sid(),
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Resource :: ejabberd:resource(),
      Prio :: 'undefined' | integer(),
      Presence :: any(),
      Info :: 'undefined' | [any()].
set_presence(SID, User, Server, Resource, Priority, Presence, Info) ->
    set_session(SID, User, Server, Resource, Priority, Info),
    ejabberd_hooks:run(set_presence_hook, jid:nameprep(Server),
                       [User, Server, Resource, Presence]).


-spec unset_presence(SID, User, Server, Resource, Status, Info) -> ok when
      SID :: 'undefined' | sid(),
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Resource :: ejabberd:resource(),
      Status :: any(),
      Info :: 'undefined' | [any()].
unset_presence(SID, User, Server, Resource, Status, Info) ->
    set_session(SID, User, Server, Resource, undefined, Info),
    LServer = jid:nameprep(Server),
    ejabberd_hooks:run(unset_presence_hook, LServer,
                       [jid:nodeprep(User), LServer,
                        jid:resourceprep(Resource), Status]).


-spec close_session_unset_presence(SID, User, Server, Resource, Status, Reason) -> ok when
      SID :: 'undefined' | sid(),
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Resource :: ejabberd:resource(),
      Status :: any(),
      Reason :: close_reason().
close_session_unset_presence(SID, User, Server, Resource, Status, Reason) ->
    close_session(SID, User, Server, Resource, Reason),
    LServer = jid:nameprep(Server),
    ejabberd_hooks:run(unset_presence_hook, LServer,
                       [jid:nodeprep(User), LServer,
                        jid:resourceprep(Resource), Status]).


-spec get_session_pid(User, Server, Resource) -> none | pid() when
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Resource :: ejabberd:resource().
get_session_pid(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    case ?SM_BACKEND:get_sessions(LUser, LServer, LResource) of
        [#session{sid = {_, Pid}}] ->
            Pid;
        _ ->
            none
    end.


-spec get_unique_sessions_number() -> integer().
get_unique_sessions_number() ->
    try
        C = ?SM_BACKEND:unique_count(),
        mongoose_metrics:update(?UNIQUE_COUNT_CACHE, C),
        C
    catch
        _:_ ->
            get_cached_unique_count()
    end.


-spec get_total_sessions_number() -> integer().
get_total_sessions_number() ->
    ?SM_BACKEND:total_count().


-spec get_vh_session_number(ejabberd:server()) -> non_neg_integer().
get_vh_session_number(Server) ->
    length(?SM_BACKEND:get_sessions(Server)).


-spec get_vh_session_list(ejabberd:server()) -> [ses_tuple()].
get_vh_session_list(Server) ->
    ?SM_BACKEND:get_sessions(Server).


-spec get_node_sessions_number() -> non_neg_integer().
get_node_sessions_number() ->
    {value, {active, Active}} = lists:keysearch(active, 1, supervisor:count_children(ejabberd_c2s_sup)),
    Active.


-spec get_full_session_list() -> [session()].
get_full_session_list() ->
    ?SM_BACKEND:get_sessions().


register_iq_handler(Host, XMLNS, Module, Fun) ->
    ejabberd_sm ! {register_iq_handler, Host, XMLNS, Module, Fun}.


register_iq_handler(Host, XMLNS, Module, Fun, Opts) ->
    ejabberd_sm ! {register_iq_handler, Host, XMLNS, Module, Fun, Opts}.


unregister_iq_handler(Host, XMLNS) ->
    ejabberd_sm ! {unregister_iq_handler, Host, XMLNS}.

%%====================================================================
%% Hook handlers
%%====================================================================

node_cleanup(Node) ->
    Timeout = timer:minutes(1),
    gen_server:call(?MODULE, {node_cleanup, Node}, Timeout).

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
-spec init(_) -> {ok, state()}.
init([]) ->
    {Backend, Opts} = ejabberd_config:get_global_option(sm_backend),
    {Mod, Code} = dynamic_compile:from_string(sm_backend(Backend)),
    code:load_binary(Mod, "ejabberd_sm_backend.erl", Code),

    ets:new(sm_iqtable, [named_table]),
    ejabberd_hooks:add(node_cleanup, global, ?MODULE, node_cleanup, 50),
    lists:foreach(
      fun(Host) ->
              ejabberd_hooks:add(roster_in_subscription, Host,
                                 ejabberd_sm, check_in_subscription, 20),
              ejabberd_hooks:add(offline_message_hook, Host,
                                 ejabberd_sm, bounce_offline_message, 100),
              ejabberd_hooks:add(offline_groupchat_message_hook, Host,
                                 ejabberd_sm, bounce_offline_message, 100),
              ejabberd_hooks:add(remove_user, Host,
                                 ejabberd_sm, disconnect_removed_user, 100)
      end, ?MYHOSTS),
    ejabberd_commands:register_commands(commands()),

    ?SM_BACKEND:start(Opts),

    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({node_cleanup, Node}, _From, State) ->
    BackendModule = ?SM_BACKEND,
    {TimeDiff, _R} = timer:tc(fun BackendModule:cleanup/1, [Node]),
    ?INFO_MSG("sessions cleanup after node=~p, took=~pms",
              [Node, erlang:round(TimeDiff / 1000)]),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
-spec handle_info(_,_) -> {'noreply',_}.
handle_info({route, From, To, Packet}, State) ->
    route(From, To, Packet),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module, Function}, State) ->
    ets:insert(sm_iqtable, {{XMLNS, Host}, Module, Function}),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module, Function, Opts}, State) ->
    ets:insert(sm_iqtable, {{XMLNS, Host}, Module, Function, Opts}),
    {noreply, State};
handle_info({unregister_iq_handler, Host, XMLNS}, State) ->
    case ets:lookup(sm_iqtable, {XMLNS, Host}) of
        [{_, Module, Function, Opts}] ->
            gen_iq_handler:stop_iq_handler(Module, Function, Opts);
        _ ->
            ok
    end,
    ets:delete(sm_iqtable, {XMLNS, Host}),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
-spec terminate(_,state()) -> 'ok'.
terminate(_Reason, _State) ->
    ejabberd_commands:unregister_commands(commands()),
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

-spec set_session(SID, User, Server, Resource, Prio, Info) -> ok | {error, any()} when
      SID :: sid() | 'undefined',
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Resource :: ejabberd:resource(),
      Prio :: priority(),
      Info :: undefined | [any()].
set_session(SID, User, Server, Resource, Priority, Info) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    US = {LUser, LServer},
    USR = {LUser, LServer, LResource},
    Session = #session{sid = SID,
                       usr = USR,
                       us = US,
                       priority = Priority,
                       info = Info},
    ?SM_BACKEND:create_session(LUser, LServer, LResource, Session).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_filter(From, To, Packet) ->
    {From, To, Packet}.

-spec do_route(From, To, Packet) -> ok when
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Packet :: jlib:xmlel() | ejabberd_c2s:broadcast().
do_route(From, To, {broadcast, _} = Broadcast) ->
    ?DEBUG("from=~p,to=~p,broadcast=~p", [From, To, Broadcast]),
    #jid{ luser = LUser, lserver = LServer, lresource = LResource} = To,
    case LResource of
        <<>> ->
            CurrentPids = get_user_present_pids(LUser, LServer),
            ejabberd_hooks:run(sm_broadcast, To#jid.lserver,
                               [From, To, Broadcast, length(CurrentPids)]),
            ?DEBUG("bc_to=~p~n", [CurrentPids]),
            lists:foreach(fun({_, Pid}) -> Pid ! Broadcast end, CurrentPids);
        _ ->
            case ?SM_BACKEND:get_sessions(LUser, LServer, LResource) of
                [] ->
                    ok; % do nothing
                Ss ->
                    Session = lists:max(Ss),
                    Pid = element(2, Session#session.sid),
                    ?DEBUG("sending to process ~p~n", [Pid]),
                    Pid ! Broadcast
            end
    end;
do_route(From, To, Packet) ->
    ?DEBUG("session manager~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
           [From, To, Packet, 8]),
    #jid{ luser = LUser, lserver = LServer, lresource = LResource} = To,
    #xmlel{name = Name, attrs = Attrs} = Packet,
    case LResource of
        <<>> ->
            do_route_no_resource(Name, xml:get_attr_s(<<"type">>, Attrs),
                                 From, To, Packet);
        _ ->
            case ?SM_BACKEND:get_sessions(LUser, LServer, LResource) of
                [] ->
                    do_route_offline(Name, xml:get_attr_s(<<"type">>, Attrs),
                                     From, To, Packet);
                Ss ->
                    Session = lists:max(Ss),
                    Pid = element(2, Session#session.sid),
                    ?DEBUG("sending to process ~p~n", [Pid]),
                    Pid ! {route, From, To, Packet}
            end
    end.

-spec do_route_no_resource_presence_prv(From, To, Packet, Type, Reason) -> boolean() when
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Packet :: jlib:xmlel(),
      Type :: 'subscribe' | 'subscribed' | 'unsubscribe' | 'unsubscribed',
      Reason :: any().
do_route_no_resource_presence_prv(From,To,Packet,Type,Reason) ->
    is_privacy_allow(From, To, Packet) andalso ejabberd_hooks:run_fold(
        roster_in_subscription,
        To#jid.lserver,
        false,
        [To#jid.user, To#jid.server, From, Type, Reason]).


-spec do_route_no_resource_presence(Type, From, To, Packet) -> boolean() when
      Type :: binary(),
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Packet :: jlib:xmlel().
do_route_no_resource_presence(<<"subscribe">>, From, To, Packet) ->
        Reason = xml:get_path_s(Packet, [{elem, <<"status">>}, cdata]),
        do_route_no_resource_presence_prv(From, To, Packet, subscribe, Reason);
do_route_no_resource_presence(<<"subscribed">>, From, To, Packet) ->
        do_route_no_resource_presence_prv(From, To, Packet, subscribed, <<>>);
do_route_no_resource_presence(<<"unsubscribe">>, From, To, Packet) ->
        do_route_no_resource_presence_prv(From, To, Packet, unsubscribe, <<>>);
do_route_no_resource_presence(<<"unsubscribed">>, From, To, Packet) ->
        do_route_no_resource_presence_prv(From, To, Packet, unsubscribed, <<>>);
do_route_no_resource_presence(_, _, _, _) ->
        true.


-spec do_route_no_resource(Name, Type, From, To, Packet) -> Result when
      Name :: undefined | binary(),
      Type :: any(),
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Packet :: jlib:xmlel(),
      Result ::ok | stop | todo | pid() | {error, lager_not_running} | {process_iq, _, _, _}.
do_route_no_resource(<<"presence">>, Type, From, To, Packet) ->
        case do_route_no_resource_presence(Type, From, To, Packet) of
            true ->
            PResources = get_user_present_resources(To#jid.luser, To#jid.lserver),
                    lists:foreach(
                      fun({_, R}) ->
      do_route(From, jid:replace_resource(To, R), Packet)
              end, PResources);
            false ->
                ok
        end;
do_route_no_resource(<<"message">>, _, From, To, Packet) ->
        route_message(From, To, Packet);
do_route_no_resource(<<"iq">>, _, From, To, Packet) ->
        process_iq(From, To, Packet);
do_route_no_resource(<<"broadcast">>, _, From, To, Packet) ->
    % Backward compatibility
    ejabberd_hooks:run(sm_broadcast, To#jid.lserver, [From, To, Packet]),
    broadcast_packet(From, To, Packet);
do_route_no_resource(_, _, _, _, _) ->
        ok.

-spec do_route_offline(Name, Type, From, To, Packet) -> ok | stop when
      Name :: 'undefined' | binary(),
      Type :: binary(),
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Packet :: jlib:xmlel().
do_route_offline(<<"message">>, _, From, To, Packet)  ->
        route_message(From, To, Packet);
do_route_offline(<<"iq">>, <<"error">>, _From, _To, _Packet) ->
        ok;
do_route_offline(<<"iq">>, <<"result">>, _From, _To, _Packet) ->
        ok;
do_route_offline(<<"iq">>, _, From, To, Packet) ->
        Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
        ejabberd_router:route(To, From, Err);
do_route_offline(_, _, _, _, _) ->
        ?DEBUG("packet droped~n", []),
        ok.

% Backward compatibility
-spec broadcast_packet(From :: ejabberd:jid(), To :: ejabberd:jid(), Packet :: jlib:xmlel()) -> ok.
broadcast_packet(From, To, Packet) ->
    #jid{user = User, server = Server} = To,
    lists:foreach(
      fun(R) ->
              do_route(From,
                       jid:replace_resource(To, R),
                       Packet)
      end, get_user_resources(User, Server)).

%% @doc The default list applies to the user as a whole,
%% and is processed if there is no active list set
%% for the target session/resource to which a stanza is addressed,
%% or if there are no current sessions for the user.
-spec is_privacy_allow(From, To, Packet) -> boolean() when
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Packet :: jlib:xmlel().
is_privacy_allow(From, To, Packet) ->
    User = To#jid.user,
    Server = To#jid.server,
    PrivacyList = ejabberd_hooks:run_fold(privacy_get_user_list, Server,
                                          #userlist{}, [User, Server]),
    is_privacy_allow(From, To, Packet, PrivacyList).


%% @doc Check if privacy rules allow this delivery
%% Function copied from ejabberd_c2s.erl
-spec is_privacy_allow(From, To, Packet, PrivacyList) -> boolean() when
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Packet :: jlib:xmlel(),
      PrivacyList :: list().
is_privacy_allow(From, To, Packet, PrivacyList) ->
    User = To#jid.user,
    Server = To#jid.server,
    allow == ejabberd_hooks:run_fold(
               privacy_check_packet, Server,
               allow,
               [User, Server, PrivacyList,
                {From, To, Packet}, in]).


-spec route_message(From, To, Packet) -> ok | stop when
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Packet :: jlib:xmlel().
route_message(From, To, Packet) ->
    LUser = To#jid.luser,
    LServer = To#jid.lserver,
    PrioPid = get_user_present_pids(LUser,LServer),
    case catch lists:max(PrioPid) of
        {Priority, _} when is_integer(Priority), Priority >= 0 ->
            lists:foreach(
              %% Route messages to all priority that equals the max, if
              %% positive
              fun({Prio, Pid}) when Prio == Priority ->
                      % we will lose message if PID is not alive
                      Pid ! {route, From, To, Packet};
                 %% Ignore other priority:
                 ({_Prio, _Pid}) ->
                      ok
              end,
              PrioPid);
        _ ->
            case xml:get_tag_attr_s(<<"type">>, Packet) of
                <<"error">> ->
                    ok;
                <<"groupchat">> ->
                    ejabberd_hooks:run(offline_groupchat_message_hook,
                                       LServer,
                                       [From, To, Packet]);
                <<"headline">> ->
                    bounce_offline_message(From, To, Packet);
                _Type ->
                    case ejabberd_auth:is_user_exists(LUser, LServer) of
                        true ->
                            case is_privacy_allow(From, To, Packet) of
                                true ->
                                    ejabberd_hooks:run(offline_message_hook,
                                                       LServer,
                                                       [From, To, Packet]);
                                false ->
                                    ejabberd_hooks:run_fold(failed_to_store_message,
                                                            LServer, Packet, [From]),
                                    ok
                            end;
                        _ ->
                            Err = jlib:make_error_reply(
                                    Packet, ?ERR_SERVICE_UNAVAILABLE),
                            ejabberd_router:route(To, From, Err)
                    end
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec clean_session_list([sid()]) -> [sid()].
clean_session_list(Ss) ->
    clean_session_list(lists:keysort(#session.usr, Ss), []).


-spec clean_session_list([sid()],[sid()]) -> [sid()].
clean_session_list([], Res) ->
    Res;
clean_session_list([S], Res) ->
    [S | Res];
clean_session_list([S1, S2 | Rest], Res) ->
    if
        S1#session.usr == S2#session.usr ->
            if
                S1#session.sid > S2#session.sid ->
                    clean_session_list([S1 | Rest], Res);
                true ->
                    clean_session_list([S2 | Rest], Res)
            end;
        true ->
            clean_session_list([S2 | Rest], [S1 | Res])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_user_present_pids(LUser, LServer) -> [{priority(), pid()}] when
      LUser :: ejabberd:luser(),
      LServer :: ejabberd:lserver().
get_user_present_pids(LUser, LServer) ->
    Ss = clean_session_list(?SM_BACKEND:get_sessions(LUser, LServer)),
    [{S#session.priority, element(2,S#session.sid)} || S <- Ss, is_integer(S#session.priority)].

-spec get_user_present_resources(LUser :: ejabberd:user(),
                                 LServer :: ejabberd:server()
                                 ) -> [{priority(), binary()}].
get_user_present_resources(LUser, LServer) ->
    Ss = ?SM_BACKEND:get_sessions(LUser, LServer),
    [{S#session.priority, element(3, S#session.usr)} ||
        S <- clean_session_list(Ss), is_integer(S#session.priority)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc On new session, check if some existing connections need to be replace
-spec check_for_sessions_to_replace(User, Server, Resource) -> ok | replaced when
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Resource :: ejabberd:resource().
check_for_sessions_to_replace(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),

    %% TODO: Depending on how this is executed, there could be an unneeded
    %% replacement for max_sessions. We need to check this at some point.
    check_existing_resources(LUser, LServer, LResource),
    check_max_sessions(LUser, LServer).

-spec check_existing_resources(LUser, LServer, LResource) -> ok when
      LUser :: 'error' | ejabberd:luser() | tuple(),
      LServer :: 'error' | ejabberd:lserver() | tuple(),
      LResource :: 'error' | ejabberd:lresource() | [byte()] | tuple().
check_existing_resources(LUser, LServer, LResource) ->
    %% A connection exist with the same resource. We replace it:
    Sessions = ?SM_BACKEND:get_sessions(LUser, LServer, LResource),
    SIDs = [S#session.sid || S <- Sessions],
    if
        SIDs == [] ->
            ok;
        true ->
            MaxSID = lists:max(SIDs),
            lists:foreach(
              fun({_, Pid} = S) when S /= MaxSID ->
                      Pid ! replaced;
                 (_) -> ok
              end, SIDs)
    end.


-spec check_max_sessions(LUser :: ejabberd:user(), LServer :: ejabberd:server()) -> ok | replaced.
check_max_sessions(LUser, LServer) ->
    %% If the max number of sessions for a given is reached, we replace the
    %% first one
    Sessions = ?SM_BACKEND:get_sessions(LUser, LServer),
    SIDs = [S#session.sid || S <- Sessions],
    MaxSessions = get_max_user_sessions(LUser, LServer),
    if
        length(SIDs) =< MaxSessions ->
            ok;
        true ->
            {_, Pid} = lists:min(SIDs),
            Pid ! replaced
    end.


%% @doc Get the user_max_session setting
%% This option defines the max number of time a given users are allowed to
%% log in. Defaults to infinity
-spec get_max_user_sessions(LUser, Host) -> infinity | pos_integer() when
      LUser :: ejabberd:user(),
      Host :: ejabberd:server().
get_max_user_sessions(LUser, Host) ->
    case acl:match_rule(
           Host, max_user_sessions, jid:make(LUser, Host, <<>>)) of
        Max when is_integer(Max) -> Max;
        infinity -> infinity;
        _ -> ?MAX_USER_SESSIONS
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_iq(From, To, Packet) -> Result when
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Packet :: jlib:xmlel(),
      Result :: ok | todo | pid() | {error, lager_not_running} | {process_iq, _, _, _}.
process_iq(From, To, Packet) ->
    IQ = jlib:iq_query_info(Packet),
    case IQ of
        #iq{xmlns = XMLNS} ->
            Host = To#jid.lserver,
            case ets:lookup(sm_iqtable, {XMLNS, Host}) of
                [{_, Module, Function}] ->
                    ResIQ = Module:Function(From, To, IQ),
                    if
                        ResIQ /= ignore ->
                            ejabberd_router:route(To, From,
                                                  jlib:iq_to_xml(ResIQ));
                        true ->
                            ok
                    end;
                [{_, Module, Function, Opts}] ->
                    gen_iq_handler:handle(Host, Module, Function, Opts,
                                          From, To, IQ);
                [] ->
                    Err = jlib:make_error_reply(
                            Packet, ?ERR_SERVICE_UNAVAILABLE),
                    ejabberd_router:route(To, From, Err)
            end;
        reply ->
            ok;
        _ ->
            Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
            ejabberd_router:route(To, From, Err),
            ok
    end.


-spec force_update_presence({binary(), ejabberd:server()}) -> 'ok'.
force_update_presence({LUser, LServer}) ->
    Ss = ?SM_BACKEND:get_sessions(LUser, LServer),
    lists:foreach(fun(#session{sid = {_, Pid}}) ->
                          Pid ! {force_update_presence, LUser}
                  end, Ss).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ejabberd commands

-spec commands() -> [ejabberd_commands:cmd(), ...].
commands() ->
        [
     %% TODO: Implement following API functions with pluggable backends architcture
     %% #ejabberd_commands{name = connected_users,
     %%                    tags = [session],
     %%                    desc = "List all established sessions",
     %%                    module = ?MODULE, function = connected_users,
     %%                    args = [],
     %%                    result = {connected_users, {list, {sessions, string}}}},
     %% #ejabberd_commands{name = connected_users_number,
     %%                    tags = [session, stats],
     %%                    desc = "Get the number of established sessions",
     %%                    module = ?MODULE, function = connected_users_number,
     %%                    args = [],
     %%                    result = {num_sessions, integer}},
     #ejabberd_commands{name = user_resources,
                        tags = [session],
                        desc = "List user's connected resources",
                        module = ?MODULE, function = user_resources,
                        args = [{user, string}, {host, string}],
                        result = {resources, {list, {resource, binary}}}}
        ].


-spec user_resources(UserStr :: string(), ServerStr :: string()) -> [binary()].
user_resources(UserStr, ServerStr) ->
    Resources = get_user_resources(list_to_binary(UserStr), list_to_binary(ServerStr)),
    lists:sort(Resources).

-spec sm_backend(backend()) -> string().
sm_backend(Backend) ->
    lists:flatten(
      ["-module(ejabberd_sm_backend).
        -export([backend/0]).

        -spec backend() -> atom().
        backend() ->
            ejabberd_sm_",
       atom_to_list(Backend),
       ".\n"]).

-spec get_cached_unique_count() -> non_neg_integer().
get_cached_unique_count() ->
    case mongoose_metrics:get_metric_value(?UNIQUE_COUNT_CACHE) of
        {ok, DataPoints} ->
            proplists:get_value(value, DataPoints);
        _ ->
            0
    end.
