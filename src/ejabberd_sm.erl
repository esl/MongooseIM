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
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------
-module(ejabberd_sm).
-author('alexey@process-one.net').

-behaviour(gen_server).
-behaviour(gen_iq_component).


%% API
-export([start/0,
         start_link/0,
         route/3,
         route/4,
         make_new_sid/0,
         open_session/4, open_session/5,
         close_session/4,
         store_info/3,
         get_info/2,
         remove_info/2,
         check_in_subscription/5,
         bounce_offline_message/4,
         disconnect_removed_user/3,
         get_user_resources/1,
         set_presence/6,
         unset_presence/5,
         close_session_unset_presence/5,
         get_unique_sessions_number/0,
         get_total_sessions_number/0,
         get_node_sessions_number/0,
         get_vh_session_number/1,
         get_vh_session_list/1,
         get_full_session_list/0,
         register_iq_handler/3,
         unregister_iq_handler/2,
         force_update_presence/2,
         user_resources/2,
         get_session_pid/1,
         get_session/1,
         get_session_ip/1,
         get_user_present_resources/1,
         get_raw_sessions/1,
         is_offline/1,
         get_user_present_pids/2,
         sync/0,
         run_session_cleanup_hook/1,
         sm_backend/0
        ]).

%% Hook handlers
-export([node_cleanup/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% xmpp_router callback
-export([do_filter/3]).
-export([do_route/4]).

-ignore_xref([bounce_offline_message/4, check_in_subscription/5, disconnect_removed_user/3,
              do_filter/3, do_route/4, force_update_presence/2, get_unique_sessions_number/0,
              get_user_present_pids/2, node_cleanup/2, start_link/0, user_resources/2,
              sm_backend/0]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("ejabberd_commands.hrl").
-include("session.hrl").

-record(state, {}).
-type state() :: #state{}.

-type sid() :: {mongoose_lib:microseconds(), pid()}.
-type priority() :: integer() | undefined.

-type session() :: #session{
                      sid      :: sid(),
                      usr      :: jid:simple_jid(),
                      us       :: jid:simple_bare_jid(),
                      priority :: priority(),
                      info     :: info()
                     }.
-type info() :: #{info_key() => any()}.

-type backend() :: ejabberd_sm_mnesia | ejabberd_sm_redis.
-type close_reason() :: resumed | normal | replaced.
-type info_key() :: atom().

-export_type([session/0,
              sid/0,
              priority/0,
              backend/0,
              close_reason/0,
              info/0
             ]).

%% default value for the maximum number of user connections
-define(MAX_USER_SESSIONS, 100).
-define(UNIQUE_COUNT_CACHE, [cache, unique_sessions_number]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok, Pid} | ignore | {error, Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

-spec start() -> {ok, pid()}.
start() ->
    Spec = {?MODULE, {?MODULE, start_link, []}, permanent, brutal_kill, worker, [?MODULE]},
    {ok, _} = ejabberd_sup:start_child(Spec).

-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
    mongoose_metrics:ensure_metric(global, ?UNIQUE_COUNT_CACHE, gauge),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% You MUST NOT call this function from the big tests.
%% In 99% you should call ejabberd_router:route/3 instead.
%% This function would fail for the first routed IQ.
-spec route(From, To, Packet) -> Acc when
      From :: jid:jid(),
      To :: jid:jid(),
      Packet :: exml:element() | mongoose_acc:t() | ejabberd_c2s:broadcast(),
      Acc :: mongoose_acc:t().
route(From, To, #xmlel{} = Packet) ->
    Acc = new_acc(From, To, Packet),
    route(From, To, Acc);
route(From, To, {broadcast, #xmlel{} = Payload}) ->
    Acc = new_acc(From, To, Payload),
    route(From, To, Acc, {broadcast, Payload});
route(From, To, {broadcast, Payload}) ->
    Acc = new_acc(To),
    route(From, To, Acc, {broadcast, Payload});
route(From, To, Acc) ->
    route(From, To, Acc, mongoose_acc:element(Acc)).

-spec new_acc(jid:jid(), jid:jid(), exml:element()) -> mongoose_acc:t().
new_acc(From, To = #jid{lserver = LServer}, Packet) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(To#jid.lserver),
    mongoose_acc:new(#{location => ?LOCATION,
                       host_type => HostType,
                       lserver => LServer,
                       element => Packet,
                       from_jid => From,
                       to_jid => To}).

-spec new_acc(jid:jid()) -> mongoose_acc:t().
new_acc(To = #jid{lserver = LServer}) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(To#jid.lserver),
    mongoose_acc:new(#{location => ?LOCATION,
                       host_type => HostType,
                       lserver => LServer,
                       element => undefined}).

route(From, To, Acc, {broadcast, Payload}) ->
    try
        do_route(Acc, From, To, {broadcast, Payload})
    catch Class:Reason:Stacktrace ->
              ?LOG_ERROR(#{what => sm_route_failed,
                           text => <<"Failed to route broadcast in ejabberd_sm">>,
                           class => Class, reason => Reason, stacktrace => Stacktrace,
                           payload => Payload, acc => Acc}),
              Acc
    end;
route(From, To, Acc, El) ->
    try
        do_route(Acc, From, To, El)
    catch Class:Reason:Stacktrace ->
              ?LOG_ERROR(#{what => sm_route_failed,
                           text => <<"Failed to route stanza in ejabberd_sm">>,
                           class => Class, reason => Reason, stacktrace => Stacktrace,
                           acc => Acc}),
              Acc
    end.

-spec make_new_sid() -> sid().
make_new_sid() ->
    {erlang:system_time(microsecond), self()}.

-spec open_session(HostType, SID, JID, Info) -> ReplacedPids when
      HostType :: binary(),
      SID :: 'undefined' | sid(),
      JID :: jid:jid(),
      Info :: info(),
      ReplacedPids :: [pid()].
open_session(HostType, SID, JID, Info) ->
    open_session(HostType, SID, JID, undefined, Info).

-spec open_session(HostType, SID, JID, Priority, Info) -> ReplacedPids when
      HostType :: binary(),
      SID :: 'undefined' | sid(),
      JID :: jid:jid(),
      Priority :: integer() | undefined,
      Info :: info(),
      ReplacedPids :: [pid()].
open_session(HostType, SID, JID, Priority, Info) ->
    set_session(SID, JID, Priority, Info),
    ReplacedPIDs = check_for_sessions_to_replace(HostType, JID),
    mongoose_hooks:sm_register_connection_hook(HostType, SID, JID, Info),
    ReplacedPIDs.

-spec close_session(Acc, SID, JID, Reason) -> Acc1 when
      Acc :: mongoose_acc:t(),
      SID :: 'undefined' | sid(),
      JID :: jid:jid(),
      Reason :: close_reason(),
      Acc1 :: mongoose_acc:t().
close_session(Acc, SID, JID, Reason) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    Info = case ejabberd_sm_backend:get_sessions(LUser, LServer, LResource) of
               [Session] ->
                   Session#session.info;
               _ ->
                   []
           end,
    ejabberd_sm_backend:delete_session(SID, LUser, LServer, LResource),
    mongoose_hooks:sm_remove_connection_hook(Acc, SID, JID, Info, Reason).

-spec store_info(jid:jid(), info_key(), any()) ->
    {ok, any()} | {error, offline}.
store_info(JID, Key, Value) ->
    case get_session(JID) of
        offline -> {error, offline};
        #session{sid = SID, priority = SPriority, info = SInfo} ->
            case SID of
                {_, Pid} when self() =:= Pid ->
                    %% It's safe to allow process update its own record
                    update_session(SID, JID, SPriority,
                                   maps:put(Key, Value, SInfo)),
                    {ok, Key};
                {_, Pid} ->
                    %% Ask the process to update its record itself
                    %% Async operation
                    ejabberd_c2s:store_session_info(Pid, JID, Key, Value),
                    {ok, Key}
            end
    end.

-spec get_info(jid:jid(), info_key()) ->
    {ok, any()} | {error, offline | not_set}.
get_info(JID, Key) ->
    case get_session(JID) of
        offline -> {error, offline};
        Session ->
            case mongoose_session:get_info(Session, Key, {error, not_set}) of
                {Key, Value} -> {ok, Value};
                Other -> Other
            end
    end.

-spec remove_info(jid:jid(), info_key()) ->
    ok | {error, offline}.
remove_info(JID, Key) ->
    case get_session(JID) of
        offline -> {error, offline};
        #session{sid = SID, priority = SPriority, info = SInfo} ->
            case SID of
                {_, Pid} when self() =:= Pid ->
                    %% It's safe to allow process update its own record
                    update_session(SID, JID, SPriority,
                                   maps:remove(Key, SInfo)),
                    ok;
                {_, Pid} ->
                    %% Ask the process to update its record itself
                    %% Async operation
                    ejabberd_c2s:remove_session_info(Pid, JID, Key),
                    ok
            end
    end.

-spec check_in_subscription(Acc, ToJID, FromJID, Type, Reason) -> any() | {stop, false} when
      Acc :: any(),
      ToJID :: jid:jid(),
      FromJID :: jid:jid(),
      Type :: any(),
      Reason :: any().
check_in_subscription(Acc, ToJID, _FromJID, _Type, _Reason) ->
    case ejabberd_auth:does_user_exist(ToJID) of
        true ->
            Acc;
        false ->
            {stop, mongoose_acc:set(hook, result, false, Acc)}
    end.

-spec bounce_offline_message(Acc, From, To, Packet) -> {stop, Acc} when
      Acc :: map(),
      From :: jid:jid(),
      To :: jid:jid(),
      Packet :: exml:element().
bounce_offline_message(Acc, From, To, Packet) ->
    Acc1 = mongoose_hooks:xmpp_bounce_message(Acc),
    E = mongoose_xmpp_errors:service_unavailable(<<"en">>, <<"Bounce offline message">>),
    {Acc2, Err} = jlib:make_error_reply(Acc1, Packet, E),
    Acc3 = ejabberd_router:route(To, From, Acc2, Err),
    {stop, Acc3}.

-spec disconnect_removed_user(mongoose_acc:t(), User :: jid:user(),
                              Server :: jid:server()) -> mongoose_acc:t().
disconnect_removed_user(Acc, User, Server) ->
    lists:map(fun({_, Pid}) -> ejabberd_c2s:terminate_session(Pid, <<"User removed">>) end,
              get_user_present_pids(User, Server)),
    Acc.


-spec get_user_resources(JID :: jid:jid()) -> [binary()].
get_user_resources(#jid{luser = LUser, lserver = LServer}) ->
    Ss = ejabberd_sm_backend:get_sessions(LUser, LServer),
    [element(3, S#session.usr) || S <- clean_session_list(Ss)].


-spec get_session_ip(JID) -> undefined | {inet:ip_address(), integer()} when
      JID :: jid:jid().
get_session_ip(JID) ->
    case get_session(JID) of
        offline -> undefined;
        Session ->
            case mongoose_session:get_info(Session, ip, undefined) of
                {ip, Val} -> Val;
                Other -> Other
            end
    end.

-spec get_session(JID) -> offline | session() when
      JID :: jid:jid().
get_session(JID) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    case ejabberd_sm_backend:get_sessions(LUser, LServer, LResource) of
        [] ->
            offline;
        Ss ->
            lists:max(Ss)
    end.

-spec get_raw_sessions(jid:jid()) -> [session()].
get_raw_sessions(#jid{luser = LUser, lserver = LServer}) ->
    clean_session_list(
      ejabberd_sm_backend:get_sessions(LUser, LServer)).

-spec set_presence(Acc, SID, JID, Prio, Presence, Info) -> Acc1 when
      Acc :: mongoose_acc:t(),
      Acc1 :: mongoose_acc:t(),
      SID :: 'undefined' | sid(),
      JID :: jid:jid(),
      Prio :: 'undefined' | integer(),
      Presence :: any(),
      Info :: info().
set_presence(Acc, SID, JID, Priority, Presence, Info) ->
    set_session(SID, JID, Priority, Info),
    mongoose_hooks:set_presence_hook(Acc, JID, Presence).


-spec unset_presence(Acc, SID, JID, Status, Info) -> Acc1 when
      Acc :: mongoose_acc:t(),
      Acc1 :: mongoose_acc:t(),
      SID :: 'undefined' | sid(),
      JID :: jid:jid(),
      Status :: binary(),
      Info :: info().
unset_presence(Acc, SID, JID, Status, Info) ->
    set_session(SID, JID, undefined, Info),
    mongoose_hooks:unset_presence_hook(Acc, JID, Status).


-spec close_session_unset_presence(Acc, SID, JID, Status, Reason) -> Acc1 when
      Acc :: mongoose_acc:t(),
      SID :: 'undefined' | sid(),
      JID :: jid:jid(),
      Status :: binary(),
      Reason :: close_reason(),
      Acc1 :: mongoose_acc:t().
close_session_unset_presence(Acc, SID, JID, Status, Reason) ->
    Acc1 = close_session(Acc, SID, JID, Reason),
    mongoose_hooks:unset_presence_hook(Acc1, JID, Status).


-spec get_session_pid(JID) -> none | pid() when
      JID :: jid:jid().
get_session_pid(JID) ->
    case get_session(JID) of
        offline -> none;
        #session{sid = {_, Pid}} -> Pid
    end.

-spec get_unique_sessions_number() -> integer().
get_unique_sessions_number() ->
    try
        C = ejabberd_sm_backend:unique_count(),
        mongoose_metrics:update(global, ?UNIQUE_COUNT_CACHE, C),
        C
    catch
        _:_ ->
            get_cached_unique_count()
    end.


-spec get_total_sessions_number() -> integer().
get_total_sessions_number() ->
    ejabberd_sm_backend:total_count().


-spec get_vh_session_number(jid:server()) -> non_neg_integer().
get_vh_session_number(Server) ->
    length(ejabberd_sm_backend:get_sessions(Server)).


-spec get_vh_session_list(jid:server()) -> [session()].
get_vh_session_list(Server) ->
    ejabberd_sm_backend:get_sessions(Server).


-spec get_node_sessions_number() -> non_neg_integer().
get_node_sessions_number() ->
    {value, {active, Active}} = lists:keysearch(active, 1,
                                                supervisor:count_children(ejabberd_c2s_sup)),
    Active.


-spec get_full_session_list() -> [session()].
get_full_session_list() ->
    ejabberd_sm_backend:get_sessions().


register_iq_handler(Host, XMLNS, IQHandler) ->
    ejabberd_sm ! {register_iq_handler, Host, XMLNS, IQHandler},
    ok.

-spec sync() -> ok.
sync() ->
    gen_server:call(ejabberd_sm, sync).

unregister_iq_handler(Host, XMLNS) ->
    ejabberd_sm ! {unregister_iq_handler, Host, XMLNS},
    ok.

-spec run_session_cleanup_hook(#session{}) -> mongoose_acc:t().
run_session_cleanup_hook(#session{usr = {U, S, R}, sid = SID}) ->
    {ok, HostType} = mongoose_domain_api:get_domain_host_type(S),
    Acc = mongoose_acc:new(
            #{location => ?LOCATION,
              host_type => HostType,
              lserver => S,
              element => undefined}),
    mongoose_hooks:session_cleanup(S, Acc, U, R, SID).

%%====================================================================
%% Hook handlers
%%====================================================================

node_cleanup(Acc, Node) ->
    Timeout = timer:minutes(1),
    Res = gen_server:call(?MODULE, {node_cleanup, Node}, Timeout),
    maps:put(?MODULE, Res, Acc).

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
    Backend = mongoose_config:get_opt(sm_backend),
    ejabberd_sm_backend:init(#{backend => Backend}),

    ets:new(sm_iqtable, [named_table, protected, {read_concurrency, true}]),
    ejabberd_hooks:add(node_cleanup, global, ?MODULE, node_cleanup, 50),
    lists:foreach(fun(HostType) -> ejabberd_hooks:add(hooks(HostType)) end,
                  ?ALL_HOST_TYPES),
    ejabberd_commands:register_commands(commands()),
    {ok, #state{}}.

hooks(HostType) ->
    [
     {roster_in_subscription, HostType, ejabberd_sm, check_in_subscription, 20},
     {offline_message_hook, HostType, ejabberd_sm, bounce_offline_message, 100},
     {offline_groupchat_message_hook, HostType, ejabberd_sm, bounce_offline_message, 100},
     {remove_user, HostType, ejabberd_sm, disconnect_removed_user, 100}
    ].

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
    {TimeDiff, _R} = timer:tc(fun ejabberd_sm_backend:cleanup/1, [Node]),
    ?LOG_INFO(#{what => sm_node_cleanup,
                text => <<"Cleaning after a node that went down">>,
                cleanup_node => Node,
                duration => erlang:round(TimeDiff / 1000)}),
    {reply, ok, State};
handle_call(sync, _From, State) ->
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
-spec handle_info(_, _) -> {'noreply', _}.
handle_info({route, From, To, Packet}, State) ->
    route(From, To, Packet),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, IQHandler}, State) ->
    case ets:insert_new(sm_iqtable, {{XMLNS, Host}, IQHandler}) of
        true -> ok;
        false ->
              ?LOG_WARNING(#{what => register_iq_handler_duplicate,
                             xmlns => XMLNS, host => Host})
    end,
    {noreply, State};
handle_info({unregister_iq_handler, Host, XMLNS}, State) ->
    case ets:lookup(sm_iqtable, {XMLNS, Host}) of
        [{_, IQHandler}] ->
            gen_iq_component:stop_iq_handler(IQHandler),
            ets:delete(sm_iqtable, {XMLNS, Host});
        _ ->
            ?LOG_WARNING(#{what => unregister_iq_handler_missing,
                           xmlns => XMLNS, host => Host})
    end,
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
-spec terminate(_, state()) -> 'ok'.
terminate(_Reason, _State) ->
    try
        ejabberd_commands:unregister_commands(commands())
    catch Class:Reason:Stacktrace ->
              ?LOG_ERROR(#{what => sm_terminate_failed,
                           text => <<"Caught error while terminating SM">>,
                           class => Class, reason => Reason, stacktrace => Stacktrace})
    end,
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

-spec set_session(SID, JID, Prio, Info) -> ok | {error, any()} when
      SID :: sid() | 'undefined',
      JID :: jid:jid(),
      Prio :: priority(),
      Info :: info().
set_session(SID, JID, Priority, Info) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    US = {LUser, LServer},
    USR = {LUser, LServer, LResource},
    Session = #session{sid = SID,
                       usr = USR,
                       us = US,
                       priority = Priority,
                       info = Info},
    ejabberd_sm_backend:create_session(LUser, LServer, LResource, Session).

-spec update_session(SID, JID, Prio, Info) -> ok | {error, any()} when
      SID :: sid() | 'undefined',
      JID :: jid:jid(),
      Prio :: priority(),
      Info :: info().
update_session(SID, JID, Priority, Info) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    US = {LUser, LServer},
    USR = {LUser, LServer, LResource},
    Session = #session{sid = SID,
                       usr = USR,
                       us = US,
                       priority = Priority,
                       info = Info},
    ejabberd_sm_backend:update_session(LUser, LServer, LResource, Session).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_filter(From, To, Packet) ->
    {From, To, Packet}.

-spec do_route(Acc, From, To, Payload) -> Acc when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Payload :: exml:element() | ejabberd_c2s:broadcast().
do_route(Acc, From, To, {broadcast, Payload} = Broadcast) ->
    ?LOG_DEBUG(#{what => sm_route_broadcast, acc => Acc, payload => Payload}),
    #jid{ luser = LUser, lserver = LServer, lresource = LResource} = To,
    case LResource of
        <<>> ->
            CurrentPids = get_user_present_pids(LUser, LServer),
            Acc1 = mongoose_hooks:sm_broadcast(Acc, From, To, Broadcast, length(CurrentPids)),
            ?LOG_DEBUG(#{what => sm_broadcast, session_pids => CurrentPids}),
            BCast = {broadcast, Payload},
            lists:foreach(fun({_, Pid}) -> Pid ! BCast end, CurrentPids),
            Acc1;
        _ ->
            case get_session_pid(To) of
                none ->
                    Acc; % do nothing
                Pid when is_pid(Pid) ->
                    ?LOG_DEBUG(#{what => sm_broadcast, session_pid => Pid}),
                    Pid ! Broadcast,
                    Acc
            end
    end;
do_route(Acc, From, To, El) ->
    ?LOG_DEBUG(#{what => sm_route, acc => Acc}),
    #jid{lresource = LResource} = To,
    #xmlel{name = Name} = El,
    case LResource of
        <<>> ->
            do_route_no_resource(Name, From, To, Acc, El);
        _ ->
            case get_session_pid(To) of
                none ->
                    do_route_offline(Name, mongoose_acc:stanza_type(Acc),
                                     From, To, Acc, El);
                Pid when is_pid(Pid) ->
                    ?LOG_DEBUG(#{what => sm_route_to_pid,
                                 session_pid => Pid, acc => Acc}),
                    Pid ! {route, From, To, Acc},
                    Acc
            end
    end.

-spec do_route_no_resource_presence_prv(From, To, Acc, Packet, Type, Reason) -> boolean() when
      From :: jid:jid(),
      To :: jid:jid(),
      Acc :: mongoose_acc:t(),
      Packet :: exml:element(),
      Type :: 'subscribe' | 'subscribed' | 'unsubscribe' | 'unsubscribed',
      Reason :: any().
do_route_no_resource_presence_prv(From, To, Acc, Packet, Type, Reason) ->
    case is_privacy_allow(From, To, Acc, Packet) of
        true ->
            Res = mongoose_hooks:roster_in_subscription(Acc, To, From, Type, Reason),
            mongoose_acc:get(hook, result, false, Res);
        false ->
            false
    end.

-spec do_route_no_resource_presence(Type, From, To, Acc, Packet) -> boolean() when
      Type :: binary(),
      From :: jid:jid(),
      To :: jid:jid(),
      Acc :: mongoose_acc:t(),
      Packet :: exml:element().
do_route_no_resource_presence(<<"subscribe">>, From, To, Acc, Packet) ->
    Reason = xml:get_path_s(Packet, [{elem, <<"status">>}, cdata]),
    do_route_no_resource_presence_prv(From, To, Acc, Packet, subscribe, Reason);
do_route_no_resource_presence(<<"subscribed">>, From, To, Acc, Packet) ->
    do_route_no_resource_presence_prv(From, To, Acc, Packet, subscribed, <<>>);
do_route_no_resource_presence(<<"unsubscribe">>, From, To, Acc, Packet) ->
    do_route_no_resource_presence_prv(From, To, Acc, Packet, unsubscribe, <<>>);
do_route_no_resource_presence(<<"unsubscribed">>, From, To, Acc, Packet) ->
    do_route_no_resource_presence_prv(From, To, Acc, Packet, unsubscribed, <<>>);
do_route_no_resource_presence(_, _, _, _, _) ->
    true.


-spec do_route_no_resource(Name, From, To, Acc, El) -> Acc when
      Name :: undefined | binary(),
      From :: jid:jid(),
      To :: jid:jid(),
      Acc :: mongoose_acc:t(),
      El :: exml:element().
do_route_no_resource(<<"presence">>, From, To, Acc, El) ->
    Type = mongoose_acc:stanza_type(Acc),
    case do_route_no_resource_presence(Type, From, To, Acc, El) of
        true ->
            PResources = get_user_present_resources(To),
            lists:foldl(fun({_, R}, A) ->
                                NewTo = jid:replace_resource(To, R),
                                NewAccParams = #{element => El, from_jid => From, to_jid => NewTo},
                                A0 = mongoose_acc:update_stanza(NewAccParams, A),
                                do_route(A0, From, NewTo, El)
                        end, Acc, PResources);
        false ->
            Acc
    end;
do_route_no_resource(<<"message">>, From, To, Acc, El) ->
    route_message(From, To, Acc, El);
do_route_no_resource(<<"iq">>, From, To, Acc, El) ->
    process_iq(From, To, Acc, El);
do_route_no_resource(_, _, _, Acc, _) ->
    Acc.

-spec do_route_offline(Name, Type, From, To, Acc, Packet) -> mongoose_acc:t() when
      Name :: 'undefined' | binary(),
      Type :: binary(),
      From :: jid:jid(),
      To :: jid:jid(),
      Acc :: mongoose_acc:t(),
      Packet :: exml:element().
do_route_offline(<<"message">>, _, From, To, Acc, Packet)  ->
    HostType = mongoose_acc:host_type(Acc),
    Drop = mongoose_hooks:sm_filter_offline_message(HostType, From, To, Packet),
    case Drop of
        false ->
            route_message(From, To, Acc, Packet);
        true ->
            ?LOG_DEBUG(#{what => sm_offline_dropped, acc => Acc}),
            Acc
    end;
do_route_offline(<<"iq">>, <<"error">>, _From, _To, Acc, _Packet) ->
    Acc;
do_route_offline(<<"iq">>, <<"result">>, _From, _To, Acc, _Packet) ->
    Acc;
do_route_offline(<<"iq">>, _, From, To, Acc, Packet) ->
    E = mongoose_xmpp_errors:service_unavailable(<<"en">>, <<"Route offline">>),
    {Acc1, Err} = jlib:make_error_reply(Acc, Packet, E),
    ejabberd_router:route(To, From, Acc1, Err);
do_route_offline(_, _, _, _, Acc, _) ->
    ?LOG_DEBUG(#{what => sm_packet_dropped, acc => Acc}),
    Acc.


%% @doc The default list applies to the user as a whole,
%% and is processed if there is no active list set
%% for the target session/resource to which a stanza is addressed,
%% or if there are no current sessions for the user.
-spec is_privacy_allow(From, To, Acc, Packet) -> boolean() when
      From :: jid:jid(),
      To :: jid:jid(),
      Acc :: mongoose_acc:t(),
      Packet :: exml:element() | mongoose_acc:t().
is_privacy_allow(From, To, Acc, Packet) ->
    HostType = mongoose_acc:host_type(Acc),
    PrivacyList = mongoose_hooks:privacy_get_user_list(HostType, To),
    is_privacy_allow(From, To, Acc, Packet, PrivacyList).


%% @doc Check if privacy rules allow this delivery
%% Function copied from ejabberd_c2s.erl
-spec is_privacy_allow(From, To, Acc, Packet, PrivacyList) -> boolean() when
      From :: jid:jid(),
      To :: jid:jid(),
      Acc :: mongoose_acc:t(),
      Packet :: exml:element(),
      PrivacyList :: mongoose_privacy:userlist().
is_privacy_allow(_From, To, Acc, _Packet, PrivacyList) ->
    {Res, _} = mongoose_privacy:privacy_check_packet(Acc, To, PrivacyList, To, in),
    allow == Res.


-spec route_message(From, To, Acc, Packet) -> Acc when
      From :: jid:jid(),
      To :: jid:jid(),
      Acc :: mongoose_acc:t(),
      Packet :: exml:element().
route_message(From, To, Acc, Packet) ->
    LUser = To#jid.luser,
    LServer = To#jid.lserver,
    PrioPid = get_user_present_pids(LUser, LServer),
    case catch lists:max(PrioPid) of
        {Priority, _} when is_integer(Priority), Priority >= 0 ->
            lists:foreach(
              %% Route messages to all priority that equals the max, if
              %% positive
              fun({Prio, Pid}) when Prio == Priority ->
                 %% we will lose message if PID is not alive
                      Pid ! {route, From, To, Acc};
                 %% Ignore other priority:
                 ({_Prio, _Pid}) ->
                      ok
              end,
              PrioPid),
              Acc;
        _ ->
            MessageType = mongoose_acc:stanza_type(Acc),
            route_message_by_type(MessageType, From, To, Acc, Packet)
    end.

route_message_by_type(<<"error">>, _From, _To, Acc, _Packet) ->
    Acc;
route_message_by_type(<<"groupchat">>, From, To, Acc, Packet) ->
    mongoose_hooks:offline_groupchat_message_hook(Acc, From, To, Packet);
route_message_by_type(<<"headline">>, From, To, Acc, Packet) ->
    {stop, Acc1} = bounce_offline_message(Acc, From, To, Packet),
    Acc1;
route_message_by_type(_, From, To, Acc, Packet) ->
    HostType = mongoose_acc:host_type(Acc),
    case ejabberd_auth:does_user_exist(HostType, To, stored) of
        true ->
            case is_privacy_allow(From, To, Acc, Packet) of
                true ->
                    mongoose_hooks:offline_message_hook(Acc, From, To, Packet);
                false ->
                    mongoose_hooks:failed_to_store_message(Acc)
            end;
        _ ->
            E = mongoose_xmpp_errors:service_unavailable(<<"en">>, <<"User not found">>),
            {Acc1, Err} = jlib:make_error_reply(Acc, Packet, E),
            ejabberd_router:route(To, From, Acc1, Err)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec clean_session_list([session()]) -> [session()].
clean_session_list(Ss) ->
    clean_session_list(lists:keysort(#session.usr, Ss), []).


-spec clean_session_list([session()], [session()]) -> [session()].
clean_session_list([], Res) ->
    Res;
clean_session_list([S], Res) ->
    [S | Res];
clean_session_list([S1, S2 | Rest], Res) ->
    case S1#session.usr == S2#session.usr of
        true ->
            case S1#session.sid > S2#session.sid of
                true -> clean_session_list([S1 | Rest], Res);
                false -> clean_session_list([S2 | Rest], Res)
            end;
        false ->
            clean_session_list([S2 | Rest], [S1 | Res])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_user_present_pids(LUser, LServer) -> [{priority(), pid()}] when
      LUser :: jid:luser(),
      LServer :: jid:lserver().
get_user_present_pids(LUser, LServer) ->
    Ss = ejabberd_sm_backend:get_sessions(LUser, LServer),
    [{S#session.priority, element(2, S#session.sid)} ||
     S <- clean_session_list(Ss), is_integer(S#session.priority)].

-spec get_user_present_resources(jid:jid()) -> [{priority(), binary()}].
get_user_present_resources(#jid{luser = LUser, lserver = LServer}) ->
    Ss = ejabberd_sm_backend:get_sessions(LUser, LServer),
    [{S#session.priority, element(3, S#session.usr)} ||
        S <- clean_session_list(Ss), is_integer(S#session.priority)].

-spec is_offline(jid:jid()) -> boolean().
is_offline(#jid{luser = LUser, lserver = LServer}) ->
    case catch lists:max(get_user_present_pids(LUser, LServer)) of
        {Priority, _} when is_integer(Priority), Priority >= 0 ->
            false;
        _ ->
            true
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc On new session, check if some existing connections need to be replace
-spec check_for_sessions_to_replace(HostType, JID) -> ReplacedPids when
      HostType :: mongooseim:host_type(),
      JID :: jid:jid(),
      ReplacedPids :: [pid()].
check_for_sessions_to_replace(HostType, JID) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    %% TODO: Depending on how this is executed, there could be an unneeded
    %% replacement for max_sessions. We need to check this at some point.
    ReplacedRedundantSessions = check_existing_resources(HostType, LUser, LServer, LResource),
    AllReplacedSessionPids = check_max_sessions(HostType, LUser, LServer, ReplacedRedundantSessions),
    [Pid ! replaced || Pid <- AllReplacedSessionPids],
    AllReplacedSessionPids.

-spec check_existing_resources(HostType, LUser, LServer, LResource) ->
        ReplacedSessionsPIDs when
      HostType :: mongooseim:host_type(),
      LUser :: jid:luser(),
      LServer :: jid:lserver(),
      LResource :: jid:lresource(),
      ReplacedSessionsPIDs :: ordsets:ordset(pid()).
check_existing_resources(_HostType, LUser, LServer, LResource) ->
    %% A connection exist with the same resource. We replace it:
    Sessions = ejabberd_sm_backend:get_sessions(LUser, LServer, LResource),
    case [S#session.sid || S <- Sessions] of
        [] -> [];
        SIDs ->
            MaxSID = lists:max(SIDs),
            ordsets:from_list([Pid || {_, Pid} = S <- SIDs, S /= MaxSID])
    end.

-spec check_max_sessions(HostType :: mongooseim:host_type(),
                         LUser :: jid:luser(),
                         LServer :: jid:lserver(),
                         ReplacedPIDs :: [pid()]) ->
    AllReplacedPIDs :: ordsets:ordset(pid()).
check_max_sessions(HostType, LUser, LServer, ReplacedPIDs) ->
    %% If the max number of sessions for a given is reached, we replace the
    %% first one
    SIDs = lists:filtermap(
                fun(Session) ->
                    {_, Pid} = SID = Session#session.sid,
                    case ordsets:is_element(Pid, ReplacedPIDs) of
                        true -> false;
                        false -> {true, SID}
                    end
                end,
                ejabberd_sm_backend:get_sessions(LUser, LServer)),
    MaxSessions = get_max_user_sessions(HostType, LUser, LServer),
    case length(SIDs) =< MaxSessions of
        true -> ordsets:to_list(ReplacedPIDs);
        false ->
            {_, Pid} = lists:min(SIDs),
            [Pid | ordsets:to_list(ReplacedPIDs)]
    end.


%% @doc Get the user_max_session setting
%% This option defines the max number of time a given users are allowed to
%% log in. Defaults to infinity
-spec get_max_user_sessions(HostType, LUser, LServer) -> Result when
      HostType :: mongooseim:host_type(),
      LUser :: jid:luser(),
      LServer :: jid:lserver(),
      Result :: infinity | pos_integer().
get_max_user_sessions(HostType, LUser, LServer) ->
    JID = jid:make_noprep(LUser, LServer, <<>>),
    case acl:match_rule(HostType, LServer, max_user_sessions, JID) of
        Max when is_integer(Max) -> Max;
        infinity -> infinity;
        _ -> ?MAX_USER_SESSIONS
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec process_iq(From, To, Acc, Packet) -> Acc when
      From :: jid:jid(),
      To :: jid:jid(),
      Acc :: mongoose_acc:t(),
      Packet :: exml:element().
process_iq(From, To, Acc0, Packet) ->
    {IQ, Acc} = mongoose_iq:info(Acc0),
    process_iq(IQ, From, To, Acc, Packet).

process_iq(#iq{type = Type}, _From, _To, Acc, _Packet) when Type == result; Type == error ->
    % results and errors are always sent to full jids, so we ignore them here
    Acc;
process_iq(#iq{xmlns = XMLNS} = IQ, From, To, Acc, Packet) ->
    Host = To#jid.lserver,
    case ets:lookup(sm_iqtable, {XMLNS, Host}) of
        [{_, IQHandler}] ->
            gen_iq_component:handle(IQHandler, Acc, From, To, IQ);
        [] ->
            E = mongoose_xmpp_errors:service_unavailable(<<"en">>, <<"Unknown xmlns=", XMLNS/binary, " for host=", Host/binary>>),
            {Acc1, Err} = jlib:make_error_reply(Acc, Packet, E),
            ejabberd_router:route(To, From, Acc1, Err)
    end;
process_iq(_, From, To, Acc, Packet) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, Packet, mongoose_xmpp_errors:bad_request()),
   ejabberd_router:route(To, From, Acc1, Err).


-spec force_update_presence(mongooseim:host_type(), {jid:luser(), jid:lserver()}) -> 'ok'.
force_update_presence(_HostType, {LUser, LServer}) ->
    Ss = ejabberd_sm_backend:get_sessions(LUser, LServer),
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
    JID = jid:make(list_to_binary(UserStr), list_to_binary(ServerStr), <<"">>),
    Resources = get_user_resources(JID),
    lists:sort(Resources).

-spec get_cached_unique_count() -> non_neg_integer().
get_cached_unique_count() ->
    case mongoose_metrics:get_metric_value(global, ?UNIQUE_COUNT_CACHE) of
        {ok, DataPoints} ->
            proplists:get_value(value, DataPoints);
        _ ->
            0
    end.

%% It is used from big tests
-spec sm_backend() -> backend().
sm_backend() ->
    mongoose_backend:get_backend_module(global, ?MODULE).
