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
-export([start/0,
         start_link/0,
         route/3,
         route/4,
         open_session/3, open_session/4,
         close_session/4,
         store_info/2,
         get_info/2,
         remove_info/2,
         check_in_subscription/6,
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
         register_iq_handler/4,
         register_iq_handler/5,
         unregister_iq_handler/2,
         force_update_presence/1,
         user_resources/2,
         get_session_pid/1,
         get_session/1,
         get_session_ip/1,
         get_user_present_resources/1,
         get_raw_sessions/1,
         is_offline/1,
         get_user_present_pids/2
        ]).

%% Deprecated API
-export([
         open_session/5,
         open_session/6,
         close_session/6,
         close_session_unset_presence/7,
         unset_presence/7,
         get_raw_sessions/2,
         store_info/4,
         set_presence/8,
         remove_info/4,
         get_user_resources/2,
         get_session_pid/3,
         get_session/3,
         get_session_ip/3,
         get_user_present_resources/2
        ]).

-deprecated({open_session, 5, eventually}).
-deprecated({open_session, 6, eventually}).
-deprecated({close_session, 6, eventually}).
-deprecated({close_session_unset_presence, 7, eventually}).
-deprecated({unset_presence, 7, eventually}).
-deprecated({get_raw_sessions, 2, eventually}).
-deprecated({store_info, 4, eventually}).
-deprecated({set_presence, 8, eventually}).
-deprecated({remove_info, 4, eventually}).
-deprecated({get_user_resources, 2, eventually}).
-deprecated({get_session_pid, 3, eventually}).
-deprecated({get_session, 3, eventually}).
-deprecated({get_session_ip, 3, eventually}).
-deprecated({get_user_present_resources, 2, eventually}).

%% Hook handlers
-export([node_cleanup/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% xmpp_router callback
-export([do_filter/3]).
-export([do_route/4]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("ejabberd_commands.hrl").
-include("mod_privacy.hrl").
-include("session.hrl").

-record(state, {}).
-type state() :: #state{}.

-type sid() :: tuple().
-type priority() :: integer() | undefined.

-type session() :: #session{
                      sid      :: sid(),
                      usr      :: jid:simple_jid(),
                      us       :: jid:simple_bare_jid(),
                      priority :: priority(),
                      info     :: info()
                     }.
-type info() :: [info_item()].

%% Session representation as 4-tuple.
-type ses_tuple() :: {USR :: jid:simple_jid(),
                      Sid :: ejabberd_sm:sid(),
                      Prio :: priority(),
                      Info :: info()}.
-type backend() :: ejabberd_sm_mnesia | ejabberd_sm_redis.
-type close_reason() :: resumed | normal | replaced.
-type info_key() :: atom().
-type info_item() :: {info_key(), any()}.

-export_type([session/0,
              sid/0,
              ses_tuple/0,
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


-spec route(From, To, Packet) -> Acc when
      From :: jid:jid(),
      To :: jid:jid(),
      Packet :: exml:element() | mongoose_acc:t() | ejabberd_c2s:broadcast(),
      Acc :: mongoose_acc:t().
route(From, To, #xmlel{} = Packet) ->
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => From#jid.lserver,
                              element => Packet,
                              from_jid => From,
                              to_jid => To }),
    route(From, To, Acc);
route(From, To, {broadcast, #xmlel{} = Payload}) ->
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              from_jid => From,
                              to_jid => To,
                              lserver => To#jid.lserver,
                              element => Payload }),
    route(From, To, Acc, {broadcast, Payload});
route(From, To, {broadcast, Payload}) ->
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => To#jid.lserver,
                              element => undefined }),
    route(From, To, Acc, {broadcast, Payload});
route(From, To, Acc) ->
    route(From, To, Acc, mongoose_acc:element(Acc)).

route(From, To, Acc, {broadcast, Payload}) ->
    case (catch do_route(Acc, From, To, {broadcast, Payload})) of
        {'EXIT', {Reason, StackTrace}} ->
            ?ERROR_MSG("error when routing from=~ts to=~ts in module=~p~n~nreason=~p~n~n"
            "broadcast=~p~n~nstack_trace=~p~n",
                [jid:to_binary(From), jid:to_binary(To),
                    ?MODULE, Reason, Payload, StackTrace]);
        Acc1 -> Acc1
    end;
route(From, To, Acc, El) ->
    case (catch do_route(Acc, From, To, El)) of
        {'EXIT', {Reason, StackTrace}} ->
            ?ERROR_MSG("error when routing from=~ts to=~ts in module=~p~n~nreason=~p~n~n"
                       "packet=~ts~n~nstack_trace=~p~n",
                       [jid:to_binary(From), jid:to_binary(To),
                        ?MODULE, Reason, exml:to_binary(El), StackTrace]);
        Acc1 -> Acc1
    end.

-spec open_session(SID, JID, Info) -> ReplacedPids when
      SID :: 'undefined' | sid(),
      JID :: jid:jid(),
      Info :: 'undefined' | [any()],
      ReplacedPids :: [pid()].
open_session(SID, JID, Info) ->
    open_session(SID, JID, undefined, Info).

-spec open_session(SID, JID, Priority, Info) -> ReplacedPids when
      SID :: 'undefined' | sid(),
      JID :: jid:jid(),
      Priority :: integer() | undefined,
      Info :: 'undefined' | [any()],
      ReplacedPids :: [pid()].
open_session(SID, JID, Priority, Info) ->
    set_session(SID, JID, Priority, Info),
    ReplacedPIDs = check_for_sessions_to_replace(JID),
    ejabberd_hooks:run(sm_register_connection_hook, JID#jid.lserver,
                       [SID, JID, Info]),
    ReplacedPIDs.

-spec close_session(Acc, SID, JID, Reason) -> Acc1 when
      Acc :: mongoose_acc:t(),
      SID :: 'undefined' | sid(),
      JID :: jid:jid(),
      Reason :: close_reason(),
      Acc1 :: mongoose_acc:t().
close_session(Acc, SID, JID, Reason) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    Info = case ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer, LResource) of
               [Session] ->
                   Session#session.info;
               _ ->
                   []
           end,
    ejabberd_gen_sm:delete_session(sm_backend(), SID, LUser, LServer, LResource),
    ejabberd_hooks:run_fold(sm_remove_connection_hook, JID#jid.lserver, Acc,
                            [SID, JID, Info, Reason]).

-spec store_info(jid:jid(), info_item()) ->
    {ok, {any(), any()}} | {error, offline}.
store_info(JID, {Key, _Value} = KV) ->
    case get_session(JID) of
        offline -> {error, offline};
        {_SUser, SID, SPriority, SInfo} ->
            case SID of
                {_, Pid} when self() =:= Pid ->
                    %% It's safe to allow process update its own record
                    update_session(SID, JID, SPriority,
                                   lists:keystore(Key, 1, SInfo, KV)),
                    {ok, KV};
                {_, Pid} ->
                    %% Ask the process to update its record itself
                    %% Async operation
                    ejabberd_c2s:store_session_info(Pid, JID, KV),
                    {ok, KV}
            end
    end.

-spec get_info(jid:jid(), info_key()) ->
    {ok, any()} | {error, offline | not_set}.
get_info(JID, Key) ->
    case get_session(JID) of
        offline -> {error, offline};
        {_SUser, _SID, _SPriority, SInfo} ->
            case lists:keyfind(Key, 1, SInfo) of
                {Key, Value} ->
                    {ok, Value};
                _ ->
                    {error, not_set}
            end
    end.

-spec remove_info(jid:jid(), info_key()) ->
    ok | {error, offline}.
remove_info(JID, Key) ->
    case get_session(JID) of
        offline -> {error, offline};
        {_SUser, SID, SPriority, SInfo} ->
            case SID of
                {_, Pid} when self() =:= Pid ->
                    %% It's safe to allow process update its own record
                    update_session(SID, JID, SPriority,
                                   lists:keydelete(Key, 1, SInfo)),
                    ok;
                {_, Pid} ->
                    %% Ask the process to update its record itself
                    %% Async operation
                    ejabberd_c2s:remove_session_info(Pid, JID, Key),
                    ok
            end
    end.

-spec check_in_subscription(Acc, User, Server, JID, Type, Reason) -> any() | {stop, false} when
      Acc :: any(),
      User :: jid:user(),
      Server :: jid:server(),
      JID :: jid:jid(),
      Type :: any(),
      Reason :: any().
check_in_subscription(Acc, User, Server, _JID, _Type, _Reason) ->
    case ejabberd_auth:is_user_exists(User, Server) of
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
bounce_offline_message(Acc, #jid{server = Server} = From, To, Packet) ->
    Acc1 = ejabberd_hooks:run_fold(xmpp_bounce_message,
                            Server,
                            Acc,
                            []),
    {Acc2, Err} = jlib:make_error_reply(Acc1, Packet, mongoose_xmpp_errors:service_unavailable()),
    Acc3 = ejabberd_router:route(To, From, Acc2, Err),
    {stop, Acc3}.

-spec disconnect_removed_user(mongoose_acc:t(), User :: jid:user(),
                              Server :: jid:server()) -> mongoose_acc:t().
disconnect_removed_user(Acc, User, Server) ->
    ejabberd_sm:route(jid:make_noprep(<<>>, <<>>, <<>>),
                      jid:make(User, Server, <<>>),
                      Acc,
                      {broadcast, {exit, <<"User removed">>}}).


-spec get_user_resources(JID :: jid:jid()) -> [binary()].
get_user_resources(JID) ->
    #jid{luser = LUser, lserver = LServer} = JID,
    Ss = ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer),
    [element(3, S#session.usr) || S <- clean_session_list(Ss)].


-spec get_session_ip(JID) -> undefined | {inet:ip_address(), integer()} when
      JID :: jid:jid().
get_session_ip(JID) ->
    case get_session(JID) of
        offline -> undefined;
        {_, _, _, Info} -> proplists:get_value(ip, Info)
    end.

-spec get_session(JID) -> offline | ses_tuple() when
      JID :: jid:jid().
get_session(JID) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    case ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer, LResource) of
        [] ->
            offline;
        Ss ->
            Session = lists:max(Ss),
            {Session#session.usr,
             Session#session.sid,
             Session#session.priority,
             Session#session.info}
    end.

-spec get_raw_sessions(jid:jid()) -> [session()].
get_raw_sessions(#jid{luser = LUser, lserver = LServer}) ->
    clean_session_list(
      ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer)).

-spec set_presence(Acc, SID, JID, Prio, Presence, Info) -> Acc1 when
      Acc :: mongoose_acc:t(),
      Acc1 :: mongoose_acc:t(),
      SID :: 'undefined' | sid(),
      JID :: jid:jid(),
      Prio :: 'undefined' | integer(),
      Presence :: any(),
      Info :: 'undefined' | [any()].
set_presence(Acc, SID, JID, Priority, Presence, Info) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    set_session(SID, JID, Priority, Info),
    ejabberd_hooks:run_fold(set_presence_hook, LServer, Acc,
                       [LUser, LServer, LResource, Presence]).


-spec unset_presence(Acc, SID, JID, Status, Info) -> Acc1 when
      Acc :: mongoose_acc:t(),
      Acc1 :: mongoose_acc:t(),
      SID :: 'undefined' | sid(),
      JID :: jid:jid(),
      Status :: any(),
      Info :: 'undefined' | [any()].
unset_presence(Acc, SID, JID, Status, Info) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    set_session(SID, JID, undefined, Info),
    ejabberd_hooks:run_fold(unset_presence_hook, LServer, Acc,
                       [LUser, LServer, LResource, Status]).


-spec close_session_unset_presence(Acc, SID, JID, Status, Reason) -> Acc1 when
      Acc :: mongoose_acc:t(),
      SID :: 'undefined' | sid(),
      JID :: jid:jid(),
      Status :: any(),
      Reason :: close_reason(),
      Acc1 :: mongoose_acc:t().
close_session_unset_presence(Acc, SID, JID, Status, Reason) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    Acc1 = close_session(Acc, SID, JID, Reason),
    ejabberd_hooks:run_fold(unset_presence_hook, LServer, Acc1,
                       [LUser, LServer, LResource, Status]).


-spec get_session_pid(JID) -> none | pid() when
      JID :: jid:jid().
get_session_pid(JID) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    case ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer, LResource) of
        [#session{sid = {_, Pid}}] ->
            Pid;
        _ ->
            none
    end.

-spec get_unique_sessions_number() -> integer().
get_unique_sessions_number() ->
    try
        C = ejabberd_gen_sm:unique_count(sm_backend()),
        mongoose_metrics:update(global, ?UNIQUE_COUNT_CACHE, C),
        C
    catch
        _:_ ->
            get_cached_unique_count()
    end.


-spec get_total_sessions_number() -> integer().
get_total_sessions_number() ->
    ejabberd_gen_sm:total_count(sm_backend()).


-spec get_vh_session_number(jid:server()) -> non_neg_integer().
get_vh_session_number(Server) ->
    length(ejabberd_gen_sm:get_sessions(sm_backend(), Server)).


-spec get_vh_session_list(jid:server()) -> [ses_tuple()].
get_vh_session_list(Server) ->
    ejabberd_gen_sm:get_sessions(sm_backend(), Server).


-spec get_node_sessions_number() -> non_neg_integer().
get_node_sessions_number() ->
    {value, {active, Active}} = lists:keysearch(active, 1,
                                                supervisor:count_children(ejabberd_c2s_sup)),
    Active.


-spec get_full_session_list() -> [ses_tuple()].
get_full_session_list() ->
    ejabberd_gen_sm:get_sessions(sm_backend()).


register_iq_handler(Host, XMLNS, Module, Fun) ->
    ejabberd_sm ! {register_iq_handler, Host, XMLNS, Module, Fun}.


register_iq_handler(Host, XMLNS, Module, Fun, Opts) ->
    ejabberd_sm ! {register_iq_handler, Host, XMLNS, Module, Fun, Opts}.


unregister_iq_handler(Host, XMLNS) ->
    ejabberd_sm ! {unregister_iq_handler, Host, XMLNS}.

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
    {Backend, Opts} = ejabberd_config:get_global_option(sm_backend),
    {Mod, Code} = dynamic_compile:from_string(sm_backend(Backend)),
    code:load_binary(Mod, "ejabberd_sm_backend.erl", Code),

    ets:new(sm_iqtable, [named_table]),

    ejabberd_hooks:add(node_cleanup, global, ?MODULE, node_cleanup, 50),
    lists:foreach(fun(Host) -> ejabberd_hooks:add(hooks(Host)) end, ?MYHOSTS),

    ejabberd_commands:register_commands(commands()),

    ejabberd_gen_sm:start(sm_backend(), Opts),

    {ok, #state{}}.

hooks(Host) ->
    [
     {roster_in_subscription, Host, ejabberd_sm, check_in_subscription, 20},
     {offline_message_hook, Host, ejabberd_sm, bounce_offline_message, 100},
     {offline_groupchat_message_hook, Host, ejabberd_sm, bounce_offline_message, 100},
     {remove_user, Host, ejabberd_sm, disconnect_removed_user, 100}
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
    BackendModule = sm_backend(),
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
-spec handle_info(_, _) -> {'noreply', _}.
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
-spec terminate(_, state()) -> 'ok'.
terminate(_Reason, _State) ->
    try
        ejabberd_commands:unregister_commands(commands())
    catch E:R:S ->
        ?ERROR_MSG("Caught error while terminating sm: ~p:~p~n~p", [E, R, S])
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
      Info :: undefined | [any()].
set_session(SID, JID, Priority, Info) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    US = {LUser, LServer},
    USR = {LUser, LServer, LResource},
    Session = #session{sid = SID,
                       usr = USR,
                       us = US,
                       priority = Priority,
                       info = Info},
    ejabberd_gen_sm:create_session(sm_backend(), LUser, LServer, LResource, Session).

-spec update_session(SID, JID, Prio, Info) -> ok | {error, any()} when
      SID :: sid() | 'undefined',
      JID :: jid:jid(),
      Prio :: priority(),
      Info :: undefined | [any()].
update_session(SID, JID, Priority, Info) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    US = {LUser, LServer},
    USR = {LUser, LServer, LResource},
    Session = #session{sid = SID,
                       usr = USR,
                       us = US,
                       priority = Priority,
                       info = Info},
    ejabberd_gen_sm:update_session(sm_backend(), LUser, LServer, LResource, Session).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_filter(From, To, Packet) ->
    {From, To, Packet}.

-spec do_route(Acc, From, To, Payload) -> Acc when
    Acc :: mongoose_acc:t(),
    From :: jid:jid(),
    To :: jid:jid(),
    Payload :: exml:element() | ejabberd_c2s:broadcast().
do_route(Acc, From, To, {broadcast, Payload} = Broadcast) ->
    ?DEBUG("from=~p, to=~p, broadcast=~p", [From, To, Broadcast]),
    #jid{ luser = LUser, lserver = LServer, lresource = LResource} = To,
    case LResource of
        <<>> ->
            CurrentPids = get_user_present_pids(LUser, LServer),
            Acc1 = ejabberd_hooks:run_fold(sm_broadcast, To#jid.lserver, Acc,
                                           [From, To, Broadcast, length(CurrentPids)]),
            ?DEBUG("bc_to=~p~n", [CurrentPids]),
            BCast = {broadcast, Payload},
            lists:foreach(fun({_, Pid}) -> Pid ! BCast end, CurrentPids),
            Acc1;
        _ ->
            case get_session_pid(To) of
                none ->
                    Acc; % do nothing
                Pid when is_pid(Pid) ->
                    ?DEBUG("sending to process ~p~n", [Pid]),
                    Pid ! Broadcast,
                    Acc
            end
    end;
do_route(Acc, From, To, El) ->
    ?DEBUG("session manager~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
           [From, To, Acc, 8]),
    #jid{lresource = LResource} = To,
    #xmlel{name = Name, attrs = Attrs} = El,
    case LResource of
        <<>> ->
            do_route_no_resource(Name, xml:get_attr_s(<<"type">>, Attrs),
                                 From, To, Acc, El);
        _ ->
            case get_session_pid(To) of
                none ->
                    do_route_offline(Name, xml:get_attr_s(<<"type">>, Attrs),
                                     From, To, Acc, El);
                Pid when is_pid(Pid) ->
                    ?DEBUG("sending to process ~p~n", [Pid]),
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
            Res = ejabberd_hooks:run_fold(
                        roster_in_subscription,
                        To#jid.lserver,
                        Acc,
                        [To#jid.user, To#jid.server, From, Type, Reason]),
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


-spec do_route_no_resource(Name, Type, From, To, Acc, El) -> Acc when
      Name :: undefined | binary(),
      Type :: any(),
      From :: jid:jid(),
      To :: jid:jid(),
      Acc :: mongoose_acc:t(),
      El :: exml:element().
do_route_no_resource(<<"presence">>, Type, From, To, Acc, El) ->
    case do_route_no_resource_presence(Type, From, To, Acc, El) of
        true ->
            PResources = get_user_present_resources(To),
            lists:foldl(fun({_, R}, A) ->
                            do_route(A, From, jid:replace_resource(To, R), El)
                        end,
                        Acc,
                        PResources);
        false ->
            Acc
    end;
do_route_no_resource(<<"message">>, _, From, To, Acc, El) ->
    route_message(From, To, Acc, El);
do_route_no_resource(<<"iq">>, _, From, To, Acc, El) ->
    process_iq(From, To, Acc, El);
do_route_no_resource(<<"broadcast">>, _, From, To, Acc, El) ->
    %% Backward compatibility
    ejabberd_hooks:run(sm_broadcast, To#jid.lserver, [From, To, Acc]),
    broadcast_packet(From, To, Acc, El);
do_route_no_resource(_, _, _, _, Acc, _) ->
    Acc.

-spec do_route_offline(Name, Type, From, To, Acc, Packet) -> mongoose_acc:t() when
      Name :: 'undefined' | binary(),
      Type :: binary(),
      From :: jid:jid(),
      To :: jid:jid(),
      Acc :: mongoose_acc:t(),
      Packet :: exml:element().
do_route_offline(<<"message">>, _, From, To, Acc, Packet)  ->
    Drop = ejabberd_hooks:run_fold(sm_filter_offline_message, To#jid.lserver,
                   false, [From, To, Packet]),
    case Drop of
        false ->
            route_message(From, To, Acc, Packet);
        true ->
            ?DEBUG("issue=\"message droped\", to=~1000p", [To]),
            Acc
    end;
do_route_offline(<<"iq">>, <<"error">>, _From, _To, Acc, _Packet) ->
    Acc;
do_route_offline(<<"iq">>, <<"result">>, _From, _To, Acc, _Packet) ->
    Acc;
do_route_offline(<<"iq">>, _, From, To, Acc, Packet) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, Packet, mongoose_xmpp_errors:service_unavailable()),
    ejabberd_router:route(To, From, Acc1, Err);
do_route_offline(_, _, _, _, Acc, _) ->
    ?DEBUG("packet droped~n", []),
    Acc.

%% Backward compatibility
-spec broadcast_packet(From :: jid:jid(),
                       To :: jid:jid(),
                       Acc :: mongoose_acc:t(),
                       El :: exml:element()) -> mongoose_acc:t().
broadcast_packet(From, To, Acc, El) ->
    lists:foldl(
      fun(A, R) ->
              do_route(A,
                       From,
                       jid:replace_resource(To, R),
                       El)
      end, Acc, get_user_resources(To)).

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
    User = To#jid.user,
    Server = To#jid.server,
    PrivacyList = ejabberd_hooks:run_fold(privacy_get_user_list, Server,
                                          #userlist{}, [User, Server]),
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
    User = To#jid.user,
    Server = To#jid.server,
    {_, Res} = mongoose_privacy:privacy_check_packet(Acc, Server, User, PrivacyList,
                                                     To, in),
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
            MessageType = xml:get_tag_attr_s(<<"type">>, Packet),
            route_message_by_type(MessageType, From, To, Acc, Packet)
    end.

route_message_by_type(<<"error">>, _From, _To, Acc, _Packet) ->
    Acc;
route_message_by_type(<<"groupchat">>, From, To, Acc, Packet) ->
    LServer = To#jid.lserver,
    ejabberd_hooks:run_fold(offline_groupchat_message_hook,
        LServer,
        Acc,
        [From, To, Packet]);
route_message_by_type(<<"headline">>, From, To, Acc, Packet) ->
    {stop, Acc1} = bounce_offline_message(Acc, From, To, Packet),
    Acc1;
route_message_by_type(_, From, To, Acc, Packet) ->
    LServer = To#jid.lserver,
    case ejabberd_auth:does_user_exist(To) of
        true ->
            case is_privacy_allow(From, To, Acc, Packet) of
                true ->
                    ejabberd_hooks:run_fold(offline_message_hook,
                        LServer,
                        Acc,
                        [From, To, Packet]);
                false ->
                    ejabberd_hooks:run_fold(failed_to_store_message,
                                            LServer,
                                            Acc,
                                            [From, Packet])
            end;
        _ ->
            {Acc1, Err} = jlib:make_error_reply(
                Acc, Packet, mongoose_xmpp_errors:service_unavailable()),
            ejabberd_router:route(To, From, Acc1, Err)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec clean_session_list([sid()]) -> [sid()].
clean_session_list(Ss) ->
    clean_session_list(lists:keysort(#session.usr, Ss), []).


-spec clean_session_list([sid()], [sid()]) -> [sid()].
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
      LUser :: jid:luser(),
      LServer :: jid:lserver().
get_user_present_pids(LUser, LServer) ->
    Ss = clean_session_list(ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer)),
    [{S#session.priority, element(2, S#session.sid)} || S <- Ss, is_integer(S#session.priority)].

-spec get_user_present_resources(jid:jid()) -> [{priority(), binary()}].
get_user_present_resources(#jid{luser = LUser, lserver = LServer}) ->
    Ss = ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer),
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
-spec check_for_sessions_to_replace(JID) -> ReplacedPids when
      JID :: jid:jid(),
      ReplacedPids :: [pid()].
check_for_sessions_to_replace(JID) ->
    #jid{luser = LUser, lserver = LServer, lresource = LResource} = JID,
    %% TODO: Depending on how this is executed, there could be an unneeded
    %% replacement for max_sessions. We need to check this at some point.
    ReplacedRedundantSessions = check_existing_resources(LUser, LServer, LResource),
    AllReplacedSessionPids = check_max_sessions(LUser, LServer, ReplacedRedundantSessions),
    [Pid ! replaced || Pid <- AllReplacedSessionPids],
    AllReplacedSessionPids.

-spec check_existing_resources(LUser, LServer, LResource) -> ReplacedSessionsPIDs when
      LUser :: 'error' | jid:luser() | tuple(),
      LServer :: 'error' | jid:lserver() | tuple(),
      LResource :: 'error' | jid:lresource() | [byte()] | tuple(),
      ReplacedSessionsPIDs :: ordsets:ordset(pid()).
check_existing_resources(LUser, LServer, LResource) ->
    %% A connection exist with the same resource. We replace it:
    Sessions = ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer, LResource),
    case [S#session.sid || S <- Sessions] of
        [] -> [];
        SIDs ->
            MaxSID = lists:max(SIDs),
            ordsets:from_list([Pid || {_, Pid} = S <- SIDs, S /= MaxSID])
    end.


-spec check_max_sessions(LUser :: jid:user(), LServer :: jid:server(),
                         ReplacedPIDs :: [pid()]) -> AllReplacedPIDs :: ordsets:ordset(pid()).
check_max_sessions(LUser, LServer, ReplacedPIDs) ->
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
                ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer)),
    MaxSessions = get_max_user_sessions(LUser, LServer),
    case length(SIDs) =< MaxSessions of
        true -> ordsets:to_list(ReplacedPIDs);
        false ->
            {_, Pid} = lists:min(SIDs),
            [Pid | ordsets:to_list(ReplacedPIDs)]
    end.


%% @doc Get the user_max_session setting
%% This option defines the max number of time a given users are allowed to
%% log in. Defaults to infinity
-spec get_max_user_sessions(LUser, Host) -> infinity | pos_integer() when
      LUser :: jid:user(),
      Host :: jid:server().
get_max_user_sessions(LUser, Host) ->
    case acl:match_rule(
           Host, max_user_sessions, jid:make_noprep(LUser, Host, <<>>)) of
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
        [{_, Module, Function}] ->
            case Module:Function(From, To, IQ) of
                {Acc1, ignore} -> Acc1;
                {Acc1, ResIQ} ->
                    ejabberd_router:route(To, From, Acc1,
                        jlib:iq_to_xml(ResIQ))
            end;
        [{_, Module, Function, Opts}] ->
            gen_iq_handler:handle(Host, Module, Function, Opts,
                                  From, To, Acc, IQ);
        [] ->
            {Acc1, Err} = jlib:make_error_reply(
                    Acc, Packet, mongoose_xmpp_errors:service_unavailable()),
            ejabberd_router:route(To, From, Acc1, Err)
    end;
process_iq(_, From, To, Acc, Packet) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, Packet, mongoose_xmpp_errors:bad_request()),
   ejabberd_router:route(To, From, Acc1, Err).


-spec force_update_presence({jid:user(), jid:server()}) -> 'ok'.
force_update_presence({LUser, LServer}) ->
    Ss = ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer),
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
    case mongoose_metrics:get_metric_value(global, ?UNIQUE_COUNT_CACHE) of
        {ok, DataPoints} ->
            proplists:get_value(value, DataPoints);
        _ ->
            0
    end.

-spec sm_backend() -> backend().
sm_backend() ->
    ejabberd_sm_backend:backend().

%%====================================================================
%% Deprecated API
%%====================================================================
open_session(SID, U, S, R, Info) ->
    mongoose_deprecations:log(
      {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
      "The function ejabberd_sm:open_session/5"
      " is deprecated, please use the #jid{} equivalent instead",
      [{log_level, warning}]),
    open_session(SID, jid:make(U, S, R), undefined, Info).

open_session(SID, U, S, R, Priority, Info) ->
    mongoose_deprecations:log(
      {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
      "The function ejabberd_sm:open_session/6"
      " is deprecated, please use the #jid{} equivalent instead",
      [{log_level, warning}]),
    open_session(SID, jid:make(U, S, R), Priority, Info).

close_session(Acc, SID, U, S, R, Reason) ->
    mongoose_deprecations:log(
      {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
      "The function ejabberd_sm:close_session/6"
      " is deprecated, please use the #jid{} equivalent instead",
      [{log_level, warning}]),
    close_session(Acc, SID, jid:make(U, S, R), Reason).

close_session_unset_presence(Acc, SID, U, S, R, Status, Reason) ->
    mongoose_deprecations:log(
      {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
      "The function ejabberd_sm:close_session_unset_presence/7"
      " is deprecated, please use the #jid{} equivalent instead",
      [{log_level, warning}]),
    close_session_unset_presence(Acc, SID, jid:make(U, S, R), Status, Reason).

unset_presence(Acc, SID, U, S, R, Status, Info) ->
    mongoose_deprecations:log(
      {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
      "The function ejabberd_sm:unset_presence/7"
      " is deprecated, please use the #jid{} equivalent instead",
      [{log_level, warning}]),
    unset_presence(Acc, SID, jid:make(U, S, R), Status, Info).

get_raw_sessions(U, S) ->
    mongoose_deprecations:log(
      {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
      "The function ejabberd_sm:get_raw_sessions/2"
      " is deprecated, please use the #jid{} equivalent instead",
      [{log_level, warning}]),
    get_raw_sessions(jid:make(U, S, <<>>)).

store_info(U, S, R, {Key, _Value} = KV) ->
    mongoose_deprecations:log(
      {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
      "The function ejabberd_sm:store_info/4"
      " is deprecated, please use the #jid{} equivalent instead",
      [{log_level, warning}]),
    store_info(jid:make(U, S, R), {Key, _Value} = KV).

set_presence(Acc, SID, U, S, R, Priority, Presence, Info) ->
    mongoose_deprecations:log(
      {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
      "The function ejabberd_sm:set_presence/8"
      " is deprecated, please use the #jid{} equivalent instead",
      [{log_level, warning}]),
    set_presence(Acc, SID, jid:make(U, S, R), Priority, Presence, Info).

remove_info(U, S, R, Key) ->
    mongoose_deprecations:log(
      {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
      "The function ejabberd_sm:remove_info/4"
      " is deprecated, please use the #jid{} equivalent instead",
      [{log_level, warning}]),
    remove_info(jid:make(U, S, R), Key).

get_user_resources(U, S) ->
    mongoose_deprecations:log(
      {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
      "The function ejabberd_sm:get_user_resources/2"
      " is deprecated, please use the #jid{} equivalent instead",
      [{log_level, warning}]),
    get_user_resources(jid:make(U, S, <<>>)).

get_session_pid(U, S, R) ->
    mongoose_deprecations:log(
      {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
      "The function ejabberd_sm:get_session_pid/3"
      " is deprecated, please use the #jid{} equivalent instead",
      [{log_level, warning}]),
    get_session_pid(jid:make(U, S, R)).

get_session(U, S, R) ->
    mongoose_deprecations:log(
      {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
      "The function ejabberd_sm:get_session/3"
      " is deprecated, please use the #jid{} equivalent instead",
      [{log_level, warning}]),
    get_session(jid:make(U, S, R)).

get_session_ip(U, S, R) ->
    mongoose_deprecations:log(
      {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
      "The function ejabberd_sm:get_session_ip/3"
      " is deprecated, please use the #jid{} equivalent instead",
      [{log_level, warning}]),
    get_session_ip(jid:make(U, S, R)).

get_user_present_resources(U, S) ->
    mongoose_deprecations:log(
      {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
      "The function ejabberd_sm:get_user_present_resources/2"
      " is deprecated, please use the #jid{} equivalent instead",
      [{log_level, warning}]),
    get_user_present_resources(jid:make(U, S, <<>>)).
