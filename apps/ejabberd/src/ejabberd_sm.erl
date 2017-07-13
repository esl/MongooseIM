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
         route/4,
         open_session/5, open_session/6,
         close_session/5,
         store_info/4,
         check_in_subscription/6,
         bounce_offline_message/4,
         disconnect_removed_user/3,
         get_user_resources/2,
         set_presence/8,
         unset_presence/7,
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
         get_raw_sessions/2,
         get_user_present_pids/2
        ]).

%% Hook handlers
-export([node_cleanup/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% xmpp_router callback
-export([do_filter/3]).
-export([do_route/4]).

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
-define(UNIQUE_COUNT_CACHE, [cache, unique_sessions_number]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok, Pid} | ignore | {error, Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
    mongoose_metrics:ensure_metric(global, ?UNIQUE_COUNT_CACHE, gauge),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec route(From, To, Packet) -> Acc when
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Packet :: jlib:xmlel() | mongoose_acc:t()| ejabberd_c2s:broadcast(),
      Acc :: mongoose_acc:t().
route(From, To, #xmlel{} = Packet) ->
    ?DEPRECATED, % used by MAM
%%    ?ERROR_MSG("Deprecated - it should be Acc: ~p", [Packet]),
    route(From, To, mongoose_acc:from_element(Packet, From, To));
route(From, To, {broadcast, Payload}) ->
    route(From, To, mongoose_acc:new(), {broadcast, Payload});
route(From, To, Acc) ->
    route(From, To, Acc, mongoose_acc:get(element, Acc)).

route(From, To, Acc, {broadcast, Payload}) ->
    case (catch do_route(Acc, From, To, {broadcast, Payload})) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("error when routing from=~ts to=~ts in module=~p~n~nreason=~p~n~n"
            "packet=~ts~n~nstack_trace=~p~n",
                [jid:to_binary(From), jid:to_binary(To),
                    ?MODULE, Reason, mongoose_acc:to_binary(Payload),
                    erlang:get_stacktrace()]);
        Acc1 -> Acc1
    end;
route(From, To, Acc, El) ->
    case (catch do_route(Acc, From, To, El)) of
        {'EXIT', Reason} ->
            ?ERROR_MSG("error when routing from=~ts to=~ts in module=~p~n~nreason=~p~n~n"
                       "packet=~ts~n~nstack_trace=~p~n",
                       [jid:to_binary(From), jid:to_binary(To),
                        ?MODULE, Reason, mongoose_acc:to_binary(Acc),
                        erlang:get_stacktrace()]);
        Acc1 -> Acc1
    end.

-spec open_session(SID, User, Server, Resource, Info) -> ReplacedPids when
      SID :: 'undefined' | sid(),
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Resource :: binary(),
      Info :: 'undefined' | [any()],
      ReplacedPids :: [pid()].
open_session(SID, User, Server, Resource, Info) ->
    open_session(SID, User, Server, Resource, undefined, Info).

-spec open_session(SID, User, Server, Resource, Priority, Info) -> ReplacedPids when
      SID :: 'undefined' | sid(),
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Resource :: binary(),
      Priority :: integer() | undefined,
      Info :: 'undefined' | [any()],
      ReplacedPids :: [pid()].
open_session(SID, User, Server, Resource, Priority, Info) ->
    set_session(SID, User, Server, Resource, Priority, Info),
    ReplacedPIDs = check_for_sessions_to_replace(User, Server, Resource),
    JID = jid:make(User, Server, Resource),
    ejabberd_hooks:run(sm_register_connection_hook, JID#jid.lserver,
                       [SID, JID, Info]),
    ReplacedPIDs.

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
    Info = case ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer, LResource) of
               [Session] ->
                   Session#session.info;
               _ ->
                   []
           end,
    ejabberd_gen_sm:delete_session(sm_backend(), SID, LUser, LServer, LResource),
    JID = jid:make(User, Server, Resource),
    ejabberd_hooks:run(sm_remove_connection_hook, JID#jid.lserver,
                       [SID, JID, Info, Reason]).

-spec store_info(ejabberd:user(), ejabberd:server(), ejabberd:resource(),
                 {any(), any()}) -> {ok, {any(), any()}} | {error, offline}.
store_info(User, Server, Resource, {Key, _Value} = KV) ->
    case get_session(User, Server, Resource) of
        offline -> {error, offline};
        {_SUser, SID, SPriority, SInfo} ->
            case SID of
                {_, Pid} when self() =:= Pid ->
                    %% It's safe to allow process update it's own record
                    set_session(SID, User, Server, Resource, SPriority,
                                lists:keystore(Key, 1, SInfo, KV)),
                    {ok, KV};
                {_, Pid} ->
                    %% Ask the process to update it's record itself
                    %% Async operation
                    ejabberd_c2s:store_session_info(Pid, User, Server, Resource, KV),
                    {ok, KV}
            end
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
            {stop, mongoose_acc:put(result, false, Acc)}
    end.

-spec bounce_offline_message(Acc, From, To, Packet) -> {stop, Acc} when
      Acc :: map(),
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Packet :: jlib:xmlel().
bounce_offline_message(Acc, #jid{server = Server} = From, To, Packet) ->
    Acc1 = ejabberd_hooks:run_fold(xmpp_bounce_message,
                            Server,
                            Acc,
                            []),
    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
    Acc2 = ejabberd_router:route(To, From, Acc1, Err),
    {stop, Acc2}.

-spec disconnect_removed_user(mongoose_acc:t(), User :: ejabberd:user(),
                              Server :: ejabberd:server()) -> mongoose_acc:t().
disconnect_removed_user(Acc, User, Server) ->
    ejabberd_sm:route(jid:make(<<>>, <<>>, <<>>),
                      jid:make(User, Server, <<>>),
                      Acc,
                      {broadcast, {exit, <<"User removed">>}}).


-spec get_user_resources(User :: ejabberd:user(), Server :: ejabberd:server()) -> [binary()].
get_user_resources(User, Server) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    Ss = ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer),
    [element(3, S#session.usr) || S <- clean_session_list(Ss)].


-spec get_session_ip(User, Server, Resource) -> undefined | {inet:ip_address(), integer()} when
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Resource :: ejabberd:resource().
get_session_ip(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),
    case ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer, LResource) of
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
-spec get_raw_sessions(ejabberd:user(), ejabberd:server()) -> [sm_session()].
get_raw_sessions(User, Server) ->
    clean_session_list(
      ejabberd_gen_sm:get_sessions(sm_backend(), jid:nodeprep(User), jid:nameprep(Server))).

-spec set_presence(Acc, SID, User, Server, Resource, Prio, Presence, Info) -> Acc1 when
      Acc :: mongoose_acc:t(),
      Acc1 :: mongoose_acc:t(),
      SID :: 'undefined' | sid(),
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Resource :: ejabberd:resource(),
      Prio :: 'undefined' | integer(),
      Presence :: any(),
      Info :: 'undefined' | [any()].
set_presence(Acc, SID, User, Server, Resource, Priority, Presence, Info) ->
    set_session(SID, User, Server, Resource, Priority, Info),
    ejabberd_hooks:run_fold(set_presence_hook, jid:nameprep(Server), Acc,
                       [User, Server, Resource, Presence]).


-spec unset_presence(Acc, SID, User, Server, Resource, Status, Info) -> Acc1 when
      Acc :: mongoose_acc:t(),
      Acc1 :: mongoose_acc:t(),
      SID :: 'undefined' | sid(),
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Resource :: ejabberd:resource(),
      Status :: any(),
      Info :: 'undefined' | [any()].
unset_presence(Acc, SID, User, Server, Resource, Status, Info) ->
    set_session(SID, User, Server, Resource, undefined, Info),
    LServer = jid:nameprep(Server),
    ejabberd_hooks:run_fold(unset_presence_hook, LServer, Acc,
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


-spec get_vh_session_number(ejabberd:server()) -> non_neg_integer().
get_vh_session_number(Server) ->
    length(ejabberd_gen_sm:get_sessions(sm_backend(), Server)).


-spec get_vh_session_list(ejabberd:server()) -> [ses_tuple()].
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
    maps:put(cleanup_result, Res, Acc).

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

    ejabberd_gen_sm:start(sm_backend(), Opts),

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
    ejabberd_gen_sm:create_session(sm_backend(), LUser, LServer, LResource, Session).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

do_filter(From, To, Packet) ->
    {From, To, Packet}.

-spec do_route(Acc, From, To, Payload) -> Acc when
    Acc :: mongoose_acc:t(),
    From :: ejabberd:jid(),
    To :: ejabberd:jid(),
    Payload :: jlib:xmlel() | ejabberd_c2s:broadcast().
do_route(Acc, From, To, {broadcast, Payload} = Broadcast) ->
    ?DEBUG("from=~p, to=~p, broadcast=~p", [From, To, Broadcast]),
    #jid{ luser = LUser, lserver = LServer, lresource = LResource} = To,
    case LResource of
        <<>> ->
            CurrentPids = get_user_present_pids(LUser, LServer),
            Acc1 = ejabberd_hooks:run_fold(sm_broadcast, To#jid.lserver, Acc,
                                           [From, To, Broadcast, length(CurrentPids)]),
            ?DEBUG("bc_to=~p~n", [CurrentPids]),
            BCast = {broadcast, mongoose_acc:strip(Acc1, Payload)},
            lists:foreach(fun({_, Pid}) -> Pid ! BCast end, CurrentPids),
            Acc1;
        _ ->
            case ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer, LResource) of
                [] ->
                    Acc; % do nothing
                Ss ->
                    Session = lists:max(Ss),
                    Pid = element(2, Session#session.sid),
                    ?DEBUG("sending to process ~p~n", [Pid]),
                    BCast = {broadcast, mongoose_acc:strip(Acc, Payload)},
                    Pid ! BCast,
                    Acc
            end
    end;
do_route(Acc, From, To, El) ->
    ?DEBUG("session manager~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
           [From, To, Acc, 8]),
    #jid{ luser = LUser, lserver = LServer, lresource = LResource} = To,
    #xmlel{name = Name, attrs = Attrs} = El,
    case LResource of
        <<>> ->
            do_route_no_resource(Name, xml:get_attr_s(<<"type">>, Attrs),
                                 From, To, Acc, El);
        _ ->
            case ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer, LResource) of
                [] ->
                    do_route_offline(Name, xml:get_attr_s(<<"type">>, Attrs),
                                     From, To, Acc, El);
                Ss ->
                    Session = lists:max(Ss),
                    Pid = element(2, Session#session.sid),
                    ?DEBUG("sending to process ~p~n", [Pid]),
                    Pid ! {route, From, To, mongoose_acc:strip(Acc, El)}
            end
    end,
    Acc.

-spec do_route_no_resource_presence_prv(From, To, Acc, Packet, Type, Reason) -> boolean() when
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Acc :: mongoose_acc:t(),
      Packet :: jlib:xmlel(),
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
            mongoose_acc:get(result, Res, false);
        false ->
            false
    end.

-spec do_route_no_resource_presence(Type, From, To, Acc, Packet) -> boolean() when
      Type :: binary(),
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Acc :: mongoose_acc:t(),
      Packet :: jlib:xmlel().
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


-spec do_route_no_resource(Name, Type, From, To, Acc, El) -> Result when
      Name :: undefined | binary(),
      Type :: any(),
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Acc :: mongoose_acc:t(),
      El :: xmlel(),
      Result ::ok | stop | todo | pid() | {error, lager_not_running} | {process_iq, _, _, _}.
do_route_no_resource(<<"presence">>, Type, From, To, Acc, El) ->
    case do_route_no_resource_presence(Type, From, To, Acc, El) of
        true ->
            PResources = get_user_present_resources(To#jid.luser, To#jid.lserver),
            lists:foreach(
              fun({_, R}) ->
                      do_route(Acc, From, jid:replace_resource(To, R), El)
              end, PResources);
        false ->
            ok
    end;
do_route_no_resource(<<"message">>, _, From, To, Acc, El) ->
    route_message(From, To, Acc, El);
do_route_no_resource(<<"iq">>, _, From, To, Acc, El) ->
    process_iq(From, To, Acc, El);
do_route_no_resource(<<"broadcast">>, _, From, To, Acc, El) ->
    %% Backward compatibility
    ejabberd_hooks:run(sm_broadcast, To#jid.lserver, [From, To, Acc]),
    broadcast_packet(From, To, Acc, El);
do_route_no_resource(_, _, _, _, _, _) ->
    ok.

-spec do_route_offline(Name, Type, From, To, Acc, Packet) -> ok | stop when
      Name :: 'undefined' | binary(),
      Type :: binary(),
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Acc :: mongoose_acc:t(),
      Packet :: jlib:xmlel().
do_route_offline(<<"message">>, _, From, To, Acc, Packet)  ->
    Drop = ejabberd_hooks:run_fold(sm_filter_offline_message, To#jid.lserver,
                   false, [From, To, Packet]),
    case Drop of
        false ->
            route_message(From, To, Acc, Packet);
        true ->
            ?DEBUG("issue=\"message droped\", to=~1000p", [To]),
            ok
    end;
do_route_offline(<<"iq">>, <<"error">>, _From, _To, _Acc, _Packet) ->
    ok;
do_route_offline(<<"iq">>, <<"result">>, _From, _To, _Acc, _Packet) ->
    ok;
do_route_offline(<<"iq">>, _, From, To, Acc, Packet) ->
    Err = jlib:make_error_reply(Packet, ?ERR_SERVICE_UNAVAILABLE),
    ejabberd_router:route(To, From, Acc, Err);
do_route_offline(_, _, _, _, _, _) ->
    ?DEBUG("packet droped~n", []),
    ok.

%% Backward compatibility
-spec broadcast_packet(From :: ejabberd:jid(),
                       To :: ejabberd:jid(),
                       Acc :: mongoose_acc:t(),
                       El :: xmlel()) -> ok.
broadcast_packet(From, To, Acc, El) ->
    #jid{user = User, server = Server} = To,
    lists:foreach(
      fun(R) ->
              do_route(Acc,
                       From,
                       jid:replace_resource(To, R),
                       El)
      end, get_user_resources(User, Server)).

%% @doc The default list applies to the user as a whole,
%% and is processed if there is no active list set
%% for the target session/resource to which a stanza is addressed,
%% or if there are no current sessions for the user.
-spec is_privacy_allow(From, To, Acc, Packet) -> boolean() when
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Acc :: mongoose_acc:t(),
      Packet :: jlib:xmlel() | mongoose_acc:t().
is_privacy_allow(From, To, Acc, Packet) ->
    User = To#jid.user,
    Server = To#jid.server,
    PrivacyList = ejabberd_hooks:run_fold(privacy_get_user_list, Server,
                                          #userlist{}, [User, Server]),
    is_privacy_allow(From, To, Acc, Packet, PrivacyList).


%% @doc Check if privacy rules allow this delivery
%% Function copied from ejabberd_c2s.erl
-spec is_privacy_allow(From, To, Acc, Packet, PrivacyList) -> boolean() when
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Acc :: mongoose_acc:t(),
      Packet :: jlib:xmlel(),
      PrivacyList :: mongoose_privacy:userlist().
is_privacy_allow(_From, To, Acc, _Packet, PrivacyList) ->
    User = To#jid.user,
    Server = To#jid.server,
    {_, Res} = mongoose_privacy:privacy_check_packet(Acc, Server, User, PrivacyList,
                                                     To, in),
    allow == Res.


-spec route_message(From, To, Acc, Packet) -> Res when
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Acc :: mongoose_acc:t(),
      Packet :: xmlel(),
      Res :: ok | stop | mongoose_acc:t() | {stop, mongoose_acc:t()}.
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
                      Pid ! {route, From, To, mongoose_acc:strip(Acc)};
                 %% Ignore other priority:
                 ({_Prio, _Pid}) ->
                      ok
              end,
              PrioPid);
        _ ->
            MessageType = xml:get_tag_attr_s(<<"type">>, Packet),
            route_message_by_type(MessageType, From, To, Acc, Packet)
    end.

route_message_by_type(<<"error">>, _From, _To, _Acc, _Packet) ->
    ok;
route_message_by_type(<<"groupchat">>, From, To, Acc, Packet) ->
    LServer = To#jid.lserver,
    ejabberd_hooks:run_fold(offline_groupchat_message_hook,
        LServer,
        Acc,
        [From, To, Packet]);
route_message_by_type(<<"headline">>, From, To, Acc, Packet) ->
    bounce_offline_message(Acc, From, To, Packet);
route_message_by_type(_, From, To, Acc, Packet) ->
    LUser = To#jid.luser,
    LServer = To#jid.lserver,
    case ejabberd_auth:is_user_exists(LUser, LServer) of
        true ->
            case is_privacy_allow(From, To, Acc, Packet) of
                true ->
                    ejabberd_hooks:run_fold(offline_message_hook,
                        LServer,
                        Acc,
                        [From, To, Packet]);
                false ->
                    ejabberd_hooks:run_fold(failed_to_store_message,
                        LServer, Packet, [From]),
                    ok
            end;
        _ ->
            Err = jlib:make_error_reply(
                Packet, ?ERR_SERVICE_UNAVAILABLE),
            ejabberd_router:route(To, From, Acc, Err)
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
      LUser :: ejabberd:luser(),
      LServer :: ejabberd:lserver().
get_user_present_pids(LUser, LServer) ->
    Ss = clean_session_list(ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer)),
    [{S#session.priority, element(2, S#session.sid)} || S <- Ss, is_integer(S#session.priority)].

-spec get_user_present_resources(LUser :: ejabberd:user(),
                                 LServer :: ejabberd:server()
                                ) -> [{priority(), binary()}].
get_user_present_resources(LUser, LServer) ->
    Ss = ejabberd_gen_sm:get_sessions(sm_backend(), LUser, LServer),
    [{S#session.priority, element(3, S#session.usr)} ||
        S <- clean_session_list(Ss), is_integer(S#session.priority)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc On new session, check if some existing connections need to be replace
-spec check_for_sessions_to_replace(User, Server, Resource) -> ReplacedPids when
      User :: ejabberd:user(),
      Server :: ejabberd:server(),
      Resource :: ejabberd:resource(),
      ReplacedPids :: [pid()].
check_for_sessions_to_replace(User, Server, Resource) ->
    LUser = jid:nodeprep(User),
    LServer = jid:nameprep(Server),
    LResource = jid:resourceprep(Resource),

    %% TODO: Depending on how this is executed, there could be an unneeded
    %% replacement for max_sessions. We need to check this at some point.
    ReplacedRedundantSessions = check_existing_resources(LUser, LServer, LResource),
    AllReplacedSessionPids = check_max_sessions(LUser, LServer, ReplacedRedundantSessions),
    [Pid ! replaced || Pid <- AllReplacedSessionPids],
    AllReplacedSessionPids.

-spec check_existing_resources(LUser, LServer, LResource) -> ReplacedSessionsPIDs when
      LUser :: 'error' | ejabberd:luser() | tuple(),
      LServer :: 'error' | ejabberd:lserver() | tuple(),
      LResource :: 'error' | ejabberd:lresource() | [byte()] | tuple(),
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


-spec check_max_sessions(LUser :: ejabberd:user(), LServer :: ejabberd:server(),
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

-spec process_iq(From, To, Acc, Packet) -> Result when
      From :: ejabberd:jid(),
      To :: ejabberd:jid(),
      Acc :: mongoose_acc:t(),
      Packet :: jlib:xmlel(),
      Result :: ok | todo | pid() | {error, lager_not_running} | {process_iq, _, _, _}.
process_iq(From, To, Acc, Packet) ->
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
                    ejabberd_router:route(To, From, Acc, Err)
            end;
        reply ->
            ok;
        _ ->
            Err = jlib:make_error_reply(Packet, ?ERR_BAD_REQUEST),
            ejabberd_router:route(To, From, Acc, Err),
            ok
    end.


-spec force_update_presence({binary(), ejabberd:server()}) -> 'ok'.
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
    case mongoose_metrics:get_metric_value(global, ?UNIQUE_COUNT_CACHE) of
        {ok, DataPoints} ->
            proplists:get_value(value, DataPoints);
        _ ->
            0
    end.

-spec sm_backend() -> backend().
sm_backend() ->
    ejabberd_sm_backend:backend().
