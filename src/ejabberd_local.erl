%%%----------------------------------------------------------------------
%%% File    : ejabberd_local.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Route local packets
%%% Created : 30 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

%%%----------------------------------------------------------------------
%%% FIXME: the code in this module uses Host term to identify domain
%%% name, not a host type.
%%%----------------------------------------------------------------------

-module(ejabberd_local).
-author('alexey@process-one.net').

-behaviour(gen_server).
-behaviour(mongoose_packet_handler).
-behaviour(gen_iq_component).

%% API
-export([start_link/0]).

-export([process_packet/5,
         route_iq/5,
         route_iq/6,
         process_iq_reply/4,
         register_iq_handler/3,
         register_host/1,
         unregister_iq_handler/2,
         unregister_host/1,
         sync/0
        ]).

%% RPC callbacks
-export([get_iq_callback/1]).

%% Hooks callbacks

-export([disco_local_features/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([do_route/4]).

-ignore_xref([do_route/4, get_iq_callback/1,
              process_iq_reply/4, start_link/0]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("session.hrl").

-record(state, {}).

-type id() :: binary().
-type callback() :: fun((From :: jid:jid(), To :: jid:jid(),
                         Acc :: mongoose_acc:t(), IQ :: jlib:iq()) ->
                             mongoose_acc:t()) |
                    fun((From :: undefined, To :: undefined,
                         Acc :: undefined, IQ :: timeout) ->
                             undefined).

-define(IQTABLE, local_iqtable).
-define(NSTABLE, local_nstable).
-define(IQRESPONSE, local_iqresponse).

%% This value is used in SIP and Megaco for a transaction lifetime.
-define(IQ_TIMEOUT, 32000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok, Pid} | ignore | {error, Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec process_iq(Acc :: mongoose_acc:t(),
                 From :: jid:jid(),
                 To :: jid:jid(),
                 El :: exml:element()
                 ) -> mongoose_acc:t().
process_iq(Acc0, From, To, El) ->
    {IQ, Acc} = mongoose_iq:info(Acc0),
    process_iq(IQ, Acc, From, To, El).

process_iq(#iq{ type = Type } = IQReply, Acc, From, To, _El)
  when Type == result; Type == error ->
    process_iq_reply(From, To, Acc, IQReply);
process_iq(#iq{ xmlns = XMLNS } = IQ, Acc, From, To, _El) ->
    Host = To#jid.lserver,
    case ets:lookup(?IQTABLE, {XMLNS, Host}) of
        [{_, IQHandler}] ->
            gen_iq_component:handle(IQHandler, Acc, From, To, IQ);
        [] ->
            T = <<"Local server does not implement this feature">>,
            ejabberd_router:route_error_reply(To, From, Acc,
                mongoose_xmpp_errors:feature_not_implemented(<<"en">>, T))
    end;
process_iq(_, Acc, From, To, El) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, El, mongoose_xmpp_errors:bad_request()),
    ejabberd_router:route(To, From, Acc1, Err).

-spec process_iq_reply(From :: jid:jid(),
                       To :: jid:jid(),
                       Acc :: mongoose_acc:t(),
                       IQ :: jlib:iq()) -> mongoose_acc:t().
process_iq_reply(From, To, Acc, #iq{id = ID} = IQ) ->
    case get_iq_callback_in_cluster(ID, Acc) of
        {ok, Callback} ->
            Callback(From, To, Acc, IQ);
        _ ->
            Acc
    end.

-spec get_iq_callback_in_cluster(id(), mongoose_acc:t()) ->
        {ok, callback()} | {error, term()}.
get_iq_callback_in_cluster(ID, Acc) ->
    %% We store information from which node the request is originating in the ID
    case parse_iq_id(ID) of
        local_node ->
            get_iq_callback(ID);
        {remote_node, NodeName} ->
            rpc:call(NodeName, ?MODULE, get_iq_callback, [ID]);
        {error, Reason} ->
            ?LOG_ERROR(#{what => parse_iq_id_failed,
                         reason => Reason, acc => Acc}),
            {error, Reason}
    end.

-spec process_packet(Acc :: mongoose_acc:t(),
                     From :: jid:jid(),
                     To ::jid:jid(),
                     El :: exml:element(),
                     Extra :: map()) -> mongoose_acc:t().
process_packet(Acc, From, To, El, _Extra) ->
    try
        do_route(Acc, From, To, El)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => routing_error, acc => Acc,
                         class => Class, reason => Reason, stacktrace => Stacktrace}),
            Acc
    end.

-spec route_iq(From :: jid:jid(),
               To :: jid:jid(),
               Acc :: mongoose_acc:t(),
               IQ :: jlib:iq(),
               F :: fun()) -> mongoose_acc:t().
route_iq(From, To, Acc, IQ, F) ->
    route_iq(From, To, Acc, IQ, F, undefined).


%% Send an iq and wait for response
%% This function is used to route IQs from the server to the client.
%% A callback function would be called once a response is received from the client.
-spec route_iq(From :: jid:jid(),
               To :: jid:jid(),
               Acc :: mongoose_acc:t(),
               IQ :: jlib:iq(),
               Callback :: callback(),
               Timeout :: undefined | integer()) -> mongoose_acc:t().
route_iq(From, To, Acc, #iq{type = Type} = IQ, Callback, Timeout)
  when is_function(Callback) ->
    Packet = case Type == set orelse Type == get of
                true ->
                     ID = make_iq_id(),
                     register_iq_response_handler(ID, Callback, Timeout),
                     jlib:iq_to_xml(IQ#iq{id = ID});
                false ->
                     jlib:iq_to_xml(IQ)
             end,
    ejabberd_router:route(From, To, Acc, Packet).

-spec register_iq_response_handler(
           ID :: id(),
           Callback :: callback(),
           Timeout :: undefined | pos_integer()) -> any().
register_iq_response_handler(ID, Callback, Timeout0) ->
    Timeout = case Timeout0 of
                  undefined ->
                      ?IQ_TIMEOUT;
                  N when is_integer(N), N > 0 ->
                      N
              end,
    TRef = erlang:start_timer(Timeout, ejabberd_local, ID),
    ets:insert(?IQRESPONSE, {ID, Callback, TRef}).

-spec register_iq_handler(Domain :: jid:server(), Namespace :: binary(),
                          IQHandler :: mongoose_iq_handler:t()) -> ok.
register_iq_handler(Domain, XMLNS, IQHandler) ->
    ejabberd_local ! {register_iq_handler, Domain, XMLNS, IQHandler},
    ok.

-spec sync() -> ok.
sync() ->
    gen_server:call(ejabberd_local, sync).

-spec unregister_iq_handler(Domain :: jid:server(), Namespace :: binary()) -> ok.
unregister_iq_handler(Domain, XMLNS) ->
    ejabberd_local ! {unregister_iq_handler, Domain, XMLNS},
    ok.

-spec bounce_resource_packet(Acc :: mongoose_acc:t(),
                             From :: jid:jid(),
                             To :: jid:jid(),
                             El :: exml:element()) -> mongoose_acc:t().
bounce_resource_packet(Acc, From, To, El) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, El, mongoose_xmpp_errors:item_not_found()),
    ejabberd_router:route(To, From, Acc1, Err),
    Acc.

-spec register_host(Host :: jid:server()) -> ok.
register_host(Host) ->
    gen_server:call(?MODULE, {register_host, Host}).

-spec unregister_host(Host :: jid:server()) -> ok.
unregister_host(Host) ->
    gen_server:call(?MODULE, {unregister_host, Host}).

-spec disco_local_features(mongoose_disco:feature_acc(),
                           map(),
                           map()) -> {ok, mongoose_disco:feature_acc()}.
disco_local_features(Acc = #{to_jid := #jid{lserver = LServer}, node := <<>>}, _, _) ->
    Features = [Feature || {_, Feature} <- ets:lookup(?NSTABLE, LServer)],
    {ok, mongoose_disco:add_features(Features, Acc)};
disco_local_features(Acc, _, _) ->
    {ok, Acc}.

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
init([]) ->
    catch ets:new(?IQTABLE, [named_table, protected, {read_concurrency, true}]),
    catch ets:new(?NSTABLE, [named_table, bag, protected, {read_concurrency, true}]),
    catch ets:new(?IQRESPONSE, [named_table, public]),
    gen_hook:add_handlers(hooks()),
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
handle_call({unregister_host, Host}, _From, State) ->
    Node = node(),
    [mongoose_c2s:stop(Pid, host_was_unregistered)
     || #session{sid = {_, Pid}} <- ejabberd_sm:get_vh_session_list(Host),
        node(Pid) =:= Node],
    do_unregister_host(Host),
    {reply, ok, State};
handle_call({register_host, Host}, _From, State) ->
    do_register_host(Host),
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
handle_info({route, Acc, From, To, El}, State) ->
    spawn(fun() -> process_packet(Acc, From, To, El, #{}) end),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, IQHandler}, State) ->
    ets:insert(?NSTABLE, {Host, XMLNS}),
    ets:insert(?IQTABLE, {{XMLNS, Host}, IQHandler}),
    {noreply, State};
handle_info({unregister_iq_handler, Host, XMLNS}, State) ->
    case ets:lookup(?IQTABLE, {XMLNS, Host}) of
        [{_, IQHandler}] ->
            gen_iq_component:stop_iq_handler(IQHandler),
            ets:delete_object(?NSTABLE, {Host, XMLNS}),
            ets:delete(?IQTABLE, {XMLNS, Host});
        _ ->
            ok
    end,
    {noreply, State};
handle_info({timeout, _TRef, ID}, State) ->
    process_iq_timeout(ID),
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
terminate(_Reason, _State) ->
    gen_hook:delete_handlers(hooks()).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

hooks() ->
    [{disco_local_features, HostType, fun ?MODULE:disco_local_features/3, #{}, 99}
     || HostType <- ?ALL_HOST_TYPES].

-spec do_route(Acc :: mongoose_acc:t(),
               From :: jid:jid(),
               To :: jid:jid(),
               El :: exml:element()) -> mongoose_acc:t().
do_route(Acc, From, To, El) ->
    ?LOG_DEBUG(#{what => local_routing, acc => Acc}),
    case directed_to(To) of
        user ->
            ejabberd_sm:route(From, To, Acc, El);
        server ->
            case El#xmlel.name of
                <<"iq">> ->
                    process_iq(Acc, From, To, El);
                _ ->
                    Acc
            end;
        local_resource ->
            case mongoose_acc:stanza_type(Acc) of
                <<"error">> -> Acc;
                <<"result">> -> Acc;
                _ -> bounce_resource_packet(Acc, From, To, El)
            end
    end.

-spec directed_to(jid:jid()) -> user | server | local_resource.
directed_to(To) ->
    directed_to(To#jid.luser, To#jid.lresource).

directed_to(<<>>, <<>>) ->
    server;
directed_to(<<>>, _) ->
    local_resource;
directed_to(_, _) ->
    user.

-spec get_iq_callback(ID :: id()) -> error | {ok, fun()}.
get_iq_callback(ID) ->
    case ets:lookup(?IQRESPONSE, ID) of
        [{ID, Callback, TRef}] ->
            erlang:cancel_timer(TRef),
            ets:delete(?IQRESPONSE, ID),
            {ok, Callback};
        _ ->
            error
    end.

-spec process_iq_timeout(id()) -> ok.
process_iq_timeout(ID) ->
    spawn(fun() -> process_iq_timeout2(ID) end), ok.

-spec process_iq_timeout2(id()) -> ok.
process_iq_timeout2(ID) ->
    case get_iq_callback(ID) of
        {ok, Function} ->
            Function(undefined, undefined, undefined, timeout), ok;
        _ ->
            ok
    end.

do_register_host(Host) ->
    mongoose_router:register_route(Host, mongoose_packet_handler:new(?MODULE)).

do_unregister_host(Host) ->
    mongoose_router:unregister_route(Host).

make_iq_id() ->
    %% Attach NodeId, so we know to which node to forward the response
    BinNodeId = mongoose_start_node_id:node_id(),
    Rand = mongoose_bin:gen_from_crypto(),
    <<BinNodeId/binary, "_", Rand/binary>>.

%% Parses ID, made by make_iq_id function
-spec parse_iq_id(ID :: binary()) ->
    local_node | {remote_node, node()}
    | {error, {unknown_node_id, term()} | bad_iq_format}.
parse_iq_id(ID) ->
    BinNodeId = mongoose_start_node_id:node_id(),
    case binary:split(ID, <<"_">>) of
        [BinNodeId, _Rest] ->
            local_node;
        [OtherBinNodeId, _Rest] ->
            case mongoose_start_node_id:node_id_to_name(OtherBinNodeId) of
                {ok, NodeName} ->
                    {remote_node, NodeName};
                {error, Reason} ->
                    {error, {unknown_node_id, Reason}}
            end;
        _ ->
            {error, bad_iq_format}
    end.
