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

-module(ejabberd_local).
-author('alexey@process-one.net').

-behaviour(gen_server).
-behaviour(mongoose_packet_handler).

%% API
-export([start_link/0]).

-export([process_packet/5,
         route_iq/5,
         route_iq/6,
         process_iq_reply/4,
         register_iq_handler/4,
         register_iq_handler/5,
         register_host/1,
         register_iq_response_handler/4,
         register_iq_response_handler/5,
         unregister_iq_handler/2,
         unregister_host/1,
         unregister_iq_response_handler/2,
         refresh_iq_handlers/0,
         bounce_resource_packet/4
        ]).

%% Hooks callbacks

-export([node_cleanup/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([do_route/4]).

%% For testing only
-export([get_iq_callback/1]).


-include("mongoose.hrl").
-include("jlib.hrl").

-record(state, {}).

-type id() :: any().
-record(iq_response, {id :: id(),
                      module,
                      function,
                      timer}).

-define(IQTABLE, local_iqtable).

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
        [{_, Module, Function}] ->
            case Module:Function(From, To, IQ) of
                {Acc1, ignore} -> Acc1;
                {Acc1, ResIQ} -> ejabberd_router:route(To, From, Acc1, jlib:iq_to_xml(ResIQ))
            end;
        [{_, Module, Function, Opts}] ->
            gen_iq_handler:handle(Host, Module, Function, Opts,
                                  From, To, Acc, IQ);
        [] ->
            ejabberd_router:route_error_reply(To, From, Acc, mongoose_xmpp_errors:feature_not_implemented())
    end;
process_iq(_, Acc, From, To, El) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, El, mongoose_xmpp_errors:bad_request()),
    ejabberd_router:route(To, From, Acc1, Err).

-spec process_iq_reply(From :: jid:jid(),
                       To :: jid:jid(),
                       mongoose_acc:t(),
                       IQ :: jlib:iq() ) -> mongoose_acc:t().
process_iq_reply(From, To, Acc, #iq{id = ID} = IQ) ->
    case get_iq_callback(ID) of
        {ok, undefined, Function} ->
            Function(From, To, Acc, IQ);
        {ok, Module, Function} ->
            Module:Function(From, To, Acc, IQ);
        _ ->
            Acc
    end.


-spec process_packet(Acc :: mongoose_acc:t(), From :: jid:jid(), To ::jid:jid(), El :: exml:element(),
                     Extra :: any()) ->
    ok | {error, lager_not_running}.
process_packet(Acc, From, To, El, _Extra) ->
    try
        do_route(Acc, From, To, El)
    catch
        _:Reason:StackTrace ->
            ?ERROR_MSG("event=routing_error,from=~ts,to=~ts,module=~p,"
                       "reason=~p,packet=~ts,stack_trace=~p",
                       [jid:to_binary(From), jid:to_binary(To),
                        ?MODULE, Reason, exml:to_binary(mongoose_acc:element(Acc)),
                        StackTrace])
    end.

-spec route_iq(From :: jid:jid(),
               To :: jid:jid(),
               Acc :: mongoose_acc:t(),
               IQ :: jlib:iq(),
               F :: fun()) -> mongoose_acc:t().
route_iq(From, To, Acc, IQ, F) ->
    route_iq(From, To, Acc, IQ, F, undefined).


-spec route_iq(From :: jid:jid(),
               To :: jid:jid(),
               Acc :: mongoose_acc:t(),
               IQ :: jlib:iq(),
               F :: fun(),
               Timeout :: undefined | integer()) -> mongoose_acc:t().
route_iq(From, To, Acc, #iq{type = Type} = IQ, F, Timeout) when is_function(F) ->
    Packet = case Type == set orelse Type == get of
                true ->
                     ID = mongoose_bin:gen_from_crypto(),
                     Host = From#jid.lserver,
                     register_iq_response_handler(Host, ID, undefined, F, Timeout),
                     jlib:iq_to_xml(IQ#iq{id = ID});
                false ->
                     jlib:iq_to_xml(IQ)
             end,
    ejabberd_router:route(From, To, Acc, Packet).

register_iq_response_handler(Host, ID, Module, Function) ->
    register_iq_response_handler(Host, ID, Module, Function, undefined).

-spec register_iq_response_handler(_Host :: jid:server(),
                               ID :: id(),
                               Module :: atom(),
                               Function :: fun(),
                               Timeout :: 'undefined' | pos_integer()) -> any().
register_iq_response_handler(_Host, ID, Module, Function, Timeout0) ->
    Timeout = case Timeout0 of
                  undefined ->
                      ?IQ_TIMEOUT;
                  N when is_integer(N), N > 0 ->
                      N
              end,
    TRef = erlang:start_timer(Timeout, ejabberd_local, ID),
    mnesia:dirty_write(#iq_response{id = ID,
                                    module = Module,
                                    function = Function,
                                    timer = TRef}).

-spec register_iq_handler(Host :: jid:server(),
                          XMLNS :: binary(),
                          Module :: atom(),
                          Function :: fun()) -> {register_iq_handler, _, _, _, _}.
register_iq_handler(Host, XMLNS, Module, Fun) ->
    ejabberd_local ! {register_iq_handler, Host, XMLNS, Module, Fun}.

-spec register_iq_handler(Host :: jid:server(),
                          XMLNS :: binary(),
                          Module :: atom(),
                          Function :: fun(),
                          Opts :: [any()]) -> {register_iq_handler, _, _, _, _, _}.
register_iq_handler(Host, XMLNS, Module, Fun, Opts) ->
    ejabberd_local ! {register_iq_handler, Host, XMLNS, Module, Fun, Opts}.

-spec unregister_iq_response_handler(_Host :: jid:server(),
                                     ID :: id()) -> 'ok'.
unregister_iq_response_handler(_Host, ID) ->
    catch get_iq_callback(ID),
    ok.

-spec unregister_iq_handler(Host :: jid:server(),
                           XMLNS :: binary()) -> {unregister_iq_handler, _, _}.
unregister_iq_handler(Host, XMLNS) ->
    ejabberd_local ! {unregister_iq_handler, Host, XMLNS}.

refresh_iq_handlers() ->
    ejabberd_local ! refresh_iq_handlers.

-spec bounce_resource_packet(Acc:: mongoose_acc:t(),
                             From :: jid:jid(),
                             To :: jid:jid(),
                             El :: exml:element()) -> {'stop', mongoose_acc:t()}.
bounce_resource_packet(Acc, From, To, El) ->
    {Acc1, Err} = jlib:make_error_reply(Acc, El, mongoose_xmpp_errors:item_not_found()),
    ejabberd_router:route(To, From, Acc1, Err),
    {stop, Acc}.

-spec register_host(Host :: jid:server()) -> ok.
register_host(Host) ->
    gen_server:call(?MODULE, {register_host, Host}).

-spec unregister_host(Host :: jid:server()) -> ok.
unregister_host(Host) ->
    gen_server:call(?MODULE, {unregister_host, Host}).

%%====================================================================
%% API
%%====================================================================

node_cleanup(Acc, Node) ->
    F = fun() ->
                Keys = mnesia:select(
                         iq_response,
                         [{#iq_response{timer = '$1', id = '$2', _ = '_'},
                           [{'==', {node, '$1'}, Node}],
                           ['$2']}]),
                lists:foreach(fun(Key) ->
                                      mnesia:delete({iq_response, Key})
                              end, Keys)
        end,
    Res = mnesia:async_dirty(F),
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
init([]) ->
    lists:foreach(fun do_register_host/1, ?MYHOSTS),
    catch ets:new(?IQTABLE, [named_table, public]),
    update_table(),
    mnesia:create_table(iq_response,
                        [{ram_copies, [node()]},
                         {attributes, record_info(fields, iq_response)}]),
    mnesia:add_table_copy(iq_response, node(), ram_copies),
    ejabberd_hooks:add(node_cleanup, global, ?MODULE, node_cleanup, 50),
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
    [ejabberd_c2s:stop(Pid)
     || {_, {_, Pid}, _, _} <- ejabberd_sm:get_vh_session_list(Host)],
    do_unregister_host(Host),
    mongoose_metrics:remove_host_metrics(Host),
    {reply, ok, State};
handle_call({register_host, Host}, _From, State) ->
    do_register_host(Host),
    mongoose_metrics:init_predefined_host_metrics(Host),
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
    process_packet(Acc, From, To, El, undefined),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module, Function}, State) ->
    ets:insert(?IQTABLE, {{XMLNS, Host}, Module, Function}),
    catch mod_disco:register_feature(Host, XMLNS),
    {noreply, State};
handle_info({register_iq_handler, Host, XMLNS, Module, Function, Opts}, State) ->
    ets:insert(?IQTABLE, {{XMLNS, Host}, Module, Function, Opts}),
    catch mod_disco:register_feature(Host, XMLNS),
    {noreply, State};
handle_info({unregister_iq_handler, Host, XMLNS}, State) ->
    case ets:lookup(?IQTABLE, {XMLNS, Host}) of
        [{_, Module, Function, Opts}] ->
            gen_iq_handler:stop_iq_handler(Module, Function, Opts);
        _ ->
            ok
    end,
    ets:delete(?IQTABLE, {XMLNS, Host}),
    catch mod_disco:unregister_feature(Host, XMLNS),
    {noreply, State};
handle_info(refresh_iq_handlers, State) ->
    lists:foreach(
      fun(T) ->
              case T of
                  {{XMLNS, Host}, _Module, _Function, _Opts} ->
                      catch mod_disco:register_feature(Host, XMLNS);
                  {{XMLNS, Host}, _Module, _Function} ->
                      catch mod_disco:register_feature(Host, XMLNS);
                  _ ->
                      ok
              end
      end, ets:tab2list(?IQTABLE)),
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
    ejabberd_hooks:delete(node_cleanup, global, ?MODULE, node_cleanup, 50),
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
-spec do_route(Acc :: mongoose_acc:t(),
               From :: jid:jid(),
               To :: jid:jid(),
               El :: mongoose_acc:t()) -> mongoose_acc:t().
do_route(Acc, From, To, El) ->
    ?DEBUG("local route~n\tfrom ~p~n\tto ~p~n\tpacket ~P~n",
           [From, To, El, 8]),
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
                _ ->
                    ejabberd_hooks:run_fold(local_send_to_resource_hook,
                                            To#jid.lserver,
                                            Acc,
                                            [From, To, El])
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

-spec update_table() -> ok | {atomic|aborted, _}.
update_table() ->
    case catch mnesia:table_info(iq_response, attributes) of
        [id, module, function] ->
            mnesia:delete_table(iq_response);
        [id, module, function, timer] ->
            ok;
        {'EXIT', _} ->
            ok
    end.

-spec get_iq_callback(ID :: id()) -> 'error' | {'ok', Mod :: atom(), fun() | atom()}.
get_iq_callback(ID) ->
    case mnesia:dirty_read(iq_response, ID) of
        [#iq_response{module = Module, timer = TRef,
                      function = Function}] ->
            cancel_timer(TRef),
            mnesia:dirty_delete(iq_response, ID),
            {ok, Module, Function};
        _ ->
            error
    end.

-spec process_iq_timeout(id()) -> id().
process_iq_timeout(ID) ->
    spawn(fun process_iq_timeout/0) ! ID.

-spec process_iq_timeout() -> ok | any().
process_iq_timeout() ->
    receive
        ID ->
            case get_iq_callback(ID) of
                {ok, undefined, Function} ->
                    Function(undefined, undefined, undefined, timeout);
                _ ->
                    ok
            end
    after 5000 ->
            ok
    end.

-spec cancel_timer(reference()) -> 'ok'.
cancel_timer(TRef) ->
    case erlang:cancel_timer(TRef) of
        false ->
            receive
                {timeout, TRef, _} ->
                    ok
            after 0 ->
                    ok
            end;
        _ ->
            ok
    end.

do_register_host(Host) ->
    ejabberd_router:register_route(Host, mongoose_packet_handler:new(?MODULE)),
    ejabberd_hooks:add(local_send_to_resource_hook, Host,
                       ?MODULE, bounce_resource_packet, 100).

do_unregister_host(Host) ->
    ejabberd_router:unregister_route(Host),
    ejabberd_hooks:delete(local_send_to_resource_hook, Host,
                          ?MODULE, bounce_resource_packet, 100).

