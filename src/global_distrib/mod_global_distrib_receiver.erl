%%==============================================================================
%% Copyright 2017 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(mod_global_distrib_receiver).
-author('konrad.zemek@erlang-solutions.com').

-behaviour(gen_mod).
-behaviour(ranch_protocol).
-behaviour(gen_server).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("global_distrib_metrics.hrl").

-export([endpoints/0, start_link/4]).
-export([start/2, stop/1]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2]).

-define(LISTEN_RETRIES, 5). %% Number of retries in case of eaddrinuse
-define(LISTEN_RETRY_DELAY, 1000). %% Milliseconds to retrying in case of eaddrinuse

-record(state, {
    socket :: mod_global_distrib_transport:t(),
    waiting_for :: header | non_neg_integer(),
    buffer = <<>> :: binary(),
    host :: undefined | atom(),
    peer :: tuple() | unknown
}).

-type state() :: #state{}.

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link(Ref :: reference(), Socket :: gen_tcp:socket(), Transport :: ranch_tcp,
                 Opts :: [term()]) -> {ok, pid()}.
start_link(Ref, Socket, ranch_tcp, Opts) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Opts}]),
    {ok, Pid}.

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

-spec start(Host :: jid:lserver(), Opts :: proplists:proplist()) -> any().
start(Host, Opts0) ->
    {local_host, LocalHost} = lists:keyfind(local_host, 1, Opts0),
    Opts = [{endpoints, [{LocalHost, 5555}]} | Opts0],
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

-spec stop(Host :: jid:lserver()) -> any().
stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

%%--------------------------------------------------------------------
%% ranch_protocol API
%%--------------------------------------------------------------------

init({Ref, RawSocket, _Opts}) ->
    process_flag(trap_exit, true),
    ok = ranch:accept_ack(Ref),
    {ok, Socket} = mod_global_distrib_transport:wrap(RawSocket, opt(tls_opts)),
    ok = mod_global_distrib_transport:setopts(Socket, [{active, once}]),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_INCOMING_ESTABLISHED, 1),
    State = #state{socket = Socket, waiting_for = header,
                   peer = mod_global_distrib_transport:peername(Socket)},
    gen_server:enter_loop(?MODULE, [], State).

%%--------------------------------------------------------------------
%% gen_server API
%%--------------------------------------------------------------------

handle_info({tcp, _Socket, RawData}, #state{socket = Socket, buffer = Buffer} = State) ->
    ok = mod_global_distrib_transport:setopts(Socket, [{active, once}]),
    {ok, Data} = mod_global_distrib_transport:recv_data(Socket, RawData),
    NewState = handle_buffered(State#state{buffer = <<Buffer/binary, Data/binary>>}),
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
    ?ERROR_MSG("event=incoming_global_distrib_socket_error,reason='~p',peer='~p'",
               [Reason, State#state.peer]),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_INCOMING_ERRORED(State#state.host), 1),
    {stop, {error, Reason}, State};
handle_info(Msg, State) ->
    ?WARNING_MSG("Received unknown message ~p", [Msg]),
    {noreply, State}.

handle_cast(_Message, _State) ->
    exit(bad_cast).

handle_call(_Message, _From, _State) ->
    exit(bad_call).

code_change(_Version, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
    ?WARNING_MSG("event=incoming_global_distrib_socket_closed,peer='~p'", [State#state.peer]),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_INCOMING_CLOSED(State#state.host), 1),
    catch mod_global_distrib_transport:close(State#state.socket),
    ignore.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec start() -> any().
start() ->
    opt(tls_opts), %% Check for required tls_opts
    mongoose_metrics:ensure_metric(global, ?GLOBAL_DISTRIB_RECV_QUEUE_TIME, histogram),
    mongoose_metrics:ensure_metric(global, ?GLOBAL_DISTRIB_INCOMING_ESTABLISHED, spiral),
    mod_global_distrib_utils:ensure_metric(?GLOBAL_DISTRIB_INCOMING_ERRORED(undefined), spiral),
    mod_global_distrib_utils:ensure_metric(?GLOBAL_DISTRIB_INCOMING_CLOSED(undefined), spiral),
    ChildMod = mod_global_distrib_worker_sup,
    Child = {ChildMod, {ChildMod, start_link, []}, permanent, 10000, supervisor, [ChildMod]},
    {ok, _}= supervisor:start_child(ejabberd_sup, Child),
    Endpoints = mod_global_distrib_utils:resolve_endpoints(opt(endpoints)),
    ets:insert(?MODULE, {endpoints, Endpoints}),
    start_listeners().

-spec stop() -> any().
stop() ->
    stop_listeners(),
    supervisor:terminate_child(ejabberd_sup, mod_global_distrib_worker_sup),
    supervisor:delete_child(ejabberd_sup, mod_global_distrib_worker_sup).

-spec opt(Key :: atom()) -> term().
opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).

-spec handle_data(Data :: binary(), state()) -> state().
handle_data(BinHost, State = #state{host = undefined}) ->
    Host = mod_global_distrib_utils:binary_to_metric_atom(BinHost),
    mod_global_distrib_utils:ensure_metric(?GLOBAL_DISTRIB_MESSAGES_RECEIVED(Host), spiral),
    mod_global_distrib_utils:ensure_metric(?GLOBAL_DISTRIB_TRANSFER_TIME(Host), histogram),
    mod_global_distrib_utils:ensure_metric(
      ?GLOBAL_DISTRIB_INCOMING_FIRST_PACKET(Host), spiral),
    mod_global_distrib_utils:ensure_metric(
      ?GLOBAL_DISTRIB_INCOMING_ERRORED(Host), spiral),
    mod_global_distrib_utils:ensure_metric(
      ?GLOBAL_DISTRIB_INCOMING_CLOSED(Host), spiral),
    mongoose_metrics:init_subscriptions(),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_INCOMING_FIRST_PACKET(Host), 1),
    State#state{host = Host};
handle_data(Data, State = #state{host = Host}) ->
    <<ClockTime:64, BinFromSize:16, _/binary>> = Data,
    TransferTime = p1_time_compat:system_time(micro_seconds) - ClockTime,
    <<_:80, BinFrom:BinFromSize/binary, BinTerm/binary>> = Data,
    Worker = mod_global_distrib_worker_sup:get_worker(BinFrom),
    Stamp = erlang:monotonic_time(),
    ok = mod_global_distrib_utils:cast_or_call(Worker, {data, Host, TransferTime, Stamp, BinTerm}),
    State.

-spec handle_buffered(state()) -> state().
handle_buffered(#state{waiting_for = header, buffer = <<Header:4/binary, Rest/binary>>} = State) ->
    Size = binary:decode_unsigned(Header),
    handle_buffered(State#state{waiting_for = Size, buffer = Rest});
handle_buffered(#state{waiting_for = Size, buffer = Buffer} = State)
  when byte_size(Buffer) >= Size ->
    <<Data:Size/binary, Rest/binary>> = Buffer,
    NewState = handle_data(Data, State),
    handle_buffered(NewState#state{waiting_for = header, buffer = Rest});
handle_buffered(State) ->
    State.

-spec endpoints() -> [mod_global_distrib_utils:endpoint()].
endpoints() ->
    opt(endpoints).

-spec start_listeners() -> any().
start_listeners() ->
    [start_listener(Endpoint, ?LISTEN_RETRIES) || Endpoint <- endpoints()],
    ok.

-spec start_listener(mod_global_distrib_utils:endpoint(),
                     RetriesLeft :: non_neg_integer()) -> any().
start_listener({Addr, Port} = Ref, RetriesLeft) ->
    ?INFO_MSG("Starting listener on ~s:~b", [inet:ntoa(Addr), Port]),
    case ranch:start_listener(Ref, 10, ranch_tcp, [{ip, Addr}, {port, Port}], ?MODULE, []) of
        {ok, _} -> ok;
        {error, eaddrinuse} when RetriesLeft > 0 ->
            ?ERROR_MSG("Failed to start listener on ~s:~b: address in use. Will retry in 1 second.",
                       [inet:ntoa(Addr), Port]),
            timer:sleep(?LISTEN_RETRY_DELAY),
            start_listener(Ref, RetriesLeft - 1)
    end.

-spec stop_listeners() -> any().
stop_listeners() ->
    lists:foreach(fun ranch:stop_listener/1, endpoints()).

add_connection_to_sender(drop) -> drop;
add_connection_to_sender({From, To, Acc0, Packet} = FPacket) ->
    Origin = mod_global_distrib:get_metadata(Acc0, origin),
    Mgr = mod_global_distrib_utils:server_to_sup_name(Origin),
    mod_global_distrib_server_sup:start_pool(Server, )
    Acc0.
