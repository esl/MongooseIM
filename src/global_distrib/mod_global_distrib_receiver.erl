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
-behaviour(mongoose_module_metrics).

-include("mongoose.hrl").
-include("jlib.hrl").
-include("global_distrib_metrics.hrl").

-export([start_link/3]).
-export([start/2, stop/1, deps/2, instrumentation/1]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, code_change/3, terminate/2]).

-define(LISTEN_RETRIES, 5). %% Number of retries in case of eaddrinuse
-define(LISTEN_RETRY_DELAY, 1000). %% Milliseconds to retrying in case of eaddrinuse

-record(state, {
    socket :: mod_global_distrib_transport:t(),
    waiting_for :: header | non_neg_integer(),
    buffer = <<>> :: binary(),
    host :: undefined | binary(),
    conn_id = <<>> :: binary(),
    peer :: tuple() | unknown
}).

-type state() :: #state{}.

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_link(Ref :: reference(), Transport :: ranch_tcp,
                 Opts :: [term()]) -> {ok, pid()}.
start_link(Ref, ranch_tcp, Opts) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [{Ref, ranch_tcp, Opts}]),
    {ok, Pid}.

%%--------------------------------------------------------------------
%% gen_mod API
%%--------------------------------------------------------------------

-spec start(mongooseim:host_type(), gen_mod:module_opts()) -> any().
start(_HostType, _Opts) ->
    ChildMod = mod_global_distrib_worker_sup,
    Child = {ChildMod, {ChildMod, start_link, []}, permanent, 10000, supervisor, [ChildMod]},
    ejabberd_sup:start_child(Child),
    start_listeners().

-spec stop(mongooseim:host_type()) -> any().
stop(_HostType) ->
    stop_listeners(),
    ejabberd_sup:stop_child(mod_global_distrib_worker_sup).

-spec deps(mongooseim:host_type(), gen_mod:module_opts()) -> gen_mod_deps:deps().
deps(_HostType, Opts) ->
    [{mod_global_distrib_utils, Opts, hard}].

-spec instrumentation(mongooseim:host_type()) -> [mongoose_instrument:spec()].
instrumentation(_HostType) ->
    [{?GLOBAL_DISTRIB_RECV_QUEUE, #{},
      #{metrics => #{time => histogram}}},
     {?GLOBAL_DISTRIB_INCOMING_ESTABLISHED, #{},
      #{metrics => #{count => spiral}}},
     {?GLOBAL_DISTRIB_INCOMING_ERRORED, #{},
      #{metrics => #{count => spiral}}},
     {?GLOBAL_DISTRIB_INCOMING_CLOSED, #{},
      #{metrics => #{count => spiral}}},
     {?GLOBAL_DISTRIB_TRANSFER, #{},
      #{metrics => #{time => histogram}}},
     {?GLOBAL_DISTRIB_MESSAGES_RECEIVED, #{},
      #{metrics => #{count => spiral}}},
     {?GLOBAL_DISTRIB_INCOMING_FIRST_PACKET, #{},
      #{metrics => #{count => spiral}}}].

%%--------------------------------------------------------------------
%% ranch_protocol API
%%--------------------------------------------------------------------

init({Ref, ranch_tcp, _Opts}) ->
    process_flag(trap_exit, true),
    {ok, RawSocket} = ranch:handshake(Ref),
    ConnOpts = opt(connections),
    {ok, Socket} = mod_global_distrib_transport:wrap(RawSocket, ConnOpts, server),
    ok = mod_global_distrib_transport:setopts(Socket, [{active, once}]),
    mongoose_instrument:execute(?GLOBAL_DISTRIB_INCOMING_ESTABLISHED, #{},
                                #{count => 1, peer => mod_global_distrib_transport:peername(Socket)}),
    State = #state{socket = Socket, waiting_for = header,
                   peer = mod_global_distrib_transport:peername(Socket)},
    gen_server:enter_loop(?MODULE, [], State).

%%--------------------------------------------------------------------
%% gen_server API
%%--------------------------------------------------------------------

handle_info({Tag, _Socket, RawData}, #state{socket = Socket, buffer = Buffer} = State)
  when Tag == tcp; Tag == ssl ->
    do_setopts_and_receive_data(Socket, Buffer, RawData, State);
handle_info({Tag, _Socket}, State) when Tag == tcp_closed; Tag == ssl_closed ->
    {stop, normal, State};
handle_info({Tag, _Socket, Reason}, State) when Tag == tcp_error; Tag == ssl_error ->
    ?LOG_ERROR(#{what => gd_incoming_socket_error, reason => Reason,
                 text => <<"mod_global_distrib_receiver received tcp_error">>,
                 peer => State#state.peer, conn_id => State#state.conn_id}),
    mongoose_instrument:execute(?GLOBAL_DISTRIB_INCOMING_ERRORED, #{}, #{count => 1, host => State#state.host}),
    {stop, {error, Reason}, State};
handle_info(Msg, State) ->
    ?UNEXPECTED_INFO(Msg),
    {noreply, State}.

handle_cast(_Message, _State) ->
    exit(bad_cast).

handle_call(_Message, _From, _State) ->
    exit(bad_call).

code_change(_Version, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    case Reason of
        normal ->
            ?LOG_INFO(#{what => gd_incoming_socket_closed,
                        peer => State#state.peer, server => State#state.host,
                        reason => Reason, conn_id => State#state.conn_id});
        _ ->
            ?LOG_WARNING(#{what => gd_incoming_socket_closed,
                           peer => State#state.peer, server => State#state.host,
                           reason => Reason, conn_id => State#state.conn_id})
    end,
    mongoose_instrument:execute(?GLOBAL_DISTRIB_INCOMING_CLOSED, #{},
                                #{count => 1, host => State#state.host}),
    catch mod_global_distrib_transport:close(State#state.socket),
    ignore.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec opt(gen_mod:opt_key() | gen_mod:key_path()) -> gen_mod:opt_value().
opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).

do_setopts_and_receive_data(Socket, Buffer, RawData, State) ->
    SetOptsResult = mod_global_distrib_transport:setopts(Socket, [{active, once}]),
    case SetOptsResult of
        ok ->
            do_receive_data(Buffer, RawData, State);
        {error, closed} ->
            {stop, normal, State};
        _ ->
            {stop, {setopts_failed, SetOptsResult}, State}
    end.

do_receive_data(Buffer, Data, State) ->
    NewState = handle_buffered(State#state{buffer = <<Buffer/binary, Data/binary>>}),
    {noreply, NewState}.

-spec handle_data(Data :: binary(), state()) -> state().
handle_data(GdStart, State = #state{host = undefined}) ->
    {ok, #xmlel{name = <<"gd_start">>, attrs = Attrs}} = exml:parse(GdStart),
    #{<<"server">> := Host, <<"conn_id">> := ConnId} = Attrs,
    mongoose_instrument:execute(?GLOBAL_DISTRIB_INCOMING_FIRST_PACKET, #{}, #{count => 1, host => Host}),
    ?LOG_INFO(#{what => gd_incoming_connection, server => Host, conn_id => ConnId}),
    State#state{host = Host, conn_id = ConnId};
handle_data(Data, State = #state{host = Host}) ->
    <<ClockTime:64, BinFromSize:16, _/binary>> = Data,
    TransferTime = erlang:system_time(microsecond) - ClockTime,
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

-spec start_listeners() -> any().
start_listeners() ->
    [start_listener(Endpoint, ?LISTEN_RETRIES) || Endpoint <- endpoints()],
    ok.

-spec start_listener(mod_global_distrib_utils:endpoint(),
                     RetriesLeft :: non_neg_integer()) -> any().
start_listener({Addr, Port} = Ref, RetriesLeft) ->
    ?LOG_INFO(#{what => gd_start_listener, address => Addr, port => Port}),
    SocketOpts = [{ip, Addr}, {port, Port}],
    RanchOpts = #{max_connections => infinity, num_acceptors => 10, socket_opts => SocketOpts},
    case ranch:start_listener(Ref, ranch_tcp, RanchOpts, ?MODULE, []) of
        {ok, _} -> ok;
        {error, eaddrinuse} when RetriesLeft > 0 ->
            ?LOG_ERROR(#{what => gd_start_listener_failed, address => Addr, port => Port,
                         text => <<"Failed to start listener: address in use. Will retry in 1 second.">>}),
            timer:sleep(?LISTEN_RETRY_DELAY),
            start_listener(Ref, RetriesLeft - 1)
    end.

-spec stop_listeners() -> any().
stop_listeners() ->
    lists:foreach(fun ranch:stop_listener/1, endpoints()).

-spec endpoints() -> [mod_global_distrib_utils:endpoint()].
endpoints() ->
    opt([connections, resolved_endpoints]).
