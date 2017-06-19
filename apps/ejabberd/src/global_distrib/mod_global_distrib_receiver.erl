-module(mod_global_distrib_receiver).

-behaviour(gen_mod).
-behaviour(ranch_protocol).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("global_distrib_metrics.hrl").

-export([start/2, stop/1]).
-export([all_endpoints/0, endpoints/0, start_link/4, init/1, handle_info/2, terminate/2]).

-record(state, {
    socket,
    worker_pool,
    waiting_for,
    buffer = <<>>
}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start(Host, Opts0) ->
    Opts = [{listen_port, 5555}, {num_of_workers, 10} | Opts0],
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

start() ->
    opt(tls_opts), %% Check for required tls_opts
    mongoose_metrics:ensure_metric(global, ?GLOBAL_DISTRIB_MESSAGES_RECEIVED, spiral),
    mongoose_metrics:ensure_metric(global, ?GLOBAL_DISTRIB_RECV_QUEUE_TIME, histogram),
    Child = mod_global_distrib_worker_sup,
    {ok, _}= supervisor:start_child(ejabberd_sup, {Child, {Child, start_link, []}, permanent, 10000, supervisor, [Child]}),
    start_listeners().

stop() ->
    stop_listeners(),
    supervisor:terminate_child(ejabberd_sup, mod_global_distrib_worker_sup),
    supervisor:delete_child(ejabberd_sup, mod_global_distrib_worker_sup).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}]),
	{ok, Pid}.

init({Ref, Socket, ranch_tcp, Opts}) ->
    [{worker_pool, WorkerPool}] = Opts,
    ok = ranch:accept_ack(Ref),
    {ok, TLSSocket} = fast_tls:tcp_to_tls(Socket, opt(tls_opts)),
    ok = fast_tls:setopts(TLSSocket, [{active, once}]),
    gen_server:enter_loop(?MODULE, [], #state{socket = TLSSocket, worker_pool = WorkerPool,
                                              waiting_for = header}).

handle_info({tcp, _Socket, TLSData}, #state{socket = Socket, buffer = Buffer} = State) ->
    ok = fast_tls:setopts(Socket, [{active, once}]),
    {ok, Data} = fast_tls:recv_data(Socket, TLSData),
    NewState = handle_buffered(State#state{buffer = <<Buffer/binary, Data/binary>>}),
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, #state{socket = Socket} = State) ->
    fast_tls:close(Socket),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    ignore.

opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).

handle_data(Data, #state{worker_pool = WorkerPool}) ->
    <<BinFromSize:16, _/binary>> = Data,
    <<_:16, BinFrom:BinFromSize/binary, BinTerm/binary>> = Data,
    Worker = mod_global_distrib_worker_sup:get_worker(BinFrom),
    Stamp = erlang:monotonic_time(),
    ok = mod_global_distrib_utils:cast_or_call(gen_server, Worker, {data, Stamp, BinTerm}).

handle_buffered(#state{waiting_for = header, buffer = <<Header:4/binary, Rest/binary>>} = State) ->
    Size = binary:decode_unsigned(Header),
    handle_buffered(State#state{waiting_for = Size, buffer = Rest});
handle_buffered(#state{waiting_for = Size, buffer = Buffer} = State)
  when byte_size(Buffer) >= Size ->
    <<Data:Size/binary, Rest/binary>> = Buffer,
    handle_data(Data, State),
    handle_buffered(State#state{waiting_for = header, buffer = Rest});
handle_buffered(State) ->
    State.

random_endpoint() ->
    Endpoints = all_endpoints(),
    N = rand:uniform(length(Endpoints)),
    lists:nth(N, Endpoints).

all_endpoints() ->
    {Res, BadNodes} = rpc:multicall(?MODULE, endpoints, [], 5000),
    case BadNodes of
        [] -> ok;
        _ -> ?ERROR_MSG("Couldn't fetch endpoints from ~p", [BadNodes])
    end,
    Res.

endpoints() ->
    lists:map(
      fun
          ({StrAddr, Port}) when is_list(StrAddr) ->
              {ok, Addr} = inet:parse_address(StrAddr),
              {Addr, Port};
          (AddrPort) ->
              AddrPort
      end,
      opt(endpoints)).

start_listeners() ->
    lists:foreach(
      fun({Addr, Port} = Ref) ->
              {ok, _} = ranch:start_listener(Ref, 10, ranch_tcp, [{ip, Addr}, {port, Port}],
                                             ?MODULE, [{worker_pool, ?MODULE}])
      end,
      endpoints()).

stop_listeners() ->
    lists:foreach(fun ranch:stop_listener/1, endpoints()).

