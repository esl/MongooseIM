-module(mod_global_distrib_receiver).

-behaviour(gen_mod).
-behaviour(ranch_protocol).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([start/2, stop/1]).
-export([start_link/4, init/1, handle_info/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start(Host, Opts0) ->
    Opts = [{receiver_workers, 1000} | Opts0],
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

start() ->
    wpool:start_sup_pool(?MODULE, [{workers, opt(receiver_workers)}, {worker, {mod_global_distrib_worker, []}}]),
    {ok, _} = ranch:start_listener(?MODULE, 1, ranch_tcp, [{port, opt(receiver_port)}], ?MODULE, [{worker_pool, ?MODULE}]).

stop() ->
    ranch:stop_listener(?MODULE),
    wpool:stop_pool(?MODULE).

start_link(Ref, Socket, Transport, Opts) ->
	Pid = proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}]),
	{ok, Pid}.

init({Ref, Socket, Transport, Opts}) ->
    [{worker_pool, WorkerPool}] = Opts,
	ok = ranch:accept_ack(Ref),
    ok = Transport:setopts(Socket, [{active, once}, binary, {packet, 4}]),
    gen_server:enter_loop(?MODULE, [], {Socket, Transport, WorkerPool}).

handle_info({tcp, _Socket, Data}, {Socket, Transport, WorkerPool} = State) ->
    ok = Transport:setopts(Socket, [{active, once}]),
    Worker = wpool_pool:best_worker(WorkerPool),
    case process_info(whereis(Worker), message_queue_len) of
        {_, X} when X > 500 -> wpool_process:call(Worker, {data, Data}, 10000); % TODO
        _ -> wpool_process:cast(Worker, {data, Data})
    end,
    {noreply, State}.

opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).
