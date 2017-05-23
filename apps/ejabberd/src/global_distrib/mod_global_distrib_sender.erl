-module(mod_global_distrib_sender).

-behaviour(gen_mod).
-behaviour(supervisor).

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([start/2, stop/1, send/2, start_link/0, init/1]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start(Host, Opts0) ->
    Opts = [{listen_port, 5555}, {num_of_connections, 1} | Opts0],
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

start() ->
    ChildSpec = {?MODULE, {?MODULE, start_link, []}, transient, 1000, supervisor, [?MODULE]},
    supervisor:start_child(ejabberd_sup, ChildSpec).

stop() ->
    ok.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

send(Server, {From, _To, Acc} = Packet) ->
    Worker = get_process_for(Server),
    BinPacket = term_to_binary(Packet),
    ok = mod_global_distrib_utils:cast_or_call(wpool_process, Worker, {data, BinPacket}),
    ejabberd_hooks:run(global_distrib_send_packet, [From, mongoose_acc:get(to_send, Acc)]),
    ok.

get_process_for(Server) ->
    Name = server_to_atom(Server),
    case whereis(Name) of
        undefined ->
            case start_pool(Name, Server) of
                {ok, Pid} -> Pid;
                {error, {already_started, Pid}} -> Pid
            end;
        _ ->
            ok
    end,
    wpool_pool:best_worker(Name).

init(_) ->
    Child = {wpool_pool, {wpool_pool, start_link, []}, temporary, 5000, worker, dynamic},
    {ok, {{simple_one_for_one, 1000000, 1}, [Child]}}.

start_pool(Name, Server) ->
    supervisor:start_child(?MODULE, [Name, [{workers, opt(num_of_connections)},
                                            {worker, {mod_global_distrib_connection, [Server]}}]]).

server_to_atom(Server) ->
    binary_to_atom(base64:encode(Server), latin1).

opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).
