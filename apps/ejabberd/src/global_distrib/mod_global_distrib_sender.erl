-module(mod_global_distrib_sender).

-behaviour(gen_mod).
-behaviour(ranch_protocol).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([start/2, stop/1, send/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start(Host, Opts) ->
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

start() ->
    ok.

stop() ->
    ok.

send(Server, {_From, _To, _Acc} = Packet) ->
    Pid = get_process_for(Server),
    BinPacket = term_to_binary(Packet),
    ok =
        case process_info(Pid, message_queue_len) of
            {_, X} when X > 500 ->
                gen_server:call(Pid, {data, BinPacket}, 10000); % TODO
            {_, _} ->
                gen_server:cast(Pid, {data, BinPacket});
            undefined ->
                ets:delete(?MODULE, Server),
                send(Server, Packet)
        end.

get_process_for(Server) ->
    case ets:lookup(?MODULE, Server) of
        [{_, Pid}] -> Pid;
        [] ->
            {ok, Pid} = gen_server:start_link(?MODULE, Server, []),
            case ets:insert_new(?MODULE, {Server, Pid}) of
                true -> Pid;
                false ->
                    gen_server:stop(Pid),
                    get_process_for(Server)
            end
    end.


init(Server) ->
    {Addr, Port} = ejabberd_config:get_local_option({global_distrib_addr, Server}),
    {ok, Socket} = gen_tcp:connect(Addr, Port, [binary, {active, false}]),
    {ok, TLSSocket} = fast_tls:tcp_to_tls(Socket, [{certfile, opt(certfile)}, {cafile, opt(cafile)}, connect]),
    fast_tls:setopts(TLSSocket, [{active, once}]),
    {ok, TLSSocket}.

handle_call({data, Data}, From, Socket) ->
    gen_server:reply(From, ok),
    handle_cast({data, Data}, Socket).

handle_cast({data, Data}, Socket) ->
    Annotated = <<(byte_size(Data)):32, Data/binary>>,
    ok = fast_tls:send(Socket, Annotated),
    {noreply, Socket}.

handle_info({tcp, _Socket, TLSData}, Socket) ->
    ok = fast_tls:setopts(Socket, [{active, once}]),
    fast_tls:recv_data(Socket, TLSData),
    {noreply, Socket};
handle_info({tcp_closed, _}, Socket) ->
    fast_tls:close(Socket),
    {stop, normal, Socket};
handle_info(_, Socket) ->
    {noreply, Socket}.

terminate(Reason, State) ->
    ignore.

opt(Key) ->
    mod_global_distrib_utils:opt(?MODULE, Key).
