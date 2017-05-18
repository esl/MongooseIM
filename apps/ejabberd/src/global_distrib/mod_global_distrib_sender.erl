-module(mod_global_distrib_sender).

-behaviour(gen_mod).
-behaviour(ranch_protocol).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(SENDERS, mod_global_distrib_senders).

-export([start/2, stop/1, send/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start(Host, Opts) ->
    mod_global_distrib_utils:start(?MODULE, Host, Opts, fun start/0).

stop(Host) ->
    mod_global_distrib_utils:stop(?MODULE, Host, fun stop/0).

start() ->
    ets:new(?SENDERS, [named_table, public, {read_concurrency, true}]).

stop() ->
    ets:delete(?SENDERS).

send(Server, {_From, _To, _Acc} = Packet) ->
    Pid = get_process_for(Server),
    BinPacket = term_to_binary(Packet),
    case process_info(Pid, message_queue_len) of
        {_, X} when X > 500 -> gen_server:call(Pid, {data, BinPacket}, 10000); % TODO
        _ -> gen_server:cast(Pid, {data, BinPacket})
    end.

get_process_for(Server) ->
    case ets:lookup(?SENDERS, Server) of
        [{_, Pid}] -> Pid;
        [] ->
            {ok, Pid} = gen_server:start_link(?MODULE, Server, []),
            case ets:insert_new(?SENDERS, {Server, Pid}) of
                true -> Pid;
                false ->
                    gen_server:stop(Pid),
                    get_process_for(Server)
            end
    end.


init(Server) ->
    {Addr, Port} = ejabberd_config:get_local_option({global_distrib_addr, Server}),
    {ok, Socket} = gen_tcp:connect(Addr, Port, [binary, {packet, 4}, {active, false}]),
    {ok, Socket}.

handle_call({data, Data}, From, Socket) ->
    gen_server:reply(From, ok),
    handle_cast({data, Data}, Socket).

handle_cast({data, Data}, Socket) ->
    gen_tcp:send(Socket, Data),
    {noreply, Socket}.

handle_info(_, Socket) ->
    {noreply, Socket}.
