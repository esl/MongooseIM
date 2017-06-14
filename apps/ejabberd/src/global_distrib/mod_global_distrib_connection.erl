-module(mod_global_distrib_connection).

-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("global_distrib_metrics.hrl").

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start_link(Server) ->
    gen_server:start_link(?MODULE, Server, []).

init(Server) ->
    {Addr, Port} = get_addr(Server),
    try
        {ok, Socket} = gen_tcp:connect(Addr, Port, [binary, {active, false}]),
        {ok, TLSSocket} = fast_tls:tcp_to_tls(Socket, [connect | opt(tls_opts)]),
        fast_tls:setopts(TLSSocket, [{active, once}]),
        {ok, TLSSocket}
    catch
        error:{badmatch, Reason} ->
            lager:error("Connection to ~p failed: ~p", [{Addr, Port}, Reason]),
            {stop, normal}
    end.

handle_call({data, _, _} = Data, From, Socket) ->
    gen_server:reply(From, ok),
    handle_cast(Data, Socket).

handle_cast({data, Stamp, Data}, Socket) ->
    QueueTimeNative = p1_time_compat:monotonic_time() - Stamp,
    QueueTimeUS = p1_time_compat:convert_time_unit(QueueTimeNative, native, micro_seconds),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_SEND_QUEUE_TIME, QueueTimeUS),
    Annotated = <<(byte_size(Data)):32, Data/binary>>,
    ok = fast_tls:send(Socket, Annotated),
    mongoose_metrics:update(global, ?GLOBAL_DISTRIB_MESSAGES_SENT, 1),
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

terminate(_Reason, _State) ->
    ignore.

opt(Key) ->
    mod_global_distrib_utils:opt(mod_global_distrib_sender, Key).

get_addr(Server) ->
    case ejabberd_config:get_local_option({global_distrib_addr, Server}) of
        undefined -> {unicode:characters_to_list(Server), opt(listen_port)};
        Other -> Other
    end.
