-module(mongoose_s2s_socket_in).

-behaviour(gen_server).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("public_key/include/public_key.hrl").

-type stanza_size() :: pos_integer() | infinity.

-type socket_module() :: ranch_tcp | just_tls.
-type socket() :: gen_tcp:socket() | ssl:sslsocket().

-record(socket_data, {sockmod = ranch_tcp :: socket_module(),
                      socket              :: term(),
                      receiver            :: pid(),
                      connection_details  :: mongoose_listener:connection_details()
                     }).

-type socket_data() :: #socket_data{}.

-record(state, {socket :: socket(),
                sockmod = ranch_tcp   :: socket_module(),
                shaper_state          :: mongoose_shaper:shaper(),
                dest_pid              :: undefined | pid(), %% gen_fsm_compat pid
                max_stanza_size       :: stanza_size(),
                parser                :: exml_stream:parser(),
                hibernate_after = 0   :: non_neg_integer()}).
-type state() :: #state{}.

-export_type([socket_data/0]).

%% transport API
-export([close/1, send_text/2, send_element/2, peername/1, change_shaper/2,
         wait_for_tls_handshake/3, get_peer_certificate/1]).
-export([start_link/2, init/1, handle_continue/2,
         handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-spec start_link(mongoose_listener:init_args(), [gen_server:start_opt()]) ->
    gen_server:start_ret().
start_link(Params, ProcOpts) ->
    gen_server:start_link(?MODULE, Params, ProcOpts).

-spec close(socket_data()) -> ok.
close(#socket_data{receiver = Receiver}) ->
    gen_server:cast(Receiver, close).

-spec wait_for_tls_handshake(socket_data(), just_tls:options(), exml:element()) ->
     socket_data().
wait_for_tls_handshake(#socket_data{receiver = Receiver} = SocketData, TLSOpts, El) ->
    tcp_to_tls(Receiver, maps:remove(ciphers, TLSOpts#{connect => false})),
    send_element(SocketData, El), %% send last negotiation chunk via tcp
    update_socket(SocketData).

-spec send_text(socket_data(), binary()) -> ok.
send_text(SocketData, Data) ->
    #socket_data{sockmod = SockMod, socket = Socket} = SocketData,
    case catch SockMod:send(Socket, Data) of
        ok ->
            update_transport_metrics(
              Data, #{sockmod => SockMod, direction => out});
        {error, timeout} ->
            ?LOG_ERROR(#{what => socket_error, reason => timeout,
                        socket => SockMod}),
            exit(normal);
        Error ->
            ?LOG_ERROR(#{what => socket_error, reason => Error,
                        socket => SockMod}),
            exit(normal)
    end.


-spec send_element(socket_data(), exml:element()) -> ok.
send_element(#socket_data{} = SocketData, El) ->
    mongoose_instrument:execute(s2s_xmpp_element_size_out, #{}, #{byte_size => exml:xml_size(El)}),
    BinEl = exml:to_binary(El),
    send_text(SocketData, BinEl).

-spec get_peer_certificate(socket_data()) -> just_tls:cert().
get_peer_certificate(#socket_data{sockmod = just_tls, socket = Socket}) ->
    just_tls:get_peer_certificate(Socket);
get_peer_certificate(_SocketData) ->
    no_peer_cert.

-spec peername(socket_data()) -> {ok, mongoose_transport:peer()}.
peername(#socket_data{connection_details = #{src_address := SrcAddr, src_port := SrcPort}}) ->
    {ok, {SrcAddr, SrcPort}}.

-spec change_shaper(socket_data(), _) -> any().
change_shaper(#socket_data{receiver = Receiver}, Shaper)  ->
    gen_server:cast(Receiver, {change_shaper, Shaper}).

%%----------------------------------------------------------------------
%% gen_server interfaces
%%----------------------------------------------------------------------
-spec init(mongoose_listener:init_args()) ->
    {ok, undefined, {continue, {do_handshake, mongoose_listener:init_args()}}}.
init(InitArgs) ->
    {ok, undefined, {continue, {do_handshake, InitArgs}}}.

handle_continue({do_handshake, {?MODULE, Ref, Transport,
                 #{shaper := Shaper,
                   max_stanza_size := MaxStanzaSize,
                   hibernate_after := HibernateAfter} = Opts}}, undefined) ->
    ShaperState = mongoose_shaper:new(Shaper),
    Parser = new_parser(MaxStanzaSize),
    {ok, Socket, ConnectionDetails} = mongoose_listener:read_connection_details(Ref, Transport, Opts),
    SocketData = #socket_data{sockmod = Transport,
                              socket = Socket,
                              receiver = self(),
                              connection_details = ConnectionDetails},
    {ok, FsmPid} = ejabberd_s2s_in:start_link(SocketData, Opts),
    State = #state{socket = Socket,
                   dest_pid = FsmPid,
                   shaper_state = ShaperState,
                   max_stanza_size = MaxStanzaSize,
                   parser = Parser,
                   hibernate_after = HibernateAfter},
    activate_socket(State),
    {noreply, State, hibernate_or_timeout(State)}.

handle_call({tcp_to_tls, TLSOpts}, From, #state{socket = TCPSocket} = State0) ->
    gen_server:reply(From, ok),
    case just_tls:tcp_to_tls(TCPSocket, TLSOpts) of
        {ok, TLSSocket} ->
            State1 = reset_parser(State0),
            State2 = State1#state{socket = TLSSocket, sockmod = just_tls},
            activate_socket(State2),
            {noreply, State2, hibernate_or_timeout(State2)};
        {error, Reason} ->
            ?LOG_ERROR(#{what => tcp_to_tls_failed, reason => Reason,
                           dest_pid => State0#state.dest_pid}),
            {stop, normal, State0}
    end;
handle_call(get_socket, _From, #state{socket = Socket} = State) ->
    {reply, {ok, Socket}, State, hibernate_or_timeout(State)};
handle_call(get_state_info, _From, #state{dest_pid = DestPid} = State) ->
    Result = gen_fsm_compat:sync_send_all_state_event(DestPid, get_state_info),
    {reply, Result, State, hibernate_or_timeout(State)};
handle_call(_Request, _From, State) ->
    {reply, ok, State, hibernate_or_timeout(State)}.

handle_cast({change_shaper, Shaper}, State) ->
    NewShaperState = mongoose_shaper:new(Shaper),
    NewState = State#state{shaper_state = NewShaperState},
    {noreply, NewState, hibernate_or_timeout(NewState)};
handle_cast(close, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State, hibernate_or_timeout(State)}.

handle_info({tcp, _, Data}, #state{sockmod = ranch_tcp} = State) ->
    NewState = process_data(Data, State),
    {noreply, NewState, hibernate_or_timeout(NewState)};
handle_info({ssl, _, Data}, #state{sockmod = just_tls} = State) ->
    NewState = process_data(Data, State),
    {noreply, NewState, hibernate_or_timeout(NewState)};
handle_info(TcpOrSslInfo, State)
  when is_tuple(TcpOrSslInfo) andalso
       tcp_closed =:= element(1, TcpOrSslInfo) orelse tcp_error =:= element(1, TcpOrSslInfo) orelse
       ssl_closed =:= element(1, TcpOrSslInfo) orelse ssl_error =:= element(1, TcpOrSslInfo) ->
    {stop, {shutdown, element(1, TcpOrSslInfo)}, State};
handle_info({timeout, _Ref, activate}, State) ->
    activate_socket(State),
    {noreply, State, hibernate_or_timeout(State)};
handle_info(timeout, State) ->
    {noreply, State, hibernate()};
handle_info(Info, State) ->
    ?UNEXPECTED_INFO(Info),
    {noreply, State, hibernate_or_timeout(State)}.

terminate(_Reason, #state{parser = Parser, dest_pid = DestPid,
                          socket = Socket, sockmod = SockMod}) ->
    free_parser(Parser),
    case DestPid of
        undefined -> ok;
        _ -> gen_fsm_compat:send_event(DestPid, closed)
    end,
    catch shutdown_socket_and_wait_for_peer_to_close(Socket, SockMod),
    ok.

%%----------------------------------------------------------------------
%% local API helpers
%%----------------------------------------------------------------------

-spec tcp_to_tls(pid(), just_tls:options()) -> ok | {error, any()}.
tcp_to_tls(Receiver, TLSOpts) ->
    gen_server_call_or_noproc(Receiver, {tcp_to_tls, TLSOpts}).

-spec update_socket(socket_data()) -> socket_data().
update_socket(#socket_data{receiver = Receiver} = SocketData) ->
    case gen_server_call_or_noproc(Receiver, get_socket) of
        {ok, TLSSocket} ->
            SocketData#socket_data{socket = TLSSocket, sockmod = just_tls};
        {error, _} ->
            exit(invalid_socket_after_upgrade_to_tls)
    end.

-spec gen_server_call_or_noproc(pid(), any()) -> Ret :: any() | {error, any()}.
gen_server_call_or_noproc(Pid, Message) ->
    try
        gen_server:call(Pid, Message)
    catch
        exit:{noproc, Extra} ->
            {error, {noproc, Extra}};
        exit:{normal, Extra} ->
            % reciver exited with normal status after the gen_server call was sent
            % but before it was processed
            {error, {died, Extra}}
    end.

%%--------------------------------------------------------------------
%% internal functions
%%--------------------------------------------------------------------

-spec activate_socket(state()) -> 'ok' | {'tcp_closed', _}.
activate_socket(#state{socket = Socket, sockmod = ranch_tcp}) ->
    inet:setopts(Socket, [{active, once}]),
    PeerName = inet:peername(Socket),
    resolve_peername(PeerName, Socket);
activate_socket(#state{socket = Socket, sockmod = just_tls}) ->
    just_tls:setopts(Socket, [{active, once}]),
    PeerName = just_tls:peername(Socket),
    resolve_peername(PeerName, Socket).

resolve_peername({ok, _}, _Socket) ->
    ok;
resolve_peername({error, _Reason}, Socket) ->
    self() ! {tcp_closed, Socket}.

-spec process_data(binary(), state()) -> state().
process_data(Data, #state{parser = Parser,
                          shaper_state = ShaperState,
                          dest_pid = DestPid,
                          sockmod = SockMod} = State) ->
    ?LOG_DEBUG(#{what => received_xml_on_stream, packet => Data, dest_pid => DestPid}),
    Size = byte_size(Data),
    {Events, NewParser} =
        case exml_stream:parse(Parser, Data) of
            {ok, NParser, Elems} ->
                {[wrap_xml_elements_and_update_metrics(E) || E <- Elems], NParser};
            {error, Reason} ->
                {[{xmlstreamerror, Reason}], Parser}
        end,
    {NewShaperState, Pause} = mongoose_shaper:update(ShaperState, Size),
    update_transport_metrics(Data, #{sockmod => SockMod, direction => in}),
    [gen_fsm_compat:send_event(DestPid, Event) || Event <- Events],
    maybe_pause(Pause, State),
    State#state{parser = NewParser, shaper_state = NewShaperState}.

wrap_xml_elements_and_update_metrics(E) ->
    mongoose_instrument:execute(s2s_xmpp_element_size_in, #{}, #{t => in, el => E, byte_size => exml:xml_size(E)}),
    wrap_xml(E).

wrap_xml(#xmlel{} = E) ->
    {xmlstreamelement, E};
wrap_xml(E) ->
    E.

-spec update_transport_metrics(binary(), #{direction := in | out, sockmod := socket_module()}) -> ok.
update_transport_metrics(Data, #{direction := in, sockmod := ranch_tcp}) ->
    mongoose_instrument:execute(s2s_tcp_data_in, #{}, #{byte_size => byte_size(Data)});
update_transport_metrics(Data, #{direction := in, sockmod := just_tls}) ->
    mongoose_instrument:execute(s2s_tls_data_in, #{}, #{byte_size => byte_size(Data)});
update_transport_metrics(Data, #{direction := out, sockmod := ranch_tcp}) ->
    mongoose_instrument:execute(s2s_tcp_data_out, #{}, #{byte_size => byte_size(Data)});
update_transport_metrics(Data, #{direction := out, sockmod := just_tls}) ->
    mongoose_instrument:execute(s2s_tls_data_out, #{}, #{byte_size => byte_size(Data)}).

-spec maybe_pause(Delay :: non_neg_integer(), state()) -> any().
maybe_pause(_, #state{dest_pid = undefined}) ->
    ok;
maybe_pause(Pause, _State) when Pause > 0 ->
    erlang:start_timer(Pause, self(), activate);
maybe_pause(_, State) ->
    activate_socket(State).

-spec new_parser(stanza_size()) -> exml_stream:parser().
new_parser(MaxStanzaSize) ->
    MaxSize = case MaxStanzaSize of
                  infinity -> 0;
                  _ -> MaxStanzaSize
              end,
    {ok, NewParser} = exml_stream:new_parser([{max_element_size, MaxSize}]),
    NewParser.

-spec reset_parser(state()) -> state().
reset_parser(#state{parser = Parser} = State) ->
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    State#state{parser = NewParser}.

-spec free_parser(exml_stream:parser()) -> ok.
free_parser(Parser) ->
    exml_stream:free_parser(Parser).

-spec hibernate() -> hibernate | infinity.
hibernate() ->
    case process_info(self(), message_queue_len) of
        {_, 0} -> hibernate;
        _ -> infinity
    end.

-spec hibernate_or_timeout(state()) -> hibernate | infinity | pos_integer().
hibernate_or_timeout(#state{hibernate_after = 0}) -> hibernate();
hibernate_or_timeout(#state{hibernate_after = HA}) -> HA.

-spec shutdown_socket_and_wait_for_peer_to_close(socket(), socket_module() ) ->  ok.
%% gen_tcp:close/2, but trying to ensure that all data is received by peer.
%%
%% This is based on tls_connection:workaround_transport_delivery_problems/2 code
%% https://github.com/erlang/otp/blob/OTP_17.0-rc2/lib/ssl/src/tls_connection.erl#L959
%%
%% There are some more docs why we need it in http://erlang.org/doc/man/gen_tcp.html#close-1
shutdown_socket_and_wait_for_peer_to_close(Socket, ranch_tcp) ->
    %% Standard trick to try to make sure all
    %% data sent to the tcp port is really delivered to the
    %% peer application before tcp port is closed so that the peer will
    %% get the correct stream end and not only a transport close.
    inet:setopts(Socket, [{active, false}]),
    ranch_tcp:shutdown(Socket, write),
    %% Will return when other side has closed or after 30 s
    %% e.g. we do not want to hang if something goes wrong
    %% with the network but we want to maximise the odds that
    %% peer application gets all data sent on the tcp connection.
    ranch_tcp:recv(Socket, 0, 30000);
shutdown_socket_and_wait_for_peer_to_close(Socket, just_tls) ->
    just_tls:close(Socket).
