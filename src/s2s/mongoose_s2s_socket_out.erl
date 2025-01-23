-module(mongoose_s2s_socket_out).
-author('piotr.nosek@erlang-solutions.com').

-include("mongoose.hrl").
-include("jlib.hrl").

-behaviour(gen_server).

%%----------------------------------------------------------------------
%% Types
%%----------------------------------------------------------------------

-type stanza_size() :: pos_integer() | infinity.
-type connection_type() :: s2s | undefined.

-type options() :: #{max_stanza_size := stanza_size(),
                     hibernate_after := non_neg_integer(),
                     connection_type := connection_type(),
                     atom() => any()}.

-export_type([socket_data/0]).

-type socket_module() :: gen_tcp | just_tls.
-type socket() :: gen_tcp:socket() | just_tls:tls_socket().

-record(socket_data, {sockmod = gen_tcp  :: socket_module(),
                      socket             :: term(),
                      receiver           :: pid(),
                      connection_type    :: connection_type(),
                      connection_details :: mongoose_listener:connection_details()
                     }).

-type socket_data() :: #socket_data{}.

-record(state, {socket :: socket(),
                sockmod = gen_tcp     :: socket_module(),
                shaper_state          :: mongoose_shaper:shaper(),
                dest_pid              :: undefined | pid(), %% gen_fsm_compat pid
                max_stanza_size       :: stanza_size(),
                parser                :: exml_stream:parser(),
                connection_type       :: connection_type(),
                hibernate_after = 0   :: non_neg_integer()}).
-type state() :: #state{}.

%% transport API
-export([connect/5, close/1, send_text/2, send_element/2]).
-export([connect_tls/2, peername/1, get_all_trasport_processes/0]).

%% gen_server API
-export([start_link/3, init/1, terminate/2,
         handle_cast/2, handle_call/3, handle_info/2]).

-ignore_xref([start_link/3, get_all_trasport_processes/0]).

%%----------------------------------------------------------------------
%% Transport API
%%----------------------------------------------------------------------

-spec connect(ConnectionType :: connection_type(),
              Addr :: atom() | string() | inet:ip_address(),
              Port :: inet:port_number(),
              Opts :: [gen_tcp:connect_option()],
              Timeout :: non_neg_integer() | infinity
              ) -> {'error', atom()} | {'ok', socket_data()}.
connect(ConnectionType, Addr, Port, Opts, Timeout) ->
    case gen_tcp:connect(Addr, Port, Opts, Timeout) of
        {ok, Socket} ->
            %% Receiver options are configurable only for listeners
            %% It might make sense to make them configurable for
            %% outgoing s2s connections as well
            ReceiverOpts = #{max_stanza_size => infinity,
                             hibernate_after => 0,
                             connection_type => ConnectionType},
            Receiver = start_child(Socket, none, ReceiverOpts),
            {SrcAddr, SrcPort} = case inet:sockname(Socket) of
                                     {ok, {A, P}} ->  {A, P};
                                     {error, _} -> {unknown, unknown}
                                 end,
            ConnectionDetails = #{dest_address => Addr, dest_port => Port, proxy => false,
                                  src_address => SrcAddr, src_port => SrcPort},
            SocketData = #socket_data{sockmod = gen_tcp,
                                      socket = Socket,
                                      receiver = Receiver,
                                      connection_type = ConnectionType,
                                      connection_details = ConnectionDetails},
            DestPid = self(),
            case gen_tcp:controlling_process(Socket, Receiver) of
                ok ->
                    set_dest_pid(Receiver, DestPid),
                    {ok, SocketData};
                {error, _Reason} = Error ->
                    gen_tcp:close(Socket),
                    Error
            end;
        {error, _Reason} = Error ->
            Error
    end.

-spec close(socket_data()) -> ok.
close(#socket_data{receiver = Receiver}) ->
    gen_server:cast(Receiver, close).

-spec connect_tls(socket_data(), just_tls:options()) -> socket_data().
connect_tls(#socket_data{receiver = Receiver} = SocketData, TLSOpts) ->
    tcp_to_tls(Receiver, TLSOpts#{connect => true}),
    update_socket(SocketData).

-spec send_text(socket_data(), binary()) -> ok.
send_text(SocketData, Data) ->
    #socket_data{sockmod = SockMod, socket = Socket,
                 connection_type = s2s} = SocketData,
    mongoose_instrument:execute(xmpp_element_size_out,
                                #{connection_type => s2s},
                                #{t => out, el => Data, byte_size => byte_size(Data)}),
    case catch SockMod:send(Socket, Data) of
        ok ->
            update_transport_metrics(Data,
                                     #{sockmod => SockMod, direction => out}),
            ok;
        {error, timeout} ->
            ?LOG_INFO(#{what => socket_error, reason => timeout,
                        socket => SockMod}),
            exit(normal);
        Error ->
            ?LOG_INFO(#{what => socket_error, reason => Error,
                        socket => SockMod}),
            exit(normal)
    end.


-spec send_element(socket_data(), exml:element()) -> ok.
send_element(#socket_data{connection_type = s2s} = SocketData, El) ->
    BinEl = exml:to_binary(El),
    send_text(SocketData, BinEl).

-spec peername(socket_data()) -> mongoose_transport:peername_return().
peername(#socket_data{connection_details = #{src_address := SrcAddr,
                                             src_port := SrcPort}}) ->
    {ok, {SrcAddr, SrcPort}}.

get_all_trasport_processes() ->
    Connections = supervisor:which_children(mongoose_s2s_socket_out_sup),
    get_transport_info(Connections).
%%----------------------------------------------------------------------
%% gen_server interfaces
%%----------------------------------------------------------------------
-spec start_link(port(), atom(), options()) ->
          ignore | {error, _} | {ok, pid()}.
start_link(Socket, Shaper, Opts) ->
    gen_server:start_link(?MODULE, {Socket, Shaper, Opts}, []).

init({Socket, Shaper, Opts}) ->
    ShaperState = mongoose_shaper:new(Shaper),
    #{max_stanza_size := MaxStanzaSize,
      hibernate_after := HibernateAfter,
      connection_type := ConnectionType} = Opts,
    Parser = new_parser(MaxStanzaSize),
    {ok, #state{socket = Socket,
                shaper_state = ShaperState,
                max_stanza_size = MaxStanzaSize,
                connection_type = ConnectionType,
                parser = Parser,
                hibernate_after = HibernateAfter}}.

handle_call({tcp_to_tls, TLSOpts}, _From, #state{socket = TCPSocket} = State0) ->
    case just_tls:tcp_to_tls(TCPSocket, TLSOpts) of
        {ok, TLSSocket} ->
            State1 = reset_parser(State0),
            State2 = State1#state{socket = TLSSocket, sockmod = just_tls},
            activate_socket(State2),
            {reply, ok, State2, hibernate_or_timeout(State2)};
        {error, Reason} ->
            ?LOG_WARNING(#{what => tcp_to_tls_failed, reason => Reason,
                           dest_pid => State0#state.dest_pid}),
            {stop, normal, State0}
    end;
handle_call(get_socket, _From, #state{socket = Socket} = State) ->
    {reply, {ok, Socket}, State, hibernate_or_timeout(State)};
handle_call({set_dest_pid, DestPid}, _From, #state{dest_pid = undefined} = State) ->
    StateAfterReset = reset_parser(State),
    NewState = StateAfterReset#state{dest_pid = DestPid},
    activate_socket(NewState),
    {reply, ok, NewState, hibernate_or_timeout(NewState)};
handle_call(_Request, _From, State) ->
    {reply, ok, State, hibernate_or_timeout(State)}.

handle_cast(close, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State, hibernate_or_timeout(State)}.

handle_info({tcp, _TCPSocket, Data}, #state{sockmod = gen_tcp} = State) ->
    NewState = process_data(Data, State),
    {noreply, NewState, hibernate_or_timeout(NewState)};
handle_info({ssl, _TCPSocket, Data}, #state{sockmod = just_tls} = State) ->
    NewState = process_data(Data, State),
    {noreply, NewState, hibernate_or_timeout(NewState)};
handle_info({Tag, _Socket}, State) when Tag == tcp_closed; Tag == ssl_closed ->
    {stop, {shutdown, Tag}, State};
handle_info({Tag, _Socket, Reason}, State) when Tag == tcp_error; Tag == ssl_error ->
    {stop, {shutdown, Tag, Reason}, State};
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
-spec start_child(port(), atom(), options()) -> pid().
start_child(Socket, Shaper, Opts) ->
    {ok, Receiver} = supervisor:start_child(mongoose_s2s_socket_out_sup,
                                            [Socket, Shaper, Opts]),
    Receiver.

get_transport_info(ConnectionList) when is_list(ConnectionList) ->
    [get_transport_info(Pid) || {_, Pid, _, _} <- ConnectionList, is_pid(Pid)];
get_transport_info(TransportPid) when is_pid(TransportPid) ->
    State = sys:get_state(TransportPid),
    maps:from_list(lists:zip(record_info(fields, state),tl(tuple_to_list(State)))).

-spec tcp_to_tls(pid(), just_tls:options()) -> ok | {error, any()}.
tcp_to_tls(Receiver, TLSOpts) ->
    gen_server_call_or_noproc(Receiver, {tcp_to_tls, TLSOpts}).

-spec update_socket(socket_data()) -> socket_data().
update_socket(#socket_data{receiver = Receiver} = SocketData) ->
    case gen_server_call_or_noproc(Receiver, get_socket) of
        {ok, TLSSocket} ->
            SocketData#socket_data{socket = TLSSocket, sockmod = just_tls};
        {error, E} ->
            exit({invalid_socket_after_upgrade_to_tls, E})
    end.

-spec set_dest_pid(pid(), pid()) -> ok | {error, any()}.
set_dest_pid(Receiver, DestPid) ->
    gen_server:call(Receiver, {set_dest_pid, DestPid}).

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
activate_socket(#state{socket = Socket, sockmod = gen_tcp}) ->
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
                          sockmod = SockMod,
                          connection_type = ConnectionType} = State) ->
    ?LOG_DEBUG(#{what => received_xml_on_stream, packet => Data, dest_pid => DestPid}),
    Size = byte_size(Data),
    {Events, NewParser} =
        case exml_stream:parse(Parser, Data) of
            {ok, NParser, Elems} ->
                {[wrap_xml_elements_and_update_metrics(E, ConnectionType) || E <- Elems], NParser};
            {error, Reason} ->
                {[#xmlstreamerror{name = Reason}], Parser}
        end,
    {NewShaperState, Pause} = mongoose_shaper:update(ShaperState, Size),
    update_transport_metrics(Data, #{sockmod => SockMod, direction => in}),
    [gen_fsm_compat:send_event(DestPid, Event) || Event <- Events],
    maybe_pause(Pause, State),
    State#state{parser = NewParser, shaper_state = NewShaperState}.

wrap_xml_elements_and_update_metrics(El, s2s) ->
    mongoose_instrument:execute(xmpp_element_size_in,
                                #{connection_type => s2s},
                                #{t => out, el => El, byte_size => exml:xml_size(El)}),
    wrap_xml(El).

wrap_xml(#xmlel{} = E) ->
    {xmlstreamelement, E};
wrap_xml(E) ->
    E.

-spec update_transport_metrics(binary(),
                               #{direction := in | out,
                                 sockmod := socket_module()}) -> ok.
update_transport_metrics(Data, #{direction := in, sockmod := gen_tcp}) ->
    mongoose_instrument:execute(tcp_data_in, #{connection_type => s2s}, #{byte_size => byte_size(Data)});
update_transport_metrics(Data, #{direction := in, sockmod := just_tls}) ->
    mongoose_instrument:execute(tls_data_in, #{connection_type => s2s}, #{byte_size => byte_size(Data)});
update_transport_metrics(Data, #{direction := out, sockmod := gen_tcp}) ->
    mongoose_instrument:execute(tcp_data_out, #{connection_type => s2s}, #{byte_size => byte_size(Data)});
update_transport_metrics(Data, #{direction := out, sockmod := just_tls}) ->
    mongoose_instrument:execute(tls_data_out, #{connection_type => s2s}, #{byte_size => byte_size(Data)}).

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
shutdown_socket_and_wait_for_peer_to_close(Socket, gen_tcp) ->
    %% Standard trick to try to make sure all
    %% data sent to the tcp port is really delivered to the
    %% peer application before tcp port is closed so that the peer will
    %% get the correct stream end and not only a transport close.
    inet:setopts(Socket, [{active, false}]),
    gen_tcp:shutdown(Socket, write),
    %% Will return when other side has closed or after 30 s
    %% e.g. we do not want to hang if something goes wrong
    %% with the network but we want to maximise the odds that
    %% peer application gets all data sent on the tcp connection.
    gen_tcp:recv(Socket, 0, 30000);
shutdown_socket_and_wait_for_peer_to_close(Socket, just_tls) ->
    just_tls:close(Socket).
