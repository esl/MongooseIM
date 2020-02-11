%%%----------------------------------------------------------------------
%% File    : ejabberd_receiver.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Socket receiver for C2S and S2S connections
%%% Created : 10 Nov 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_receiver).
-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/4,
         start/4,
         change_shaper/2,
         starttls/2,
         get_socket/1,
         compress/2,
         become_controller/2,
         close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("mongoose.hrl").
-include("jlib.hrl").

-record(state, {socket,
                sock_mod,
                shaper_state,
                c2s_pid,
                max_stanza_size,
                stanza_chunk_size,
                parser,
                timeout,
                hibernate_after = 0 :: non_neg_integer()}).
-type state() :: #state{}.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok, Pid} | ignore | {error, Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec start_link(_, _, _, _) -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(Socket, SockMod, Shaper, ConnOpts) ->
    gen_server:start_link(
      ?MODULE, [Socket, SockMod, Shaper, ConnOpts], []).

%%--------------------------------------------------------------------
%% Function: start() -> {ok, Pid} | ignore | {error, Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Socket, SockMod, Shaper, ConnOpts) ->
    {ok, Pid} = supervisor:start_child(
                  ejabberd_receiver_sup,
                  [Socket, SockMod, Shaper, ConnOpts]),
    Pid.

-spec change_shaper(atom() | pid() | {atom(), _} | {'via', _, _}, _) -> 'ok'.
change_shaper(Pid, Shaper) ->
    gen_server:cast(Pid, {change_shaper, Shaper}).

starttls(Pid, TLSOpts) ->
    gen_server_call_or_noproc(Pid, {starttls, TLSOpts}).

get_socket(Pid) ->
    gen_server_call_or_noproc(Pid, get_socket).

compress(Pid, ZlibSocket) ->
    gen_server_call_or_noproc(Pid, {compress, ZlibSocket}).

become_controller(Pid, C2SPid) ->
    gen_server:call(Pid, {become_controller, C2SPid}).

-spec close(atom() | pid() | {atom(), _} | {'via', _, _}) -> 'ok'.
close(Pid) ->
    gen_server:cast(Pid, close).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
-spec init([any(), ...]) -> {'ok', state()}.
init([Socket, SockMod, Shaper, ConnOpts]) ->
    ShaperState = shaper:new(Shaper),
    Timeout = case SockMod of
                  ssl ->
                      20;
                  _ ->
                      infinity
              end,
    MaxStanzaSize =
        case lists:keyfind(max_stanza_size, 1, ConnOpts) of
            {_, Size} -> Size;
            _ -> infinity
        end,
    HibernateAfter =
        case lists:keyfind(hibernate_after, 1, ConnOpts) of
            {_, HA} -> HA;
            _ -> 0
        end,
    {ok, #state{socket = Socket,
                sock_mod = SockMod,
                shaper_state = ShaperState,
                max_stanza_size = MaxStanzaSize,
                stanza_chunk_size = 0,
                timeout = Timeout,
                hibernate_after = HibernateAfter}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(get_socket, _From, #state{socket = Socket} = State) ->
    {reply, {ok, Socket}, State, maybe_hibernate(State)};
handle_call({starttls, TLSOpts}, From, #state{socket = TCPSocket} = State) ->
    %% the next message from client is part of TLS handshake, it must
    %% be handled by TLS library (another process in case of just_tls)
    %% so deactivating the socket.
    deactivate_socket(State),
    %% TLS handshake always starts from client's request, let
    %% ejabberd_socket finish starttls negotiation and notify
    %% client that it can start TLS handshake.
    gen_server:reply(From, ok),
    case ejabberd_tls:tcp_to_tls(TCPSocket, TLSOpts) of
        {ok, TLSSocket} ->
            StateAfterReset = reset_parser(State),
            NewState = StateAfterReset#state{socket   = TLSSocket,
                                             sock_mod = ejabberd_tls},
            %% fast_tls requires dummy recv_data/2 call to accomplish TLS
            %% handshake. such call is simply ignored by just_tls backend.
            case ejabberd_tls:recv_data(TLSSocket, <<"">>) of
                {ok, TLSData} ->
                    NewState2 = process_data(TLSData, NewState),
                    {noreply, NewState2, maybe_hibernate(NewState2)};
                {error, Reason} ->
                    ?WARNING_MSG("tcp_to_tls failed with reason ~p~n", [Reason]),
                    {stop, normal, NewState}
            end;
        {error, Reason} ->
            ?WARNING_MSG("tcp_to_tls failed with reason ~p~n", [Reason]),
            {stop, normal, State}
    end;
handle_call({compress, ZlibSocket}, _From,
  #state{c2s_pid = C2SPid} = State) ->
    StateAfterReset = reset_parser(State),
    NewState = StateAfterReset#state{socket = ZlibSocket,
                                 sock_mod = ejabberd_zlib},
    case ejabberd_zlib:recv_data(ZlibSocket, "") of
        {ok, ZlibData} ->
            NewState2 = process_data(ZlibData, NewState),
            {reply, ok, NewState2, maybe_hibernate(NewState2)};
        {error, inflate_size_exceeded} ->
            apply(gen_fsm(), send_event,
                [C2SPid, {xmlstreamerror, <<"child element too big">>}]),
            {reply, ok, NewState, maybe_hibernate(NewState)};
        {error, inflate_error} ->
            {stop, normal, ok, NewState}
    end;
handle_call({become_controller, C2SPid}, _From, State) ->
    StateAfterReset = reset_parser(State),
    NewState = StateAfterReset#state{c2s_pid = C2SPid},
    activate_socket(NewState),
    Reply = ok,
    {reply, Reply, NewState, maybe_hibernate(NewState)};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State, maybe_hibernate(State)}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({change_shaper, Shaper}, State) ->
    NewShaperState = shaper:new(Shaper),
    NewState = State#state{shaper_state = NewShaperState},
    {noreply, NewState, maybe_hibernate(NewState)};
handle_cast(close, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State, maybe_hibernate(State)}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({Tag, _TCPSocket, Data},
      #state{socket = Socket,
       c2s_pid = C2SPid,
       sock_mod = SockMod} = State)
  when (Tag == tcp) or (Tag == ssl) ->
    case SockMod of
        ejabberd_tls ->
            mongoose_metrics:update(global,
                            [data, xmpp, received, encrypted_size], size(Data)),
            case ejabberd_tls:recv_data(Socket, Data) of
                {ok, TLSData} ->
                    NewState = process_data(TLSData, State),
                    {noreply, NewState, maybe_hibernate(NewState)};
                {error, _Reason} ->
                    {stop, normal, State}
            end;
        ejabberd_zlib ->
            mongoose_metrics:update(global,
                           [data, xmpp, received, compressed_size], size(Data)),
            case ejabberd_zlib:recv_data(Socket, Data) of
                {ok, ZlibData} ->
                    NewState = process_data(ZlibData, State),
                    {noreply, NewState, maybe_hibernate(NewState)};
                {error, inflate_size_exceeded} ->
                    apply(gen_fsm(), send_event,
                       [C2SPid, {xmlstreamerror, <<"child element too big">>}]),
                    {noreply, State, maybe_hibernate(State)};
                {error, inflate_error} ->
                    {stop, normal, State}
            end;
        _ ->
            NewState = process_data(Data, State),
            {noreply, NewState, maybe_hibernate(NewState)}
    end;
handle_info({Tag, _TCPSocket}, State)
  when (Tag == tcp_closed) or (Tag == ssl_closed) ->
    {stop, normal, State};
handle_info({Tag, _TCPSocket, Reason}, State)
  when (Tag == tcp_error) or (Tag == ssl_error) ->
    case Reason of
        timeout ->
            {noreply, State, maybe_hibernate(State)};
        _ ->
            {stop, normal, State}
    end;
handle_info({timeout, _Ref, activate}, State) ->
    activate_socket(State),
    {noreply, State, maybe_hibernate(State)};
handle_info(timeout, State) ->
    {noreply, State, hibernate()};
handle_info(_Info, State) ->
    {noreply, State, maybe_hibernate(State)}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{parser = Parser,
        c2s_pid = C2SPid} = State) ->
    free_parser(Parser),
    case C2SPid of
        undefined -> ok;
        _ -> gen_fsm_compat:send_event(C2SPid, closed)
    end,
    catch shutdown_socket_and_wait_for_peer_to_close(State#state.socket, State#state.sock_mod),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

-spec activate_socket(state()) -> 'ok' | {'tcp_closed', _}.
activate_socket(#state{socket = Socket, sock_mod = gen_tcp}) ->
    inet:setopts(Socket, [{active, once}]),
    PeerName = inet:peername(Socket),
    resolve_peername(PeerName, Socket);
activate_socket(#state{socket = Socket, sock_mod = SockMod}) ->
    SockMod:setopts(Socket, [{active, once}]),
    PeerName = SockMod:peername(Socket),
    resolve_peername(PeerName, Socket).

resolve_peername({ok, _}, _Socket) ->
    ok;
resolve_peername({error, _Reason}, Socket) ->
    self() ! {tcp_closed, Socket}.

-spec deactivate_socket(state()) -> 'ok' | {'error', _}.
deactivate_socket(#state{socket = Socket,
                         sock_mod = SockMod}) ->
    case SockMod of
      gen_tcp -> inet:setopts(Socket, [{active, false}]);
      _ -> SockMod:setopts(Socket, [{active, false}])
    end.

%% @doc Data processing for connectors directly generating xmlel in
%% Erlang data structure.
%% WARNING: Shaper does not work with Erlang data structure.
-spec process_data(binary() | maybe_improper_list(), state()) -> state().
process_data([], State) ->
    activate_socket(State),
    State;
process_data(Els, #state{c2s_pid = undefined} = State) when is_list(Els) ->
    State;
process_data([Element|Els], #state{c2s_pid = C2SPid} = State)
  when element(1, Element) == xmlel;
       element(1, Element) == xmlstreamstart;
       element(1, Element) == xmlstreamelement;
       element(1, Element) == xmlstreamend ->
    catch gen_fsm_compat:send_event(C2SPid, element_wrapper(Element)),
    process_data(Els, State);
%% Data processing for connectors receivind data as string.
process_data(Data, #state{parser = Parser,
                          shaper_state = ShaperState,
                          stanza_chunk_size = ChunkSize,
                          c2s_pid = C2SPid} = State) ->
    ?DEBUG("Received XML on stream = \"~s\"", [Data]),
    Size = byte_size(Data),
    maybe_run_keep_alive_hook(Size, State),
    {C2SEvents, NewParser} =
        case exml_stream:parse(Parser, Data) of
            {ok, NParser, Elems} -> {[wrap_if_xmlel(E) || E <- Elems], NParser};
            {error, Reason} -> {[{xmlstreamerror, Reason}], Parser}
        end,
    NewChunkSize = update_stanza_size(C2SEvents, ChunkSize, Size),
    {NewShaperState, Pause} = shaper:update(ShaperState, Size),
    [gen_fsm_compat:send_event(C2SPid, Event) || Event <- C2SEvents],
    maybe_pause(Pause, State),
    State#state{parser = NewParser, shaper_state = NewShaperState, stanza_chunk_size = NewChunkSize}.

wrap_if_xmlel(#xmlel{} = E) -> {xmlstreamelement, E};
wrap_if_xmlel(E) -> E.

update_stanza_size([_|_], ChunkSize, Size) ->
    mongoose_metrics:update(global,
                            [data, xmpp, received, xml_stanza_size], ChunkSize + Size),
    0;
update_stanza_size(_, ChunkSize, Size) ->
    ChunkSize + Size.

maybe_pause(_, #state{c2s_pid = undefined}) ->
    ok;
maybe_pause(Pause, _State) when Pause > 0 ->
    erlang:start_timer(Pause, self(), activate);
maybe_pause(_, State) ->
    activate_socket(State).

maybe_run_keep_alive_hook(Size, #state{c2s_pid = C2SPid})
  when Size < 3, is_pid(C2SPid) ->
    %% yes it can happen that the data is shorter than 3 bytes and contain
    %% some part of xml but this will not harm the keep_alive_hook
    gen_fsm_compat:send_all_state_event(C2SPid, keep_alive_packet);
maybe_run_keep_alive_hook(_, _) ->
    ok.

%% @doc Element coming from XML parser are wrapped inside xmlstreamelement
%% When we receive directly xmlel tuple (from a socket module
%% speaking directly Erlang XML), we wrap it inside the same
%% xmlstreamelement coming from the XML parser.
-spec element_wrapper(exml:element() | tuple()) -> tuple().
element_wrapper(#xmlel{} = XMLElement) ->
    {xmlstreamelement, XMLElement};
element_wrapper(Element) ->
    Element.

reset_parser(#state{parser = undefined, max_stanza_size = Size} = State) ->
    MaxSize = case Size of
                  infinity -> 0;
                  _ -> Size
              end,
    {ok, NewParser} = exml_stream:new_parser([{start_tag, <<"stream:stream">>},
                                              {max_child_size, MaxSize}]),
    State#state{parser = NewParser, stanza_chunk_size = 0};
reset_parser(#state{parser = Parser} = State) ->
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    State#state{parser = NewParser, stanza_chunk_size = 0}.

free_parser(undefined) ->
    ok;
free_parser(Parser) ->
    exml_stream:free_parser(Parser).

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

gen_fsm() -> p1_fsm.

-spec hibernate() -> hibernate | infinity.
hibernate() ->
    case process_info(self(), message_queue_len) of
        {_, 0} -> hibernate;
        _ -> infinity
    end.

-spec maybe_hibernate(state()) -> hibernate | infinity | pos_integer().
maybe_hibernate(#state{hibernate_after = 0}) -> hibernate();
maybe_hibernate(#state{hibernate_after = HA}) -> HA.

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
shutdown_socket_and_wait_for_peer_to_close(Socket, SockMod) ->
    SockMod:close(Socket).
