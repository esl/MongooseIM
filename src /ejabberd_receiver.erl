%%%----------------------------------------------------------------------
%%% File    : ejabberd_receiver.erl
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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_receiver).
-author('alexey@process-one.net').

-behaviour(gen_server).

%% API
-export([start_link/4,
         start/3,
         start/4,
         change_shaper/2,
         starttls/2,
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
                timeout}).
-type state() :: #state{}.

-define(HIBERNATE_TIMEOUT, 90000).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok, Pid} | ignore | {error, Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
-spec start_link(_, _, _, _) -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(Socket, SockMod, Shaper, MaxStanzaSize) ->
    gen_server:start_link(
      ?MODULE, [Socket, SockMod, Shaper, MaxStanzaSize], []).

%%--------------------------------------------------------------------
%% Function: start() -> {ok, Pid} | ignore | {error, Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start(Socket, SockMod, Shaper) ->
    start(Socket, SockMod, Shaper, infinity).

start(Socket, SockMod, Shaper, MaxStanzaSize) ->
    {ok, Pid} = supervisor:start_child(
                  ejabberd_receiver_sup,
                  [Socket, SockMod, Shaper, MaxStanzaSize]),
    Pid.

-spec change_shaper(atom() | pid() | {atom(), _} | {'via', _, _}, _) -> 'ok'.
change_shaper(Pid, Shaper) ->
    gen_server:cast(Pid, {change_shaper, Shaper}).

starttls(Pid, TLSSocket) ->
    gen_server_call_or_noproc(Pid, {starttls, TLSSocket}).

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
init([Socket, SockMod, Shaper, MaxStanzaSize]) ->
    ShaperState = shaper:new(Shaper),
    Timeout = case SockMod of
                  ssl ->
                      20;
                  _ ->
                      infinity
              end,
    {ok, #state{socket = Socket,
                sock_mod = SockMod,
                shaper_state = ShaperState,
                max_stanza_size = MaxStanzaSize,
                stanza_chunk_size = 0,
                timeout = Timeout}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({starttls, TLSSocket}, _From, State) ->
    StateAfterReset = reset_parser(State),
    NewState = StateAfterReset#state{socket = TLSSocket,
                                 sock_mod = fast_tls},
    case fast_tls:recv_data(TLSSocket, <<"">>) of
        {ok, TLSData} ->
            {reply, ok, process_data(TLSData, NewState), ?HIBERNATE_TIMEOUT};
        {error, _Reason} ->
            {stop, normal, ok, NewState}
    end;
handle_call({compress, ZlibSocket}, _From,
  #state{c2s_pid = C2SPid} = State) ->
    StateAfterReset = reset_parser(State),
    NewState = StateAfterReset#state{socket = ZlibSocket,
                                 sock_mod = ejabberd_zlib},
    case ejabberd_zlib:recv_data(ZlibSocket, "") of
        {ok, ZlibData} ->
            {reply, ok, process_data(ZlibData, NewState), ?HIBERNATE_TIMEOUT};
        {error, inflate_size_exceeded} ->
            apply(gen_fsm(), send_event,
                [C2SPid, {xmlstreamerror, <<"child element too big">>}]),
            {reply, ok, NewState, ?HIBERNATE_TIMEOUT};
        {error, inflate_error} ->
            {stop, normal, ok, NewState}
    end;
handle_call({become_controller, C2SPid}, _From, State) ->
    StateAfterReset = reset_parser(State),
    NewState = StateAfterReset#state{c2s_pid = C2SPid},
    activate_socket(NewState),
    Reply = ok,
    {reply, Reply, NewState, ?HIBERNATE_TIMEOUT};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State, ?HIBERNATE_TIMEOUT}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({change_shaper, Shaper}, State) ->
    NewShaperState = shaper:new(Shaper),
    {noreply, State#state{shaper_state = NewShaperState}, ?HIBERNATE_TIMEOUT};
handle_cast(close, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

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
        fast_tls ->
            mongoose_metrics:update(global,
                            [data, xmpp, received, encrypted_size], size(Data)),
            case fast_tls:recv_data(Socket, Data) of
                {ok, TLSData} ->
                    {noreply, process_data(TLSData, State),
                    ?HIBERNATE_TIMEOUT};
                {error, _Reason} ->
                    {stop, normal, State}
            end;
        ejabberd_zlib ->
            mongoose_metrics:update(global,
                           [data, xmpp, received, compressed_size], size(Data)),
            case ejabberd_zlib:recv_data(Socket, Data) of
                {ok, ZlibData} ->
                    {noreply, process_data(ZlibData, State),
                    ?HIBERNATE_TIMEOUT};
                {error, inflate_size_exceeded} ->
                    apply(gen_fsm(), send_event,
                       [C2SPid, {xmlstreamerror, <<"child element too big">>}]),
                    {noreply, State, ?HIBERNATE_TIMEOUT};
                {error, inflate_error} ->
                    {stop, normal, State}
            end;
        _ ->
            {noreply, process_data(Data, State), ?HIBERNATE_TIMEOUT}
    end;
handle_info({Tag, _TCPSocket}, State)
  when (Tag == tcp_closed) or (Tag == ssl_closed) ->
    {stop, normal, State};
handle_info({Tag, _TCPSocket, Reason}, State)
  when (Tag == tcp_error) or (Tag == ssl_error) ->
    case Reason of
        timeout ->
            {noreply, State, ?HIBERNATE_TIMEOUT};
        _ ->
            {stop, normal, State}
    end;
handle_info({timeout, _Ref, activate}, State) ->
    activate_socket(State),
    {noreply, State, ?HIBERNATE_TIMEOUT};
handle_info(timeout, State) ->
    proc_lib:hibernate(gen_server, enter_loop, [?MODULE, [], State]),
    {noreply, State, ?HIBERNATE_TIMEOUT};
handle_info(_Info, State) ->
    {noreply, State, ?HIBERNATE_TIMEOUT}.

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
        _ -> gen_fsm:send_event(C2SPid, closed)
    end,
    catch (State#state.sock_mod):close(State#state.socket),
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
activate_socket(#state{socket = Socket,
                       sock_mod = SockMod}) ->
    PeerName =
        case SockMod of
            gen_tcp ->
                inet:setopts(Socket, [{active, once}]),
                inet:peername(Socket);
            _ ->
                SockMod:setopts(Socket, [{active, once}]),
                SockMod:peername(Socket)
        end,
    case PeerName of
        {error, _Reason} ->
            self() ! {tcp_closed, Socket};
        {ok, _} ->
            ok
    end.

%% @doc Data processing for connectors directly generating xmlel in
%% Erlang data structure.
%% WARNING: Shaper does not work with Erlang data structure.
-spec process_data(binary() | maybe_improper_list(), state()) -> state().
process_data([], State) ->
    activate_socket(State),
    State;
process_data([Element|Els], #state{c2s_pid = C2SPid} = State)
  when element(1, Element) == xmlel;
       element(1, Element) == xmlstreamstart;
      element(1, Element) == xmlstreamelement;
       element(1, Element) == xmlstreamend ->
    case C2SPid of
        undefined ->
            State;
        _ ->
            catch gen_fsm:send_event(C2SPid, element_wrapper(Element)),
            process_data(Els, State)
    end;
%% Data processing for connectors receivind data as string.
process_data(Data, #state{parser = Parser,
                          shaper_state = ShaperState,
                          stanza_chunk_size = ChunkSize,
                          c2s_pid = C2SPid} = State) ->
    ?DEBUG("Received XML on stream = \"~s\"", [Data]),
    Size = size(Data),
    maybe_run_keep_alive_hook(Size, State),
    {C2SEvents, NewParser} =
        case exml_stream:parse(Parser, Data) of
            {ok, NParser, Elems} -> {[wrap_if_xmlel(E) || E <- Elems], NParser};
            {error, Reason} -> {[{xmlstreamerror, Reason}], Parser}
        end,
    NewChunkSize = update_stanza_size(C2SEvents, ChunkSize, Size),
    {NewShaperState, Pause} = shaper:update(ShaperState, Size),
    [gen_fsm:send_event(C2SPid, Event) || Event <- C2SEvents],
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
    gen_fsm:send_all_state_event(C2SPid, keep_alive_packet);
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
    catch exit:{noproc, Extra} ->
        {error, {noproc, Extra}}
    end.

gen_fsm() -> p1_fsm.
