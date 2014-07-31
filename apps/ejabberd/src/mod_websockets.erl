%%%===================================================================
%%% @copyright (C) 2013, Erlang Solutions Ltd.
%%% @doc Module providing support for websockets in ejabberd
%%% @end
%%%===================================================================
-module(mod_websockets).
-behaviour(gen_mod).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

%% gen_mod callbacks
-export([start/2,
         stop/1]).

%% cowboy_http_handler callbacks
-export([init/3,
         handle/2,
         terminate/3]).

%% cowboy_http_websocket_handler callbacks
-export([websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

%% ejabberd_socket compatibility
-export([starttls/2, starttls/3,
         compress/1, compress/3,
         reset_stream/1,
         send/2,
         send_xml/2,
         change_shaper/2,
         monitor/1,
         get_sockmod/1,
         close/1,
         peername/1]).

%% ejabberd_listener compatibility
-export([socket_type/0,
         start_listener/2]).

-export([stop/0]).


-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml_stream.hrl").

-define(LISTENER, ?MODULE).
-define(GEN_FSM, p1_fsm).
-define(NS_FRAMING, <<"urn:ietf:params:xml:ns:xmpp-framing">>).

-record(websocket, {pid :: pid(),
                    peername :: string()}).
-record(ws_state, {c2s_pid :: pid(),
                   open_tag :: stream | open,
                   parser :: exml_stream:parser()}).

%%--------------------------------------------------------------------
%% ejabberd_listener compatibility
%%--------------------------------------------------------------------
-spec socket_type() -> independent.
socket_type() ->
    independent.

% -spec start_listener(list())
start_listener({Port, IP, ws}, Opts) ->
    Dispatch = get_dispatch(Opts),
    NumAcceptors = gen_mod:get_opt(num_acceptors, Opts, 100),
    start_ws(NumAcceptors, Port, IP, Dispatch);

start_listener({Port, IP, wss}, Opts) ->
    Dispatch = get_dispatch(Opts),
    NumAcceptors = gen_mod:get_opt(num_acceptors, Opts, 100),
    SSLPort = gen_mod:get_opt(ssl_port, Opts, Port),
    SSLCert = gen_mod:get_opt(cert, Opts, undefined),
    SSLKey = gen_mod:get_opt(key, Opts, undefined),
    SSLKeyPass = gen_mod:get_opt(key_pass, Opts, undefined),
    start_wss(NumAcceptors, SSLPort, IP, SSLCert, SSLKey, SSLKeyPass, Dispatch).

%%--------------------------------------------------------------------
%% gen_mod callbacks
%%--------------------------------------------------------------------

start(_Host, Opts) ->
    NumAcceptors = gen_mod:get_opt(num_acceptors, Opts, 100),
    Port = gen_mod:get_opt(port, Opts, undefined),
    IP = gen_mod:get_opt(ip, Opts, {0,0,0,0}),
    SSLPort = gen_mod:get_opt(ssl_port, Opts, undefined),
    SSLCert = gen_mod:get_opt(cert, Opts, undefined),
    SSLKey = gen_mod:get_opt(key, Opts, undefined),
    SSLKeyPass = gen_mod:get_opt(key_pass, Opts, undefined),
    Dispatch = get_dispatch(Opts),
    {ok, _} = start_ws(NumAcceptors, Port, IP, Dispatch),
    {ok, _} = start_wss(NumAcceptors, SSLPort, IP, SSLCert, SSLKey,
                        SSLKeyPass, Dispatch).

start_ws(_, undefined, _, _) ->
    {ok, not_started};
start_ws(NumAcceptors, Port, IP, Dispatch) ->
    case cowboy:start_http(?LISTENER, NumAcceptors,
                                [{port, Port}, {ip, IP}],
                                [{env, [{dispatch, Dispatch}]}]) of
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

start_wss(_, _, _, undefined, undefined, undefined, _) ->
    {ok, not_started};
start_wss(NumAcceptors, Port, IP, Cert, Key, Pass, Dispatch) ->
    case cowboy:start_https({?LISTENER, secure}, NumAcceptors,
                                [
                                    {certfile, Cert},
                                    {keyfile, Key},
                                    {password, Pass},
                                    {ip, IP},
                                    {port, Port}
                                ],
                                [{env, [{dispatch, Dispatch}]}]) of
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {ok, Pid} ->
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop() ->
    stop(any).

stop(_Host) ->
    cowboy:stop_listener({?LISTENER, secure}),
    cowboy:stop_listener(?LISTENER),
    ok.

%%--------------------------------------------------------------------
%% cowboy_http_handler callbacks
%%--------------------------------------------------------------------

init(Transport, Req, Opts) ->
    ?DEBUG("cowboy init: ~p~n", [{Transport, Req, Opts}]),
    {upgrade, protocol, cowboy_websocket}.

handle(Req, State) ->
        {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
        ok.

%%--------------------------------------------------------------------
%% cowboy_http_websocket_handler callbacks
%%--------------------------------------------------------------------

% Called for every new websocket connection.
websocket_init(Transport, Req, Opts) ->
    ?DEBUG("websocket_init: ~p~n", [{Transport, Req, Opts}]),
    {Peer, NewReq} = cowboy_req:peer(Req),
    NewReq2 = cowboy_req:set_resp_header("Sec-WebSocket-Protocol", "xmpp", NewReq),
    SocketData = #websocket{pid=self(),
                            peername = Peer},
    C2SOpts = [{xml_socket, true} | Opts],
    case ejabberd_c2s:start({?MODULE, SocketData}, C2SOpts) of
        {ok, Pid} ->
            ?DEBUG("started c2s via websockets: ~p", [Pid]),
            State = #ws_state{c2s_pid = Pid},
            {ok, NewReq2, State};
        {error, Reason} ->
            ?WARNING_MSG("c2s start failed: ~p", [Reason]),
            {shutdown, NewReq2}
    end.

% Called when a text message arrives.
websocket_handle({text, Msg}, Req, State) ->
    ?DEBUG("Received: ~p", [Msg]),
    {ok, NewState} = handle_text(Msg, State),
    {ok, Req, NewState};

websocket_handle({binary, Msg}, Req, State) ->
    ?DEBUG("Received binary: ~p", [Msg]),
    {ok, NewState} = handle_text(Msg, State),
    {ok, Req, NewState};

% With this callback we can handle other kind of
% messages, like binary.
websocket_handle(Any, Req, State) ->
    ?DEBUG("Received non-text: ~p", [Any]),
    {ok, Req, State}.

% Other messages from the system are handled here.
websocket_info({send, Text}, Req, State) ->
    {reply, {text, Text}, Req, State};
websocket_info({send_xml, XML}, Req, State) ->
    XML1 = process_server_stream_root(replace_stream_ns(XML, State), State),
    Text = exml:to_iolist(XML1),
    {reply, {text, Text}, Req, State};
websocket_info(reset_stream, Req, #ws_state{parser = undefined} = State) ->
    {ok, Req, State};
websocket_info(reset_stream, Req, #ws_state{parser = Parser} = State) ->
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    {ok, Req, State#ws_state{ parser = NewParser, open_tag = undefined }};
websocket_info(stop, Req, #ws_state{parser = undefined} = State) ->
    {shutdown, Req, State};
websocket_info(stop, Req, #ws_state{parser = Parser} = State) ->
    exml_stream:free_parser(Parser),
    {shutdown, Req, State};
websocket_info(Info, Req, State) ->
    ?DEBUG("unknown info: ~p", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Callbacks implementation
%%--------------------------------------------------------------------

handle_text(Text, #ws_state{ parser = undefined } = State) ->
    ParserOpts = get_parser_opts(Text),
    {ok, Parser} = exml_stream:new_parser(ParserOpts),
    handle_text(Text, State#ws_state{ parser = Parser });
handle_text(Text, #ws_state{c2s_pid = C2S, parser = Parser} = State) ->
    {ok, NewParser, Elements} = exml_stream:parse(Parser, Text),
    State1 = State#ws_state{ parser = NewParser },
    {Elements1, State2} = process_client_stream_start(Elements, State1),
    [send_to_c2s(C2S, process_client_stream_end(
                        replace_stream_ns(Elem, State2), State2)) || Elem <- Elements1],
    {ok, State2}.

send_to_c2s(C2S, #xmlel{} = Element) ->
    send_to_c2s(C2S, {xmlstreamelement, Element});
send_to_c2s(C2S, StreamElement) ->
    ?GEN_FSM:send_event(C2S, StreamElement).

%%--------------------------------------------------------------------
%% ejabberd_socket compatibility
%%--------------------------------------------------------------------

starttls(SocketData, TLSOpts) ->
    starttls(SocketData, TLSOpts, <<>>).

starttls(_SocketData, _TLSOpts, _Data) ->
    throw({error, tls_not_allowed_on_websockets}).

compress(SocketData) ->
    compress(SocketData, <<>>, 0).

compress(_SocketData, _Data, _InflateSizeLimit) ->
    throw({error, compression_not_allowed_on_websockets}).

reset_stream(#websocket{pid = Pid} = SocketData) ->
    Pid ! reset_stream,
    SocketData.

send_xml(SocketData, {xmlstreamraw, Text}) ->
    send(SocketData, Text);
send_xml(SocketData, {xmlstreamelement, XML}) ->
    send_xml(SocketData, XML);
send_xml(#websocket{pid = Pid}, XML) ->
    Pid ! {send_xml, xml:escape_cdata_and_attr(XML)},
    ok.

send(#websocket{pid = Pid}, Data) ->
    Pid ! {send, Data},
    ok.

change_shaper(SocketData, _Shaper) ->
    SocketData. %% TODO: we ignore shapers for now

monitor(#websocket{pid = Pid}) ->
    erlang:monitor(process, Pid).

get_sockmod(_SocketData) ->
    ?MODULE.

close(#websocket{pid = Pid}) ->
    Pid ! close.

peername(#websocket{peername = PeerName}) ->
    {ok, PeerName}.

%%--------------------------------------------------------------------
%% Helpers for handling both
%% http://datatracker.ietf.org/doc/draft-ietf-xmpp-websocket
%% and older
%% http://tools.ietf.org/id/draft-moffitt-xmpp-over-websocket
%%--------------------------------------------------------------------

process_client_stream_start(Elements, #ws_state{ open_tag = OpenTag } = State)
  when OpenTag =/= undefined ->
    {Elements, State};
process_client_stream_start([#xmlstreamstart{ name = <<"stream", _/binary>>}
                             | _] = Elements, State) ->
    {Elements, State#ws_state{ open_tag = stream }};
process_client_stream_start([#xmlel{ name = <<"open">>, attrs = Attrs }], State) ->
    Attrs1 = lists:keyreplace(<<"xmlns">>, 1, Attrs, {<<"xmlns">>, ?NS_CLIENT}),
    Attrs2 = [{<<"xmlns:stream">>, ?NS_STREAM} | Attrs1],
    NewStart = #xmlstreamstart{ name = <<"stream:stream">>, attrs = Attrs2 },
    {[NewStart], State#ws_state{ open_tag = open }};
process_client_stream_start(_, #ws_state{ c2s_pid = C2SPid } = State) ->
    send_to_c2s(C2SPid, {xmlstreamerror, <<"Unknown opening tag">>}),
    {[], State}.

process_client_stream_end(#xmlel{ name = <<"close">> }, #ws_state{ open_tag = open }) ->
    #xmlstreamend{ name = <<"stream:stream">> };
process_client_stream_end(Element, _) ->
    Element.

process_server_stream_root(#xmlstreamstart{ name = <<"stream", _/binary>>, attrs = Attrs },
                           #ws_state{ open_tag = open }) ->
    Attrs1 = lists:keydelete(<<"xmlns:stream">>, 1, Attrs),
    Attrs2 = lists:keyreplace(<<"xmlns">>, 1, Attrs1, {<<"xmlns">>, ?NS_FRAMING}),
    #xmlel{ name = <<"open">>, attrs = Attrs2 };
process_server_stream_root(#xmlstreamend{ name = <<"stream", _/binary>> },
                           #ws_state{ open_tag = open }) ->
    #xmlel{ name = <<"close">>, attrs = [{<<"xmlns">>, ?NS_FRAMING}] };
process_server_stream_root(Element, _) ->
    Element.

replace_stream_ns(#xmlel{ name = <<"stream:", ElementName/binary>> } = Element,
                  #ws_state{ open_tag = open }) ->
    Element#xmlel{ name = ElementName, attrs = [{<<"xmlns">>, ?NS_STREAM} | Element#xmlel.attrs] };
replace_stream_ns(Element, _State) ->
    Element.

get_parser_opts(<<"<open", _/binary>>) -> [{infinite_stream, true}, {autoreset, true}]; % new-type WS
get_parser_opts(_) -> []. % old-type WS

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

get_dispatch(Opts) ->
    WSHost = gen_mod:get_opt(host, Opts, '_'), %% default to any
    WSPrefix = gen_mod:get_opt(prefix, Opts, "/ws-xmpp"),
    cowboy_router:compile([{WSHost, [{WSPrefix, ?MODULE, Opts}] }]).
