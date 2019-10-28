%%%===================================================================
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%% @doc Module providing support for websockets in MongooseIM
%%% @end
%%%===================================================================
-module(mod_websockets).

-behaviour(cowboy_websocket).
-behaviour(mongoose_transport).

%% cowboy_http_websocket_handler callbacks
-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

%% ejabberd_socket compatibility
-export([starttls/2, starttls/3,
         compress/1, compress/3,
         send/2,
         send_xml/2,
         change_shaper/2,
         monitor/1,
         get_sockmod/1,
         close/1,
         peername/1,
         get_peer_certificate/1,
         set_ping/2,
         disable_ping/1]).

-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml_stream.hrl").

-define(LISTENER, ?MODULE).
-define(NS_FRAMING, <<"urn:ietf:params:xml:ns:xmpp-framing">>).
-define(NS_COMPONENT, <<"jabber:component:accept">>).

-record(websocket, {
          pid :: pid(),
          peername :: mongoose_transport:peer(),
          peercert :: undefined | binary()
         }).
-record(ws_state, {
          peer :: mongoose_transport:peer() | undefined,
          fsm_pid :: pid() | undefined,
          open_tag :: stream | open | undefined,
          parser :: exml_stream:parser() | undefined,
          opts :: proplists:proplist() | undefined,
          ping_rate :: integer() | none,
          max_stanza_size :: integer() | infinity,
          peercert :: undefined | passed | binary()
          %% the passed value is used to clear the certificate from the handlers state after it's passed down to the socket()
         }).

-type socket() :: #websocket{}.

%%--------------------------------------------------------------------
%% Common callbacks for all cowboy behaviours
%%--------------------------------------------------------------------

init(Req, Opts) ->
    Peer = cowboy_req:peer(Req),
    PeerCert = cowboy_req:cert(Req),
    Req1 = add_sec_websocket_protocol_header(Req),
    ?DEBUG("cowboy init: ~p~n", [{Req, Opts}]),
    Timeout = gen_mod:get_opt(timeout, Opts, 60000),

    AllModOpts = [{peer, Peer}, {peercert, PeerCert} | Opts],
    %% upgrade protocol
    {cowboy_websocket, Req1, AllModOpts, #{idle_timeout => Timeout}}.

terminate(_Reason, _Req, _State) ->
    ok.

%%--------------------------------------------------------------------
%% cowboy_http_websocket_handler callbacks
%%--------------------------------------------------------------------

% Called for every new websocket connection.
websocket_init(Opts) ->
    ?DEBUG("websocket_init: ~p~n", [Opts]),
    PingRate = gen_mod:get_opt(ping_rate, Opts, none),
    MaxStanzaSize = gen_mod:get_opt(max_stanza_size, Opts, infinity),
    Peer = gen_mod:get_opt(peer, Opts),
    PeerCert = gen_mod:get_opt(peercert, Opts),
    ?DEBUG("ping rate is ~p", [PingRate]),
    maybe_send_ping_request(PingRate),
    State = #ws_state{opts = Opts,
                      ping_rate = PingRate,
                      max_stanza_size = MaxStanzaSize,
                      peer = Peer,
                      peercert = PeerCert},
    {ok, State}.

% Called when a text message arrives.
websocket_handle({text, Msg}, State) ->
    ?DEBUG("Received: ~p", [Msg]),
    handle_text(Msg, State);

websocket_handle({binary, Msg}, State) ->
    ?DEBUG("Received binary: ~p", [Msg]),
    handle_text(Msg, State);

websocket_handle({pong, Payload}, State) ->
    ?DEBUG("Received pong frame: ~p", [Payload]),
    {ok, State};

% With this callback we can handle other kind of
% messages, like binary.
websocket_handle(Any, State) ->
    ?DEBUG("Received non-text: ~p", [Any]),
    {ok, State}.

% Other messages from the system are handled here.
websocket_info({send, Text}, State) ->
    ?DEBUG("Sent text: ~s", [Text]),
    {reply, {text, Text}, State};
websocket_info({send_xml, XML}, State) ->
    XML1 = process_server_stream_root(replace_stream_ns(XML, State), State),
    Text = exml:to_iolist(XML1),
    ?DEBUG("Sent XML: ~s", [Text]),
    {reply, {text, Text}, State};
websocket_info({set_ping, Value}, State = #ws_state{ping_rate = none})
  when is_integer(Value) and (Value > 0)->
    send_ping_request(Value),
    {ok, State#ws_state{ping_rate = Value}};
websocket_info({set_ping, Value}, State) when is_integer(Value) and (Value > 0)->
    {ok, State#ws_state{ping_rate = Value}};
websocket_info(disable_ping, State)->
    {ok, State#ws_state{ping_rate = none}};
websocket_info(do_ping, State = #ws_state{ping_rate = none}) ->
    %% probalby someone disabled pings
    {ok, State};
websocket_info(do_ping, State) ->
    %% send ping frame to the client
    send_ping_request(State#ws_state.ping_rate),
    {reply, ping, State};
websocket_info(stop, #ws_state{parser = undefined} = State) ->
    {stop, State};
websocket_info(stop, #ws_state{parser = Parser} = State) ->
    exml_stream:free_parser(Parser),
    {stop, State};
websocket_info(Info, State) ->
    ?DEBUG("unknown info: ~p", [Info]),
    {ok, State}.

%%--------------------------------------------------------------------
%% Callbacks implementation
%%--------------------------------------------------------------------

handle_text(Text, #ws_state{ parser = undefined } = State) ->
    ParserOpts = get_parser_opts(Text, State),
    {ok, Parser} = exml_stream:new_parser(ParserOpts),
    handle_text(Text, State#ws_state{ parser = Parser });
handle_text(Text, #ws_state{parser = Parser} = State) ->
    case exml_stream:parse(Parser, Text) of
    {ok, NewParser, Elements} ->
        State1 = State#ws_state{ parser = NewParser },
        case maybe_start_fsm(Elements, State1) of
            {ok, State2} ->
                process_client_elements(Elements, State2);
            {stop, _} = Shutdown ->
                Shutdown
        end;
    {error, Reason} ->
        process_parse_error(Reason, State)
    end.

process_client_elements(Elements, #ws_state{fsm_pid = FSM} = State) ->
    {Elements1, State1} = process_client_stream_start(Elements, State),
    [send_to_fsm(FSM, process_client_stream_end(
                replace_stream_ns(Elem, State1), State1)) || Elem <- Elements1],
    {ok, State1}.

process_parse_error(_Reason, #ws_state{fsm_pid = undefined} = State) ->
    {stop, State};
process_parse_error(Reason, #ws_state{fsm_pid = FSM} = State) ->
    send_to_fsm(FSM, {xmlstreamerror, Reason}),
    {ok, State}.

send_to_fsm(FSM, #xmlel{} = Element) ->
    send_to_fsm(FSM, {xmlstreamelement, Element});
send_to_fsm(FSM, StreamElement) ->
    p1_fsm:send_event(FSM, StreamElement).

maybe_start_fsm([#xmlstreamstart{ name = <<"stream", _/binary>>, attrs = Attrs}
                 | _],
                #ws_state{fsm_pid = undefined, opts = Opts}=State) ->
    case lists:keyfind(<<"xmlns">>, 1, Attrs) of
        {<<"xmlns">>, ?NS_COMPONENT} ->
            ServiceOpts = gen_mod:get_opt(ejabberd_service, Opts, []),
            do_start_fsm(ejabberd_service, ServiceOpts, State);
        _ ->
            {stop, State}
    end;
maybe_start_fsm([#xmlel{ name = <<"open">> }],
                #ws_state{fsm_pid = undefined, opts = Opts}=State) ->
    do_start_fsm(ejabberd_c2s, Opts, State);
maybe_start_fsm(_Els, State) ->
    {ok, State}.

do_start_fsm(FSMModule, Opts, State = #ws_state{peer = Peer, peercert = PeerCert}) ->
    SocketData = #websocket{pid = self(),
                            peername = Peer,
                            peercert = PeerCert},
    Opts1 = [{xml_socket, true} | Opts],
    case call_fsm_start(FSMModule, SocketData, Opts1) of
        {ok, Pid} ->
            ?DEBUG("started ~p via websockets: ~p", [FSMModule, Pid]),
            NewState = State#ws_state{fsm_pid = Pid, peercert = passed},
            {ok, NewState};
        {error, Reason} ->
            ?WARNING_MSG("~p start failed: ~p", [FSMModule, Reason]),
            {stop, State#ws_state{peercert = passed}}
    end.

call_fsm_start(ejabberd_c2s, SocketData, Opts) ->
    ejabberd_c2s:start({?MODULE, SocketData}, Opts);
call_fsm_start(ejabberd_service, SocketData, Opts) ->
    ejabberd_service:start({?MODULE, SocketData}, Opts).

%%--------------------------------------------------------------------
%% ejabberd_socket compatibility
%%--------------------------------------------------------------------
-spec starttls(socket(), _) -> no_return().
starttls(SocketData, TLSOpts) ->
    starttls(SocketData, TLSOpts, <<>>).

-spec starttls(socket(), _, _) -> no_return().
starttls(_SocketData, _TLSOpts, _Data) ->
    throw({error, tls_not_allowed_on_websockets}).

-spec compress(socket()) -> no_return().
compress(SocketData) ->
    compress(SocketData, <<>>, 0).

-spec compress(socket(), _, _) -> no_return().
compress(_SocketData, _Data, _InflateSizeLimit) ->
    throw({error, compression_not_allowed_on_websockets}).

-spec send_xml(socket(), mongoose_transport:send_xml_input()) -> ok.
send_xml(SocketData, {xmlstreamraw, Text}) ->
    send(SocketData, Text);
send_xml(SocketData, {xmlstreamelement, XML}) ->
    send_xml(SocketData, XML);
send_xml(#websocket{pid = Pid}, XML) ->
    Pid ! {send_xml, XML},
    ok.

send(#websocket{pid = Pid}, Data) ->
    Pid ! {send, Data},
    ok.

change_shaper(SocketData, _Shaper) ->
    SocketData. %% TODO: we ignore shapers for now

-spec monitor(socket()) -> reference().
monitor(#websocket{pid = Pid}) ->
    erlang:monitor(process, Pid).

get_sockmod(_SocketData) ->
    ?MODULE.

close(#websocket{pid = Pid}) ->
    Pid ! stop.

-spec peername(socket()) -> mongoose_transport:peername_return().
peername(#websocket{peername = PeerName}) ->
    {ok, PeerName}.

get_peer_certificate(#websocket{peercert = undefined}) ->
    no_peer_cert;
get_peer_certificate(#websocket{peercert = PeerCert}) ->
    Decoded = public_key:pkix_decode_cert(PeerCert, plain),
    {ok, Decoded}.

set_ping(#websocket{pid = Pid}, Value) ->
    Pid ! {set_ping, Value}.

disable_ping(#websocket{pid = Pid}) ->
    Pid ! disable_ping.

%%--------------------------------------------------------------------
%% Helpers for handling both
%% http://datatracker.ietf.org/doc/draft-ietf-xmpp-websocket
%% and older
%% http://tools.ietf.org/id/draft-moffitt-xmpp-over-websocket
%%--------------------------------------------------------------------

process_client_stream_start([#xmlstreamstart{ name = <<"stream", _/binary>>}
                             | _] = Elements, State) ->
    {Elements, State#ws_state{ open_tag = stream }};
process_client_stream_start([#xmlel{ name = <<"open">>, attrs = Attrs }], State) ->
    Attrs1 = lists:keyreplace(<<"xmlns">>, 1, Attrs, {<<"xmlns">>, ?NS_CLIENT}),
    Attrs2 = [{<<"xmlns:stream">>, ?NS_STREAM} | Attrs1],
    NewStart = #xmlstreamstart{ name = <<"stream:stream">>, attrs = Attrs2 },
    {[NewStart], State#ws_state{ open_tag = open }};
process_client_stream_start(Elements, State) ->
    {Elements, State}.

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
replace_stream_ns(Element, #ws_state{ open_tag = open }) ->
    case should_have_jabber_client(Element) of
        true ->
            JabberClient = {<<"xmlns">>, <<"jabber:client">>},
            NewAtrrs = lists:keystore(<<"xmlns">>, 1,
                                      Element#xmlel.attrs, JabberClient),
            Element#xmlel{attrs = NewAtrrs};
        false ->
            Element
    end;
replace_stream_ns(Element, _State) ->
    Element.

get_parser_opts(Text, #ws_state{ max_stanza_size = infinity }) ->
    [{max_child_size, 0} | get_parser_opts(Text)];
get_parser_opts(Text, #ws_state{ max_stanza_size = MaxStanzaSize }) ->
    [{max_child_size, MaxStanzaSize} | get_parser_opts(Text)].

get_parser_opts(<<"<open", _/binary>>) ->
    [{infinite_stream, true}, {autoreset, true}]; % new-type WS
get_parser_opts(_) ->
    [{start_tag, <<"stream:stream">>}]. % old-type WS

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
should_have_jabber_client(#xmlel{name = <<"iq">>}) -> true;
should_have_jabber_client(#xmlel{name = <<"message">>}) -> true;
should_have_jabber_client(#xmlel{name = <<"presence">>}) -> true;
should_have_jabber_client(_) -> false.

send_ping_request(PingRate) ->
    Dest = self(),
    ?DEBUG("Sending websocket ping request to ~p", [Dest]),
    erlang:send_after(PingRate, Dest, do_ping).

maybe_send_ping_request(none) ->
    ok;
maybe_send_ping_request(PingRate) ->
    send_ping_request(PingRate).


%%--------------------------------------------------------------------
%% add_sec_websocket_protocol_header
%%--------------------------------------------------------------------

add_sec_websocket_protocol_header(Req) ->
     case cowboy_req:parse_header(<<"sec-websocket-protocol">>, Req) of
         undefined ->
             %% Server should not set "sec-websocket-protocol" response header
             %% if it does not present in the request
             Req;
         Protocols ->
             case case_insensitive_match(<<"xmpp">>, Protocols) of
                 {matched, MatchedProtocol} ->
                     cowboy_req:set_resp_header(<<"Sec-WebSocket-Protocol">>, MatchedProtocol, Req);
                 nomatch ->
                     %% Do not agree with client, do not add a response header
                     ?DEBUG("issue=not_supported_procol, protocol=~p", [Protocols]),
                     Req
             end
     end.

case_insensitive_match(LowerPattern, [Case | Cases]) ->
    LowerCase = stringprep:tolower(Case),
    case LowerCase of
        LowerPattern ->
            {matched, Case};
        _ ->
            case_insensitive_match(LowerPattern, Cases)
    end;
case_insensitive_match(_, []) ->
    nomatch.
