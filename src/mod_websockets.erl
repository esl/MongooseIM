%%%===================================================================
%%% @copyright (C) 2016, Erlang Solutions Ltd.
%%% @doc Module providing support for websockets in MongooseIM
%%% @end
%%%===================================================================
-module(mod_websockets).

-behaviour(mongoose_http_handler).
-behaviour(cowboy_websocket).
-behaviour(mongoose_xmpp_socket).

%% mongoose_http_handler callbacks
-export([config_spec/0,
         instrumentation/0]).

%% cowboy_http_websocket_handler callbacks
-export([init/2,
         websocket_init/1,
         websocket_handle/2,
         websocket_info/2,
         terminate/3]).

%% mongoose_xmpp_socket callbacks
-export([peername/1,
         tcp_to_tls/3,
         handle_data/2,
         activate/1,
         close/1,
         send_xml/2,
         get_peer_certificate/1,
         has_peer_cert/2,
         is_channel_binding_supported/1,
         export_key_materials/5,
         is_ssl/1]).

-ignore_xref([instrumentation/0]).

-include("mongoose.hrl").
-include("mongoose_config_spec.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml_stream.hrl").

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
          parser :: exml_stream:parser() | undefined,
          opts :: mongoose_listener:options(),
          ping_rate :: integer() | none,
          max_stanza_size :: integer() | infinity,
          peercert :: undefined | passed | binary()
          %% the passed value is used to clear the certificate from the handlers state after it's passed down to the socket()
         }).

-type socket() :: #websocket{}.

%% mongoose_http_handler callbacks

-spec config_spec() -> mongoose_config_spec:config_section().
config_spec() ->
    #section{items = #{<<"timeout">> => #option{type = int_or_infinity,
                                                validate = non_negative},
                       <<"ping_rate">> => #option{type = integer,
                                                  validate = positive},
                       <<"max_stanza_size">> => #option{type = int_or_infinity,
                                                        validate = positive},
                       <<"state_timeout">> => #option{type = int_or_infinity,
                                                          validate = non_negative},
                       <<"backwards_compatible_session">> => #option{type = boolean}},
             defaults = #{<<"timeout">> => 60000,
                          <<"max_stanza_size">> => infinity,
                          <<"state_timeout">> => 5000,
                          <<"backwards_compatible_session">> => true}
            }.

%% mongoose_http_handler instrumentation
-spec instrumentation() -> [mongoose_instrument:spec()].
instrumentation() ->
    [{mod_websocket_data_sent, #{},
      #{metrics => #{byte_size => spiral}}},
     {mod_websocket_data_received, #{},
      #{metrics => #{byte_size => spiral}}}].

%%--------------------------------------------------------------------
%% Common callbacks for all cowboy behaviours
%%--------------------------------------------------------------------

init(Req, Opts = #{timeout := Timeout}) ->
    Peer = cowboy_req:peer(Req),
    PeerCert = cowboy_req:cert(Req),
    Req1 = add_sec_websocket_protocol_header(Req),
    ?LOG_DEBUG(#{what => ws_init, text => <<"New websockets request">>,
                 req => Req, opts => Opts}),
    AllModOpts = Opts#{peer => Peer, peer_cert => PeerCert},
    %% upgrade protocol
    {cowboy_websocket, Req1, AllModOpts, #{idle_timeout => Timeout}}.

terminate(_Reason, _Req, #ws_state{fsm_pid = undefined}) ->
    ok;
terminate(Reason, _Req, #ws_state{fsm_pid = FSM}) when Reason =:= normal;
                                                       Reason =:= stop;
                                                       Reason =:= timeout;
                                                       Reason =:= remote ->
    FSM ! {websockets_closed, undefined},
    ok;
terminate({remote, _, _}, _Req, #ws_state{fsm_pid = FSM}) ->
    FSM ! {websockets_closed, undefined},
    ok;
terminate(_Reason, _Req, #ws_state{fsm_pid = FSM}) ->
    FSM ! {websockets_error, undefined},
    ok.

%%--------------------------------------------------------------------
%% cowboy_http_websocket_handler callbacks
%%--------------------------------------------------------------------

% Called for every new websocket connection.
websocket_init(Opts = #{peer := Peer, peer_cert := PeerCert, max_stanza_size := MaxStanzaSize}) ->
    PingRate = maps:get(ping_rate, Opts, none),
    maybe_send_ping_request(PingRate),
    ?LOG_DEBUG(#{what => ws_init, text => <<"New websockets connection">>,
                 peer => Peer, opts => Opts}),
    State = #ws_state{opts = Opts,
                      ping_rate = PingRate,
                      max_stanza_size = MaxStanzaSize,
                      peer = Peer,
                      peercert = PeerCert},
    {ok, State}.

% Called when a text message arrives.
websocket_handle({text, Msg}, State) ->
    ?LOG_DEBUG(#{what => ws_received, msg => Msg, peer => State#ws_state.peer}),
    handle_text(Msg, State);

websocket_handle({pong, Payload}, State) ->
    ?LOG_DEBUG(#{what => ws_pong, text => <<"Received pong frame over WebSockets">>,
                 msg => Payload, peer => State#ws_state.peer}),
    {ok, State};

% With this callback we can handle other kind of
% messages, like binary.
websocket_handle(Any, State) ->
    ?LOG_DEBUG(#{what => ws_received, text => <<"Received non-text over WebSockets">>,
                 msg => Any, peer => State#ws_state.peer}),
    {ok, State}.

% Other messages from the system are handled here.
websocket_info({send_xml, XML}, State) ->
    XML1 = process_server_stream_root(replace_stream_ns(XML)),
    Text = exml:to_iolist(XML1),
    mongoose_instrument:execute(mod_websocket_data_sent, #{}, #{byte_size => iolist_size(Text)}),
    ?LOG_DEBUG(#{what => ws_send, text => <<"Sending xml over WebSockets">>,
                 packet => Text, peer => State#ws_state.peer}),
    {reply, {text, Text}, State};
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
    ?LOG_DEBUG(#{what => unexpected_message, msg => Info}),
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
        mongoose_instrument:execute(mod_websocket_data_received, #{}, #{byte_size => byte_size(Text)}),
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
    Elements1 = process_client_stream_start(Elements),
    [send_to_fsm(FSM, process_client_stream_end(replace_stream_ns(Elem))) || Elem <- Elements1],
    {ok, State}.

process_parse_error(_Reason, #ws_state{fsm_pid = undefined} = State) ->
    {stop, State};
process_parse_error(Reason, #ws_state{fsm_pid = FSM} = State) ->
    send_to_fsm(FSM, #xmlstreamerror{name = Reason}),
    {ok, State}.

send_to_fsm(FSM, Element) ->
    FSM ! {tcp, undefined, Element},
    ok.

maybe_start_fsm([#xmlel{ name = <<"open">> }],
                #ws_state{fsm_pid = undefined,
                          opts = #{ip_tuple := IPTuple, port := Port,
                                   state_timeout := StateTimeout,
                                   backwards_compatible_session := BackwardsCompatible}} = State) ->
    Opts = #{
        access => all,
        shaper => none,
        max_stanza_size => 0,
        state_timeout => StateTimeout,
        backwards_compatible_session => BackwardsCompatible,
        module => ejabberd_cowboy,
        connection_type => c2s,
        hibernate_after => 0,
        ip_tuple => IPTuple,
        ip_address => inet:ntoa(IPTuple),
        ip_version => mongoose_listener_config:ip_version(IPTuple),
        port => Port, proto => tcp},
    do_start_fsm(Opts, State);
maybe_start_fsm(_Els, #ws_state{fsm_pid = undefined} = State) ->
    {stop, State};
maybe_start_fsm(_Els, State) ->
    {ok, State}.

do_start_fsm(Opts, State = #ws_state{peer = Peer, peercert = PeerCert}) ->
    SocketData = #websocket{pid = self(),
                            peername = Peer,
                            peercert = PeerCert},
    case call_fsm_start(SocketData, Opts) of
        {ok, Pid} ->
            ?LOG_DEBUG(#{what => ws_c2s_started, c2s_pid => Pid,
                         text => <<"WebSockets starts c2s process">>,
                         peer => State#ws_state.peer}),
            NewState = State#ws_state{fsm_pid = Pid, peercert = passed},
            {ok, NewState};
        {error, Reason} ->
            ?LOG_WARNING(#{what => ws_c2s_start_failed, reason => Reason,
                           text => <<"WebSockets fails to start c2s process">>,
                           peer => State#ws_state.peer}),
            {stop, State#ws_state{peercert = passed}}
    end.

call_fsm_start(SocketData, #{hibernate_after := HibernateAfterTimeout} = Opts) ->
    mongoose_c2s:start({?MODULE, SocketData, Opts},
                       [{hibernate_after, HibernateAfterTimeout}]).

%%--------------------------------------------------------------------
%% Helpers for handling
%% https://datatracker.ietf.org/doc/rfc7395/
%%--------------------------------------------------------------------

process_client_stream_start([#xmlel{ name = <<"open">>, attrs = Attrs }]) ->
    Attrs1 = replace_xmlns(Attrs, ?NS_CLIENT),
    Attrs2 = Attrs1#{<<"xmlns:stream">> => ?NS_STREAM},
    NewStart = #xmlstreamstart{ name = <<"stream:stream">>, attrs = Attrs2 },
    [NewStart];
process_client_stream_start(Elements) ->
    Elements.

process_client_stream_end(#xmlel{ name = <<"close">> }) ->
    #xmlstreamend{ name = <<"stream:stream">> };
process_client_stream_end(Element) ->
    Element.

process_server_stream_root(#xmlstreamstart{ name = <<"stream", _/binary>>, attrs = Attrs }) ->
    Attrs1 = maps:remove(<<"xmlns:stream">>, Attrs),
    Attrs2 = replace_xmlns(Attrs1, ?NS_FRAMING),
    #xmlel{ name = <<"open">>, attrs = Attrs2 };
process_server_stream_root(#xmlstreamend{ name = <<"stream", _/binary>> }) ->
    #xmlel{ name = <<"close">>, attrs = #{<<"xmlns">> => ?NS_FRAMING}};
process_server_stream_root(Element) ->
    Element.

replace_xmlns(Attrs, XmlNS) when is_map_key(<<"xmlns">>, Attrs) ->
    Attrs#{<<"xmlns">> => XmlNS};
replace_xmlns(Attrs, _XmlNS) ->
    Attrs.

replace_stream_ns(#xmlel{ name = <<"stream:", ElementName/binary>>, attrs = Attrs} = Element) ->
    Element#xmlel{ name = ElementName, attrs = Attrs#{<<"xmlns">> => ?NS_STREAM}};
replace_stream_ns(Element) ->
    case should_have_jabber_client(Element) of
        true ->
            #xmlel{attrs = Attrs} = Element,
            Element#xmlel{attrs = Attrs#{<<"xmlns">> => <<"jabber:client">>}};
        false ->
            Element
    end.

get_parser_opts(Text, #ws_state{ max_stanza_size = infinity }) ->
    [{max_element_size, 0} | get_parser_opts(Text)];
get_parser_opts(Text, #ws_state{ max_stanza_size = MaxStanzaSize }) ->
    [{max_element_size, MaxStanzaSize} | get_parser_opts(Text)].

get_parser_opts(<<"<open", _/binary>>) ->
    [{infinite_stream, true}]; % new-type WS
get_parser_opts(_) ->
    []. % old-type WS

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------
should_have_jabber_client(#xmlel{name = <<"iq">>}) -> true;
should_have_jabber_client(#xmlel{name = <<"message">>}) -> true;
should_have_jabber_client(#xmlel{name = <<"presence">>}) -> true;
should_have_jabber_client(_) -> false.

send_ping_request(PingRate) ->
    Dest = self(),
    ?LOG_DEBUG(#{what => ws_schedule_ping,
                 text => <<"Sending websocket ping request">>,
                 ping_rate => PingRate}),
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
                     ?LOG_DEBUG(#{what => ws_unknown_protocol,
                                  text => <<"Header sec-websocket-protocol does not contain xmpp option">>,
                                  protocols => Protocols}),
                     Req
             end
     end.

case_insensitive_match(LowerPattern, [Case | Cases]) ->
    LowerCase = jid:str_tolower(Case),
    case LowerCase of
        LowerPattern ->
            {matched, Case};
        _ ->
            case_insensitive_match(LowerPattern, Cases)
    end;
case_insensitive_match(_, []) ->
    nomatch.

%% mongoose_xmpp_socket callbacks

-spec peername(socket()) -> mongoose_transport:peer().
peername(#websocket{peername = PeerName}) ->
    PeerName.

-spec tcp_to_tls(socket(), mongoose_listener:options(), mongoose_xmpp_socket:side()) ->
  {ok, socket()} | {error, term()}.
tcp_to_tls(_Socket, _LOpts, server) ->
    {error, tcp_to_tls_not_supported_for_websockets}.

-spec handle_data(socket(), {tcp | ssl, term(), term()}) ->
  iodata() | {raw, [exml:element()]} | {error, term()}.
handle_data(_Socket, {_Kind, _Term, Packet}) ->
    {raw, [Packet]}.

-spec activate(socket()) -> ok.
activate(_Socket) ->
    ok.

-spec close(socket()) -> ok.
close(#websocket{pid = Pid}) ->
    Pid ! stop,
    ok.

-spec send_xml(socket(), iodata() | exml:element() | [exml:element()]) ->
    ok | {error, term()}.
send_xml(#websocket{pid = Pid}, XMLs) when is_list(XMLs) ->
    [Pid ! {send_xml, XML} || XML <- XMLs],
    ok;
send_xml(#websocket{pid = Pid}, XML) ->
    Pid ! {send_xml, XML},
    ok.

-spec has_peer_cert(socket(), mongoose_listener:options()) -> boolean().
has_peer_cert(Socket, _) ->
    get_peer_certificate(Socket) /= no_peer_cert.

-spec get_peer_certificate(socket()) -> mongoose_xmpp_socket:peercert_return().
get_peer_certificate(#websocket{peercert = undefined}) ->
    no_peer_cert;
get_peer_certificate(#websocket{peercert = PeerCert}) ->
    Decoded = public_key:pkix_decode_cert(PeerCert, plain),
    {ok, Decoded}.

-spec is_channel_binding_supported(socket()) -> boolean().
is_channel_binding_supported(_Socket) ->
    false.

-spec export_key_materials(socket(), Labels, Contexts, WantedLengths, ConsumeSecret) ->
    {error, export_key_materials_not_supported_for_websockets}
      when
      Labels :: [binary()],
      Contexts :: [binary() | no_context],
      WantedLengths :: [non_neg_integer()],
      ConsumeSecret :: boolean().
export_key_materials(_Socket, _, _, _, _) ->
    {error, export_key_materials_not_supported_for_websockets}.

-spec is_ssl(socket()) -> boolean().
is_ssl(_Socket) ->
    false.
