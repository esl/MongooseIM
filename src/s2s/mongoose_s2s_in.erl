-module(mongoose_s2s_in).

-include_lib("exml/include/exml_stream.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

-behaviour(gen_statem).

-record(s2s_data, {
          host_type :: undefined | mongooseim:host_type(),
          myname = ?MYNAME :: jid:lserver(),
          auth_domain :: jid:lserver() | undefined,
          lang = ?MYLANG :: ejabberd:lang(),
          streamid = mongoose_bin:gen_from_crypto() :: binary(),
          socket :: mongoose_xmpp_socket:socket(),
          parser :: exml_stream:parser(),
          shaper :: mongoose_shaper:shaper(),
          listener_opts :: mongoose_listener:options(),
          connections = #{} :: map(),
          authenticated = false :: boolean()
         }).
-type data() :: #s2s_data{}.
-type maybe_ok() :: ok | {error, atom()}.
-type fsm_res() :: gen_statem:event_handler_result(state(), data()).
-type stream_state() :: stream_start | authenticated.
-type state() :: connect
               | {wait_for_stream, stream_state()}
               | wait_for_feature_before_auth
               | wait_for_session_establishment
               | stream_established.

-type connection_info() ::
        #{pid => pid(),
          direction => in,
          state_name => state(),
          addr => inet:ip_address(),
          port => inet:port_number(),
          streamid => ejabberd_s2s:stream_id(),
          tls => boolean(),
          tls_enabled => boolean(),
          tls_options => undefined | just_tls:options(),
          authenticated => boolean(),
          shaper => mongoose_shaper:shaper(),
          domains => [jid:lserver()]}.

-export_type([connection_info/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3]).

-export([start_link/2, exit/2, get_state_info/1, send_validity_from_s2s_out/3]).
-ignore_xref([exit/2]).

-spec start_link(mongoose_listener:init_args(), [gen_statem:start_opt()]) ->
    gen_statem:start_ret().
start_link(Params, ProcOpts) ->
    gen_statem:start_link(?MODULE, Params, ProcOpts).

-spec exit(pid(), binary() | atom()) -> ok.
exit(Pid, Reason) ->
    gen_statem:cast(Pid, {exit, Reason}).

-spec get_state_info(pid()) -> term().
get_state_info(Pid) ->
    gen_statem:call(Pid, get_state_info, 5000).

-spec send_validity_from_s2s_out(pid(), boolean(), ejabberd_s2s:fromto()) -> ok.
send_validity_from_s2s_out(Pid, IsValid, FromTo)
  when is_pid(Pid), is_boolean(IsValid), is_tuple(FromTo) ->
    Event = {validity_from_s2s_out, IsValid, FromTo},
    gen_statem:cast(Pid, Event).

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    handle_event_function.

-spec init(mongoose_listener:init_args()) -> gen_statem:init_result(state(), undefined).
init({Transport, Ref, LOpts}) ->
    ConnectEvent = {next_event, internal, {connect, {Transport, Ref, LOpts}}},
    {ok, connect, undefined, ConnectEvent}.

-spec handle_event(gen_statem:event_type(), term(), state(), data()) -> fsm_res().
handle_event(internal, {connect, {Transport, Ref, LOpts}}, connect, _) when is_atom(Transport) ->
    #{shaper := ShaperName, max_stanza_size := MaxStanzaSize} = LOpts,
    {ok, Parser} = exml_stream:new_parser([{max_element_size, MaxStanzaSize}]),
    Shaper = mongoose_shaper:new(ShaperName),
    Socket = mongoose_xmpp_socket:accept(Transport, s2s, Ref, LOpts),
    ?LOG_DEBUG(#{what => s2s_in_started, text => "New incoming S2S connection", socket => Socket}),
    Data = #s2s_data{socket = Socket, parser = Parser, shaper = Shaper, listener_opts = LOpts},
    {next_state, {wait_for_stream, stream_start}, Data, state_timeout(LOpts)};
handle_event(internal, #xmlstreamstart{attrs = Attrs}, {wait_for_stream, StreamState}, Data) ->
    handle_stream_start(Data, Attrs, StreamState);
handle_event(internal, Unexpected, {wait_for_stream, _}, Data) ->
    handle_maybe_hide_service_name(Data, Unexpected);
handle_event(internal, #xmlstreamstart{} = Unexpected, _, Data) ->
    Info = #{location => ?LOCATION, last_event => Unexpected},
    stream_start_error(Data, Info, mongoose_xmpp_errors:policy_violation());
handle_event(internal, #xmlstreamend{}, _, Data) ->
    send_xml(Data, ?XML_STREAM_TRAILER),
    {stop, {shutdown, stream_end}};
handle_event(internal, #xmlstreamerror{name = <<"element too big">> = Err}, _, Data) ->
    stream_error(Data, mongoose_xmpp_errors:policy_violation(Data#s2s_data.lang, Err));
handle_event(internal, #xmlstreamerror{name = Err}, _, Data) ->
    stream_error(Data, mongoose_xmpp_errors:xml_not_well_formed(Data#s2s_data.lang, Err));
handle_event(internal, #xmlel{name = <<"starttls">>} = El, wait_for_feature_before_auth, Data) ->
    handle_starttls(Data, El);
handle_event(internal, #xmlel{name = <<"auth">>} = El, wait_for_feature_before_auth, Data) ->
    handle_auth_start(Data, El);
handle_event(internal, #xmlel{name = <<"db:", _/binary>>} = El, State, Data) ->
    handle_dialback(Data, El, State);
handle_event(internal, #xmlel{} = El, stream_established, Data) ->
    handle_session_established(Data, El);
handle_event(info, {Tag, _, _} = SocketData, _, Data) when Tag =:= tcp; Tag =:= ssl ->
    handle_socket_data(Data, SocketData);
handle_event(info, {Tag, _}, State, _) when Tag =:= tcp_closed; Tag =:= ssl_closed ->
    ?LOG_WARNING(#{what => s2s_connection_closed, tag => Tag, state => State}),
    {stop, {shutdown, Tag}};
handle_event(info, {Tag, _, Reason}, State, _) when Tag =:= tcp_error; Tag =:= ssl_error ->
    ?LOG_WARNING(#{what => s2s_connection_error, tag => Tag, reason => Reason, state => State}),
    {stop, {shutdown, {Tag, Reason}}};
handle_event(cast, {validity_from_s2s_out, IsValid, {From, To}}, _, Data)
  when is_boolean(IsValid), is_binary(From), is_binary(To) ->
    handle_validity_from_s2s_out(Data, IsValid, {From, To});
handle_event(cast, {exit, system_shutdown}, _, Data) ->
    Error = mongoose_xmpp_errors:system_shutdown(),
    send_xml(Data, Error),
    send_xml(Data, ?XML_STREAM_TRAILER),
    {stop, {shutdown, system_shutdown}};
handle_event(cast, {exit, Reason}, _, Data) when is_binary(Reason) ->
    StreamConflict = mongoose_xmpp_errors:stream_conflict(Data#s2s_data.lang, Reason),
    send_xml(Data, StreamConflict),
    send_xml(Data, ?XML_STREAM_TRAILER),
    {stop, {shutdown, Reason}};
handle_event(cast, {stop, Reason}, _, _) ->
    {stop, {shutdown, Reason}};
handle_event({call, From}, get_state_info, State, Data) ->
    Info = handle_get_state_info(Data, State),
    {keep_state_and_data, [{reply, From, Info}]};
handle_event({timeout, activate_socket}, activate_socket, _, #s2s_data{socket = Socket}) ->
    mongoose_xmpp_socket:activate(Socket),
    keep_state_and_data;
handle_event(state_timeout, state_timeout_termination, State, Data) ->
    ?LOG_WARNING(#{what => s2s_state_timeout, state => State}),
    send_xml(Data, mongoose_xmpp_errors:connection_timeout()),
    send_xml(Data, ?XML_STREAM_TRAILER),
    {stop, {shutdown, state_timeout}};
handle_event(EventType, EventContent, State, Data) ->
    ?LOG_WARNING(#{what => unknown_statem_event,
                   s2s_state => State, s2s_data => Data,
                   event_type => EventType, event_content => EventContent}),
    keep_state_and_data.

-spec terminate(term(), state(), data()) -> term().
terminate(Reason, State, #s2s_data{parser = Parser, socket = Socket} = Data) ->
    ?LOG_DEBUG(#{what => s2s_statem_terminate, reason => Reason, s2s_state => State, s2s_data => Data}),
    exml_stream:free_parser(Parser),
    mongoose_xmpp_socket:close(Socket).

-spec handle_stream_start(data(), exml:attrs(), stream_state()) -> fsm_res().
handle_stream_start(D0, #{<<"xmlns">> := ?NS_SERVER, <<"to">> := Server} = Attrs, stream_start) ->
    LServer = jid:nameprep(Server),
    case is_binary(LServer) andalso mongoose_domain_api:get_host_type(LServer) of
        {ok, HostType} ->
            D1 = D0#s2s_data{myname = LServer, host_type = HostType},
            stream_start_features_before_auth(D1, Attrs);
        _ ->
            Info = #{location => ?LOCATION, last_event => {stream_start, Attrs}},
            stream_start_error(D0, Info, mongoose_xmpp_errors:host_unknown())
    end;
handle_stream_start(#s2s_data{myname = LServer} = D0,
                    #{<<"xmlns">> := ?NS_SERVER, <<"to">> := Server} = Attrs, authenticated) ->
    case jid:nameprep(Server) of
        LServer ->
            stream_start_after_auth(D0, Attrs);
        _Other ->
            Msg = <<"The 'to' attribute differs from the originally provided one">>,
            Info = #{location => ?LOCATION, last_event => {stream_start, Attrs},
                     expected_server => LServer, provided_server => Server},
            stream_start_error(D0, Info, mongoose_xmpp_errors:host_unknown(?MYLANG, Msg))
    end;
handle_stream_start(D0, #{<<"xmlns">> := ?NS_SERVER} = Attrs, _State) ->
    Msg = <<"The 'to' attribute is missing">>,
    Info = #{location => ?LOCATION, last_event => {stream_start, Attrs}},
    stream_start_error(D0, Info, mongoose_xmpp_errors:improper_addressing(?MYLANG, Msg));
handle_stream_start(D0, Attrs, _State) ->
    Info = #{location => ?LOCATION, last_event => {stream_start, Attrs}},
    stream_start_error(D0, Info, mongoose_xmpp_errors:invalid_namespace()).

-spec handle_maybe_hide_service_name(data(), term()) -> fsm_res().
handle_maybe_hide_service_name(Data, Unexpected) ->
    case mongoose_config:get_opt(hide_service_name, false) of
        true ->
            {stop, {shutdown, stream_error}};
        false ->
            Info = #{location => ?LOCATION, last_event => Unexpected},
            stream_start_error(Data, Info, mongoose_xmpp_errors:xml_not_well_formed())
    end.

-spec handle_starttls(data(), exml:element()) -> fsm_res().
handle_starttls(#s2s_data{socket = TcpSocket,
                          parser = Parser,
                          listener_opts = LOpts = #{tls := _}} = Data,
                #xmlel{attrs = #{<<"xmlns">> := ?NS_TLS}} = El) ->
    send_xml(Data, tls_proceed()), %% send last negotiation chunk via tcp
    case mongoose_xmpp_socket:tcp_to_tls(TcpSocket, LOpts, server) of
        {ok, TlsSocket} ->
            {ok, NewParser} = exml_stream:reset_parser(Parser),
            NewData = Data#s2s_data{socket = TlsSocket,
                                    parser = NewParser,
                                    streamid = mongoose_bin:gen_from_crypto()},
            {next_state, {wait_for_stream, stream_start}, NewData, state_timeout(LOpts)};
        {error, already_tls_connection} ->
            ErrorStanza = mongoose_xmpp_errors:bad_request(Data#s2s_data.lang, <<"bad_config">>),
            Err = jlib:make_error_reply(El, ErrorStanza),
            send_xml(Data, Err),
            {stop, {shutdown, starttls_error}};
        {error, closed} ->
            {stop, {shutdown, tls_closed}};
        {error, timeout} ->
            {stop, {shutdown, tls_timeout}};
        {error, {tls_alert, TlsAlert}} ->
            {stop, TlsAlert}
    end;
handle_starttls(#s2s_data{listener_opts = #{tls := _}} = Data,
                #xmlel{attrs = #{<<"xmlns">> := _}}) ->
    stream_error(Data, mongoose_xmpp_errors:invalid_namespace());
handle_starttls(Data, _El) ->
    %% As defined in https://datatracker.ietf.org/doc/html/rfc6120#section-5.4.2.2, cause 2
    send_xml(Data, tls_failure()),
    {stop, {shutdown, tls_failure}}.

-spec handle_auth_start(data(), exml:element()) -> fsm_res().
handle_auth_start(#s2s_data{socket = Socket} = Data,
                  #xmlel{attrs = #{<<"xmlns">> := ?NS_SASL,
                                   <<"mechanism">> := <<"EXTERNAL">>}} = El) ->
    ClientIn = base64:mime_decode(exml_query:cdata(El)),
    AuthDomain = jid:nameprep(ClientIn),
    CertData = mongoose_xmpp_socket:get_peer_certificate(Socket),
    case is_binary(AuthDomain) andalso check_auth_domain(AuthDomain, CertData) of
        true ->
            NewData = Data#s2s_data{streamid = mongoose_bin:gen_from_crypto(),
                                    authenticated = true,
                                    auth_domain = AuthDomain},
            send_xml(NewData, sasl_success()),
            ?LOG_DEBUG(#{what => s2s_auth_success,
                         text => <<"Accepted s2s authentication">>,
                         socket => Data#s2s_data.socket, auth_domain => AuthDomain}),
            {next_state, {wait_for_stream, authenticated}, NewData, state_timeout(Data)};
        false ->
            send_xml(Data, sasl_failure()),
            send_xml(Data, ?XML_STREAM_TRAILER),
            ?LOG_WARNING(#{what => s2s_in_auth_failed, domain => AuthDomain}),
            mongoose_instrument:execute(s2s_auth_failed, #{},
                                        #{local_domain => Data#s2s_data.myname,
                                          remote_domain => AuthDomain,
                                          direction => in, count => 1}),
            {stop, normal, Data}
    end;
handle_auth_start(#s2s_data{} = Data,
                  #xmlel{attrs = #{<<"xmlns">> := ?NS_SASL, <<"mechanism">> := _}}) ->
    stream_error(Data, invalid_mechanism());
handle_auth_start(#s2s_data{} = Data, #xmlel{attrs = #{<<"xmlns">> := _}}) ->
    stream_error(Data, mongoose_xmpp_errors:invalid_namespace()).

-spec handle_dialback(data(), exml:element(), state()) -> fsm_res().
handle_dialback(#s2s_data{} = Data, #xmlel{} = El, _) ->
    case mongoose_s2s_dialback:parse_key(El) of
        %% Incoming dialback key, we have to verify it using mongoose_s2s_out before
        %% accepting any incoming stanzas
        %% (we have to receive the `validity_from_s2s_out' event first).
        {step_1, FromTo, StreamID, Key} = Parsed ->
            ?LOG_DEBUG(#{what => s2s_in_get_key, from_to => FromTo, stream_id => StreamID, key => Key}),
            %% Checks if the from domain is allowed and if the to
            %% domain is handled by this server:
            case {mongoose_s2s_lib:allow_host(FromTo), is_local_host_known(FromTo)} of
                {true, true} ->
                    mongoose_s2s_out:terminate_if_waiting_delay(FromTo),
                    StartType = {verify, self(), Key, Data#s2s_data.streamid},
                    %% Could we reuse an existing mongoose_s2s_out connection
                    %% instead of making a new one?
                    mongoose_s2s_out:start_connection(FromTo, StartType),
                    Conns = maps:put(FromTo, wait_for_verification, Data#s2s_data.connections),
                    NewData = Data#s2s_data{connections = Conns},
                    {next_state, stream_established, NewData};
                {_, false} ->
                    send_xml(Data, mongoose_xmpp_errors:host_unknown()),
                    ?LOG_WARNING(#{what => s2s_in_key_from_uknown_host, element => El,
                                   parsed => Parsed, from_to => FromTo}),
                    {stop, normal, Data};
                {false, _} ->
                    send_xml(Data, mongoose_xmpp_errors:invalid_from()),
                    ?LOG_WARNING(#{what => s2s_in_key_with_invalid_from, element => El}),
                    {stop, normal, Data}
            end;
        %% Incoming dialback verification request
        %% We have to check it using secrets and reply if it is valid or not
        {step_2, FromTo, StreamID, Key} ->
            ?LOG_DEBUG(#{what => s2s_in_verify_key,
                         from_to => FromTo, stream_id => StreamID, key => Key}),
            IsValid = Key =:= ejabberd_s2s:key(Data#s2s_data.host_type, FromTo, StreamID),
            send_xml(Data, mongoose_s2s_dialback:step_3(FromTo, StreamID, IsValid)),
            {next_state, stream_established, Data}
    end.

-spec handle_session_established(data(), exml:element()) -> fsm_res().
handle_session_established(Data, El) ->
    NewEl = jlib:remove_attr(<<"xmlns">>, El),
    RemoteJid = jid:from_binary(exml_query:attr(El, <<"from">>, <<>>)),
    LocalJid = jid:from_binary(exml_query:attr(El, <<"to">>, <<>>)),
    case {RemoteJid, LocalJid, is_valid_stanza(NewEl)} of
        {#jid{}, #jid{}, true} ->
            route_incoming_stanza(Data, NewEl, RemoteJid, LocalJid);
        _ ->
            ?LOG_WARNING(#{what => s2s_in_route_failed, reason => invalid_stanza, element => El}),
            keep_state_and_data
    end.

-spec route_incoming_stanza(data(), exml:element(), jid:jid(), jid:jid()) -> fsm_res().
route_incoming_stanza(Data, El, RemoteJid, LocalJid) ->
    LRemoteServer = RemoteJid#jid.lserver,
    LLocalServer = LocalJid#jid.lserver,
    FromTo = {LLocalServer, LRemoteServer},
    Acc = mongoose_acc:new(#{location => ?LOCATION,
                             lserver => LLocalServer,
                             element => El,
                             from_jid => RemoteJid,
                             to_jid => LocalJid}),
    case is_s2s_authenticated_or_connected(Data, FromTo) of
        true ->
            route_stanza(Acc),
            keep_state_and_data;
        false ->
            ?LOG_WARNING(#{what => s2s_in_route_failed, reason => not_allowed, element => El}),
            keep_state_and_data
    end.

-spec route_stanza(mongoose_acc:t()) -> any().
route_stanza(Acc) ->
    From = mongoose_acc:from_jid(Acc),
    To = mongoose_acc:to_jid(Acc),
    Acc1 = mongoose_hooks:s2s_receive_packet(Acc),
    ejabberd_router:route(From, To, Acc1).

-spec is_s2s_authenticated_or_connected(data(), ejabberd_s2s:fromto()) -> boolean().
is_s2s_authenticated_or_connected(Data, FromTo) ->
    is_s2s_authenticated(Data, FromTo) orelse is_s2s_connected(Data, FromTo).

-spec is_s2s_authenticated(data(), ejabberd_s2s:fromto()) -> boolean().
is_s2s_authenticated(#s2s_data{authenticated = false}, _) ->
    false;
is_s2s_authenticated(Data, FromTo) ->
    same_auth_domain(Data, FromTo) andalso is_local_host_known(FromTo).

-spec same_auth_domain(data(), ejabberd_s2s:fromto()) -> boolean().
same_auth_domain(#s2s_data{auth_domain = AuthDomain}, {_, LRemoteServer}) ->
    LRemoteServer =:= AuthDomain.

-spec is_s2s_connected(data(), ejabberd_s2s:fromto()) -> boolean().
is_s2s_connected(Data, FromTo) ->
    established =:= maps:get(FromTo, Data#s2s_data.connections, false).

-spec is_local_host_known(ejabberd_s2s:fromto()) -> boolean().
is_local_host_known({LLocalServer, _}) ->
    mongoose_router:is_registered_route(LLocalServer)
        orelse mongoose_component:has_component(LLocalServer)
        orelse is_known_domain(LLocalServer).

-spec is_known_domain(jid:lserver()) -> boolean().
is_known_domain(Domain) ->
    case mongoose_domain_api:get_host_type(Domain) of
        {ok, _HostType} ->
            true;
        _ ->
            false
    end.

-spec is_valid_stanza(exml:element()) -> boolean().
is_valid_stanza(#xmlel{name = <<"message">>}) -> true;
is_valid_stanza(#xmlel{name = <<"presence">>}) -> true;
is_valid_stanza(#xmlel{name = <<"iq">>}) -> true;
is_valid_stanza(#xmlel{name = _}) -> false.

-spec handle_socket_data(data(), {tcp | ssl, _, binary()}) -> fsm_res().
handle_socket_data(#s2s_data{socket = Socket} = Data, Payload) ->
    case mongoose_xmpp_socket:handle_data(Socket, Payload) of
        {error, _Reason} ->
            {stop, {shutdown, socket_error}, Data};
        Packet when is_binary(Packet) ->
            handle_socket_packet(Data, Packet)
    end.

-spec handle_socket_packet(data(), binary()) -> fsm_res().
handle_socket_packet(#s2s_data{parser = Parser} = Data, Packet) ->
    ?LOG_DEBUG(#{what => received_xml_on_stream, packet => Packet, s2s_pid => self()}),
    case exml_stream:parse(Parser, Packet) of
        {error, Reason} ->
            NextEvent = {next_event, internal, #xmlstreamerror{name = iolist_to_binary(Reason)}},
            {keep_state, Data, NextEvent};
        {ok, NewParser, XmlElements} ->
            Size = iolist_size(Packet),
            NewData = Data#s2s_data{parser = NewParser},
            handle_socket_elements(NewData, XmlElements, Size)
    end.

-spec handle_socket_elements(data(), [exml_stream:element()], non_neg_integer()) -> fsm_res().
handle_socket_elements(#s2s_data{shaper = Shaper} = Data, Elements, Size) ->
    [execute_element_event(Element, Data, xmpp_element_in) || Element = #xmlel{} <- Elements],
    {NewShaper, Pause} = mongoose_shaper:update(Shaper, Size),
    NewData = Data#s2s_data{shaper = NewShaper},
    StreamEvents0 = [ {next_event, internal, XmlEl} || XmlEl <- Elements ],
    StreamEvents1 = maybe_add_pause(NewData, StreamEvents0, Pause),
    {keep_state, NewData, StreamEvents1}.

-spec maybe_add_pause(data(), [gen_statem:action()], integer()) -> [gen_statem:action()].
maybe_add_pause(_, StreamEvents, Pause) when Pause > 0 ->
    [{{timeout, activate_socket}, Pause, activate_socket} | StreamEvents];
maybe_add_pause(#s2s_data{socket = Socket}, StreamEvents, _) ->
    mongoose_xmpp_socket:activate(Socket),
    StreamEvents.

-spec handle_validity_from_s2s_out(data(), boolean(), ejabberd_s2s:fromto()) ->
    {next_state, stream_established, data()}.
handle_validity_from_s2s_out(Data, IsValid, FromTo) ->
    send_xml(Data, mongoose_s2s_dialback:step_4(FromTo, IsValid)),
    {next_state, stream_established, update_connections(Data, IsValid, FromTo)}.

-spec handle_get_state_info(data(), state()) -> connection_info().
handle_get_state_info(#s2s_data{socket = Socket, listener_opts = LOpts} = Data, State) ->
    {Addr, Port} = mongoose_xmpp_socket:get_ip(Socket),
    Domains = case Data#s2s_data.authenticated of
                  true ->
                      [Data#s2s_data.auth_domain];
                  false ->
                      Connections = Data#s2s_data.connections,
                      [LRemoteServer || {{_, LRemoteServer}, established} <-
                          maps:to_list(Connections)]
              end,
    #{pid => self(),
      direction => in,
      state_name => State,
      addr => Addr,
      port => Port,
      streamid => Data#s2s_data.streamid,
      tls => maps:is_key(tls, LOpts),
      tls_enabled => mongoose_xmpp_socket:is_ssl(Socket),
      tls_options => maps:get(tls, LOpts, undefined),
      authenticated => Data#s2s_data.authenticated,
      shaper => Data#s2s_data.shaper,
      domains => Domains}.

-spec update_connections(data(), boolean(), ejabberd_s2s:fromto()) -> data().
update_connections(#s2s_data{connections = Cons} = Data, true, FromTo) ->
    Data#s2s_data{connections = maps:put(FromTo, established, Cons)};
update_connections(#s2s_data{connections = Cons} = Data, false, FromTo) ->
    Data#s2s_data{connections = maps:remove(FromTo, Cons)}.

-spec state_timeout(data() | mongoose_listener:options()) ->
    {state_timeout, timeout(), state_timeout_termination}.
state_timeout(#s2s_data{listener_opts = LOpts}) ->
    state_timeout(LOpts);
state_timeout(#{state_timeout := Timeout}) ->
    {state_timeout, Timeout, state_timeout_termination}.

-spec stream_start_features_before_auth(data(), exml:attrs()) -> fsm_res().
stream_start_features_before_auth(
  #s2s_data{host_type = HostType, myname = LServer, socket = Socket} = Data,
  #{<<"version">> := ?XMPP_VERSION}) ->
    IsSSL = mongoose_xmpp_socket:is_ssl(Socket),
    StreamFeatures0 = mongoose_hooks:s2s_stream_features(HostType, LServer),
    StreamFeatures = add_tls_elems(Data, IsSSL, StreamFeatures0),
    Features = #xmlel{name = <<"stream:features">>,
                      attrs = #{<<"from">> => LServer},
                      children = StreamFeatures},
    send_xml(Data, stream_header(Data)),
    send_xml(Data, Features),
    {next_state, wait_for_feature_before_auth, Data, state_timeout(Data)};
stream_start_features_before_auth(#s2s_data{} = Data, #{<<"xmlns:db">> := ?NS_SERVER_DIALBACK}) ->
    send_xml(Data, stream_header(Data)),
    {next_state, wait_for_feature_before_auth, Data, state_timeout(Data)};
stream_start_features_before_auth(Data, Attrs) ->
    Info = #{location => ?LOCATION, last_event => {stream_start, Attrs}},
    stream_start_error(Data, Info, mongoose_xmpp_errors:invalid_xml()).

-spec stream_start_after_auth(data(), exml:attrs()) -> fsm_res().
stream_start_after_auth(#s2s_data{host_type = HostType, myname = LServer} = Data,
                        #{<<"version">> := ?XMPP_VERSION}) ->
    StreamFeatures = mongoose_hooks:s2s_stream_features(HostType, LServer),
    Features = #xmlel{name = <<"stream:features">>,
                      attrs = #{<<"from">> => LServer},
                      children = StreamFeatures},
    send_xml(Data, stream_header(Data)),
    send_xml(Data, Features),
    {next_state, stream_established, Data};
stream_start_after_auth(Data, #{<<"xmlns:db">> := ?NS_SERVER_DIALBACK}) ->
    send_xml(Data, stream_header(Data)),
    {next_state, stream_established, Data};
stream_start_after_auth(Data, Attrs) ->
    Info = #{location => ?LOCATION, last_event => {stream_start, Attrs}},
    stream_start_error(Data, Info, mongoose_xmpp_errors:invalid_xml()).

-spec stream_start_error(data(), map(), exml:element()) -> fsm_res().
stream_start_error(Data, Info, Error) ->
    send_xml(Data, stream_header(Data)),
    send_xml(Data, Error),
    send_xml(Data, ?XML_STREAM_TRAILER),
    ?LOG_WARNING(Info#{what => s2s_in_stream_start_error, element => Error}),
    {stop, normal, Data}.

-spec stream_error(data(), exml:element()) -> fsm_res().
stream_error(Data, Error) ->
    ?LOG_WARNING(#{what => s2s_in_stream_start_error, element => Error}),
    send_xml(Data, Error),
    send_xml(Data, ?XML_STREAM_TRAILER),
    {stop, {shutdown, stream_error}, Data}.

-spec send_xml(data(), exml_stream:element()) -> maybe_ok().
send_xml(#s2s_data{socket = Socket} = Data, Elem) ->
    case Elem of
        #xmlel{} -> execute_element_event(Elem, Data, xmpp_element_out);
        _ -> ok
    end,
    mongoose_xmpp_socket:send_xml(Socket, Elem).

check_auth_domain(error, _) ->
    false;
check_auth_domain(AuthDomain, {ok, Cert}) ->
    case mongoose_s2s_lib:domain_utf8_to_ascii(AuthDomain, binary) of
        false ->
            false;
        PCAuthDomain ->
            lists:any(
              fun(D) -> match_domain(PCAuthDomain, D) end,
              cert_utils:get_cert_domains(Cert))
    end;
check_auth_domain(_, _) ->
    false.

-spec match_domain(binary(), binary()) -> boolean().
match_domain(Domain, Domain) ->
    true;
match_domain(Domain, Pattern) ->
    DLabels = binary:split(Domain, <<".">>, [global]),
    PLabels = binary:split(Pattern, <<".">>, [global]),
    match_labels(DLabels, PLabels).

-spec match_labels([binary()], [binary()]) -> boolean().
match_labels([], []) ->
    true;
match_labels([], [_ | _]) ->
    false;
match_labels([_ | _], []) ->
    false;
match_labels([DL | DLabels], [PL | PLabels]) ->
    PLlist = binary_to_list(PL),
    case lists:all(fun(C) -> (($a =< C) andalso (C =< $z))
                                 orelse (($0 =< C) andalso (C =< $9))
                                 orelse (C == $-) orelse (C == $*)
                   end, PLlist) of
        true ->
            Regexp = xmerl_regexp:sh_to_awk(PLlist),
            case re:run(binary_to_list(DL), Regexp, [{capture, none}]) of
                match ->
                    match_labels(DLabels, PLabels);
                nomatch ->
                    false
            end;
        false ->
            false
    end.

-spec stream_header(data()) -> exml_stream:start().
stream_header(#s2s_data{myname = LServer, streamid = Id, lang = Lang}) ->
    Attrs = #{<<"xmlns:stream">> => ?NS_STREAM,
              <<"xmlns">> => ?NS_SERVER,
              <<"xmlns:db">> => ?NS_SERVER_DIALBACK,
              <<"version">> => ?XMPP_VERSION,
              <<"xml:lang">> => Lang,
              <<"id">> => Id,
              <<"from">> => LServer
             },
    #xmlstreamstart{name = <<"stream:stream">>, attrs = Attrs}.

-spec add_tls_elems(data(), boolean(), [exml:element()]) -> [exml:element()].
add_tls_elems(#s2s_data{listener_opts = #{tls := #{mode := starttls_required}}}, false, F) ->
    [#xmlel{name = <<"starttls">>,
           attrs = #{<<"xmlns">> => ?NS_TLS},
           children = [#xmlel{name = <<"required">>}]} | F];
add_tls_elems(#s2s_data{listener_opts = #{tls := #{mode := starttls}}}, false, F) ->
    [#xmlel{name = <<"starttls">>,
            attrs = #{<<"xmlns">> => ?NS_TLS}} | F];
add_tls_elems(#s2s_data{socket = Socket, listener_opts = #{tls := _}}, true, F) ->
    case mongoose_xmpp_socket:get_peer_certificate(Socket) of
        {ok, _} ->
            [#xmlel{name = <<"mechanisms">>,
                    attrs = #{<<"xmlns">> => ?NS_SASL},
                    children = [#xmlel{name = <<"mechanism">>,
                                       children = [#xmlcdata{content = <<"EXTERNAL">>}]}]} | F];
        no_peer_cert -> F
    end;
add_tls_elems(#s2s_data{listener_opts = #{}}, _, F) ->
    F.

-spec tls_proceed() -> exml:element().
tls_proceed() ->
    #xmlel{name = <<"proceed">>,
           attrs = #{<<"xmlns">> => ?NS_TLS}}.

-spec tls_failure() -> exml:element().
tls_failure() ->
    #xmlel{name = <<"failure">>,
           attrs = #{<<"xmlns">> => ?NS_TLS}}.

-spec sasl_success() -> exml:element().
sasl_success() ->
    #xmlel{name = <<"success">>,
           attrs = #{<<"xmlns">> => ?NS_SASL}}.

-spec sasl_failure() -> exml:element().
sasl_failure() ->
    #xmlel{name = <<"failure">>,
           attrs = #{<<"xmlns">> => ?NS_SASL}}.

-spec invalid_mechanism() -> exml:element().
invalid_mechanism() ->
    #xmlel{name = <<"failure">>,
           attrs = #{<<"xmlns">> => ?NS_SASL},
           children = [#xmlel{name = <<"invalid-mechanism">>}]}.

%% Instrumentation helpers

-spec execute_element_event(exml:element(), data(), mongoose_instrument:event_name()) -> ok.
execute_element_event(Element, #s2s_data{host_type = HostType, myname = LServer}, EventName) ->
    Metadata = #{lserver => LServer, pid => self(), module => ?MODULE},
    mongoose_instrument_xmpp:execute_element_event(EventName, s2s, HostType, Element, Metadata).
