-module(mongoose_c2s).

-behaviour(gen_statem).
-include("mongoose_logger.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml_stream.hrl").
-define(C2S_OPEN_TIMEOUT, 60000).
-define(AUTH_RETRIES, 3).
-define(BIND_RETRIES, 5).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3]).

%% utils
-export([filter_mechanism/1]).

-record(state, {
          host_type = <<>> :: mongooseim:host_type(),
          lserver = <<>> :: jid:lserver(),
          jid :: undefined | jid:jid(),
          sid = ejabberd_sm:make_new_sid() :: ejabberd_sm:sid(),
          streamid = new_stream_id() :: binary(),
          ranch_ref :: ranch:ref(),
          transport :: module(),
          socket :: ranch_transport:socket(),
          parser :: undefined | exml_stream:parser(),
          listener_opts :: mongoose_listener:options()
         }).
-type state() :: #state{}.
-type maybe_ok() :: ok | {error, atom()}.
-type fsm_res() :: gen_statem:event_handler_result(fsm_state(), state()).

-type retries() :: 0..64.
-type stream_state() :: stream_start | authenticated.
-type fsm_state() :: connecting
                   | {wait_for_stream, stream_state()}
                   | {wait_for_feature_before_auth, cyrsasl:sasl_state(), retries()}
                   | {wait_for_feature_after_auth, retries()}
                   | {wait_for_sasl_response, cyrsasl:sasl_state(), retries()}
                   | session_established
                   | resume_session.

-export_type([state/0]).

%%%----------------------------------------------------------------------
%%% gen_statem
%%%----------------------------------------------------------------------
-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    handle_event_function.

-spec init({ranch:ref(), module(), mongoose_listener:options()}) ->
    gen_statem:init_result(fsm_state(), state()).
init({Ref, Transport, Opts}) ->
    StateData = #state{ranch_ref = Ref, transport = Transport, listener_opts = Opts},
    gen_statem:cast(self(), connect),
    {ok, connecting, StateData}.

-spec handle_event(gen_statem:event_type(), term(), fsm_state(), state()) -> fsm_res().
handle_event(cast, connect, connecting,
             StateData = #state{listener_opts = Opts, ranch_ref = Ref, transport = Transport}) ->
    maps:get(proxy_protocol, Opts) andalso ranch:recv_proxy_header(Ref, 1000),
    {ok, Socket} = ranch:handshake(Ref),
    Transport:setopts(Socket, [{active, once}]),
    {ok, NewParser} = exml_stream:new_parser([{max_child_size, 65536}]),
    NewState = StateData#state{lserver = ?MYNAME, socket = Socket, parser = NewParser},
    {next_state, {wait_for_stream, stream_start}, NewState};

%% TODO in any state? probably only during session_established
handle_event(info, {route, From, To, Acc}, _, StateData) ->
    handle_incoming_stanza(StateData, Acc, From, To),
    keep_state_and_data;

handle_event(info, {TcpOrSSl, Socket, Input}, _FsmState,
             StateData = #state{socket = Socket, transport = Transport})
  when TcpOrSSl =:= tcp orelse TcpOrSSl =:= ssl ->
    ?LOG_DEBUG(#{what => received_xml_on_stream, packet => Input, c2s_pid => self()}),
    Transport:setopts(Socket, [{active, once}]),
    {StreamEvents, NewParser} = parse_input(Input, StateData),
    NextEvents = [ {next_event, internal, Event} || Event <- StreamEvents ],
    {keep_state, StateData#state{parser = NewParser}, NextEvents};

handle_event(internal, #xmlstreamstart{name = Name, attrs = Attrs}, {wait_for_stream, StreamState}, StateData) ->
    StreamStart = #xmlel{name = Name, attrs = Attrs},
    handle_stream_start(StateData, StreamStart, StreamState);
handle_event(internal, _Unexpected, {wait_for_stream, _}, StateData = #state{lserver = LServer}) ->
    case mongoose_config:get_opt(hide_service_name, false) of
        true ->
            {stop, {shutdown, stream_error}};
        false ->
            send_header(StateData, LServer, <<"1.0">>, <<>>),
            c2s_stream_error(StateData, mongoose_xmpp_errors:xml_not_well_formed())
    end;
handle_event(internal, #xmlstreamstart{}, _, StateData) ->
    c2s_stream_error(StateData, mongoose_xmpp_errors:policy_violation());

handle_event(internal, #xmlstreamend{}, _, StateData) ->
    send_trailer(StateData),
    {stop, {shutdown, stream_end}};
handle_event(internal, {xmlstreamerror, _}, _, StateData) ->
    c2s_stream_error(StateData, mongoose_xmpp_errors:xml_not_well_formed());
handle_event(internal, #xmlel{name = <<"starttls">>} = El, {wait_for_feature_before_auth, _, _}, StateData) ->
    case exml_query:attr(El, <<"xmlns">>) of
        ?NS_TLS ->
            handle_starttls(StateData);
        _ ->
            c2s_stream_error(StateData, mongoose_xmpp_errors:invalid_namespace())
    end;
handle_event(internal, #xmlel{name = <<"auth">>} = El, {wait_for_feature_before_auth, SaslState, Retries}, StateData) ->
    case exml_query:attr(El, <<"xmlns">>) of
        ?NS_SASL ->
            handle_auth_start(StateData, El, SaslState, Retries);
        _ ->
            c2s_stream_error(StateData, mongoose_xmpp_errors:invalid_namespace())
    end;
handle_event(internal, #xmlel{name = <<"response">>} = El, {wait_for_sasl_response, SaslState, Retries}, StateData) ->
    case exml_query:attr(El, <<"xmlns">>) of
        ?NS_SASL ->
            handle_auth_continue(StateData, El, SaslState, Retries);
        _ ->
            c2s_stream_error(StateData, mongoose_xmpp_errors:invalid_namespace())
    end;
handle_event(internal, #xmlel{name = <<"iq">>} = El, {wait_for_feature_after_auth, Retries}, StateData) ->
    case jlib:iq_query_info(El) of
        #iq{type = set, xmlns = ?NS_BIND} = IQ ->
            handle_bind_resource(StateData, IQ, El, Retries);
        _ ->
            Err = jlib:make_error_reply(El, mongoose_xmpp_errors:bad_request()),
            send_element_from_server_jid(StateData, Err),
            maybe_retry_state(StateData, {wait_for_feature_after_auth, Retries - 1}, Retries)
    end;
handle_event(internal, #xmlel{} = El, session_established, StateData) ->
    case verify_from(El, StateData#state.jid) of
        false ->
            c2s_stream_error(StateData, mongoose_xmpp_errors:invalid_from());
        true ->
            handle_c2s_packet(StateData, El)
    end;

handle_event({timeout, replaced_wait_timeout}, ReplacedPids, FsmState, StateData) ->
    [ case erlang:is_process_alive(Pid) of
          false -> ok;
          true ->
              ?LOG_WARNING(#{what => c2s_replaced_wait_timeout,
                             text => <<"Some processes are not responding when handling replace messages">>,
                             replaced_pid => Pid, state_name => FsmState, c2s_state => StateData})
      end || Pid <- ReplacedPids ],
    keep_state_and_data;

handle_event(info, {Closed, Socket}, _, _StateData = #state{socket = Socket})
  when Closed =:= tcp_closed; Closed =:= ssl_closed ->
    {stop, {shutdown, socket_closed}};
handle_event(info, {Error, Socket}, _, _StateData = #state{socket = Socket})
  when Error =:= tcp_error; Error =:= ssl_error ->
    {stop, {shutdown, socket_error}};

handle_event(info, replaced, _FsmState, StateData) ->
    StreamConflict = mongoose_xmpp_errors:stream_conflict(),
    send_element_from_server_jid(StateData, StreamConflict),
    send_trailer(StateData),
    {stop, {shutdown, replaced}};

handle_event(EventType, EventContent, FsmState, StateData) ->
    ?LOG_DEBUG(#{what => unknown_info_event, stuff => [EventType, EventContent, FsmState, StateData]}),
    keep_state_and_data.

terminate(Reason, FsmState, StateData) ->
    ?LOG_DEBUG(#{what => c2s_statem_terminate, stuff => [Reason, FsmState, StateData]}),
    close_session(StateData, FsmState, Reason),
    close_parser(StateData),
    close_socket(StateData),
    ok.

%%%----------------------------------------------------------------------
%%% helpers
%%%----------------------------------------------------------------------
-spec handle_stream_start(state(), exml:element(), stream_state()) -> fsm_res().
handle_stream_start(S0, StreamStart, StreamState) ->
    Server = jid:nameprep(exml_query:attr(StreamStart, <<"to">>, <<>>)),
    S1 = S0#state{lserver = Server},
    case {StreamState,
          exml_query:attr(StreamStart, <<"xmlns:stream">>, <<>>),
          exml_query:attr(StreamStart, <<"version">>, <<>>),
          mongoose_domain_api:get_domain_host_type(Server)} of
        {stream_start, ?NS_STREAM, <<"1.0">>, {ok, HostType}} ->
            S = S1#state{host_type = HostType},
            stream_start_features_before_auth(S);
        {authenticated, ?NS_STREAM, <<"1.0">>, {ok, HostType}} ->
            S = S1#state{host_type = HostType},
            stream_start_features_after_auth(S);
        {_, ?NS_STREAM, _Pre1_0, {ok, HostType}} ->
            %% (http://xmpp.org/rfcs/rfc6120.html#streams-negotiation-features)
            S = S1#state{host_type = HostType},
            stream_start_error(S, mongoose_xmpp_errors:unsupported_version());
        {_, ?NS_STREAM, _, {error, not_found}} ->
            stream_start_error(S1, mongoose_xmpp_errors:host_unknown());
        {_, _, _, _} ->
            stream_start_error(S1, mongoose_xmpp_errors:invalid_namespace())
    end.

-spec handle_starttls(state()) -> fsm_res().
handle_starttls(StateData = #state{transport = Transport, socket = TcpSocket, parser = Parser}) ->
    Transport:setopts(TcpSocket, [{active, false}]),
    send_xml(StateData, mongoose_c2s_stanzas:tls_proceed()), %% send last negotiation chunk via tcp
    TlsOpts = [{dhfile, "priv/ssl/fake_dh_server.pem"},
               {certfile, "priv/ssl/fake_server.pem"},
               {protocol_options, ["no_sslv2","no_sslv3","no_tlsv1","no_tlsv1_1"]},
               {verify, verify_none}],
    {ok, TlsSocket} = ranch_ssl:handshake(TcpSocket, TlsOpts, 5000),
    ranch_ssl:setopts(TlsSocket, [{active, once}]),
    {ok, NewParser} = exml_stream:reset_parser(Parser),
    NewStateData = StateData#state{transport = ranch_ssl,
                                   socket = TlsSocket,
                                   parser = NewParser,
                                   streamid = new_stream_id()},
    {next_state, {wait_for_stream, stream_start}, NewStateData}.

-spec handle_auth_start(state(), exml:element(), cyrsasl:sasl_state(), retries()) -> fsm_res().
handle_auth_start(StateData, El, SaslState, Retries) ->
    Mech = exml_query:attr(El, <<"mechanism">>),
    ClientIn = base64:mime_decode(exml_query:cdata(El)),
    HostType = StateData#state.host_type,
    AuthMech = [M || M <- cyrsasl:listmech(HostType), filter_mechanism(M)],
    SocketData = #{socket => StateData#state.socket, auth_mech => AuthMech},
    StepResult = cyrsasl:server_start(SaslState, Mech, ClientIn, SocketData),
    handle_sasl_step(StateData, StepResult, SaslState, Retries).

-spec handle_auth_continue(state(), exml:element(), cyrsasl:sasl_state(), retries()) -> fsm_res().
handle_auth_continue(StateData, El, SaslState, Retries) ->
    ClientIn = base64:mime_decode(exml_query:cdata(El)),
    StepResult = cyrsasl:server_step(SaslState, ClientIn),
    handle_sasl_step(StateData, StepResult, SaslState, Retries).

-spec handle_sasl_step(state(), term(), cyrsasl:sasl_state(), retries()) -> fsm_res().
handle_sasl_step(StateData, {ok, Creds}, _, _) ->
    handle_sasl_success(StateData, Creds);
handle_sasl_step(StateData, {continue, ServerOut, NewSaslState}, _, Retries) ->
    Challenge  = [#xmlcdata{content = jlib:encode_base64(ServerOut)}],
    send_element_from_server_jid(StateData, mongoose_c2s_stanzas:sasl_challenge_stanza(Challenge)),
    {next_state, {wait_for_sasl_response, NewSaslState, Retries}, StateData};
handle_sasl_step(#state{host_type = HostType, lserver = Server} = StateData,
                 {error, Error, Username}, SaslState, Retries) ->
    ?LOG_INFO(#{what => auth_failed,
                text => <<"Failed SASL authentication">>,
                user => Username, lserver => Server, c2s_state => StateData}),
    mongoose_hooks:auth_failed(HostType, Server, Username),
    send_element_from_server_jid(StateData, mongoose_c2s_stanzas:sasl_failure_stanza(Error)),
    maybe_retry_state(StateData, {wait_for_feature_before_auth, SaslState, Retries - 1}, Retries);
handle_sasl_step(#state{host_type = HostType, lserver = Server} = StateData,
                 {error, Error}, SaslState, Retries) ->
    mongoose_hooks:auth_failed(HostType, Server, unknown),
    send_element_from_server_jid(StateData, mongoose_c2s_stanzas:sasl_failure_stanza(Error)),
    maybe_retry_state(StateData, {wait_for_feature_before_auth, SaslState, Retries - 1}, Retries).

-spec handle_sasl_success(state(), term()) -> fsm_res().
handle_sasl_success(State, Creds) ->
    ServerOut = mongoose_credentials:get(Creds, sasl_success_response, undefined),
    send_element_from_server_jid(State, mongoose_c2s_stanzas:sasl_success_stanza(ServerOut)),
    User = mongoose_credentials:get(Creds, username),
    NewState = State#state{streamid = new_stream_id(),
                           jid = jid:make_bare(User, State#state.lserver)},
    ?LOG_INFO(#{what => auth_success, text => <<"Accepted SASL authentication">>,
                c2s_state => NewState}),
    {next_state, {wait_for_stream, authenticated}, NewState}.

-spec stream_start_features_before_auth(state()) -> fsm_res().
stream_start_features_before_auth(#state{host_type = HostType, lserver = LServer} = S) ->
    send_header(S, LServer, <<"1.0">>, ?MYLANG),
    Creds = mongoose_credentials:new(LServer, HostType, #{}),
    SASLState = cyrsasl:server_new(<<"jabber">>, LServer, HostType, <<>>, [], Creds),
    StreamFeatures = mongoose_c2s_stanzas:stream_features_before_auth(HostType, LServer),
    send_element_from_server_jid(S, StreamFeatures),
    {next_state, {wait_for_feature_before_auth, SASLState, ?AUTH_RETRIES}, S, ?C2S_OPEN_TIMEOUT}.

-spec stream_start_features_after_auth(state()) -> fsm_res().
stream_start_features_after_auth(#state{host_type = HostType, lserver = LServer} = S) ->
    send_header(S, LServer, <<"1.0">>, ?MYLANG),
    StreamFeatures = mongoose_c2s_stanzas:stream_features_after_auth(HostType, LServer),
    send_element_from_server_jid(S, StreamFeatures),
    {next_state, {wait_for_feature_after_auth, ?BIND_RETRIES}, S}.

-spec handle_bind_resource(state(), jlib:iq(), exml:element(), retries()) -> fsm_res().
handle_bind_resource(StateData, #iq{sub_el = SubEl} = IQ, El, Retries) ->
    R1 = exml_query:path(SubEl, [{element, <<"resource">>}, cdata]),
    R2 = jid:resourceprep(R1),
    handle_bind_resource(StateData, IQ, R2, El, Retries).

-spec handle_bind_resource(state(), jlib:iq(), jid:lresource() | error, exml:element(), retries()) ->
    fsm_res().
handle_bind_resource(StateData, _, error, El, Retries) ->
    Err = jlib:make_error_reply(El, mongoose_xmpp_errors:bad_request()),
    send_element_from_server_jid(StateData, Err),
    maybe_retry_state(StateData, {wait_for_feature_after_auth, Retries - 1}, Retries);
handle_bind_resource(StateData, IQ, <<>>, _, _) ->
    do_bind_resource(StateData, IQ, generate_random_resource());
handle_bind_resource(StateData, IQ, Res, _, _) ->
    do_bind_resource(StateData, IQ, Res).

%% Note that RFC 3921 said:
%% > Upon establishing a session, a connected resource is said to be an "active resource".
%% But, RFC 6121 says:
%% > [RFC3921] specified one additional
  % precondition: formal establishment of an instant messaging and
  % presence session.  Implementation and deployment experience has
  % shown that this additional step is unnecessary.  However, for
  % backward compatibility an implementation MAY still offer that
  % feature.  This enables older software to connect while letting
  % newer software save a round trip.
-spec do_bind_resource(state(), jlib:iq(), jid:lresource()) -> fsm_res().
do_bind_resource(StateData, IQ, Res) ->
    Jid = jid:replace_resource(StateData#state.jid, Res),
    {NewStateData, FsmActions} = open_session(StateData, Jid),
    BindResult = mongoose_c2s_stanzas:successful_resource_binding(IQ, Jid),
    send_element_from_server_jid(StateData, BindResult),
    {next_state, session_established, NewStateData, FsmActions}.

%% Note that RFC 3921 said:
%% > If no priority is provided, a server SHOULD consider the priority to be zero.
%% But, RFC 6121 says:
%% > If no priority is provided, the processing server or client MUST consider the priority to be zero.
open_session(#state{host_type = HostType, sid = SID} = StateData, Jid) ->
    ?LOG_INFO(#{what => c2s_opened_session, text => <<"Opened session">>, c2s_state => StateData}),
    FsmActions = case ejabberd_sm:open_session(HostType, SID, Jid, 0, #{}) of
                     [] -> [];
                     ReplacedPids ->
                         Timeout = mongoose_config:get_opt({replaced_wait_timeout, HostType}),
                         [{{timeout, replaced_wait_timeout}, Timeout, ReplacedPids}]
                 end,
    {StateData#state{jid = Jid}, FsmActions}.

-spec maybe_retry_state(state(), fsm_state(), retries()) -> fsm_res().
maybe_retry_state(StateData, _, 0) ->
    {stop, {shutdown, retries}, StateData};
maybe_retry_state(StateData, NextFsmState, _) ->
    {next_state, NextFsmState, StateData}.

%% @doc Check 'from' attribute in stanza RFC 6120 Section 8.1.2.1
-spec verify_from(exml:element(), jid:jid()) -> boolean().
verify_from(El, StateJid) ->
    case exml_query:attr(El, <<"from">>) of
        undefined -> true;
        SJid ->
            jid:are_equal(jid:from_binary(SJid), StateJid)
    end.

handle_c2s_packet(StateData, El) ->
    Acc0 = element_to_origin_accum(StateData, El),
    Acc1 = mongoose_hooks:c2s_preprocessing_hook(StateData#state.host_type, Acc0, StateData),
    case mongoose_acc:get(hook, result, undefined, Acc1) of
        drop -> {next_state, session_established, StateData};
        _ ->
            NewStateData = process_outgoing_stanza(StateData, Acc1, mongoose_acc:stanza_name(Acc1)),
            {keep_state, NewStateData}
    end.

%% @doc Process packets sent by user (coming from user on c2s XMPP connection)
-spec process_outgoing_stanza(state(), mongoose_acc:t(), binary()) -> state().
process_outgoing_stanza(StateData = #state{host_type = HostType}, Acc, <<"message">>) ->
    {FromJid, ToJid, El} = mongoose_acc:packet(Acc),
    Acc1 = mongoose_hooks:user_send_packet(HostType, Acc, FromJid, ToJid, El),
    _Acc2 = route(Acc1),
    StateData;
process_outgoing_stanza(StateData = #state{host_type = HostType}, Acc, <<"presence">>) ->
    Acc1 = mongoose_hooks:c2s_update_presence(HostType, Acc),
    {FromJid, ToJid, El} = mongoose_acc:packet(Acc1),
    Acc2 = mongoose_hooks:user_send_packet(HostType, Acc1, FromJid, ToJid, El),
    case jid:are_bare_equal(FromJid, ToJid) of
        true ->
            presence_update(StateData, Acc2, FromJid, ToJid, El);
        _ ->
            StateData
    end,
    StateData;
process_outgoing_stanza(StateData = #state{host_type = HostType}, Acc, <<"iq">>) ->
    Acc1 = mongoose_iq:update_acc_info(Acc),
    El = mongoose_acc:element(Acc1),
    {FromJid, ToJid, El} = mongoose_acc:packet(Acc1),
    Acc2 = mongoose_hooks:user_send_packet(HostType, Acc1, FromJid, ToJid, El),
    _Acc3 = route(Acc2),
    StateData;
process_outgoing_stanza(StateData, _Acc, _Name) ->
    StateData.

handle_incoming_stanza(StateData, Acc, _From, _To) ->
    El = mongoose_acc:element(Acc),
    send_element(StateData, El, Acc).

-spec route(mongoose_acc:t()) -> mongoose_acc:t().
route(Acc) ->
    {FromJid, ToJid, El} = mongoose_acc:packet(Acc),
    ejabberd_router:route(FromJid, ToJid, Acc, El).

%% @doc User updates his presence (non-directed presence packet)
-spec presence_update(state(), mongoose_acc:t(), jid:jid(), jid:jid(), exml:element()) -> state().
presence_update(StateData, Acc, FromJid, ToJid, El) ->
    case mongoose_acc:stanza_type(Acc) of
        undefined ->
            presence_update_to_available(StateData, Acc, FromJid, ToJid, El);
        <<"unavailable">> ->
            Status = exml_query:path(El, [{element, <<"status">>}, cdata], <<>>),
            ejabberd_sm:unset_presence(Acc, StateData#state.sid, StateData#state.jid, Status, #{}),
            StateData#state{};
        <<"error">> -> StateData;
        <<"probe">> -> StateData;
        <<"subscribe">> -> StateData;
        <<"subscribed">> -> StateData;
        <<"unsubscribe">> -> StateData;
        <<"unsubscribed">> -> StateData
    end.

presence_update_to_available(StateData, Acc, FromJid, ToJid, Packet) ->
    Acc1 = mongoose_hooks:user_available_hook(Acc, FromJid),
    presence_broadcast_first(StateData, Acc1, FromJid, ToJid, Packet).

presence_broadcast_first(_StateData, Acc, FromJid, ToJid, Packet) ->
    ejabberd_router:route(FromJid, ToJid, Acc, Packet).

%% @doc This function is executed when c2s receives a stanza from the TCP connection.
-spec element_to_origin_accum(state(), exml:element()) -> mongoose_acc:t().
element_to_origin_accum(StateData = #state{sid = SID, jid = Jid}, El) ->
    BaseParams = #{host_type => StateData#state.host_type,
                   lserver => StateData#state.lserver,
                   location => ?LOCATION,
                   element => El,
                   from_jid => Jid},
    Params = case exml_query:attr(El, <<"to">>) of
                 undefined -> BaseParams#{ to_jid => jid:to_bare(Jid) };
                 _ToBin -> BaseParams
             end,
    Acc = mongoose_acc:new(Params),
    mongoose_acc:set_permanent(c2s, [{origin_sid, SID}, {origin_jid, Jid}], Acc).

-spec stream_start_error(state(), exml:element()) -> fsm_res().
stream_start_error(StateData, Error) ->
    send_header(StateData, ?MYNAME, <<"">>, ?MYLANG),
    c2s_stream_error(StateData, Error).

-spec send_header(StateData :: state(),
                  Server :: jid:lserver(),
                  Version :: binary(),
                  Lang :: ejabberd:lang()) -> any().
send_header(StateData, Server, Version, Lang) ->
    Header = mongoose_c2s_stanzas:stream_header(Server, Version, Lang, StateData#state.streamid),
    send_text(StateData, Header).

send_trailer(StateData) ->
    send_text(StateData, ?STREAM_TRAILER).

-spec c2s_stream_error(state(), exml:element()) -> fsm_res().
c2s_stream_error(StateData, Error) ->
    ?LOG_DEBUG(#{what => c2s_stream_error, xml_error => Error, c2s_state => StateData}),
    send_element_from_server_jid(StateData, Error),
    send_text(StateData, ?STREAM_TRAILER),
    {stop, {shutdown, stream_error}, StateData}.

-spec send_element_from_server_jid(state(), exml:element()) -> any().
send_element_from_server_jid(StateData, El) ->
    Acc = mongoose_acc:new(
            #{host_type => StateData#state.host_type,
              lserver => StateData#state.lserver,
              location => ?LOCATION,
              from_jid => jid:make_noprep(<<>>, StateData#state.lserver, <<>>),
              to_jid => StateData#state.jid,
              element => El}),
    send_element(StateData, El, Acc).

-spec parse_input(iodata(), state()) -> {[exml_stream:element()], exml_stream:parser()}.
parse_input(Packet, #state{parser = Parser}) ->
    case exml_stream:parse(Parser, Packet) of
        {ok, NewParser, Elems} -> {Elems, NewParser};
        {error, Reason} -> {[{xmlstreamerror, Reason}], Parser}
    end.

-spec close_parser(state()) -> ok.
close_parser(#state{parser = undefined}) -> ok;
close_parser(#state{parser = Parser}) -> exml_stream:free_parser(Parser).

-spec close_socket(state()) -> term().
close_socket(#state{socket = Socket, transport = Transport})
  when Socket =/= undefined, Transport =/= undefined ->
    catch Transport:close(Socket);
close_socket(_) ->
    ok.

-spec close_session(state(), fsm_state(), term()) -> any().
close_session(StateData, session_established, Reason) ->
    Status = close_session_status(Reason),
    PresenceUnavailable = mongoose_c2s_stanzas:presence_unavailable(Status),
    Acc = mongoose_acc:new(#{host_type => StateData#state.host_type,
                             lserver => StateData#state.lserver,
                             location => ?LOCATION,
                             from_jid => StateData#state.jid,
                             to_jid => jid:to_bare(StateData#state.jid),
                             element => PresenceUnavailable}),
    ejabberd_sm:close_session_unset_presence(
      Acc, StateData#state.sid, StateData#state.jid, Status, sm_unset_reason(Reason));
close_session(_, _, _) ->
    ok.

close_session_status({shutdown, retries}) ->
    <<"Too many attempts">>;
close_session_status({shutdown, replaced}) ->
    <<"Replaced by new connection">>;
close_session_status(normal) ->
    <<>>;
close_session_status(_) ->
    <<"Unknown condition">>.

sm_unset_reason({shutdown, retries}) ->
    retries;
sm_unset_reason({shutdown, replaced}) ->
    replaced;
sm_unset_reason(normal) ->
    normal;
sm_unset_reason(_) ->
    error.

%% @doc This is the termination point - from here stanza is sent to the user
-spec send_element(state(), exml:element(), mongoose_acc:t()) -> maybe_ok().
send_element(StateData = #state{host_type = <<>>}, El, _) ->
    send_xml(StateData, El);
send_element(StateData = #state{host_type = HostType}, El, Acc) ->
    mongoose_hooks:xmpp_send_element(HostType, Acc, El),
    send_xml(StateData, El).

-spec send_xml(state(), exml_stream:element()) -> maybe_ok().
send_xml(StateData, Xml) ->
    send_text(StateData, exml:to_iolist(Xml)).

-spec send_text(state(), iodata()) -> maybe_ok().
send_text(StateData = #state{socket = Socket, transport = Transport}, Text) ->
    ?LOG_DEBUG(#{what => c2s_send_text, text => <<"Send XML to the socket">>,
                 send_text => Text, c2s_state => StateData}),
    Transport:send(Socket, Text).

filter_mechanism(<<"EXTERNAL">>) -> false;
filter_mechanism(<<"SCRAM-SHA-1-PLUS">>) -> false;
filter_mechanism(<<"SCRAM-SHA-", _N:3/binary, "-PLUS">>) -> false;
filter_mechanism(_) -> true.

new_stream_id() ->
    mongoose_bin:gen_from_crypto().

-spec generate_random_resource() -> jid:lresource().
generate_random_resource() ->
    <<(mongoose_bin:gen_from_timestamp())/binary, "-",(mongoose_bin:gen_from_crypto())/binary>>.
