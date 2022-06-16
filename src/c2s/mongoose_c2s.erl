-module(mongoose_c2s).

-behaviour(gen_statem).
-include("mongoose_logger.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml_stream.hrl").
-define(AUTH_RETRIES, 3).
-define(BIND_RETRIES, 5).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3]).

%% utils
-export([get_host_type/1, get_lserver/1, get_sid/1, get_jid/1, get_handler/2, filter_mechanism/1]).

-type handler_res() :: #{handlers => #{module() => term()},
                         actions => [gen_statem:action()],
                         fsm_state => fsm_state(),
                         stop => Reason :: term()}.
-type handler_fn() :: fun( (atom(), state()) -> handler_res()).
-export_type([handler_res/0, handler_fn/0]).

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
          shaper :: undefined | shaper:shaper(),
          listener_opts :: mongoose_listener:options(),
          handlers = #{} :: #{ module() => term() }
         }).
-type state() :: #state{}.
-type maybe_ok() :: ok | {error, atom()}.
-type fsm_res() :: gen_statem:event_handler_result(fsm_state(), state()).

%% C2S hooks
-type hook_params() :: #{c2s_data := state(), c2s_state := fsm_state()}.
-type hook_fn() :: fun((mongoose_acc:t(), hook_params(), gen_hook:hook_extra()) ->
                            {ok | stop, mongoose_acc:t()}).
-export_type([hook_params/0, hook_fn/0]).

-type retries() :: 0..64.
-type stream_state() :: stream_start | authenticated.
-type fsm_state() :: connecting
                   | {wait_for_stream, stream_state()}
                   | {wait_for_feature_before_auth, cyrsasl:sasl_state(), retries()}
                   | {wait_for_feature_after_auth, retries()}
                   | {wait_for_sasl_response, cyrsasl:sasl_state(), retries()}
                   | session_established
                   | resume_session.

-export_type([state/0, fsm_state/0]).

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
             StateData = #state{ranch_ref = Ref,
                                transport = Transport,
                                listener_opts = #{shaper := ShaperName,
                                                  proxy_protocol := Proxy,
                                                  max_stanza_size := MaxStanzaSize}}) ->
    Socket = get_socket_maybe_after_proxy_and_ip_blacklist(Ref, Transport, Proxy),
    {ok, NewParser} = exml_stream:new_parser([{max_child_size, MaxStanzaSize}]),
    ShaperState = shaper:new(ShaperName),
    NewStateData = StateData#state{lserver = ?MYNAME, socket = Socket,
                                   parser = NewParser, shaper = ShaperState},
    activate_socket(NewStateData),
    {next_state, {wait_for_stream, stream_start}, NewStateData, state_timeout()};

%% TODO in any state? probably only during session_established
handle_event(info, {route, Acc}, _, StateData) ->
    handle_incoming_stanza(StateData, Acc),
    keep_state_and_data;
handle_event(info, {route, _From, _To, Acc}, _, StateData) ->
    handle_incoming_stanza(StateData, Acc),
    keep_state_and_data;

handle_event(info, {TcpOrSSl, Socket, Input}, _FsmState, StateData = #state{socket = Socket})
  when TcpOrSSl =:= tcp orelse TcpOrSSl =:= ssl ->
    ?LOG_DEBUG(#{what => received_xml_on_stream, packet => Input, c2s_pid => self()}),
    handle_socket_data(StateData, Input);
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
handle_event(internal, {xmlstreamerror, <<"child element too big">> = Err}, _, StateData) ->
    c2s_stream_error(StateData, mongoose_xmpp_errors:policy_violation(?MYLANG, Err));
handle_event(internal, {xmlstreamerror, Err}, _, StateData) ->
    c2s_stream_error(StateData, mongoose_xmpp_errors:xml_not_well_formed(?MYLANG, Err));
handle_event(internal, #xmlel{name = <<"starttls">>} = El, {wait_for_feature_before_auth, SaslState, Retries}, StateData) ->
    case exml_query:attr(El, <<"xmlns">>) of
        ?NS_TLS ->
            handle_starttls(StateData, El, SaslState, Retries);
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

handle_event({timeout, activate_socket}, activate_socket, _, StateData) ->
    activate_socket(StateData),
    keep_state_and_data;
handle_event({timeout, replaced_wait_timeout}, ReplacedPids, FsmState, StateData) ->
    [ case erlang:is_process_alive(Pid) of
          false -> ok;
          true ->
              ?LOG_WARNING(#{what => c2s_replaced_wait_timeout,
                             text => <<"Some processes are not responding when handling replace messages">>,
                             replaced_pid => Pid, state_name => FsmState, c2s_state => StateData})
      end || Pid <- ReplacedPids ],
    keep_state_and_data;
handle_event({timeout, Name}, Handler, _, StateData) when is_atom(Name), is_function(Handler, 2) ->
    Acc = Handler(Name, StateData),
    handle_state(StateData, Acc);

handle_event(info, {Closed, Socket}, _, _StateData = #state{socket = Socket})
  when Closed =:= tcp_closed; Closed =:= ssl_closed ->
    {stop, {shutdown, socket_closed}};
handle_event(info, {Error, Socket}, _, _StateData = #state{socket = Socket})
  when Error =:= tcp_error; Error =:= ssl_error ->
    {stop, {shutdown, socket_error}};

handle_event(state_timeout, state_timeout_termination, _FsmState, StateData) ->
    StreamConflict = mongoose_xmpp_errors:connection_timeout(),
    send_element_from_server_jid(StateData, StreamConflict),
    send_trailer(StateData),
    {stop, {shutdown, state_timeout}};

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
-spec get_socket_maybe_after_proxy_and_ip_blacklist(ranch:ref(), module(), boolean()) ->
    ranch_transport:socket().
get_socket_maybe_after_proxy_and_ip_blacklist(Ref, _, true) ->
    {ok, #{src_address := PeerIp}} = ranch:recv_proxy_header(Ref, 1000),
    verify_ip_is_not_blacklisted(PeerIp),
    {ok, Socket} = ranch:handshake(Ref),
    Socket;
get_socket_maybe_after_proxy_and_ip_blacklist(Ref, Transport, false) ->
    {ok, Socket} = ranch:handshake(Ref),
    {ok, {PeerIp, _}} = Transport:peername(Socket),
    verify_ip_is_not_blacklisted(PeerIp),
    Socket.

-spec verify_ip_is_not_blacklisted(inet:ip_address()) -> ok | no_return().
verify_ip_is_not_blacklisted(PeerIp) ->
    case mongoose_hooks:check_bl_c2s(PeerIp) of
        true ->
            ?LOG_INFO(#{what => c2s_blacklisted_ip, ip => PeerIp,
                        text => <<"Connection attempt from blacklisted IP">>}),
            throw({stop, {shutdown, ip_blacklisted}});
        false ->
            ok
    end.

-spec handle_stream_start(state(), exml:element(), stream_state()) -> fsm_res().
handle_stream_start(S0, StreamStart, StreamState) ->
    LServer = jid:nameprep(exml_query:attr(StreamStart, <<"to">>, <<>>)),
    case {StreamState,
          exml_query:attr(StreamStart, <<"xmlns:stream">>, <<>>),
          exml_query:attr(StreamStart, <<"version">>, <<>>),
          mongoose_domain_api:get_domain_host_type(LServer)} of
        {stream_start, ?NS_STREAM, <<"1.0">>, {ok, HostType}} ->
            S = S0#state{host_type = HostType, lserver = LServer},
            stream_start_features_before_auth(S);
        {authenticated, ?NS_STREAM, <<"1.0">>, {ok, HostType}} ->
            S = S0#state{host_type = HostType, lserver = LServer},
            stream_start_features_after_auth(S);
        {_, ?NS_STREAM, _Pre1_0, {ok, HostType}} ->
            %% (http://xmpp.org/rfcs/rfc6120.html#streams-negotiation-features)
            S = S0#state{host_type = HostType, lserver = LServer},
            stream_start_error(S, mongoose_xmpp_errors:unsupported_version());
        {_, ?NS_STREAM, _, {error, not_found}} ->
            stream_start_error(S0, mongoose_xmpp_errors:host_unknown());
        {_, _, _, _} ->
            stream_start_error(S0, mongoose_xmpp_errors:invalid_namespace())
    end.

-spec handle_starttls(state(), exml:element(), cyrsasl:sasl_state(), retries()) -> fsm_res().
handle_starttls(StateData = #state{transport = ranch_ssl}, El, SaslState, Retries) ->
    Err = jlib:make_error_reply(El, mongoose_xmpp_errors:bad_request()),
    send_element_from_server_jid(StateData, Err),
    maybe_retry_state(StateData, {wait_for_feature_before_auth, SaslState, Retries - 1}, Retries);
handle_starttls(StateData = #state{transport = ranch_tcp,
                                   socket = TcpSocket,
                                   listener_opts = #{tls := #{opts := TlsOpts}},
                                   parser = Parser}, _, _, _) ->
    ranch_tcp:setopts(TcpSocket, [{active, false}]),
    send_xml(StateData, mongoose_c2s_stanzas:tls_proceed()), %% send last negotiation chunk via tcp
    case ranch_ssl:handshake(TcpSocket, TlsOpts, 1000) of
        {ok, TlsSocket} ->
            {ok, NewParser} = exml_stream:reset_parser(Parser),
            NewStateData = StateData#state{transport = ranch_ssl,
                                           socket = TlsSocket,
                                           parser = NewParser,
                                           streamid = new_stream_id()},
            activate_socket(NewStateData),
            {next_state, {wait_for_stream, stream_start}, NewStateData, state_timeout()};
        {error, closed} ->
            {stop, {shutdown, tls_closed}};
        {error, timeout} ->
            {stop, {shutdown, tls_timeout}};
        {error, {tls_alert, TlsAlert}} ->
            {stop, TlsAlert}
    end.

-spec handle_auth_start(state(), exml:element(), cyrsasl:sasl_state(), retries()) -> fsm_res().
handle_auth_start(StateData, El, SaslState, Retries) ->
    case {StateData#state.transport, StateData#state.listener_opts} of
        {ranch_tcp, #{tls := #{mode := starttls_required}}} ->
            c2s_stream_error(StateData, mongoose_xmpp_errors:policy_violation(?MYLANG, <<"Use of STARTTLS required">>));
        _ ->
            do_handle_auth_start(StateData, El, SaslState, Retries)
    end.

-spec do_handle_auth_start(state(), exml:element(), cyrsasl:sasl_state(), retries()) -> fsm_res().
do_handle_auth_start(StateData, El, SaslState, Retries) ->
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
    {next_state, {wait_for_sasl_response, NewSaslState, Retries}, StateData, state_timeout()};
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
    {next_state, {wait_for_stream, authenticated}, NewState, state_timeout()}.

-spec stream_start_features_before_auth(state()) -> fsm_res().
stream_start_features_before_auth(#state{host_type = HostType, lserver = LServer,
                                         listener_opts = LOpts} = S) ->
    send_header(S, LServer, <<"1.0">>, ?MYLANG),
    CredOpts = mongoose_credentials:make_opts(LOpts),
    Creds = mongoose_credentials:new(LServer, HostType, CredOpts),
    SASLState = cyrsasl:server_new(<<"jabber">>, LServer, HostType, <<>>, [], Creds),
    StreamFeatures = mongoose_c2s_stanzas:stream_features_before_auth(HostType, LServer, LOpts),
    send_element_from_server_jid(S, StreamFeatures),
    {next_state, {wait_for_feature_before_auth, SASLState, ?AUTH_RETRIES}, S, state_timeout()}.

-spec stream_start_features_after_auth(state()) -> fsm_res().
stream_start_features_after_auth(#state{host_type = HostType, lserver = LServer} = S) ->
    send_header(S, LServer, <<"1.0">>, ?MYLANG),
    StreamFeatures = mongoose_c2s_stanzas:stream_features_after_auth(HostType, LServer),
    send_element_from_server_jid(S, StreamFeatures),
    {next_state, {wait_for_feature_after_auth, ?BIND_RETRIES}, S, state_timeout()}.

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
    {next_state, NextFsmState, StateData, state_timeout()}.

%% @doc Check 'from' attribute in stanza RFC 6120 Section 8.1.2.1
-spec verify_from(exml:element(), jid:jid()) -> boolean().
verify_from(El, StateJid) ->
    case exml_query:attr(El, <<"from">>) of
        undefined -> true;
        SJid ->
            jid:are_equal(jid:from_binary(SJid), StateJid)
    end.

-spec handle_c2s_packet(state(), exml:element()) -> fsm_res().
handle_c2s_packet(StateData = #state{host_type = HostType}, El) ->
    Acc0 = element_to_origin_accum(StateData, El),
    Acc1 = mongoose_hooks:c2s_preprocessing_hook(HostType, Acc0, StateData),
    case mongoose_acc:get(hook, result, undefined, Acc1) of
        drop -> {next_state, session_established, StateData};
        _ -> do_handle_c2s_packet(StateData, Acc1)
    end.

-spec do_handle_c2s_packet(state(), mongoose_acc:t()) -> fsm_res().
do_handle_c2s_packet(StateData = #state{host_type = HostType}, Acc) ->
    case user_send_packet(HostType, Acc, hook_arg(StateData)) of
        {ok, Acc1} ->
            Acc2 = process_outgoing_stanza(StateData, Acc1, mongoose_acc:stanza_name(Acc1)),
            handle_state_after_packet(StateData, Acc2);
        {stop, Acc1} ->
            handle_state_after_packet(StateData, Acc1)
    end.

%% @doc Process packets sent by user (coming from user on c2s XMPP connection)
-spec process_outgoing_stanza(state(), mongoose_acc:t(), binary()) -> mongoose_acc:t().
process_outgoing_stanza(StateData = #state{host_type = HostType}, Acc, <<"message">>) ->
    TS0 = mongoose_acc:timestamp(Acc),
    Acc1 = user_send_message(HostType, Acc, hook_arg(StateData)),
    Acc2 = maybe_route(Acc1),
    TS1 = erlang:system_time(microsecond),
    mongoose_metrics:update(HostType, [data, xmpp, sent, message, processing_time], (TS1 - TS0)),
    Acc2;
process_outgoing_stanza(StateData = #state{host_type = HostType}, Acc, <<"iq">>) ->
    Acc1 = mongoose_iq:update_acc_info(Acc),
    Acc2 = user_send_iq(HostType, Acc1, hook_arg(StateData)),
    maybe_route(Acc2);
process_outgoing_stanza(StateData = #state{host_type = HostType}, Acc, <<"presence">>) ->
    user_send_presence(HostType, Acc, hook_arg(StateData)),
    Acc;
process_outgoing_stanza(StateData = #state{host_type = HostType}, Acc, _) ->
    user_send_non_stanza(HostType, Acc, hook_arg(StateData)).

-spec handle_state_after_packet(state(), mongoose_acc:t()) -> fsm_res().
handle_state_after_packet(StateData, Acc) ->
    Res = maps:from_list(mongoose_acc:get(c2s, Acc)),
    handle_state(StateData, Res).

-type statem_actions() :: #{handlers => #{atom() => {module(), atom(), term()}},
                            actions => [gen_statem:action()],
                            stop => atom()}.

-spec handle_state(state(), statem_actions()) -> fsm_res().
handle_state(StateData, #{stop := Reason}) ->
    {stop, {shutdown, Reason}, StateData};
handle_state(StateData = #state{handlers = StateHandlers},
             #{handlers := Handlers, actions := Actions}) ->
    {keep_state, StateData#state{handlers = maps:merge(StateHandlers, Handlers)}, Actions};
handle_state(StateData = #state{handlers = StateHandlers}, #{handlers := Handlers}) ->
    {keep_state, StateData#state{handlers = maps:merge(StateHandlers, Handlers)}};
handle_state(StateData, #{actions := Actions}) ->
    {keep_state, StateData, Actions};
handle_state(StateData, #{}) ->
    {keep_state, StateData}.

-spec hook_arg(state()) -> handler_params().
hook_arg(StateData) ->
    #{c2s => StateData, handlers => #{}}.

-spec handle_incoming_stanza(state(), mongoose_acc:t()) -> maybe_ok().
handle_incoming_stanza(StateData, Acc) ->
    El = mongoose_acc:element(Acc),
    send_element(StateData, El, Acc).

-spec maybe_route({ok | stop, mongoose_acc:t()}) -> mongoose_acc:t().
maybe_route({ok, Acc}) ->
    {FromJid, ToJid, El} = mongoose_acc:packet(Acc),
    ejabberd_router:route(FromJid, ToJid, Acc, El),
    Acc;
maybe_route({stop, Acc}) ->
    Acc.

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
    mongoose_acc:set_permanent(c2s, [{module, ?MODULE}, {origin_sid, SID}, {origin_jid, Jid}], Acc).

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

-spec handle_socket_data(state(), iodata()) -> fsm_res().
handle_socket_data(StateData = #state{parser = Parser, shaper = Shaper}, Packet) ->
    case exml_stream:parse(Parser, Packet) of
        {error, Reason} ->
            NextEvent = {next_event, internal, {xmlstreamerror, iolist_to_binary(Reason)}},
            {keep_state, StateData, NextEvent};
        {ok, NewParser, XmlElements} ->
            Size = byte_size(Packet),
            {NewShaper, Pause} = shaper:update(Shaper, Size),
            mongoose_metrics:update(global, [data, xmpp, received, xml_stanza_size], Size),
            NewStateData = StateData#state{parser = NewParser, shaper = NewShaper},
            MaybePauseTimeout = maybe_pause(NewStateData, Pause),
            StreamEvents = [ {next_event, internal, XmlEl} || XmlEl <- XmlElements ],
            {keep_state, NewStateData, MaybePauseTimeout ++ StreamEvents}
    end.

-spec activate_socket(state()) -> any().
activate_socket(#state{socket = Socket, transport = ranch_tcp}) ->
    ranch_tcp:setopts(Socket, [{active, once}]);
activate_socket(#state{socket = Socket, transport = ranch_ssl}) ->
    ranch_ssl:setopts(Socket, [{active, once}]).

-spec maybe_pause(state(), integer()) -> any().
maybe_pause(_StateData, Pause) when Pause > 0 ->
    [{{timeout, activate_socket}, Pause, activate_socket}];
maybe_pause(StateData, _) ->
    activate_socket(StateData),
    [].

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

state_timeout() ->
    Timeout = mongoose_config:get_opt(c2s_state_timeout),
    {state_timeout, Timeout, state_timeout_termination}.

filter_mechanism(<<"EXTERNAL">>) -> false;
filter_mechanism(<<"SCRAM-SHA-1-PLUS">>) -> false;
filter_mechanism(<<"SCRAM-SHA-", _N:3/binary, "-PLUS">>) -> false;
filter_mechanism(_) -> true.

-spec get_host_type(state()) -> mongooseim:host_type().
get_host_type(#state{host_type = HostType}) ->
    HostType.

-spec get_lserver(state()) -> jid:lserver().
get_lserver(#state{lserver = LServer}) ->
    LServer.

-spec get_sid(state()) -> ejabberd_sm:sid().
get_sid(#state{sid = Sid}) ->
    Sid.

-spec get_jid(state()) -> jid:jid().
get_jid(#state{jid = Jid}) ->
    Jid.

-spec get_handler(state(), atom()) -> term().
get_handler(#state{handlers = Handlers}, HandlerName) ->
    maps:get(HandlerName, Handlers, {error, not_found}).

new_stream_id() ->
    mongoose_bin:gen_from_crypto().

-spec generate_random_resource() -> jid:lresource().
generate_random_resource() ->
    <<(mongoose_bin:gen_from_timestamp())/binary, "-",(mongoose_bin:gen_from_crypto())/binary>>.

-spec user_send_packet(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: mongoose_c2s:handler_params(),
    Result :: {ok | stop, mongoose_acc:t()}.
user_send_packet(HostType, Acc, Params) ->
    {From, To, El} = mongoose_acc:packet(Acc),
    Args = [From, To, El],
    ParamsWithLegacyArgs = ejabberd_hooks:add_args(Params, Args),
    gen_hook:run_fold(user_send_packet, HostType, Acc, ParamsWithLegacyArgs).

-spec user_send_message(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: mongoose_c2s:handler_params(),
    Result :: {ok | stop, mongoose_acc:t()}.
user_send_message(HostType, Acc, Params) ->
    gen_hook:run_fold(user_send_message, HostType, Acc, Params).

-spec user_send_iq(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: mongoose_c2s:handler_params(),
    Result :: {ok | stop, mongoose_acc:t()}.
user_send_iq(HostType, Acc, Params) ->
    gen_hook:run_fold(user_send_iq, HostType, Acc, Params).

-spec user_send_presence(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: mongoose_c2s:handler_params(),
    Result :: {ok | stop, mongoose_acc:t()}.
user_send_presence(HostType, Acc, Params) ->
    gen_hook:run_fold(user_send_presence, HostType, Acc, Params).

-spec user_send_non_stanza(HostType, Acc, Params) -> Result when
    HostType :: mongooseim:host_type(),
    Acc :: mongoose_acc:t(),
    Params :: mongoose_c2s:handler_params(),
    Result :: {ok | stop, mongoose_acc:t()}.
user_send_non_stanza(HostType, Acc, Params) ->
    gen_hook:run_fold(user_send_non_stanza, HostType, Acc, Params).
