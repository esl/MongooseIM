-module(mongoose_c2s).
-xep([{xep, 170}, {version, "1.0"}]).

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
-export([start_link/2, start/2, stop/2, exit/2, async/3, async_with_state/3, call/3, cast/3]).
-export([create_data/1, get_host_type/1, get_lserver/1, get_sid/1, get_jid/1,
         get_info/1, set_info/2,
         get_mod_state/2, get_listener_opts/1, merge_mod_state/2, remove_mod_state/2,
         get_ip/1, get_socket/1, get_lang/1, get_stream_id/1, hook_arg/5]).
-export([get_auth_mechs/1, get_auth_mechs_to_announce/1,
         c2s_stream_error/2, maybe_retry_state/1, merge_states/2]).
-export([route/2, reroute_buffer/2, reroute_buffer_to_pid/3, open_session/1]).
-export([set_jid/2, set_auth_module/2, state_timeout/1, handle_state_after_packet/3]).
-export([replace_resource/2, generate_random_resource/0]).
-export([verify_user/4, maybe_open_session/3]).

-ignore_xref([get_ip/1, get_socket/1]).

-record(c2s_data, {
          host_type :: undefined | mongooseim:host_type(),
          lserver = ?MYNAME :: jid:lserver(),
          lang = ?MYLANG :: ejabberd:lang(),
          sid = ejabberd_sm:make_new_sid() :: ejabberd_sm:sid(),
          streamid = new_stream_id() :: binary(),
          jid :: undefined | jid:jid(),
          socket :: undefined | mongoose_xmpp_socket:socket(),
          parser :: undefined | exml_stream:parser(),
          shaper :: undefined | mongoose_shaper:shaper(),
          listener_opts :: undefined | mongoose_listener:options(),
          state_mod = #{} :: #{module() => term()},
          info = #{} :: info()
         }).
-type data() :: #c2s_data{}.
-type maybe_ok() :: ok | {error, atom()}.
-type fsm_res(State) :: gen_statem:event_handler_result(state(State), data()).
-type fsm_res() :: fsm_res(term()).
-type packet() :: {jid:jid(), jid:jid(), exml:element()}.
-type info() :: #{atom() => term()}.

-type retries() :: 0..8.
-type stream_state() :: stream_start | authenticated.
-type state(State) :: connect
                    | {wait_for_stream, stream_state()}
                    | {wait_for_feature_before_auth, mongoose_acc:t(), retries()}
                    | {wait_for_feature_after_auth, retries()}
                    | {wait_for_sasl_response, mongoose_acc:t(), retries()}
                    | wait_for_session_establishment
                    | session_established
                    | ?EXT_C2S_STATE(State).
-type state() :: state(term()).

-export_type([packet/0, data/0, state/0, state/1, fsm_res/0, fsm_res/1, retries/0]).

-export([instrumentation/0]).

-spec instrumentation() -> [mongoose_instrument:spec()].
instrumentation() ->
    lists:flatmap(fun instrumentation/1, ?ALL_HOST_TYPES) ++
        mongoose_instrument_xmpp:instrumentation(c2s).

-spec instrumentation(mongooseim:host_type()) -> [mongoose_instrument:spec()].
instrumentation(HostType) when is_binary(HostType) ->
    [{c2s_message_processed, #{host_type => HostType}, #{metrics => #{time => histogram}}},
     {c2s_auth_failed, #{host_type => HostType}, #{metrics => #{count => spiral}}}].

%%%----------------------------------------------------------------------
%%% gen_statem
%%%----------------------------------------------------------------------

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    handle_event_function.

-spec init(mongoose_listener:init_args()) -> gen_statem:init_result(state(), data()).
init({Transport, Ref, LOpts}) ->
    ConnectEvent = {next_event, internal, {connect, {Transport, Ref}}},
    {ok, connect, #c2s_data{listener_opts = LOpts}, ConnectEvent}.

-spec handle_event(gen_statem:event_type(), term(), state(), data()) -> fsm_res().
handle_event(internal, {connect, {Transport, Ref}}, connect, StateData) ->
    #{shaper := ShaperName, max_stanza_size := MaxStanzaSize} = LOpts =
        StateData#c2s_data.listener_opts,
    C2SSocket = mongoose_xmpp_socket:accept(Transport, c2s, Ref, LOpts),
    verify_ip_is_not_blacklisted(C2SSocket),
    {ok, Parser} = exml_stream:new_parser([{max_element_size, MaxStanzaSize}]),
    Shaper = mongoose_shaper:new(ShaperName),
    StateData1 = StateData#c2s_data{socket = C2SSocket, parser = Parser, shaper = Shaper},
    {next_state, {wait_for_stream, stream_start}, StateData1, state_timeout(LOpts)};

handle_event(internal, #xmlstreamstart{attrs = Attrs}, {wait_for_stream, StreamState}, StateData) ->
    handle_stream_start(StateData, Attrs, StreamState);
handle_event(internal, Unexpected, {wait_for_stream, _}, StateData) ->
    handle_maybe_hide_service_name(StateData, Unexpected);
handle_event(internal, #xmlstreamstart{}, _, StateData) ->
    c2s_stream_error(StateData, mongoose_xmpp_errors:policy_violation());

handle_event(internal, #xmlstreamend{}, _, StateData) ->
    send_trailer(StateData),
    {stop, {shutdown, stream_end}};
handle_event(internal, #xmlstreamerror{name = <<"element too big">> = Err}, _, StateData) ->
    c2s_stream_error(StateData, mongoose_xmpp_errors:policy_violation(StateData#c2s_data.lang, Err));
handle_event(internal, #xmlstreamerror{name = Err}, _, StateData) ->
    c2s_stream_error(StateData, mongoose_xmpp_errors:xml_not_well_formed(StateData#c2s_data.lang, Err));
handle_event(internal, #xmlel{name = <<"starttls">>} = El, {wait_for_feature_before_auth, SaslAcc, Retries}, StateData) ->
    case exml_query:attr(El, <<"xmlns">>) of
        ?NS_TLS ->
            handle_starttls(StateData, El, SaslAcc, Retries);
        _ ->
            c2s_stream_error(StateData, mongoose_xmpp_errors:invalid_namespace())
    end;
handle_event(internal, #xmlel{name = <<"auth">>} = El, {wait_for_feature_before_auth, SaslAcc, Retries}, StateData) ->
    case exml_query:attr(El, <<"xmlns">>) of
        ?NS_SASL ->
            handle_auth_start(StateData, El, SaslAcc, Retries);
        _ ->
            c2s_stream_error(StateData, mongoose_xmpp_errors:invalid_namespace())
    end;
handle_event(internal, #xmlel{name = <<"response">>} = El, {wait_for_sasl_response, SaslAcc, Retries}, StateData) ->
    case exml_query:attr(El, <<"xmlns">>) of
        ?NS_SASL ->
            handle_auth_continue(StateData, El, SaslAcc, Retries);
        _ ->
            c2s_stream_error(StateData, mongoose_xmpp_errors:invalid_namespace())
    end;
handle_event(internal, #xmlel{name = <<"abort">>} = El, {wait_for_sasl_response, SaslAcc, Retries}, StateData) ->
    case exml_query:attr(El, <<"xmlns">>) of
        ?NS_SASL ->
            handle_sasl_abort(StateData, SaslAcc, Retries);
        _ ->
            c2s_stream_error(StateData, mongoose_xmpp_errors:invalid_namespace())
    end;
handle_event(internal, #xmlel{name = <<"iq">>} = El, {wait_for_feature_after_auth, _} = C2SState, StateData) ->
    case jlib:iq_query_info(El) of
        #iq{type = set, xmlns = ?NS_BIND} = IQ ->
            handle_bind_resource(StateData, C2SState, El, IQ);
        _ ->
            Err = jlib:make_error_reply(El, mongoose_xmpp_errors:bad_request()),
            send_element_from_server_jid(StateData, Err),
            maybe_retry_state(StateData, C2SState)
    end;
handle_event(internal, #xmlel{name = <<"iq">>} = El, wait_for_session_establishment = C2SState, StateData) ->
    case jlib:iq_query_info(El) of
        #iq{type = set, xmlns = ?NS_SESSION} = IQ ->
            handle_session_establishment(StateData, C2SState, El, IQ);
        _ ->
            handle_foreign_packet(StateData, C2SState, El)
    end;
handle_event(internal, #xmlel{} = El, session_established = C2SState, StateData) ->
    case verify_from(El, StateData#c2s_data.jid) of
        false ->
            c2s_stream_error(StateData, mongoose_xmpp_errors:invalid_from());
        true ->
            case verify_to(El) of
                true ->
                    handle_c2s_packet(StateData, C2SState, El);
                false ->
                    Err = jlib:make_error_reply(El, mongoose_xmpp_errors:jid_malformed()),
                    Acc = send_element_from_server_jid(StateData, Err),
                    handle_state_after_packet(StateData, C2SState, Acc)
            end
    end;

handle_event(internal, {flush, Acc}, C2SState, StateData) ->
    handle_flush(StateData, C2SState, Acc);

%% This is an xml packet in any state other than session_established,
%% which is not one of the default ones defined in the RFC. For example stream management.
handle_event(internal, #xmlel{} = El, C2SState, StateData) ->
    handle_foreign_packet(StateData, C2SState, El);

handle_event(info, Info, FsmState, StateData) ->
    handle_info(StateData, FsmState, Info);

handle_event(cast, Info, FsmState, StateData) ->
    handle_cast(StateData, FsmState, Info);

handle_event({timeout, Name}, Payload, C2SState, StateData) ->
    handle_timeout(StateData, C2SState, Name, Payload);

handle_event(state_timeout, state_timeout_termination, {wait_for_stream, stream_start}, StateData) ->
    StreamError = mongoose_xmpp_errors:connection_timeout(),
    stream_start_error(StateData, StreamError);
handle_event(state_timeout, state_timeout_termination, _FsmState, StateData) ->
    StreamConflict = mongoose_xmpp_errors:connection_timeout(),
    send_element_from_server_jid(StateData, StreamConflict),
    send_trailer(StateData),
    {stop, {shutdown, state_timeout}};

handle_event(EventType, EventContent, C2SState, StateData) ->
    handle_foreign_event(StateData, C2SState, EventType, EventContent).

-spec terminate(term(), state(), data()) -> term().
terminate(Reason, connect, _StateData) ->
    ?LOG_INFO(#{what => c2s_failed_to_initialize, reason => Reason});
terminate(Reason, C2SState, #c2s_data{host_type = HostType, lserver = LServer, sid = SID} = StateData) ->
    ?LOG_DEBUG(#{what => c2s_statem_terminate, reason => Reason, c2s_state => C2SState, c2s_data => StateData}),
    Params = hook_arg(StateData, C2SState, terminate, Reason, Reason),
    Acc0 = mongoose_acc:new(#{host_type => HostType, lserver => LServer, location => ?LOCATION}),
    Acc1 = mongoose_acc:set_permanent(c2s, [{origin_sid, SID}], Acc0),
    Acc2 = mongoose_c2s_hooks:user_terminate(HostType, Acc1, Params),
    Acc3 = do_close_session(StateData, C2SState, Acc2, Reason),
    mongoose_c2s_hooks:reroute_unacked_messages(HostType, Acc3, Params),
    bounce_messages(StateData),
    close_parser(StateData),
    close_socket(StateData),
    ok.

%%%----------------------------------------------------------------------
%%% socket helpers
%%%----------------------------------------------------------------------

-spec handle_socket_data(data(), {_, _, iodata()}) -> fsm_res().
handle_socket_data(StateData = #c2s_data{socket = Socket}, Payload) ->
    case mongoose_xmpp_socket:handle_data(Socket, Payload) of
        {error, _Reason} ->
            {stop, {shutdown, socket_error}, StateData};
        Data ->
            handle_socket_packet(StateData, Data)
    end.

-spec handle_socket_packet(data(), iodata() | {raw, [exml_stream:element()]}) -> fsm_res().
handle_socket_packet(StateData, {raw, Elements}) ->
    ?LOG_DEBUG(#{what => received_raw_on_stream, elements => Elements, c2s_pid => self()}),
    handle_socket_elements(StateData, Elements, 0);
handle_socket_packet(StateData = #c2s_data{parser = Parser}, Packet) ->
    ?LOG_DEBUG(#{what => received_xml_on_stream, packet => Packet, c2s_pid => self()}),
    case exml_stream:parse(Parser, Packet) of
        {error, Reason} ->
            NextEvent = {next_event, internal, #xmlstreamerror{name = Reason}},
            {keep_state, StateData, NextEvent};
        {ok, NewParser, XmlElements} ->
            Size = iolist_size(Packet),
            NewStateData = StateData#c2s_data{parser = NewParser},
            handle_socket_elements(NewStateData, XmlElements, Size)
    end.

-spec handle_socket_elements(data(), [exml:element()], non_neg_integer()) -> fsm_res().
handle_socket_elements(StateData = #c2s_data{shaper = Shaper}, Elements, Size) ->
    [execute_element_event(Element, StateData, xmpp_element_in) || Element = #xmlel{} <- Elements],
    {NewShaper, Pause} = mongoose_shaper:update(Shaper, Size),
    NewStateData = StateData#c2s_data{shaper = NewShaper},
    StreamEvents0 = [ {next_event, internal, XmlEl} || XmlEl <- Elements ],
    StreamEvents1 = maybe_add_pause(NewStateData, StreamEvents0, Pause),
    {keep_state, NewStateData, StreamEvents1}.

-spec maybe_add_pause(data(), [gen_statem:action()], integer()) -> [gen_statem:action()].
maybe_add_pause(_, StreamEvents, Pause) when Pause > 0 ->
    [{{timeout, activate_socket}, Pause, activate_socket} | StreamEvents];
maybe_add_pause(#c2s_data{socket = Socket}, StreamEvents, _) ->
    mongoose_xmpp_socket:activate(Socket),
    StreamEvents.

-spec close_socket(data()) -> ok | {error, term()}.
close_socket(#c2s_data{socket = undefined}) ->
    ok;
close_socket(#c2s_data{socket = Socket}) ->
    mongoose_xmpp_socket:close(Socket).

-spec activate_socket(data()) -> ok | {error, term()}.
activate_socket(#c2s_data{socket = Socket}) ->
    mongoose_xmpp_socket:activate(Socket).

%%%----------------------------------------------------------------------
%%% error handler helpers
%%%----------------------------------------------------------------------

-spec handle_foreign_event(data(), state(), gen_statem:event_type(), term()) -> fsm_res().
handle_foreign_event(StateData = #c2s_data{host_type = HostType, lserver = LServer},
                     C2SState, EventType, EventContent) ->
    Params = hook_arg(StateData, C2SState, EventType, EventContent, undefined),
    AccParams = #{host_type => HostType, lserver => LServer, location => ?LOCATION},
    Acc0 = mongoose_acc:new(AccParams),
    case mongoose_c2s_hooks:foreign_event(HostType, Acc0, Params) of
        {ok, _Acc1} ->
            ?LOG_WARNING(#{what => unknown_statem_event, c2s_state => C2SState,
                           event_type => EventType, event_content => EventContent}),
            keep_state_and_data;
        {stop, Acc1} ->
            handle_state_after_packet(StateData, C2SState, Acc1)
    end.

-spec handle_stop_request(data(), state(), atom()) -> fsm_res().
handle_stop_request(StateData = #c2s_data{host_type = HostType, lserver = LServer}, C2SState, Reason) ->
    Params = hook_arg(StateData, C2SState, cast, {stop, Reason}, Reason),
    AccParams = #{host_type => HostType, lserver => LServer, location => ?LOCATION},
    Acc0 = mongoose_acc:new(AccParams),
    Res = mongoose_c2s_hooks:user_stop_request(HostType, Acc0, Params),
    stop_if_unhandled(StateData, C2SState, Res, Reason).

-spec handle_socket_closed(data(), state(), term()) -> fsm_res().
handle_socket_closed(StateData = #c2s_data{host_type = HostType, lserver = LServer}, C2SState, SocketClosed) ->
    Params = hook_arg(StateData, C2SState, info, SocketClosed, SocketClosed),
    AccParams = #{host_type => HostType, lserver => LServer, location => ?LOCATION},
    Acc0 = mongoose_acc:new(AccParams),
    Res = mongoose_c2s_hooks:user_socket_closed(HostType, Acc0, Params),
    stop_if_unhandled(StateData, C2SState, Res, socket_closed).

-spec handle_socket_error(data(), state(), term()) -> fsm_res().
handle_socket_error(StateData = #c2s_data{host_type = HostType, lserver = LServer}, C2SState, SocketError) ->
    Params = hook_arg(StateData, C2SState, info, SocketError, SocketError),
    AccParams = #{host_type => HostType, lserver => LServer, location => ?LOCATION},
    Acc0 = mongoose_acc:new(AccParams),
    Res = mongoose_c2s_hooks:user_socket_error(HostType, Acc0, Params),
    stop_if_unhandled(StateData, C2SState, Res, socket_error).

%% These conditions required that one and only one handler declared full control over it,
%% by making the hook stop at that point. If so, the process remains alive,
%% in control of the handler, otherwise, the condition is treated as terminal.
-spec stop_if_unhandled(data(), state(), gen_hook:hook_fn_ret(mongoose_acc:t()), atom()) -> fsm_res().
stop_if_unhandled(StateData, C2SState, {stop, Acc}, _) ->
    handle_state_after_packet(StateData, C2SState, Acc);
stop_if_unhandled(_, _, {ok, _Acc}, Reason) ->
    {stop, {shutdown, Reason}}.

%%%----------------------------------------------------------------------
%%% helpers
%%%----------------------------------------------------------------------

-spec handle_stream_start(data(), exml:attrs(), stream_state()) -> fsm_res().
handle_stream_start(S0, Attrs, StreamState) ->
    Lang = get_xml_lang(Attrs),
    From = maybe_capture_from_jid_in_stream_start(Attrs),
    LServer = jid:nameprep(maps:get(<<"to">>, Attrs, <<>>)),
    case {StreamState,
          maps:get(<<"xmlns:stream">>, Attrs, <<>>),
          maps:get(<<"version">>, Attrs, <<>>),
          mongoose_domain_api:get_domain_host_type(LServer)} of
        {stream_start, ?NS_STREAM, ?XMPP_VERSION, {ok, HostType}} ->
            S = S0#c2s_data{host_type = HostType, lserver = LServer, jid = From, lang = Lang},
            stream_start_features_before_auth(S);
        {authenticated, ?NS_STREAM, ?XMPP_VERSION, {ok, HostType}} ->
            S = S0#c2s_data{host_type = HostType, lserver = LServer, lang = Lang},
            stream_start_features_after_auth(S);
        {_, ?NS_STREAM, _Pre1_0, {ok, HostType}} ->
            %% (http://xmpp.org/rfcs/rfc6120.html#streams-negotiation-features)
            S = S0#c2s_data{host_type = HostType, lserver = LServer, jid = From, lang = Lang},
            stream_start_error(S, mongoose_xmpp_errors:unsupported_version());
        {_, ?NS_STREAM, _, {error, not_found}} ->
            stream_start_error(S0, mongoose_xmpp_errors:host_unknown());
        {_, _, _, _} ->
            stream_start_error(S0, mongoose_xmpp_errors:invalid_namespace())
    end.

%% We conditionally set the jid field before authentication if it was provided by the client in the
%% stream-start stanza, as extensions might use this (carefully, as it hasn't been proved this is
%% the real identity of the client!). See RFC6120#section-4.7.1
-spec maybe_capture_from_jid_in_stream_start(exml:attrs()) -> undefined | jid:jid().
maybe_capture_from_jid_in_stream_start(#{<<"from">> := From}) ->
    case jid:from_binary(From) of
        error -> undefined;
        Jid -> Jid
    end;
maybe_capture_from_jid_in_stream_start(_) ->
    undefined.

%% As stated in BCP47, 4.4.1:
%% Protocols or specifications that specify limited buffer sizes for
%% language tags MUST allow for language tags of at least 35 characters.
%% Do not store long language tag to avoid possible DoS/flood attacks
-spec get_xml_lang(exml:attrs()) -> <<_:8, _:_*8>>.
get_xml_lang(#{<<"xml:lang">> := Lang})
  when is_binary(Lang), 0 < byte_size(Lang), byte_size(Lang) =< 35 ->
    Lang;
get_xml_lang(_) ->
    ?MYLANG.

-spec handle_maybe_hide_service_name(data(), term()) -> fsm_res().
handle_maybe_hide_service_name(StateData, _Unexpected) ->
    case mongoose_config:get_opt(hide_service_name, false) of
        true ->
            {stop, {shutdown, stream_error}};
        false ->
            send_header(StateData),
            c2s_stream_error(StateData, mongoose_xmpp_errors:xml_not_well_formed())
    end.

-spec handle_starttls(data(), exml:element(), mongoose_acc:t(), retries()) -> fsm_res().
handle_starttls(StateData = #c2s_data{socket = TcpSocket,
                                      parser = Parser,
                                      listener_opts = LOpts = #{tls := _}}, El, SaslAcc, Retries) ->
    send_xml(StateData, mongoose_c2s_stanzas:tls_proceed()), %% send last negotiation chunk via tcp
    case mongoose_xmpp_socket:tcp_to_tls(TcpSocket, LOpts, server) of
        {ok, TlsSocket} ->
            {ok, NewParser} = exml_stream:reset_parser(Parser),
            NewStateData = StateData#c2s_data{socket = TlsSocket,
                                              parser = NewParser,
                                              streamid = new_stream_id()},
            {next_state, {wait_for_stream, stream_start}, NewStateData, state_timeout(LOpts)};
        {error, already_tls_connection} ->
            ErrorStanza = mongoose_xmpp_errors:bad_request(StateData#c2s_data.lang, <<"bad_config">>),
            Err = jlib:make_error_reply(El, ErrorStanza),
            send_element_from_server_jid(StateData, Err),
            maybe_retry_state(StateData, {wait_for_feature_before_auth, SaslAcc, Retries});
        {error, closed} ->
            {stop, {shutdown, tls_closed}};
        {error, timeout} ->
            {stop, {shutdown, tls_timeout}};
        {error, {tls_alert, TlsAlert}} ->
            {stop, {shutdown, {tls_alert, TlsAlert}}}
    end;
handle_starttls(StateData, _El, _SaslAcc, _Retries) ->
    %% As defined in https://datatracker.ietf.org/doc/html/rfc6120#section-5.4.2.2, cause 2
    send_element_from_server_jid(StateData, mongoose_c2s_stanzas:tls_failure()),
    {stop, {shutdown, tls_failure}}.

-spec handle_auth_start(data(), exml:element(), mongoose_acc:t(), retries()) -> fsm_res().
handle_auth_start(StateData, El, SaslAcc, Retries) ->
    Mech = exml_query:attr(El, <<"mechanism">>),
    ClientIn = base64:mime_decode(exml_query:cdata(El)),
    StepResult = mongoose_c2s_sasl:start(StateData, SaslAcc, Mech, ClientIn),
    handle_sasl_step(StateData, StepResult, Retries).

-spec handle_auth_continue(data(), exml:element(), mongoose_acc:t(), retries()) -> fsm_res().
handle_auth_continue(StateData, El, SaslAcc, Retries) ->
    ClientIn = base64:mime_decode(exml_query:cdata(El)),
    StepResult = mongoose_c2s_sasl:continue(StateData, SaslAcc, ClientIn),
    handle_sasl_step(StateData, StepResult, Retries).

-spec handle_sasl_step(data(), mongoose_c2s_sasl:result(), retries()) -> fsm_res().
handle_sasl_step(StateData, {success, NewSaslAcc, Result}, _Retries) ->
    handle_sasl_success(StateData, NewSaslAcc, Result);
handle_sasl_step(StateData, {continue, NewSaslAcc, Result}, Retries) ->
    handle_sasl_continue(StateData, NewSaslAcc, Result, Retries);
handle_sasl_step(StateData, {failure, NewSaslAcc, Result}, Retries) ->
    handle_sasl_failure(StateData, NewSaslAcc, Result, Retries);
handle_sasl_step(StateData, {error, NewSaslAcc, Result}, Retries) ->
    handle_sasl_error(StateData, NewSaslAcc, Result, Retries).

-spec handle_sasl_success(data(), mongoose_acc:t(), mongoose_c2s_sasl:success()) -> fsm_res().
handle_sasl_success(StateData = #c2s_data{jid = MaybeInitialJid, info = Info}, SaslAcc,
                    #{server_out := MaybeServerOut, jid := SaslJid, auth_module := AuthMod}) ->
    case verify_initial_jid(MaybeInitialJid, SaslJid) of
        true ->
            StateData1 = StateData#c2s_data{streamid = new_stream_id(), jid = SaslJid,
                                            info = maps:merge(Info, #{auth_module => AuthMod})},
            El = mongoose_c2s_stanzas:sasl_success_stanza(MaybeServerOut),
            send_acc_from_server_jid(StateData1, SaslAcc, El),
            ?LOG_INFO(#{what => auth_success, text => <<"Accepted SASL authentication">>, c2s_data => StateData1}),
            {next_state, {wait_for_stream, authenticated}, StateData1, state_timeout(StateData1)};
        false ->
            c2s_stream_error(StateData, mongoose_xmpp_errors:invalid_from())
    end.

%% 6.4.6.  SASL Success: https://www.rfc-editor.org/rfc/rfc6120.html#section-6.4.6
-spec verify_initial_jid(undefined | jid:jid(), jid:jid()) -> boolean().
verify_initial_jid(undefined, _Jid) ->
    true;
verify_initial_jid(InitialJid, SaslJid) ->
    jid:are_bare_equal(InitialJid, SaslJid).

-spec handle_sasl_continue(data(), mongoose_acc:t(), mongoose_c2s_sasl:continue(), retries()) -> fsm_res().
handle_sasl_continue(StateData, SaslAcc, #{server_out := ServerOut}, Retries) ->
    El = mongoose_c2s_stanzas:sasl_challenge_stanza(ServerOut),
    NewSaslAcc = send_acc_from_server_jid(StateData, SaslAcc, El),
    {next_state, {wait_for_sasl_response, NewSaslAcc, Retries}, StateData, state_timeout(StateData)}.

-spec handle_sasl_failure(data(), mongoose_acc:t(), mongoose_c2s_sasl:failure(), retries()) -> fsm_res().
handle_sasl_failure(#c2s_data{host_type = HostType, lserver = LServer} = StateData, SaslAcc,
                    #{server_out := ServerOut, maybe_username := Username}, Retries) ->
    ?LOG_INFO(#{what => auth_failed, text => <<"Failed SASL authentication">>,
                username => Username, lserver => LServer, c2s_state => StateData}),
    mongoose_hooks:auth_failed(HostType, LServer, Username),
    mongoose_instrument:execute(c2s_auth_failed, #{host_type => HostType},
                                #{count => 1, server => LServer, username => Username}),
    El = mongoose_c2s_stanzas:sasl_failure_stanza(ServerOut),
    NewSaslAcc = send_acc_from_server_jid(StateData, SaslAcc, El),
    maybe_retry_state(StateData, {wait_for_feature_before_auth, NewSaslAcc, Retries}).

-spec handle_sasl_error(data(), mongoose_acc:t(), mongoose_c2s_sasl:error(), retries()) -> fsm_res().
handle_sasl_error(#c2s_data{lang = Lang} = StateData, _SaslAcc,
                  #{type := Type, text := Text}, _Retries) ->
    Error = mongoose_xmpp_errors:Type(Lang, Text),
    c2s_stream_error(StateData, Error).

-spec handle_sasl_abort(data(), mongoose_acc:t(), retries()) -> fsm_res().
handle_sasl_abort(StateData, SaslAcc, Retries) ->
    Error = #{server_out => <<"aborted">>, maybe_username => undefined},
    handle_sasl_failure(StateData, SaslAcc, Error, Retries).

-spec stream_start_features_before_auth(data()) -> fsm_res().
stream_start_features_before_auth(#c2s_data{sid = SID, listener_opts = LOpts} = StateData) ->
    send_header(StateData),
    SaslAcc0 = mongoose_c2s_sasl:new(StateData),
    SaslAcc1 = mongoose_acc:set_permanent(c2s, [{origin_sid, SID}], SaslAcc0),
    StreamFeatures = mongoose_c2s_stanzas:stream_features_before_auth(StateData),
    SaslAcc2 = send_acc_from_server_jid(StateData, SaslAcc1, StreamFeatures),
    {next_state, {wait_for_feature_before_auth, SaslAcc2, ?AUTH_RETRIES}, StateData, state_timeout(LOpts)}.

-spec stream_start_features_after_auth(data()) -> fsm_res().
stream_start_features_after_auth(#c2s_data{listener_opts = LOpts} = StateData) ->
    send_header(StateData),
    StreamFeatures = mongoose_c2s_stanzas:stream_features_after_auth(StateData),
    send_element_from_server_jid(StateData, StreamFeatures),
    {next_state, {wait_for_feature_after_auth, ?BIND_RETRIES}, StateData, state_timeout(LOpts)}.

-spec handle_bind_resource(data(), state(), exml:element(), jlib:iq()) -> fsm_res().
handle_bind_resource(StateData, C2SState, El, #iq{sub_el = SubEl} = IQ) ->
    Resource = case exml_query:path(SubEl, [{element, <<"resource">>}, cdata]) of
                   undefined -> <<>>;
                   Val -> jid:resourceprep(Val)
               end,
    case Resource of
        error ->
            Err = jlib:make_error_reply(El, mongoose_xmpp_errors:bad_request()),
            send_element_from_server_jid(StateData, Err),
            maybe_retry_state(StateData, C2SState);
        Resource ->
            NewStateData = replace_resource(StateData, Resource),
            NextState = maybe_wait_for_session(NewStateData),
            Jid = get_jid(NewStateData),
            BindResult = mongoose_c2s_stanzas:successful_resource_binding(IQ, Jid),
            MAcc = mongoose_c2s_acc:new(#{c2s_state => NextState, socket_send => [BindResult]}),
            Acc = element_to_origin_accum(NewStateData, El),
            Acc1 = mongoose_acc:set_statem_acc(MAcc, Acc),
            HookParams = hook_arg(NewStateData, C2SState, internal, El, undefined),
            HostType = NewStateData#c2s_data.host_type,
            Return = verify_user(NextState, HostType, HookParams, Acc1),
            Return1 = maybe_open_session(NextState, Return, NewStateData),
            finish_open_session(Return1, NewStateData, C2SState, NextState)
    end.

-spec handle_session_establishment(data(), state(), exml:element(), jlib:iq()) -> fsm_res().
handle_session_establishment(#c2s_data{host_type = HostType, lserver = LServer} = StateData, C2SState, El, IQ) ->
    Acc0 = element_to_origin_accum(StateData, El),
    SessEstablished = mongoose_c2s_stanzas:successful_session_establishment(IQ),
    ServerJid = jid:make_noprep(<<>>, LServer, <<>>),
    ParamsAcc = #{from_jid => ServerJid, to_jid => StateData#c2s_data.jid, element => SessEstablished},
    SessEstablishedAcc = mongoose_acc:update_stanza(ParamsAcc, Acc0),
    MAcc = mongoose_c2s_acc:new(#{c2s_state => session_established, route => [SessEstablishedAcc]}),
    Acc1 = mongoose_acc:set_statem_acc(MAcc, Acc0),
    HookParams = hook_arg(StateData, C2SState, internal, El, undefined),
    {_, Acc2} = mongoose_c2s_hooks:user_send_packet(HostType, Acc1, HookParams),
    {_, Acc3} = mongoose_c2s_hooks:user_send_iq(HostType, Acc2, HookParams),
    Return = verify_user(session_established, HostType, HookParams, Acc3),
    Return1 = maybe_open_session(session_established, Return, StateData),
    finish_open_session(Return1, StateData, C2SState, session_established).

-spec verify_user(state(), mongooseim:host_type(), mongoose_c2s_hooks:params(), mongoose_acc:t()) ->
    {ok | stop, mongoose_acc:t()}.
verify_user(wait_for_session_establishment, _, _, Acc) ->
    {ok, Acc};
verify_user(session_established, HostType, #{c2s_data := StateData} = HookParams, Acc) ->
    case mongoose_c2s_hooks:user_open_session(HostType, Acc, HookParams) of
        {ok, Acc1} ->
            ?LOG_INFO(#{what => c2s_opened_session, c2s_data => StateData}),
            {ok, Acc1};
        {stop, Acc1} ->
            Jid = StateData#c2s_data.jid,
            Acc2 = mongoose_hooks:forbidden_session(HostType, Acc1, Jid),
            ?LOG_INFO(#{what => forbidden_session, text => <<"User not allowed to open session">>,
                        acc => Acc2, c2s_state => StateData}),
            {stop, Acc2}
    end.

%% Note that RFC 3921 said:
%% > Upon establishing a session, a connected resource is said to be an "active resource".
%% But, RFC 6121 says:
%% > [RFC3921] specified one additional
%%  precondition: formal establishment of an instant messaging and
%%  presence session.  Implementation and deployment experience has
%%  shown that this additional step is unnecessary.  However, for
%%  backward compatibility an implementation MAY still offer that
%%  feature.  This enables older software to connect while letting
%%  newer software save a round trip.
%% Note that RFC 3921 said:
%% > If no priority is provided, a server SHOULD consider the priority to be zero.
%% But, RFC 6121 says:
%% > If no priority is provided, the processing server or client MUST consider the priority to be zero.
-spec maybe_open_session(state(), {ok | stop, mongoose_acc:t()}, data()) ->
    {ok, mongoose_acc:t(), data()} | {stop, mongoose_acc:t()}.
maybe_open_session(session_established, {ok, Acc1}, StateData = #c2s_data{host_type = HostType}) ->
    {ReplacedPids, StateData2} = open_session(StateData),
    FsmActions = case ReplacedPids of
                     [] -> [];
                     _ ->
                         Timeout = mongoose_config:get_opt({replaced_wait_timeout, HostType}),
                         [{{timeout, replaced_wait_timeout}, Timeout, ReplacedPids}]
                 end,
    Acc2 = mongoose_c2s_acc:to_acc(Acc1, actions, FsmActions),
    {ok, Acc2, StateData2};
maybe_open_session(wait_for_session_establishment, {ok, Acc}, StateData) ->
    {ok, Acc, StateData};
maybe_open_session(_, {stop, Acc}, _StateData) ->
    {stop, Acc}.

-spec finish_open_session(_, data(), state(), state()) -> fsm_res().
finish_open_session({ok, Acc, StateData}, _, _OldC2SState, NewC2SState) ->
    handle_state_after_packet(StateData, NewC2SState, Acc);
finish_open_session({stop, Acc}, StateData, OldC2SState, _NewC2SState) ->
    El = mongoose_acc:element(Acc),
    Err = jlib:make_error_reply(El, mongoose_xmpp_errors:not_allowed()),
    send_element_from_server_jid(StateData, Err),
    maybe_retry_state(StateData, OldC2SState).

maybe_wait_for_session(#c2s_data{listener_opts = #{backwards_compatible_session := false}}) ->
    session_established;
maybe_wait_for_session(#c2s_data{listener_opts = #{backwards_compatible_session := true}}) ->
    wait_for_session_establishment.

-spec maybe_retry_state(data(), state()) -> fsm_res().
maybe_retry_state(StateData = #c2s_data{listener_opts = LOpts}, C2SState) ->
    case maybe_retry_state(C2SState) of
        {stop, Reason} ->
            {stop, Reason, StateData};
        NextFsmState ->
            {next_state, NextFsmState, StateData, state_timeout(LOpts)}
    end.

-spec handle_cast(data(), state(), term()) -> fsm_res().
handle_cast(StateData, _C2SState, {exit, {replaced, _} = Reason}) ->
    ReasonText = <<"Replaced by new connection">>,
    StreamConflict = mongoose_xmpp_errors:stream_conflict(StateData#c2s_data.lang, ReasonText),
    send_element_from_server_jid(StateData, StreamConflict),
    send_trailer(StateData),
    {stop, {shutdown, Reason}};
handle_cast(StateData, _C2SState, {exit, Reason}) when is_binary(Reason) ->
    StreamConflict = mongoose_xmpp_errors:stream_conflict(StateData#c2s_data.lang, Reason),
    send_element_from_server_jid(StateData, StreamConflict),
    send_trailer(StateData),
    {stop, {shutdown, Reason}};
handle_cast(StateData, _C2SState, {exit, system_shutdown}) ->
    Error = mongoose_xmpp_errors:system_shutdown(),
    send_element_from_server_jid(StateData, Error),
    send_trailer(StateData),
    {stop, {shutdown, system_shutdown}};
handle_cast(StateData, C2SState, {stop, Reason}) ->
    handle_stop_request(StateData, C2SState, Reason);
handle_cast(_StateData, _C2SState, {async, Fun, Args}) ->
    apply(Fun, Args),
    keep_state_and_data;

handle_cast(StateData, _C2SState, {async_with_state, Fun, Args}) ->
    StateData2 = apply(Fun, [StateData | Args]),
    {keep_state, StateData2};
handle_cast(StateData, C2SState, Event) ->
    handle_foreign_event(StateData, C2SState, cast, Event).

-spec handle_info(data(), state(), term()) -> fsm_res().
handle_info(StateData, C2SState, {route, Acc}) ->
    handle_route(StateData, C2SState, Acc);
handle_info(StateData, C2SState, #xmlel{} = El) ->
    handle_c2s_packet(StateData, C2SState, El);
handle_info(StateData, _C2SState, {TcpOrSSl, _Socket, _Packet} = SocketData)
  when TcpOrSSl =:= tcp; TcpOrSSl =:= ssl ->
    handle_socket_data(StateData, SocketData);
handle_info(StateData, C2SState, {Closed, _Socket} = SocketData)
  when Closed =:= tcp_closed; Closed =:= ssl_closed; Closed =:= websockets_closed ->
    handle_socket_closed(StateData, C2SState, SocketData);
handle_info(StateData, C2SState, {Error, _Socket} = SocketData)
  when Error =:= tcp_error; Error =:= ssl_error; Error =:= websockets_error ->
    handle_socket_error(StateData, C2SState, SocketData);
handle_info(StateData, C2SState, Info) ->
    handle_foreign_event(StateData, C2SState, info, Info).

-spec handle_timeout(data(), state(), atom(), term()) -> fsm_res().
handle_timeout(StateData, _C2SState, activate_socket, activate_socket) ->
    activate_socket(StateData),
    keep_state_and_data;
handle_timeout(StateData, C2SState, replaced_wait_timeout, ReplacedPids) ->
    [ verify_process_alive(StateData, C2SState, Pid) || Pid <- ReplacedPids ],
    keep_state_and_data;
handle_timeout(StateData, C2SState, Name, Handler) when is_atom(Name), is_function(Handler, 2) ->
    C2sAcc = Handler(Name, StateData),
    handle_state_result(StateData, C2SState, undefined, C2sAcc);
handle_timeout(StateData, _C2SState, state_timeout, state_timeout_termination) ->
    StreamConflict = mongoose_xmpp_errors:connection_timeout(),
    send_element_from_server_jid(StateData, StreamConflict),
    send_trailer(StateData),
    {stop, {shutdown, state_timeout}};
handle_timeout(StateData, C2SState, Name, Payload) ->
    handle_foreign_event(StateData, C2SState, {timeout, Name}, Payload).

verify_process_alive(StateData, C2SState, Pid) ->
    IsAlive = case node(Pid) =:= node() of
                  true -> erlang:is_process_alive(Pid);
                  false -> rpc:call(node(Pid), erlang, is_process_alive, [Pid])
              end,
    case IsAlive of
        false -> ok;
        true ->
            ?LOG_WARNING(#{what => c2s_replaced_wait_timeout,
                           text => <<"Some processes are not responding when handling replace messages">>,
                           replaced_pid => Pid, state_name => C2SState, c2s_data => StateData})
    end.

-spec maybe_retry_state(state()) -> state() | {stop, term()}.
maybe_retry_state(connect) -> connect;
maybe_retry_state(wait_for_session_establishment) -> {stop, {shutdown, stream_end}};
maybe_retry_state(session_established) -> session_established;
maybe_retry_state({wait_for_stream, StreamState}) ->
    {wait_for_stream, StreamState};
maybe_retry_state({wait_for_feature_after_auth, 0}) ->
    {stop, {shutdown, retries}};
maybe_retry_state({wait_for_feature_before_auth, _, 0}) ->
    {stop, {shutdown, retries}};
maybe_retry_state({wait_for_sasl_response, _, 0}) ->
    {stop, {shutdown, retries}};
maybe_retry_state({wait_for_feature_after_auth, Retries}) ->
    {wait_for_feature_after_auth, Retries - 1};
maybe_retry_state({wait_for_feature_before_auth, SaslAcc, Retries}) ->
    {wait_for_feature_before_auth, SaslAcc, Retries - 1};
maybe_retry_state({wait_for_sasl_response, SaslAcc, Retries}) ->
    {wait_for_sasl_response, SaslAcc, Retries - 1};
maybe_retry_state(?EXT_C2S_STATE(_) = State) ->
    State.

%% @doc Check 'from' attribute in stanza RFC 6120 Section 8.1.2.1
-spec verify_from(exml:element(), jid:jid()) -> boolean().
verify_from(El, StateJid) ->
    case exml_query:attr(El, <<"from">>) of
        undefined -> true;
        GJid ->
            case jid:from_binary(GJid) of
                error ->
                    false;
                #jid{lresource = <<>>} = GivenJid ->
                    jid:are_bare_equal(GivenJid, StateJid);
                #jid{} = GivenJid ->
                    jid:are_equal(GivenJid, StateJid)
            end
    end.

verify_to(El) ->
    case exml_query:attr(El, <<"to">>) of
        undefined ->
            true;
        Jid ->
            case jid:from_binary(Jid) of
                error -> false;
                _ -> true
            end
    end.

-spec verify_ip_is_not_blacklisted(mongoose_xmpp_socket:socket()) -> ok | no_return().
verify_ip_is_not_blacklisted(Socket) ->
    {PeerIp, _} = mongoose_xmpp_socket:get_ip(Socket),
    case mongoose_hooks:check_bl_c2s(PeerIp) of
        true ->
            ?LOG_INFO(#{what => c2s_blacklisted_ip, ip => PeerIp,
                        text => <<"Connection attempt from blacklisted IP">>}),
            throw({stop, {shutdown, ip_blacklisted}});
        false ->
            ok
    end.

-spec handle_foreign_packet(data(), state(), exml:element()) -> fsm_res().
handle_foreign_packet(StateData = #c2s_data{host_type = HostType, lserver = LServer}, C2SState, El) ->
    ?LOG_DEBUG(#{what => packet_before_session_established_sent, packet => El, c2s_pid => self()}),
    ServerJid = jid:make_noprep(<<>>, LServer, <<>>),
    AccParams = #{host_type => HostType, lserver => LServer, location => ?LOCATION,
                  element => El, from_jid => ServerJid, to_jid => ServerJid},
    Acc0 = mongoose_acc:new(AccParams),
    HookParams = hook_arg(StateData, C2SState, internal, El, undefined),
    {_, Acc1} = mongoose_c2s_hooks:user_send_xmlel(HostType, Acc0, HookParams),
    handle_state_after_packet(StateData, C2SState, Acc1).

-spec handle_c2s_packet(data(), state(), exml:element()) -> fsm_res().
handle_c2s_packet(StateData = #c2s_data{host_type = HostType}, C2SState, El) ->
    HookParams = hook_arg(StateData, C2SState, internal, El, undefined),
    Acc = element_to_origin_accum(StateData, El),
    case mongoose_c2s_hooks:user_send_packet(HostType, Acc, HookParams) of
        {ok, Acc1} ->
            Acc2 = handle_stanza_from_client(StateData, HookParams, Acc1, mongoose_acc:stanza_name(Acc1)),
            handle_state_after_packet(StateData, C2SState, Acc2);
        {stop, Acc1} ->
            handle_state_after_packet(StateData, C2SState, Acc1)
    end.

%% @doc Process packets sent by the user (coming from user on c2s XMPP connection)
-spec handle_stanza_from_client(data(), mongoose_c2s_hooks:params(), mongoose_acc:t(), binary()) ->
    mongoose_acc:t().
handle_stanza_from_client(#c2s_data{host_type = HostType}, HookParams, Acc, <<"message">>) ->
    TS0 = mongoose_acc:timestamp(Acc),
    Acc1 = mongoose_c2s_hooks:user_send_message(HostType, Acc, HookParams),
    Acc2 = maybe_route(Acc1),
    TS1 = erlang:system_time(microsecond),
    mongoose_instrument:execute(c2s_message_processed, #{host_type => HostType}, #{time => (TS1 - TS0)}),
    Acc2;
handle_stanza_from_client(#c2s_data{host_type = HostType}, HookParams, Acc, <<"iq">>) ->
    Acc1 = mongoose_c2s_hooks:user_send_iq(HostType, Acc, HookParams),
    maybe_route(Acc1);
handle_stanza_from_client(#c2s_data{host_type = HostType}, HookParams, Acc, <<"presence">>) ->
    {_, Acc1} = mongoose_c2s_hooks:user_send_presence(HostType, Acc, HookParams),
    Acc1;
handle_stanza_from_client(#c2s_data{host_type = HostType}, HookParams, Acc, _) ->
    {_, Acc1} = mongoose_c2s_hooks:user_send_xmlel(HostType, Acc, HookParams),
    Acc1.

-spec maybe_route(gen_hook:hook_fn_ret(mongoose_acc:t())) -> mongoose_acc:t().
maybe_route({ok, Acc}) ->
    {FromJid, ToJid, El} = mongoose_acc:packet(Acc),
    ejabberd_router:route(FromJid, ToJid, Acc, El),
    Acc;
maybe_route({stop, Acc}) ->
    Acc.

-spec handle_route(data(), state(), mongoose_acc:t()) -> fsm_res().
handle_route(StateData = #c2s_data{host_type = HostType}, C2SState, Acc) ->
    {From, To, El} = mongoose_acc:packet(Acc),
    FinalEl = jlib:replace_from_to(From, To, El),
    ParamsAcc = #{from_jid => From, to_jid => To, element => FinalEl},
    Acc1 = mongoose_acc:update_stanza(ParamsAcc, Acc),
    HookParams = hook_arg(StateData, C2SState, info, El, route),
    Res = mongoose_c2s_hooks:user_receive_packet(HostType, Acc1, HookParams),
    handle_route_packet(StateData, C2SState, HookParams, Res).

-spec handle_route_packet(data(), state(), mongoose_c2s_hooks:params(), mongoose_c2s_hooks:result()) -> fsm_res().
handle_route_packet(StateData, C2SState, HookParams, {ok, Acc}) ->
    StanzaName = mongoose_acc:stanza_name(Acc),
    case process_stanza_to_client(StateData, HookParams, Acc, StanzaName) of
        {ok, Acc3} ->
            handle_flush(StateData, C2SState, Acc3);
        {stop, Acc3} ->
            handle_state_after_packet(StateData, C2SState, Acc3)
    end;
handle_route_packet(StateData, C2SState, _, {stop, Acc}) ->
    handle_state_after_packet(StateData, C2SState, Acc).

-spec handle_flush(data(), state(), mongoose_acc:t()) -> fsm_res().
handle_flush(StateData = #c2s_data{host_type = HostType}, C2SState, Acc) ->
    HookParams = hook_arg(StateData, C2SState, info, Acc, flush),
    Res = mongoose_c2s_hooks:xmpp_presend_element(HostType, Acc, HookParams),
    Acc1 = maybe_send_element(StateData, Res),
    handle_state_after_packet(StateData, C2SState, Acc1).

-spec maybe_send_element(data(), mongoose_c2s_hooks:result()) -> mongoose_acc:t().
maybe_send_element(StateData, {ok, Acc}) ->
    send(StateData, Acc);
maybe_send_element(_, {stop, Acc}) ->
    Acc.

%% @doc Process packets sent to the user (coming to user on c2s XMPP connection)
-spec process_stanza_to_client(data(), mongoose_c2s_hooks:params(), mongoose_acc:t(), binary()) ->
    mongoose_c2s_hooks:result().
process_stanza_to_client(#c2s_data{host_type = HostType}, Params, Acc, <<"message">>) ->
    mongoose_c2s_hooks:user_receive_message(HostType, Acc, Params);
process_stanza_to_client(#c2s_data{host_type = HostType}, Params, Acc, <<"iq">>) ->
    mongoose_c2s_hooks:user_receive_iq(HostType, Acc, Params);
process_stanza_to_client(#c2s_data{host_type = HostType}, Params, Acc, <<"presence">>) ->
    mongoose_c2s_hooks:user_receive_presence(HostType, Acc, Params);
process_stanza_to_client(#c2s_data{host_type = HostType}, Params, Acc, _) ->
    mongoose_c2s_hooks:user_receive_xmlel(HostType, Acc, Params).

-spec handle_state_after_packet(data(), state(), mongoose_acc:t()) -> fsm_res().
handle_state_after_packet(StateData, C2SState, Acc) ->
    handle_state_result(StateData, C2SState, Acc, mongoose_c2s_acc:get_statem_result(Acc)).

-spec handle_state_result(data(),
                          state(),
                          undefined | mongoose_acc:t(),
                          mongoose_c2s_acc:t()) -> fsm_res().
handle_state_result(StateData0, _, _, #{c2s_data := MaybeNewFsmData, hard_stop := Reason})
  when Reason =/= undefined ->
    StateData1 = case MaybeNewFsmData of
                     undefined -> StateData0;
                     _ -> MaybeNewFsmData
                 end,
    {stop, {shutdown, Reason}, StateData1};
handle_state_result(StateData0, C2SState, MaybeAcc,
                    #{state_mod := ModuleStates, actions := MaybeActions,
                      c2s_state := MaybeNewFsmState, c2s_data := MaybeNewFsmData,
                      socket_send := MaybeSocketSend}) ->
    NextFsmState = case MaybeNewFsmState of
                       undefined -> C2SState;
                       _ -> MaybeNewFsmState
                   end,
    StateData1 = case MaybeNewFsmData of
                     undefined -> StateData0;
                     _ -> MaybeNewFsmData
                 end,
    StateData2 = case map_size(ModuleStates) of
                     0 -> StateData1;
                     _ -> merge_mod_state(StateData1, ModuleStates)
                 end,
    [maybe_send_xml(StateData2, MaybeAcc, Send) || Send <- MaybeSocketSend ],
    {next_state, NextFsmState, StateData2, MaybeActions}.

%% @doc This function is executed when c2s receives a stanza from the TCP connection.
-spec element_to_origin_accum(data(), exml:element()) -> mongoose_acc:t().
element_to_origin_accum(StateData = #c2s_data{sid = SID, jid = Jid}, El) ->
    BaseParams = #{host_type => StateData#c2s_data.host_type,
                   lserver => StateData#c2s_data.lserver,
                   location => ?LOCATION,
                   element => El,
                   from_jid => Jid},
    Params = case exml_query:attr(El, <<"to">>) of
                 undefined -> BaseParams#{ to_jid => jid:to_bare(Jid) };
                 _ToBin -> BaseParams
             end,
    Acc = mongoose_acc:new(Params),
    mongoose_acc:set_permanent(c2s, [{module, ?MODULE}, {origin_sid, SID}, {origin_jid, Jid}], Acc).

-spec stream_start_error(data(), exml:element()) -> fsm_res().
stream_start_error(StateData, Error) ->
    send_header(StateData),
    c2s_stream_error(StateData, Error).

-spec send_header(StateData :: data()) -> any().
send_header(StateData) ->
    Header = mongoose_c2s_stanzas:stream_header(StateData),
    send_xml(StateData, Header).

send_trailer(StateData) ->
    send_xml(StateData, ?XML_STREAM_TRAILER).

-spec c2s_stream_error(data(), exml:element()) -> fsm_res().
c2s_stream_error(StateData, Error) ->
    ?LOG_DEBUG(#{what => c2s_stream_error, xml_error => Error, c2s_data => StateData}),
    send_element_from_server_jid(StateData, Error),
    send_xml(StateData, ?XML_STREAM_TRAILER),
    {stop, {shutdown, stream_error}, StateData}.

-spec bounce_messages(data()) -> ok.
bounce_messages(StateData) ->
    receive
        {route, Acc} ->
            reroute_one(StateData, Acc),
            bounce_messages(StateData);
        _ ->
            bounce_messages(StateData)
    after 0 -> ok
    end.

-spec reroute_one(data(), mongoose_acc:t()) -> mongoose_acc:t().
reroute_one(#c2s_data{sid = Sid}, Acc) ->
    {From, To, _El} = mongoose_acc:packet(Acc),
    Acc2 = patch_acc_for_reroute(Acc, Sid),
    ejabberd_router:route(From, To, Acc2).

-spec reroute_buffer(data(), [mongoose_acc:t()]) -> term().
reroute_buffer(StateData = #c2s_data{host_type = HostType, jid = Jid}, Buffer) ->
    OrderedBuffer = lists:reverse(Buffer),
    FilteredBuffer = mongoose_hooks:filter_unacknowledged_messages(HostType, Jid, OrderedBuffer),
    [reroute_one(StateData, BufferedAcc) || BufferedAcc <- FilteredBuffer].

-spec reroute_buffer_to_pid(data(), pid(), [mongoose_acc:t()]) -> term().
reroute_buffer_to_pid(StateData = #c2s_data{host_type = HostType, jid = Jid}, Pid, Buffer) ->
    OrderedBuffer = lists:reverse(Buffer),
    FilteredBuffer = mongoose_hooks:filter_unacknowledged_messages(HostType, Jid, OrderedBuffer),
    [reroute_one_to_pid(StateData, Pid, BufferedAcc) || BufferedAcc <- FilteredBuffer].

-spec reroute_one_to_pid(data(), pid(), mongoose_acc:t()) -> {route, mongoose_acc:t()}.
reroute_one_to_pid(#c2s_data{sid = Sid}, Pid, Acc) ->
    Acc2 = patch_acc_for_reroute(Acc, Sid),
    route(Pid, Acc2).

-spec route(pid(), mongoose_acc:t()) -> {route, mongoose_acc:t()}.
route(Pid, Acc) ->
    Pid ! {route, Acc}.

-spec open_session(data()) -> {[pid()], data()}.
open_session(
  StateData = #c2s_data{host_type = HostType, sid = Sid, jid = Jid,
                        socket = Socket, info = Info}) ->
    NewFields = #{ip => mongoose_xmpp_socket:get_ip(Socket),
                  conn => mongoose_xmpp_socket:get_conn_type(Socket)},
    Info2 = maps:merge(Info, NewFields),
    ReplacedPids = ejabberd_sm:open_session(HostType, Sid, Jid, 0, Info2),
    {ReplacedPids, StateData#c2s_data{info = Info2}}.

-spec close_session(data(), mongoose_acc:t(), term()) -> mongoose_acc:t().
close_session(#c2s_data{sid = Sid, jid = Jid, info = Info}, Acc, Reason) ->
    ejabberd_sm:close_session(Acc, Sid, Jid, sm_unset_reason(Reason), Info).

-spec patch_acc_for_reroute(mongoose_acc:t(), ejabberd_sm:sid()) -> mongoose_acc:t().
patch_acc_for_reroute(Acc, Sid) ->
    case mongoose_acc:stanza_name(Acc) of
        <<"message">> ->
            Acc;
        _ -> %% IQs and presences are allowed to come to the same Sid only
            case mongoose_acc:get(c2s, receiver_sid, undefined, Acc) of
                undefined ->
                    mongoose_acc:set_permanent(c2s, receiver_sid, Sid, Acc);
                _ ->
                    Acc
            end
    end.

-spec close_parser(data()) -> ok.
close_parser(#c2s_data{parser = undefined}) -> ok;
close_parser(#c2s_data{parser = Parser}) -> exml_stream:free_parser(Parser).

-spec do_close_session(data(), state(), mongoose_acc:t(), term()) -> mongoose_acc:t().
do_close_session(C2SData, session_established, Acc, Reason) ->
    close_session(C2SData, Acc, Reason);
do_close_session(C2SData, ?EXT_C2S_STATE(_), Acc, Reason) ->
    close_session(C2SData, Acc, Reason);
do_close_session(_, _, Acc, _) ->
    Acc.

sm_unset_reason({shutdown, Reason}) ->
    Reason;
sm_unset_reason(normal) ->
    normal;
sm_unset_reason(_) ->
    error.

%% @doc These are the termination points - from here stanza is sent to the user
-spec send(data(), mongoose_acc:t()) -> mongoose_acc:t().
send(StateData, Acc) ->
    El = mongoose_acc:element(Acc),
    do_send_element(StateData, Acc, El).

-spec send_element_from_server_jid(data(), exml:element()) -> any().
send_element_from_server_jid(StateData, El) ->
    Acc = mongoose_acc:new(
            #{host_type => StateData#c2s_data.host_type,
              lserver => StateData#c2s_data.lserver,
              location => ?LOCATION,
              from_jid => jid:make_noprep(<<>>, StateData#c2s_data.lserver, <<>>),
              to_jid => StateData#c2s_data.jid,
              element => El}),
    do_send_element(StateData, Acc, El).

-spec send_acc_from_server_jid(data(), mongoose_acc:t(), exml:element()) -> mongoose_acc:t().
send_acc_from_server_jid(StateData = #c2s_data{lserver = LServer, jid = Jid}, Acc0, El) ->
    ServerJid = jid:make_noprep(<<>>, LServer, <<>>),
    ParamsAcc = #{from_jid => ServerJid, to_jid => Jid, element => El},
    Acc1 = mongoose_acc:update_stanza(ParamsAcc, Acc0),
    do_send_element(StateData, Acc1, El).

-spec maybe_send_xml(data(), mongoose_acc:t(), exml:element()) -> mongoose_acc:t().
maybe_send_xml(StateData = #c2s_data{host_type = HostType, lserver = LServer}, undefined, ToSend) ->
    Acc = mongoose_acc:new(#{host_type => HostType, lserver => LServer, location => ?LOCATION}),
    do_send_element(StateData, Acc, ToSend);
maybe_send_xml(StateData, Acc, ToSend) ->
    do_send_element(StateData, Acc, ToSend).

-spec do_send_element(data(), mongoose_acc:t(), exml:element()) -> mongoose_acc:t().
do_send_element(StateData = #c2s_data{host_type = undefined}, Acc, El) ->
    send_xml(StateData, El),
    Acc;
do_send_element(StateData = #c2s_data{host_type = HostType}, Acc, #xmlel{} = El) ->
    Res = send_xml(StateData, El),
    Acc1 = mongoose_acc:set(c2s, send_result, Res, Acc),
    mongoose_hooks:xmpp_send_element(HostType, Acc1, El).

-spec send_xml(data(), exml_stream:element() | [exml_stream:element()]) -> maybe_ok().
send_xml(Data, XmlElement) when is_tuple(XmlElement) ->
    send_xml(Data, [XmlElement]);
send_xml(#c2s_data{socket = Socket} = StateData, XmlElements) when is_list(XmlElements) ->
    [execute_element_event(Element, StateData, xmpp_element_out)
     || Element = #xmlel{} <- XmlElements],
    mongoose_xmpp_socket:send_xml(Socket, XmlElements).

state_timeout(#c2s_data{listener_opts = LOpts}) ->
    state_timeout(LOpts);
state_timeout(#{state_timeout := Timeout}) ->
    {state_timeout, Timeout, state_timeout_termination}.

-spec replace_resource(data(), binary()) -> data().
replace_resource(StateData, <<>>) ->
    replace_resource(StateData, generate_random_resource());
replace_resource(#c2s_data{jid = OldJid} = StateData, NewResource) ->
    StateData#c2s_data{jid = jid:replace_resource_noprep(OldJid, NewResource)}.

-spec new_stream_id() -> binary().
new_stream_id() ->
    mongoose_bin:gen_from_crypto().

-spec generate_random_resource() -> jid:lresource().
generate_random_resource() ->
    <<(mongoose_bin:gen_from_timestamp())/binary, "-", (mongoose_bin:gen_from_crypto())/binary>>.

-spec hook_arg(data(), state(), terminate | gen_statem:event_type(), term(), term()) ->
    mongoose_c2s_hooks:params().
hook_arg(StateData, C2SState, EventType, #{event_tag := EventTag,
                                           event_content := EventContent}, Reason) ->
    #{c2s_data => StateData, c2s_state => C2SState,
      event_type => EventType, event_tag => EventTag, event_content => EventContent,
      reason => Reason};
hook_arg(StateData, C2SState, EventType, EventContent, Reason) ->
    #{c2s_data => StateData, c2s_state => C2SState,
      event_type => EventType, event_content => EventContent,
      reason => Reason}.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start(mongoose_listener:init_args(), [gen_statem:start_opt()]) ->
    supervisor:startchild_ret().
start(Params, ProcOpts) ->
    supervisor:start_child(mongoose_c2s_sup, [Params, ProcOpts]).

-spec start_link(mongoose_listener:init_args(), [gen_statem:start_opt()]) ->
    gen_statem:start_ret().
start_link(Params, ProcOpts) ->
    gen_statem:start_link(?MODULE, Params, ProcOpts).

-spec stop(gen_statem:server_ref(), atom()) -> ok.
stop(Pid, Reason) ->
    gen_statem:cast(Pid, {stop, Reason}).

-spec exit(pid(), binary() | atom() | {replaced, pid()}) -> ok.
exit(Pid, Reason) ->
    gen_statem:cast(Pid, {exit, Reason}).

-spec async(pid(), fun(), [term()]) -> ok.
async(Pid, Fun, Args) ->
    gen_statem:cast(Pid, {async, Fun, Args}).

-spec async_with_state(pid(), fun(), [term()]) -> ok.
async_with_state(Pid, Fun, Args) ->
    gen_statem:cast(Pid, {async_with_state, Fun, Args}).

-spec call(pid(), atom(), term()) -> term().
call(Pid, EventTag, EventContent) ->
    gen_statem:call(Pid, #{event_tag => EventTag, event_content => EventContent}, 5000).

-spec cast(pid(), atom(), term()) -> ok.
cast(Pid, EventTag, EventContent) ->
    gen_statem:cast(Pid, #{event_tag => EventTag, event_content => EventContent}).

-spec create_data(#{host_type := mongooseim:host_type(), jid := jid:jid()}) -> data().
create_data(#{host_type := HostType, jid := Jid}) ->
    #c2s_data{host_type = HostType, jid = Jid}.

-spec get_auth_mechs(data()) -> [mongoose_c2s_sasl:mechanism()].
get_auth_mechs(#c2s_data{host_type = HostType} = StateData) ->
    [M || M <- cyrsasl:listmech(HostType), filter_mechanism(StateData, M)].

%% Mechanisms without XEP-0484 token mechanisms
%% (HT mechanisms are announced as inlined instead)
-spec get_auth_mechs_to_announce(data()) -> [mongoose_c2s_sasl:mechanism()].
get_auth_mechs_to_announce(StateData) ->
    [M || M <- get_auth_mechs(StateData), not skip_announce_mechanism(M)].

-spec skip_announce_mechanism(binary()) -> boolean().
skip_announce_mechanism(Mech) ->
    mod_fast_auth_token_generic_mech:skip_announce_mechanism(Mech).

-spec filter_mechanism(data(), binary()) -> boolean().
filter_mechanism(#c2s_data{socket = Socket}, <<"SCRAM-SHA-1-PLUS">>) ->
    mongoose_xmpp_socket:is_channel_binding_supported(Socket);
filter_mechanism(#c2s_data{socket = Socket}, <<"SCRAM-SHA-", _N:3/binary, "-PLUS">>) ->
    mongoose_xmpp_socket:is_channel_binding_supported(Socket);
filter_mechanism(#c2s_data{socket = Socket, listener_opts = LOpts}, <<"EXTERNAL">>) ->
    mongoose_xmpp_socket:has_peer_cert(Socket, LOpts);
filter_mechanism(_, _) ->
    true.

-spec get_host_type(data()) -> mongooseim:host_type().
get_host_type(#c2s_data{host_type = HostType}) ->
    HostType.

-spec get_lserver(data()) -> jid:lserver().
get_lserver(#c2s_data{lserver = LServer}) ->
    LServer.

-spec get_sid(data()) -> ejabberd_sm:sid().
get_sid(#c2s_data{sid = Sid}) ->
    Sid.

-spec get_ip(data()) -> mongoose_transport:peer().
get_ip(#c2s_data{socket = Socket}) ->
    mongoose_xmpp_socket:get_ip(Socket).

-spec get_socket(data()) -> mongoose_xmpp_socket:socket() | undefined.
get_socket(#c2s_data{socket = Socket}) ->
    Socket.

-spec get_jid(data()) -> jid:jid() | undefined.
get_jid(#c2s_data{jid = Jid}) ->
    Jid.

-spec set_jid(data(), jid:jid()) -> data().
set_jid(StateData, NewJid) ->
    StateData#c2s_data{jid = NewJid}.

-spec set_auth_module(data(), module()) -> data().
set_auth_module(StateData = #c2s_data{info = Info}, AuthModule) ->
    StateData#c2s_data{info = maps:merge(Info, #{auth_module => AuthModule})}.

-spec get_info(data()) -> info().
get_info(#c2s_data{info = Info}) ->
    Info.

-spec set_info(data(), info()) -> data().
set_info(StateData, Info) ->
    StateData#c2s_data{info = Info}.

-spec get_lang(data()) -> ejabberd:lang().
get_lang(#c2s_data{lang = Lang}) ->
    Lang.

-spec get_stream_id(data()) -> binary().
get_stream_id(#c2s_data{streamid = StreamId}) ->
    StreamId.

-spec get_mod_state(data(), module()) -> {ok, term()} | {error, not_found}.
get_mod_state(#c2s_data{state_mod = ModStates}, ModName) ->
    case maps:get(ModName, ModStates, undefined) of
        undefined -> {error, not_found};
        ModState -> {ok, ModState}
    end.

-spec get_listener_opts(data()) -> mongoose_listener:options().
get_listener_opts(#c2s_data{listener_opts = ListenerOpts}) ->
    ListenerOpts.

-spec merge_mod_state(data(), #{module() => term()}) -> data().
merge_mod_state(StateData = #c2s_data{state_mod = ModStates}, MoreModStates) ->
    StateData#c2s_data{state_mod = maps:merge(ModStates, MoreModStates)}.

-spec remove_mod_state(data(), module()) -> data().
remove_mod_state(StateData = #c2s_data{state_mod = ModStates}, ModName) ->
    StateData#c2s_data{state_mod = maps:remove(ModName, ModStates)}.

-spec merge_states(data(), data()) -> data().
merge_states(S0 = #c2s_data{}, S1 = #c2s_data{}) ->
    S1#c2s_data{
      host_type = S0#c2s_data.host_type,
      lserver = S0#c2s_data.lserver,
      jid = S0#c2s_data.jid,
      state_mod = S0#c2s_data.state_mod,
      info = S0#c2s_data.info
     }.

%% Instrumentation helpers

-spec execute_element_event(exml:element(), data(), mongoose_instrument:event_name()) -> ok.
execute_element_event(Element, #c2s_data{host_type = HostType, jid = Jid}, EventName) ->
    mongoose_instrument_xmpp:execute_element_event(EventName, c2s, HostType, Element,
                                                   #{jid => Jid}).
