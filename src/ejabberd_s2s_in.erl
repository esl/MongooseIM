%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s_in.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Serve incoming s2s connection
%%% Created :  6 Dec 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_s2s_in).
-author('alexey@process-one.net').
-behaviour(gen_fsm_compat).
-behaviour(mongoose_listener).

%% mongoose_listener API
-export([start_listener/1]).

%% External exports
-export([start/2,
         start_link/2,
         send_validity_from_s2s_out/3,
         match_domain/2]).

%% gen_fsm callbacks
-export([init/1,
         wait_for_stream/2,
         wait_for_feature_request/2,
         stream_established/2,
         handle_event/3,
         handle_sync_event/4,
         code_change/4,
         handle_info/3,
         terminate/3]).

-export_type([connection_info/0]).

-ignore_xref([match_domain/2, start/2, start_link/2, stream_established/2,
              wait_for_feature_request/2, wait_for_stream/2]).

-include("mongoose.hrl").
-include("jlib.hrl").

-record(state, {socket                  :: mongoose_transport:socket_data(),
                streamid                :: ejabberd_s2s:stream_id(),
                shaper                  :: mongoose_shaper:shaper(),
                tls = false             :: boolean(),
                tls_enabled = false     :: boolean(),
                tls_required = false    :: boolean(),
                tls_cert_verify = false :: boolean(),
                tls_options             :: mongoose_tls:options(),
                server                  :: jid:lserver() | undefined,
                host_type               :: mongooseim:host_type() | undefined,
                authenticated = false   :: boolean(),
                auth_domain             :: jid:lserver() | undefined,
                connections = #{}       :: map(),
                timer                   :: reference()
              }).
-type state() :: #state{}.

-type connection_info() ::
        #{pid => pid(),
          direction => in,
          statename => statename(),
          addr => inet:ip_address(),
          port => inet:port_number(),
          streamid => ejabberd_s2s:stream_id(),
          tls => boolean(),
          tls_enabled => boolean(),
          tls_options => mongoose_tls:options(),
          authenticated => boolean(),
          shaper => mongoose_shaper:shaper(),
          domains => [jid:lserver()]}.

-type statename() :: 'stream_established' | 'wait_for_feature_request'.
%% FSM handler return value
-type fsm_return() :: {'stop', Reason :: 'normal', state()}
                    | {'next_state', statename(), state()}
                    | {'next_state', statename(), state(), Timeout :: integer()}.
%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-define(STREAM_HEADER(Version),
        (<<"<?xml version='1.0'?>"
         "<stream:stream "
         "xmlns:stream='http://etherx.jabber.org/streams' "
         "xmlns='jabber:server' "
         "xmlns:db='jabber:server:dialback' "
         "id='", (StateData#state.streamid)/binary, "'", Version/binary, ">">>)
       ).

-type socket() :: term().
-type options() :: #{shaper := atom(), tls := mongoose_tls:options(), atom() => any()}.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-spec start(socket(), options()) ->
          {error, _} | {ok, undefined | pid()} | {ok, undefined | pid(), _}.
start(Socket, Opts) ->
    supervisor:start_child(ejabberd_s2s_in_sup, [Socket, Opts]).

-spec start_link(socket(), options()) -> ignore | {error, _} | {ok, pid()}.
start_link(Socket, Opts) ->
    gen_fsm_compat:start_link(ejabberd_s2s_in, [Socket, Opts], ?FSMOPTS).

-spec start_listener(options()) -> ok.
start_listener(Opts) ->
    mongoose_tcp_listener:start_listener(Opts).

-spec send_validity_from_s2s_out(pid(), boolean(), ejabberd_s2s:fromto()) -> ok.
send_validity_from_s2s_out(Pid, IsValid, FromTo) when is_boolean(IsValid) ->
    Event = {validity_from_s2s_out, IsValid, FromTo},
    p1_fsm:send_event(Pid, Event).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%%----------------------------------------------------------------------
-spec init([socket() | options(), ...]) -> {ok, wait_for_stream, state()}.
init([Socket, #{shaper := Shaper, tls := TLSOpts}]) ->
    ?LOG_DEBUG(#{what => s2s_in_started,
                 text => <<"New incoming S2S connection">>,
                 socket => Socket}),
    Timer = erlang:start_timer(mongoose_s2s_lib:timeout(), self(), []),
    {ok, wait_for_stream,
     #state{socket = Socket,
            streamid = new_id(),
            shaper = Shaper,
            tls_enabled = false,
            tls_options = TLSOpts,
            timer = Timer}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------

-spec wait_for_stream(ejabberd:xml_stream_item(), state()) -> fsm_return().
wait_for_stream({xmlstreamstart, _Name, Attrs} = Event, StateData) ->
    case maps:from_list(Attrs) of
        AttrMap = #{<<"xmlns">> := <<"jabber:server">>, <<"to">> := Server} ->
            case StateData#state.server of
                undefined ->
                    case mongoose_domain_api:get_host_type(Server) of
                        {error, not_found} ->
                            Info = #{location => ?LOCATION, last_event => Event},
                            stream_start_error(StateData, Info, mongoose_xmpp_errors:host_unknown());
                        {ok, HostType} ->
                            UseTLS = mongoose_config:get_opt([{s2s, HostType}, use_starttls]),
                            {StartTLS, TLSRequired, TLSCertVerify} = get_tls_params(UseTLS),
                            start_stream(AttrMap, StateData#state{server = Server,
                                                                  host_type = HostType,
                                                                  tls = StartTLS,
                                                                  tls_required = TLSRequired,
                                                                  tls_cert_verify = TLSCertVerify})
                    end;
                Server ->
                    start_stream(AttrMap, StateData);
                _Other ->
                    Msg = <<"The 'to' attribute differs from the originally provided one">>,
                    Info = #{location => ?LOCATION, last_event => Event,
                             expected_server => StateData#state.server, provided_server => Server},
                    stream_start_error(StateData, Info, mongoose_xmpp_errors:host_unknown(?MYLANG, Msg))
            end;
        #{<<"xmlns">> := <<"jabber:server">>} ->
            Msg = <<"The 'to' attribute is missing">>,
            Info = #{location => ?LOCATION, last_event => Event},
            stream_start_error(StateData, Info, mongoose_xmpp_errors:improper_addressing(?MYLANG, Msg));
        _ ->
            Info = #{location => ?LOCATION, last_event => Event},
            stream_start_error(StateData, Info, mongoose_xmpp_errors:invalid_namespace())
    end;
wait_for_stream({xmlstreamerror, _} = Event, StateData) ->
    Info = #{location => ?LOCATION, last_event => Event,
             reason => s2s_in_wait_for_stream_error},
    stream_start_error(StateData, Info, mongoose_xmpp_errors:xml_not_well_formed());
wait_for_stream(timeout, StateData) ->
    ?LOG_WARNING(#{what => s2s_in_wait_for_stream_timeout}),
    {stop, normal, StateData};
wait_for_stream(closed, StateData) ->
    ?LOG_WARNING(#{what => s2s_in_wait_for_stream_closed}),
    {stop, normal, StateData}.

start_stream(#{<<"version">> := <<"1.0">>, <<"from">> := RemoteServer} = Event,
             StateData = #state{tls = true, authenticated = false, server = Server,
                                host_type = HostType}) ->
    SASL = case StateData#state.tls_enabled of
               true ->
                   verify_cert_and_get_sasl(StateData#state.socket,
                                            StateData#state.tls_cert_verify);
               _Else ->
                   []
           end,
    StartTLS = get_tls_xmlel(StateData),
    case SASL of
        {error_cert_verif, CertError} ->
            ?LOG_WARNING(#{what => s2s_connection_closing,
                           text => <<"Closing s2s connection">>,
                           server => StateData#state.server,
                           remote_server => RemoteServer,
                           reason => cert_error,
                           cert_error => CertError}),
            Info = #{location => ?LOCATION, last_event => Event, reason => error_cert_verif},
            stream_start_error(StateData, Info,
                               mongoose_xmpp_errors:policy_violation(?MYLANG, CertError));
            %% We were stopping ejabberd_s2s_out connection in the older version of the code
            %% from this location. But stopping outgoing connections just because a non-verified
            %% incoming connection fails is an abuse risk (a hacker could connect with an invalid
            %% certificate, it should not cause stopping ejabberd_s2s_out connections).
        _ ->
            send_text(StateData, ?STREAM_HEADER(<<" version='1.0'">>)),
            send_element(StateData,
                         #xmlel{name = <<"stream:features">>,
                                children = SASL ++ StartTLS ++ stream_features(HostType, Server)}),
            {next_state, wait_for_feature_request, StateData}
    end;
start_stream(#{<<"version">> := <<"1.0">>},
             StateData = #state{authenticated = true, host_type = HostType, server = Server}) ->
    send_text(StateData, ?STREAM_HEADER(<<" version='1.0'">>)),
    send_element(StateData, #xmlel{name = <<"stream:features">>,
                                   children = stream_features(HostType, Server)}),
    {next_state, stream_established, StateData};
start_stream(#{<<"xmlns:db">> := <<"jabber:server:dialback">>}, StateData) ->
    send_text(StateData, ?STREAM_HEADER(<<>>)),
    {next_state, stream_established, StateData};
start_stream(Event, StateData) ->
    Info = #{location => ?LOCATION, last_event => Event},
    stream_start_error(StateData, Info, mongoose_xmpp_errors:invalid_xml()).

stream_start_error(StateData, Info, Error) ->
    send_text(StateData, ?STREAM_HEADER(<<>>)),
    send_element(StateData, Error),
    send_text(StateData, ?STREAM_TRAILER),
    ?LOG_WARNING(Info#{what => s2s_in_stream_start_error, element => Error}),
    {stop, normal, StateData}.

-spec wait_for_feature_request(ejabberd:xml_stream_item(), state()
                              ) -> fsm_return().
wait_for_feature_request({xmlstreamelement, El}, StateData) ->
    #xmlel{name = Name, attrs = Attrs, children = Els} = El,
    TLS = StateData#state.tls,
    TLSEnabled = StateData#state.tls_enabled,
    case {xml:get_attr_s(<<"xmlns">>, Attrs), Name} of
        {?NS_TLS, <<"starttls">>} when TLS == true,
                                       TLSEnabled == false ->
            ?LOG_DEBUG(#{what => s2s_starttls}),
            TLSOpts = tls_options_with_certfile(StateData),
            TLSSocket = mongoose_transport:wait_for_tls_handshake(
                                                 StateData#state.socket, TLSOpts,
                                                 #xmlel{name = <<"proceed">>,
                                                        attrs = [{<<"xmlns">>, ?NS_TLS}]}),
            {next_state, wait_for_stream,
             StateData#state{socket = TLSSocket,
                             streamid = new_id(),
                             tls_enabled = true,
                             tls_options = TLSOpts
                            }};
        {?NS_SASL, <<"auth">>} when TLSEnabled ->
            Mech = xml:get_attr_s(<<"mechanism">>, Attrs),
            case Mech of
                <<"EXTERNAL">> ->
                    Auth = jlib:decode_base64(xml:get_cdata(Els)),
                    AuthDomain = jid:nameprep(Auth),
                    CertData = mongoose_transport:get_peer_certificate(
                                 StateData#state.socket),
                    AuthRes = check_auth_domain(AuthDomain, CertData),
                    handle_auth_res(AuthRes, AuthDomain, StateData);
                _ ->
                    send_element(StateData,
                                 #xmlel{name = <<"failure">>,
                                        attrs = [{<<"xmlns">>, ?NS_SASL}],
                                        children = [#xmlel{name = <<"invalid-mechanism">>}]}),
                    ?LOG_WARNING(#{what => s2s_in_invalid_mechanism}),
                    {stop, normal, StateData}
            end;
        _ ->
            stream_established({xmlstreamelement, El}, StateData)
    end;
wait_for_feature_request({xmlstreamend, _Name}, StateData) ->
    send_text(StateData, ?STREAM_TRAILER),
    ?LOG_WARNING(#{what => s2s_in_got_stream_end_before_feature_request}),
    {stop, normal, StateData};
wait_for_feature_request({xmlstreamerror, _}, StateData) ->
    send_element(StateData, mongoose_xmpp_errors:xml_not_well_formed()),
    send_text(StateData, ?STREAM_TRAILER),
    ?LOG_WARNING(#{what => s2s_in_got_stream_error_before_feature_request}),
    {stop, normal, StateData};
wait_for_feature_request(closed, StateData) ->
    ?LOG_WARNING(#{what => s2s_in_got_closed_before_feature_request}),
    {stop, normal, StateData}.

tls_options_with_certfile(#state{host_type = HostType, tls_options = TLSOptions}) ->
    case mongoose_s2s_lib:lookup_certfile(HostType) of
        {ok, CertFile} -> TLSOptions#{certfile => CertFile};
        {error, not_found} -> TLSOptions
    end.

-spec stream_established(ejabberd:xml_stream_item(), state()) -> fsm_return().
stream_established({xmlstreamelement, El}, StateData) ->
    cancel_timer(StateData#state.timer),
    Timer = erlang:start_timer(mongoose_s2s_lib:timeout(), self(), []),
    case mongoose_s2s_dialback:parse_key(El) of
        %% Incoming dialback key, we have to verify it using ejabberd_s2s_out before
        %% accepting any incoming stanzas
        %% (we have to receive the `validity_from_s2s_out' event first).
        {step_1, FromTo, StreamID, Key} = Parsed ->
            ?LOG_DEBUG(#{what => s2s_in_get_key,
                         from_to => FromTo, stream_id => StreamID, key => Key}),
            %% Checks if the from domain is allowed and if the to
            %% domain is handled by this server:
            case {mongoose_s2s_lib:allow_host(FromTo), is_local_host_known(FromTo)} of
                {true, true} ->
                    ejabberd_s2s_out:terminate_if_waiting_delay(FromTo),
                    StartType = {verify, self(), Key, StateData#state.streamid},
                    %% Could we reuse an existing ejabberd_s2s_out connection
                    %% instead of making a new one?
                    ejabberd_s2s_out:start(FromTo, StartType),
                    Conns = maps:put(FromTo, wait_for_verification,
                                     StateData#state.connections),
                    change_shaper(StateData, FromTo),
                    {next_state,
                     stream_established,
                     StateData#state{connections = Conns, timer = Timer}};
                {_, false} ->
                    send_element(StateData, mongoose_xmpp_errors:host_unknown()),
                    ?LOG_WARNING(#{what => s2s_in_key_from_uknown_host, element => El,
                                   parsed => Parsed, from_to => FromTo}),
                    {stop, normal, StateData};
                {false, _} ->
                    send_element(StateData, mongoose_xmpp_errors:invalid_from()),
                    ?LOG_WARNING(#{what => s2s_in_key_with_invalid_from, element => El}),
                    {stop, normal, StateData}
            end;
        %% Incoming dialback verification request
        %% We have to check it using secrets and reply if it is valid or not
        {step_2, FromTo, StreamID, Key} ->
            ?LOG_DEBUG(#{what => s2s_in_verify_key,
                         from_to => FromTo, stream_id => StreamID, key => Key}),
            IsValid = Key =:= ejabberd_s2s:key(StateData#state.host_type, FromTo, StreamID),
            send_element(StateData, mongoose_s2s_dialback:step_3(FromTo, StreamID, IsValid)),
            {next_state, stream_established, StateData#state{timer = Timer}};
        false ->
            Res = parse_and_route_incoming_stanza(El, StateData),
            handle_routing_result(Res, El, StateData),
            {next_state, stream_established, StateData#state{timer = Timer}}
    end;
stream_established({validity_from_s2s_out, IsValid, FromTo}, StateData) ->
    handle_validity_from_s2s_out(IsValid, FromTo, StateData);
stream_established({xmlstreamend, _Name}, StateData) ->
    send_text(StateData, ?STREAM_TRAILER),
    {stop, normal, StateData};
stream_established({xmlstreamerror, _}, StateData) ->
    send_element(StateData, mongoose_xmpp_errors:xml_not_well_formed()),
    send_text(StateData, ?STREAM_TRAILER),
    ?LOG_WARNING(#{what => s2s_in_stream_error, state_name => stream_established}),
    {stop, normal, StateData};
stream_established(timeout, StateData) ->
    {stop, normal, StateData};
stream_established(closed, StateData) ->
    {stop, normal, StateData}.

-spec handle_validity_from_s2s_out(boolean(), ejabberd_s2s:fromto(), #state{}) ->
    {next_state, stream_established, #state{}}.
handle_validity_from_s2s_out(IsValid, FromTo, StateData) ->
    send_element(StateData, mongoose_s2s_dialback:step_4(FromTo, IsValid)),
    {next_state, stream_established, update_connections(IsValid, FromTo, StateData)}.

update_connections(true, FromTo, StateData = #state{connections = Cons}) ->
    StateData#state{connections = maps:put(FromTo, established, Cons)};
update_connections(false, FromTo, StateData = #state{connections = Cons}) ->
    StateData#state{connections = maps:remove(FromTo, Cons)}.

handle_routing_result(ok, _El, _StateData) ->
    ok;
handle_routing_result({error, Reason}, El, _StateData) ->
    ?LOG_WARNING(#{what => s2s_in_route_failed, reason => Reason, element => El}).

parse_and_route_incoming_stanza(El, StateData) ->
    NewEl = jlib:remove_attr(<<"xmlns">>, El),
    RemoteJid = jid:from_binary(exml_query:attr(El, <<"from">>, <<>>)),
    LocalJid = jid:from_binary(exml_query:attr(El, <<"to">>, <<>>)),
    case {RemoteJid, LocalJid, is_valid_stanza(NewEl)} of
        {#jid{}, #jid{}, true} ->
            route_incoming_stanza(RemoteJid, LocalJid, NewEl, StateData);
        _ ->
            {error, invalid_stanza}
    end.

-spec route_incoming_stanza(RemoteJid :: jid:jid(),
                            LocalJid :: jid:jid(),
                            El :: exml:element(),
                            StateData :: state()) -> ok | {error, term()}.
route_incoming_stanza(RemoteJid, LocalJid, El, StateData) ->
    LRemoteServer = RemoteJid#jid.lserver,
    LLocalServer = LocalJid#jid.lserver,
    FromTo = {LLocalServer, LRemoteServer},
    Acc = mongoose_acc:new(#{ location => ?LOCATION,
                              lserver => LLocalServer,
                              element => El,
                              from_jid => RemoteJid,
                              to_jid => LocalJid }),
    case is_s2s_authenticated_or_connected(FromTo, StateData) of
        true ->
            route_stanza(Acc);
        false ->
            {error, not_allowed}
    end.

is_s2s_authenticated_or_connected(FromTo, StateData) ->
    is_s2s_authenticated(FromTo, StateData) orelse
        is_s2s_connected(FromTo, StateData).

-spec is_s2s_authenticated(ejabberd_s2s:fromto(), #state{}) -> boolean().
is_s2s_authenticated(_FromTo, #state{authenticated = false}) ->
    false;
is_s2s_authenticated(FromTo, State) ->
    same_auth_domain(FromTo, State) andalso is_local_host_known(FromTo).

-spec same_auth_domain(ejabberd_s2s:fromto(), #state{}) -> boolean().
same_auth_domain({_, LRemoteServer}, #state{auth_domain = AuthDomain}) ->
    LRemoteServer =:= AuthDomain.

-spec is_s2s_connected(ejabberd_s2s:fromto(), #state{}) -> boolean().
is_s2s_connected(FromTo, StateData) ->
    established =:= maps:get(FromTo, StateData#state.connections, false).

-spec is_valid_stanza(exml:element()) -> boolean().
is_valid_stanza(#xmlel{name = Name}) ->
    is_valid_stanza_name(Name).

is_valid_stanza_name(<<"iq">>) -> true;
is_valid_stanza_name(<<"message">>) -> true;
is_valid_stanza_name(<<"presence">>) -> true;
is_valid_stanza_name(_) -> false.

-spec route_stanza(mongoose_acc:t()) -> ok.
route_stanza(Acc) ->
    From = mongoose_acc:from_jid(Acc),
    To = mongoose_acc:to_jid(Acc),
    Acc1 = mongoose_hooks:s2s_receive_packet(Acc),
    ejabberd_router:route(From, To, Acc1),
    ok.

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

-spec handle_sync_event(any(), any(), statename(), state()) ->
    {reply, ok | connection_info(), statename(), state()}.
handle_sync_event(get_state_info, _From, StateName, StateData) ->
    {reply, handle_get_state_info(StateName, StateData), StateName, StateData};
handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info({timeout, Timer, _}, _StateName,
            #state{timer = Timer} = StateData) ->
    {stop, normal, StateData};
handle_info(_, StateName, StateData) ->
    {next_state, StateName, StateData}.


%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
-spec terminate(any(), statename(), state()) -> 'ok'.
terminate(Reason, StateName, StateData) ->
    ?LOG_DEBUG(#{what => s2s_in_stopped, reason => Reason, state_name => StateName}),
    mongoose_transport:close(StateData#state.socket),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

-spec send_text(state(), binary()) -> ok.
send_text(StateData, Text) ->
    mongoose_transport:send_text(StateData#state.socket, Text).

-spec send_element(state(), exml:element()) -> ok.
send_element(StateData, El) ->
    mongoose_transport:send_element(StateData#state.socket, El).

-spec stream_features(mongooseim:host_type(), binary()) -> [exml:element()].
stream_features(HostType, Domain) ->
    mongoose_hooks:s2s_stream_features(HostType, Domain).

-spec change_shaper(state(), ejabberd_s2s:fromto()) -> ok.
change_shaper(StateData, {LLocalServer, LRemoteServer}) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(LLocalServer),
    JID = jid:make(<<>>, LRemoteServer, <<>>),
    Shaper = acl:match_rule(HostType, StateData#state.shaper, JID),
    mongoose_transport:change_shaper(StateData#state.socket, Shaper),
    ok.


-spec new_id() -> binary().
new_id() ->
    mongoose_bin:gen_from_crypto().


-spec cancel_timer(reference()) -> 'ok'.
cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    receive
        {timeout, Timer, _} ->
            ok
    after 0 ->
            ok
    end.

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

verify_cert_and_get_sasl(Socket, TLSCertVerify) ->
    case mongoose_transport:get_peer_certificate(Socket) of
        {ok, _} ->
            [#xmlel{name = <<"mechanisms">>,
                    attrs = [{<<"xmlns">>, ?NS_SASL}],
                    children = [#xmlel{name = <<"mechanism">>,
                                       children = [#xmlcdata{content = <<"EXTERNAL">>}]}]}];
        {bad_cert, CertVerifyRes} ->
            check_sasl_tls_certveify(TLSCertVerify, CertVerifyRes);
        no_peer_cert -> []
    end.

check_sasl_tls_certveify(true, CertVerifyRes) ->
    {error_cert_verif, CertVerifyRes};
check_sasl_tls_certveify(false, _) ->
    [].

check_auth_domain(error, _) ->
    false;
check_auth_domain(AuthDomain, {ok, Cert}) ->
    case mongoose_s2s_lib:domain_utf8_to_ascii(AuthDomain) of
        false ->
            false;
        PCAuthDomain ->
            lists:any(
              fun(D) -> match_domain( PCAuthDomain, D) end,
              cert_utils:get_cert_domains(Cert))
    end;
check_auth_domain(_, _) ->
    false.

handle_auth_res(true, AuthDomain, StateData) ->
    send_element(StateData,
                 #xmlel{name = <<"success">>,
                        attrs = [{<<"xmlns">>, ?NS_SASL}]}),
    ?LOG_DEBUG(#{what => s2s_auth_success,
                 text => <<"Accepted s2s authentication">>,
                 socket => StateData#state.socket, auth_domain => AuthDomain}),
    {next_state, wait_for_stream,
     StateData#state{streamid = new_id(),
                     authenticated = true,
                     auth_domain = AuthDomain
                    }};
handle_auth_res(_, _, StateData) ->
    send_element(StateData,
                 #xmlel{name = <<"failure">>,
                        attrs = [{<<"xmlns">>, ?NS_SASL}]}),
    send_text(StateData, ?STREAM_TRAILER),
    ?LOG_WARNING(#{what => s2s_in_auth_failed}),
    {stop, normal, StateData}.


get_tls_params(false) ->
    {false, false, false};
get_tls_params(true) ->
    {true, false, false};
get_tls_params(optional) ->
    {true, false, false};
get_tls_params(required) ->
    {true, true, false};
get_tls_params(required_trusted) ->
    {true, true, true}.

get_tls_xmlel(#state{tls_enabled = true}) ->
    [];
get_tls_xmlel(#state{tls_enabled = false, tls_required = false}) ->
    [#xmlel{name = <<"starttls">>,
            attrs = [{<<"xmlns">>, ?NS_TLS}]}];
get_tls_xmlel(#state{tls_enabled = false, tls_required = true}) ->
    [#xmlel{name = <<"starttls">>,
            attrs = [{<<"xmlns">>, ?NS_TLS}],
            children = [#xmlel{name = <<"required">>}]}].

-spec is_local_host_known(ejabberd_s2s:fromto()) -> boolean().
is_local_host_known({LLocalServer, _}) ->
    mongoose_router:is_registered_route(LLocalServer)
        orelse mongoose_component:has_component(LLocalServer)
        orelse is_known_domain(LLocalServer).

is_known_domain(Domain) ->
    case mongoose_domain_api:get_host_type(Domain) of
        {ok, _HostType} ->
            true;
        _ ->
            false
    end.

-spec handle_get_state_info(statename(), state()) -> connection_info().
handle_get_state_info(StateName, StateData) ->
    {ok, {Addr, Port}} = mongoose_transport:peername(StateData#state.socket),
    Domains = case StateData#state.authenticated of
                  true ->
                      [StateData#state.auth_domain];
                  false ->
                      Connections = StateData#state.connections,
                      [LRemoteServer || {{_, LRemoteServer}, established} <-
                          maps:to_list(Connections)]
              end,
    #{pid => self(),
      direction => in,
      statename => StateName,
      addr => Addr,
      port => Port,
      streamid => StateData#state.streamid,
      tls => StateData#state.tls,
      tls_enabled => StateData#state.tls_enabled,
      tls_options => StateData#state.tls_options,
      authenticated => StateData#state.authenticated,
      shaper => StateData#state.shaper,
      domains => Domains}.
