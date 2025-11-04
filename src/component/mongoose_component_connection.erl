-module(mongoose_component_connection).

-xep([{xep, 114}, {version, "1.6"}]).
-xep([{xep, 225}, {version, "0.2"}, {status, partial}]).

-include("mongoose_logger.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").
-include_lib("exml/include/exml_stream.hrl").
-include("external_component.hrl").

-define(RETRIES, 3).

-behaviour(gen_statem).
-export([callback_mode/0, init/1, handle_event/4, terminate/3]).

%% utils
-export([start_link/2, exit/2, route/2]).

-record(component_data, {
          lserver = ?MYNAME :: jid:lserver(),
          streamid = mongoose_bin:gen_from_crypto() :: binary(),
          is_subdomain = false :: boolean(),
          component :: undefined | mongoose_component:external_component(),
          socket :: undefined | mongoose_xmpp_socket:socket(),
          parser :: undefined | exml_stream:parser(),
          shaper :: undefined | mongoose_shaper:shaper(),
          listener_opts :: mongoose_listener:options()
         }).
-type data() :: #component_data{}.
-type maybe_ok() :: ok | {error, atom()}.
-type fsm_res() :: gen_statem:event_handler_result(state(), data()).
-type packet() :: {jid:jid(), jid:jid(), exml:element()}.

-type retries() :: 0..3.
-type state() :: connect
               | wait_for_stream
               | wait_for_handshake
               | stream_established.

-export_type([packet/0, data/0, state/0, fsm_res/0, retries/0]).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

-spec start_link(mongoose_listener:init_args(), [gen_statem:start_opt()]) ->
    gen_statem:start_ret().
start_link(Params, ProcOpts) ->
    gen_statem:start_link(?MODULE, Params, ProcOpts).

-spec exit(pid(), binary() | atom()) -> ok.
exit(Pid, Reason) ->
    gen_statem:cast(Pid, {exit, Reason}).

-spec route(pid(), mongoose_acc:t()) -> {route, mongoose_acc:t()}.
route(Pid, Acc) ->
    Pid ! {route, Acc}.

%%%----------------------------------------------------------------------
%%% gen_statem
%%%----------------------------------------------------------------------

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    handle_event_function.

-spec init(mongoose_listener:init_args()) -> gen_statem:init_result(state(), undefined).
init({Transport, Ref, LOpts}) ->
    ConnectEvent = {next_event, internal, {connect, {Transport, Ref, LOpts}}},
    {ok, connect, undefined, ConnectEvent}.

-spec handle_event(gen_statem:event_type(), term(), state(), data()) -> fsm_res().
handle_event(internal, {connect, {Transport, Ref, LOpts}}, connect, _) ->
    #{shaper := ShaperName, max_stanza_size := MaxStanzaSize} = LOpts,
    {ok, Parser} = exml_stream:new_parser([{max_element_size, MaxStanzaSize}]),
    Shaper = mongoose_shaper:new(ShaperName),
    Socket = mongoose_xmpp_socket:accept(Transport, component, Ref, LOpts),
    StateData = #component_data{socket = Socket, parser = Parser, shaper = Shaper, listener_opts = LOpts},
    {next_state, wait_for_stream, StateData, state_timeout(LOpts)};
handle_event(internal, #xmlstreamstart{attrs = Attrs}, wait_for_stream, StateData) ->
    handle_stream_start(StateData, Attrs);
handle_event(internal, #xmlel{name = <<"handshake">>} = El, wait_for_handshake, StateData) ->
    handle_handshake(StateData, El);
handle_event(internal, #xmlel{} = El, stream_established, StateData) ->
    handle_stream_established(StateData, El);
handle_event(internal, #xmlstreamend{}, _, StateData) ->
    send_trailer(StateData),
    {stop, {shutdown, stream_end}};
handle_event(internal, #xmlstreamstart{}, _, StateData) ->
    stream_start_error(StateData, mongoose_xmpp_errors:policy_violation());
handle_event(cast, {exit, Reason}, _, StateData) when is_binary(Reason) ->
    StreamConflict = mongoose_xmpp_errors:stream_conflict(?MYLANG, Reason),
    send_xml(StateData, StreamConflict),
    send_trailer(StateData),
    {stop, {shutdown, Reason}};
handle_event(cast, {exit, system_shutdown}, _, StateData) ->
    Error = mongoose_xmpp_errors:system_shutdown(?MYLANG, <<"System shutting down">>),
    send_xml(StateData, Error),
    send_trailer(StateData),
    {stop, {shutdown, system_shutdown}};
handle_event(info, {route, Acc}, State, StateData) ->
    handle_route(StateData, State, Acc);
handle_event(info, {TcpOrSSl, _Socket, _Packet} = SocketData, _, StateData)
  when TcpOrSSl =:= tcp; TcpOrSSl =:= ssl ->
    handle_socket_data(StateData, SocketData);
handle_event(info, TcpOrSslInfo, _, _)
  when is_tuple(TcpOrSslInfo) andalso
       tcp_closed =:= element(1, TcpOrSslInfo) orelse tcp_error =:= element(1, TcpOrSslInfo) orelse
       ssl_closed =:= element(1, TcpOrSslInfo) orelse ssl_error =:= element(1, TcpOrSslInfo) ->
    {stop, {shutdown, element(1, TcpOrSslInfo)}};
handle_event({timeout, activate_socket}, activate_socket, _State, StateData) ->
    activate_socket(StateData),
    keep_state_and_data;
handle_event(state_timeout, state_timeout_termination, _State, StateData) ->
    StreamConflict = mongoose_xmpp_errors:connection_timeout(),
    send_xml(StateData, StreamConflict),
    send_trailer(StateData),
    {stop, {shutdown, state_timeout}};
handle_event(EventType, EventContent, State, StateData) ->
    ?LOG_WARNING(#{what => unknown_statem_event,
                   component_state => State, component_data => StateData,
                   event_type => EventType, event_content => EventContent}),
    keep_state_and_data.

-spec terminate(term(), undefined | state(), data()) -> any().
terminate(Reason, stream_established, StateData) ->
    unregister_routes(StateData),
    terminate(Reason, undefined, StateData);
terminate(Reason, State, StateData) ->
    ?LOG_INFO(#{what => component_statem_terminate,
                reason => Reason,
                component => StateData#component_data.lserver,
                component_state => State,
                component_data => StateData}),
    close_parser(StateData),
    close_socket(StateData).

%%%----------------------------------------------------------------------
%%% helpers
%%%----------------------------------------------------------------------

-spec handle_socket_data(data(), {_, _, iodata()}) -> fsm_res().
handle_socket_data(StateData = #component_data{socket = Socket}, Payload) ->
    case mongoose_xmpp_socket:handle_data(Socket, Payload) of
        {error, _Reason} ->
            {stop, {shutdown, socket_error}, StateData};
        Data ->
            handle_socket_packet(StateData, Data)
    end.

-spec handle_socket_packet(data(), iodata()) -> fsm_res().
handle_socket_packet(StateData = #component_data{parser = Parser}, Packet) ->
    ?LOG_DEBUG(#{what => received_xml_on_stream, packet => Packet, component_pid => self()}),
    case exml_stream:parse(Parser, Packet) of
        {error, Reason} ->
            NextEvent = {next_event, internal, #xmlstreamerror{name = iolist_to_binary(Reason)}},
            {keep_state, StateData, NextEvent};
        {ok, NewParser, XmlElements} ->
            Size = iolist_size(Packet),
            NewStateData = StateData#component_data{parser = NewParser},
            handle_socket_elements(NewStateData, XmlElements, Size)
    end.

-spec handle_socket_elements(data(), [exml_stream:element()], non_neg_integer()) -> fsm_res().
handle_socket_elements(StateData = #component_data{lserver = LServer, shaper = Shaper}, Elements, Size) ->
    [execute_element_event(Element, LServer, xmpp_element_in) || Element = #xmlel{} <- Elements],
    {NewShaper, Pause} = mongoose_shaper:update(Shaper, Size),
    NewStateData = StateData#component_data{shaper = NewShaper},
    StreamEvents0 = [ {next_event, internal, XmlEl} || XmlEl <- Elements ],
    StreamEvents1 = maybe_add_pause(NewStateData, StreamEvents0, Pause),
    {keep_state, NewStateData, StreamEvents1}.

-spec maybe_add_pause(data(), [gen_statem:action()], integer()) -> [gen_statem:action()].
maybe_add_pause(_, StreamEvents, Pause) when Pause > 0 ->
    [{{timeout, activate_socket}, Pause, activate_socket} | StreamEvents];
maybe_add_pause(#component_data{socket = Socket}, StreamEvents, _) ->
    mongoose_xmpp_socket:activate(Socket),
    StreamEvents.

-spec close_socket(data()) -> ok | {error, term()}.
close_socket(#component_data{socket = undefined}) ->
    ok;
close_socket(#component_data{socket = Socket}) ->
    mongoose_xmpp_socket:close(Socket).

-spec activate_socket(data()) -> ok | {error, term()}.
activate_socket(#component_data{socket = Socket}) ->
    mongoose_xmpp_socket:activate(Socket).

-spec handle_stream_start(data(), exml:attrs()) -> fsm_res().
handle_stream_start(S0, Attrs) ->
    LServer = jid:nameprep(maps:get(<<"to">>, Attrs, <<>>)),
    XmlNs = maps:get(<<"xmlns">>, Attrs, <<>>),
    case {XmlNs, LServer} of
        {?NS_COMPONENT_ACCEPT, LServer} when is_binary(LServer) ->
            IsSubdomain = <<"true">> =:= maps:get(<<"is_subdomain">>, Attrs, <<>>),
            S1 = S0#component_data{lserver = LServer, is_subdomain = IsSubdomain},
            send_header(S1),
            {next_state, wait_for_handshake, S1, state_timeout(S1)};
        {?NS_COMPONENT_ACCEPT, error} ->
            stream_start_error(S0, mongoose_xmpp_errors:host_unknown());
        {_, _} ->
            stream_start_error(S0, mongoose_xmpp_errors:invalid_namespace())
    end.

-spec handle_handshake(data(), exml:element()) -> fsm_res().
handle_handshake(StateData, El) ->
    #component_data{lserver = LServer,
                    streamid = StreamId,
                    listener_opts = #{password := Password}} = StateData,
    Proof = create_proof(StreamId, Password),
    Digest = exml_query:cdata(El),
    case crypto:hash_equals(Digest, Proof) of
        true ->
            try_register_routes(StateData, ?RETRIES);
        false ->
            mongoose_instrument:execute(component_auth_failed, #{},
                                        #{count => 1, lserver => LServer}),
            Error = mongoose_xmpp_errors:stream_not_authorized(?MYLANG, <<"Invalid handshake">>),
            stream_start_error(StateData, Error)
    end.

%% The XML character data of the handshake element is computed according to the following algorithm:
%% 1. Concatenate the Stream ID received from the server with the shared secret.
%% 2. Hash the concatenated string according to the SHA1 algorithm, i.e., SHA1( concat (sid, password)).
%% 3. Ensure that the hash output is in hexadecimal format, not binary or base64.
%% 4. Convert the hash output to all lowercase characters.
-spec create_proof(binary(), binary()) -> binary().
create_proof(StreamId, Password) ->
    Concat = [StreamId, Password],
    Sha1 = crypto:hash(sha, Concat),
    binary:encode_hex(Sha1, lowercase).

%% Once authenticated, the component can send stanzas through the server and receive stanzas from
%% the server. All stanzas sent to the server MUST possess a 'from' attribute and a 'to' attribute,
%% as in the 'jabber:server' namespace. The domain identifier portion of the JID contained in the
%% 'from' attribute MUST match the hostname of the component. However, this is the only restriction
%% on 'from' addresses, and the component MAY send stanzas from any user at its hostname.
-spec handle_stream_established(data(), exml:element()) -> fsm_res().
handle_stream_established(StateData, #xmlel{name = Name} = El) ->
    #component_data{lserver = LServer, listener_opts = #{check_from := CheckFrom}} = StateData,
    NewEl = jlib:remove_attr(<<"xmlns">>, El),
    FromJid = jid:from_binary(exml_query:attr(El, <<"from">>, <<>>)),
    IsValidFromJid =
        case {CheckFrom, FromJid} of
            %% The default is the standard behaviour in XEP-0114
            {true, #jid{lserver = FromLServer}} ->
                FromLServer =:= LServer;
            %% If the admin does not want to check the from field when accept packets from any
            %% address. In this case, the component can send packet of behalf of the server users.
            _ ->
                true
        end,
    ToJid = jid:from_binary(exml_query:attr(El, <<"to">>, <<>>)),
    IsStanza = (<<"iq">> =:= Name)
        orelse (<<"message">> =:= Name)
        orelse (<<"presence">> =:= Name),
    case IsStanza andalso IsValidFromJid andalso (error =/= ToJid) of
        true ->
            Acc = element_to_origin_accum(StateData, FromJid, ToJid, NewEl),
            ejabberd_router:route(FromJid, ToJid, Acc, NewEl);
        false ->
            ?LOG_INFO(#{what => comp_bad_request,
                        text => <<"Not valid Name or error in FromJid or ToJid">>,
                        stanza_name => Name, from_jid => FromJid, to_jid => ToJid}),
            Err = jlib:make_error_reply(NewEl, mongoose_xmpp_errors:bad_request()),
            send_xml(StateData, Err),
            error
    end,
    keep_state_and_data.

-spec try_register_routes(data(), retries()) -> fsm_res().
try_register_routes(StateData, Retries) ->
    case register_routes(StateData) of
        {ok, Component} ->
            send_xml(StateData, #xmlel{name = <<"handshake">>}),
            {next_state, stream_established, StateData#component_data{component = Component}};
        {error, Reason} ->
            #component_data{listener_opts = #{conflict_behaviour := ConflictBehaviour}} = StateData,
            RoutesInfo = lookup_routes(StateData),
            ?LOG_ERROR(#{what => comp_registration_conflict,
                         text => <<"Another connection from a component with the same name">>,
                         component => StateData#component_data.lserver,
                         reason => Reason, retries => Retries, routes_info => RoutesInfo,
                         conflict_behaviour => ConflictBehaviour}),
            handle_registration_conflict(ConflictBehaviour, RoutesInfo, StateData, Retries)
    end.

-spec register_routes(data()) -> {ok, mongoose_component:external_component()} | {error, any()}.
register_routes(#component_data{lserver = SubDomain,
                                is_subdomain = true,
                                listener_opts = #{hidden_components := AreHidden}}) ->
    Prefix = {prefix, <<SubDomain/binary, ".">>},
    Handler = mongoose_packet_handler:new(mongoose_component, #{pid => self()}),
    [ mongoose_domain_api:register_subdomain(HostType, Prefix, Handler)
      || HostType <- ?ALL_HOST_TYPES ],
    mongoose_component:register_component(SubDomain, node(), Handler, true, AreHidden);
register_routes(#component_data{lserver = LServer,
                                listener_opts = #{hidden_components := AreHidden}}) ->
    Handler = mongoose_packet_handler:new(mongoose_component, #{pid => self()}),
    mongoose_component:register_component(LServer, node(), Handler, false, AreHidden).

-spec lookup_routes(data()) -> [{_, _}].
lookup_routes(#component_data{lserver = LServer}) ->
    [{LServer, mongoose_component:lookup_component(LServer)}].

handle_registration_conflict(kick_old, RoutesInfo, StateData, Retries) when Retries > 0 ->
    %% see lookup_routes
    Pids = lists:usort(routes_info_to_pids(RoutesInfo)),
    Results = lists:map(fun stop_process/1, Pids),
    AllOk = lists:all(fun(Result) -> Result =:= ok end, Results),
    case AllOk of
        true ->
            %% Do recursive call
            try_register_routes(StateData, Retries - 1);
        false ->
            ?LOG_ERROR(#{what => comp_registration_kick_failed,
                         text => <<"Failed to stop old component connection. Disconnecting next.">>,
                         component => StateData#component_data.lserver,
                         component_pids => Pids, results => Results}),
            do_disconnect_on_conflict(StateData)
    end;
handle_registration_conflict(_Behaviour, _RoutesInfo, StateData, _Retries) ->
    do_disconnect_on_conflict(StateData).

do_disconnect_on_conflict(StateData) ->
    send_xml(StateData, mongoose_xmpp_errors:stream_conflict()),
    {stop, normal, StateData}.

stop_process(Pid) ->
    ?MODULE:exit(Pid, <<"Replaced by new connection">>),
    MonRef = erlang:monitor(process, Pid),
    receive
        {'DOWN', MonRef, process, Pid, _Reason} ->
            ok
    after 5000 ->
            erlang:demonitor(MonRef, [flush]),
            {error, timeout}
    end.

routes_info_to_pids(RoutesInfo) ->
    {_Hosts, ExtComponentsPerHost} = lists:unzip(RoutesInfo),
    %% Flatten the list of lists
    ExtComponents = lists:append(ExtComponentsPerHost),
    %% Ignore handlers from other modules
    [maps:get(pid, mongoose_packet_handler:extra(H))
     || #external_component{handler = H} <- ExtComponents,
        mongoose_packet_handler:module(H) =:= mongoose_component].

-spec handle_route(data(), state(), mongoose_acc:t()) -> fsm_res().
handle_route(StateData = #component_data{}, _, Acc) ->
    ?LOG_DEBUG(#{what => comp_route,
                 text => <<"Route packet to an external component">>,
                 component => StateData#component_data.lserver, acc => Acc}),
    {From, To, Packet} = mongoose_acc:packet(Acc),
    #component_data{listener_opts = #{access := Access}} = StateData,
    case acl:match_rule(global, Access, From) of
        allow ->
            mongoose_hooks:packet_to_component(Acc, From, To),
            Packet2 = jlib:replace_from_to(From, To, Packet),
            send_xml(StateData, Packet2);
        deny ->
            ejabberd_router:route_error_reply(To, From, Acc, mongoose_xmpp_errors:not_allowed())
    end,
    keep_state_and_data.

-spec element_to_origin_accum(data(), jid:jid(), jid:jid(), exml:element()) -> mongoose_acc:t().
element_to_origin_accum(StateData, FromJid, ToJid, El) ->
    Params = #{host_type => undefined,
               lserver => StateData#component_data.lserver,
               location => ?LOCATION,
               element => El,
               from_jid => FromJid,
               to_jid => ToJid},
    Acc = mongoose_acc:new(Params),
    mongoose_acc:set_permanent(component, [{module, ?MODULE}, {origin_jid, 'TODO'}], Acc).

-spec stream_start_error(data(), exml:element()) -> fsm_res().
stream_start_error(StateData, Error) ->
    ?LOG_DEBUG(#{what => stream_start_error, xml_error => Error, component_state => StateData}),
    send_xml(StateData, Error),
    send_xml(StateData, ?XML_STREAM_TRAILER),
    {stop, {shutdown, stream_error}, StateData}.

-spec send_header(StateData :: data()) -> any().
send_header(StateData) ->
    Header = stream_header(StateData),
    send_xml(StateData, Header).

-spec send_trailer(data()) -> maybe_ok().
send_trailer(StateData) ->
    send_xml(StateData, ?XML_STREAM_TRAILER).

-spec close_parser(data()) -> ok.
close_parser(#component_data{parser = undefined}) ->
    ok;
close_parser(#component_data{parser = Parser}) ->
    exml_stream:free_parser(Parser).

-spec send_xml(data(), exml_stream:element() | [exml_stream:element()]) -> maybe_ok().
send_xml(Data, XmlElement) when is_tuple(XmlElement) ->
    send_xml(Data, [XmlElement]);
send_xml(#component_data{lserver = LServer, socket = Socket}, XmlElements) when is_list(XmlElements) ->
    [execute_element_event(Element, LServer, xmpp_element_out) || Element = #xmlel{} <- XmlElements],
    mongoose_xmpp_socket:send_xml(Socket, XmlElements).

state_timeout(#component_data{listener_opts = LOpts}) ->
    state_timeout(LOpts);
state_timeout(#{state_timeout := Timeout}) ->
    {state_timeout, Timeout, state_timeout_termination}.

-spec stream_header(data()) -> exml_stream:start().
stream_header(StateData) ->
    LServer = StateData#component_data.lserver,
    StreamId = StateData#component_data.streamid,
    Attrs = #{<<"xmlns:stream">> => <<"http://etherx.jabber.org/streams">>,
              <<"xmlns">> => ?NS_COMPONENT_ACCEPT,
              <<"id">> => StreamId,
              <<"from">> => LServer},
    #xmlstreamstart{name = <<"stream:stream">>, attrs = Attrs}.

-spec unregister_routes(data()) -> any().
unregister_routes(#component_data{component = undefined}) ->
    ok;
unregister_routes(#component_data{lserver = SubDomain, is_subdomain = true, component = Component}) ->
    Prefix = {prefix, <<SubDomain/binary, ".">>},
    [ mongoose_domain_api:unregister_subdomain(HostType, Prefix)
      || HostType <- ?ALL_HOST_TYPES ],
    mongoose_component:unregister_component(Component);
unregister_routes(#component_data{component = Component}) ->
    mongoose_component:unregister_component(Component).

%%% Instrumentation helpers

-spec execute_element_event(exml:element(), jid:lserver(), mongoose_instrument:event_name()) -> ok.
execute_element_event(Element, LServer, EventName) ->
    mongoose_instrument_xmpp:execute_element_event(EventName, component, undefined, Element,
                                                   #{lserver => LServer}).
