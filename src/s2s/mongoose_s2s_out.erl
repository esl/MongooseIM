-module(mongoose_s2s_out).

-behaviour(gen_statem).

-include_lib("exml/include/exml_stream.hrl").
-include("mongoose.hrl").
-include("jlib.hrl").

-type features_before_auth() ::
    #{sasl_external := boolean(),
      starttls := false | optional | required,
      is_ssl := boolean()}.
-type options() :: #{connection_timeout := timeout(),
                     max_stanza_size := non_neg_integer(),
                     max_retry_delay := pos_integer(),
                     shaper := atom(),
                     state_timeout := timeout(),
                     stream_timeout := timeout(),
                     tls := just_tls:options(),
                     atom() => term() % other options are not used in this module
                    }.
-record(s2s_data, {
          host_type :: mongooseim:host_type(),
          myname :: jid:lserver(),
          remote_server :: jid:lserver(),
          socket :: mongoose_xmpp_socket:socket(),
          parser :: exml_stream:parser(),
          shaper :: mongoose_shaper:shaper(),
          opts :: options(), %% This is very similar to listener opts
          process_type :: process_type(),
          streamid = mongoose_bin:gen_from_crypto() :: binary(),
          remote_streamid = <<>> :: ejabberd_s2s:stream_id(),
          dialback_enabled = true :: boolean()
         }).
-type data() :: ejabberd_s2s:fromto() | #s2s_data{}.
-type maybe_ok() :: ok | {error, atom()}.
-type fsm_res() :: gen_statem:event_handler_result(state(), data()).
-type stream_state() :: stream_start | authenticated.
-type state() :: connect
               | {wait_for_stream, stream_state()}
               | {wait_for_features, stream_state()}
               | wait_for_starttls_proceed
               | wait_for_auth_result
               | wait_for_session_establishment
               | wait_for_validation
               | stream_established
               | bounce_and_disconnect.

-type connection_info() ::
    #{pid => pid(),
      direction => out,
      state_name => state(),
      addr => unknown | inet:ip_address(),
      port => unknown | inet:port_number(),
      streamid => ejabberd_s2s:stream_id() | undefined,
      shaper => mongoose_shaper:shaper(),
      tls => boolean(),
      tls_required => boolean(),
      tls_enabled => boolean(),
      tls_options => #{} | just_tls:options(),
      authenticated => boolean(),
      dialback_enabled => boolean(),
      server => jid:lserver(),
      myname => jid:lserver(),
      process_type => process_type()}.

-export_type([connection_info/0]).

%% gen_statem callbacks
-export([callback_mode/0, init/1, handle_event/4, terminate/3]).

-export([start_connection/2,
         start_link/2,
         stop_connection/2,
         route/2,
         get_state_info/1,
         terminate_if_waiting_delay/1]).
-ignore_xref([start_link/2, stop_connection/2]).

-type process_type() ::
    new |
    {verify, S2SIn :: pid(), Key :: ejabberd_s2s:s2s_dialback_key(), SID :: ejabberd_s2s:stream_id()}.
-type init_args() :: {ejabberd_s2s:fromto(), process_type()}.

-spec start_connection(ejabberd_s2s:fromto(), process_type()) ->
    supervisor:startchild_ret().
start_connection(FromTo, PType) ->
    supervisor:start_child(mongoose_s2s_out_sup, [FromTo, PType]).

-spec start_link(ejabberd_s2s:fromto(), process_type()) -> gen_statem:start_ret().
start_link(FromTo, PType) ->
    gen_statem:start_link(?MODULE, {FromTo, PType}, []).

-spec stop_connection(pid(), binary() | atom()) -> ok.
stop_connection(Pid, Reason) ->
    gen_statem:cast(Pid, {exit, Reason}).

-spec get_state_info(pid()) -> term().
get_state_info(Pid) ->
    gen_statem:call(Pid, get_state_info, 5000).

-spec terminate_if_waiting_delay(ejabberd_s2s:fromto()) -> ok.
terminate_if_waiting_delay(FromTo) ->
    [ gen_statem:cast(Pid, terminate_if_waiting_before_retry)
      || Pid <- ejabberd_s2s:get_s2s_out_pids(FromTo) ],
    ok.

-spec route(pid(), mongoose_acc:t()) -> any().
route(Pid, Acc) ->
    Pid ! {route, Acc}.

-spec callback_mode() -> gen_statem:callback_mode_result().
callback_mode() ->
    handle_event_function.

-spec init(init_args()) -> gen_statem:init_result(state(), data()).
init({FromTo, new} = InitArgs) ->
    case ejabberd_s2s:try_register(FromTo) of
        true ->
            process_flag(trap_exit, true),
            ConnectEvent = {next_event, internal, {connect, InitArgs}},
            {ok, connect, FromTo, ConnectEvent};
        false ->
            ignore
    end;
init({FromTo, _Type} = InitArgs) ->
    process_flag(trap_exit, true),
    ConnectEvent = {next_event, internal, {connect, InitArgs}},
    {ok, connect, FromTo, ConnectEvent}.

-spec handle_event(gen_statem:event_type(), term(), state(), data()) -> fsm_res().
handle_event(internal, {connect, {{FromServer, ToServer}, PType}}, connect, _)
  when is_binary(FromServer), is_binary(ToServer),
       new =:= PType orelse is_tuple(PType) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(FromServer),
    Opts = get_s2s_out_config(HostType),
    #{shaper := ShaperName, max_stanza_size := MaxStanzaSize} = Opts,
    {ok, Parser} = exml_stream:new_parser([{max_element_size, MaxStanzaSize}]),
    Shaper = mongoose_shaper:new(ShaperName),
    case open_socket(HostType, ToServer, Opts) of
        {error, _Reason} ->
            {keep_state_and_data, wait_before_retry_timeout(Opts)};
        Socket ->
            Data = #s2s_data{host_type = HostType,
                             myname = FromServer,
                             remote_server = ToServer,
                             socket = Socket,
                             parser = Parser,
                             shaper = Shaper,
                             opts = Opts,
                             process_type = PType},
            send_xml(Data, stream_header(Data)),
            {next_state, {wait_for_stream, stream_start}, Data, state_timeout(Data)}
    end;
handle_event(internal, #xmlstreamstart{attrs = Attrs}, {wait_for_stream, StreamState}, Data) ->
    handle_stream_start(Data, Attrs, StreamState);

handle_event(internal, #xmlel{name = <<"stream:features">>} = El,
             {wait_for_features, StreamState}, Data) ->
    handle_features(Data, El, StreamState);
handle_event(internal, #xmlel{name = <<"success">>, attrs = #{<<"xmlns">> := ?NS_SASL}},
                wait_for_auth_result, Data) ->
    NewData = Data#s2s_data{streamid = mongoose_bin:gen_from_crypto()},
    send_xml(NewData, stream_header(NewData)),
    {next_state, {wait_for_stream, authenticated}, NewData, state_timeout(NewData)};
handle_event(internal, #xmlel{name = <<"failure">>, attrs = #{<<"xmlns">> := ?NS_SASL}},
             wait_for_auth_result, #s2s_data{myname = LocalDomain, remote_server = RemoteDomain}) ->
    mongoose_instrument:execute(s2s_auth_failed, #{},
                                #{local_domain => LocalDomain,
                                  remote_domain => RemoteDomain,
                                  direction => out, count => 1}),
    {stop, s2s_sasl_failure};

handle_event(internal,
             #xmlel{name = <<"proceed">>, attrs = #{<<"xmlns">> := ?NS_TLS}} = El,
             wait_for_starttls_proceed,
             #s2s_data{socket = TcpSocket, parser = Parser, opts = Opts} = Data) ->
    case mongoose_xmpp_socket:tcp_to_tls(TcpSocket, Opts, client) of
        {ok, TlsSocket} ->
            {ok, NewParser} = exml_stream:reset_parser(Parser),
            NewData = Data#s2s_data{socket = TlsSocket,
                                    parser = NewParser,
                                    streamid = mongoose_bin:gen_from_crypto()},
            send_xml(NewData, stream_header(NewData)),
            {next_state, {wait_for_stream, stream_start}, NewData, state_timeout(Opts)};
        {error, already_tls_connection} ->
            ErrorStanza = mongoose_xmpp_errors:bad_request(?MYLANG, <<"bad_config">>),
            send_xml(Data, jlib:make_error_reply(El, ErrorStanza)),
            {stop, {shutdown, starttls_error}};
        {error, Reason} ->
            {stop, {shutdown, Reason}}
    end;

handle_event(internal, #xmlel{name = <<"db:", _/binary>>} = El, wait_for_validation,
             #s2s_data{myname = LServer, remote_server = RemoteServer,
                       socket = Socket, opts = Opts, process_type = new} = Data) ->
    case mongoose_s2s_dialback:parse_validity(El) of
        {step_4, {LServer, RemoteServer}, _StreamID, true} ->
            case {mongoose_xmpp_socket:is_ssl(Socket), Opts} of
                {false, #{tls := #{mode := starttls_required}}} ->
                    {stop, {shutdown, tls_required_but_unavailable}};
                _ ->
                    {next_state, stream_established, Data, stream_timeout(Data)}
            end;
        {step_4, {LServer, RemoteServer}, _StreamID, false} ->
            {stop, {shutdown, invalid_dialback_key}};
        _ ->
            {keep_state_and_data, state_timeout(Data)}
    end;
handle_event(internal, #xmlel{name = <<"db:", _/binary>>} = El, wait_for_validation,
             #s2s_data{myname = LServer, remote_server = RemoteServer,
                       process_type = {verify, VPid, _Key, _SID}} = Data) ->
    case mongoose_s2s_dialback:parse_validity(El) of
        {step_3, {LServer, RemoteServer} = FromTo, _StreamID, IsValid} ->
            mongoose_s2s_in:send_validity_from_s2s_out(VPid, IsValid, FromTo),
            {stop, normal};
        _ ->
            {keep_state_and_data, state_timeout(Data)}
    end;

handle_event(internal, #xmlstreamend{}, _, Data) ->
    send_xml(Data, ?XML_STREAM_TRAILER),
    {stop, {shutdown, stream_end}};
handle_event(internal, {disconnect, Reason}, _, _) ->
    {stop, {shutdown, Reason}};
handle_event(info, {route, Acc}, stream_established, Data) ->
    send_xml(Data, mongoose_acc:element(Acc)),
    {keep_state_and_data, stream_timeout(Data)};
handle_event(info, {route, Acc}, bounce_and_disconnect, _) ->
    E = mongoose_xmpp_errors:remote_server_not_found(?MYLANG, <<"From s2s (waiting)">>),
    bounce_one(E, Acc),
    keep_state_and_data;
handle_event(info, {route, _}, _, _) ->
    {keep_state_and_data, postpone};
handle_event(info, {Tag, _, Payload} = SocketData, _, Data)
  when is_binary(Payload) andalso (Tag =:= tcp orelse Tag =:= ssl) ->
    handle_socket_data(Data, SocketData);
handle_event(info, {Tag, _}, _, _)
  when Tag =:= tcp_closed; Tag =:= ssl_closed ->
    {stop, {shutdown, Tag}};
handle_event(info, {Tag, _, Reason}, _, _)
  when Tag =:= tcp_error; Tag =:= ssl_error ->
    {stop, {shutdown, {Tag, Reason}}};
handle_event(cast, terminate_if_waiting_before_retry, connect, _) ->
    {stop, {shutdown, terminate_if_waiting_before_retry}};
handle_event(cast, terminate_if_waiting_before_retry, _, _) ->
    keep_state_and_data;
handle_event(cast, {exit, Reason}, _, {_, _}) ->
    {stop, {shutdown, Reason}};
handle_event(cast, {exit, system_shutdown}, _, #s2s_data{} = Data) ->
    Error = mongoose_xmpp_errors:system_shutdown(),
    send_xml(Data, Error),
    send_xml(Data, ?XML_STREAM_TRAILER),
    {stop, {shutdown, system_shutdown}};
handle_event(cast, {exit, Reason}, _, #s2s_data{} = Data)
  when is_binary(Reason) ->
    StreamConflict = mongoose_xmpp_errors:stream_conflict(?MYLANG, Reason),
    send_xml(Data, StreamConflict),
    send_xml(Data, ?XML_STREAM_TRAILER),
    {stop, {shutdown, Reason}};
handle_event(cast, {stop, Reason}, _, _) ->
    {stop, {shutdown, Reason}};
handle_event({call, From}, get_state_info, State, Data) ->
    Info = handle_get_state_info(Data, State),
    {keep_state_and_data, [{reply, From, Info}]};
handle_event(timeout, stream_timeout, _, _) ->
    {stop, {shutdown, s2s_out_stream_timeout}};
handle_event({timeout, activate_socket}, activate_socket, _, #s2s_data{socket = Socket}) ->
    mongoose_xmpp_socket:activate(Socket),
    keep_state_and_data;
handle_event({timeout, wait_before_retry}, wait_before_retry_timeout, connect, {From, To}) ->
    ejabberd_s2s:remove_connection({From, To}, self()),
    bounce_and_disconnect({From, To});
handle_event(state_timeout, state_timeout_termination, bounce_and_disconnect, _) ->
    {stop, {shutdown, state_timeout}};
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
terminate(_, _, #s2s_data{myname = MyName, remote_server = ToServer,
                          parser = Parser, socket = Socket, process_type = new}) ->
    ejabberd_s2s:remove_connection({MyName, ToServer}, self()),
    bounce_messages(mongoose_xmpp_errors:remote_server_not_found(?MYLANG, <<"Bounced by s2s">>)),
    exml_stream:free_parser(Parser),
    mongoose_xmpp_socket:close(Socket);
terminate(_Reason, _State, #s2s_data{parser = Parser, socket = Socket}) ->
    bounce_messages(mongoose_xmpp_errors:remote_server_not_found(?MYLANG, <<"Bounced by s2s">>)),
    exml_stream:free_parser(Parser),
    mongoose_xmpp_socket:close(Socket);
terminate(_Reason, _State, _FromTo) ->
    bounce_messages(mongoose_xmpp_errors:remote_server_not_found(?MYLANG, <<"Bounced by s2s">>)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This change of state triggers processing again postponed events ({route, Acc}),
%% followed by a disconnection.
-spec bounce_and_disconnect(ejabberd_s2s:fromto()) -> fsm_res().
bounce_and_disconnect({FromServer, _To} = Data) ->
    {ok, HostType} = mongoose_domain_api:get_host_type(FromServer),
    Opts = get_s2s_out_config(HostType),
    {next_state, bounce_and_disconnect, Data, state_timeout(Opts)}.

-spec open_socket(mongooseim:host_type(), jid:lserver(), options()) ->
    mongoose_xmpp_socket:socket() | {error, atom() | inet:posix()}.
open_socket(HostType, ToServer, Opts = #{connection_timeout := Timeout}) ->
    EnforceTls = is_tls_enforced(Opts),
    AddrList = mongoose_addr_list:get_addr_list(HostType, ToServer, EnforceTls),
    Fold = fun(Addr, {error, _}) ->
                   mongoose_xmpp_socket:connect(Addr, Opts, s2s, Timeout);
              (_, Socket) ->
                   Socket
           end,
    case lists:foldl(Fold, {error, badarg}, AddrList) of
        {error, Reason} ->
            ?LOG_DEBUG(#{what => s2s_out_failed, address_list => AddrList, reason => Reason}),
            {error, Reason};
        Socket ->
            Socket
    end.

-spec is_tls_enforced(options()) -> boolean().
is_tls_enforced(#{tls := #{mode := tls}}) ->
    true;
is_tls_enforced(_) ->
    false.

-spec handle_stream_start(data(), exml:attrs(), stream_state()) -> fsm_res().
handle_stream_start(#s2s_data{myname = MyName, remote_server = RemoteServer} = D0,
                    #{<<"xmlns">> := ?NS_SERVER,
                      <<"version">> := ?XMPP_VERSION,
                      <<"id">> := RemoteStreamID,
                      <<"from">> := From} = Attrs,
                    StreamState) ->
    MaybeDialback = maps:get(<<"xmlns:db">>, Attrs, undefined),
    case is_valid_from(From, RemoteServer) andalso
         is_valid_to(Attrs, MyName) andalso
         is_valid_dialback_ns(MaybeDialback) of
        true ->
            DialbackEnabled = ?NS_SERVER_DIALBACK =:= MaybeDialback,
            D1 = D0#s2s_data{remote_streamid = RemoteStreamID, dialback_enabled = DialbackEnabled},
            {next_state, {wait_for_features, StreamState}, D1, state_timeout(D1)};
        false ->
            send_xml(D0, mongoose_xmpp_errors:invalid_from()),
            {stop, {shutdown, invalid_from_to}, D0}
    end;
handle_stream_start(#s2s_data{} = Data, _, _) ->
    send_xml(Data, mongoose_xmpp_errors:invalid_namespace()),
    {stop, normal, Data}.

-spec is_valid_from(jid:server(), jid:lserver()) -> boolean().
is_valid_from(From, RemoteServer) ->
    jid:nameprep(From) =:= RemoteServer.

-spec is_valid_to(exml:attrs(), jid:lserver()) -> boolean().
is_valid_to(Attrs, MyName) ->
    case maps:get(<<"to">>, Attrs, true) of
        true -> true;
        To -> jid:nameprep(To) =:= MyName
    end.

-spec is_valid_dialback_ns(undefined | binary()) -> boolean().
is_valid_dialback_ns(undefined) -> true;
is_valid_dialback_ns(?NS_SERVER_DIALBACK) -> true;
is_valid_dialback_ns(_) -> false.

-spec handle_features(data(), exml:element(), stream_state()) -> fsm_res().
handle_features(#s2s_data{socket = Socket} = Data, #xmlel{children = Children}, StreamState) ->
    IsSsl = mongoose_xmpp_socket:is_ssl(Socket),
    InitAcc = #{sasl_external => false, starttls => false, is_ssl => IsSsl},
    Input = lists:foldl(fun parse_auth_and_tls/2, InitAcc, Children),
    handle_parsed_features(Data, Input, StreamState).

-spec parse_auth_and_tls(exml:element(), features_before_auth()) -> features_before_auth().
parse_auth_and_tls(#xmlel{name = <<"mechanisms">>,
                          attrs = #{<<"xmlns">> := ?NS_SASL},
                          children = Els1}, Acc) ->
    Pred = fun(Child) ->
                   #xmlel{name = <<"mechanism">>,
                          children = [#xmlcdata{content = <<"EXTERNAL">>}]} =:= Child
           end,
    Acc#{sasl_external => lists:any(Pred, Els1)};
parse_auth_and_tls(#xmlel{name = <<"starttls">>,
                          attrs = #{<<"xmlns">> := ?NS_TLS},
                          children = [#xmlcdata{content = <<"required">>}]}, Acc) ->
    Acc#{starttls => required};
parse_auth_and_tls(#xmlel{name = <<"starttls">>,
                          attrs = #{<<"xmlns">> := ?NS_TLS}}, Acc) ->
    Acc#{starttls => optional};
parse_auth_and_tls(_, Acc) ->
    Acc.

-spec handle_parsed_features(data(), features_before_auth(), stream_state()) -> fsm_res().
handle_parsed_features(#s2s_data{opts = #{tls := _}} = Data,
                       #{sasl_external := _, starttls := required, is_ssl := false},
                       _) ->
    send_xml(Data, starttls()),
    {next_state, wait_for_starttls_proceed, Data, state_timeout(Data)};
handle_parsed_features(#s2s_data{opts = #{tls := _}} = Data,
                       #{sasl_external := _, starttls := optional, is_ssl := false},
                       _) ->
    send_xml(Data, starttls()),
    {next_state, wait_for_starttls_proceed, Data, state_timeout(Data)};
handle_parsed_features(#s2s_data{} = Data,
                       #{sasl_external := false, starttls := false, is_ssl := _},
                       authenticated) ->
    {next_state, stream_established, Data#s2s_data{}};
handle_parsed_features(#s2s_data{process_type = new} = Data,
                       #{sasl_external := true, starttls := _, is_ssl := true},
                       stream_start) ->
    Elem = #xmlel{name = <<"auth">>,
                  attrs = #{<<"xmlns">> => ?NS_SASL, <<"mechanism">> => <<"EXTERNAL">>},
                  children = [#xmlcdata{content = base64:encode(Data#s2s_data.myname)}]},
    send_xml(Data, Elem),
    {next_state, wait_for_auth_result, Data, state_timeout(Data)};
handle_parsed_features(#s2s_data{dialback_enabled = true} = Data,
                       #{sasl_external := _, starttls := _, is_ssl := _},
                       _) ->
    send_dialback_request(Data);
handle_parsed_features(#s2s_data{} = Data,
                       #{sasl_external := _, starttls := _, is_ssl := _},
                       _) ->
    {next_state, connect, Data, wait_before_retry_timeout(Data)}.

-spec send_dialback_request(data()) -> fsm_res().
send_dialback_request(#s2s_data{host_type = HostType,
                                myname = LServer,
                                remote_server = RemoteServer,
                                process_type = new} = Data) ->
    FromTo = {LServer, RemoteServer},
    Key1 = ejabberd_s2s:key(HostType, FromTo, Data#s2s_data.remote_streamid),
    %% Initiating server sends dialback key
    send_xml(Data, mongoose_s2s_dialback:step_1(FromTo, Key1)),
    {next_state, wait_for_validation, Data, state_timeout(Data)};
send_dialback_request(#s2s_data{myname = LServer,
                                remote_server = RemoteServer,
                                process_type = {verify, _Pid, Key, SID}} = Data) ->
    %% This is an outbound s2s connection created at step 1 in mongoose_s2s_in:handle_dialback/3
    %% Its purpose is just to verify the dialback procedure and can be closed afterwards.
    send_xml(Data, mongoose_s2s_dialback:step_2({LServer, RemoteServer}, Key, SID)),
    {next_state, wait_for_validation, Data, state_timeout(Data)}.

-spec bounce_messages(exml:element()) -> ok.
bounce_messages(Error) ->
    receive
        {route, Acc} ->
            bounce_one(Error, Acc),
            bounce_messages(Error);
        _ ->
            bounce_messages(Error)
    after 0 -> ok
    end.

%% @doc Bounce a single message (xmlel)
-spec bounce_one(exml:element(), mongoose_acc:t()) -> mongoose_acc:t().
bounce_one(Error, Acc) ->
    case mongoose_acc:stanza_type(Acc) of
        <<"error">> -> Acc;
        <<"result">> -> Acc;
        _ ->
            {From, To, Packet} = mongoose_acc:packet(Acc),
            {Acc1, Err} = jlib:make_error_reply(Acc, Packet, Error),
            ejabberd_router:route(To, From, Acc1, Err)
    end.

-spec send_xml(data(), exml_stream:element()) -> maybe_ok().
send_xml(#s2s_data{socket = Socket} = Data, Elem) ->
    case Elem of
        #xmlel{} -> execute_element_event(Elem, Data, xmpp_element_out);
        _ -> ok
    end,
    mongoose_xmpp_socket:send_xml(Socket, Elem).

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
    case exml_stream:parse(Parser, Packet) of
        {error, Reason} ->
            NextEvent = {next_event, internal, #xmlstreamerror{name = Reason}},
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

%% This will trigger if after a while the gen_statem state has not changed.
%% Should be enabled for all transitions except into `stream_established`.
-spec state_timeout(data() | options()) -> {state_timeout, timeout(), state_timeout_termination}.
state_timeout(#s2s_data{opts = Opts}) ->
    state_timeout(Opts);
state_timeout(#{state_timeout := Timeout}) ->
    {state_timeout, Timeout, state_timeout_termination}.

%% This will trigger _only_ during `stream_established`, to garbage-collect unused connections.
-spec stream_timeout(data() | options()) -> {timeout, timeout(), stream_timeout}.
stream_timeout(#s2s_data{opts = Opts}) ->
    stream_timeout(Opts);
stream_timeout(#{stream_timeout := Timeout}) ->
    {timeout, Timeout, stream_timeout}.

%% Sockets failed to connect entirely
%% The initial delay is random between 1 and `max_retry_delay` seconds
-spec wait_before_retry_timeout(data() | options()) ->
    {{timeout, wait_before_retry}, timeout(), wait_before_retry_timeout}.
wait_before_retry_timeout(#s2s_data{opts = Opts}) ->
    wait_before_retry_timeout(Opts);
wait_before_retry_timeout(#{max_retry_delay := MaxRetryDelay}) ->
    Delay = 999 + rand:uniform(timer:seconds(MaxRetryDelay) - 999),
    {{timeout, wait_before_retry}, Delay, wait_before_retry_timeout}.

-spec handle_get_state_info(data(), state()) -> connection_info().
handle_get_state_info(#s2s_data{socket = Socket, opts = Opts} = Data, State) ->
    {Addr, Port} = mongoose_xmpp_socket:get_ip(Socket),
    #{pid => self(),
      direction => out,
      state_name => State,
      addr => Addr,
      port => Port,
      streamid => Data#s2s_data.streamid,
      tls => maps:is_key(tls, Opts),
      tls_enabled => mongoose_xmpp_socket:is_ssl(Socket),
      tls_options => maps:get(tls, Opts, #{}),
      authenticated => stream_established =:= State,
      shaper => Data#s2s_data.shaper,
      dialback_enabled => Data#s2s_data.dialback_enabled,
      server => Data#s2s_data.remote_server,
      myname => Data#s2s_data.myname,
      process_type => Data#s2s_data.process_type}.

-spec get_s2s_out_config(mongooseim:host_type()) -> options().
get_s2s_out_config(HostType) ->
    mongoose_config:get_opt([{s2s, HostType}, outgoing]).

-spec stream_header(data()) -> exml_stream:start().
stream_header(#s2s_data{myname = LServer, remote_server = Remote, streamid = Id, socket = Socket}) ->
    Attrs = #{<<"xmlns:stream">> => ?NS_STREAM,
              <<"xmlns">> => ?NS_SERVER,
              <<"xmlns:db">> => ?NS_SERVER_DIALBACK,
              <<"version">> => ?XMPP_VERSION,
              <<"xml:lang">> => ?MYLANG,
              <<"to">> => Remote,
              <<"id">> => Id},
    %% RFC6120 §4.7.1:
    %% Because a server is a "public entity" on the XMPP network, it MUST include
    %% the 'from' attribute after the confidentiality and integrity of the stream
    %% are protected via TLS or an equivalent security layer.
    case mongoose_xmpp_socket:is_ssl(Socket) of
        true ->
            #xmlstreamstart{name = <<"stream:stream">>, attrs = Attrs#{<<"from">> => LServer}};
        false ->
            #xmlstreamstart{name = <<"stream:stream">>, attrs = Attrs}
    end.

-spec starttls() -> exml:element().
starttls() ->
    #xmlel{name = <<"starttls">>, attrs = #{<<"xmlns">> => ?NS_TLS}}.

%% Instrumentation helpers

-spec execute_element_event(exml:element(), data(), mongoose_instrument:event_name()) -> ok.
execute_element_event(Element, #s2s_data{host_type = HostType, myname = LServer}, EventName) ->
    Metadata = #{lserver => LServer, pid => self(), module => ?MODULE},
    mongoose_instrument_xmpp:execute_element_event(EventName, s2s, HostType, Element, Metadata).
