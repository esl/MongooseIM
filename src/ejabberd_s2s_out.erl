%%%----------------------------------------------------------------------
%%% File    : ejabberd_s2s_out.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage outgoing server-to-server connections
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

-module(ejabberd_s2s_out).
-author('alexey@process-one.net').
-behaviour(p1_fsm).

% TODO this should be in a separate module after feature/cets is merged
-xep([{xep, 220}, {version, "1.1.1"}]).

%% External exports
-export([start/2,
         start_link/2,
         start_connection/1,
         stop_connection/1,
         terminate_if_waiting_delay/1]).

%% p1_fsm callbacks (same as gen_fsm)
-export([init/1,
         open_socket/2,
         wait_for_stream/2,
         wait_for_validation/2,
         wait_for_features/2,
         wait_for_auth_result/2,
         wait_for_starttls_proceed/2,
         reopen_socket/2,
         wait_before_retry/2,
         stream_established/2,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         print_state/1,
         code_change/4]).

-export_type([connection_info/0]).

-ignore_xref([open_socket/2, print_state/1,
              reopen_socket/2, start_link/2, stream_established/2,
              wait_before_retry/2, wait_for_auth_result/2,
              wait_for_features/2, wait_for_starttls_proceed/2, wait_for_stream/2,
              wait_for_stream/2, wait_for_validation/2]).

-type verify_requester() :: false | {S2SIn :: pid(), Key :: ejabberd_s2s:s2s_dialback_key(), SID :: ejabberd_s2s:stream_id()}.

-include("mongoose.hrl").
-include("jlib.hrl").

-record(state, {socket,
                streamid                :: ejabberd_s2s:stream_id() | undefined,
                remote_streamid = <<>>  :: ejabberd_s2s:stream_id(),
                use_v10                 :: boolean(),
                tls = false             :: boolean(),
                tls_required = false    :: boolean(),
                tls_enabled = false     :: boolean(),
                tls_options             :: mongoose_tls:options(),
                authenticated = false   :: boolean(),
                dialback_enabled = true :: boolean(),
                try_auth = true         :: boolean(),
                from_to                 :: ejabberd_s2s:fromto(),
                myname                  :: jid:lserver(),
                server                  :: jid:lserver(),
                queue                   :: element_queue(),
                host_type               :: mongooseim:host_type(),
                delay_to_retry          :: non_neg_integer() | undefined,
                is_registered = false   :: boolean(),
                verify = false          :: verify_requester(),
                timer                   :: reference()
              }).
-type state() :: #state{}.

-type connection_info() ::
    #{pid => pid(),
      direction => out,
      statename => statename(),
      addr => unknown | inet:ip_address(),
      port => unknown | inet:port_number(),
      streamid => ejabberd_s2s:stream_id() | undefined,
      use_v10 => boolean(),
      tls => boolean(),
      tls_required => boolean(),
      tls_enabled => boolean(),
      tls_options => mongoose_tls:options(),
      authenticated => boolean(),
      dialback_enabled => boolean(),
      try_auth => boolean(),
      myname => jid:lserver(),
      server => jid:lserver(),
      delay_to_retry => undefined | non_neg_integer(),
      verify => verify_requester()}.

-type element_queue() :: queue:queue(#xmlel{}).
-type statename() :: open_socket
                   | wait_for_stream
                   | wait_for_features
                   | wait_for_auth_result
                   | wait_for_starttls_proceed
                   | wait_for_validation
                   | wait_before_retry.

%% FSM handler return value
-type fsm_return() :: {'stop', Reason :: 'normal', state()}
                    | {'next_state', statename(), state()}
                    | {'next_state', statename(), state(), Timeout :: integer()}.

-type addr() :: #{ip_tuple := inet:ip_address(),
                  port := inet:port_number(),
                  type := inet | inet6}.

%%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

-define(FSMTIMEOUT, 30000).

%% We do not block on send anymore.
-define(TCP_SEND_TIMEOUT, 15000).

-define(STREAM_HEADER(From, To, Other),
        <<"<?xml version='1.0'?>",
          "<stream:stream "
          "xmlns:stream='http://etherx.jabber.org/streams' "
          "xmlns='jabber:server' "
          "xmlns:db='jabber:server:dialback' "
          "from='", (From)/binary, "' ",
          "to='", (To)/binary, "' ",
          (Other)/binary, ">">>
       ).

-define(SOCKET_DEFAULT_RESULT, {error, badarg}).


-define(CLOSE_GENERIC(StateName, Reason, StateData),
    ?LOG_INFO(#{what => s2s_out_closing, text => <<"Closing s2s connection">>,
                state_name => StateName, reason => Reason,
                myname => StateData#state.myname, server => StateData#state.server}),
    {stop, normal, StateData}).

-define(CLOSE_GENERIC(StateName, Reason, El, StateData),
    ?LOG_INFO(#{what => s2s_out_closing, text => <<"Closing s2s connection on stanza">>,
                state_name => StateName, reason => Reason, exml_packet => El,
                myname => StateData#state.myname, server => StateData#state.server}),
    {stop, normal, StateData}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-spec start(ejabberd_s2s:fromto(), _) -> {'error', _} | {'ok', 'undefined' | pid()} | {'ok', 'undefined' | pid(), _}.
start(FromTo, Type) ->
    supervisor:start_child(ejabberd_s2s_out_sup, [FromTo, Type]).

-spec start_link(ejabberd_s2s:fromto(), _) -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(FromTo, Type) ->
    p1_fsm:start_link(ejabberd_s2s_out, [FromTo, Type],
                      fsm_limit_opts() ++ ?FSMOPTS).

start_connection(Pid) ->
    p1_fsm:send_event(Pid, init).

stop_connection(Pid) ->
    p1_fsm:send_event(Pid, closed).

%%%----------------------------------------------------------------------
%%% Callback functions from p1_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%%----------------------------------------------------------------------
-spec init(list()) -> {'ok', 'open_socket', state()}.
init([{From, Server} = FromTo, Type]) ->
    process_flag(trap_exit, true),
    ?LOG_DEBUG(#{what => s2s_out_started,
                 text => <<"New outgoing s2s connection">>,
                 from => From, server => Server, type => Type}),
    {ok, HostType} = mongoose_domain_api:get_host_type(From),
    {TLS, TLSRequired} = case mongoose_config:get_opt([{s2s, HostType}, use_starttls]) of
              UseTls when (UseTls==false) ->
                  {false, false};
              UseTls when (UseTls==true) or (UseTls==optional) ->
                  {true, false};
              UseTls when (UseTls==required) or (UseTls==required_trusted) ->
                  {true, true}
          end,
    UseV10 = TLS,
    {IsRegistered, Verify} = case Type of
                        new ->
                            {true, false};
                        {verify, Pid, Key, SID} ->
                            start_connection(self()),
                            {false, {Pid, Key, SID}}
                    end,
    Timer = erlang:start_timer(mongoose_s2s_lib:timeout(), self(), []),
    {ok, open_socket, #state{use_v10 = UseV10,
                             tls = TLS,
                             tls_required = TLSRequired,
                             tls_options = tls_options(HostType),
                             queue = queue:new(),
                             from_to = FromTo,
                             myname = From,
                             host_type = HostType,
                             server = Server,
                             is_registered = IsRegistered,
                             verify = Verify,
                             timer = Timer}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
-spec open_socket(_, state()) -> fsm_return().
open_socket(init, StateData = #state{host_type = HostType}) ->
    log_s2s_out(StateData#state.is_registered,
                StateData#state.myname,
                StateData#state.server,
                StateData#state.tls),
    ?LOG_DEBUG(#{what => s2s_open_socket,
                 myname => StateData#state.myname,
                 server => StateData#state.server,
                 is_registered => StateData#state.is_registered,
                 verify => StateData#state.verify}),
    AddrList = get_addr_list(HostType, StateData#state.server),
    case lists:foldl(fun(_, {ok, Socket}) ->
                             {ok, Socket};
                        (#{ip_tuple := Addr, port := Port, type := Type}, _) ->
                             open_socket2(HostType, Type, Addr, Port)
                     end, ?SOCKET_DEFAULT_RESULT, AddrList) of
        {ok, Socket} ->
            Version = case StateData#state.use_v10 of
                          true -> <<" version='1.0'">>;
                          false -> <<"">>
                      end,
            NewStateData = StateData#state{socket = Socket,
                                           tls_enabled = false,
                                           streamid = new_id()},
            send_text(NewStateData,
                      ?STREAM_HEADER(StateData#state.myname, StateData#state.server, Version)),
            {next_state, wait_for_stream, NewStateData, ?FSMTIMEOUT};
        {error, Reason} ->
            ?LOG_WARNING(#{what => s2s_out_failed, reason => Reason,
                           text => <<"Outgoing s2s connection failed (remote server not found)">>,
                           myname => StateData#state.myname, server => StateData#state.server}),
            wait_before_reconnect(StateData)
    end;
open_socket(closed, StateData) ->
    ?CLOSE_GENERIC(open_socket, closed, StateData);
open_socket(timeout, StateData) ->
    ?CLOSE_GENERIC(open_socket, timeout, StateData);
open_socket(_, StateData) ->
    {next_state, open_socket, StateData}.

-spec open_socket2(mongooseim:host_type(), inet | inet6, inet:ip_address(), inet:port_number()) ->
          {'error', _} | {'ok', _}.
open_socket2(HostType, Type, Addr, Port) ->
    ?LOG_DEBUG(#{what => s2s_out_connecting,
                 address => Addr, port => Port}),
    Timeout = outgoing_s2s_timeout(HostType),
    SockOpts = [binary,
                {packet, 0},
                {send_timeout, ?TCP_SEND_TIMEOUT},
                {send_timeout_close, true},
                {active, false},
                Type],

    case (catch mongoose_transport:connect(s2s, Addr, Port, SockOpts, Timeout)) of
        {ok, _Socket} = R -> R;
        {error, Reason} = R ->
            ?LOG_DEBUG(#{what => s2s_out_failed,
                         address => Addr, port => Port, reason => Reason}),
            R;
        {'EXIT', Reason} ->
            ?LOG_DEBUG(#{what => s2s_out_failed,
                         text => <<"Failed to open s2s socket because of crashing">>,
                         address => Addr, port => Port, reason => Reason}),
            {error, Reason}
    end.

%%----------------------------------------------------------------------

-spec wait_for_stream(ejabberd:xml_stream_item(), state()) -> fsm_return().
wait_for_stream({xmlstreamstart, _Name, Attrs}, StateData0) ->
    RemoteStreamID = xml:get_attr_s(<<"id">>, Attrs),
    StateData = StateData0#state{remote_streamid = RemoteStreamID},
    case {xml:get_attr_s(<<"xmlns">>, Attrs),
          xml:get_attr_s(<<"xmlns:db">>, Attrs),
          xml:get_attr_s(<<"version">>, Attrs) == <<"1.0">>} of
        {<<"jabber:server">>, <<"jabber:server:dialback">>, false} ->
            send_dialback_request(StateData);
        {<<"jabber:server">>, <<"jabber:server:dialback">>, true} when
        StateData#state.use_v10 ->
            {next_state, wait_for_features, StateData, ?FSMTIMEOUT};
        %% Clause added to handle Tigase's workaround for an old ejabberd bug:
        {<<"jabber:server">>, <<"jabber:server:dialback">>, true} when
        not StateData#state.use_v10 ->
            send_dialback_request(StateData);
        {<<"jabber:server">>, <<"">>, true} when StateData#state.use_v10 ->
            {next_state, wait_for_features, StateData#state{dialback_enabled = false}, ?FSMTIMEOUT};
        {NSProvided, DB, _} ->
            send_element(StateData, mongoose_xmpp_errors:invalid_namespace()),
            ?LOG_INFO(#{what => s2s_out_closing,
                        text => <<"Closing s2s connection: (invalid namespace)">>,
                        namespace_provided => NSProvided,
                        namespace_expected => <<"jabber:server">>,
                        xmlns_dialback_provided => DB,
                        all_attributes => Attrs,
                        myname => StateData#state.myname, server => StateData#state.server}),
            {stop, normal, StateData}
    end;
wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_element(StateData, mongoose_xmpp_errors:xml_not_well_formed()),
    send_text(StateData, ?STREAM_TRAILER),
    ?CLOSE_GENERIC(wait_for_stream, xmlstreamerror, StateData);
wait_for_stream({xmlstreamend, _Name}, StateData) ->
    ?CLOSE_GENERIC(wait_for_stream, xmlstreamend, StateData);
wait_for_stream(timeout, StateData) ->
    ?CLOSE_GENERIC(wait_for_stream, timeout, StateData);
wait_for_stream(closed, StateData) ->
    ?CLOSE_GENERIC(wait_for_stream, closed, StateData).


-spec wait_for_validation(ejabberd:xml_stream_item(), state()) -> fsm_return().
wait_for_validation({xmlstreamelement, El}, StateData = #state{from_to = FromTo}) ->
    case mongoose_s2s_dialback:parse_validity(El) of
        {step_3, FromTo, StreamID, IsValid} ->
            ?LOG_DEBUG(#{what => s2s_receive_verify,
                         from_to => FromTo, stream_id => StreamID, is_valid => IsValid}),
            case StateData#state.verify of
                false ->
                    %% This is unexpected condition.
                    %% We've received step_3 reply, but there is no matching outgoing connection.
                    %% We could close the connection here.
                    next_state(wait_for_validation, StateData);
                {Pid, _Key, _SID} ->
                    ejabberd_s2s_in:send_validity_from_s2s_out(Pid, IsValid, FromTo),
                    next_state(wait_for_validation, StateData)
            end;
        {step_4, FromTo, StreamID, IsValid} ->
            ?LOG_DEBUG(#{what => s2s_receive_result,
                         from_to => FromTo, stream_id => StreamID, is_valid => IsValid}),
            #state{tls_enabled = Enabled, tls_required = Required} = StateData,
            case IsValid of
                true when (Enabled==true) or (Required==false) ->
                    %% Initiating server receives valid verification result from receiving server (Step 4)
                    send_queue(StateData, StateData#state.queue),
                    ?LOG_INFO(#{what => s2s_out_connected,
                                text => <<"New outgoing s2s connection established">>,
                                tls_enabled => StateData#state.tls_enabled,
                                myname => StateData#state.myname, server => StateData#state.server}),
                    {next_state, stream_established,
                     StateData#state{queue = queue:new()}};
                true when (Enabled==false) and (Required==true) ->
                    %% TODO: bounce packets
                    ?CLOSE_GENERIC(wait_for_validation, tls_required_but_unavailable, El, StateData);
                _ ->
                    %% TODO: bounce packets
                    ?CLOSE_GENERIC(wait_for_validation, invalid_dialback_key, El, StateData)
            end;
        false ->
            {next_state, wait_for_validation, StateData, ?FSMTIMEOUT*3}
    end;
wait_for_validation({xmlstreamend, _Name}, StateData) ->
    ?CLOSE_GENERIC(wait_for_validation, xmlstreamend, StateData);
wait_for_validation({xmlstreamerror, _}, StateData) ->
    send_element(StateData, mongoose_xmpp_errors:xml_not_well_formed()),
    send_text(StateData, ?STREAM_TRAILER),
    ?CLOSE_GENERIC(wait_for_validation, xmlstreamerror, StateData);
wait_for_validation(timeout, #state{verify = {VPid, VKey, SID}} = StateData)
  when is_pid(VPid) and is_binary(VKey) and is_binary(SID) ->
    %% This is an auxiliary s2s connection for dialback.
    %% This timeout is normal and doesn't represent a problem.
    ?LOG_INFO(#{what => s2s_out_validation_timeout,
                text => <<"Timeout in verify outgoing s2s connection. Stopping">>,
                myname => StateData#state.myname, server => StateData#state.server}),
    {stop, normal, StateData};
wait_for_validation(timeout, StateData) ->
    ?LOG_INFO(#{what => s2s_out_connect_timeout,
                text => <<"Connection timeout in outgoing s2s connection. Stopping">>,
                myname => StateData#state.myname, server => StateData#state.server}),
    {stop, normal, StateData};
wait_for_validation(closed, StateData) ->
    ?LOG_INFO(#{what => s2s_out_validation_closed,
                text => <<"Connection closed when waiting for validation in outgoing s2s connection. Stopping">>,
                myname => StateData#state.myname, server => StateData#state.server}),
    {stop, normal, StateData}.


-spec wait_for_features(ejabberd:xml_stream_item(), state()) -> fsm_return().
wait_for_features({xmlstreamelement, El}, StateData) ->
    case El of
        #xmlel{name = <<"stream:features">>, children = Els} ->
            {SASLEXT, StartTLS, StartTLSRequired} =
                lists:foldl(
                  fun(#xmlel{name = <<"mechanisms">>, attrs = Attrs1,
                             children = Els1} = _El1, Acc) ->
                          Attr =  xml:get_attr_s(<<"xmlns">>, Attrs1),
                          get_acc_with_new_sext(Attr, Els1, Acc);
                     (#xmlel{name = <<"starttls">>, attrs = Attrs1} = El1, Acc) ->
                          Attr = xml:get_attr_s(<<"xmlns">>, Attrs1),
                          get_acc_with_new_tls(Attr, El1, Acc);
                     (_, Acc) ->
                          Acc
                  end, {false, false, false}, Els),
            handle_parsed_features({SASLEXT, StartTLS, StartTLSRequired, StateData});
        _ ->
            send_element(StateData, mongoose_xmpp_errors:bad_format()),
            send_text(StateData, ?STREAM_TRAILER),
            ?CLOSE_GENERIC(wait_for_features, bad_format, El, StateData)
    end;
wait_for_features({xmlstreamend, _Name}, StateData) ->
    ?CLOSE_GENERIC(wait_for_features, xmlstreamend, StateData);
wait_for_features({xmlstreamerror, _}, StateData) ->
    send_element(StateData, mongoose_xmpp_errors:xml_not_well_formed()),
    send_text(StateData, ?STREAM_TRAILER),
    ?CLOSE_GENERIC(wait_for_features, xmlstreamerror, StateData);
wait_for_features(timeout, StateData) ->
    ?CLOSE_GENERIC(wait_for_features, timeout, StateData);
wait_for_features(closed, StateData) ->
    ?CLOSE_GENERIC(wait_for_features, closed, StateData).


-spec wait_for_auth_result(ejabberd:xml_stream_item(), state()) -> fsm_return().
wait_for_auth_result({xmlstreamelement, El}, StateData) ->
    case El of
        #xmlel{name = <<"success">>, attrs = Attrs} ->
            case xml:get_attr_s(<<"xmlns">>, Attrs) of
                ?NS_SASL ->
                    ?LOG_DEBUG(#{what => s2s_auth_success,
                                 myname => StateData#state.myname,
                                 server => StateData#state.server}),
                    send_text(StateData,
                              ?STREAM_HEADER(StateData#state.myname, StateData#state.server,
                                              <<" version='1.0'">>)),
                    {next_state, wait_for_stream,
                     StateData#state{streamid = new_id(),
                                     authenticated = true
                                    }, ?FSMTIMEOUT};
                _ ->
                    send_element(StateData, mongoose_xmpp_errors:bad_format()),
                    send_text(StateData, ?STREAM_TRAILER),
                    ?CLOSE_GENERIC(wait_for_auth_result, bad_format, El, StateData)
            end;
        #xmlel{name = <<"failure">>, attrs = Attrs} ->
            case xml:get_attr_s(<<"xmlns">>, Attrs) of
                ?NS_SASL ->
                    ?LOG_WARNING(#{what => s2s_auth_failure,
                                   text => <<"Received failure result in ejabberd_s2s_out. Restarting">>,
                                   myname => StateData#state.myname,
                                   server => StateData#state.server}),
                    mongoose_transport:close(StateData#state.socket),
                    {next_state, reopen_socket,
                     StateData#state{socket = undefined}, ?FSMTIMEOUT};
                _ ->
                    send_element(StateData, mongoose_xmpp_errors:bad_format()),
                    send_text(StateData, ?STREAM_TRAILER),
                    ?CLOSE_GENERIC(wait_for_auth_result, bad_format, El, StateData)
            end;
        _ ->
            send_element(StateData, mongoose_xmpp_errors:bad_format()),
            send_text(StateData, ?STREAM_TRAILER),
            ?CLOSE_GENERIC(wait_for_auth_result, bad_format, El, StateData)
    end;
wait_for_auth_result({xmlstreamend, _Name}, StateData) ->
    ?CLOSE_GENERIC(wait_for_auth_result, xmlstreamend, StateData);
wait_for_auth_result({xmlstreamerror, _}, StateData) ->
    send_element(StateData, mongoose_xmpp_errors:xml_not_well_formed()),
    send_text(StateData, ?STREAM_TRAILER),
    ?CLOSE_GENERIC(wait_for_auth_result, xmlstreamerror, StateData);
wait_for_auth_result(timeout, StateData) ->
    ?CLOSE_GENERIC(wait_for_auth_result, timeout, StateData);
wait_for_auth_result(closed, StateData) ->
    ?CLOSE_GENERIC(wait_for_auth_result, closed, StateData).


-spec wait_for_starttls_proceed(ejabberd:xml_stream_item(), state()) -> fsm_return().
wait_for_starttls_proceed({xmlstreamelement, El}, StateData) ->
    case El of
        #xmlel{name = <<"proceed">>, attrs = Attrs} ->
            case xml:get_attr_s(<<"xmlns">>, Attrs) of
                ?NS_TLS ->
                    ?LOG_DEBUG(#{what => s2s_starttls,
                                 myname => StateData#state.myname,
                                 server => StateData#state.server}),
                    TLSSocket = mongoose_transport:connect_tls(StateData#state.socket,
                                                               StateData#state.tls_options),
                    NewStateData = StateData#state{socket = TLSSocket,
                                                   streamid = new_id(),
                                                   tls_enabled = true},
                    send_text(NewStateData,
                              ?STREAM_HEADER(StateData#state.myname, StateData#state.server,
                                <<" version='1.0'">>)),
                    {next_state, wait_for_stream, NewStateData, ?FSMTIMEOUT};
                _ ->
                    send_element(StateData, mongoose_xmpp_errors:bad_format()),
                    send_text(StateData, ?STREAM_TRAILER),
                    ?CLOSE_GENERIC(wait_for_auth_result, bad_format, El, StateData)
            end;
        _ ->
            ?CLOSE_GENERIC(wait_for_auth_result, bad_format, El, StateData)
    end;
wait_for_starttls_proceed({xmlstreamend, _Name}, StateData) ->
    ?CLOSE_GENERIC(wait_for_starttls_proceed, xmlstreamend, StateData);
wait_for_starttls_proceed({xmlstreamerror, _}, StateData) ->
    send_element(StateData, mongoose_xmpp_errors:xml_not_well_formed()),
    send_text(StateData, ?STREAM_TRAILER),
    ?CLOSE_GENERIC(wait_for_starttls_proceed, xmlstreamerror, StateData);
wait_for_starttls_proceed(timeout, StateData) ->
    ?CLOSE_GENERIC(wait_for_starttls_proceed, timeout, StateData);
wait_for_starttls_proceed(closed, StateData) ->
    ?CLOSE_GENERIC(wait_for_starttls_proceed, closed, StateData).


-spec reopen_socket(ejabberd:xml_stream_item(), state()) -> fsm_return().
reopen_socket({xmlstreamelement, _El}, StateData) ->
    {next_state, reopen_socket, StateData, ?FSMTIMEOUT};
reopen_socket({xmlstreamend, _Name}, StateData) ->
    {next_state, reopen_socket, StateData, ?FSMTIMEOUT};
reopen_socket({xmlstreamerror, _}, StateData) ->
    {next_state, reopen_socket, StateData, ?FSMTIMEOUT};
reopen_socket(timeout, StateData) ->
    ?CLOSE_GENERIC(reopen_socket, timeout, StateData);
reopen_socket(closed, StateData) ->
    p1_fsm:send_event(self(), init),
    {next_state, open_socket, StateData, ?FSMTIMEOUT}.


%% @doc This state is use to avoid reconnecting to often to bad sockets
-spec wait_before_retry(ejabberd:xml_stream_item(), state()) -> fsm_return().
wait_before_retry(_Event, StateData) ->
    {next_state, wait_before_retry, StateData, ?FSMTIMEOUT}.

-spec stream_established(ejabberd:xml_stream_item(), state()) -> fsm_return().
stream_established({xmlstreamelement, El}, StateData = #state{from_to = FromTo}) ->
    ?LOG_DEBUG(#{what => s2s_out_stream_established, exml_packet => El,
                 myname => StateData#state.myname, server => StateData#state.server}),
    case mongoose_s2s_dialback:parse_validity(El) of
        {step_3, FromTo, StreamID, IsValid} ->
            ?LOG_DEBUG(#{what => s2s_recv_verify,
                         from_to => FromTo, stream_id => StreamID, is_valid => IsValid,
                         myname => StateData#state.myname, server => StateData#state.server}),
            case StateData#state.verify of
                {VPid, _VKey, _SID} ->
                    ejabberd_s2s_in:send_validity_from_s2s_out(VPid, IsValid, FromTo);
                _ ->
                    ok
            end;
        {step_4, _FromTo, _StreamID, _IsValid} ->
            ok;
        false ->
            ok
    end,
    {next_state, stream_established, StateData};
stream_established({xmlstreamend, _Name}, StateData) ->
    ?CLOSE_GENERIC(stream_established, xmlstreamend, StateData);
stream_established({xmlstreamerror, _}, StateData) ->
    send_element(StateData, mongoose_xmpp_errors:xml_not_well_formed()),
    send_text(StateData, ?STREAM_TRAILER),
    ?CLOSE_GENERIC(stream_established, xmlstreamerror, StateData);
stream_established(timeout, StateData) ->
    ?CLOSE_GENERIC(stream_established, timeout, StateData);
stream_established(closed, StateData) ->
    ?CLOSE_GENERIC(stream_established, closed, StateData).


%%----------------------------------------------------------------------
%% Func: StateName/3
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
%%state_name(Event, From, StateData) ->
%%    Reply = ok,
%%    {reply, Reply, state_name, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_event(_Event, StateName, StateData) ->
    next_state(StateName, StateData).

handle_sync_event(get_state_info, _From, StateName, StateData) ->
    {reply, handle_get_state_info(StateName, StateData), StateName, StateData};
handle_sync_event(_Event, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData, get_timeout_interval(StateName)}.


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info({send_element, Acc, El}, StateName, StateData) ->
    case StateName of
        stream_established ->
            cancel_timer(StateData#state.timer),
            Timer = erlang:start_timer(mongoose_s2s_lib:timeout(), self(), []),
            send_element(StateData, El),
            {next_state, StateName, StateData#state{timer = Timer}};
        %% In this state we bounce all message: We are waiting before
        %% trying to reconnect
        wait_before_retry ->
            bounce_element(Acc, El, mongoose_xmpp_errors:remote_server_not_found(<<"en">>, <<"From s2s">>)),
            {next_state, StateName, StateData};
        _ ->
            Q = queue:in({Acc, El}, StateData#state.queue),
            next_state(StateName, StateData#state{queue = Q})
    end;
handle_info({timeout, Timer, _}, wait_before_retry,
            #state{timer = Timer} = StateData) ->
    ?LOG_INFO(#{what => s2s_reconnect_delay_expired,
                text => <<"Reconnect delay expired: Will now retry to connect when needed.">>,
                myname => StateData#state.myname,
                server => StateData#state.server}),
    {stop, normal, StateData};
handle_info({timeout, Timer, _}, StateName,
            #state{timer = Timer} = StateData) ->
    ?CLOSE_GENERIC(StateName, s2s_out_timeout, StateData);
handle_info(terminate_if_waiting_before_retry, wait_before_retry, StateData) ->
    ?CLOSE_GENERIC(wait_before_retry, terminate_if_waiting_before_retry, StateData);
handle_info(terminate_if_waiting_before_retry, StateName, StateData) ->
    next_state(StateName, StateData);
handle_info(_, StateName, StateData) ->
    next_state(StateName, StateData).

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
    ?LOG_DEBUG(#{what => s2s_out_closed, text => <<"ejabberd_s2s_out terminated">>,
                 reason => Reason, state_name => StateName,
                 myname => StateData#state.myname, server => StateData#state.server}),
    case StateData#state.is_registered of
        false ->
            ok;
        true ->
            ejabberd_s2s:remove_connection(
              {StateData#state.myname, StateData#state.server}, self())
    end,
    E = mongoose_xmpp_errors:remote_server_not_found(<<"en">>, <<"Bounced by s2s">>),
    %% bounce queue manage by process and Erlang message queue
    bounce_queue(StateData#state.queue, E),
    case queue:is_empty(StateData#state.queue) of
        true ->
            ok;
        false ->
            ?LOG_WARNING(#{what => s2s_terminate_non_empty,
                           state_name => StateName, reason => Reason,
                           queue => lists:sublist(queue:to_list(StateData#state.queue), 10),
                           authenticated => StateData#state.authenticated})
    end,
    bounce_messages(E),
    case StateData#state.socket of
        undefined ->
            ok;
        _Socket ->
            mongoose_transport:close(StateData#state.socket)
    end,
    ok.

%%----------------------------------------------------------------------
%% Func: print_state/1
%% Purpose: Prepare the state to be printed on error log
%% Returns: State to print
%%----------------------------------------------------------------------
print_state(State) ->
    State.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

-spec send_text(state(), binary()) -> 'ok'.
send_text(StateData, Text) ->
    mongoose_transport:send_text(StateData#state.socket, Text).


-spec send_element(state(), exml:element()|mongoose_acc:t()) -> 'ok'.
send_element(StateData, #xmlel{} = El) ->
    mongoose_transport:send_element(StateData#state.socket, El).

-spec send_element(state(), mongoose_acc:t(), exml:element()) -> mongoose_acc:t().
send_element(StateData, Acc, El) ->
    mongoose_transport:send_element(StateData#state.socket, El),
    Acc.


-spec send_queue(state(), Q :: element_queue()) -> 'ok'.
send_queue(StateData, Q) ->
    case queue:out(Q) of
        {{value, {Acc, El}}, Q1} ->
            send_element(StateData, Acc, El),
            send_queue(StateData, Q1);
        {empty, _Q1} ->
            ok
    end.


%% @doc Bounce a single message (xmlel)
-spec bounce_element(Acc :: mongoose_acc:t(), El :: exml:element(), Error :: exml:element()) -> 'ok'.
bounce_element(Acc, El, Error) ->
    case mongoose_acc:stanza_type(Acc) of
        <<"error">> -> ok;
        <<"result">> -> ok;
        _ ->
            From = mongoose_acc:from_jid(Acc),
            To = mongoose_acc:to_jid(Acc),
            {Acc1, Err} = jlib:make_error_reply(Acc, El, Error),
            ejabberd_router:route(To, From, Acc1, Err)
    end.


-spec bounce_queue(Q :: element_queue(), Error :: exml:element()) -> 'ok'.
bounce_queue(Q, Error) ->
    case queue:out(Q) of
        {{value, {Acc, El}}, Q1} ->
            bounce_element(Acc, El, Error),
            bounce_queue(Q1, Error);
        {empty, _} ->
            ok
    end.


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


-spec bounce_messages(exml:element()) -> 'ok'.
bounce_messages(Error) ->
    receive
        {send_element, Acc, El} ->
            bounce_element(Acc, El, Error),
            bounce_messages(Error)
    after 0 ->
            ok
    end.


-spec send_dialback_request(state()) -> fsm_return().
send_dialback_request(StateData) ->
    IsRegistered = case StateData#state.is_registered of
              false ->
                  ejabberd_s2s:try_register(StateData#state.from_to);
              true ->
                  true
          end,
    NewStateData = StateData#state{is_registered = IsRegistered},
    try
        case IsRegistered of
            false ->
                %% Still not registered in the s2s table as an outgoing connection
                ok;
            true ->
                Key1 = ejabberd_s2s:key(
                         StateData#state.host_type,
                         StateData#state.from_to,
                         StateData#state.remote_streamid),
                %% Initiating server sends dialback key
                send_element(StateData, mongoose_s2s_dialback:step_1(StateData#state.from_to, Key1))
        end,
        case StateData#state.verify of
            false ->
                ok;
            {_Pid, Key2, SID} ->
                %% Receiving server sends verification request
                send_element(StateData, mongoose_s2s_dialback:step_2(StateData#state.from_to, Key2, SID))
        end,
        {next_state, wait_for_validation, NewStateData, ?FSMTIMEOUT*6}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => s2s_out_send_dialback_request_failed,
                         class => Class, reason => Reason, stacktrace => Stacktrace,
                         myname => StateData#state.myname, server => StateData#state.server}),
            {stop, normal, NewStateData}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SRV support

-include_lib("kernel/include/inet.hrl").

-spec lookup_services(mongooseim:host_type(), jid:lserver()) -> [addr()].
lookup_services(HostType, Server) ->
    case mongoose_s2s_lib:domain_utf8_to_ascii(Server) of
        false -> [];
        ASCIIAddr -> do_lookup_services(HostType, ASCIIAddr)
    end.

-spec do_lookup_services(mongooseim:host_type(),jid:lserver()) -> [addr()].
do_lookup_services(HostType, Server) ->
    Res = srv_lookup(HostType, Server),
    case Res of
        {error, Reason} ->
            ?LOG_DEBUG(#{what => s2s_srv_lookup_failed,
                         reason => Reason, server => Server}),
            [];
        {ok, #hostent{h_addr_list = AddrList, h_addrtype = Type}} ->
            %% Probabilities are not exactly proportional to weights
            %% for simplicity (higher weights are overvalued)
            case (catch lists:map(fun calc_addr_index/1, AddrList)) of
                {'EXIT', _Reason} ->
                    [];
                IndexedAddrs ->
                    Addrs = [#{ip_tuple => Addr, port => Port, type => Type}
                            || {_Index, Addr, Port} <- lists:keysort(1, IndexedAddrs)],
                    ?LOG_DEBUG(#{what => s2s_srv_lookup_success,
                                 addresses => Addrs, server => Server}),
                    Addrs
            end
    end.


-spec srv_lookup(mongooseim:host_type(), jid:lserver()) ->
          {'error', atom()} | {'ok', inet:hostent()}.
srv_lookup(HostType, Server) ->
    #{timeout := TimeoutSec, retries := Retries} = mongoose_config:get_opt([{s2s, HostType}, dns]),
    srv_lookup(Server, timer:seconds(TimeoutSec), Retries).


%% @doc XXX - this behaviour is suboptimal in the case that the domain
%% has a "_xmpp-server._tcp." but not a "_jabber._tcp." record and
%% we don't get a DNS reply for the "_xmpp-server._tcp." lookup. In this
%% case we'll give up when we get the "_jabber._tcp." nxdomain reply.
-spec srv_lookup(jid:server(),
                 Timeout :: non_neg_integer(),
                 Retries :: pos_integer()
                 ) -> {'error', atom()} | {'ok', inet:hostent()}.
srv_lookup(_Server, _Timeout, Retries) when Retries < 1 ->
    {error, timeout};
srv_lookup(Server, Timeout, Retries) ->
    case inet_res:getbyname("_xmpp-server._tcp." ++ binary_to_list(Server), srv, Timeout) of
        {error, _Reason} ->
            case inet_res:getbyname("_jabber._tcp." ++ binary_to_list(Server), srv, Timeout) of
                {error, timeout} ->
                    ?LOG_ERROR(#{what => s2s_dns_lookup_failed,
                                 text => <<"The DNS servers timed out on request for IN SRV."
                                           " You should check your DNS configuration.">>,
                                 nameserver => inet_db:res_option(nameserver),
                                 server => Server}),
                    srv_lookup(Server, Timeout, Retries - 1);
                R -> R
            end;
        {ok, _HEnt} = R -> R
    end.

-spec lookup_addrs(mongooseim:host_type(), jid:server()) -> [addr()].
lookup_addrs(HostType, Server) ->
    Port = outgoing_s2s_port(HostType),
    lists:foldl(fun(Type, []) ->
                        [#{ip_tuple => Addr, port => Port, type => Type}
                         || Addr <- lookup_addrs_for_type(Server, Type)];
                   (_Type, Addrs) ->
                        Addrs
                end, [], outgoing_s2s_types(HostType)).

-spec lookup_addrs_for_type(jid:lserver(), inet | inet6) -> [inet:ip_address()].
lookup_addrs_for_type(Server, Type) ->
    case inet:gethostbyname(binary_to_list(Server), Type) of
        {ok, #hostent{h_addr_list = Addrs}} ->
            ?LOG_DEBUG(#{what => s2s_srv_resolve_success,
                         type => Type, server => Server, addresses => Addrs}),
            Addrs;
        {error, Reason} ->
            ?LOG_DEBUG(#{what => s2s_srv_resolve_failed,
                         type => Type, server => Server, reason => Reason}),
            []
    end.


-spec outgoing_s2s_port(mongooseim:host_type()) -> inet:port_number().
outgoing_s2s_port(HostType) ->
    mongoose_config:get_opt([{s2s, HostType}, outgoing, port]).


-spec outgoing_s2s_types(mongooseim:host_type()) -> [inet | inet6, ...].
outgoing_s2s_types(HostType) ->
    %% DISCUSSION: Why prefer IPv4 first?
    %%
    %% IPv4 connectivity will be available for everyone for
    %% many years to come. So, there's absolutely no benefit
    %% in preferring IPv6 connections which are flaky at best
    %% nowadays.
    %%
    %% On the other hand content providers hesitate putting up
    %% AAAA records for their sites due to the mentioned
    %% quality of current IPv6 connectivity. Making IPv6 the a
    %% `fallback' may avoid these problems elegantly.
    [ip_version_to_type(V) || V <- mongoose_config:get_opt([{s2s, HostType}, outgoing, ip_versions])].

ip_version_to_type(4) -> inet;
ip_version_to_type(6) -> inet6.

-spec outgoing_s2s_timeout(mongooseim:host_type()) -> non_neg_integer() | infinity.
outgoing_s2s_timeout(HostType) ->
    mongoose_config:get_opt([{s2s, HostType}, outgoing, connection_timeout], 10000).

%% @doc Human readable S2S logging: Log only new outgoing connections as INFO
%% Do not log dialback
log_s2s_out(false, _, _, _) -> ok;
%% Log new outgoing connections:
log_s2s_out(_, Myname, Server, Tls) ->
    ?LOG_INFO(#{what => s2s_out,
                text => <<"Trying to open s2s connection">>,
                myname => Myname, server => Server, tls => Tls}).

next_state(StateName, StateData) ->
    {next_state, StateName, StateData,
     get_timeout_interval(StateName)}.

%% @doc Calculate timeout depending on which state we are in:
%% Can return integer > 0 | infinity
-spec get_timeout_interval(statename()) -> 'infinity' | non_neg_integer().
get_timeout_interval(StateName) ->
    case StateName of
        %% Validation implies dialback: Networking can take longer:
        wait_for_validation ->
            ?FSMTIMEOUT*6;
        %% When stream is established, we only rely on S2S Timeout timer:
        stream_established ->
            infinity;
        _ ->
            ?FSMTIMEOUT
    end.


%% @doc This function is intended to be called at the end of a state
%% function that want to wait for a reconnect delay before stopping.
-spec wait_before_reconnect(state()) -> fsm_return().
wait_before_reconnect(StateData) ->
    E = mongoose_xmpp_errors:remote_server_not_found(<<"en">>, <<"From s2s (waiting)">>),
    %% bounce queue manage by process and Erlang message queue
    bounce_queue(StateData#state.queue, E),
    bounce_messages(E),
    cancel_timer(StateData#state.timer),
    Delay = case StateData#state.delay_to_retry of
                undefined ->
                    %% The initial delay is random between 1 and 15 seconds
                    %% Return a random integer between 1000 and 15000
                    MicroSecs = erlang:system_time(microsecond),
                    (MicroSecs rem 14000) + 1000;
                D1 ->
                    %% Duplicate the delay with each successive failed
                    %% reconnection attempt, but don't exceed the max
                    lists:min([D1 * 2, get_max_retry_delay(StateData#state.host_type)])
            end,
    Timer = erlang:start_timer(Delay, self(), []),
    {next_state, wait_before_retry, StateData#state{timer=Timer,
                                                    delay_to_retry = Delay,
                                                    queue = queue:new()}}.


%% @doc Get the maximum allowed delay for retry to reconnect (in milliseconds).
%% The default value is 5 minutes.
%% The option {s2s_max_retry_delay, Seconds} can be used (in seconds).
get_max_retry_delay(HostType) ->
    mongoose_config:get_opt([{s2s, HostType}, max_retry_delay]) * 1000.


%% @doc Terminate s2s_out connections that are in state wait_before_retry
-spec terminate_if_waiting_delay(ejabberd_s2s:fromto()) -> ok.
terminate_if_waiting_delay(FromTo) ->
    Pids = ejabberd_s2s:get_s2s_out_pids(FromTo),
    lists:foreach(
      fun(Pid) ->
              Pid ! terminate_if_waiting_before_retry
      end,
      Pids).


-spec fsm_limit_opts() -> [{'max_queue', integer()}].
fsm_limit_opts() ->
    case mongoose_config:lookup_opt(max_fsm_queue) of
        {ok, N} ->
            [{max_queue, N}];
        {error, not_found} ->
            []
    end.

-spec get_addr_list(mongooseim:host_type(), jid:lserver()) -> [addr()].
get_addr_list(HostType, Server) ->
    lists:foldl(fun(F, []) -> F(HostType, Server);
                   (_, Result) -> Result
                end, [], [fun get_predefined_addresses/2,
                          fun lookup_services/2,
                          fun lookup_addrs/2]).

%% @doc Get IPs predefined for a given s2s domain in the configuration
-spec get_predefined_addresses(mongooseim:host_type(), jid:lserver()) -> [addr()].
get_predefined_addresses(HostType, Server) ->
    case mongoose_config:lookup_opt([{s2s, HostType}, address, Server]) of
        {ok, #{ip_address := IPAddress} = M} ->
            {ok, IPTuple} = inet:parse_address(IPAddress),
            Port = get_predefined_port(HostType, M),
            [#{ip_tuple => IPTuple, port => Port, type => addr_type(IPTuple)}];
        {error, not_found} ->
            []
    end.

get_predefined_port(_HostType, #{port := Port}) -> Port;
get_predefined_port(HostType, _Addr) -> outgoing_s2s_port(HostType).

addr_type(Addr) when tuple_size(Addr) =:= 4 -> inet;
addr_type(Addr) when tuple_size(Addr) =:= 8 -> inet6.

get_acc_with_new_sext(?NS_SASL, Els1, {_SEXT, STLS, STLSReq}) ->
    NewSEXT =
        lists:any(
          fun(#xmlel{name = <<"mechanism">>,
                     children = Els2}) ->
                  case xml:get_cdata(Els2) of
                      <<"EXTERNAL">> -> true;
                      _ -> false
                  end;
             (_) -> false
          end, Els1),
    {NewSEXT, STLS, STLSReq};
get_acc_with_new_sext(_, _, Acc) ->
    Acc.

get_acc_with_new_tls(?NS_TLS, El1, {SEXT, _STLS, _STLSReq}) ->
    Req = case xml:get_subtag(El1, <<"required">>) of
              #xmlel{} -> true;
              false -> false
          end,
    {SEXT, true, Req};
get_acc_with_new_tls(_, _, Acc) ->
    Acc.

tls_options(HostType) ->
    Ciphers = mongoose_config:get_opt([{s2s, HostType}, ciphers]),
    Options = #{verify_mode => peer, ciphers => Ciphers},
    case mongoose_s2s_lib:lookup_certfile(HostType) of
        {ok, CertFile} -> Options#{certfile => CertFile};
        {error, not_found} -> Options
    end.

calc_addr_index({Priority, Weight, Port, Host}) ->
    N = case Weight of
            0 -> 0;
            _ -> (Weight + 1) * rand:uniform()
        end,
    {Priority * 65536 - N, Host, Port}.

handle_parsed_features({false, false, _, StateData = #state{authenticated = true}}) ->
    send_queue(StateData, StateData#state.queue),
    ?LOG_INFO(#{what => s2s_out_connected,
                text => <<"New outgoing s2s connection established">>,
                myname => StateData#state.myname, server => StateData#state.server}),
    {next_state, stream_established,
     StateData#state{queue = queue:new()}};
handle_parsed_features({true, _, _, StateData = #state{try_auth = true, is_registered = true}}) ->
    send_element(StateData,
                 #xmlel{name = <<"auth">>,
                        attrs = [{<<"xmlns">>, ?NS_SASL},
                                 {<<"mechanism">>, <<"EXTERNAL">>}],
                        children =
                            [#xmlcdata{content = jlib:encode_base64(
                                                   StateData#state.myname)}]}),
    {next_state, wait_for_auth_result,
     StateData#state{try_auth = false}, ?FSMTIMEOUT};
handle_parsed_features({_, true, _, StateData = #state{tls = true, tls_enabled = false}}) ->
    send_element(StateData,
                 #xmlel{name = <<"starttls">>,
                        attrs = [{<<"xmlns">>, ?NS_TLS}]}),
    {next_state, wait_for_starttls_proceed, StateData,
     ?FSMTIMEOUT};
handle_parsed_features({_, _, true, StateData = #state{tls = false}}) ->
    ?LOG_DEBUG(#{what => s2s_out_restarted,
                 myname => StateData#state.myname, server => StateData#state.server}),
    mongoose_transport:close(StateData#state.socket),
    {next_state, reopen_socket,
     StateData#state{socket = undefined,
                     use_v10 = false}, ?FSMTIMEOUT};
handle_parsed_features({_, _, _, StateData = #state{dialback_enabled = true}}) ->
    send_dialback_request(StateData);
handle_parsed_features({_, _, _, StateData}) ->
    ?LOG_DEBUG(#{what => s2s_out_restarted,
                 myname => StateData#state.myname, server => StateData#state.server}),
    % TODO: clear message queue
    mongoose_transport:close(StateData#state.socket),
    {next_state, reopen_socket, StateData#state{socket = undefined,
                                                use_v10 = false}, ?FSMTIMEOUT}.

handle_get_state_info(StateName, StateData) ->
    {Addr, Port} = get_peername(StateData#state.socket),
    #{pid => self(),
      direction => out,
      statename => StateName,
      addr => Addr,
      port => Port,
      streamid => StateData#state.streamid,
      use_v10 => StateData#state.use_v10,
      tls => StateData#state.tls,
      tls_required => StateData#state.tls_required,
      tls_enabled => StateData#state.tls_enabled,
      tls_options => StateData#state.tls_options,
      authenticated => StateData#state.authenticated,
      dialback_enabled => StateData#state.dialback_enabled,
      try_auth => StateData#state.try_auth,
      myname => StateData#state.myname,
      server => StateData#state.server,
      delay_to_retry => StateData#state.delay_to_retry,
      verify => StateData#state.verify}.

get_peername(undefined) ->
    {unknown, unknown};
get_peername(Socket) ->
    {ok, {Addr, Port}} = mongoose_transport:peername(Socket),
    {Addr, Port}.
