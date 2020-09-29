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

%% External exports
-export([start/3,
         start_link/3,
         start_connection/1,
         terminate_if_waiting_delay/2,
         stop_connection/1]).

%% p1_fsm callbacks (same as gen_fsm)
-export([init/1,
         open_socket/2,
         wait_for_stream/2,
         wait_for_validation/2,
         wait_for_features/2,
         wait_for_auth_result/2,
         wait_for_starttls_proceed/2,
         relay_to_bridge/2,
         reopen_socket/2,
         wait_before_retry/2,
         stream_established/2,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         print_state/1,
         code_change/4,
         test_get_addr_port/1,
         get_addr_port/1]).

-include("mongoose.hrl").
-include("jlib.hrl").

-record(state, {socket,
                streamid,
                remote_streamid = <<>>,
                use_v10,
                tls = false             :: boolean(),
                tls_required = false    :: boolean(),
                tls_enabled = false     :: boolean(),
                tls_options = [connect] :: list(),
                authenticated = false   :: boolean(),
                db_enabled = true       :: boolean(),
                try_auth = true         :: boolean(),
                myname, server, queue,
                delay_to_retry = undefined_delay,
                new = false             :: boolean(),
                verify = false          :: false | {pid(), Key :: binary(), SID :: binary()},
                bridge,
                timer                   :: reference()
              }).
-type state() :: #state{}.

-type element_queue() :: queue:queue(#xmlel{}).
-type statename() :: open_socket
                   | wait_for_stream
                   | wait_for_features
                   | wait_for_auth_result
                   | wait_for_starttls_proceed
                   | wait_for_validation
                   | wait_before_retry
                   | relay_to_bridge.
%% FSM handler return value
-type fsm_return() :: {'stop', Reason :: 'normal', state()}
                    | {'next_state', statename(), state()}
                    | {'next_state', statename(), state(), Timeout :: integer()}.
%%-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

%% Module start with or without supervisor:
-ifdef(NO_TRANSIENT_SUPERVISORS).
-define(SUPERVISOR_START, p1_fsm:start(ejabberd_s2s_out, [From, Host, Type],
                                       fsm_limit_opts() ++ ?FSMOPTS)).
-else.
-define(SUPERVISOR_START, supervisor:start_child(ejabberd_s2s_out_sup,
                                                 [From, Host, Type])).
-endif.

-define(FSMTIMEOUT, 30000).

%% We do not block on send anymore.
-define(TCP_SEND_TIMEOUT, 15000).

%% Maximum delay to wait before retrying to connect after a failed attempt.
%% Specified in milliseconds. Default value is 5 minutes.
-define(MAX_RETRY_DELAY, 300000).

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
-spec start(_, _, _) -> {'error', _} | {'ok', 'undefined' | pid()} | {'ok', 'undefined' | pid(), _}.
start(From, Host, Type) ->
    ?SUPERVISOR_START.


-spec start_link(_, _, _) -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link(From, Host, Type) ->
    p1_fsm:start_link(ejabberd_s2s_out, [From, Host, Type],
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
-spec init([any(), ...]) -> {'ok', 'open_socket', state()}.
init([From, Server, Type]) ->
    process_flag(trap_exit, true),
    ?LOG_DEBUG(#{what => s2s_out_started,
                 text => <<"New outgoing s2s connection">>,
                 from => From, server => Server, type => Type}),
    {TLS, TLSRequired} = case ejabberd_config:get_local_option(s2s_use_starttls) of
              UseTls when (UseTls==undefined) or (UseTls==false) ->
                  {false, false};
              UseTls when (UseTls==true) or (UseTls==optional) ->
                  {true, false};
              UseTls when (UseTls==required) or (UseTls==required_trusted) ->
                  {true, true}
          end,
    UseV10 = TLS,
    TLSOpts = case ejabberd_config:get_local_option(s2s_certfile) of
                  undefined ->
                      [connect];
                  CertFile ->
                      [{certfile, CertFile}, connect]
              end,
    TLSOpts2 = case ejabberd_config:get_local_option(s2s_ciphers) of
                       undefined ->
                               TLSOpts;
                       Ciphers ->
                               [{ciphers, Ciphers} | TLSOpts]
               end,
    {New, Verify} = case Type of
                        new ->
                            {true, false};
                        {verify, Pid, Key, SID} ->
                            start_connection(self()),
                            {false, {Pid, Key, SID}}
                    end,
    Timer = erlang:start_timer(ejabberd_s2s:timeout(), self(), []),
    {ok, open_socket, #state{use_v10 = UseV10,
                             tls = TLS,
                             tls_required = TLSRequired,
                             tls_options = TLSOpts2,
                             queue = queue:new(),
                             myname = From,
                             server = Server,
                             new = New,
                             verify = Verify,
                             timer = Timer}}.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
-spec open_socket(_, state()) -> fsm_return().
open_socket(init, StateData) ->
    log_s2s_out(StateData#state.new,
                StateData#state.myname,
                StateData#state.server,
                StateData#state.tls),
    ?LOG_DEBUG(#{what => s2s_open_socket,
                 myname => StateData#state.myname,
                 server => StateData#state.server,
                 new => StateData#state.new,
                 verify => StateData#state.verify}),
    AddrList = get_addr_list(StateData#state.server),
    case lists:foldl(fun({_Addr, _Port}, {ok, Socket}) ->
                             {ok, Socket};
                        ({Addr, Port}, _Acc) ->
                             open_socket1(Addr, Port)
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
            case mongoose_hooks:find_s2s_bridge(undefined,
                                                StateData#state.myname,
                                                StateData#state.server) of
                {Mod, Fun, Type} ->
                    ?LOG_INFO(#{what => s2s_out_bridge_found,
                                text => <<"Found a bridge, relay_to_bridge next.">>,
                                type => Type,
                                myname => StateData#state.myname, server => StateData#state.server}),
                    NewStateData = StateData#state{bridge={Mod, Fun}},
                    {next_state, relay_to_bridge, NewStateData};
                _ ->
                    wait_before_reconnect(StateData)
            end
    end;
open_socket(closed, StateData) ->
    ?CLOSE_GENERIC(open_socket, closed, StateData);
open_socket(timeout, StateData) ->
    ?CLOSE_GENERIC(open_socket, timeout, StateData);
open_socket(_, StateData) ->
    {next_state, open_socket, StateData}.

%%----------------------------------------------------------------------
%% IPv4
-spec open_socket1(Host :: binary() | inet:ip_address(),
                   Port :: inet:port_number()) -> {'error', _} | {'ok', _}.
open_socket1({_, _, _, _} = Addr, Port) ->
    open_socket2(inet, Addr, Port);
%% IPv6
open_socket1({_, _, _, _, _, _, _, _} = Addr, Port) ->
    open_socket2(inet6, Addr, Port);
%% Hostname
open_socket1(Host, Port) ->
    lists:foldl(fun(_Family, {ok, _Socket} = R) ->
                        R;
                   (Family, _) ->
                        Addrs = get_addrs(Host, Family),
                        lists:foldl(fun(_Addr, {ok, _Socket} = R) ->
                                            R;
                                       (Addr, _) ->
                                            open_socket1(Addr, Port)
                                    end, ?SOCKET_DEFAULT_RESULT, Addrs)
                end, ?SOCKET_DEFAULT_RESULT, outgoing_s2s_families()).


-spec open_socket2(Type :: 'inet' | 'inet6',
                   Addr :: inet:ip_address(),
                   Port :: inet:port_number()) -> {'error', _} | {'ok', _}.
open_socket2(Type, Addr, Port) ->
    ?LOG_DEBUG(#{what => s2s_out_connecting,
                 address => Addr, port => Port}),
    Timeout = outgoing_s2s_timeout(),
    SockOpts = [binary,
                {packet, 0},
                {send_timeout, ?TCP_SEND_TIMEOUT},
                {send_timeout_close, true},
                {active, false},
                Type],

    case (catch ejabberd_socket:connect(Addr, Port, SockOpts, Timeout)) of
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
            send_db_request(StateData);
        {<<"jabber:server">>, <<"jabber:server:dialback">>, true} when
        StateData#state.use_v10 ->
            {next_state, wait_for_features, StateData, ?FSMTIMEOUT};
        %% Clause added to handle Tigase's workaround for an old ejabberd bug:
        {<<"jabber:server">>, <<"jabber:server:dialback">>, true} when
        not StateData#state.use_v10 ->
            send_db_request(StateData);
        {<<"jabber:server">>, <<"">>, true} when StateData#state.use_v10 ->
            {next_state, wait_for_features, StateData#state{db_enabled = false}, ?FSMTIMEOUT};
        {NSProvided, DB, _} ->
            send_text(StateData, exml:to_binary(mongoose_xmpp_errors:invalid_namespace())),
            ?LOG_INFO(#{what => s2s_out_closing,
                        text => <<"Closing s2s connection: (invalid namespace)">>,
                        namespace_provided => NSProvided,
                        namespace_expected => <<"jabber:server">>,
                        xmlnsdb_provided => DB,
                        all_attributes => Attrs,
                        myname => StateData#state.myname, server => StateData#state.server}),
            {stop, normal, StateData}
    end;
wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
              <<(mongoose_xmpp_errors:xml_not_well_formed_bin())/binary, (?STREAM_TRAILER)/binary>>),
    ?CLOSE_GENERIC(wait_for_stream, xmlstreamerror, StateData);
wait_for_stream({xmlstreamend, _Name}, StateData) ->
    ?CLOSE_GENERIC(wait_for_stream, xmlstreamend, StateData);
wait_for_stream(timeout, StateData) ->
    ?CLOSE_GENERIC(wait_for_stream, timeout, StateData);
wait_for_stream(closed, StateData) ->
    ?CLOSE_GENERIC(wait_for_stream, closed, StateData).


-spec wait_for_validation(ejabberd:xml_stream_item(), state()) -> fsm_return().
wait_for_validation({xmlstreamelement, El}, StateData) ->
    case is_verify_res(El) of
        {result, To, From, Id, Type} ->
            ?LOG_DEBUG(#{what => s2s_receive_result,
                         from => From, to => To, message_id => Id, type => Type}),
            case {Type, StateData#state.tls_enabled, StateData#state.tls_required} of
                {<<"valid">>, Enabled, Required} when (Enabled==true) or (Required==false) ->
                    send_queue(StateData, StateData#state.queue),
                    ?LOG_INFO(#{what => s2s_out_connected,
                                text => <<"New outgoing s2s connection established">>,
                                tls_enabled => StateData#state.tls_enabled,
                                myname => StateData#state.myname, server => StateData#state.server}),
                    mongoose_hooks:s2s_connect_hook(StateData#state.myname,
                                                    ok,
                                                    StateData#state.server),
                    {next_state, stream_established,
                     StateData#state{queue = queue:new()}};
                {<<"valid">>, Enabled, Required} when (Enabled==false) and (Required==true) ->
                    %% TODO: bounce packets
                    ?CLOSE_GENERIC(wait_for_validation, tls_required_but_unavailable, El, StateData);
                _ ->
                    %% TODO: bounce packets
                    ?CLOSE_GENERIC(wait_for_validation, invalid_dialback_key, El, StateData)
            end;
        {verify, To, From, Id, Type} ->
            ?LOG_DEBUG(#{what => s2s_receive_verify,
                         from => From, to => To, message_id => Id, type => Type}),
            case StateData#state.verify of
                false ->
                    NextState = wait_for_validation,
                    %% TODO: Should'nt we close the connection here ?
                    {next_state, NextState, StateData,
                     get_timeout_interval(NextState)};
                {Pid, _Key, _SID} ->
                    send_event(Type, Pid, StateData),
                    NextState = wait_for_validation,
                    {next_state, NextState, StateData,
                     get_timeout_interval(NextState)}

            end;
        _ ->
            {next_state, wait_for_validation, StateData, ?FSMTIMEOUT*3}
    end;
wait_for_validation({xmlstreamend, _Name}, StateData) ->
    ?CLOSE_GENERIC(wait_for_validation, xmlstreamend, StateData);
wait_for_validation({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
              <<(mongoose_xmpp_errors:xml_not_well_formed_bin())/binary, (?STREAM_TRAILER)/binary>>),
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
            send_text(StateData,
                      <<(mongoose_xmpp_errors:bad_format_bin())/binary,
                      (?STREAM_TRAILER)/binary>>),
            ?CLOSE_GENERIC(wait_for_features, bad_format, El, StateData)
    end;
wait_for_features({xmlstreamend, _Name}, StateData) ->
    ?CLOSE_GENERIC(wait_for_features, xmlstreamend, StateData);
wait_for_features({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
              <<(mongoose_xmpp_errors:xml_not_well_formed_bin())/binary, (?STREAM_TRAILER)/binary>>),
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
                    send_text(StateData,
                              <<(mongoose_xmpp_errors:bad_format_bin())/binary,
                              (?STREAM_TRAILER)/binary>>),
                    ?CLOSE_GENERIC(wait_for_auth_result, bad_format, El, StateData)
            end;
        #xmlel{name = <<"failure">>, attrs = Attrs} ->
            case xml:get_attr_s(<<"xmlns">>, Attrs) of
                ?NS_SASL ->
                    ?LOG_INFO(#{what => s2s_auth_failure,
                                text => <<"Received failure result in ejabberd_s2s_out. Restarting">>,
                                myname => StateData#state.myname,
                                server => StateData#state.server}),
                    ejabberd_socket:close(StateData#state.socket),
                    {next_state, reopen_socket,
                     StateData#state{socket = undefined}, ?FSMTIMEOUT};
                _ ->
                    send_text(StateData,
                              <<(mongoose_xmpp_errors:bad_format_bin())/binary,
                              (?STREAM_TRAILER)/binary>>),
                    ?CLOSE_GENERIC(wait_for_auth_result, bad_format, El, StateData)
            end;
        _ ->
            send_text(StateData,
                      <<(mongoose_xmpp_errors:bad_format_bin())/binary,
                              (?STREAM_TRAILER)/binary>>),
            ?CLOSE_GENERIC(wait_for_auth_result, bad_format, El, StateData)
    end;
wait_for_auth_result({xmlstreamend, _Name}, StateData) ->
    ?CLOSE_GENERIC(wait_for_auth_result, xmlstreamend, StateData);
wait_for_auth_result({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
              <<(mongoose_xmpp_errors:xml_not_well_formed_bin())/binary, (?STREAM_TRAILER)/binary>>),
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
                    Socket = StateData#state.socket,
                    TLSOpts = get_tls_opts_with_certfile(StateData),
                    TLSOpts2 = get_tls_opts_with_ciphers(TLSOpts),
                    TLSSocket = ejabberd_socket:starttls(Socket, TLSOpts2),
                    NewStateData = StateData#state{socket = TLSSocket,
                                                   streamid = new_id(),
                                                   tls_enabled = true,
                                                   tls_options = TLSOpts2
                                                  },
                    send_text(NewStateData,
                              ?STREAM_HEADER(StateData#state.myname, StateData#state.server,
                                <<" version='1.0'">>)),
                    {next_state, wait_for_stream, NewStateData, ?FSMTIMEOUT};
                _ ->
                    send_text(StateData,
                              <<(mongoose_xmpp_errors:bad_format_bin())/binary,
                              (?STREAM_TRAILER)/binary>>),
                    ?CLOSE_GENERIC(wait_for_auth_result, bad_format, El, StateData)
            end;
        _ ->
            ?CLOSE_GENERIC(wait_for_auth_result, bad_format, El, StateData)
    end;
wait_for_starttls_proceed({xmlstreamend, _Name}, StateData) ->
    ?CLOSE_GENERIC(wait_for_starttls_proceed, xmlstreamend, StateData);
wait_for_starttls_proceed({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
              <<(mongoose_xmpp_errors:xml_not_well_formed_bin())/binary, (?STREAM_TRAILER)/binary>>),
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


-spec relay_to_bridge(ejabberd:xml_stream_item(), state()) -> fsm_return().
relay_to_bridge(stop, StateData) ->
    wait_before_reconnect(StateData);
relay_to_bridge(closed, StateData) ->
    ?CLOSE_GENERIC(relay_to_bridge, closed, StateData);
relay_to_bridge(_Event, StateData) ->
    {next_state, relay_to_bridge, StateData}.


-spec stream_established(ejabberd:xml_stream_item(), state()) -> fsm_return().
stream_established({xmlstreamelement, El}, StateData) ->
    ?LOG_DEBUG(#{what => s2s_out_stream_established, exml_packet => El,
                 myname => StateData#state.myname, server => StateData#state.server}),
    case is_verify_res(El) of
        {verify, VTo, VFrom, VId, VType} ->
            ?LOG_DEBUG(#{what => s2s_recv_verify,
                         to => VTo, from => VFrom, message_id => VId, type => VType,
                         myname => StateData#state.myname, server => StateData#state.server}),
            case StateData#state.verify of
                {VPid, _VKey, _SID} ->
                    send_event(VType, VPid, StateData);
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    {next_state, stream_established, StateData};
stream_established({xmlstreamend, _Name}, StateData) ->
    ?CLOSE_GENERIC(stream_established, xmlstreamend, StateData);
stream_established({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
              <<(mongoose_xmpp_errors:xml_not_well_formed_bin())/binary, (?STREAM_TRAILER)/binary>>),
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
    {next_state, StateName, StateData, get_timeout_interval(StateName)}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: The associated StateData for this connection
%%   {reply, Reply, NextStateName, NextStateData}
%%   Reply = {state_infos, [{InfoName::atom(), InfoValue::any()]
%%----------------------------------------------------------------------
handle_sync_event(get_state_infos, _From, StateName, StateData) ->
    {Addr, Port} = try ejabberd_socket:peername(StateData#state.socket) of
                       {ok, {A, P}} ->  {A, P}
                   catch
                       _:_ ->
                           {unknown, unknown}
                   end,
    Infos = [
             {direction, out},
             {statename, StateName},
             {addr, Addr},
             {port, Port},
             {streamid, StateData#state.streamid},
             {use_v10, StateData#state.use_v10},
             {tls, StateData#state.tls},
             {tls_required, StateData#state.tls_required},
             {tls_enabled, StateData#state.tls_enabled},
             {tls_options, StateData#state.tls_options},
             {authenticated, StateData#state.authenticated},
             {db_enabled, StateData#state.db_enabled},
             {try_auth, StateData#state.try_auth},
             {myname, StateData#state.myname},
             {server, StateData#state.server},
             {delay_to_retry, StateData#state.delay_to_retry},
             {verify, StateData#state.verify}
            ],
    Reply = {state_infos, Infos},
    {reply, Reply, StateName, StateData};

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData, get_timeout_interval(StateName)}.


code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info({send_text, Text}, StateName, StateData) ->
    ?LOG_ERROR(#{what => s2s_send_text, text => <<"Deprecated ejabberd_s2s_out send_text">>,
                 myname => StateData#state.myname, server => StateData#state.server,
                 send_text => Text}),
    send_text(StateData, Text),
    cancel_timer(StateData#state.timer),
    Timer = erlang:start_timer(ejabberd_s2s:timeout(), self(), []),
    {next_state, StateName, StateData#state{timer = Timer},
     get_timeout_interval(StateName)};
handle_info({send_element, Acc, El}, StateName, StateData) ->
    case StateName of
        stream_established ->
            cancel_timer(StateData#state.timer),
            Timer = erlang:start_timer(ejabberd_s2s:timeout(), self(), []),
            send_element(StateData, El),
            {next_state, StateName, StateData#state{timer = Timer}};
        %% In this state we bounce all message: We are waiting before
        %% trying to reconnect
        wait_before_retry ->
            bounce_element(Acc, El, mongoose_xmpp_errors:remote_server_not_found()),
            {next_state, StateName, StateData};
        relay_to_bridge ->
            %% In this state we relay all outbound messages
            %% to a foreign protocol bridge such as SMTP, SIP, etc.
            {Mod, Fun} = StateData#state.bridge,
            ?LOG_DEBUG(#{what => s2s_relay_stanza,
                         text => <<"Relaying stanza to bridge">>,
                         bridge_module => Mod, bridge_fun => Fun, acc => Acc,
                         myname => StateData#state.myname, server => StateData#state.server}),
            case catch Mod:Fun(El) of
                {'EXIT', Reason} ->
                    ?LOG_ERROR(#{what => s2s_relay_to_bridge_failed,
                                 bridge_module => Mod, bridge_fun => Fun,
                                 reason => Reason, acc => Acc,
                                 myname => StateData#state.myname,
                                 server => StateData#state.server}),
                    bounce_element(Acc, El, mongoose_xmpp_errors:internal_server_error()),
                    wait_before_reconnect(StateData);
                _ ->
                    {next_state, StateName, StateData}
            end;
        _ ->
            Q = queue:in({Acc, El}, StateData#state.queue),
            {next_state, StateName, StateData#state{queue = Q},
             get_timeout_interval(StateName)}
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
    {next_state, StateName, StateData, get_timeout_interval(StateName)};
handle_info(_, StateName, StateData) ->
    {next_state, StateName, StateData, get_timeout_interval(StateName)}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(Reason, StateName, StateData) ->
    ?LOG_DEBUG(#{what => s2s_out_closed, text => <<"ejabberd_s2s_out terminated">>,
                 reason => Reason, state_name => StateName,
                 myname => StateData#state.myname, server => StateData#state.server}),
    case StateData#state.new of
        false ->
            ok;
        true ->
            ejabberd_s2s:remove_connection(
              {StateData#state.myname, StateData#state.server}, self())
    end,
    %% bounce queue manage by process and Erlang message queue
    bounce_queue(StateData#state.queue, mongoose_xmpp_errors:remote_server_not_found()),
    bounce_messages(mongoose_xmpp_errors:remote_server_not_found()),
    case StateData#state.socket of
        undefined ->
            ok;
        _Socket ->
            ejabberd_socket:close(StateData#state.socket)
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
    ejabberd_socket:send(StateData#state.socket, Text).


-spec send_element(state(), exml:element()|mongoose_acc:t()) -> 'ok'.
send_element(StateData, #xmlel{} = El) ->
    send_text(StateData, exml:to_binary(El)).

-spec send_element(state(), mongoose_acc:t(), exml:element()) -> mongoose_acc:t().
send_element(StateData, Acc, El) ->
    send_text(StateData, exml:to_binary(El)),
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


-spec send_db_request(state()) -> fsm_return().
send_db_request(StateData) ->
    Server = StateData#state.server,
    New = case StateData#state.new of
              false ->
                  ejabberd_s2s:try_register(
                         {StateData#state.myname, Server});
              true ->
                  true
          end,
    NewStateData = StateData#state{new = New},
    try
        case New of
            false ->
                ok;
            true ->
                Key1 = ejabberd_s2s:key(
                         {StateData#state.myname, Server},
                         StateData#state.remote_streamid),
                send_element(StateData,
                             #xmlel{name = <<"db:result">>,
                                    attrs = [{<<"from">>, StateData#state.myname},
                                             {<<"to">>, Server}],
                                    children = [#xmlcdata{content = Key1}]})
        end,
        case StateData#state.verify of
            false ->
                ok;
            {_Pid, Key2, SID} ->
                send_element(StateData,
                             #xmlel{name = <<"db:verify">>,
                                    attrs = [{<<"from">>, StateData#state.myname},
                                             {<<"to">>, StateData#state.server},
                                             {<<"id">>, SID}],
                                    children = [#xmlcdata{content = Key2}]})
        end,
        {next_state, wait_for_validation, NewStateData, ?FSMTIMEOUT*6}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR(#{what => s2s_out_send_db_request_failed,
                         class => Class, reason => Reason, stacktrace => Stacktrace,
                         myname => StateData#state.myname, server => StateData#state.server}),
            {stop, normal, NewStateData}
    end.


-spec is_verify_res(exml:element()) -> 'false' | {'result', _, _, _, _} | {'verify', _, _, _, _}.
is_verify_res(#xmlel{name = Name,
                     attrs = Attrs}) when Name == <<"db:result">> ->
    {result,
     xml:get_attr_s(<<"to">>, Attrs),
     xml:get_attr_s(<<"from">>, Attrs),
     xml:get_attr_s(<<"id">>, Attrs),
     xml:get_attr_s(<<"type">>, Attrs)};
is_verify_res(#xmlel{name = Name,
                     attrs = Attrs}) when Name == <<"db:verify">> ->
    {verify,
     xml:get_attr_s(<<"to">>, Attrs),
     xml:get_attr_s(<<"from">>, Attrs),
     xml:get_attr_s(<<"id">>, Attrs),
     xml:get_attr_s(<<"type">>, Attrs)};
is_verify_res(_) ->
    false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SRV support

-include_lib("kernel/include/inet.hrl").

-spec get_addr_port(jid:server()) -> [{inet:ip_address(), inet:port_number()}].
get_addr_port(Server) ->
    Res = srv_lookup(Server),
    case Res of
        {error, Reason} ->
            ?LOG_DEBUG(#{what => s2s_srv_lookup_failed,
                         reason => Reason, server => Server}),
            [{Server, outgoing_s2s_port()}];
        {ok, #hostent{h_addr_list = AddrList}} ->
            %% Probabilities are not exactly proportional to weights
            %% for simplicity (higher weights are overvalued)
            case (catch lists:map(fun calc_addr_index/1, AddrList)) of
                {'EXIT', _Reason} ->
                    [{Server, outgoing_s2s_port()}];
                SortedList ->
                    List = lists:keysort(1, SortedList),
                    List2 = remove_addr_index(List),
                    ?LOG_DEBUG(#{what => s2s_srv_lookup_success,
                                 addresses => List2, server => Server}),
                    List2
            end
    end.


-spec srv_lookup(jid:server()) -> {'error', atom()} | {'ok', inet:hostent()}.
srv_lookup(Server) ->
    Options = case ejabberd_config:get_local_option(s2s_dns_options) of
                  L when is_list(L) -> L;
                  _ -> []
              end,
    TimeoutMs = timer:seconds(proplists:get_value(timeout, Options, 10)),
    Retries = proplists:get_value(retries, Options, 2),
    srv_lookup(Server, TimeoutMs, Retries).


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


test_get_addr_port(Server) ->
    lists:foldl(
      fun(_, Acc) ->
              [HostPort | _] = get_addr_port(Server),
              case lists:keysearch(HostPort, 1, Acc) of
                  false ->
                      [{HostPort, 1} | Acc];
                  {value, {_, Num}} ->
                      lists:keyreplace(HostPort, 1, Acc, {HostPort, Num + 1})
              end
      end, [], lists:seq(1, 100000)).


-spec get_addrs(Host :: atom() | binary() | string(),
                Family :: 'inet4' | 'inet6' | 'ipv4' | 'ipv6'
                ) -> [inet:ip_address()].
get_addrs(Host, Family) when is_binary(Host) ->
    get_addrs(binary_to_list(Host), Family);
get_addrs(Host, Family) ->
    Type = case Family of
               inet4 -> inet;
               ipv4 -> inet;
               inet6 -> inet6;
               ipv6 -> inet6
           end,
    case inet:gethostbyname(Host, Type) of
        {ok, #hostent{h_addr_list = Addrs}} ->
            ?LOG_DEBUG(#{what => s2s_srv_resolve_success,
                         type => Type, server => Host, addresses => Addrs}),
            Addrs;
        {error, Reason} ->
            ?LOG_DEBUG(#{what => s2s_srv_resolve_failed,
                         type => Type, server => Host, reason => Reason}),
            []
    end.


-spec outgoing_s2s_port() -> integer().
outgoing_s2s_port() ->
    case ejabberd_config:get_local_option(outgoing_s2s_port) of
        Port when is_integer(Port) ->
            Port;
        undefined ->
            5269
    end.


-spec outgoing_s2s_families() -> ['ipv4' | 'ipv6', ...].
outgoing_s2s_families() ->
    case ejabberd_config:get_local_option(outgoing_s2s_families) of
        Families when is_list(Families) ->
            Families;
        undefined ->
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
            [ipv4, ipv6]
    end.


-spec outgoing_s2s_timeout() -> non_neg_integer() | infinity.
outgoing_s2s_timeout() ->
    case ejabberd_config:get_local_option(outgoing_s2s_timeout) of
        Timeout when is_integer(Timeout) ->
            Timeout;
        infinity ->
            infinity;
        undefined ->
            %% 10 seconds
            10000
    end.


%% @doc Human readable S2S logging: Log only new outgoing connections as INFO
%% Do not log dialback
log_s2s_out(false, _, _, _) -> ok;
%% Log new outgoing connections:
log_s2s_out(_, Myname, Server, Tls) ->
    ?LOG_INFO(#{what => s2s_out,
                text => <<"Trying to open s2s connection">>,
                myname => Myname, server => Server, tls => Tls}).

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
    %% bounce queue manage by process and Erlang message queue
    bounce_queue(StateData#state.queue, mongoose_xmpp_errors:remote_server_not_found()),
    bounce_messages(mongoose_xmpp_errors:remote_server_not_found()),
    cancel_timer(StateData#state.timer),
    Delay = case StateData#state.delay_to_retry of
                undefined_delay ->
                    %% The initial delay is random between 1 and 15 seconds
                    %% Return a random integer between 1000 and 15000
                    MicroSecs = erlang:system_time(microsecond),
                    (MicroSecs rem 14000) + 1000;
                D1 ->
                    %% Duplicate the delay with each successive failed
                    %% reconnection attempt, but don't exceed the max
                    lists:min([D1 * 2, get_max_retry_delay()])
            end,
    Timer = erlang:start_timer(Delay, self(), []),
    {next_state, wait_before_retry, StateData#state{timer=Timer,
                                                    delay_to_retry = Delay,
                                                    queue = queue:new()}}.


%% @doc Get the maximum allowed delay for retry to reconnect (in milliseconds).
%% The default value is 5 minutes.
%% The option {s2s_max_retry_delay, Seconds} can be used (in seconds).
get_max_retry_delay() ->
    case ejabberd_config:get_local_option(s2s_max_retry_delay) of
        Seconds when is_integer(Seconds) ->
            Seconds*1000;
        _ ->
            ?MAX_RETRY_DELAY
    end.


%% @doc Terminate s2s_out connections that are in state wait_before_retry
terminate_if_waiting_delay(From, To) ->
    FromTo = {From, To},
    Pids = ejabberd_s2s:get_connections_pids(FromTo),
    lists:foreach(
      fun(Pid) ->
              Pid ! terminate_if_waiting_before_retry
      end,
      Pids).


-spec fsm_limit_opts() -> [{'max_queue', integer()}].
fsm_limit_opts() ->
    case ejabberd_config:get_local_option(max_fsm_queue) of
        N when is_integer(N) ->
            [{max_queue, N}];
        _ ->
            []
    end.


-spec get_addr_list(jid:server()) -> [{inet:ip_address(), inet:port_number()}].
get_addr_list(Server) ->
    case get_predefined_addresses(Server) of
        [] ->
            case ejabberd_s2s:domain_utf8_to_ascii(Server) of
                false -> [];
                ASCIIAddr -> get_addr_port(ASCIIAddr)
            end;

        Addrs ->
            Addrs
    end.


%% @doc Get IPs predefined for a given s2s domain in the configuration
-spec get_predefined_addresses(jid:server()) -> [{inet:ip_address(), inet:port_number()}].
get_predefined_addresses(Server) ->
    S2SAddr = ejabberd_config:get_local_option({s2s_addr, Server}),
    do_get_predefined_addresses(S2SAddr).

-spec do_get_predefined_addresses(undefined | string() | inet:ip_address() |
                                  {string() | inet:ip_address(), non_neg_integer()}) ->
                                         [{inet:ip_address(), non_neg_integer()}].
do_get_predefined_addresses(undefined) ->
    [];
do_get_predefined_addresses({{_, _, _, _}, Port} = IP4Port) when is_integer(Port) ->
    [IP4Port];
do_get_predefined_addresses({{_, _, _, _, _, _, _, _}, Port} = IP6Port) when is_integer(Port) ->
    [IP6Port];
do_get_predefined_addresses({_, _, _, _} = IP4) ->
    [{IP4, outgoing_s2s_port()}];
do_get_predefined_addresses({_, _, _, _, _, _, _, _} = IP6) ->
    [{IP6, outgoing_s2s_port()}];
do_get_predefined_addresses({List, Port}) when is_list(List), is_integer(Port) ->
    {ok, Addr} = inet:parse_strict_address(List),
    do_get_predefined_addresses({Addr, Port});
do_get_predefined_addresses(List) when is_list(List) ->
    do_get_predefined_addresses({List, outgoing_s2s_port()}).

send_event(<<"valid">>, Pid, StateData) ->
    p1_fsm:send_event(
      Pid, {valid,
            StateData#state.server,
            StateData#state.myname});
send_event(_, Pid, StateData) ->
    p1_fsm:send_event(
      Pid, {invalid,
            StateData#state.server,
            StateData#state.myname}).

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

get_tls_opts_with_certfile(StateData) ->
    case ejabberd_config:get_local_option(
           {domain_certfile,
            binary_to_list(StateData#state.myname)}) of
        undefined ->
            StateData#state.tls_options;
        CertFile ->
            [{certfile, CertFile} |
             lists:keydelete(
               certfile, 1,
               StateData#state.tls_options)]
    end.

get_tls_opts_with_ciphers(TLSOpts) ->
    case ejabberd_config:get_local_option(s2s_ciphers) of
        undefined ->
            TLSOpts;
        Ciphers ->
            [{ciphers, Ciphers} | TLSOpts]
    end.

calc_addr_index({Priority, Weight, Port, Host}) ->
    N = case Weight of
            0 -> 0;
            _ -> (Weight + 1) * rand:uniform()
        end,
    {Priority * 65536 - N, Host, Port}.

remove_addr_index(List) ->
    lists:map(
      fun({_, Host, Port}) ->
              {Host, Port}
      end, List).

handle_parsed_features({false, false, _, StateData = #state{authenticated = true}}) ->
    send_queue(StateData, StateData#state.queue),
    ?LOG_INFO(#{what => s2s_out_connected,
                text => <<"New outgoing s2s connection established">>,
                myname => StateData#state.myname, server => StateData#state.server}),
    mongoose_hooks:s2s_connect_hook(StateData#state.myname,
                                    ok,
                                    StateData#state.server),
    {next_state, stream_established,
     StateData#state{queue = queue:new()}};
handle_parsed_features({true, _, _, StateData = #state{try_auth = true, new = New}}) when
    New /= false ->
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
    ejabberd_socket:close(StateData#state.socket),
    {next_state, reopen_socket,
     StateData#state{socket = undefined,
                     use_v10 = false}, ?FSMTIMEOUT};
handle_parsed_features({_, _, _, StateData = #state{db_enabled = true}}) ->
    send_db_request(StateData);
handle_parsed_features({_, _, _, StateData}) ->
    ?LOG_DEBUG(#{what => s2s_out_restarted,
                 myname => StateData#state.myname, server => StateData#state.server}),
    % TODO: clear message queue
    ejabberd_socket:close(StateData#state.socket),
    {next_state, reopen_socket, StateData#state{socket = undefined,
                                                use_v10 = false}, ?FSMTIMEOUT}.
