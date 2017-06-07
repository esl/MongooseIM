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
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
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

-include("ejabberd.hrl").
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
%% Specified in miliseconds. Default value is 5 minutes.
-define(MAX_RETRY_DELAY, 300000).

-define(STREAM_HEADER,
        <<"<?xml version='1.0'?>"
        "<stream:stream "
        "xmlns:stream='http://etherx.jabber.org/streams' "
        "xmlns='jabber:server' "
        "xmlns:db='jabber:server:dialback' "
        "from='~s' "
        "to='~s'~s>">>
       ).

-define(STREAM_TRAILER, <<"</stream:stream>">>).

-define(INVALID_NAMESPACE_ERR,
        exml:to_binary(?SERR_INVALID_NAMESPACE)).

-define(HOST_UNKNOWN_ERR,
        exml:to_binary(?SERR_HOST_UNKNOWN)).

-define(INVALID_XML_ERR,
        exml:to_binary(?SERR_XML_NOT_WELL_FORMED)).

-define(SOCKET_DEFAULT_RESULT, {error, badarg}).

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
    ?DEBUG("started: ~p", [{From, Server, Type}]),
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
    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
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
    ?DEBUG("open_socket: ~p", [{StateData#state.myname,
                                StateData#state.server,
                                StateData#state.new,
                                StateData#state.verify}]),
    AddrList = get_predefined_addresses(StateData#state.server) ++
               case ejabberd_s2s:domain_utf8_to_ascii(StateData#state.server) of
                   false ->
                       [];
                   ASCIIAddr ->
                       get_addr_port(ASCIIAddr)
               end,
    case lists:foldl(fun({Addr, Port}, Acc) ->
                             case Acc of
                                 {ok, Socket} ->
                                     {ok, Socket};
                                 _ ->
                                     open_socket1(Addr, Port)
                             end
                     end, ?SOCKET_DEFAULT_RESULT, AddrList) of
        {ok, Socket} ->
            Version = if
                          StateData#state.use_v10 ->
                              <<" version='1.0'">>;
                          true ->
                              <<"">>
                      end,
            NewStateData = StateData#state{socket = Socket,
                                           tls_enabled = false,
                                           streamid = new_id()},
            send_text(NewStateData, list_to_binary(
                                      io_lib:format(?STREAM_HEADER,
                                                    [StateData#state.myname, StateData#state.server,
                                                     Version]))),
            {next_state, wait_for_stream, NewStateData, ?FSMTIMEOUT};
        {error, _Reason} ->
            ?INFO_MSG("s2s connection: ~s -> ~s (remote server not found)",
                      [StateData#state.myname, StateData#state.server]),
            case ejabberd_hooks:run_fold(find_s2s_bridge,
                                         undefined,
                                         [StateData#state.myname,
                                          StateData#state.server]) of
                {Mod, Fun, Type} ->
                    ?INFO_MSG("found a bridge to ~s for: ~s -> ~s",
                              [Type, StateData#state.myname,
                               StateData#state.server]),
                    NewStateData = StateData#state{bridge={Mod, Fun}},
                    {next_state, relay_to_bridge, NewStateData};
                _ ->
                    wait_before_reconnect(StateData)
            end
    end;
open_socket(closed, StateData) ->
    ?INFO_MSG("s2s connection: ~s -> ~s (stopped in open socket)",
              [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
open_socket(timeout, StateData) ->
    ?INFO_MSG("s2s connection: ~s -> ~s (timeout in open socket)",
              [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
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
    ?DEBUG("s2s_out: connecting to ~p:~p~n", [Addr, Port]),
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
            ?DEBUG("s2s_out: connect return ~p~n", [Reason]),
            R;
        {'EXIT', Reason} ->
            ?DEBUG("s2s_out: connect crashed ~p~n", [Reason]),
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
            send_text(StateData, ?INVALID_NAMESPACE_ERR),
            ?INFO_MSG("Closing s2s connection: ~s -> ~s (invalid namespace).~n"
                      "Namespace provided: ~p~nNamespace expected: \"jabber:server\"~n"
                      "xmlns:db provided: ~p~nAll attributes: ~p",
                      [StateData#state.myname, StateData#state.server, NSProvided, DB, Attrs]),
            {stop, normal, StateData}
    end;
wait_for_stream({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
              <<(?INVALID_XML_ERR)/binary, (?STREAM_TRAILER)/binary>>),
    ?INFO_MSG("Closing s2s connection: ~s -> ~s (invalid xml)",
              [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
wait_for_stream({xmlstreamend, _Name}, StateData) ->
    ?INFO_MSG("Closing s2s connection: ~s -> ~s (xmlstreamend)",
              [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
wait_for_stream(timeout, StateData) ->
    ?INFO_MSG("Closing s2s connection: ~s -> ~s (timeout in wait_for_stream)",
              [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
wait_for_stream(closed, StateData) ->
    ?INFO_MSG("Closing s2s connection: ~s -> ~s (close in wait_for_stream)",
              [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData}.


-spec wait_for_validation(ejabberd:xml_stream_item(), state()) -> fsm_return().
wait_for_validation({xmlstreamelement, El}, StateData) ->
    case is_verify_res(El) of
        {result, To, From, Id, Type} ->
            ?DEBUG("recv result: ~p", [{From, To, Id, Type}]),
            case {Type, StateData#state.tls_enabled, StateData#state.tls_required} of
                {<<"valid">>, Enabled, Required} when (Enabled==true) or (Required==false) ->
                    send_queue(StateData, StateData#state.queue),
                    ?INFO_MSG("Connection established: ~s -> ~s with TLS=~p",
                              [StateData#state.myname, StateData#state.server, StateData#state.tls_enabled]),
                    ejabberd_hooks:run(s2s_connect_hook,
                                       [StateData#state.myname,
                                        StateData#state.server]),
                    {next_state, stream_established,
                     StateData#state{queue = queue:new()}};
                {<<"valid">>, Enabled, Required} when (Enabled==false) and (Required==true) ->
                    %% TODO: bounce packets
                    ?INFO_MSG("Closing s2s connection: ~s -> ~s (TLS is required but unavailable)",
                              [StateData#state.myname, StateData#state.server]),
                    {stop, normal, StateData};
                _ ->
                    %% TODO: bounce packets
                    ?INFO_MSG("Closing s2s connection: ~s -> ~s (invalid dialback key)",
                              [StateData#state.myname, StateData#state.server]),
                    {stop, normal, StateData}
            end;
        {verify, To, From, Id, Type} ->
            ?DEBUG("recv verify: ~p", [{From, To, Id, Type}]),
            case StateData#state.verify of
                false ->
                    NextState = wait_for_validation,
                    %% TODO: Should'nt we close the connection here ?
                    {next_state, NextState, StateData,
                     get_timeout_interval(NextState)};
                {Pid, _Key, _SID} ->
                    case Type of
                        <<"valid">> ->
                            p1_fsm:send_event(
                              Pid, {valid,
                                    StateData#state.server,
                                    StateData#state.myname});
                        _ ->
                            p1_fsm:send_event(
                              Pid, {invalid,
                                    StateData#state.server,
                                    StateData#state.myname})
                    end,
                    if
                        StateData#state.verify == false ->
                            {stop, normal, StateData};
                        true ->
                            NextState = wait_for_validation,
                            {next_state, NextState, StateData,
                             get_timeout_interval(NextState)}
                    end
            end;
        _ ->
            {next_state, wait_for_validation, StateData, ?FSMTIMEOUT*3}
    end;
wait_for_validation({xmlstreamend, _Name}, StateData) ->
    ?INFO_MSG("wait for validation: ~s -> ~s (xmlstreamend)",
              [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
wait_for_validation({xmlstreamerror, _}, StateData) ->
    ?INFO_MSG("wait for validation: ~s -> ~s (xmlstreamerror)",
              [StateData#state.myname, StateData#state.server]),
    send_text(StateData,
              <<(?INVALID_XML_ERR)/binary, (?STREAM_TRAILER)/binary>>),
    {stop, normal, StateData};
wait_for_validation(timeout, #state{verify = {VPid, VKey, SID}} = StateData)
  when is_pid(VPid) and is_binary(VKey) and is_binary(SID) ->
    %% This is an auxiliary s2s connection for dialback.
    %% This timeout is normal and doesn't represent a problem.
    ?DEBUG("wait_for_validation: ~s -> ~s (timeout in verify connection)",
           [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
wait_for_validation(timeout, StateData) ->
    ?INFO_MSG("wait_for_validation: ~s -> ~s (connect timeout)",
              [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
wait_for_validation(closed, StateData) ->
    ?INFO_MSG("wait for validation: ~s -> ~s (closed)",
              [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData}.


-spec wait_for_features(ejabberd:xml_stream_item(), state()) -> fsm_return().
wait_for_features({xmlstreamelement, El}, StateData) ->
    case El of
        #xmlel{name = <<"stream:features">>, children = Els} ->
            {SASLEXT, StartTLS, StartTLSRequired} =
                lists:foldl(
                  fun(#xmlel{name = <<"mechanisms">>, attrs = Attrs1,
                             children = Els1} = _El1,
                      {_SEXT, STLS, STLSReq} = Acc) ->
                          case xml:get_attr_s(<<"xmlns">>, Attrs1) of
                              ?NS_SASL ->
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
                              _ ->
                                  Acc
                          end;
                     (#xmlel{name = <<"starttls">>, attrs = Attrs1} = El1,
                      {SEXT, _STLS, _STLSReq} = Acc) ->
                          case xml:get_attr_s(<<"xmlns">>, Attrs1) of
                              ?NS_TLS ->
                                  Req = case xml:get_subtag(El1, <<"required">>) of
                                            #xmlel{} -> true;
                                            false -> false
                                        end,
                                  {SEXT, true, Req};
                              _ ->
                                  Acc
                          end;
                     (_, Acc) ->
                          Acc
                  end, {false, false, false}, Els),
            if
                (not SASLEXT) and (not StartTLS) and
                StateData#state.authenticated ->
                    send_queue(StateData, StateData#state.queue),
                    ?INFO_MSG("Connection established: ~s -> ~s",
                              [StateData#state.myname, StateData#state.server]),
                    ejabberd_hooks:run(s2s_connect_hook,
                                       [StateData#state.myname,
                                        StateData#state.server]),
                    {next_state, stream_established,
                     StateData#state{queue = queue:new()}};
                SASLEXT and StateData#state.try_auth and
                (StateData#state.new /= false) ->
                    send_element(StateData,
                                  #xmlel{name = <<"auth">>,
                                         attrs = [{<<"xmlns">>, ?NS_SASL},
                                                  {<<"mechanism">>, <<"EXTERNAL">>}],
                                          children = [#xmlcdata{content = jlib:encode_base64(
                                                                            StateData#state.myname)}]}),
                     {next_state, wait_for_auth_result,
                      StateData#state{try_auth = false}, ?FSMTIMEOUT};
                 StartTLS and StateData#state.tls and
                  (not StateData#state.tls_enabled) ->
                     send_element(StateData,
                                  #xmlel{name = <<"starttls">>,
                                         attrs = [{<<"xmlns">>, ?NS_TLS}]}),
                     {next_state, wait_for_starttls_proceed, StateData,
                      ?FSMTIMEOUT};
                 StartTLSRequired and (not StateData#state.tls) ->
                     ?DEBUG("restarted: ~p", [{StateData#state.myname,
                                               StateData#state.server}]),
                     ejabberd_socket:close(StateData#state.socket),
                     {next_state, reopen_socket,
                      StateData#state{socket = undefined,
                                      use_v10 = false}, ?FSMTIMEOUT};
                 StateData#state.db_enabled ->
                     send_db_request(StateData);
                 true ->
                     ?DEBUG("restarted: ~p", [{StateData#state.myname,
                                               StateData#state.server}]),
                     % TODO: clear message queue
                     ejabberd_socket:close(StateData#state.socket),
                     {next_state, reopen_socket, StateData#state{socket = undefined,
                                                                 use_v10 = false}, ?FSMTIMEOUT}
            end;
        _ ->
            send_text(StateData,
                      <<(exml:to_binary(?SERR_BAD_FORMAT))/binary,
                      (?STREAM_TRAILER)/binary>>),
            ?INFO_MSG("Closing s2s connection: ~s -> ~s (bad format)",
                      [StateData#state.myname, StateData#state.server]),
            {stop, normal, StateData}
    end;
wait_for_features({xmlstreamend, _Name}, StateData) ->
    ?INFO_MSG("wait_for_features: xmlstreamend", []),
    {stop, normal, StateData};
wait_for_features({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
              <<(?INVALID_XML_ERR)/binary, (?STREAM_TRAILER)/binary>>),
    ?INFO_MSG("wait for features: xmlstreamerror", []),
    {stop, normal, StateData};
wait_for_features(timeout, StateData) ->
    ?INFO_MSG("wait for features: timeout", []),
    {stop, normal, StateData};
wait_for_features(closed, StateData) ->
    ?INFO_MSG("wait for features: closed", []),
    {stop, normal, StateData}.


-spec wait_for_auth_result(ejabberd:xml_stream_item(), state()) -> fsm_return().
wait_for_auth_result({xmlstreamelement, El}, StateData) ->
    case El of
        #xmlel{name = <<"success">>, attrs = Attrs} ->
            case xml:get_attr_s(<<"xmlns">>, Attrs) of
                ?NS_SASL ->
                    ?DEBUG("auth: ~p", [{StateData#state.myname,
                                         StateData#state.server}]),
                    send_text(StateData,
                              list_to_binary(
                                io_lib:format(?STREAM_HEADER,
                                              [StateData#state.myname, StateData#state.server,
                                               <<" version='1.0'">>]))),
                    {next_state, wait_for_stream,
                     StateData#state{streamid = new_id(),
                                     authenticated = true
                                    }, ?FSMTIMEOUT};
                _ ->
                    send_text(StateData,
                              <<(exml:to_binary(?SERR_BAD_FORMAT))/binary,
                              (?STREAM_TRAILER)/binary>>),
                    ?INFO_MSG("Closing s2s connection: ~s -> ~s (bad format)",
                              [StateData#state.myname, StateData#state.server]),
                    {stop, normal, StateData}
            end;
        #xmlel{name = <<"failure">>, attrs = Attrs} ->
            case xml:get_attr_s(<<"xmlns">>, Attrs) of
                ?NS_SASL ->
                    ?DEBUG("restarted: ~p", [{StateData#state.myname,
                                              StateData#state.server}]),
                    ejabberd_socket:close(StateData#state.socket),
                    {next_state, reopen_socket,
                     StateData#state{socket = undefined}, ?FSMTIMEOUT};
                _ ->
                    send_text(StateData,
                              <<(exml:to_binary(?SERR_BAD_FORMAT))/binary,
                              (?STREAM_TRAILER)/binary>>),
                    ?INFO_MSG("Closing s2s connection: ~s -> ~s (bad format)",
                              [StateData#state.myname, StateData#state.server]),
                    {stop, normal, StateData}
            end;
        _ ->
            send_text(StateData,
                      <<(exml:to_binary(?SERR_BAD_FORMAT))/binary,
                              (?STREAM_TRAILER)/binary>>),
            ?INFO_MSG("Closing s2s connection: ~s -> ~s (bad format)",
                      [StateData#state.myname, StateData#state.server]),
            {stop, normal, StateData}
    end;
wait_for_auth_result({xmlstreamend, _Name}, StateData) ->
    ?INFO_MSG("wait for auth result: xmlstreamend", []),
    {stop, normal, StateData};
wait_for_auth_result({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
              <<(?INVALID_XML_ERR)/binary, (?STREAM_TRAILER)/binary>>),
    ?INFO_MSG("wait for auth result: xmlstreamerror", []),
    {stop, normal, StateData};
wait_for_auth_result(timeout, StateData) ->
    ?INFO_MSG("wait for auth result: timeout", []),
    {stop, normal, StateData};
wait_for_auth_result(closed, StateData) ->
    ?INFO_MSG("wait for auth result: closed", []),
    {stop, normal, StateData}.


-spec wait_for_starttls_proceed(ejabberd:xml_stream_item(), state()) -> fsm_return().
wait_for_starttls_proceed({xmlstreamelement, El}, StateData) ->
    case El of
        #xmlel{name = <<"proceed">>, attrs = Attrs} ->
            case xml:get_attr_s(<<"xmlns">>, Attrs) of
                ?NS_TLS ->
                    ?DEBUG("starttls: ~p", [{StateData#state.myname,
                                             StateData#state.server}]),
                    Socket = StateData#state.socket,
                    TLSOpts = case ejabberd_config:get_local_option(
                                     {domain_certfile,
                                      binary_to_list(StateData#state.myname)}) of
                                  undefined ->
                                      StateData#state.tls_options;
                                  CertFile ->
                                      [{certfile, CertFile} |
                                       lists:keydelete(
                                         certfile, 1,
                                         StateData#state.tls_options)]
                              end,
                    TLSOpts2 = case ejabberd_config:get_local_option(s2s_ciphers) of
                                       undefined ->
                                               TLSOpts;
                                       Ciphers ->
                                               [{ciphers, Ciphers} | TLSOpts]
                               end,
                    TLSSocket = ejabberd_socket:starttls(Socket, TLSOpts2),
                    NewStateData = StateData#state{socket = TLSSocket,
                                                   streamid = new_id(),
                                                   tls_enabled = true,
                                                   tls_options = TLSOpts2
                                                  },
                    send_text(NewStateData,
                      list_to_binary(
                        io_lib:format(?STREAM_HEADER,
                                      [StateData#state.myname, StateData#state.server,
                                       <<" version='1.0'">>]))),
                    {next_state, wait_for_stream, NewStateData, ?FSMTIMEOUT};
                _ ->
                    send_text(StateData,
                              <<(exml:to_binary(?SERR_BAD_FORMAT))/binary,
                              (?STREAM_TRAILER)/binary>>),
                    ?INFO_MSG("Closing s2s connection: ~s -> ~s (bad format)",
                              [StateData#state.myname, StateData#state.server]),
                    {stop, normal, StateData}
            end;
        _ ->
            ?INFO_MSG("Closing s2s connection: ~s -> ~s (bad format)",
                      [StateData#state.myname, StateData#state.server]),
            {stop, normal, StateData}
    end;
wait_for_starttls_proceed({xmlstreamend, _Name}, StateData) ->
    ?INFO_MSG("wait for starttls proceed: xmlstreamend", []),
    {stop, normal, StateData};
wait_for_starttls_proceed({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
              <<(?INVALID_XML_ERR)/binary, (?STREAM_TRAILER)/binary>>),
    ?INFO_MSG("wait for starttls proceed: xmlstreamerror", []),
    {stop, normal, StateData};
wait_for_starttls_proceed(timeout, StateData) ->
    ?INFO_MSG("wait for starttls proceed: timeout", []),
    {stop, normal, StateData};
wait_for_starttls_proceed(closed, StateData) ->
    ?INFO_MSG("wait for starttls proceed: closed", []),
    {stop, normal, StateData}.


-spec reopen_socket(ejabberd:xml_stream_item(), state()) -> fsm_return().
reopen_socket({xmlstreamelement, _El}, StateData) ->
    {next_state, reopen_socket, StateData, ?FSMTIMEOUT};
reopen_socket({xmlstreamend, _Name}, StateData) ->
    {next_state, reopen_socket, StateData, ?FSMTIMEOUT};
reopen_socket({xmlstreamerror, _}, StateData) ->
    {next_state, reopen_socket, StateData, ?FSMTIMEOUT};
reopen_socket(timeout, StateData) ->
    ?INFO_MSG("reopen socket: timeout", []),
    {stop, normal, StateData};
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
    ?INFO_MSG("relay to bridge: ~s -> ~s (closed)",
              [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
relay_to_bridge(_Event, StateData) ->
    {next_state, relay_to_bridge, StateData}.


-spec stream_established(ejabberd:xml_stream_item(), state()) -> fsm_return().
stream_established({xmlstreamelement, El}, StateData) ->
    ?DEBUG("s2S stream established", []),
    case is_verify_res(El) of
        {verify, VTo, VFrom, VId, VType} ->
            ?DEBUG("recv verify: ~p", [{VFrom, VTo, VId, VType}]),
            case StateData#state.verify of
                {VPid, _VKey, _SID} ->
                    case VType of
                        <<"valid">> ->
                            p1_fsm:send_event(
                              VPid, {valid,
                                     StateData#state.server,
                                     StateData#state.myname});
                        _ ->
                            p1_fsm:send_event(
                              VPid, {invalid,
                                     StateData#state.server,
                                     StateData#state.myname})
                    end;
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    {next_state, stream_established, StateData};
stream_established({xmlstreamend, _Name}, StateData) ->
    ?INFO_MSG("Connection closed in stream established: ~s -> ~s (xmlstreamend)",
              [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
stream_established({xmlstreamerror, _}, StateData) ->
    send_text(StateData,
              <<(?INVALID_XML_ERR)/binary, (?STREAM_TRAILER)/binary>>),
    ?INFO_MSG("stream established: ~s -> ~s (xmlstreamerror)",
              [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
stream_established(timeout, StateData) ->
    ?INFO_MSG("stream established: ~s -> ~s (timeout)",
              [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData};
stream_established(closed, StateData) ->
    ?INFO_MSG("stream established: ~s -> ~s (closed)",
              [StateData#state.myname, StateData#state.server]),
    {stop, normal, StateData}.


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
                      {ok, {A, P}} ->  {A, P};
                      {error, _} -> {unknown, unknown}
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
    ?ERROR_MSG("{s2s_out:send_text, Text}: ~p~n", [{send_text, Text}]), % is it ever called?
    send_text(StateData, Text),
    cancel_timer(StateData#state.timer),
    Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
    {next_state, StateName, StateData#state{timer = Timer},
     get_timeout_interval(StateName)};
handle_info({send_element, Acc, El}, StateName, StateData) ->
    case StateName of
        stream_established ->
            cancel_timer(StateData#state.timer),
            Timer = erlang:start_timer(?S2STIMEOUT, self(), []),
            send_element(StateData, El),
            {next_state, StateName, StateData#state{timer = Timer}};
        %% In this state we bounce all message: We are waiting before
        %% trying to reconnect
        wait_before_retry ->
            bounce_element(Acc, El, ?ERR_REMOTE_SERVER_NOT_FOUND),
            {next_state, StateName, StateData};
        relay_to_bridge ->
            %% In this state we relay all outbound messages
            %% to a foreign protocol bridge such as SMTP, SIP, etc.
            {Mod, Fun} = StateData#state.bridge,
            ?DEBUG("relaying stanza via ~p:~p/1", [Mod, Fun]),
            case catch Mod:Fun(El) of
                {'EXIT', Reason} ->
                    ?ERROR_MSG("Error while relaying to bridge: ~p", [Reason]),
                    bounce_element(Acc, El, ?ERR_INTERNAL_SERVER_ERROR),
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
    ?INFO_MSG("Reconnect delay expired: Will now retry to connect to ~s when needed.", [StateData#state.server]),
    {stop, normal, StateData};
handle_info({timeout, Timer, _}, _StateName,
            #state{timer = Timer} = StateData) ->
    ?INFO_MSG("Closing connection with ~s: timeout", [StateData#state.server]),
    {stop, normal, StateData};
handle_info(terminate_if_waiting_before_retry, wait_before_retry, StateData) ->
    {stop, normal, StateData};
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
    ?DEBUG("terminated: ~p", [{Reason, StateName}]),
    case StateData#state.new of
        false ->
            ok;
        true ->
            ejabberd_s2s:remove_connection(
              {StateData#state.myname, StateData#state.server}, self())
    end,
    %% bounce queue manage by process and Erlang message queue
    bounce_queue(StateData#state.queue, ?ERR_REMOTE_SERVER_NOT_FOUND),
    bounce_messages(?ERR_REMOTE_SERVER_NOT_FOUND),
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


-spec send_element(state(), jlib:xmlel()|mongoose_acc:t()) -> 'ok'.
send_element(StateData, #xmlel{} = El) ->
    send_text(StateData, exml:to_binary(El)).

-spec send_element(state(), mongoose_acc:t(), jlib:xmlel()) -> mongoose_acc:t().
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
-spec bounce_element(Acc :: mongoose_acc:t(), El :: xmlel(), Error :: xmlel()) -> 'ok'.
bounce_element(Acc, El, Error) ->
    case mongoose_acc:get(type, Acc) of
        <<"error">> -> ok;
        <<"result">> -> ok;
        _ ->
            From = mongoose_acc:get(from_jid, Acc),
            To = mongoose_acc:get(to_jid, Acc),
            Err = jlib:make_error_reply(El, Error),
            ejabberd_router:route(To, From, Acc, Err)
    end.


-spec bounce_queue(Q :: element_queue(), Error :: jlib:xmlel()) -> 'ok'.
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
    list_to_binary(randoms:get_string()).


-spec cancel_timer(reference()) -> 'ok'.
cancel_timer(Timer) ->
    erlang:cancel_timer(Timer),
    receive
        {timeout, Timer, _} ->
            ok
    after 0 ->
            ok
    end.


-spec bounce_messages(jlib:xmlel()) -> 'ok'.
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
        _:_ ->
            {stop, normal, NewStateData}
    end.


-spec is_verify_res(jlib:xmlel()) -> 'false' | {'result', _, _, _, _} | {'verify', _, _, _, _}.
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

-spec get_addr_port(ejabberd:server()) -> [{inet:ip_address(), inet:port_number()}].
get_addr_port(Server) ->
    Res = srv_lookup(Server),
    case Res of
        {error, Reason} ->
            ?DEBUG("srv lookup of '~s' failed: ~p~n", [Server, Reason]),
            [{Server, outgoing_s2s_port()}];
        {ok, #hostent{h_addr_list = AddrList}} ->
            %% Probabilities are not exactly proportional to weights
            %% for simplicity (higher weigths are overvalued)
            random:seed(randoms:good_seed()),
            case (catch lists:map(
                          fun({Priority, Weight, Port, Host}) ->
                                  N = case Weight of
                                          0 -> 0;
                                          _ -> (Weight + 1) * random:uniform()
                                      end,
                                  {Priority * 65536 - N, Host, Port}
                          end, AddrList)) of
                {'EXIT', _Reason} ->
                    [{Server, outgoing_s2s_port()}];
                SortedList ->
                    List = lists:map(
                             fun({_, Host, Port}) ->
                                     {Host, Port}
                             end, lists:keysort(1, SortedList)),
                    ?DEBUG("srv lookup of '~s': ~p~n", [Server, List]),
                    List
            end
    end.


-spec srv_lookup(ejabberd:server()) -> {'error', atom()} | {'ok', inet:hostent()}.
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
-spec srv_lookup(ejabberd:server(),
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
                    ?ERROR_MSG("The DNS servers~n  ~p~ntimed out on request"
                               " for ~p IN SRV."
                               " You should check your DNS configuration.",
                               [inet_db:res_option(nameserver), Server]),
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
            ?DEBUG("~s of ~s resolved to: ~p~n", [Type, Host, Addrs]),
            Addrs;
        {error, Reason} ->
            ?DEBUG("~s lookup of '~s' failed: ~p~n", [Type, Host, Reason]),
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
    case ejabberd_config:get_local_option(outgoing_s2s_options) of
        {Families, _} when is_list(Families) ->
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
    case ejabberd_config:get_local_option(outgoing_s2s_options) of
        {_, Timeout} when is_integer(Timeout) ->
            Timeout;
        {_, infinity} ->
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
    ?INFO_MSG("Trying to open s2s connection: ~s -> ~s with TLS=~p", [Myname, Server, Tls]).


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
    bounce_queue(StateData#state.queue, ?ERR_REMOTE_SERVER_NOT_FOUND),
    bounce_messages(?ERR_REMOTE_SERVER_NOT_FOUND),
    cancel_timer(StateData#state.timer),
    Delay = case StateData#state.delay_to_retry of
                undefined_delay ->
                    %% The initial delay is random between 1 and 15 seconds
                    %% Return a random integer between 1000 and 15000
                    {_, _, MicroSecs} = p1_time_compat:timestamp(),
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


%% @doc Get the maximum allowed delay for retry to reconnect (in miliseconds).
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


%% @doc Get IPs predefined for a given s2s domain in the configuration
-spec get_predefined_addresses(atom()) -> [{inet:ip_address(), inet:port_number()}].
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
