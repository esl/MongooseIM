%%%----------------------------------------------------------------------
%%% File    : ejabberd_listener.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Manage socket listener
%%% Created : 16 Nov 2002 by Alexey Shchepin <alexey@process-one.net>
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

-module(ejabberd_listener).
-author('alexey@process-one.net').

-export([start_link/0, init/1, start/3,
         init/3,
         accept/3,
         udp_recv/3,
         start_listeners/0,
         start_listener/3,
         stop_listeners/0,
         stop_listener/2,
         parse_listener_portip/2,
         add_listener/3,
         delete_listener/3,
         delete_listener/2
        ]).

-include("mongoose.hrl").

%% We do not block on send anymore.
-define(TCP_SEND_TIMEOUT, 15000).

-type proto() :: tcp | udp | ws | wss.
-type addr() :: inet:ip4_address() | string().
-type portnum() :: inet:port_number().
-type port_ip_proto() :: portnum() | {portnum(), addr() | proto()} | {portnum(), addr(), proto()}.

-spec start_link() -> 'ignore' | {'error', _} | {'ok', pid()}.
start_link() ->
    supervisor:start_link({local, ejabberd_listeners}, ?MODULE, []).


init(_) ->
    %bind_tcp_ports(),
    {ok, {{one_for_one, 10, 1}, []}}.


-spec start_listeners() -> 'ignore' | {'ok', {{_, _, _}, [any()]}}.
start_listeners() ->
    case ejabberd_config:get_local_option(listen) of
        undefined ->
            ignore;
        Ls ->
            Ls2 = lists:map(
                fun({Port, Module, Opts}) ->
                        case start_listener(Port, Module, Opts) of
                            {ok, _Pid} = R -> R;
                            {error, Error} ->
                                throw(Error)
                        end
                end, Ls),
            report_duplicated_portips(Ls),
            {ok, {{one_for_one, 10, 1}, Ls2}}
    end.

-spec report_duplicated_portips([integer() | tuple()]) -> ok.
report_duplicated_portips(L) ->
    LKeys = [Port || {Port, _, _} <- L],
    LNoDupsKeys = proplists:get_keys(L),
    case LKeys -- LNoDupsKeys of
        [] -> ok;
        Dups ->
            ?CRITICAL_MSG("In the ejabberd configuration there are duplicated "
                          "Port number + IP address:~n  ~p",
                          [Dups])
  end.

-spec start(Port :: _,
            Module :: atom() | tuple(),
            Opts :: [any()]) -> any().
start(Port, Module, Opts) ->
    %% at this point, Module:socket_type() must not be 'independent'
    start_dependent(Port, Module, Opts).

-spec start_dependent(Port :: _,
                      Module :: atom() | tuple(),
                      Opts :: [any()]) -> {ok, pid()} | {error, _}.
start_dependent(Port, Module, Opts) ->
    try check_listener_options(Opts) of
        ok ->
            proc_lib:start_link(?MODULE, init, [Port, Module, Opts])
    catch
        throw:{error, Error} ->
            ?ERROR_MSG(Error, []),
            {error, Error}
    end.

-spec init(PortIP :: port_ip_proto(),
           Module :: atom(),
           Opts :: [any()]) -> no_return().
init(PortIP, Module, RawOpts) ->
    {Port, IPT, IPS, IPV, Proto, OptsClean} = parse_listener_portip(PortIP, RawOpts),
    {Opts, SockOpts} = prepare_opts(IPT, IPV, OptsClean),

    if Proto == udp ->
            init_udp(PortIP, Module, Opts, SockOpts, Port, IPS);
       true ->
            init_tcp(PortIP, Module, Opts, SockOpts, Port, IPS)
    end.

-type udp_listen_option() :: gen_udp:option()
                           | {ip, _}
                           | {fd, pos_integer()}
                           | {ifaddr, _}
                           | inet:address_family()
                           | {port, inet:port_number()}.
-spec init_udp(PortIPProto :: port_ip_proto(),
               Module :: atom(),
               Opts :: [any(), ...],
               SockOpts :: [udp_listen_option()],
               Port :: inet:port_number(),
               IPS :: [any()]) -> no_return().
init_udp(PortIPProto, Module, Opts, SockOpts, Port, IPS) ->
    case gen_udp:open(Port, [binary,
                             {active, false},
                             {reuseaddr, true} |
                             SockOpts]) of
        {ok, Socket} ->
            %% Inform my parent that this port was opened succesfully
            proc_lib:init_ack({ok, self()}),
            ?MODULE:udp_recv(Socket, Module, Opts);
        {error, Reason} ->
            socket_error(Reason, PortIPProto, Module, SockOpts, Port, IPS)
    end.

-spec init_tcp(PortIPProto :: port_ip_proto(),
               Module :: atom(),
               Opts :: [any(), ...],
               SockOpts :: [gen_tcp:listen_option()],
               Port :: inet:port_number(),
               IPS :: [any()]) -> no_return().
init_tcp(PortIPProto, Module, Opts, SockOpts, Port, IPS) ->
    ListenSocket = listen_tcp(PortIPProto, Module, SockOpts, Port, IPS),
    %% Inform my parent that this port was opened succesfully
    proc_lib:init_ack({ok, self()}),
    %% And now start accepting connection attempts
    ?MODULE:accept(ListenSocket, Module, Opts).

-spec listen_tcp(PortIPPRoto :: port_ip_proto(),
                 Module :: atom(),
                 SockOpts :: [gen_tcp:listen_option()],
                 Port :: inet:port_number(),
                 IPS :: [any()]) -> port().
listen_tcp(PortIPProto, Module, SockOpts, Port, IPS) ->
    DefaultSockOpts = [binary,
                       {backlog, 100},
                       {packet, 0},
                       {active, false},
                       {reuseaddr, true},
                       {nodelay, true},
                       {send_timeout, 120},
                       {keepalive, true},
                       {send_timeout_close, true}],
    FinalSockOpts = override_sock_opts(SockOpts, DefaultSockOpts),
    Res = listen_or_retry(Port, FinalSockOpts, 10),
    case Res of
        {ok, ListenSocket} ->
            ListenSocket;
        {error, Reason} ->
            socket_error(Reason, PortIPProto, Module, SockOpts, Port, IPS)
    end.

%% Process exit and socket release are not transactional
%% So, there can be a short period of time when we can't bind
listen_or_retry(Port, SockOpts, Retries) ->
    case gen_tcp:listen(Port, SockOpts) of
        {ok, ListenSocket} ->
            {ok, ListenSocket};
        {error, eaddrinuse} when Retries > 0 ->
            timer:sleep(100),
            listen_or_retry(Port, SockOpts, Retries-1);
        {error, Reason} ->
            {error, Reason}
    end.

override_sock_opts([], Opts) ->
    Opts;
override_sock_opts([Override | OverrideOpts], Opts) ->
    NewOpts = do_override(Override, Opts),
    override_sock_opts(OverrideOpts, NewOpts).

do_override({ip, _} = IP, Opts) ->
    lists:keystore(ip, 1, Opts, IP);
do_override({backlog, _} = Backlog, Opts) ->
    lists:keystore(backlog, 1, Opts, Backlog);
do_override(inet6, Opts) ->
    [inet6 | lists:delete(inet6, Opts)];
do_override(inet, Opts) ->
    [inet | lists:delete(inet, Opts)];
do_override(_, Opts) ->
    Opts.


%% @doc Parse any kind of ejabberd listener specification.
%% The parsed options are returned in several formats.
%% OptsClean does not include inet/inet6 or ip options.
%% Opts can include the options inet6 and {ip, Tuple},
%% but they are only used when no IP address was specified in the PortIP.
%% The IP version (either IPv4 or IPv6) is inferred from the IP address type,
%% so the option inet/inet6 is only used when no IP is specified at all.
-spec parse_listener_portip(PortIPProto :: port_ip_proto(),
                            Opts :: [any()]
                            ) -> {portnum(),
                                  IPT :: tuple(),
                                  IPS :: [any()],
                                  IPV :: 'inet' | 'inet6',
                                  Proto :: proto(),
                                  OptsClean :: [any()]}.
parse_listener_portip(PortIPProto, Opts) ->
    {IPOpt, Opts2} = strip_ip_option(Opts),
    {IPVOpt, OptsClean} = case lists:member(inet6, Opts2) of
                              true -> {inet6, Opts2 -- [inet6]};
                              false -> {inet, Opts2}
                          end,
    {Port, IPT, IPS, Proto} =
        case add_proto(PortIPProto, Opts) of
            {P, Prot} ->
                T = get_ip_tuple(IPOpt, IPVOpt),
                S = inet_parse:ntoa(T),
                {P, T, S, Prot};
            {P, T, Prot} when is_integer(P) and is_tuple(T) ->
                S = inet_parse:ntoa(T),
                {P, T, S, Prot};
            {P, S, Prot} when is_integer(P) and is_list(S) ->
                [S | _] = string:tokens(S, "/"),
                {ok, T} = inet_parse:address(S),
                {P, T, S, Prot}
        end,
    IPV = case size(IPT) of
              4 -> inet;
              8 -> inet6
          end,
    {Port, IPT, IPS, IPV, Proto, OptsClean}.

-spec prepare_opts(IPT :: tuple(),
                   IPV :: 'inet' | 'inet6',
                   OptsClean :: [any()]) -> {[any(), ...], [any()]}.
prepare_opts(IPT, IPV, OptsClean) ->
    %% The first inet|inet6 and the last {ip, _} work,
    %% so overriding those in Opts
    Opts = [IPV | OptsClean] ++ [{ip, IPT}],
    SockOpts = lists:filter(fun({ip, _}) -> true;
                               (inet6) -> true;
                               (inet) -> true;
                               ({backlog, _}) -> true;
                               (_) -> false
                            end, Opts),
    {Opts, SockOpts}.


-spec add_proto(PortIPProto :: port_ip_proto(),
                Opts :: [any()]
                ) -> {P :: portnum(), proto()}
                   | {P :: portnum(), Addr :: addr(), proto()}.
add_proto(Port, Opts) when is_integer(Port) ->
    {Port, get_proto(Opts)};
add_proto({Port, Proto}, _Opts) when is_atom(Proto) ->
    {Port, normalize_proto(Proto)};
add_proto({Port, Addr}, Opts) ->
    {Port, Addr, get_proto(Opts)};
add_proto({Port, Addr, Proto}, _Opts) ->
    {Port, Addr, normalize_proto(Proto)}.

-spec strip_ip_option(Opts :: [any()]) -> {'no_ip_option' | tuple(), [any()]}.
strip_ip_option(Opts) ->
    {IPL, OptsNoIP} = lists:partition(
                        fun({ip, _}) -> true;
                           (_) -> false
                        end,
                        Opts),
    case IPL of
        %% Only the first ip option is considered
        [{ip, T1} | _] when is_tuple(T1) ->
            {T1, OptsNoIP};
        [] ->
            {no_ip_option, OptsNoIP}
    end.

-spec get_ip_tuple('no_ip_option' | tuple(), 'inet' | 'inet6'
                  ) -> 'no_ip_option' | tuple().
get_ip_tuple(no_ip_option, inet) ->
    {0, 0, 0, 0};
get_ip_tuple(no_ip_option, inet6) ->
    {0, 0, 0, 0, 0, 0, 0, 0};
get_ip_tuple(IPOpt, _IPVOpt) ->
    IPOpt.

-spec accept(Socket :: port(),
             Module :: atom(),
             Opts :: [any(), ...]) -> no_return().
accept(ListenSocket, Module, Opts) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            case {inet:sockname(Socket), inet:peername(Socket)} of
                {{ok, Addr}, {ok, PAddr}} ->
                    ?INFO_MSG("(~w) Accepted connection ~w -> ~w",
                              [Socket, PAddr, Addr]);
                _ ->
                    ok
            end,
            ejabberd_socket:start(Module, gen_tcp, Socket, Opts),
            ?MODULE:accept(ListenSocket, Module, Opts);
        {error, Reason} ->
            ?INFO_MSG("(~w) Failed TCP accept: ~w",
                      [ListenSocket, Reason]),
            ?MODULE:accept(ListenSocket, Module, Opts)
    end.

-spec udp_recv(Socket :: port(),
               Module :: atom(),
               Opts :: [any(), ...]) -> no_return().
udp_recv(Socket, Module, Opts) ->
    case gen_udp:recv(Socket, 0) of
        {ok, {Addr, Port, Packet}} ->
            case catch Module:udp_recv(Socket, Addr, Port, Packet, Opts) of
                {'EXIT', Reason} ->
                    ?ERROR_MSG("failed to process UDP packet:~n"
                               "** Source: {~p, ~p}~n"
                               "** Reason: ~p~n** Packet: ~p",
                               [Addr, Port, Reason, Packet]);
                _ ->
                    ok
            end,
            ?MODULE:udp_recv(Socket, Module, Opts);
        {error, Reason} ->
            ?ERROR_MSG("unexpected UDP error: ~s", [format_error(Reason)]),
            throw({error, Reason})
    end.

-spec start_listener(PortIPProto :: port_ip_proto(),
                     Module :: atom(),
                     Opts :: [any()]) -> {'error', pid()} | {'ok', _}.
start_listener(Port, Module, Opts) ->
    case start_listener2(Port, Module, Opts) of
        {ok, _Pid} = R -> R;
        {error, {{'EXIT', {undef, [{M, _F, _A}|_]}}, _} = Error} ->
            ?ERROR_MSG("Error starting the ejabberd listener: ~p.~n"
                       "It could not be loaded or is not an ejabberd listener.~n"
                       "Error: ~p~n", [Module, Error]),
            {error, {module_not_available, M}};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {error, Error} ->
            {error, Error}
    end.

start_listener2(Port, Module, Opts) ->
    %% It is only required to start the supervisor in some cases.
    %% But it doesn't hurt to attempt to start it for any listener.
    %% So, it's normal (and harmless) that in most cases this call returns: {error, {already_started, pid()}}
    start_module_sup(Port, Module),
    start_listener_sup(Port, Module, Opts).

-spec start_module_sup(_, Module :: module())
      -> {'error', _} | {'ok', 'undefined' | pid()} | {'ok', 'undefined' | pid(), _}.
start_module_sup(_PortIPProto, Module) ->
    Proc1 = gen_mod:get_module_proc("sup", Module),
    ChildSpec1 =
        {Proc1,
         {ejabberd_tmp_sup, start_link, [Proc1, Module]},
         permanent,
         infinity,
         supervisor,
         [ejabberd_tmp_sup]},
    supervisor:start_child(ejabberd_sup, ChildSpec1).

-spec start_listener_sup(port_ip_proto(), Module :: atom(), Opts :: [any()])
      -> {'error', _} | {'ok', 'undefined' | pid()} | {'ok', 'undefined' | pid(), _}.
start_listener_sup(PortIPProto, Module, Opts) ->
    case Module:socket_type() of
        independent ->
            Module:start_listener(PortIPProto, Opts);
        _ ->

            ChildSpec = {PortIPProto,
                         {?MODULE, start, [PortIPProto, Module, Opts]},
                         transient,
                         brutal_kill,
                         worker,
                         [?MODULE]},
            supervisor:start_child(ejabberd_listeners, ChildSpec)
    end.

-spec stop_listeners() -> 'ok'.
stop_listeners() ->
    Ports = ejabberd_config:get_local_option(listen),
    lists:foreach(
      fun({PortIpNetp, Module, _Opts}) ->
              stop_listener(PortIpNetp, Module)
      end,
      Ports).

-spec stop_listener(PortIPProto :: port_ip_proto(),
                    Module :: atom())
      -> 'ok' | {'error', 'not_found' | 'restarting' | 'running' | 'simple_one_for_one'}.
stop_listener(PortIPProto, _Module) ->
    supervisor:terminate_child(ejabberd_listeners, PortIPProto),
    supervisor:delete_child(ejabberd_listeners, PortIPProto).

%% @doc Add a listener and store in config if success
-type listener_option() :: inet | inet6 | {ip, tuple()} | atom() | tuple().
-spec add_listener(PortIPProto :: port_ip_proto(),
                   Module :: atom(),
                   Opts :: [listener_option()]
                   ) -> 'ok' | {'error', _}.
add_listener(PortIPProto, Module, Opts) ->
    {Port, IPT, _, _, Proto, _} = parse_listener_portip(PortIPProto, Opts),
    PortIP1 = {Port, IPT, Proto},
    case start_listener(PortIP1, Module, Opts) of
        {ok, _Pid} ->
            Ports = case ejabberd_config:get_local_option(listen) of
                        undefined ->
                            [];
                        Ls ->
                            Ls
                    end,
            Ports1 = lists:keydelete(PortIP1, 1, Ports),
            Ports2 = [{PortIP1, Module, Opts} | Ports1],
            ejabberd_config:add_local_option(listen, Ports2),
            ok;
        {error, Error} ->
            {error, Error}
    end.

-spec delete_listener(PortIPProto :: port_ip_proto(),
                      Module :: atom())
      -> 'ok' | {'error', 'not_found' | 'restarting' | 'running' | 'simple_one_for_one'}.
delete_listener(PortIPProto, Module) ->
    delete_listener(PortIPProto, Module, []).

-spec delete_listener(PortIPProto :: port_ip_proto(),
                      Module :: atom(),
                      Opts :: [listener_option()])
      -> 'ok' | {'error', 'not_found' | 'restarting' | 'running' | 'simple_one_for_one'}.
delete_listener(PortIPProto, Module, Opts) ->
%%    this one stops a listener and deletes it from configuration, used while reloading config
    {Port, IPT, _, _, Proto, _} = parse_listener_portip(PortIPProto, Opts),
    PortIP1 = {Port, IPT, Proto},
    Ports = case ejabberd_config:get_local_option(listen) of
                undefined ->
                    [];
                Ls ->
                    Ls
            end,
    Ports1 = lists:keydelete(PortIP1, 1, Ports),
    ejabberd_config:add_local_option(listen, Ports1),
    stop_listener(PortIP1, Module).

%%%
%%% Check options
%%%

-spec check_listener_options([any()]) -> 'ok'.
check_listener_options(Opts) ->
    case includes_deprecated_ssl_option(Opts) of
        false -> ok;
        true ->
            Error = "There is a problem with your ejabberd configuration file: "
                "the option 'ssl' for listening sockets is no longer available."
                " To get SSL encryption use the option 'tls'.",
            throw({error, Error})
    end,
    case certfile_readable(Opts) of
        true -> ok;
        {false, Path} ->
            ErrorText = "There is a problem in the configuration: "
                "the specified file is not readable: ",
            throw({error, ErrorText ++ Path})
    end,
    ok.

%% @doc Parse the options of the socket,
%% and return if the deprecated option 'ssl' is included
-spec includes_deprecated_ssl_option(list()) -> boolean().
includes_deprecated_ssl_option(Opts) ->
    case lists:keysearch(ssl, 1, Opts) of
        {value, {ssl, _SSLOpts}} ->
            true;
        _ ->
            lists:member(ssl, Opts)
    end.

-spec certfile_readable([any()]) -> 'true' | {'false', string()}.
certfile_readable(Opts) ->
    case proplists:lookup(certfile, Opts) of
        none -> true;
        {certfile, Path} ->
            case ejabberd_config:is_file_readable(Path) of
                true -> true;
                false -> {false, Path}
            end
    end.

-spec get_proto([any()]) -> 'tcp' | 'udp' | 'ws' | 'wss'.
get_proto(Opts) ->
    case proplists:get_value(proto, Opts) of
        undefined ->
            tcp;
        Proto ->
            normalize_proto(Proto)
    end.

-spec normalize_proto(_) -> 'tcp' | 'udp' | 'ws' | 'wss'.
normalize_proto(tcp) -> tcp;
normalize_proto(udp) -> udp;
normalize_proto(ws)  -> ws;
normalize_proto(wss) -> wss;
normalize_proto(UnknownProto) ->
    ?WARNING_MSG("There is a problem in the configuration: "
                 "~p is an unknown IP protocol. Using tcp as fallback",
                 [UnknownProto]),
    tcp.

socket_error(Reason, PortIP, Module, SockOpts, Port, IPS) ->
    ReasonT = case Reason of
                  eaddrnotavail ->
                      "IP address not available: " ++ IPS;
                  eaddrinuse ->
                      "IP address and port number already used: "
                          ++IPS++" "++integer_to_list(Port);
                  _ ->
                      format_error(Reason)
              end,
    ?ERROR_MSG("Failed to open socket:~n  ~p~nReason: ~s",
               [{Port, Module, SockOpts}, ReasonT]),
    throw({Reason, PortIP}).

-spec format_error(atom()) -> string().
format_error(Reason) ->
    case inet:format_error(Reason) of
        "unknown POSIX error" ->
            atom_to_list(Reason);
        ReasonStr ->
            ReasonStr
    end.
