%%==============================================================================
%% Copyright 2018 Erlang Solutions Ltd.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

-module(mongoose_tcp_listener).
-author('konrad.zemek@erlang-solutions.com').

-include("mongoose.hrl").

-behaviour(supervisor).

%% We do not block on send anymore.
-define(TCP_SEND_TIMEOUT, 15000).

-export([start_listener/1, start_link/1, init/1]).

%% Internal
-export([start_accept_loop/3, accept_loop/3]).

-ignore_xref([start_link/1, start_accept_loop/3]).

-type options() :: #{module := module(),
                     port := inet:port_number(),
                     ip_tuple := inet:ip_address(),
                     ip_address := string(),
                     ip_version := 4 | 6,
                     proto := tcp,
                     num_acceptors := pos_integer(),
                     backlog := non_neg_integer(),
                     proxy_protocol := boolean(),
                     atom() => any()}.

-type connection_details() :: #{
        proxy        := boolean(),
        version      => 1 | 2,
        src_address  := inet:ip_address() | binary(),
        src_port     := inet:port_number(),
        dest_address := inet:ip_address() | binary(),
        dest_port    := inet:port_number()
       }.
-export_type([options/0, connection_details/0]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec start_listener(options()) -> ok.
start_listener(Opts = #{proto := tcp}) ->
    ListenerId = mongoose_listener_config:listener_id(Opts),
    mongoose_listener_sup:start_child(listener_child_spec(ListenerId, Opts)).

listener_child_spec(ListenerId, Opts) ->
    #{id => ListenerId,
      start => {?MODULE, start_link, [Opts]},
      restart => permanent,
      shutdown => 1000,
      type => supervisor,
      modules => [?MODULE]}.

-spec start_link(options()) -> any().
start_link(Opts) ->
    supervisor:start_link(?MODULE, Opts).

-spec init(options()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(Opts = #{module := Module, num_acceptors := NumAcceptors}) ->
    try
        ListenSocket = listen_tcp(Opts),
        Id = mongoose_listener_config:listener_id(Opts),
        Children = [make_childspec({Id, I}, ListenSocket, Module, Opts)
                    || I <- lists:seq(1, NumAcceptors)],
        {ok, {#{strategy => one_for_one, intensity => 100, period => 1}, Children}}
    catch
        Error -> exit(Error)
    end.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

-spec start_accept_loop(Socket :: port(),
                        Module :: module(),
                        Opts :: options()) -> {ok, pid()}.
start_accept_loop(ListenSock, Module, Opts) ->
    Pid = proc_lib:spawn_link(?MODULE, accept_loop, [ListenSock, Module, Opts]),
    {ok, Pid}.

-spec accept_loop(Socket :: port(),
                  Module :: module(),
                  Opts :: options()) -> no_return().
accept_loop(ListenSocket, Module, Opts = #{proxy_protocol := ProxyProtocol}) ->
    case do_accept(ListenSocket, ProxyProtocol) of
        {ok, Socket, ConnectionDetails} ->
            ?LOG_INFO(#{what => tcp_accepted,
                        socket => Socket, handler_module => Module,
                        conn_details => ConnectionDetails}),
            ejabberd_socket:start(
              Module, gen_tcp, Socket, Opts, ConnectionDetails),
            ?MODULE:accept_loop(ListenSocket, Module, Opts);
        {error, Reason} ->
            ?LOG_INFO(#{what => tcp_accept_failed,
                        listen_socket => ListenSocket,
                        reason => Reason, handler_module => Module}),
            ?MODULE:accept_loop(ListenSocket, Module, Opts)
    end.

-spec do_accept(gen_tcp:socket(), boolean()) ->
    {ok, gen_tcp:socket(), connection_details()} | {error, term()}.
do_accept(ListenSocket, ProxyProtocol) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} when ProxyProtocol ->
            read_proxy_header(Socket);
        {ok, Socket} ->
            {ok, {DestAddr, DestPort}} = inet:sockname(Socket),
            {ok, {SrcAddr, SrcPort}} = inet:peername(Socket),
            {ok, Socket, #{proxy => false,
                           src_address => SrcAddr,
                           src_port => SrcPort,
                           dest_address => DestAddr,
                           dest_port => DestPort}};
        Other ->
            Other
    end.

-spec read_proxy_header(gen_tcp:socket()) -> {ok, gen_tcp:socket(), connection_details()}.
read_proxy_header(Socket) ->
    {ok, ProxyInfo} = ranch_tcp:recv_proxy_header(Socket, 1000),
    {ok, Socket, #{proxy => true,
                   src_address => maps:get(src_address, ProxyInfo),
                   src_port => maps:get(src_port, ProxyInfo),
                   dest_address => maps:get(dest_address, ProxyInfo),
                   dest_port => maps:get(dest_port, ProxyInfo),
                   version => maps:get(version, ProxyInfo)
                  }}.

-spec make_childspec(Id :: term(), ListenSock :: port(),
                     Module :: module(), Opts :: options()) ->
                            supervisor:child_spec().
make_childspec(Id, ListenSock, Module, Opts) ->
    #{id => Id,
      start => {?MODULE, start_accept_loop, [ListenSock, Module, Opts]},
      restart => permanent,
      shutdown => brutal_kill,
      type => worker,
      modules => [?MODULE]}.

-spec listen_tcp(options()) -> port().
listen_tcp(Opts = #{port := Port}) ->
    SockOpts = prepare_socket_opts(Opts),
    Res = listen_or_retry(Port, SockOpts, 10),
    case Res of
        {ok, ListenSocket} ->
            ListenSocket;
        {error, Reason} ->
            error(#{what => mongoose_tcp_listener_init_failed,
                    reason => Reason,
                    text => inet:format_error(Reason),
                    options => Opts})
    end.

prepare_socket_opts(#{ip_version := IPVersion, ip_tuple := IPTuple, backlog := Backlog}) ->
    [binary,
     {packet, 0},
     {active, false},
     {reuseaddr, true},
     {nodelay, true},
     {send_timeout, ?TCP_SEND_TIMEOUT},
     {keepalive, true},
     {send_timeout_close, true},
     mongoose_listener_config:address_family(IPVersion),
     {ip, IPTuple},
     {backlog, Backlog}].

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
