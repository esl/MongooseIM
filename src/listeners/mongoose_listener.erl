%% @doc Manage starting and stopping of configured listeners
-module(mongoose_listener).

-include("mongoose_logger.hrl").

%% API
-export([start/0, stop/0]).
-export([suspend_listeners_and_shutdown_connections/0]).

%% Only for tests
-export([start_listener/1, stop_listener/1]).
-ignore_xref([start_listener/1, stop_listener/1]).

%% Helpers
-export([child_spec/1, read_connection_details/3, listener_id/1]).

-callback listener_spec(options()) -> supervisor:child_spec().
-callback instrumentation(options()) -> [mongoose_instrument:spec()].
-optional_callbacks([instrumentation/1]).

-type options() :: #{port := inet:port_number(),
                     ip_tuple := inet:ip_address(),
                     ip_address := string(),
                     ip_version := inet:address_family(),
                     proto := tcp,
                     module := module(),
                     connection_type := connection_type(),
                     hibernate_after := timeout(),
                     tls => just_tls:options(),
                     %% HTTP
                     handlers => list(),
                     transport => ranch:opts(),
                     protocol => cowboy:opts(),
                     %% XMPP
                     access => atom(),
                     backlog => non_neg_integer(),
                     max_connections => infinity | pos_integer(),
                     max_stanza_size => non_neg_integer(),
                     num_acceptors => pos_integer(),
                     proxy_protocol => boolean(),
                     reuse_port => boolean(),
                     shaper => mongoose_shaper:name(),
                     %% C2S
                     allowed_auth_methods => [atom()],
                     backwards_compatible_session => boolean(),
                     state_timeout => non_neg_integer(),
                     %% Components
                     password => binary(),
                     check_from => boolean(),
                     hidden_components => boolean(),
                     conflict_behaviour => disconnect | kick_old,
                     %% WebSockets
                     peer => mongoose_transport:peer(),
                     peer_cert => undefined | binary()
                    }.

-type id() :: {inet:port_number(), inet:ip_address(), tcp}.
-type transport_module() :: ranch_tcp | ranch_ssl | module().
-type typed_listeners() :: [{Type :: ranch | cowboy, Listener :: ranch:ref()}].
-type init_args() :: {transport_module(), ranch:ref(), options()}.
-type connection_type() :: c2s | s2s | component | http.

-type connection_details() :: #{
        proxy        := boolean(),
        version      => 1 | 2,
        src_address  := inet:ip_address(),
        src_port     := inet:port_number(),
        dest_address := inet:ip_address(),
        dest_port    := inet:port_number()
       }.

-export_type([options/0, init_args/0, connection_type/0,
              transport_module/0, connection_details/0, id/0]).

%% API

-spec start() -> ok.
start() ->
    Listeners = mongoose_config:get_opt(listen),
    mongoose_instrument:set_up(instrumentation(Listeners)),
    lists:foreach(fun start_listener/1, Listeners).

-spec stop() -> ok.
stop() ->
    Listeners = mongoose_config:get_opt(listen),
    lists:foreach(fun stop_listener/1, Listeners),
    mongoose_instrument:tear_down(instrumentation(Listeners)).

%% Internal functions

start_listener(#{module := Module} = Opts) ->
    try
        ChildSpec = Module:listener_spec(Opts),
        mongoose_listener_sup:start_child(ChildSpec)
    catch
        Class:Reason:Stacktrace ->
            ?LOG_CRITICAL(#{what => listener_failed_to_start,
                            text => <<"Failed to start a listener">>,
                            module => Module, opts => Opts,
                            class => Class, reason => Reason, stacktrace => Stacktrace}),
            erlang:raise(Class, Reason, Stacktrace)
    end.

stop_listener(Opts) ->
    ListenerId = listener_id(Opts),
    supervisor:terminate_child(mongoose_listener_sup, ListenerId),
    supervisor:delete_child(mongoose_listener_sup, ListenerId).

%% Return deduplicated instrumentation specs.
%% Each listener module could be started more than once on different ports.
-spec instrumentation([options()]) -> [mongoose_instrument:spec()].
instrumentation(Listeners) ->
    %% c2s instrumentation is shared between Bosh, Websockets and TCP listeners
    lists:usort([Spec || Listener <- Listeners, Spec <- listener_instrumentation(Listener)])
    ++ mongoose_c2s:instrumentation().

-spec listener_instrumentation(options()) -> [mongoose_instrument:spec()].
listener_instrumentation(Opts = #{module := Module}) ->
    case mongoose_lib:is_exported(Module, instrumentation, 1) of
        true ->
            Module:instrumentation(Opts);
        false ->
            []
    end.

-spec suspend_listeners_and_shutdown_connections() -> StoppedCount :: non_neg_integer().
suspend_listeners_and_shutdown_connections() ->
    TypedListeners = get_typed_listeners(),
    suspend_listeners(TypedListeners),
    broadcast_c2s_shutdown_sup() +
        broadcast_c2s_shutdown_to_regular_c2s_connections(TypedListeners).

-spec suspend_listeners(typed_listeners()) -> ok.
suspend_listeners(TypedListeners) ->
    [ranch:suspend_listener(Ref) || {_Type, Ref} <- TypedListeners],
    ok.

-spec get_typed_listeners() -> typed_listeners().
get_typed_listeners() ->
    Children = supervisor:which_children(mongoose_listener_sup),
    Listeners1 = [{cowboy, ejabberd_cowboy:ref(Listener)}
                  || {Listener, _, _, [ejabberd_cowboy]} <- Children],
    Listeners2 = [{ranch, Ref}
                  || {Ref, _, _, [mongoose_c2s_listener]} <- Children],
    Listeners1 ++ Listeners2.

-spec broadcast_c2s_shutdown_sup() -> StoppedCount :: non_neg_integer().
broadcast_c2s_shutdown_sup() ->
    %% Websocket c2s connections have two processes per user:
    %% - one is websocket Cowboy process.
    %% - one is under mongoose_c2s_sup.
    %%
    %% Regular XMPP connections are not under mongoose_c2s_sup,
    %% they are under the Ranch listener, which is a child of mongoose_listener_sup.
    %%
    %% We could use ejabberd_sm to get both Websocket and regular XMPP sessions,
    %% but waiting till the list size is zero is much more computationally
    %% expensive in that case.
    Children = supervisor:which_children(mongoose_c2s_sup),
    lists:foreach(
        fun({_, Pid, _, _}) ->
            mongoose_c2s:exit(Pid, system_shutdown)
        end,
        Children),
    mongoose_lib:wait_until(
        fun() ->
              Res = supervisor:count_children(mongoose_c2s_sup),
              proplists:get_value(active, Res)
        end,
        0),
    length(Children).

%% Based on https://ninenines.eu/docs/en/ranch/2.1/guide/connection_draining/
-spec broadcast_c2s_shutdown_to_regular_c2s_connections(typed_listeners()) ->
    non_neg_integer().
broadcast_c2s_shutdown_to_regular_c2s_connections(TypedListeners) ->
    Refs = [Ref || {ranch, Ref} <- TypedListeners],
    StoppedCount = lists:foldl(
        fun(Ref, Count) ->
            Conns = ranch:procs(Ref, connections),
            [mongoose_c2s:exit(Pid, system_shutdown) || Pid <- Conns],
            length(Conns) + Count
        end, 0, Refs),
    lists:foreach(
        fun(Ref) ->
            ok = ranch:wait_for_connections(Ref, '==', 0)
        end, Refs),
    StoppedCount.

-spec read_connection_details
    (ranch:ref(), ranch_tcp, options()) ->
        {ok, inet:socket(), connection_details()} | {error, term()};
    (ranch:ref(), ranch_ssl, options()) ->
        {ok, ssl:sslsocket(), connection_details()} | {error, term()};
    (ranch:ref(), module(), options()) ->
        {ok, term(), connection_details()} | {error, term()}.
read_connection_details(Ref, _Transport, #{proxy_protocol := true}) ->
    {ok, #{src_address := PeerIp, src_port := PeerPort, dest_address := DestAddr,
           dest_port := DesPort, version := Version}} = ranch:recv_proxy_header(Ref, 1000),
    {ok, Socket} = ranch:handshake(Ref),
    {ok, Socket, #{proxy => true,
                   src_address => PeerIp,
                   src_port => PeerPort,
                   dest_address => DestAddr,
                   dest_port => DesPort,
                   version => Version}};
read_connection_details(Ref, ranch_tcp, _Opts) ->
    {ok, Socket} = ranch:handshake(Ref),
    {ok, {DestAddr, DestPort}} = ranch_tcp:sockname(Socket),
    {ok, {SrcAddr, SrcPort}} = ranch_tcp:peername(Socket),
    ranch_tcp:setopts(Socket, [{active, once}]),
    {ok, Socket, #{proxy => false,
                   src_address => SrcAddr,
                   src_port => SrcPort,
                   dest_address => DestAddr,
                   dest_port => DestPort}};
read_connection_details(Ref, ranch_ssl, _Opts) ->
    {ok, Socket} = ranch:handshake(Ref),
    {ok, {DestAddr, DestPort}} = ranch_ssl:sockname(Socket),
    {ok, {SrcAddr, SrcPort}} = ranch_ssl:peername(Socket),
    ranch_ssl:setopts(Socket, [{active, once}]),
    {ok, Socket, #{proxy => false,
                   src_address => SrcAddr,
                   src_port => SrcPort,
                   dest_address => DestAddr,
                   dest_port => DestPort}}.

%% @doc Create a unique ID based on the listening socket address
-spec listener_id(options()) -> id().
listener_id(#{port := Port, ip_tuple := IPTuple, proto := Proto}) ->
    {Port, IPTuple, Proto}.

-spec child_spec(options()) -> supervisor:child_spec().
child_spec(#{module := Module} = Opts) ->
    ListenerId = listener_id(Opts),
    TransportModule = transport_module(Opts),
    TransportOpts = transport_opts(Opts),
    #{} = ChildSpec = ranch:child_spec(ListenerId, TransportModule, TransportOpts, Module, Opts),
    ChildSpec#{id => ListenerId, modules => [Module, ranch_embedded_sup]}.

-spec transport_module(#{tls => any(), _ => _}) -> transport_module().
transport_module(#{tls := #{mode := tls}}) ->
    ranch_ssl;
transport_module(_) ->
    ranch_tcp.

-spec transport_opts(map()) -> map().
transport_opts(#{port := Port,
                 ip_version := IPVersion,
                 ip_tuple := IPTuple,
                 backlog := Backlog,
                 num_acceptors := NumAcceptors,
                 max_connections := MaxConnections,
                 reuse_port := ReusePort} = Opts) ->
    SocketOpts = [{nodelay, true},
                  {keepalive, true},
                  {ip, IPTuple},
                  {port, Port},
                  {backlog, Backlog},
                  IPVersion
                  | maybe_reuseport(ReusePort)],
    TransportOpts = #{max_connections => MaxConnections,
                      num_acceptors => NumAcceptors,
                      num_listen_sockets => num_listen_sockets(ReusePort),
                      socket_opts => SocketOpts},
    maybe_tls_opts(Opts, TransportOpts).

-spec maybe_tls_opts(map(), map()) -> map().
maybe_tls_opts(#{tls := #{mode := tls} = TLSOpts}, #{socket_opts := SocketOpts} = TransportOpts) ->
    SslSocketOpts = just_tls:make_server_opts(TLSOpts),
    TransportOpts#{socket_opts => SocketOpts ++ SslSocketOpts};
maybe_tls_opts(_, TransportOpts) ->
    TransportOpts.

-spec maybe_reuseport(boolean()) -> [gen_tcp:option()].
maybe_reuseport(false) -> [];
maybe_reuseport(true) -> [{reuseport, true}, {reuseport_lb, true}].

-spec num_listen_sockets(boolean()) -> non_neg_integer().
num_listen_sockets(false) -> 1;
num_listen_sockets(true) -> erlang:system_info(schedulers_online).
