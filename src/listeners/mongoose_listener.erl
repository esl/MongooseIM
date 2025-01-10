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
-export([listener_id/1, prepare_socket_opts/1, element_spirals/0]).

-callback start_listener(options()) -> {ok, supervisor:child_spec()}.
-callback instrumentation(options()) -> [mongoose_instrument:spec()].
-optional_callbacks([instrumentation/1]).

-type options() :: #{port := inet:port_number(),
                     ip_tuple := inet:ip_address(),
                     ip_address := string(),
                     ip_version := inet:address_family(),
                     proto := proto(),
                     any() := any()}.
-type id() :: {inet:port_number(), inet:ip_address(), proto()}.
-type proto() :: tcp.
-type typed_listeners() :: [{Type :: ranch | cowboy, Listener :: ranch:ref()}].

-type connection_details() :: #{
        proxy        := boolean(),
        version      => 1 | 2,
        src_address  := inet:ip_address() | binary(),
        src_port     := inet:port_number(),
        dest_address := inet:ip_address() | binary(),
        dest_port    := inet:port_number()
       }.

-export_type([options/0, connection_details/0, id/0, proto/0]).

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

start_listener(Opts = #{module := Module}) ->
    try
        {ok, ChildSpec} = Module:start_listener(Opts),
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
    lists:usort([Spec || Listener <- Listeners, Spec <- listener_instrumentation(Listener)]).

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

-spec element_spirals() -> [atom()].
element_spirals() ->
    [count, stanza_count, message_count, iq_count, presence_count,
     error_count, message_error_count, iq_error_count, presence_error_count].

%% @doc Create a unique ID based on the listening socket address
-spec listener_id(options()) -> id().
listener_id(#{port := Port, ip_tuple := IPTuple, proto := Proto}) ->
    {Port, IPTuple, Proto}.

-spec prepare_socket_opts(map()) -> map().
prepare_socket_opts(#{port := Port,
                      ip_version := IPVersion,
                      ip_tuple := IPTuple,
                      backlog := Backlog,
                      num_acceptors := NumAcceptors,
                      max_connections := MaxConnections,
                      reuse_port := ReusePort}) ->
    SocketOpts = [{nodelay, true},
                  {keepalive, true},
                  {ip, IPTuple},
                  {port, Port},
                  {backlog, Backlog},
                  IPVersion
                  | maybe_reuseport(ReusePort)],
    #{max_connections => MaxConnections,
      num_acceptors => NumAcceptors,
      num_listen_sockets => num_listen_sockets(ReusePort),
      socket_opts => SocketOpts}.

maybe_reuseport(false) -> [];
maybe_reuseport(true) -> [{reuseport, true}, {reuseport_lb, true}].

num_listen_sockets(false) -> 1;
num_listen_sockets(true) -> erlang:system_info(schedulers_online).
