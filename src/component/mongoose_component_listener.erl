-module(mongoose_component_listener).

-behaviour(mongoose_listener).
-export([start_listener/1, instrumentation/1]).

-behaviour(ranch_protocol).
-export([start_link/3]).

-type conflict_behaviour() :: disconnect | kick_old.

-type options() :: #{module := ?MODULE,
                     access := atom(),
                     shaper := atom(),
                     password := binary(),
                     check_from := boolean(),
                     hidden_components := boolean(),
                     conflict_behaviour := conflict_behaviour(),
                     atom() => any()}.

-export_type([conflict_behaviour/0, options/0]).

%% mongoose_listener
-spec instrumentation(_) -> [mongoose_instrument:spec()].
instrumentation(_) ->
    [{component_tcp_data_in, #{}, #{metrics => #{byte_size => spiral}}},
     {component_tcp_data_out, #{}, #{metrics => #{byte_size => spiral}}},
     {component_tls_data_in, #{}, #{metrics => #{byte_size => spiral}}},
     {component_tls_data_out, #{}, #{metrics => #{byte_size => spiral}}},
     {component_xmpp_element_size_out, #{}, #{metrics => #{byte_size => histogram}}},
     {component_xmpp_element_size_in, #{}, #{metrics => #{byte_size => histogram}}},
     {component_auth_failed, #{}, #{metrics => #{count => spiral}}},
     {component_element_in, #{},
      #{metrics => maps:from_list([{Metric, spiral} || Metric <- mongoose_listener:element_spirals()])}},
     {component_element_out, #{},
      #{metrics => maps:from_list([{Metric, spiral} || Metric <- mongoose_listener:element_spirals()])}}].

%% mongoose_listener
-spec start_listener(options()) -> {ok, supervisor:child_spec()}.
start_listener(Opts) ->
    TransportModule = transport_module(Opts),
    TransportOpts0 = mongoose_listener:prepare_socket_opts(Opts),
    TransportOpts = maybe_tls_opts(Opts, TransportOpts0),
    ListenerId = mongoose_listener_config:listener_id(Opts),
    ChildSpec0 = ranch:child_spec(ListenerId, TransportModule, TransportOpts, ?MODULE, Opts),
    ChildSpec1 = ChildSpec0#{id := ListenerId, modules => [?MODULE, ranch_embedded_sup]},
    {ok, ChildSpec1}.

transport_module(#{tls := _}) ->
    ranch_ssl;
transport_module(#{}) ->
    ranch_tcp.

maybe_tls_opts(#{tls := TLSOpts}, #{socket_opts := SocketOpts} = TransportOpts) ->
    SslSocketOpts = just_tls:make_cowboy_ssl_opts(TLSOpts),
    TransportOpts#{socket_opts => SocketOpts ++ SslSocketOpts};
maybe_tls_opts(_, TransportOpts) ->
    TransportOpts.

%% ranch_protocol
-spec start_link(ranch:ref(), module(), mongoose_component_connection:listener_opts()) -> {ok, pid()}.
start_link(Ref, Transport, Opts = #{hibernate_after := HibernateAfterTimeout}) ->
    ProcessOpts = [{hibernate_after, HibernateAfterTimeout}],
    mongoose_component_connection:start_link({mongoose_component_ranch, {Transport, Ref}, Opts}, ProcessOpts).
